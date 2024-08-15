#' Final Models
#'
#' Select Best Models and Prep Final Outputs
#'
#' @param run_info run info using the [set_run_info()] function.
#' @param average_models If TRUE, create simple averages of individual models.
#' @param max_model_average Max number of models to average together. Will
#'   create model averages for 2 models up until input value or max number of
#'   models ran.
#' @param weekly_to_daily If TRUE, convert a week forecast down to day by
#'   evenly splitting across each day of week. Helps when aggregating
#'   up to higher temporal levels like month or quarter.
#' @param parallel_processing Default of NULL runs no parallel processing and
#'   forecasts each individual time series one after another. 'local_machine'
#'   leverages all cores on current machine Finn is running on. 'spark'
#'   runs time series in parallel on a spark cluster in Azure Databricks or
#'   Azure Synapse.
#' @param inner_parallel Run components of forecast process inside a specific
#'   time series in parallel. Can only be used if parallel_processing is
#'   set to NULL or 'spark'.
#' @param num_cores Number of cores to run when parallel processing is set up.
#'   Used when running parallel computations on local machine or within Azure.
#'   Default of NULL uses total amount of cores on machine minus one. Can't be
#'   greater than number of cores on machine minus 1.
#'
#' @return Final model outputs are written to disk.
#'
#' @examples
#' \donttest{
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(
#'     Date >= "2013-01-01",
#'     Date <= "2015-06-01"
#'   )
#'
#' run_info <- set_run_info()
#'
#' prep_data(run_info,
#'   input_data = data_tbl,
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month",
#'   forecast_horizon = 3
#' )
#'
#' prep_models(run_info,
#'   models_to_run = c("arima", "ets"),
#'   back_test_scenarios = 3
#' )
#'
#' train_models(run_info,
#'   run_global_models = FALSE
#' )
#'
#' final_models(run_info)
#' }
#' @export
final_models <- function(run_info,
                         average_models = TRUE,
                         max_model_average = 3,
                         weekly_to_daily = TRUE,
                         parallel_processing = NULL,
                         inner_parallel = FALSE,
                         num_cores = NULL) {
  cli::cli_progress_step("Selecting Best Models")

  # check input values
  check_input_type("run_info", run_info, "list")
  check_input_type("average_models", average_models, "logical")
  check_input_type("max_model_average", max_model_average, "numeric")
  check_input_type("num_cores", num_cores, c("NULL", "numeric"))
  check_parallel_processing(
    run_info,
    parallel_processing,
    inner_parallel
  )

  # get combos
  combo_list <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*_models.", run_info$data_output
    )
  ) %>%
    tibble::tibble(
      Path = .,
      File = fs::path_file(.)
    ) %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Type"), sep = "-", remove = TRUE) %>%
    dplyr::filter(Combo != hash_data("All-Data")) %>%
    dplyr::pull(Combo) %>%
    unique()

  # get run splits
  model_train_test_tbl <- read_file(run_info,
    path = paste0(
      "/prep_models/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
      "-train_test_split.", run_info$data_output
    ),
    return_type = "df"
  )

  # check if a previous run already has necessary outputs
  prev_combo_list <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*average_models.", run_info$data_output
    )
  ) %>%
    tibble::tibble(
      Path = .,
      File = fs::path_file(.)
    ) %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Run_Type"), sep = "-", remove = TRUE) %>%
    dplyr::pull(Combo) %>%
    unique()

  current_combo_list <- combo_list

  current_combo_list_final <- setdiff(
    current_combo_list,
    prev_combo_list
  )

  prev_log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  date_type <- prev_log_df$date_type
  forecast_approach <- prev_log_df$forecast_approach
  negative_forecast <- prev_log_df$negative_forecast
  run_global_models <- prev_log_df$run_global_models
  run_local_models <- prev_log_df$run_local_models
  run_ensemble_models <- prev_log_df$run_ensemble_models

  if ((length(current_combo_list_final) == 0 & length(prev_combo_list) > 0) | sum(colnames(prev_log_df) %in% "weighted_mape")) {

    # check if input values have changed
    current_log_df <- tibble::tibble(
      average_models = average_models,
      max_model_average = max_model_average,
    ) %>%
      data.frame()

    prev_log_df <- prev_log_df %>%
      dplyr::select(colnames(current_log_df)) %>%
      data.frame()

    if (hash_data(current_log_df) == hash_data(prev_log_df)) {
      cli::cli_alert_info("Best Models Already Selected")
      return(cli::cli_progress_done())
    } else {
      stop("Inputs have recently changed in 'final_models', please revert back to original inputs or start a new run with 'set_run_info'",
        call. = FALSE
      )
    }
  }

  # parallel run info
  par_info <- par_start(
    run_info = run_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    task_length = length(current_combo_list)
  )

  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  # submit tasks
  best_model_tbl <- foreach::foreach(
    x = current_combo_list,
    .combine = "rbind",
    .packages = packages,
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .noexport = NULL
  ) %op%
    {
      combo <- x

      # get individual and ensemble model predictions
      train_test_id_list <- model_train_test_tbl %>%
        dplyr::filter(Run_Type %in% c("Back_Test", "Future_Forecast")) %>%
        dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID)) %>%
        dplyr::pull(Train_Test_ID) %>%
        unique()

      single_model_tbl <- NULL
      if (run_local_models) {
        single_model_tbl <- tryCatch(
          {
            read_file(run_info,
              path = paste0(
                "/forecasts/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
                "-", combo, "-single_models.", run_info$data_output
              ),
              return_type = "df"
            )
          },
          warning = function(w) {
            # do nothing
          },
          error = function(e) {
            NULL
          }
        )
      }

      ensemble_model_tbl <- NULL
      if (run_ensemble_models) {
        ensemble_model_tbl <- tryCatch(
          {
            read_file(run_info,
              path = paste0(
                "/forecasts/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
                "-", combo, "-ensemble_models.", run_info$data_output
              ),
              return_type = "df"
            )
          },
          warning = function(w) {
            # do nothing
          },
          error = function(e) {
            NULL
          }
        )
      }

      global_model_tbl <- NULL
      if (run_global_models) {
        global_model_tbl <- tryCatch(
          {
            read_file(run_info,
              path = paste0(
                "/forecasts/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
                "-", combo, "-global_models.", run_info$data_output
              ),
              return_type = "df"
            )
          },
          warning = function(w) {
            # do nothing
          },
          error = function(e) {
            NULL
          }
        )
      }

      local_model_tbl <- single_model_tbl %>%
        rbind(ensemble_model_tbl)

      # check if model averaging already happened
      if ("Best_Model" %in% colnames(local_model_tbl %>% rbind(global_model_tbl))) {
        # see if average models file exists and add to model tbl
        average_model_tbl <- tryCatch(
          {
            read_file(run_info,
              path = paste0(
                "/forecasts/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
                "-", combo, "-average_models.", run_info$data_output
              ),
              return_type = "df"
            )
          },
          warning = function(w) {
            # do nothing
          },
          error = function(e) {
            NULL
          }
        )

        local_model_tbl <- local_model_tbl %>%
          rbind(average_model_tbl)

        best_model_check <- TRUE
      } else {
        best_model_check <- FALSE
      }

      # combine all forecasts
      predictions_tbl <- local_model_tbl %>%
        rbind(global_model_tbl) %>%
        dplyr::select(Combo, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Date, Forecast, Target) %>%
        dplyr::filter(Train_Test_ID %in% train_test_id_list)

      # get model list
      if (!is.null(local_model_tbl)) {
        local_model_list <- local_model_tbl %>%
          dplyr::pull(Model_ID) %>%
          unique()
      } else {
        local_model_list <- NULL
      }

      if (!is.null(global_model_tbl)) {
        global_model_list <- global_model_tbl %>%
          dplyr::pull(Model_ID) %>%
          unique()
      } else {
        global_model_list <- NULL
      }

      final_model_list <- c(local_model_list, global_model_list)

      # simple model averaging
      if (average_models & length(final_model_list) > 1 & !best_model_check) {

        # create model combinations list
        model_combinations <- tibble::tibble()

        for (number in 2:min(length(final_model_list), max_model_average)) {
          temp <- data.frame(gtools::combinations(v = final_model_list, n = length(final_model_list), r = number))

          temp <- temp %>%
            tidyr::unite(Model_Combo, colnames(temp)) %>%
            dplyr::select(Model_Combo) %>%
            tibble::tibble()

          model_combinations <- rbind(model_combinations, temp)
        }

        iter_list <- model_combinations %>%
          dplyr::pull(Model_Combo)

        par_info <- par_start(
          run_info = run_info,
          parallel_processing = if (inner_parallel) {
            "local_machine"
          } else {
            NULL
          },
          num_cores = num_cores,
          task_length = nrow(iter_list)
        )

        inner_cl <- par_info$cl
        inner_packages <- par_info$packages
        `%op%` <- par_info$foreach_operator

        averages_tbl <- foreach::foreach(
          x = iter_list,
          .combine = "rbind",
          .packages = inner_packages,
          .errorhandling = "remove",
          .verbose = FALSE,
          .inorder = FALSE,
          .multicombine = TRUE,
          .noexport = NULL
        ) %op%
          {

            # get list of models to average
            model_list <- strsplit(x, "_")[[1]]

            # create model average
            final_tbl <- predictions_tbl %>%
              dplyr::filter(Model_ID %in% model_list) %>%
              dplyr::group_by(Combo, Train_Test_ID, Date) %>%
              dplyr::summarise(
                Target = mean(Target, na.rm = TRUE),
                Forecast = mean(Forecast, na.rm = TRUE)
              ) %>%
              dplyr::mutate(Model_ID = x) %>%
              dplyr::select(Combo, Model_ID, Train_Test_ID, Date, Target, Forecast) %>%
              dplyr::ungroup()

            return(final_tbl)
          } %>%
          base::suppressPackageStartupMessages()

        par_end(inner_cl)
      } else {
        averages_tbl <- NULL
      }

      # choose best model
      final_predictions_tbl <- predictions_tbl %>%
        dplyr::select(Combo, Model_ID, Train_Test_ID, Date, Forecast, Target) %>%
        rbind(averages_tbl)

      back_test_mape <- final_predictions_tbl %>%
        dplyr::mutate(
          Train_Test_ID = as.numeric(Train_Test_ID),
          Target = ifelse(Target == 0, 0.1, Target)
        ) %>%
        dplyr::filter(Train_Test_ID != 1) %>%
        dplyr::mutate(MAPE = round(abs((Forecast - Target) / Target), digits = 4))

      best_model_mape <- back_test_mape %>%
        dplyr::group_by(Model_ID, Combo) %>%
        dplyr::mutate(
          Combo_Total = sum(abs(Target), na.rm = TRUE),
          weighted_MAPE = (abs(Target) / Combo_Total) * MAPE
        ) %>%
        dplyr::summarise(Rolling_MAPE = sum(weighted_MAPE, na.rm = TRUE)) %>%
        dplyr::arrange(Rolling_MAPE) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Combo) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()

      best_model_tbl <- best_model_mape %>%
        dplyr::mutate(Best_Model = "Yes") %>%
        dplyr::select(Combo, Model_ID, Best_Model)

      back_test_mape_final <- back_test_mape %>%
        dplyr::left_join(best_model_tbl,
          by = c("Combo", "Model_ID")
        ) %>%
        dplyr::mutate(
          Best_Model = ifelse(!is.na(Best_Model), "Yes", "No"),
          Train_Test_ID = Train_Test_ID - 1
        ) %>%
        dplyr::rename(Back_Test_Scenario = Train_Test_ID) %>%
        dplyr::group_by(Combo, Model_ID, Back_Test_Scenario) %>%
        dplyr::mutate(Horizon = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::select(Combo, Model_ID, Back_Test_Scenario, Horizon, Date, Forecast, Target, MAPE, Best_Model)

      best_model_final_tbl <- tibble::tibble(Model_ID = stringr::str_split(best_model_tbl$Model_ID, "_")[[1]]) %>%
        dplyr::mutate(
          Combo = best_model_tbl$Combo,
          Best_Model = "Yes"
        ) %>%
        tidyr::separate(col = "Model_ID", into = c("Model_Name", "Recipe_ID", "Model_Type"), sep = "--", remove = FALSE)

      # if a simple model average is the most accurate store the results
      if (nrow(best_model_final_tbl) > 1) {
        model_avg_final_tbl <- final_predictions_tbl %>%
          dplyr::right_join(best_model_tbl,
            by = c("Combo", "Model_ID")
          ) %>%
          dplyr::mutate(
            Combo_ID = Combo,
            Model_Name = "NA",
            Model_Type = "local",
            Recipe_ID = "simple_average",
            Hyperparameter_ID = "NA",
            Best_Model = "Yes"
          ) %>%
          dplyr::group_by(Combo_ID, Model_ID, Train_Test_ID) %>%
          dplyr::mutate(Horizon = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          create_prediction_intervals(model_train_test_tbl) %>%
          convert_weekly_to_daily(date_type, weekly_to_daily)

        write_data(
          x = model_avg_final_tbl,
          combo = unique(model_avg_final_tbl$Combo_ID),
          run_info = run_info,
          output_type = "data",
          folder = "forecasts",
          suffix = "-average_models"
        )

        if (!is.null(single_model_tbl)) {
          single_model_final_tbl <- single_model_tbl %>%
            dplyr::mutate(Best_Model = "No") %>%
            create_prediction_intervals(model_train_test_tbl) %>%
            convert_weekly_to_daily(date_type, weekly_to_daily)

          write_data(
            x = single_model_final_tbl,
            combo = unique(single_model_final_tbl$Combo),
            run_info = run_info,
            output_type = "data",
            folder = "forecasts",
            suffix = "-single_models"
          )
        }

        if (!is.null(ensemble_model_tbl)) {
          ensemble_model_final_tbl <- ensemble_model_tbl %>%
            dplyr::mutate(Best_Model = "No") %>%
            create_prediction_intervals(model_train_test_tbl) %>%
            convert_weekly_to_daily(date_type, weekly_to_daily)

          write_data(
            x = ensemble_model_final_tbl,
            combo = unique(ensemble_model_final_tbl$Combo),
            run_info = run_info,
            output_type = "data",
            folder = "forecasts",
            suffix = "-ensemble_models"
          )
        }

        if (!is.null(global_model_tbl)) {
          global_model_final_tbl <- global_model_tbl %>%
            dplyr::mutate(Best_Model = "No") %>%
            create_prediction_intervals(model_train_test_tbl) %>%
            convert_weekly_to_daily(date_type, weekly_to_daily)

          write_data(
            x = global_model_final_tbl,
            combo = unique(global_model_final_tbl$Combo),
            run_info = run_info,
            output_type = "data",
            folder = "forecasts",
            suffix = "-global_models"
          )
        }
      } else { # choose the most accurate individual model and write outputs

        final_model_tbl <- tibble::tibble(Model_ID = final_model_list) %>%
          dplyr::left_join(
            best_model_final_tbl %>%
              dplyr::select(Model_ID, Best_Model),
            by = "Model_ID"
          ) %>%
          dplyr::mutate(Best_Model = ifelse(!is.na(Best_Model), "Yes", "No"))

        if (!is.null(single_model_tbl)) {
          single_model_final_tbl <- single_model_tbl %>%
            remove_best_model() %>%
            dplyr::left_join(final_model_tbl,
              by = "Model_ID"
            ) %>%
            create_prediction_intervals(model_train_test_tbl) %>%
            convert_weekly_to_daily(date_type, weekly_to_daily)

          write_data(
            x = single_model_final_tbl,
            combo = unique(single_model_final_tbl$Combo),
            run_info = run_info,
            output_type = "data",
            folder = "forecasts",
            suffix = "-single_models"
          )
        }

        if (!is.null(ensemble_model_tbl)) {
          ensemble_model_final_tbl <- ensemble_model_tbl %>%
            remove_best_model() %>%
            dplyr::left_join(final_model_tbl,
              by = "Model_ID"
            ) %>%
            create_prediction_intervals(model_train_test_tbl) %>%
            convert_weekly_to_daily(date_type, weekly_to_daily)

          write_data(
            x = ensemble_model_final_tbl,
            combo = unique(ensemble_model_final_tbl$Combo),
            run_info = run_info,
            output_type = "data",
            folder = "forecasts",
            suffix = "-ensemble_models"
          )
        }

        if (!is.null(global_model_tbl)) {
          global_model_final_tbl <- global_model_tbl %>%
            remove_best_model() %>%
            dplyr::left_join(final_model_tbl,
              by = "Model_ID"
            ) %>%
            create_prediction_intervals(model_train_test_tbl) %>%
            convert_weekly_to_daily(date_type, weekly_to_daily)

          write_data(
            x = global_model_final_tbl,
            combo = unique(global_model_final_tbl$Combo),
            run_info = run_info,
            output_type = "data",
            folder = "forecasts",
            suffix = "-global_models"
          )
        }
      }

      return(best_model_mape)
    } %>%
    base::suppressPackageStartupMessages()

  # clean up any parallel run process
  par_end(cl)

  # reconcile hierarchical forecasts
  if (forecast_approach != "bottoms_up") {
    cli::cli_progress_step("Reconciling Hierarchical Forecasts")

    reconcile_hierarchical_data(
      run_info,
      parallel_processing,
      forecast_approach,
      negative_forecast,
      num_cores
    )
  }

  # update logging file
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  ) %>%
    dplyr::mutate(
      average_models = average_models,
      max_model_average = max_model_average,
      weighted_mape = base::mean(best_model_tbl$Rolling_MAPE, na.rm = TRUE)
    )

  write_data(
    x = log_df,
    combo = NULL,
    run_info = run_info,
    output_type = "log",
    folder = "logs",
    suffix = NULL
  )
}

#' Create prediction intervals
#'
#' @param fcst_tbl forecast table to use to create prediction intervals
#' @param train_test_split train test split
#' @param conf_levels confidence levels for prediction intervals (default: c(0.80, 0.95))
#' @param split_ratio split ratio for calibration and test sets (default: 0.8)
#'
#' @return data frame with prediction intervals
#' @noRd
create_prediction_intervals <- function(fcst_tbl, train_test_split, conf_levels = c(0.80, 0.95), split_ratio = 0.8) {
  log_file <- paste0("logs/prediction_intervals_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  log_conn <- file(log_file, open = "wt")
  
  log_message <- function(...) {
    message <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", paste(...))
    cat(message, "\n", file = log_conn, append = TRUE)
  }
  
  tryCatch({
    log_message("=======================START TEST==========================")
    log_message("Confidence levels:", paste(conf_levels, collapse = ", "))
    log_message("Split ratio:", split_ratio)
    
    back_test_ids <- train_test_split %>%
      dplyr::filter(Run_Type == "Back_Test") %>%
      dplyr::pull(Train_Test_ID)
    
    fcst_tbl_filtered <- fcst_tbl %>%
      dplyr::filter(Train_Test_ID %in% back_test_ids) %>%
      dplyr::arrange(Date, Horizon)
    
    log_message("Filtered forecast table rows:", nrow(fcst_tbl_filtered))

    results <- list()
    
    for (combo in unique(fcst_tbl_filtered$Combo)) {
      for (model_id in unique(fcst_tbl_filtered$Model_ID[fcst_tbl_filtered$Combo == combo])) {
        log_message("Processing Combo:", combo, "Model ID:", model_id)
        
        combo_model_data <- fcst_tbl_filtered %>%
          dplyr::filter(Combo == combo, Model_ID == model_id)

        # Combine calibration and test sets across all horizons
        calibration_test_data <- list()
        
        for (h in unique(combo_model_data$Horizon)) {
          horizon_data <- combo_model_data %>% dplyr::filter(Horizon == h)
          
          n <- nrow(horizon_data)
          split_index <- ceiling(split_ratio * n)
          calibration_set <- horizon_data[1:split_index, ]
          test_set <- horizon_data[(split_index + 1):n, ]
          
          calibration_test_data[[h]] <- list(
            calibration_set = calibration_set,
            test_set = test_set
          )
        }
        
        # Calculate Z-scores and apply to all test sets across horizons
        all_calibration_data <- do.call(rbind, lapply(calibration_test_data, function(x) x$calibration_set))
        residuals <- all_calibration_data$Target - all_calibration_data$Forecast
        residual_std_dev <- sd(residuals, na.rm = TRUE)
        
        z_scores <- c(qnorm((1 + conf_levels[1]) / 2), qnorm((1 + conf_levels[2]) / 2))
        z_vals <- z_scores * residual_std_dev

        log_message(sprintf("Z values: %.2f, %.2f", z_vals[1], z_vals[2]))

        all_test_data <- do.call(rbind, lapply(calibration_test_data, function(x) x$test_set))
        all_test_data <- all_test_data %>%
          dplyr::mutate(
            lo_80_z = Forecast - z_vals[1],
            hi_80_z = Forecast + z_vals[1],
            lo_95_z = Forecast - z_vals[2],
            hi_95_z = Forecast + z_vals[2],
            covered_80_z = Target >= lo_80_z & Target <= hi_80_z,
            covered_95_z = Target >= lo_95_z & Target <= hi_95_z
          )

        z_coverage <- list(
          z_80 = mean(all_test_data$covered_80_z, na.rm = TRUE),
          z_95 = mean(all_test_data$covered_95_z, na.rm = TRUE)
        )

        log_message(sprintf("Z-Score Coverage: 80%%: %.2f%%, 95%%: %.2f%%", 
                            z_coverage$z_80 * 100, z_coverage$z_95 * 100))

        # Apply conformal method per horizon
        horizon_results <- purrr::map_dfr(unique(combo_model_data$Horizon), function(h) {
          horizon_data <- combo_model_data %>% dplyr::filter(Horizon == h)
          calibration_set <- calibration_test_data[[h]]$calibration_set
          test_set <- calibration_test_data[[h]]$test_set

          residuals <- calibration_set$Target - calibration_set$Forecast
          q_vals <- sapply(conf_levels, function(cl) {
            alpha <- 1 - cl
            two_sided_alpha <- alpha / 2
            quantile(abs(residuals), probs = 1 - two_sided_alpha, na.rm = TRUE)
          })

          log_message(sprintf("Horizon: %d, Q values: %.2f, %.2f", h, q_vals[1], q_vals[2]))

          test_set <- test_set %>%
            dplyr::mutate(
              lo_80_conf = Forecast - q_vals[1],
              hi_80_conf = Forecast + q_vals[1],
              lo_95_conf = Forecast - q_vals[2],
              hi_95_conf = Forecast + q_vals[2],
              covered_80_conf = Target >= lo_80_conf & Target <= hi_80_conf,
              covered_95_conf = Target >= lo_95_conf & Target <= hi_95_conf
            )

          coverage <- list(
            conf_80 = mean(test_set$covered_80_conf, na.rm = TRUE),
            conf_95 = mean(test_set$covered_95_conf, na.rm = TRUE)
          )

          log_message(sprintf("Horizon: %d, Conformal Coverage: 80%%: %.2f%%, 95%%: %.2f%%", 
                              h, coverage$conf_80 * 100, coverage$conf_95 * 100))

          dplyr::tibble(
            Combo = combo,
            Model_ID = model_id,
            Horizon = h,
            q_val_80 = q_vals[1],
            q_val_95 = q_vals[2],
            z_val_80 = z_vals[1],
            z_val_95 = z_vals[2],
            coverage_conf_80 = coverage$conf_80,
            coverage_conf_95 = coverage$conf_95,
            coverage_z_80 = z_coverage$z_80,
            coverage_z_95 = z_coverage$z_95
          )
        })

        results[[length(results) + 1]] <- horizon_results
        
        # Create calibration test plots
        # create_calibration_plots(combo_model_data, horizon_results, combo, model_id)
      }
    }
    
    # Combine all results
    results <- do.call(rbind, results)

    # Add prediction intervals to the full forecast table
    fcst_tbl <- fcst_tbl %>%
      dplyr::left_join(results, by = c("Combo", "Model_ID", "Horizon")) %>%
      dplyr::mutate(
        lo_80 = Forecast - pmax(z_val_80, q_val_80, na.rm = TRUE),
        hi_80 = Forecast + pmax(z_val_80, q_val_80, na.rm = TRUE),
        lo_95 = Forecast - pmax(z_val_95, q_val_95, na.rm = TRUE),
        hi_95 = Forecast + pmax(z_val_95, q_val_95, na.rm = TRUE),
        method_80 = ifelse(z_val_80 > q_val_80, "Z-Score", "Conformal"),
        method_95 = ifelse(z_val_95 > q_val_95, "Z-Score", "Conformal"),
        coverage_80 = pmax(coverage_z_80, coverage_conf_80, na.rm = TRUE),
        coverage_95 = pmax(coverage_z_95, coverage_conf_95, na.rm = TRUE)
      )

    create_calibration_plots(combo_model_data, horizon_results, combo, model_id, split_ratio)

    create_final_plot(fcst_tbl, combo, model_id)
    
    filename <- paste0("final_forecast_table/forecast_table_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    write.csv(fcst_tbl, file = filename, row.names = FALSE)
    log_message("Forecast table written to:", filename)
    
    log_message("=======================COMPLETE==========================")
  }, error = function(e) {
    log_message("Error occurred:", conditionMessage(e))
    print(e)
  }, finally = {
    close(log_conn)
  })
  
  return(fcst_tbl)
}
create_calibration_plots <- function(combo_model_data, horizon_results, combo, model_id, split_ratio) {
  # Ensure correct data matching by joining on Combo, Model_ID, and Horizon
  plot_data <- dplyr::left_join(combo_model_data, horizon_results, by = c("Combo", "Model_ID", "Horizon"))
  
  # Initialize lists to store calibration and test data for all horizons
  all_calibration_sets <- list()
  all_test_sets <- list()
  
  # Iterate through each horizon and split into calibration and test sets
  for (h in unique(plot_data$Horizon)) {
    horizon_data <- plot_data %>% dplyr::filter(Horizon == h)
    
    # Split into calibration and test sets
    n <- nrow(horizon_data)
    split_index <- ceiling(split_ratio * n)
    calibration_set <- horizon_data[1:split_index, ]
    test_set <- horizon_data[(split_index + 1):n, ]
    
    # Append calibration and test sets to their respective lists
    all_calibration_sets[[length(all_calibration_sets) + 1]] <- calibration_set
    all_test_sets[[length(all_test_sets) + 1]] <- test_set
  }
  
  # Combine all calibration and test sets into separate data frames
  calibration_data <- do.call(rbind, all_calibration_sets)
  test_data <- do.call(rbind, all_test_sets)

  # Calculate overall coverage using only the test data
  overall_coverage <- test_data %>%
    dplyr::summarise(
      conf_80 = mean(Target >= (Forecast - q_val_80) & Target <= (Forecast + q_val_80), na.rm = TRUE),
      conf_95 = mean(Target >= (Forecast - q_val_95) & Target <= (Forecast + q_val_95), na.rm = TRUE),
      z_80 = mean(Target >= (Forecast - z_val_80) & Target <= (Forecast + z_val_80), na.rm = TRUE),
      z_95 = mean(Target >= (Forecast - z_val_95) & Target <= (Forecast + z_val_95), na.rm = TRUE)
    )
  
  coverage_subtitle <- sprintf(
    "Test Data Coverage - Conformal: 80%%: %.2f%%, 95%%: %.2f%% | Z-Score: 80%%: %.2f%%, 95%%: %.2f%%",
    overall_coverage$conf_80 * 100, overall_coverage$conf_95 * 100,
    overall_coverage$z_80 * 100, overall_coverage$z_95 * 100
  )
  
  # Histogram of absolute residuals using calibration data
  h <- ggplot2::ggplot(calibration_data, ggplot2::aes(x = abs(Target - Forecast))) +
    ggplot2::geom_histogram(binwidth = function(x) diff(range(x)) / 30, fill = "steelblue", color = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = q_val_80, color = "80% Conformal"), linetype = "dashed", size = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = q_val_95, color = "95% Conformal"), linetype = "dashed", size = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = z_val_80, color = "80% Z-Score"), linetype = "dotted", size = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = z_val_95, color = "95% Z-Score"), linetype = "dotted", size = 1) +
    ggplot2::facet_wrap(~Horizon, scales = "free") +
    ggplot2::scale_color_manual(values = c("80% Conformal" = "blue", "95% Conformal" = "red",
                                           "80% Z-Score" = "green", "95% Z-Score" = "purple")) +
    ggplot2::labs(
      title = paste("Calibration Test: Histogram of Absolute Residuals for Model ID:", model_id, "and Combo:", combo),
      subtitle = "Based on Calibration Data",
      x = "Absolute Residuals", y = "Frequency"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      legend.background = ggplot2::element_rect(fill = "white")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Thresholds"))
  
  ggplot2::ggsave(paste0("plots/calibration_histogram_", combo, "_", model_id, ".png"), plot = h, width = 15, height = 10, dpi = 300, bg = "white")

  # Time series plot with prediction intervals using test data
  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = Date, y = Target)) +
    ggplot2::geom_line(ggplot2::aes(color = "Actual"), linetype = "dashed", size = 1) +
    ggplot2::geom_line(ggplot2::aes(y = Forecast, color = "Forecast"), linetype = "dashed", size = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Forecast - q_val_80, ymax = Forecast + q_val_80, 
                                      fill = "80% Confidence: Conformal"), alpha = 0.3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Forecast - q_val_95, ymax = Forecast + q_val_95, 
                                      fill = "95% Confidence: Conformal"), alpha = 0.2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Forecast - z_val_80, ymax = Forecast + z_val_80, 
                                      fill = "80% Confidence: Z-Score"), alpha = 0.3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Forecast - z_val_95, ymax = Forecast + z_val_95, 
                                      fill = "95% Confidence: Z-Score"), alpha = 0.2) +
    ggplot2::scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
    ggplot2::scale_fill_manual(values = c(
      "80% Confidence: Conformal" = "#ffa601",
      "95% Confidence: Conformal" = "#ffbb00",
      "80% Confidence: Z-Score" = "#af4ced",
      "95% Confidence: Z-Score" = "#cc91f1"
    )) +
    ggplot2::facet_wrap(~Horizon, scales = "free_y") +
    ggplot2::labs(
      title = paste("Test Data: Prediction Intervals for Model ID:", model_id, "and Combo:", combo),
      subtitle = coverage_subtitle,
      x = "Date", y = "Values"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      legend.background = ggplot2::element_rect(fill = "white"),
      legend.key = ggplot2::element_rect(fill = "white", colour = "white"),
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_line(color = "gray95")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(title = "Prediction Intervals"),
      color = ggplot2::guide_legend(title = "Series")
    )

  ggplot2::ggsave(paste0("plots/test_timeseries_", combo, "_", model_id, ".png"), plot = p, width = 15, height = 10, dpi = 300, bg = "white")
}


create_final_plot <- function(fcst_tbl, combo, model_id) {
  plot_data <- fcst_tbl %>%
    dplyr::filter(Combo == combo, Model_ID == model_id) %>%
    dplyr::arrange(Date)
  
  # Separate actual data and forecast data
  actual_data <- plot_data %>% dplyr::filter(!is.na(Target))
  forecast_data <- plot_data %>% dplyr::filter(is.na(Target))
  
  f_plot <- ggplot2::ggplot() +
    # Plot actual data (Target)
    ggplot2::geom_line(data = actual_data, ggplot2::aes(x = Date, y = Target), color = "red") +
    ggplot2::geom_point(data = actual_data, ggplot2::aes(x = Date, y = Target), color = "red") +
    
    # Plot forecast and PIs only where Target is NA
    ggplot2::geom_line(data = forecast_data, ggplot2::aes(x = Date, y = Forecast), color = "blue") +
    ggplot2::geom_point(data = forecast_data, ggplot2::aes(x = Date, y = Forecast), color = "blue") +
    ggplot2::geom_ribbon(data = forecast_data, ggplot2::aes(x = Date, ymin = lo_80, ymax = hi_80), 
                         fill = "lightblue", alpha = 0.3) +
    ggplot2::geom_ribbon(data = forecast_data, ggplot2::aes(x = Date, ymin = lo_95, ymax = hi_95), 
                         fill = "lightblue", alpha = 0.2) +
    
    ggplot2::labs(
      title = paste("Final Forecast with Prediction Intervals for Model ID:", model_id, "and Combo:", combo),
      subtitle = paste("80% PI Method:", unique(forecast_data$method_80), "- Coverage:", format(unique(forecast_data$coverage_80) * 100, digits = 2), "%\n",
                       "95% PI Method:", unique(forecast_data$method_95), "- Coverage:", format(unique(forecast_data$coverage_95) * 100, digits = 2), "%"),
      x = "Date", y = "Values"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      legend.background = ggplot2::element_rect(fill = "white"),
      legend.key = ggplot2::element_rect(fill = "white", colour = "white"),
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_line(color = "gray95")
    )
  
  ggplot2::ggsave(paste0("plots/final_forecast_", combo, "_", model_id, ".png"), plot = f_plot, width = 15, height = 10, dpi = 300, bg = "white")
}
#' Convert weekly forecast down to daily
#'
#' @param fcst_tbl forecast table to use to create prediction intervals
#' @param date_type date type
#' @param weekly_to_daily if weekly forecast should be converted to daily
#'
#' @return data frame with final forecasts
#' @noRd
convert_weekly_to_daily <- function(fcst_tbl,
                                    date_type,
                                    weekly_to_daily) {
  if (date_type == "week" & weekly_to_daily) { # allocate from weekly to daily

    final_tbl <- fcst_tbl %>%
      dplyr::group_by(
        Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID,
        Train_Test_ID, Hyperparameter_ID, Best_Model, Combo, Horizon
      ) %>%
      dplyr::group_split() %>%
      purrr::map(.f = function(df) {
        daily_tbl <- df %>%
          dplyr::mutate(Date_Day = Date) %>%
          timetk::pad_by_time(Date_Day, .by = "day", .pad_value = NA, .end_date = max(df$Date) + 6) %>%
          tidyr::fill(tidyr::everything(), .direction = "down") %>%
          dplyr::mutate(
            Target = Target / 7,
            Forecast = Forecast / 7,
            lo_95 = lo_95 / 7,
            lo_80 = lo_80 / 7,
            hi_80 = hi_80 / 7,
            hi_95 = hi_95 / 7
          ) %>%
          dplyr::select(
            Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Hyperparameter_ID,
            Best_Model, Combo, Horizon, Date, Date_Day, Target, Forecast, lo_95, lo_80, hi_80, hi_95
          )

        return(daily_tbl)
      }) %>%
      dplyr::bind_rows()
  } else {
    final_tbl <- fcst_tbl %>%
      dplyr::select(
        Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Hyperparameter_ID,
        Best_Model, Combo, Horizon, Date, Target, Forecast, lo_95, lo_80, hi_80, hi_95
      )
  }

  return(final_tbl)
}

#' Check if there is a best model column and remove it
#'
#' @param df data frame
#'
#' @return data frame with no best model column
#' @noRd
remove_best_model <- function(df) {
  if ("Best_Model" %in% names(df)) {
    df <- df %>% dplyr::select(-Best_Model)
  }
  return(df)
}
