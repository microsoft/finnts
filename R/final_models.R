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
#'#This function calculates prediction intervals using both conformal prediction and z-score methods,
# adjusting for different time horizons in forecasting data.
#' @param fcst_tbl forecast table to use to create prediction intervals
#' @param train_test_split train test split
#' @param conf_levels confidence levels for prediction intervals (default: c(0.80, 0.95))
#' @param split_ratio split ratio for calibration and test sets (default: 0.8)
#'
#' @return data frame with prediction intervals
#' @noRd
create_prediction_intervals <- function(fcst_tbl, train_test_split, conf_levels = c(0.80, 0.95), split_ratio = 0.8) {
  
  # Step 1: Set up logging
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
    

    # Step 2: Filter and prepare data
    back_test_ids <- train_test_split %>%
      dplyr::filter(Run_Type == "Back_Test") %>%
      dplyr::pull(Train_Test_ID)
    
    fcst_tbl_filtered <- fcst_tbl %>%
      dplyr::filter(Train_Test_ID %in% back_test_ids) %>%
      dplyr::arrange(Date, Horizon)
    
    log_message("Filtered forecast table rows:", nrow(fcst_tbl_filtered))

    results <- list()
    

    # Step 3: Process each combination of Combo and Model_ID
    for (combo in unique(fcst_tbl_filtered$Combo)) {
      for (model_id in unique(fcst_tbl_filtered$Model_ID[fcst_tbl_filtered$Combo == combo])) {
        log_message("Processing Combo:", combo, "Model ID:", model_id)
        
        combo_model_data <- fcst_tbl_filtered %>%
          dplyr::filter(Combo == combo, Model_ID == model_id)

        # Step 4: Split data into calibration and test sets
        n <- nrow(combo_model_data)
        split_index <- ceiling(split_ratio * n)
        calibration_set <- combo_model_data[1:split_index, ]
        test_set <- combo_model_data[(split_index + 1):n, ]

        # Step 5: Calculate Z-scores using calibration data
        residuals <- calibration_set$Target - calibration_set$Forecast
        residual_std_dev <- sd(residuals, na.rm = TRUE)
        
        z_scores <- c(qnorm((1 + conf_levels[1]) / 2), qnorm((1 + conf_levels[2]) / 2))
        z_vals <- z_scores * residual_std_dev

        log_message(sprintf("Z values: %.2f, %.2f", z_vals[1], z_vals[2]))


#Expanding window implementation to catch more uncertainty
# Step 6: Apply adaptive rolling window conformal method per horizon
horizon_results <- purrr::map_dfr(unique(calibration_set$Horizon), function(h) {
  horizon_data <- calibration_set %>% 
    dplyr::filter(Horizon >= h & Horizon <= h + 2) %>%
    dplyr::arrange(Date)
  


    n_horizon <- nrow(horizon_data)
    min_window_size <- 3  #TO DO: make some floor?
# Dynamic window size based on data variance
  min_window_size <- max(3, round(n_horizon * 0.1))
  window_size <- min(min_window_size, n_horizon - 1)
    
    # Collect all residuals from rolling windows
    residuals <- numeric() 
    
    
    for (i in 1:(n_horizon - window_size + 1)) {
      window_data <- horizon_data[i:(i + window_size - 1), ]
      current_residuals <- window_data$Target - window_data$Forecast
      residuals <- c(residuals, current_residuals) #To make sure it changes the variable out of the loop scope

    
    }
  
  # Calculate quantiles using all collected residuals
  q_vals <- sapply(conf_levels, function(cl) {
    alpha <- 1 - cl
    quantile(abs(residuals), probs = 1 - alpha/2, na.rm = TRUE)
  })
  
  log_message(sprintf("Horizon: %d, Q values: %.2f, %.2f", h, q_vals[1], q_vals[2]))
  
  dplyr::tibble(
    Combo = combo,
    Model_ID = model_id,
    Horizon = h,
    q_val_80 = q_vals[1],
    q_val_95 = q_vals[2]
  )
})

# Add z-values to horizon_results
horizon_results <- horizon_results %>%
  dplyr::mutate(
    z_val_80 = z_vals[1],
    z_val_95 = z_vals[2]
  )

# Step 7: Handle missing horizons
max_horizon <- max(horizon_results$Horizon)
max_horizon_values <- horizon_results %>%
  dplyr::filter(Horizon == max_horizon) %>%
  dplyr::select(q_val_80, q_val_95, z_val_80, z_val_95)

# Add fallback values for horizons not in calibration data
all_horizons <- unique(combo_model_data$Horizon)
missing_horizons <- setdiff(all_horizons, horizon_results$Horizon)

if (length(missing_horizons) > 0) {
  log_message(sprintf("Adding fallback values for horizons: %s", paste(missing_horizons, collapse = ", ")))
  
  fallback_results <- purrr::map_dfr(missing_horizons, function(h) {
    dplyr::tibble(
      Combo = combo,
      Model_ID = model_id,
      Horizon = h,
      q_val_80 = max_horizon_values$q_val_80,
      q_val_95 = max_horizon_values$q_val_95,
      z_val_80 = max_horizon_values$z_val_80,
      z_val_95 = max_horizon_values$z_val_95
    )
  })
  
  horizon_results <- dplyr::bind_rows(horizon_results, fallback_results)
}

        # Step 8: Calculate coverage on test set
        test_set_with_intervals <- test_set %>%
          dplyr::left_join(horizon_results, by = c("Combo", "Model_ID", "Horizon")) %>%
          dplyr::mutate(
            covered_80_conf = Target >= (Forecast - q_val_80) & Target <= (Forecast + q_val_80),
            covered_95_conf = Target >= (Forecast - q_val_95) & Target <= (Forecast + q_val_95),
            covered_80_z = Target >= (Forecast - z_val_80) & Target <= (Forecast + z_val_80),
            covered_95_z = Target >= (Forecast - z_val_95) & Target <= (Forecast + z_val_95)
          )

        coverage <- test_set_with_intervals %>%
          dplyr::summarise(
            coverage_conf_80 = mean(covered_80_conf, na.rm = TRUE),
            coverage_conf_95 = mean(covered_95_conf, na.rm = TRUE),
            coverage_z_80 = mean(covered_80_z, na.rm = TRUE),
            coverage_z_95 = mean(covered_95_z, na.rm = TRUE)
          )

        log_message(sprintf("All-up Conformal Coverage: 80%%: %.2f%%, 95%%: %.2f%%", 
                            coverage$coverage_conf_80 * 100, coverage$coverage_conf_95 * 100))
        log_message(sprintf("All-up Z-Score Coverage: 80%%: %.2f%%, 95%%: %.2f%%", 
                            coverage$coverage_z_80 * 100, coverage$coverage_z_95 * 100))
        # Step 9: Store results
        results[[length(results) + 1]] <- horizon_results %>%
          dplyr::mutate(
            coverage_conf_80 = coverage$coverage_conf_80,
            coverage_conf_95 = coverage$coverage_conf_95,
            coverage_z_80 = coverage$coverage_z_80,
            coverage_z_95 = coverage$coverage_z_95
          )
        # Step 10: Create calibration plots (function not provided in the original code)
        create_calibration_plots(combo_model_data, horizon_results, combo, model_id, split_ratio, coverage)
      }
    
     log_message("==========================================================")
    }
    
    # Step 11: Combine all results
    results <- do.call(rbind, results)
    fcst_tbl_copy <- fcst_tbl
    # Step 13: Create final plots
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

    z_fcst_tbl <- fcst_tbl_copy %>%
      dplyr::left_join(results, by = c("Combo", "Model_ID", "Horizon")) %>%
      dplyr::mutate(
        lo_80 = Forecast - z_val_80,
        hi_80 = Forecast + z_val_80,
        lo_95 = Forecast - z_val_95,
        hi_95 = Forecast + z_val_95,
        method_80 = "Z-Score", 
        method_95 = "Z-Score", 
        coverage_80 = coverage_z_80,
        coverage_95 = coverage_z_95, 
      )
  

    



    for (combo in unique(fcst_tbl$Combo)) {
      for (model_id in unique(fcst_tbl$Model_ID[fcst_tbl$Combo == combo])) {
        log_message("Creating plots for Combo:", combo, "Model ID:", model_id)
        create_final_plot(fcst_tbl, combo, model_id)
        z_create_final_plot(z_fcst_tbl, combo, model_id)
        plot_fitted_model_with_intervals(fcst_tbl, combo, model_id)
      }
    }

    # Step 14: Save results
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
create_calibration_plots <- function(combo_model_data, horizon_results, combo, model_id, split_ratio, coverage) {
  # Split data into calibration and test sets
  n <- nrow(combo_model_data)
  split_index <- ceiling(split_ratio * n)
  calibration_data <- combo_model_data[1:split_index, ]
  test_data <- combo_model_data[(split_index + 1):n, ]

  # Join test data with horizon-specific results
  test_data <- test_data %>%
    dplyr::left_join(horizon_results, by = c("Combo", "Model_ID", "Horizon"))

  coverage_subtitle <- sprintf(
    "All-up Coverage - Conformal: 80%%: %.2f%%, 95%%: %.2f%% | Z-Score: 80%%: %.2f%%, 95%%: %.2f%%",
    coverage$coverage_conf_80 * 100, coverage$coverage_conf_95 * 100,
    coverage$coverage_z_80 * 100, coverage$coverage_z_95 * 100
  )
  
  # Histogram of absolute residuals using calibration data
  h <- ggplot2::ggplot(calibration_data, ggplot2::aes(x = abs(Target - Forecast))) +
    ggplot2::geom_histogram(binwidth = function(x) diff(range(x)) / 30, fill = "steelblue", color = "black") +
    ggplot2::geom_vline(data = horizon_results, ggplot2::aes(xintercept = q_val_80, color = "80% Conformal"), linetype = "dashed", size = 1) +
    ggplot2::geom_vline(data = horizon_results, ggplot2::aes(xintercept = q_val_95, color = "95% Conformal"), linetype = "dashed", size = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = unique(horizon_results$z_val_80), color = "80% Z-Score"), linetype = "dotted", size = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = unique(horizon_results$z_val_95), color = "95% Z-Score"), linetype = "dotted", size = 1) +
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
  
  # Calculate overall coverage
  overall_coverage_80 <- mean(forecast_data$coverage_80, na.rm = TRUE)
  overall_coverage_95 <- mean(forecast_data$coverage_95, na.rm = TRUE)
  
  # Determine the dominant method
  method_80 <- names(which.max(table(forecast_data$method_80)))
  method_95 <- names(which.max(table(forecast_data$method_95)))
  
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
      subtitle = paste("80% PI Method:", method_80, "- Coverage:", format(overall_coverage_80 * 100, digits = 2), "%\n",
                       "95% PI Method:", method_95, "- Coverage:", format(overall_coverage_95 * 100, digits = 2), "%"),
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


z_create_final_plot <- function(fcst_tbl, combo, model_id) {
  plot_data <- fcst_tbl %>%
    dplyr::filter(Combo == combo, Model_ID == model_id) %>%
    dplyr::arrange(Date)
  
  # Separate actual data and forecast data
  actual_data <- plot_data %>% dplyr::filter(!is.na(Target))
  forecast_data <- plot_data %>% dplyr::filter(is.na(Target))
  
  # Calculate overall coverage
  overall_coverage_80 <- mean(forecast_data$coverage_80, na.rm = TRUE)
  overall_coverage_95 <- mean(forecast_data$coverage_95, na.rm = TRUE)
  
  # Determine the dominant method
  method_80 <- names(which.max(table(forecast_data$method_80)))
  method_95 <- names(which.max(table(forecast_data$method_95)))
  
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
      title = paste("Final Forecast with **Z-Score** Prediction Intervals for Model ID:", model_id, "and Combo:", combo),
      subtitle = paste("80% PI Method:", method_80, "- Coverage:", format(overall_coverage_80 * 100, digits = 2), "%\n",
                       "95% PI Method:", method_95, "- Coverage:", format(overall_coverage_95 * 100, digits = 2), "%"),
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
  
  ggplot2::ggsave(paste0("plots/z_final_forecast_", combo, "_", model_id, ".png"), plot = f_plot, width = 15, height = 10, dpi = 300, bg = "white")
}



plot_fitted_model_with_intervals <- function(fcst_tbl, combo, model_id) {
  # Filter data for the specific combo and model_id
  plot_data <- fcst_tbl %>%
    dplyr::filter(Combo == combo, Model_ID == model_id) %>%
    dplyr::arrange(Date, Horizon)
  
  # Aggregate fitted values and intervals across all folds
  aggregated_data <- plot_data %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(
      Target = mean(Target, na.rm = TRUE),
      Forecast = mean(Forecast, na.rm = TRUE),
      lo_80 = mean(lo_80, na.rm = TRUE),
      hi_80 = mean(hi_80, na.rm = TRUE),
      lo_95 = mean(lo_95, na.rm = TRUE),
      hi_95 = mean(hi_95, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Date)
  
  # Identify the last date with actual data
  last_actual_date <- max(aggregated_data$Date[!is.na(aggregated_data$Target)])
  
  # Create the plot
  p <- ggplot2::ggplot(aggregated_data, ggplot2::aes(x = Date)) +
    # Plot actual data
    ggplot2::geom_line(ggplot2::aes(y = Target, color = "Actual"), size = 1) +
    ggplot2::geom_point(ggplot2::aes(y = Target, color = "Actual"), size = 2) +
    
    # Plot fitted values and forecast
    ggplot2::geom_line(ggplot2::aes(y = Forecast, color = "Fitted/Forecast"), size = 1, linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(y = Forecast, color = "Fitted/Forecast"), size = 2) +
    
    # Plot prediction intervals
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo_80, ymax = hi_80, fill = "80% PI"), alpha = 0.3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo_95, ymax = hi_95, fill = "95% PI"), alpha = 0.2) +
    
    # Add a vertical line to separate in-sample and out-of-sample periods
    ggplot2::geom_vline(xintercept = last_actual_date, linetype = "dotted", color = "red", size = 1) +
    
    # Customize colors and labels
    ggplot2::scale_color_manual(values = c("Actual" = "blue", "Fitted/Forecast" = "red"),
                                name = "Data") +
    ggplot2::scale_fill_manual(values = c("80% PI" = "lightblue", "95% PI" = "lightgreen"),
                               name = "Prediction Intervals") +
    
    ggplot2::labs(
      title = paste("Fitted Model with Prediction Intervals for Model ID:", model_id, "and Combo:", combo),
      subtitle = "Vertical red line separates in-sample (fitted) and out-of-sample (forecast) periods",
      x = "Date",
      y = "Values"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      legend.background = ggplot2::element_rect(fill = "white"),
      legend.key = ggplot2::element_rect(fill = "white", colour = "white"),
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_line(color = "gray95")
    )
  
  # Save the plot
  ggplot2::ggsave(
    paste0("plots/fitted_model_with_intervals_", combo, "_", model_id, ".png"),
    plot = p,
    width = 15,
    height = 10,
    dpi = 300,
    bg = "white"
  )
  
  return(p)
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
