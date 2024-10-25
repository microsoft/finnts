#' Final Models
#'
#' Select Best Models and Prep Final Outputs
#'
#' @param run_info run info using the [set_run_info()] function.
#' @param average_models If TRUE, create simple averages of individual models
#'  and save the most accurate one.
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
  ) %>%
    sample()

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

  if (sum(colnames(prev_log_df) %in% "weighted_mape")) {
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
    x = current_combo_list_final,
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
        return(data.frame(Combo_Hash = combo))
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
      if (average_models & length(final_model_list) > 1) {
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

      # choose best average model
      if (!is.null(averages_tbl)) {
        avg_back_test_mape <- averages_tbl %>%
          dplyr::mutate(
            Train_Test_ID = as.numeric(Train_Test_ID),
            Target = ifelse(Target == 0, 0.1, Target)
          ) %>%
          dplyr::filter(Train_Test_ID != 1) %>%
          dplyr::mutate(MAPE = round(abs((Forecast - Target) / Target), digits = 4))

        avg_best_model_mape <- avg_back_test_mape %>%
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

        avg_best_model_tbl <- avg_best_model_mape %>%
          dplyr::select(Combo, Model_ID)
      }

      # choose best overall model
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

        if (!is.null(averages_tbl)) {
          avg_model_final_tbl <- averages_tbl %>%
            dplyr::right_join(avg_best_model_tbl,
              by = c("Combo", "Model_ID")
            ) %>%
            dplyr::mutate(
              Combo_ID = Combo,
              Model_Name = "NA",
              Model_Type = "local",
              Recipe_ID = "simple_average",
              Hyperparameter_ID = "NA",
              Best_Model = "No"
            ) %>%
            dplyr::group_by(Combo_ID, Model_ID, Train_Test_ID) %>%
            dplyr::mutate(Horizon = dplyr::row_number()) %>%
            dplyr::ungroup() %>%
            create_prediction_intervals(model_train_test_tbl) %>%
            convert_weekly_to_daily(date_type, weekly_to_daily)

          write_data(
            x = avg_model_final_tbl,
            combo = unique(avg_model_final_tbl$Combo),
            run_info = run_info,
            output_type = "data",
            folder = "forecasts",
            suffix = "-average_models"
          )
        }

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

      return(data.frame(Combo_Hash = combo))
    } %>%
    base::suppressPackageStartupMessages()

  # clean up any parallel run process
  par_end(cl)

  # condense outputs into less files for larger runs
  if (length(combo_list) > 3000) {
    cli::cli_progress_step("Condensing Forecasts")

    condense_data(
      run_info,
      parallel_processing,
      num_cores
    )
  }

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

  # calculate weighted mape
  weighted_mape <- get_forecast_data(run_info = run_info) %>%
    dplyr::filter(
      Run_Type == "Back_Test",
      Best_Model == "Yes"
    ) %>%
    dplyr::mutate(
      Target = ifelse(Target == 0, 0.1, Target)
    ) %>%
    dplyr::mutate(
      MAPE = round(abs((Forecast - Target) / Target), digits = 4),
      Total = sum(Target, na.rm = TRUE),
      Weight = (MAPE * Target) / Total
    ) %>%
    dplyr::pull(Weight) %>%
    sum() %>%
    round(digits = 4)

  # update logging file
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  ) %>%
    dplyr::mutate(
      average_models = average_models,
      max_model_average = max_model_average,
      weighted_mape = round(weighted_mape, digits = 4)
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
#'
#' @return data frame with prediction intervals
#' @noRd
create_prediction_intervals <- function(fcst_tbl,
                                        train_test_split) {
  back_test_id <- train_test_split %>%
    dplyr::filter(Run_Type == "Back_Test") %>%
    dplyr::select(Train_Test_ID) %>%
    dplyr::pull(Train_Test_ID)

  prediction_interval_tbl <- fcst_tbl %>%
    dplyr::filter(Train_Test_ID %in% back_test_id) %>%
    dplyr::mutate(Residual = Target - Forecast) %>%
    dplyr::group_by(Combo, Model_ID) %>%
    dplyr::summarise(Residual_Std_Dev = sd(Residual, na.rm = TRUE)) %>%
    dplyr::ungroup()

  final_tbl <- fcst_tbl %>%
    dplyr::left_join(prediction_interval_tbl,
      by = c("Model_ID", "Combo")
    ) %>%
    dplyr::mutate(
      lo_80 = ifelse(Train_Test_ID == 1, Forecast - (1.28 * Residual_Std_Dev), NA),
      lo_95 = ifelse(Train_Test_ID == 1, Forecast - (1.96 * Residual_Std_Dev), NA),
      hi_80 = ifelse(Train_Test_ID == 1, Forecast + (1.28 * Residual_Std_Dev), NA),
      hi_95 = ifelse(Train_Test_ID == 1, Forecast + (1.96 * Residual_Std_Dev), NA)
    ) %>%
    dplyr::select(-Residual_Std_Dev)

  return(final_tbl)
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
