#' Final Models
#'
#' Select Best Models and Prep Final Outputs
#'
#' @details Candidates are screened for complete, finite predictions and extreme
#'   future magnitudes using original-scale prepared actuals. Among eligible
#'   candidates within 0.5 percentage points or 5 percent relative weighted MAPE
#'   of the best eligible accuracy, whichever allowance is larger, selection
#'   prefers lower risk and fewer future level, trend, and seasonal concerns.
#'   Tied candidates with assessed seasonal evidence then prefer smaller amplitude
#'   distortion beyond historical cycle variation, before weighted MAPE and model
#'   identifier. This preference alone does not reject a forecast. Repeated strong
#'   historical seasonality can also support phase checks over at least three
#'   informative future periods, even when the horizon is shorter than a cycle.
#'   Short histories remain usable; unsupported seasonal checks are not assessed.
#'   If all candidates fail the required checks, selection raises an error.
#'   Evaluation is deterministic for fixed inputs and creates no diagnostic files.
#'   If an individual model wins, the best eligible simple average is still saved
#'   with `Best_Model = "No"`, using the same quality-aware ranking among averages.
#'   If an average wins overall, that exact average is saved as the best model.
#'   No average artifact is required when no eligible average can be formed.
#'   Quality selection happens before hierarchical reconciliation, at each prepared
#'   hierarchy node. The selected mixture is reconciled without a second future
#'   plausibility evaluation or a whole-hierarchy replacement model. Reconciled
#'   backtests still supply reported accuracy; reconciliation does not require
#'   retained quality rankings. On retry, saved individual and average outputs
#'   must identify one complete winner per series. A `Best_Model` column or an
#'   average filename alone is not proof of completion. Incomplete selections
#'   rebuild averages and winner flags from existing predictions without fitting
#'   models again. Complete saved winners are reused without future-quality
#'   reassessment, and every series remains in the returned result.
#'
#' @param run_info run info using the [set_run_info()] function.
#' @param average_models If TRUE, create simple averages of individual models
#'  and save the eligible average selected by accuracy and future-quality checks.
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

  # get run splits
  model_train_test_tbl <- read_file(run_info,
    path = paste0(
      "/prep_models/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name),
      "-train_test_split.", run_info$data_output
    ),
    return_type = "df"
  )

  # read previous log
  prev_log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  date_type <- prev_log_df$date_type
  forecast_approach <- prev_log_df$forecast_approach
  negative_forecast <- prev_log_df$negative_forecast
  run_global_models <- prev_log_df$run_global_models
  run_local_models <- prev_log_df$run_local_models
  run_ensemble_models <- prev_log_df$run_ensemble_models
  selection_results <- list()
  rejected_combos <- character()
  all_reused <- FALSE

  if (forecast_approach != "bottoms_up" & date_type == "week") {
    # turn off daily conversion before hts recon
    initial_weekly_to_daily <- FALSE
  } else {
    initial_weekly_to_daily <- weekly_to_daily
  }

  # define columns to check for input changes
  cols_check_list <- c("average_models", "max_model_average", "weekly_to_daily")

  # check if input values have changed from previous run
  if (all(cols_check_list %in% colnames(prev_log_df))) {
    # create current log
    current_log_df <- tibble::tibble(
      average_models = average_models,
      max_model_average = max_model_average,
      weekly_to_daily = weekly_to_daily
    ) %>%
      data.frame()

    # get previous log
    prev_log_df_aligned <- prev_log_df %>%
      dplyr::select(tidyselect::all_of(cols_check_list)) %>%
      data.frame()

    if (hash_data(normalize_log_df(current_log_df)) != hash_data(normalize_log_df(prev_log_df_aligned))) {
      diff_details <- format_input_diff(prev_log_df_aligned, current_log_df)
      stop(
        "Inputs have recently changed in 'final_models'.\n",
        "The following inputs differ from the previous run:\n",
        diff_details, "\n",
        "Please revert back to original inputs or start a ",
        "new run with 'set_run_info'.",
        call. = FALSE
      )
    }
  }

  # get combos and check previous completion
  if ("combo" %in% names(run_info)) {
    # Single combo mode - no need to check previously completed combos
    combo_list <- run_info$combo
    combo_diff <- combo_list
  } else {
    # Multi combo mode - get all combos and check which are complete
    combo_list <- list_files(
      run_info$storage_object,
      paste0(
        run_info$path, "/forecasts/*", hash_data(run_info$project_name), "-",
        hash_data(run_info$run_name), "*_models.", run_info$data_output
      )
    ) %>%
      tibble::tibble(
        Path = .,
        File = fs::path_file(.)
      ) %>%
      tidyr::separate(File, into = c("Project", "Run", "Combo", "Type"), sep = "-", remove = TRUE) %>%
      dplyr::filter(Combo != hash_data("All-Data")) %>%
      dplyr::pull(Combo) %>%
      unique()

    combo_diff <- combo_list
  }

  # check if previous run is complete
  recon_complete <- TRUE
  if (forecast_approach != "bottoms_up") {
    recon_files <- list_files(
      run_info$storage_object,
      paste0(
        run_info$path, "/forecasts/*", hash_data(run_info$project_name), "-",
        hash_data(run_info$run_name), "-", hash_data("Best-Model"),
        "-reconciled.", run_info$data_output
      )
    )
    recon_complete <- length(recon_files) > 0
  }

  if (!length(combo_diff)) stop("No forecast data found for this run.", call. = FALSE)

  if (length(combo_diff) > 0) {
    # filter to only combos that need to be processed
    current_combo_list_final <- combo_diff %>%
      sample()

    # parallel run info
    par_info <- par_start(
      run_info = run_info,
      parallel_processing = parallel_processing,
      num_cores = num_cores,
      task_length = length(current_combo_list_final)
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
          single_model_tbl <- read_final_predictions(run_info, combo, "-single_models")
        }

        ensemble_model_tbl <- NULL
        if (run_ensemble_models) {
          ensemble_model_tbl <- read_final_predictions(run_info, combo, "-ensemble_models")
        }

        global_model_tbl <- NULL
        if (run_global_models) {
          global_model_tbl <- read_final_predictions(run_info, combo, "-global_models")
        }

        all_model_tbl <- dplyr::bind_rows(single_model_tbl, ensemble_model_tbl, global_model_tbl)

        # error if no forecast data was found
        if (is.null(all_model_tbl) || nrow(all_model_tbl) == 0) {
          stop(paste0("No forecast data found for combo '", combo, "'."), call. = FALSE)
        }

        # validate required columns before proceeding
        required_cols <- c("Combo", "Model_ID", "Model_Name", "Model_Type", "Recipe_ID", "Train_Test_ID", "Date", "Forecast", "Target")
        missing_cols <- setdiff(required_cols, colnames(all_model_tbl))
        if (length(missing_cols) > 0) {
          stop(paste0(
            "Combo '", combo, "': forecast data is missing required columns: ",
            paste(missing_cols, collapse = ", ")
          ), call. = FALSE)
        }

        combo_name <- unique(all_model_tbl$Combo)
        if (isTRUE(run_info$allow_quality_rejection) && length(combo_name) == 1 &&
          identical(as.character(prev_log_df[["selection_status"]]), "rejected")) {
          rejected <- rejected_agent_selection(combo_name, "rejected_evaluation")
          return(selection_worker_result(combo_name, rejected$selections[[1]], reused = TRUE))
        }
        series_data <- read_series_history(run_info, combo_name, run_log = prev_log_df)
        saved_average <- read_selection_file(run_info, "forecasts", "-average_models", combo_name, optional = TRUE)
        saved_rows <- dplyr::bind_rows(native_forecast_rows(all_model_tbl, date_type),
          if (average_models) native_forecast_rows(saved_average, date_type))
        existing_selection <- completed_forecast_selection(saved_rows, series_data, model_train_test_tbl)
        if (!is.null(existing_selection)) {
          return(selection_worker_result(combo_name, existing_selection, reused = TRUE))
        }
        if (nrow(saved_average)) {
          saved_average$Best_Model <- "No"
          write_data(saved_average, combo = combo_name, run_info = run_info,
            output_type = "data", folder = "forecasts", suffix = "-average_models")
        }
        single_model_tbl <- unfinalized_forecast_rows(single_model_tbl, date_type)
        ensemble_model_tbl <- unfinalized_forecast_rows(ensemble_model_tbl, date_type)
        global_model_tbl <- unfinalized_forecast_rows(global_model_tbl, date_type)
        local_model_tbl <- if (is.null(single_model_tbl) && is.null(ensemble_model_tbl)) {
          NULL
        } else dplyr::bind_rows(single_model_tbl, ensemble_model_tbl)
        all_model_tbl <- dplyr::bind_rows(local_model_tbl, global_model_tbl)

        # combine all forecasts
        predictions_tbl <- all_model_tbl %>%
          dplyr::select(Combo, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Date, Forecast, Target) %>%
          dplyr::filter(Train_Test_ID %in% train_test_id_list) %>%
          adjust_combo_column()

        # identify models with incomplete back test coverage. tune::fit_resamples
        # silently drops folds that error out (transient API failures from
        # foundation model endpoints, etc). a model with missing back test
        # folds would otherwise be evaluated on a smaller, often easier sample
        # than its competitors and could win Best_Model unfairly. flag these
        # models so downstream Best_Model selection excludes them, while still
        # keeping their partial rows in the output for visibility. only back
        # test Train_Test_ID values are considered here; future forecast output
        # is not part of back test fold coverage.
        back_test_id_list <- setdiff(train_test_id_list, 1)
        expected_back_test_fold_count <- length(back_test_id_list)

        if (expected_back_test_fold_count > 0) {
          model_back_test_coverage <- predictions_tbl %>%
            dplyr::filter(Train_Test_ID %in% back_test_id_list) %>%
            dplyr::group_by(Combo, Model_ID) %>%
            dplyr::summarise(
              fold_count = dplyr::n_distinct(Train_Test_ID),
              actual_ids = list(sort(unique(Train_Test_ID))),
              .groups = "drop"
            )

          partial_models_tbl <- model_back_test_coverage %>%
            dplyr::filter(fold_count < expected_back_test_fold_count) %>%
            dplyr::mutate(
              missing_ids = lapply(
                actual_ids,
                function(ids) sort(setdiff(back_test_id_list, ids))
              )
            )
        } else {
          partial_models_tbl <- data.frame(
            Combo = character(),
            Model_ID = character(),
            fold_count = integer(),
            actual_ids = I(list()),
            missing_ids = I(list()),
            stringsAsFactors = FALSE
          )
          model_back_test_coverage <- data.frame(
            Combo = character(),
            Model_ID = character(),
            fold_count = integer(),
            stringsAsFactors = FALSE
          )
        }

        if (nrow(partial_models_tbl) > 0) {
          for (i in seq_len(nrow(partial_models_tbl))) {
            partial_combo <- partial_models_tbl$Combo[i]
            partial_model_id <- partial_models_tbl$Model_ID[i]
            actual_ids <- partial_models_tbl$actual_ids[[i]]
            missing_ids <- partial_models_tbl$missing_ids[[i]]
            cli::cli_alert_warning(
              "Combo '{partial_combo}': model '{partial_model_id}' produced only {length(actual_ids)} of {expected_back_test_fold_count} back test folds (missing fold IDs: {paste(missing_ids, collapse = ', ')}). Excluding from Best_Model selection."
            )
          }
        }

        complete_model_ids <- if (expected_back_test_fold_count > 0) {
          model_back_test_coverage %>%
            dplyr::filter(fold_count == expected_back_test_fold_count) %>%
            dplyr::pull(Model_ID) %>%
            unique()
        } else {
          predictions_tbl %>%
            dplyr::pull(Model_ID) %>%
            unique()
        }

        # get model list
        if (!is.null(local_model_tbl)) {
          local_model_list <- local_model_tbl %>%
            dplyr::pull(Model_ID) %>%
            unique()
          local_model_list <- intersect(local_model_list, complete_model_ids)
          if (length(local_model_list) == 0) local_model_list <- NULL
        } else {
          local_model_list <- NULL
        }

        if (!is.null(global_model_tbl)) {
          global_model_list <- global_model_tbl %>%
            dplyr::pull(Model_ID) %>%
            unique()
          global_model_list <- intersect(global_model_list, complete_model_ids)
          if (length(global_model_list) == 0) global_model_list <- NULL
        } else {
          global_model_list <- NULL
        }

        final_model_list <- c(local_model_list, global_model_list)

        individual_selection <- select_series_forecasts(
          predictions_tbl, series_data, model_train_test_tbl,
          unique(predictions_tbl$Model_ID)
        )
        final_model_list <- individual_selection$rankings$Model_ID[
          individual_selection$rankings$Eligible
        ]
        if (length(final_model_list) == 0) {
          write_rejected_forecasts(
            list("-single_models" = single_model_tbl, "-ensemble_models" = ensemble_model_tbl,
              "-global_models" = global_model_tbl),
            run_info, unique(predictions_tbl$Combo), model_train_test_tbl, date_type, initial_weekly_to_daily
          )
          return(selection_worker_result(unique(predictions_tbl$Combo), individual_selection))
        }

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
                  Forecast = mean(Forecast)
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
          dplyr::mutate(MAPE = round(abs((Forecast - Target) / abs(Target)), digits = 4))

        # build allow-list of Model_IDs eligible to win Best_Model: individual
        # models with complete back test coverage plus any model averages
        # (averages_tbl was already constructed from complete models only).
        eligible_model_ids <- unique(c(
          final_model_list,
          if (!is.null(averages_tbl)) unique(averages_tbl$Model_ID) else character(0)
        ))

        selection <- select_series_forecasts(
          final_predictions_tbl, series_data, model_train_test_tbl, eligible_model_ids
        )
        if (is.na(selection$selected_id)) {
          abort_forecast_selection(unique(predictions_tbl$Combo), selection)
        }
        if (!is.null(averages_tbl)) {
          average_selection <- rank_forecast_candidates(selection$rankings[
            selection$rankings$Model_ID %in% averages_tbl$Model_ID, , drop = FALSE
          ])
          avg_best_model_tbl <- tibble::tibble(
            Combo = unique(predictions_tbl$Combo), Model_ID = average_selection$selected_id
          ) %>% dplyr::filter(!is.na(Model_ID))
        }
        selected_checks <- selection$rankings[selection$rankings$Model_ID == selection$selected_id, ]
        if (selected_checks$Violations > 0) {
          cli::cli_alert_warning("Selected forecast has plausibility concerns: {paste(selected_checks$Reasons[[1]], collapse = ', ')}")
        }
        best_model_mape <- selection$rankings %>%
          dplyr::filter(Model_ID == selection$selected_id) %>%
          dplyr::transmute(
            Combo = unique(predictions_tbl$Combo), Model_ID, Rolling_MAPE = .data$WMAPE
          )

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
            adjust_combo_column() %>%
            dplyr::group_by(Combo_ID, Model_ID, Train_Test_ID) %>%
            dplyr::mutate(Horizon = dplyr::row_number()) %>%
            dplyr::ungroup() %>%
            create_prediction_intervals(model_train_test_tbl) %>%
            convert_weekly_to_daily(date_type, initial_weekly_to_daily)

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
              adjust_combo_column() %>%
              dplyr::mutate(Best_Model = "No") %>%
              create_prediction_intervals(model_train_test_tbl) %>%
              convert_weekly_to_daily(date_type, initial_weekly_to_daily)

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
              adjust_combo_column() %>%
              dplyr::mutate(Best_Model = "No") %>%
              create_prediction_intervals(model_train_test_tbl) %>%
              convert_weekly_to_daily(date_type, initial_weekly_to_daily)

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
              adjust_combo_column() %>%
              dplyr::mutate(Best_Model = "No") %>%
              create_prediction_intervals(model_train_test_tbl) %>%
              convert_weekly_to_daily(date_type, initial_weekly_to_daily)

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
          # build allow-list of every Model_ID that appears in the per-model
          # outputs (complete + partial). complete models that win Best_Model
          # get "Yes"; everything else (including partial-coverage models)
          # gets "No". this mirrors the prior contract that every row in
          # *_models.csv has a non-NA Best_Model flag.
          all_single_model_ids <- if (!is.null(single_model_tbl)) {
            unique(single_model_tbl$Model_ID)
          } else {
            character(0)
          }
          all_ensemble_model_ids <- if (!is.null(ensemble_model_tbl)) {
            unique(ensemble_model_tbl$Model_ID)
          } else {
            character(0)
          }
          all_global_model_ids <- if (!is.null(global_model_tbl)) {
            unique(global_model_tbl$Model_ID)
          } else {
            character(0)
          }
          all_known_model_ids <- unique(c(
            final_model_list,
            all_single_model_ids,
            all_ensemble_model_ids,
            all_global_model_ids
          ))

          final_model_tbl <- tibble::tibble(Model_ID = all_known_model_ids) %>%
            dplyr::left_join(
              best_model_final_tbl %>%
                dplyr::select(Model_ID, Best_Model),
              by = "Model_ID"
            ) %>%
            dplyr::mutate(Best_Model = ifelse(!is.na(Best_Model), "Yes", "No"))

          if (!is.null(averages_tbl) && nrow(avg_best_model_tbl) > 0) {
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
              adjust_combo_column() %>%
              dplyr::group_by(Combo_ID, Model_ID, Train_Test_ID) %>%
              dplyr::mutate(Horizon = dplyr::row_number()) %>%
              dplyr::ungroup() %>%
              create_prediction_intervals(model_train_test_tbl) %>%
              convert_weekly_to_daily(date_type, initial_weekly_to_daily)

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
              adjust_combo_column() %>%
              remove_best_model() %>%
              dplyr::left_join(final_model_tbl,
                by = "Model_ID"
              ) %>%
              create_prediction_intervals(model_train_test_tbl) %>%
              convert_weekly_to_daily(date_type, initial_weekly_to_daily)

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
              adjust_combo_column() %>%
              remove_best_model() %>%
              dplyr::left_join(final_model_tbl,
                by = "Model_ID"
              ) %>%
              create_prediction_intervals(model_train_test_tbl) %>%
              convert_weekly_to_daily(date_type, initial_weekly_to_daily)

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
              adjust_combo_column() %>%
              remove_best_model() %>%
              dplyr::left_join(final_model_tbl,
                by = "Model_ID"
              ) %>%
              create_prediction_intervals(model_train_test_tbl) %>%
              convert_weekly_to_daily(date_type, initial_weekly_to_daily)

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

        return(selection_worker_result(unique(predictions_tbl$Combo), selection))
      } %>%
      base::suppressPackageStartupMessages()

    # clean up any parallel run process
    par_end(cl)
    selection_results <- stats::setNames(best_model_tbl$Selection, best_model_tbl$Combo)
    all_reused <- all(best_model_tbl$Reused)
    rejected_combos <- names(selection_results)[vapply(selection_results, function(result) {
      !is.null(result) && is.na(result$selected_id)
    }, logical(1))]

    # condense outputs into less files for larger runs
    if (length(combo_list) > 3000 && length(rejected_combos) == 0 && !all_reused) {
      cli::cli_progress_step("Condensing Forecasts")

      condense_data(
        run_info,
        parallel_processing,
        num_cores
      )
    }
  } # end combo processing

  if (length(rejected_combos)) {
    if (forecast_approach != "bottoms_up") {
      abort_forecast_selection(rejected_combos, selection_results[[rejected_combos[1]]])
    }
    if (!isTRUE(run_info$allow_quality_rejection)) {
      rejection <- selection_results[[rejected_combos[1]]]
      if (all(vapply(rejection$rankings$Reasons, function(reasons) "incomplete_backtests" %in% reasons, logical(1)))) {
        rejection$rankings$Reasons[[1]] <- c(rejection$rankings$Reasons[[1]], "no models produced complete back test coverage")
      }
      abort_forecast_selection(rejected_combos, rejection)
    }
    partial_log <- read_selection_file(run_info, "logs")
    partial_log$average_models <- average_models
    partial_log$max_model_average <- max_model_average
    partial_log$weekly_to_daily <- weekly_to_daily
    partial_log$weighted_mape <- NA_real_
    write_data(partial_log, combo = NULL, run_info = run_info, output_type = "log", folder = "logs", suffix = NULL)
    return(invisible(list(selections = selection_results, rejected_combos = rejected_combos)))
  }

  # reconcile hierarchical forecasts
  if (forecast_approach != "bottoms_up" && (!all_reused || !recon_complete)) {
    cli::cli_progress_step("Reconciling Hierarchical Forecasts")

    reconcile_hierarchical_data(
      run_info,
      parallel_processing,
      forecast_approach,
      negative_forecast,
      weekly_to_daily,
      date_type,
      num_cores
    )
  }

  # validate that every combo has a best model
  fcst_data <- get_forecast_data(run_info = run_info)

  # calculate weighted mape
  weighted_mape <- fcst_data %>%
    dplyr::filter(
      Run_Type == "Back_Test",
      Best_Model == "Yes"
    ) %>%
    dplyr::mutate(
      Target = ifelse(Target == 0, 0.1, Target)
    ) %>%
    dplyr::mutate(
      MAPE = round(abs((Forecast - Target) / abs(Target)), digits = 4),
      Total = sum(abs(Target), na.rm = TRUE),
      Weight = (MAPE * abs(Target)) / Total
    ) %>%
    dplyr::pull(Weight) %>%
    sum(na.rm = TRUE) %>%
    round(digits = 4)

  # update logging file
  log_df <- prev_log_df %>%
    dplyr::mutate(
      average_models = average_models,
      max_model_average = max_model_average,
      weekly_to_daily = weekly_to_daily,
      weighted_mape = round(weighted_mape, digits = 4)
    )

  if (!all_reused || !recon_complete || !isTRUE(as.numeric(prev_log_df[["weighted_mape"]]) == weighted_mape)) {
    write_data(
      x = log_df,
      combo = NULL,
      run_info = run_info,
      output_type = "log",
      folder = "logs",
      suffix = NULL
    )
  }
  if (all_reused) cli::cli_alert_info("Best Models Already Selected")
  result <- list(selections = selection_results, rejected_combos = rejected_combos)
  if (forecast_approach != "bottoms_up") {
    result <- hierarchical_selection_result(run_info, prev_log_df, selection_results,
      fcst_data, model_train_test_tbl)
  }
  invisible(result)
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

#' Validate that every combo has at least one Best_Model = "Yes"
#'
#' @param forecast_tbl data frame with Combo and Best_Model columns
#' @param context character string describing the calling function for error messages
#'
#' @return forecast_tbl invisibly if valid; otherwise stops with an error
#' @noRd
validate_best_model <- function(forecast_tbl, context = "forecast") {
  if (!"Best_Model" %in% colnames(forecast_tbl)) {
    stop(
      paste0("Error in ", context, "(). Best_Model column is missing from forecast data."),
      call. = FALSE
    )
  }

  combos_missing <- forecast_tbl %>%
    dplyr::group_by(Combo) %>%
    dplyr::summarise(has_best = any(Best_Model == "Yes", na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!has_best) %>%
    dplyr::pull(Combo)

  if (length(combos_missing) > 0) {
    stop(
      paste0(
        "Error in ", context, "(). The following combos are missing a best model (Best_Model == 'Yes'): ",
        paste(combos_missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(forecast_tbl)
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

#' Adjust Combo column if read in as logical
#'
#' @param input_data input data
#'
#' @return data frame with adjusted Combo column
#' @noRd
adjust_combo_column <- function(input_data) {
  # adjust Combo column if read in as logical, converting FALSE to F and TRUE to T
  if ("Combo" %in% names(input_data) && is.logical(input_data$Combo)) {
    input_data <- input_data %>%
      dplyr::mutate(Combo = ifelse(Combo, as.character("T"), as.character("F")))
  }
  return(input_data)
}
