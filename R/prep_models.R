#' Prep Models
#'
#' Preps various aspects of run before training models. Things like train/test
#'   splits, creating hyperparameters, etc.
#'
#' @details Experimental custom enrollment requires named `finnts_custom_model`
#'   envelopes holding an exact passive definition plus technical validation and
#'   approval bound to its content hash (`version_id`). Validation contains
#'   `version_id`, `technical_passed = TRUE`, and a nonempty list of check-summary
#'   strings. Manual approval contains the same `version_id`,
#'   `intent_confirmed = TRUE` and `allow_code = TRUE`. Explicit automatic approval
#'   has `intent_confirmed = FALSE`, `allow_code = TRUE` and `mode = "automatic"`.
#'   Unknown fields or modes fail. These are trusted-caller attestations, not
#'   authenticated human identity, a sandbox or an approval inferred from storage.
#'   Use [create_custom_model()] for manual or explicitly automatic authoring.
#'
#'   Supplied custom models run when `models_to_run` is NULL or names them; an
#'   explicit `models_to_run` that omits one is an error. Custom models train on
#'   the original-scale `C1` recipe, which must be in the `recipes_to_run` given
#'   to [prep_data()]; otherwise preparation errors. Built-in models selected in
#'   the same run use R1/R2 with any supported settings (stationary, Box-Cox,
#'   outlier cleaning, multistep, feature selection, ensembles). Built-in models
#'   on a C1-only run error, and a prepared C1 recipe without custom models is
#'   ignored with a warning. Custom forecasts can feed simple averages and
#'   ensembles; `negative_forecast = FALSE` floors custom output at zero like
#'   built-ins. Local/mounted storage, CSV/RDS/Parquet data and RDS model
#'   objects are required; sequential, local-machine and Spark execution and
#'   `inner_parallel` are supported. Custom recipes retain only Date, Combo and
#'   declared predictors; no implicit PCA, feature selection or target
#'   transformation is applied.
#'   C1 driver availability is independent of `external_regressors` in
#'   [prep_data()]. Required predictors must exist with complete supported
#'   values, including supplied future values, or preparation errors.
#'
#'   A definition with reviewed hierarchy capability permits `standard_hierarchy`
#'   or `grouped_hierarchy` with local univariate original-scale C1 logic. Every
#'   prepared node receives the same explicitly selected mixed custom/FinnTS
#'   pool. Custom-only hierarchy requests fail; no built-in is silently added.
#'   Forecast use requires local-only custom training, no regressors, and no
#'   weekly-to-daily expansion. Topology and exact input artifacts are pinned
#'   before enrollment.
#'
#'   Definitions are pinned in the normal prepared workflows, with passive
#'   attestations attached to that same artifact; no separate standard registry
#'   or pool-manifest copy is required. Reentry requires the same approved effective pool; changed
#'   definitions or preparation context require a new run. Supplying definitions
#'   without selecting them leaves built-in defaults unchanged. Registry presence
#'   alone grants neither enrollment nor execution consent. No source is evaluated
#'   during preparation and no packages are installed.
#'
#' @param run_info Run info using the [set_run_info()] function.
#' @param back_test_scenarios Number of specific back test folds to run when
#'   determining the best model. Default of NULL will automatically choose
#'   the number of back tests to run based on historical data size,
#'   which tries to always use a minimum of 80% of the data when training a model.
#' @param back_test_spacing Number of periods to move back for each back
#'   test scenario. Default of NULL moves back 1 period at a time for year,
#'   quarter, and month data. Moves back 4 for week and 7 for day data.
#' @param models_to_run List of models to run. Default of NULL runs all models.
#' @param models_not_to_run List of models not to run, overrides values in
#'   models_to_run. Default of NULL doesn't turn off any model.
#' @param run_ensemble_models If TRUE, prep for ensemble models.
#' @param pca If TRUE, run principle component analysis on any lagged features
#'   to speed up model run time. Default of NULL runs PCA on day and week
#'   date types across all local multivariate models, and also for global models
#'   across all date types.
#' @param num_hyperparameters Number of hyperparameter combinations to test
#'   out on validation data for model tuning.
#' @param seasonal_period One to three unique, finite numeric seasonal periods
#'   greater than 1. Custom values are used by `stlm-arima`, `stlm-ets`, and
#'   `tbats`. A value of NULL uses the defaults for `date_type`.
#' @param seed Set seed for random number generator. Numeric value.
#' @param custom_models Named list of experimental, version-bound approved
#'   `finnts_custom_model` envelopes. A NULL `models_to_run` runs all built-in
#'   models plus every supplied custom model; an explicit `models_to_run` must
#'   name every supplied custom model, otherwise an error is raised. Supplied
#'   custom models cannot also appear in `models_not_to_run`.
#'   Raw definitions or registry references do not grant execution permission.
#'
#' @return Writes outputs related to model prep to disk.
#'
#' @examples
#' \donttest{
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(
#'     Date >= "2012-01-01",
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
#'   models_to_run = c("arima", "ets", "glmnet")
#' )
#' }
#' @export
prep_models <- function(run_info,
                        back_test_scenarios = NULL,
                        back_test_spacing = NULL,
                        models_to_run = NULL,
                        models_not_to_run = NULL,
                        run_ensemble_models = TRUE,
                        pca = NULL,
                        num_hyperparameters = 10,
                        seasonal_period = NULL,
                        seed = 123,
                        custom_models = NULL) {
  # check input values
  check_input_type("run_info", run_info, "list")
  check_input_type("back_test_scenarios", back_test_scenarios, c("NULL", "numeric"))
  check_input_type("back_test_spacing", back_test_spacing, c("NULL", "numeric"))
  check_input_type("models_to_run", models_to_run, c("NULL", "list", "character"))
  check_input_type("models_not_to_run", models_not_to_run, c("NULL", "list", "character"))
  check_input_type("pca", pca, c("NULL", "logical"))
  check_input_type("num_hyperparameters", num_hyperparameters, "numeric")
  seasonal_period <- validate_seasonal_period(seasonal_period)
  check_input_type("seed", seed, "numeric")

  custom_pool <- custom_run_pool(models_to_run, models_not_to_run, custom_models)

  if (is.null(run_info$storage_object) && run_info$data_output %in% c("csv", "parquet", "rds")) {
    run_info$recipe_inventory <- new.env(parent = emptyenv())
  }

  # create model workflows
  model_workflows(
    run_info,
    models_to_run,
    models_not_to_run,
    pca,
    seasonal_period,
    seed,
    custom_pool
  )

  # create model hyperparameters
  model_hyperparameters(
    run_info,
    num_hyperparameters,
    seed
  )

  # create train test splits
  train_test_split(
    run_info,
    back_test_scenarios,
    back_test_spacing,
    run_ensemble_models
  )
}

#' Validate seasonal periods
#'
#' @param seasonal_period Seasonal periods supplied to [prep_models()].
#'
#' @return A numeric vector of seasonal periods, or NULL.
#' @noRd
validate_seasonal_period <- function(seasonal_period) {
  if (is.null(seasonal_period)) {
    return(NULL)
  }

  if (is.list(seasonal_period)) {
    numeric_scalars <- vapply(
      seasonal_period,
      function(value) is.numeric(value) && length(value) == 1,
      logical(1)
    )

    if (length(seasonal_period) == 0 || !all(numeric_scalars)) {
      stop(
        "'seasonal_period' must be a numeric vector or a list of ",
        "individual numeric values.",
        call. = FALSE
      )
    }

    seasonal_period <- unlist(seasonal_period, use.names = FALSE)
  }

  if (!is.numeric(seasonal_period)) {
    stop(
      "'seasonal_period' must be a numeric vector or a list of ",
      "individual numeric values.",
      call. = FALSE
    )
  }

  if (length(seasonal_period) < 1 || length(seasonal_period) > 3) {
    stop(
      "'seasonal_period' must contain between 1 and 3 values.",
      call. = FALSE
    )
  }

  seasonal_period <- as.numeric(seasonal_period)

  if (any(!is.finite(seasonal_period))) {
    stop(
      "'seasonal_period' values must be finite and cannot be missing.",
      call. = FALSE
    )
  }

  if (any(seasonal_period <= 1)) {
    stop("'seasonal_period' values must be greater than 1.", call. = FALSE)
  }

  if (anyDuplicated(seasonal_period)) {
    stop("'seasonal_period' values must be unique.", call. = FALSE)
  }

  seasonal_period
}

#' Gets the back testing spacing
#'
#' Checks if back_test_spacing is set to NULL and gets the right one
#'
#'
#' @param back_test_spacing back_test_spacing override
#' @param date_type year, quarter, month, week, day
#'
#' @return Returns back_test_spacing
#' @noRd
get_back_test_spacing <- function(back_test_spacing,
                                  date_type) {
  if (!is.null(back_test_spacing)) {
    return(back_test_spacing)
  }


  back_test_spacing <- switch(date_type,
    "day" = 7,
    "week" = 4,
    1
  )
  return(back_test_spacing)
}

#' Gets the back testing scenarios
#'
#' Gets back testing scenarios accounting for splits
#'
#' @param input_tbl full data table
#' @param hist_end_date historical end date
#' @param forecast_horizon forecast horizon
#' @param back_test_scenarios back test scenarios
#' @param back_test_spacing back test spacing
#'
#' @return Returns back_test_scenarios and hist_periods_80
#' @noRd
get_back_test_scenario_hist_periods <- function(input_tbl,
                                                hist_end_date,
                                                forecast_horizon,
                                                back_test_scenarios,
                                                back_test_spacing) {
  historical_periods <- input_tbl %>%
    dplyr::filter(Date <= hist_end_date) %>%
    dplyr::select(Date) %>%
    unique() %>%
    nrow() %>%
    as.numeric()

  hist_periods_80 <- floor(historical_periods * 0.8) # used with time series CV in multivariate models

  if (is.null(back_test_scenarios)) {
    historical_periods_20 <- floor(historical_periods * 0.2)

    # account for initial back tests that are smaller than the forecast horizon (1, 2, 3, etc up to fcst horizon)
    if (historical_periods_20 > forecast_horizon) {
      back_test_scenarios <- floor(historical_periods_20 / back_test_spacing)
    } else {
      back_test_scenarios <- floor(forecast_horizon / back_test_spacing)
    }
  }

  back_test_scenarios <- back_test_scenarios + 1

  return(list(
    hist_periods_80 = hist_periods_80,
    back_test_scenarios = back_test_scenarios
  ))
}

#' Gets the train test splits
#'
#' @param run_info run info using the 'set_run_info' function.
#' @param back_test_scenarios Number of specific back test folds to run when
#'   determining the best model. Default of NULL will automatically choose
#'   the number of back tests to run based on historical data size,
#'   which tries to always use a minimum of 80% of the data when training a model.
#' @param back_test_spacing Number of periods to move back for each back
#'   test scenario. Default of NULL moves back 1 period at a time for year,
#'   quarter, and month data. Moves back 4 for week and 7 for day data.
#' @param run_ensemble_models If TRUE, prep for ensemble models.
#' @param model_list List of models to run. Used in agent mode.
#' @param return If TRUE, returns the train test splits as a tibble
#'
#' @return Returns table of train test splits
#' @noRd
train_test_split <- function(run_info,
                             back_test_scenarios = NULL,
                             back_test_spacing = NULL,
                             run_ensemble_models = TRUE,
                             model_list = NULL,
                             return = FALSE) {
  cli::cli_progress_step("Creating Train Test Splits")

  # get inputs from previous functions
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  hist_end_date <- as.Date(log_df$hist_end_date)
  date_type <- log_df$date_type
  forecast_horizon <- as.numeric(log_df$forecast_horizon)
  date_type <- log_df$date_type

  if (is.null(run_ensemble_models) & date_type %in% c("day", "week")) {
    run_ensemble_models <- FALSE
  } else if (is.null(run_ensemble_models)) {
    run_ensemble_models <- TRUE
  } else {
    # do nothing
  }

  # adjust based on models planned to run
  if (is.null(model_list)) {
    model_workflow_list <- read_file(run_info,
      path = paste0(
        "/prep_models/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name),
        "-model_workflows.", run_info$object_output
      ),
      return_type = "df"
    ) %>%
      dplyr::pull(Model_Name) %>%
      unique()
  } else {
    model_workflow_list <- model_list
  }

  # models with hyperparameters to tune
  hyperparam_model_list <- list_hyperparmater_models()

  # ensemble models
  ensemble_model_list <- list_ensemble_models()

  if (sum(model_workflow_list %in% ensemble_model_list) == 0 & run_ensemble_models) {
    run_ensemble_models <- FALSE
    cli::cli_alert_info("Turning ensemble models off since no multivariate models were chosen to run.")
    cli::cli_progress_update()
  }

  # check if input values have changed
  if (sum(colnames(log_df) %in% c("back_test_scenarios", "back_test_spacing", "run_ensemble_models")) == 3) {
    current_log_df <- tibble::tibble(
      back_test_scenarios = ifelse(is.null(back_test_scenarios), NA, back_test_scenarios),
      back_test_spacing = ifelse(is.null(back_test_spacing), NA, back_test_spacing),
      run_ensemble_models = run_ensemble_models
    ) %>%
      data.frame()

    prev_log_df <- log_df %>%
      dplyr::select(colnames(current_log_df)) %>%
      data.frame()

    if (hash_data(normalize_log_df(current_log_df)) == hash_data(normalize_log_df(prev_log_df))) {
      cli::cli_alert_info("Train Test Splits Already Created")
      return(cli::cli_progress_done())
    } else {
      diff_details <- format_input_diff(prev_log_df, current_log_df)
      stop(
        "Inputs have recently changed in 'prep_models'.\n",
        "The following inputs differ from the previous run:\n",
        diff_details, "\n",
        "Please revert back to original inputs or start a ",
        "new run with 'set_run_info'.",
        call. = FALSE
      )
    }
  }

  # get back test info
  back_test_spacing_final <- get_back_test_spacing(
    back_test_spacing,
    date_type
  )

  # pull out first recipe data
  if ("combo" %in% names(run_info)) {
    # R recipes come first; a custom-only run uses C1 for date splits
    first_recipe <- logged_recipe_ids(log_df)[1]

    # get combo value from run info
    combo_value <- run_info$combo

    # construct file path directly
    file_name <- paste0(
      run_info$path, "/prep_data/", hash_data(run_info$project_name), "-",
      hash_data(run_info$run_name), "-", combo_value, "-", first_recipe, ".",
      run_info$data_output
    ) %>% fs::path_tidy()
  } else {
    file_name <- if (is.null(run_info$storage_object) &&
      run_info$data_output %in% c("csv", "parquet", "rds")) {
      preparation_recipe_files(run_info)[1]
    } else list_files(
      run_info$storage_object,
      paste0(
        run_info$path, "/prep_data/*", hash_data(run_info$project_name), "-",
        hash_data(run_info$run_name), "-*.", run_info$data_output
      )
    )[1]
  }

  source_path <- switch(class(run_info$storage_object)[[1]],
    "NULL" = gsub(fs::path(run_info$path), "", file_name),
    blob_container = gsub(fs::path(run_info$path), "", file_name),
    ms_drive = fs::path("/prep_data/", file_name)
  )

  # Fold planning needs dates only; arbitrary C1 predictors can otherwise
  # collide with timetk metadata or data-masked loop variables such as id.
  temp_tbl <- read_file(run_info,
    path = source_path,
    return_type = "df"
  ) %>%
    dplyr::select(Date)

  # get back test info
  bt <- temp_tbl %>%
    get_back_test_scenario_hist_periods(
      hist_end_date,
      forecast_horizon,
      back_test_scenarios,
      back_test_spacing_final
    )

  back_test_scenarios_final <- bt$back_test_scenarios
  back_test_initial <- bt$hist_periods_80

  # create train/test split info
  train_test_initial <- temp_tbl %>%
    timetk::time_series_cv(
      date_var = Date,
      initial = "1 year",
      assess = forecast_horizon,
      skip = back_test_spacing_final,
      cumulative = TRUE,
      slice_limit = 1000
    ) %>%
    timetk::tk_time_series_cv_plan() %>%
    tidyr::separate(col = .id, into = c(NA, "Slice_ID"), sep = "Slice")

  train_test_final <- tibble::tibble()

  for (id in unique(train_test_initial$Slice_ID)) {
    temp_tbl <- train_test_initial %>%
      dplyr::filter(Slice_ID == id)

    train_tbl <- temp_tbl %>%
      dplyr::filter(.key == "training") %>%
      dplyr::select(Date)

    if (as.numeric(id) == 1) {
      run_type <- "Future_Forecast"

      test_tbl <- temp_tbl %>%
        dplyr::filter(.key == "testing") %>%
        dplyr::select(Date)
    } else if (as.numeric(id) > 1 && as.numeric(id) <= back_test_scenarios_final) {
      run_type <- "Back_Test"

      test_tbl <- temp_tbl %>%
        dplyr::filter(Date <= hist_end_date)
    } else if (as.numeric(id) > back_test_scenarios_final & as.numeric(id) < max(back_test_scenarios_final + (forecast_horizon / back_test_spacing_final) + 1, back_test_scenarios_final * 1.5)) {
      run_type <- "Validation"

      back_test_date <- train_test_final %>%
        dplyr::filter(Run_Type == "Back_Test")

      test_tbl <- temp_tbl %>%
        dplyr::filter(Date <= min(back_test_date$Train_End))
    } else {
      run_type <- "Ensemble"

      test_tbl <- temp_tbl %>%
        dplyr::filter(.key == "testing") %>%
        dplyr::select(Date)
    }

    train_test_tbl <- tibble::tibble(
      Run_Type = run_type,
      Train_Test_ID = id,
      Train_End = max(train_tbl$Date),
      Test_End = max(test_tbl$Date)
    )

    train_test_final <- rbind(train_test_final, train_test_tbl) %>%
      dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID))
  }

  # check for back test and validation data
  if (!("Validation" %in% unique(train_test_final$Run_Type))) {
    stop("No validation data produced. Add more historical data, shorten the forecast horizon, or shorten the number of back test scenarios")
  } else if (!("Back_Test" %in% unique(train_test_final$Run_Type))) {
    stop("No back testing data produced. Shorten the forecast horizon, or shorten the number of back test scenarios or back test spacing")
  }

  # adjust based on models planned to run
  if (sum(model_workflow_list %in% hyperparam_model_list) == 0) {
    train_test_final <- train_test_final %>%
      dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test")) %>%
      rbind(
        train_test_final %>%
          dplyr::filter(Run_Type == "Validation") %>%
          dplyr::slice(1)
      )
  } else if (sum(model_workflow_list %in% hyperparam_model_list) > 0 & !run_ensemble_models) {
    train_test_final <- train_test_final %>%
      dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test", "Validation"))
  } else {
    # do nothing, keep the existing tbl
  }

  # write train test info
  write_data(
    x = train_test_final,
    combo = NULL,
    run_info = run_info,
    output_type = "data",
    folder = "prep_models",
    suffix = "-train_test_split"
  )

  # update logging file
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  ) %>%
    dplyr::mutate(
      back_test_scenarios = ifelse(is.null(back_test_scenarios), NA, back_test_scenarios),
      back_test_spacing = ifelse(is.null(back_test_spacing), NA, back_test_spacing),
      run_ensemble_models = run_ensemble_models
    )

  write_data(
    x = log_df,
    combo = NULL,
    run_info = run_info,
    output_type = "log",
    folder = "logs",
    suffix = NULL
  )

  if (return) {
    return(train_test_final)
  } else {
    cli::cli_progress_done()
  }
}

#' Locate one combo's prepared recipe files for workflow building
#'
#' Uses the run's own combo when set, otherwise discovers one sample combo
#' from a single listing of R recipe files (C1 files on a custom-only run).
#' Paths for every logged recipe are then derived from that sample path, so
#' no further listing is needed. C1 is returned only when `include_custom`.
#'
#' @param run_info Run info list.
#' @param log_df Run log row with `recipes_to_run` and `date_type`.
#' @param include_custom Logical; include the C1 recipe path.
#'
#' @return Tibble with Path and Recipe columns, one row per recipe.
#' @noRd
model_recipe_files <- function(run_info, log_df, include_custom = FALSE) {
  recipes <- logged_recipe_ids(log_df)
  if (!include_custom) {
    recipes <- setdiff(recipes, "C1")
  }
  extension <- run_info$data_output
  if ("combo" %in% names(run_info)) {
    sample_path <- paste0(
      run_info$path, "/prep_data/", hash_data(run_info$project_name), "-",
      hash_data(run_info$run_name), "-", run_info$combo, "-", recipes[[1]], ".", extension
    ) %>% fs::path_tidy()
  } else {
    sample_files <- if (is.null(run_info$storage_object) &&
      extension %in% c("csv", "parquet", "rds")) {
      preparation_recipe_files(run_info)
    } else {
      prefix <- paste0(
        run_info$path, "/prep_data/*", hash_data(run_info$project_name), "-",
        hash_data(run_info$run_name)
      )
      files <- list_files(run_info$storage_object, paste0(prefix, "*R*.", extension))
      if (!length(files)) {
        files <- list_files(run_info$storage_object, paste0(prefix, "-*-C1.", extension))
      }
      files
    }
    sample_files <- as.character(unlist(sample_files))
    if (!length(sample_files)) {
      stop("No prepared recipe files were found; run prep_data first.", call. = FALSE)
    }
    sample_path <- sort(sample_files, method = "radix")[[1]]
  }
  sample_recipe <- sub(".*-([^-.]+)\\.[^.]+$", "\\1", fs::path_file(sample_path))
  pattern <- paste0("-", sample_recipe, ".", extension, "$")
  tibble::tibble(
    Path = vapply(recipes, function(recipe) {
      sub(pattern, paste0("-", recipe, ".", extension), sample_path)
    }, character(1), USE.NAMES = FALSE),
    Recipe = recipes
  )
}

#' Gets model workflows
#'
#' @param run_info run info
#' @param models_to_run models to run
#' @param models_not_to_run models not to run
#' @param pca pca
#' @param seasonal_period seasonal period
#' @param seed Set seed for random number generator. Numeric value.
#' @param custom_pool Internal resolved enrollment, or NULL for built-in runs.
#'
#' @details Custom models get workflows only on C1; built-in models only on
#'   R1/R2. Errors when built-ins are selected without an R recipe; warns when
#'   C1 was prepared without custom models.
#'
#' @return Writes the model workflow table and updated run log.
#' @noRd
model_workflows <- function(run_info,
                            models_to_run = NULL,
                            models_not_to_run = NULL,
                            pca = NULL,
                            seasonal_period = NULL,
                            seed = 123,
                            custom_pool = NULL) {
  cli::cli_progress_step("Creating Model Workflows")

  set.seed(seed)

  # get inputs
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )
  custom_run <- custom_run_prepare(run_info, custom_pool, log_df)

  date_type <- log_df$date_type
  forecast_approach <- log_df$forecast_approach
  forecast_horizon <- log_df$forecast_horizon
  multistep_horizon <- log_df$multistep_horizon
  external_regressors <- if (is.na(log_df$external_regressors)) {
    NULL
  } else {
    unlist(strsplit(log_df$external_regressors, split = "---"))
  }
  lag_periods <- if (!"lag_periods" %in% colnames(log_df) ||
    length(log_df$lag_periods) == 0 || is.na(log_df$lag_periods[[1]])) {
    NULL
  } else {
    as.numeric(unlist(strsplit(as.character(log_df$lag_periods[[1]]), split = "---")))
  }

  if (is.null(pca) & date_type %in% c("day", "week")) {
    pca <- TRUE
  } else if (is.null(pca)) {
    pca <- FALSE
  } else {
    # do nothing
  }

  model_seasonal_period <- if (is.null(seasonal_period)) {
    get_seasonal_periods(date_type)
  } else {
    seasonal_period
  }

  # check if input values have changed
  if (sum(colnames(log_df) %in% c("models_to_run", "models_not_to_run", "pca")) == 3) {
    current_log_df <- tibble::tibble(
      models_to_run = ifelse(is.null(models_to_run), NA, paste(models_to_run, collapse = "---")),
      models_not_to_run = ifelse(is.null(models_not_to_run), NA, paste(models_not_to_run, collapse = "---")),
      pca = ifelse(is.null(pca), NA, pca),
      seasonal_period = ifelse(is.null(seasonal_period), NA, paste(seasonal_period, collapse = "---"))
    ) %>%
      data.frame()

    prev_log_df <- log_df %>%
      dplyr::select(colnames(current_log_df)) %>%
      dplyr::mutate(seasonal_period = ifelse(is.numeric(seasonal_period), as.character(seasonal_period), seasonal_period)) %>%
      data.frame()

    if (!is.null(custom_run)) {
      current_log_df$models_to_run <- prev_log_df$models_to_run <- paste(custom_run$pool$selected, collapse = "---")
      current_log_df$models_not_to_run <- prev_log_df$models_not_to_run <- NA_character_
      custom_run_check_workflows(read_exact_artifact(run_info,
        local_artifact_path(run_info, "prep_models", "-model_workflows", extension = "rds"),
        return_type = "object"), custom_run)
    }
    if (hash_data(normalize_log_df(current_log_df)) == hash_data(normalize_log_df(prev_log_df))) {
      cli::cli_alert_info("Model Workflows Already Created")
      return(cli::cli_progress_done())
    } else {
      diff_details <- format_input_diff(prev_log_df, current_log_df)
      stop(
        "Inputs have recently changed in 'prep_models'.\n",
        "The following inputs differ from the previous run:\n",
        diff_details, "\n",
        "Please revert back to original inputs or start a ",
        "new run with 'set_run_info'.",
        call. = FALSE
      )
    }
  }

  # pull out recipe data for a single combo
  input_tbl <- tibble::tibble()

  file_name_tbl <- model_recipe_files(run_info, log_df, include_custom = !is.null(custom_run))
  logged_recipes <- logged_recipe_ids(log_df)
  file_name_tbl <- file_name_tbl[file_name_tbl$Recipe != "C1" | !is.null(custom_run), , drop = FALSE]
  custom_names <- if (is.null(custom_run)) character() else names(custom_run$pool$custom)

  for (recipe in file_name_tbl$Recipe) {
    temp_path <- file_name_tbl %>%
      dplyr::filter(Recipe == recipe) %>%
      dplyr::pull(Path)

    source_path <- switch(class(run_info$storage_object)[[1]],
      "NULL" = gsub(fs::path(run_info$path), "", temp_path),
      blob_container = gsub(fs::path(run_info$path), "", temp_path),
      ms_drive = fs::path("/prep_data/", temp_path)
    )

    temp_file_tbl <- read_file(run_info,
      path = source_path,
      return_type = "df"
    )

    temp_final_tbl <- tibble::tibble(
      Recipe = recipe,
      Data = list(temp_file_tbl)
    )

    input_tbl <- rbind(input_tbl, temp_final_tbl)
  }

  # tibble to add model workflows to
  model_workflow_tbl <- tibble::tibble()

  # models to run
  ml_models <- list_models()

  if (!is.null(custom_run)) {
    ml_models <- custom_run$pool$selected
  } else if (is.null(models_to_run) & is.null(models_not_to_run)) {
    # do nothing, using existing ml_models list
  } else if (is.null(models_to_run) & !is.null(models_not_to_run)) {
    ml_models <- setdiff(ml_models, models_not_to_run)
  } else {
    if (!is.null(models_not_to_run)) {
      cli::cli_alert_warning("Note: 'models_to_run' argument overrides the 'models_not_to_run' argument")
    }

    if (forecast_approach != "bottoms_up") {
      # add snaive model to help fix hierarchical forecast reconciliation issues
      ml_models <- unique(c(models_to_run, "snaive"))
    } else {
      ml_models <- models_to_run
    }
  }

  r2_models <- list_r2_models()

  builtin_models <- setdiff(ml_models, custom_names)
  check_builtin_recipes(builtin_models, file_name_tbl$Recipe)
  if (is.null(custom_run) && "C1" %in% logged_recipes) {
    warning("Recipe C1 was prepared but no custom models were selected; C1 is ignored.", call. = FALSE)
  }

  iter_tbl <- tibble::tibble()

  for (recipe in unique(input_tbl$Recipe)) {
    recipe_models <- if (recipe == "C1") intersect(ml_models, custom_names) else builtin_models
    iter_tbl <- rbind(
      iter_tbl,
      tibble::tibble(Model = recipe_models, Recipe = rep(recipe, length(recipe_models)))
    )
  }

  for (x in iter_tbl %>% dplyr::group_split(dplyr::row_number(), .keep = FALSE)) {
    # get initial data
    model <- x %>%
      dplyr::pull(Model)

    recipe <- x %>%
      dplyr::pull(Recipe)

    recipe_tbl <- input_tbl %>%
      dplyr::filter(Recipe == recipe) %>%
      dplyr::select(Data) %>%
      tidyr::unnest(Data)

    # adjust data if outliers have been cleaned
    if ("Target_Original" %in% colnames(recipe_tbl)) {
      recipe_tbl <- recipe_tbl %>%
        dplyr::select(-Target_Original)
    }

    if (!is.null(custom_run) && model %in% names(custom_run$pool$custom)) {
      workflow_tbl <- custom_run_workflows(custom_run$pool$custom[[model]], recipe_tbl,
        custom_run$manifest$context)
      model_workflow_tbl <- dplyr::bind_rows(model_workflow_tbl, workflow_tbl)
      next
    }

    # get args to feed into model spec functions
    if (recipe == "R1") {
      avail_arg_list <- list(
        "train_data" = recipe_tbl,
        "frequency" = get_frequency_number(date_type),
        "horizon" = forecast_horizon,
        "seasonal_period" = model_seasonal_period,
        "model_type" = "single",
        "pca" = pca,
        "multistep" = multistep_horizon,
        "external_regressors" = external_regressors,
        "lag_periods" = lag_periods
      )
    } else {
      avail_arg_list <- list(
        "train_data" = recipe_tbl,
        "frequency" = get_frequency_number(date_type),
        "horizon" = forecast_horizon,
        "seasonal_period" = model_seasonal_period,
        "model_type" = "single",
        "pca" = pca,
        "multistep" = FALSE,
        "external_regressors" = external_regressors
      )
    }

    # don't create workflows for models that only use R1 recipe
    if (recipe == "R2" & !(model %in% r2_models)) {
      next
    }

    # get specific model spec
    fn_to_invoke <- get(gsub("-", "_", model))

    exp_arg_list <- formalArgs(fn_to_invoke)

    avail_names <- names(avail_arg_list)

    inp_arg_list <- list()

    for (x in avail_names) {
      if (x %in% exp_arg_list) {
        inp_arg_list[x] <- avail_arg_list[x]
      }
    }

    model_workflow <- do.call(fn_to_invoke, inp_arg_list, quote = TRUE)

    workflow_tbl <- tibble::tibble(
      Model_Name = model,
      Model_Recipe = recipe,
      Model_Workflow = list(model_workflow)
    )

    model_workflow_tbl <- dplyr::bind_rows(model_workflow_tbl, workflow_tbl)
  }

  model_workflow_tbl <- model_workflow_tbl %>% dplyr::arrange(Model_Name)
  if (!is.null(custom_run))
    attr(model_workflow_tbl, "finnts_custom_manifest") <- custom_run$manifest

  # write model workflow info
  write_data(
    x = model_workflow_tbl,
    combo = NULL,
    run_info = run_info,
    output_type = "object",
    folder = "prep_models",
    suffix = "-model_workflows"
  )
  if (!is.null(custom_run) &&
      !identical(custom_run_read(run_info, required = TRUE)$manifest$manifest_id, custom_run$manifest$manifest_id))
    stop("Custom manifest read-back failed.", call. = FALSE)

  # update logging file
  if (!is.null(custom_run)) {
    log_df$custom_pool_id <- custom_run$manifest$pool_id
    log_df$custom_manifest_id <- custom_run$manifest$manifest_id
  }
  log_df <- log_df %>%
    dplyr::mutate(
      models_to_run = ifelse(is.null(models_to_run), NA, paste(models_to_run, collapse = "---")),
      models_not_to_run = ifelse(is.null(models_not_to_run), NA, paste(models_not_to_run, collapse = "---")),
      pca = ifelse(is.null(pca), NA, pca),
      seasonal_period = ifelse(is.null(seasonal_period), NA, paste(seasonal_period, collapse = "---"))
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

#' Get model hyperparameters
#'
#' @param run_info run info
#' @param num_hyperparameters number of hyperparameter combinations
#' @param seed Set seed for random number generator. Numeric value.
#'
#' @return table of model hyperparameters
#' @noRd
model_hyperparameters <- function(run_info,
                                  num_hyperparameters = 10,
                                  seed = 123) {
  cli::cli_progress_step("Creating Model Hyperparameters")

  # check if input values have changed
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )
  date_type <- log_df$date_type

  if (sum(colnames(log_df) %in% c("num_hyperparameters")) == 1) {
    current_log_df <- tibble::tibble(
      num_hyperparameters = num_hyperparameters
    ) %>%
      data.frame()

    prev_log_df <- log_df %>%
      dplyr::select(colnames(current_log_df)) %>%
      data.frame()

    if (hash_data(normalize_log_df(current_log_df)) == hash_data(normalize_log_df(prev_log_df))) {
      cli::cli_alert_info("Model Hyperparameters Already Created")
      return(cli::cli_progress_done())
    } else {
      diff_details <- format_input_diff(prev_log_df, current_log_df)
      stop(
        "Inputs have recently changed in 'prep_models'.\n",
        "The following inputs differ from the previous run:\n",
        diff_details, "\n",
        "Please revert back to original inputs or start a ",
        "new run with 'set_run_info'.",
        call. = FALSE
      )
    }
  }

  # get model workflow info
  model_workflow_tbl <- read_file(run_info,
    path = paste0(
      "/prep_models/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name),
      "-model_workflows.", run_info$object_output
    ),
    return_type = "df"
  )

  # get recipe input data
  input_tbl <- tibble::tibble()

  file_name_tbl <- model_recipe_files(run_info, log_df,
    include_custom = "C1" %in% model_workflow_tbl$Model_Recipe
  )
  file_name_tbl <- file_name_tbl[file_name_tbl$Recipe %in% model_workflow_tbl$Model_Recipe, , drop = FALSE]

  for (recipe in file_name_tbl$Recipe) {
    temp_path <- file_name_tbl %>%
      dplyr::filter(Recipe == recipe) %>%
      dplyr::pull(Path)

    source_path <- switch(class(run_info$storage_object)[[1]],
      "NULL" = gsub(fs::path(run_info$path), "", temp_path),
      blob_container = gsub(fs::path(run_info$path), "", temp_path),
      ms_drive = fs::path("/prep_data/", temp_path)
    )

    temp_file_tbl <- read_file(run_info,
      path = source_path,
      return_type = "df"
    )

    temp_final_tbl <- tibble::tibble(
      Recipe = recipe,
      Data = list(temp_file_tbl)
    )

    input_tbl <- rbind(input_tbl, temp_final_tbl)
  }

  iter_tbl <- model_workflow_tbl %>%
    dplyr::select(Model_Name, Model_Recipe) %>%
    dplyr::distinct()

  # get hyperparameters
  hyperparameters_tbl <- tibble::tibble()

  for (x in iter_tbl %>% dplyr::group_split(dplyr::row_number(), .keep = FALSE)) {
    model <- x %>%
      dplyr::pull(Model_Name)

    recipe <- x %>%
      dplyr::pull(Model_Recipe)

    temp_tbl <- model_workflow_tbl %>%
      dplyr::filter(
        Model_Name == model,
        Model_Recipe == recipe
      )

    model_workflow <- temp_tbl$Model_Workflow[[1]]

    model_spec <- model_workflow %>%
      workflows::extract_spec_parsnip()

    recipe_features <- input_tbl %>%
      dplyr::filter(Recipe == recipe) %>%
      dplyr::select(Data) %>%
      tidyr::unnest(Data)

    # adjust data if outliers have been cleaned
    if ("Target_Original" %in% names(recipe_features)) {
      recipe_features <- recipe_features %>%
        dplyr::select(-Target_Original)
    }

    if (workflows::extract_parameter_set_dials(model_spec) %>% nrow() > 0) {
      if (model == "svm-rbf") {
        parameters <- model_spec %>%
          workflows::extract_parameter_set_dials()
      } else {
        parameters <- model_spec %>%
          workflows::extract_parameter_set_dials() %>%
          dials::finalize(recipe_features, force = FALSE)
      }

      if (model == "mars") {
        parameters <- stats::update(
          parameters,
          prune_method = mars_automatic_prune_method()
        )
      }

      set.seed(seed)

      grid <- suppressWarnings(
        dials::grid_space_filling(parameters, size = num_hyperparameters)
      )

      hyperparameters_temp <- grid %>%
        dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
        purrr::map_df(tidyr::nest, data = tidyselect::everything()) %>%
        dplyr::rename(Hyperparameters = data) %>%
        tibble::rowid_to_column("Hyperparameter_Combo") %>%
        dplyr::mutate(
          Model = model,
          Recipe = recipe
        )
    } else {
      hyperparameters_temp <- tibble::tibble(
        Hyperparameter_Combo = 1,
        Hyperparameters = list(tibble::tibble()),
        Model = model,
        Recipe = recipe
      )
    }

    hyperparameters_tbl <- rbind(hyperparameters_tbl, hyperparameters_temp)
  }

  # write model hyperparameter info
  write_data(
    x = hyperparameters_tbl %>% dplyr::select(Model, Recipe, Hyperparameter_Combo, Hyperparameters),
    combo = NULL,
    run_info = run_info,
    output_type = "object",
    folder = "prep_models",
    suffix = "-model_hyperparameters"
  )

  # update logging file
  log_df <- log_df %>%
    dplyr::mutate(
      num_hyperparameters = num_hyperparameters
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

#' Gets the right frequency numbers
#'
#' @param date_type year, quarter, month, week, day
#'
#' @return Returns frequency_number
#' @noRd
get_frequency_number <- function(date_type) {
  frequency_number <- switch(date_type,
    "year" = 1,
    "quarter" = 4,
    "month" = 12,
    "week" = 52.17857, # 365.25 / 7
    "day" = 365.25
  )

  return(frequency_number)
}

#' Gets the right date type
#'
#' @param frequency number
#'
#' @return Returns date_type
#' @noRd
get_date_type <- function(frequency) {
  date_type <- switch(as.character(frequency),
    "1" = "year",
    "4" = "quarter",
    "12" = "month",
    "52.17857" = "week",
    "365.25" = "day"
  )

  return(date_type)
}

#' Gets the seasonal periods
#'
#' @param date_type year, quarter, month, week, day
#'
#' @return Returns seasonal_periods
#' @noRd
get_seasonal_periods <- function(date_type) {
  seasonal_periods <- switch(date_type,
    "year" = c(2, 3),
    "quarter" = c(4, 2, 8),
    "month" = c(12, 6, 3),
    "week" = c(365.25 / 7, (365.25 / 7) / 4, (365.25 / 7) / 12),
    "day" = c(365.25, 365.25 / 4, 365.25 / 12)
  )

  return(seasonal_periods)
}
