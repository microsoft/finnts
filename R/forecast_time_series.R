#' Finn Forecast Framework
#'
#' Calls the Finn forecast framework to automatically forecast any historical time series.
#'
#' @param run_info Run info using [set_run_info()]
#' @param input_data A data frame or tibble of historical time series data. Can also include external regressors for both
#'   historical and future data.
#' @param combo_variables List of column headers within input data to be used to separate individual time series.
#' @param target_variable The column header formatted as a character value within input data you want to forecast.
#' @param date_type The date granularity of the input data. Finn accepts the following as a character string
#'   day, week, month, quarter, year.
#' @param forecast_horizon Number of periods to forecast into the future.
#' @param external_regressors List of column headers within input data to be used as features in multivariate models.
#' @param run_name Name used when submitting jobs to external compute like Azure Batch. Formatted as a character string.
#' @param hist_start_date Date value of when your input_data starts. Default of NULL is to use earliest date value in
#'   input_data.
#' @param hist_end_date Date value of when your input_data ends.Default of NULL is to use the latest date value in
#'   input_data.
#' @param combo_cleanup_date Date value to remove individual time series that don't contain non-zero values after
#'   that specified date. Default of NULL is to not remove any time series and attempt to forecast all of them.
#' @param fiscal_year_start Month number of start of fiscal year of input data, aids in building out date features.
#'   Formatted as a numeric value. Default of 1 assumes fiscal year starts in January.
#' @param clean_missing_values If TRUE, cleans missing values. Only impute values for missing data within an
#'   existing series, and does not add new values onto the beginning or end, but does provide a value of 0 for said
#'   values. Turned off when running hierarchical forecasts.
#' @param clean_outliers If TRUE, outliers are cleaned and inputted with values more in line with historical data
#' @param back_test_scenarios Number of specific back test folds to run when determining the best model.
#'   Default of NULL will automatically choose the number of back tests to run based on historical data size,
#'   which tries to always use a minimum of 80% of the data when training a model.
#' @param back_test_spacing Number of periods to move back for each back test scenario. Default of NULL moves back 1
#'   period at a time for year, quarter, and month data. Moves back 4 for week and 7 for day data.
#' @param modeling_approach How Finn should approach your data. Current default and only option is 'accuracy'. In the
#'   future this could evolve to other areas like optimizing for interpretability over accuracy.
#' @param forecast_approach How the forecast is created. The default of 'bottoms_up' trains models for each individual
#'   time series. 'grouped_hierarchy' creates a grouped time series to forecast at while 'standard_hierarchy' creates
#'   a more traditional hierarchical time series to forecast, both based on the hts package.
#' @param parallel_processing Default of NULL runs no parallel processing and
#'   forecasts each individual time series one after another. 'local_machine'
#'   leverages all cores on current machine Finn is running on. 'spark'
#'   runs time series in parallel on a spark cluster in Azure Databricks or
#'   Azure Synapse.
#' @param inner_parallel Run components of forecast process inside a specific
#'   time series in parallel. Can only be used if parallel_processing is
#'   set to NULL or 'spark'.
#' @param num_cores Number of cores to run when parallel processing is set up. Used when running parallel computations
#'   on local machine or within Azure. Default of NULL uses total amount of cores on machine minus one. Can't be greater
#'   than number of cores on machine minus 1.
#' @param target_log_transformation If TRUE, log transform target variable before training models.
#' @param negative_forecast If TRUE, allow forecasts to dip below zero.
#' @param fourier_periods List of values to use in creating fourier series as features. Default of NULL automatically chooses
#'   these values based on the date_type.
#' @param lag_periods List of values to use in creating lag features. Default of NULL automatically chooses these values
#'   based on date_type.
#' @param rolling_window_periods List of values to use in creating rolling window features. Default of NULL automatically
#'   chooses these values based on date type.
#' @param recipes_to_run List of recipes to run on multivariate models that can run different recipes. A value of NULL runs
#'   all recipes, but only runs the R1 recipe for weekly and daily date types, and also for global models to prevent memory issues.
#'   A value of "all" runs all recipes, regardless of date type or if it's a local/global model. A list like c("R1") or c("R2")
#'   would only run models with the R1 or R2 recipe.
#' @param pca If TRUE, run principle component analysis on any lagged features to speed up model run time. Default of NULL runs
#'   PCA on day and week date types across all local multivariate models, and also for global models across all date types.
#' @param models_to_run List of models to run. Default of NULL runs all models.
#' @param models_not_to_run List of models not to run, overrides values in models_to_run. Default of NULL doesn't turn off
#'   any model.
#' @param run_global_models If TRUE, run multivariate models on the entire data set (across all time series) as a global model.
#'   Can be override by models_not_to_run. Default of NULL runs global models for all date types except week and day.
#' @param run_local_models If TRUE, run models by individual time series as local models.
#' @param run_ensemble_models If TRUE, run ensemble models. Default of NULL runs ensemble models only for quarter and month
#'   date types.
#' @param average_models If TRUE, create simple averages of individual models.
#' @param max_model_average Max number of models to average together. Will create model averages for 2 models up until input value
#'   or max number of models ran.
#' @param feature_selection Implement feature selection before model training
#' @param weekly_to_daily If TRUE, convert a week forecast down to day by evenly splitting across each day of week. Helps when aggregating
#'   up to higher temporal levels like month or quarter.
#' @param seed Set seed for random number generator. Numeric value.
#' @param run_model_parallel If TRUE, runs model training in parallel, only works when parallel_processing is set to
#'   'local_machine' or 'spark'. Recommended to use a value of FALSE and leverage
#'   inner_parallel for new features.
#' @param return_data If TRUE, return the forecast results. Used to be backwards compatible
#'   with previous finnts versions. Recommended to use a value of FALSE and leverage
#'   [get_forecast_data()] for new features.
#'
#' @return A list of three separate data sets: the future forecast, the back test results, and the best model per time series.
#'
#' @examples
#' \donttest{
#'
#' run_info <- set_run_info()
#'
#' finn_forecast <- forecast_time_series(
#'   run_info = run_info,
#'   input_data = m750 %>% dplyr::rename(Date = date),
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month",
#'   forecast_horizon = 3,
#'   back_test_scenarios = 6,
#'   run_model_parallel = FALSE,
#'   models_to_run = c("arima", "ets", "snaive"),
#'   return_data = FALSE
#' )
#'
#' fcst_tbl <- get_forecast_data(run_info)
#'
#' models_tbl <- get_trained_models(run_info)
#' }
#' @export
forecast_time_series <- function(run_info = NULL,
                                 input_data,
                                 combo_variables,
                                 target_variable,
                                 date_type,
                                 forecast_horizon,
                                 external_regressors = NULL,
                                 hist_start_date = NULL,
                                 hist_end_date = NULL,
                                 combo_cleanup_date = NULL,
                                 fiscal_year_start = 1,
                                 clean_missing_values = TRUE,
                                 clean_outliers = FALSE,
                                 back_test_scenarios = NULL,
                                 back_test_spacing = NULL,
                                 modeling_approach = "accuracy",
                                 forecast_approach = "bottoms_up",
                                 parallel_processing = NULL,
                                 inner_parallel = FALSE,
                                 num_cores = NULL,
                                 target_log_transformation = FALSE,
                                 negative_forecast = FALSE,
                                 fourier_periods = NULL,
                                 lag_periods = NULL,
                                 rolling_window_periods = NULL,
                                 recipes_to_run = NULL,
                                 pca = NULL,
                                 models_to_run = NULL,
                                 models_not_to_run = NULL,
                                 run_global_models = NULL,
                                 run_local_models = TRUE,
                                 run_ensemble_models = NULL,
                                 average_models = TRUE,
                                 max_model_average = 3,
                                 feature_selection = FALSE,
                                 weekly_to_daily = TRUE,
                                 seed = 123,
                                 run_model_parallel = FALSE,
                                 return_data = TRUE,
                                 run_name = "finnts_forecast") {
  if (is.null(run_info)) {
    run_info <- set_run_info()
  }

  if (run_model_parallel) {
    inner_parallel <- TRUE
    cli::cli_alert_warning("run_model_parallel is deprecated, please use inner_parallel argument instead")
  }

  prep_data(
    run_info = run_info,
    input_data = input_data,
    combo_variables = combo_variables,
    target_variable = target_variable,
    date_type = date_type,
    forecast_horizon = forecast_horizon,
    external_regressors = external_regressors,
    hist_start_date = hist_start_date,
    hist_end_date = hist_end_date,
    combo_cleanup_date = combo_cleanup_date,
    fiscal_year_start = fiscal_year_start,
    clean_missing_values = clean_missing_values,
    clean_outliers = clean_outliers,
    forecast_approach = forecast_approach,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    target_log_transformation = target_log_transformation,
    fourier_periods = fourier_periods,
    lag_periods = lag_periods,
    rolling_window_periods = rolling_window_periods,
    recipes_to_run = recipes_to_run
  )

  prep_models(
    run_info = run_info,
    back_test_scenarios = back_test_scenarios,
    back_test_spacing = back_test_spacing,
    models_to_run = models_to_run,
    models_not_to_run = models_not_to_run,
    run_ensemble_models = run_ensemble_models,
    pca = pca,
    num_hyperparameters = 10,
    seed = seed
  )

  train_models(
    run_info = run_info,
    run_global_models = run_global_models,
    run_local_models = run_local_models,
    global_model_recipes = "R1",
    feature_selection = feature_selection,
    negative_forecast = negative_forecast,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores,
    seed = seed
  )

  ensemble_models(
    run_info = run_info,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores,
    seed = seed
  )

  final_models(
    run_info = run_info,
    average_models = average_models,
    max_model_average = max_model_average,
    weekly_to_daily = weekly_to_daily,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores
  )

  if (return_data) {
    fcst_results <- forecast_backwards_compatibility(run_info, input_data)

    return(fcst_results)
  }
}

#' Get return data from forecast_time_series
#'
#' Keeps backwards compatibility with previous finnts versions
#'
#' @param run_info Run info using [set_run_info()]
#' @param hist_tbl historical data given to [forecast_time_series()]
#'
#' @return Returns list with forecast results
#' @noRd
forecast_backwards_compatibility <- function(run_info,
                                             hist_tbl) {
  cli::cli_alert_warning("return_data is deprecated, please use 'get_forecast_data()' to get finnts results")

  # initial forecast results
  initial_fcst_tbl <- get_forecast_data(run_info)

  # get input values
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  combo_variables <- strsplit(log_df$combo_variables, split = "---")[[1]]
  target_variable <- log_df$target_variable
  forecast_approach <- log_df$forecast_approach

  # future forecast results
  future_forecast_tbl <- initial_fcst_tbl %>%
    dplyr::filter(Run_Type == "Future_Forecast") %>%
    dplyr::mutate(Model_ID = ifelse(Best_Model == "Yes", "Best-Model", Model_ID)) %>%
    dplyr::select(Combo, combo_variables, Model_ID, Date, Forecast, lo_95, lo_80, hi_80, hi_95) %>%
    dplyr::rename(Target = Forecast) %>%
    rbind(
      hist_tbl %>%
        dplyr::collect() %>%
        tidyr::unite(
          col = "Combo",
          tidyselect::all_of(combo_variables),
          sep = "---",
          remove = FALSE
        ) %>%
        dplyr::rename(Target = tidyselect::all_of(target_variable)) %>%
        dplyr::mutate(
          Model_ID = NA,
          lo_95 = Target,
          lo_80 = Target,
          hi_80 = Target,
          hi_95 = Target
        ) %>%
        dplyr::select(Combo, tidyselect::all_of(combo_variables), Model_ID, Date, Target, lo_95, lo_80, hi_80, hi_95)
    ) %>%
    dplyr::mutate(Type = ifelse(is.na(Model_ID), "Historical", "Forecast")) %>%
    dplyr::relocate(Type, .before = Date) %>%
    dplyr::rename(
      Model = Model_ID,
      lo.95 = lo_95,
      lo.80 = lo_80,
      hi.80 = hi_80,
      hi.95 = hi_95
    ) %>%
    dplyr::arrange(Combo, Date, Model)

  colnames(future_forecast_tbl)[colnames(future_forecast_tbl) == "Target"] <- target_variable

  # back test results
  back_test_tbl <- initial_fcst_tbl %>%
    dplyr::filter(Run_Type == "Back_Test") %>%
    dplyr::mutate(MAPE = abs((Forecast - Target) / Target)) %>%
    dplyr::select(
      Combo, tidyselect::all_of(combo_variables), Train_Test_ID, Date, Model_ID, Horizon,
      Forecast, Target, MAPE, Best_Model
    ) %>%
    dplyr::rename(
      Model = Model_ID,
      Back_Test_Scenario = Train_Test_ID,
      FCST = Forecast
    ) %>%
    dplyr::mutate(
      Number = as.numeric(Back_Test_Scenario) - 1,
      Number_Char = ifelse(Number < 10,
        paste0("0", Number),
        paste0("", Number)
      ),
      Back_Test_Scenario = paste0("Back_Test_", Number_Char)
    ) %>%
    dplyr::select(-Number, -Number_Char)

  # best model
  if (forecast_approach == "bottoms_up") {
    best_model_tbl <- back_test_tbl %>%
      dplyr::filter(Best_Model == "Yes") %>%
      dplyr::select(Combo, Model, Best_Model) %>%
      dplyr::distinct()
  } else {
    # read in unreconciled results
    best_model_tbl <- read_file(run_info,
      path = paste0(
        "/forecasts/*", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
        "*models.", run_info$data_output
      ),
      return_type = "df"
    ) %>%
      dplyr::filter(Best_Model == "Yes") %>%
      dplyr::select(Combo_ID, Model_ID, Best_Model) %>%
      dplyr::rename(
        Combo = Combo_ID,
        Model = Model_ID
      ) %>%
      dplyr::distinct()
  }

  return(list(
    final_fcst = future_forecast_tbl,
    back_test_data = back_test_tbl,
    back_test_best_MAPE = best_model_tbl
  ))
}
