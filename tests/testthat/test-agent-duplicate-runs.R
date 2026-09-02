test_that("multipart agent settings ignore ordering and duplicate values", {
  first <- data.frame(
    models_to_run = "xgboost---arima---glmnet",
    external_regressors = "Driver_B---Driver_A",
    recipes_to_run = "R2---R1",
    lag_periods = "9---3---6---3",
    rolling_window_periods = "12---3---9---6",
    seasonal_period = "12---3---6",
    multistep_horizon = FALSE,
    agent_version = 1,
    stringsAsFactors = FALSE
  )
  second <- data.frame(
    models_to_run = "glmnet---xgboost---arima",
    external_regressors = "Driver_A---Driver_B",
    recipes_to_run = "R1---R2",
    lag_periods = "6---9---3",
    rolling_window_periods = "9---6---12---3",
    seasonal_period = "6---12---3",
    multistep_horizon = FALSE,
    agent_version = 1,
    stringsAsFactors = FALSE
  )
  original <- first

  normalized_first <- normalize_agent_run_inputs(
    first,
    date_type = "month",
    forecast_horizon = 11
  )
  normalized_second <- normalize_agent_run_inputs(
    second,
    date_type = "month",
    forecast_horizon = 11
  )

  expect_identical(first, original)
  expect_equal(normalized_first, normalized_second)
  expect_true(does_param_set_exist(normalized_first, normalized_second))
  expect_equal(
    normalized_first$lag_periods,
    "R1:feature=3---6---9|R2:feature=3---6---9"
  )
  expect_equal(normalized_first$rolling_window_periods, "3---6---9---12")
})

test_that("NULL settings match their explicit monthly defaults", {
  implicit <- data.frame(
    models_to_run = NA_character_,
    external_regressors = NA_character_,
    recipes_to_run = NA_character_,
    lag_periods = NA_character_,
    rolling_window_periods = NA_character_,
    seasonal_period = NA_character_,
    multistep_horizon = FALSE,
    agent_version = 1,
    stringsAsFactors = FALSE
  )
  explicit <- data.frame(
    models_to_run = paste(rev(list_models()), collapse = "---"),
    external_regressors = "NULL",
    recipes_to_run = "R2---R1",
    lag_periods = "11---12",
    rolling_window_periods = "12---9---6---3",
    seasonal_period = "3---6---12",
    multistep_horizon = FALSE,
    agent_version = 1,
    stringsAsFactors = FALSE
  )

  normalized_implicit <- normalize_agent_run_inputs(
    implicit,
    date_type = "month",
    forecast_horizon = 11
  )
  normalized_explicit <- normalize_agent_run_inputs(
    explicit,
    date_type = "month",
    forecast_horizon = 11
  )

  expect_equal(normalized_implicit, normalized_explicit)
  expect_true(does_param_set_exist(normalized_explicit, normalized_implicit))
})

test_that("serialized seasonal defaults match NULL at every cadence", {
  for (date_type in c("year", "quarter", "month", "week", "day")) {
    implicit <- data.frame(seasonal_period = NA_character_)
    explicit <- data.frame(
      seasonal_period = paste(
        get_seasonal_periods(date_type),
        collapse = "---"
      )
    )

    normalized_implicit <- normalize_agent_run_inputs(
      implicit,
      date_type = date_type,
      forecast_horizon = 11
    )
    normalized_explicit <- normalize_agent_run_inputs(
      explicit,
      date_type = date_type,
      forecast_horizon = 11
    )

    expect_equal(
      normalized_implicit,
      normalized_explicit,
      info = date_type
    )
  }
})

test_that("observed duplicate run patterns normalize to the same inputs", {
  model_set <- paste(
    c(
      "arima", "ets", "meanf", "nnetar", "prophet", "snaive",
      "stlm-arima", "tbats", "theta", "cubist", "glmnet", "xgboost",
      "chronos2", "chronos-bolt-base", "chronos-bolt-tiny"
    ),
    collapse = "---"
  )
  run_3 <- data.frame(
    clean_outliers = FALSE,
    lag_periods = "21",
    rolling_window_periods = NA_character_,
    recipes_to_run = "R1",
    models_to_run = model_set,
    seasonal_period = NA_character_,
    multistep_horizon = FALSE,
    stringsAsFactors = FALSE
  )
  run_4 <- run_3
  run_4$rolling_window_periods <- "3---6---9---12"
  run_6 <- run_3
  run_6$clean_outliers <- TRUE
  run_9 <- run_6
  run_9$rolling_window_periods <- "12---9---6---3"

  normalize <- function(value) {
    normalize_agent_run_inputs(
      value,
      date_type = "month",
      forecast_horizon = 11
    )
  }

  expect_equal(normalize(run_3), normalize(run_4))
  expect_equal(normalize(run_6), normalize(run_9))
  expect_false(does_param_set_exist(normalize(run_3), normalize(run_6)))
})

test_that("genuine agent setting changes remain distinct", {
  baseline <- data.frame(
    models_to_run = "arima---glmnet",
    external_regressors = "Driver_A---Driver_B",
    recipes_to_run = "R1",
    lag_periods = "3---6---9",
    rolling_window_periods = "3---6---9---12",
    seasonal_period = "3---6---12",
    multistep_horizon = FALSE,
    agent_version = 1,
    stringsAsFactors = FALSE
  )
  alternatives <- list(
    transform(baseline, models_to_run = "arima---xgboost"),
    transform(baseline, external_regressors = "Driver_A---Driver_C"),
    transform(baseline, recipes_to_run = "R1---R2"),
    transform(baseline, lag_periods = "3---6---12"),
    transform(baseline, rolling_window_periods = "2---4---8"),
    transform(baseline, seasonal_period = "4---8---12"),
    transform(baseline, multistep_horizon = TRUE)
  )
  normalized_baseline <- normalize_agent_run_inputs(
    baseline,
    date_type = "month",
    forecast_horizon = 11
  )

  for (alternative in alternatives) {
    normalized_alternative <- normalize_agent_run_inputs(
      alternative,
      date_type = "month",
      forecast_horizon = 11
    )
    expect_false(
      does_param_set_exist(normalized_alternative, normalized_baseline)
    )
  }
})

test_that("default normalization remains date and recipe aware", {
  inputs <- data.frame(
    lag_periods = NA_character_,
    rolling_window_periods = NA_character_,
    recipes_to_run = NA_character_,
    seasonal_period = NA_character_,
    multistep_horizon = FALSE,
    stringsAsFactors = FALSE
  )

  monthly <- normalize_agent_run_inputs(
    inputs,
    date_type = "month",
    forecast_horizon = 11
  )
  weekly <- normalize_agent_run_inputs(
    inputs,
    date_type = "week",
    forecast_horizon = 11
  )
  global_monthly <- normalize_agent_run_inputs(
    inputs[, setdiff(names(inputs), "recipes_to_run"), drop = FALSE],
    date_type = "month",
    forecast_horizon = 11,
    default_recipes = "R1"
  )

  expect_equal(monthly$recipes_to_run, "R1---R2")
  expect_equal(weekly$recipes_to_run, "R1")
  expect_false(identical(monthly$lag_periods, weekly$lag_periods))
  expect_false(identical(
    monthly$rolling_window_periods,
    weekly$rolling_window_periods
  ))
  expect_match(global_monthly$lag_periods, "^R1:feature=")
  expect_false(grepl("R2:feature=", global_monthly$lag_periods, fixed = TRUE))
})

test_that("reason_inputs rejects reordered multipart duplicate settings", {
  llm <- new.env(parent = emptyenv())
  llm$chat <- function(...) {
    paste0(
      '{"models_to_run":"glmnet---arima",',
      '"external_regressors":"Driver_B---Driver_A",',
      '"clean_missing_values":"FALSE",',
      '"clean_outliers":"FALSE",',
      '"forecast_approach":"bottoms_up",',
      '"stationary":"FALSE",',
      '"feature_selection":"FALSE",',
      '"multistep_horizon":"FALSE",',
      '"seasonal_period":"12---3---6",',
      '"recipes_to_run":"R1",',
      '"lag_periods":"9---3---6",',
      '"rolling_window_periods":"9---6---3",',
      '"reasoning":"test"}'
    )
  }
  agent_info <- list(
    llm = llm,
    project_info = list(
      combo_variables = "id",
      target_variable = "Target",
      date_type = "month"
    ),
    forecast_horizon = 11,
    external_regressors = c("Driver_A", "Driver_B"),
    hist_end_date = as.Date("2026-07-01"),
    negative_forecast = FALSE,
    agent_version = 1,
    forecast_approach = "bottoms_up"
  )
  previous_runs <- data.frame(
    agent_version = 1,
    run_number = 1,
    best_run = "yes",
    weighted_mape = 0.2,
    model_avg_wmape = 0.3,
    model_median_wmape = 0.25,
    model_std_wmape = 0.1,
    models_to_run = "arima---glmnet",
    external_regressors = "Driver_A---Driver_B",
    clean_missing_values = FALSE,
    clean_outliers = FALSE,
    forecast_approach = "bottoms_up",
    stationary = FALSE,
    feature_selection = FALSE,
    multistep_horizon = FALSE,
    seasonal_period = "3---6---12",
    recipes_to_run = "R1",
    lag_periods = "3---6---9",
    rolling_window_periods = "3---6---9",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    load_run_results = function(...) previous_runs,
    get_total_run_count = function(...) 1,
    load_eda_results = function(...) "No EDA needed for this test.",
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  expect_error(
    reason_inputs(
      agent_info = agent_info,
      combo = "combo-hash",
      weighted_mape_goal = 0.05
    ),
    "Duplicate parameter set detected"
  )
})

test_that("reason_inputs rejects one in any proposed seasonal period position", {
  response_for <- function(seasonal_period) {
    paste0(
      '{"models_to_run":"stlm-arima---tbats",',
      '"external_regressors":"NULL",',
      '"clean_missing_values":"FALSE",',
      '"clean_outliers":"FALSE",',
      '"forecast_approach":"bottoms_up",',
      '"stationary":"FALSE",',
      '"feature_selection":"FALSE",',
      '"multistep_horizon":"FALSE",',
      '"seasonal_period":"', seasonal_period, '",',
      '"recipes_to_run":"R1",',
      '"lag_periods":"NULL",',
      '"rolling_window_periods":"NULL",',
      '"reasoning":"test"}'
    )
  }
  agent_info <- list(
    project_info = list(
      combo_variables = "id",
      target_variable = "Target",
      date_type = "month"
    ),
    forecast_horizon = 11,
    external_regressors = character(0),
    hist_end_date = as.Date("2026-07-01"),
    negative_forecast = FALSE,
    agent_version = 1,
    forecast_approach = "bottoms_up"
  )

  testthat::local_mocked_bindings(
    load_run_results = function(...) NULL,
    get_total_run_count = function(...) 0,
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  for (seasonal_period in c("1", "1---12", "12---1", "12---3---1")) {
    llm <- new.env(parent = emptyenv())
    llm$chat <- function(...) response_for(seasonal_period)
    agent_info$llm <- llm

    expect_error(
      reason_inputs(
        agent_info = agent_info,
        combo = "combo-hash",
        weighted_mape_goal = 0.05
      ),
      "Invalid proposed seasonal_period.*greater than 1",
      info = seasonal_period
    )
  }

  for (seasonal_period in c("NULL", "2", "2---6---12")) {
    llm <- new.env(parent = emptyenv())
    llm$chat <- function(...) response_for(seasonal_period)
    agent_info$llm <- llm

    result <- reason_inputs(
      agent_info = agent_info,
      combo = "combo-hash",
      weighted_mape_goal = 0.05
    )

    if (seasonal_period == "NULL") {
      expect_identical(result$seasonal_period, "NULL")
    } else {
      expect_equal(
        result$seasonal_period,
        as.numeric(strsplit(seasonal_period, "---", fixed = TRUE)[[1]])
      )
    }
  }
})

test_that("reason_inputs retry corrects a proposed seasonal period of one", {
  llm <- new.env(parent = emptyenv())
  llm$prompts <- character(0)
  llm$call_count <- 0L
  llm$chat <- function(prompt, ...) {
    llm$call_count <- llm$call_count + 1L
    llm$prompts <- c(llm$prompts, prompt)
    seasonal_period <- if (llm$call_count == 1L) "1" else "12---6---3"
    paste0(
      '{"models_to_run":"stlm-arima---tbats",',
      '"external_regressors":"NULL",',
      '"clean_missing_values":"FALSE",',
      '"clean_outliers":"FALSE",',
      '"forecast_approach":"bottoms_up",',
      '"stationary":"FALSE",',
      '"feature_selection":"FALSE",',
      '"multistep_horizon":"FALSE",',
      '"seasonal_period":"', seasonal_period, '",',
      '"recipes_to_run":"R1",',
      '"lag_periods":"NULL",',
      '"rolling_window_periods":"NULL",',
      '"reasoning":"test"}'
    )
  }
  agent_info <- list(
    llm = llm,
    project_info = list(
      combo_variables = "id",
      target_variable = "Target",
      date_type = "month"
    ),
    forecast_horizon = 11,
    external_regressors = character(0),
    hist_end_date = as.Date("2026-07-01"),
    negative_forecast = FALSE,
    agent_version = 1,
    forecast_approach = "bottoms_up"
  )
  node <- list(
    fn = "reason_inputs",
    max_retry = 3,
    retry_mode = "plain"
  )
  context <- list(
    args = list(
      agent_info = agent_info,
      combo = "combo-hash",
      weighted_mape_goal = 0.05,
      last_error = NULL
    ),
    results = list(),
    attempts = list()
  )

  testthat::local_mocked_bindings(
    load_run_results = function(...) NULL,
    get_total_run_count = function(...) 0,
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  result <- execute_node(node, context, llm)

  expect_true(result$ok)
  expect_identical(llm$call_count, 2L)
  expect_match(
    llm$prompts[[2]],
    "Invalid proposed seasonal_period.*greater than 1"
  )
  expect_equal(
    result$ctx$results$reason_inputs$seasonal_period,
    c(12, 6, 3)
  )
})

test_that("exhausted invalid seasonal periods abort optimization gracefully", {
  llm <- new.env(parent = emptyenv())
  llm$call_count <- 0L
  llm$chat <- function(...) {
    llm$call_count <- llm$call_count + 1L
    paste0(
      '{"models_to_run":"stlm-arima---tbats",',
      '"external_regressors":"NULL",',
      '"clean_missing_values":"FALSE",',
      '"clean_outliers":"FALSE",',
      '"forecast_approach":"bottoms_up",',
      '"stationary":"FALSE",',
      '"feature_selection":"FALSE",',
      '"multistep_horizon":"FALSE",',
      '"seasonal_period":"1",',
      '"recipes_to_run":"R1",',
      '"lag_periods":"NULL",',
      '"rolling_window_periods":"NULL",',
      '"reasoning":"test"}'
    )
  }
  agent_info <- list(
    llm = llm,
    project_info = list(
      combo_variables = "id",
      target_variable = "Target",
      date_type = "month"
    ),
    forecast_horizon = 11,
    external_regressors = character(0),
    hist_end_date = as.Date("2026-07-01"),
    negative_forecast = FALSE,
    agent_version = 2,
    forecast_approach = "bottoms_up"
  )
  context <- list(
    args = list(
      agent_info = agent_info,
      combo = "combo-hash",
      weighted_mape_goal = 0.05,
      last_error = NULL
    ),
    results = list(),
    attempts = list()
  )

  testthat::local_mocked_bindings(
    load_run_results = function(...) NULL,
    get_total_run_count = function(...) 1,
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  result <- execute_node(
    list(
      fn = "reason_inputs",
      max_retry = 3,
      retry_mode = "plain"
    ),
    context,
    llm
  )

  expect_true(result$ok)
  expect_identical(llm$call_count, 4L)
  expect_identical(result$ctx$results$reason_inputs$abort, "TRUE")
  expect_match(
    result$ctx$results$reason_inputs$reasoning,
    "Invalid proposed seasonal_period.*Aborting optimization"
  )
})

test_that("local agent prompt requires seasonal periods greater than one", {
  agent_info <- list(
    project_info = list(
      combo_variables = "id",
      target_variable = "Target",
      date_type = "month"
    ),
    external_regressors = character(0),
    forecast_horizon = 11,
    hist_end_date = as.Date("2026-07-01"),
    agent_version = 1
  )

  testthat::local_mocked_bindings(
    load_eda_results = function(...) "No EDA needed for this test.",
    get_foundation_model_suffix = function() "",
    .package = "finnts"
  )

  prompt <- iterate_forecast_system_prompt(
    agent_info = agent_info,
    combo = "combo-hash",
    weighted_mape_goal = 0.05
  )

  expect_match(prompt, "You MUST NOT use 1 in any position", fixed = TRUE)
  expect_match(prompt, '"seasonal_period"       : "NULL|2---6---12"', fixed = TRUE)
  expect_false(grepl('"seasonal_period"       : "NULL|1---2---3"', prompt, fixed = TRUE))
})

test_that("legacy invalid seasonal periods use cadence defaults during updates", {
  for (seasonal_period in c("1", "1---2", "1---2---3", "1---9", "1---2---12", "12---1", "12---3---1")) {
    expect_warning(
      result <- resolve_previous_seasonal_period(seasonal_period, "month"),
      "previous run used an invalid seasonal_period.*monthly defaults",
      info = seasonal_period
    )
    expect_null(result, info = seasonal_period)
  }

  expect_null(resolve_previous_seasonal_period(NA_character_, "month"))
  expect_equal(resolve_previous_seasonal_period("12---6---3", "month"), c(12, 6, 3))
})
