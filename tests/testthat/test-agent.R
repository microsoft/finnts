# tests/testthat/test-agent.R

# Skip all tests in this file on CRAN
skip_on_cran()

# Helper function to check if LLM credentials are available
has_llm_credentials <- function() {
  # Check for Azure OpenAI credentials
  azure_ok <- !is.na(Sys.getenv("AZURE_OPENAI_ENDPOINT", unset = NA)) &&
    !is.na(Sys.getenv("AZURE_OPENAI_API_KEY", unset = NA)) &&
    nzchar(Sys.getenv("AZURE_OPENAI_ENDPOINT", unset = "")) &&
    nzchar(Sys.getenv("AZURE_OPENAI_API_KEY", unset = ""))

  # Check for OpenAI credentials
  openai_ok <- !is.na(Sys.getenv("OPENAI_API_KEY", unset = NA)) &&
    nzchar(Sys.getenv("OPENAI_API_KEY", unset = ""))

  return(azure_ok || openai_ok)
}

# Create a simple test LLM function
create_test_llm <- function() {
  if (has_llm_credentials()) {
    # Use Azure OpenAI if available
    if (!is.na(Sys.getenv("AZURE_OPENAI_ENDPOINT", unset = NA)) &&
      nzchar(Sys.getenv("AZURE_OPENAI_ENDPOINT", unset = ""))) {
      return(ellmer::chat_azure_openai(model = "gpt-4o-mini"))
    } else {
      # Fall back to OpenAI
      return(ellmer::chat_openai(model = "gpt-4o-mini"))
    }
  } else {
    skip("No LLM credentials available for testing")
  }
}

# * Test data setup ----

# Create small test dataset with 2 time series and ~2.5 years of monthly data
test_data_multi <- timetk::m4_monthly %>%
  dplyr::filter(id %in% c("M750", "M1")) %>%
  dplyr::filter(date >= as.Date("2012-07-01"), date <= as.Date("2015-01-01")) %>%
  dplyr::rename(Date = date) %>%
  dplyr::mutate(id = as.character(id))

# Single time series for simpler tests
test_data_single <- timetk::m4_monthly %>%
  dplyr::filter(id == "M750") %>%
  dplyr::filter(date >= as.Date("2012-07-01"), date <= as.Date("2015-01-01")) %>%
  dplyr::rename(Date = date) %>%
  dplyr::mutate(
    id = as.character(id),
    regressor1 = rnorm(dplyr::n())
  )

# * Test set_project_info ----

test_that("set_project_info creates valid project for agent", {
  project <- set_project_info(
    project_name = "agent_test",
    path = tempdir(),
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    fiscal_year_start = 1
  )

  expect_type(project, "list")
  expect_equal(project$project_name, "agent_test")
  expect_equal(project$combo_variables, c("id"))
  expect_equal(project$target_variable, "value")
  expect_equal(project$date_type, "month")
})

test_that("set_project_info rejects 'Date' as combo variable", {
  expect_error(
    set_project_info(
      project_name = "date_combo_test",
      combo_variables = c("Date"),
      target_variable = "value",
      date_type = "month"
    ),
    "Date.*combo variable.*reserved for the time stamp"
  )

  expect_error(
    set_project_info(
      project_name = "date_combo_test",
      combo_variables = c("id", "Date"),
      target_variable = "value",
      date_type = "month"
    ),
    "Date.*combo variable.*reserved for the time stamp"
  )
})

test_that("set_project_info rejects 'Date' as target variable", {
  expect_error(
    set_project_info(
      project_name = "date_target_test",
      combo_variables = c("id"),
      target_variable = "Date",
      date_type = "month"
    ),
    "Date.*target variable.*reserved for the time stamp"
  )
})

# * Test set_agent_info ----

test_that("set_agent_info creates valid agent info object", {
  skip_if_not(has_llm_credentials(), "LLM credentials not available")

  project <- set_project_info(
    project_name = "agent_test_info",
    path = tempdir(),
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    fiscal_year_start = 1
  )

  driver_llm <- create_test_llm()

  agent_info <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = test_data_single,
    forecast_horizon = 3,
    external_regressors = NULL,
    allow_hierarchical_forecast = FALSE,
    overwrite = TRUE
  )

  expect_type(agent_info, "list")
  expect_true("project_info" %in% names(agent_info))
  expect_true("driver_llm" %in% names(agent_info))
  expect_true("forecast_horizon" %in% names(agent_info))
  expect_equal(agent_info$forecast_horizon, 3)
  expect_true("run_id" %in% names(agent_info))
  expect_true("agent_version" %in% names(agent_info))
  expect_true("forecast_approach" %in% names(agent_info))
  expect_true("negative_forecast" %in% names(agent_info))
  expect_equal(agent_info$negative_forecast, FALSE)
})

test_that("set_agent_info validates inputs correctly", {
  skip_if_not(has_llm_credentials(), "LLM credentials not available")

  project <- set_project_info(
    project_name = "agent_validation",
    path = tempdir(),
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month"
  )

  driver_llm <- create_test_llm()

  # Test invalid forecast_horizon
  expect_error(
    set_agent_info(
      project_info = project,
      driver_llm = driver_llm,
      input_data = test_data_single,
      forecast_horizon = "3" # should be numeric
    ),
    "forecast_horizon"
  )

  # Test invalid input_data
  expect_error(
    set_agent_info(
      project_info = project,
      driver_llm = driver_llm,
      input_data = "not a data frame",
      forecast_horizon = 3
    ),
    "input_data"
  )

  # Test invalid negative_forecast
  expect_error(
    set_agent_info(
      project_info = project,
      driver_llm = driver_llm,
      input_data = test_data_single,
      forecast_horizon = 3,
      negative_forecast = "yes"
    ),
    "negative_forecast"
  )
})

test_that("set_agent_info handles hierarchical forecast detection", {
  skip_if_not(has_llm_credentials(), "LLM credentials not available")

  project <- set_project_info(
    project_name = "agent_hierarchy",
    path = tempdir(),
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month"
  )

  driver_llm <- create_test_llm()

  agent_info <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = test_data_multi,
    forecast_horizon = 3,
    allow_hierarchical_forecast = TRUE,
    overwrite = TRUE
  )

  expect_true("forecast_approach" %in% names(agent_info))
  # With only 2 flat time series, should default to bottoms_up
  expect_equal(agent_info$forecast_approach, "bottoms_up")
})

# * Test input-changed error messages ----

test_that("set_project_info error lists changed inputs", {
  temp_path <- tempdir()

  # baseline call establishes the log
  set_project_info(
    project_name = "proj_change_test",
    path = temp_path,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    fiscal_year_start = 1,
    data_output = "csv",
    object_output = "rds",
    weekly_to_daily = TRUE,
    overwrite = TRUE
  )

  # helper for project change tests
  expect_project_change_error <- function(field_regex, ...) {
    expect_error(
      set_project_info(
        project_name = "proj_change_test",
        path = temp_path,
        ...
      ),
      regexp = field_regex
    )
  }

  # change target_variable
  expect_project_change_error(
    "target_variable.*expected.*value.*got.*new_target",
    combo_variables = c("id"), target_variable = "new_target",
    date_type = "month", fiscal_year_start = 1, overwrite = FALSE
  )

  # change combo_variables
  expect_project_change_error(
    "combo_variables.*expected.*id.*got.*id, group",
    combo_variables = c("id", "group"), target_variable = "value",
    date_type = "month", fiscal_year_start = 1, overwrite = FALSE
  )

  # change date_type
  expect_project_change_error(
    "date_type.*expected.*month.*got.*quarter",
    combo_variables = c("id"), target_variable = "value",
    date_type = "quarter", fiscal_year_start = 1, overwrite = FALSE
  )

  # change fiscal_year_start
  expect_project_change_error(
    "fiscal_year_start.*expected.*1.*got.*6",
    combo_variables = c("id"), target_variable = "value",
    date_type = "month", fiscal_year_start = 6, overwrite = FALSE
  )

  # change data_output
  expect_project_change_error(
    "data_output.*expected.*csv.*got.*parquet",
    combo_variables = c("id"), target_variable = "value",
    date_type = "month", fiscal_year_start = 1,
    data_output = "parquet", overwrite = FALSE
  )

  # change object_output
  expect_project_change_error(
    "object_output.*expected.*rds.*got.*qs2",
    combo_variables = c("id"), target_variable = "value",
    date_type = "month", fiscal_year_start = 1,
    object_output = "qs2", overwrite = FALSE
  )

  # change weekly_to_daily
  expect_project_change_error(
    "weekly_to_daily.*expected.*TRUE.*got.*FALSE",
    combo_variables = c("id"), target_variable = "value",
    date_type = "month", fiscal_year_start = 1,
    weekly_to_daily = FALSE, overwrite = FALSE
  )
})

test_that("set_project_info warns instead of errors on path change", {
  temp_path <- tempdir()

  # baseline call establishes the log
  set_project_info(
    project_name = "proj_path_warn_test",
    path = temp_path,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    fiscal_year_start = 1,
    data_output = "csv",
    object_output = "rds",
    weekly_to_daily = TRUE,
    overwrite = TRUE
  )

  # modify the stored log to have a different path value so a mismatch is

  # detected when calling set_project_info again with the same path
  log_file <- list.files(
    file.path(temp_path, "logs"),
    pattern = "-project\\.csv$",
    full.names = TRUE
  )
  log_file <- log_file[grep(hash_data("proj_path_warn_test"), log_file)]
  log_data <- utils::read.csv(log_file, stringsAsFactors = FALSE)
  log_data$path <- "/old/fake/path"
  utils::write.csv(log_data, log_file, row.names = FALSE)

  # calling again with same path but log has different stored path -> warn
  expect_warning(
    result <- set_project_info(
      project_name = "proj_path_warn_test",
      path = temp_path,
      combo_variables = c("id"),
      target_variable = "value",
      date_type = "month",
      fiscal_year_start = 1,
      data_output = "csv",
      object_output = "rds",
      weekly_to_daily = TRUE,
      overwrite = FALSE
    ),
    regexp = "path.*input has changed"
  )

  # should still return a valid project list
  expect_type(result, "list")
  expect_equal(result$path, temp_path)
})

test_that("set_agent_info error lists changed inputs", {
  skip_if_not(has_llm_credentials(), "LLM credentials not available")

  project <- set_project_info(
    project_name = "agent_change_test",
    path = tempdir(),
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    fiscal_year_start = 1,
    overwrite = TRUE
  )

  driver_llm <- create_test_llm()

  # baseline call establishes the log
  set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = test_data_single,
    forecast_horizon = 3,
    external_regressors = NULL,
    hist_end_date = NULL,
    hist_start_date = NULL,
    back_test_scenarios = NULL,
    back_test_spacing = NULL,
    combo_cleanup_date = NULL,
    allow_hierarchical_forecast = FALSE,
    run_global_models = TRUE,
    run_local_models = TRUE,
    overwrite = TRUE
  )

  # helper for agent change tests
  expect_agent_change_error <- function(field_regex, ...) {
    expect_error(
      set_agent_info(
        project_info = project,
        driver_llm = driver_llm,
        input_data = test_data_single,
        ...
      ),
      regexp = field_regex
    )
  }

  # change forecast_horizon
  expect_agent_change_error(
    "forecast_horizon.*expected.*3.*got.*6",
    forecast_horizon = 6, external_regressors = NULL,
    allow_hierarchical_forecast = FALSE,
    run_global_models = TRUE, run_local_models = TRUE,
    overwrite = FALSE
  )

  # change external_regressors from NULL to a value
  expect_agent_change_error(
    "external_regressors.*expected.*NULL.*got.*regressor1",
    forecast_horizon = 3, external_regressors = c("regressor1"),
    allow_hierarchical_forecast = FALSE,
    run_global_models = TRUE, run_local_models = TRUE,
    overwrite = FALSE
  )

  # change hist_end_date from default (max date in data) to a different date
  expect_agent_change_error(
    "hist_end_date.*expected.*2015-01-01.*got.*2014-06-01",
    forecast_horizon = 3, external_regressors = NULL,
    hist_end_date = as.Date("2014-06-01"),
    allow_hierarchical_forecast = FALSE,
    run_global_models = TRUE, run_local_models = TRUE,
    overwrite = FALSE
  )

  # change hist_start_date from default (min date in data) to a different date
  expect_agent_change_error(
    "hist_start_date.*expected.*2012-07-01.*got.*2013-01-01",
    forecast_horizon = 3, external_regressors = NULL,
    hist_start_date = as.Date("2013-01-01"),
    allow_hierarchical_forecast = FALSE,
    run_global_models = TRUE, run_local_models = TRUE,
    overwrite = FALSE
  )

  # change back_test_scenarios from NULL to a value
  expect_agent_change_error(
    "back_test_scenarios.*expected.*NULL.*got.*2",
    forecast_horizon = 3, external_regressors = NULL,
    back_test_scenarios = 2,
    allow_hierarchical_forecast = FALSE,
    run_global_models = TRUE, run_local_models = TRUE,
    overwrite = FALSE
  )

  # change back_test_spacing from NULL to a value
  expect_agent_change_error(
    "back_test_spacing.*expected.*NULL.*got.*3",
    forecast_horizon = 3, external_regressors = NULL,
    back_test_spacing = 3,
    allow_hierarchical_forecast = FALSE,
    run_global_models = TRUE, run_local_models = TRUE,
    overwrite = FALSE
  )

  # change combo_cleanup_date from NULL to a date
  expect_agent_change_error(
    "combo_cleanup_date.*expected.*NULL.*got.*2014-01-01",
    forecast_horizon = 3, external_regressors = NULL,
    combo_cleanup_date = as.Date("2014-01-01"),
    allow_hierarchical_forecast = FALSE,
    run_global_models = TRUE, run_local_models = TRUE,
    overwrite = FALSE
  )

  # change run_global_models
  expect_agent_change_error(
    "run_global_models.*expected.*TRUE.*got.*FALSE",
    forecast_horizon = 3, external_regressors = NULL,
    allow_hierarchical_forecast = FALSE,
    run_global_models = FALSE, run_local_models = TRUE,
    overwrite = FALSE
  )

  # change run_local_models
  expect_agent_change_error(
    "run_local_models.*expected.*TRUE.*got.*FALSE",
    forecast_horizon = 3, external_regressors = NULL,
    allow_hierarchical_forecast = FALSE,
    run_global_models = TRUE, run_local_models = FALSE,
    overwrite = FALSE
  )
})

# * Test iterate_forecast workflow ----

test_that("iterate_forecast completes with all getter functions and ask_agent", {
  skip_if_not(has_llm_credentials(), "LLM credentials not available")

  project <- set_project_info(
    project_name = "agent_iterate_complete",
    path = tempdir(),
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    fiscal_year_start = 1
  )

  driver_llm <- create_test_llm()

  agent_info <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = test_data_single,
    forecast_horizon = 3,
    overwrite = TRUE,
    back_test_scenarios = 1,
    back_test_spacing = 3
  )

  # Verify agent_info structure
  expect_true("forecast_approach" %in% names(agent_info))
  expect_type(agent_info$forecast_approach, "character")

  # Run iterate_forecast
  iterate_forecast(
    agent_info = agent_info,
    weighted_mape_goal = 0.05,
    max_iter = 1,
    parallel_processing = NULL,
    seed = 123
  )

  # Test get_agent_forecast
  forecast <- get_agent_forecast(agent_info = agent_info)
  expect_s3_class(forecast, "data.frame")
  expect_true("Combo" %in% colnames(forecast))
  expect_true("Date" %in% colnames(forecast))
  expect_true("Forecast" %in% colnames(forecast))
  expect_true(nrow(forecast) > 0)

  # Test get_best_agent_run
  best_run <- get_best_agent_run(agent_info = agent_info)
  expect_s3_class(best_run, "data.frame")
  expect_true("combo" %in% colnames(best_run))
  expect_true("weighted_mape" %in% colnames(best_run))
  expect_true(nrow(best_run) >= 1)
  expect_true(all(best_run$weighted_mape >= 0))

  # Test get_eda_data
  eda <- get_eda_data(agent_info = agent_info)
  expect_type(eda, "list")
  expect_true(length(eda) > 0)

  # Test get_summarized_models
  models <- get_summarized_models(agent_info = agent_info)
  expect_type(models, "list")
  expect_true(length(models) > 0)

  # Test ask_agent with a simple question
  answer <- ask_agent(
    agent_info = agent_info,
    question = "How many time series were forecasted?"
  )
  expect_type(answer, "character")
  expect_true(nchar(answer) > 0)

  # Test resolve_combo_hashes with single time series
  combo_hashes <- get_total_combos(agent_info)
  expect_length(combo_hashes, 1)
  resolved <- resolve_combo_hashes(agent_info, combo_hashes)
  expect_equal(resolved, "M750")

  # Test ask_agent input validation
  expect_error(
    ask_agent(
      agent_info = agent_info,
      question = 123 # should be character
    ),
    "question"
  )
})

test_that("iterate_forecast validates parameters", {
  skip_if_not(has_llm_credentials(), "LLM credentials not available")

  project <- set_project_info(
    project_name = "agent_iterate_validate",
    path = tempdir(),
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month"
  )

  driver_llm <- create_test_llm()

  agent_info <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = test_data_single,
    forecast_horizon = 3,
    overwrite = TRUE
  )

  # Invalid max_iter
  expect_error(
    iterate_forecast(
      agent_info = agent_info,
      max_iter = "not_numeric"
    ),
    "max_iter"
  )

  # Invalid weighted_mape_goal
  expect_error(
    iterate_forecast(
      agent_info = agent_info,
      weighted_mape_goal = "not_numeric"
    ),
    "weighted_mape_goal"
  )
})

# * Test update_forecast new combo handling ----

test_that("update_forecast creates simple forecasts for new time series within cap", {
  skip_if_not(has_llm_credentials(), "LLM credentials not available")

  project <- set_project_info(
    project_name = "agent_resolve_hash_test",
    path = NULL,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    overwrite = TRUE
  )

  driver_llm <- create_test_llm()

  # Initial run with single time series
  initial_data <- test_data_single %>%
    dplyr::filter(Date <= as.Date("2014-10-01"))

  agent_info1 <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = initial_data,
    forecast_horizon = 3,
    overwrite = TRUE,
    back_test_scenarios = 1,
    back_test_spacing = 3
  )

  iterate_forecast(
    agent_info = agent_info1,
    max_iter = 1,
    seed = 123
  )

  # Update with 2 time series (adding M1) — within the floor of 10
  updated_data <- test_data_multi %>%
    dplyr::filter(Date <= as.Date("2015-01-01"))

  agent_info2 <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = updated_data,
    forecast_horizon = 3,
    overwrite = TRUE,
    back_test_scenarios = 1,
    back_test_spacing = 3
  )

  # Should succeed (1 new series is within cap of max(10, 20% of 1))
  expect_no_error(
    update_forecast(
      agent_info = agent_info2,
      allow_iterate_forecast = FALSE,
      seed = 123
    )
  )

  # Verify forecast includes both original and new time series
  forecast <- get_agent_forecast(agent_info = agent_info2)
  expect_true("M750" %in% forecast$Combo)
  expect_true("M1" %in% forecast$Combo)

  # Verify best run includes both time series
  best_run <- get_best_agent_run(agent_info = agent_info2)
  expect_true("M750" %in% best_run$combo)
  expect_true("M1" %in% best_run$combo)

  # Verify resolve_combo_hashes works with 2 time series
  combo_hashes <- get_total_combos(agent_info2)
  expect_length(combo_hashes, 2)
  resolved <- resolve_combo_hashes(agent_info2, combo_hashes)
  expect_true("M750" %in% resolved)
  expect_true("M1" %in% resolved)
})

# * Test update_forecast forecast approach mismatch ----

test_that("update_forecast errors when forecast approach changes between runs", {
  skip_if_not(has_llm_credentials(), "LLM credentials not available")

  project <- set_project_info(
    project_name = "agent_approach_mismatch_test",
    path = NULL,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    overwrite = TRUE
  )

  driver_llm <- create_test_llm()

  # Initial run with bottoms_up approach
  initial_data <- test_data_single %>%
    dplyr::filter(Date <= as.Date("2014-10-01"))

  agent_info1 <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = initial_data,
    forecast_horizon = 3,
    overwrite = TRUE,
    back_test_scenarios = 1,
    back_test_spacing = 3
  )

  iterate_forecast(
    agent_info = agent_info1,
    max_iter = 1,
    seed = 123
  )

  # Update with same data but override forecast approach
  updated_data <- test_data_single %>%
    dplyr::filter(Date <= as.Date("2015-01-01"))

  agent_info2 <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = updated_data,
    forecast_horizon = 3,
    overwrite = TRUE,
    back_test_scenarios = 1,
    back_test_spacing = 3
  )

  # Manually override forecast approach to simulate a mismatch
  agent_info2$forecast_approach <- "standard_hierarchy"

  expect_error(
    update_forecast(
      agent_info = agent_info2,
      allow_iterate_forecast = FALSE,
      seed = 123
    ),
    "Current forecast approach is 'standard_hierarchy' but the previous agent run used 'bottoms_up'"
  )
})

# * Test update_forecast workflow ----

test_that("update_forecast completes with getter functions and ask_agent", {
  skip_if_not(has_llm_credentials(), "LLM credentials not available")

  project <- set_project_info(
    project_name = "agent_update_complete",
    path = NULL,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month"
  )

  driver_llm <- create_test_llm()

  # Initial run with data through 2014-10-01
  initial_data <- test_data_single %>%
    dplyr::filter(Date <= as.Date("2014-10-01"))

  agent_info1 <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = initial_data,
    forecast_horizon = 3,
    overwrite = TRUE,
    back_test_scenarios = 1,
    back_test_spacing = 3
  )

  iterate_forecast(
    agent_info = agent_info1,
    max_iter = 1,
    seed = 123
  )

  # Update with more recent data
  updated_data <- test_data_single %>%
    dplyr::filter(Date <= as.Date("2015-01-01"))

  agent_info2 <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    input_data = updated_data,
    forecast_horizon = 3,
    overwrite = TRUE,
    back_test_scenarios = 1,
    back_test_spacing = 3
  )

  # Run update_forecast
  update_forecast(
    agent_info = agent_info2,
    weighted_mape_goal = 0.05,
    allow_iterate_forecast = FALSE, # Don't iterate to keep test fast
    seed = 123
  )

  # Test get_agent_forecast with updated data
  updated_forecast <- get_agent_forecast(agent_info = agent_info2)
  expect_s3_class(updated_forecast, "data.frame")
  expect_true(nrow(updated_forecast) > 0)

  # Test get_best_agent_run with updated data
  updated_best_run <- get_best_agent_run(agent_info = agent_info2)
  expect_s3_class(updated_best_run, "data.frame")
  expect_true(nrow(updated_best_run) >= 1)

  # Test get_eda_data remains accessible
  updated_eda <- get_eda_data(agent_info = agent_info2)
  expect_type(updated_eda, "list")

  # Test get_summarized_models with updated data
  updated_models <- get_summarized_models(agent_info = agent_info2)
  expect_type(updated_models, "list")

  # Test ask_agent after update
  answer <- ask_agent(
    agent_info = agent_info2,
    question = "What is the forecast accuracy?"
  )
  expect_type(answer, "character")
  expect_true(nchar(answer) > 0)
})

# * Integration test ----

test_that("full agent workflow with multiple time series completes successfully", {
  skip_if_not(has_llm_credentials(), "LLM credentials not available")

  # Comprehensive integration test with 2 time series
  project <- set_project_info(
    project_name = "agent_integration",
    path = NULL,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    fiscal_year_start = 1
  )

  driver_llm <- create_test_llm()
  reason_llm <- create_test_llm() # Can be same as driver

  # Step 1: Set up agent
  agent_info <- set_agent_info(
    project_info = project,
    driver_llm = driver_llm,
    reason_llm = reason_llm,
    input_data = test_data_multi,
    forecast_horizon = 3,
    external_regressors = NULL,
    allow_hierarchical_forecast = FALSE,
    overwrite = TRUE,
    back_test_scenarios = 1,
    back_test_spacing = 3
  )

  expect_type(agent_info, "list")

  # Step 2: Run forecast iteration
  iterate_forecast(
    agent_info = agent_info,
    weighted_mape_goal = 0.001, # Set a low goal to force multiple iterations
    max_iter = 2,
    parallel_processing = NULL,
    seed = 123
  )

  # Step 3: Get and validate all results
  forecast <- get_agent_forecast(agent_info = agent_info)
  best_run <- get_best_agent_run(agent_info = agent_info)
  eda <- get_eda_data(agent_info = agent_info)
  models <- get_summarized_models(agent_info = agent_info)

  # Verify forecast structure
  expect_s3_class(forecast, "data.frame")
  expect_true(all(c("Combo", "Date", "Forecast") %in% colnames(forecast)))
  combos_in_forecast <- unique(forecast$Combo)
  expect_true(length(combos_in_forecast) >= 2)

  # Verify best_run structure
  expect_s3_class(best_run, "data.frame")
  expect_true(all(c("combo", "weighted_mape") %in% colnames(best_run)))
  expect_true(all(best_run$weighted_mape >= 0))
  expect_true(nrow(best_run) >= 2) # Should have entries for both time series

  # Verify EDA results
  expect_type(eda, "list")
  expect_true(length(eda) > 0)

  # Verify model summaries
  expect_type(models, "list")
  expect_true(length(models) > 0)

  # Step 4: Test ask_agent

  # Question about accuracy
  answer1 <- ask_agent(
    agent_info = agent_info,
    question = "What is the average accuracy across all time series?"
  )
  expect_type(answer1, "character")
  expect_true(nchar(answer1) > 0)
})
