# tests/testthat/test-input_checks.R

test_that("check_input_type validates character type", {
  expect_silent(check_input_type("x", "hello", "character"))
  expect_error(
    check_input_type("x", 123, "character"),
    "invalid type for input name 'x'"
  )
})

test_that("check_input_type validates numeric type", {
  expect_silent(check_input_type("x", 42, "numeric"))
  expect_error(
    check_input_type("x", "abc", "numeric"),
    "invalid type for input name 'x'"
  )
})

test_that("check_input_type validates logical type", {
  expect_silent(check_input_type("x", TRUE, "logical"))
  expect_error(
    check_input_type("x", "yes", "logical"),
    "invalid type for input name 'x'"
  )
})

test_that("check_input_type validates expected values", {
  expect_silent(check_input_type("x", "csv", "character", c("csv", "parquet")))
  expect_error(
    check_input_type("x", "json", "character", c("csv", "parquet")),
    "invalid value for input name 'x'"
  )
})

test_that("check_input_type accepts multiple types", {
  expect_silent(check_input_type("x", "hello", c("character", "numeric")))
  expect_silent(check_input_type("x", 42, c("character", "numeric")))
})

test_that("check_input_type passes with NULL expected_value", {
  expect_silent(check_input_type("x", "anything", "character", NULL))
})

test_that("check_input_data catches missing combo variables", {
  data <- tibble::tibble(
    Date = as.Date("2020-01-01"),
    Target = 100,
    id = "A"
  )

  expect_error(
    check_input_data(
      data,
      combo_variables = c("missing_col"),
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "combo variables do not match"
  )
})

test_that("check_input_data catches missing target variable", {
  data <- tibble::tibble(
    Date = as.Date("2020-01-01"),
    Value = 100,
    id = "A"
  )

  expect_error(
    check_input_data(
      data,
      combo_variables = c("id"),
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "target variable does not match"
  )
})

test_that("check_input_data catches non-numeric target", {
  data <- tibble::tibble(
    Date = as.Date("2020-01-01"),
    Target = "abc",
    id = "A"
  )

  expect_error(
    check_input_data(
      data,
      combo_variables = c("id"),
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "Target variable in input data needs to be numeric"
  )
})

test_that("check_input_data catches missing Date column", {
  data <- tibble::tibble(
    date_col = as.Date("2020-01-01"),
    Target = 100,
    id = "A"
  )

  expect_error(
    check_input_data(
      data,
      combo_variables = c("id"),
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "date column in input data needs to be named as 'Date'"
  )
})

test_that("check_input_data catches non-date Date column", {
  data <- tibble::tibble(
    Date = "2020-01-01",
    Target = 100,
    id = "A"
  )

  expect_error(
    check_input_data(
      data,
      combo_variables = c("id"),
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "date column in input data needs to be formatted as a date"
  )
})

test_that("check_input_data catches invalid fiscal_year_start", {
  data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Target = c(100, 200, 300),
    id = c("A", "A", "A")
  )

  expect_error(
    check_input_data(
      data,
      combo_variables = c("id"),
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 13,
      parallel_processing = NULL
    ),
    "fiscal year start should be a number from 1 to 12"
  )
})

test_that("check_input_data catches missing external regressors", {
  data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01")),
    Target = c(100, 200),
    id = c("A", "A")
  )

  expect_error(
    check_input_data(
      data,
      combo_variables = c("id"),
      target_variable = "Target",
      external_regressors = c("missing_xreg"),
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "external regressors do not match"
  )
})

test_that("check_input_data catches duplicate rows", {
  data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-01-01", "2020-02-01")),
    Target = c(100, 100, 200),
    id = c("A", "A", "A")
  )

  expect_error(
    check_input_data(
      data,
      combo_variables = c("id"),
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "duplicate rows have been detected"
  )
})

test_that("check_input_data passes with valid data", {
  data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Target = c(100, 200, 300),
    id = c("A", "A", "A")
  )

  expect_silent(
    check_input_data(
      data,
      combo_variables = c("id"),
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    )
  )
})

test_that("check_parallel_processing passes with NULL", {
  run_info <- list(path = "/tmp/test")

  expect_silent(
    check_parallel_processing(
      run_info,
      parallel_processing = NULL
    )
  )
})

test_that("check_parallel_processing errors on invalid value", {
  run_info <- list(path = "/tmp/test")

  expect_error(
    check_parallel_processing(
      run_info,
      parallel_processing = "invalid"
    ),
    "parallel processing input must be one of these values"
  )
})

test_that("check_parallel_processing errors on local_machine with inner_parallel", {
  run_info <- list(path = "/tmp/test")

  expect_error(
    check_parallel_processing(
      run_info,
      parallel_processing = "local_machine",
      inner_parallel = TRUE
    ),
    "cannot run parallel process"
  )
})

test_that("check_parallel_processing errors on spark without sc", {
  run_info <- list(path = "/dbfs/test")

  expect_error(
    check_parallel_processing(
      run_info,
      parallel_processing = "spark"
    ),
    "Ensure that you are connected to a spark cluster"
  )
})

test_that("check_agent_info validates list type", {
  expect_error(
    check_agent_info("not_a_list"),
    "agent_info should be a list"
  )
})

test_that("check_agent_info catches missing elements", {
  incomplete_info <- list(
    agent_version = "1.0",
    run_id = "test"
  )

  expect_error(
    check_agent_info(incomplete_info),
    "agent_info is missing required elements"
  )
})

test_that("check_agent_info passes with all required elements", {
  full_info <- list(
    agent_version = "1.0",
    run_id = "test",
    project_info = list(),
    driver_llm = "gpt-4",
    reason_llm = "gpt-4",
    forecast_horizon = 3,
    external_regressors = NULL,
    hist_end_date = as.Date("2024-01-01"),
    back_test_scenarios = 3,
    back_test_spacing = 1,
    combo_cleanup_date = NULL,
    overwrite = FALSE
  )

  expect_silent(
    check_agent_info(full_info)
  )
})

test_that("check_input_data catches unevenly spaced month dates", {
  data <- tibble::tibble(
    Date = as.Date(c("2020-01-15", "2020-02-20", "2020-03-01")),
    Target = c(100, 200, 300),
    id = c("A", "A", "A")
  )

  expect_error(
    check_input_data(
      data,
      combo_variables = c("id"),
      target_variable = "Target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "historical date values are not evenly spaced"
  )
})

# -- check_input_type edge cases --

test_that("check_input_type accepts NULL with NULL type", {
  expect_silent(check_input_type("x", NULL, c("character", "NULL")))
})

test_that("check_input_type accepts vector of expected values", {
  expect_silent(check_input_type("x", c("R1", "R2"), "character", c("R1", "R2", "R3")))
})
