# tests/testthat/test-input_checks.R

# Skip all tests in this file on CRAN
skip_on_cran()

# * Test data setup ----

# Valid base dataset
valid_data <- data.frame(
  Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
  id = rep("A", 12),
  value = rnorm(12),
  xreg1 = rnorm(12)
)

# Dataset with an extra date-formatted column
data_with_date_col <- valid_data
data_with_date_col$order_date <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12)

# * Happy path ----

test_that("check_input_data passes with valid inputs", {
  expect_no_error(
    finnts:::check_input_data(
      input_data = valid_data,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = c("xreg1"),
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    )
  )
})

# * 'Date' as combo variable ----

test_that("check_input_data rejects 'Date' as combo variable", {
  expect_error(
    finnts:::check_input_data(
      input_data = valid_data,
      combo_variables = c("Date"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "Date.*combo variable.*reserved for the time stamp"
  )

  # Also when 'Date' is among multiple combo variables
  expect_error(
    finnts:::check_input_data(
      input_data = valid_data,
      combo_variables = c("id", "Date"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "Date.*combo variable.*reserved for the time stamp"
  )
})

# * 'Date' as target variable ----

test_that("check_input_data rejects 'Date' as target variable", {
  expect_error(
    finnts:::check_input_data(
      input_data = valid_data,
      combo_variables = c("id"),
      target_variable = "Date",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "Date.*target variable.*reserved for the time stamp"
  )
})

# * 'Date' as external regressor ----

test_that("check_input_data rejects 'Date' as external regressor", {
  expect_error(
    finnts:::check_input_data(
      input_data = valid_data,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = c("Date"),
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "Date.*external regressor.*reserved for the time stamp"
  )

  # Also when 'Date' is among multiple xregs
  expect_error(
    finnts:::check_input_data(
      input_data = valid_data,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = c("xreg1", "Date"),
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "Date.*external regressor.*reserved for the time stamp"
  )
})

# * Date-formatted column as combo variable ----

test_that("check_input_data rejects date-formatted combo variable", {
  expect_error(
    finnts:::check_input_data(
      input_data = data_with_date_col,
      combo_variables = c("order_date"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "combo variable 'order_date'.*date-formatted"
  )

  # POSIXct column should also be rejected
  data_posix <- valid_data
  data_posix$ts_col <- as.POSIXct(valid_data$Date)
  expect_error(
    finnts:::check_input_data(
      input_data = data_posix,
      combo_variables = c("ts_col"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "combo variable 'ts_col'.*date-formatted"
  )
})
