# tests/testthat/test-input_checks.R

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

# * Test set_run_info input change detection ----

test_that("set_run_info error lists changed inputs", {
  temp_path <- tempdir()

  # baseline call establishes the log
  set_run_info(
    project_name = "run_change_test",
    run_name = "run_change_run",
    path = temp_path,
    data_output = "csv",
    object_output = "rds",
    add_unique_id = FALSE
  )

  # helper for run change tests
  expect_run_change_error <- function(field_regex, ...) {
    expect_error(
      set_run_info(
        project_name = "run_change_test",
        run_name = "run_change_run",
        path = temp_path,
        add_unique_id = FALSE,
        ...
      ),
      regexp = field_regex
    )
  }

  # change data_output
  expect_run_change_error(
    "data_output.*expected.*csv.*got.*parquet",
    data_output = "parquet", object_output = "rds"
  )

  # change object_output
  expect_run_change_error(
    "object_output.*expected.*rds.*got.*qs2",
    data_output = "csv", object_output = "qs2"
  )
})

test_that("set_run_info warns instead of errors on path change", {
  temp_path <- tempdir()

  # baseline call establishes the log
  set_run_info(
    project_name = "run_path_warn_test",
    run_name = "run_path_warn_run",
    path = temp_path,
    data_output = "csv",
    object_output = "rds",
    add_unique_id = FALSE
  )

  # modify the stored log to have a different path value
  log_file <- list.files(
    file.path(temp_path, "logs"),
    pattern = paste0(
      finnts:::hash_data("run_path_warn_test"), "-",
      finnts:::hash_data("run_path_warn_run"), "\\.csv$"
    ),
    full.names = TRUE
  )
  log_data <- utils::read.csv(log_file, stringsAsFactors = FALSE)
  log_data$path <- "/old/fake/path"
  utils::write.csv(log_data, log_file, row.names = FALSE)

  # calling again with same path but log has different stored path -> warn
  expect_warning(
    result <- set_run_info(
      project_name = "run_path_warn_test",
      run_name = "run_path_warn_run",
      path = temp_path,
      data_output = "csv",
      object_output = "rds",
      add_unique_id = FALSE
    ),
    regexp = "path.*input has changed"
  )

  # should still return a valid list
  expect_type(result, "list")
  expect_equal(result$path, temp_path)
})

# * Test format_input_diff ----

test_that("format_input_diff reports changed fields", {
  prev <- data.frame(
    combo_variables = "id",
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 3,
    stringsAsFactors = FALSE
  )

  # single change
  curr <- prev
  curr$date_type <- "quarter"
  result <- finnts:::format_input_diff(prev, curr)
  expect_match(result, "date_type")
  expect_match(result, "expected.*month.*got.*quarter")

  # multiple changes
  curr2 <- prev
  curr2$target_variable <- "revenue"
  curr2$forecast_horizon <- 6
  result2 <- finnts:::format_input_diff(prev, curr2)
  expect_match(result2, "target_variable.*expected.*value.*got.*revenue")
  expect_match(result2, "forecast_horizon.*expected.*3.*got.*6")

  # no changes
  result3 <- finnts:::format_input_diff(prev, prev)
  expect_match(result3, "no column-level differences")

  # nullable fields display NA as NULL
  prev_na <- data.frame(external_regressors = NA_character_, stringsAsFactors = FALSE)
  curr_na <- data.frame(external_regressors = "xreg1", stringsAsFactors = FALSE)
  result4 <- finnts:::format_input_diff(prev_na, curr_na, nullable_fields = "external_regressors")
  expect_match(result4, "expected.*NULL.*got.*xreg1")
})
