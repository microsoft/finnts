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
    check_input_data(
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
    check_input_data(
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
    check_input_data(
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
    check_input_data(
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
    check_input_data(
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
    check_input_data(
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
    check_input_data(
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
    check_input_data(
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

# * Missing combo / target / external regressor columns ----

test_that("check_input_data rejects combo variables missing from input data", {
  expect_error(
    check_input_data(
      input_data = valid_data,
      combo_variables = c("missing_combo"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "combo variables do not match column headers.*Missing columns: missing_combo"
  )
})

test_that("check_input_data rejects a target variable missing from input data", {
  expect_error(
    check_input_data(
      input_data = valid_data,
      combo_variables = c("id"),
      target_variable = "missing_target",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "target variable 'missing_target' does not match a column header"
  )
})

test_that("check_input_data rejects external regressors missing from input data", {
  expect_error(
    check_input_data(
      input_data = valid_data,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = c("missing_xreg"),
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "external regressors do not match column headers.*Missing columns: missing_xreg"
  )
})

# * Non-numeric target variable ----

test_that("check_input_data rejects a non-numeric target variable", {
  data_char_target <- valid_data
  data_char_target$value <- as.character(data_char_target$value)
  expect_error(
    check_input_data(
      input_data = data_char_target,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "Target variable in input data needs to be numeric"
  )
})

# * Date column presence and formatting ----

test_that("check_input_data requires a column named 'Date'", {
  data_no_date <- valid_data
  names(data_no_date)[names(data_no_date) == "Date"] <- "when"
  expect_error(
    check_input_data(
      input_data = data_no_date,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "date column in input data needs to be named as 'Date'"
  )
})

test_that("check_input_data requires the 'Date' column to be date-formatted", {
  data_bad_date <- valid_data
  data_bad_date$Date <- as.character(data_bad_date$Date)
  expect_error(
    check_input_data(
      input_data = data_bad_date,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "date column in input data needs to be formatted as a date value"
  )
})

# * Even day-of-month spacing for month/quarter/year data ----

test_that("check_input_data rejects uneven day-of-month spacing for month data", {
  data_uneven <- valid_data
  data_uneven$Date <- as.Date(c(
    "2020-01-01", "2020-02-02", "2020-03-03", "2020-04-04",
    "2020-05-05", "2020-06-06", "2020-07-07", "2020-08-08",
    "2020-09-09", "2020-10-10", "2020-11-11", "2020-12-12"
  ))
  expect_error(
    check_input_data(
      input_data = data_uneven,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "historical date values are not evenly spaced.*day of the month"
  )
})

test_that("check_input_data does not apply day-of-month spacing check to week data", {
  data_uneven <- valid_data
  data_uneven$Date <- as.Date(c(
    "2020-01-01", "2020-02-02", "2020-03-03", "2020-04-04",
    "2020-05-05", "2020-06-06", "2020-07-07", "2020-08-08",
    "2020-09-09", "2020-10-10", "2020-11-11", "2020-12-12"
  ))
  expect_no_error(
    check_input_data(
      input_data = data_uneven,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "week",
      fiscal_year_start = 1,
      parallel_processing = NULL
    )
  )
})

# * fiscal_year_start range ----

test_that("check_input_data rejects fiscal_year_start outside 1-12", {
  expect_error(
    check_input_data(
      input_data = valid_data,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 13,
      parallel_processing = NULL
    ),
    "fiscal year start should be a number from 1 to 12"
  )

  expect_error(
    check_input_data(
      input_data = valid_data,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 0,
      parallel_processing = NULL
    ),
    "fiscal year start should be a number from 1 to 12"
  )
})

# * Duplicate combo-Date rows ----

test_that("check_input_data rejects duplicate combo-Date rows", {
  data_dup <- rbind(valid_data, valid_data[1, ])
  expect_error(
    check_input_data(
      input_data = data_dup,
      combo_variables = c("id"),
      target_variable = "value",
      external_regressors = NULL,
      date_type = "month",
      fiscal_year_start = 1,
      parallel_processing = NULL
    ),
    "duplicate rows have been detected in the input data"
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
      hash_data("run_path_warn_test"), "-",
      hash_data("run_path_warn_run"), "\\.csv$"
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
  result <- format_input_diff(prev, curr)
  expect_match(result, "date_type")
  expect_match(result, "expected.*month.*got.*quarter")

  # multiple changes
  curr2 <- prev
  curr2$target_variable <- "revenue"
  curr2$forecast_horizon <- 6
  result2 <- format_input_diff(prev, curr2)
  expect_match(result2, "target_variable.*expected.*value.*got.*revenue")
  expect_match(result2, "forecast_horizon.*expected.*3.*got.*6")

  # no changes
  result3 <- format_input_diff(prev, prev)
  expect_match(result3, "no column-level differences")

  # nullable fields display NA as NULL
  prev_na <- data.frame(external_regressors = NA_character_, stringsAsFactors = FALSE)
  curr_na <- data.frame(external_regressors = "xreg1", stringsAsFactors = FALSE)
  result4 <- format_input_diff(prev_na, curr_na, nullable_fields = "external_regressors")
  expect_match(result4, "expected.*NULL.*got.*xreg1")
})

# * Test normalize_log_df ----

test_that("normalize_log_df converts all columns to character", {
  df <- data.frame(
    a = 1L,
    b = TRUE,
    c = as.Date("2024-01-01"),
    d = 3.14,
    e = "hello",
    stringsAsFactors = FALSE
  )
  result <- normalize_log_df(df)
  expect_true(all(sapply(result, is.character)))
  expect_equal(result$a, "1")
  expect_equal(result$b, "TRUE")
  expect_equal(result$c, "2024-01-01")
  expect_equal(result$d, "3.14")
  expect_equal(result$e, "hello")
})

test_that("normalize_log_df produces identical hashes for type-different but value-same data frames", {
  # simulate in-memory construction (native types)
  current <- data.frame(
    clean_missing_values = TRUE,
    forecast_horizon = 3L,
    hist_start_date = as.Date("2024-01-01"),
    combo_variables = "region--segment",
    stringsAsFactors = FALSE
  )
  # simulate CSV round-trip (auto-guessed types from vroom/read.csv)
  prev <- data.frame(
    clean_missing_values = "TRUE",
    forecast_horizon = 3.0,
    hist_start_date = "2024-01-01",
    combo_variables = "region--segment",
    stringsAsFactors = FALSE
  )
  # raw hashes should differ due to type differences
  expect_false(hash_data(current) == hash_data(prev))
  # normalized hashes should match since values are the same
  expect_equal(
    hash_data(normalize_log_df(current)),
    hash_data(normalize_log_df(prev))
  )
})

test_that("normalize_log_df still detects actual value changes", {
  df1 <- data.frame(
    forecast_horizon = 3,
    date_type = "month",
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    forecast_horizon = 6,
    date_type = "month",
    stringsAsFactors = FALSE
  )
  expect_false(
    hash_data(normalize_log_df(df1)) == hash_data(normalize_log_df(df2))
  )
})
