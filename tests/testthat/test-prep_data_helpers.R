# tests/testthat/test-prep_data_helpers.R
# Unit tests for pure helper functions in R/prep_data.R and R/input_checks.R

# * get_frequency_number ----

test_that("get_frequency_number returns correct values for all date types", {
  expect_equal(get_frequency_number("year"), 1)
  expect_equal(get_frequency_number("quarter"), 4)
  expect_equal(get_frequency_number("month"), 12)
  expect_equal(get_frequency_number("week"), 365.25 / 7)
  expect_equal(get_frequency_number("day"), 365.25)
})

# * get_fourier_periods ----

test_that("get_fourier_periods returns user-supplied periods unchanged", {
  user_periods <- c(2, 4, 6)
  expect_equal(get_fourier_periods(user_periods, "month"), user_periods)
})

test_that("get_fourier_periods returns defaults for each date type", {
  expect_equal(get_fourier_periods(NULL, "year"), c(1, 2, 3, 4, 5))
  expect_equal(get_fourier_periods(NULL, "quarter"), c(1, 2, 3, 4))
  expect_equal(get_fourier_periods(NULL, "month"), c(3, 6, 9, 12))
  expect_true(length(get_fourier_periods(NULL, "week")) > 0)
  expect_true(length(get_fourier_periods(NULL, "day")) > 0)
})

# * get_lag_periods ----

test_that("get_lag_periods returns user-supplied periods unchanged", {
  user_lags <- c(1, 3, 6)
  expect_equal(get_lag_periods(user_lags, "month", 3), user_lags)
})

test_that("get_lag_periods non-multistep: only returns lags >= forecast_horizon", {
  result <- get_lag_periods(NULL, "month", forecast_horizon = 6)
  expect_true(all(result >= 6))
})

test_that("get_lag_periods non-multistep: always includes forecast_horizon", {
  result <- get_lag_periods(NULL, "year", forecast_horizon = 5)
  expect_true(5 %in% result)
})

test_that("get_lag_periods non-multistep: returns unique values", {
  result <- get_lag_periods(NULL, "month", forecast_horizon = 12)
  expect_equal(result, unique(result))
})

test_that("get_lag_periods multistep: appends forecast_horizon when larger than defaults", {
  result <- get_lag_periods(NULL, "month", forecast_horizon = 24, multistep_horizon = TRUE)
  expect_true(24 %in% result)
})

test_that("get_lag_periods multistep month: includes standard monthly lags", {
  result <- get_lag_periods(NULL, "month", forecast_horizon = 3, multistep_horizon = TRUE)
  expect_true(all(result > 0))
  expect_true(length(result) >= 3)
})

# * get_rolling_window_periods ----

test_that("get_rolling_window_periods returns user-supplied periods unchanged", {
  user_periods <- c(2, 4)
  expect_equal(get_rolling_window_periods(user_periods, "month"), user_periods)
})

test_that("get_rolling_window_periods returns defaults for each date type", {
  for (dt in c("year", "quarter", "month", "week", "day")) {
    result <- get_rolling_window_periods(NULL, dt)
    expect_true(is.numeric(result), info = paste("Failed for date_type:", dt))
    expect_true(length(result) > 0, info = paste("Empty result for date_type:", dt))
  }
})

# * get_date_regex ----

test_that("get_date_regex returns a non-empty string for each date type", {
  for (dt in c("year", "quarter", "month", "week", "day")) {
    result <- get_date_regex(dt)
    expect_type(result, "character")
    expect_true(nchar(result) > 0, info = paste("Empty regex for date_type:", dt))
  }
})

test_that("get_date_regex year regex excludes finer-grained features", {
  regex <- get_date_regex("year")
  expect_match(regex, "month")
  expect_match(regex, "week")
  expect_match(regex, "day")
})

test_that("get_date_regex day regex is least restrictive (no day filter)", {
  regex <- get_date_regex("day")
  expect_false(grepl("day", regex))
})

# * check_input_type ----

test_that("check_input_type passes for correct type", {
  expect_no_error(check_input_type("my_var", 42L, "integer"))
  expect_no_error(check_input_type("my_var", "hello", "character"))
  expect_no_error(check_input_type("my_var", TRUE, "logical"))
})

test_that("check_input_type errors for wrong type with informative message", {
  expect_error(
    check_input_type("forecast_horizon", "three", "numeric"),
    "invalid type.*forecast_horizon.*numeric"
  )
  expect_error(
    check_input_type("my_flag", 1L, "logical"),
    "invalid type.*my_flag.*logical"
  )
})

test_that("check_input_type errors when value not in expected_value set", {
  expect_error(
    check_input_type("date_type", "biweekly", "character", c("day", "week", "month", "quarter", "year")),
    "invalid value.*date_type"
  )
})

test_that("check_input_type passes when value is in expected_value set", {
  expect_no_error(
    check_input_type("date_type", "month", "character", c("day", "week", "month", "quarter", "year"))
  )
})

test_that("check_input_type accepts multiple allowed types", {
  expect_no_error(check_input_type("x", 1.5, c("numeric", "integer")))
  expect_no_error(check_input_type("x", 1L, c("numeric", "integer")))
})
