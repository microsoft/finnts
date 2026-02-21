# tests/testthat/test-prep_models_helpers.R
# Tests for helper functions in prep_models.R

test_that("get_back_test_spacing returns custom when provided", {
  result <- get_back_test_spacing(3, "month")
  expect_equal(result, 3)
})

test_that("get_back_test_spacing returns 1 for monthly data", {
  result <- get_back_test_spacing(NULL, "month")
  expect_equal(result, 1)
})

test_that("get_back_test_spacing returns 1 for yearly data", {
  result <- get_back_test_spacing(NULL, "year")
  expect_equal(result, 1)
})

test_that("get_back_test_spacing returns 1 for quarterly data", {
  result <- get_back_test_spacing(NULL, "quarter")
  expect_equal(result, 1)
})

test_that("get_back_test_spacing returns 4 for weekly data", {
  result <- get_back_test_spacing(NULL, "week")
  expect_equal(result, 4)
})

test_that("get_back_test_spacing returns 7 for daily data", {
  result <- get_back_test_spacing(NULL, "day")
  expect_equal(result, 7)
})

test_that("get_frequency_number prep_models returns correct values", {
  # prep_models has its own version of get_frequency_number
  # testing through prep_models namespace
  expect_equal(get_frequency_number("year"), 1)
  expect_equal(get_frequency_number("quarter"), 4)
  expect_equal(get_frequency_number("month"), 12)
})

test_that("get_date_type returns correct date type", {
  expect_equal(get_date_type(1), "year")
  expect_equal(get_date_type(4), "quarter")
  expect_equal(get_date_type(12), "month")
  expect_equal(get_date_type(52.17857), "week")
  expect_equal(get_date_type(365.25), "day")
})

test_that("get_seasonal_periods returns correct values", {
  result_year <- get_seasonal_periods("year")
  expect_equal(result_year, c(1, 2, 3))

  result_month <- get_seasonal_periods("month")
  expect_equal(result_month, c(12, 6, 3))

  result_quarter <- get_seasonal_periods("quarter")
  expect_equal(result_quarter, c(4, 2, 8))
})

test_that("get_back_test_scenario_hist_periods computes correctly", {
  input_tbl <- tibble::tibble(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 50),
    Combo = "A",
    Target = rnorm(50)
  )

  result <- get_back_test_scenario_hist_periods(
    input_tbl,
    hist_end_date = as.Date("2024-02-01"),
    forecast_horizon = 3,
    back_test_scenarios = NULL,
    back_test_spacing = 1
  )

  expect_true("hist_periods_80" %in% names(result))
  expect_true("back_test_scenarios" %in% names(result))
  expect_true(result$hist_periods_80 > 0)
  expect_true(result$back_test_scenarios > 0)
})

test_that("get_back_test_scenario_hist_periods uses custom scenarios", {
  input_tbl <- tibble::tibble(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 50),
    Combo = "A",
    Target = rnorm(50)
  )

  result <- get_back_test_scenario_hist_periods(
    input_tbl,
    hist_end_date = as.Date("2024-02-01"),
    forecast_horizon = 3,
    back_test_scenarios = 5,
    back_test_spacing = 1
  )

  expect_equal(result$back_test_scenarios, 6) # scenarios + 1
})

# -- get_frequency_number additional tests --

test_that("get_frequency_number returns correct values for week and day", {
  expect_equal(get_frequency_number("week"), 52.17857)
  expect_equal(get_frequency_number("day"), 365.25)
})

# -- get_seasonal_periods additional tests --

test_that("get_seasonal_periods returns correct values for week", {
  result <- get_seasonal_periods("week")
  expect_equal(result, c(365.25 / 7, (365.25 / 7) / 4, (365.25 / 7) / 12))
})

test_that("get_seasonal_periods returns correct values for day", {
  result <- get_seasonal_periods("day")
  expect_equal(result, c(365.25, 365.25 / 4, 365.25 / 12))
})

# -- get_back_test_scenario_hist_periods edge cases --

test_that("get_back_test_scenario_hist_periods with short data", {
  input_tbl <- tibble::tibble(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 8),
    Combo = "A",
    Target = rnorm(8)
  )

  result <- get_back_test_scenario_hist_periods(
    input_tbl,
    hist_end_date = as.Date("2020-08-01"),
    forecast_horizon = 3,
    back_test_scenarios = NULL,
    back_test_spacing = 1
  )

  expect_type(result, "list")
  expect_true("back_test_scenarios" %in% names(result))
  expect_true(result$back_test_scenarios >= 1)
})
