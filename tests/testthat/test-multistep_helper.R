# tests/testthat/test-multistep_helper.R

test_that("multi_future_xreg_check returns NULL when no external regressors", {
  data <- tibble::tibble(
    Date = as.Date("2020-01-01"),
    Combo = "A",
    Target = 100
  )

  result <- multi_future_xreg_check(data, NULL)
  expect_null(result)
})

test_that("multi_future_xreg_check returns NULL when no xregs match columns", {
  data <- tibble::tibble(
    Date = as.Date("2020-01-01"),
    Combo = "A",
    Target = 100
  )

  result <- multi_future_xreg_check(data, c("nonexistent_col"))
  expect_null(result)
})

test_that("multi_future_xreg_check returns matching regressors", {
  data <- tibble::tibble(
    Date = as.Date("2020-01-01"),
    Combo = "A",
    Target = 100,
    xreg1 = 50,
    xreg2 = 25
  )

  result <- multi_future_xreg_check(data, c("xreg1", "xreg2"))
  expect_equal(result, c("xreg1", "xreg2"))
})

test_that("multi_future_xreg_check returns only matching regressors", {
  data <- tibble::tibble(
    Date = as.Date("2020-01-01"),
    Combo = "A",
    Target = 100,
    xreg1 = 50
  )

  result <- multi_future_xreg_check(data, c("xreg1", "missing_xreg"))
  expect_equal(result, c("xreg1"))
})

test_that("get_multi_lags returns lags up to and including min above horizon", {
  lag_periods <- c(1, 2, 3, 6, 12)
  forecast_horizon <- 3

  result <- get_multi_lags(lag_periods, forecast_horizon)

  # should include all lags from 1 to the first lag >= forecast_horizon (3)
  expect_equal(result, c(1, 2, 3))
})

test_that("get_multi_lags with horizon matching a lag period", {
  lag_periods <- c(1, 3, 6, 12)
  forecast_horizon <- 6

  result <- get_multi_lags(lag_periods, forecast_horizon)
  expect_equal(result, c(1, 3, 6))
})

test_that("get_multi_lags with horizon smaller than smallest lag", {
  lag_periods <- c(3, 6, 12)
  forecast_horizon <- 1

  result <- get_multi_lags(lag_periods, forecast_horizon)
  expect_equal(result, 3)
})

test_that("multi_feature_selection selects correct columns without future xregs", {
  data <- tibble::tibble(
    Combo = "A",
    Target = 100,
    Date_num = 1,
    lag3_val = 50,
    lag6_val = 40,
    lag12_val = 30
  )

  result <- multi_feature_selection(
    data,
    future_xregs = NULL,
    lag_periods = c(3, 6, 12),
    lag = 6,
    target = FALSE
  )

  expect_true("lag6_val" %in% colnames(result))
  expect_true("lag12_val" %in% colnames(result))
  expect_false("Combo" %in% colnames(result))
  expect_false("Target" %in% colnames(result))
})

test_that("multi_feature_selection includes Combo and Target when target = TRUE", {
  data <- tibble::tibble(
    Combo = "A",
    Target = 100,
    Date_num = 1,
    lag3_val = 50,
    lag6_val = 40,
    lag12_val = 30
  )

  result <- multi_feature_selection(
    data,
    future_xregs = NULL,
    lag_periods = c(3, 6, 12),
    lag = 3,
    target = TRUE
  )

  expect_true("Combo" %in% colnames(result))
  expect_true("Target" %in% colnames(result))
})

test_that("multi_feature_selection includes future xregs when provided", {
  data <- tibble::tibble(
    Combo = "A",
    Target = 100,
    Date_num = 1,
    lag3_val = 50,
    lag6_val = 40,
    xreg1 = 10
  )

  result <- multi_feature_selection(
    data,
    future_xregs = c("xreg1"),
    lag_periods = c(3, 6),
    lag = 3,
    target = TRUE
  )

  expect_true("xreg1" %in% colnames(result))
})
