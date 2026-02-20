# tests/testthat/test-multistep_models.R
# Tests for multistep model spec/print/update/translate functions

# GLMNET Multistep ----

test_that("glmnet_multistep creates model spec", {
  spec <- glmnet_multistep(
    mode = "regression",
    mixture = 0.5,
    penalty = 0.01
  )
  expect_s3_class(spec, "glmnet_multistep")
  expect_equal(spec$mode, "regression")
})

test_that("print.glmnet_multistep outputs text", {
  spec <- glmnet_multistep(mode = "regression")
  expect_output(print(spec), "GLMNET Multistep Horizon")
})

test_that("update.glmnet_multistep updates parameters", {
  spec <- glmnet_multistep(mode = "regression", mixture = 0.5)
  updated <- update(spec, mixture = 0.8)
  expect_s3_class(updated, "glmnet_multistep")
})

test_that("update.glmnet_multistep fresh=TRUE replaces args", {
  spec <- glmnet_multistep(mode = "regression", mixture = 0.5, penalty = 0.01)
  updated <- update(spec, mixture = 0.9, fresh = TRUE)
  expect_s3_class(updated, "glmnet_multistep")
})

test_that("translate.glmnet_multistep sets engine", {
  spec <- glmnet_multistep(mode = "regression") %>%
    parsnip::set_engine("glmnet_multistep_horizon")
  translated <- translate(spec)
  expect_s3_class(translated, "glmnet_multistep")
})

test_that("translate.glmnet_multistep uses default engine", {
  spec <- glmnet_multistep(mode = "regression")
  expect_message(
    translated <- translate(spec),
    "glmnet_multistep_horizon"
  )
  expect_s3_class(translated, "glmnet_multistep")
})

# MARS Multistep ----

test_that("mars_multistep creates model spec", {
  spec <- mars_multistep(
    mode = "regression",
    num_terms = 10,
    prod_degree = 2
  )
  expect_s3_class(spec, "mars_multistep")
  expect_equal(spec$mode, "regression")
})

test_that("print.mars_multistep outputs text", {
  spec <- mars_multistep(mode = "regression")
  expect_output(print(spec), "MARS Multistep Horizon")
})

test_that("update.mars_multistep updates parameters", {
  spec <- mars_multistep(mode = "regression", num_terms = 10)
  updated <- update(spec, num_terms = 20)
  expect_s3_class(updated, "mars_multistep")
})

test_that("update.mars_multistep fresh=TRUE replaces args", {
  spec <- mars_multistep(mode = "regression", num_terms = 10, prod_degree = 2)
  updated <- update(spec, num_terms = 15, fresh = TRUE)
  expect_s3_class(updated, "mars_multistep")
})

test_that("translate.mars_multistep sets engine", {
  spec <- mars_multistep(mode = "regression") %>%
    parsnip::set_engine("mars_multistep_horizon")
  translated <- translate(spec)
  expect_s3_class(translated, "mars_multistep")
})

test_that("translate.mars_multistep uses default engine", {
  spec <- mars_multistep(mode = "regression")
  expect_message(
    translated <- translate(spec),
    "mars_multistep_horizon"
  )
  expect_s3_class(translated, "mars_multistep")
})

# SVM-POLY Multistep ----

test_that("svm_poly_multistep creates model spec", {
  spec <- svm_poly_multistep(
    mode = "regression",
    cost = 1,
    degree = 2
  )
  expect_s3_class(spec, "svm_poly_multistep")
  expect_equal(spec$mode, "regression")
})

test_that("print.svm_poly_multistep outputs text", {
  spec <- svm_poly_multistep(mode = "regression")
  expect_output(print(spec), "SVM-POLY Multistep Horizon")
})

test_that("update.svm_poly_multistep updates parameters", {
  spec <- svm_poly_multistep(mode = "regression", cost = 1)
  updated <- update(spec, cost = 2)
  expect_s3_class(updated, "svm_poly_multistep")
})

test_that("update.svm_poly_multistep fresh=TRUE replaces args", {
  spec <- svm_poly_multistep(mode = "regression", cost = 1, degree = 2)
  updated <- update(spec, cost = 3, fresh = TRUE)
  expect_s3_class(updated, "svm_poly_multistep")
})

test_that("translate.svm_poly_multistep sets engine", {
  spec <- svm_poly_multistep(mode = "regression") %>%
    parsnip::set_engine("svm_poly_multistep_horizon")
  translated <- translate(spec)
  expect_s3_class(translated, "svm_poly_multistep")
})

test_that("translate.svm_poly_multistep uses default engine", {
  spec <- svm_poly_multistep(mode = "regression")
  expect_message(
    translated <- translate(spec),
    "svm_poly_multistep_horizon"
  )
  expect_s3_class(translated, "svm_poly_multistep")
})

# SVM-RBF Multistep ----

test_that("svm_rbf_multistep creates model spec", {
  spec <- svm_rbf_multistep(
    mode = "regression",
    cost = 1,
    rbf_sigma = 0.01
  )
  expect_s3_class(spec, "svm_rbf_multistep")
  expect_equal(spec$mode, "regression")
})

test_that("print.svm_rbf_multistep outputs text", {
  spec <- svm_rbf_multistep(mode = "regression")
  expect_output(print(spec), "SVM-RBF Multistep Horizon")
})

test_that("update.svm_rbf_multistep updates parameters", {
  spec <- svm_rbf_multistep(mode = "regression", cost = 1)
  updated <- update(spec, cost = 2)
  expect_s3_class(updated, "svm_rbf_multistep")
})

test_that("update.svm_rbf_multistep fresh=TRUE replaces args", {
  spec <- svm_rbf_multistep(mode = "regression", cost = 1, rbf_sigma = 0.01)
  updated <- update(spec, cost = 3, fresh = TRUE)
  expect_s3_class(updated, "svm_rbf_multistep")
})

test_that("translate.svm_rbf_multistep sets engine", {
  spec <- svm_rbf_multistep(mode = "regression") %>%
    parsnip::set_engine("svm_rbf_multistep_horizon")
  translated <- translate(spec)
  expect_s3_class(translated, "svm_rbf_multistep")
})

test_that("translate.svm_rbf_multistep uses default engine", {
  spec <- svm_rbf_multistep(mode = "regression")
  expect_message(
    translated <- translate(spec),
    "svm_rbf_multistep_horizon"
  )
  expect_s3_class(translated, "svm_rbf_multistep")
})

# XGBOOST Multistep ----

test_that("xgboost_multistep creates model spec", {
  spec <- xgboost_multistep(
    mode = "regression",
    tree_depth = 6,
    trees = 100
  )
  expect_s3_class(spec, "xgboost_multistep")
  expect_equal(spec$mode, "regression")
})

test_that("print.xgboost_multistep outputs text", {
  spec <- xgboost_multistep(mode = "regression")
  expect_output(print(spec), "XGBoost Multistep Horizon")
})

test_that("update.xgboost_multistep updates parameters", {
  spec <- xgboost_multistep(mode = "regression", trees = 100)
  updated <- update(spec, trees = 200)
  expect_s3_class(updated, "xgboost_multistep")
})

test_that("update.xgboost_multistep fresh=TRUE replaces args", {
  spec <- xgboost_multistep(mode = "regression", trees = 100, tree_depth = 6)
  updated <- update(spec, trees = 300, fresh = TRUE)
  expect_s3_class(updated, "xgboost_multistep")
})

test_that("translate.xgboost_multistep sets engine", {
  spec <- xgboost_multistep(mode = "regression") %>%
    parsnip::set_engine("xgboost_multistep_horizon")
  translated <- translate(spec)
  expect_s3_class(translated, "xgboost_multistep")
})

test_that("translate.xgboost_multistep uses default engine", {
  spec <- xgboost_multistep(mode = "regression")
  expect_message(
    translated <- translate(spec),
    "xgboost_multistep_horizon"
  )
  expect_s3_class(translated, "xgboost_multistep")
})

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
