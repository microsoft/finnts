# tests/testthat/test-feature_selection.R
# Tests for feature selection functions in feature_selection.R

# -- target_corr_fn tests --

test_that("target_corr_fn returns correlated features", {
  set.seed(123)
  n <- 50
  x1 <- rnorm(n)
  data <- tibble::tibble(
    Combo = rep("A", n),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = n),
    Target = x1 * 2 + rnorm(n, sd = 0.1),
    Feature_A = x1,
    Feature_B = rnorm(n)
  )

  result <- target_corr_fn(data, threshold = 0.5)

  expect_true("term" %in% colnames(result))
  expect_true("Target" %in% colnames(result))
  expect_true("Feature_A" %in% result$term)
})

test_that("target_corr_fn respects threshold", {
  set.seed(42)
  n <- 50
  x1 <- rnorm(n)
  data <- tibble::tibble(
    Combo = rep("A", n),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = n),
    Target = x1 * 2 + rnorm(n, sd = 0.1),
    Feature_A = x1 + rnorm(n, sd = 0.5),
    Feature_B = rnorm(n)
  )

  result_high <- target_corr_fn(data, threshold = 0.9)
  result_low <- target_corr_fn(data, threshold = 0.1)

  expect_true(nrow(result_low) >= nrow(result_high))
})

test_that("target_corr_fn handles single feature", {
  set.seed(123)
  n <- 30
  data <- tibble::tibble(
    Combo = rep("A", n),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = n),
    Target = rnorm(n),
    Feature_A = rnorm(n)
  )

  result <- target_corr_fn(data, threshold = 0.5)
  expect_s3_class(result, "tbl_df")
})

test_that("target_corr_fn handles no correlated features", {
  set.seed(999)
  n <- 100
  data <- tibble::tibble(
    Combo = rep("A", n),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = n),
    Target = rnorm(n),
    Feature_A = rnorm(n),
    Feature_B = rnorm(n)
  )

  result <- target_corr_fn(data, threshold = 0.99)
  expect_s3_class(result, "tbl_df")
})

# -- Variable importance function tests --

test_that("vip_rf_fn produces variable importance scores", {
  set.seed(42)
  data <- tibble::tibble(
    Date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
    Target = rnorm(100, mean = 50, sd = 10),
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )
  data$Target <- data$Target + 2 * data$x1

  result <- vip_rf_fn(data, seed = 42)
  expect_s3_class(result, "tbl_df")
  expect_true("Variable" %in% names(result))
  expect_true("Importance" %in% names(result))
  expect_true(all(result$Importance > 0))
})

test_that("vip_lm_fn produces variable importance scores", {
  set.seed(42)
  data <- tibble::tibble(
    Combo = rep("A", 100),
    Date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
    Target = rnorm(100, mean = 50, sd = 10),
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )
  data$Target <- data$Target + 3 * data$x1

  result <- vip_lm_fn(data, seed = 42)
  expect_s3_class(result, "tbl_df")
  expect_true("Variable" %in% names(result))
  expect_true("Importance" %in% names(result))
  expect_true(all(result$Importance > 0))
})

test_that("vip_cubist_fn produces variable importance scores", {
  set.seed(42)
  data <- tibble::tibble(
    Date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
    Target = rnorm(100, mean = 50, sd = 10),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  data$Target <- data$Target + 5 * data$x1

  result <- vip_cubist_fn(data, seed = 42)
  expect_s3_class(result, "tbl_df")
  expect_true("Variable" %in% names(result))
  expect_true("Importance" %in% names(result))
})

# -- boruta_fn tests --

test_that("boruta_fn selects important features", {
  set.seed(42)
  n <- 100
  x1 <- rnorm(n)
  data <- tibble::tibble(
    Date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    Combo = rep("A", n),
    Target = x1 * 5 + rnorm(n, sd = 0.1),
    Feature_Strong = x1,
    Feature_Noise = rnorm(n)
  )

  result <- boruta_fn(data = data, iterations = 50, seed = 42)
  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true("Feature_Strong" %in% result)
})

test_that("boruta_fn returns character vector", {
  set.seed(123)
  n <- 80
  data <- tibble::tibble(
    Date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    Target = rnorm(n, 50, 10),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  data$Target <- data$Target + 3 * data$x1

  result <- boruta_fn(data = data, iterations = 20, seed = 123)
  expect_type(result, "character")
})

# -- target_corr_fn edge cases --

test_that("target_corr_fn with exact threshold boundary", {
  set.seed(42)
  n <- 200
  x <- rnorm(n)
  data <- tibble::tibble(
    Combo = rep("A", n),
    Date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = n),
    Target = x,
    Feature_Perfect = x
  )

  result <- target_corr_fn(data, threshold = 0.99)
  expect_s3_class(result, "tbl_df")
  expect_true("Feature_Perfect" %in% result$term)
})
