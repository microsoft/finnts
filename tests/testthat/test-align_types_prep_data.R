# Tests for align_types in agent_info.R and additional prep_data.R helpers

test_that("align_types converts integer to match numeric", {
  df1 <- data.frame(x = 1.5, y = "a", stringsAsFactors = FALSE)
  df2 <- data.frame(x = 1L, y = "b", stringsAsFactors = FALSE)
  result <- align_types(df1, df2)
  expect_type(result$x, "double")
  expect_equal(result$x, 1.0)
})

test_that("align_types converts numeric to match integer", {
  df1 <- data.frame(x = 1L, y = "a", stringsAsFactors = FALSE)
  df2 <- data.frame(x = 2.0, y = "b", stringsAsFactors = FALSE)
  result <- align_types(df1, df2)
  expect_type(result$x, "integer")
})

test_that("align_types converts character to match factor", {
  df1 <- data.frame(x = factor("a"))
  df2 <- data.frame(x = "a", stringsAsFactors = FALSE)
  result <- align_types(df1, df2)
  expect_s3_class(result$x, "factor")
})

test_that("align_types converts to match Date", {
  df1 <- data.frame(x = as.Date("2020-01-01"))
  df2 <- data.frame(x = "2020-01-01", stringsAsFactors = FALSE)
  result <- align_types(df1, df2)
  expect_s3_class(result$x, "Date")
  expect_equal(result$x, as.Date("2020-01-01"))
})

test_that("align_types converts to match logical", {
  df1 <- data.frame(x = TRUE)
  df2 <- data.frame(x = 1L)
  result <- align_types(df1, df2)
  expect_type(result$x, "logical")
  expect_true(result$x)
})

test_that("align_types converts to match character", {
  df1 <- data.frame(x = "abc", stringsAsFactors = FALSE)
  df2 <- data.frame(x = 123)
  result <- align_types(df1, df2)
  expect_type(result$x, "character")
  expect_equal(result$x, "123")
})

test_that("align_types only affects shared columns", {
  df1 <- data.frame(x = 1.0, stringsAsFactors = FALSE)
  df2 <- data.frame(x = 1L, extra = "keep", stringsAsFactors = FALSE)
  result <- align_types(df1, df2)
  expect_type(result$x, "double")
  expect_equal(result$extra, "keep")
})

test_that("align_types handles POSIXct conversion", {
  df1 <- data.frame(x = as.POSIXct("2020-01-01 12:00:00", tz = "UTC"))
  df2 <- data.frame(x = "2020-01-01 12:00:00", stringsAsFactors = FALSE)
  result <- align_types(df1, df2)
  expect_s3_class(result$x, "POSIXct")
})

# Tests for get_xregs_future_values_tbl in prep_data.R
test_that("get_xregs_future_values_tbl returns matching columns", {
  data_tbl <- tibble::tibble(
    Combo = rep("A", 10),
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 10),
    xreg1 = 1:10,
    xreg2 = c(rep(NA, 5), 6:10)
  )
  hist_end_date <- as.Date("2020-05-01")
  result <- get_xregs_future_values_tbl(
    data_tbl, c("xreg1", "xreg2"), hist_end_date
  )
  expect_true("Combo" %in% names(result))
  expect_true("Date" %in% names(result))
  expect_true("xreg1" %in% names(result))
  expect_true("xreg2" %in% names(result))
})

test_that("get_xregs_future_values_tbl excludes all-NA future regressors", {
  data_tbl <- tibble::tibble(
    Combo = rep("A", 10),
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 10),
    xreg1 = 1:10,
    xreg2 = c(1:5, rep(NA, 5))
  )
  hist_end_date <- as.Date("2020-05-01")
  result <- get_xregs_future_values_tbl(
    data_tbl, c("xreg1", "xreg2"), hist_end_date
  )
  expect_true("xreg1" %in% names(result))
  expect_false("xreg2" %in% names(result))
})

test_that("get_xregs_future_values_tbl with no external regressors", {
  data_tbl <- tibble::tibble(
    Combo = rep("A", 5),
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 5)
  )
  result <- get_xregs_future_values_tbl(
    data_tbl, character(0), as.Date("2020-03-01")
  )
  expect_equal(names(result), c("Combo", "Date"))
})

# Tests for clean_outliers_missing_values in prep_data.R
test_that("clean_outliers_missing_values with no cleaning", {
  df <- tibble::tibble(
    Target = c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10),
    xreg1 = 1:10
  )
  result <- clean_outliers_missing_values(
    df,
    clean_outliers = FALSE,
    clean_missing_values = FALSE,
    frequency_number = 1,
    external_regressors = "xreg1"
  )
  expect_equal(result$Target, df$Target)
})

test_that("clean_outliers_missing_values imputes missing values", {
  df <- tibble::tibble(
    Target = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10),
    xreg1 = 1:10
  )
  result <- clean_outliers_missing_values(
    df,
    clean_outliers = FALSE,
    clean_missing_values = TRUE,
    frequency_number = 1,
    external_regressors = "xreg1"
  )
  expect_false(any(is.na(result$Target)))
})

test_that("clean_outliers_missing_values cleans outliers", {
  set.seed(123)
  df <- tibble::tibble(
    Target = c(rep(5, 20), 500, rep(5, 9)),
    xreg1 = 1:30
  )
  result <- clean_outliers_missing_values(
    df,
    clean_outliers = TRUE,
    clean_missing_values = FALSE,
    frequency_number = 1,
    external_regressors = "xreg1"
  )
  expect_true("Target_Original" %in% names(result))
  # outlier should be cleaned
  expect_true(result$Target[21] < 500)
})

# Tests for make_stationary in prep_data.R
test_that("make_stationary handles already stationary data", {
  df <- tibble::tibble(
    Combo = rep("A", 30),
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 30),
    Target = rnorm(30)
  )
  result <- make_stationary(df)
  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_true("diff_info" %in% names(result))
  expect_s3_class(result$data, "tbl_df")
  expect_equal(nrow(result$diff_info), 1)
  expect_equal(result$diff_info$Combo, "A")
})

test_that("make_stationary preserves non-numeric columns", {
  df <- tibble::tibble(
    Combo = rep("A", 30),
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 30),
    Target = rnorm(30),
    Category = rep(c("x", "y"), 15)
  )
  result <- make_stationary(df)
  expect_true("Category" %in% names(result$data))
  expect_equal(result$data$Category, df$Category)
})

test_that("make_stationary handles binary numeric columns", {
  df <- tibble::tibble(
    Combo = rep("A", 30),
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 30),
    Target = rnorm(30),
    Binary = rep(c(0, 1), 15)
  )
  result <- make_stationary(df)
  # binary column should not be differenced (only 2 unique values)
  expect_equal(result$data$Binary, df$Binary)
})
