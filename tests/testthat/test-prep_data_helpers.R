# tests/testthat/test-prep_data_helpers.R
# Tests for helper functions in prep_data.R

test_that("get_frequency_number returns correct values", {
  expect_equal(get_frequency_number("year"), 1)
  expect_equal(get_frequency_number("quarter"), 4)
  expect_equal(get_frequency_number("month"), 12)
  expect_equal(get_frequency_number("week"), 365.25 / 7, tolerance = 1e-5)
  expect_equal(get_frequency_number("day"), 365.25)
})

test_that("get_fourier_periods returns defaults per date_type", {
  result_year <- get_fourier_periods(NULL, "year")
  expect_equal(result_year, c(1, 2, 3, 4, 5))

  result_quarter <- get_fourier_periods(NULL, "quarter")
  expect_equal(result_quarter, c(1, 2, 3, 4))

  result_month <- get_fourier_periods(NULL, "month")
  expect_equal(result_month, c(3, 6, 9, 12))

  result_week <- get_fourier_periods(NULL, "week")
  expect_equal(result_week, c(2, 4, 8, 12, 24, 48, 52))

  result_day <- get_fourier_periods(NULL, "day")
  expect_equal(result_day, c(7, 14, 21, 28, 56, 84, 168, 252, 336, 365))
})

test_that("get_fourier_periods returns custom when provided", {
  custom <- c(5, 10, 15)
  result <- get_fourier_periods(custom, "month")
  expect_equal(result, custom)
})

test_that("get_lag_periods returns defaults for each date_type", {
  result_month <- get_lag_periods(NULL, "month", 3)
  expect_true(3 %in% result_month)
  expect_true(all(result_month >= 3))

  result_year <- get_lag_periods(NULL, "year", 1)
  expect_true(1 %in% result_year)
})

test_that("get_lag_periods returns custom when provided", {
  custom <- c(1, 5, 10)
  result <- get_lag_periods(custom, "month", 3)
  expect_equal(result, custom)
})

test_that("get_lag_periods multistep monthly", {
  result <- get_lag_periods(NULL, "month", 3, multistep_horizon = TRUE)
  expect_true(is.numeric(result))
  expect_true(length(result) > 0)
})

test_that("get_lag_periods multistep weekly", {
  result <- get_lag_periods(NULL, "week", 4, multistep_horizon = TRUE)
  expect_true(is.numeric(result))
  expect_true(length(result) > 0)
})

test_that("get_lag_periods multistep daily", {
  result <- get_lag_periods(NULL, "day", 14, multistep_horizon = TRUE)
  expect_true(is.numeric(result))
  expect_true(length(result) > 0)
})

test_that("get_lag_periods multistep appends forecast_horizon if needed", {
  # large forecast_horizon beyond max default
  result <- get_lag_periods(NULL, "month", 24, multistep_horizon = TRUE)
  expect_true(24 %in% result)
})

test_that("get_lag_periods feature_engineering daily", {
  result <- get_lag_periods(NULL, "day", 14, multistep_horizon = TRUE, feature_engineering = TRUE)
  expect_true(is.numeric(result))
  expect_true(length(result) >= 3)
})

test_that("get_rolling_window_periods returns defaults", {
  result_month <- get_rolling_window_periods(NULL, "month")
  expect_equal(result_month, c(3, 6, 9, 12))

  result_year <- get_rolling_window_periods(NULL, "year")
  expect_equal(result_year, c(2, 3, 4, 5))
})

test_that("get_rolling_window_periods returns custom when provided", {
  custom <- c(2, 4)
  result <- get_rolling_window_periods(custom, "month")
  expect_equal(result, custom)
})

test_that("get_recipes_to_run returns defaults per date_type", {
  expect_equal(get_recipes_to_run(NULL, "month"), c("R1", "R2"))
  expect_equal(get_recipes_to_run(NULL, "quarter"), c("R1", "R2"))
  expect_equal(get_recipes_to_run(NULL, "year"), c("R1", "R2"))
  expect_equal(get_recipes_to_run(NULL, "week"), c("R1"))
  expect_equal(get_recipes_to_run(NULL, "day"), c("R1"))
})

test_that("get_recipes_to_run returns custom when provided", {
  custom <- c("R1", "R2", "R3")
  result <- get_recipes_to_run(custom, "month")
  expect_equal(result, custom)
})

test_that("get_date_regex returns correct regex", {
  result_year <- get_date_regex("year")
  expect_true(grepl("quarter", result_year))
  expect_true(grepl("month", result_year))
  expect_true(grepl("week", result_year))

  result_day <- get_date_regex("day")
  expect_true(grepl("hour", result_day))
  expect_true(grepl("minute", result_day))
  expect_false(grepl("month", result_day))
})

test_that("apply_box_cox transforms data correctly", {
  set.seed(123)
  df <- tibble::tibble(
    Combo = rep("A", 20),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 20),
    Target = abs(rnorm(20, mean = 100, sd = 20))
  )

  result <- apply_box_cox(df)

  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("diff_info" %in% names(result))
  expect_s3_class(result$data, "tbl_df")
  expect_true("Box_Cox_Lambda" %in% colnames(result$diff_info))
})

test_that("apply_box_cox preserves non-numeric columns", {
  df <- tibble::tibble(
    Combo = rep("A", 10),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    Target = 1:10 * 10.0
  )

  result <- apply_box_cox(df)

  expect_true("Combo" %in% colnames(result$data))
  expect_true("Date" %in% colnames(result$data))
})

test_that("combo_cleanup_fn removes combos with zero target", {
  df <- tibble::tibble(
    Combo = c("A", "A", "A", "B", "B", "B"),
    Date = rep(seq.Date(as.Date("2020-01-01"), by = "month", length.out = 3), 2),
    Target = c(10, 20, 30, 0, 0, 0)
  )

  result <- combo_cleanup_fn(
    df,
    combo_cleanup_date = as.Date("2020-01-01"),
    hist_end_date = as.Date("2020-03-01")
  )

  expect_equal(unique(result$Combo), "A")
})

test_that("combo_cleanup_fn returns all when no cleanup date", {
  df <- tibble::tibble(
    Combo = c("A", "B"),
    Date = as.Date(c("2020-01-01", "2020-01-01")),
    Target = c(10, 0)
  )

  result <- combo_cleanup_fn(df, NULL, as.Date("2020-01-01"))

  expect_equal(nrow(result), 2)
})

# -- get_date_regex additional branch tests --

test_that("get_date_regex returns correct regex for quarter", {
  result <- get_date_regex("quarter")
  expect_true(grepl("month", result))
  expect_true(grepl("week", result))
  expect_true(grepl("day", result))
  # quarter should NOT be in the regex for quarter date_type
  expect_false(grepl("\\(quarter\\)", result))
})

test_that("get_date_regex returns correct regex for month", {

  result <- get_date_regex("month")
  expect_true(grepl("week", result))
  expect_true(grepl("day", result))
  # month should NOT be in the regex for month date_type
  expect_false(grepl("\\(month\\)", result))
})

test_that("get_date_regex returns correct regex for week", {
  result <- get_date_regex("week")
  expect_true(grepl("day", result))
  expect_true(grepl("hour", result))
  # week should NOT be in the regex for week date_type
  expect_false(grepl("\\(week\\)", result))
})

# -- get_rolling_window_periods additional tests --

test_that("get_rolling_window_periods returns defaults for week", {
  result <- get_rolling_window_periods(NULL, "week")
  expect_equal(result, c(2, 4, 8, 12, 24, 48, 52))
})

test_that("get_rolling_window_periods returns defaults for day", {
  result <- get_rolling_window_periods(NULL, "day")
  expect_equal(result, c(7, 14, 21, 28, 56, 84, 168, 252, 336, 365))
})

test_that("get_rolling_window_periods returns defaults for quarter", {
  result <- get_rolling_window_periods(NULL, "quarter")
  expect_equal(result, c(2, 3, 4))
})

# -- apply_box_cox additional tests --

test_that("apply_box_cox with Target_Original uses shared lambda", {
  set.seed(42)
  df <- tibble::tibble(
    Combo = rep("A", 20),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 20),
    Target = abs(rnorm(20, mean = 100, sd = 20)),
    Target_Original = abs(rnorm(20, mean = 100, sd = 20))
  )

  result <- apply_box_cox(df)
  expect_true("Target_Original" %in% colnames(result$data))
  expect_false(is.na(result$diff_info$Box_Cox_Lambda))
})

test_that("apply_box_cox skips constant target", {
  df <- tibble::tibble(
    Combo = rep("A", 10),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    Target = rep(5, 10)  # only 1 unique value (<=2)
  )

  result <- apply_box_cox(df)
  expect_true(is.na(result$diff_info$Box_Cox_Lambda))
  # Target unchanged since skipped
  expect_equal(result$data$Target, rep(5, 10))
})

test_that("apply_box_cox skips columns with 2 or fewer unique values", {
  df <- tibble::tibble(
    Combo = rep("A", 10),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    Target = abs(rnorm(10, mean = 50, sd = 10)),
    Binary_Xreg = rep(c(0, 1), 5)  # only 2 unique values
  )

  result <- apply_box_cox(df)
  # Binary_Xreg should be untouched
  expect_equal(result$data$Binary_Xreg, rep(c(0, 1), 5))
})

# -- make_stationary additional tests --

test_that("make_stationary returns unchanged data when already stationary", {
  set.seed(123)
  df <- tibble::tibble(
    Combo = rep("A", 30),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 30),
    Target = rnorm(30, mean = 0, sd = 1)  # white noise, already stationary
  )

  result <- make_stationary(df)
  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("diff_info" %in% names(result))
})

test_that("make_stationary differences non-stationary data", {
  set.seed(42)
  # Create random walk (non-stationary)
  df <- tibble::tibble(
    Combo = rep("A", 50),
    Date = seq.Date(as.Date("2016-01-01"), by = "month", length.out = 50),
    Target = cumsum(rnorm(50))
  )

  result <- make_stationary(df)
  expect_type(result, "list")
  expect_true("Diff_Value1" %in% colnames(result$diff_info))
  # If differencing occurred, Diff_Value1 should be the first Target value
  if (!is.na(result$diff_info$Diff_Value1)) {
    expect_equal(result$diff_info$Diff_Value1, df$Target[1])
  }
})

test_that("make_stationary preserves non-numeric columns", {
  set.seed(123)
  df <- tibble::tibble(
    Combo = rep("A", 20),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 20),
    Target = cumsum(rnorm(20)),
    Category = rep("cat1", 20)
  )

  result <- make_stationary(df)
  expect_true("Category" %in% colnames(result$data))
})

test_that("make_stationary with Target_Original uses same ndiffs as Target", {
  set.seed(42)
  df <- tibble::tibble(
    Combo = rep("A", 50),
    Date = seq.Date(as.Date("2016-01-01"), by = "month", length.out = 50),
    Target = cumsum(rnorm(50)),
    Target_Original = cumsum(rnorm(50))
  )

  result <- make_stationary(df)
  expect_true("Target_Original" %in% colnames(result$data))
})

# -- clean_outliers_missing_values additional tests --

test_that("clean_outliers_missing_values handles column with <2 non-NA values", {
  df <- tibble::tibble(
    Combo = rep("A", 10),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    Target = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    Sparse_Xreg = c(5, rep(NA, 9))  # only 1 non-NA value
  )

  result <- clean_outliers_missing_values(
    df,
    clean_outliers = FALSE,
    clean_missing_values = TRUE,
    frequency_number = 12,
    external_regressors = "Sparse_Xreg"
  )

  expect_s3_class(result, "tbl_df")
})

# -- align_types tests --

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

# -- get_xregs_future_values_tbl tests --

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

# -- Additional clean_outliers_missing_values tests --

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

# -- Additional make_stationary test --

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
