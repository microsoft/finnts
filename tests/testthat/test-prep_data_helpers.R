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
