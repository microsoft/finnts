# safe_diff_inv_vec tests

test_that("safe_diff_inv_vec preserves length with no mid-vector NAs", {
  x <- c(NA, 1, 2, 3, 4, 5)
  result <- safe_diff_inv_vec(x, num_diffs = 1, initial_value = 10)

  expect_equal(length(result), length(x))
  expect_equal(result, timetk::diff_inv_vec(x, difference = 1, initial_values = 10))
})

test_that("safe_diff_inv_vec preserves length with mid-vector NAs (single diff)", {
  x <- c(NA, 1, 2, NA, 4, 5)
  result <- safe_diff_inv_vec(x, num_diffs = 1, initial_value = 10)

  expect_equal(length(result), length(x))
  expect_true(is.na(result[4]))
  expect_false(any(is.na(result[-c(1, 4)])))
})

test_that("safe_diff_inv_vec preserves length with mid-vector NAs (double diff)", {
  x <- c(NA, NA, 1, NA, 3, 4)
  result <- safe_diff_inv_vec(x, num_diffs = 2, initial_value = c(10, 20))

  expect_equal(length(result), length(x))
  expect_true(is.na(result[4]))
  expect_false(any(is.na(result[-c(1, 2, 4)])))
})

test_that("safe_diff_inv_vec preserves length with multiple mid-vector NAs", {
  x <- c(NA, 1, NA, 3, NA, 5)
  result <- safe_diff_inv_vec(x, num_diffs = 1, initial_value = 100)

  expect_equal(length(result), length(x))
  expect_true(is.na(result[3]))
  expect_true(is.na(result[5]))
})

test_that("safe_diff_inv_vec handles trailing NAs", {
  x <- c(NA, 1, 2, 3, NA, NA)
  result <- safe_diff_inv_vec(x, num_diffs = 1, initial_value = 50)

  expect_equal(length(result), length(x))
  expect_true(is.na(result[5]))
  expect_true(is.na(result[6]))
})

test_that("safe_diff_inv_vec is identical to diff_inv_vec when no mid NAs", {
  x <- c(NA, 5, 10, 15, 20)
  safe_result <- safe_diff_inv_vec(x, num_diffs = 1, initial_value = 100)
  direct_result <- timetk::diff_inv_vec(x, difference = 1, initial_values = 100)

  expect_equal(safe_result, direct_result)
})

# undifference_forecast tests

test_that("undifference_forecast returns input unchanged when no differencing needed", {
  forecast_data <- tibble::tibble(
    Train_Test_ID = c(1, 1, 1),
    Date = as.Date(c("2024-04-01", "2024-05-01", "2024-06-01")),
    Target = c(NA, NA, NA),
    Forecast = c(10, 20, 30),
    Combo = "A"
  )

  recipe_data <- tibble::tibble(
    Date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    Target = c(5, 10, 15)
  )

  diff_tbl <- tibble::tibble(Diff_Value1 = NA_real_, Diff_Value2 = NA_real_)

  result <- undifference_forecast(forecast_data, recipe_data, diff_tbl)
  expect_equal(result, forecast_data)
})

test_that("undifference_forecast handles recipe data with mid-vector NAs", {
  # Simulate a differenced series where the original data had a missing value,
  # causing a mid-vector NA in the differenced Target column
  dates <- seq.Date(as.Date("2024-01-01"), by = "month", length.out = 8)

  recipe_data <- tibble::tibble(
    Date = dates[1:5],
    Target = c(1, 2, NA, 4, 5) # mid-vector NA from missing original data
  )

  forecast_data <- tibble::tibble(
    Train_Test_ID = c(1, 1, 1),
    Date = dates[6:8],
    Target = c(NA, NA, NA),
    Forecast = c(6, 7, 8),
    Combo = "A"
  )

  diff_tbl <- tibble::tibble(Diff_Value1 = 100, Diff_Value2 = NA_real_)

  result <- undifference_forecast(forecast_data, recipe_data, diff_tbl)

  expect_equal(nrow(result), 3)
  expect_equal(result$Date, dates[6:8])
  expect_true(all(!is.na(result$Forecast)))
})

test_that("undifference_forecast preserves output length with multiple back test splits and mid NAs", {
  dates <- seq.Date(as.Date("2024-01-01"), by = "month", length.out = 12)

  recipe_data <- tibble::tibble(
    Date = dates[1:8],
    Target = c(1, 2, NA, 4, 5, NA, 7, 8) # two mid-vector NAs
  )

  forecast_data <- tibble::tibble(
    Train_Test_ID = c(rep(1, 2), rep(2, 2)),
    Date = c(dates[9:10], dates[11:12]),
    Target = rep(NA, 4),
    Forecast = c(9, 10, 11, 12),
    Combo = "A"
  )

  diff_tbl <- tibble::tibble(Diff_Value1 = 50, Diff_Value2 = NA_real_)

  result <- undifference_forecast(forecast_data, recipe_data, diff_tbl)

  expect_equal(nrow(result), 4)
  expect_equal(sort(unique(result$Train_Test_ID)), c(1, 2))
  expect_true(all(!is.na(result$Forecast)))
})

# undifference_recipe tests

test_that("undifference_recipe returns input unchanged when no differencing needed", {
  recipe_data <- tibble::tibble(
    Date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01")),
    Target = c(10, 20, 30, NA)
  )

  diff_tbl <- tibble::tibble(Diff_Value1 = NA_real_, Diff_Value2 = NA_real_)

  result <- undifference_recipe(recipe_data, diff_tbl, as.Date("2024-03-01"))
  expect_equal(result, recipe_data)
})

test_that("undifference_recipe handles mid-vector NAs without size mismatch", {
  recipe_data <- tibble::tibble(
    Date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01")),
    Target = c(1, 2, NA, 4, NA) # mid-vector NA in historical, NA for future
  )

  diff_tbl <- tibble::tibble(Diff_Value1 = 100, Diff_Value2 = NA_real_)

  result <- undifference_recipe(recipe_data, diff_tbl, as.Date("2024-04-01"))

  # should have 4 historical rows + 1 future row

  expect_equal(nrow(result), 5)
  # future row Target should still be NA
  expect_true(is.na(result$Target[5]))
})

test_that("undifference_recipe handles Target_Original with mid-vector NAs", {
  recipe_data <- tibble::tibble(
    Date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01")),
    Target = c(1, 2, NA, 4, NA),
    Target_Original = c(1, 3, NA, 5, NA) # original values before outlier cleaning
  )

  diff_tbl <- tibble::tibble(Diff_Value1 = 100, Diff_Value2 = NA_real_)

  result <- undifference_recipe(recipe_data, diff_tbl, as.Date("2024-04-01"))

  expect_equal(nrow(result), 5)
  expect_true("Target_Original" %in% colnames(result))
})

test_that("undifference_recipe handles double differencing with mid-vector NAs", {
  recipe_data <- tibble::tibble(
    Date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                     "2024-05-01", "2024-06-01")),
    Target = c(1, 2, 3, NA, 5, NA)
  )

  diff_tbl <- tibble::tibble(Diff_Value1 = 100, Diff_Value2 = 200)

  result <- undifference_recipe(recipe_data, diff_tbl, as.Date("2024-05-01"))

  expect_equal(nrow(result), 6)
})
