# tests/testthat/test-timegpt_helpers.R
# Tests for pure helper functions in timegpt_model.R that don't require API credentials

# -- is_azure_url tests --

test_that("is_azure_url returns TRUE for Azure URLs", {

  expect_true(is_azure_url("https://my-resource.azure.com/v1"))
  expect_true(is_azure_url("https://azure.example.com/api"))
  expect_true(is_azure_url("https://my.azure-model.com/"))
  expect_true(is_azure_url("http://test.azure.net/endpoint"))
})

test_that("is_azure_url returns FALSE for non-Azure URLs", {
  expect_false(is_azure_url("https://api.nixtla.io/v1"))
  expect_false(is_azure_url("https://example.com/api"))
  expect_false(is_azure_url("https://my-endpoint.openai.com"))
})

test_that("is_azure_url returns FALSE for NULL or empty string", {
  expect_false(is_azure_url(NULL))
  expect_false(is_azure_url(""))
})

test_that("is_azure_url is case-insensitive", {
  expect_true(is_azure_url("https://my.AZURE.com/api"))
  expect_true(is_azure_url("https://my.Azure.COM/api"))
})

# -- normalize_url tests --

test_that("normalize_url adds trailing slash if missing", {
  expect_warning(
    result <- normalize_url("https://api.example.com"),
    "did not end with"
  )
  expect_equal(result, "https://api.example.com/")
})

test_that("normalize_url keeps trailing slash if present", {
  result <- normalize_url("https://api.example.com/")
  expect_equal(result, "https://api.example.com/")
})

test_that("normalize_url warns when appending slash", {
  expect_warning(normalize_url("https://api.example.com"), "NIXTLA_BASE_URL")
})

# -- get_timegpt_min_size tests --

test_that("get_timegpt_min_size returns correct size for each date_type", {
  expect_equal(get_timegpt_min_size("day"), 300)
  expect_equal(get_timegpt_min_size("week"), 64)
  expect_equal(get_timegpt_min_size("month"), 48)
  expect_equal(get_timegpt_min_size("quarter"), 48)
  expect_equal(get_timegpt_min_size("year"), 48)
})

test_that("get_timegpt_min_size returns 48 for NULL", {
  expect_equal(get_timegpt_min_size(NULL), 48)
})

test_that("get_timegpt_min_size returns 48 for unknown date_type", {
  expect_equal(get_timegpt_min_size("unknown"), 48)
})

# -- is_long_horizon_forecast tests --

test_that("is_long_horizon_forecast returns TRUE for long horizons", {
  expect_true(is_long_horizon_forecast(15, "day"))
  expect_true(is_long_horizon_forecast(105, "week"))
  expect_true(is_long_horizon_forecast(25, "month"))
  expect_true(is_long_horizon_forecast(9, "quarter"))
  expect_true(is_long_horizon_forecast(3, "year"))
})

test_that("is_long_horizon_forecast returns FALSE for short horizons", {
  expect_false(is_long_horizon_forecast(14, "day"))
  expect_false(is_long_horizon_forecast(104, "week"))
  expect_false(is_long_horizon_forecast(24, "month"))
  expect_false(is_long_horizon_forecast(8, "quarter"))
  expect_false(is_long_horizon_forecast(2, "year"))
})

test_that("is_long_horizon_forecast returns FALSE for NULL inputs", {
  expect_false(is_long_horizon_forecast(NULL, "month"))
  expect_false(is_long_horizon_forecast(10, NULL))
  expect_false(is_long_horizon_forecast(NULL, NULL))
})

test_that("is_long_horizon_forecast uses default threshold for unknown date_type", {
  expect_true(is_long_horizon_forecast(25, "unknown"))
  expect_false(is_long_horizon_forecast(24, "unknown"))
})

# -- pad_time_series_data tests --

test_that("pad_time_series_data returns unchanged when min_size is NULL", {
  df <- tibble::tibble(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    Combo = rep("A", 10),
    y = rnorm(10)
  )
  result <- pad_time_series_data(df, "month", min_size = NULL)
  expect_equal(nrow(result), 10)
})

test_that("pad_time_series_data returns unchanged when date_type is NULL", {
  df <- tibble::tibble(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    Combo = rep("A", 10),
    y = rnorm(10)
  )
  result <- pad_time_series_data(df, NULL, min_size = 48)
  expect_equal(nrow(result), 10)
})

test_that("pad_time_series_data returns unchanged when data already meets min_size", {
  df <- tibble::tibble(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 50),
    Combo = rep("A", 50),
    y = rnorm(50)
  )
  result <- pad_time_series_data(df, "month", min_size = 48)
  expect_equal(nrow(result), 50)
})

test_that("pad_time_series_data pads monthly data to min_size", {
  df <- tibble::tibble(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 20),
    Combo = rep("A", 20),
    y = rnorm(20)
  )
  result <- pad_time_series_data(df, "month", min_size = 30)
  expect_true(nrow(result) >= 30)
  expect_true(all(result$Combo == "A"))
})

test_that("pad_time_series_data fills padded rows with zeros", {
  df <- tibble::tibble(
    Date = seq.Date(as.Date("2022-01-01"), by = "month", length.out = 5),
    Combo = rep("A", 5),
    y = rep(100, 5),
    xreg = rep(50, 5)
  )
  result <- pad_time_series_data(df, "month", min_size = 10)
  # padded rows should have y = 0
  padded_rows <- result %>% dplyr::filter(Date < as.Date("2022-01-01"))
  expect_true(all(padded_rows$y == 0))
  expect_true(all(padded_rows$xreg == 0))
})

test_that("pad_time_series_data handles daily data", {
  df <- tibble::tibble(
    Date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 100),
    Combo = rep("A", 100),
    y = rnorm(100)
  )
  result <- pad_time_series_data(df, "day", min_size = 300)
  expect_true(nrow(result) >= 300)
})

test_that("pad_time_series_data handles weekly data", {
  df <- tibble::tibble(
    Date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 30),
    Combo = rep("A", 30),
    y = rnorm(30)
  )
  result <- pad_time_series_data(df, "week", min_size = 64)
  expect_true(nrow(result) >= 64)
})

test_that("pad_time_series_data handles multiple combos", {
  df <- rbind(
    tibble::tibble(
      Date = seq.Date(as.Date("2022-01-01"), by = "month", length.out = 48),
      Combo = rep("A", 48),
      y = rnorm(48)
    ),
    tibble::tibble(
      Date = seq.Date(as.Date("2023-01-01"), by = "month", length.out = 10),
      Combo = rep("B", 10),
      y = rnorm(10)
    )
  )
  result <- pad_time_series_data(df, "month", min_size = 48)
  # Combo A already has 48 rows so no padding, Combo B should be padded
  a_rows <- result %>% dplyr::filter(Combo == "A")
  b_rows <- result %>% dplyr::filter(Combo == "B")
  expect_equal(nrow(a_rows), 48)
  expect_true(nrow(b_rows) >= 48)
})

test_that("pad_time_series_data errors on invalid date_type", {
  df <- tibble::tibble(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 5),
    Combo = rep("A", 5),
    y = rnorm(5)
  )
  expect_error(pad_time_series_data(df, "invalid_type", min_size = 10), "Unsupported date_type")
})

# -- finetune_steps tests --

test_that("finetune_steps creates a dials parameter", {
  param <- finetune_steps()
  expect_s3_class(param, "quant_param")
  expect_equal(param$type, "integer")
})

test_that("finetune_steps uses default range", {
  param <- finetune_steps()
  expect_equal(param$range$lower, 0L)
  expect_equal(param$range$upper, 200L)
})

test_that("finetune_steps accepts custom range", {
  param <- finetune_steps(range = c(10L, 50L))
  expect_equal(param$range$lower, 10L)
  expect_equal(param$range$upper, 50L)
})

# -- finetune_depth tests --

test_that("finetune_depth creates a dials parameter", {
  param <- finetune_depth()
  expect_s3_class(param, "quant_param")
  expect_equal(param$type, "integer")
})

test_that("finetune_depth uses default range", {
  param <- finetune_depth()
  expect_equal(param$range$lower, 1L)
  expect_equal(param$range$upper, 5L)
})

test_that("finetune_depth accepts custom range", {
  param <- finetune_depth(range = c(2L, 10L))
  expect_equal(param$range$lower, 2L)
  expect_equal(param$range$upper, 10L)
})

# -- timegpt_model spec tests --

test_that("timegpt_model creates model spec", {
  model <- timegpt_model(forecast_horizon = 6)
  expect_s3_class(model, "timegpt_model")
  expect_equal(model$mode, "regression")
})

test_that("timegpt_model prints without error", {
  model <- timegpt_model(forecast_horizon = 6)
  expect_output(print(model), "Main Arguments")
})

test_that("timegpt_model update works", {
  model <- timegpt_model(forecast_horizon = 6)
  updated <- update(model, forecast_horizon = 12)
  expect_s3_class(updated, "timegpt_model")
})

test_that("timegpt_model update with fresh=TRUE replaces args", {
  model <- timegpt_model(forecast_horizon = 6, frequency = "M")
  updated <- update(model, forecast_horizon = 3, fresh = TRUE)
  expect_s3_class(updated, "timegpt_model")
})
