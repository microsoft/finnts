skip_on_cran()

# TimesFM Model Unit Tests (no API call needed)

test_that("TimesFM model can be initialized", {
  model <- timesfm_model(forecast_horizon = 6)

  expect_s3_class(model, "timesfm_model")
  expect_equal(model$mode, "regression")
})

test_that("TimesFM fit function stores training data and parameters", {
  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    Combo = "1"
  )
  y <- rnorm(10, mean = 100, sd = 10)

  fit <- timesfm_model_fit_impl(x, y, forecast_horizon = 3, frequency = 12)

  expect_s3_class(fit, "timesfm_model_fit")
  expect_true("train_data" %in% names(fit))
  expect_equal(nrow(fit$train_data), 10)
  expect_equal(fit$forecast_horizon, 3)
  expect_equal(fit$frequency, 12)
  expect_true("y" %in% colnames(fit$train_data))
  expect_equal(fit$train_data$y, y)
})

test_that("TimesFM fit excludes external regressors", {
  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    Combo = "1",
    temperature_original = rnorm(10, mean = 20, sd = 5)
  )
  y <- rnorm(10, mean = 100, sd = 10)

  fit <- timesfm_model_fit_impl(x, y, forecast_horizon = 3, frequency = 12)

  expect_false("temperature_original" %in% colnames(fit$train_data))
  expect_true(all(c("Date", "Combo", "y") %in% colnames(fit$train_data)))
})

test_that("TimesFM fit function rejects empty data", {
  x <- data.frame(Date = as.Date(character(0)), Combo = character(0))
  y <- numeric(0)

  expect_error(
    timesfm_model_fit_impl(x, y, forecast_horizon = 3),
    "at least one row"
  )
})

# Environment variable helpers

test_that("get_timesfm_env errors when variable is not set", {
  withr::local_envvar(TIMESFM_API_URL = "")
  expect_error(get_timesfm_env("TIMESFM_API_URL"), "not set")
})

test_that("get_timesfm_env returns value when variable is set", {
  withr::local_envvar(TIMESFM_API_URL = "https://example.com/api")
  expect_equal(get_timesfm_env("TIMESFM_API_URL"), "https://example.com/api")
})

# Frequency mapping

test_that("map_timesfm_freq maps finnts frequency numbers to TimesFM freq strings", {
  expect_equal(map_timesfm_freq(365.25), "D")
  expect_equal(map_timesfm_freq(52.17857), "W")
  expect_equal(map_timesfm_freq(12), "MS")
  expect_equal(map_timesfm_freq(4), "QS")
  expect_equal(map_timesfm_freq(1), "YS")
})

test_that("map_timesfm_freq defaults to MS for unknown frequency", {
  expect_equal(map_timesfm_freq(999), "MS")
  expect_equal(map_timesfm_freq(NULL), "MS")
})

# Payload building

test_that("build_timesfm_payload creates correct API structure", {
  train_df <- data.frame(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = "series_A",
    y = c(10, 20, 30)
  )

  payload <- build_timesfm_payload(train_df, horizon = 3, freq = "MS")

  expect_true(is.list(payload))
  expect_equal(payload$horizon, 3)
  expect_equal(payload$freq, "MS")
  expect_true(is.data.frame(payload$data) || is.list(payload$data))
  expect_true("unique_id" %in% names(payload$data))
  expect_true("ds" %in% names(payload$data))
  expect_true("y" %in% names(payload$data))
  expect_false("Combo" %in% names(payload$data))
  expect_false("Date" %in% names(payload$data))
})

test_that("build_timesfm_payload converts dates to character", {
  train_df <- data.frame(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = "series_A",
    y = c(10, 20, 30)
  )

  payload <- build_timesfm_payload(train_df, horizon = 3, freq = "MS")

  expect_true(is.character(payload$data$ds))
  expect_equal(payload$data$ds, c("2020-01-01", "2020-02-01", "2020-03-01"))
})

# Response parsing

test_that("parse_timesfm_response parses successful response", {
  mock_body <- '[
    {"unique_id": "s1", "ds": "2026-01-01", "forecast": 100.5},
    {"unique_id": "s1", "ds": "2026-02-01", "forecast": 101.3}
  ]'

  mock_response <- structure(
    list(
      status_code = 200L,
      content = charToRaw(mock_body),
      headers = list(`content-type` = "application/json")
    ),
    class = "response"
  )

  result <- parse_timesfm_response(mock_response)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("forecast" %in% colnames(result))
})

test_that("parse_timesfm_response errors on non-2xx status", {
  mock_response <- structure(
    list(
      status_code = 500L,
      content = charToRaw('{"error": "internal"}'),
      headers = list(`content-type` = "application/json")
    ),
    class = "response"
  )

  expect_error(parse_timesfm_response(mock_response), "HTTP 500")
})

# Print and update methods

test_that("print.timesfm_model runs without error", {
  model <- timesfm_model(forecast_horizon = 6, frequency = 12)
  expect_output(print(model))
})

test_that("update.timesfm_model updates forecast_horizon", {
  model <- timesfm_model(forecast_horizon = 6, frequency = 12)
  updated <- update(model, forecast_horizon = 12)
  expect_s3_class(updated, "timesfm_model")
})

# Model list tests

test_that("timesfm appears in list_models()", {
  expect_true("timesfm" %in% list_models())
})

test_that("timesfm appears in list_foundation_models()", {
  expect_true("timesfm" %in% list_foundation_models())
})

test_that("timesfm does NOT appear in list_multivariate_models()", {
  expect_false("timesfm" %in% list_multivariate_models())
})

test_that("timesfm does NOT appear in list_global_models()", {
  expect_false("timesfm" %in% list_global_models())
})

# Workflow builder

test_that("timesfm workflow builder creates a valid workflow", {
  train_data <- data.frame(
    Target = rnorm(24),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "1"
  )

  wf <- timesfm(train_data = train_data, horizon = 6, frequency = 12)

  expect_s3_class(wf, "workflow")
})

# Input validation tests

test_that("validate_timesfm_inputs rejects missing train columns", {
  bad_train <- data.frame(Date = Sys.Date(), y = 1)
  new_data <- data.frame(Date = Sys.Date(), Combo = "A")
  expect_error(validate_timesfm_inputs(bad_train, new_data, 3), "Combo")
})

test_that("validate_timesfm_inputs rejects missing new_data columns", {
  train <- data.frame(Date = Sys.Date(), Combo = "A", y = 1)
  bad_new <- data.frame(Date = Sys.Date())
  expect_error(validate_timesfm_inputs(train, bad_new, 3), "Combo")
})

test_that("validate_timesfm_inputs rejects empty train_df", {
  train <- data.frame(Date = as.Date(character()), Combo = character(), y = numeric())
  new_data <- data.frame(Date = Sys.Date(), Combo = "A")
  expect_error(validate_timesfm_inputs(train, new_data, 3), "no rows")
})

test_that("validate_timesfm_inputs rejects invalid horizon", {
  train <- data.frame(Date = Sys.Date(), Combo = "A", y = 1)
  new_data <- data.frame(Date = Sys.Date(), Combo = "A")
  expect_error(validate_timesfm_inputs(train, new_data, -1), "positive integer")
  expect_error(validate_timesfm_inputs(train, new_data, "abc"), "positive integer")
})

# Float target values

test_that("TimesFM fit stores float target values correctly", {
  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
    Combo = "1"
  )
  y <- c(100.123, 200.456, 300.789, 99.001, 50.555, 75.999,
         123.456, 789.012, 0.001, 999.999, 42.42, 1.1)

  fit <- timesfm_model_fit_impl(x, y, forecast_horizon = 3, frequency = 12)

  expect_equal(fit$train_data$y, y)
  expect_true(is.numeric(fit$train_data$y))
  expect_true(all(fit$train_data$y == y))
})

# Integration tests (require real API credentials)

test_that("TimesFM fit and predict works with real API", {
  skip_on_cran()
  skip_if(
    !nzchar(Sys.getenv("TIMESFM_API_URL")) || !nzchar(Sys.getenv("TIMESFM_API_TOKEN")),
    "TIMESFM_API_URL and TIMESFM_API_TOKEN not set"
  )

  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "series_1"
  )
  y <- sin(seq_len(24) * pi / 6) * 100 + 500

  fit <- timesfm_model_fit_impl(x, y, forecast_horizon = 6, frequency = 12)

  new_data <- data.frame(
    Date = seq.Date(as.Date("2022-01-01"), by = "month", length.out = 6),
    Combo = "series_1"
  )

  preds <- timesfm_model_predict_impl(fit, new_data)

  expect_length(preds, 6)
  expect_true(all(is.numeric(preds)))
  expect_true(all(is.finite(preds)))
})

test_that("TimesFM workflow fit with external regressors present", {
  skip_on_cran()
  skip_if(
    !nzchar(Sys.getenv("TIMESFM_API_URL")) || !nzchar(Sys.getenv("TIMESFM_API_TOKEN")),
    "TIMESFM_API_URL and TIMESFM_API_TOKEN not set"
  )

  train_data <- data.frame(
    Target = sin(seq_len(24) * pi / 6) * 100 + 500,
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    Combo = "series_1",
    temperature_original = rnorm(24, 20, 5)
  )

  wf <- timesfm(train_data = train_data, horizon = 6, frequency = 12)
  wf_fit <- generics::fit(wf, train_data)

  expect_s3_class(wf_fit, "workflow")

  fit_obj <- workflows::extract_fit_parsnip(wf_fit)$fit
  expect_false("temperature_original" %in% colnames(fit_obj$train_data))
})
