make_arima_fast_fixture <- function(rows = 140L) {
  set.seed(123)
  index <- seq_len(rows)
  data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = rows),
    Combo = "series",
    Target = 50 + 0.1 * index + 4 * sin(2 * pi * index / 7) + stats::rnorm(rows)
  )
}

test_that("daily arima uses the adaptive engine and persists predictions", {
  training_data <- make_arima_fast_fixture()
  daily_workflow <- arima(training_data, frequency = 365.25, horizon = 14)

  expect_identical(
    workflows::extract_spec_parsnip(daily_workflow)$engine,
    "arima_fast"
  )
  non_daily_engines <- vapply(
    c(365.25 / 7, 12, 4, 1),
    function(frequency) {
      workflows::extract_spec_parsnip(
        arima(training_data, frequency = frequency, horizon = 14)
      )$engine
    },
    character(1)
  )
  expect_identical(non_daily_engines, rep("auto_arima", 4))

  fitted_workflow <- generics::fit(daily_workflow, training_data)
  future_data <- data.frame(
    Date = seq.Date(max(training_data$Date) + 1, by = "day", length.out = 14),
    Combo = "series"
  )
  predictions <- predict(fitted_workflow, future_data)

  expect_equal(nrow(predictions), 14)
  expect_true(all(is.finite(predictions$.pred)))

  fitted_engine <- workflows::extract_fit_parsnip(fitted_workflow)$fit
  expect_s3_class(fitted_engine, "arima_fast_fit_impl")
  expect_false(fitted_engine$fallback)
  expect_equal(fitted_engine$model$arma[5], 1)
  expect_true(all(is.finite(fitted_engine$candidate_scores$score)))

  model_path <- tempfile(fileext = ".rds")
  saveRDS(fitted_workflow, model_path)
  restored_predictions <- predict(readRDS(model_path), future_data)
  expect_equal(restored_predictions, predictions, tolerance = 0)

  summary <- summarize_model_arima(fitted_workflow)
  expected_summary_names <- c(
    "actual_engine", "strategy", "transformed_order_str",
    "effective_order_str", "validation_wmape", "candidate_count", "fallback"
  )
  expect_identical(unique(summary$engine), "arima_fast")
  expect_true(all(expected_summary_names %in% summary$name))
  expect_identical(
    summary$value[summary$name == "actual_engine"],
    "arima_fast"
  )

  parsnip_fit <- workflows::extract_fit_parsnip(fitted_workflow)
  parsnip_fit$fit$strategy <- "fourier_medium"
  parsnip_fit$fit$seasonal_lag <- 0L
  parsnip_fit$fit$week_k <- 3L
  parsnip_fit$fit$year_k <- 6L
  fourier_summary <- summarize_model_arima_fast(
    parsnip_fit,
    tibble::tibble(),
    tibble::tibble(),
    tibble::tibble(),
    digits = 6
  )
  expect_match(
    fourier_summary$value[fourier_summary$name == "effective_order_str"],
    "Fourier\\(weekly K=3, annual K=6\\)"
  )

  parsnip_fit$fit$strategy <- "difference_365"
  parsnip_fit$fit$seasonal_lag <- 365L
  parsnip_fit$fit$week_k <- 0L
  parsnip_fit$fit$year_k <- 0L
  annual_summary <- summarize_model_arima_fast(
    parsnip_fit,
    tibble::tibble(),
    tibble::tibble(),
    tibble::tibble(),
    digits = 6
  )
  expect_match(
    annual_summary$value[annual_summary$name == "effective_order_str"],
    "\\(0,1,0\\)\\[365\\]"
  )
})

test_that(
  "annual difference reconstruction supports horizons beyond one cycle",
  {
    rows <- 760L
    index <- seq_len(rows)
    dates <- seq.Date(as.Date("2022-01-01"), by = "day", length.out = rows)
    target <- 100 + 0.03 * index + 10 * sin(2 * pi * index / 365)
    strategy <- arima_fast_strategy_specs(rows)[[5]]

    expect_identical(strategy$name, "difference_365")
    candidate <- arima_fast_fit_candidate(target, dates, strategy)
    future_dates <- seq.Date(max(dates) + 1, by = "day", length.out = 370)
    predictions <- arima_fast_predict_candidate(candidate, future_dates)

    expect_length(predictions, 370)
    expect_true(all(is.finite(predictions)))
    expect_equal(candidate$model$arma[5], 1)
    expect_identical(candidate$seasonal_lag, 365L)
  }
)

test_that("adaptive arima fallback is finite and deterministic", {
  dates <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = 20)
  target <- seq(10, 29)
  fallback <- arima_fast_fallback(target, dates)
  future_data <- data.frame(
    Date = seq.Date(max(dates) + 1, by = "day", length.out = 5)
  )

  first <- arima_fast_model_predict_impl(fallback, future_data)
  second <- arima_fast_model_predict_impl(fallback, future_data)

  expect_identical(fallback$strategy, "drift_fallback")
  expect_true(fallback$fallback)
  expect_equal(first, second, tolerance = 0)
  expect_true(all(is.finite(first)))
})

test_that("short constant histories fall back without failing", {
  dates <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = 15)
  fitted <- arima_fast_model_fit_impl(
    data.frame(Date = dates),
    rep(10, length(dates)),
    forecast_horizon = 5,
    frequency = 365.25
  )
  predictions <- arima_fast_model_predict_impl(
    fitted,
    data.frame(Date = seq.Date(max(dates) + 1, by = "day", length.out = 5))
  )

  expect_true(fitted$fallback)
  expect_identical(fitted$strategy, "drift_fallback")
  expect_equal(predictions, rep(10, 5), tolerance = 0)
})

test_that("adaptive arima rejects invalid date inputs", {
  training_data <- make_arima_fast_fixture(30)
  training_data$Date[2] <- training_data$Date[1]

  expect_error(
    arima_fast_model_fit_impl(
      training_data["Date"],
      training_data$Target,
      forecast_horizon = 7,
      frequency = 365.25
    ),
    "unique, consecutive daily dates"
  )

  valid_data <- make_arima_fast_fixture(40)
  fitted <- arima_fast_model_fit_impl(
    valid_data["Date"],
    valid_data$Target,
    forecast_horizon = 7,
    frequency = 365.25
  )
  expect_error(
    arima_fast_model_predict_impl(
      fitted,
      data.frame(Date = max(valid_data$Date))
    ),
    "after the training history"
  )
})
