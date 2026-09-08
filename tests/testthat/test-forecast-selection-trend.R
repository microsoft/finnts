test_that("supported compounding keeps its quality and eligibility over long horizons", {
  actuals <- 100 * 1.25^(0:11)
  for (forecast_horizon in c(16L, 24L)) {
    expected <- utils::tail(actuals, 1) * 1.25^seq_len(forecast_horizon)
    fixture <- make_selection_case(
      actuals = actuals,
      futures = list(growth = expected, flat = rep(utils::tail(actuals, 1), forecast_horizon)),
      errors = c(growth = 0.01, flat = 0.013), date_type = "year"
    )
    original <- fixture
    selection <- do.call(select_forecast_candidate, fixture)
    growth <- selection$rankings[selection$rankings$Model_ID == "growth", ]

    expect_true(growth$Eligible, info = paste("horizon", forecast_horizon))
    expect_equal(growth$Checks[[1]][["level"]], 0, tolerance = 1e-10)
    expect_equal(growth$Checks[[1]][["trend"]], 0, tolerance = 1e-10)
    expect_identical(selection$selected_id, "growth")
    expect_identical(fixture, original)
  }
})

test_that("multi-year additive and proportional paths retain supported seasonality", {
  for (date_type in c("month", "quarter")) {
    period <- if (date_type == "month") 12L else 4L
    history_size <- 5L * period
    for (forecast_horizon in period * 1:3) {
      for (mode in c("additive", "log")) {
        positions <- seq_len(history_size + forecast_horizon)
        seasonal <- cos(2 * pi * (positions - 0.5) / period)
        values <- if (mode == "additive") {
          100 + 2 * positions + 10 * seasonal
        } else 100 * exp(0.04 * positions + 0.1 * seasonal)
        actuals <- values[seq_len(history_size)]
        expected <- utils::tail(values, forecast_horizon)
        fixture <- make_selection_case(actuals,
          futures = list(growth = expected, flat = rep(utils::tail(actuals, 1), forecast_horizon)),
          errors = c(growth = 0.01, flat = 0.013), date_type = date_type)
        reference <- forecast_reference(fixture$history, forecast_horizon, fixture$context)
        selection <- do.call(select_forecast_candidate, fixture)
        growth <- selection$rankings[selection$rankings$Model_ID == "growth", ]

        expect_identical(reference$trend$mode, mode)
        expect_equal(reference$trend$projection, if (mode == "log") {
          log(expected) - log(max(actuals))
        } else expected / max(actuals), tolerance = 1e-10)
        expect_true(growth$Eligible)
        expect_equal(growth$Checks[[1]][["level"]], 0, tolerance = 1e-10)
        expect_equal(growth$Checks[[1]][["trend"]], 0, tolerance = 1e-10)
        expect_identical(selection$selected_id, "growth")
      }
    }
  }
})

test_that("supported seasonal phases align at offset cutoffs and partial horizons", {
  detrended_profile <- function(pattern) {
    centered_time <- seq_along(pattern) - mean(seq_along(pattern))
    residual <- pattern - mean(pattern) -
      centered_time * sum(centered_time * pattern) / sum(centered_time^2)
    phases <- rep(seq_len(12), length.out = length(pattern))
    vapply(seq_len(12), function(phase) stats::median(residual[phases == phase]), numeric(1))
  }
  for (mode in c("additive", "log")) {
    for (forecast_horizon in c(3L, 12L, 24L)) {
      history_size <- 50L
      positions <- seq_len(history_size + forecast_horizon)
      seasonal <- cos(2 * pi * (positions - 0.5) / 12)
      values <- if (mode == "additive") {
        100 + positions + 20 * seasonal
      } else 100 * exp(0.02 * positions + 0.2 * seasonal)
      future_positions <- history_size + seq_len(forecast_horizon)
      reversed <- if (mode == "additive") {
        100 + future_positions - 20 * seasonal[future_positions]
      } else 100 * exp(0.02 * future_positions - 0.2 * seasonal[future_positions])
      fixture <- make_selection_case(values[seq_len(history_size)],
        futures = list(intact = utils::tail(values, forecast_horizon), reversed = reversed),
        errors = c(intact = 0.013, reversed = 0.01))
      selection <- do.call(select_forecast_candidate, fixture)
      intact <- selection$rankings[selection$rankings$Model_ID == "intact", ]
      reversed_score <- selection$rankings[selection$rankings$Model_ID == "reversed", ]
      expected_fidelity <- 0
      if (forecast_horizon >= 12) {
        window_size <- min(history_size, max(36, 2 * forecast_horizon))
        complete_size <- 12 * floor(window_size / 12)
        historical_amplitude <- diff(range(detrended_profile(
          utils::tail(seasonal[seq_len(history_size)], complete_size))))
        future_amplitude <- diff(range(detrended_profile(seasonal[future_positions])))
        expected_fidelity <- max(0, abs(future_amplitude - historical_amplitude) /
          historical_amplitude - 0.05)
      }

      expect_true(intact$Eligible)
      expect_equal(intact$Checks[[1]][["level"]], 0, tolerance = 1e-10)
      expect_equal(intact$Seasonal_Fidelity, expected_fidelity, tolerance = 1e-10)
      expect_true("seasonal_phase" %in% reversed_score$Reasons[[1]])
      expect_identical(selection$selected_id, "intact")
    }
  }
})

test_that("unsupported trend evidence retains legacy reference scoring", {
  actuals <- 100 + seq_len(48)
  base <- make_selection_case(actuals, futures = list(only = 149:160), errors = c(only = 0.03))
  fixtures <- list(
    flat = make_selection_case(rep(100, 48)),
    two_cycles = make_selection_case(101:124),
    zero = make_selection_case(rep(0, 48)),
    recent_step = make_selection_case(c(rep(100, 40), rep(200, 8))),
    unstable = make_selection_case(100 + rep(c(0, 2, -3, 1), 12)),
    irregular = base, missing = base, nonfinite = base,
    unobserved = base, unknown_observed = base
  )
  fixtures$irregular$history$Date[20] <- fixtures$irregular$history$Date[20] + 1
  fixtures$missing$history$Target[20] <- NA_real_
  fixtures$nonfinite$history$Target[20] <- Inf
  fixtures$unobserved$history$Observed <- TRUE
  fixtures$unobserved$history$Observed[20] <- FALSE
  fixtures$unknown_observed$history$Observed <- TRUE
  fixtures$unknown_observed$history$Observed[20] <- NA

  for (fixture in fixtures) {
    horizon <- length(unique(fixture$forecasts$Date))
    reference <- forecast_reference(fixture$history, horizon, fixture$context)
    expect_null(reference$trend)
    legacy <- reference
    legacy$trend <- NULL
    candidate <- fixture$forecasts$Forecast[fixture$forecasts$Model_ID == fixture$forecasts$Model_ID[1]]
    expect_identical(forecast_path_risk(candidate, reference), forecast_path_risk(candidate, legacy))
  }
})

test_that("signed histories and nonpositive log forecasts remain finite and unclipped", {
  for (actuals in list(-100 - seq_len(36), seq(-0.02, 0.015, length.out = 36))) {
    expected <- utils::tail(actuals, 1) + stats::median(diff(actuals)) * 1:12
    fixture <- make_selection_case(actuals,
      futures = list(only = expected), errors = c(only = 0.03), date_type = "year")
    reference <- forecast_reference(fixture$history, 12, fixture$context)
    expect_identical(reference$trend$mode, "additive")
    expect_equal(forecast_path_risk(expected, reference)$components[["level"]], 0)
  }
  actuals <- 100 * 1.1^(0:35)
  for (invalid_value in c(0, -1)) {
    fixture <- make_selection_case(actuals,
      futures = list(only = c(invalid_value, utils::tail(actuals, 1) * 1.1^(2:12))),
      errors = c(only = 0.03), date_type = "year")
    original <- fixture$forecasts
    expect_warning(selection <- do.call(select_forecast_candidate, fixture), NA)
    expect_true(selection$rankings$Eligible)
    expect_true("level_deviation" %in% selection$rankings$Reasons[[1]])
    expect_gte(selection$rankings$Risk, 1)
    expect_true(is.na(selection$rankings$Checks[[1]][["trend"]]))
    expect_true(is.na(selection$rankings$Seasonal_Fidelity))
    expect_identical(fixture$forecasts, original)
  }
})

test_that("supported future magnitude bounds retain exact backtest protection", {
  actuals <- 100 * 1.25^(0:11)
  expected <- utils::tail(actuals, 1) * 1.25^(1:24)
  fixture <- make_selection_case(actuals,
    futures = list(growth = expected, safe = rep(utils::tail(actuals, 1), 24)),
    errors = c(growth = 0.01, safe = 0.013), date_type = "year")
  for (direction in c(-1, 1)) {
    for (multiple in c(100 - 1e-7, 100, 100 + 1e-7)) {
      altered <- fixture
      index <- utils::tail(which(altered$forecasts$Model_ID == "growth"), 1)
      altered$forecasts$Forecast[index] <- direction * multiple * utils::tail(expected, 1)
      result <- do.call(select_forecast_candidate, altered)
      score <- result$rankings[result$rankings$Model_ID == "growth", ]
      expect_identical(score$Eligible, multiple <= 100)
      expect_identical("catastrophic_magnitude" %in% score$Reasons[[1]], multiple > 100)
    }
  }
  historical_scale <- max(as.numeric(stats::quantile(abs(actuals), 0.95)),
    stats::mad(actuals), stats::mad(diff(actuals)))
  fixture$backtests$Forecast[fixture$backtests$Model_ID == "growth"] <- 101 * historical_scale
  result <- do.call(select_forecast_candidate, fixture)
  expect_false(result$rankings$Eligible[result$rankings$Model_ID == "growth"])
  expect_true("catastrophic_magnitude" %in%
    result$rankings$Reasons[[which(result$rankings$Model_ID == "growth")]])
})

test_that("invalid trend projections fall back without numerical warnings", {
  fixture <- make_selection_case(100 * 10^(0:11),
    futures = list(only = rep(1, 400)), errors = c(only = 0.03), date_type = "year")
  expect_warning(reference <- forecast_reference(fixture$history, 400, fixture$context), NA)
  expect_null(reference$trend)
  decreasing <- make_selection_case(100 * 0.5^(0:11),
    futures = list(only = rep(1, 2000)), errors = c(only = 0.03), date_type = "year")
  expect_warning(reference <- forecast_reference(decreasing$history, 2000, decreasing$context), NA)
  expect_null(reference$trend)
})

test_that("reference fitting and uncertainty follow independent drift formulas", {
  actuals <- 100 + seq_len(36)
  fixture <- make_selection_case(actuals,
    futures = list(only = 137:148), errors = c(only = 0.03), date_type = "year")
  reference <- forecast_reference(fixture$history, 12, fixture$context)
  expected_scale <- as.numeric(stats::quantile(utils::tail(actuals, 24) / 136, 0.95))
  expect_identical(reference$trend$mode, "additive")
  expect_equal(reference$trend$drift, 1 / 136, tolerance = 1e-12)
  expect_equal(reference$trend$projection, (137:148) / 136, tolerance = 1e-12)
  expect_equal(reference$trend$widths, 6 * 0.05 * expected_scale * sqrt(1:12),
    tolerance = 1e-12)

  actuals <- 100 * 1.1^(0:35)
  fixture <- make_selection_case(actuals,
    futures = list(only = utils::tail(actuals, 1) * 1.1^(1:12)),
    errors = c(only = 0.03), date_type = "year")
  reference <- forecast_reference(fixture$history, 12, fixture$context)
  expect_identical(reference$trend$mode, "log")
  expect_equal(reference$trend$drift, log(1.1), tolerance = 1e-12)
  expect_equal(reference$trend$projection, log(1.1) * 1:12, tolerance = 1e-12)
  expect_equal(reference$trend$widths, 6 * log1p(0.05) * sqrt(1:12), tolerance = 1e-12)
})

test_that("both chronological blocks must support the proposed drift", {
  original_fit <- forecast_trend_fit
  for (error_fraction in c(0.79, 0.81)) {
    local({
      captured_fraction <- error_fraction
      calls <- list()
      local_mocked_bindings(forecast_trend_fit = function(values, period, horizon) {
        calls[[length(calls) + 1L]] <<- values
        if (any(values < 0)) return(NULL)
        result <- original_fit(values, period, horizon)
        if (!is.null(result) && length(values) < 12) {
          result$projection <- result$projection + captured_fraction * 2.5 / 256
        }
        result
      })
      fixture <- make_selection_case(245:256,
        futures = list(only = 257:260), errors = c(only = 0.03), date_type = "year")
      reference <- forecast_reference(fixture$history, 4, fixture$context)
      expect_identical(!is.null(reference$trend), captured_fraction < 0.8)
      additive_calls <- calls[vapply(calls, function(values) all(values > 0), logical(1))]
      expect_equal(vapply(additive_calls, length, integer(1)), c(12L, 10L, 11L))
      for (values in additive_calls) {
        expect_equal(values, (245:256)[seq_along(values)] / 256)
      }
    })
  }
})

test_that("trend scoring detects immediate and delayed unsupported growth", {
  actuals <- 100 * exp(0.04 * seq_len(48))
  expected <- utils::tail(actuals, 1) * exp(0.04 * seq_len(24))
  immediate <- expected
  immediate[1] <- 20 * immediate[1]
  delayed <- expected * exp(c(rep(0, 12), 0.3 * seq_len(12)))
  fixture <- make_selection_case(actuals,
    futures = list(intact = expected, immediate = immediate, delayed = delayed),
    errors = c(intact = 0.013, immediate = 0.01, delayed = 0.011))
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, "intact")
  for (candidate in c("immediate", "delayed")) {
    score <- result$rankings[result$rankings$Model_ID == candidate, ]
    expect_true(score$Risk > 0 || !score$Eligible)
    expect_true(any(c("level_deviation", "trend_deviation", "catastrophic_magnitude") %in%
      score$Reasons[[1]]))
  }
  spike <- make_selection_case(c(rep(100, 35), 400),
    futures = list(returning = rep(100, 6), persistent = rep(400, 6)),
    errors = c(returning = 0.01, persistent = 0.013), date_type = "year")
  expect_identical(do.call(select_forecast_candidate, spike)$selected_id, "returning")
})

test_that("trend support is independent of units order and future values", {
  actuals <- 100 * exp(0.04 * seq_len(48))
  expected <- utils::tail(actuals, 1) * exp(0.04 * seq_len(24))
  fixture <- make_selection_case(actuals,
    futures = list(growth = expected, flat = rep(utils::tail(actuals, 1), 24)),
    errors = c(growth = 0.01, flat = 0.013))
  baseline <- do.call(select_forecast_candidate, fixture)
  reference <- prepare_forecast_evaluation(fixture$history, fixture$context)$reference
  for (unit_scale in c(1e-100, 1e100)) {
    altered <- fixture
    altered$history$Target <- altered$history$Target * unit_scale
    for (table in c("forecasts", "backtests")) {
      altered[[table]]$Forecast <- altered[[table]]$Forecast * unit_scale
      altered[[table]]$Target <- altered[[table]]$Target * unit_scale
      altered[[table]] <- altered[[table]][rev(seq_len(nrow(altered[[table]]))), ]
    }
    altered$history <- altered$history[rev(seq_len(nrow(altered$history))), ]
    selected <- do.call(select_forecast_candidate, altered)
    expect_identical(selected$selected_id, baseline$selected_id)
    expect_equal(selected$rankings$Risk, baseline$rankings$Risk, tolerance = 1e-10)
    expect_equal(selected$rankings$WMAPE, baseline$rankings$WMAPE, tolerance = 1e-10)
  }
  future_history <- data.frame(Date = unique(fixture$forecasts$Date), Target = 1e200)
  altered_history <- rbind(fixture$history, future_history)
  altered_context <- fixture$context
  altered_context$candidate_ids <- c("unrelated", "candidate")
  expect_identical(prepare_forecast_evaluation(altered_history, altered_context)$reference, reference)
  altered <- fixture
  altered$forecasts$Target <- 1e200
  expect_identical(do.call(select_forecast_candidate, altered), baseline)
})

test_that("cached trend references are reused regardless of candidate count", {
  original_reference <- forecast_trend_reference
  calls <- 0L
  local_mocked_bindings(forecast_trend_reference = function(...) {
    calls <<- calls + 1L
    original_reference(...)
  })
  actuals <- 100 + seq_len(48)
  fixture <- make_selection_case(actuals,
    futures = list(growth = 149:160, flat = rep(148, 12)),
    errors = c(growth = 0.01, flat = 0.013))
  evaluation <- prepare_forecast_evaluation(fixture$history, fixture$context)
  expect_identical(calls, 1L)
  fixture$context$forecast_evaluation <- evaluation
  first <- do.call(select_forecast_candidate, fixture)
  expanded <- fixture
  for (table in c("backtests", "forecasts")) {
    copies <- expanded[[table]][expanded[[table]]$Model_ID == "growth", ]
    copies$Model_ID <- "second_growth"
    expanded[[table]] <- rbind(expanded[[table]], copies)
  }
  second <- do.call(select_forecast_candidate, expanded)
  expect_identical(first$selected_id, second$selected_id)
  expect_identical(calls, 1L)
  expect_identical(fixture$context$forecast_evaluation, evaluation)
})

make_trend_selection_artifacts <- function(fixture) {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 60)
  values <- 100 * exp(0.1 * seq_along(dates))
  history <- data.frame(Combo = "Synthetic", Date = dates,
    Target = c(values[1:48], rep(NA_real_, 12)))
  write_data(history, combo = "Synthetic", run_info = fixture$run_info,
    output_type = "data", folder = "prep_data", suffix = "-R1")
  rows <- fixture$forecasts
  rows$Target <- ifelse(rows$Train_Test_ID == 1, NA_real_, values[match(rows$Date, dates)])
  errors <- c(arima = 0.01, meanf = 0.013, snaive = 0.014, ets = 0.005)
  rows$Forecast <- rows$Target * (1 + errors[rows$Model_Name])
  future <- rows$Train_Test_ID == 1
  rows$Forecast[future] <- values[match(rows$Date[future], dates)]
  rows$Forecast[future & rows$Model_Name == "meanf"] <- values[48]
  rows$Forecast[future & rows$Model_Name == "snaive"] <- 1.02 * rows$Forecast[future & rows$Model_Name == "snaive"]
  rows$Forecast[future & rows$Model_Name == "ets"] <- 1000 * rows$Forecast[future & rows$Model_Name == "ets"]
  write_data(rows, combo = "Synthetic", run_info = fixture$run_info,
    output_type = "data", folder = "forecasts", suffix = "-single_models")
  fixture$forecasts <- rows
  fixture
}

test_that("final_models uses trend scores without changing constituent predictions", {
  evaluations <- list()
  original_selector <- select_series_forecasts
  local_mocked_bindings(
    par_start = function(...) {
      list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
    },
    select_series_forecasts = function(predictions, ...) {
      evaluations[[length(evaluations) + 1L]] <<- predictions
      original_selector(predictions, ...)
    }
  )
  catalogue <- data.frame(Model_Name = c("arima", "meanf", "snaive", "ets"),
    Model_Type = "local", Recipe_ID = "R1")
  catalogue$Model_ID <- paste(catalogue$Model_Name, "local", "R1", sep = "--")
  fixture <- make_catalogue_selection_fixture("trend", catalogue)
  fixture <- make_trend_selection_artifacts(fixture)
  result <- final_models(fixture$run_info, average_models = TRUE, max_model_average = 3,
    weekly_to_daily = FALSE, num_cores = 1)
  rankings <- result$selections$Synthetic$rankings
  expect_length(evaluations, 2L)
  expect_equal(nrow(rankings), 7L)
  expect_identical(result$selections$Synthetic$selected_id, "arima--local--R1")
  average_ids <- rankings$Model_ID[grepl("_", rankings$Model_ID, fixed = TRUE)]
  expect_length(average_ids, 4L)
  expect_false(any(grepl("ets--local--R1", average_ids, fixed = TRUE)))
  for (model_id in average_ids) {
    members <- strsplit(model_id, "_", fixed = TRUE)[[1]]
    components <- lapply(members, function(member) {
      rows <- fixture$forecasts[fixture$forecasts$Model_ID == member, ]
      rows[order(rows$Train_Test_ID, rows$Date), ]
    })
    measured <- evaluations[[2]][evaluations[[2]]$Model_ID == model_id, ]
    measured <- measured[order(measured$Train_Test_ID, measured$Date), ]
    expected <- Reduce(`+`, lapply(components, `[[`, "Forecast")) / length(components)
    expect_equal(measured$Forecast, expected, tolerance = 1e-10)
  }
  retained <- read_fcst_file(locate_single_models_file(fixture$run_info))
  key <- function(rows) paste(rows$Model_ID, rows$Train_Test_ID, rows$Date)
  expect_equal(retained$Forecast[match(key(fixture$forecasts), key(retained))],
    fixture$forecasts$Forecast, tolerance = 1e-10)
  expect_identical(unique(retained$Model_ID[retained$Best_Model == "Yes"]), "arima--local--R1")
  saved_average <- read_selection_file(fixture$run_info, "forecasts", "-average_models", "Synthetic")
  expect_true(all(saved_average$Best_Model == "No"))
})