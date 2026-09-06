local_corner_test_safety <- function(.env = parent.frame()) {
  unexpected_work <- function(...) {
    stop("Selection corner tests must not perform I/O, fit models, or reconcile.", call. = FALSE)
  }
  testthat::local_mocked_bindings(
    read_file = unexpected_work, list_files = unexpected_work, write_data = unexpected_work,
    prep_data = unexpected_work, prep_models = unexpected_work, train_models = unexpected_work,
    fit_models = unexpected_work, final_models = unexpected_work, reconcile = unexpected_work,
    .package = "finnts", .env = .env
  )
}

test_that("selection requires finite actuals inside the recent reference window", {
  local_corner_test_safety()
  for (unusable in c(NA_real_, NaN, Inf, -Inf)) {
    fixture <- make_selection_case(actuals = c(rep(100, 36), rep(unusable, 36)))
    expect_error(do.call(select_forecast_candidate, fixture),
      "usable historical actuals.*reference window")
  }
})

test_that("partly missing and zero reference windows remain usable", {
  local_corner_test_safety()
  for (recent_value in c(0, 100)) {
    fixture <- make_selection_case(actuals = c(rep(100, 36), rep(NA_real_, 35), recent_value),
      futures = list(only = rep(recent_value, 6)), errors = c(only = 0.03))
    fixture$backtests$Forecast <- recent_value
    expect_silent(reference <- forecast_reference(fixture$history, 6L, fixture$context))
    expect_equal(reference$reference * if (reference$normalization > 0) reference$normalization else 1,
      rep(recent_value, 6))
    expect_silent(result <- do.call(select_forecast_candidate, fixture))
    expect_identical(result$selected_id, "only")
    expect_true(result$rankings$Eligible)
    expect_equal(result$rankings$Risk, 0)
    expect_true(is.finite(result$rankings$WMAPE))
    expect_true(is.na(result$rankings$Seasonal_Fidelity))
  }
})

test_that("future values cannot rescue an unusable historical window", {
  local_corner_test_safety()
  fixture <- make_selection_case(actuals = c(rep(100, 36), rep(NA_real_, 36)))
  fixture$forecasts$Target <- 100
  fixture$history <- rbind(fixture$history,
    data.frame(Date = max(fixture$history$Date) + 1, Target = 100))
  expect_error(do.call(select_forecast_candidate, fixture), "reference window")
  fixture$history$Target <- NA_real_
  expect_error(do.call(select_forecast_candidate, fixture), "unique dated historical actuals")
})

test_that("empty candidate pools have an explicit input error", {
  local_corner_test_safety()
  fixture <- make_selection_case()
  fixture$context$candidate_ids <- character()
  expect_error(do.call(select_forecast_candidate, fixture), "at least one candidate")
  series <- fixture$context
  series$history <- fixture$history
  predictions <- dplyr::bind_rows(fixture$backtests, fixture$forecasts)[0, ]
  expect_error(select_series_forecasts(predictions, series, series$train_test_split),
    "at least one candidate")
})

test_that("single and all-invalid pools retain their selection semantics", {
  local_corner_test_safety()
  fixture <- make_selection_case(futures = list(only = rep(200, 6)), errors = c(only = 0.08))
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, "only")
  expect_true(result$rankings$Eligible)
  expect_gt(result$rankings$Violations, 0L)
  fixture$forecasts$Forecast[1] <- Inf
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, NA_character_)
  expect_false(result$rankings$Eligible)
  fixture <- make_selection_case()
  fixture$forecasts$Forecast <- Inf
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, NA_character_)
  expect_false(any(result$rankings$Eligible))
  summary <- agent_selection_summary(list(selections = list(series = result)))
  expect_false(summary$acceptable)
  expect_identical(summary$status, "rejected")
  expect_identical(summary$weighted_mape, Inf)
})

test_that("required keys cannot be replaced while preserving row counts", {
  local_corner_test_safety()
  fixture <- make_selection_case(futures = list(accurate = rep(100, 6), safe = rep(100, 6)),
    errors = c(accurate = 0.02, safe = 0.023))
  keys <- c("Train_Test_ID", "Date")
  defects <- c("duplicate", "missing", "extra", "off_calendar", "wrong_fold", "missing_date", "missing_fold")
  for (table in c("backtests", "forecasts")) {
    for (defect in defects) {
      altered <- fixture
      rows <- altered[[table]]
      indices <- which(rows$Model_ID == "accurate")
      if (defect == "duplicate") rows[indices[1], keys] <- rows[indices[2], keys]
      if (defect == "missing") rows <- rows[-indices[1], ]
      if (defect == "extra") rows <- rbind(rows, rows[indices[1], ])
      if (defect == "off_calendar") rows$Date[indices[1]] <- rows$Date[indices[1]] + 1
      if (defect == "wrong_fold") rows$Train_Test_ID[indices[1]] <- 97L
      if (defect == "missing_date") rows$Date[indices[1]] <- as.Date(NA)
      if (defect == "missing_fold") rows$Train_Test_ID[indices[1]] <- NA_integer_
      altered[[table]] <- rows
      expected <- fixture[[table]][fixture[[table]]$Model_ID == "accurate", keys]
      actual <- rows[rows$Model_ID == "accurate", keys]
      expect_false(forecast_keys_complete(actual, expected), info = paste(table, defect))
      if (!defect %in% c("missing", "extra")) expect_equal(nrow(actual), nrow(expected))
      result <- do.call(select_forecast_candidate, altered)
      expect_identical(result$selected_id, "safe")
      expect_false(result$rankings$Eligible[result$rankings$Model_ID == "accurate"])
      expect_true(result$rankings$Eligible[result$rankings$Model_ID == "safe"])
      reason <- if (table == "backtests") "incomplete_backtests" else "incomplete_forecast"
      expect_true(reason %in% result$rankings$Reasons[[match("accurate", result$rankings$Model_ID)]])
    }
  }
  expected <- fixture$backtests[fixture$backtests$Model_ID == "safe", keys]
  equivalent <- expected[nrow(expected):1, ]
  equivalent$Train_Test_ID <- as.character(equivalent$Train_Test_ID)
  equivalent$Date <- as.character(equivalent$Date)
  expect_true(forecast_keys_complete(equivalent, expected))
  expect_false(forecast_keys_complete(expected[0, ], expected[0, ]))
})

test_that("validation-only rows are excluded from candidate evaluation", {
  local_corner_test_safety()
  fixture <- make_selection_case()
  expected <- do.call(select_forecast_candidate, fixture)
  validation <- fixture$backtests
  validation$Train_Test_ID <- 99L
  validation$Forecast <- Inf
  predictions <- dplyr::bind_rows(fixture$backtests, fixture$forecasts, validation)
  splits <- fixture$context$train_test_split
  validation_split <- splits[2, ]
  validation_split$Train_Test_ID <- 99L
  validation_split$Run_Type <- "Validation"
  splits <- rbind(splits, validation_split)
  series <- fixture$context
  series$history <- fixture$history
  expect_equal(select_series_forecasts(predictions, series, splits), expected)
})

test_that("backtest accuracy uses original signed zero and missing actuals", {
  local_corner_test_safety()
  dates <- as.Date("2024-01-01") + 0:4
  history <- data.frame(Date = dates, Target = c(-10, 0, 20, NA_real_, Inf))
  backtests <- data.frame(Train_Test_ID = c(rep(2L, 5), 3L, 3L),
    Date = dates[c(1:5, 1, 3)], Forecast = c(-9, 0, 24, 999, 999, -8, 18), Target = -999)
  accuracy <- forecast_backtest_accuracy(history, backtests)
  expect_equal(accuracy$WMAPE, 9.1 / 60.1, tolerance = 1e-12)
  expect_equal(accuracy$Log_Weight, log(60.1), tolerance = 1e-12)
  rounded <- data.frame(Date = dates[1], Forecast = -8.76544)
  expect_equal(forecast_backtest_accuracy(history, rounded)$WMAPE, 0.1235)
  backtests$Target <- 1e100
  expect_identical(forecast_backtest_accuracy(history, backtests), accuracy)
  backtests$Forecast <- 0
  expect_equal(forecast_backtest_accuracy(history, backtests)$WMAPE, 1)
  backtests$Forecast[4] <- NA_real_
  expect_true(is.na(forecast_backtest_accuracy(history, backtests)$WMAPE))
  backtests$Forecast <- 0
  history$Target <- NA_real_
  unavailable <- forecast_backtest_accuracy(history, backtests)
  expect_true(is.na(unavailable$WMAPE))
  expect_true(is.na(unavailable$Log_Weight))
})

test_that("missing backtest actuals cannot create an eligible accuracy score", {
  local_corner_test_safety()
  fixture <- make_selection_case(actuals = c(rep(100, 30), rep(NA_real_, 6)))
  fixture$backtests$Forecast <- 100
  fixture$backtests$Target <- 100
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, NA_character_)
  expect_false(any(result$rankings$Eligible))
  expect_true(all(vapply(result$rankings$Reasons, function(reasons) {
    "unavailable_accuracy" %in% reasons
  }, logical(1))))
})

test_that("Agent accuracy respects unequal volumes without losing completeness", {
  local_corner_test_safety()
  selection <- function(wmape, volume) {
    list(selected_id = "chosen", rankings = tibble::tibble(
      Model_ID = "chosen", Eligible = TRUE, WMAPE = wmape, Log_Weight = log(volume),
      Risk = 0, Violations = 0L, Reasons = list(character()), Seasonal_Fidelity = NA_real_))
  }
  complete <- list(selections = list(small = selection(0.1, 10), large = selection(0.2, 1000)))
  summary <- agent_selection_summary(complete)
  expect_equal(summary$weighted_mape, 201 / 1010, tolerance = 1e-12)
  expect_true(summary$acceptable)
  scaled <- list(selections = list(small = selection(0.1, 1e251), large = selection(0.2, 1e253)))
  expect_equal(agent_selection_summary(scaled)$weighted_mape, summary$weighted_mape, tolerance = 1e-12)
  partial <- list(selections = list(small = complete$selections$small, large = NULL))
  summary <- agent_selection_summary(partial)
  expect_identical(summary$status, "partial")
  expect_identical(summary$weighted_mape, Inf)
  expect_false(summary$acceptable)
  expect_identical(rank_agent_run_selections(list(partial, complete))$best_run_index, 2L)
})

test_that("catastrophic magnitude boundaries apply to both prediction periods", {
  local_corner_test_safety()
  fixture <- make_selection_case(futures = list(accurate = rep(100, 6), safe = rep(100, 6)))
  for (table in c("backtests", "forecasts")) {
    for (direction in c(-1, 1)) {
      for (multiple in c(100 - 1e-7, 100, 100 + 1e-7)) {
        altered <- fixture
        index <- which(altered[[table]]$Model_ID == "accurate")[1]
        altered[[table]]$Forecast[index] <- direction * 100 * multiple
        result <- do.call(select_forecast_candidate, altered)
        candidate <- result$rankings[result$rankings$Model_ID == "accurate", ]
        expect_identical(candidate$Eligible, multiple <= 100)
        expect_identical("catastrophic_magnitude" %in% candidate$Reasons[[1]], multiple > 100)
        expect_true(result$rankings$Eligible[result$rankings$Model_ID == "safe"])
      }
    }
  }
})

test_that("zero-history forecasts use soft level checks instead of magnitude ratios", {
  local_corner_test_safety()
  fixture <- make_selection_case(actuals = rep(0, 36),
    futures = list(zero = rep(0, 6), nonzero = rep(1e9, 6)), errors = c(zero = 0, nonzero = 0))
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, "zero")
  expect_true(all(result$rankings$Eligible))
  nonzero <- result$rankings[result$rankings$Model_ID == "nonzero", ]
  expect_identical(nonzero$Reasons[[1]], "unsupported_level")
  expect_equal(nonzero$Risk, 1)
  expect_equal(nonzero$WMAPE, 1)
})

test_that("accuracy ceilings retain their numerical tolerance", {
  local_corner_test_safety()
  for (best in c(0, 0.08, 0.2)) {
    ceiling <- best + max(0.005, 0.05 * best)
    tolerance <- 8 * .Machine$double.eps
    for (difference in c(-1e-6, 0, tolerance / 2, tolerance * 4)) {
      rankings <- tibble::tibble(Model_ID = c("accuracy", "fidelity"), Eligible = TRUE,
        WMAPE = c(best, ceiling + difference), Risk = 0, Violations = 0L,
        Reasons = list(character(), character()), Seasonal_Fidelity = c(1, 0))
      expected <- if (difference <= tolerance) "fidelity" else "accuracy"
      expect_identical(rank_forecast_candidates(rankings)$selected_id, expected)
      expect_identical(rank_forecast_candidates(rankings[2:1, ])$selected_id, expected)
    }
  }
})

test_that("unknown fidelity affects only its tied shortlisted cohort", {
  local_corner_test_safety()
  rankings <- tibble::tibble(Model_ID = c("accuracy", "fidelity", "other", "unknown"),
    Eligible = TRUE, WMAPE = c(0.08, 0.083, 0.084, 0.082), Risk = c(0, 0, 0.2, 0.2),
    Violations = c(0L, 0L, 1L, 1L), Reasons = rep(list(character()), 4),
    Seasonal_Fidelity = c(0.5, 0, 0, NA_real_))
  for (unknown in c(NA_real_, NaN, Inf)) {
    rows <- rankings
    rows$Seasonal_Fidelity[4] <- unknown
    expect_identical(rank_forecast_candidates(rows)$selected_id, "fidelity")
    rows$Risk[4] <- 0
    rows$Violations[4] <- 0L
    expect_identical(rank_forecast_candidates(rows)$selected_id, "accuracy")
    rows$WMAPE[4] <- 0.1
    expect_identical(rank_forecast_candidates(rows)$selected_id, "fidelity")
    rows$Eligible[4] <- FALSE
    rows$WMAPE[4] <- 0
    expect_identical(rank_forecast_candidates(rows)$selected_id, "fidelity")
  }
  rankings$Risk <- c(2, 1, 1, 3)
  rankings$Violations <- c(1L, 1L, 2L, 1L)
  expect_identical(rank_forecast_candidates(rankings)$selected_id, "fidelity")
  rankings$Seasonal_Fidelity <- NULL
  expect_identical(rank_forecast_candidates(rankings)$selected_id, "fidelity")
})

test_that("replacement selectors must satisfy the existing structural contract", {
  local_corner_test_safety()
  fixture <- make_selection_case()
  valid <- do.call(select_forecast_candidate, fixture)
  duplicate <- valid
  duplicate$rankings <- rbind(valid$rankings, valid$rankings[1, ])
  missing <- valid
  missing$rankings <- valid$rankings[-1, ]
  extra <- valid
  extra_row <- valid$rankings[1, ]
  extra_row$Model_ID <- "extra"
  extra$rankings <- rbind(valid$rankings, extra_row)
  missing_column <- valid
  missing_column$rankings$Risk <- NULL
  multiple <- valid
  multiple$selected_id <- c("accurate", "safe")
  unknown <- valid
  unknown$selected_id <- "unknown"
  series <- fixture$context
  series$history <- fixture$history
  predictions <- dplyr::bind_rows(fixture$backtests, fixture$forecasts)
  for (invalid in list(duplicate, missing, extra, missing_column, multiple, unknown)) {
    expect_error(select_series_forecasts(predictions, series, series$train_test_split,
      selector = function(...) invalid), "invalid selection contract")
  }
  invalid <- valid
  invalid$rankings$Eligible[invalid$rankings$Model_ID == invalid$selected_id] <- FALSE
  expect_error(select_series_forecasts(predictions, series, series$train_test_split,
    selector = function(...) invalid), "ineligible candidate")
  reordered <- valid
  reordered$rankings <- reordered$rankings[2:1, ]
  reordered$rankings$Seasonal_Fidelity <- NULL
  expect_identical(select_series_forecasts(predictions, series, series$train_test_split,
    selector = function(...) reordered), reordered)
})

test_that("seasonal evidence thresholds preserve assessed and unassessed states", {
  local_corner_test_safety()
  profile <- 10 * cos(2 * pi * (seq_len(12) - 0.5) / 12)
  fixture <- make_selection_case(100 + rep(profile, 3),
    futures = list(only = 100 + profile), errors = c(only = 0.03))
  reference <- forecast_reference(fixture$history, 12L, fixture$context)
  for (strength in c(0.6 - 1e-7, 0.6, 0.6 + 1e-7)) {
    controlled <- reference
    controlled$seasonal_strength <- strength
    risk <- forecast_path_risk(100 - profile, controlled)
    expect_identical("seasonal_phase" %in% risk$reasons, strength >= 0.6)
    expect_identical(is.finite(risk$seasonal_fidelity), strength >= 0.6)
  }
  for (period in c(2L, 3L)) {
    pattern <- if (period == 2L) c(10, -10) else c(5, -10, 5)
    fixture <- make_selection_case(100 + rep(pattern, 12),
      futures = list(only = 100 - pattern), errors = c(only = 0.03), period = period)
    controlled <- forecast_reference(fixture$history, period, fixture$context)
    controlled$seasonal_strength <- 1
    risk <- forecast_path_risk(100 - pattern, controlled)
    expect_identical("seasonal_phase" %in% risk$reasons, period >= 3L)
    expect_identical(is.finite(risk$seasonal_fidelity), period >= 3L)
  }
  tolerance <- 0.2 * diff(range(reference$profile))
  reference$amplitude_tolerance <- tolerance
  for (amplitude in c(0.8, 1, 1.2, 1.5)) {
    risk <- forecast_path_risk(100 + amplitude * profile, reference)
    expect_equal(risk$seasonal_fidelity, max(0, abs(amplitude - 1) - 0.2), tolerance = 1e-10)
  }
  reference$normalization <- 1
  reference$reference <- rep(100, 3)
  reference$width <- 100
  reference$short_seasonality <- list(profile = c(1, 0, -1), trend = 0,
    phase_noise = 2, amplitude_tolerance = 0.1)
  for (amplitude in c(0.5, 1, 2)) {
    risk <- forecast_path_risk(100 + amplitude * c(-1, 0, 1), reference)
    expect_identical("seasonal_phase" %in% risk$reasons, amplitude > 1)
    expect_identical(is.na(risk$components[["seasonality"]]), amplitude <= 1)
    expect_true(is.finite(risk$seasonal_fidelity))
  }
})

test_that("seasonal comparisons align offset cutoffs and incomplete future cycles", {
  local_corner_test_safety()
  settings <- list(list(cadence = "day", period = 7L), list(cadence = "month", period = 12L),
    list(cadence = "quarter", period = 4L))
  for (setting in settings) {
    period <- setting$period
    for (offset in c(1L, period - 1L)) {
      history_size <- 3L * period + offset
      history <- 100 + 10 * cos(2 * pi * (seq_len(history_size) - 0.5) / period)
      for (horizon in c(period - 1L, period, period + 1L, 2L * period + 1L)) {
        future <- 100 + 10 * cos(2 * pi * (history_size + seq_len(horizon) - 0.5) / period)
        fixture <- make_selection_case(history, futures = list(intact = future, reversed = 200 - future),
          errors = c(intact = 0.083, reversed = 0.08), date_type = setting$cadence)
        result <- do.call(select_forecast_candidate, fixture)
        intact <- result$rankings[result$rankings$Model_ID == "intact", ]
        reversed <- result$rankings[result$rankings$Model_ID == "reversed", ]
        expect_identical(result$selected_id, "intact", info = paste(setting$cadence, offset, horizon))
        expect_true(all(result$rankings$Eligible))
        expect_true(all(is.finite(result$rankings$Risk) & result$rankings$Risk >= 0))
        expect_true(all(is.finite(result$rankings$Seasonal_Fidelity) & result$rankings$Seasonal_Fidelity >= 0))
        expect_false("seasonal_phase" %in% intact$Reasons[[1]])
        expect_true("seasonal_phase" %in% reversed$Reasons[[1]])
      }
    }
  }
})

test_that("weekly invalid values stay within their complete forecast key", {
  local_corner_test_safety()
  dates <- as.Date("2026-01-01") + c(0, 7)
  native <- expand.grid(Combo = c("A", "B"), Model_ID = c("arima", "ets"), Train_Test_ID = 1:2,
    Date = dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  native$Forecast <- 7 * seq_len(nrow(native))
  native$Target <- ifelse(native$Train_Test_ID == 1L, NA_real_, native$Forecast + 7)
  expanded <- native[rep(seq_len(nrow(native)), each = 7L), ]
  expanded$Date_Day <- expanded$Date + rep(0:6, nrow(native))
  expanded$Forecast <- expanded$Forecast / 7
  expanded$Target <- expanded$Target / 7
  keys <- c("Combo", "Model_ID", "Train_Test_ID", "Date")
  for (combo in c("A", "B")) {
    altered <- expanded
    group <- altered$Combo == combo & altered$Model_ID == "arima" &
      altered$Train_Test_ID == 2L & altered$Date == dates[2]
    altered$Forecast[group & altered$Date_Day == dates[2] + 1] <- Inf
    altered$Forecast[group & altered$Date_Day == dates[2] + 5] <- -Inf
    expected <- native
    invalid <- expected$Combo == combo & expected$Model_ID == "arima" &
      expected$Train_Test_ID == 2L & expected$Date == dates[2]
    expected$Forecast[invalid] <- NaN
    expected <- dplyr::arrange(tibble::as_tibble(expected), Combo, Model_ID, Train_Test_ID, Date)
    for (reverse in c(FALSE, TRUE)) {
      rows <- if (reverse) altered[rev(seq_len(nrow(altered))), ] else altered
      result <- native_forecast_rows(rows, "week")
      expect_equal(nrow(result), nrow(native))
      expect_false(anyDuplicated(result[, keys]) > 0)
      expect_equal(sum(!is.finite(result$Forecast)), 1L)
      expect_equal(dplyr::arrange(tibble::as_tibble(result), Combo, Model_ID, Train_Test_ID, Date), expected)
    }
  }
})

test_that("bounded generated cases preserve selection invariants", {
  local_corner_test_safety()
  withr::local_seed(20260905)
  cadences <- c("day", "week", "month", "quarter", "year")
  for (case_index in seq_len(48L)) {
    cadence <- cadences[(case_index - 1L) %% length(cadences) + 1L]
    period <- switch(cadence, day = 7L, week = 52L, month = 12L, quarter = 4L, year = 1L)
    history_size <- sample(12:72, 1)
    horizon <- sample(1:25, 1)
    candidate_count <- (case_index - 1L) %% 4L + 1L
    direction <- sample(c(-1, 1), 1)
    slope <- sample(c(-0.1, 0, 0.1), 1)
    positions <- seq_len(history_size)
    actuals <- direction * (100 + slope * positions + 10 * sin(2 * pi * positions / period))
    positions <- history_size + seq_len(horizon)
    continued <- direction * (100 + slope * positions + 10 * sin(2 * pi * positions / period))
    model_ids <- sprintf("candidate-%02d", seq_len(candidate_count))
    futures <- stats::setNames(list(continued, rep(stats::median(actuals), horizon),
      1.7 * continued, 100000 * continued)[seq_len(candidate_count)], model_ids)
    errors <- stats::setNames(sample(seq(0.02, 0.18, by = 0.0005), candidate_count), model_ids)
    fixture <- make_selection_case(actuals, futures, errors, date_type = cadence)
    result <- do.call(select_forecast_candidate, fixture)
    expect_true(result$selected_id %in% model_ids, info = paste("generated case", case_index))
    chosen <- result$rankings[result$rankings$Model_ID == result$selected_id, ]
    expect_true(chosen$Eligible)
    expected_accuracy <- vapply(model_ids, function(model_id) {
      rows <- fixture$backtests[fixture$backtests$Model_ID == model_id, ]
      truth <- fixture$history$Target[match(rows$Date, fixture$history$Date)]
      sum(round(abs((rows$Forecast - truth) / abs(truth)), 4) * abs(truth)) / sum(abs(truth))
    }, numeric(1))
    expect_equal(result$rankings$WMAPE[match(model_ids, result$rankings$Model_ID)],
      unname(expected_accuracy), tolerance = 1e-12)
    eligible <- result$rankings[result$rankings$Eligible, ]
    best <- min(eligible$WMAPE)
    ceiling <- best + max(0.005, 0.05 * best)
    expect_lte(chosen$WMAPE, ceiling + 8 * .Machine$double.eps)
    finalists <- eligible[eligible$WMAPE <= ceiling + 8 * .Machine$double.eps, ]
    finalists <- finalists[finalists$Risk == min(finalists$Risk), ]
    finalists <- finalists[finalists$Violations == min(finalists$Violations), ]
    if (all(is.finite(finalists$Seasonal_Fidelity) & finalists$Seasonal_Fidelity >= 0)) {
      finalists <- finalists[finalists$Seasonal_Fidelity == min(finalists$Seasonal_Fidelity), ]
    }
    finalists <- finalists[finalists$WMAPE == min(finalists$WMAPE), ]
    expect_identical(result$selected_id, sort(finalists$Model_ID, method = "radix")[1])

    shuffled <- fixture
    shuffled$history <- shuffled$history[sample.int(nrow(shuffled$history)), ]
    for (table in c("backtests", "forecasts")) {
      shuffled[[table]] <- shuffled[[table]][sample.int(nrow(shuffled[[table]])), ]
    }
    shuffled$context$candidate_ids <- sample(model_ids)
    expect_identical(do.call(select_forecast_candidate, shuffled)$selected_id, result$selected_id)

    extra <- fixture
    backtest <- fixture$backtests[fixture$backtests$Model_ID == model_ids[1], ]
    future <- fixture$forecasts[fixture$forecasts$Model_ID == model_ids[1], ]
    backtest$Model_ID <- "hard-invalid"
    future$Model_ID <- "hard-invalid"
    backtest$Forecast <- backtest$Target
    future$Forecast[1] <- Inf
    extra$backtests <- rbind(extra$backtests, backtest)
    extra$forecasts <- rbind(extra$forecasts, future)
    augmented <- do.call(select_forecast_candidate, extra)
    expect_identical(augmented$selected_id, result$selected_id)
    expect_false(augmented$rankings$Eligible[augmented$rankings$Model_ID == "hard-invalid"])

    scaled <- fixture
    unit_scale <- if (case_index %% 2L) 1e-50 else 1e50
    scaled$history$Target <- scaled$history$Target * unit_scale
    for (table in c("backtests", "forecasts")) {
      scaled[[table]]$Forecast <- scaled[[table]]$Forecast * unit_scale
      scaled[[table]]$Target <- scaled[[table]]$Target * unit_scale
    }
    scaled$forecasts$Target <- -1e200
    expect_identical(do.call(select_forecast_candidate, scaled)$selected_id, result$selected_id)
  }
})