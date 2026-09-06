normalize_series_history <- function(data, hist_end_date, recipe = "R1",
                                     combo_info = NULL, stationary = FALSE,
                                     box_cox = FALSE) {
  required <- c("Date", "Target")
  if (!is.data.frame(data) || !all(required %in% names(data)) || nrow(data) == 0) {
    stop("Prepared history must contain nonempty Date and Target columns.", call. = FALSE)
  }
  if (identical(recipe, "R2")) {
    if (!all(c("Horizon", "Origin") %in% names(data))) {
      stop("R2 history is missing Horizon or Origin.", call. = FALSE)
    }
    data <- data[!is.na(data$Horizon) & data$Horizon == 1, , drop = FALSE]
  }
  data$Date <- as.Date(data$Date)
  data <- data[order(data$Date), , drop = FALSE]
  if (nrow(data) == 0 || anyNA(data$Date) || anyDuplicated(data$Date)) {
    stop("Prepared history must have one observation per date.", call. = FALSE)
  }
  if (identical(recipe, "R2") &&
      (anyNA(data$Origin) || any(diff(data$Origin) != 1))) {
    stop("R2 history has inconsistent Horizon 1 origins.", call. = FALSE)
  }
  calendar <- data$Date
  data <- data[, intersect(c("Combo", "Date", "Target", "Target_Original"), names(data)), drop = FALSE]
  if (stationary || box_cox) {
    if (!is.data.frame(combo_info) || nrow(combo_info) != 1) {
      stop("Original-scale history requires the series transformation metadata.", call. = FALSE)
    }
    if (stationary) {
      if (!all(c("Diff_Value1", "Diff_Value2") %in% names(combo_info))) {
        stop("History transformation metadata is missing differencing values.", call. = FALSE)
      }
      data <- undifference_recipe(data, combo_info, hist_end_date)
    }
    if (box_cox) {
      if (!"Box_Cox_Lambda" %in% names(combo_info)) {
        stop("History transformation metadata is missing Box_Cox_Lambda.", call. = FALSE)
      }
      if (!is.na(combo_info$Box_Cox_Lambda)) {
        for (target_column in intersect(c("Target", "Target_Original"), names(data))) {
          data[[target_column]] <- timetk::box_cox_inv_vec(
            data[[target_column]], lambda = combo_info$Box_Cox_Lambda
          )
        }
      }
    }
  }
  actual_column <- if ("Target_Original" %in% names(data)) "Target_Original" else "Target"
  history <- data.frame(Date = data$Date, Target = data[[actual_column]])
  history <- history[history$Date <= as.Date(hist_end_date), , drop = FALSE]
  if (nrow(history) == 0 || !any(is.finite(history$Target))) {
    stop("Prepared history contains no usable historical actuals.", call. = FALSE)
  }
  list(history = history, calendar = calendar, hist_end_date = as.Date(hist_end_date))
}

read_series_history <- function(run_info, combo, run_log = NULL, cache = NULL) {
  if (length(combo) != 1 || is.na(combo)) {
    stop("Read prepared history for exactly one series at a time.", call. = FALSE)
  }
  prefix <- paste0(hash_data(run_info$project_name), "-", hash_data(run_info$run_name))
  cache_key <- paste(prefix, hash_data(combo), sep = "-")
  if (!is.null(cache) && exists(cache_key, envir = cache, inherits = FALSE)) {
    return(get(cache_key, envir = cache, inherits = FALSE))
  }
  if (is.null(run_log)) {
    run_log <- read_file(run_info, file_list = fs::path(
      run_info$path, "logs", paste0(prefix, ".csv")
    ), return_type = "df")
  }
  if (nrow(run_log) != 1 || !"hist_end_date" %in% names(run_log)) {
    stop("Prepared history requires the existing run log and historical cutoff.", call. = FALSE)
  }
  recipes <- run_log[["recipes_to_run"]]
  if (is.null(recipes) || is.na(recipes) || recipes == "all") {
    recipe <- "R1"
  } else {
    recipe_list <- strsplit(recipes, "---", fixed = TRUE)[[1]]
    recipe <- if ("R1" %in% recipe_list) "R1" else "R2"
  }
  prepared <- read_file(run_info, file_list = fs::path(
    run_info$path, "prep_data", paste0(prefix, "-", hash_data(combo), "-", recipe, ".", run_info$data_output)
  ), return_type = "df")
  prepared <- adjust_combo_column(prepared)
  if ("Combo" %in% names(prepared) && any(is.na(prepared$Combo) | as.character(prepared$Combo) != combo)) {
    stop("Prepared history does not match the requested series.", call. = FALSE)
  }
  stationary <- isTRUE(as.logical(run_log[["stationary"]]))
  box_cox <- isTRUE(as.logical(run_log[["box_cox"]]))
  combo_info <- NULL
  if (stationary || box_cox) {
    metadata_key <- paste0(prefix, "-orig_combo_info")
    if (!is.null(cache) && exists(metadata_key, envir = cache, inherits = FALSE)) {
      combo_info <- get(metadata_key, envir = cache, inherits = FALSE)
    } else {
      combo_info <- read_file(run_info, file_list = fs::path(
        run_info$path, "prep_data", paste0(metadata_key, ".", run_info$data_output)
      ), return_type = "df")
      if (!is.null(cache)) assign(metadata_key, combo_info, envir = cache)
    }
    combo_info <- adjust_combo_column(combo_info)
    combo_info <- combo_info[!is.na(combo_info$Combo) & combo_info$Combo == combo, , drop = FALSE]
  }
  result <- normalize_series_history(
    prepared, hist_end_date = run_log$hist_end_date, recipe = recipe,
    combo_info = combo_info, stationary = stationary, box_cox = box_cox
  )
  result$date_type <- run_log$date_type
  result$seasonal_period <- run_log[["seasonal_period"]]
  if (!is.null(cache)) assign(cache_key, result, envir = cache)
  result
}

forecast_selection_keys <- function(context, run_type) {
  supplied <- context[[if (run_type == "Back_Test") "expected_backtests" else "expected_forecasts"]]
  if (!is.null(supplied)) return(supplied)
  splits <- context$train_test_split
  splits <- splits[splits$Run_Type == run_type, , drop = FALSE]
  dplyr::bind_rows(lapply(seq_len(nrow(splits)), function(split_index) {
    dates <- context$calendar[
      context$calendar > as.Date(splits$Train_End[split_index]) &
        context$calendar <= as.Date(splits$Test_End[split_index])
    ]
    data.frame(Train_Test_ID = rep(splits$Train_Test_ID[split_index], length(dates)), Date = dates)
  }))
}

forecast_keys_complete <- function(predictions, expected) {
  if (length(predictions$Date) != nrow(expected) || nrow(expected) == 0) return(FALSE)
  ids <- as.character(predictions$Train_Test_ID)
  dates <- as.Date(predictions$Date)
  if (anyNA(ids) || anyNA(dates)) return(FALSE)
  key_order <- order(ids, dates, method = "radix")
  ids <- ids[key_order]
  dates <- as.numeric(dates[key_order])
  if (any(utils::head(ids, -1) == utils::tail(ids, -1) & diff(dates) == 0)) return(FALSE)
  expected_ids <- as.character(expected$Train_Test_ID)
  expected_dates <- as.Date(expected$Date)
  expected_order <- order(expected_ids, expected_dates, method = "radix")
  identical(ids, expected_ids[expected_order]) &&
    identical(dates, as.numeric(expected_dates[expected_order]))
}

forecast_seasonal_period <- function(context) {
  configured <- context[["seasonal_period"]]
  if (!is.null(configured) && length(configured) && !all(is.na(configured))) {
    periods <- suppressWarnings(as.numeric(unlist(strsplit(as.character(configured), "---", fixed = TRUE))))
    periods <- periods[is.finite(periods) & periods > 1 & periods == floor(periods)]
    if (length(periods)) return(min(periods))
  }
  switch(context[["date_type"]] %||% "year",
    day = 7L, week = 52L, month = 12L, quarter = 4L, 1L
  )
}

finite_mad <- function(values) {
  values <- values[is.finite(values)]
  if (length(values)) stats::mad(values) else 0
}

forecast_reference <- function(history, horizon, context) {
  period <- forecast_seasonal_period(context)
  window_size <- max(12, 3 * period, 2 * horizon)
  actuals <- utils::tail(history$Target, window_size)
  if (!any(is.finite(actuals))) {
    stop("Forecast selection requires usable historical actuals in its reference window.", call. = FALSE)
  }
  normalization <- max(abs(actuals[is.finite(actuals)]))
  values <- if (normalization > 0) actuals / normalization else actuals
  finite_values <- values[is.finite(values)]
  scale <- max(as.numeric(stats::quantile(abs(finite_values), 0.95)), finite_mad(values), finite_mad(diff(values)))
  reference <- rep(stats::median(utils::tail(finite_values, min(length(finite_values), max(period, 4)))), horizon)
  differences <- diff(values)
  seasonal_available <- period > 1 && length(values) >= period && all(is.finite(utils::tail(values, period)))
  if (seasonal_available) {
    reference <- rep(utils::tail(values, period), length.out = horizon)
    if (length(values) > period) differences <- values[-seq_len(period)] - utils::head(values, -period)
  }
  profile <- rep(0, period)
  strength <- NA_real_
  cycle_profiles <- NULL
  amplitude_tolerance <- NA_real_
  short_seasonality <- NULL
  cycles <- floor(length(values) / period)
  if (seasonal_available && cycles >= 2) {
    complete <- utils::tail(values, cycles * period)
    if (all(is.finite(complete))) {
      time <- seq_along(complete)
      detrended <- stats::lm.fit(cbind(1, time), complete)$residuals
      phases <- rep(seq_len(period), cycles)
      profile <- vapply(seq_len(period), function(phase) stats::median(detrended[phases == phase]), numeric(1))
      profile <- profile - mean(profile)
      cycle_profiles <- matrix(detrended, nrow = cycles, byrow = TRUE)
      cycle_profiles <- sweep(cycle_profiles, 1, rowMeans(cycle_profiles), "-")
      cycle_amplitudes <- apply(cycle_profiles, 1, function(cycle) diff(range(cycle)))
      amplitude_tolerance <- max(3 * finite_mad(cycle_amplitudes), 0.05 * diff(range(profile)))
      variation <- stats::var(detrended)
      if (is.finite(variation) && variation > 0) {
        strength <- max(0, 1 - stats::var(detrended - profile[phases]) / variation)
      }
      if (is.finite(strength) && strength >= 0.6 && period >= 3 && horizon >= 3 && horizon < period) {
        observed_phases <- seq_len(horizon)
        observed_profile <- profile[observed_phases]
        observed_amplitude <- diff(range(observed_profile))
        observed_cycles <- cycle_profiles[, observed_phases, drop = FALSE]
        phase_residuals <- sweep(observed_cycles, 2, observed_profile, "-")
        phase_noise <- max(3 * finite_mad(as.numeric(phase_residuals)), 0.05 * diff(range(profile)))
        if (observed_amplitude > phase_noise) {
          observed_amplitudes <- apply(observed_cycles, 1, function(cycle) diff(range(cycle)))
          short_seasonality <- list(
            profile = observed_profile - mean(observed_profile),
            trend = stats::median(diff(complete - profile[phases])),
            phase_noise = phase_noise,
            amplitude_tolerance = max(3 * finite_mad(observed_amplitudes), 0.05 * observed_amplitude)
          )
        }
      }
    }
  }
  list(
    normalization = normalization, scale = scale, values = values,
    reference = reference, width = max(finite_mad(differences), 0.05 * scale),
    period = period, profile = profile, seasonal_strength = strength,
    seasonal_available = seasonal_available, cycle_profiles = cycle_profiles,
    amplitude_tolerance = amplitude_tolerance, short_seasonality = short_seasonality
  )
}

forecast_path_risk <- function(forecasts, reference) {
  reasons <- character()
  components <- c(level = NA_real_, trend = NA_real_, seasonality = NA_real_)
  seasonal_fidelity <- NA_real_
  if (!length(forecasts) || any(!is.finite(forecasts))) {
    return(list(risk = 0, reasons = reasons, components = components, seasonal_fidelity = seasonal_fidelity))
  }
  if (reference$normalization == 0) {
    if (any(forecasts != 0)) {
      components["level"] <- 1
      reasons <- "unsupported_level"
    } else {
      components["level"] <- 0
    }
  } else {
    path <- forecasts / reference$normalization
    widths <- 6 * reference$width * sqrt(seq_along(path))
    components["level"] <- max(0, max(abs(path - reference$reference) / widths) - 1)
    if (components["level"] > 0) reasons <- c(reasons, "level_deviation")
    values <- reference$values
    period <- reference$period
    strong_seasonality <- is.finite(reference$seasonal_strength) && reference$seasonal_strength >= 0.6
    if (strong_seasonality) {
      phases <- ((seq_along(values) - length(values) - 1) %% period) + 1
      values <- values - reference$profile[phases]
      adjusted <- path - rep(reference$profile, length.out = length(path))
    } else {
      adjusted <- path
    }
    span <- min(length(path), length(values))
    if (span >= 4 && all(is.finite(values))) {
      changes <- diff(values)
      slopes <- if (span %% 2 == 0) {
        stats::runmed(changes, k = span - 1, endrule = "constant")[
          seq.int(span / 2, length.out = length(values) - span + 1)
        ]
      } else {
        vapply(seq_len(length(values) - span + 1), function(start) {
          stats::median(changes[seq.int(start, length.out = span - 1)])
        }, numeric(1))
      }
      slope_scale <- max(finite_mad(slopes), 0.05 * reference$scale / span)
      components["trend"] <- max(0, abs(stats::median(diff(adjusted)) - stats::median(slopes)) / (6 * slope_scale) - 1)
      if (components["trend"] > 0) reasons <- c(reasons, "trend_deviation")
    }
    if (strong_seasonality && length(path) >= period && period >= 3) {
      time <- seq_along(path)
      detrended <- stats::lm.fit(cbind(1, time), path)$residuals
      phases <- rep(seq_len(period), length.out = length(path))
      future_profile <- vapply(seq_len(period), function(phase) stats::median(detrended[phases == phase]), numeric(1))
      historical_amplitude <- diff(range(reference$profile))
      future_amplitude <- diff(range(future_profile))
      if (historical_amplitude > 0) {
        amplitude_tolerance <- reference$amplitude_tolerance %||% NA_real_
        if (is.finite(amplitude_tolerance)) {
          seasonal_fidelity <- min(.Machine$double.xmax,
            max(0, (abs(future_amplitude - historical_amplitude) - amplitude_tolerance) / historical_amplitude))
        }
        ratio <- future_amplitude / historical_amplitude
        amplitude_risk <- if (ratio == 0) 100 else max(0, ratio / 3 - 1, 1 / (3 * ratio) - 1)
        phase_correlation <- if (stats::sd(future_profile) > 0) stats::cor(reference$profile, future_profile) else NA_real_
        phase_risk <- if (is.finite(phase_correlation)) max(0, -phase_correlation) else 0
        components["seasonality"] <- max(amplitude_risk, phase_risk)
        if (amplitude_risk > 0) reasons <- c(reasons, "seasonal_amplitude")
        if (phase_risk > 0) reasons <- c(reasons, "seasonal_phase")
      }
    } else if (strong_seasonality && !is.null(reference$short_seasonality) &&
               length(path) == length(reference$short_seasonality$profile)) {
      short_reference <- reference$short_seasonality
      future_profile <- path - short_reference$trend * seq_along(path)
      future_profile <- future_profile - mean(future_profile)
      historical_amplitude <- diff(range(short_reference$profile))
      future_amplitude <- diff(range(future_profile))
      seasonal_fidelity <- min(.Machine$double.xmax,
        max(0, (abs(future_amplitude - historical_amplitude) - short_reference$amplitude_tolerance) /
          historical_amplitude))
      if (future_amplitude > short_reference$phase_noise) {
        phase_correlation <- stats::cor(short_reference$profile, future_profile)
        if (is.finite(phase_correlation)) {
          components["seasonality"] <- max(0, -phase_correlation)
          if (components["seasonality"] > 0) reasons <- c(reasons, "seasonal_phase")
        }
      }
    }
  }
  components[is.infinite(components)] <- .Machine$double.xmax
  list(risk = max(c(0, components), na.rm = TRUE), reasons = reasons, components = components,
    seasonal_fidelity = seasonal_fidelity)
}

prepare_forecast_evaluation <- function(history, context) {
  history$Date <- as.Date(history$Date)
  cutoff <- as.Date(context$hist_end_date)
  history <- history[history$Date <= cutoff, , drop = FALSE]
  history <- history[order(history$Date), , drop = FALSE]
  if (anyNA(history$Date) || anyDuplicated(history$Date) || !any(is.finite(history$Target))) {
    stop("Forecast selection requires unique dated historical actuals.", call. = FALSE)
  }
  expected_backtests <- forecast_selection_keys(context, "Back_Test")
  expected_forecasts <- forecast_selection_keys(context, "Future_Forecast")
  list(history = history, expected_backtests = expected_backtests,
    expected_forecasts = expected_forecasts,
    reference = forecast_reference(history, nrow(expected_forecasts), context))
}

forecast_backtest_accuracy <- function(history, backtest) {
  actuals <- history$Target[match(as.Date(backtest$Date), as.Date(history$Date))]
  target <- ifelse(actuals == 0, 0.1, actuals)
  valid_target <- is.finite(target)
  weighted_mape <- NA_real_
  log_weight <- NA_real_
  if (all(is.finite(backtest$Forecast)) && any(valid_target)) {
    target <- target[valid_target]
    errors <- round(abs((backtest$Forecast[valid_target] - target) / abs(target)), 4)
    weights <- abs(target) / max(abs(target))
    weighted_mape <- sum(errors * (weights / sum(weights)))
    log_weight <- log(max(abs(target))) + log(sum(weights))
  }
  list(WMAPE = weighted_mape, Log_Weight = log_weight)
}

evaluate_forecast_candidates <- function(history, backtests, forecasts, context) {
  evaluation <- context$forecast_evaluation %||% prepare_forecast_evaluation(history, context)
  history <- evaluation$history
  expected_backtests <- evaluation$expected_backtests
  expected_forecasts <- evaluation$expected_forecasts
  candidate_ids <- context$candidate_ids
  if (is.null(candidate_ids)) candidate_ids <- unique(c(backtests$Model_ID, forecasts$Model_ID))
  candidate_ids <- sort(unique(as.character(candidate_ids)), method = "radix")
  if (length(candidate_ids) == 0L) {
    stop("Forecast selection requires at least one candidate.", call. = FALSE)
  }
  if (anyNA(candidate_ids) || any(!nzchar(candidate_ids))) stop("Candidate identities must be nonmissing.", call. = FALSE)
  reference <- evaluation$reference
  rankings <- lapply(candidate_ids, function(candidate_id) {
    backtest_indices <- which(backtests$Model_ID == candidate_id)
    future_indices <- which(forecasts$Model_ID == candidate_id)
    future_indices <- future_indices[order(forecasts$Date[future_indices])]
    backtest <- list(Train_Test_ID = backtests$Train_Test_ID[backtest_indices],
      Date = backtests$Date[backtest_indices], Forecast = backtests$Forecast[backtest_indices])
    future <- list(Train_Test_ID = forecasts$Train_Test_ID[future_indices],
      Date = forecasts$Date[future_indices], Forecast = forecasts$Forecast[future_indices])
    reasons <- character()
    if (!forecast_keys_complete(backtest, expected_backtests)) reasons <- c(reasons, "incomplete_backtests")
    if (!forecast_keys_complete(future, expected_forecasts)) reasons <- c(reasons, "incomplete_forecast")
    predictions <- c(backtest$Forecast, future$Forecast)
    if (any(!is.finite(predictions))) reasons <- c(reasons, "nonfinite_forecast")
    if (reference$normalization > 0 && reference$scale > 0 &&
        any(abs(predictions / reference$normalization) / reference$scale > 100, na.rm = TRUE)) {
      reasons <- c(reasons, "catastrophic_magnitude")
    }
    accuracy <- forecast_backtest_accuracy(history, backtest)
    if (!is.finite(accuracy$WMAPE)) reasons <- c(reasons, "unavailable_accuracy")
    risk <- forecast_path_risk(if (length(reasons) == 0) future$Forecast else numeric(), reference)
    tibble::new_tibble(list(
      Model_ID = candidate_id, Eligible = length(reasons) == 0,
      WMAPE = accuracy$WMAPE, Log_Weight = accuracy$Log_Weight, Risk = risk$risk,
      Violations = length(risk$reasons), Reasons = list(c(reasons, risk$reasons)),
      Checks = list(risk$components), Seasonal_Fidelity = risk$seasonal_fidelity
    ), nrow = 1L)
  })
  if (length(rankings) == 1) rankings[[1]] else dplyr::bind_rows(rankings)
}

order_forecast_candidates <- function(rankings) {
  indices <- order(!rankings$Eligible, rankings$Risk, rankings$Violations,
    rankings$WMAPE, rankings$Model_ID, method = "radix", na.last = TRUE)
  if (length(indices) < 2L || !"Seasonal_Fidelity" %in% names(rankings)) return(indices)
  ordered <- rankings[indices, , drop = FALSE]
  same_tier <- utils::head(ordered$Eligible, -1) == utils::tail(ordered$Eligible, -1) &
    utils::head(ordered$Risk, -1) == utils::tail(ordered$Risk, -1) &
    utils::head(ordered$Violations, -1) == utils::tail(ordered$Violations, -1)
  same_tier[is.na(same_tier)] <- FALSE
  groups <- split(seq_along(indices), cumsum(c(TRUE, !same_tier)))
  for (positions in groups) {
    fidelity <- ordered$Seasonal_Fidelity[positions]
    if (all(is.finite(fidelity) & fidelity >= 0)) {
      indices[positions] <- indices[positions][order(fidelity, ordered$WMAPE[positions],
        ordered$Model_ID[positions], method = "radix", na.last = TRUE)]
    }
  }
  indices
}

rank_forecast_candidates <- function(rankings) {
  if (nrow(rankings) == 1) {
    selected_id <- if (isTRUE(rankings$Eligible[1]) && is.finite(rankings$WMAPE[1])) {
      rankings$Model_ID[1]
    } else NA_character_
    return(list(selected_id = selected_id, rankings = rankings))
  }
  eligible <- rankings[rankings$Eligible, , drop = FALSE]
  selected_id <- NA_character_
  if (nrow(eligible) > 0) {
    best_accuracy <- min(eligible$WMAPE)
    ceiling <- best_accuracy + max(0.005, 0.05 * best_accuracy)
    tolerance <- 8 * .Machine$double.eps * max(1, abs(ceiling))
    shortlist <- eligible[eligible$WMAPE <= ceiling + tolerance, , drop = FALSE]
    shortlist <- shortlist[order_forecast_candidates(shortlist), , drop = FALSE]
    selected_id <- shortlist$Model_ID[1]
  }
  rankings <- rankings[order_forecast_candidates(rankings), , drop = FALSE]
  list(selected_id = selected_id, rankings = rankings)
}

select_forecast_candidate <- function(history, backtests, forecasts, context) {
  rank_forecast_candidates(evaluate_forecast_candidates(history, backtests, forecasts, context))
}

select_series_forecasts <- function(predictions, series_data, splits,
                                    candidate_ids = unique(predictions$Model_ID),
                                    selector = select_forecast_candidate) {
  context <- series_data
  context$train_test_split <- splits
  context$candidate_ids <- candidate_ids
  backtest_ids <- splits$Train_Test_ID[splits$Run_Type == "Back_Test"]
  future_ids <- splits$Train_Test_ID[splits$Run_Type == "Future_Forecast"]
  predictions$Date <- as.Date(predictions$Date)
  result <- selector(
    series_data$history,
    predictions[predictions$Train_Test_ID %in% backtest_ids, , drop = FALSE],
    predictions[predictions$Train_Test_ID %in% future_ids, , drop = FALSE],
    context
  )
  validate_forecast_selection(result, candidate_ids)
}

screen_ensemble_inputs <- function(predictions, series_data, splits) {
  selection <- select_series_forecasts(predictions, series_data, splits)
  valid_ids <- selection$rankings$Model_ID[selection$rankings$Eligible]
  predictions <- predictions[predictions$Model_ID %in% valid_ids, , drop = FALSE]
  expected <- unique(predictions[, c("Train_Test_ID", "Date"), drop = FALSE])
  valid_ids <- valid_ids[vapply(valid_ids, function(model_id) {
    rows <- predictions[predictions$Model_ID == model_id, , drop = FALSE]
    all(is.finite(rows$Forecast)) && forecast_keys_complete(rows, expected)
  }, logical(1))]
  predictions[predictions$Model_ID %in% valid_ids, , drop = FALSE]
}

validate_forecast_selection <- function(selection, candidate_ids) {
  if (!is.list(selection) || length(selection$selected_id) != 1 ||
      !is.data.frame(selection$rankings) ||
      !all(c("Model_ID", "Eligible", "WMAPE", "Risk", "Violations", "Reasons") %in% names(selection$rankings)) ||
      anyDuplicated(selection$rankings$Model_ID) ||
      anyNA(selection$rankings$Eligible) ||
      !setequal(selection$rankings$Model_ID, candidate_ids) ||
      (!is.na(selection$selected_id) && !selection$selected_id %in% candidate_ids)) {
    stop("The forecast selector returned an invalid selection contract.", call. = FALSE)
  }
  if (!is.na(selection$selected_id) &&
      !isTRUE(selection$rankings$Eligible[match(selection$selected_id, selection$rankings$Model_ID)])) {
    stop("The forecast selector selected an ineligible candidate.", call. = FALSE)
  }
  selection
}

abort_forecast_selection <- function(combo, selection) {
  rlang::abort(
    paste0("No acceptable forecast candidate for series '", combo, "': ",
           paste(unique(unlist(selection$rankings$Reasons)), collapse = ", "), "."),
    class = "finnts_forecast_selection_rejected", combo = combo, selection = selection
  )
}

selection_worker_result <- function(combo, selection = NULL, reused = FALSE) {
  result <- data.frame(Combo = combo, stringsAsFactors = FALSE)
  result$Selection <- list(selection)
  result$Reused <- reused
  result
}

rejected_agent_selection <- function(combos, reason) {
  selections <- stats::setNames(lapply(combos, function(combo) {
    list(selected_id = NA_character_, rankings = tibble::tibble(
      Model_ID = "Rejected", Eligible = FALSE, WMAPE = NA_real_, Risk = 0,
      Violations = 0L, Reasons = list(reason)
    ))
  }), combos)
  list(selections = selections, rejected_combos = combos, unpublished = TRUE)
}

write_rejected_forecasts <- function(tables, run_info, combo, splits, date_type, weekly_to_daily) {
  for (suffix in names(tables)) {
    rows <- tables[[suffix]]
    if (is.null(rows) || nrow(rows) == 0) next
    rows$Best_Model <- "No"
    rows <- convert_weekly_to_daily(create_prediction_intervals(rows, splits), date_type, weekly_to_daily)
    write_data(rows, combo = combo, run_info = run_info, output_type = "data", folder = "forecasts", suffix = suffix)
  }
}

native_forecast_rows <- function(rows, date_type) {
  if (identical(date_type, "week") && "Date_Day" %in% names(rows)) {
    invalid <- !is.finite(rows$Forecast)
    if (any(invalid)) {
      groups <- dplyr::group_indices(dplyr::group_by(rows, Combo, Model_ID, Train_Test_ID, Date))
      invalid_values <- vapply(split(rows$Forecast[invalid], groups[invalid]), sum, numeric(1))
      matched <- match(groups, names(invalid_values))
      affected <- !is.na(matched)
      rows$Forecast[affected] <- invalid_values[matched[affected]]
    }
    rows <- rows[!duplicated(rows[, c("Combo", "Model_ID", "Train_Test_ID", "Date")]), , drop = FALSE]
    rows$Forecast <- rows$Forecast * 7
    rows$Target <- rows$Target * 7
    rows$Date_Day <- NULL
  }
  rows
}

read_candidate_forecasts <- function(run_info, combos, run_log = NULL, cache = NULL, reconciled = TRUE) {
  if (is.null(run_log)) run_log <- read_selection_file(run_info, "logs", cache = cache)
  if (reconciled && !identical(run_log$forecast_approach, "bottoms_up")) {
    rows <- read_selection_file(run_info, "forecasts", "-reconciled", "Best-Model", cache = cache)
    rows <- rows[rows$Combo %in% combos, , drop = FALSE]
  } else {
    rows <- dplyr::bind_rows(lapply(combos, function(combo) {
      suffixes <- c(
        if (isTRUE(as.logical(run_log$run_local_models))) "-single_models",
        if (isTRUE(as.logical(run_log$run_global_models))) "-global_models",
        if (isTRUE(as.logical(run_log$run_ensemble_models))) "-ensemble_models",
        if (isTRUE(as.logical(run_log[["average_models"]]))) "-average_models"
      )
      dplyr::bind_rows(lapply(suffixes, function(suffix) {
        read_selection_file(run_info, "forecasts", suffix, combo, optional = TRUE, cache = cache)
      }))
    }))
  }
  if (nrow(rows) == 0) stop("No candidate forecasts were found in the exact run artifacts.", call. = FALSE)
  rows$Date <- as.Date(rows$Date)
  native_forecast_rows(rows, run_log$date_type)
}

read_final_predictions <- function(run_info, combo_hash, suffix) {
  filename <- paste0(hash_data(run_info$project_name), "-", hash_data(run_info$run_name),
    "-", combo_hash, suffix, ".", run_info$data_output)
  path <- fs::path(run_info$path, "forecasts", filename)
  if (is.null(run_info$storage_object) && !file.exists(path)) return(NULL)
  rows <- read_file(run_info, file_list = path, return_type = "df")
  if (!is.data.frame(rows) || !nrow(rows)) {
    stop("The model prediction artifact is empty or unreadable: ", filename, call. = FALSE)
  }
  rows
}

completed_forecast_selection <- function(predictions, series, splits) {
  required <- c("Best_Model", "Model_ID", "Train_Test_ID", "Date", "Forecast",
    "lo_80", "lo_95", "hi_80", "hi_95")
  if (!is.data.frame(predictions) || !nrow(predictions) || !all(required %in% names(predictions)) ||
      anyNA(predictions$Best_Model) || any(!predictions$Best_Model %in% c("Yes", "No"))) {
    return(NULL)
  }
  selection <- selected_forecast_accuracy(predictions, series, splits)
  if (is.na(selection$selected_id)) return(NULL)
  components <- strsplit(selection$selected_id, "_", fixed = TRUE)[[1]]
  if (length(components) > 1) {
    missing <- setdiff(components, unique(predictions$Model_ID))
    if (length(missing)) {
      stop("Saved average component predictions are missing: ", paste(missing, collapse = ", "),
        ". Restore the original model outputs before retrying.", call. = FALSE)
    }
    for (component in components) {
      rows <- predictions[predictions$Model_ID == component, , drop = FALSE]
      rows$Best_Model <- "Yes"
      if (is.na(selected_forecast_accuracy(rows, series, splits)$selected_id)) return(NULL)
    }
    required_ids <- splits$Train_Test_ID[splits$Run_Type %in% c("Back_Test", "Future_Forecast")]
    average <- predictions[predictions$Model_ID %in% components & predictions$Train_Test_ID %in% required_ids, ] %>%
      dplyr::group_by(Train_Test_ID, Date) %>%
      dplyr::summarise(Forecast = mean(Forecast), .groups = "drop") %>%
      dplyr::arrange(Train_Test_ID, Date)
    saved <- predictions[predictions$Model_ID == selection$selected_id & predictions$Train_Test_ID %in% required_ids, ] %>%
      dplyr::arrange(Train_Test_ID, Date)
    if (!isTRUE(all.equal(average$Forecast, saved$Forecast, check.attributes = FALSE))) return(NULL)
  }
  selection
}

unfinalized_forecast_rows <- function(rows, date_type) {
  if (is.null(rows) || !nrow(rows)) return(NULL)
  rows <- native_forecast_rows(rows, date_type)
  rows[, setdiff(names(rows), c("Best_Model", "lo_80", "lo_95", "hi_80", "hi_95", "Run_Type")), drop = FALSE]
}

selected_forecast_accuracy <- function(predictions, series, splits, require_forecast = TRUE) {
  series$train_test_split <- splits
  selected <- if ("Best_Model" %in% names(predictions)) {
    predictions[!is.na(predictions$Best_Model) & predictions$Best_Model == "Yes", , drop = FALSE]
  } else predictions[0, , drop = FALSE]
  selected_ids <- unique(as.character(selected$Model_ID))
  backtest <- selected[selected$Train_Test_ID %in% splits$Train_Test_ID[splits$Run_Type == "Back_Test"], , drop = FALSE]
  future <- selected[selected$Train_Test_ID %in% splits$Train_Test_ID[splits$Run_Type == "Future_Forecast"], , drop = FALSE]
  reasons <- character()
  if (length(selected_ids) != 1 || anyNA(selected_ids) || any(!nzchar(selected_ids))) {
    reasons <- c(reasons, "missing_or_ambiguous_winner")
  }
  if (!forecast_keys_complete(backtest, forecast_selection_keys(series, "Back_Test"))) {
    reasons <- c(reasons, "incomplete_backtests")
  }
  if (require_forecast && !forecast_keys_complete(future, forecast_selection_keys(series, "Future_Forecast"))) {
    reasons <- c(reasons, "incomplete_forecast")
  }
  if (any(!is.finite(backtest$Forecast)) || (require_forecast && any(!is.finite(future$Forecast)))) {
    reasons <- c(reasons, "nonfinite_forecast")
  }
  accuracy <- forecast_backtest_accuracy(series$history, backtest)
  if (!is.finite(accuracy$WMAPE)) reasons <- c(reasons, "unavailable_accuracy")
  model_id <- if (length(selected_ids) == 1 && !anyNA(selected_ids)) selected_ids else "Unselected"
  rankings <- tibble::tibble(Model_ID = model_id, Eligible = length(reasons) == 0,
    WMAPE = accuracy$WMAPE, Log_Weight = accuracy$Log_Weight,
    Risk = NA_real_, Violations = NA_integer_, Reasons = list(reasons),
    Checks = list(numeric()), Seasonal_Fidelity = NA_real_)
  list(selected_id = if (rankings$Eligible) model_id else NA_character_, rankings = rankings)
}

assess_agent_run <- function(run_info, run_log, combos, cache = new.env(parent = emptyenv()),
                             check_quality = FALSE) {
  hierarchical <- !identical(run_log$forecast_approach, "bottoms_up")
  splits <- read_selection_file(run_info, "prep_models", "-train_test_split", cache = cache)
  if (hierarchical && !check_quality) {
    rows <- read_candidate_forecasts(run_info, combos, run_log, cache)
    result <- hierarchical_selection_result(run_info, run_log, list(), rows, splits, combos, cache)
    result$run_info <- run_info
    result$run_log <- run_log
    result$forecasts <- rows
    return(result)
  }
  source_combos <- if (hierarchical) read_selection_hierarchy(run_info, cache)$hts_combos else combos
  rows <- read_candidate_forecasts(run_info, source_combos, run_log, cache, reconciled = !hierarchical)
  selections <- stats::setNames(lapply(source_combos, function(combo) {
    series <- read_series_history(run_info, combo, run_log, cache)
    predictions <- rows[rows$Combo == combo, , drop = FALSE]
    if (!check_quality) return(selected_forecast_accuracy(predictions, series, splits))
    selected <- if ("Best_Model" %in% names(predictions)) predictions[!is.na(predictions$Best_Model) & predictions$Best_Model == "Yes", , drop = FALSE] else predictions[0, ]
    selection <- select_series_forecasts(
      if (nrow(selected)) selected else predictions, series, splits
    )
    if (nrow(selected) == 0) selection$selected_id <- NA_character_
    selection
  }), source_combos)
  result <- list(selections = selections, source_selections = NULL, rejected_combos = character())
  if (hierarchical) {
    rows <- read_candidate_forecasts(run_info, combos, run_log, cache)
    result <- hierarchical_selection_result(run_info, run_log, selections, rows, splits, combos, cache)
  }
  result$run_info <- run_info
  result$run_log <- run_log
  result$forecasts <- rows
  result
}

hierarchical_selection_result <- function(run_info, run_log, source_selections, forecasts, splits,
                                          combos = NULL, cache = new.env(parent = emptyenv())) {
  hierarchy <- read_selection_hierarchy(run_info, cache)
  if (is.null(combos)) combos <- hierarchy$original_combos
  stored_combos <- utils::tail(hierarchy$hts_combos, length(hierarchy$original_combos))
  if (anyNA(match(combos, hierarchy$original_combos))) stop("Unknown original hierarchy series.", call. = FALSE)
  forecasts <- native_forecast_rows(forecasts, run_log$date_type)
  selections <- stats::setNames(lapply(combos, function(combo) {
    stored <- stored_combos[match(combo, hierarchy$original_combos)]
    series <- read_series_history(run_info, stored, run_log, cache)
    selected_forecast_accuracy(forecasts[forecasts$Combo == combo, , drop = FALSE],
      series, splits, require_forecast = FALSE)
  }), combos)
  list(selections = selections, source_selections = source_selections,
    rejected_combos = names(selections)[vapply(selections, function(selection) is.na(selection$selected_id), logical(1))])
}

agent_selection_summary <- function(result, check_quality = FALSE) {
  chosen <- lapply(result$selections, function(selection) {
    if (is.null(selection) || is.na(selection$selected_id)) return(NULL)
    if (nrow(selection$rankings) == 1 && identical(selection$rankings$Model_ID, selection$selected_id)) {
      return(selection$rankings)
    }
    selection$rankings[selection$rankings$Model_ID == selection$selected_id, , drop = FALSE]
  })
  rows <- dplyr::bind_rows(chosen)
  complete <- length(chosen) > 0 && nrow(rows) == length(chosen) &&
    all(rows$Eligible) && all(is.finite(rows$WMAPE))
  source <- if (check_quality && length(result$source_selections)) {
    agent_selection_summary(list(selections = result$source_selections), check_quality = TRUE)
  } else NULL
  if (!is.null(source)) complete <- complete && is.finite(source$weighted_mape)
  weights <- if (nrow(rows) && "Log_Weight" %in% names(rows) && all(is.finite(rows$Log_Weight))) {
    exp(rows$Log_Weight - max(rows$Log_Weight))
  } else rep(1, nrow(rows))
  fidelity <- rows[["Seasonal_Fidelity"]]
  list(
    weighted_mape = if (complete) sum(rows$WMAPE * weights) / sum(weights) else Inf,
    acceptable = complete && (!check_quality || isTRUE(result$quality_accepted) ||
      (all(!is.na(rows$Violations) & rows$Violations == 0) && (is.null(source) || source$acceptable))),
    status = if (complete) "evaluated" else if (nrow(rows)) "partial" else "rejected",
    risk = if (!is.null(source)) source$risk else if (nrow(rows)) max(rows$Risk) else 0,
    violations = if (!is.null(source)) source$violations else if (nrow(rows)) sum(rows$Violations) else 0L,
    seasonal_fidelity = if (!is.null(source)) source$seasonal_fidelity else
      if (complete && length(fidelity) == nrow(rows) && all(is.finite(fidelity) & fidelity >= 0)) {
        max(fidelity)
      } else NA_real_
  )
}

rank_agent_run_selections <- function(results) {
  best_accuracy <- function(rankings) {
    if (!nrow(rankings)) return(NA_integer_)
    rankings <- rankings[!is.na(rankings$Eligible) & rankings$Eligible & is.finite(rankings$WMAPE), , drop = FALSE]
    if (!nrow(rankings)) return(NA_integer_)
    as.integer(rankings$Model_ID[which.min(rankings$WMAPE)])
  }
  combos <- unique(unlist(lapply(results, function(result) names(result$selections))))
  series_winners <- stats::setNames(lapply(combos, function(combo) {
    rankings <- dplyr::bind_rows(lapply(seq_along(results), function(run_index) {
      selection <- results[[run_index]]$selections[[combo]]
      if (is.null(selection) || is.na(selection$selected_id)) return(NULL)
      row <- selection$rankings[selection$rankings$Model_ID == selection$selected_id, , drop = FALSE]
      row$Model_ID <- sprintf("%08d", run_index)
      row
    }))
    best_accuracy(rankings)
  }), combos)
  aggregate <- dplyr::bind_rows(lapply(seq_along(results), function(run_index) {
    result <- results[[run_index]]
    summary <- agent_selection_summary(list(selections = result$selections))
    tibble::tibble(Model_ID = sprintf("%08d", run_index),
      Eligible = is.finite(summary$weighted_mape) && setequal(names(result$selections), combos),
      WMAPE = summary$weighted_mape)
  }))
  list(series = series_winners, best_run_index = best_accuracy(aggregate))
}

agent_selection_pool <- function(agent_info, run_logs, combos, cache = new.env(parent = emptyenv())) {
  if (!is.data.frame(run_logs) || nrow(run_logs) == 0) return(list())
  run_logs <- run_logs[run_logs$agent_version == agent_info$agent_version, , drop = FALSE]
  run_logs <- run_logs[order(run_logs$created, run_logs$run_name), , drop = FALSE]
  lapply(seq_len(nrow(run_logs)), function(run_index) {
    log <- run_logs[run_index, , drop = FALSE]
    info <- agent_info$project_info
    info$project_name <- log$project_name
    info$run_name <- log$run_name
    key <- paste0("selection:", info$project_name, ":", info$run_name, ":", paste(combos, collapse = "|"))
    if (exists(key, cache, inherits = FALSE)) return(get(key, cache, inherits = FALSE))
    result <- if (identical(as.character(log$selection_status), "rejected")) {
      selections <- stats::setNames(lapply(combos, function(combo) {
        list(selected_id = NA_character_, rankings = tibble::tibble(
          Model_ID = "Rejected", Eligible = FALSE, WMAPE = NA_real_, Risk = 0,
          Violations = 0L, Reasons = list("rejected_evaluation")
        ))
      }), combos)
      list(selections = selections, run_info = info, run_log = log)
    } else assess_agent_run(info, log, combos, cache)
    assign(key, result, cache)
    result
  })
}

agent_selection_combos <- function(agent_info, combo = NULL) {
  known <- agent_info$selection_combos
  if (is.null(combo) && length(known)) return(known)
  if (!is.null(combo) && length(known)) {
    hashes <- vapply(known, hash_data, character(1), USE.NAMES = FALSE)
    matched <- known[hashes == combo]
    if (length(matched)) return(matched)
  }
  if (is.null(combo)) stop("Global selection requires the known series list.", call. = FALSE)
  info <- agent_info$project_info
  filename <- paste0(hash_data(info$project_name), "-", hash_data(agent_info$run_id), "-", combo, ".", info$data_output)
  input <- read_file(info, file_list = fs::path(info$path, "input_data", filename), return_type = "df")
  combos <- unique(as.character(input$Combo))
  if (length(combos) != 1 || is.na(combos)) stop("Unable to identify the exact series for selection.", call. = FALSE)
  combos
}

read_selection_file <- function(run_info, folder, suffix = NULL, combo = NULL,
                                optional = FALSE, cache = NULL) {
  prefix <- paste0(hash_data(run_info$project_name), "-", hash_data(run_info$run_name))
  extension <- if (folder == "logs") "csv" else run_info$data_output
  filename <- paste0(prefix, if (!is.null(combo)) paste0("-", hash_data(combo)), suffix, ".", extension)
  path <- fs::path(run_info$path, folder, filename)
  if (!is.null(cache) && exists(path, cache, inherits = FALSE)) return(get(path, cache, inherits = FALSE))
  if (optional && is.null(run_info$storage_object) && !file.exists(path)) return(tibble::tibble())
  result <- read_file(run_info, file_list = path, return_type = "df")
  if (!is.data.frame(result) || nrow(result) == 0) {
    stop("The exact forecast artifact is empty or unreadable: ", filename, call. = FALSE)
  }
  if (!is.null(cache)) assign(path, result, cache)
  result
}

read_selection_hierarchy <- function(run_info, cache = NULL) {
  filename <- paste0(hash_data(run_info$project_name), "-", hash_data(run_info$run_name),
    "-hts_info.", run_info$object_output)
  path <- fs::path(run_info$path, "prep_data", filename)
  if (!is.null(cache) && exists(path, cache, inherits = FALSE)) return(get(path, cache, inherits = FALSE))
  hierarchy <- read_file(run_info, file_list = path, return_type = "object")
  if (!is.list(hierarchy) || !all(c("original_combos", "hts_combos") %in% names(hierarchy))) {
    stop("Hierarchy metadata is missing the original series mapping.", call. = FALSE)
  }
  if (!is.null(cache)) assign(path, hierarchy, cache)
  hierarchy
}

assess_update_forecasts <- function(forecasts, run_info, run_log, splits,
                                    expected_components = NULL, cache = new.env(parent = emptyenv()),
                                    combos = NULL) {
  hierarchical <- !is.null(run_log[["forecast_approach"]]) && !identical(run_log$forecast_approach, "bottoms_up")
  hierarchy <- if (hierarchical) read_selection_hierarchy(run_info, cache) else NULL
  if (is.null(combos)) combos <- if (hierarchical) hierarchy$original_combos else unique(as.character(forecasts$Combo))
  if (hierarchical && !setequal(unique(as.character(forecasts$Combo)), hierarchy$hts_combos)) {
    return(list(forecasts = forecasts[0, ], source_forecasts = forecasts, selections = list(),
      source_selections = NULL, quality_rejected_combos = vapply(combos, hash_data, character(1), USE.NAMES = FALSE)))
  }
  if (!hierarchical) forecasts <- forecasts[forecasts$Combo %in% combos, , drop = FALSE]
  accepted <- character()
  rejected <- if (hierarchical) character() else setdiff(combos, unique(as.character(forecasts$Combo)))
  selections <- list()
  for (combo in unique(as.character(forecasts$Combo))) {
    rows <- forecasts[forecasts$Combo == combo, , drop = FALSE]
    series <- read_series_history(run_info, combo, run_log, cache)
    selection <- select_series_forecasts(rows, series, splits)
    selected_ids <- unique(rows$Model_ID[rows$Best_Model == "Yes"])
    components <- unique(rows$Model_ID[!is.na(rows$Recipe_ID) & rows$Recipe_ID != "simple_average"])
    required_components <- if (is.list(expected_components)) expected_components[[combo]] else expected_components
    if (!is.null(required_components)) components <- unique(c(components, required_components))
    required <- unique(c(components, selected_ids))
    required_rows <- match(required, selection$rankings$Model_ID)
    hard_pass <- length(selected_ids) == 1 && !anyNA(required_rows) && all(selection$rankings$Eligible[required_rows])
    delivered <- selection$rankings[selection$rankings$Model_ID %in% selected_ids, , drop = FALSE]
    if (hard_pass && nrow(delivered) == 1 && delivered$Violations == 0) {
      accepted <- c(accepted, combo)
      selection$selected_id <- selected_ids
    } else {
      rejected <- c(rejected, combo)
    }
    selections[[combo]] <- selection
  }
  result <- list(
    forecasts = forecasts[forecasts$Combo %in% accepted, , drop = FALSE],
    quality_rejected_combos = vapply(rejected, hash_data, character(1), USE.NAMES = FALSE),
    selections = selections, source_selections = NULL, source_forecasts = forecasts
  )
  if (hierarchical) {
    if (length(rejected)) {
      result$forecasts <- forecasts[0, ]
      result$quality_rejected_combos <- vapply(combos, hash_data, character(1), USE.NAMES = FALSE)
      return(result)
    }
    result$forecasts <- reconcile(forecasts[forecasts$Best_Model == "Yes", , drop = FALSE],
      run_info, run_log$forecast_approach, run_log$negative_forecast)
    result$forecasts <- result$forecasts[result$forecasts$Combo %in% combos, , drop = FALSE]
    selected <- hierarchical_selection_result(run_info, run_log, selections, result$forecasts, splits, combos, cache)
    result$selections <- selected$selections
    result$source_selections <- selections
  }
  result
}

record_agent_selection_attempt <- function(run_log, result, agent_info) {
  summary <- agent_selection_summary(result)
  run_log$weighted_mape <- if (is.finite(summary$weighted_mape)) summary$weighted_mape else NA_real_
  run_log$selection_status <- summary$status
  run_log$model_avg_wmape <- run_log$weighted_mape
  run_log$model_median_wmape <- run_log$weighted_mape
  run_log$model_std_wmape <- 0
  run_log$agent_version <- as.numeric(agent_info$agent_version)
  run_log$agent_forecast_approach <- agent_info$forecast_approach
  run_log
}

read_selected_agent_forecasts <- function(run_info, combos, combo_variables) {
  run_log <- read_selection_file(run_info, "logs")
  rows <- read_candidate_forecasts(run_info, combos, run_log)
  splits <- read_selection_file(run_info, "prep_models", "-train_test_split")
  rows <- create_prediction_intervals(rows, splits)
  rows <- convert_weekly_to_daily(rows, run_log$date_type, isTRUE(as.logical(run_log$weekly_to_daily)))
  rows <- rows %>%
    dplyr::select(-tidyselect::any_of(c("Run_Type", "Combo_ID", "Hyperparameter_ID"))) %>%
    dplyr::left_join(splits[, c("Train_Test_ID", "Run_Type")], by = "Train_Test_ID") %>%
    tidyr::separate(Combo, into = combo_variables, sep = "--", remove = FALSE)
  validate_best_model(rows, "read_selected_agent_forecasts")
  rows
}

validate_reconciliation_predictions <- function(predictions) {
  if (any(!is.finite(predictions$Forecast))) {
    rlang::abort("Reconciliation received non-finite forecast predictions.",
      class = "finnts_forecast_selection_rejected",
      combo = unique(predictions$Combo[!is.finite(predictions$Forecast)]))
  }
  invisible(predictions)
}