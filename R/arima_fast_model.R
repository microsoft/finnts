# Adaptive Fast ARIMA Model Implementation

#' Initialize the adaptive fast ARIMA parsnip model
#'
#' @return NULL
#' @noRd
make_arima_fast_model <- function() {
  parsnip::set_new_model("arima_fast_model")
  parsnip::set_model_mode(
    model = "arima_fast_model",
    mode = "regression"
  )

  parsnip::set_model_arg(
    model = "arima_fast_model",
    eng = "arima_fast",
    parsnip = "forecast_horizon",
    original = "forecast_horizon",
    func = list(fun = "forecast_horizon"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "arima_fast_model",
    eng = "arima_fast",
    parsnip = "frequency",
    original = "frequency",
    func = list(fun = "frequency"),
    has_submodel = FALSE
  )

  parsnip::set_model_engine(
    model = "arima_fast_model",
    mode = "regression",
    eng = "arima_fast"
  )

  parsnip::set_dependency(
    model = "arima_fast_model",
    eng = "arima_fast",
    pkg = "finnts"
  )
  parsnip::set_dependency(
    model = "arima_fast_model",
    eng = "arima_fast",
    pkg = "forecast"
  )

  parsnip::set_encoding(
    model = "arima_fast_model",
    eng = "arima_fast",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "arima_fast_model",
    eng = "arima_fast",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "finnts", fun = "arima_fast_model_fit_impl"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "arima_fast_model",
    eng = "arima_fast",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "finnts", fun = "arima_fast_model_predict_impl"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )
}

#' Adaptive fast ARIMA model specification
#'
#' @param mode A single character string for the model mode.
#' @param forecast_horizon Number of periods to forecast.
#' @param frequency Frequency of the input series.
#'
#' @return An adaptive fast ARIMA model specification.
#' @keywords internal
#' @export
arima_fast_model <- function(
    mode = "regression",
    forecast_horizon = NULL,
    frequency = NULL) {
  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon),
    frequency = rlang::enquo(frequency)
  )

  parsnip::new_model_spec(
    "arima_fast_model",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

arima_fast_fourier_matrix <- function(dates, week_k, year_k) {
  date_number <- as.numeric(as.Date(dates))
  values <- list()

  if (week_k > 0) {
    for (harmonic in seq_len(week_k)) {
      angle <- 2 * pi * harmonic * date_number / 7
      values[[paste0("week_sin_", harmonic)]] <- sin(angle)
      values[[paste0("week_cos_", harmonic)]] <- cos(angle)
    }
  }

  if (year_k > 0) {
    for (harmonic in seq_len(year_k)) {
      angle <- 2 * pi * harmonic * date_number / 365.25
      values[[paste0("year_sin_", harmonic)]] <- sin(angle)
      values[[paste0("year_cos_", harmonic)]] <- cos(angle)
    }
  }

  as.matrix(as.data.frame(values, check.names = FALSE))
}

arima_fast_strategy_specs <- function(minimum_training_rows) {
  strategies <- list(
    list(
      name = "raw_d0", difference = 0L, lag = 0L,
      week_k = 0L, year_k = 0L, complexity = 1L
    ),
    list(
      name = "raw_d1", difference = 1L, lag = 0L,
      week_k = 0L, year_k = 0L, complexity = 2L
    )
  )

  if (minimum_training_rows >= 60L) {
    strategies <- append(strategies, list(
      list(
        name = "difference_7", difference = 0L, lag = 7L,
        week_k = 0L, year_k = 0L, complexity = 3L
      )
    ))
  }

  if (minimum_training_rows >= 728L) {
    strategies <- append(strategies, list(
      list(
        name = "difference_364", difference = 0L, lag = 364L,
        week_k = 0L, year_k = 0L, complexity = 4L
      )
    ))
  }

  if (minimum_training_rows >= 730L) {
    strategies <- append(strategies, list(
      list(
        name = "difference_365", difference = 0L, lag = 365L,
        week_k = 0L, year_k = 0L, complexity = 5L
      ),
      list(
        name = "fourier_low", difference = NA_integer_, lag = 0L,
        week_k = 2L, year_k = 3L, complexity = 6L
      ),
      list(
        name = "fourier_medium", difference = NA_integer_, lag = 0L,
        week_k = 3L, year_k = 6L, complexity = 7L
      )
    ))
  }

  strategies
}

arima_fast_fit_candidate <- function(y, dates, strategy) {
  lag_value <- strategy$lag
  transformed_y <- y
  transformed_dates <- dates

  if (lag_value > 0L) {
    transformed_y <- y[(lag_value + 1L):length(y)] -
      y[seq_len(length(y) - lag_value)]
    transformed_dates <- dates[(lag_value + 1L):length(dates)]
  }

  if (length(transformed_y) < 20L || any(!is.finite(transformed_y))) {
    stop(
      "ARIMA candidate has insufficient finite training values.",
      call. = FALSE
    )
  }

  difference <- strategy$difference
  if (is.na(difference)) {
    difference <- tryCatch(
      min(1L, as.integer(forecast::ndiffs(transformed_y, max.d = 1L))),
      error = function(error) 0L
    )
  }

  xreg <- NULL
  if (strategy$week_k > 0L || strategy$year_k > 0L) {
    xreg <- arima_fast_fourier_matrix(
      transformed_dates,
      strategy$week_k,
      strategy$year_k
    )
  }

  has_fourier_terms <- !is.null(xreg)

  model <- suppressWarnings(forecast::auto.arima(
    y = stats::ts(transformed_y, frequency = 1),
    d = difference,
    seasonal = FALSE,
    xreg = xreg,
    max.p = if (has_fourier_terms) 2 else 5,
    max.q = if (has_fourier_terms) 2 else 5,
    max.order = if (has_fourier_terms) 3 else 8,
    max.d = 1,
    stepwise = TRUE,
    nmodels = 30,
    approximation = TRUE,
    allowdrift = difference == 1L,
    allowmean = difference == 0L,
    method = "CSS-ML",
    optim.control = list(maxit = 100)
  ))

  list(
    model = model,
    strategy = strategy$name,
    difference = difference,
    seasonal_lag = lag_value,
    week_k = strategy$week_k,
    year_k = strategy$year_k,
    complexity = strategy$complexity,
    train_y = y,
    train_dates = dates,
    fallback = FALSE
  )
}

arima_fast_predict_candidate <- function(object, new_dates) {
  new_dates <- as.Date(new_dates)
  last_date <- max(object$train_dates)
  offsets <- as.integer(new_dates - last_date)

  if (length(offsets) == 0L || anyNA(offsets) || any(offsets <= 0L)) {
    stop(
      "ARIMA prediction dates must occur after the training history.",
      call. = FALSE
    )
  }

  horizon <- max(offsets)
  future_dates <- seq.Date(last_date + 1, by = "day", length.out = horizon)
  future_xreg <- NULL
  if (object$week_k > 0L || object$year_k > 0L) {
    future_xreg <- arima_fast_fourier_matrix(
      future_dates,
      object$week_k,
      object$year_k
    )
  }

  transformed_forecast <- as.numeric(forecast::forecast(
    object$model,
    h = horizon,
    xreg = future_xreg
  )$mean)

  if (object$seasonal_lag == 0L) {
    return(transformed_forecast[offsets])
  }

  lag_value <- object$seasonal_lag
  reconstructed <- c(object$train_y, rep(NA_real_, horizon))
  training_rows <- length(object$train_y)

  for (index in seq_len(horizon)) {
    anchor_index <- training_rows + index - lag_value
    reconstructed[training_rows + index] <-
      transformed_forecast[index] + reconstructed[anchor_index]
  }

  reconstructed[training_rows + offsets]
}

arima_fast_score <- function(actual, forecast_values, training_values) {
  if (
    length(actual) != length(forecast_values) ||
      any(!is.finite(forecast_values))
  ) {
    return(Inf)
  }

  denominator <- sum(abs(actual), na.rm = TRUE)
  if (is.finite(denominator) && denominator > sqrt(.Machine$double.eps)) {
    return(sum(abs(forecast_values - actual), na.rm = TRUE) / denominator)
  }

  scale <- mean(abs(training_values), na.rm = TRUE)
  if (!is.finite(scale) || scale <= sqrt(.Machine$double.eps)) {
    scale <- 1
  }
  mean(abs(forecast_values - actual), na.rm = TRUE) / scale
}

arima_fast_validation_ends <- function(rows, horizon) {
  ends <- rows - horizon
  ends[ends >= 30L]
}

arima_fast_fallback <- function(y, dates) {
  drift <- if (length(y) > 1L) mean(diff(y), na.rm = TRUE) else 0
  if (!is.finite(drift)) {
    drift <- 0
  }

  list(
    model = NULL,
    strategy = "drift_fallback",
    difference = 1L,
    seasonal_lag = 0L,
    week_k = 0L,
    year_k = 0L,
    complexity = Inf,
    train_y = y,
    train_dates = dates,
    drift = drift,
    fallback = TRUE
  )
}

#' Fit an adaptive fast ARIMA model
#'
#' @param x A data frame containing a `Date` column.
#' @param y Numeric training values.
#' @param forecast_horizon Number of periods to forecast.
#' @param frequency Frequency of the input series.
#'
#' @return A fitted adaptive fast ARIMA object.
#' @keywords internal
#' @export
arima_fast_model_fit_impl <- function(
    x,
    y,
    forecast_horizon = NULL,
    frequency = NULL) {
  x <- as.data.frame(x)
  if (!"Date" %in% names(x)) {
    stop("Adaptive fast ARIMA requires a 'Date' predictor.", call. = FALSE)
  }
  if (nrow(x) != length(y) || length(y) == 0L) {
    stop(
      "ARIMA predictors and outcome must have the same non-zero length.",
      call. = FALSE
    )
  }

  dates <- as.Date(x$Date)
  y <- as.numeric(y)
  order_index <- order(dates)
  dates <- dates[order_index]
  y <- y[order_index]

  if (anyNA(dates) || any(!is.finite(y))) {
    stop(
      "Adaptive fast ARIMA requires finite targets and valid dates.",
      call. = FALSE
    )
  }
  if (anyDuplicated(dates) || any(diff(dates) != 1)) {
    stop(
      "Adaptive fast ARIMA requires unique, consecutive daily dates.",
      call. = FALSE
    )
  }

  horizon <- suppressWarnings(as.integer(forecast_horizon))
  if (length(horizon) == 0L || is.na(horizon) || horizon < 1L) {
    horizon <- min(28L, max(7L, floor(length(y) / 5L)))
  }
  horizon <- min(horizon, 92L, max(1L, floor(length(y) / 4L)))

  validation_ends <- arima_fast_validation_ends(length(y), horizon)
  if (length(validation_ends) == 0L) {
    validation_ends <- max(20L, length(y) - horizon)
  }
  minimum_training_rows <- min(validation_ends)
  strategies <- arima_fast_strategy_specs(minimum_training_rows)

  scores <- lapply(strategies, function(strategy) {
    fold_errors <- numeric()
    error_message <- NA_character_

    for (training_end in validation_ends) {
      assessment_end <- min(length(y), training_end + horizon)
      training_start <- max(1L, training_end - 1825L + 1L)
      training_indices <- seq.int(training_start, training_end)
      assessment_indices <- seq.int(training_end + 1L, assessment_end)

      candidate <- tryCatch(
        arima_fast_fit_candidate(
          y[training_indices],
          dates[training_indices],
          strategy
        ),
        error = function(error) {
          error_message <<- conditionMessage(error)
          NULL
        }
      )

      if (is.null(candidate)) {
        fold_errors <- Inf
        break
      }

      predictions <- tryCatch(
        arima_fast_predict_candidate(candidate, dates[assessment_indices]),
        error = function(error) {
          error_message <<- conditionMessage(error)
          rep(NA_real_, length(assessment_indices))
        }
      )
      fold_errors <- c(
        fold_errors,
        arima_fast_score(
          y[assessment_indices],
          predictions,
          y[training_indices]
        )
      )
    }

    data.frame(
      strategy = strategy$name,
      score = if (length(fold_errors)) mean(fold_errors) else Inf,
      complexity = strategy$complexity,
      error = error_message,
      stringsAsFactors = FALSE
    )
  })
  score_tbl <- do.call(rbind, scores)

  valid_scores <- score_tbl[is.finite(score_tbl$score), , drop = FALSE]
  selected <- NULL
  selected_strategy <- NULL

  if (nrow(valid_scores) > 0L) {
    all_valid_scores <- valid_scores[
      order(valid_scores$score, valid_scores$complexity),
      ,
      drop = FALSE
    ]
    best_score <- min(valid_scores$score)
    tolerance <- best_score * 1.02
    competitive_scores <- valid_scores[
      valid_scores$score <= tolerance,
      ,
      drop = FALSE
    ]
    competitive_scores <- competitive_scores[
      order(competitive_scores$complexity, competitive_scores$score), ,
      drop = FALSE
    ]

    candidate_order <- c(
      competitive_scores$strategy,
      setdiff(all_valid_scores$strategy, competitive_scores$strategy)
    )
    for (strategy_name in candidate_order) {
      strategy <- strategies[[which(vapply(
        strategies,
        function(value) identical(value$name, strategy_name),
        logical(1)
      ))[[1]]]]
      selected <- tryCatch(
        arima_fast_fit_candidate(y, dates, strategy),
        error = function(error) NULL
      )
      if (!is.null(selected)) {
        selected_strategy <- strategy_name
        break
      }
    }
  }

  if (is.null(selected)) {
    selected <- arima_fast_fallback(y, dates)
    selected_strategy <- selected$strategy
  }

  selected_score <- score_tbl$score[
    match(selected_strategy, score_tbl$strategy)
  ]
  if (!length(selected_score) || !is.finite(selected_score)) {
    selected_score <- NA_real_
  }

  fit_obj <- c(selected, list(
    frequency = frequency,
    forecast_horizon = forecast_horizon,
    validation_wmape = selected_score,
    candidate_scores = tibble::as_tibble(score_tbl),
    candidate_count = nrow(score_tbl)
  ))
  class(fit_obj) <- "arima_fast_fit_impl"
  fit_obj
}

#' Predict an adaptive fast ARIMA model
#'
#' @param object A fitted adaptive fast ARIMA object.
#' @param new_data Future data containing a `Date` column.
#' @param ... Additional arguments.
#'
#' @return Numeric predictions.
#' @keywords internal
#' @export
arima_fast_model_predict_impl <- function(object, new_data, ...) {
  new_data <- as.data.frame(new_data)
  if (!"Date" %in% names(new_data)) {
    stop(
      "Adaptive fast ARIMA prediction requires a 'Date' column.",
      call. = FALSE
    )
  }

  if (isTRUE(object$fallback)) {
    offsets <- as.integer(as.Date(new_data$Date) - max(object$train_dates))
    if (anyNA(offsets) || any(offsets <= 0L)) {
      stop(
        "ARIMA prediction dates must occur after the training history.",
        call. = FALSE
      )
    }
    return(object$train_y[[length(object$train_y)]] + offsets * object$drift)
  }

  predictions <- arima_fast_predict_candidate(object, new_data$Date)
  if (length(predictions) != nrow(new_data) || any(!is.finite(predictions))) {
    stop(
      "Adaptive fast ARIMA produced incomplete or non-finite predictions.",
      call. = FALSE
    )
  }
  predictions
}

#' Print an adaptive fast ARIMA model specification
#'
#' @param x An adaptive fast ARIMA model specification.
#' @param ... Additional arguments.
#'
#' @return The model specification, invisibly.
#' @keywords internal
#' @export
print.arima_fast_model <- function(x, ...) {
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' Update an adaptive fast ARIMA model specification
#'
#' @param object An adaptive fast ARIMA model specification.
#' @param parameters A parameter object or `NULL`.
#' @param forecast_horizon Number of periods to forecast.
#' @param frequency Frequency of the input series.
#' @param fresh If `TRUE`, return a specification containing only new values.
#' @param ... Additional arguments.
#'
#' @return An updated adaptive fast ARIMA model specification.
#' @keywords internal
#' @importFrom stats update
#' @export
update.arima_fast_model <- function(
    object,
    parameters = NULL,
    forecast_horizon = NULL,
    frequency = NULL,
    fresh = FALSE,
    ...) {
  eng_args <- object$eng_args

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }

  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon),
    frequency = rlang::enquo(frequency)
  )
  args <- parsnip::update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- purrr::map_lgl(args, parsnip::null_value)
    if (any(null_args)) {
      args <- args[!null_args]
    }
    if (length(args) > 0L) {
      object$args[names(args)] <- args
    }
    if (length(eng_args) > 0L) {
      object$eng_args[names(eng_args)] <- eng_args
    }
  }

  parsnip::new_model_spec(
    "arima_fast_model",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

#' @export
predict.arima_fast_fit_impl <- function(object, new_data, ...) {
  arima_fast_model_predict_impl(object, new_data, ...)
}

#' @export
print.arima_fast_fit_impl <- function(x, ...) {
  cat("Adaptive Fast ARIMA\n")
  cat("Strategy:", x$strategy, "\n")
  if (!is.null(x$model) && length(x$model$arma) >= 7L) {
    cat(sprintf(
      "Fitted order: ARIMA(%d,%d,%d)\n",
      x$model$arma[1], x$model$arma[6], x$model$arma[2]
    ))
  }
  invisible(x)
}
