# Chronos 2 Model Implementation

#' Initialize custom Chronos 2 parsnip model
#'
#' @return NA
#' @noRd
make_chronos2_model <- function() {
  parsnip::set_new_model("chronos2_model")
  parsnip::set_model_mode(model = "chronos2_model", mode = "regression")

  # Model arguments
  parsnip::set_model_arg(
    model = "chronos2_model",
    eng = "chronos2_model",
    parsnip = "forecast_horizon",
    original = "forecast_horizon",
    func = list(fun = "forecast_horizon"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "chronos2_model",
    eng = "chronos2_model",
    parsnip = "frequency",
    original = "frequency",
    func = list(fun = "frequency"),
    has_submodel = FALSE
  )

  parsnip::set_model_engine(
    model = "chronos2_model",
    mode = "regression",
    eng = "chronos2_model"
  )

  parsnip::set_dependency(
    model = "chronos2_model",
    eng = "chronos2_model",
    pkg = "finnts"
  )

  parsnip::set_encoding(
    model = "chronos2_model",
    eng = "chronos2_model",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "chronos2_model",
    eng = "chronos2_model",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "finnts", fun = "chronos2_model_fit_impl"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "chronos2_model",
    eng = "chronos2_model",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "finnts", fun = "chronos2_model_predict_impl"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )
}

#' Chronos 2 model specification
#'
#' @inheritParams parsnip::boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param forecast_horizon forecast horizon
#' @param frequency frequency of data
#'
#' @return Get Chronos 2 model
#' @keywords internal
#' @export
chronos2_model <- function(
  mode = "regression",
  forecast_horizon = NULL,
  frequency = NULL
) {
  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon),
    frequency = rlang::enquo(frequency)
  )

  parsnip::new_model_spec(
    "chronos2_model",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

#' Fit Chronos 2 model
#'
#' Since Chronos 2 is a foundation model with no local training,
#' the fit step simply stores the training data and parameters
#' for use during prediction.
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param forecast_horizon forecast horizon
#' @param frequency frequency of data
#'
#' @return Fitted Chronos 2 model object
#' @keywords internal
#' @export
chronos2_model_fit_impl <- function(
  x,
  y,
  forecast_horizon = NULL,
  frequency = NULL
) {
  # Build dataframe with target column
  train_df <- as.data.frame(x)
  train_df$y <- y
  train_df <- tibble::as_tibble(train_df)

  if (!is.data.frame(train_df)) {
    stop("Input 'x' must be convertible to a data frame.")
  }
  if (!"y" %in% names(train_df)) {
    stop("Input data must contain a 'y' column.")
  }
  if (nrow(train_df) == 0) {
    stop("Input data frame must have at least one row.")
  }

  # No actual training — store data and params for predict step
  fit_obj <- list(
    train_data = train_df,
    forecast_horizon = forecast_horizon,
    frequency = frequency
  )

  class(fit_obj) <- "chronos2_model_fit"
  return(fit_obj)
}

#' Bridge prediction function for Chronos 2 Models
#'
#' Prepares the training and future data, calls the Chronos API via
#' \code{chronos_forecast()}, and returns the point predictions.
#'
#' @inheritParams parsnip::predict.model_fit
#' @param object model object
#' @param new_data input data to predict
#' @param ... Additional arguments
#'
#' @return numeric vector of predictions
#' @keywords internal
#' @export
chronos2_model_predict_impl <- function(object, new_data, ...) {
  # Prepare training data
  full_train_df <- object$train_data
  test_start <- min(new_data$Date, na.rm = TRUE)
  train_df <- full_train_df %>% dplyr::filter(Date < test_start)

  num_combos_in_new <- length(unique(new_data$Combo))

  if (num_combos_in_new == 0) {
    stop("No unique combos found in new_data$Combo. Cannot calculate periods per combo.")
  }

  periods_per_combo <- nrow(new_data) / num_combos_in_new
  h <- periods_per_combo

  # Pad combos with fewer than 3 observations (Chronos 2 minimum requirement)
  frequency <- object$frequency
  date_type <- if (!is.null(frequency)) get_date_type(frequency) else NULL
  train_df <- pad_chronos2_data(train_df, date_type)

  # Detect exogenous columns (_original suffix from finnts preprocessing)
  # Controller handles dropping all-NA columns from future_data
  exogenous_cols <- colnames(train_df)[grepl("_original", colnames(train_df))]
  if (length(exogenous_cols) == 0) exogenous_cols <- NULL

  # Call the Chronos API via the controller
  result_df <- chronos_forecast(
    train_df        = train_df,
    new_data        = new_data,
    model_type      = "chronos2",
    horizon         = h,
    exogenous_cols  = exogenous_cols,
    global          = TRUE,
    quantile_levels = c(0.1, 0.5, 0.9)
  )

  num_combos_in_train <- length(unique(train_df$Combo))
  expected_length <- h * num_combos_in_train

  # Validate forecast length
  if (nrow(result_df) != expected_length) {
    stop(sprintf(
      "Chronos 2 forecast length (%d) does not match expected (%d). train_df has %d combos, %d periods per combo.",
      nrow(result_df), expected_length, num_combos_in_train, h
    ))
  }

  return(as.numeric(result_df$predictions))
}

#' Print custom Chronos 2 model
#'
#' @param x model object
#' @param ... Additional arguments
#'
#' @return Prints model info
#' @keywords internal
#' @export
print.chronos2_model <- function(x, ...) {
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' Update parameter in custom Chronos 2 model
#'
#' @param object model object
#' @param parameters parameters
#' @param forecast_horizon forecast horizon
#' @param frequency frequency
#' @param fresh fresh
#' @param ... extra args
#'
#' @return Updated model
#' @keywords internal
#' @importFrom stats update
#' @export
update.chronos2_model <- function(
  object,
  parameters = NULL,
  forecast_horizon = NULL,
  frequency = NULL,
  fresh = FALSE,
  ...
) {
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
    if (length(args) > 0) {
      object$args[names(args)] <- args
    }
    if (length(eng_args) > 0) {
      object$eng_args[names(eng_args)] <- eng_args
    }
  }

  parsnip::new_model_spec(
    "chronos2_model",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

#' Pad Chronos 2 training data to minimum 3 rows per combo
#'
#' Chronos 2 requires at least 3 time points per series. During back-testing,
#' early splits may have fewer. This adds only the exact number of rows needed
#' (with y = 0 and numerics = 0) before the earliest date to reach 3.
#'
#' @param train_df Data frame with columns: Date, Combo, y, and optionally others.
#' @param date_type Character: "day", "week", "month", "quarter", or "year".
#'   Used for calendar-aware date stepping. Falls back to inferring from the
#'   data when NULL.
#'
#' @return Padded data frame with at least 3 rows per Combo.
#' @noRd
pad_chronos2_data <- function(train_df, date_type = NULL) {
  min_rows <- 3L

  combo_counts <- train_df %>%
    dplyr::group_by(Combo) %>%
    dplyr::summarise(
      n_rows = dplyr::n(),
      earliest_date = min(Date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_rows < min_rows)

  if (nrow(combo_counts) == 0) {
    return(train_df)
  }

  # Validate date_type when provided
  valid_date_types <- c("day", "week", "month", "quarter", "year")
  if (!is.null(date_type) && !date_type %in% valid_date_types) {
    stop(
      "Unsupported date_type: '", date_type, "'. Expected one of: ",
      paste(valid_date_types, collapse = ", ")
    )
  }

  # Fallback: infer step from the data when date_type is unavailable
  if (is.null(date_type)) {
    all_dates <- sort(unique(train_df$Date))
    if (length(all_dates) >= 2) {
      date_step <- as.integer(diff(all_dates[1:2]))
    } else {
      date_step <- 1L
    }
  }

  pad_rows <- lapply(seq_len(nrow(combo_counts)), function(i) {
    combo_name <- combo_counts$Combo[i]
    n_existing <- combo_counts$n_rows[i]
    earliest <- combo_counts$earliest_date[i]
    n_to_add <- min_rows - n_existing

    # Generate exactly n_to_add dates stepping backward
    if (!is.null(date_type)) {
      new_dates <- vapply(seq_len(n_to_add), function(k) {
        as.character(switch(date_type,
          "day"     = earliest - lubridate::days(k),
          "week"    = earliest - lubridate::weeks(k),
          "month"   = earliest - months(k),
          "quarter" = earliest - months(k * 3),
          "year"    = earliest - lubridate::years(k)
        ))
      }, character(1))
      new_dates <- as.Date(new_dates)
    } else {
      new_dates <- earliest - (seq_len(n_to_add) * date_step)
    }

    # Build a template row: same columns, y = 0, numerics = 0
    template <- train_df %>%
      dplyr::filter(Combo == combo_name) %>%
      dplyr::slice(1)

    template$y <- 0

    num_cols <- setdiff(
      names(template)[vapply(template, is.numeric, logical(1))],
      "y"
    )
    for (col in num_cols) {
      template[[col]] <- 0
    }

    pad <- template[rep(1, n_to_add), , drop = FALSE]
    pad$Date <- new_dates
    pad
  })

  dplyr::bind_rows(train_df, pad_rows) %>%
    dplyr::arrange(Combo, Date)
}
