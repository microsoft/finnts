# Chronos Bolt Base Model Implementation

#' Initialize custom Chronos Bolt Base parsnip model
#'
#' @return NA
#' @noRd
make_chronos_bolt_base_model <- function() {
  parsnip::set_new_model("chronos_bolt_base_model")
  parsnip::set_model_mode(
    model = "chronos_bolt_base_model",
    mode = "regression"
  )

  # Model arguments
  parsnip::set_model_arg(
    model = "chronos_bolt_base_model",
    eng = "chronos_bolt_base_model",
    parsnip = "forecast_horizon",
    original = "forecast_horizon",
    func = list(fun = "forecast_horizon"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "chronos_bolt_base_model",
    eng = "chronos_bolt_base_model",
    parsnip = "frequency",
    original = "frequency",
    func = list(fun = "frequency"),
    has_submodel = FALSE
  )

  parsnip::set_model_engine(
    model = "chronos_bolt_base_model",
    mode = "regression",
    eng = "chronos_bolt_base_model"
  )

  parsnip::set_dependency(
    model = "chronos_bolt_base_model",
    eng = "chronos_bolt_base_model",
    pkg = "finnts"
  )

  parsnip::set_encoding(
    model = "chronos_bolt_base_model",
    eng = "chronos_bolt_base_model",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "chronos_bolt_base_model",
    eng = "chronos_bolt_base_model",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "finnts", fun = "chronos_bolt_base_model_fit_impl"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "chronos_bolt_base_model",
    eng = "chronos_bolt_base_model",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "finnts", fun = "chronos_bolt_base_model_predict_impl"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )
}

#' Chronos Bolt Base model specification
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param forecast_horizon forecast horizon
#' @param frequency frequency of data
#'
#' @return Get Chronos Bolt Base model
#' @keywords internal
#' @export
chronos_bolt_base_model <- function(
    mode = "regression",
    forecast_horizon = NULL,
    frequency = NULL) {
  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon),
    frequency = rlang::enquo(frequency)
  )

  parsnip::new_model_spec(
    "chronos_bolt_base_model",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

#' Fit Chronos Bolt Base model
#'
#' Since Chronos Bolt Base is a foundation model with no local training,
#' the fit step simply stores the training data and parameters
#' for use during prediction.
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param forecast_horizon forecast horizon
#' @param frequency frequency of data
#'
#' @return Fitted Chronos Bolt Base model object
#' @keywords internal
#' @export
chronos_bolt_base_model_fit_impl <- function(
    x,
    y,
    forecast_horizon = NULL,
    frequency = NULL) {
  # Build dataframe with only the required columns (Date, Combo, target)
  # External regressors are not supported and are excluded here
  train_df <- as.data.frame(x)
  train_df$y <- y
  train_df <- tibble::as_tibble(train_df)
  train_df <- train_df[, intersect(c("Date", "Combo", "y"), names(train_df))]

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

  class(fit_obj) <- "chronos_bolt_base_model_fit"
  return(fit_obj)
}

#' Bridge prediction function for Chronos Bolt Base Models
#'
#' Prepares the training and future data, calls the Chronos API via
#' \code{chronos_forecast()}, and returns the point predictions.
#'
#' @param object model object
#' @param new_data input data to predict
#' @param ... Additional arguments
#'
#' @return numeric vector of predictions
#' @keywords internal
#' @export
chronos_bolt_base_model_predict_impl <- function(object, new_data, ...) {
  # Prepare training data
  full_train_df <- object$train_data
  test_start <- min(new_data$Date, na.rm = TRUE)
  train_df <- full_train_df %>% dplyr::filter(Date < test_start)

  num_combos_in_new <- length(unique(new_data$Combo))

  if (num_combos_in_new == 0) {
    stop(
      "No unique combos found in new_data$Combo. ",
      "Cannot calculate periods per combo."
    )
  }

  periods_per_combo <- nrow(new_data) / num_combos_in_new
  h <- periods_per_combo

  # Pad combos with fewer than 3 observations (Chronos minimum requirement)
  frequency <- object$frequency
  date_type <- if (!is.null(frequency)) get_date_type(frequency) else NULL
  train_df <- pad_chronos2_data(train_df, date_type)

  # Chronos Bolt Base does NOT support external regressors
  exogenous_cols <- NULL

  # Call the Chronos API via the controller
  result_df <- chronos_forecast(
    train_df        = train_df,
    new_data        = new_data,
    model_type      = "chronos-bolt-base",
    horizon         = h,
    exogenous_cols  = exogenous_cols,
    global          = FALSE,
    quantile_levels = c(0.1, 0.5, 0.9)
  )

  num_combos_in_train <- length(unique(train_df$Combo))
  expected_length <- h * num_combos_in_train

  # Validate forecast length
  if (nrow(result_df) != expected_length) {
    stop(sprintf(
      paste0(
        "Chronos Bolt Base forecast length (%d) ",
        "does not match expected (%d). ",
        "train_df has %d combos, %d periods per combo."
      ),
      nrow(result_df), expected_length,
      num_combos_in_train, h
    ))
  }

  return(as.numeric(result_df$predictions))
}

#' Print custom Chronos Bolt Base model
#'
#' @param x model object
#' @param ... Additional arguments
#'
#' @return Prints model info
#' @keywords internal
#' @export
print.chronos_bolt_base_model <- function(x, ...) {
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' Update parameter in custom Chronos Bolt Base model
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
update.chronos_bolt_base_model <- function(
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
    if (length(args) > 0) {
      object$args[names(args)] <- args
    }
    if (length(eng_args) > 0) {
      object$eng_args[names(eng_args)] <- eng_args
    }
  }

  parsnip::new_model_spec(
    "chronos_bolt_base_model",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}
