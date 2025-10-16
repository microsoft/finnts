# TimeGPT Model Implementation

#' Initialize custom TimeGPT parsnip model
#'
#' @return NA
#' @noRd
make_timegpt <- function() {
  parsnip::set_new_model("timegpt")
  parsnip::set_model_mode(model = "timegpt", mode = "regression")

  # Model arguments
  parsnip::set_model_arg(
    model = "timegpt",
    eng = "timegpt",
    parsnip = "forecast_horizon",
    original = "forecast_horizon",
    func = list(fun = "forecast_horizon"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "timegpt",
    eng = "timegpt",
    parsnip = "api_key",
    original = "api_key",
    func = list(fun = "api_key"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "timegpt",
    eng = "timegpt",
    parsnip = "external_regressors",
    original = "external_regressors",
    func = list(fun = "external_regressors"),
    has_submodel = FALSE
  )

  #TODO : add hyperparameters for finetune

  parsnip::set_model_engine(
    model = "timegpt",
    mode = "regression",
    eng = "timegpt"
  )

  parsnip::set_dependency(
    model = "timegpt",
    eng = "timegpt",
    pkg = "nixtlar"
  )

  parsnip::set_encoding(
    model = "timegpt",
    eng = "timegpt",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "timegpt",
    eng = "timegpt",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "finnts", fun = "timegpt_fit_impl"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "timegpt",
    eng = "timegpt",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "finnts", fun = "timegpt_predict_impl"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )
}

#' TimeGPT model specification
#'
#' @param mode Model mode (regression)
#' @param forecast_horizon Number of periods to forecast
#' @param external_regressors List of external regressors
#' @param api_key TimeGPT API key
#'
#' @return TimeGPT model specification
#' @export
timegpt <- function(
  mode = "regression",
  forecast_horizon = NULL,
  external_regressors = NULL,
  api_key = NULL
) {
  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon),
    external_regressors = rlang::enquo(external_regressors),
    api_key = rlang::enquo(api_key)
  )

  parsnip::new_model_spec(
    "timegpt",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

#' Fit TimeGPT model
#'
#' @param x Training data
#' @param y Target variable
#' @param ... Additional arguments
#'
#' @return Fitted TimeGPT model object
#' @keywords internal
#' @export
timegpt_fit_impl <- function(
  x,
  y,
  forecast_horizon = NULL,
  external_regressors = NULL,
  api_key = NULL,
  ...
) {
  if (is.null(api_key)) {
    # env support for FinnTS API workflow
    api_key <- Sys.getenv("TIMEGPT_API_KEY")
  }

  if (is.null(api_key) || api_key == "") {
    stop("API key is required for TimeGPT model")
  }

  nixtlar::nixtla_client_setup(api_key = api_key)

  args <- list(
    forecast_horizon = forecast_horizon,
    external_regressors = external_regressors,
    api_key = api_key
  )

  return(list(
    fit = list(ready = TRUE),
    args = args
  ))
}

#' Predict with TimeGPT model
#'
#' @param object Fitted TimeGPT model
#' @param new_data New data for prediction
#' @param ... Additional arguments
#'
#' @return Predictions
#' @keywords internal
#' @export
timegpt_predict_impl <- function(object, new_data, ...) {
  # Transform data to TimeGPT format
  args <- object$args

  # Call TimeGPT API with column name specifications
  results <- nixtlar::nixtla_client_forecast(
    df = new_data,
    h = args$forecast_horizon,
    time_col = "Date",
    target_col = "Target",
    id_col = "Combo",
    hist_exog_list = args$external_regressors
  )

  # Return numeric vector (following XGBoost pattern)
  return(as.numeric(results$TimeGPT))
}

#' Print TimeGPT model specification
#'
#' @param x TimeGPT model specification
#' @param ... Additional arguments
#'
#' @return Invisible model specification
#' @export
print.timegpt <- function(x, ...) {
  cat("TimeGPT Model (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}
