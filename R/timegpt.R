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
#'
#' @return TimeGPT model specification
#' @export
timegpt <- function(
  mode = "regression",
  forecast_horizon = NULL,
  external_regressors = NULL
) {
  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon),
    external_regressors = rlang::enquo(external_regressors)
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
  external_regressors = NULL
) {
  #build dataframe for timegpt nixtla forecast client
  train_df <- as.data.frame(x)
  train_df$y <- y
  train_df <- tibble::as_tibble(train_df)

  if (
    !is.data.frame(train_df) ||
      !"y" %in% names(train_df) ||
      nrow(train_df) == 0
  ) {
    stop("Invalid input")
  }

  #since there is no actual training involved, we just pass the data through fit object and return it
  fit_obj <- list(
    train_data = train_df,
    forecast_horizon = forecast_horizon,
    external_regressors = external_regressors
  )

  class(fit_obj) <- "timegpt_fit"
  return(fit_obj)
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
  api_key <- Sys.getenv("TIMEGPT_API_KEY")
  if (api_key == "") {
    stop("TimeGPT API key not set. Please set TIMEGPT_API_KEY environment variable.")
  }

  nixtlar::nixtla_client_setup(api_key = api_key)

  full_train_df <- object$train_data

  #handle train/test splits
  test_start <- min(new_data$Date, na.rm = TRUE)
  train_df <- full_train_df %>% dplyr::filter(Date < test_start)
  h <- nrow(new_data)

  # Call TimeGPT
  results <- nixtlar::nixtla_client_forecast(
    df = train_df,
    h = h,
    time_col = "Date",
    target_col = "y",
    id_col = "Combo",
    hist_exog_list = object$external_regressors
  )

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
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' Update method for timegpt model specs
#'
#'
#' @param object A timegpt model specification
#' @param parameters A dials::parameters object or NULL
#' @param forecast_horizon Optional new horizon
#' @param external_regressors Optional new xreg list
#' @param fresh Whether to replace (TRUE) or merge (FALSE) arguments
#' @param ... Additional args (ignored)
#'
#' @return An updated timegpt model specification
#' @export
update.timegpt <- function(
  object,
  parameters = NULL,
  forecast_horizon = NULL,
  external_regressors = NULL,
  fresh = FALSE,
  ...
) {
  eng_args <- object$eng_args

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }

  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon),
    external_regressors = rlang::enquo(external_regressors)
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
    "timegpt",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}
