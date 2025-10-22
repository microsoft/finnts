# TimeGPT Model Implementation

#' Initialize custom TimeGPT parsnip model
#'
#' @return NA
#' @noRd
make_timegpt_model <- function() {
  parsnip::set_new_model("timegpt_model")
  parsnip::set_model_mode(model = "timegpt_model", mode = "regression")

  # Model arguments
  parsnip::set_model_arg(
    model = "timegpt_model",
    eng = "timegpt_model",
    parsnip = "forecast_horizon",
    original = "forecast_horizon",
    func = list(fun = "forecast_horizon"),
    has_submodel = FALSE
  )

  # TODO : add hyperparameters for finetune

  parsnip::set_model_engine(
    model = "timegpt_model",
    mode = "regression",
    eng = "timegpt_model"
  )

  parsnip::set_dependency(
    model = "timegpt_model",
    eng = "timegpt_model",
    pkg = "nixtlar"
  )

  parsnip::set_encoding(
    model = "timegpt_model",
    eng = "timegpt_model",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )


  parsnip::set_fit(
    model = "timegpt_model",
    eng = "timegpt_model",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "finnts", fun = "timegpt_model_fit_impl"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "timegpt_model",
    eng = "timegpt_model",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "finnts", fun = "timegpt_model_predict_impl"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )
}

#' TimeGPT model specification
#'
#' @inheritParams parsnip::boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param forecast_horizon forecast horizon
#'
#' @return Get TimeGPT model
#' @keywords internal
#' @export
timegpt_model <- function(
  mode = "regression",
  forecast_horizon = NULL
) {
  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon)
  )

  parsnip::new_model_spec(
    "timegpt_model",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

#' Fit TimeGPT model
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param forecast_horizon forecast horizon
#'
#' @return Fitted TimeGPT model object
#' @keywords internal
#' @export
timegpt_model_fit_impl <- function(
  x,
  y,
  forecast_horizon = NULL
) {
  # build dataframe for timegpt nixtla forecast client
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

  # since there is no actual training involved, we just pass the data through fit object and return it
  fit_obj <- list(
    train_data = train_df,
    forecast_horizon = forecast_horizon
  )

  class(fit_obj) <- "timegpt_model_fit"
  return(fit_obj)
}

#' Check if base URL is Azure endpoint and not Nixtla default
#'
#' @param url Base URL to check
#' @return Logical indicating if URL is Azure endpoint
#' @noRd
is_azure_url <- function(url) {
  if (is.null(url) || !nzchar(url)) {
    return(FALSE)
  }
  # azure is part of domain or subdomain
  grepl("^https?://[^/]*azure", url, ignore.case = TRUE)
}

#' Ensure URL ends with trailing slash
#'
#' @param url Base URL to normalize
#' @return URL with trailing slash
#' @noRd
normalize_url <- function(url) {
  if (!grepl("/$", url)) {
    warning("Base URL should end with '/'. Automatically appending it.")
    url <- paste0(url, "/")
  }
  url
}

#' Setup TimeGPT API client
#'
#' @return Logical indicating if Azure API should be used
#' @noRd
setup_timegpt_client <- function() {
  # Check if client is already configured
  base_url <- getOption("NIXTLA_BASE_URL")
  api_key <- getOption("NIXTLA_API_KEY")

  # Client already configured and API key is valid, determine API type from URL
  if (!is.null(base_url) && !is.null(api_key) && nixtlar::nixtla_validate_api_key()) {
    return(is_azure_url(base_url))
  }

  # Client not configured, check environment variables
  env_base_url <- Sys.getenv("NIXTLA_BASE_URL")
  env_api_key <- Sys.getenv("NIXTLA_API_KEY")

  if (nzchar(env_base_url) && nzchar(env_api_key)) {
    env_base_url <- normalize_url(env_base_url)

    # nixtlar::get_client_setup prioritizes env over options, need to update to normalized url
    Sys.setenv(NIXTLA_BASE_URL = env_base_url)

    nixtlar::nixtla_client_setup(base_url = env_base_url, api_key = env_api_key)
    return(is_azure_url(env_base_url))
  } else if (nzchar(env_api_key)) {
    # Only API key found, assume Nixtla default endpoint
    nixtlar::nixtla_client_setup(api_key = env_api_key)
    return(FALSE)
  } else {
    stop(
      "No TimeGPT credentials found. Set either:\n",
      "  - NIXTLA_API_KEY and NIXTLA_BASE_URL (for Azure or custom endpoints), or\n",
      "  - NIXTLA_API_KEY only (for default Nixtla API)"
    )
  }
}

#' Bridge prediction Function for TimeGPT Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param object model object
#' @param new_data input data to predict
#' @param ... Additional arguments
#'
#' @return predictions
#' @keywords internal
#' @export
timegpt_model_predict_impl <- function(object, new_data, ...) {
  # Setup API client and determine which endpoint to use
  use_azure <- setup_timegpt_client()

  # Prepare training data
  full_train_df <- object$train_data
  test_start <- min(new_data$Date, na.rm = TRUE)
  train_df <- full_train_df %>% dplyr::filter(Date < test_start)
  h <- nrow(new_data)

  # Make forecast based on API type
  forecast_args <- list(
    df = train_df,
    h = h,
    time_col = "Date",
    target_col = "y",
    id_col = "Combo",
    level = c(80, 95)
  )

  if (use_azure) {
    forecast_args$model <- "azureai"
  }
  base_url <- getOption("NIXTLA_BASE_URL")
  api_key <- getOption("NIXTLA_API_KEY")

  results <- do.call(nixtlar::nixtla_client_forecast, forecast_args)

  # Validate forecast data
  if (length(results$TimeGPT) != h) {
    stop(sprintf(
      "TimeGPT forecast length (%d) does not match expected horizon (%d).",
      length(results$TimeGPT), h
    ))
  }

  return(as.numeric(results$TimeGPT))
}

#' Print custom TimeGPT model
#'
#' @param x model object
#' @param ... Additional arguments
#'
#' @return Prints model info
#' @keywords internal
#' @export
print.timegpt_model <- function(x, ...) {
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' Update parameter in custom TimeGPT model
#'
#' @param object model object
#' @param parameters parameters
#' @param forecast_horizon forecast horizon
#' @param fresh fresh
#' @param ... extra args passed to TimeGPT
#'
#' @return Updated model
#' @keywords internal
#' @importFrom stats update
#' @export
update.timegpt_model <- function(
  object,
  parameters = NULL,
  forecast_horizon = NULL,
  fresh = FALSE,
  ...
) {
  eng_args <- object$eng_args

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }

  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon)
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
    "timegpt_model",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}
