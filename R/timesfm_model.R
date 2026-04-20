# TimesFM Model Implementation

#' Initialize custom TimesFM parsnip model
#'
#' @return NA
#' @noRd
make_timesfm_model <- function() {
  parsnip::set_new_model("timesfm_model")
  parsnip::set_model_mode(
    model = "timesfm_model",
    mode = "regression"
  )

  parsnip::set_model_arg(
    model = "timesfm_model",
    eng = "timesfm_model",
    parsnip = "forecast_horizon",
    original = "forecast_horizon",
    func = list(fun = "forecast_horizon"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "timesfm_model",
    eng = "timesfm_model",
    parsnip = "frequency",
    original = "frequency",
    func = list(fun = "frequency"),
    has_submodel = FALSE
  )

  parsnip::set_model_engine(
    model = "timesfm_model",
    mode = "regression",
    eng = "timesfm_model"
  )

  parsnip::set_dependency(
    model = "timesfm_model",
    eng = "timesfm_model",
    pkg = "finnts"
  )

  parsnip::set_encoding(
    model = "timesfm_model",
    eng = "timesfm_model",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "timesfm_model",
    eng = "timesfm_model",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "finnts", fun = "timesfm_model_fit_impl"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "timesfm_model",
    eng = "timesfm_model",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "finnts", fun = "timesfm_model_predict_impl"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )
}

#' TimesFM model specification
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param forecast_horizon forecast horizon
#' @param frequency frequency of data
#'
#' @return Get TimesFM model
#' @keywords internal
#' @export
timesfm_model <- function(
  mode = "regression",
  forecast_horizon = NULL,
  frequency = NULL
) {
  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon),
    frequency = rlang::enquo(frequency)
  )

  parsnip::new_model_spec(
    "timesfm_model",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

#' Fit TimesFM model
#'
#' Since TimesFM is a foundation model with no local training,
#' the fit step simply stores the training data and parameters
#' for use during prediction.
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param forecast_horizon forecast horizon
#' @param frequency frequency of data
#'
#' @return Fitted TimesFM model object
#' @keywords internal
#' @export
timesfm_model_fit_impl <- function(
  x,
  y,
  forecast_horizon = NULL,
  frequency = NULL
) {
  train_df <- as.data.frame(x)
  train_df$y <- y
  train_df <- tibble::as_tibble(train_df)
  # TimesFM does NOT support external regressors — keep only Date, Combo, y
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

  fit_obj <- list(
    train_data = train_df,
    forecast_horizon = forecast_horizon,
    frequency = frequency
  )

  class(fit_obj) <- "timesfm_model_fit"
  fit_obj
}

#' Bridge prediction function for TimesFM Models
#'
#' Prepares the training data, calls the TimesFM API, and returns
#' the point predictions.
#'
#' @param object model object
#' @param new_data input data to predict
#' @param ... Additional arguments
#'
#' @return numeric vector of predictions
#' @keywords internal
#' @export
timesfm_model_predict_impl <- function(object, new_data, ...) {
  full_train_df <- object$train_data
  test_start <- min(new_data$Date, na.rm = TRUE)
  train_df <- full_train_df |> dplyr::filter(Date < test_start)

  num_combos_in_new <- length(unique(new_data$Combo))

  if (num_combos_in_new == 0) {
    stop(
      "No unique combos found in new_data$Combo. ",
      "Cannot calculate periods per combo."
    )
  }

  periods_per_combo <- nrow(new_data) / num_combos_in_new
  h <- periods_per_combo

  # Validate inputs before sending to API
  validate_timesfm_inputs(train_df, new_data, h)

  # Map finnts frequency number to TimesFM freq string
  frequency <- object$frequency
  freq <- map_timesfm_freq(frequency)

  # Build payload and call API
  payload <- build_timesfm_payload(train_df, horizon = h, freq = freq)
  result_df <- send_timesfm_request(payload)

  num_combos_in_train <- length(unique(train_df$Combo))
  expected_length <- h * num_combos_in_train

  if (nrow(result_df) != expected_length) {
    stop(sprintf(
      paste0(
        "TimesFM forecast length (%d) ",
        "does not match expected (%d). ",
        "train_df has %d combos, %d periods per combo."
      ),
      nrow(result_df), expected_length,
      num_combos_in_train, h
    ))
  }

  as.numeric(result_df$forecast)
}

#' Validate TimesFM prediction inputs
#'
#' Checks that train_df and new_data have the required columns and
#' reasonable shapes before sending to the API.
#'
#' @param train_df Data frame with columns: Date, Combo, y.
#' @param new_data Data frame with columns: Date, Combo.
#' @param horizon Integer. Forecast horizon.
#'
#' @return Invisible NULL. Raises errors if validation fails.
#' @noRd
validate_timesfm_inputs <- function(train_df, new_data, horizon) {
  required_train_cols <- c("Date", "Combo", "y")
  missing_train <- setdiff(required_train_cols, colnames(train_df))
  if (length(missing_train) > 0) {
    stop(
      "train_df is missing required columns: ",
      paste(missing_train, collapse = ", "),
      call. = FALSE
    )
  }

  required_new_cols <- c("Date", "Combo")
  missing_new <- setdiff(required_new_cols, colnames(new_data))
  if (length(missing_new) > 0) {
    stop(
      "new_data is missing required columns: ",
      paste(missing_new, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(train_df) == 0) {
    stop("train_df has no rows.", call. = FALSE)
  }

  if (!is.numeric(horizon) || length(horizon) != 1 || horizon < 1) {
    stop("horizon must be a positive integer.", call. = FALSE)
  }

  invisible(NULL)
}

#' Read and validate a TimesFM environment variable
#'
#' @param var_name Character. Name of the environment variable.
#'
#' @return The non-empty value of the environment variable.
#' @noRd
get_timesfm_env <- function(var_name) {
  val <- Sys.getenv(var_name, unset = "")
  if (!nzchar(val)) {
    stop(
      sprintf("Environment variable '%s' is not set. ", var_name),
      "Please set it before calling the TimesFM API.",
      call. = FALSE
    )
  }
  val
}

#' Map finnts frequency number to TimesFM freq string
#'
#' TimesFM accepts: "D" (daily), "W" (weekly), "MS" (monthly start),
#' "QS" (quarterly start), "YS" (yearly start).
#'
#' @param frequency Numeric. finnts frequency number
#'   (365.25, 52.17857, 12, 4, 1).
#'
#' @return Character. TimesFM frequency string.
#' @noRd
map_timesfm_freq <- function(frequency) {
  if (is.null(frequency)) {
    return("MS")
  }

  freq_str <- switch(as.character(frequency),
    "365.25"   = "D",
    "52.17857" = "W",
    "12"       = "MS",
    "4"        = "QS",
    "1"        = "YS",
    "MS"
  )

  freq_str
}

#' Build the TimesFM API request payload
#'
#' Renames Date -> ds, Combo -> unique_id, keeps y.
#'
#' @param train_df Data frame with columns: Date, Combo, y.
#' @param horizon Integer. Number of forecast periods per series.
#' @param freq Character. TimesFM frequency string.
#'
#' @return Named list with keys: data, horizon, freq.
#' @noRd
build_timesfm_payload <- function(train_df, horizon, freq) {
  out <- train_df[, c("Combo", "Date", "y"), drop = FALSE]
  colnames(out)[colnames(out) == "Combo"] <- "unique_id"
  colnames(out)[colnames(out) == "Date"] <- "ds"

  # Convert Date objects to character for clean JSON serialisation
  out$ds <- as.character(out$ds)

  list(
    data = out,
    horizon = horizon,
    freq = freq
  )
}

#' Send a POST request to the TimesFM API
#'
#' @param payload A named list representing the full API request body.
#'
#' @return A data frame parsed from the JSON response.
#' @noRd
send_timesfm_request <- function(payload) {
  api_url <- get_timesfm_env("TIMESFM_API_URL")
  api_token <- get_timesfm_env("TIMESFM_API_TOKEN")

  body_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows")

  response <- httr::POST(
    url = api_url,
    httr::add_headers(
      Authorization = paste("Bearer", api_token),
      `Content-Type` = "application/json"
    ),
    body = body_json,
    encode = "raw"
  )

  parse_timesfm_response(response)
}

#' Parse the HTTP response from the TimesFM API
#'
#' On success (2xx) returns a data frame. On failure raises a descriptive error.
#'
#' @param response An \code{httr} response object.
#'
#' @return A data frame with columns: unique_id, ds, forecast.
#' @noRd
parse_timesfm_response <- function(response) {
  status <- httr::status_code(response)

  if (status < 200 || status >= 300) {
    body_text <- tryCatch(
      httr::content(response, as = "text", encoding = "UTF-8"),
      error = function(e) "<unable to read response body>"
    )
    stop(
      sprintf("TimesFM API request failed (HTTP %d): %s", status, body_text),
      call. = FALSE
    )
  }

  raw_text <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(
    raw_text,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE
  )

  if (!is.data.frame(result)) {
    stop(
      "Unexpected API response format: ",
      "expected a JSON array of objects.",
      call. = FALSE
    )
  }

  result
}

#' Print custom TimesFM model
#'
#' @param x model object
#' @param ... Additional arguments
#'
#' @return Prints model info
#' @keywords internal
#' @export
print.timesfm_model <- function(x, ...) {
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' Update parameter in custom TimesFM model
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
update.timesfm_model <- function(
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
    "timesfm_model",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}
