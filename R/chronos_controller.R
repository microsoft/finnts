# Chronos Controller Utility
# Handles POST requests to the Chronos forecasting API.
#
# Accepts finnts-style data frames (Date, Combo, y, exogenous columns),
# transforms them to the API schema (ds, unique_id, y, ...),
# sends the request, and returns a tidy data frame of forecasts.

# ──────────────────────────────────────────────────────────────────────
# Main entry point
# ──────────────────────────────────────────────────────────────────────

#' Call the Chronos forecast API
#'
#' Takes finnts-format training & future data frames, translates them
#' to the Chronos API payload schema, sends a POST request, and returns
#' the forecast as a data frame.
#'
#' @param train_df Data frame with columns: \code{Date}, \code{Combo},
#'   \code{y}, and optionally exogenous regressor columns.
#' @param new_data Data frame with columns: \code{Date}, \code{Combo},
#'   and optionally exogenous regressor columns for the future horizon.
#'   Columns that are all \code{NA} are dropped automatically.
#' @param model_type Character. Model identifier (e.g. \code{"chronos2"},
#'   \code{"chronos-bolt-base"}).
#' @param horizon Integer. Number of forecast periods per series.
#' @param exogenous_cols Character vector of exogenous column names in
#'   \code{train_df} / \code{new_data}. If \code{NULL} (default), any column
#'   that is not \code{Date}, \code{Combo}, or \code{y} is treated as exogenous.
#' @param global Logical. Fit a single global model across all series?
#'   Default \code{TRUE}.
#' @param quantile_levels Numeric vector. Quantile levels for prediction
#'   intervals. Default \code{c(0.1, 0.5, 0.9)}.
#'
#' @return A data frame with columns: \code{unique_id}, \code{ds},
#'   \code{target_name}, \code{predictions}, and one column per quantile level.
#' @noRd
chronos_forecast <- function(train_df,
                             new_data,
                             model_type,
                             horizon,
                             exogenous_cols = NULL,
                             global = TRUE,
                             quantile_levels = c(0.1, 0.5, 0.9)) {
  # ---- Validate inputs ----
  validate_chronos_inputs(train_df, new_data, horizon)

  # ---- Resolve exogenous columns ----
  if (is.null(exogenous_cols)) {
    exogenous_cols <- setdiff(colnames(train_df), c("Date", "Combo", "y"))
  }

  # ---- Build API payload pieces ----
  data_payload <- build_data_payload(train_df, exogenous_cols)
  future_payload <- build_future_payload(new_data, exogenous_cols)

  payload <- list(
    model_type = model_type,
    horizon = horizon,
    global = global,
    quantile_levels = quantile_levels,
    data = data_payload
  )

  if (!is.null(future_payload)) {
    payload$future_data <- future_payload
  }

  # ---- Send request & return data frame ----
  send_chronos_request(payload)
}


# ──────────────────────────────────────────────────────────────────────
# Input validation
# ──────────────────────────────────────────────────────────────────────

#' Validate inputs for chronos_forecast
#'
#' @param train_df Training data frame.
#' @param new_data Future data frame.
#' @param horizon Forecast horizon.
#' @noRd
validate_chronos_inputs <- function(train_df, new_data, horizon) {
  required_train <- c("Date", "Combo", "y")
  missing_train <- setdiff(required_train, colnames(train_df))
  if (length(missing_train) > 0) {
    stop(
      "train_df is missing required columns: ",
      paste(missing_train, collapse = ", "),
      call. = FALSE
    )
  }

  required_new <- c("Date", "Combo")
  missing_new <- setdiff(required_new, colnames(new_data))
  if (length(missing_new) > 0) {
    stop(
      "new_data is missing required columns: ",
      paste(missing_new, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(horizon) || horizon < 1) {
    stop("horizon must be a positive integer.", call. = FALSE)
  }
}


# ──────────────────────────────────────────────────────────────────────
# Payload builders
# ──────────────────────────────────────────────────────────────────────

#' Build the "data" portion of the API payload from train_df
#'
#' Renames Date -> ds, Combo -> unique_id, keeps y and exogenous columns.
#'
#' @param train_df Training data frame.
#' @param exogenous_cols Character vector of exogenous column names.
#'
#' @return A data frame with API column names (unique_id, ds, y, ...).
#' @noRd
build_data_payload <- function(train_df, exogenous_cols) {
  keep_cols <- intersect(exogenous_cols, colnames(train_df))

  out <- train_df[, c("Combo", "Date", "y", keep_cols), drop = FALSE]
  colnames(out)[colnames(out) == "Combo"] <- "unique_id"
  colnames(out)[colnames(out) == "Date"] <- "ds"

  # Convert Date objects to character for clean JSON serialisation
  out$ds <- as.character(out$ds)

  out
}

#' Build the "future_data" portion of the API payload from new_data
#'
#' Renames Date -> ds, Combo -> unique_id.
#' Drops any exogenous column that is entirely NA (no future values provided).
#' Returns NULL when there are no usable exogenous columns.
#'
#' @param new_data Future data frame.
#' @param exogenous_cols Character vector of exogenous column names.
#'
#' @return A data frame with API column names, or \code{NULL} if no
#'   future exogenous values are available.
#' @noRd
build_future_payload <- function(new_data, exogenous_cols) {
  keep_cols <- intersect(exogenous_cols, colnames(new_data))

  if (length(keep_cols) == 0) {
    return(NULL)
  }

  # Drop columns that are entirely NA (no future values supplied)
  has_values <- vapply(
    keep_cols,
    function(col) any(!is.na(new_data[[col]])),
    logical(1)
  )
  keep_cols <- keep_cols[has_values]

  if (length(keep_cols) == 0) {
    return(NULL)
  }

  out <- new_data[, c("Combo", "Date", keep_cols), drop = FALSE]
  colnames(out)[colnames(out) == "Combo"] <- "unique_id"
  colnames(out)[colnames(out) == "Date"] <- "ds"

  out$ds <- as.character(out$ds)

  out
}


# ──────────────────────────────────────────────────────────────────────
# HTTP layer
# ──────────────────────────────────────────────────────────────────────

#' Send a POST request to the Chronos API
#'
#' serialises the payload to JSON, sends the request, and parses
#' the response into a data frame.
#'
#' @param payload A named list representing the full API request body.
#'
#' @return A data frame parsed from the JSON response.
#' @noRd
send_chronos_request <- function(payload) {
  api_url <- get_chronos_env("CHRONOS_API_URL")
  api_token <- get_chronos_env("CHRONOS_API_TOKEN")

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

  parse_chronos_response(response)
}

#' Read and validate a Chronos environment variable
#'
#' @param var_name Character. Name of the environment variable.
#'
#' @return The non-empty value of the environment variable.
#' @noRd
get_chronos_env <- function(var_name) {
  val <- Sys.getenv(var_name, unset = "")
  if (!nzchar(val)) {
    stop(
      sprintf(
        "Environment variable '%s' is not set. "
        , var_name
      ),
      "Please set it before calling the Chronos API.",
      call. = FALSE
    )
  }
  val
}

#' Parse the HTTP response from the Chronos API
#'
#' On success (2xx) returns a data frame. On failure raises a descriptive error.
#'
#' @param response An \code{httr} response object.
#'
#' @return A data frame with columns: unique_id, ds, target_name,
#'   predictions, and quantile columns.
#' @noRd
parse_chronos_response <- function(response) {
  status <- httr::status_code(response)

  if (status < 200 || status >= 300) {
    body_text <- tryCatch(
      httr::content(response, as = "text", encoding = "UTF-8"),
      error = function(e) "<unable to read response body>"
    )
    stop(
      sprintf("Chronos API request failed (HTTP %d): %s", status, body_text),
      call. = FALSE
    )
  }

  raw_text <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(raw_text, simplifyVector = TRUE, simplifyDataFrame = TRUE)

  if (!is.data.frame(result)) {
    stop("Unexpected API response format: expected a JSON array of objects.", call. = FALSE)
  }

  result
}
