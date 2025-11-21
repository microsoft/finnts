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

  parsnip::set_model_arg(
    model = "timegpt_model",
    eng = "timegpt_model",
    parsnip = "frequency",
    original = "frequency",
    func = list(fun = "frequency"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "timegpt_model",
    eng = "timegpt_model",
    parsnip = "finetune_steps",
    original = "finetune_steps",
    func = list(fun = "finetune_steps"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "timegpt_model",
    eng = "timegpt_model",
    parsnip = "finetune_depth",
    original = "finetune_depth",
    func = list(fun = "finetune_depth"),
    has_submodel = FALSE
  )

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
#' @param frequency frequency of data
#' @param finetune_steps finetune steps
#' @param finetune_depth finetune depth
#'
#' @return Get TimeGPT model
#' @keywords internal
#' @export
timegpt_model <- function(
  mode = "regression",
  forecast_horizon = NULL,
  frequency = NULL,
  finetune_steps = NULL,
  finetune_depth = NULL
) {
  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon),
    frequency = rlang::enquo(frequency),
    finetune_steps = rlang::enquo(finetune_steps),
    finetune_depth = rlang::enquo(finetune_depth)
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
#' @param frequency frequency of data
#' @param finetune_steps finetune steps
#' @param finetune_depth finetune depth
#'
#' @return Fitted TimeGPT model object
#' @keywords internal
#' @export
timegpt_model_fit_impl <- function(
  x,
  y,
  forecast_horizon = NULL,
  frequency = NULL,
  finetune_steps = NULL,
  finetune_depth = NULL
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
    forecast_horizon = forecast_horizon,
    frequency = frequency,
    finetune_steps = finetune_steps,
    finetune_depth = finetune_depth
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
    warning("NIXTLA_BASE_URL did not end with '/'. Automatically appended.")
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


#' Get TimeGPT minimum size requirement based on date_type
#'
#' @param date_type Date type string ("day", "week", "month", "quarter", "year")
#' @return Minimum size requirement
#' @noRd
get_timegpt_min_size <- function(date_type) {
  if (is.null(date_type)) {
    return(48) # Default to monthly minimum
  }

  # Simple switch on string instead of numeric comparisons
  min_size <- switch(date_type,
    "day"     = 300,
    "week"    = 64,
    "month"   = 48,
    "quarter" = 48,
    "year"    = 48,
    48 # Default
  )

  return(min_size)
}

#' Pad time series data to meet minimum size requirements
#'
#' Pads time series data backward from the earliest date to meet minimum row
#' requirements. Useful for models that require a minimum amount of historical data.
#' Padded rows have target variable and numeric external regressors set to 0.
#'
#' @param train_df Data frame with columns: Date, Combo, y, and optionally external regressors
#' @param date_type Character string: "day", "week", "month", "quarter", or "year"
#' @param min_size Minimum number of rows required per combo (default: NULL, no padding)
#' @return Padded data frame with same structure as input, but with additional rows
#'   where y and numeric external regressors are set to 0
#' @noRd
pad_time_series_data <- function(train_df, date_type, min_size = NULL) {
  if (is.null(min_size) || is.null(date_type)) {
    return(train_df) # No padding if min_size or date_type is unknown
  }

  # Identify which combos need padding
  combo_info <- train_df %>%
    dplyr::group_by(Combo) %>%
    dplyr::summarise(
      n_rows = dplyr::n(),
      earliest_date = min(Date, na.rm = TRUE),
      latest_date = max(Date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      needs_padding = n_rows < min_size,
      rows_to_add = pmax(0L, min_size - n_rows)
    )

  # Filter to only combos that actually need padding
  combos_to_pad <- combo_info %>%
    dplyr::filter(needs_padding)

  # Only proceed if there are combos that need padding
  if (nrow(combos_to_pad) == 0) {
    return(train_df) # No padding needed
  }

  # Validate date_type upfront
  valid_date_types <- c("day", "week", "month", "quarter", "year")
  if (!date_type %in% valid_date_types) {
    stop(
      "Unsupported date_type: '", date_type, "'. Expected one of: ",
      paste(valid_date_types, collapse = ", ")
    )
  }

  # For each combo, calculate how far back we need to go to add required rows
  combos_to_pad <- combos_to_pad %>%
    dplyr::mutate(
      start_date = dplyr::case_when(
        date_type == "day" ~ earliest_date - lubridate::days(rows_to_add),
        date_type == "week" ~ earliest_date - lubridate::weeks(rows_to_add),
        date_type == "month" ~ earliest_date - months(rows_to_add),
        date_type == "quarter" ~ earliest_date - months(rows_to_add * 3),
        date_type == "year" ~ earliest_date - lubridate::years(rows_to_add)
      )
    )

  # Create complete date sequences for each combo
  create_date_sequence <- function(start, end, by_type) {
    start <- as.Date(start)
    end <- as.Date(end)

    switch(by_type,
      "day"     = seq.Date(start, end, by = "day"),
      "week"    = seq.Date(start, end, by = "week"),
      "month"   = seq.Date(start, end, by = "month"),
      "quarter" = seq.Date(start, end, by = "3 months"), # Proper quarterly sequence
      "year"    = seq.Date(start, end, by = "year"),
      stop("Unsupported date_type: '", by_type, "'")
    )
  }

  # Create full date grids for all combos that need padding
  # This creates a complete date range from (start_date to latest_date) for each combo
  padded_grids <- combos_to_pad %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      date_seq = list(create_date_sequence(start_date, latest_date, date_type))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(Combo, date_seq) %>%
    tidyr::unnest(date_seq) %>%
    dplyr::rename(Date = date_seq)

  # Join original data onto the padded grids
  all_cols <- colnames(train_df)
  target_col <- "y"
  other_cols <- setdiff(all_cols, c("Combo", "Date", target_col))

  # Left join original data onto padded grid
  train_df_padded <- padded_grids %>%
    dplyr::left_join(
      train_df %>% dplyr::filter(Combo %in% combos_to_pad$Combo),
      by = c("Combo", "Date")
    ) %>%
    dplyr::mutate(
      # Fill target variable with 0 for padded rows
      y = dplyr::if_else(is.na(y), 0, y)
    )

  # Handle external regressors and other numeric columns
  train_df_padded <- train_df_padded %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.numeric) & !dplyr::any_of(c("y")),
        .fns = ~ dplyr::if_else(is.na(.x), 0, .x)
      )
    )

  # Combine padded and non-padded combos
  combos_no_padding <- combo_info %>%
    dplyr::filter(!needs_padding) %>%
    dplyr::pull(Combo)

  # Combine both groups back together
  if (length(combos_no_padding) > 0) {
    train_df_no_padding <- train_df %>%
      dplyr::filter(Combo %in% combos_no_padding)

    train_df <- dplyr::bind_rows(train_df_padded, train_df_no_padding)
  } else {
    # All combos needed padding
    train_df <- train_df_padded
  }

  # Final cleanup and ordering
  train_df <- train_df %>%
    dplyr::arrange(Combo, Date)

  return(train_df)
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

  num_combos_in_new <- length(unique(new_data$Combo))
  
  if (num_combos_in_new == 0) {
    stop("No unique combos found in new_data$Combo. Cannot calculate periods per combo.")
  }
  
  periods_per_combo <- nrow(new_data) / num_combos_in_new
  # added support for handling horizon for multiple combos in new_data when timegpt forecats as global model
  h <- periods_per_combo

  frequency <- object$frequency
  forecast_horizon <- object$forecast_horizon
  date_type <- NULL
  if (!is.null(frequency)) {
    date_type <- get_date_type(frequency)
  }

  is_long_horizon <- FALSE
  if (!is.null(forecast_horizon) && !is.null(date_type)) {
    # long horizon should be based on forecast_horizon and not on train split h
    is_long_horizon <- is_long_horizon_forecast(forecast_horizon, date_type)
  }


  # Apply minimum size constraints ONLY for Azure AI (model == "azureai")
  # handling padding for data constraints
  if (use_azure && !is.null(date_type)) {
    # Get minimum required rows based on date frequency
    min_size <- get_timegpt_min_size(date_type)

    # Pad the training data to meet the minimum size requirement
    train_df <- pad_time_series_data(train_df, date_type, min_size)
  }

  # Extract columns containing _original since these indicate exogenous regressors as part of data pre processing and not arguments
  # Search for _original is not position specific in case external regressor was one hot encoded (e.g., category_original_A)
  original_cols <- colnames(train_df)[grepl("_original", colnames(train_df))]

  # Make forecast based on API type
  forecast_args <- list(
    df = train_df,
    h = h,
    time_col = "Date",
    target_col = "y",
    id_col = "Combo",
    level = c(80, 95)
  )
  # Add finetune parameters if provided (use defaults if NULL)
  if (!is.null(object$finetune_steps)) {
    forecast_args$finetune_steps <- object$finetune_steps
  }
  if (!is.null(object$finetune_depth)) {
    forecast_args$finetune_depth <- object$finetune_depth
  }
  # handling 3 cases for exogenous regressors:
  # 1. only future values present
  # 2. only historical values present
  # 3. both historical and future values present
  if (length(original_cols) > 0) {
    # Separate columns with future values vs without
    cols_with_future <- c()
    cols_without_future <- c()

    for (col in original_cols) {
      if (col %in% colnames(new_data) && any(!is.na(new_data[[col]]))) {
        cols_with_future <- c(cols_with_future, col)
      } else {
        cols_without_future <- c(cols_without_future, col)
      }
    }

    # SPECIAL CASE: h=1 doesn't work with future X_df, use hist_exog_list for all
    if (h == 1) {
      all_xreg_cols <- c(cols_with_future, cols_without_future)
      if (length(all_xreg_cols) > 0) {
        forecast_args$hist_exog_list <- all_xreg_cols
      }
    } else {
      # Normal case: h > 1
      if (length(cols_with_future) > 0) {
        forecast_args$X_df <- new_data %>%
          dplyr::select(Date, Combo, tidyselect::all_of(cols_with_future)) %>%
          # X_df only accepts ds, unique_id as column names
          dplyr::rename(unique_id = Combo, ds = Date)
      }

      if (length(cols_without_future) > 0) {
        forecast_args$hist_exog_list <- cols_without_future
      }
    }
  }


  if (is_long_horizon) {
    forecast_args$model <- "timegpt-1-long-horizon"
  } else if (use_azure) {
    forecast_args$model <- "azureai"
  }

  results <- do.call(nixtlar::nixtla_client_forecast, forecast_args)

  num_combos_in_train <- length(unique(train_df$Combo))
  expected_timegpt_length <- h * num_combos_in_train

  # Validate forecast data
  if (length(results$TimeGPT) != expected_timegpt_length) {
    stop(sprintf(
      "TimeGPT forecast length (%d) does not match expected (%d). train_df has %d combos, %d periods per combo.",
      length(results$TimeGPT), expected_timegpt_length, num_combos_in_train, h
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
#' @param frequency frequency
#' @param finetune_steps finetune steps
#' @param finetune_depth finetune depth
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
  frequency = NULL,
  finetune_steps = NULL,
  finetune_depth = NULL,
  ...
) {
  eng_args <- object$eng_args

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }

  args <- list(
    forecast_horizon = rlang::enquo(forecast_horizon),
    frequency = rlang::enquo(frequency),
    finetune_steps = rlang::enquo(finetune_steps),
    finetune_depth = rlang::enquo(finetune_depth)
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

#' Finetune steps parameter definition
#'
#' Defines the tunable parameter used to control the number of fine-tuning steps
#' for the TimeGPT model. This parameter is exposed via `tune::tune()` and can
#' be searched over a specified integer range.
#'
#' @param range Integer vector of length 2 giving the minimum and maximum
#'   allowable values (default: `c(0L, 200L)`).
#'
#' @return A `dials` parameter object describing the finetune steps hyperparameter.
#' @export
finetune_steps <- function(range = c(0L, 200L)) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = NULL,
    label = c(finetune_steps = "Finetune Steps"),
    finalize = NULL
  )
}

#' Finetune depth parameter definition
#'
#' Defines the tunable parameter used to control the fine-tuning depth
#' (number of layers) for the TimeGPT model. Like `finetune_steps()`, this can
#' be passed through `tune::tune()` and searched over an integer range.
#'
#' @param range Integer vector of length 2 giving the minimum and maximum
#'   allowable values (default: `c(1L, 5L)`).
#'
#' @return A `dials` parameter object describing the finetune depth hyperparameter.
#' @export
finetune_depth <- function(range = c(1L, 5L)) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = NULL,
    label = c(finetune_depth = "Finetune Depth"),
    finalize = NULL
  )
}

#' Determine if forecast horizon is "long" based on date_type
#'
#' Long horizon is defined as more than two seasonal periods:
#' - Daily: > 14 days (2 weeks)
#' - Weekly: > 104 weeks (2 years)
#' - Monthly: > 24 months (2 years)
#' - Quarterly: > 8 quarters (2 years)
#' - Yearly: > 2 years
#'
#' @param forecast_horizon Number of periods to forecast
#' @param date_type Date type string ("day", "week", "month", "quarter", "year")
#' @return Logical indicating if this is a long horizon forecast
#' @noRd
is_long_horizon_forecast <- function(forecast_horizon, date_type) {
  if (is.null(forecast_horizon) || is.null(date_type)) {
    return(FALSE)
  }

  threshold <- switch(date_type,
    "day"     = 14, # > 2 weeks
    "week"    = 104, # > 2 years
    "month"   = 24, # > 2 years
    "quarter" = 8, # > 2 years
    "year"    = 2, # > 2 years
    24 # Default to monthly threshold
  )

  return(forecast_horizon > threshold)
}
