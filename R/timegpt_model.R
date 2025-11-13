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

  # TODO : add hyperparameters for finetune

   parsnip::set_model_arg(
    model = "timegpt_model",
    eng = "timegpt_model",
    parsnip = "finetune_steps",
    original = "finetune_steps",
    func = list(fun = "finetune_steps"),
    has_submodel = FALSE
  )

  # Add finetune_depth argument
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

#' Get TimeGPT minimum size requirement based on frequency
#'
#' @param frequency Frequency number (1=year, 4=quarter, 12=month, 52.17857=week, 365.25=day)
#' @return Minimum size requirement
#' @noRd
get_timegpt_min_size <- function(frequency) {
  if (is.null(frequency)) {
    return(48) # Default to monthly minimum
  }
  
  # Map frequency to minimum size
  if (frequency >= 365.25) {
    # Daily or hourly
    if (frequency == 365.25) {
      return(300) # Daily
    } else {
      return(1008) # Hourly/subhourly
    }
  } else if (abs(frequency - 52.17857) < 0.1) {
    return(64) # Weekly
  } else if (frequency == 12) {
    return(48) # Monthly
  } else if (frequency == 4) {
    return(48) # Quarterly
  } else if (frequency == 1) {
    return(48) # Yearly
  } else {
    return(48) # Default
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

  num_combos_in_new <- length(unique(new_data$Combo))
  periods_per_combo <- nrow(new_data) / num_combos_in_new
  h <- periods_per_combo 
  # Get frequency from fit object
  frequency <- object$frequency

  # Apply minimum size constraints ONLY for Azure (model == "azureai")
 # Replace the current padding block that builds padding_dates via lubridate::period(...)
if (use_azure && !is.null(frequency)) {
  
  min_size <- get_timegpt_min_size(frequency)
  date_type <- get_date_type(frequency)  # convert numeric frequency → "day|week|month|quarter|year"
print("RAN1")
  train_df <- train_df %>%
    dplyr::group_by(Combo) %>%
    dplyr::group_modify(function(.x, .y) {
      n_rows <- nrow(.x)
      if (n_rows < min_size) {
        message("RAN")
        message(n_rows)
        message(min_size)
        print(paste0("RAN ", n_rows, " ", min_size, " ", date_type))
        rows_to_add <- min_size - n_rows
        earliest_date <- min(.x$Date, na.rm = TRUE)

        # Use lubridate::period() with explicit unit strings (no months())
      offset <- switch(date_type,
        "day"     = lubridate::period(rows_to_add, "days"),
        "week"    = lubridate::period(rows_to_add, "weeks"),
        "month"   = lubridate::period(rows_to_add, "months"),
        "quarter" = lubridate::period(rows_to_add * 3, "months"),
        "year"    = lubridate::period(rows_to_add, "years"),
        lubridate::period(rows_to_add, "months")
      )

      start_date <- earliest_date - offset
      cli::cli_inform("Start date: {start_date}")
      print(start_date)

        # pad using timetk (no invalid unit strings)
        padded_df <- .x %>%
          timetk::pad_by_time(
            .date_var = Date,
            .by = date_type,
            .pad_value = 0,
            .start_date = start_date,
            .end_date = max(.x$Date, na.rm = TRUE)
          )

        return(padded_df)
      } else {
        message("NOT RAN")
        return(.x)
      }
    }) %>%
    dplyr::ungroup()
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

    # Determine if this is a long horizon forecast
  is_long_horizon <- is_long_horizon_forecast(h, frequency)
  
  if (is_long_horizon) {
    print("-------is_long_horizon")
    forecast_args$model <- "timegpt-1-long-horizon"
  } else if (use_azure) {
    forecast_args$model <- "azureai"
  }

  results <- do.call(nixtlar::nixtla_client_forecast, forecast_args)

   num_combos_in_train <- length(unique(train_df$Combo))
  expected_timegpt_length <- h * num_combos_in_train  
print(paste0("h and num_combos_in_train: ", h, " ", num_combos_in_train))

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
    ...) {
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

# Custom parameter functions for TimeGPT integer hyperparameters
finetune_steps <- function(range = c(10L, 200L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(finetune_steps = "Finetune Steps"),
    finalize = NULL
  )
}

finetune_depth <- function(range = c(2L, 5L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(finetune_depth = "Finetune Depth"),
    finalize = NULL
  )
}

#' Determine if forecast horizon is "long" based on frequency
#'
#' Long horizon is defined as more than two seasonal periods:
#' - Hourly: > 48 hours (2 days)
#' - Daily: > 14 days (2 weeks)
#' - Weekly: > 104 weeks (2 years)
#' - Monthly: > 24 months (2 years)
#' - Quarterly: > 8 quarters (2 years)
#' - Yearly: > 2 years
#'
#' @param forecast_horizon Number of periods to forecast
#' @param frequency Frequency number (1=year, 4=quarter, 12=month, 52.17857=week, 365.25=day)
#' @return Logical indicating if this is a long horizon forecast
#' @noRd
is_long_horizon_forecast <- function(forecast_horizon, frequency) {
  if (is.null(forecast_horizon) || is.null(frequency)) {
    return(FALSE)
  }
  
  # Determine seasonal period and threshold based on frequency
  if (frequency >= 365.25) {
    # Daily or hourly
    if (frequency == 365.25) {
      # Daily: seasonal period is 7 days (weekly pattern), threshold is 14 days
      threshold <- 14
    } else {
      # Hourly: seasonal period is 24 hours (daily pattern), threshold is 48 hours
      threshold <- 48
    }
  } else if (abs(frequency - 52.17857) < 0.1) {
    # Weekly: seasonal period is 52 weeks (annual pattern), threshold is 104 weeks
    threshold <- 104
  } else if (frequency == 12) {
    # Monthly: seasonal period is 12 months (annual pattern), threshold is 24 months
    threshold <- 24
  } else if (frequency == 4) {
    # Quarterly: seasonal period is 4 quarters (annual pattern), threshold is 8 quarters
    threshold <- 8
  } else if (frequency == 1) {
    # Yearly: seasonal period is 1 year, threshold is 2 years
    threshold <- 2
  } else {
    # Default: use monthly threshold
    threshold <- 24
  }
  
  return(forecast_horizon > threshold)
}
