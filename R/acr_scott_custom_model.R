# Register the custom data-frame engine once during package loading. Returns
# invisibly after updating parsnip's registry; no fits, storage or workers run.
make_acr_scott_custom_model <- function() {
  model <- "acr_scott_custom_model"
  engine <- "acr_scott_custom"
  parsnip::set_new_model(model)
  parsnip::set_model_mode(model = model, mode = "regression")
  parsnip::set_model_engine(model = model, mode = "regression", eng = engine)
  parsnip::set_dependency(model = model, eng = engine, pkg = "finnts")
  parsnip::set_model_arg(model = model, eng = engine, parsnip = "options", original = "options",
                        func = list(fun = "options"), has_submodel = FALSE)
  parsnip::set_encoding(model = model, eng = engine, mode = "regression", options = list(
    predictor_indicators = "none", compute_intercept = FALSE,
    remove_intercept = FALSE, allow_sparse_x = FALSE))
  parsnip::set_fit(model = model, eng = engine, mode = "regression", value = list(
    interface = "data.frame", protect = c("x", "y"),
    func = c(pkg = "finnts", fun = "acr_scott_custom_model_fit_impl"), defaults = list()))
  parsnip::set_pred(model = model, eng = engine, mode = "regression", type = "numeric", value = list(
    pre = NULL, post = NULL,
    func = c(pkg = "finnts", fun = "acr_scott_custom_model_predict_impl"),
    args = list(object = rlang::expr(object$fit), new_data = rlang::expr(new_data))))
}

#' Scott custom monthly forecasting model
#'
#' A global peer-aware rule model for nonnegative monthly revenue totals or
#' average daily rates. Growth references are estimated from training rows only;
#' each series retains its own recursive growth path. Use the acr_scott_custom
#' engine, at least two series, and a structural recipe retaining Date, Combo
#' and the configured peer, profile and exposure columns. No engineered target
#' lags, target transformations or automatic feature selection are used.
#'
#' @param mode Only regression is supported.
#' @param options Named list. pool_by is a required nonempty column-name vector.
#'   pool_fallbacks is an ordered list of strictly coarser nonempty subsets;
#'   min_pool_series defaults to 3; insufficient_pool is error or series.
#'   target_mode is monthly_total or daily_rate. exposure optionally names a
#'   positive adjusted-days column for monthly totals; otherwise calendar days
#'   are used. profile is standard or high_growth; profile_column optionally
#'   names a constant per-series profile. calendar_policy is scott_main,
#'   scott_pricing or none. policy_version is scott_corrected_v1. Unknown or
#'   incompatible settings fail when constructing the workflow or fitting.
#' @return A parsnip model specification. Fitting stores references and history;
#'   prediction supports one through twelve monthly offsets after the cutoff.
#' @export
acr_scott_custom_model <- function(mode = "regression", options = NULL) {
  if (!is.null(options)) options <- acr_scott_options(options)
  parsnip::new_model_spec("acr_scott_custom_model",
    args = list(options = rlang::new_quosure(options, baseenv())), eng_args = NULL,
    mode = mode, method = NULL, engine = NULL)
}

#' Print the Scott custom model specification
#' @param x A Scott custom model specification.
#' @param ... Additional printer arguments.
#' @return The specification invisibly, after printing its parameters.
#' @export
print.acr_scott_custom_model <- function(x, ...) {
  parsnip::model_printer(x, ...)
  invisible(x)
}

#' Update the Scott custom model specification
#' @param object A Scott custom model specification.
#' @param parameters Optional named parameter list containing options.
#' @param options Replacement complete model options.
#' @param fresh If TRUE, discard previous options when none are provided.
#' @param ... Unused additional arguments.
#' @return An updated specification without modifying a fitted model.
#' @export
update.acr_scott_custom_model <- function(object, parameters = NULL,
                                         options = NULL, fresh = FALSE, ...) {
  args <- parsnip::update_main_parameters(list(options = rlang::enquo(options)), parameters)
  if (fresh || !parsnip::null_value(args$options)) {
    value <- rlang::eval_tidy(args$options)
    if (!is.null(value)) value <- acr_scott_options(value)
    object$args <- list(options = rlang::new_quosure(value, baseenv()))
  }
  object
}

# Construct a pass-through workflow from representative R1 data and options.
# Only structural columns and optional observed-row metadata enter the engine;
# returns an unfitted workflow with no data-derived reference estimates.
acr_scott_custom <- function(train_data, options) {
  options <- acr_scott_options(options)
  columns <- c("Target", acr_scott_predictors(options),
               intersect(".acr_scott_observed", names(train_data)))
  recipe <- recipes::recipe(Target ~ ., data = train_data[, columns, drop = FALSE])
  specification <- acr_scott_custom_model(options = options) %>%
    parsnip::set_engine("acr_scott_custom")
  get_workflow_simple(specification, recipe)
}

# Validate and canonicalize model options without reading outcomes or storage.
# Column sets are order-insensitive; fallback-level order is meaningful. Unknown
# options and unsafe peer/target contracts fail before fitting or preparation.
acr_scott_options <- function(options, dimensions = NULL) {
  defaults <- list(pool_by = NULL, pool_fallbacks = list(), min_pool_series = 3L,
                   insufficient_pool = "error", target_mode = "monthly_total",
                   exposure = NULL, profile = "standard", profile_column = NULL,
                   calendar_policy = "scott_main", policy_version = "scott_corrected_v1")
  if (!is.list(options) || is.null(names(options)) || anyDuplicated(names(options)) ||
      any(!names(options) %in% names(defaults))) {
    stop("acr-scott-custom requires a named options list with supported settings.", call. = FALSE)
  }
  for (name in names(options)) defaults[name] <- options[name]
  options <- defaults
  if (!is.character(options$pool_by) || !length(options$pool_by) ||
      anyNA(options$pool_by) || any(!nzchar(options$pool_by))) {
    stop("pool_by must name at least one combo variable.", call. = FALSE)
  }
  options$pool_by <- sort(unique(options$pool_by))
  structural_names <- c(options$pool_by, options$exposure, options$profile_column)
  if (any(structural_names %in% c("Date", "Combo", "Target")) ||
      any(startsWith(structural_names, ".acr_scott_"))) {
    stop("Custom structural columns must not use reserved Date, Combo, Target or .acr_scott_ names.", call. = FALSE)
  }
  if (!is.null(dimensions) && !all(options$pool_by %in% dimensions)) {
    stop("pool_by must be a subset of the available combo variables.", call. = FALSE)
  }
  if (!is.list(options$pool_fallbacks)) stop("pool_fallbacks must be an ordered list.", call. = FALSE)
  previous <- options$pool_by
  for (index in seq_along(options$pool_fallbacks)) {
    columns <- options$pool_fallbacks[[index]]
    if (!is.character(columns) || !length(columns) || anyNA(columns) ||
        !all(columns %in% previous) || length(unique(columns)) >= length(previous)) {
      stop("Each pool fallback must be a nonempty strictly coarser subset of the preceding level.", call. = FALSE)
    }
    options$pool_fallbacks[[index]] <- sort(unique(columns))
    previous <- options$pool_fallbacks[[index]]
  }
  minimum <- options$min_pool_series
  if (!is.numeric(minimum) || length(minimum) != 1L || !is.finite(minimum) ||
      minimum < 2 || minimum != as.integer(minimum)) {
    stop("min_pool_series must be an integer of at least two.", call. = FALSE)
  }
  options$min_pool_series <- as.integer(minimum)
  choices <- list(insufficient_pool = c("error", "series"),
                  target_mode = c("monthly_total", "daily_rate"),
                  profile = c("standard", "high_growth"),
                  calendar_policy = c("scott_main", "scott_pricing", "none"),
                  policy_version = "scott_corrected_v1")
  for (name in names(choices)) {
    value <- options[[name]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !value %in% choices[[name]]) {
      stop(paste0("Invalid acr-scott-custom option: ", name), call. = FALSE)
    }
  }
  for (name in c("exposure", "profile_column")) {
    value <- options[[name]]
    if (!is.null(value) && (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))) {
      stop(paste0(name, " must be NULL or one column name."), call. = FALSE)
    }
  }
  if (options$target_mode == "daily_rate" && !is.null(options$exposure)) {
    stop("daily_rate targets must not specify an exposure column.", call. = FALSE)
  }
  options
}

# Return the model's structural predictor names. Metadata columns are required
# even if constant; they must not be dummy-encoded or selected as learned xregs.
acr_scott_predictors <- function(options) {
  unique(c("Date", "Combo", options$pool_by, options$exposure, options$profile_column))
}

# Validate predictor columns and return a plain data frame with month keys,
# positive exposure and explicit profile. No target-derived data is constructed.
acr_scott_inputs <- function(data, options) {
  data <- as.data.frame(data)
  required <- acr_scott_predictors(options)
  if (!all(required %in% names(data))) {
    stop(paste0("Missing acr-scott-custom predictors: ",
                paste(setdiff(required, names(data)), collapse = ", ")), call. = FALSE)
  }
  if (!inherits(data$Date, "Date") || anyNA(data$Date)) {
    stop("acr-scott-custom requires valid Date values.", call. = FALSE)
  }
  data$Combo <- as.character(data$Combo)
  blank_peers <- vapply(data[options$pool_by], function(values) {
    any(!nzchar(trimws(as.character(values))))
  }, logical(1))
  if (anyNA(data$Combo) || any(!nzchar(trimws(data$Combo))) || anyNA(data[options$pool_by]) || any(blank_peers)) {
    stop("Series and peer identifiers must not be missing or blank.", call. = FALSE)
  }
  data$.month <- acr_scott_month(data$Date)
  if (anyDuplicated(data[c("Combo", ".month")])) {
    stop("acr-scott-custom requires unique Combo/month rows.", call. = FALSE)
  }
  data$.exposure <- if (options$target_mode == "daily_rate") {
    rep(1, nrow(data))
  } else if (is.null(options$exposure)) {
    as.numeric(lubridate::days_in_month(data$Date))
  } else {
    data[[options$exposure]]
  }
  if (!is.numeric(data$.exposure) || any(!is.finite(data$.exposure) | data$.exposure <= 0)) {
    stop("Exposure must be finite and strictly positive for every required month.", call. = FALSE)
  }
  data$.profile <- if (is.null(options$profile_column)) {
    rep(options$profile, nrow(data))
  } else as.character(data[[options$profile_column]])
  if (anyNA(data$.profile) || any(!data$.profile %in% c("standard", "high_growth"))) {
    stop("Profiles must be standard or high_growth.", call. = FALSE)
  }
  data
}

# Hash explicit typed peer values and column names, never concatenated Combo
# strings. Equal rows get equal keys independent of input ordering.
acr_scott_pool_keys <- function(data, columns) {
  vapply(seq_len(nrow(data)), function(index) {
    values <- as.list(data[index, columns, drop = FALSE])
    values <- lapply(values, function(value) if (is.factor(value)) as.character(value) else value)
    digest::digest(values, algo = "xxhash64")
  }, character(1))
}

# Build one peer reference from growth rows no later than cutoff. Equal growth
# observations, not revenue weights, reproduce Scott's pooling convention.
# Invalid month-specific dispersion uses the overall valid training dispersion.
acr_scott_pool_fit <- function(history, cutoff) {
  history <- history[history$.month > cutoff - 36L & is.finite(history$growth) &
                       history$growth > -3 & history$growth < 3, , drop = FALSE]
  if (!nrow(history)) stop("Insufficient usable growth for peer references.", call. = FALSE)
  center <- stats::median(history$growth)
  spread <- if (nrow(history) > 1L) stats::sd(history$growth) else 0
  months <- seq.int(cutoff - 11L, cutoff)
  centers <- vapply(months, function(month) {
    values <- history$growth[history$.month == month]
    if (length(values)) stats::median(values) else NA_real_
  }, numeric(1))
  spreads <- vapply(months, function(month) {
    values <- history$growth[history$.month == month]
    if (length(values) > 1L) stats::sd(values) else NA_real_
  }, numeric(1))
  list(center = center, spread = spread,
       median_reference = acr_scott_reference_fit(centers, center),
       sd_reference = acr_scott_reference_fit(spreads, spread),
       last_median = if (is.finite(tail(centers, 1))) tail(centers, 1) else center,
       series_count = length(unique(history$Combo)), observations = nrow(history))
}

#' Fit the Scott custom forecasting engine
#'
#' @param x Data frame with Date, Combo and configured structural predictors.
#' @param y Nonnegative monthly totals or daily rates in the configured units.
#' @param options Named model options, including pool_by. See
#'   [acr_scott_custom_model()] for the supported contract.
#' @return A serializable acr_scott_custom_fit with shared references and series
#'   history. Invalid dates, gaps, units, profiles or peer groups raise errors.
#' @keywords internal
#' @export
acr_scott_custom_model_fit_impl <- function(x, y, options) {
  options <- acr_scott_options(options, names(x))
  data <- acr_scott_inputs(x, options)
  if (!is.numeric(y) || length(y) != nrow(data) || !length(y) ||
      any(!is.finite(y) | y < 0)) {
    stop("acr-scott-custom requires finite nonnegative targets matching predictor rows.", call. = FALSE)
  }
  data$rate <- as.numeric(y) / data$.exposure
  if (any(!is.finite(data$rate))) stop("Normalized daily rates must be finite.", call. = FALSE)
  if (".acr_scott_observed" %in% names(data)) {
    if (anyNA(data$.acr_scott_observed) || !all(data$.acr_scott_observed %in% c(0, 1))) {
      stop("Invalid observed-row metadata.", call. = FALSE)
    }
    data <- data[as.logical(data$.acr_scott_observed), , drop = FALSE]
  }
  data <- data[order(data$Combo, data$.month), , drop = FALSE]
  combos <- unique(data$Combo)
  if (length(combos) < 2L) stop("acr-scott-custom global fitting requires at least two series.", call. = FALSE)
  cutoff <- max(data$.month)
  histories <- lapply(combos, function(combo) {
    series <- data[data$Combo == combo, , drop = FALSE]
    if (nrow(series) < 7L || any(diff(series$.month) != 1L) || tail(series$.month, 1) != cutoff) {
      stop(paste0("Series ", combo, " requires at least seven consecutive observed months ending at the cutoff."), call. = FALSE)
    }
    if (nrow(unique(series[c(options$pool_by, ".profile")])) != 1L) {
      stop(paste0("Peer identity and profile must be constant within series ", combo), call. = FALSE)
    }
    previous <- head(series$rate, -1L)
    current <- tail(series$rate, -1L)
    growth <- ifelse(previous > 0, current / previous - 1,
                     ifelse(current == 0, 0, NA_real_))
    series$growth <- c(NA_real_, growth)
    series
  })
  history <- do.call(rbind, histories)
  levels <- c(list(options$pool_by), options$pool_fallbacks)
  keys <- lapply(levels, function(columns) acr_scott_pool_keys(history, columns))
  pools <- list()
  series_state <- list()
  for (combo in combos) {
    own <- history[history$Combo == combo, , drop = FALSE]
    first_index <- match(combo, history$Combo)
    selected_key <- NULL
    fallback <- "none"
    for (level in seq_along(levels)) {
      key <- keys[[level]][first_index]
      members <- history[keys[[level]] == key, , drop = FALSE]
      usable <- is.finite(members$growth) & members$.month > cutoff - 36L &
        members$growth > -3 & members$growth < 3
      if (length(unique(members$Combo[usable])) >= options$min_pool_series) {
        selected_key <- paste0(level, "-", key)
        fallback <- if (level == 1L) "none" else paste0("level_", level)
        if (is.null(pools[[selected_key]])) pools[[selected_key]] <- acr_scott_pool_fit(members, cutoff)
        break
      }
    }
    if (is.null(selected_key)) {
      if (options$insufficient_pool != "series") {
        stop(paste0("Insufficient peer series for ", combo, "; supply an approved coarser pool or explicit series fallback."), call. = FALSE)
      }
      selected_key <- paste0("series-", digest::digest(combo, algo = "xxhash64"))
      pools[[selected_key]] <- acr_scott_pool_fit(own, cutoff)
      fallback <- "series"
    }
    series_state[[combo]] <- list(
      history = tail(own[c("Date", ".month", "rate", "growth")], 37L),
      identity = own[1, options$pool_by, drop = FALSE],
      profile = own$.profile[1], pool = selected_key, pool_fallback = fallback)
  }
  structure(list(schema_version = 1L, options = options, cutoff = cutoff,
                 cutoff_date = max(data$Date), pools = pools, series = series_state),
            class = "acr_scott_custom_fit")
}

#' Explain Scott custom forecasts
#'
#' @param object A fitted engine, parsnip fit or tidymodels workflow.
#' @param new_data Future predictor rows with dates, series IDs and configured
#'   exposure and peer/profile columns. Supplied exposure requires contiguous
#'   monthly rows from the fit cutoff through the requested horizon.
#' @return A data frame in input row order with forecast, peer, analogue, growth,
#'   cap and calendar diagnostics. Prediction is deterministic and does not
#'   mutate the fit. Unknown series, changed identities or invalid dates fail.
#' @export
explain_acr_scott_custom <- function(object, new_data) {
  if (inherits(object, "workflow")) object <- workflows::extract_fit_parsnip(object)$fit
  if (inherits(object, "model_fit")) object <- object$fit
  if (!inherits(object, "acr_scott_custom_fit")) stop("Expected a fitted acr-scott-custom model.", call. = FALSE)
  options <- object$options
  data <- acr_scott_inputs(new_data, options)
  if (!nrow(data)) return(data.frame(Combo = character(), Date = as.Date(character()), Forecast = numeric()))
  if (any(!data$Combo %in% names(object$series))) stop("Unknown forecast series; refit on the new series schema.", call. = FALSE)
  offsets <- data$.month - object$cutoff
  if (any(offsets < 1L | offsets > 12L)) stop("Forecast dates must be 1 through 12 months after the fitted cutoff.", call. = FALSE)
  data$.row <- seq_len(nrow(data))
  results <- list()
  for (combo in unique(data$Combo)) {
    requested <- data[data$Combo == combo, , drop = FALSE]
    own <- object$series[[combo]]
    identity <- acr_scott_pool_keys(own$identity, options$pool_by)
    if (any(acr_scott_pool_keys(requested, options$pool_by) != identity) ||
        any(requested$.profile != own$profile)) {
      stop("Forecast peer identity or profile changed; refit the model.", call. = FALSE)
    }
    horizon <- max(requested$.month - object$cutoff)
    if (!is.null(options$exposure) && !setequal(requested$.month, object$cutoff + seq_len(horizon))) {
      stop("Supplied exposure requires all intervening future months.", call. = FALSE)
    }
    pool <- object$pools[[own$pool]]
    centers <- acr_scott_reference_predict(pool$median_reference, seq_len(horizon))
    spreads <- acr_scott_reference_predict(pool$sd_reference, seq_len(horizon), dispersion = TRUE)
    history <- own$history
    for (offset in seq_len(horizon)) {
      month <- object$cutoff + offset
      date <- as.Date(sprintf("%04d-%02d-01", (month - 1L) %/% 12L, (month - 1L) %% 12L + 1L))
      analogue_months <- month - c(36L, 24L, 12L)
      mom <- history$growth[match(analogue_months, history$.month)]
      bases <- history$rate[match(analogue_months - 1L, history$.month)]
      quality <- acr_scott_quality(mom, pool$center, pool$spread)
      recent_values <- tail(history$growth, 6L)
      recent_values <- recent_values[is.finite(recent_values)]
      recent <- if (length(recent_values)) stats::median(recent_values) else pool$center
      previous <- tail(history$rate, 1)
      if (previous == 0) {
        branch <- list(rule = "zero_base", growth = 0, unclamped = 0, analogue = NA_integer_,
                       cap_lower = NA_real_, cap_upper = NA_real_, weights = numeric(), damping = numeric())
      } else {
        branch <- acr_scott_growth(own$profile, mom, quality, bases, previous,
                                   centers[offset], spreads[offset], pool$center, pool$spread,
                                   recent, if (offset == 1L) pool$last_median else centers[offset - 1L])
      }
      growth <- acr_scott_calendar(branch$growth, (month - 1L) %% 12L + 1L, options$calendar_policy)
      rate <- max(0, previous * (1 + growth))
      if (!is.finite(rate) || !is.finite(growth)) stop(paste0("Nonfinite custom forecast for ", combo, " at ", date), call. = FALSE)
      exposure <- if (options$target_mode == "daily_rate") 1 else if (is.null(options$exposure)) {
        as.numeric(lubridate::days_in_month(date))
      } else requested$.exposure[match(month, requested$.month)]
      effective <- if (previous == 0) 0 else rate / previous - 1
      if (!is.finite(rate * exposure)) stop("Monthly forecast is not finite in the supplied units.", call. = FALSE)
      history <- rbind(history, data.frame(Date = date, .month = month, rate = rate, growth = effective))
      requested_index <- match(month, requested$.month)
      if (!is.na(requested_index)) {
        results[[length(results) + 1L]] <- data.frame(
          .row = requested$.row[requested_index], Combo = combo, Date = requested$Date[requested_index],
          Forecast = rate * exposure, DailyRate = rate, Exposure = exposure, Rule = branch$rule,
          Pool = own$pool, PoolFallback = own$pool_fallback, Profile = own$profile,
          AnalogueYearsAgo = if (is.na(branch$analogue)) NA_integer_ else 4L - branch$analogue,
          OldestGrowth = mom[1], MiddleGrowth = mom[2], LatestGrowth = mom[3],
          OldestQuality = quality[1], MiddleQuality = quality[2], LatestQuality = quality[3],
          OldestBase = bases[1], MiddleBase = bases[2], LatestBase = bases[3], PreviousRate = previous,
          Weights = if (length(branch$weights)) paste(names(branch$weights), branch$weights, sep = "=", collapse = ";") else "zero_base=1",
          Damping = paste(names(branch$damping), branch$damping, sep = "=", collapse = ";"),
          MedianReferenceMode = pool$median_reference$reason, SDReferenceMode = pool$sd_reference$reason,
          RecentGrowth = recent, ReferenceMedian = centers[offset], ReferenceSD = spreads[offset],
          GrowthBeforeCap = branch$unclamped, GrowthAfterCap = branch$growth,
          GrowthAfterCalendar = growth, EffectiveGrowth = effective,
          CapLower = branch$cap_lower, CapUpper = branch$cap_upper,
          stringsAsFactors = FALSE)
      }
    }
  }
  result <- do.call(rbind, results)
  result <- result[order(result$.row), setdiff(names(result), ".row"), drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Predict with the Scott custom engine
#'
#' @param object Fitted acr_scott_custom_fit object.
#' @param new_data Future structural predictors; see [explain_acr_scott_custom()].
#' @param ... Unused additional arguments.
#' @return Numeric original-unit forecasts, one per requested row. Errors and
#'   ordering follow [explain_acr_scott_custom()]. No fitted state is modified.
#' @keywords internal
#' @export
acr_scott_custom_model_predict_impl <- function(object, new_data, ...) {
  explain_acr_scott_custom(object, new_data)$Forecast
}

# Validate opted-in source data before any padding or artifact writes. Returns
# canonical options and target-free structural metadata aligned to explicit
# Combo/date keys. Historical missingness is an error, not an imputation policy.
acr_scott_prepare <- function(data, dimensions, target, options, hist_start,
                              hist_end, horizon, date_type, stationary, box_cox,
                              clean_outliers, clean_missing_values,
                              forecast_approach, parallel_processing, recipes) {
  if (stationary || box_cox || clean_outliers || clean_missing_values) {
    stop("acr-scott-custom requires stationary=FALSE, box_cox=FALSE, clean_outliers=FALSE and clean_missing_values=FALSE.", call. = FALSE)
  }
  if (date_type != "month" || forecast_approach != "bottoms_up" ||
      identical(parallel_processing, "spark") ||
      !is.numeric(horizon) || length(horizon) != 1L || !is.finite(horizon) ||
      horizon < 1 || horizon > 12 || horizon != as.integer(horizon) ||
      (!is.null(recipes) && !"R1" %in% recipes)) {
    stop("acr-scott-custom supports monthly bottoms_up R1 runs, horizons 1:12 and local execution only.", call. = FALSE)
  }
  if (any(startsWith(names(data), ".acr_scott_"))) stop("Input uses reserved .acr_scott_ columns.", call. = FALSE)
  options <- acr_scott_options(options, dimensions)
  if (target %in% c(dimensions, options$exposure, options$profile_column)) {
    stop("The target column cannot also be a combo, exposure or profile column.", call. = FALSE)
  }
  data <- as.data.frame(data)
  data <- tidyr::unite(data, "Combo", tidyselect::all_of(dimensions), sep = "--", remove = FALSE)
  historical <- data[data$Date >= hist_start & data$Date <= hist_end, , drop = FALSE]
  checked <- acr_scott_inputs(historical, options)
  if (!is.numeric(historical[[target]]) || any(!is.finite(historical[[target]]) | historical[[target]] < 0)) {
    stop("Custom historical targets must be finite and nonnegative; missing observations cannot be imputed.", call. = FALSE)
  }
  for (combo in unique(checked$Combo)) {
    own <- checked[checked$Combo == combo, , drop = FALSE]
    months <- sort(own$.month)
    if (length(months) < 7L || any(diff(months) != 1L) || max(months) != acr_scott_month(hist_end)) {
      stop(paste0("Series ", combo, " needs seven consecutive historical months through hist_end_date."), call. = FALSE)
    }
    if (nrow(unique(own[c(options$pool_by, ".profile")])) != 1L) {
      stop("Custom peer dimensions and profiles must be constant within each series.", call. = FALSE)
    }
    if (!is.null(options$profile_column)) {
      supplied <- data[[options$profile_column]][data$Combo == combo]
      supplied <- as.character(supplied[!is.na(supplied)])
      if (any(supplied != own$.profile[1])) {
        stop("Future profile values must match the fitted series profile.", call. = FALSE)
      }
    }
  }
  if (length(unique(checked$Combo)) < 2L) stop("acr-scott-custom requires at least two series.", call. = FALSE)
  columns <- unique(c("Combo", "Date", dimensions, options$exposure, options$profile_column))
  metadata <- data[data$Date >= hist_start, columns, drop = FALSE]
  metadata$.acr_scott_observed <- as.integer(metadata$Date <= hist_end)
  list(options = options, metadata = metadata)
}

# Attach target-free model metadata to one prepared R1 series, after engineering.
# Static identity is copied from observed source rows; observed flags and custom
# exposure use exact dates, so padding never becomes a genuine zero observation.
acr_scott_attach_metadata <- function(data, metadata, combo, dimensions, options) {
  source <- metadata[metadata$Combo == combo, , drop = FALSE]
  source <- source[order(source$Date), , drop = FALSE]
  if (!nrow(source)) stop("Missing custom series metadata.", call. = FALSE)
  index <- match(data$Date, source$Date)
  data$.acr_scott_observed <- source$.acr_scott_observed[index]
  data$.acr_scott_observed[is.na(data$.acr_scott_observed)] <- 0L
  for (name in dimensions) {
    value <- source[[name]][1]
    if (is.factor(value)) value <- as.character(value)
    data[[paste0(".acr_scott_dim_", name)]] <- paste0("acr:", jsonlite::serializeJSON(value))
  }
  if (!is.null(options$profile_column)) data$.acr_scott_profile <- source[[options$profile_column]][1]
  if (!is.null(options$exposure)) {
    data$.acr_scott_exposure <- source[[options$exposure]][index]
    leading <- data$Date < min(source$Date)
    data$.acr_scott_exposure[leading] <- 1
    if (any(!is.finite(data$.acr_scott_exposure) | data$.acr_scott_exposure <= 0)) {
      stop("Custom exposure must be provided for every historical and future month.", call. = FALSE)
    }
  }
  data
}

# Remove custom-only metadata before another model or feature selector sees R1.
# Returns the same ordinary columns and values, including existing original xregs.
acr_scott_strip_metadata <- function(data) {
  data[, !startsWith(names(data), ".acr_scott_"), drop = FALSE]
}

# Restore configured typed structural columns for the custom engine only.
# Standard R1 engineered columns remain available to the caller but the custom
# recipe selects only structural predictors. Existing target values are unchanged.
acr_scott_restore_metadata <- function(data, options) {
  dimensions <- names(data)[startsWith(names(data), ".acr_scott_dim_")]
  for (name in dimensions) {
    values <- as.character(data[[name]])
    if (anyNA(values) || any(!startsWith(values, "acr:"))) {
      stop("Invalid typed custom metadata; regenerate R1 in a new run.", call. = FALSE)
    }
    unique_values <- unique(values)
    decoded <- lapply(unique_values, function(value) jsonlite::unserializeJSON(substring(value, 5L)))
    data[[substring(name, nchar(".acr_scott_dim_") + 1L)]] <-
      do.call(c, decoded[match(values, unique_values)])
  }
  if (!is.null(options$exposure)) data[[options$exposure]] <- data$.acr_scott_exposure
  if (!is.null(options$profile_column)) data[[options$profile_column]] <- data$.acr_scott_profile
  data
}

# Read canonical model options from an existing run log without artifact I/O.
# Legacy logs without the opt-in field return NULL; invalid JSON is not ignored.
acr_scott_log_options <- function(log) {
  value <- log[["acr_scott_custom_options"]]
  if (is.null(value) || !length(value) || is.na(value[[1]])) return(NULL)
  options <- jsonlite::fromJSON(value[[1]], simplifyVector = FALSE)
  options$pool_by <- unlist(options$pool_by, use.names = FALSE)
  options$pool_fallbacks <- lapply(options$pool_fallbacks, unlist, use.names = FALSE)
  acr_scott_options(options)
}

# Summarize a fitted workflow using the existing FinnTS summary schema. Returns
# configuration and reference coefficients only; no importance package is needed.
summarize_model_acr_scott_custom <- function(workflow) {
  fit <- workflows::extract_fit_parsnip(workflow)$fit
  if (!inherits(fit, "acr_scott_custom_fit")) stop("Expected acr-scott-custom workflow.", call. = FALSE)
  result <- tibble::tibble(
    model_class = "acr_scott_custom_fit", engine = "acr_scott_custom",
    section = c("outcome", rep("engine_param", 5)),
    name = c("target_mode", "policy_version", "pool_by", "series_count", "pool_count", "cutoff"),
    value = c(fit$options$target_mode, fit$options$policy_version,
              paste(fit$options$pool_by, collapse = ", "), length(fit$series),
              length(fit$pools), as.character(fit$cutoff_date)))
  for (pool_name in names(fit$pools)) {
    pool <- fit$pools[[pool_name]]
    for (reference_name in c("median_reference", "sd_reference")) {
      reference <- pool[[reference_name]]
      coefficients <- if (is.null(reference$model)) c(fallback = reference$fallback) else stats::coef(reference$model)
      result <- dplyr::bind_rows(result, tibble::tibble(
        model_class = "acr_scott_custom_fit", engine = "acr_scott_custom", section = "coefficient",
        name = paste(pool_name, reference_name, names(coefficients), sep = ":"),
        value = as.character(unname(coefficients))))
    }
  }
  result
}

# Calculate manual-validation metrics by explicit groups. Requires finite Actual
# and Forecast columns in identical units. Returns original-unit MAE/signed bias
# and absolute-denominator WMAPE; a zero denominator remains undefined (NA).
acr_scott_validation_metrics <- function(data, groups) {
  data %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(groups))) %>%
    dplyr::summarise(
      Observations = dplyr::n(),
      AbsoluteActual = sum(abs(.data$Actual)),
      AbsoluteError = sum(abs(.data$Forecast - .data$Actual)),
      MAE = mean(abs(.data$Forecast - .data$Actual)),
      Bias = mean(.data$Forecast - .data$Actual),
      WMAPE = if (sum(abs(.data$Actual)) == 0) NA_real_ else
        sum(abs(.data$Forecast - .data$Actual)) / sum(abs(.data$Actual)),
      BiasPercent = if (sum(abs(.data$Actual)) == 0) NA_real_ else
        sum(.data$Forecast - .data$Actual) / sum(abs(.data$Actual)),
      .groups = "drop")
}