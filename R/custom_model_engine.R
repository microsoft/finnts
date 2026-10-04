# Register one package-owned regression adapter at load time. This mutates only
# parsnip's model database; no user source or optional engine is loaded or run.
make_custom_model_engine <- function() {
  parsnip::set_new_model("finnts_custom")
  parsnip::set_model_mode(model = "finnts_custom", mode = "regression")
  parsnip::set_model_engine(model = "finnts_custom", mode = "regression", eng = "finnts")
  parsnip::set_dependency(model = "finnts_custom", eng = "finnts", pkg = "finnts")
  parsnip::set_encoding(model = "finnts_custom", eng = "finnts", mode = "regression",
    options = list(predictor_indicators = "none", compute_intercept = FALSE,
      remove_intercept = FALSE, allow_sparse_x = FALSE))
  parsnip::set_fit(model = "finnts_custom", eng = "finnts", mode = "regression", value = list(
    interface = "data.frame", protect = c("x", "y"),
    func = c(pkg = "finnts", fun = "custom_model_fit_impl"), defaults = list()))
  parsnip::set_pred(model = "finnts_custom", eng = "finnts", mode = "regression", type = "numeric", value = list(
    pre = NULL, post = NULL,
    func = c(pkg = "finnts", fun = "custom_model_predict_impl"),
    args = list(object = rlang::expr(object$fit), new_data = rlang::expr(new_data))))
}

# Build an internal, non-tunable spec. Literal quosures avoid capturing a user's
# session, training data or provider object in the engine arguments.
custom_model_spec <- function(definition, context, allow_code = FALSE) {
  arguments <- lapply(list(definition = definition, context = context, allow_code = allow_code),
    rlang::new_quosure, env = emptyenv())
  parsnip::new_model_spec("finnts_custom", args = list(), eng_args = arguments,
    mode = "regression", method = NULL, engine = "finnts")
}

# Compose an unfitted workflow from an M1 definition and an unprepped recipe.
# Explicit context describes the prepared representation, not global run state.
# Construction is inert; allow_code will gate execution rather than construction.
custom_model_workflow <- function(definition, recipe, model_type, recipe_id,
                                  date_type, forecast_horizon, target_scale,
                                  allow_code = FALSE) {
  context <- list(model_type = model_type, recipe_id = recipe_id,
    date_type = date_type, forecast_horizon = forecast_horizon, target_scale = target_scale)
  custom_model_context(definition, context, allow_code)
  custom_model_recipe(recipe, definition)
  workflows::workflow() %>%
    workflows::add_model(custom_model_spec(definition, context, allow_code)) %>%
    workflows::add_recipe(recipe)
}

# Validate identity and scalar representation metadata without executing source.
# Returns only the five approved fields, excluding arbitrary session/run state.
# Execution permission is checked separately before any namespace/source loading.
custom_model_context <- function(definition, context, allow_code) {
  validate_custom_model_definition(definition)
  if (is.null(definition$version_id) ||
    !identical(definition$version_id, custom_model_definition_digest(definition))) {
    stop("Custom model version_id does not match its definition.", call. = FALSE)
  }
  if (!is.logical(allow_code) || length(allow_code) != 1L || is.na(allow_code)) {
    stop("Custom model allow_code must be TRUE or FALSE.", call. = FALSE)
  }
  fields <- c("model_type", "recipe_id", "date_type", "forecast_horizon", "target_scale")
  if (!is.list(context) || anyDuplicated(names(context)) || !setequal(names(context), fields)) {
    stop("Custom model context must contain only its five representation fields.", call. = FALSE)
  }
  permitted <- list(model_type = definition$model_type, recipe_id = definition$requirements$recipes,
    date_type = definition$requirements$date_types, target_scale = definition$requirements$target_scale)
  for (field in names(permitted)) {
    value <- context[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !value %in% permitted[[field]]) {
      stop("Custom model has incompatible ", field, ".", call. = FALSE)
    }
  }
  horizon <- context$forecast_horizon
  if (!is.numeric(horizon) || length(horizon) != 1L || !is.finite(horizon) ||
    !horizon %in% definition$requirements$forecast_horizon) {
    stop("Custom model has incompatible forecast_horizon.", call. = FALSE)
  }
  context[fields]
}

# Check an untrained recipe without fitting it. This experimental adapter admits
# only standard predictor-only transformations with inspectable selectors; it
# rejects opaque mutation, row-changing steps and outcome/identity changes.
# This compatibility guard is not a sandbox for third-party recipe code.
custom_model_recipe <- function(recipe, definition) {
  if (!inherits(recipe, "recipe") || !is.null(recipe$last_term_info) ||
    any(vapply(recipe$steps, function(step) isTRUE(step$trained), logical(1)))) {
    stop("Custom model requires an unprepped recipe.", call. = FALSE)
  }
  info <- recipe$var_info
  required <- unique(c("Date", "Combo", definition$requirements$predictors))
  predictors <- info$variable[info$role %in% "predictor"]
  if (!identical(info$variable[info$role %in% "outcome"], "Target") ||
    !all(required %in% predictors)) {
    stop("Custom model recipe must retain Target as outcome and required Date/Combo predictors.", call. = FALSE)
  }
  protected <- c("Target", "Date", "Combo", "Horizon", ".finnts_row")
  supported <- c("step_normalize", "step_center", "step_scale", "step_log", "step_sqrt",
    "step_impute_mean", "step_impute_median", "step_impute_mode", "step_rm",
    "step_zv", "step_nzv", "step_dummy", "step_unknown", "step_novel")
  for (step in recipe$steps) {
    if (!class(step)[[1]] %in% supported || isTRUE(step$skip)) {
      stop("Custom model recipe contains an unsupported or skipped step.", call. = FALSE)
    }
    selected <- recipes::recipes_eval_select(step$terms, recipe$template, info)
    if (any(names(selected) %in% protected) ||
      (class(step)[[1]] %in% c("step_rm", "step_zv", "step_nzv", "step_dummy") &&
        any(names(selected) %in% required))) {
      stop("Custom model recipe step changes a protected or required column.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Query one declared dependency at execution time without attaching/installing.
# Kept small so tests can simulate an absent optional dependency on any machine.
custom_model_package_available <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

# Build sibling source functions in a private base-parented environment after
# explicit consent. Namespace availability and hash checks precede evaluation.
# Trusted code can still access files/network: these checks are not a sandbox.
custom_model_functions <- function(definition, allow_code) {
  if (!isTRUE(allow_code)) {
    stop("Custom model execution requires explicit allow_code = TRUE for trusted code.", call. = FALSE)
  }
  if (!identical(definition$version_id, custom_model_definition_digest(definition))) {
    stop("Custom model version_id does not match its definition.", call. = FALSE)
  }
  for (package in definition$packages) {
    if (!custom_model_package_available(package)) {
      stop("Custom model requires unavailable package '", package, "'; no packages were installed.", call. = FALSE)
    }
  }
  environment <- new.env(parent = baseenv())
  for (name in names(definition$source)) {
    assign(name, eval(parse(text = enc2utf8(definition$source[[name]])), envir = environment),
      envir = environment)
  }
  lockEnvironment(environment, bindings = TRUE)
  environment
}

# Validate data identities after baking and return predictor data. Recipes turn
# character predictors into factors by default; restore Combo labels, never their
# integer codes. R2 may repeat dates at distinct horizons, not duplicate keys.
custom_model_data <- function(data, definition, context) {
  required <- unique(c("Date", "Combo", definition$requirements$predictors,
    if (context$recipe_id == "R2") "Horizon"))
  if (!is.data.frame(data) || !nrow(data) || anyDuplicated(names(data)) ||
    !all(required %in% names(data)) || ".finnts_row" %in% names(data)) {
    stop("Custom model data has missing required predictors or invalid row identity.", call. = FALSE)
  }
  if (is.factor(data$Combo)) data$Combo <- as.character(data$Combo)
  if (!inherits(data$Date, "Date") || anyNA(data$Date) || any(!is.finite(as.numeric(data$Date))) ||
    !is.character(data$Combo) || anyNA(data$Combo) || any(!nzchar(trimws(data$Combo)))) {
    stop("Custom model requires unchanged Date and character Combo identity columns.", call. = FALSE)
  }
  keys <- c("Combo", "Date")
  if (context$recipe_id == "R2") {
    if (!is.numeric(data$Horizon) || anyNA(data$Horizon) ||
      any(!data$Horizon %in% definition$requirements$forecast_horizon)) {
      stop("Custom model R2 Horizon values are unsupported.", call. = FALSE)
    }
    keys <- c(keys, "Horizon")
  }
  if (anyDuplicated(data[keys])) stop("Custom model data has duplicate row keys.", call. = FALSE)
  if (context$model_type == "local" && length(unique(data$Combo)) != 1L) {
    stop("Custom model local fitting/prediction requires exactly one Combo.", call. = FALSE)
  }
  data
}

# Reject fitted states with process-bound resources. Plain model objects and
# namespace/base formula environments are supported; arbitrary closures, private
# environments, connections and external pointers are not portable contracts.
custom_model_state <- function(value, depth = 0L) {
  if (depth > 100L || typeof(value) %in% c("externalptr", "weakref", "closure", "builtin", "special") ||
    inherits(value, "connection")) {
    stop("Custom model fitted state contains unsupported nonportable state.", call. = FALSE)
  }
  if (is.environment(value)) {
    if (!identical(value, baseenv()) && !identical(value, emptyenv()) && !isNamespace(value)) {
      stop("Custom model fitted state contains a nonportable environment.", call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (is.list(value) || is.pairlist(value)) {
    for (element in value) custom_model_state(element, depth + 1L)
  }
  for (attribute in attributes(value)) custom_model_state(attribute, depth + 1L)
  invisible(TRUE)
}

#' Fit a trusted custom-model definition through parsnip
#'
#' @param x Baked predictor data with Date and character Combo identity.
#' @param y Numeric Target outcome supplied only from the analysis fold.
#' @param definition An immutable internal M1 definition with its version_id.
#' @param context Scalar model_type, recipe_id, date_type, forecast_horizon and
#'   target_scale fields. Cutoffs are derived here, never supplied by a caller.
#' @param allow_code Explicit permission to run trusted R source. Not a sandbox
#'   or a substitute for the later user-facing approval workflow.
#' @return Internal fitted state, definition and fold-specific context.
#' @details No packages are installed. Source errors propagate without fallback.
#'   Source must return portable state; this adapter does not invert transforms.
#'   Definition schema 3 sorts each series chronologically before fitting. Earlier
#'   definitions retain their original row order and execution semantics.
#' @keywords internal
#' @export
custom_model_fit_impl <- function(x, y, definition, context, allow_code = FALSE) {
  context <- custom_model_context(definition, context, allow_code)
  if (!isTRUE(allow_code)) stop("Custom model execution requires allow_code = TRUE.", call. = FALSE)
  x <- custom_model_data(x, definition, context)
  if ("Target" %in% names(x) || !is.numeric(y) || !is.null(dim(y)) ||
    length(y) != nrow(x) || any(!is.finite(y))) {
    stop("Custom model fit requires one finite numeric Target per analysis row.", call. = FALSE)
  }
  functions <- custom_model_functions(definition, allow_code)
  data <- x
  data$Target <- y
  if (identical(definition$schema_version, 3L)) {
    data <- data[order(data$Combo, data$Date, method = "radix"), , drop = FALSE]
    rownames(data) <- NULL
  }
  context$cutoff <- max(data$Date)
  context$series_cutoffs <- lapply(split(data$Date, data$Combo), max)
  state <- functions$fit(data = data, context = context, parameters = definition$fixed_parameters)
  custom_model_state(state)
  list(definition = definition, context = context, allow_code = allow_code, state = state)
}

#' Predict with a trusted custom-model fitted state
#'
#' @param object Internal fitted object from custom_model_fit_impl().
#' @param new_data Baked requested predictor rows; any Target is removed before
#'   calling source. Dates must follow each series' training cutoff.
#' @return Numeric predictions in the original requested row order.
#' @details Source returns a data frame with .finnts_row and finite numeric .pred.
#'   Missing, duplicate or extra row IDs are errors, not fallback opportunities.
#'   Identity and execution consent are checked on every call. Trusted R source
#'   is not sandboxed; do not use this experimental engine for untrusted code.
#'   Definition schema 3 requires the complete forecast horizon for each included
#'   series. It supplies chronological rows and aligned integer context$forecast_step
#'   values, then restores the caller's row order. Series may be batched separately;
#'   partial-date requests fail rather than inventing missing driver values.
#' @keywords internal
#' @export
custom_model_predict_impl <- function(object, new_data) {
  fields <- c("model_type", "recipe_id", "date_type", "forecast_horizon", "target_scale")
  context <- object$context
  custom_model_context(object$definition, context[fields], object$allow_code)
  if (!isTRUE(object$allow_code)) stop("Custom model execution requires allow_code = TRUE.", call. = FALSE)
  new_data$Target <- NULL
  new_data <- custom_model_data(new_data, object$definition, context)
  temporal <- identical(object$definition$schema_version, 3L)
  steps <- integer(nrow(new_data))
  for (combo in unique(new_data$Combo)) {
    cutoff <- context$series_cutoffs[[combo]]
    if (is.null(cutoff)) stop("Custom model cannot predict a Combo without training history.", call. = FALSE)
    dates <- seq(cutoff, by = switch(context$date_type, year = "year", quarter = "quarter",
      month = "month", week = "week", day = "day"), length.out = context$forecast_horizon + 1L)[-1L]
    if (any(!new_data$Date[new_data$Combo == combo] %in% dates)) {
      stop("Custom model requested dates are outside the forecast horizon after the training cutoff.", call. = FALSE)
    }
    rows <- which(new_data$Combo == combo)
    if (temporal && !setequal(new_data$Date[rows], dates)) {
      stop("Custom model requires the complete forecast horizon for each requested series.", call. = FALSE)
    }
    steps[rows] <- match(new_data$Date[rows], dates)
  }
  functions <- custom_model_functions(object$definition, object$allow_code)
  custom_model_state(object$state)
  original_rows <- seq_len(nrow(new_data))
  new_data$.finnts_row <- original_rows
  if (temporal) {
    execution_order <- order(new_data$Combo, new_data$Date, method = "radix")
    new_data <- new_data[execution_order, , drop = FALSE]
    rownames(new_data) <- NULL
    context$forecast_step <- steps[execution_order]
  }
  result <- functions$predict(object = object$state, new_data = new_data, context = context)
  if (!is.data.frame(result) || anyDuplicated(names(result)) ||
    !all(c(".finnts_row", ".pred") %in% names(result)) || nrow(result) != nrow(new_data) ||
    !is.numeric(result$.finnts_row) || anyNA(result$.finnts_row) || anyDuplicated(result$.finnts_row) ||
    !setequal(result$.finnts_row, new_data$.finnts_row) || !is.numeric(result$.pred) ||
    !is.null(dim(result$.pred)) || any(!is.finite(result$.pred))) {
    stop("Custom model prediction must return exact unique row IDs and finite numeric .pred values.", call. = FALSE)
  }
  as.numeric(result$.pred[match(original_rows, result$.finnts_row)])
}