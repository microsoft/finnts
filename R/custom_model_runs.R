# Reject executable/attributed nested metadata before interpreting attestations
# or manifests. Plain lists and finite atomic values are accepted, with unique
# nonblank names and bounded depth. Returns invisibly or errors without dispatch.
custom_run_passive <- function(value, depth = 0L) {
  if (depth > 64L || !typeof(value) %in% c("NULL", "list", "character", "logical", "integer", "double") ||
    any(!names(attributes(value)) %in% "names")) {
    stop("Custom run metadata must contain passive data only.", call. = FALSE)
  }
  if (!is.null(names(value)) && (anyNA(names(value)) || anyDuplicated(names(value)) ||
    any(!nzchar(trimws(names(value)))))) stop("Custom run metadata names must be unique and nonblank.", call. = FALSE)
  if (is.list(value)) {
    for (entry in value) custom_run_passive(entry, depth + 1L)
  } else if (anyNA(value) || (is.numeric(value) && any(!is.finite(value)))) {
    stop("Custom run metadata must be finite and nonmissing.", call. = FALSE)
  }
  invisible(TRUE)
}

# Validate the experimental trusted-caller envelope, never infer approval from
# registry presence. Returns the unchanged envelope after checking exact M1
# identity and technical validation. Manual approval requires intent_confirmed
# TRUE; explicit automatic approval requires FALSE plus mode "automatic". Both
# require allow_code TRUE and the exact version; unknown fields/policies fail.
# This is not authenticated human identity, a sandbox or a public authoring API.
custom_run_envelope <- function(model) {
  if (typeof(model) != "list" || !identical(attr(model, "class"), "finnts_custom_model") ||
    any(!names(attributes(model)) %in% c("class", "names")) ||
    length(model) != 4L || !setequal(names(model), c("schema_version", "definition", "validation", "approval")) ||
    !identical(model$schema_version, 1L)) {
    stop("custom_models requires approved finnts_custom_model envelopes, not raw definitions or references.", call. = FALSE)
  }
  definition <- custom_model_current_definition(model$definition)
  validation <- model$validation
  approval <- model$approval
  custom_run_passive(validation)
  custom_run_passive(approval)
  if (!is.list(validation) || length(validation) != 3L ||
    !setequal(names(validation), c("version_id", "technical_passed", "checks")) ||
    !identical(validation$version_id, definition$version_id) ||
    !identical(validation$technical_passed, TRUE) ||
    !is.list(validation$checks) || !length(validation$checks) ||
    !all(vapply(validation$checks, function(check) {
      is.character(check) && length(check) == 1L && nzchar(trimws(check))
    }, logical(1)))) {
    stop("Custom model requires current version-bound technical validation and check summaries.", call. = FALSE)
  }
  automatic <- is.list(approval) && identical(approval$mode, "automatic")
  approval_fields <- c("version_id", "intent_confirmed", "allow_code", if (automatic) "mode")
  if (!is.list(approval) || length(approval) != length(approval_fields) ||
    !setequal(names(approval), approval_fields) ||
    !identical(approval$version_id, definition$version_id) ||
    !identical(approval$intent_confirmed, !automatic) || !identical(approval$allow_code, TRUE)) {
    stop("Custom model requires current version-bound manual confirmation or explicit automatic authorization and allow_code = TRUE.", call. = FALSE)
  }
  model
}

# Resolve enrollment through M3a after checking every supplied envelope. Returns
# the passive pool plus only selected attestations; never executes source or
# enrolls unselected customs. Legacy NULL/empty custom input returns NULL so
# existing built-in selection semantics remain with their original owner.
custom_run_pool <- function(models_to_run, models_not_to_run, custom_models) {
  if (is.null(custom_models) || identical(custom_models, list())) return(NULL)
  if (typeof(custom_models) != "list" || any(!names(attributes(custom_models)) %in% "names") ||
    is.null(names(custom_models))) stop("custom_models must be a uniquely named list.", call. = FALSE)
  custom_model_pool_names(names(custom_models), "custom_models names")
  envelopes <- lapply(custom_models, custom_run_envelope)
  definitions <- lapply(envelopes, function(model) model$definition)
  pool <- resolve_custom_model_pool(models_to_run, models_not_to_run, definitions)
  if (!length(pool$custom)) return(NULL)
  pool$envelopes <- envelopes[names(pool$custom)]
  pool
}

# Validate an original-scale R1 representation from scalar saved preparation
# settings and caller storage. Returns a canonical context used for restart
# identity. Pool eligibility is separate so isolated authoring can inspect a
# hierarchy. No settings are changed; incompatible transforms/backends error.
# The legacy default rejects hierarchy; reviewed callers opt in explicitly and
# still validate definition capabilities/enrollment separately before execution.
custom_run_context <- function(run_info, settings, run_ensemble_models, allow_hierarchy = FALSE) {
  if (!is.null(run_info$storage_object) || !identical(run_info$object_output, "rds") ||
    !run_info$data_output %in% c("csv", "rds", "parquet")) {
    stop("Custom runs require local/mounted storage, RDS model objects and CSV/RDS/Parquet data.", call. = FALSE)
  }
  approach <- settings$forecast_approach
  if (!is.character(approach) || length(approach) != 1L || is.na(approach) ||
    !approach %in% c("bottoms_up", "standard_hierarchy", "grouped_hierarchy")) {
    stop("Custom runs require a supported forecast_approach.", call. = FALSE)
  }
  if (approach != "bottoms_up" && !isTRUE(allow_hierarchy)) {
    stop("Custom runs require forecast_approach = bottoms_up without explicit reviewed hierarchy validation.", call. = FALSE)
  }
  required <- list(forecast_approach = approach, recipes_to_run = "R1", stationary = FALSE,
    box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE, multistep_horizon = FALSE)
  for (field in names(required)) {
    if (!identical(settings[[field]], required[[field]])) {
      stop("Custom runs require ", field, " = ", as.character(required[[field]]), ".", call. = FALSE)
    }
  }
  if (!identical(run_ensemble_models, FALSE)) stop("Custom runs require run_ensemble_models = FALSE.", call. = FALSE)
  c(required, list(date_type = as.character(settings$date_type),
    forecast_horizon = as.numeric(settings$forecast_horizon), run_ensemble_models = FALSE))
}

# Match reviewed capabilities to the requested representation before forecast
# writes. Hierarchy uses a shared explicitly mixed pool, never winner counts.
# This does not enroll supplied-but-unselected models or execute their source.
custom_run_eligibility <- function(pool, context) {
  hierarchical <- context$forecast_approach != "bottoms_up"
  for (definition in pool$custom) {
    hierarchy <- definition$requirements$hierarchy
    if (hierarchical && (is.null(hierarchy) ||
      !identical(hierarchy$forecast_approach, context$forecast_approach))) {
      stop("Custom model lacks matching reviewed hierarchy capability; create and approve a compatible definition.", call. = FALSE)
    }
    if (!hierarchical && !is.null(hierarchy)) {
      stop("Custom model has hierarchy-only approval; use a separately approved bottoms_up definition.", call. = FALSE)
    }
    custom_model_context(definition, list(model_type = definition$model_type[[1]], recipe_id = "R1",
      date_type = context$date_type, forecast_horizon = context$forecast_horizon, target_scale = "original"), FALSE)
  }
  if (hierarchical && !length(pool$built_in)) {
    stop("Custom hierarchy requires a mixed custom/FinnTS pool; use bottoms_up for custom-only forecasts.", call. = FALSE)
  }
  if (hierarchical) custom_run_hierarchy_builtins(pool$built_in, context)
  invisible(NULL)
}

# Check selected built-ins through their existing inert workflow constructors
# and parsnip dependency declarations. Synthetic rows disclose no caller data;
# no fit, provider request or installation occurs. Foundation-model execution
# is not qualified for this profile and cannot serve as a token competitor.
custom_run_hierarchy_builtins <- function(models, context) {
  if (any(models %in% list_foundation_models())) {
    stop("Custom hierarchy foundation-model execution is not qualified; select a supported local R1 FinnTS competitor.", call. = FALSE)
  }
  sample <- data.frame(Date = seq(as.Date("2000-01-01"), by = context$date_type, length.out = 24),
    Combo = "eligibility", Target = seq_len(24))
  available <- list(train_data = sample, frequency = get_frequency_number(context$date_type),
    horizon = context$forecast_horizon, seasonal_period = get_seasonal_periods(context$date_type),
    model_type = "single", pca = FALSE, multistep = FALSE, external_regressors = NULL, lag_periods = NULL)
  for (model in models) {
    constructor <- get(gsub("-", "_", model, fixed = TRUE), envir = asNamespace("finnts"), mode = "function", inherits = FALSE)
    workflow <- do.call(constructor, available[intersect(names(available), names(formals(constructor)))])
    packages <- parsnip::required_pkgs(workflows::extract_spec_parsnip(workflow))
    for (package in packages) {
      if (!custom_model_package_available(package)) {
        stop("Custom hierarchy FinnTS competitor requires unavailable package '", package, "'; no packages installed.", call. = FALSE)
      }
    }
  }
  invisible(NULL)
}

# Reject unsupported hierarchy-level publication controls without changing them.
# These restrictions apply only to the new custom hierarchy profile, not legacy
# bottom-up averaging or built-in hierarchy workflows. No I/O is performed.
custom_run_hierarchy_controls <- function(context, external_regressors = NULL,
                                           average_models = FALSE, weekly_to_daily = FALSE) {
  if (context$forecast_approach == "bottoms_up") return(invisible(NULL))
  if (length(external_regressors)) stop("Custom hierarchy requires external_regressors = NULL.", call. = FALSE)
  if (!identical(average_models, FALSE)) stop("Custom hierarchy requires average_models = FALSE.", call. = FALSE)
  if (context$date_type == "week" && !identical(weekly_to_daily, FALSE)) {
    stop("Custom hierarchy requires weekly_to_daily = FALSE.", call. = FALSE)
  }
  invisible(NULL)
}

# Validate a native-cadence historical panel before hierarchy preparation can
# fill missing leaf cells with zero. Return normalized historical rows without
# changing caller data or writing artifacts; missing/duplicate cells are errors.
custom_run_hierarchy_panel <- function(data, combo_variables, target_variable, date_type,
                                        hist_start_date = NULL, hist_end_date = NULL) {
  if (!is.data.frame(data) || !nrow(data) || !length(combo_variables) ||
    !all(c("Date", combo_variables, target_variable) %in% names(data)) ||
    !inherits(data$Date, "Date") || anyNA(data$Date) || !is.numeric(data[[target_variable]])) {
    stop("Custom hierarchy requires a balanced finite historical panel with Date and declared leaf columns.", call. = FALSE)
  }
  data <- normalize_combo_values(data, combo_variables)
  finite <- is.finite(data[[target_variable]])
  if (!any(finite)) stop("Custom hierarchy requires a balanced finite historical panel.", call. = FALSE)
  if (is.null(hist_start_date)) hist_start_date <- min(data$Date)
  if (is.null(hist_end_date)) hist_end_date <- max(data$Date[finite])
  if (!inherits(hist_start_date, "Date") || !inherits(hist_end_date, "Date") ||
    length(hist_start_date) != 1L || length(hist_end_date) != 1L ||
    anyNA(c(hist_start_date, hist_end_date)) || hist_start_date > hist_end_date) {
    stop("Custom hierarchy requires valid historical bounds.", call. = FALSE)
  }
  leaves <- unique(data[combo_variables])
  history <- data[data$Date >= hist_start_date & data$Date <= hist_end_date, , drop = FALSE]
  if (!nrow(history) || any(!is.finite(history[[target_variable]])) ||
    anyNA(history[combo_variables]) || anyDuplicated(history[c(combo_variables, "Date")])) {
    stop("Custom hierarchy requires a balanced finite historical panel without duplicate leaf dates.", call. = FALSE)
  }
  dates <- seq(hist_start_date, hist_end_date, by = date_type)
  counts <- history %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(combo_variables))) %>%
    dplyr::summarise(.rows = dplyr::n(), .groups = "drop")
  if (nrow(counts) != nrow(leaves) || any(counts$.rows != length(dates)) || any(!history$Date %in% dates)) {
    stop("Custom hierarchy requires a balanced finite historical panel at the requested cadence.", call. = FALSE)
  }
  history
}

# Bind existing local hierarchy topology and input bytes to the run context.
# Initial enrollment also validates leaf/node histories; reuse rehashes the same
# exact files without repreparing or rereading every table. No directory listing
# or storage write occurs, and incomplete topology/artifacts are hard errors.
# return_data retains validated historical node rows for bounded authoring, with
# max_rows checked before combining them. The returned source hashes cover the
# same read interval; no caller needs to read those tables again.
custom_run_hierarchy_context <- function(run_info, context, settings, validate_data = TRUE,
                                         return_data = FALSE, max_rows = Inf) {
  if (context$forecast_approach == "bottoms_up") return(context)
  metadata_path <- local_artifact_path(run_info, "prep_data", "-hts_info", extension = "rds")
  input_path <- local_artifact_path(run_info, "prep_data", "-hts_data")
  local_artifact_files(metadata_path)
  metadata_hash <- digest::digest(file = metadata_path, algo = "sha256")
  metadata <- read_exact_artifact(run_info, metadata_path, return_type = "object")
  if (!is.list(metadata) || !all(c("original_combos", "hts_combos", "nodes") %in% names(metadata))) {
    stop("Custom hierarchy has invalid topology metadata.", call. = FALSE)
  }
  for (field in c("original_combos", "hts_combos")) {
    values <- metadata[[field]]
    if (!is.character(values) || !length(values) || anyNA(values) || any(!nzchar(values)) || anyDuplicated(values)) {
      stop("Custom hierarchy has invalid topology identities.", call. = FALSE)
    }
  }
  paths <- local_artifact_path(run_info, "prep_data", "-R1", vapply(metadata$hts_combos, hash_data, character(1)))
  paths <- local_artifact_files(c(metadata_path, input_path, paths))
  hashes <- vapply(as.character(paths), function(path) digest::digest(file = path, algo = "sha256"), character(1))
  if (!identical(unname(hashes[match(metadata_path, as.character(paths))]), metadata_hash)) {
    stop("Custom hierarchy topology changed while reading.", call. = FALSE)
  }
  node_data <- list()
  row_count <- 0L
  if (validate_data) {
    leaves <- read_exact_artifact(run_info, input_path)
    history <- custom_run_hierarchy_panel(leaves, "Combo", "Target", context$date_type,
      as.Date(settings$hist_start_date), as.Date(settings$hist_end_date))
    if (!setequal(unique(history$Combo), metadata$original_combos)) {
      stop("Custom hierarchy leaf data does not match its topology.", call. = FALSE)
    }
    for (combo in metadata$hts_combos) {
      rows <- read_exact_artifact(run_info, local_artifact_path(run_info, "prep_data", "-R1", hash_data(combo)))
      history <- custom_run_hierarchy_panel(rows, "Combo", "Target", context$date_type,
        as.Date(settings$hist_start_date), as.Date(settings$hist_end_date))
      if (!identical(unique(as.character(rows$Combo)), combo)) stop("Custom hierarchy node artifact has wrong ownership.", call. = FALSE)
      row_count <- row_count + nrow(history)
      if (row_count > max_rows) stop("Custom hierarchy histories exceed the authoring row limit; supply a smaller hierarchy.", call. = FALSE)
      if (return_data) node_data[[combo]] <- history
    }
    current <- vapply(as.character(paths), function(path) digest::digest(file = path, algo = "sha256"), character(1))
    if (!identical(current, hashes)) stop("Custom hierarchy context changed while reading.", call. = FALSE)
  }
  context$hierarchy_id <- digest::digest(unname(hashes), algo = "sha256", serializeVersion = 2)
  if (return_data) return(list(context = context, data = dplyr::bind_rows(node_data), topology = metadata,
    sources = stats::setNames(as.list(unname(hashes)), as.character(paths))))
  context
}

# Derive the exact run-scoped manifest path in fixed RDS format. No I/O occurs.
custom_run_path <- function(run_info) {
  local_artifact_path(run_info, "prep_models", "-custom-model-pool", extension = "rds")
}

# Compute a canonical passive manifest digest, sorting named maps recursively
# and effective selected names; sequence-valued check data remains ordered.
# The stored manifest_id is excluded. No source execution or storage access.
custom_run_digest <- function(manifest) {
  manifest$manifest_id <- NULL
  manifest$selected <- sort(manifest$selected, method = "radix")
  # Normalize named list maps and text without mutating the original manifest.
  normalize <- function(value) {
    if (is.list(value)) {
      value <- lapply(value, normalize)
      if (!is.null(names(value))) value <- value[order(names(value), method = "radix")]
    } else if (is.character(value)) value <- enc2utf8(value)
    value
  }
  digest::digest(normalize(manifest), algo = "sha256", serializeVersion = 2)
}

# Read a present manifest strictly, distinguishing RDS NULL from absence, then
# validate its passive record, attestations and exact registered definitions.
# Returns NULL only for a genuinely absent optional file. No listing or rewrite.
custom_run_read <- function(run_info, required = FALSE) {
  paths <- local_artifact_files(custom_run_path(run_info), allow_missing = !required)
  if (!length(paths)) return(NULL)
  manifest <- read_exact_artifact(run_info, paths, return_type = "object")
  custom_run_passive(manifest)
  fields <- c("schema_version", "selected", "references", "evidence", "context", "pool_id", "manifest_id")
  if (!is.list(manifest) || length(manifest) != length(fields) ||
    !setequal(names(manifest), fields) || !identical(manifest$schema_version, 1L) ||
    !identical(manifest$manifest_id, custom_run_digest(manifest))) {
    stop("Invalid custom run manifest identity.", call. = FALSE)
  }
  pool <- resolve_custom_model_pool(manifest$selected, custom_models = manifest$references, run_info = run_info)
  if (!length(pool$custom) || !identical(pool$pool_id, manifest$pool_id) ||
    !setequal(names(manifest$evidence), names(pool$custom))) stop("Invalid custom run pool identity.", call. = FALSE)
  for (alias in names(pool$custom)) {
    evidence <- manifest$evidence[[alias]]
    custom_run_envelope(structure(c(list(schema_version = 1L, definition = pool$custom[[alias]]),
      evidence), class = "finnts_custom_model"))
  }
  list(manifest = manifest, pool = pool)
}

# Pin selected definitions/attestations before workflow completion is logged.
# Existing same-run content is immutable; omission, changed selection/context or
# stale evidence errors. No custom pool leaves legacy runs untouched, except a
# known custom marker/manifest cannot be silently reused as a built-in-only run.
# Hierarchy eligibility is checked before any registration or manifest write.
custom_run_prepare <- function(run_info, pool, log_df, run_ensemble_models) {
  marked <- "custom_pool_id" %in% names(log_df) && !is.na(log_df$custom_pool_id[[1]])
  if (is.null(pool) && !marked) {
    if (!is.null(run_info$storage_object)) return(NULL)
    if (!length(local_artifact_files(custom_run_path(run_info), allow_missing = TRUE))) return(NULL)
  }
  if (is.null(pool)) stop("This custom run requires the same approved custom_models; start a new run for another pool.", call. = FALSE)
  context <- custom_run_context(run_info, log_df, run_ensemble_models, allow_hierarchy = TRUE)
  custom_run_eligibility(pool, context)
  custom_run_hierarchy_controls(context,
    if (is.null(log_df$external_regressors) || all(is.na(log_df$external_regressors))) NULL else log_df$external_regressors)
  context <- custom_run_hierarchy_context(run_info, context, log_df, validate_data = !marked)
  for (definition in pool$custom) {
    custom_model_context(definition, list(model_type = definition$model_type[[1]], recipe_id = "R1",
      date_type = context$date_type, forecast_horizon = context$forecast_horizon,
      target_scale = "original"), TRUE)
  }
  references <- lapply(pool$custom, function(definition) {
    list(registry_schema_version = 1L, name = definition$name, version_id = definition$version_id)
  })
  evidence <- lapply(pool$envelopes, function(model) model[c("validation", "approval")])
  manifest <- list(schema_version = 1L, selected = sort(pool$selected, method = "radix"),
    references = references, evidence = evidence, context = context, pool_id = pool$pool_id)
  manifest$manifest_id <- custom_run_digest(manifest)
  previous <- custom_run_read(run_info, required = marked)
  if (!is.null(previous)) {
    if (!identical(previous$manifest$manifest_id, manifest$manifest_id) ||
      (marked && (!identical(log_df$custom_pool_id, manifest$pool_id) ||
        !identical(log_df$custom_manifest_id, manifest$manifest_id)))) {
      stop("Custom model pool, approval or preparation changed; start a new run.", call. = FALSE)
    }
  } else {
    if ("models_to_run" %in% names(log_df)) stop("Cannot enroll custom models into an existing prepared run.", call. = FALSE)
    for (definition in pool$custom) write_custom_model_definition(definition, run_info)
    write_data(manifest, NULL, run_info, "object", "prep_models", "-custom-model-pool")
    previous <- custom_run_read(run_info, required = TRUE)
    if (!identical(previous$manifest$manifest_id, manifest$manifest_id)) stop("Custom manifest read-back failed.", call. = FALSE)
  }
  list(manifest = manifest, pool = pool)
}

# Return stable identity columns plus caller-declared predictors in first-seen
# order. Authoring metadata and workflow recipes share this rule; prepared
# features are not implicit inputs. Target is supplied separately for fitting.
# Callers own declaration/value validation; this helper performs no I/O.
custom_model_predictor_columns <- function(predictors) {
  unique(c("Date", "Combo", predictors))
}

# Validate required predictors on each actual prepared dataset, not just the
# first recipe sample. Returns the declared identity/predictor names; missing,
# nonfinite, unsupported or outcome/assessment input values error without I/O.
custom_run_predictors <- function(definition, data) {
  predictors <- custom_model_predictor_columns(definition$requirements$predictors)
  forbidden <- c("Target", "Target_Original", "Train_Test_ID", "Run_Type", "Horizon", "Origin", ".finnts_row", ".row")
  if (any(predictors %in% forbidden) || !all(c("Target", predictors) %in% names(data))) {
    stop("Custom model has missing required predictors or outcome/assessment leakage columns.", call. = FALSE)
  }
  for (field in predictors) {
    values <- data[[field]]
    if (anyNA(values) || (is.numeric(values) && any(!is.finite(values))) ||
      !(is.numeric(values) || is.character(values) || is.factor(values) || is.logical(values) || inherits(values, "Date"))) {
      stop("Custom model requires complete supported predictor values: ", field, ".", call. = FALSE)
    }
  }
  predictors
}

# Build inert mode-specific workflows using only declared predictors and stable
# Date/Combo identity. Returns workflow rows, never fitted state. The base-parented
# dot formula supports opaque column names without capturing preparation state.
custom_run_workflows <- function(definition, data, context) {
  predictors <- custom_run_predictors(definition, data)
  recipe <- recipes::recipe(stats::as.formula("Target ~ .", env = baseenv()), data = data[c("Target", predictors)])
  rows <- lapply(definition$model_type, function(mode) {
    workflow <- custom_model_workflow(definition, recipe, mode, "R1", context$date_type,
      context$forecast_horizon, "original", allow_code = TRUE)
    tibble::tibble(Model_Name = definition$name, Model_Recipe = "R1", Model_Type = mode,
      Model_Workflow = list(workflow))
  })
  dplyr::bind_rows(rows)
}

# Load and validate recorded enrollment before standard-stage shortcuts. Legacy
# logs return NULL without registry reads. Custom markers require the exact
# manifest/context; subsequent callers receive one portable, call-local pool.
# Saved hierarchy enrollment must still satisfy the shared mixed-pool policy.
custom_run_load <- function(run_info, log_df) {
  if (!"custom_pool_id" %in% names(log_df) || is.na(log_df$custom_pool_id[[1]])) {
    if (is.null(run_info$storage_object) &&
      length(local_artifact_files(custom_run_path(run_info), allow_missing = TRUE))) {
      stop("Custom manifest exists without completed preparation metadata; rerun prep_models with the same approved pool.", call. = FALSE)
    }
    return(NULL)
  }
  custom <- custom_run_read(run_info, required = TRUE)
  context <- custom_run_context(run_info, log_df, log_df$run_ensemble_models, allow_hierarchy = TRUE)
  custom_run_eligibility(custom$pool, context)
  custom_run_hierarchy_controls(context,
    if (is.null(log_df$external_regressors) || all(is.na(log_df$external_regressors))) NULL else log_df$external_regressors)
  context <- custom_run_hierarchy_context(run_info, context, log_df, validate_data = FALSE)
  if (!identical(custom$manifest$pool_id, log_df$custom_pool_id) ||
    !identical(custom$manifest$manifest_id, log_df$custom_manifest_id) ||
    !identical(context, custom$manifest$context)) {
    stop("Custom run metadata does not match its pinned manifest.", call. = FALSE)
  }
  custom
}

# Validate enabled execution controls and declaration availability without source
# execution or installation. Enforces at least one enabled mode per selected
# custom; called again after actual series routing has resolved global behavior.
# Explicit hierarchy context additionally requires local-only execution.
custom_run_training_controls <- function(custom, local, global, recipes, feature_selection,
                                          negative_forecast, parallel_processing, inner_parallel) {
  if (is.null(custom)) return(invisible(NULL))
  approach <- custom$manifest$context$forecast_approach
  if (!is.null(approach) && approach != "bottoms_up" && (!identical(local, TRUE) || !identical(global, FALSE))) {
    stop("Custom hierarchy requires run_local_models = TRUE and run_global_models = FALSE.", call. = FALSE)
  }
  if (!identical(feature_selection, FALSE) || !identical(negative_forecast, TRUE) ||
    !identical(inner_parallel, FALSE) ||
    !(is.null(parallel_processing) || identical(parallel_processing, "local_machine"))) {
    stop("Custom runs require feature_selection = FALSE, negative_forecast = TRUE, inner_parallel = FALSE and sequential/local_machine execution.", call. = FALSE)
  }
  enabled <- c(if (isTRUE(local)) "local", if (isTRUE(global)) "global")
  for (definition in custom$pool$custom) {
    if (!length(intersect(enabled, definition$model_type)) ||
      ("global" %in% intersect(enabled, definition$model_type) && !identical(unname(unlist(recipes)), "R1"))) {
      stop("Custom model ", definition$name, " has no compatible enabled mode/recipe.", call. = FALSE)
    }
    for (package in definition$packages) {
      if (!custom_model_package_available(package)) stop("Custom model requires unavailable package '", package, "'; no packages installed.", call. = FALSE)
    }
  }
  invisible(NULL)
}

# Validate every prepared custom row against its pinned definition, mode and
# representation before fitting or reuse. Built-in rows may not carry custom
# engines. No source evaluation; returns the table unchanged or errors.
custom_run_check_workflows <- function(table, custom) {
  if (is.null(custom)) return(table)
  if (!all(c("Model_Name", "Model_Recipe", "Model_Type", "Model_Workflow") %in% names(table)) ||
    any(!table$Model_Name %in% custom$pool$selected) ||
    anyDuplicated(table[c("Model_Name", "Model_Recipe", "Model_Type")])) {
    stop("Prepared workflows do not match the custom run pool.", call. = FALSE)
  }
  for (alias in names(custom$pool$custom)) {
    definition <- custom$pool$custom[[alias]]
    rows <- table[table$Model_Name == alias, ]
    if (!setequal(rows$Model_Type, definition$model_type) || any(rows$Model_Recipe != "R1")) {
      stop("Prepared custom workflow modes do not match the pinned definition.", call. = FALSE)
    }
    for (index in seq_len(nrow(rows))) {
      spec <- workflows::extract_spec_parsnip(rows$Model_Workflow[[index]])
      arguments <- lapply(spec$eng_args, rlang::eval_tidy)
      if (!inherits(spec, "finnts_custom") || !identical(spec$engine, "finnts") ||
        !identical(arguments$definition$version_id, definition$version_id) ||
        !identical(arguments$context, list(model_type = rows$Model_Type[[index]], recipe_id = "R1",
          date_type = custom$manifest$context$date_type, forecast_horizon = custom$manifest$context$forecast_horizon,
          target_scale = "original")) || !identical(arguments$allow_code, TRUE)) {
        stop("Prepared custom workflow identity/consent mismatch.", call. = FALSE)
      }
      custom_model_current_definition(arguments$definition)
      recipe <- workflows::extract_recipe(rows$Model_Workflow[[index]], estimated = FALSE)
      if (length(recipe$steps) || !setequal(recipe$var_info$variable,
        unique(c("Target", "Date", "Combo", definition$requirements$predictors)))) {
        stop("Prepared custom workflow preprocessing does not match its declared predictors.", call. = FALSE)
      }
      custom_model_recipe(recipe, definition)
    }
  }
  for (index in which(!table$Model_Name %in% names(custom$pool$custom))) {
    if (inherits(workflows::extract_spec_parsnip(table$Model_Workflow[[index]]), "finnts_custom")) {
      stop("Custom engine has an unregistered built-in alias.", call. = FALSE)
    }
  }
  table
}

# Replace only custom ID prefixes with their full content version. Built-in IDs
# and readable Model_Name remain unchanged, including underscores in aliases.
custom_run_model_ids <- function(table, custom) {
  if (is.null(custom)) return(table)
  for (alias in names(custom$pool$custom)) {
    selected <- table$Model_Name == alias
    table$Model_ID[selected] <- paste0("custom-", custom$pool$custom[[alias]]$version_id,
      "--", table$Model_Type[selected], "--", table$Recipe_ID[selected])
  }
  table
}

# Fit custom workflows on chronological splits without changing their scoring
# rows. Schema-3 source receives each included series' complete forecast horizon
# from existing predictor rows; analysis indices and targets stay unchanged.
# Missing rows fail before fitting, never trigger imputation. Validate complete
# execution coverage, then return only originally requested predictions. Legacy
# definitions retain the original resamples. No provider or artifact I/O occurs.
custom_run_fit_resamples <- function(workflow, data, splits, control) {
  arguments <- lapply(workflows::extract_spec_parsnip(workflow)$eng_args, rlang::eval_tidy)
  temporal <- identical(arguments$definition$schema_version, 3L)
  execution <- splits
  if (temporal) {
    expanded <- lapply(splits$splits, function(split) {
      analysis <- rsample::analysis(split)
      assessment <- rsample::assessment(split)
      rows <- unlist(lapply(unique(assessment$Combo), function(combo) {
        history <- analysis$Date[analysis$Combo == combo]
        if (!length(history)) stop("Custom resampling requires training history for every requested series.", call. = FALSE)
        dates <- seq(max(history), by = arguments$context$date_type,
          length.out = arguments$context$forecast_horizon + 1L)[-1L]
        selected <- which(data$Combo == combo & data$Date %in% dates)
        if (length(selected) != length(dates) || !setequal(data$Date[selected], dates)) {
          stop("Custom resampling requires existing predictor rows for the complete forecast horizon.", call. = FALSE)
        }
        selected
      }), use.names = FALSE)
      rsample::make_splits(list(analysis = split$in_id, assessment = sort(rows)), split$data)
    })
    execution <- rsample::manual_rset(expanded, splits$id)
  }
  predictions <- tune::collect_predictions(tune::fit_resamples(workflow, resamples = execution,
    metrics = NULL, control = control))
  if (temporal) {
    custom_run_check_predictions(predictions, data, execution)
    requested <- dplyr::bind_rows(lapply(seq_len(nrow(splits)), function(index) {
      tibble::tibble(id = as.character(splits$id[[index]]), .row = splits$splits[[index]]$out_id)
    }))
    predictions <- dplyr::semi_join(predictions, requested, by = c("id", ".row"))
  }
  predictions
}

# Check complete custom fold/series coverage after fit_resamples. Missing folds
# cannot turn a failed custom into a successful built-in fallback. Expected keys
# come from the same chronological split assessments, never generated source.
custom_run_check_predictions <- function(predictions, data, splits) {
  expected <- dplyr::bind_rows(lapply(seq_len(nrow(splits)), function(index) {
    assessment <- rsample::assessment(splits$splits[[index]])
    tibble::tibble(id = as.character(splits$id[[index]]), Combo = assessment$Combo, Date = assessment$Date)
  }))
  actual <- tibble::tibble(id = as.character(predictions$id), Combo = data$Combo[predictions$.row],
    Date = data$Date[predictions$.row])
  if (any(!is.finite(predictions$.pred)) || anyDuplicated(actual) ||
    nrow(actual) != nrow(expected) || nrow(dplyr::anti_join(expected, actual, by = c("id", "Combo", "Date")))) {
    stop("Custom model did not produce every required finite fold/series prediction.", call. = FALSE)
  }
  invisible(NULL)
}

# Build the exact enabled individual candidate identity table for validation.
# Built-ins retain legacy IDs; custom IDs use full versions. No artifacts read.
custom_run_candidates <- function(custom, local, global) {
  rows <- list()
  for (alias in custom$pool$selected) {
    modes <- if (alias %in% names(custom$pool$custom)) custom$pool$custom[[alias]]$model_type else
      c("local", if (alias %in% list_global_models()) "global")
    modes <- intersect(modes, c(if (isTRUE(local)) "local", if (isTRUE(global)) "global"))
    if (!length(modes)) next
    rows[[alias]] <- tibble::tibble(Model_Name = alias, Model_Type = modes, Recipe_ID = "R1",
      Model_ID = paste(alias, modes, "R1", sep = "--"))
  }
  custom_run_model_ids(dplyr::bind_rows(rows), custom)
}

# Reject foreign/stale individual IDs and invalid average components, including
# alias/type/recipe mismatches. Fitted custom workflows additionally bind their
# engine definition/context and consent to the pinned version. required_custom
# requires every applicable custom ID; optional averages need only valid members.
# Returns invisibly; malformed or incomplete membership raises an error.
custom_run_membership <- function(rows, custom, candidates, averages = FALSE, required_custom = FALSE) {
  if (is.null(rows)) return(invisible(NULL))
  fields <- c("Model_ID", "Model_Name", "Model_Type", "Recipe_ID")
  if (!is.data.frame(rows) || !all(fields %in% names(rows))) stop("Custom run artifact has invalid pool schema.", call. = FALSE)
  expected_custom <- candidates$Model_ID[candidates$Model_Name %in% names(custom$pool$custom)]
  if (required_custom && any(!expected_custom %in% rows$Model_ID)) {
    stop("Saved artifact is missing a selected custom model identity.", call. = FALSE)
  }
  for (index in seq_len(nrow(rows))) {
    row <- rows[index, ]
    if (isTRUE(row$Recipe_ID == "simple_average") && averages) {
      components <- strsplit(row$Model_ID, "_", fixed = TRUE)[[1]]
      if (length(components) < 2L || anyDuplicated(components) || any(!components %in% candidates$Model_ID)) {
        stop("Saved average contains an invalid custom pool component identity.", call. = FALSE)
      }
      next
    }
    expected <- candidates[match(row$Model_ID, candidates$Model_ID), ]
    if (anyNA(row[fields]) || anyNA(expected) ||
      !identical(as.character(unlist(row[fields], use.names = FALSE)),
        as.character(unlist(expected[fields], use.names = FALSE)))) {
      stop("Saved model identity does not belong to the pinned custom run pool.", call. = FALSE)
    }
    if ("Model_Fit" %in% names(rows) && row$Model_Name %in% names(custom$pool$custom)) {
      fit <- workflows::extract_fit_parsnip(row$Model_Fit[[1]])$fit
      definition <- custom$pool$custom[[row$Model_Name]]
      if (!identical(fit$definition$version_id, definition$version_id) ||
        !identical(fit$context[c("model_type", "recipe_id", "date_type", "forecast_horizon", "target_scale")],
          list(model_type = row$Model_Type, recipe_id = "R1", date_type = custom$manifest$context$date_type,
            forecast_horizon = custom$manifest$context$forecast_horizon, target_scale = "original")) ||
        !identical(fit$allow_code, TRUE)) {
        stop("Saved custom fitted model identity/consent mismatch.", call. = FALSE)
      }
      custom_model_current_definition(fit$definition)
      custom_model_context(fit$definition, fit$context[c("model_type", "recipe_id", "date_type", "forecast_horizon", "target_scale")], TRUE)
    }
  }
  invisible(NULL)
}

# Audit present saved artifacts before any completed-run shortcut, using one
# coordinating discovery only when series names are unknown. Known paths are
# read exactly once; returns per-combo forecast tables for final selection reuse.
# Missing required completed artifacts, empty required payloads and mismatched
# identities error rather than being repaired or treated as fallback candidates.
custom_run_audit <- function(run_info, custom, local, global, combos = NULL, complete = FALSE) {
  if (is.null(custom)) return(NULL)
  candidates <- custom_run_candidates(custom, local, global)
  if (is.null(combos)) {
    if (length(run_info$combo)) combos <- run_info$combo else {
      files <- local_artifact_inventory(run_info, "prep_data", "-*-R1")
      prefix <- paste0(hash_data(run_info$project_name), "-", hash_data(run_info$run_name), "-")
      combos <- sub(paste0("-R1\\.", run_info$data_output, "$"), "", substring(basename(files), nchar(prefix) + 1L))
    }
  }
  # Read one validated known artifact; a present empty required table is invalid.
  read_rows <- function(folder, suffix, combo, required, object = FALSE) {
    path <- local_artifact_path(run_info, folder, suffix, combo,
      extension = if (object) "rds" else run_info$data_output)
    present <- local_artifact_files(path, allow_missing = !required)
    if (!length(present)) return(NULL)
    rows <- read_exact_artifact(run_info, present, return_type = if (object) "object" else "df")
    if (is.null(rows) || !is.data.frame(rows) || (required && !nrow(rows))) stop("Invalid required custom run artifact.", call. = FALSE)
    averages <- identical(suffix, "-average_models")
    mode <- if (identical(suffix, "-global_models") ||
      (object && identical(combo, hash_data("All-Data")))) "global" else "local"
    allowed <- if (averages) candidates else candidates[candidates$Model_Type == mode, ]
    custom_run_membership(rows, custom, allowed, averages = averages,
      required_custom = !averages && !identical(suffix, "-ensemble_models"))
    rows
  }
  saved <- list()
  for (combo in unique(combos)) {
    saved[[combo]] <- list(
      single = read_rows("forecasts", "-single_models", combo, complete && isTRUE(local)),
      global = read_rows("forecasts", "-global_models", combo, complete && isTRUE(global)),
      average = read_rows("forecasts", "-average_models", combo, FALSE),
      ensemble = read_rows("forecasts", "-ensemble_models", combo, FALSE))
    if (!is.null(saved[[combo]]$ensemble) && nrow(saved[[combo]]$ensemble)) stop("Learned ensemble artifact is outside the custom run pool.", call. = FALSE)
    read_rows("models", "-single_models", combo, complete && isTRUE(local), object = TRUE)
  }
  read_rows("models", "-single_models", hash_data("All-Data"), complete && isTRUE(global), object = TRUE)
  saved
}