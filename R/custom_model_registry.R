# Normalize a flat character vector/list of model names without interpreting
# aliases. NULL denotes unspecified; empty explicit selections, nontext entries,
# missing/blank or duplicate names error. Empty exclusions are permitted.
custom_model_pool_names <- function(value, field, empty = FALSE) {
  if (is.null(value)) return(NULL)
  if (typeof(value) == "list" && !is.object(value) &&
    all(vapply(value, function(entry) {
      is.character(entry) && !is.object(entry) && is.null(dim(entry)) && length(entry) == 1L
    }, logical(1)))) {
    value <- unlist(value, use.names = FALSE)
    if (is.null(value)) value <- character()
  }
  if (!is.character(value) || is.object(value) || !is.null(dim(value)) ||
    (!empty && !length(value)) || anyNA(value) ||
    any(!nzchar(trimws(value))) || anyDuplicated(value)) {
    stop(field, " must contain unique, nonblank model names in a flat character vector or list.",
      call. = FALSE)
  }
  unname(value)
}

# Validate passive M1 content and require a current stored version identity.
# Returns the original definition unchanged; syntax is parsed, never evaluated.
# Missing or stale digests error before registry I/O or candidate resolution.
custom_model_current_definition <- function(definition) {
  validate_custom_model_definition(definition)
  if (!identical(definition$version_id, custom_model_definition_digest(definition))) {
    stop("Custom model version_id does not match its definition content.", call. = FALSE)
  }
  definition
}

# Validate a plain schema-v1 reference before deriving any storage path. Only
# safe model names and full lowercase digests are accepted; caller-controlled
# paths, approvals, consent and extra attributes/fields are rejected. Returns
# the unchanged passive reference, which identifies content but authorizes none.
custom_model_reference <- function(reference) {
  fields <- c("registry_schema_version", "name", "version_id")
  if (typeof(reference) != "list" ||
    any(!names(attributes(reference)) %in% "names") ||
    length(reference) != length(fields) || !setequal(names(reference), fields) ||
    !identical(reference$registry_schema_version, 1L)) {
    stop("Custom model reference requires registry schema 1L, name and version_id only.", call. = FALSE)
  }
  for (field in c("name", "version_id")) {
    value <- reference[[field]]
    if (!is.character(value) || length(value) != 1L || anyNA(value) ||
      !is.null(attributes(value))) {
      stop("Custom model reference ", field, " must be plain scalar text.", call. = FALSE)
    }
  }
  reserved <- c(list_models(), "all", "all-data", "best-model", "local", "global", "ensemble", "average")
  if (!grepl("^[a-z][a-z0-9_-]*$", reference$name) ||
    grepl("--", reference$name, fixed = TRUE) || reference$name %in% reserved ||
    !grepl("^[a-f0-9]{64}$", reference$version_id)) {
    stop("Custom model reference requires a safe nonreserved name and full lowercase SHA-256 version_id.",
      call. = FALSE)
  }
  reference
}

# Derive project/root-scoped RDS storage from a validated reference and caller
# run_info. A copied run token makes paths independent of forecast run names;
# the full digest is the suffix, never arbitrary alias text. Local NULL paths
# use tempdir. Remote roots must be explicit. Returns settings/path/suffix without
# modifying the caller, enumerating artifacts or performing I/O.
custom_model_registry_location <- function(reference, run_info) {
  reference <- custom_model_reference(reference)
  if (!is.list(run_info) || !is.character(run_info$project_name) ||
    length(run_info$project_name) != 1L || anyNA(run_info$project_name) ||
    !nzchar(trimws(run_info$project_name))) {
    stop("Registry run_info requires a nonblank project_name.", call. = FALSE)
  }
  if (!is.null(run_info$storage_object) &&
    !inherits(run_info$storage_object, c("blob_container", "ms_drive"))) {
    stop("Unsupported storage object for the custom model registry.", call. = FALSE)
  }
  if (is.null(run_info$path) && is.null(run_info$storage_object)) run_info$path <- tempdir()
  if (!is.character(run_info$path) || length(run_info$path) != 1L ||
    anyNA(run_info$path) || !nzchar(trimws(run_info$path))) {
    stop("Registry run_info requires an explicit storage root for remote artifacts.", call. = FALSE)
  }
  run_info$run_name <- ".finnts-custom-model-registry-v1"
  run_info$object_output <- "rds"
  suffix <- paste0("-custom-definition-", reference$version_id)
  list(run_info = run_info, suffix = suffix,
    path = local_artifact_path(run_info, "prep_models", suffix, extension = "rds"))
}

# Read one exact record and return its validated M1 definition. Optional absence
# returns NULL only when metadata/transport confirms no file. Present RDS NULL
# is malformed, not absent. Remote content is downloaded once by the existing
# exact transport, then read locally; provider/deserialization errors propagate.
# Schema/name/hash mismatches error without modifying the artifact or listing.
custom_model_read_record <- function(reference, location, allow_missing = FALSE) {
  info <- location$run_info
  path <- location$path
  if (is.null(info$storage_object)) {
    path <- local_artifact_files(path, allow_missing = allow_missing)
    if (!length(path)) return(NULL)
  } else {
    directory <- tempfile("finnts-custom-registry-")
    fs::dir_create(directory)
    destination <- fs::path(directory, fs::path_file(path))
    if (!download_exact_artifact(info$storage_object, path, destination, allow_missing)) return(NULL)
    info$storage_object <- NULL
    path <- destination
  }
  record <- read_exact_artifact(info, path, return_type = "object")
  fields <- c("registry_schema_version", "name", "version_id", "definition")
  if (typeof(record) != "list" || any(!names(attributes(record)) %in% "names") ||
    length(record) != length(fields) || !setequal(names(record), fields)) {
    stop("Malformed custom model registry record.", call. = FALSE)
  }
  stored <- custom_model_reference(record[c("registry_schema_version", "name", "version_id")])
  if (!identical(stored, reference[c("registry_schema_version", "name", "version_id")])) {
    stop("Custom model registry record does not match its reference.", call. = FALSE)
  }
  definition <- custom_model_current_definition(record$definition)
  if (!identical(definition$name, reference$name) ||
    !identical(definition$version_id, reference$version_id)) {
    stop("Custom model registry definition does not match its reference.", call. = FALSE)
  }
  definition
}

# Persist a current passive M1 definition in fixed-format registry storage.
# Returns its exact schema/name/version reference. One optional exact read avoids
# rewriting existing valid content; an absent artifact is written once through
# write_data and verified by one required read-back. Corrupt existing artifacts
# error untouched. Sequential idempotence is not a cross-provider transaction,
# cryptographic authorization, user approval or permission to execute source.
write_custom_model_definition <- function(definition, run_info) {
  definition <- custom_model_current_definition(definition)
  reference <- custom_model_reference(list(registry_schema_version = 1L,
    name = definition$name, version_id = definition$version_id))
  location <- custom_model_registry_location(reference, run_info)
  existing <- custom_model_read_record(reference, location, allow_missing = TRUE)
  if (is.null(existing)) {
    record <- c(reference, list(definition = definition))
    write_data(record, combo = NULL, run_info = location$run_info,
      output_type = "object", folder = "prep_models", suffix = location$suffix)
    custom_model_read_record(reference, location)
  }
  reference
}

# Resolve one passive exact-version reference within the caller's project/root.
# Returns its verified definition, never a workflow or approved envelope. Missing
# required artifacts, provider failures, malformed records and stale hashes error;
# there is no alternate-version lookup, regeneration, cache or fallback model.
read_custom_model_definition <- function(reference, run_info) {
  reference <- custom_model_reference(reference)
  location <- custom_model_registry_location(reference, run_info)
  custom_model_read_record(reference, location)
}

# Resolve an internal candidate pool, not a public approval/enrollment envelope.
# Explicit flat model names override exclusions and retain requested order;
# NULL selects the existing built-in catalog minus exclusions, never customs.
# custom_models is a uniquely named list of passive, current M1 definitions or
# schema-v1 references. All entries are validated, but only selected references
# are fetched using run_info. Unique selections and matching aliases guarantee
# at most one read per reference per call, with no persistent cache.
# Returns selected names, built-in names, named custom definitions/capabilities,
# and a SHA-256 pool_id over sorted selected names and selected custom versions.
# Capabilities are declarations, not verified data compatibility. No execution,
# fitting, installation, storage mutation or permission is granted. Unknown
# selected names error before any reads; provider errors are never fallbacks.
resolve_custom_model_pool <- function(models_to_run = NULL,
                                      models_not_to_run = NULL,
                                      custom_models = list(),
                                      run_info = NULL) {
  selected <- custom_model_pool_names(models_to_run, "models_to_run")
  excluded <- custom_model_pool_names(models_not_to_run, "models_not_to_run", empty = TRUE)
  catalog <- list_models()
  if (is.null(custom_models)) custom_models <- list()
  if (typeof(custom_models) != "list" ||
    any(!names(attributes(custom_models)) %in% "names") ||
    (length(custom_models) && is.null(names(custom_models)))) {
    stop("custom_models must be a uniquely named list of passive definitions or references.",
      call. = FALSE)
  }
  aliases <- custom_model_pool_names(names(custom_models), "custom_models names", empty = TRUE)
  if (any(aliases %in% catalog)) {
    stop("custom_models names must not collide with built-in models.", call. = FALSE)
  }
  references <- character()
  for (alias in aliases) {
    entry <- custom_models[[alias]]
    if (is.list(entry) && "registry_schema_version" %in% names(entry)) {
      entry <- custom_model_reference(entry)
      references <- c(references, alias)
    } else {
      entry <- custom_model_current_definition(entry)
    }
    if (!identical(alias, entry$name)) {
      stop("Custom model alias must match its declared name.", call. = FALSE)
    }
  }
  if (is.null(selected)) selected <- setdiff(catalog, excluded)
  if (!length(selected)) stop("The selected model pool must not be empty.", call. = FALSE)
  unknown <- setdiff(selected, c(catalog, aliases))
  if (length(unknown)) {
    stop("Unknown selected model names: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  custom <- custom_models[selected[selected %in% aliases]]
  for (alias in intersect(names(custom), references)) {
    custom[[alias]] <- read_custom_model_definition(custom[[alias]], run_info)
  }
  capabilities <- lapply(custom, function(definition) {
    c(list(model_type = definition$model_type), definition$requirements)
  })
  versions <- vapply(custom, function(definition) definition$version_id, character(1))
  versions <- if (length(versions)) versions[order(names(versions), method = "radix")] else character()
  identity <- list(selected = sort(selected, method = "radix"), versions = versions)
  list(
    selected = selected,
    built_in = selected[selected %in% catalog],
    custom = custom,
    capabilities = capabilities,
    pool_id = digest::digest(identity, algo = "sha256", serializeVersion = 2)
  )
}