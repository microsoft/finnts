# Construct an internal, inert custom-model definition from human instructions,
# confirmed interpretation, local/global capabilities, named function source,
# data requirements, fixed parameters and existing-package declarations.
# Optional lineage and display timestamps are data only. Returns a serializable
# definition with a content identity, not a fitted model or approval to execute.
# Every new definition has the current chronological runtime. An omitted runtime
# at construction uses series-separable prediction; saved definitions must already
# contain the complete current contract and are never upgraded during validation.
# Source is parsed but never evaluated; malformed definitions raise errors.
new_custom_model_definition <- function(name,
                                        instructions,
                                        interpretation,
                                        model_type,
                                        source,
                                        requirements,
                                        fixed_parameters = list(),
                                        packages = character(),
                                        parent_version = NULL,
                                        created_at = NULL) {
  if (is.list(requirements) && is.null(requirements$runtime)) requirements$runtime <- list(
    version = 2L, prediction_scope = "complete_horizon", cohort = "series", group_columns = character()
  )
  definition <- list(
    schema_version = 3L,
    name = name,
    origin = "human_defined",
    instructions = instructions,
    interpretation = interpretation,
    model_type = model_type,
    source = source,
    requirements = requirements,
    fixed_parameters = fixed_parameters,
    packages = packages,
    parent_version = parent_version,
    created_at = created_at
  )
  validate_custom_model_definition(definition)
  definition$version_id <- custom_model_definition_digest(definition)
  structure(definition, class = "finnts_custom_model_definition")
}

# Return the canonical declared-package eligibility boundary without loading or
# inspecting installed namespaces. Authoring separately intersects this set with
# real installed metadata; definition validation uses the same names.
custom_model_allowed_packages <- function() {
  c(
    "finnts", "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils",
    "boot", "class", "cluster", "codetools", "foreign", "KernSmooth", "lattice",
    "MASS", "Matrix", "mgcv", "nlme", "nnet", "rpart", "spatial", "survival",
    "callr", "cli", "Cubist", "dials", "digest", "doParallel", "dplyr", "earth",
    "feasts", "foreach", "forecast", "fs", "generics", "glue", "glmnet", "gtools",
    "httr", "hts", "jsonlite", "kernlab", "lubridate", "magrittr", "parsnip",
    "plyr", "purrr", "recipes", "rlang", "rsample", "rules", "snakecase", "stringr",
    "tibble", "tidyr", "tidyselect", "timetk", "tune", "vroom", "workflows",
    "arrow", "AzureStor", "Boruta", "caret", "corrr", "ellmer", "energy", "knitr",
    "Microsoft365R", "nixtlar", "notebookutils", "qs2", "ranger", "reactable",
    "rmarkdown", "sparklyr", "testthat", "tseries", "withr", "xgboost", "vip", "modeltime"
  )
}

# Inspect the current definition without executing source or reading packages.
# Definitions contain only plain lists and finite, nonmissing atomic values;
# named lists are maps and unnamed lists/vectors retain sequence semantics.
# The only root class allowed is finnts_custom_model_definition. Package names
# are checked against the declared FinnTS/base/recommended boundary, not installed
# availability. Returns invisible TRUE or an actionable error. The stored digest
# is shape-checked, not authenticated: this is neither safety nor approval.
# Chronological original-scale R1 and complete-horizon execution are mandatory.
# Optional hierarchy metadata requires explicit local per-node reconciliation
# and univariate logic; format tags reject obsolete development artifacts.
validate_custom_model_definition <- function(definition) {
  # Recursively reject executable or attributed values before S3 dispatch is
  # possible. Plain names are allowed, must be unique, and cannot be blank.
  check_passive <- function(value, field, depth = 0L) {
    if (depth > 64L ||
      !typeof(value) %in% c("NULL", "list", "character", "logical", "integer", "double") ||
      any(!names(attributes(value)) %in% "names")) {
      stop("Custom model ", field, " must contain passive, unclassed data only.", call. = FALSE)
    }
    labels <- names(value)
    if (!is.null(labels) && (anyNA(labels) ||
      any(!nzchar(trimws(labels))) || anyDuplicated(enc2utf8(labels)))) {
      stop("Custom model ", field, " must have unique names without blanks.", call. = FALSE)
    }
    if (is.list(value)) {
      for (index in seq_along(value)) {
        label <- if (is.null(labels)) as.character(index) else labels[[index]]
        check_passive(value[[index]], paste0(field, "$", label), depth + 1L)
      }
    } else if (is.numeric(value) && any(!is.finite(value))) {
      stop("Custom model ", field, " requires finite, nonmissing values.", call. = FALSE)
    } else if (anyNA(value)) {
      stop("Custom model ", field, " requires nonmissing values.", call. = FALSE)
    }
    if (is.character(value) &&
      (any(Encoding(value) == "bytes") || any(!validUTF8(enc2utf8(value))))) {
      stop("Custom model ", field, " requires valid text encoding.", call. = FALSE)
    }
    invisible(NULL)
  }

  # Check scalar text or unique string sets after passive-data validation.
  # Empty sets are permitted only for declarations such as optional predictors.
  check_text <- function(value, field, scalar = FALSE, empty = FALSE) {
    if (!is.character(value) || (!empty && !length(value)) ||
      (scalar && length(value) != 1L) || any(!nzchar(trimws(value))) ||
      anyDuplicated(enc2utf8(value))) {
      stop("Custom model ", field, " must contain valid, unique text values.", call. = FALSE)
    }
    invisible(NULL)
  }

  if (typeof(definition) != "list" ||
    any(!names(attributes(definition)) %in% c("names", "class")) ||
    (!is.null(attr(definition, "class")) &&
      !identical(attr(definition, "class"), "finnts_custom_model_definition"))) {
    stop("Custom model definition must be a passive named list.", call. = FALSE)
  }
  definition <- unclass(definition)
  check_passive(definition, "definition")
  required <- c(
    "schema_version", "name", "origin", "instructions", "interpretation",
    "model_type", "source", "requirements", "fixed_parameters", "packages",
    "parent_version", "created_at"
  )
  if (!all(required %in% names(definition))) {
    stop("Custom model is missing fields: ",
      paste(setdiff(required, names(definition)), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (any(!names(definition) %in% c(required, "version_id"))) {
    stop("Custom model contains unsupported fields: ",
      paste(setdiff(names(definition), c(required, "version_id")), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!identical(definition$schema_version, 3L)) stop("Unsupported custom-model development format; recreate the model.", call. = FALSE)
  for (field in c("name", "origin", "instructions", "interpretation")) {
    check_text(definition[[field]], field, scalar = TRUE)
  }
  if (!identical(definition$origin, "human_defined")) {
    stop("Custom model origin must be human_defined; refinement is unsupported.", call. = FALSE)
  }
  reserved <- c(list_models(), "all", "all-data", "best-model", "local", "global", "ensemble", "average")
  if (!grepl("^[a-z][a-z0-9_-]*$", definition$name) ||
    grepl("--", definition$name, fixed = TRUE) || tolower(definition$name) %in% reserved) {
    stop("Custom model name must be a safe, nonreserved identifier without '--'.", call. = FALSE)
  }
  if (!is.character(definition$model_type) ||
    !length(definition$model_type) || anyNA(definition$model_type) ||
    anyDuplicated(definition$model_type) ||
    !all(definition$model_type %in% c("local", "global"))) {
    stop("Custom model model_type must contain unique local/global values.", call. = FALSE)
  }
  if (!is.character(definition$source) ||
    !all(c("fit", "predict") %in% names(definition$source)) ||
    any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", names(definition$source)))) {
    stop("Custom model source must include named fit and predict strings.", call. = FALSE)
  }
  for (source in definition$source) {
    parsed <- tryCatch(parse(text = enc2utf8(source), keep.source = FALSE),
      error = function(error) NULL
    )
    if (length(parsed) != 1L || !is.call(parsed[[1L]]) ||
      !identical(parsed[[1L]][[1L]], as.name("function"))) {
      stop("Custom model source entries must each be one function expression.", call. = FALSE)
    }
  }

  requirements <- definition$requirements
  requirement_fields <- c(
    "predictors", "recipes", "target_scale", "date_types",
    "forecast_horizon", "missing_data", "runtime", if ("hierarchy" %in% names(requirements)) "hierarchy"
  )
  if (!is.list(requirements) || !setequal(names(requirements), requirement_fields)) {
    stop("Custom model requirements must declare: ",
      paste(requirement_fields, collapse = ", "), ".",
      call. = FALSE
    )
  }
  check_text(requirements$predictors, "requirements$predictors", empty = TRUE)
  check_text(requirements$recipes, "requirements$recipes")
  if (!identical(unname(requirements$recipes), "R1")) stop("Custom model requirements$recipes must use R1.", call. = FALSE)
  check_text(requirements$target_scale, "requirements$target_scale", scalar = TRUE)
  if (!identical(requirements$target_scale, "original")) stop("Custom model requirements$target_scale must be original.", call. = FALSE)
  check_text(requirements$date_types, "requirements$date_types")
  if (!all(requirements$date_types %in% c("year", "quarter", "month", "week", "day"))) {
    stop("Custom model requirements$date_types must use supported Finn cadences.", call. = FALSE)
  }
  horizon <- requirements$forecast_horizon
  if (!is.numeric(horizon) || !length(horizon) || any(horizon <= 0) ||
    any(horizon != floor(horizon)) || anyDuplicated(horizon)) {
    stop("Custom model requirements$forecast_horizon must contain unique positive whole numbers.", call. = FALSE)
  }
  check_text(requirements$missing_data, "requirements$missing_data", scalar = TRUE)
  custom_model_runtime(requirements$runtime, requirements$predictors, definition$model_type)
  if ("hierarchy" %in% names(requirements)) {
    hierarchy <- requirements$hierarchy
    fields <- c("forecast_approach", "application", "reconciliation")
    if (!is.list(hierarchy) || !setequal(names(hierarchy), fields)) {
      stop("Custom model hierarchy requires forecast_approach, application and reconciliation.", call. = FALSE)
    }
    for (field in fields) check_text(hierarchy[[field]], paste0("requirements$hierarchy$", field), scalar = TRUE)
    if (!hierarchy$forecast_approach %in% c("standard_hierarchy", "grouped_hierarchy") ||
      !identical(hierarchy$application, "each_prepared_node") ||
      !identical(hierarchy$reconciliation, "finnts_reconciliation")) {
      stop("Custom model hierarchy requires per-node base forecasts and explicit FinnTS reconciliation.", call. = FALSE)
    }
    if (!identical(unname(definition$model_type), "local") || !identical(unname(requirements$recipes), "R1") ||
      !identical(requirements$target_scale, "original") || length(setdiff(requirements$predictors, c("Date", "Combo")))) {
      stop("Custom model hierarchy requires local original-scale R1 without business predictors.", call. = FALSE)
    }
  }
  if (!is.list(definition$fixed_parameters) ||
    (length(definition$fixed_parameters) && is.null(names(definition$fixed_parameters)))) {
    stop("Custom model fixed_parameters must be a named list.", call. = FALSE)
  }

  check_text(definition$packages, "packages", empty = TRUE)
  # The shared eligibility boundary mirrors DESCRIPTION plus R base/recommended packages.
  # A contract test checks every declared dependency so changes cannot drift.
  allowed_packages <- custom_model_allowed_packages()
  if (!all(definition$packages %in% allowed_packages)) {
    stop("Custom model packages must already be declared by FinnTS or supplied with R.", call. = FALSE)
  }
  for (field in c("parent_version", "version_id")) {
    value <- definition[[field]]
    if (!is.null(value)) {
      check_text(value, field, scalar = TRUE)
      if (!grepl("^[a-f0-9]{64}$", value)) {
        stop("Custom model ", field, " must be a lowercase SHA-256 digest.", call. = FALSE)
      }
    }
  }
  if (!is.null(definition$created_at)) {
    check_text(definition$created_at, "created_at", scalar = TRUE)
  }
  invisible(TRUE)
}

# Hash current semantic content with SHA-256 and version-2 R serialization.
# Normalize text/names to UTF-8, sort named list maps and declared unordered sets,
# and preserve vector/unnamed-list sequence order and numeric types in fixed
# parameters. Source names are unordered; source text is otherwise exact.
# The stored digest and created_at display field are excluded. Returns the
# recomputed hash without changing input or comparing it to a stored hash.
# No source execution, storage access or approval occurs here.
custom_model_definition_digest <- function(definition) {
  validate_custom_model_definition(definition)

  # Return a UTF-8, radix-ordered copy of plain data. Named lists are maps, while
  # atomic vectors and unnamed lists remain ordered sequences, including names.
  normalize <- function(value) {
    labels <- names(value)
    if (is.list(value)) {
      value <- lapply(value, normalize)
    } else if (is.character(value)) {
      value <- enc2utf8(value)
    }
    if (!is.null(labels)) {
      names(value) <- enc2utf8(labels)
      if (is.list(value)) {
        value <- value[order(names(value), method = "radix")]
      }
    }
    value
  }

  content <- unclass(definition)
  content$version_id <- NULL
  content$created_at <- NULL
  content <- normalize(content)
  for (field in c("model_type", "packages")) {
    content[[field]] <- sort(unname(content[[field]]), method = "radix")
  }
  content$source <- content$source[order(names(content$source), method = "radix")]
  for (field in c("predictors", "recipes", "date_types")) {
    content$requirements[[field]] <- sort(unname(content$requirements[[field]]), method = "radix")
  }
  content$requirements$forecast_horizon <- sort(as.numeric(content$requirements$forecast_horizon))
  digest::digest(content, algo = "sha256", serializeVersion = 2)
}
