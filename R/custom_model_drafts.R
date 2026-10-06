# Hash passive authoring state consistently; these digests detect change, not
# authenticated human approval. The stored digest is excluded from its own hash.
custom_draft_digest <- function(state) {
  state$state_digest <- NULL
  digest::digest(unclass(state), algo = "sha256", serializeVersion = 2)
}

# Reject executable values and unrecognized attributes throughout a trusted
# draft before checking its digest. Reviewed tables/dates and existing inert
# model classes are allowed; callbacks and session environments are not.
custom_draft_passive <- function(value, depth = 0L) {
  if (depth > 64L || !typeof(value) %in% c("NULL", "list", "character", "logical", "integer", "double")) {
    custom_author_abort("draft", "Draft contains nonpassive content.")
  }
  attributes <- attributes(value)
  allowed <- "names"
  if (!is.null(attributes$class)) {
    if (!identical(attributes$class, "Date") && !identical(attributes$class, "data.frame") && !identical(
      attributes$class,
      "finnts_custom_model"
    ) && !identical(attributes$class, "finnts_custom_model_definition")) {
      custom_author_abort("draft", "Draft contains an unsupported class.")
    }
    allowed <- c(allowed, "class", if (identical(attributes$class, "data.frame")) "row.names")
  }
  if (any(!names(attributes) %in% allowed) || anyDuplicated(names(value)))
    custom_author_abort("draft", "Draft has unsupported attributes or duplicate fields.")
  if (typeof(value) == "list") {
    for (entry in value) custom_draft_passive(entry, depth + 1L)
  } else if (anyNA(value) || (is.numeric(value) && any(!is.finite(value))))
    custom_author_abort("draft", "Draft values must be finite and nonmissing.")
  invisible(NULL)
}

# Measure bounded diagnostic JSON consistently, including Date tables in the
# accepted contract. This serialization neither evaluates nor writes source.
custom_draft_diagnostic_size <- function(diagnostics) {
  nchar(jsonlite::toJSON(diagnostics, auto_unbox = TRUE, null = "null", dataframe = "rows", Date = "ISO8601", digits = NA),
    type = "bytes"
  )
}

# Fingerprint each validated source function for later review comparisons.
# Retains names and hashes only, never another source snapshot or fitted data.
custom_draft_source_fingerprints <- function(source) {
  vapply(source, digest::digest, character(1), algo = "sha256", serializeVersion = 2)
}

# Capture only whitelisted provider-visible contract fields. Hash the retained
# passive proposal, never a Chat, full state or arbitrary condition. Unknown or
# executable fields are omitted explicitly. The enclosing diagnostic cap may
# remove the proposal while retaining its hash and package-owned field/code.
custom_draft_contract_rejection <- function(proposal, settings, failure, field = NULL) {
  allowed <- custom_author_contract_fields(settings$authoring_protocol, settings$name, settings$model_type, identical(
    settings$approval_mode,
    "automatic"
  ))
  safe <- tryCatch(
    {
      custom_run_passive(proposal)
      if (!is.list(proposal))
        stop("Invalid contract snapshot")
      proposal[intersect(names(proposal), allowed)]
    },
    error = function(error) NULL
  )
  list(
    schema_version = 1L, proposal_id = if (is.null(safe)) NULL else digest::digest(safe, algo = "sha256", serializeVersion = 2),
    proposal = safe, field = custom_author_contract_field(failure$reason, field), code = failure$reason, omitted = is.null(safe) ||
      length(safe) != length(proposal)
  )
}

# Validate the optional rejected-contract snapshot independently of accepted
# contract/source identity. Fields and codes are closed package vocabularies;
# retained content must be passive, whitelisted and bound to its own digest.
validate_custom_draft_contract_rejection <- function(rejection) {
  custom_author_fields(rejection, c("schema_version", "proposal_id", "proposal", "field", "code", "omitted"), "diagnostics")
  codes <- c(
    "invalid_contract", "invalid_contract_response", "invalid_contract_metadata", "invalid_parameters_json", "protocol_clarification",
    "invalid_defaults_used", "invalid_parameter_schema", "invalid_error_contract", "invalid_validation_properties",
    "invalid_examples_shape", "invalid_examples", "example_mode", "example_horizon", "contract_validation_error", "invalid_history_requirements",
    "insufficient_example_history", "business_clarification"
  )
  if (!identical(rejection$schema_version, 1L) || !custom_author_text(rejection$field) || !custom_author_text(rejection$code) ||
    !rejection$code %in% codes || !is.logical(rejection$omitted) || length(rejection$omitted) != 1L || is.na(rejection$omitted) ||
    (!is.null(rejection$proposal_id) && (!custom_author_text(rejection$proposal_id) || !grepl("^[a-f0-9]{64}$", rejection$proposal_id))) ||
    (is.null(rejection$proposal) && !rejection$omitted))
    custom_author_abort("diagnostics", "Invalid rejected contract snapshot.")
  custom_author_contract_field(rejection$code, rejection$field)
  if (!is.null(rejection$proposal)) {
    allowed <- c(
      custom_author_contract_fields(), "examples", "defaults_used", "history_requirements", "parameter_schema",
      "error_policy", "validation_properties", "prediction_cohort"
    )
    if (!is.list(rejection$proposal) || (length(rejection$proposal) && is.null(names(rejection$proposal))) || any(!names(rejection$proposal) %in%
      allowed) || !identical(rejection$proposal_id, digest::digest(rejection$proposal, algo = "sha256", serializeVersion = 2))) {
      custom_author_abort("diagnostics", "Rejected contract content or identity changed.")
    }
  }
  rejection
}

# Validate current bounded diagnostic history and hash-bound snapshots without executing source.
# Require passive records, closed failure codes and exact source fingerprints on accepted source entries.
# Failure-specific details and rejected proposals may be absent; the enclosing history may not exceed one MiB.
validate_custom_draft_diagnostics <- function(diagnostics) {
  custom_draft_passive(diagnostics)
  if (!is.list(diagnostics) || !setequal(names(diagnostics), c("schema_version", "history", "snapshot", "omitted", if ("rejected_contract" %in%
    names(diagnostics)) "rejected_contract")) || !identical(diagnostics$schema_version, 1L) || !is.list(diagnostics$history) ||
    length(diagnostics$history) > 12L || !is.logical(diagnostics$omitted) || length(diagnostics$omitted) != 1L)
    custom_author_abort("diagnostics", "Invalid diagnostic history.")
  # Accept absent optional hashes or bounded lowercase SHA-256 identities.
  valid_hash <- function(value) is.null(value) || (custom_author_text(value) && grepl("^[a-f0-9]{64}$", value))
  if ("rejected_contract" %in% names(diagnostics))
    validate_custom_draft_contract_rejection(diagnostics$rejected_contract)
  for (entry in diagnostics$history) {
    custom_author_fields(entry, c(
      "stage", "attempt", "contract_id", "version_id", "source_id", "outcome", "failure",
      if ("source_fingerprints" %in% names(entry)) "source_fingerprints"
    ), "diagnostics")
    if (!custom_author_text(entry$stage) || !entry$stage %in% c("contract", "source", "validation") || !is.integer(entry$attempt) ||
      length(entry$attempt) != 1L || !entry$attempt %in% 1:3 || !custom_author_text(entry$outcome) || !entry$outcome %in%
      c("accepted", "rejected", "validated") || !all(vapply(
      entry[c("contract_id", "version_id", "source_id")], valid_hash,
      logical(1)
    ))) {
      custom_author_abort("diagnostics", "Invalid diagnostic attempt.")
    }
    if ("source_fingerprints" %in% names(entry)) {
      fingerprints <- entry$source_fingerprints
      if (entry$stage != "source" || entry$outcome != "accepted" || is.null(entry$source_id) || is.null(entry$version_id) ||
        !is.character(fingerprints) || !length(fingerprints) || length(fingerprints) > 32L || is.null(names(fingerprints)) ||
        anyDuplicated(names(fingerprints)) || !all(vapply(names(fingerprints), custom_author_text, logical(1))) ||
        !all(grepl("^[a-f0-9]{64}$", fingerprints))) {
        custom_author_abort("diagnostics", "Invalid source fingerprints.")
      }
      if (!is.null(diagnostics$snapshot$source) && identical(entry$source_id, diagnostics$snapshot$source_id) && !identical(
        fingerprints,
        custom_draft_source_fingerprints(diagnostics$snapshot$source)
      )) {
        custom_author_abort("diagnostics", "Source fingerprints differ from the captured source.")
      }
    }
    if (!is.null(entry$failure)) {
      validate_custom_author_failure(entry$failure)
      if (!identical(entry$failure$version_id, entry$version_id))
        custom_author_abort("diagnostics", "Failure version changed.")
    }
  }
  snapshot <- diagnostics$snapshot
  if (!is.null(snapshot)) {
    if (!is.list(snapshot) || !setequal(names(snapshot), c("contract_id", "version_id", "source_id", "contract", "source")) ||
      !all(vapply(snapshot[c("contract_id", "version_id", "source_id")], valid_hash, logical(1)))) {
      custom_author_abort("diagnostics", "Invalid diagnostic snapshot.")
    }
    if (!is.null(snapshot$contract) && !identical(snapshot$contract_id, digest::digest(snapshot$contract,
      algo = "sha256",
      serializeVersion = 2
    )))
      custom_author_abort("diagnostics", "Diagnostic contract changed.")
    if (!is.null(snapshot$source) && (!is.character(snapshot$source) || is.null(names(snapshot$source)) || !identical(
      snapshot$source_id,
      digest::digest(snapshot$source, algo = "sha256", serializeVersion = 2)
    ))) {
      custom_author_abort("diagnostics", "Diagnostic source changed.")
    }
  }
  if (custom_draft_diagnostic_size(diagnostics) > 1048576L)
    custom_author_abort("diagnostics", "Diagnostic history exceeds one MiB.")
  diagnostics
}

# Append one bounded outcome, keeping only a single latest source/contract
# snapshot. Oversized snapshots retain hashes plus an explicit omission marker.
# Rejected malformed proposals are omitted, never serialized as arbitrary data.
# Contract proposals retain only known passive response fields with separate
# field/code/hash evidence. Unknown or oversized content is explicitly omitted.
# Body-protocol calculations are assembled passively so repairs receive the exact wrapper
# and calculation source; malformed bodies remain explicitly omitted.
# New complete-function inputs use the same normalization as candidate creation,
# so snapshot hashes describe the reviewed code, not an unnormalized response.
# Accepted source entries retain compact function fingerprints for repair review.
# Versioned calendar helpers are frozen into snapshots exactly as in candidates.
custom_draft_record <- function(state, outcome, failure = NULL, proposal = NULL, field = NULL) {
  diagnostics <- state$diagnostics %||% list(schema_version = 1L, history = list(), snapshot = NULL, omitted = FALSE)
  source <- NULL
  if (state$stage == "contract" && identical(outcome, "rejected") && !is.null(proposal)) {
    diagnostics$rejected_contract <- custom_draft_contract_rejection(proposal, state$settings, failure, field)
  }
  if (!is.null(proposal) && state$stage == "source") {
    source <- tryCatch(
      {
        proposal <- custom_author_source_response(proposal)
        proposal <- custom_author_assemble(proposal)
        custom_author_fields(proposal, "source", "diagnostics")
        entries <- proposal$source
        if (!is.list(entries) || !length(entries) || length(entries) > 32L)
          stop("Invalid source snapshot")
        for (entry in entries) {
          custom_author_fields(entry, c("name", "code"), "diagnostics")
          if (!custom_author_text(entry$name) || !custom_author_text(entry$code))
            stop("Invalid source snapshot")
        }
        names <- vapply(entries, function(entry) entry$name, character(1))
        if (anyDuplicated(names))
          stop("Invalid source snapshot")
        stats::setNames(vapply(entries, function(entry) entry$code, character(1)), names)
      },
      error = function(error) NULL
    )
  }
  if (state$stage %in% c("contract", "source")) {
    diagnostics$snapshot <- list(contract_id = state$contract_id, version_id = state$definition$version_id, source_id = if (is.null(source)) NULL else digest::digest(source,
      algo = "sha256", serializeVersion = 2
    ), contract = state$contract, source = source)
    diagnostics$omitted <- (state$stage == "source" && !is.null(proposal) && is.null(source)) || isTRUE(diagnostics$rejected_contract$omitted)
  }
  entry <- list(
    stage = state$stage, attempt = state$attempts[[if (state$stage == "validation") "source" else state$stage]],
    contract_id = state$contract_id, version_id = if (is.null(failure)) state$definition$version_id else failure$version_id,
    source_id = diagnostics$snapshot$source_id, outcome = outcome, failure = failure
  )
  if (state$stage == "source" && identical(outcome, "accepted") && !is.null(source)) {
    entry$source_fingerprints <- custom_draft_source_fingerprints(source)
  }
  diagnostics$history <- tail(c(diagnostics$history, list(entry)), 12L)
  if (custom_draft_diagnostic_size(diagnostics) > 1048576L && !is.null(diagnostics$rejected_contract)) {
    diagnostics$rejected_contract["proposal"] <- list(NULL)
    diagnostics$rejected_contract$omitted <- TRUE
    diagnostics$omitted <- TRUE
  }
  if (custom_draft_diagnostic_size(diagnostics) > 1048576L) {
    diagnostics$snapshot[c("contract", "source")] <- list(NULL, NULL)
    diagnostics$omitted <- TRUE
  }
  state$diagnostics <- validate_custom_draft_diagnostics(diagnostics)
  state
}

# Capture runtime/package metadata and the current-session execution contract.
# Old isolated-execution consent cannot be reused with broader session access.
# Missing metadata is a typed draft error; no private paths enter review text.
custom_draft_environment <- function(metadata) {
  packages <- sort(unique(c("finnts", "ellmer", "callr", metadata$available_packages)), method = "radix")
  result <- list(r_version = as.character(getRversion()), packages = stats::setNames(lapply(packages, function(package) {
    description <- system.file("DESCRIPTION", package = package)
    if (!nzchar(description)) custom_author_abort("draft", paste("Package metadata is unavailable for", package, "- restore the package or start a new draft."))
    list(version = as.character(utils::packageVersion(package)), description = digest::digest(file = description, algo = "sha256"))
  }), packages))
  result$validation_execution <- "current_session"
  result
}

# Decode only an exact caller-owned RDS artifact. Failures stay typed and do not
# disclose arbitrary deserializer text or private paths; nothing is rewritten.
custom_draft_read <- function(path) {
  if (!custom_author_text(path) || !file.exists(path))
    custom_author_abort("draft", "Required draft artifact is missing; restore it or restart.")
  tryCatch(readRDS(path), error = function(error) custom_author_abort("draft", "Required draft artifact is corrupt or unreadable."))
}

# Create an internal lifecycle with no fitted state or session. Persisted mode
# writes a bounded private snapshot in a unique caller-owned/temporary directory;
# only references enter the draft. Synchronous calls keep data outside the state.
new_custom_model_draft <- function(settings, data, draft_path, persist) {
  if (!is.null(settings$validation_examples)) {
    settings$validation_examples <- lapply(settings$validation_examples, function(example) {
      for (field in intersect(c("history", "new_data", "expected"), names(example))) {
        if (is.data.frame(example[[field]]))
          example[[field]] <- as.data.frame(example[[field]])
      }
      example
    })
    custom_draft_passive(settings$validation_examples)
  }
  store <- NULL
  context <- NULL
  if (persist) {
    if (!is.null(draft_path) && !custom_author_text(draft_path))
      custom_author_abort("draft", "draft_path must be a local directory.")
    root <- if (is.null(draft_path))
      tempdir()
    else draft_path
    if (!dir.exists(root) && !dir.create(root, recursive = TRUE))
      custom_author_abort("draft", "Cannot create draft directory.")
    store <- tempfile("finnts-draft-", tmpdir = normalizePath(root, winslash = "/", mustWork = TRUE))
    if (!dir.create(store))
      custom_author_abort("draft", "Cannot create private draft directory.")
    store <- normalizePath(store, winslash = "/", mustWork = TRUE)
    snapshot <- file.path(store, "context.rds")
    saveRDS(data[c("data", "metadata")], snapshot)
    context <- list(snapshot = snapshot, hash = digest::digest(file = snapshot, algo = "sha256"), sources = data$sources %||%
      list(), durable = !is.null(draft_path))
    message(if (is.null(draft_path))
      "Draft context is temporary and may expire at session end."
    else "Draft context is retained in your chosen directory; manage its retention as confidential data.")
  }
  state <- structure(list(
    schema_version = 2L, draft_id = digest::digest(list(store, Sys.time(), Sys.getpid()), algo = "sha256"),
    revision = 0L, status = "running", stage = "contract", settings = settings, metadata = data$metadata, context = context,
    environment = custom_draft_environment(data$metadata), answers = list(), contract = NULL, contract_id = NULL, definition = NULL,
    report = NULL, checks = NULL, consent = list(intent = NULL, test = NULL), attempts = list(contract = 0L, source = 0L),
    diagnostic = "initial", pending = NULL, receipts = list(), result = NULL, store = store, state_digest = NULL
  ), class = "finnts_custom_model_draft")
  state$diagnostics <- list(schema_version = 1L, history = list(), snapshot = NULL, omitted = FALSE)
  custom_draft_save(state)
}

# Persist a full immutable revision before advancing the exact current pointer.
# A torn pointer is a hard error on resume. No deletion, repair, transaction or
# concurrent-writer guarantee is provided; files belong to the draft directory.
custom_draft_save <- function(state) {
  if (!is.null(state$store) && state$revision > 0L) {
    current <- custom_draft_read(file.path(state$store, "current.rds"))
    if (!identical(current$draft_id, state$draft_id) || !identical(current$revision, state$revision) || !identical(
      current$state_digest,
      state$state_digest
    ))
      custom_author_abort("draft", "A newer draft revision exists; reload current.rds.")
  }
  state$revision <- state$revision + 1L
  state$state_digest <- custom_draft_digest(state)
  if (!is.null(state$store)) {
    filename <- paste0(state$revision, "-", state$state_digest, ".rds")
    path <- file.path(state$store, filename)
    if (file.exists(path))
      custom_author_abort("draft", "Draft revision already exists; restore the current state.")
    saveRDS(state, path)
    if (!identical(readRDS(path), state))
      custom_author_abort("draft", "Draft revision verification failed.")
    pointer <- list(
      schema_version = 2L, draft_id = state$draft_id, revision = state$revision, state_digest = state$state_digest,
      filename = filename
    )
    saveRDS(pointer, file.path(state$store, "current.rds"))
    if (!identical(readRDS(file.path(state$store, "current.rds")), pointer))
      custom_author_abort("draft", "Draft pointer verification failed.")
  }
  state
}

# Set one human gate, binding the request to the next state revision and all
# preceding evidence. The public prompt remains identical to synchronous review.
custom_draft_gate <- function(state, stage, prompt, identity = NULL, review = list(), questions = NULL) {
  state$status <- "review_required"
  state$stage <- stage
  state$pending <- list(request_id = NULL, stage = stage, prompt = prompt, identity = identity, review = review, questions = questions)
  next_state <- state
  next_state$revision <- state$revision + 1L
  state$pending$request_id <- custom_draft_request_id(next_state)
  custom_draft_save(state)
}

# Bind a pending response to the whole passive revision, including the review
# payload and question IDs. Exclude only the digest and request ID themselves.
custom_draft_request_id <- function(state) {
  state$pending["request_id"] <- list(NULL)
  custom_draft_digest(state)
}

# Bind the fixed contract, exact source and validation metadata to a conditional approval review.
# Optional measured repair evidence identifies the prior failure and changed source functions, never inferred consent.
# Return a passive review with explicit execution/hierarchy warnings; do not run candidates or disclose real series values.
custom_draft_review <- function(contract, definition, metadata, diagnostics = NULL) {
  review <- list(contract = contract, definition = definition, validation_sample = metadata, warning = paste(
    "Generated R runs in your current R session with your account permissions and session/environment access.",
    "There is no sandbox or reliable automatic interruption; stuck code may require restarting R.", "Yes authorizes this exact version and approves it only if validation passes."
  ))
  if (!is.null(diagnostics)) {
    validate_custom_draft_diagnostics(diagnostics)
    contract_id <- digest::digest(contract, algo = "sha256", serializeVersion = 2)
    failures <- Filter(function(entry) identical(entry$contract_id, contract_id) && entry$stage == "validation" && entry$outcome ==
      "rejected" && !is.null(entry$failure), diagnostics$history)
    if (length(failures)) {
      previous <- tail(failures, 1L)[[1L]]
      sources <- Filter(function(entry) identical(entry$contract_id, contract_id) && entry$stage == "source" && entry$outcome ==
        "accepted" && identical(entry$version_id, previous$version_id) && !is.null(entry$source_fingerprints), diagnostics$history)
      changed <- NULL
      if (length(sources)) {
        old <- tail(sources, 1L)[[1L]]$source_fingerprints
        current <- custom_draft_source_fingerprints(definition$source)
        functions <- sort(union(names(old), names(current)), method = "radix")
        changed <- functions[vapply(
          functions, function(name) !identical(unname(old[name]), unname(current[name])),
          logical(1)
        )]
      }
      review$repair <- list(failure = previous$failure, changed_functions = changed)
    }
  }
  if (!is.null(contract$requirements$hierarchy)) {
    review$reconciliation <- paste(
      "The rule produces base forecasts independently at every hierarchy node, including aggregates.",
      "Normal FinnTS reconciliation may adjust these numbers before bottom-level publication.", "Forecast execution requires an explicitly selected shared mixed custom/FinnTS pool."
    )
  }
  review
}

# Validate current draft identity, protocol, policy, contract, diagnostics and exact approval receipts.
# Recompute passive hashes and report summaries; no package discovery or candidate code executes.
# Unknown formats, missing required fields, altered contexts or stale consent fail rather than migrate.
validate_custom_model_draft <- function(state) {
  fields <- c(
    "schema_version", "draft_id", "revision", "status", "stage", "settings", "metadata", "context", "environment",
    "answers", "contract", "contract_id", "definition", "report", "checks", "consent", "attempts", "diagnostic", "pending",
    "receipts", "result", "store", "state_digest", "diagnostics"
  )
  if (typeof(state) != "list" || !identical(attr(state, "class"), "finnts_custom_model_draft") || length(state) != length(fields) ||
    anyDuplicated(names(state)) || !setequal(names(state), fields) || any(!names(attributes(state)) %in% c("names", "class"))) {
    custom_author_abort("draft", "Invalid draft schema or identity.")
  }
  custom_draft_passive(unclass(state))
  validate_custom_draft_diagnostics(state$diagnostics)
  if (!identical(state$schema_version, 2L))
    custom_author_abort("draft", "Draft uses an obsolete approval workflow; start a new draft.")
  if (!identical(state$state_digest, custom_draft_digest(state)) || !custom_author_text(state$draft_id) || !grepl(
    "^[a-f0-9]{64}$",
    state$draft_id
  ) || !is.integer(state$revision) || length(state$revision) != 1L || state$revision < 1L || length(state$status) !=
    1L || !state$status %in% c("running", "review_required", "approved") || length(state$stage) != 1L || !state$stage %in%
    c("contract", "clarify", "source", "review_create", "validation", "approved")) {
    custom_author_abort("draft", "Invalid draft lifecycle identity.")
  }
  custom_author_fields(
    state$settings[setdiff(names(state$settings), "validation_examples")], c(
      "instructions", "name",
      "model_type", "max_attempts", "validation_timeout", "approval_mode", if ("automatic_policy" %in% names(state$settings)) "automatic_policy",
      "authoring_protocol", "required_properties", if ("forecast_approach" %in% names(state$settings)) "forecast_approach"
    ),
    "draft"
  )
  protocol <- state$settings$authoring_protocol
  {
    validate_custom_author_protocol(protocol, state$metadata, state$settings$model_type, !is.null(state$settings$validation_examples))
    if (!is.null(state$contract) && (!identical(state$contract$authoring_protocol, protocol$version) || !identical(
      state$contract$package_policy,
      protocol$package_policy
    ) || !identical(state$contract$example_scaffold, protocol$scaffold) || !identical(
      state$contract$requirements$predictors,
      protocol$predictors
    ) || (!identical(state$contract$requirements$missing_data, protocol$missing_data)))) {
      custom_author_abort("protocol", "Contract differs from frozen authoring protocol.")
    }
    if (!is.null(state$contract)) {
      history <- custom_author_history_requirements(state$contract$history_requirements)
      if (!identical(history, state$contract$history_requirements))
        custom_author_abort("protocol", "History requirements changed.")
      custom_author_check_history(state$contract$examples, history, state$metadata$date_type)
      parameters <- custom_author_typed_parameters(state$contract$fixed_parameters, state$contract$parameter_schema)
      behavior <- custom_author_behavior_contract(
        state$contract$error_policy, state$contract$validation_properties,
        state$contract$examples, state$contract$model_type
      )
      provenance <- if (is.null(state$settings$validation_examples))
        "llm_proposed_unverified"
      else "caller_supplied_unverified"
      if (!identical(parameters$values, state$contract$fixed_parameters) || !identical(parameters$schema, state$contract$parameter_schema) ||
        !identical(behavior$error_policy, state$contract$error_policy) || !identical(
        behavior$validation_properties,
        state$contract$validation_properties
      ) || !identical(state$contract$expectation_provenance, provenance))
        custom_author_abort("protocol", "Typed validation contract changed.")
    }
    required <- state$settings$required_properties
    if (!is.character(required) || anyNA(required) || anyDuplicated(required) || !identical(required, sort(required)) ||
      !all(required %in% c("constant_forecast", "target_scale_equivariant", "independent_series", "fixed_window", "relative_calendar"))) {
      custom_author_abort("protocol", "Caller-required properties changed.")
    }
    if (!is.null(state$contract) && (!identical(state$contract$required_properties, required) || !all(required %in% state$contract$validation_properties) ||
      !identical(state$contract$requirements$runtime$version, 2L))) {
      custom_author_abort("protocol", "Property authority or runtime contract changed.")
    }
    if (!is.null(state$contract))
      custom_model_runtime(state$contract$requirements$runtime, state$contract$requirements$predictors, state$contract$model_type)
  }
  policy <- state$settings$approval_mode
  if (!custom_author_text(policy) || !policy %in% c("manual", "automatic"))
    custom_author_abort("draft", "Invalid saved approval policy.")
  if (policy == "automatic" && (!is.list(state$settings$automatic_policy) || !identical(
    state$settings$automatic_policy$catalog_version,
    2L
  ) || !identical(state$settings$automatic_policy$catalog, custom_author_defaults()))) {
    custom_author_abort("draft", "Automatic defaults policy changed; start a new draft.")
  }
  if (policy == "manual" && !is.null(state$settings$automatic_policy))
    custom_author_abort("draft", "Manual drafts cannot carry automatic policy.")
  if (!identical(state$settings$forecast_approach, state$metadata$forecast_approach)) {
    custom_author_abort("draft", "Draft hierarchy approach changed.")
  }
  custom_author_fields(state$attempts, c("contract", "source"), "draft")
  custom_author_fields(state$consent, c("intent", "test"), "draft")
  if (!is.integer(state$settings$max_attempts) || length(state$settings$max_attempts) != 1L || !state$settings$max_attempts %in%
    1:3 || !is.numeric(state$settings$validation_timeout) || length(state$settings$validation_timeout) != 1L || state$settings$validation_timeout <=
    0 || state$settings$validation_timeout > 120 || any(!vapply(state$attempts, function(count) is.integer(count) &&
    length(count) == 1L && count >= 0L && count <= state$settings$max_attempts, logical(1)))) {
    custom_author_abort("draft", "Invalid draft attempt limits.")
  }
  if (!is.null(state$definition))
    custom_model_current_definition(state$definition)
  if (!is.null(state$contract) && !identical(state$contract_id, digest::digest(state$contract, algo = "sha256", serializeVersion = 2))) {
    custom_author_abort("draft", "Confirmed contract changed.")
  }
  if (policy == "automatic" && !is.null(state$contract)) {
    saved_policy <- state$settings$automatic_policy
    assumptions <- custom_author_assumptions(
      list(defaults_used = setdiff(
        names(state$contract$automatic_defaults$defaults_used),
        saved_policy$defaulted
      ), model_type = state$contract$model_type, predictors = state$contract$requirements$predictors),
      saved_policy
    )
    if (!identical(assumptions, state$contract$automatic_defaults) || !identical(saved_policy$effective$model_type, state$settings$model_type) ||
      !identical(saved_policy$effective$max_attempts, state$settings$max_attempts) || !identical(
      saved_policy$effective$validation_timeout,
      state$settings$validation_timeout
    )) {
      custom_author_abort("draft", "Automatic assumptions or effective settings changed.")
    }
  }
  if (!is.null(state$definition)) {
    source <- state$definition$source
    expected <- custom_author_definition(state$contract, list(source = lapply(names(source), function(name) list(
      name = name,
      code = source[[name]]
    ))), assembled = TRUE)
    if (!identical(expected$version_id, state$definition$version_id))
      custom_author_abort("draft", "Candidate differs from confirmed intent.")
  }
  for (receipt in state$receipts) {
    custom_author_fields(receipt, c("response_digest", "state_digest", "stage", "identity", "contract_id"), "draft")
  }
  if (!is.null(state$consent$intent) || !is.null(state$consent$test)) {
    if (is.null(state$definition) || !identical(state$consent$intent, state$contract_id) || !identical(
      state$consent$test,
      state$definition$version_id
    ))
      custom_author_abort("draft", "Missing exact candidate consent.")
    valid <- vapply(names(state$receipts), function(request_id) {
      receipt <- state$receipts[[request_id]]
      authorization <- if (policy == "automatic")
        list(request_id = request_id, authorization = "automatic", contract_id = state$contract_id)
      else list(request_id = request_id, answer = "yes")
      identical(receipt$stage, if (policy == "automatic")
        "automatic_authorization"
      else "review_create") && identical(receipt$identity, state$definition$version_id) && identical(
        receipt$contract_id,
        state$contract_id
      ) && identical(receipt$response_digest, digest::digest(authorization, algo = "sha256", serializeVersion = 2))
    }, logical(1))
    if (!any(valid))
      custom_author_abort("draft", "Missing exact policy-bound authorization receipt.")
  }
  if (state$stage %in% c("source", "review_create", "validation", "approved") && is.null(state$contract_id))
    custom_author_abort("draft", "Missing fixed interpretation.")
  if (state$stage %in% c("validation", "approved") && (is.null(state$definition) || !identical(state$consent$intent, state$contract_id) ||
    !identical(state$consent$test, state$definition$version_id)))
    custom_author_abort("draft", "Missing source-specific consent.")
  if (state$stage == "approved") {
    if (!identical(state$report$examples_id, digest::digest(state$contract$examples, algo = "sha256", serializeVersion = 2))) {
      custom_author_abort("draft", "Confirmed validation examples changed.")
    }
    checks <- custom_author_checks(state$report, state$definition, state$contract_id, state$contract$examples, state$metadata,
      policy, state$contract$automatic_defaults,
      contract = state$contract
    )
    if (!identical(checks, state$checks))
      custom_author_abort("draft", "Validation evidence changed.")
  }
  if (!is.null(state$pending)) {
    request <- state$pending
    if (!setequal(names(request), c("request_id", "stage", "prompt", "identity", "review", "questions")) || !identical(
      request$stage,
      state$stage
    ) || !identical(state$status, "review_required") || !custom_author_text(request$request_id)) {
      custom_author_abort("draft", "Invalid pending review request.")
    }
    expected <- switch(state$stage,
      review_create = "Create model? [y/N/details/edit]",
      clarify = "Clarify the business rule."
    )
    if (!identical(request$prompt, expected))
      custom_author_abort("draft", "Pending review prompt changed.")
    if (!identical(request$request_id, custom_draft_request_id(state)))
      custom_author_abort("draft", "Pending request identity changed.")
    if (state$stage == "review_create" && (!identical(request$identity, state$definition$version_id) || !identical(
      request$review,
      custom_draft_review(state$contract, state$definition, state$metadata, if ("repair" %in% names(request$review))
        state$diagnostics
      else NULL)
    ))) {
      custom_author_abort("draft", "Review content changed.")
    }
  } else if (state$status == "review_required" && !state$stage %in% c("contract", "source"))
    custom_author_abort("draft", "Missing pending review.")
  if (!identical(state$status == "approved", !is.null(state$result)) || (!is.null(state$result) && (state$stage != "approved" ||
    !is.null(state$pending) || !identical(state$result$definition, state$definition) || !identical(
    state$result$validation$checks,
    state$checks
  )))) {
    custom_author_abort("draft", "Draft status does not grant approval.")
  }
  if (!is.null(state$result)) {
    custom_run_envelope(state$result)
    if (!identical(state$result$approval$mode %||% "manual", policy))
      custom_author_abort("draft", "Result approval policy changed.")
  }
  state
}

# Resolve the exact latest revision from a trusted draft object or RDS path.
# An old object is only usable for a previously accepted identical response;
# callers cannot use stale snapshots to roll back current consent or counters.
custom_draft_load <- function(draft) {
  supplied <- if (custom_author_text(draft))
    custom_draft_read(draft)
  else draft
  pointer <- is.list(supplied) && !inherits(supplied, "finnts_custom_model_draft")
  store <- if (pointer && custom_author_text(draft))
    dirname(normalizePath(draft, winslash = "/", mustWork = TRUE))
  else validate_custom_model_draft(supplied)$store
  if (!custom_author_text(store))
    custom_author_abort("draft", "A deferred draft requires its saved context directory.")
  current <- custom_draft_read(file.path(store, "current.rds"))
  custom_author_fields(current, c("schema_version", "draft_id", "revision", "state_digest", "filename"), "draft")
  if (!identical(current$schema_version, 2L))
    custom_author_abort("draft", "Draft uses an obsolete approval workflow; start a new draft.")
  expected <- paste0(current$revision, "-", current$state_digest, ".rds")
  if (!is.integer(current$revision) || length(current$revision) != 1L || !custom_author_text(current$state_digest) || !grepl(
    "^[a-f0-9]{64}$",
    current$state_digest
  ) || !identical(current$filename, expected) || grepl("[/\\\\]", expected))
    custom_author_abort("draft", "Invalid draft pointer.")
  state <- validate_custom_model_draft(custom_draft_read(file.path(store, expected)))
  if (!identical(state$draft_id, current$draft_id) || !identical(state$revision, current$revision) || !identical(
    state$state_digest,
    current$state_digest
  ) || !identical(state$store, store))
    custom_author_abort("draft", "Draft pointer mismatch.")
  if (!pointer && !identical(state$draft_id, supplied$draft_id))
    custom_author_abort("draft", "Draft ownership changed.")
  list(state = state, supplied = if (pointer) state else supplied)
}

# Verify saved data, environment and execution contract before reusing consent.
# Obsolete isolation consent requires a new draft. Read only deterministic data
# paths and snapshot bytes, never rediscover or reprepare.
custom_draft_data <- function(state) {
  if (!is.list(state$environment) || !identical(state$environment$validation_execution, "current_session")) {
    custom_author_abort("draft", "Draft uses an obsolete validation execution contract; start a new draft.")
  }
  context <- state$context
  if (!is.list(context) || !setequal(names(context), c("snapshot", "hash", "sources", "durable")) || !identical(
    context$snapshot,
    file.path(state$store, "context.rds")
  ) || !file.exists(context$snapshot) || !identical(digest::digest(
    file = context$snapshot,
    algo = "sha256"
  ), context$hash)) {
    custom_author_abort("draft", "Draft context is missing or changed; restart with a durable draft_path.")
  }
  for (path in names(context$sources)) {
    if (!file.exists(path) || !identical(digest::digest(file = path, algo = "sha256"), context$sources[[path]])) {
      custom_author_abort("draft", "Prepared source context changed; start a new draft.")
    }
  }
  if (!identical(state$environment, custom_draft_environment(state$metadata)))
    custom_author_abort("draft", "Draft runtime/package environment changed.")
  data <- custom_draft_read(context$snapshot)
  custom_draft_passive(data)
  if (!is.list(data) || !setequal(names(data), c("data", "metadata")) || !is.data.frame(data$data) || !nrow(data$data) ||
    nrow(data$data) > 10000L || !all(c("Date", "Combo", "Target") %in% names(data$data)) || !is.numeric(data$data$Target) ||
    any(!is.finite(data$data$Target)))
    custom_author_abort("draft", "Invalid validation snapshot.")
  if (!identical(data$metadata, state$metadata))
    custom_author_abort("draft", "Draft metadata changed.")
  data
}

# Normalize one revision-bound decision and persist a combined consent receipt
# before execution. Details is read-only; edits clear consent and retain budgets.
# A running checkpoint after interruption is never auto-retried.
custom_draft_answer <- function(state, responses) {
  custom_author_fields(responses, c("request_id", "answer"), "draft")
  if (!custom_author_text(responses$request_id))
    custom_author_abort("draft", "request_id must be plain text.")
  request <- state$pending
  if (is.null(request) || !identical(responses$request_id, request$request_id))
    custom_author_abort("draft", "Stale or incorrect request_id.")
  answer <- custom_author_response(request, responses$answer)
  if (identical(answer, "details")) {
    custom_author_review(request$review, details = TRUE)
    return(state)
  }
  editing <- is.list(answer) && identical(answer$action, "edit")
  if (editing && (state$attempts$contract >= state$settings$max_attempts || state$attempts$source >= state$settings$max_attempts)) {
    custom_author_abort("draft", "Edit exceeds max_attempts; start a new draft.")
  }
  normalized <- list(request_id = request$request_id, answer = answer)
  state$receipts[[request$request_id]] <- list(
    response_digest = digest::digest(normalized, algo = "sha256", serializeVersion = 2),
    state_digest = state$state_digest, stage = request$stage, identity = request$identity, contract_id = state$contract_id
  )
  state["pending"] <- list(NULL)
  state$status <- "running"
  if (editing) {
    state$settings$instructions <- paste(state$settings$instructions, "Requested change:", answer$instructions, sep = "\n")
    state[c("definition", "report", "checks", "result")] <- list(NULL, NULL, NULL, NULL)
    state$consent[c("intent", "test")] <- list(NULL, NULL)
    state$diagnostic <- "user_edit"
    state$stage <- "contract"
  } else if (request$stage == "clarify") {
    state$answers[[length(state$answers) + 1L]] <- answer
    state$stage <- "contract"
  } else if (request$stage == "review_create") {
    state$consent$intent <- state$contract_id
    state$consent$test <- state$definition$version_id
    state$stage <- "validation"
  } else custom_author_abort("draft", "Invalid human gate.")
  custom_draft_save(state)
}

# Continue a current draft using its saved inputs, policy and cumulative attempts.
# Validate context before reusing consent; only exact response replay is idempotent. New proposals require a session.
# Package permissions cannot be broadened. Persistence and generated execution follow the existing lifecycle gates.
custom_draft_resume <- function(draft, responses, llm, deferred, interact, approval = NULL, package_policy = NULL) {
  loaded <- custom_draft_load(draft)
  state <- loaded$state
  policy <- state$settings$approval_mode
  if (!is.null(package_policy) && !identical(package_policy, state$settings$authoring_protocol$package_policy$mode)) {
    custom_author_abort("draft", "Package policy cannot change on resume; start a new draft.")
  }
  if (!is.null(approval) && !identical(approval, policy))
    custom_author_abort("draft", "Approval policy cannot change on resume; start a new draft.")
  if (policy == "automatic" && (!is.null(interact) || !is.null(responses)))
    custom_author_abort("draft", "Automatic approval cannot use interact or manual responses.")
  if (policy == "automatic")
    deferred <- FALSE
  data <- custom_draft_data(state)
  if (identical(state$status, "running"))
    custom_author_abort("draft", "Interrupted authoring action; restore reviewed state or start a new draft.")
  if (!is.null(responses)) {
    custom_author_fields(responses, c("request_id", "answer"), "draft")
    if (!custom_author_text(responses$request_id))
      custom_author_abort("draft", "request_id must be plain text.")
    receipt <- state$receipts[[responses$request_id]]
    if (!is.null(receipt)) {
      normalized <- list(request_id = responses$request_id, answer = tryCatch(custom_author_response(list(
        stage = receipt$stage,
        questions = if (receipt$stage == "clarify") responses$answer else NULL
      ), responses$answer), finnts_custom_model_authoring_error = function(error) custom_author_abort(
        "draft",
        "Previously accepted response changed."
      )))
      if (!identical(receipt$response_digest, digest::digest(normalized, algo = "sha256", serializeVersion = 2)))
        custom_author_abort("draft", "Previously accepted response changed.")
      return(if (is.null(state$result)) state else state$result)
    }
  }
  if (!identical(state$state_digest, loaded$supplied$state_digest))
    custom_author_abort("draft", "Stale draft revision; reload current.rds.")
  if (!is.null(responses) && !is.null(state$pending) && identical(responses$request_id, state$pending$request_id) && identical(custom_author_response(
    state$pending,
    responses$answer
  ), "details")) {
    custom_author_review(state$pending$review, details = TRUE)
    return(state)
  }
  needs_provider <- state$stage %in% c("clarify", "contract", "source") && (!is.null(responses) || is.null(state$pending) ||
    !deferred)
  if (needs_provider && is.null(llm))
    custom_author_abort("draft", "Supply llm for the next authoring proposal.")
  session <- NULL
  if (needs_provider) {
    check_agent_ellmer_version()
    session <- new_llm_session(llm)
  }
  if (!is.null(responses))
    state <- custom_draft_answer(state, responses)
  custom_draft_drive(state, data, session, llm, deferred, interact)
}

# Persist automatic authorization of one exact candidate under the saved caller
# policy. This is not a human response; the record precedes every execution and
# is bound to the pending revision, fixed contract and generated source version.
custom_draft_authorize <- function(state) {
  if (!identical(state$settings$approval_mode, "automatic") || is.null(state$pending) || !identical(
    state$pending$stage,
    "review_create"
  ))
    custom_author_abort("draft", "Missing explicit automatic authorization.")
  request <- state$pending
  authorization <- list(request_id = request$request_id, authorization = "automatic", contract_id = state$contract_id)
  state$receipts[[request$request_id]] <- list(
    response_digest = digest::digest(authorization, algo = "sha256", serializeVersion = 2),
    state_digest = state$state_digest, stage = "automatic_authorization", identity = state$definition$version_id, contract_id = state$contract_id
  )
  state$consent <- list(intent = state$contract_id, test = state$definition$version_id)
  state["pending"] <- list(NULL)
  state$status <- "running"
  state$stage <- "validation"
  custom_draft_save(state)
}

# Build a bounded, passive UX report from trusted lifecycle state or a condition.
# This is advisory feedback, never an approval receipt or automatic retry grant.
# Provider/condition text, real rows, source, callbacks and credentials are not
# copied. Questions are capped provider-visible business questions; callbacks
# only sanitize questions and derive nonnegative remaining counts. Closed next
# actions distinguish needed consent, new business input and exhausted budgets.
# Preflight conditions retain only known stage labels; absent state/model means
# approval policy is unknown. Manual candidate review requires user input too.
custom_author_feedback <- function(state = NULL, error = NULL, model = NULL) {
  approved <- is.null(error) && (identical(state$status, "approved") || !is.null(model))
  failure <- if (is.null(error) || !length(state$diagnostics$history))
    NULL
  else tail(state$diagnostics$history, 1L)[[1L]]$failure
  code <- if (approved)
    "approved"
  else if (is.null(error)) {
    if (identical(state$stage, "clarify"))
      "business_clarification"
    else "review_required"
  } else error$code
  known <- names(custom_author_failure_messages())
  if (!custom_author_text(code) || !code %in% c(known, "approved", "initial", "review_required", "no_repair_progress")) {
    code <- if (!is.null(error) && !inherits(error, "finnts_custom_model_authoring_error"))
      "operation_failed"
    else "invalid_input"
  }
  needs_input <- identical(state$stage, "clarify") || identical(code, "business_clarification")
  status <- if (approved)
    "approved"
  else if (needs_input)
    "needs_input"
  else if (is.null(error))
    "review_required"
  else "failed"
  stage <- if (approved)
    "approved"
  else if (!is.null(failure) && identical(code, failure$reason))
    failure$phase
  else state$stage %||% "input"
  if (!is.null(error) && is.null(state)) {
    stages <- c(
      "input", "interaction", "draft", "examples", "environment", "validation", "contract", "source", "proposal",
      "clarification", "diagnostics", "approval", "review_create", "provider"
    )
    stage <- if (custom_author_text(error$stage) && error$stage %in% stages)
      error$stage
    else "unknown"
  }
  policy <- if (is.null(state) && is.null(model))
    NULL
  else state$settings$approval_mode %||% model$approval$mode
  used <- state$attempts %||% list(contract = NULL, source = NULL)
  limit <- state$settings$max_attempts
  remaining <- if (is.null(limit))
    list(contract = NULL, source = NULL)
  else lapply(used, function(count) as.integer(max(0L, limit - count)))
  next_action <- if (approved)
    "use_model"
  else if (needs_input)
    "ask_user"
  else if (status == "review_required") {
    if (identical(state$stage, "review_create"))
      "review_candidate"
    else "supply_llm"
  } else if (code %in% c("operation_failed", "provider_failure"))
    "inspect_operation"
  else if (code == "invalid_input")
    "correct_inputs"
  else "inspect_failure"
  messages <- custom_author_failure_messages()
  message <- if (approved)
    "Model passed its declared checks; arbitrary business-rule correctness is not guaranteed."
  else if (needs_input)
    "Business clarification is required; do not invent the answer."
  else if (status == "review_required")
    "Review the current request; only explicit authorization permits candidate execution."
  else if (code %in% names(messages))
    messages[[code]]
  else "The operation did not produce an approved model. Inspect the condition and supplied inputs before taking further action."
  questions <- state$pending$questions
  if (is.null(questions) && needs_input) {
    proposal <- state$diagnostics$rejected_contract$proposal$questions
    questions <- tryCatch(custom_author_questions(proposal)$business, error = function(error) character())
    if (!length(questions) && is.character(proposal))
      questions <- proposal
  }
  questions <- lapply(head(as.list(questions), 8L), function(question) {
    if (!custom_author_text(question))
      return("Review the pending business question.")
    substr(trimws(gsub("[[:cntrl:]]", " ", question)), 1L, 500L)
  })
  if (length(questions) && is.null(names(questions)))
    names(questions) <- paste0("question_", seq_along(questions))
  details <- list()
  if (identical(code, "provider_failure")) {
    stage <- "provider"
    if (!is.null(error$provider))
      details$provider <- validate_custom_author_provider(error$provider)
  }
  if (!is.null(failure) && identical(code, failure$reason)) {
    failure <- validate_custom_author_failure(failure, state$definition)
    details <- failure[intersect(c(
      "example_index", "model_type", "unbound_symbol", "property", "dependency", "bindings",
      "rule_error", "comparisons"
    ), names(failure))]
  }
  resolved <- state$metadata$resolved_inputs %||% list()
  if (!is.null(model$validation$checks$resolved_inputs))
    resolved <- jsonlite::fromJSON(model$validation$checks$resolved_inputs)
  report <- list(
    schema_version = 1L, status = status, stage = stage, code = code, message = message, next_action = next_action,
    needs_user_input = needs_input || identical(next_action, "review_candidate"), approval_mode = policy, version_id = state$definition$version_id %||%
      model$definition$version_id %||% failure$version_id, contract_id = state$contract_id, draft_id = state$draft_id,
    revision = state$revision, request_id = state$pending$request_id, attempts = list(
      used = used, limit_per_phase = limit,
      remaining = remaining
    ), questions = questions, questions_provenance = if (length(questions)) "provider_proposed" else NULL,
    details = details, resolved_inputs = resolved, validation = list(technical_passed = approved, expectation_provenance = state$contract$expectation_provenance %||%
      model$validation$checks$expectation_provenance, checks = model$validation$checks %||% list(), business_correctness_guaranteed = FALSE),
    artifacts = list(draft_reference = if (is.null(state$store)) NULL else file.path(state$store, "current.rds"))
  )
  custom_run_passive(report)
  report
}

# Attach a package-built passive report without changing condition classes or
# replacing existing raw diagnostics. A local content fingerprint distinguishes
# these reports from foreign error$feedback fields and detects accidental edits;
# it is not authentication or authorization. Trusted lifecycle state overrides
# any prior report. Unknown or altered reports are rebuilt without private text.
custom_author_error_feedback <- function(error, state = NULL) {
  fingerprint <- attr(error, "finnts_feedback_id", exact = TRUE)
  owned <- custom_author_text(fingerprint) && grepl("^[a-f0-9]{64}$", fingerprint)
  if (!is.null(state) || !owned || !identical(fingerprint, digest::digest(error$feedback, algo = "sha256", serializeVersion = 2))) {
    error$feedback <- custom_author_feedback(state = state, error = error)
    attr(error, "finnts_feedback_id") <- digest::digest(error$feedback, algo = "sha256", serializeVersion = 2)
  }
  error
}

#' Read Custom Model Creation Feedback
#'
#' Obtain a machine-readable status report without generating code, granting
#' consent, loading training data or writing files. Error conditions keep their
#' ordinary R error semantics; pending drafts cannot be enrolled as models.
#' @param x An approved custom model, pending draft, or captured error condition.
#' @return A passive version-1 list with status, stage/code, next action, bounded
#'   questions/details, candidate/request identities and remaining phase budgets.
#'   NULL counts mean unavailable historical evidence, not unlimited attempts.
#' @details Status is `approved`, `review_required`, `needs_input` or `failed`.
#'   Questions are provider proposals, not verified input errors. Remaining counts
#'   do not authorize retries or execution; use the existing pending draft and
#'   exact `request_id` for continuation. Approved envelopes do not retain attempt
#'   histories, so those counts are NULL. Older models may lack resolved inputs.
#'   The accessor validates model/draft integrity and can reject altered records.
#'   Authoring errors carry this report in `feedback`; other captured errors
#'   receive a generic report. Foreign or altered attached feedback is rebuilt
#'   without mutating the supplied condition. Unknown stage/policy is not invented.
#'   `needs_user_input` covers business questions and manual candidate review.
#'   Catching an error never converts it into an enrollable model. Private raw
#'   rows, source and arbitrary exception text are not included. Column names,
#'   provider-visible questions and explicitly chosen artifact paths may still
#'   be confidential. There is no automatic upload, retry or approval.
#' @seealso [create_custom_model()]
#' @examples
#' failure <- tryCatch(create_custom_model(), error = identity)
#' custom_model_feedback(failure)
#' @export
custom_model_feedback <- function(x) {
  if (inherits(x, "finnts_custom_model")) {
    return(custom_author_feedback(model = custom_run_envelope(x)))
  }
  if (inherits(x, "finnts_custom_model_draft"))
    return(custom_author_feedback(state = validate_custom_model_draft(x)))
  if (inherits(x, "error")) {
    return(custom_author_error_feedback(x)$feedback)
  }
  custom_author_abort("input", "Feedback requires a custom model, pending draft or captured error.")
}

# Drive the current contract, clarification, source, review and validation lifecycle with bounded attempts.
# Caller examples and accepted contracts stay fixed; candidate repairs require exact renewed authorization.
# Only recognized proposal/source defects consume the existing budgets. Operational failures and unchanged failed source stop.
# Persist transitions and bounded diagnostics before returning an approved envelope, a pending draft, or a typed error.
# Transport and approved generated R have side effects; this workflow is not a sandbox or a hard timeout.
custom_draft_drive <- function(state, data, session, llm, deferred, interact) {
  tryCatch(repeat {
    automatic <- identical(state$settings$approval_mode, "automatic")
    if (!is.null(state$environment) && !identical(state$environment, custom_draft_environment(state$metadata))) {
      custom_author_abort("draft", "Draft runtime/package environment changed; start a new draft.")
    }
    if (!is.null(state$result))
      return(custom_run_envelope(state$result))
    if (!is.null(state$pending)) {
      if (automatic) {
        if (state$pending$stage != "review_create")
          custom_author_abort("clarification", "Automatic mode needs more explicit business context; no candidate was executed.")
        state <- custom_draft_authorize(state)
      } else {
        if (deferred)
          return(state)
        request <- state$pending
        answer <- custom_author_interact(
          interact, request$stage, request$prompt, request$identity, request$review,
          request$questions
        )
        if (!is.null(state$environment) && !identical(state$environment, custom_draft_environment(state$metadata))) {
          custom_author_abort("draft", "Draft runtime/package environment changed; start a new draft.")
        }
        state <- custom_draft_answer(state, list(request_id = request$request_id, answer = answer))
      }
    }
    settings <- state$settings
    if (state$stage %in% c("contract", "source")) {
      stage <- state$stage
      if (state$attempts[[stage]] >= settings$max_attempts) {
        reason <- "No candidate passed required checks within max_attempts."
        feedback <- if (stage == "contract")
          custom_author_contract_feedback(state$diagnostic)
        else NULL
        if (!is.null(feedback)) {
          reason <- paste(reason, "Last contract rejection:", feedback$message)
          if (!is.null(state$diagnostics$rejected_contract)) {
            reason <- paste(reason, "Field:", state$diagnostics$rejected_contract$field)
          }
          if (identical(state$diagnostic, "insufficient_example_history")) {
            history <- custom_author_history_feedback(
              state$diagnostics$rejected_contract$proposal, settings$authoring_protocol,
              settings$validation_examples, state$metadata
            )
            if (!is.null(history) && !history$observed_history_possible) {
              reason <- paste(reason, "Forecast-relative lags reaching unobserved periods:", paste(head(
                history$unobserved_lag_periods,
                8L
              ), collapse = ", "), if (length(history$unobserved_lag_periods) > 8L)
                "(additional lags omitted)."
              else ".", history$guidance)
            }
          }
        }
        if (stage == "source" && length(state$diagnostics$history)) {
          failure <- tail(state$diagnostics$history, 1L)[[1]]$failure
          if (!is.null(failure)) {
            reason <- paste(
              reason, "Last failure:", failure$phase, "-", failure$message, failure$source_message %||%
                "", custom_author_dependency_summary(failure$dependency), custom_author_binding_summary(failure$bindings),
              if (!is.null(failure$unbound_symbol))
                paste("Missing variable:", validate_custom_author_unbound_symbol(failure$unbound_symbol))
              else ""
            )
            feedback <- list(code = failure$reason)
          }
        }
        custom_author_abort(if (stage == "contract")
          "clarification"
        else "validation", reason, code = feedback$code)
      }
      if (is.null(session)) {
        if (is.null(llm)) {
          if (automatic)
            custom_author_abort("provider", "Supply llm for automatic authoring or repair.")
          state$status <- "review_required"
          return(custom_draft_save(state))
        }
        check_agent_ellmer_version()
        session <- new_llm_session(llm)
      }
      state$status <- "running"
      state$attempts[[stage]] <- state$attempts[[stage]] + 1L
      state <- custom_draft_save(state)
      metadata <- data$metadata
      metadata$forecast_approach <- settings$forecast_approach %||% "bottoms_up"
      if (stage == "contract") {
        inputs <- c("Target", custom_model_predictor_columns(settings$authoring_protocol$predictors))
        metadata$schema <- metadata$schema[names(metadata$schema) %in% inputs]
      }
      payload <- if (stage == "contract")
        list(
          instructions = settings$instructions, metadata = metadata, name = settings$name, model_type = settings$model_type,
          answers = state$answers
        )
      else list(contract = state$contract, diagnostic = state$diagnostic)
      if (stage == "contract" && !is.null(settings$validation_examples))
        payload$validation_examples <- settings$validation_examples
      payload$approval_mode <- settings$approval_mode
      payload$required_properties <- settings$required_properties
      payload$protocol <- settings$authoring_protocol
      if (automatic)
        payload$automatic_policy <- settings$automatic_policy
      if (stage == "contract" && !state$diagnostic %in% c("initial", "user_edit")) {
        payload$feedback <- custom_author_contract_feedback(state$diagnostic)
        if (!is.null(state$diagnostics$rejected_contract)) {
          payload$feedback$field <- state$diagnostics$rejected_contract$field
        }
        rejected <- state$diagnostics$rejected_contract$proposal
        if (identical(state$diagnostic, "protocol_clarification"))
          payload$feedback$facts <- custom_author_questions(rejected$questions)$facts
        if (identical(state$diagnostic, "invalid_parameter_schema")) {
          payload$feedback$parameter_issue <- tryCatch(
            {
              custom_author_typed_parameters(custom_author_json(rejected$parameters_json), rejected$parameter_schema)
              NULL
            },
            error = function(error) error$parameter_issue
          )
        }
        if (identical(state$diagnostic, "insufficient_example_history")) {
          payload$feedback$history <- custom_author_history_feedback(
            state$diagnostics$rejected_contract$proposal,
            settings$authoring_protocol, settings$validation_examples, data$metadata
          )
        }
      }
      if (stage == "source" && length(state$diagnostics$history)) {
        failure <- tail(state$diagnostics$history, 1L)[[1]]$failure
        if (!is.null(failure)) {
          payload$failure <- validate_custom_author_failure(failure)
          payload$previous_source <- state$diagnostics$snapshot$source
          payload$previous_version_id <- failure$version_id
          payload$contract_id <- state$contract_id
          payload$dependency_candidates <- custom_author_dependency_candidates(failure$dependency, settings$authoring_protocol$package_policy$available)
        }
      }
      proposal <- custom_author_request(session, stage, payload)
      if (stage == "contract") {
        classified <- NULL
        valid <- tryCatch(
          {
            custom_author_fields(proposal, custom_author_contract_fields(
              settings$authoring_protocol, settings$name,
              settings$model_type, automatic
            ), "contract")
            classified <- custom_author_questions(proposal$questions)
            TRUE
          },
          error = function(error) FALSE
        )
        if (!valid) {
          state$diagnostic <- "invalid_contract_response"
          state <- custom_draft_record(state, "rejected", custom_author_failure(NULL, "contract", state$diagnostic),
            proposal = proposal
          )
          state <- custom_draft_save(state)
          next
        }
        if (!is.null(classified) && !length(classified$business) && length(classified$facts)) {
          state$diagnostic <- "protocol_clarification"
          state <- custom_draft_record(state, "rejected", custom_author_failure(NULL, "contract", state$diagnostic),
            proposal = proposal, field = "questions"
          )
          state <- custom_draft_save(state)
          next
        }
        questions <- if (is.null(classified))
          proposal$questions
        else classified$business
        if (length(questions)) {
          if (automatic) {
            state$diagnostic <- "business_clarification"
            state <- custom_draft_record(state, "rejected", custom_author_failure(NULL, "contract", state$diagnostic),
              proposal = proposal
            )
            state <- custom_draft_save(state)
            questions <- vapply(as.list(questions), function(question) {
              text <- trimws(gsub("[[:cntrl:]]", " ", question))
              if (nchar(text) > 500L)
                paste0(substr(text, 1L, 500L), "... [truncated]")
              else text
            }, character(1))
            custom_author_abort("clarification", paste(
              c(
                "Automatic mode needs more explicit business context; unresolved questions cannot be guessed.",
                "Provider clarification (not an independently verified input error):", paste0("- ", questions), "Full questions: diagnostics$rejected_contract$proposal$questions."
              ),
              collapse = "\n"
            ), code = "business_clarification", field = "questions")
          }
          state <- custom_draft_gate(state, "clarify", "Clarify the business rule.", questions = stats::setNames(
            as.list(questions),
            paste0("question_", seq_along(questions))
          ))
          next
        }
        assumptions <- NULL
        if (automatic) {
          assumptions_proposal <- proposal
          assumptions_proposal$model_type <- settings$model_type
          assumptions_proposal$predictors <- settings$authoring_protocol$predictors
          assumptions <- {
            tryCatch(custom_author_assumptions(assumptions_proposal, settings$automatic_policy), finnts_custom_model_authoring_error = identity)
          }
          if (inherits(assumptions, "finnts_custom_model_authoring_error")) {
            state$diagnostic <- "invalid_defaults_used"
            state <- custom_draft_record(state, "rejected", custom_author_failure(NULL, "contract", state$diagnostic),
              proposal = proposal
            )
            state <- custom_draft_save(state)
            next
          }
          proposal$defaults_used <- NULL
        }
        rejection_field <- NULL
        contract <- tryCatch(custom_author_contract(proposal, settings$instructions, data$metadata, settings$name,
          settings$model_type, settings$validation_examples, settings$authoring_protocol,
          required_properties = settings$required_properties %||%
            character()
        ), error = function(error) {
          if (!inherits(error, "finnts_custom_model_authoring_error")) {
            state <<- custom_draft_record(state, "rejected", custom_author_failure(NULL, "contract", "contract_validation_error"),
              proposal = proposal
            )
            state <<- custom_draft_save(state)
            custom_author_abort("environment", "Contract validation failed unexpectedly; no further provider attempt was performed.",
              code = "contract_validation_error"
            )
          }
          example_error <- inherits(error, "finnts_custom_model_authoring_error") && identical(error$stage, "examples")
          recognized <- if (inherits(error, "finnts_custom_model_authoring_error"))
            error$code
          else NULL
          feedback <- custom_author_contract_feedback(recognized %||% if (example_error)
            "invalid_examples"
          else "invalid_contract")
          rejection_field <<- if (inherits(error, "finnts_custom_model_authoring_error"))
            error$field
          else NULL
          retry_history <- identical(recognized, "insufficient_example_history")
          if (example_error && !is.null(settings$validation_examples) && !retry_history) {
            custom_author_abort("examples", paste(
              "Caller-provided validation_examples failed.", feedback$message,
              "Correct those examples and start a new call; they were not changed."
            ), code = feedback$code)
          }
          state$diagnostic <<- feedback$code
          NULL
        })
        if (is.null(contract)) {
          state <- custom_draft_record(state, "rejected", custom_author_failure(NULL, "contract", state$diagnostic),
            proposal = proposal, field = rejection_field
          )
          state <- custom_draft_save(state)
          next
        }
        if (automatic) {
          contract$automatic_defaults <- assumptions
          if (is.null(settings$validation_examples)) {
            contract$examples <- lapply(contract$examples, function(example) {
              example$tolerance <- 1e-08
              example
            })
          }
        }
        if (!is.null(state$contract) && (!setequal(contract$model_type, state$contract$model_type) || !setequal(
          contract$requirements$predictors,
          state$contract$requirements$predictors
        ))) {
          custom_author_abort("contract", "Edits cannot change model modes or predictors; start a new draft with explicit metadata.")
        }
        state$contract <- contract
        state$contract_id <- digest::digest(contract, algo = "sha256", serializeVersion = 2)
        state <- custom_draft_record(state, "accepted")
        if (!state$diagnostic %in% c("initial", "user_edit"))
          state$diagnostic <- "initial"
        state$stage <- "source"
        state <- custom_draft_save(state)
      } else {
        inspected <- tryCatch(custom_author_source_response(proposal), error = identity)
        if (inherits(inspected, "finnts_custom_model_authoring_error") && identical(inspected$code, "implementation_difficulty")) {
          state$diagnostic <- "implementation_difficulty"
          state <- custom_draft_record(state, "rejected", custom_author_failure(NULL, "source_contract", "implementation_difficulty"))
          state <- custom_draft_save(state)
          next
        }
        if (inherits(inspected, "finnts_custom_model_authoring_error") && identical(inspected$code, "contract_conflict")) {
          state$diagnostic <- "contract_conflict"
          state <- custom_draft_record(state, "rejected", custom_author_failure(NULL, "source_contract", "contract_conflict"))
          state <- custom_draft_save(state)
          stop(inspected)
        }
        if (inherits(inspected, "error")) {
          state$diagnostic <- "invalid_source_contract"
          state <- custom_draft_record(
            state, "rejected", custom_author_failure(NULL, "source_contract", "source_contract"),
            proposal
          )
          state <- custom_draft_save(state)
          next
        }
        rejection <- NULL
        definition <- tryCatch(custom_author_definition(state$contract, proposal), error = function(error) {
          code <- if (inherits(error, "finnts_custom_model_authoring_error"))
            error$code
          else NULL
          if (is.null(code) || !code %in% c(
            "source_syntax", "source_signature", "source_guard", "source_dependency",
            "source_binding", "source_body_format", "source_reserved_helper", "date_class_loss"
          ))
            code <- "source_contract"
          phase <- switch(code,
            source_syntax = "source_parse",
            source_signature = "source_signature",
            source_guard = "source_guard",
            source_dependency = "source_guard",
            source_binding = "source_signature",
            source_body_format = "source_signature",
            date_class_loss = "source_guard",
            "source_contract"
          )
          rejection <<- custom_author_failure(NULL, phase, code, dependency = error$dependency, bindings = error$bindings)
          NULL
        })
        if (is.null(definition)) {
          state$diagnostic <- "invalid_source_contract"
          state <- custom_draft_record(state, "rejected", rejection, proposal)
          state <- custom_draft_save(state)
          next
        }
        state$definition <- definition
        state <- custom_draft_record(state, "accepted", proposal = proposal)
        state <- custom_draft_gate(
          state, "review_create", "Create model? [y/N/details/edit]", definition$version_id,
          custom_draft_review(state$contract, definition, data$metadata, if (!automatic)
            state$diagnostics
          else NULL)
        )
      }
    } else if (state$stage == "validation") {
      definition <- custom_model_current_definition(state$definition)
      if (!identical(state$consent$intent, state$contract_id) || !identical(state$consent$test, definition$version_id))
        custom_author_abort("draft", "Missing exact execution consent.")
      arguments <- list(definition = definition, examples = state$contract$examples, data = data, timeout = settings$validation_timeout)
      arguments$package_policy <- settings$authoring_protocol$package_policy
      arguments$contract <- state$contract
      report <- do.call(custom_author_validate, arguments)
      custom_run_passive(report)
      if (!isTRUE(report$passed)) {
        if (!custom_author_text(report$code) || !report$code %in% c(
          "intent_mismatch", "candidate_validation_error",
          "validation_timeout", "insufficient_global_series", "insufficient_history", "incomplete_holdout"
        )) {
          custom_author_abort("environment", "Validation infrastructure or dependency failure.")
        }
        state$diagnostic <- report$code
        phase <- switch(report$code,
          intent_mismatch = "example_compare",
          validation_timeout = "validation_budget",
          insufficient_history = "holdout_data",
          insufficient_global_series = "holdout_data",
          incomplete_holdout = "holdout_data",
          "validation"
        )
        failure <- report$failure %||% custom_author_failure(definition, phase, if (report$code == "candidate_validation_error")
          "candidate_error"
        else report$code)
        validate_custom_author_failure(failure, definition)
        if (custom_author_llm_examples_advisory(state$contract) && (identical(report$code, "intent_mismatch") ||
          identical(failure$reason, "intent_mismatch"))) {
          custom_author_abort("environment", "Generated numerical expectations cannot request a source rewrite; validation returned an inconsistent report.",
            code = "comparison_policy_error"
          )
        }
        previous <- Filter(function(entry) identical(entry$contract_id, state$contract_id) && identical(
          entry$version_id,
          definition$version_id
        ) && identical(entry$outcome, "rejected") && !is.null(entry$failure) && identical(
          entry$failure$phase,
          failure$phase
        ) && identical(entry$failure$reason, failure$reason), state$diagnostics$history)
        state <- custom_draft_record(state, "rejected", failure)
        state[c("definition", "report", "checks")] <- list(NULL, NULL, NULL)
        state$consent[c("intent", "test")] <- list(NULL, NULL)
        state$stage <- "source"
        state <- custom_draft_save(state)
        if (identical(failure$reason, "input_type_mismatch")) {
          custom_author_abort("environment", paste(failure$message, "No further source repair was requested."), code = "input_type_mismatch")
        }
        if (length(previous)) {
          custom_author_abort("validation", "The identical candidate failed the same check again; no further provider attempt was performed. Fixed expectations were not changed.",
            code = "no_repair_progress"
          )
        }
        next
      }
      if (!identical(report$examples_id, digest::digest(state$contract$examples, algo = "sha256", serializeVersion = 2)))
        custom_author_abort("validation", "Measured examples do not match the confirmed contract.")
      state$checks <- custom_author_checks(report, definition, state$contract_id, state$contract$examples, data$metadata,
        state$settings$approval_mode, state$contract$automatic_defaults,
        contract = state$contract
      )
      state$report <- report
      state <- custom_draft_record(state, "validated")
      state$stage <- "approved"
    } else if (state$stage == "approved") {
      custom_model_current_definition(state$definition)
      authorization <- list(version_id = state$definition$version_id, intent_confirmed = !automatic, allow_code = TRUE)
      if (automatic)
        authorization$mode <- "automatic"
      state$result <- custom_run_envelope(structure(list(schema_version = 1L, definition = state$definition, validation = list(
        version_id = state$definition$version_id,
        technical_passed = TRUE, checks = state$checks
      ), approval = authorization), class = "finnts_custom_model"))
      state$status <- "approved"
      state <- custom_draft_save(state)
      custom_author_result(state$result, state$report)
    } else custom_author_abort("draft", "Invalid lifecycle stage.")
  }, error = function(error) {
    diagnostics <- state$diagnostics %||% list(schema_version = 1L, history = list(), snapshot = NULL, omitted = FALSE)
    bundle <- c(validate_custom_draft_diagnostics(diagnostics), list(stage = state$stage, draft_reference = if (is.null(state$store)) NULL else file.path(
      state$store,
      "current.rds"
    )))
    if (custom_draft_diagnostic_size(bundle) > 1048576L) {
      bundle$snapshot[c("contract", "source")] <- list(NULL, NULL)
      bundle$omitted <- TRUE
    }
    error$diagnostics <- bundle
    stop(custom_author_error_feedback(error, state = state))
  })
}

#' Print a Custom Model or Pending Draft
#'
#' Display a compact status summary without exposing private review contents,
#' source code or artifact paths. Full identities remain available in the object.
#' @param x A validated custom model or pending authoring draft to summarize.
#' @param ... Unused print arguments.
#' @return The object invisibly. Invalid model/draft evidence raises an error.
#' @seealso [create_custom_model()], [custom_model_feedback()]
#' @export
print.finnts_custom_model <- function(x, ...) {
  model <- custom_run_envelope(x)
  cat("<finnts_custom_model>\n", "Name: ", model$definition$name, "\n", "Version: ", substr(
    model$definition$version_id,
    1L, 12L
  ), "\n", "Mode: ", paste(model$definition$model_type, collapse = ", "), "\n", "Status: approved\n", "Validation checks: ",
  length(model$validation$checks), "\n",
  sep = ""
  )
  if (identical(model$approval$mode, "automatic"))
    cat("Authorization: automatic (not manually reviewed)\n")
  invisible(x)
}

#' @rdname print.finnts_custom_model
#' @export
print.finnts_custom_model_draft <- function(x, ...) {
  state <- validate_custom_model_draft(x)
  cat("<finnts_custom_model_draft>\n", "Draft: ", substr(state$draft_id, 1L, 12L), "\n", "Revision: ", state$revision,
    "\n", "Status: ", state$status, "\n", "Next: ", state$stage, if (is.null(state$pending) && state$stage %in% c(
      "contract",
      "source"
    ))
      " (llm required)"
    else "", "\n",
    sep = ""
  )
  if (!is.null(state$definition))
    cat("Name: ", state$definition$name, "\n", "Version: ", substr(state$definition$version_id, 1L, 12L), "\n", "Mode: ",
      paste(state$definition$model_type, collapse = ", "), "\n",
      sep = ""
    )
  else if (!is.null(state$contract))
    cat("Name: ", state$contract$name, "\n", "Mode: ", paste(state$contract$model_type, collapse = ", "), "\n", sep = "")
  invisible(x)
}
