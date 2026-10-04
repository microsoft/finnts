# Build a tiny passive approved fixture without preparing data or running source.
# Attestations are synthetic test evidence, not authenticated human approval.
agent_custom_fixture <- function(name = "business_mean", modes = c("local", "global"), predictors = character()) {
  definition <- finnts:::new_custom_model_definition(
    name, "Use the analysis mean.", "Use the historical mean without transformations.", modes,
    source = c(fit = "function(data, context, parameters) stop('source execution sentinel')",
      predict = "function(object, new_data, context) stop('source execution sentinel')"),
    requirements = list(predictors = predictors, recipes = "R1", target_scale = "original",
      date_types = "month", forecast_horizon = c(1, 2), missing_data = "error"),
    fixed_parameters = list(offset = 0)
  )
  model <- structure(list(schema_version = 1L, definition = definition,
    validation = list(version_id = definition$version_id, technical_passed = TRUE,
      checks = list(arithmetic = "Synthetic fixture only")),
    approval = list(version_id = definition$version_id, intent_confirmed = TRUE, allow_code = TRUE)),
    class = "finnts_custom_model")
  list(model = model, info = list(project_info = list(date_type = "month",
    storage_object = NULL, object_output = "rds", data_output = "csv"), forecast_horizon = 2,
    external_regressors = predictors, run_local_models = TRUE, run_global_models = TRUE,
    negative_forecast = TRUE, forecast_approach = "bottoms_up", allow_hierarchical_forecast = FALSE))
}

test_that("approved custom candidates form a passive Agent contract", {
  fixture <- agent_custom_fixture()
  original <- fixture
  contract <- resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = fixture$model))
  expect_identical(contract$schema_version, 1L)
  expect_identical(contract$selected, "business_mean")
  expect_identical(contract$mode_models, list(local = "business_mean", global = "business_mean"))
  expect_identical(contract$envelopes$business_mean, fixture$model)
  expect_match(contract$pool_id, "^[a-f0-9]{64}$")
  expect_match(contract$contract_id, "^[a-f0-9]{64}$")
  expect_identical(fixture, original)
  expect_identical(contract, unserialize(serialize(contract, NULL)))
  expect_null(resolve_agent_custom_models(fixture$info, custom_models = list(business_mean = fixture$model)))
})

test_that("automatic Agent contracts retain policy provenance without executing source", {
  fixture <- agent_custom_fixture()
  manual <- resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = fixture$model))
  fixture$model$approval$intent_confirmed <- FALSE
  fixture$model$approval$mode <- "automatic"
  local_mocked_bindings(custom_author_request = function(...) stop("Unexpected provider request"),
    custom_model_fit_impl = function(...) stop("Unexpected fit"), .package = "finnts")
  automatic <- resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = fixture$model))
  expect_identical(automatic$pool_id, manual$pool_id)
  expect_false(identical(automatic$contract_id, manual$contract_id))
  expect_identical(automatic$envelopes$business_mean$approval, fixture$model$approval)
  path <- tempfile(fileext = ".rds")
  saveRDS(automatic, path)
  expect_identical(validate_agent_custom_models(readRDS(path)), automatic)
  changed <- automatic
  changed$envelopes$business_mean$approval$mode <- NULL
  expect_error(validate_agent_custom_models(changed), "version-bound")
  changed$envelopes$business_mean$approval$intent_confirmed <- TRUE
  expect_error(validate_agent_custom_models(changed), "identity|contract")
})

# Refresh only synthetic attestations after an intentional fixture change.
# Production approval cannot be manufactured by this test-only convenience.
agent_custom_reapprove <- function(model) {
  model$definition$version_id <- finnts:::custom_model_definition_digest(model$definition)
  model$validation$version_id <- model$approval$version_id <- model$definition$version_id
  model
}

test_that("Agent contracts preserve selection and reject invalid envelopes", {
  fixture <- agent_custom_fixture()
  expect_null(resolve_agent_custom_models(list()))
  expect_null(resolve_agent_custom_models(list(), custom_models = list()))
  models <- list(business_mean = fixture$model)
  mixed <- resolve_agent_custom_models(fixture$info, c("business_mean", "meanf"), models)
  expect_identical(mixed$selected, c("business_mean", "meanf"))
  expect_identical(mixed$mode_models$local, c("business_mean", "meanf"))
  expect_identical(mixed$mode_models$global, "business_mean")
  expect_null(resolve_agent_custom_models(fixture$info, "meanf", models))
  expect_error(resolve_agent_custom_models(fixture$info, "unknown", models), "Unknown")
  expect_error(resolve_agent_custom_models(fixture$info, "business_mean", list(other = fixture$model)), "alias")
  for (invalid in list(fixture$model$definition, list(registry_schema_version = 1L), stats::lm(mpg ~ wt, mtcars))) {
    expect_error(resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = invalid)), "envelopes")
  }
  invalid <- fixture$model
  invalid$definition$source[["fit"]] <- "function(...) 1"
  expect_error(resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = invalid)), "version_id")
  invalid <- fixture$model
  invalid$approval$allow_code <- FALSE
  expect_error(resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = invalid)), "allow_code")
})

test_that("Agent contracts validate metadata and declared modes without I/O", {
  fixture <- agent_custom_fixture(modes = "local", predictors = "Driver")
  models <- list(business_mean = fixture$model)
  # Every forbidden dependency is replaced by an unconditional failure sentinel.
  testthat::local_mocked_bindings(
    read_file = function(...) stop("Unexpected artifact read"),
    write_data = function(...) stop("Unexpected artifact write"),
    list_files = function(...) stop("Unexpected artifact discovery"),
    read_custom_model_definition = function(...) stop("Unexpected registry read"),
    custom_model_workflow = function(...) stop("Unexpected workflow construction"),
    custom_model_fit_impl = function(...) stop("Unexpected source execution"),
    custom_author_request = function(...) stop("Unexpected provider request"),
    get_foundation_model_suffix = function(...) stop("Unexpected credential lookup"), .package = "finnts")
  contract <- resolve_agent_custom_models(fixture$info, "business_mean", models)
  expect_identical(contract$mode_models, list(local = "business_mean", global = character()))
  invalid_contexts <- list(
    list(run_local_models = FALSE), list(negative_forecast = FALSE), list(allow_hierarchical_forecast = TRUE),
    list(forecast_approach = "standard_hierarchy"), list(forecast_horizon = 3),
    list(forecast_horizon = 1.5), list(forecast_horizon = NA_real_), list(forecast_horizon = Inf),
    list(external_regressors = character()), list(run_global_models = NA), list(run_global_models = c(TRUE, FALSE))
  )
  for (values in invalid_contexts) {
    info <- utils::modifyList(fixture$info, values)
    expect_error(resolve_agent_custom_models(info, "business_mean", models))
  }
  for (field in c("run_local_models", "run_global_models", "negative_forecast", "allow_hierarchical_forecast", "forecast_approach")) {
    info <- fixture$info
    info[[field]] <- NULL
    expect_error(resolve_agent_custom_models(info, "business_mean", models))
  }
  for (values in list(list(date_type = "week"), list(storage_object = list()),
    list(object_output = "qs"), list(data_output = "unsupported"))) {
    info <- fixture$info
    info$project_info <- utils::modifyList(info$project_info, values)
    expect_error(resolve_agent_custom_models(info, "business_mean", models))
  }
  global <- agent_custom_fixture(modes = "global")
  global$info$run_local_models <- FALSE
  expect_identical(resolve_agent_custom_models(global$info, "business_mean", list(business_mean = global$model))$mode_models,
    list(local = character(), global = "business_mean"))
})

test_that("Agent contract identity is canonical and sensitive to approved content", {
  fixture <- agent_custom_fixture(predictors = "Driver")
  fixture$info$external_regressors <- c("Driver", "Other")
  models <- list(business_mean = fixture$model)
  first <- resolve_agent_custom_models(fixture$info, c("business_mean", "meanf"), models)
  second_info <- fixture$info
  second_info$external_regressors <- rev(second_info$external_regressors)
  second_info$llm <- new.env()
  second_info$input_data <- data.frame(private_value = 12345)
  second_info$project_info$path <- "PRIVATE_PATH_SENTINEL"
  models$business_mean$definition$created_at <- "2026-01-01T00:00:00Z"
  models$unused <- agent_custom_fixture("unused")$model
  second <- resolve_agent_custom_models(second_info, c("meanf", "business_mean"), rev(models))
  expect_identical(first$pool_id, second$pool_id)
  expect_identical(first$contract_id, second$contract_id)
  expect_identical(second$envelopes$business_mean$definition$created_at, "2026-01-01T00:00:00Z")
  expect_length(second$envelopes, 1L)
  expect_false(any(c("llm", "input_data", "path") %in% names(second$context)))
  for (field in c("source", "fixed_parameters", "interpretation", "requirements", "evidence")) {
    model <- fixture$model
    if (field == "source") model$definition$source[["fit"]] <- "function(...) 1"
    if (field == "fixed_parameters") model$definition$fixed_parameters$offset <- 1
    if (field == "interpretation") model$definition$interpretation <- "A clarified historical mean."
    if (field == "requirements") model$definition$requirements$forecast_horizon <- 2
    if (field == "evidence") model$validation$checks$arithmetic <- "Different synthetic check"
    model <- agent_custom_reapprove(model)
    changed <- resolve_agent_custom_models(fixture$info, c("business_mean", "meanf"), list(business_mean = model))
    expect_false(identical(first$contract_id, changed$contract_id), info = field)
  }
  changed_info <- fixture$info
  changed_info$forecast_horizon <- 1
  expect_false(identical(first$contract_id,
    resolve_agent_custom_models(changed_info, c("business_mean", "meanf"), list(business_mean = fixture$model))$contract_id))
})

test_that("Agent contract revalidation rejects stale and malformed derived records", {
  fixture <- agent_custom_fixture()
  contract <- resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = fixture$model))
  expect_identical(validate_agent_custom_models(contract), contract)
  expect_identical(validate_agent_custom_models(unserialize(serialize(contract, NULL))), contract)
  for (field in c("contract_id", "pool_id", "mode_models", "context", "envelopes", "extra")) {
    changed <- contract
    if (field %in% c("contract_id", "pool_id")) changed[[field]] <- paste(rep("a", 64), collapse = "")
    if (field == "mode_models") changed$mode_models$local <- c("business_mean", "arima")
    if (field == "context") changed$context$stationary <- TRUE
    if (field == "envelopes") changed$envelopes$unused <- agent_custom_fixture("unused")$model
    if (field == "extra") changed$extra <- new.env()
    expect_error(validate_agent_custom_models(changed), info = field)
  }
  reordered <- contract[rev(names(contract))]
  reordered$context <- contract$context[rev(names(contract$context))]
  expect_identical(validate_agent_custom_models(reordered), reordered)
  changed <- contract
  changed$selected <- as.list(changed$selected)
  expect_error(validate_agent_custom_models(changed), "schema|names|type")
  changed <- contract
  changed$mode_models$local <- as.list(changed$mode_models$local)
  expect_error(validate_agent_custom_models(changed), "modes|names|type")
})

# Complete serialized proposal matching the existing Agent reasoning boundary.
# Optional replacements exercise one invalid setting without changing the oracle.
agent_custom_proposal <- function(...) {
  utils::modifyList(list(models_to_run = "business_mean", external_regressors = "NULL",
    clean_missing_values = FALSE, clean_outliers = FALSE, forecast_approach = "bottoms_up",
    stationary = FALSE, feature_selection = FALSE, multistep_horizon = FALSE,
    seasonal_period = "NULL", recipes_to_run = "R1", lag_periods = "NULL",
    rolling_window_periods = "NULL", reasoning = "Use the approved fixed rule."), list(...))
}

test_that("candidate projection contains only enrolled per-mode descriptive metadata", {
  fixture <- agent_custom_fixture(modes = "local")
  fixture$model$definition$fixed_parameters$note <- "PARAMETER_SENTINEL"
  fixture$model$validation$checks$arithmetic <- "EVIDENCE_SENTINEL"
  fixture$model <- agent_custom_reapprove(fixture$model)
  fixture$info$llm <- new.env(parent = emptyenv())
  fixture$info$llm$credentials <- "NOT_A_CREDENTIAL_SENTINEL"
  fixture$info$input_data <- data.frame(Combo = "PRIVATE_SERIES_SENTINEL", Target = 12345)
  before <- serialize(fixture, NULL)
  contract <- resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = fixture$model))
  testthat::local_mocked_bindings(get_foundation_model_suffix = function(...) stop("Unexpected credential lookup"),
    custom_author_request = function(...) stop("Unexpected provider request"), .package = "finnts")
  candidates <- agent_custom_candidates(contract, "combo-hash")
  expect_identical(candidates$models, "business_mean")
  expect_identical(candidates$contract_id, contract$contract_id)
  expect_identical(candidates$custom$business_mean$model_type, "local")
  expect_setequal(names(candidates$custom$business_mean), c("name", "interpretation", "model_type", "version_id",
    "predictors", "date_type", "forecast_horizon", "target_scale", "recipe_id"))
  expect_false(grepl("sentinel|12345", jsonlite::toJSON(candidates), ignore.case = TRUE))
  expect_length(agent_custom_candidates(contract)$models, 0L)
  expect_length(agent_custom_candidates(contract)$custom, 0L)
  expect_identical(serialize(fixture, NULL), before)
  expect_error(agent_custom_candidates(contract, ""))
  expect_error(agent_custom_candidates(contract, foundation_suffix = c("", "")))
})

test_that("candidate availability filters foundations without adding unselected built-ins", {
  fixture <- agent_custom_fixture()
  contract <- resolve_agent_custom_models(fixture$info, c("business_mean", "meanf", "chronos2"),
    list(business_mean = fixture$model))
  expect_identical(agent_custom_candidates(contract, "combo-hash")$models, c("business_mean", "meanf"))
  expect_identical(agent_custom_candidates(contract, "combo-hash", "---chronos2")$models,
    c("business_mean", "meanf", "chronos2"))
  expect_identical(agent_custom_candidates(contract, foundation_suffix = "---chronos2")$models,
    c("business_mean", "chronos2"))
  custom_only <- resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = fixture$model))
  expect_identical(agent_custom_candidates(custom_only, "combo-hash", "---chronos2")$models, "business_mean")
})

test_that("custom Agent proposals use existing parsing and preserve the pinned rule", {
  fixture <- agent_custom_fixture(predictors = "Driver")
  contract <- resolve_agent_custom_models(fixture$info, c("business_mean", "arima"), list(business_mean = fixture$model))
  before <- contract
  proposal <- agent_custom_proposal(models_to_run = "business_mean---arima---business_mean", external_regressors = "Driver",
    stationary = "FALSE", clean_missing_values = "FALSE", lag_periods = "2---3")
  result <- validate_agent_custom_proposal(proposal, contract, "combo-hash")
  expect_identical(result$models_to_run, c("business_mean", "arima"))
  expect_identical(result$external_regressors, "Driver")
  expect_identical(result$lag_periods, c(2, 3))
  expect_false(result$stationary)
  expect_true(result$negative_forecast)
  expect_identical(contract, before)
  expect_identical(validate_agent_custom_proposal(agent_custom_proposal(models_to_run = "arima"),
    contract, "combo-hash")$models_to_run, "arima")
  expect_identical(validate_agent_custom_proposal(agent_custom_proposal(external_regressors = "Driver"),
    contract)$models_to_run, "business_mean")
})

test_that("custom Agent proposal errors cannot change source, enrollment or preprocessing", {
  fixture <- agent_custom_fixture(modes = "local", predictors = "Driver")
  contract <- resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = fixture$model))
  invalid <- list(models_to_run = "arima", external_regressors = "NULL", clean_missing_values = TRUE,
    clean_outliers = TRUE, forecast_approach = "standard_hierarchy", stationary = TRUE,
    feature_selection = TRUE, multistep_horizon = TRUE, recipes_to_run = "R2", lag_periods = "Inf",
    source = "function(...) 0", approval = "APPROVE", negative_forecast = TRUE, contract_id = contract$contract_id)
  for (field in names(invalid)) {
    proposal <- agent_custom_proposal(external_regressors = "Driver")
    proposal[[field]] <- invalid[[field]]
    error <- tryCatch(validate_agent_custom_proposal(proposal, contract, "combo-hash"), error = identity)
    expect_s3_class(error, "finnts_reason_proposal_invalid")
  }
  for (field in names(agent_custom_proposal())) {
    proposal <- agent_custom_proposal(external_regressors = "Driver")
    proposal[[field]] <- NULL
    expect_error(validate_agent_custom_proposal(proposal, contract, "combo-hash"),
      class = "finnts_reason_proposal_invalid", info = field)
  }
  expect_error(validate_agent_custom_proposal(agent_custom_proposal(external_regressors = "Driver"), contract),
    class = "finnts_reason_proposal_invalid")
  stale <- contract
  stale$contract_id <- paste(rep("a", 64), collapse = "")
  error <- tryCatch(validate_agent_custom_proposal(agent_custom_proposal(), stale, "combo-hash"), error = identity)
  expect_s3_class(error, "error")
  expect_false(inherits(error, "finnts_reason_proposal_invalid"))
  expect_error(agent_custom_candidates(stale, "combo-hash"), "identity")
})

test_that("installed passive Agent contracts validate in an exact fresh namespace", {
  namespace <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace, "Meta", "package.rds")),
    "Fresh-process contract validation is checked from the installed namespace")
  fixture <- agent_custom_fixture()
  contract <- resolve_agent_custom_models(fixture$info, "business_mean", list(business_mean = fixture$model))
  # The child receives only the passive fixture and exact package identity; it
  # validates and projects metadata without fitting or executing candidate code.
  worker <- function(contract, namespace, version) {
    loaded <- loadNamespace("finnts")
    stopifnot(identical(normalizePath(getNamespaceInfo(loaded, "path")), normalizePath(namespace)),
      identical(as.character(utils::packageVersion("finnts")), version))
    restored <- unserialize(serialize(contract, NULL))
    validated <- get("validate_agent_custom_models", envir = loaded)(restored)
    list(contract = validated, candidates = get("agent_custom_candidates", envir = loaded)(validated, "combo-hash"))
  }
  environment(worker) <- baseenv()
  result <- callr::r(worker, args = list(contract = contract, namespace = namespace,
    version = as.character(utils::packageVersion("finnts"))), libpath = .libPaths(),
    system_profile = FALSE, user_profile = FALSE, timeout = 60)
  expect_identical(result$contract, contract)
  expect_identical(result$candidates$models, "business_mean")
  expect_identical(result$candidates$contract_id, contract$contract_id)
})