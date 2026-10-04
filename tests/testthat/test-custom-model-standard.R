# Construct a synthetic trusted-caller attestation for deterministic tests only.
# Fixed arithmetic and passive check summaries are not a public authoring API.
# Temporal fixtures opt into schema 3; legacy cases retain their original identity.
standard_custom_model <- function(name = "business_mean", modes = c("local", "global"), offset = 0, temporal = FALSE) {
  definition <- finnts:::new_custom_model_definition(
    name, "Use the historical mean plus a fixed offset.",
    "Fit the mean of analysis targets only.", modes,
    source = c(
      fit = paste0("function(data, context, parameters) { ",
        "stopifnot(max(data$Date) == context$cutoff); ",
        "mean(data$Target) + parameters$offset }"),
      predict = paste0("function(object, new_data, context) { ",
        "stopifnot(!'Target' %in% names(new_data)); data.frame(",
        ".finnts_row = new_data$.finnts_row, .pred = rep(object, nrow(new_data))) }")
    ),
    requirements = c(list(predictors = character(), recipes = "R1", target_scale = "original",
      date_types = "month", forecast_horizon = 1:3, missing_data = "error"),
      if (temporal) list(runtime = list(version = 1L, prediction_scope = "complete_horizon"))),
    fixed_parameters = list(offset = offset)
  )
  structure(list(schema_version = 1L, definition = definition,
    validation = list(version_id = definition$version_id, technical_passed = TRUE,
      checks = list(arithmetic = "Synthetic fixture checked independently")),
    approval = list(version_id = definition$version_id, intent_confirmed = TRUE, allow_code = TRUE)),
    class = "finnts_custom_model")
}

standard_input <- data.frame(
  Date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 24), 2),
  id = rep(c("A", "B"), each = 24), value = c(rep(100, 24), rep(200, 24))
)
standard_template <- set_run_info(project_name = "custom-standard", run_name = "test",
  path = withr::local_tempdir(), data_output = "csv", object_output = "rds", add_unique_id = FALSE)
prep_data(standard_template, standard_input, combo_variables = "id", target_variable = "value",
  date_type = "month", forecast_horizon = 2, recipes_to_run = "R1", stationary = FALSE,
  box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE)

# Copy immutable prepared artifacts to an isolated root; never reuse fitted data.
standard_run <- function(path) {
  info <- standard_template
  info$path <- path
  for (folder in c("logs", "prep_data")) {
    fs::dir_copy(fs::path(standard_template$path, folder), fs::path(path, folder))
  }
  fs::dir_create(fs::path(path, c("models", "forecasts", "prep_models")))
  info
}

test_that("prep_models enrolls approved custom workflows without executing source", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model()
  prep_models(info, models_to_run = "business_mean", custom_models = list(business_mean = model),
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  workflows <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(
    info, "prep_models", "-model_workflows", extension = "rds"), return_type = "object")
  expect_identical(unique(workflows$Model_Name), "business_mean")
  expect_setequal(workflows$Model_Type, c("local", "global"))
  expect_true(all(vapply(workflows$Model_Workflow, inherits, logical(1), "workflow")))
  recipe <- recipes::step_zv(workflows::extract_recipe(workflows$Model_Workflow[[1]], estimated = FALSE),
    recipes::all_numeric_predictors())
  workflows$Model_Workflow[[1]] <- workflows::update_recipe(workflows$Model_Workflow[[1]], recipe)
  expect_error(finnts:::custom_run_check_workflows(workflows, finnts:::custom_run_read(info, TRUE)), "preprocessing")
})

test_that("approval and supported preparation settings fail before enrollment", {
  model <- standard_custom_model()
  for (field in c("intent_confirmed", "allow_code")) {
    invalid <- model
    invalid$approval[[field]] <- FALSE
    expect_error(finnts:::custom_run_pool("business_mean", NULL, list(business_mean = invalid)), "intent|allow_code")
  }
  invalid <- model
  invalid$validation$version_id <- paste(rep("a", 64), collapse = "")
  expect_error(finnts:::custom_run_pool("business_mean", NULL, list(business_mean = invalid)), "validation")
  expect_error(finnts:::custom_run_pool("business_mean", NULL, list(business_mean = model$definition)), "envelopes")
  invalid <- model
  invalid$approval$session <- new.env()
  expect_error(finnts:::custom_run_pool("business_mean", NULL, list(business_mean = invalid)), "passive")
  expect_null(finnts:::custom_run_pool(NULL, NULL, list(business_mean = model)))
  info <- standard_run(withr::local_tempdir())
  log <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info, "logs", extension = "csv"))
  for (field in c("stationary", "box_cox", "clean_missing_values", "clean_outliers", "multistep_horizon")) {
    changed <- log
    changed[[field]] <- TRUE
    expect_error(finnts:::custom_run_context(info, changed, FALSE), field)
  }
  expect_error(finnts:::custom_run_context(info, log, TRUE), "run_ensemble_models")
})

test_that("automatic envelopes are strict and keep source and model identity unchanged", {
  manual <- standard_custom_model()
  automatic <- manual
  automatic$approval$intent_confirmed <- FALSE
  automatic$approval$mode <- "automatic"
  expect_identical(custom_run_envelope(manual), manual)
  expect_identical(custom_run_envelope(automatic), automatic)
  expect_identical(automatic$definition, manual$definition)
  expect_identical(custom_run_pool("business_mean", NULL, list(business_mean = automatic))$pool_id,
    custom_run_pool("business_mean", NULL, list(business_mean = manual))$pool_id)
  for (defect in c("human", "allow_code", "missing_mode", "unknown_mode", "manual_mode", "version", "extra")) {
    invalid <- automatic
    if (defect == "human") invalid$approval$intent_confirmed <- TRUE
    if (defect == "allow_code") invalid$approval$allow_code <- FALSE
    if (defect == "missing_mode") invalid$approval$mode <- NULL
    if (defect == "unknown_mode") invalid$approval$mode <- "auto"
    if (defect == "manual_mode") invalid$approval$mode <- "manual"
    if (defect == "version") invalid$approval$version_id <- paste(rep("a", 64), collapse = "")
    if (defect == "extra") invalid$approval$extra <- TRUE
    expect_error(custom_run_envelope(invalid), "version-bound", info = defect)
  }
  invalid <- manual
  invalid$approval$mode <- "manual"
  expect_error(custom_run_envelope(invalid), "version-bound")
})

test_that("automatic forecasts round-trip authorization without authoring or fallback", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model(modes = "local")
  model$approval$intent_confirmed <- FALSE
  model$approval$mode <- "automatic"
  path <- tempfile(fileext = ".rds")
  saveRDS(model, path)
  local_mocked_bindings(custom_author_request = function(...) stop("Unexpected provider request"),
    custom_author_validate = function(...) stop("Unexpected authoring validation"),
    new_llm_session = function(...) stop("Unexpected LLM session"), .package = "finnts")
  prep_models(info, models_to_run = "business_mean", custom_models = list(business_mean = readRDS(path)),
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  saved <- custom_run_read(info, TRUE)
  expect_identical(saved$manifest$evidence$business_mean$approval, model$approval)
  train_models(info, run_local_models = TRUE, run_global_models = FALSE, negative_forecast = TRUE)
  final_models(info, average_models = FALSE)
  forecast <- get_forecast_data(info)
  expect_identical(unique(forecast$Model_Name), "business_mean")
  expect_equal(forecast$Forecast, ifelse(forecast$Combo == "A", 100, 200))
  expect_identical(unique(forecast$Model_ID), paste0("custom-", model$definition$version_id, "--local--R1"))
  expect_identical(custom_run_read(info, TRUE)$manifest$evidence$business_mean$approval, model$approval)
})

test_that("local and global custom training retain arithmetic and version identity", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model(temporal = TRUE)
  prep_models(info, models_to_run = c("business_mean", "meanf"),
    custom_models = list(business_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1)
  train_models(info, run_global_models = TRUE, run_local_models = TRUE, negative_forecast = TRUE)
  paths <- finnts:::local_artifact_path(info, "forecasts", "-single_models",
    combo = vapply(c("A", "B"), finnts:::hash_data, character(1)), extension = "csv")
  local <- finnts:::read_exact_artifact(info, paths)
  custom <- local[local$Model_Name == "business_mean", ]
  expect_setequal(custom$Combo, c("A", "B"))
  expect_equal(custom$Forecast, ifelse(custom$Combo == "A", 100, 200))
  expect_identical(unique(custom$Model_ID), paste0("custom-", model$definition$version_id, "--local--R1"))
  expect_true("meanf" %in% local$Model_Name)
  global <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info, "forecasts", "-global_models",
    combo = vapply(c("A", "B"), finnts:::hash_data, character(1)), extension = "csv"))
  expect_equal(global$Forecast, rep(150, nrow(global)))
  expect_identical(unique(global$Model_ID), paste0("custom-", model$definition$version_id, "--global--R1"))

  final_models(info, average_models = FALSE)
  forecast <- get_forecast_data(info)
  expect_true(any(forecast$Best_Model == "Yes"))
  expect_true("business_mean" %in% forecast$Model_Name)
  before <- tools::md5sum(paths)
  prep_models(info, models_to_run = c("meanf", "business_mean"),
    custom_models = list(business_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1)
  train_models(info, run_global_models = TRUE, run_local_models = TRUE, negative_forecast = TRUE)
  final_models(info, average_models = FALSE)
  expect_identical(tools::md5sum(paths), before)
  changed <- standard_custom_model(offset = 1)
  expect_error(prep_models(info, models_to_run = c("meanf", "business_mean"),
    custom_models = list(business_mean = changed), run_ensemble_models = FALSE), "changed|new run")
  expect_error(prep_models(info, models_to_run = "meanf", run_ensemble_models = FALSE), "same approved")
  stale <- finnts:::read_exact_artifact(info, paths[[1]])
  stale$Model_ID[stale$Model_Name == "business_mean"] <- paste0("custom-", changed$definition$version_id, "--local--R1")
  finnts:::write_data(stale, "A", info, "data", "forecasts", "-single_models")
  expect_error(train_models(info, run_global_models = TRUE, negative_forecast = TRUE), "identity|pool")
  expect_error(final_models(info, average_models = FALSE), "identity|pool")
  expect_error(ensemble_models(info), "identity|pool")

  pinned <- finnts:::custom_run_read(info, TRUE)
  candidates <- finnts:::custom_run_candidates(pinned, TRUE, TRUE)
  fit_path <- finnts:::local_artifact_path(info, "models", "-single_models", finnts:::hash_data("A"), extension = "rds")
  fitted <- finnts:::read_exact_artifact(info, fit_path, return_type = "object")
  custom_index <- which(fitted$Model_Name == "business_mean")
  fitted$Model_Fit[[custom_index]]$fit$fit$fit$context$forecast_horizon <- 1
  expect_error(finnts:::custom_run_membership(fitted, pinned, candidates), "context|identity")
})

test_that("high-level custom-only forecasts preserve aliases and reject implicit transforms", {
  model <- standard_custom_model(modes = "local")
  info <- set_run_info(project_name = "custom-wrapper", run_name = "test", path = withr::local_tempdir(),
    data_output = "csv", object_output = "rds", add_unique_id = FALSE)
  args <- list(run_info = info, input_data = standard_input, combo_variables = "id",
    target_variable = "value", date_type = "month", forecast_horizon = 2,
    models_to_run = "business_mean", custom_models = list(business_mean = model),
    recipes_to_run = "R1", stationary = FALSE, clean_missing_values = FALSE,
    run_ensemble_models = FALSE, negative_forecast = TRUE, run_global_models = FALSE,
    back_test_scenarios = 1, average_models = FALSE, return_data = FALSE)
  invalid <- args
  invalid$stationary <- TRUE
  expect_error(do.call(forecast_time_series, invalid), "stationary")
  expect_length(list.files(fs::path(info$path, "prep_data")), 0L)
  do.call(forecast_time_series, args)
  result <- get_forecast_data(info)
  expect_identical(unique(result$Model_Name), "business_mean")
  expect_true(all(result$Best_Model == "Yes"))
  expect_equal(result$Forecast, ifelse(result$Combo == "A", 100, 200))
})

test_that("custom-only averages retain exact components and reject foreign saved identities", {
  info <- standard_run(withr::local_tempdir())
  models <- list(low_rule = standard_custom_model("low_rule", "local", -10),
    high_rule = standard_custom_model("high_rule", "local", 10))
  prep_models(info, models_to_run = names(models), custom_models = models,
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  train_models(info, negative_forecast = TRUE)
  final_models(info)
  result <- get_forecast_data(info)
  average <- result[result$Recipe_ID == "simple_average", ]
  expect_gt(nrow(average), 0L)
  expect_true(all(average$Best_Model == "Yes"))
  expect_equal(average$Forecast, ifelse(average$Combo == "A", 100, 200))
  expect_setequal(result$Model_Name[result$Recipe_ID == "R1"], names(models))
  before <- result
  final_models(info)
  expect_equal(get_forecast_data(info), before)
  average_path <- finnts:::local_artifact_path(info, "forecasts", "-average_models", finnts:::hash_data("A"))
  rows <- finnts:::read_exact_artifact(info, average_path)
  rows$Model_ID <- sub("custom-[a-f0-9]+", "arima", rows$Model_ID)
  finnts:::write_data(rows, "A", info, "data", "forecasts", "-average_models")
  expect_error(final_models(info), "component identity")
})

test_that("global-only custom forecasts preserve negative arithmetic", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model(modes = "global", offset = -300)
  prep_models(info, models_to_run = "business_mean", custom_models = list(business_mean = model),
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  expect_error(train_models(info, run_global_models = FALSE, negative_forecast = TRUE), "enabled mode")
  train_models(info, run_local_models = FALSE, run_global_models = TRUE, negative_forecast = TRUE)
  paths <- finnts:::local_artifact_path(info, "forecasts", "-global_models",
    vapply(c("A", "B"), finnts:::hash_data, character(1)))
  rows <- finnts:::read_exact_artifact(info, paths)
  expect_equal(rows$Forecast, rep(-150, nrow(rows)))
  expect_identical(unique(rows$Model_Type), "global")
})

test_that("malformed manifests and unsupported controls fail without execution", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model()
  prep_models(info, models_to_run = "business_mean", custom_models = list(business_mean = model),
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  custom <- finnts:::custom_run_read(info, TRUE)
  for (change in list(list(feature_selection = TRUE), list(negative_forecast = FALSE),
    list(inner_parallel = TRUE), list(parallel_processing = "spark"))) {
    args <- modifyList(list(custom = custom, local = TRUE, global = TRUE, recipes = "R1",
      feature_selection = FALSE, negative_forecast = TRUE, parallel_processing = NULL, inner_parallel = FALSE), change)
    expect_error(do.call(finnts:::custom_run_training_controls, args), "Custom runs require")
  }
  for (change in list(list(forecast_approach = "standard_hierarchy"), list(recipes_to_run = "R2"))) {
    expect_error(finnts:::custom_run_context(info, modifyList(custom$manifest$context, change), FALSE),
      names(change))
  }
  invalid <- info
  invalid$object_output <- "qs2"
  expect_error(finnts:::custom_run_context(invalid, custom$manifest$context, FALSE), "RDS")
  invalid$object_output <- "rds"
  invalid$storage_object <- structure(list(), class = "blob_container")
  expect_error(finnts:::custom_run_context(invalid, custom$manifest$context, FALSE), "local/mounted")
  original <- finnts:::custom_model_package_available
  testthat::local_mocked_bindings(custom_model_package_available = function(package) {
    if (package == "stats") FALSE else original(package)
  }, .package = "finnts")
  custom$pool$custom[[1]]$packages <- "stats"
  expect_error(finnts:::custom_run_training_controls(custom, TRUE, TRUE, "R1", FALSE, TRUE, NULL, FALSE), "unavailable package")
  missing <- info
  missing$path <- withr::local_tempdir()
  fs::dir_copy(fs::path(info$path, "logs"), fs::path(missing$path, "logs"))
  expect_error(train_models(missing, negative_forecast = TRUE), "Missing required Finn artifact")
  path <- finnts:::custom_run_path(info)
  saveRDS(NULL, path)
  before <- digest::digest(file = path)
  expect_error(train_models(info, negative_forecast = TRUE), "manifest")
  expect_error(prep_models(info, models_to_run = "business_mean", custom_models = list(business_mean = model),
    run_ensemble_models = FALSE), "manifest")
  expect_identical(digest::digest(file = path), before)
})

test_that("custom source failures cannot turn into successful built-in fallbacks", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model(modes = "local")
  model$definition$source[["fit"]] <- "function(...) stop('deliberate custom source error')"
  model$definition$version_id <- finnts:::custom_model_definition_digest(model$definition)
  model$validation$version_id <- model$approval$version_id <- model$definition$version_id
  prep_models(info, models_to_run = c("business_mean", "meanf"), custom_models = list(business_mean = model),
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  expect_error(train_models(info, negative_forecast = TRUE), "business_mean.*deliberate custom source error")
  expect_length(list.files(fs::path(info$path, "forecasts")), 0L)
  data <- data.frame(Target = 1, Date = as.Date("2020-01-01"), Combo = "A")
  definition <- model$definition
  definition$requirements$predictors <- "missing_driver"
  definition$version_id <- finnts:::custom_model_definition_digest(definition)
  expect_error(finnts:::custom_run_workflows(definition, data,
    list(date_type = "month", forecast_horizon = 2)), "missing required predictors")
  definition$requirements$predictors <- "Target"
  definition$version_id <- finnts:::custom_model_definition_digest(definition)
  expect_error(finnts:::custom_run_workflows(definition, data,
    list(date_type = "month", forecast_horizon = 2)), "leakage")
})

test_that("required custom artifact membership cannot omit a selected candidate", {
  model <- standard_custom_model()
  pool <- finnts:::custom_run_pool("business_mean", NULL, list(business_mean = model))
  custom <- list(pool = pool, manifest = list(context = list(date_type = "month", forecast_horizon = 2)))
  candidates <- finnts:::custom_run_candidates(custom, TRUE, TRUE)
  empty <- candidates[0, ]
  expect_error(finnts:::custom_run_membership(empty, custom, candidates, required_custom = TRUE), "missing.*custom")
  rows <- candidates[candidates$Model_Type == "global", ]
  expect_error(finnts:::custom_run_membership(rows, custom,
    candidates[candidates$Model_Type == "local", ]), "identity")
})

test_that("installed custom training matches sequential execution on two PSOCK workers", {
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace_path, "Meta", "package.rds")),
    "Actual custom training workers are verified from the installed namespace")
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  sequential <- standard_run(withr::local_tempdir())
  parallel <- standard_run(withr::local_tempdir())
  model <- standard_custom_model()
  for (info in list(sequential, parallel)) {
    prep_models(info, models_to_run = "business_mean", custom_models = list(business_mean = model),
      run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  }
  train_models(sequential, run_global_models = TRUE, negative_forecast = TRUE)
  train_models(parallel, run_global_models = TRUE, negative_forecast = TRUE,
    parallel_processing = "local_machine", num_cores = 2)
  for (suffix in c("-single_models", "-global_models")) {
    combos <- vapply(c("A", "B"), finnts:::hash_data, character(1))
    expected <- finnts:::read_exact_artifact(sequential, finnts:::local_artifact_path(sequential, "forecasts", suffix, combos))
    actual <- finnts:::read_exact_artifact(parallel, finnts:::local_artifact_path(parallel, "forecasts", suffix, combos))
    expect_equal(actual, expected)
  }
})

test_that("each actual series validates required drivers before source execution", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model(modes = "local")
  model$definition$requirements$predictors <- "Driver"
  model$definition$version_id <- finnts:::custom_model_definition_digest(model$definition)
  model$validation$version_id <- model$approval$version_id <- model$definition$version_id
  for (combo in c("A", "B")) {
    path <- finnts:::local_artifact_path(info, "prep_data", "-R1", finnts:::hash_data(combo))
    rows <- finnts:::read_exact_artifact(info, path)
    rows$Driver <- 1
    finnts:::write_data(rows, combo, info, "data", "prep_data", "-R1")
  }
  prep_models(info, models_to_run = "business_mean", custom_models = list(business_mean = model),
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  path <- finnts:::local_artifact_path(info, "prep_data", "-R1", finnts:::hash_data("B"))
  rows <- finnts:::read_exact_artifact(info, path)
  rows$Driver[[1]] <- NA_real_
  finnts:::write_data(rows, "B", info, "data", "prep_data", "-R1")
  expect_error(train_models(info, negative_forecast = TRUE), "complete supported predictor values: Driver")
})