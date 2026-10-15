test_that("malformed manifests and unsupported controls fail without execution", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model()
  prep_models(info,
    models_to_run = "business_mean", custom_models = list(business_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  custom <- finnts:::custom_run_read(info, TRUE)
  expect_error(finnts:::custom_run_training_controls(custom, TRUE, TRUE, NA), "logical negative_forecast")
  expect_no_error(finnts:::custom_run_training_controls(custom, TRUE, TRUE, FALSE))
  expect_error(finnts:::custom_run_training_controls(custom, FALSE, FALSE, TRUE), "no compatible enabled mode")
  for (change in list(list(forecast_approach = "standard_hierarchy"), list(recipes_to_run = "R2"))) {
    expect_error(finnts:::custom_run_context(info, modifyList(custom$manifest$context, change)), names(change))
  }
  invalid <- info
  invalid$object_output <- "qs2"
  expect_error(finnts:::custom_run_context(invalid, custom$manifest$context), "RDS")
  invalid$object_output <- "rds"
  for (backend in c("blob_container", "ms_drive")) {
    invalid$storage_object <- structure(list(), class = backend)
    expect_identical(
      finnts:::custom_run_context(invalid, custom$manifest$context),
      finnts:::custom_run_context(info, custom$manifest$context)
    )
  }
  invalid$storage_object <- structure(list(), class = "unsupported_backend")
  expect_error(finnts:::custom_run_context(invalid, custom$manifest$context), "supported Finn storage")
  original <- finnts:::custom_model_package_available
  testthat::local_mocked_bindings(custom_model_package_available = function(package) {
    if (package == "stats")
      FALSE
    else original(package)
  }, .package = "finnts")
  custom$pool$custom[[1]]$packages <- "stats"
  expect_error(finnts:::custom_run_training_controls(custom, TRUE, TRUE, TRUE), "unavailable package")
  missing <- info
  missing$path <- withr::local_tempdir()
  fs::dir_copy(fs::path(info$path, "logs"), fs::path(missing$path, "logs"))
  expect_error(train_models(missing, negative_forecast = TRUE), "Missing required Finn artifact")
  path <- finnts:::custom_run_path(info)
  saveRDS(NULL, path)
  before <- digest::digest(file = path)
  expect_error(train_models(info, negative_forecast = TRUE), "manifest")
  expect_error(
    prep_models(info, models_to_run = "business_mean", custom_models = list(business_mean = model), run_ensemble_models = FALSE),
    "manifest"
  )
  expect_identical(digest::digest(file = path), before)
})

test_that("custom source failures cannot turn into successful built-in fallbacks", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model(modes = "local")
  model$definition$source[["fit"]] <- "function(...) stop('deliberate custom source error')"
  model$definition$version_id <- finnts:::custom_model_definition_digest(model$definition)
  model$validation$version_id <- model$approval$version_id <- model$definition$version_id
  prep_models(info,
    models_to_run = c("business_mean", "meanf"), custom_models = list(business_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  expect_error(train_models(info, negative_forecast = TRUE), "business_mean.*deliberate custom source error")
  expect_length(list.files(fs::path(info$path, "forecasts")), 0L)
  data <- data.frame(Target = 1, Date = as.Date("2020-01-01"), Combo = "A")
  definition <- model$definition
  definition$requirements$predictors <- "missing_driver"
  definition$version_id <- finnts:::custom_model_definition_digest(definition)
  expect_error(finnts:::custom_run_workflows(definition, data, list(date_type = "month", forecast_horizon = 2)), "missing required predictors")
  definition$requirements$predictors <- "Target"
  definition$version_id <- finnts:::custom_model_definition_digest(definition)
  expect_error(finnts:::custom_run_workflows(definition, data, list(date_type = "month", forecast_horizon = 2)), "leakage")
})

test_that("required custom artifact membership cannot omit a selected candidate", {
  model <- standard_custom_model()
  pool <- finnts:::custom_run_pool("business_mean", NULL, list(business_mean = model))
  custom <- list(pool = pool, manifest = list(context = list(date_type = "month", forecast_horizon = 2)))
  candidates <- finnts:::custom_run_candidates(custom, TRUE, TRUE)
  empty <- candidates[0, ]
  expect_error(finnts:::custom_run_membership(empty, custom, candidates, required_custom = TRUE), "missing.*custom")
  rows <- candidates[candidates$Model_Type == "global", ]
  expect_error(finnts:::custom_run_membership(rows, custom, candidates[candidates$Model_Type == "local", ]), "identity")
})

test_that("each actual series validates required drivers before source execution", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model(modes = "local")
  model$definition$requirements$predictors <- "Driver"
  model$definition$version_id <- finnts:::custom_model_definition_digest(model$definition)
  model$validation$version_id <- model$approval$version_id <- model$definition$version_id
  for (combo in c("A", "B")) {
    path <- finnts:::local_artifact_path(info, "prep_data", "-C1", finnts:::hash_data(combo))
    rows <- finnts:::read_exact_artifact(info, path)
    rows$Driver <- 1
    finnts:::write_data(rows, combo, info, "data", "prep_data", "-C1")
  }
  prep_models(info,
    models_to_run = "business_mean", custom_models = list(business_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  path <- finnts:::local_artifact_path(info, "prep_data", "-C1", finnts:::hash_data("B"))
  rows <- finnts:::read_exact_artifact(info, path)
  rows$Driver[[1]] <- NA_real_
  finnts:::write_data(rows, "B", info, "data", "prep_data", "-C1")
  expect_error(train_models(info, negative_forecast = TRUE), "complete supported predictor values: Driver")
})
