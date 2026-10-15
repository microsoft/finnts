test_that("prep_models enrolls approved custom workflows without executing source", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model()
  # Standard preparation must carry approval in its normal workflow artifact,
  # without registering another copy of the caller-supplied definition.
  local_mocked_bindings(write_custom_model_definition = function(...) {
    stop("Unexpected redundant standard registration")
  }, .package = "finnts")
  prep_models(info,
    models_to_run = "business_mean", custom_models = list(business_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  workflows <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info, "prep_models", "-model_workflows",
    extension = "rds"
  ), return_type = "object")
  expect_identical(unique(workflows$Model_Name), "business_mean")
  expect_setequal(workflows$Model_Type, c("local", "global"))
  expect_true(all(vapply(workflows$Model_Workflow, inherits, logical(1), "workflow")))
  expect_identical(attr(workflows, "finnts_custom_manifest")$evidence$business_mean$approval, model$approval)
  expect_false(file.exists(finnts:::local_artifact_path(info, "prep_models", "-custom-model-pool", extension = "rds")))
  recipe <- recipes::step_zv(workflows::extract_recipe(workflows$Model_Workflow[[1]], estimated = FALSE), recipes::all_numeric_predictors())
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
    expect_identical(finnts:::custom_run_context(info, changed)$recipe_id, "C1")
  }
  changed <- log
  changed$recipes_to_run <- "R1"
  expect_error(finnts:::custom_run_context(info, changed), "need C1")
  changed$recipes_to_run <- "R1---C1"
  expect_identical(finnts:::custom_run_context(info, changed)$recipe_id, "C1")
})
