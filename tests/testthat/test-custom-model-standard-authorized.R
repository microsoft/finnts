test_that("automatic envelopes are strict and keep source and model identity unchanged", {
  manual <- standard_custom_model()
  automatic <- manual
  automatic$approval$intent_confirmed <- FALSE
  automatic$approval$mode <- "automatic"
  expect_identical(custom_run_envelope(manual), manual)
  expect_identical(custom_run_envelope(automatic), automatic)
  expect_identical(automatic$definition, manual$definition)
  expect_identical(custom_run_pool("business_mean", NULL, list(business_mean = automatic))$pool_id, custom_run_pool(
    "business_mean",
    NULL, list(business_mean = manual)
  )$pool_id)
  for (defect in c("human", "allow_code", "missing_mode", "unknown_mode", "manual_mode", "version", "extra")) {
    invalid <- automatic
    if (defect == "human")
      invalid$approval$intent_confirmed <- TRUE
    if (defect == "allow_code")
      invalid$approval$allow_code <- FALSE
    if (defect == "missing_mode")
      invalid$approval$mode <- NULL
    if (defect == "unknown_mode")
      invalid$approval$mode <- "auto"
    if (defect == "manual_mode")
      invalid$approval$mode <- "manual"
    if (defect == "version")
      invalid$approval$version_id <- paste(rep("a", 64), collapse = "")
    if (defect == "extra")
      invalid$approval$extra <- TRUE
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
  local_mocked_bindings(
    custom_author_request = function(...) stop("Unexpected provider request"), custom_author_validate = function(...) stop("Unexpected authoring validation"),
    new_llm_session = function(...) stop("Unexpected LLM session"), .package = "finnts"
  )
  prep_models(info,
    models_to_run = "business_mean", custom_models = list(business_mean = readRDS(path)), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  saved <- custom_run_read(info, TRUE)
  expect_identical(saved$manifest$evidence$business_mean$approval, model$approval)
  train_models(info, run_local_models = TRUE, run_global_models = FALSE, negative_forecast = TRUE)
  final_models(info, average_models = FALSE)
  forecast <- get_forecast_data(info)
  expect_identical(unique(forecast$Model_Name), "business_mean")
  expect_equal(forecast$Forecast, ifelse(forecast$Combo == "A", 100, 200))
  expect_identical(unique(forecast$Model_ID), paste0("custom-", model$definition$version_id, "--local--C1"))
  expect_identical(custom_run_read(info, TRUE)$manifest$evidence$business_mean$approval, model$approval)
})
