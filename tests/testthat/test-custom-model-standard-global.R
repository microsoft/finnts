test_that("global-only custom forecasts preserve negative arithmetic", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model(modes = "global", offset = -300)
  prep_models(info,
    models_to_run = "business_mean", custom_models = list(business_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  expect_error(train_models(info, run_global_models = FALSE, negative_forecast = TRUE), "enabled mode")
  train_models(info, run_local_models = FALSE, run_global_models = TRUE, negative_forecast = TRUE)
  paths <- finnts:::local_artifact_path(info, "forecasts", "-global_models", vapply(c("A", "B"), finnts:::hash_data, character(1)))
  rows <- finnts:::read_exact_artifact(info, paths)
  expect_equal(rows$Forecast, rep(-150, nrow(rows)))
  expect_identical(unique(rows$Model_Type), "global")
})

test_that("custom forecasts floor negative output at zero when negative_forecast is FALSE", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model(modes = "global", offset = -300)
  prep_models(info,
    models_to_run = "business_mean", custom_models = list(business_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  train_models(info, run_local_models = FALSE, run_global_models = TRUE, negative_forecast = FALSE)
  paths <- finnts:::local_artifact_path(info, "forecasts", "-global_models", vapply(c("A", "B"), finnts:::hash_data, character(1)))
  rows <- finnts:::read_exact_artifact(info, paths)
  expect_gt(nrow(rows), 0L)
  expect_equal(rows$Forecast, rep(0, nrow(rows)))
  expect_identical(unique(rows$Model_Type), "global")
})
