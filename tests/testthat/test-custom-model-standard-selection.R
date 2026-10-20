test_that("custom-only averages retain exact components and reject foreign saved identities", {
  info <- standard_run(withr::local_tempdir())
  models <- list(low_rule = standard_custom_model("low_rule", "local", -10), high_rule = standard_custom_model(
    "high_rule",
    "local", 10
  ))
  prep_models(info,
    models_to_run = names(models), custom_models = models, run_ensemble_models = FALSE, back_test_scenarios = 1,
    num_hyperparameters = 1
  )
  train_models(info, negative_forecast = TRUE)
  final_models(info)
  result <- get_forecast_data(info)
  average <- result[result$Recipe_ID == "simple_average", ]
  expect_gt(nrow(average), 0L)
  expect_true(all(average$Best_Model == "Yes"))
  expect_equal(average$Forecast, ifelse(average$Combo == "A", 100, 200))
  expect_setequal(result$Model_Name[result$Recipe_ID == "C1"], names(models))
  before <- result
  final_models(info)
  expect_equal(get_forecast_data(info), before)
  average_path <- finnts:::local_artifact_path(info, "forecasts", "-average_models", finnts:::hash_data("A"))
  rows <- finnts:::read_exact_artifact(info, average_path)
  rows$Model_ID <- sub("custom-[a-f0-9]+", "arima", rows$Model_ID)
  finnts:::write_data(rows, "A", info, "data", "forecasts", "-average_models")
  expect_error(final_models(info), "component identity")
})
