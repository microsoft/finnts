test_that("local and global custom training retain arithmetic and version identity", {
  info <- standard_run(withr::local_tempdir())
  model <- standard_custom_model()
  prep_models(info,
    models_to_run = c("business_mean", "meanf"), custom_models = list(business_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  train_models(info, run_global_models = TRUE, run_local_models = TRUE, negative_forecast = TRUE)
  paths <- finnts:::local_artifact_path(info, "forecasts", "-single_models", combo = vapply(
    c("A", "B"), finnts:::hash_data,
    character(1)
  ), extension = "csv")
  local <- finnts:::read_exact_artifact(info, paths)
  custom <- local[local$Model_Name == "business_mean", ]
  expect_setequal(custom$Combo, c("A", "B"))
  expect_equal(custom$Forecast, ifelse(custom$Combo == "A", 100, 200))
  expect_identical(unique(custom$Model_ID), paste0("custom-", model$definition$version_id, "--local--C1"))
  expect_true("meanf" %in% local$Model_Name)
  global <- finnts:::read_exact_artifact(info, finnts:::local_artifact_path(info, "forecasts", "-global_models", combo = vapply(c(
    "A",
    "B"
  ), finnts:::hash_data, character(1)), extension = "csv"))
  expect_equal(global$Forecast, rep(150, nrow(global)))
  expect_identical(unique(global$Model_ID), paste0("custom-", model$definition$version_id, "--global--C1"))
  final_models(info, average_models = FALSE)
  forecast <- get_forecast_data(info)
  expect_true(any(forecast$Best_Model == "Yes"))
  expect_true("business_mean" %in% forecast$Model_Name)
  before <- tools::md5sum(paths)
  prep_models(info,
    models_to_run = c("meanf", "business_mean"), custom_models = list(business_mean = model), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1
  )
  train_models(info, run_global_models = TRUE, run_local_models = TRUE, negative_forecast = TRUE)
  final_models(info, average_models = FALSE)
  expect_identical(tools::md5sum(paths), before)
  changed <- standard_custom_model(offset = 1)
  expect_error(prep_models(info,
    models_to_run = c("meanf", "business_mean"), custom_models = list(business_mean = changed),
    run_ensemble_models = FALSE
  ), "changed|new run")
  expect_error(prep_models(info, models_to_run = "meanf", run_ensemble_models = FALSE), "same approved")
  stale <- finnts:::read_exact_artifact(info, paths[[1]])
  stale$Model_ID[stale$Model_Name == "business_mean"] <- paste0("custom-", changed$definition$version_id, "--local--C1")
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

test_that("high-level custom-only forecasts preserve aliases and leave C1 untransformed", {
  model <- standard_custom_model(modes = "local")
  info <- set_run_info(
    project_name = "custom-wrapper", run_name = "test", path = withr::local_tempdir(), data_output = "csv",
    object_output = "rds", add_unique_id = FALSE
  )
  args <- list(
    run_info = info, input_data = standard_input, combo_variables = "id", target_variable = "value", date_type = "month",
    forecast_horizon = 2, models_to_run = "business_mean", custom_models = list(business_mean = model), recipes_to_run = "C1",
    stationary = FALSE, clean_missing_values = FALSE, run_ensemble_models = FALSE, negative_forecast = TRUE, run_global_models = FALSE,
    back_test_scenarios = 1, average_models = FALSE, return_data = FALSE
  )
  args$stationary <- TRUE
  do.call(forecast_time_series, args)
  prepared <- setdiff(list.files(fs::path(info$path, "prep_data")), grep("orig_combo_info", list.files(fs::path(info$path, "prep_data")), value = TRUE))
  expect_gt(length(prepared), 0L)
  expect_true(all(grepl("-C1\\.csv$", prepared)))
  result <- get_forecast_data(info)
  expect_identical(unique(result$Model_Name), "business_mean")
  expect_true(all(result$Best_Model == "Yes"))
  expect_equal(result$Forecast, ifelse(result$Combo == "A", 100, 200))
})

test_that("CSV custom finalization keeps numeric-looking combo labels as text", {
  model <- standard_custom_model(modes = "local")
  info <- set_run_info(
    project_name = "custom-numeric-combo", run_name = "test", path = withr::local_tempdir(), data_output = "csv",
    object_output = "rds", add_unique_id = FALSE
  )
  input <- standard_input
  input$id <- ifelse(input$id == "A", "17", "007")
  forecast_time_series(
    run_info = info, input_data = input, combo_variables = "id", target_variable = "value", date_type = "month",
    forecast_horizon = 2, models_to_run = "business_mean", custom_models = list(business_mean = model),
    recipes_to_run = "C1", stationary = FALSE, clean_missing_values = FALSE, run_ensemble_models = FALSE,
    negative_forecast = TRUE, run_global_models = FALSE, back_test_scenarios = 1, average_models = FALSE,
    return_data = FALSE
  )
  result <- get_forecast_data(info)
  expect_setequal(unique(as.character(result$Combo)), c("17", "007"))
  expect_true(all(result$Best_Model == "Yes"))
  expect_equal(result$Forecast, ifelse(as.character(result$Combo) == "17", 100, 200))
})
