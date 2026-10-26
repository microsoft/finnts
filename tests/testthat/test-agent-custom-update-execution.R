test_that("real saved-workflow refits classify only recoverable model execution", {
  info <- standard_run(withr::local_tempdir())
  prep_models(info, models_to_run = "meanf", run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1)
  train_models(info, run_local_models = TRUE, run_global_models = FALSE)
  trained <- read_exact_artifact(info,
    local_artifact_path(info, "models", "-single_models", hash_data("A"), "rds"),
    return_type = "object")
  prepared <- get_prepped_models(info)
  splits <- prepared %>% dplyr::filter(Type == "Train_Test_Splits") %>%
    dplyr::select(Data) %>% tidyr::unnest(Data)
  parameters <- prepared %>% dplyr::filter(Type == "Model_Hyperparameters") %>%
    dplyr::select(Data) %>% tidyr::unnest(Data)
  arguments <- list(run_info = info, combo = "A", combo_info_tbl = tibble::tibble(),
    trained_models_tbl = trained, model_train_test_tbl = splits,
    model_hyperparameter_tbl = parameters, prev_run_log_tbl = read_selection_file(info, "logs"),
    forecast_horizon = 2, retune_hyperparameters = FALSE, num_cores = 1,
    inner_parallel = FALSE, seed = 123)
  # Inject the fault only at the actual model fit, after artifact/config checks.
  testthat::local_mocked_bindings(
    fit = function(...) stop("Numerical fit failure"), .package = "generics")
  expect_error(do.call(fit_models, arguments), "Numerical fit failure",
    class = "finnts_update_model_error")
  testthat::local_mocked_bindings(
    fit = function(...) rlang::abort("Hard artifact failure", class = "finnts_update_artifact_error"),
    .package = "generics")
  expect_error(do.call(fit_models, arguments), "Hard artifact failure",
    class = "finnts_update_artifact_error")
})

test_that("local and global saved-workflow updates preserve historical prediction dispatch", {
  info <- set_run_info("historical-update-dispatch", "test", path = withr::local_tempdir(),
    data_output = "csv", object_output = "rds", add_unique_id = FALSE)
  data <- custom_driver_data(2L)
  data$Units <- rep(c(rep(110, 24), NA, NA), 2)
  data$Price <- rep(c(rep(10, 24), 12, 13), 2)
  args <- custom_driver_prep(info, data, xregs = NULL)
  args$recipes_to_run <- "C1"
  do.call(prep_data, args)
  model <- historical_driver_model()
  prep_models(info, models_to_run = "unit_growth", custom_models = list(unit_growth = model),
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  log <- read_selection_file(info, "logs")
  log$negative_forecast <- TRUE
  info$custom_agent_run <- custom_run_load(info, log)
  prepared <- get_prepped_models(info)
  splits <- prepared %>% dplyr::filter(Type == "Train_Test_Splits") %>%
    dplyr::select(Data) %>% tidyr::unnest(Data)
  panel <- get_prepped_data(info, recipe = "C1")
  parameters <- prepared %>% dplyr::filter(Type == "Model_Hyperparameters") %>%
    dplyr::select(Data) %>% tidyr::unnest(Data)
  # Finalize one predecessor fit per mode; no training pipeline is needed just
  # to obtain the saved input for the production update fitter.
  for (mode in c("local", "global")) {
    selected <- if (mode == "local") panel[panel$Combo == "series1", ] else panel
    history <- selected[!is.na(selected$Target), ]
    rows <- custom_run_workflows(model$definition, history,
      list(date_type = "month", forecast_horizon = 2))
    workflow <- generics::fit(rows$Model_Workflow[[match(mode, rows$Model_Type)]], history)
    models <- tibble::tibble(Model_ID = paste0("custom-", model$definition$version_id, "--", mode, "--C1"),
      Model_Name = "unit_growth", Model_Type = mode, Recipe_ID = "C1", Model_Fit = list(workflow))
    updated <- fit_models(info, if (mode == "global") "All-Data" else "series1",
      tibble::tibble(), models, splits, parameters, log, 2, FALSE, 1, FALSE)
    path <- withr::local_tempfile(fileext = ".rds")
    saveRDS(updated$Model_Fit[[1]], path)
    restored <- readRDS(path)
    expect_s3_class(restored, "finnts_custom_workflow")
    request <- selected[is.na(selected$Target), c("Date", "Combo", "Price")]
    expect_equal(predict(restored, request)$.pred, rep(c(1452, 1730.3), if (mode == "global") 2 else 1),
      tolerance = 1e-8)
  }
})
