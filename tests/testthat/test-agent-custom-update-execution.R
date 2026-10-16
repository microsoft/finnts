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
