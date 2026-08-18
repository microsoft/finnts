skip_on_cran()

run_path <- tempfile("finnts-summary-feature-selection-")
dir.create(run_path)

summary_feature_data <- timetk::m4_monthly %>%
  dplyr::mutate(id = as.character(id)) %>%
  dplyr::rename(Date = date) %>%
  dplyr::filter(id == "M2", Date >= "2013-01-01") %>%
  dplyr::mutate(
    xreg1 = value * 0.5 + seq_len(dplyr::n()) / 100,
    xreg2 = value * -0.3 + rev(seq_len(dplyr::n())) / 100
  )

run_info <- set_run_info(
  project_name = "summary_feature_selection",
  run_name = "multistep_xregs",
  path = run_path,
  add_unique_id = FALSE
)

prep_data(
  run_info = run_info,
  input_data = summary_feature_data,
  combo_variables = "id",
  target_variable = "value",
  date_type = "month",
  forecast_horizon = 3,
  external_regressors = c("xreg1", "xreg2"),
  stationary = TRUE,
  lag_periods = c(1, 2, 4),
  recipes_to_run = "R1",
  multistep_horizon = TRUE
)

expected_models <- c("xgboost", "cubist", "glmnet", "mars", "svm-poly", "svm-rbf")
prep_models(
  run_info = run_info,
  back_test_scenarios = 1,
  models_to_run = expected_models,
  run_ensemble_models = FALSE,
  num_hyperparameters = 1,
  pca = TRUE
)
train_models(run_info, feature_selection = TRUE)

trained_models <- get_trained_models(run_info)
forecast <- read_unselected_forecast_data(run_info)$forecast

test_that("multistep models combine stationarity and feature selection", {
  expect_setequal(unique(trained_models$Model_Name), expected_models)
  expect_setequal(unique(forecast$Model_Name), expected_models)
  expect_true(all(is.finite(forecast$Forecast)))

  for (model_name in expected_models) {
    workflow <- get_model_workflow(trained_models, model_name)
    fitted_model <- workflows::extract_fit_engine(workflow)
    spec <- workflows::extract_spec_parsnip(workflow)
    selected_features <- rlang::eval_tidy(spec$args$selected_features)

    expect_equal(length(fitted_model$models), 3, info = model_name)
    expect_setequal(
      names(selected_features),
      c("model_lag_1", "model_lag_2", "model_lag_4")
    )
    expect_true(all(lengths(selected_features) > 0), info = model_name)
    expect_false(
      any(grepl("lag1($|_)", selected_features$model_lag_2)),
      info = model_name
    )
    expect_false(
      any(grepl("lag(1|2)($|_)", selected_features$model_lag_4)),
      info = model_name
    )
  }
})

test_that("multistep feature-selected model summaries retain predictors", {
  summarizers <- list(
    xgboost = summarize_model_xgboost,
    cubist = summarize_model_cubist,
    glmnet = summarize_model_glmnet,
    mars = summarize_model_mars,
    `svm-poly` = summarize_model_svm_poly,
    `svm-rbf` = summarize_model_svm_rbf
  )

  for (model_name in names(summarizers)) {
    result <- summarizers[[model_name]](
      get_model_workflow(trained_models, model_name)
    )
    validate_summary_output(result, model_name)
    model_type <- result %>%
      dplyr::filter(section == "engine_param", name == "model_type") %>%
      dplyr::pull(value)
    n_models <- result %>%
      dplyr::filter(section == "engine_param", name == "n_models") %>%
      dplyr::pull(value)
    expect_identical(model_type[[1]], "Multistep Horizon", info = model_name)
    expect_identical(n_models[[1]], "3", info = model_name)
    expect_gt(
      sum(result$section == "predictor"),
      0
    )
  }
})

unlink(run_path, recursive = TRUE)