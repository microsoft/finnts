# Integration test: cubist multistep horizon fit ----

test_that("multistep_horizon monthly data with cubist", {
  data <- timetk::m4_monthly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "M2",
      Date >= "2012-01-01"
    )

  run_info <- set_run_info()

  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 2,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    back_test_scenarios = 2,
    models_to_run = "cubist",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1,
    pca = TRUE
  )

  train_models(run_info = run_info)

  workflow_tbl <- get_trained_models(run_info = run_info) %>%
    dplyr::select(Model_Fit)

  model_length <- length(workflow_tbl$Model_Fit[[1]]$fit$fit$fit$models)

  expect_equal(model_length, 2)
})

# Integration test: xgboost multistep horizon fit and predict ----

test_that("xgboost multistep_horizon monthly data", {
  data <- timetk::m4_monthly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "M2",
      Date >= "2012-01-01"
    )

  run_info <- set_run_info()

  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 2,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    back_test_scenarios = 2,
    models_to_run = "xgboost",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1,
    pca = TRUE
  )

  train_models(run_info = run_info)

  workflow_tbl <- get_trained_models(run_info = run_info) %>%
    dplyr::select(Model_Fit)

  fitted_model <- workflow_tbl$Model_Fit[[1]]$fit$fit$fit

  # trained model should have 2 sub-models (one per horizon step)
  expect_equal(length(fitted_model$models), 2)

  # model names should follow "model_lag_N" pattern
  expect_true(all(grepl("^model_lag_\\d+$", names(fitted_model$models))))

  # fitted values should exist and be numeric
  expect_true(is.numeric(fitted_model$data$.fitted))
  expect_true(all(!is.na(fitted_model$data$.fitted)))

  # residuals should be computed
  expect_true(is.numeric(fitted_model$data$.residuals))
})

test_that("xgboost multistep_horizon forecast_horizon = 3", {
  data <- timetk::m4_monthly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "M2",
      Date >= "2012-01-01"
    )

  run_info <- set_run_info()

  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 3,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    back_test_scenarios = 2,
    models_to_run = "xgboost",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1,
    pca = TRUE
  )

  train_models(run_info = run_info)

  workflow_tbl <- get_trained_models(run_info = run_info) %>%
    dplyr::select(Model_Fit)

  fitted_model <- workflow_tbl$Model_Fit[[1]]$fit$fit$fit

  # with forecast_horizon = 3, should have 3 sub-models
  expect_equal(length(fitted_model$models), 3)
})

# Unit tests: xgb version-safe accessors ----

test_that("xgb_get_feature_names returns feature names from model", {
  x <- matrix(rnorm(100), ncol = 2)
  colnames(x) <- c("feat_a", "feat_b")
  y <- rnorm(50)

  dtrain <- xgboost::xgb.DMatrix(data = x, label = y)
  model <- xgboost::xgb.train(
    params = list(max_depth = 2, eta = 0.3, objective = "reg:squarederror", nthread = 1),
    data = dtrain,
    nrounds = 3,
    verbose = 0
  )

  feat_names <- xgb_get_feature_names(model)
  expect_equal(sort(feat_names), c("feat_a", "feat_b"))
})

test_that("xgb_get_niter returns iteration count", {
  x <- matrix(rnorm(100), ncol = 2)
  colnames(x) <- c("f1", "f2")
  y <- rnorm(50)

  dtrain <- xgboost::xgb.DMatrix(data = x, label = y)
  model <- xgboost::xgb.train(
    params = list(max_depth = 2, eta = 0.3, objective = "reg:squarederror", nthread = 1),
    data = dtrain,
    nrounds = 5,
    verbose = 0
  )

  n_iter <- xgb_get_niter(model)
  expect_equal(n_iter, 5)
})

# Unit tests: predict edge cases ----

test_that("xgboost predict returns correct number of predictions", {
  data <- timetk::m4_monthly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "M2",
      Date >= "2012-01-01"
    )

  run_info <- set_run_info()

  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 2,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    back_test_scenarios = 2,
    models_to_run = "xgboost",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1,
    pca = TRUE
  )

  train_models(run_info = run_info)

  final_models(run_info = run_info)

  forecast_tbl <- get_forecast_data(run_info = run_info)

  expect_gt(nrow(forecast_tbl), 0)

  fcst_values <- forecast_tbl %>%
    dplyr::filter(Run_Type == "Future_Forecast") %>%
    dplyr::pull(Forecast)

  expect_true(is.numeric(fcst_values))
  expect_true(all(!is.na(fcst_values)))
})

test_that("xgboost multistep prediction count matches horizon", {
  data <- timetk::m4_monthly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "M2",
      Date >= "2012-01-01"
    )

  fh <- 2
  run_info <- set_run_info()

  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    forecast_horizon = fh,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    back_test_scenarios = 2,
    models_to_run = "xgboost",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1,
    pca = TRUE
  )

  train_models(run_info = run_info)
  final_models(run_info = run_info)

  forecast_tbl <- get_forecast_data(run_info = run_info)

  forecast_counts <- forecast_tbl %>%
    dplyr::filter(Run_Type == "Future_Forecast", Best_Model == "Yes") %>%
    dplyr::group_by(Combo) %>%
    dplyr::summarize(n = dplyr::n(), .groups = "drop")

  expect_true(all(forecast_counts$n == fh))
})
