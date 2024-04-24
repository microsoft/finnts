
test_that("multistep_horizon yearly data", {

  # Mock data setup
  data <- timetk::m4_yearly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(id == "Y1")

  run_info <- set_run_info()

  # prep data and models
  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "year",
    forecast_horizon = 4,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    back_test_scenarios = 4,
    models_to_run = "xgboost",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1, 
    pca = TRUE
  )

  # train models
  train_models(run_info = run_info)

  # pull trained model
  workflow_tbl <- get_trained_models(run_info = run_info) %>%
    dplyr::select(Model_Fit)

  model_length <- length(workflow_tbl$Model_Fit[[1]]$fit$fit$fit$models)

  # Assertions
  expect_equal(model_length, 4)
})

test_that("multistep_horizon quarterly data", {

  # Mock data setup
  data <- timetk::m4_quarterly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(id == "Q10")

  run_info <- set_run_info()

  # prep data and models
  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "quarter",
    forecast_horizon = 6,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    back_test_scenarios = 6,
    models_to_run = "mars",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1, 
    pca = TRUE
  )

  # train models
  train_models(
    run_info = run_info,
    feature_selection = TRUE
  )

  # pull trained model
  workflow_tbl <- get_trained_models(run_info = run_info) %>%
    dplyr::select(Model_Fit)

  model_length <- length(workflow_tbl$Model_Fit[[1]]$fit$fit$fit$models)

  # Assertions
  expect_equal(model_length, 5)
})

test_that("multistep_horizon monthly data", {

  # Mock data setup
  data <- timetk::m4_monthly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "M2",
      Date >= "2012-01-01"
    )

  run_info <- set_run_info()

  # prep data and models
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

  # train models
  train_models(run_info = run_info)

  # pull trained model
  workflow_tbl <- get_trained_models(run_info = run_info) %>%
    dplyr::select(Model_Fit)

  model_length <- length(workflow_tbl$Model_Fit[[1]]$fit$fit$fit$models)

  # Assertions
  expect_equal(model_length, 2)
})

test_that("multistep_horizon weekly data", {

  # Mock data setup
  data <- timetk::m4_weekly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "W10",
      Date >= "2014-01-01"
    )

  run_info <- set_run_info()

  # prep data and models
  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "week",
    forecast_horizon = 4,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    back_test_scenarios = 4,
    models_to_run = "glmnet",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1, 
    pca = TRUE
  )

  # train models
  train_models(run_info = run_info)

  # pull trained model
  workflow_tbl <- get_trained_models(run_info = run_info) %>%
    dplyr::select(Model_Fit)

  model_length <- length(workflow_tbl$Model_Fit[[1]]$fit$fit$fit$models)

  # Assertions
  expect_equal(model_length, 1)
})

test_that("multistep_horizon daily data", {

  # Mock data setup
  data <- timetk::m4_daily %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "D10",
      Date >= "2014-01-01"
    )

  run_info <- set_run_info()

  # prep data and models
  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "day",
    forecast_horizon = 30,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )

  prep_models(
    run_info = run_info,
    back_test_scenarios = 4,
    back_test_spacing = 7,
    models_to_run = "glmnet",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1, 
    pca = TRUE
  )

  # train models
  train_models(run_info = run_info)

  # pull trained model
  workflow_tbl <- get_trained_models(run_info = run_info) %>%
    dplyr::select(Model_Fit)

  model_length <- length(workflow_tbl$Model_Fit[[1]]$fit$fit$fit$models)

  # Assertions
  expect_equal(model_length, 2)
})
