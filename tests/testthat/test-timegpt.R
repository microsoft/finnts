# tests/testthat/test-timegpt.R

# Skip all tests in this file on CRAN
skip_on_cran()

# Helper function to check TimeGPT credentials are available
has_timegpt_credentials <- function() {
  azure_url <- Sys.getenv("NIXTLA_BASE_URL", unset = NA)
  api_key <- Sys.getenv("NIXTLA_API_KEY", unset = NA)

  # Check if credentials exist
  if (is.na(azure_url) || is.na(api_key) ||
    !nzchar(azure_url) || !nzchar(api_key)) {
    return(FALSE)
  }
  return(TRUE)
}

test_that("TimeGPT API key validation", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")

  # Get credentials
  azure_url <- Sys.getenv("NIXTLA_BASE_URL")
  api_key <- Sys.getenv("NIXTLA_API_KEY")

  # Normalize URL
  azure_url <- finnts:::normalize_url(azure_url)
  Sys.setenv(NIXTLA_BASE_URL = azure_url)

  # Setup client
  nixtlar::nixtla_client_setup(base_url = azure_url, api_key = api_key)

  # Validate API key - expect TRUE
  expect_true(nixtlar::nixtla_validate_api_key())
})

test_that("TimeGPT model can be initialized", {
  model <- timegpt_model(forecast_horizon = 6)

  expect_s3_class(model, "timegpt_model")
  expect_equal(model$mode, "regression")
})

test_that("TimeGPT fit function works without external regressors", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Simple data
  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1"
  )
  y <- rnorm(48, mean = 100, sd = 10)

  fit <- timegpt_model_fit_impl(x, y, forecast_horizon = 3)

  expect_s3_class(fit, "timegpt_model_fit")
  expect_true("train_data" %in% names(fit))
  expect_equal(nrow(fit$train_data), 48)
})

test_that("TimeGPT predict function works without external regressors", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Training data
  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1"
  )
  y <- rnorm(48, mean = 100, sd = 10)

  fit <- timegpt_model_fit_impl(x, y, forecast_horizon = 3)

  # Prediction data
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1"
  )

  preds <- timegpt_model_predict_impl(fit, new_data)

  expect_type(preds, "double")
  expect_length(preds, 3)
  expect_true(all(!is.na(preds)))
})

test_that("TimeGPT works with historical-only external regressors through parsnip workflow", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Training data with external regressor
  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5)
  )

  # Get recipe (this tests recipe creation)
  recipe_spec <- get_recipe_timegpt(train_data)

  # Create model spec
  model_spec <- timegpt_model(forecast_horizon = 3) %>%
    parsnip::set_engine("timegpt_model")

  # Create workflow (this is the full pipeline)
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  # Fit the workflow (tests recipe prep + model fit)
  wf_fit <- parsnip::fit(wf, data = train_data)

  # Prediction data (external regressor is NA - historical only)
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = NA_real_
  )

  # Predict (tests full predict pipeline with recipe)
  preds <- predict(wf_fit, new_data)


  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("TimeGPT works with future external regressors (X_df) through parsnip workflow", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Training data with external regressor
  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5)
  )

  # Get recipe
  recipe_spec <- get_recipe_timegpt(train_data)

  # Create model spec
  model_spec <- timegpt_model(forecast_horizon = 3) %>%
    parsnip::set_engine("timegpt_model")

  # Create workflow
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  # Fit the workflow
  wf_fit <- parsnip::fit(wf, data = train_data)

  # Prediction data with future external regressor values
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = c(22, 23, 21)
  )

  # Predict
  preds <- predict(wf_fit, new_data)

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("TimeGPT works with mixed external regressors (X_df + hist_exog_list) through parsnip workflow", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Training data with multiple external regressors
  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5),
    holiday_original = sample(0:1, 48, replace = TRUE)
  )

  # Get recipe
  recipe_spec <- get_recipe_timegpt(train_data)

  # Create model spec
  model_spec <- timegpt_model(forecast_horizon = 3) %>%
    parsnip::set_engine("timegpt_model")

  # Create workflow
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  # Fit the workflow
  wf_fit <- parsnip::fit(wf, data = train_data)

  # Prediction data: temperature has future values, holiday does not
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = c(22, 23, 21),
    holiday_original = NA_real_
  )

  # Predict
  preds <- predict(wf_fit, new_data)

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("TimeGPT handles one-hot encoded external regressors through parsnip workflow", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Training data with categorical that will be one-hot encoded
  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    region_original = sample(c("North", "South", "East"), 48, replace = TRUE)
  )

  # Get recipe (will one-hot encode region_original)
  recipe_spec <- get_recipe_timegpt(train_data)

  # Create model spec
  model_spec <- timegpt_model(forecast_horizon = 3) %>%
    parsnip::set_engine("timegpt_model")

  # Create workflow
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  # Fit the workflow
  wf_fit <- parsnip::fit(wf, data = train_data)

  # Prediction data with future values
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    region_original = c("North", "South", "North")
  )

  # Predict
  preds <- predict(wf_fit, new_data)

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("TimeGPT handles one-hot encoded external regressors (historical-only) through parsnip workflow", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Training data with categorical that will be one-hot encoded
  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    region_original = sample(c("North", "South", "East"), 48, replace = TRUE)
  )

  # Get recipe (will one-hot encode region_original)
  recipe_spec <- get_recipe_timegpt(train_data)

  # Create model spec
  model_spec <- timegpt_model(forecast_horizon = 3) %>%
    parsnip::set_engine("timegpt_model")

  # Create workflow
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  # Fit the workflow
  wf_fit <- parsnip::fit(wf, data = train_data)

  # Prediction data WITHOUT future values (historical only)
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    region_original = NA_character_ # No future values - use hist_exog_list
  )

  # Predict
  preds <- predict(wf_fit, new_data)

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("TimeGPT handles mixed numeric and categorical external regressors through parsnip workflow", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Training data with both numeric and categorical external regressors
  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5),
    region_original = sample(c("North", "South", "East"), 48, replace = TRUE)
  )

  # Get recipe (will one-hot encode region_original, keep temperature_original numeric)
  recipe_spec <- get_recipe_timegpt(train_data)

  # Create model spec
  model_spec <- timegpt_model(forecast_horizon = 3) %>%
    parsnip::set_engine("timegpt_model")

  # Create workflow
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  # Fit the workflow
  wf_fit <- parsnip::fit(wf, data = train_data)

  # Prediction data with both numeric and categorical future values
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = c(22, 23, 21),
    region_original = c("North", "South", "North")
  )

  # Predict
  preds <- suppressWarnings(predict(wf_fit, new_data))

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("TimeGPT handles mixed numeric and categorical external regressors with partial future values", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Training data with both numeric and categorical external regressors
  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5),
    region_original = sample(c("North", "South", "East"), 48, replace = TRUE)
  )

  # Get recipe
  recipe_spec <- get_recipe_timegpt(train_data)

  # Create model spec
  model_spec <- timegpt_model(forecast_horizon = 3) %>%
    parsnip::set_engine("timegpt_model")

  # Create workflow
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  # Fit the workflow
  wf_fit <- parsnip::fit(wf, data = train_data)

  # Prediction data: temperature has future values, region is historical-only
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = c(22, 23, 21),
    region_original = NA_character_ # Historical-only
  )

  # Predict
  preds <- suppressWarnings(predict(wf_fit, new_data))

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("TimeGPT Model pipeline Integration test with external regressors", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Mock data setup with only numeric external regressor
  data <- timetk::m4_monthly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "M2",
      Date >= "2010-01-01"
    ) %>%
    dplyr::mutate(
      temperature = rnorm(dplyr::n(), mean = 20, sd = 5)
    )


  run_info <- set_run_info()

  # prep data
  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 3,
    external_regressors = c("temperature"),
  )

  # prep models
  prep_models(
    run_info = run_info,
    models_to_run = "timegpt",
  )

  # train models
  train_models(
    run_info = run_info,
    run_global_models = FALSE
  )

  # pull trained model
  trained_models <- get_trained_models(run_info = run_info)

  # Assertions
  expect_s3_class(trained_models, "data.frame")
  expect_true(nrow(trained_models) > 0)
  expect_true("Model_Fit" %in% colnames(trained_models))

  # Extract the actual fit from the workflow
  fit_obj <- trained_models$Model_Fit[[1]]$fit$fit$fit # Navigate through workflow structure
  expect_s3_class(fit_obj, "timegpt_model_fit")
  expect_true("train_data" %in% names(fit_obj))

  # Check that external regressors are present in training data
  train_data_cols <- colnames(fit_obj$train_data)
  expect_true(any(grepl("temperature_original", train_data_cols)))
})


test_that("TimeGPT Model pipeline Integration test with future external regressors", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")


  # Historical data (up to 2015-06-01)
  hist_data <- timetk::m4_monthly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "M2",
      Date >= "2010-01-01",
      Date <= "2015-06-01"
    ) %>%
    dplyr::mutate(
      temperature = rnorm(dplyr::n(), mean = 20, sd = 5)
    )

  # Future data (beyond hist_end_date) with temperature values
  future_data <- tibble::tibble(
    Date = seq.Date(as.Date("2015-07-01"), by = "month", length.out = 3),
    id = "M2",
    value = NA_real_, # No future target values
    temperature = c(22, 23, 21) # FUTURE external regressor values
  )

  # Combine historical and future
  data <- dplyr::bind_rows(hist_data, future_data)


  run_info <- set_run_info()

  # prep data
  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 3,
    external_regressors = c("temperature"),
    hist_end_date = as.Date("2015-06-01")
  )

  # prep models
  prep_models(
    run_info = run_info,
    models_to_run = "timegpt",
  )

  # train models
  train_models(
    run_info = run_info,
    run_global_models = FALSE
  )

  ensemble_models(run_info = run_info)
  final_models(run_info = run_info)
  # Get forecasts

  forecasts <- get_forecast_data(run_info = run_info) %>%
    dplyr::filter(Date > as.Date("2015-06-01"))

  # Assertions
  expect_s3_class(forecasts, "data.frame")
  expect_equal(nrow(forecasts), 3)
  expect_true(all(!is.na(forecasts$Forecast)))
  trained_models <- get_trained_models(run_info = run_info)
  fit_obj <- trained_models$Model_Fit[[1]]$fit$fit$fit
  expect_true("train_data" %in% names(fit_obj))
  expect_true(any(grepl("temperature_original", colnames(fit_obj$train_data))))
})


test_that("Predictor variables for non TimeGPT exclude *_original columns", {
  set.seed(123)
  data <- timetk::m4_monthly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date, value = value) %>%
    dplyr::filter(id == "M2", Date >= as.Date("2010-01-01"), Date <= as.Date("2012-12-01")) %>%
    dplyr::mutate(
      temperature = rnorm(dplyr::n(), 20, 5)
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
    external_regressors = c("temperature")
  )

  prep_models(
    run_info = run_info,
    models_to_run = c("xgboost", "cubist"),
    num_hyperparameters = 1
  )

  # Extract all workflows and R1 training data
  pm <- get_prepped_models(run_info)
  wf_tbl <- pm %>%
    dplyr::filter(Type == "Model_Workflows") %>%
    dplyr::pull(Data) %>%
    .[[1]]

  r1_train <- get_prepped_data(run_info = run_info, recipe = "R1")

  check_predictors <- function(model_name) {
    # extract specific workflow
    wf <- wf_tbl %>%
      dplyr::filter(Model_Name == model_name, Model_Recipe == "R1") %>%
      dplyr::pull(Model_Workflow) %>%
      .[[1]]
    rec <- workflows::extract_recipe(wf, estimated = FALSE)
    rec_prep <- recipes::prep(rec, training = r1_train)
    baked <- recipes::juice(rec_prep)
    preds <- setdiff(colnames(baked), "Target")

    expect_true(length(preds) > 0, info = paste("No predictors found for", model_name))
    expect_false(any(grepl("_original", preds)),
      info = paste("Predictors include *_original columns for", model_name)
    )
  }

  check_predictors("xgboost")
  check_predictors("cubist")
})

test_that("TimeGPT pads monthly data below minimum size for Azure", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")

  # Monthly data with only 25 rows (below minimum of 48 for monthly)
  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 25),
    Combo = "1"
  )
  y <- rnorm(25, mean = 100, sd = 10)

  # Fit with frequency = 12 (monthly)
  fit <- timegpt_model_fit_impl(x, y, forecast_horizon = 3, frequency = 12)

  # Check that data was padded (should have at least 48 rows)
  expect_gte(nrow(fit$train_data), 48,
    info = "Monthly data below 48 rows should be padded to at least 48 rows"
  )

  # Check that padding starts with zeros
  train_df <- fit$train_data
  # Find where padding starts (before original data)
  original_start_date <- min(x$Date)
  padded_rows <- train_df %>%
    dplyr::filter(Date < original_start_date)

  if (nrow(padded_rows) > 0) {
    expect_true(all(padded_rows$y == 0),
      info = "Padded rows should have y = 0"
    )
  }
})

test_that("TimeGPT does not pad data above minimum size", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")

  azure_url <- Sys.getenv("NIXTLA_BASE_URL", unset = NA)
  skip_if(
    is.na(azure_url) || !nzchar(azure_url) || !grepl("azure", azure_url, ignore.case = TRUE),
    "Azure endpoint not configured - skipping padding test"
  )

  # Monthly data with 60 rows (above minimum of 48)
  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 60),
    Combo = "1"
  )
  y <- rnorm(60, mean = 100, sd = 10)

  fit <- timegpt_model_fit_impl(x, y, forecast_horizon = 3, frequency = 12)

  # Data should not be padded (should have exactly 60 rows)
  expect_equal(nrow(fit$train_data), 60,
    info = "Data above minimum size should not be padded"
  )
})

test_that("TimeGPT padding works with multiple combos (global model)", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")

  azure_url <- Sys.getenv("NIXTLA_BASE_URL", unset = NA)
  skip_if(
    is.na(azure_url) || !nzchar(azure_url) || !grepl("azure", azure_url, ignore.case = TRUE),
    "Azure endpoint not configured - skipping padding test"
  )

  # Two combos, each with 25 rows (below minimum of 48)
  x <- data.frame(
    Date = rep(seq.Date(as.Date("2020-01-01"), by = "month", length.out = 25), 2),
    Combo = rep(c("M1", "M2"), each = 25)
  )
  y <- rnorm(50, mean = 100, sd = 10)

  fit <- timegpt_model_fit_impl(x, y, forecast_horizon = 3, frequency = 12)

  # Check that both combos were padded
  train_df <- fit$train_data
  combo_counts <- train_df %>%
    dplyr::count(Combo)

  expect_true(all(combo_counts$n >= 48),
    info = "All combos should be padded to at least minimum size"
  )
})

test_that("TimeGPT tune parameters are assigned actual values after finalization", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10)
  )

  recipe_spec <- get_recipe_timegpt(train_data)

  # Create model spec with tune::tune() placeholders
  model_spec <- timegpt_model(
    forecast_horizon = 6,
    frequency = 12,
    finetune_steps = tune::tune(),
    finetune_depth = tune::tune()
  ) %>%
    parsnip::set_engine("timegpt_model")

  # Create workflow
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  # Simulate what tune::finalize_workflow()
  finalized_spec <- wf %>%
    workflows::update_model(
      parsnip::set_args(
        model_spec,
        finetune_steps = 80,
        finetune_depth = 3
      )
    )

  # Extract finalized model spec
  finalized_model <- finalized_spec$fit$actions$model$spec

  finetune_steps_value <- rlang::eval_tidy(finalized_model$args$finetune_steps)
  finetune_depth_value <- rlang::eval_tidy(finalized_model$args$finetune_depth)

  # Check that they evaluate to the correct values
  expect_equal(finetune_steps_value, 80,
    info = "finetune_steps should evaluate to 80 after finalization"
  )
  expect_equal(finetune_depth_value, 3,
    info = "finetune_depth should evaluate to 3 after finalization"
  )


  wf_fit <- parsnip::fit(finalized_spec, data = train_data)

  fit_obj <- wf_fit$fit$fit$fit

  expect_equal(fit_obj$finetune_steps, 80,
    info = "finetune_steps value should be stored in fit object"
  )
  expect_equal(fit_obj$finetune_depth, 3,
    info = "finetune_depth value should be stored in fit object"
  )
})

test_that("TimeGPT uses long-horizon model for monthly forecasts > 24 months", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")

  # First verify the helper function detects long horizon correctly
  expect_true(finnts:::is_long_horizon_forecast(25, "month"),
    info = "Helper function: 25 months should be detected as long horizon (> 24)"
  )

  expect_false(finnts:::is_long_horizon_forecast(12, "month"),
    info = "Helper function: 12 months should NOT be detected as long horizon (< 24)"
  )

  # Now test the full pipeline with monthly data and forecast_horizon = 25 (> 24, so long horizon)
  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 60),
    Combo = "1"
  )
  y <- rnorm(60, mean = 100, sd = 10)

  fit <- timegpt_model_fit_impl(x, y, forecast_horizon = 25, frequency = 12)

  expect_equal(fit$forecast_horizon, 25)
  expect_equal(fit$frequency, 12)

  new_data <- data.frame(
    Date = seq.Date(as.Date("2025-01-01"), by = "month", length.out = 25),
    Combo = "1"
  )

  # The predict function should use long-horizon model
  # If it works, the model selection was correct
  preds <- timegpt_model_predict_impl(fit, new_data)

  expect_type(preds, "double")
  expect_length(preds, 25)
  expect_true(all(!is.na(preds)),
    info = "Long-horizon forecast should produce valid predictions"
  )
})
