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
  skip_on_cran()

  model <- timegpt_model(forecast_horizon = 6)

  expect_s3_class(model, "timegpt_model")
  expect_equal(model$mode, "regression")
})

test_that("TimeGPT fit function works without external regressors", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  forecasts <- get_forecast_data(run_info = run_info)

  forecasts <- get_forecast_data(run_info = run_info) %>%
    dplyr::filter(Date > as.Date("2015-06-01"))

  # Assertions
  expect_s3_class(forecasts, "data.frame")
  expect_equal(nrow(forecasts), 3)
  # expect_equal(nrow(forecasts), 3)
  expect_true(all(!is.na(forecasts$Forecast)))

  trained_models <- get_trained_models(run_info = run_info)
  fit_obj <- trained_models$Model_Fit[[1]]$fit$fit$fit
  expect_true("train_data" %in% names(fit_obj))
  expect_true(any(grepl("temperature_original", colnames(fit_obj$train_data))))
})
