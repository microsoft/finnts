# tests/testthat/test-chronos.R

# Skip all tests in this file on CRAN
skip_on_cran()

has_chronos_credentials <- function() {
  api_url <- Sys.getenv("CHRONOS_API_URL", unset = NA)
  api_token <- Sys.getenv("CHRONOS_API_TOKEN", unset = NA)

  if (is.na(api_url) || is.na(api_token) ||
    !nzchar(api_url) || !nzchar(api_token)) {
    return(FALSE)
  }
  return(TRUE)
}

# Chronos 2 Model Unit Tests (no API call needed)

test_that("Chronos 2 model can be initialized", {
  model <- chronos2_model(forecast_horizon = 6)

  expect_s3_class(model, "chronos2_model")
  expect_equal(model$mode, "regression")
})

test_that("Chronos 2 fit function stores training data and parameters", {
  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    Combo = "1"
  )
  y <- rnorm(10, mean = 100, sd = 10)

  fit <- chronos2_model_fit_impl(x, y, forecast_horizon = 3, frequency = 12)

  expect_s3_class(fit, "chronos2_model_fit")
  expect_true("train_data" %in% names(fit))
  expect_equal(nrow(fit$train_data), 10)
  expect_equal(fit$forecast_horizon, 3)
  expect_equal(fit$frequency, 12)
  expect_true("y" %in% colnames(fit$train_data))
  expect_equal(fit$train_data$y, y)
})

test_that("Chronos 2 fit function rejects empty data", {
  x <- data.frame(Date = as.Date(character(0)), Combo = character(0))
  y <- numeric(0)

  expect_error(
    chronos2_model_fit_impl(x, y, forecast_horizon = 3),
    "at least one row"
  )
})
######
test_that("Chronos 2 fit preserves external regressors", {
  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    Combo = "1",
    temperature_original = rnorm(10, mean = 20, sd = 5)
  )
  y <- rnorm(10, mean = 100, sd = 10)

  fit <- chronos2_model_fit_impl(x, y, forecast_horizon = 3)

  expect_true("temperature_original" %in% colnames(fit$train_data))
})

test_that("pad_chronos2_data pads combos with fewer than 3 rows", {
  train_df <- data.frame(
    Date = as.Date(c("2020-01-01", "2020-02-01")),
    Combo = "A",
    y = c(10, 20)
  )

  padded <- pad_chronos2_data(train_df, date_type = "month")

  # Should have at least 3 rows for combo "A"
  expect_true(nrow(padded) >= 3)

  # Original rows preserved
  original_rows <- padded %>% dplyr::filter(Date >= as.Date("2020-01-01"))
  expect_equal(nrow(original_rows), 2)
  expect_equal(sort(original_rows$y), c(10, 20))
})

test_that("pad_chronos2_data does not pad combos with 3+ rows", {
  train_df <- data.frame(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01")),
    Combo = "A",
    y = c(10, 20, 30, 40)
  )

  padded <- pad_chronos2_data(train_df, date_type = "month")

  expect_equal(nrow(padded), 4)
  expect_equal(padded$y, c(10, 20, 30, 40))
})

test_that("pad_chronos2_data sets y = 0 for padded rows", {
  train_df <- data.frame(
    Date = as.Date("2020-03-01"),
    Combo = "A",
    y = 42
  )

  padded <- pad_chronos2_data(train_df, date_type = "month")

  padded_rows <- padded %>% dplyr::filter(Date < as.Date("2020-03-01"))
  expect_true(all(padded_rows$y == 0))
})

test_that("pad_chronos2_data zeros out numeric exogenous columns in padded rows", {
  train_df <- data.frame(
    Date = as.Date("2020-03-01"),
    Combo = "A",
    y = 42,
    temperature_original = 25.0
  )

  padded <- pad_chronos2_data(train_df, date_type = "month")

  padded_rows <- padded %>% dplyr::filter(Date < as.Date("2020-03-01"))
  expect_true(all(padded_rows$temperature_original == 0))

  # Original value is preserved
  original_row <- padded %>% dplyr::filter(Date == as.Date("2020-03-01"))
  expect_equal(original_row$temperature_original, 25.0)
})

test_that("pad_chronos2_data works with multiple combos of different lengths", {
  train_df <- data.frame(
    Date = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
      "2020-01-01"
    )),
    Combo = c("A", "A", "A", "A", "B"),
    y = c(10, 20, 30, 40, 50)
  )

  padded <- pad_chronos2_data(train_df, date_type = "month")

  # Combo A already has 4 rows — should remain 4

  a_count <- sum(padded$Combo == "A")
  expect_equal(a_count, 4)

  # Combo B had 1 row — should be padded to 3
  b_count <- sum(padded$Combo == "B")
  expect_true(b_count >= 3)

  # Original B value preserved
  b_original <- padded %>% dplyr::filter(Combo == "B", Date == as.Date("2020-01-01"))
  expect_equal(b_original$y, 50)
})

test_that("pad_chronos2_data creates backward-stepping dates", {
  train_df <- data.frame(
    Date = as.Date(c("2020-03-01", "2020-04-01")),
    Combo = "A",
    y = c(10, 20)
  )

  padded <- pad_chronos2_data(train_df, date_type = "month")

  # Should have a date before 2020-03-01
  expect_true(min(padded$Date) < as.Date("2020-03-01"))
  # Padded date should be exactly one month before
  expect_equal(min(padded$Date), as.Date("2020-02-01"))
  # Data should be sorted
  expect_true(all(diff(padded$Date) >= 0))
})

test_that("pad_chronos2_data uses calendar-aware monthly steps for single-row data", {
  # Single row of monthly data — without date_type this would fall back to daily
  train_df <- data.frame(
    Date = as.Date("2020-03-01"),
    Combo = "A",
    y = 42
  )

  padded <- pad_chronos2_data(train_df, date_type = "month")

  expect_equal(nrow(padded), 3)
  # Padded dates should be exactly 1 and 2 months before
  expect_equal(
    sort(padded$Date),
    as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
  )
})

# Chronos 2 API Integration Tests

test_that("Chronos 2 fit function works without external regressors", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1"
  )
  y <- rnorm(48, mean = 100, sd = 10)

  fit <- chronos2_model_fit_impl(x, y, forecast_horizon = 3, frequency = 12)

  expect_s3_class(fit, "chronos2_model_fit")
  expect_true("train_data" %in% names(fit))
  expect_equal(nrow(fit$train_data), 48)
})

test_that("Chronos 2 predict function works without external regressors", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  x <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1"
  )
  y <- rnorm(48, mean = 100, sd = 10)

  fit <- chronos2_model_fit_impl(x, y, forecast_horizon = 3, frequency = 12)

  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1"
  )

  preds <- chronos2_model_predict_impl(fit, new_data)

  expect_type(preds, "double")
  expect_length(preds, 3)
  expect_true(all(!is.na(preds)))
})

test_that("Chronos 2 works with historical-only external regressors through parsnip workflow", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos2_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos2_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  # Prediction data (external regressor is NA — historical only)
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = NA_real_
  )

  preds <- predict(wf_fit, new_data)

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("Chronos 2 works with future external regressors through parsnip workflow", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos2_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos2_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = c(22.0, 23.0, 21.0)
  )

  preds <- predict(wf_fit, new_data)

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("Chronos 2 works with mixed external regressors through parsnip workflow", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5),
    holiday_original = sample(0:1, 48, replace = TRUE)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos2_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos2_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  # temperature has future values, holiday does not
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = c(22.0, 23.0, 21.0),
    holiday_original = NA_real_
  )

  preds <- predict(wf_fit, new_data)

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("Chronos 2 handles one-hot encoded external regressors through parsnip workflow", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    region_original = sample(c("North", "South", "East"), 48, replace = TRUE)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos2_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos2_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    region_original = c("North", "South", "North")
  )

  preds <- predict(wf_fit, new_data)

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("Chronos 2 handles one-hot encoded external regressors (historical-only) through parsnip workflow", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    region_original = sample(c("North", "South", "East"), 48, replace = TRUE)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos2_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos2_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    region_original = NA_character_
  )

  preds <- predict(wf_fit, new_data)

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("Chronos 2 handles mixed numeric and categorical external regressors through parsnip workflow", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5),
    region_original = sample(c("North", "South", "East"), 48, replace = TRUE)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos2_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos2_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = c(22.0, 23.0, 21.0),
    region_original = c("North", "South", "North")
  )

  preds <- suppressWarnings(predict(wf_fit, new_data))

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

test_that("Chronos 2 handles mixed numeric and categorical external regressors with partial future values", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5),
    region_original = sample(c("North", "South", "East"), 48, replace = TRUE)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)

  model_spec <- chronos2_model(forecast_horizon = 3, frequency = 12) %>%
    parsnip::set_engine("chronos2_model")

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec)

  wf_fit <- parsnip::fit(wf, data = train_data)

  # temperature has future values, region is historical-only
  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = c(22.0, 23.0, 21.0),
    region_original = NA_character_
  )

  preds <- suppressWarnings(predict(wf_fit, new_data))

  expect_s3_class(preds, "data.frame")
  expect_true(".pred" %in% colnames(preds))
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred)))
})

# Section 6 — Chronos 2 Full Pipeline Integration Tests

test_that("Chronos 2 Model pipeline integration test with external regressors", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

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

  prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 3,
    external_regressors = c("temperature"),
  )

  prep_models(
    run_info = run_info,
    models_to_run = "chronos2"
  )

  train_models(
    run_info = run_info,
    run_global_models = FALSE
  )

  trained_models <- get_trained_models(run_info = run_info)

  expect_s3_class(trained_models, "data.frame")
  expect_true(nrow(trained_models) > 0)
  expect_true("Model_Fit" %in% colnames(trained_models))

  fit_obj <- trained_models$Model_Fit[[1]]$fit$fit$fit
  expect_s3_class(fit_obj, "chronos2_model_fit")
  expect_true("train_data" %in% names(fit_obj))

  train_data_cols <- colnames(fit_obj$train_data)
  expect_true(any(grepl("temperature_original", train_data_cols)))
})

# Section 8 — chronos2_permutation_importance Unit Tests

test_that("chronos2_permutation_importance returns NULL when no _original columns", {
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12)
  y_vals <- rnorm(12, 100, 10)

  mold <- list(
    predictors = data.frame(Date = dates, Combo = "A"),
    outcomes = tibble::tibble(y = y_vals)
  )

  obj <- structure(
    list(train_data = data.frame(Date = dates, Combo = "A", y = y_vals),
         forecast_horizon = 3, frequency = 12),
    class = "chronos2_model_fit"
  )

  result <- chronos2_permutation_importance(chronos2_obj = obj, mold = mold)

  expect_null(result)
})

test_that("chronos2_permutation_importance returns NULL when required columns missing", {
  # Missing Date column in predictors
  mold <- list(
    predictors = data.frame(
      Combo = "A",
      temperature_original = rnorm(12, 20, 5)
    ),
    outcomes = tibble::tibble(value = rnorm(12, 100, 10))
  )

  obj <- structure(
    list(train_data = data.frame(Combo = "A", y = rnorm(12)),
         forecast_horizon = 3, frequency = 12),
    class = "chronos2_model_fit"
  )

  result <- chronos2_permutation_importance(chronos2_obj = obj, mold = mold)

  expect_null(result)
})

test_that("chronos2_permutation_importance returns NULL when too few rows per combo", {
  # Only 4 rows with horizon=3 leaves 1 training row per combo (< 3)
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = 4)
  y_vals <- rnorm(4, 100, 10)
  temp <- rnorm(4, 20, 5)

  mold <- list(
    predictors = data.frame(Date = dates, Combo = "A", temperature_original = temp),
    outcomes = tibble::tibble(y = y_vals)
  )

  obj <- structure(
    list(train_data = data.frame(Date = dates, Combo = "A", y = y_vals, temperature_original = temp),
         forecast_horizon = 3, frequency = 12),
    class = "chronos2_model_fit"
  )

  result <- chronos2_permutation_importance(chronos2_obj = obj, mold = mold)

  expect_null(result)
})

test_that("chronos2_permutation_importance computes importance with API", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  set.seed(42)
  n <- 24
  temp <- rnorm(n, mean = 20, sd = 5)
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n)
  y_vals <- 100 + 2 * temp + rnorm(n, sd = 3)

  mold <- list(
    predictors = data.frame(Date = dates, Combo = "A", temperature_original = temp),
    outcomes = tibble::tibble(y = y_vals)
  )

  obj <- structure(
    list(train_data = data.frame(Date = dates, Combo = "A", y = y_vals, temperature_original = temp),
         forecast_horizon = 3, frequency = 12),
    class = "chronos2_model_fit"
  )

  result <- chronos2_permutation_importance(chronos2_obj = obj, mold = mold, nsim = 2L)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("Variable", "Importance") %in% colnames(result)))
  expect_true("temperature" %in% result$Variable)
  expect_type(result$Importance, "double")
})

test_that("chronos2_permutation_importance works with multiple regressors", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  set.seed(42)
  n <- 24
  temp <- rnorm(n, mean = 20, sd = 5)
  fuel <- rnorm(n, mean = 3, sd = 0.5)
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = n)
  y_vals <- 100 + 2 * temp + 5 * fuel + rnorm(n, sd = 3)

  mold <- list(
    predictors = data.frame(Date = dates, Combo = "A",
                            temperature_original = temp, fuel_price_original = fuel),
    outcomes = tibble::tibble(y = y_vals)
  )

  obj <- structure(
    list(train_data = data.frame(Date = dates, Combo = "A", y = y_vals,
                                 temperature_original = temp, fuel_price_original = fuel),
         forecast_horizon = 3, frequency = 12),
    class = "chronos2_model_fit"
  )

  result <- chronos2_permutation_importance(chronos2_obj = obj, mold = mold, nsim = 2L)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("temperature" %in% result$Variable)
  expect_true("fuel_price" %in% result$Variable)
})

test_that("chronos2_permutation_importance works with multiple combos", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  set.seed(42)
  n <- 24
  dates <- rep(seq.Date(as.Date("2020-01-01"), by = "month", length.out = n), 2)
  combos <- rep(c("A", "B"), each = n)
  y_vals <- rnorm(n * 2, 100, 10)
  temp <- rnorm(n * 2, 20, 5)

  mold <- list(
    predictors = data.frame(Date = dates, Combo = combos, temperature_original = temp),
    outcomes = tibble::tibble(y = y_vals)
  )

  obj <- structure(
    list(train_data = data.frame(Date = dates, Combo = combos, y = y_vals, temperature_original = temp),
         forecast_horizon = 3, frequency = 12),
    class = "chronos2_model_fit"
  )

  result <- chronos2_permutation_importance(chronos2_obj = obj, mold = mold, nsim = 2L)

  expect_s3_class(result, "data.frame")
  expect_true("temperature" %in% result$Variable)
})

test_that("Chronos 2 Model pipeline integration test with future external regressors", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

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

  future_data <- tibble::tibble(
    Date = seq.Date(as.Date("2015-07-01"), by = "month", length.out = 3),
    id = "M2",
    value = NA_real_,
    temperature = c(22.0, 23.0, 21.0)
  )

  data <- dplyr::bind_rows(hist_data, future_data)

  run_info <- set_run_info()

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

  prep_models(
    run_info = run_info,
    models_to_run = "chronos2"
  )

  train_models(
    run_info = run_info,
    run_global_models = FALSE
  )

  ensemble_models(run_info = run_info)
  final_models(run_info = run_info)

  forecasts <- get_forecast_data(run_info = run_info) %>%
    dplyr::filter(Date > as.Date("2015-06-01"))

  expect_s3_class(forecasts, "data.frame")
  expect_equal(nrow(forecasts), 3)
  expect_true(all(!is.na(forecasts$Forecast)))
  trained_models <- get_trained_models(run_info = run_info)
  fit_obj <- trained_models$Model_Fit[[1]]$fit$fit$fit
  expect_true("train_data" %in% names(fit_obj))
  expect_true(any(grepl("temperature_original", colnames(fit_obj$train_data))))
})

# Section 7 — Chronos Controller Integration Tests (require credentials)

test_that("chronos_forecast sends request and returns forecast data frame", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  train_df <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    y = rnorm(48, mean = 100, sd = 10)
  )

  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1"
  )

  result <- chronos_forecast(
    train_df = train_df,
    new_data = new_data,
    model_type = "chronos2",
    horizon = 3,
    exogenous_cols = NULL,
    global = TRUE,
    quantile_levels = c(0.1, 0.5, 0.9)
  )

  expect_s3_class(result, "data.frame")
  expect_true("predictions" %in% colnames(result))
  expect_equal(nrow(result), 3)
})

test_that("chronos_forecast includes exogenous columns in payload", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  train_df <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    y = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5)
  )

  new_data <- data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3),
    Combo = "1",
    temperature_original = c(22.0, 23.0, 21.0)
  )

  result <- chronos_forecast(
    train_df = train_df,
    new_data = new_data,
    model_type = "chronos2",
    horizon = 3,
    exogenous_cols = "temperature_original",
    global = TRUE,
    quantile_levels = c(0.1, 0.5, 0.9)
  )

  expect_s3_class(result, "data.frame")
  expect_true("predictions" %in% colnames(result))
  expect_equal(nrow(result), 3)
})

test_that("chronos_forecast works with multiple combos", {
  skip_if_not(has_chronos_credentials(), "Chronos credentials not set")

  train_df <- data.frame(
    Date = rep(seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48), 2),
    Combo = rep(c("M1", "M2"), each = 48),
    y = rnorm(96, mean = 100, sd = 10)
  )

  new_data <- data.frame(
    Date = rep(seq.Date(as.Date("2024-01-01"), by = "month", length.out = 3), 2),
    Combo = rep(c("M1", "M2"), each = 3)
  )

  result <- chronos_forecast(
    train_df = train_df,
    new_data = new_data,
    model_type = "chronos2",
    horizon = 3,
    exogenous_cols = NULL,
    global = TRUE,
    quantile_levels = c(0.1, 0.5, 0.9)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6) # 3 periods × 2 combos
})
