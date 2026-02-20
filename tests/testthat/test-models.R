# tests/testthat/test-models.R
# Tests for models.R, model workflows, and train_models.R helpers

# -- Model listing functions --

test_that("list_models returns expected model list", {
  models <- list_models()

  expect_type(models, "character")
  expect_true(length(models) == 23)
  expect_true("arima" %in% models)
  expect_true("ets" %in% models)
  expect_true("xgboost" %in% models)
  expect_true("cubist" %in% models)
  expect_true("glmnet" %in% models)
  expect_true("prophet" %in% models)
  expect_true("snaive" %in% models)
  expect_true("timegpt" %in% models)
  expect_true("meanf" %in% models)
  expect_true("theta" %in% models)
})

test_that("list_hyperparmater_models returns hyperparameter models", {
  models <- list_hyperparmater_models()

  expect_type(models, "character")
  expect_true(length(models) == 13)
  expect_true("xgboost" %in% models)
  expect_true("cubist" %in% models)
  expect_true("glmnet" %in% models)
  expect_true("timegpt" %in% models)
  expect_false("arima" %in% models)
  expect_false("ets" %in% models)
  expect_false("snaive" %in% models)
})

test_that("list_ensemble_models returns ensemble models", {
  models <- list_ensemble_models()

  expect_type(models, "character")
  expect_true(length(models) == 5)
  expect_true("cubist" %in% models)
  expect_true("glmnet" %in% models)
  expect_true("xgboost" %in% models)
  expect_true("svm-poly" %in% models)
  expect_true("svm-rbf" %in% models)
})

test_that("list_r2_models returns R2 recipe models", {
  models <- list_r2_models()

  expect_type(models, "character")
  expect_true(length(models) == 5)
  expect_true("cubist" %in% models)
  expect_true("xgboost" %in% models)
})

test_that("list_global_models returns global models", {
  models <- list_global_models()

  expect_type(models, "character")
  expect_true(length(models) == 2)
  expect_true("xgboost" %in% models)
  expect_true("timegpt" %in% models)
})

test_that("list_multivariate_models returns multivariate models", {
  models <- list_multivariate_models()

  expect_type(models, "character")
  expect_true(length(models) == 12)
  expect_true("cubist" %in% models)
  expect_true("arimax" %in% models)
  expect_true("timegpt" %in% models)
})

test_that("list_multistep_models returns multistep models", {
  models <- list_multistep_models()

  expect_type(models, "character")
  expect_true(length(models) == 6)
  expect_true("cubist" %in% models)
  expect_true("glmnet" %in% models)
  expect_true("mars" %in% models)
  expect_true("xgboost" %in% models)
  expect_true("svm-poly" %in% models)
  expect_true("svm-rbf" %in% models)
})

# -- Recipe functions --

test_that("get_recipe_simple creates a recipe with Target ~ Date", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300)
  )

  recipe <- get_recipe_simple(train_data)
  expect_s3_class(recipe, "recipe")
  expect_true("Target" %in% recipe$var_info$variable)
  expect_true("Date" %in% recipe$var_info$variable)
})

test_that("get_recipe_combo creates a recipe with Target ~ Date + Combo", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300)
  )

  recipe <- get_recipe_combo(train_data)
  expect_s3_class(recipe, "recipe")
  expect_true("Combo" %in% recipe$var_info$variable)
})

test_that("get_recipe_configurable creates a recipe with default settings", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300),
    Feature1 = c(1, 2, 3)
  )

  recipe <- get_recipe_configurable(train_data)
  expect_s3_class(recipe, "recipe")
})

test_that("get_recipe_configurable handles rm_date options", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300),
    Date_index.num = c(1, 2, 3)
  )

  recipe_with_adj <- get_recipe_configurable(train_data, rm_date = "with_adj")
  expect_s3_class(recipe_with_adj, "recipe")

  recipe_with_adj_index <- get_recipe_configurable(train_data, rm_date = "with_adj_index")
  expect_s3_class(recipe_with_adj_index, "recipe")

  recipe_none <- get_recipe_configurable(train_data, rm_date = "none")
  expect_s3_class(recipe_none, "recipe")
})

test_that("get_recipe_configurable handles center_scale option", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300),
    Feature1 = c(1, 2, 3)
  )

  recipe <- get_recipe_configurable(train_data, center_scale = TRUE, pca = FALSE)
  expect_s3_class(recipe, "recipe")
})

test_that("get_recipe_configurable handles pca = FALSE", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300),
    Feature1 = c(1, 2, 3)
  )

  recipe <- get_recipe_configurable(train_data, pca = FALSE)
  expect_s3_class(recipe, "recipe")
})

test_that("get_recipe_configurable handles step_nzv options", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300),
    Feature1 = c(1, 2, 3)
  )

  recipe_zv <- get_recipe_configurable(train_data, step_nzv = "zv")
  expect_s3_class(recipe_zv, "recipe")

  recipe_nzv <- get_recipe_configurable(train_data, step_nzv = "nzv")
  expect_s3_class(recipe_nzv, "recipe")
})

test_that("get_recipe_configurable handles dummy_one_hot FALSE", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300),
    Category = c("X", "Y", "Z")
  )

  recipe <- get_recipe_configurable(
    train_data,
    dummy_one_hot = FALSE,
    character_factor = TRUE,
    pca = FALSE
  )
  expect_s3_class(recipe, "recipe")
})

test_that("get_recipe_configurable handles corr and lincomb", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300),
    Feature1 = c(1, 2, 3),
    Feature2 = c(10, 20, 30)
  )

  recipe <- get_recipe_configurable(
    train_data,
    corr = TRUE,
    lincomb = TRUE,
    pca = FALSE
  )
  expect_s3_class(recipe, "recipe")
})

test_that("get_recipe_configurable handles mutate_adj_half", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300),
    Date_half = c(1, 1, 1),
    Date_quarter = c(1, 1, 1)
  )

  recipe <- get_recipe_configurable(train_data, mutate_adj_half = TRUE, pca = FALSE)
  expect_s3_class(recipe, "recipe")
})

test_that("get_recipe_configurable handles norm_date_adj_year", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300),
    Date_index.num = c(1, 2, 3),
    Date_year = c(2020, 2020, 2020)
  )

  recipe <- get_recipe_configurable(train_data, norm_date_adj_year = TRUE, pca = FALSE)
  expect_s3_class(recipe, "recipe")
})

test_that("get_recipe_configurable removes _original columns", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300),
    temp_original = c(10, 20, 30)
  )

  recipe <- get_recipe_configurable(train_data, pca = FALSE)
  expect_s3_class(recipe, "recipe")
  step_ids <- sapply(recipe$steps, function(s) s$id)
  expect_true("step_remove_original" %in% step_ids)
})

# -- Helper to create minimal training data for model workflows --

make_train_data <- function(n = 36) {
  tibble::tibble(
    Combo = rep("test_combo", n),
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = n),
    Target = sin(seq_len(n) / 6 * pi) * 10 + 50 + rnorm(n, sd = 2)
  )
}

test_that("get_recipe_configurable with arimax options creates valid recipe", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- get_recipe_configurable(
    train_data,
    step_nzv = "zv",
    dummy_one_hot = TRUE,
    corr = TRUE,
    pca = FALSE,
    lincomb = TRUE
  )
  expect_s3_class(result, "recipe")
})

# -- Workflow functions --

test_that("get_workflow_simple creates valid workflow", {
  train_data <- tibble::tibble(
    Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    Combo = c("A", "A", "A"),
    Target = c(100, 200, 300)
  )

  recipe <- get_recipe_simple(train_data)
  model_spec <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
  wflw <- get_workflow_simple(model_spec, recipe)

  expect_s3_class(wflw, "workflow")
})

# -- Resampling functions --

test_that("get_resample_kfold returns vfold_cv object", {
  train_data <- tibble::tibble(
    x = rnorm(100),
    y = rnorm(100)
  )

  result <- get_resample_kfold(train_data)
  expect_s3_class(result, "vfold_cv")
})

test_that("get_resample_tscv creates time series CV splits", {
  train_data <- make_train_data(60) %>% dplyr::select(-Combo)
  result <- get_resample_tscv(
    train_data = train_data,
    tscv_initial = 24,
    horizon = 3,
    back_test_spacing = 6
  )
  expect_s3_class(result, "rset")
})

test_that("get_space_filling_grid creates parameter grid", {
  model_spec <- parsnip::boost_tree(
    mode = "regression",
    trees = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = tune::tune()
  ) %>% parsnip::set_engine("xgboost")

  result <- get_space_filling_grid(model_spec)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 10)
})

# -- Simple univariate model workflow tests --

test_that("arima creates a workflow", {
  train_data <- make_train_data()
  result <- arima(train_data, frequency = 12)
  expect_s3_class(result, "workflow")
})

test_that("croston creates a workflow", {
  train_data <- make_train_data()
  result <- croston(train_data, frequency = 12)
  expect_s3_class(result, "workflow")
})

test_that("ets creates a workflow", {
  train_data <- make_train_data()
  result <- ets(train_data, frequency = 12)
  expect_s3_class(result, "workflow")
})

test_that("theta creates a workflow", {
  train_data <- make_train_data()
  result <- theta(train_data, frequency = 12)
  expect_s3_class(result, "workflow")
})

test_that("tbats creates a workflow", {
  train_data <- make_train_data()
  result <- tbats(train_data, seasonal_period = c(12, NA, NA))
  expect_s3_class(result, "workflow")
})

test_that("stlm_arima creates a workflow", {
  train_data <- make_train_data()
  result <- stlm_arima(train_data, seasonal_period = c(12, NA, NA))
  expect_s3_class(result, "workflow")
})

test_that("stlm_ets creates a workflow", {
  train_data <- make_train_data()
  result <- stlm_ets(train_data, seasonal_period = c(12, NA, NA))
  expect_s3_class(result, "workflow")
})

test_that("prophet creates a workflow", {
  train_data <- make_train_data()
  result <- prophet(train_data)
  expect_s3_class(result, "workflow")
})

test_that("nnetar creates a workflow", {
  train_data <- make_train_data()
  result <- nnetar(train_data, horizon = 3, frequency = 12)
  expect_s3_class(result, "workflow")
})

# -- Multivariate model workflow tests (non-multistep) --

test_that("arimax creates a workflow", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- arimax(train_data, frequency = 12, pca = FALSE)
  expect_s3_class(result, "workflow")
})

test_that("arima_boost creates a workflow", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- arima_boost(train_data, frequency = 12, pca = FALSE)
  expect_s3_class(result, "workflow")
})

# -- ML model workflows (non-multistep) --

test_that("glmnet non-multistep creates a workflow", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- glmnet(
    train_data, pca = FALSE, multistep = FALSE,
    horizon = 3, external_regressors = "xreg1", frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("mars non-multistep creates a workflow", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- mars(
    train_data, pca = FALSE, multistep = FALSE,
    horizon = 3, external_regressors = "xreg1", frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("cubist non-multistep creates a workflow", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- cubist(
    train_data, pca = FALSE, multistep = FALSE,
    horizon = 3, external_regressors = "xreg1", frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("svm_poly non-multistep creates a workflow", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- svm_poly(
    train_data, model_type = "single", pca = FALSE, multistep = FALSE,
    horizon = 3, external_regressors = "xreg1", frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("svm_rbf non-multistep creates a workflow", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- svm_rbf(
    train_data, model_type = "single", pca = FALSE, multistep = FALSE,
    horizon = 3, external_regressors = "xreg1", frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("xgboost non-multistep creates a workflow", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- xgboost(
    train_data, pca = FALSE, multistep = FALSE,
    horizon = 3, external_regressors = "xreg1", frequency = 12
  )
  expect_s3_class(result, "workflow")
})

# -- ML model workflows (multistep) --

test_that("glmnet multistep creates a workflow", {
  train_data <- make_train_data()
  result <- glmnet(
    train_data, pca = FALSE, multistep = TRUE,
    horizon = 3, external_regressors = NULL, frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("xgboost multistep creates a workflow", {
  train_data <- make_train_data()
  result <- xgboost(
    train_data, pca = FALSE, multistep = TRUE,
    horizon = 3, external_regressors = NULL, frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("cubist multistep creates a workflow", {
  train_data <- make_train_data()
  result <- cubist(
    train_data, pca = FALSE, multistep = TRUE,
    horizon = 3, external_regressors = NULL, frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("mars multistep creates a workflow", {
  train_data <- make_train_data()
  result <- mars(
    train_data, pca = FALSE, multistep = TRUE,
    horizon = 3, external_regressors = NULL, frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("svm_poly multistep creates a workflow", {
  train_data <- make_train_data()
  result <- svm_poly(
    train_data, model_type = "single", pca = FALSE, multistep = TRUE,
    horizon = 3, external_regressors = NULL, frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("svm_rbf multistep creates a workflow", {
  train_data <- make_train_data()
  result <- svm_rbf(
    train_data, model_type = "single", pca = FALSE, multistep = TRUE,
    horizon = 3, external_regressors = NULL, frequency = 12
  )
  expect_s3_class(result, "workflow")
})

# -- Ensemble model types --

test_that("svm_poly ensemble creates a workflow", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- svm_poly(
    train_data, model_type = "ensemble", pca = FALSE, multistep = FALSE,
    horizon = 3, external_regressors = "xreg1", frequency = 12
  )
  expect_s3_class(result, "workflow")
})

test_that("svm_rbf ensemble creates a workflow", {
  train_data <- make_train_data()
  train_data$xreg1 <- rnorm(nrow(train_data))
  result <- svm_rbf(
    train_data, model_type = "ensemble", pca = FALSE, multistep = FALSE,
    horizon = 3, external_regressors = "xreg1", frequency = 12
  )
  expect_s3_class(result, "workflow")
})

# -- Fitting and tuning helpers --

test_that("get_fit_simple fits a workflow", {
  train_data <- make_train_data(60)
  wflw <- arima(train_data, frequency = 12)
  result <- get_fit_simple(train_data, wflw)
  expect_s3_class(result, "workflow")
  expect_true(workflows::is_trained_workflow(result))
})

# -- train_models.R helper functions --

test_that("negative_fcst_adj replaces NA with zero", {
  data <- tibble::tibble(Forecast = c(10, NA, 30))
  result <- negative_fcst_adj(data, TRUE)
  expect_equal(result$Forecast, c(10, 0, 30))
})

test_that("negative_fcst_adj replaces NaN with zero", {
  data <- tibble::tibble(Forecast = c(10, NaN, 30))
  result <- negative_fcst_adj(data, TRUE)
  expect_equal(result$Forecast, c(10, 0, 30))
})

test_that("negative_fcst_adj replaces Inf with zero", {
  data <- tibble::tibble(Forecast = c(10, Inf, -Inf))
  result <- negative_fcst_adj(data, TRUE)
  expect_equal(result$Forecast, c(10, 0, 0))
})

test_that("negative_fcst_adj keeps negative when negative_forecast=TRUE", {
  data <- tibble::tibble(Forecast = c(-10, 20, -30))
  result <- negative_fcst_adj(data, TRUE)
  expect_equal(result$Forecast, c(-10, 20, -30))
})

test_that("negative_fcst_adj zeroes negatives when negative_forecast=FALSE", {
  data <- tibble::tibble(Forecast = c(-10, 20, -30))
  result <- negative_fcst_adj(data, FALSE)
  expect_equal(result$Forecast, c(0, 20, 0))
})

test_that("negative_fcst_adj handles all NA/NaN/Inf", {
  data <- tibble::tibble(Forecast = c(NA, NaN, Inf, -Inf))
  result <- negative_fcst_adj(data, TRUE)
  expect_equal(result$Forecast, c(0, 0, 0, 0))
})

test_that("negative_fcst_adj handles empty data frame", {
  data <- tibble::tibble(Forecast = numeric(0))
  result <- negative_fcst_adj(data, TRUE)
  expect_equal(nrow(result), 0)
})

test_that("create_splits returns manual_rset object", {
  data <- tibble::tibble(
    Combo = rep("A", 12),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
    Target = 1:12
  )

  train_test_splits <- tibble::tibble(
    Run_Type = c("Validation", "Back_Test"),
    Train_Test_ID = c(2, 1),
    Train_End = as.Date(c("2020-08-01", "2020-09-01")),
    Test_End = as.Date(c("2020-10-01", "2020-12-01"))
  )

  result <- create_splits(data, train_test_splits)

  expect_s3_class(result, "rset")
  expect_equal(nrow(result), 2)
  expect_true("splits" %in% colnames(result))
  expect_true("id" %in% colnames(result))
})

test_that("create_splits assigns correct IDs", {
  data <- tibble::tibble(
    Combo = rep("A", 12),
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
    Target = 1:12
  )

  train_test_splits <- tibble::tibble(
    Run_Type = c("Validation"),
    Train_Test_ID = c(3),
    Train_End = as.Date(c("2020-08-01")),
    Test_End = as.Date(c("2020-12-01"))
  )

  result <- create_splits(data, train_test_splits)
  expect_equal(result$id, "3")
})

# -- undifference / adjust_column_types helpers --

test_that("undifference_forecast undifferences with single diff", {
  original_target <- cumsum(c(10, rep(1, 9)))
  diff_target <- c(NA, diff(original_target))

  recipe_data <- tibble::tibble(
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 7),
    Target = diff_target[1:7]
  )

  forecast_data <- tibble::tibble(
    Train_Test_ID = rep(1, 3),
    Date = seq(as.Date("2020-08-01"), by = "month", length.out = 3),
    Target = diff_target[8:10],
    Forecast = diff_target[8:10] + 0.1
  )

  diff_tbl <- tibble::tibble(
    Combo = "A",
    Diff_Value1 = original_target[1],
    Diff_Value2 = NA
  )

  result <- undifference_forecast(forecast_data, recipe_data, diff_tbl)
  expect_s3_class(result, "tbl_df")
  expect_true("Forecast" %in% names(result))
  expect_true("Target" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that("undifference_forecast returns unchanged when no diffs", {
  forecast_data <- tibble::tibble(
    Date = as.Date(c("2020-07-01", "2020-08-01")),
    Target = c(70, 80),
    Forecast = c(72, 78),
    Train_Test_ID = c(1, 1)
  )

  recipe_data <- tibble::tibble(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 6),
    Target = c(10, 20, 30, 40, 50, 60)
  )

  diff_tbl <- tibble::tibble(
    Diff_Value1 = NA_real_,
    Diff_Value2 = NA_real_
  )

  result <- undifference_forecast(forecast_data, recipe_data, diff_tbl)
  expect_equal(result, forecast_data)
})

test_that("undifference_recipe undifferences recipe data", {
  original_target <- cumsum(c(10, rep(1, 9)))
  diff_target <- c(NA, diff(original_target))

  recipe_data <- tibble::tibble(
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 10),
    Target = diff_target
  )

  diff_tbl <- tibble::tibble(
    Combo = "A",
    Diff_Value1 = original_target[1],
    Diff_Value2 = NA
  )

  hist_end_date <- as.Date("2020-07-01")
  result <- undifference_recipe(recipe_data, diff_tbl, hist_end_date)
  expect_s3_class(result, "tbl_df")
  expect_true("Target" %in% names(result))
})

test_that("undifference_recipe returns unchanged when no diffs", {
  recipe_data <- tibble::tibble(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 6),
    Target = c(10, 20, 30, 40, 50, 60)
  )

  diff_tbl <- tibble::tibble(
    Diff_Value1 = NA_real_,
    Diff_Value2 = NA_real_
  )

  result <- undifference_recipe(recipe_data, diff_tbl, as.Date("2020-06-01"))
  expect_equal(result, recipe_data)
})

test_that("adjust_column_types converts columns to match recipe", {
  data <- tibble::tibble(
    x1 = c("1", "2", "3"),
    x2 = c(1.0, 2.0, 3.0),
    Target = c(10, 20, 30)
  )

  recipe <- recipes::recipe(Target ~ ., data = data.frame(
    x1 = 1.0, x2 = "a", Target = 1.0
  ))
  recipe <- recipes::prep(recipe, training = data.frame(
    x1 = 1.0, x2 = "a", Target = 1.0
  ))

  result <- adjust_column_types(data, recipe)
  expect_s3_class(result, "tbl_df")
})

test_that("adjust_column_types converts numeric columns", {
  data <- tibble::tibble(
    x = c("1", "2", "3"),
    y = c("a", "b", "c")
  )

  recipe <- recipes::recipe(y ~ x, data = tibble::tibble(x = 1.0, y = "a"))
  prepped <- recipes::prep(recipe)

  result <- adjust_column_types(data, prepped)
  expect_type(result$x, "double")
})
