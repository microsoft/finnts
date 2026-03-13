# tests/testthat/test-summarize_models.R
# Tests for summarize_model_*() functions across all model types

skip_on_cran()

# helper: validate that a summary tibble has the expected structure
validate_summary_output <- function(summary_tbl, model_name) {
  expect_true(tibble::is_tibble(summary_tbl) || is.data.frame(summary_tbl))
  expect_true(nrow(summary_tbl) >= 1)

  expected_cols <- c("model_class", "engine", "section", "name", "value")
  for (col in expected_cols) {
    expect_true(col %in% colnames(summary_tbl))
  }

  expect_true(is.character(summary_tbl$section))
  expect_true(is.character(summary_tbl$name))
  expect_true(is.character(summary_tbl$value))
  expect_true(all(!is.na(summary_tbl$section)))
  na_idx <- is.na(summary_tbl$name)
  expect(
    !any(na_idx),
    sprintf(
      "Found %d NA name(s) in sections: %s",
      sum(na_idx),
      paste(summary_tbl$section[na_idx], collapse = ", ")
    )
  )

  valid_sections <- c(
    "predictor", "outcome", "recipe_step", "model_arg",
    "engine_param", "coefficient", "importance", "xreg_coefficient"
  )
  actual_sections <- unique(summary_tbl$section)
  expect_true(all(actual_sections %in% valid_sections))
  expect_true("outcome" %in% actual_sections)
}

# helper: get the workflow for a specific model from a trained models tibble
get_model_workflow <- function(trained_tbl, model_name) {
  row <- trained_tbl %>%
    dplyr::filter(Model_Name == model_name) %>%
    dplyr::slice(1)
  if (nrow(row) == 0) {
    stop(paste0("No trained model found with name '", model_name, "'"))
  }
  row$Model_Fit[[1]]
}

# * Univariate models without xregs ----

data_no_xregs <- timetk::m4_monthly %>%
  dplyr::mutate(id = as.character(id)) %>%
  dplyr::rename(Date = date) %>%
  dplyr::filter(
    id == "M2",
    Date >= "2012-01-01"
  )

run_info_univariate <- set_run_info()

prep_data(
  run_info = run_info_univariate,
  input_data = data_no_xregs,
  combo_variables = c("id"),
  target_variable = "value",
  date_type = "month",
  forecast_horizon = 3,
  recipes_to_run = "R1"
)

prep_models(
  run_info = run_info_univariate,
  back_test_scenarios = 2,
  models_to_run = c(
    "arima", "ets", "croston", "meanf", "snaive",
    "theta", "stlm-arima", "stlm-ets", "tbats"
  ),
  run_ensemble_models = FALSE,
  num_hyperparameters = 1,
  pca = TRUE
)

train_models(run_info = run_info_univariate)

trained_univariate <- get_trained_models(run_info = run_info_univariate)

test_that("summarize arima without xregs", {
  wf <- get_model_workflow(trained_univariate, "arima")
  result <- summarize_model_arima(wf)
  validate_summary_output(result, "arima")
  expect_true("model_arg" %in% result$section)
  sections <- unique(result$section)
  expect_true(any(c("coefficient", "engine_param") %in% sections))
})

test_that("summarize ets without xregs", {
  wf <- get_model_workflow(trained_univariate, "ets")
  result <- summarize_model_ets(wf)
  validate_summary_output(result, "ets")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize croston without xregs", {
  wf <- get_model_workflow(trained_univariate, "croston")
  result <- summarize_model_croston(wf)
  validate_summary_output(result, "croston")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize meanf without xregs", {
  wf <- get_model_workflow(trained_univariate, "meanf")
  result <- summarize_model_meanf(wf)
  validate_summary_output(result, "meanf")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize snaive without xregs", {
  wf <- get_model_workflow(trained_univariate, "snaive")
  result <- summarize_model_snaive(wf)
  validate_summary_output(result, "snaive")
  sp_row <- result %>% dplyr::filter(section == "model_arg", name == "seasonal_period")
  expect_equal(nrow(sp_row), 1)
})

test_that("summarize theta without xregs", {
  wf <- get_model_workflow(trained_univariate, "theta")
  result <- summarize_model_theta(wf)
  validate_summary_output(result, "theta")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize stlm-arima without xregs", {
  wf <- get_model_workflow(trained_univariate, "stlm-arima")
  result <- summarize_model_stlm_arima(wf)
  validate_summary_output(result, "stlm-arima")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize stlm-ets without xregs", {
  wf <- get_model_workflow(trained_univariate, "stlm-ets")
  result <- summarize_model_stlm_ets(wf)
  validate_summary_output(result, "stlm-ets")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize tbats without xregs", {
  wf <- get_model_workflow(trained_univariate, "tbats")
  result <- summarize_model_tbats(wf)
  validate_summary_output(result, "tbats")
  expect_true("model_arg" %in% result$section)
})

rm(trained_univariate)

# * Multivariate models without xregs ----

run_info_multivariate <- set_run_info()

prep_data(
  run_info = run_info_multivariate,
  input_data = data_no_xregs,
  combo_variables = c("id"),
  target_variable = "value",
  date_type = "month",
  forecast_horizon = 3,
  recipes_to_run = "R1"
)

prep_models(
  run_info = run_info_multivariate,
  back_test_scenarios = 2,
  models_to_run = c(
    "nnetar", "prophet", "arimax", "arima-boost",
    "prophet-boost", "prophet-xregs", "nnetar-xregs"
  ),
  run_ensemble_models = FALSE,
  num_hyperparameters = 1,
  pca = TRUE
)

train_models(run_info = run_info_multivariate)

trained_multivariate <- get_trained_models(run_info = run_info_multivariate)

test_that("summarize nnetar without xregs", {
  wf <- get_model_workflow(trained_multivariate, "nnetar")
  result <- summarize_model_nnetar(wf)
  validate_summary_output(result, "nnetar")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize prophet without xregs", {
  wf <- get_model_workflow(trained_multivariate, "prophet")
  result <- summarize_model_prophet(wf)
  validate_summary_output(result, "prophet")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize arimax without xregs", {
  wf <- get_model_workflow(trained_multivariate, "arimax")
  result <- summarize_model_arimax(wf)
  validate_summary_output(result, "arimax")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize arima-boost without xregs", {
  wf <- get_model_workflow(trained_multivariate, "arima-boost")
  result <- summarize_model_arima_boost(wf)
  validate_summary_output(result, "arima-boost")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize prophet-boost without xregs", {
  wf <- get_model_workflow(trained_multivariate, "prophet-boost")
  result <- summarize_model_prophet_boost(wf)
  validate_summary_output(result, "prophet-boost")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize prophet-xregs without xregs", {
  wf <- get_model_workflow(trained_multivariate, "prophet-xregs")
  result <- summarize_model_prophet(wf)
  validate_summary_output(result, "prophet-xregs")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize nnetar-xregs without xregs", {
  wf <- get_model_workflow(trained_multivariate, "nnetar-xregs")
  result <- summarize_model_nnetar(wf)
  validate_summary_output(result, "nnetar-xregs")
  expect_true("model_arg" %in% result$section)
})

rm(trained_multivariate)

# * Multistep models without xregs ----

run_info_multistep <- set_run_info()

prep_data(
  run_info = run_info_multistep,
  input_data = data_no_xregs,
  combo_variables = c("id"),
  target_variable = "value",
  date_type = "month",
  forecast_horizon = 2,
  recipes_to_run = "R1",
  multistep_horizon = TRUE
)

prep_models(
  run_info = run_info_multistep,
  back_test_scenarios = 2,
  models_to_run = c("xgboost", "cubist", "glmnet", "mars", "svm-poly", "svm-rbf"),
  run_ensemble_models = FALSE,
  num_hyperparameters = 1,
  pca = TRUE
)

train_models(run_info = run_info_multistep)

trained_multistep <- get_trained_models(run_info = run_info_multistep)

test_that("summarize xgboost multistep without xregs", {
  wf <- get_model_workflow(trained_multistep, "xgboost")
  result <- summarize_model_xgboost(wf)
  validate_summary_output(result, "xgboost-multistep")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Multistep Horizon")
  n_models_row <- result %>% dplyr::filter(section == "engine_param", name == "n_models")
  expect_equal(n_models_row$value[[1]], "2")
})

test_that("summarize cubist multistep without xregs", {
  wf <- get_model_workflow(trained_multistep, "cubist")
  result <- summarize_model_cubist(wf)
  validate_summary_output(result, "cubist-multistep")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Multistep Horizon")
  n_models_row <- result %>% dplyr::filter(section == "engine_param", name == "n_models")
  expect_equal(n_models_row$value[[1]], "2")
})

test_that("summarize glmnet multistep without xregs", {
  wf <- get_model_workflow(trained_multistep, "glmnet")
  result <- summarize_model_glmnet(wf)
  validate_summary_output(result, "glmnet-multistep")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Multistep Horizon")
  n_models_row <- result %>% dplyr::filter(section == "engine_param", name == "n_models")
  expect_equal(n_models_row$value[[1]], "2")
})

test_that("summarize svm-poly multistep without xregs", {
  wf <- get_model_workflow(trained_multistep, "svm-poly")
  result <- summarize_model_svm_poly(wf)
  validate_summary_output(result, "svm-poly-multistep")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Multistep Horizon")
  n_models_row <- result %>% dplyr::filter(section == "engine_param", name == "n_models")
  expect_equal(n_models_row$value[[1]], "2")
})

test_that("summarize svm-rbf multistep without xregs", {
  wf <- get_model_workflow(trained_multistep, "svm-rbf")
  result <- summarize_model_svm_rbf(wf)
  validate_summary_output(result, "svm-rbf-multistep")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Multistep Horizon")
  n_models_row <- result %>% dplyr::filter(section == "engine_param", name == "n_models")
  expect_equal(n_models_row$value[[1]], "2")
})

rm(trained_multistep)

# * Non-multistep ML models without xregs ----

run_info_standard_ml <- set_run_info()

prep_data(
  run_info = run_info_standard_ml,
  input_data = data_no_xregs,
  combo_variables = c("id"),
  target_variable = "value",
  date_type = "month",
  forecast_horizon = 3,
  recipes_to_run = "R1",
  multistep_horizon = FALSE
)

prep_models(
  run_info = run_info_standard_ml,
  back_test_scenarios = 2,
  models_to_run = c("cubist", "glmnet", "mars", "svm-poly", "svm-rbf"),
  run_ensemble_models = FALSE,
  num_hyperparameters = 1,
  pca = TRUE
)

train_models(run_info = run_info_standard_ml)

trained_standard_ml <- get_trained_models(run_info = run_info_standard_ml)

test_that("summarize cubist non-multistep without xregs", {
  wf <- get_model_workflow(trained_standard_ml, "cubist")
  result <- summarize_model_cubist(wf)
  validate_summary_output(result, "cubist-standard")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  if (nrow(ms_rows) > 0) {
    expect_false(ms_rows$value[1] == "Multistep Horizon")
  }
})

test_that("summarize glmnet non-multistep without xregs", {
  wf <- get_model_workflow(trained_standard_ml, "glmnet")
  result <- summarize_model_glmnet(wf)
  validate_summary_output(result, "glmnet-standard")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  if (nrow(ms_rows) > 0) {
    expect_false(ms_rows$value[1] == "Multistep Horizon")
  }
})

test_that("summarize mars non-multistep without xregs", {
  wf <- get_model_workflow(trained_standard_ml, "mars")
  result <- summarize_model_mars(wf)
  validate_summary_output(result, "mars-standard")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  if (nrow(ms_rows) > 0) {
    expect_false(ms_rows$value[1] == "Multistep Horizon")
  }
})

test_that("summarize svm-poly non-multistep without xregs", {
  wf <- get_model_workflow(trained_standard_ml, "svm-poly")
  result <- summarize_model_svm_poly(wf)
  validate_summary_output(result, "svm-poly-standard")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  if (nrow(ms_rows) > 0) {
    expect_false(ms_rows$value[1] == "Multistep Horizon")
  }
})

test_that("summarize svm-rbf non-multistep without xregs", {
  wf <- get_model_workflow(trained_standard_ml, "svm-rbf")
  result <- summarize_model_svm_rbf(wf)
  validate_summary_output(result, "svm-rbf-standard")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  if (nrow(ms_rows) > 0) {
    expect_false(ms_rows$value[1] == "Multistep Horizon")
  }
})

rm(trained_standard_ml)

# non-multistep xgboost runs as a global model (needs 2+ combos)
data_multi_combo <- timetk::m4_monthly %>%
  dplyr::mutate(id = as.character(id)) %>%
  dplyr::rename(Date = date) %>%
  dplyr::filter(
    id %in% c("M1", "M2"),
    Date >= "2012-01-01"
  )

run_info_xgb_global <- set_run_info()

prep_data(
  run_info = run_info_xgb_global,
  input_data = data_multi_combo,
  combo_variables = c("id"),
  target_variable = "value",
  date_type = "month",
  forecast_horizon = 3,
  recipes_to_run = "R1",
  multistep_horizon = FALSE
)

prep_models(
  run_info = run_info_xgb_global,
  back_test_scenarios = 2,
  models_to_run = "xgboost",
  run_ensemble_models = FALSE,
  num_hyperparameters = 1,
  pca = TRUE
)

train_models(
  run_info = run_info_xgb_global,
  run_global_models = TRUE,
  run_local_models = FALSE
)

trained_xgb_global <- get_trained_models(run_info = run_info_xgb_global)

test_that("summarize xgboost non-multistep (global) without xregs", {
  wf <- get_model_workflow(trained_xgb_global, "xgboost")
  result <- summarize_model_xgboost(wf)
  validate_summary_output(result, "xgboost-standard")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Standard")
})

rm(trained_xgb_global)

# * Multivariate models WITH xregs ----

set.seed(42)
data_with_xregs <- data_no_xregs %>%
  dplyr::mutate(
    xreg1 = value * 0.5 + rnorm(dplyr::n(), mean = 10, sd = 2),
    xreg2 = value * -0.3 + rnorm(dplyr::n(), mean = 50, sd = 5)
  )

run_info_xregs_mv <- set_run_info()

prep_data(
  run_info = run_info_xregs_mv,
  input_data = data_with_xregs,
  combo_variables = c("id"),
  target_variable = "value",
  date_type = "month",
  forecast_horizon = 3,
  external_regressors = c("xreg1", "xreg2"),
  recipes_to_run = "R1"
)

prep_models(
  run_info = run_info_xregs_mv,
  back_test_scenarios = 2,
  models_to_run = c(
    "arimax", "arima-boost", "prophet-boost",
    "prophet-xregs", "nnetar-xregs"
  ),
  run_ensemble_models = FALSE,
  num_hyperparameters = 1,
  pca = TRUE
)

train_models(run_info = run_info_xregs_mv)

trained_xregs_mv <- get_trained_models(run_info = run_info_xregs_mv)

test_that("summarize arimax with xregs", {
  wf <- get_model_workflow(trained_xregs_mv, "arimax")
  result <- summarize_model_arimax(wf)
  validate_summary_output(result, "arimax-xregs")
  preds <- result %>% dplyr::filter(section == "predictor")
  expect_gt(nrow(preds), 0)
})

test_that("summarize arima-boost with xregs", {
  wf <- get_model_workflow(trained_xregs_mv, "arima-boost")
  result <- summarize_model_arima_boost(wf)
  validate_summary_output(result, "arima-boost-xregs")
  preds <- result %>% dplyr::filter(section == "predictor")
  expect_gt(nrow(preds), 0)
})

test_that("summarize prophet-boost with xregs", {
  wf <- get_model_workflow(trained_xregs_mv, "prophet-boost")
  result <- summarize_model_prophet_boost(wf)
  validate_summary_output(result, "prophet-boost-xregs")
  preds <- result %>% dplyr::filter(section == "predictor")
  expect_gt(nrow(preds), 0)
})

test_that("summarize prophet-xregs with xregs", {
  wf <- get_model_workflow(trained_xregs_mv, "prophet-xregs")
  result <- summarize_model_prophet(wf)
  validate_summary_output(result, "prophet-xregs-xregs")
  preds <- result %>% dplyr::filter(section == "predictor")
  expect_gt(nrow(preds), 0)
})

test_that("summarize nnetar-xregs with xregs", {
  wf <- get_model_workflow(trained_xregs_mv, "nnetar-xregs")
  result <- summarize_model_nnetar(wf)
  validate_summary_output(result, "nnetar-xregs-xregs")
  preds <- result %>% dplyr::filter(section == "predictor")
  expect_gt(nrow(preds), 0)
})

rm(trained_xregs_mv)

# * Multistep models WITH xregs + feature selection ----

run_info_xregs_ms <- set_run_info()

prep_data(
  run_info = run_info_xregs_ms,
  input_data = data_with_xregs,
  combo_variables = c("id"),
  target_variable = "value",
  date_type = "month",
  forecast_horizon = 2,
  external_regressors = c("xreg1", "xreg2"),
  recipes_to_run = "R1",
  multistep_horizon = TRUE
)

prep_models(
  run_info = run_info_xregs_ms,
  back_test_scenarios = 2,
  models_to_run = c("xgboost", "cubist", "glmnet", "mars", "svm-poly", "svm-rbf"),
  run_ensemble_models = FALSE,
  num_hyperparameters = 1,
  pca = TRUE
)

train_models(
  run_info = run_info_xregs_ms,
  feature_selection = TRUE
)

trained_xregs_ms <- get_trained_models(run_info = run_info_xregs_ms)

test_that("summarize xgboost multistep with xregs and feature selection", {
  wf <- get_model_workflow(trained_xregs_ms, "xgboost")
  result <- summarize_model_xgboost(wf)
  validate_summary_output(result, "xgboost-multistep-xregs-fs")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Multistep Horizon")
  preds <- result %>% dplyr::filter(section == "predictor")
  expect_gt(nrow(preds), 0)
})

test_that("summarize cubist multistep with xregs and feature selection", {
  wf <- get_model_workflow(trained_xregs_ms, "cubist")
  result <- summarize_model_cubist(wf)
  validate_summary_output(result, "cubist-multistep-xregs-fs")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Multistep Horizon")
  preds <- result %>% dplyr::filter(section == "predictor")
  expect_gt(nrow(preds), 0)
})

test_that("summarize glmnet multistep with xregs and feature selection", {
  wf <- get_model_workflow(trained_xregs_ms, "glmnet")
  result <- summarize_model_glmnet(wf)
  validate_summary_output(result, "glmnet-multistep-xregs-fs")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Multistep Horizon")
  preds <- result %>% dplyr::filter(section == "predictor")
  expect_gt(nrow(preds), 0)
})

test_that("summarize svm-poly multistep with xregs and feature selection", {
  wf <- get_model_workflow(trained_xregs_ms, "svm-poly")
  result <- summarize_model_svm_poly(wf)
  validate_summary_output(result, "svm-poly-multistep-xregs-fs")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Multistep Horizon")
  preds <- result %>% dplyr::filter(section == "predictor")
  expect_gt(nrow(preds), 0)
})

test_that("summarize svm-rbf multistep with xregs and feature selection", {
  wf <- get_model_workflow(trained_xregs_ms, "svm-rbf")
  result <- summarize_model_svm_rbf(wf)
  validate_summary_output(result, "svm-rbf-multistep-xregs-fs")
  ms_rows <- result %>% dplyr::filter(section == "engine_param", name == "model_type")
  expect_equal(ms_rows$value[[1]], "Multistep Horizon")
  preds <- result %>% dplyr::filter(section == "predictor")
  expect_gt(nrow(preds), 0)
})

rm(trained_xregs_ms)

# * TimeGPT summarize ----

has_timegpt_credentials <- function() {
  azure_url <- Sys.getenv("NIXTLA_BASE_URL", unset = NA)
  api_key <- Sys.getenv("NIXTLA_API_KEY", unset = NA)
  if (is.na(azure_url) || is.na(api_key) ||
    !nzchar(azure_url) || !nzchar(api_key)) {
    return(FALSE)
  }
  return(TRUE)
}

test_that("summarize timegpt without xregs", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10)
  )

  recipe_spec <- get_recipe_simple(train_data)
  model_spec <- timegpt_model(forecast_horizon = 3) %>%
    parsnip::set_engine("timegpt_model")
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))

  result <- summarize_model_timegpt(wf)
  validate_summary_output(result, "timegpt")
  expect_true("model_arg" %in% result$section)
})

test_that("summarize timegpt with xregs", {
  skip_if_not(has_timegpt_credentials(), "NIXTLA credentials not set")

  train_data <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 48),
    Combo = "1",
    Target = rnorm(48, mean = 100, sd = 10),
    temperature_original = rnorm(48, mean = 20, sd = 5)
  )

  recipe_spec <- get_recipe_foundation_model(train_data)
  model_spec <- timegpt_model(forecast_horizon = 3) %>%
    parsnip::set_engine("timegpt_model")
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec) %>%
    workflows::add_model(model_spec) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))

  result <- summarize_model_timegpt(wf)
  validate_summary_output(result, "timegpt-xregs")
  expect_true("model_arg" %in% result$section)
  xreg_rows <- result %>% dplyr::filter(
    section == "engine_param", name == "n_external_regressors"
  )
  expect_gt(nrow(xreg_rows), 0)
})
