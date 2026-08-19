test_that("cubist and xgboost multistep pipelines fit and predict", {
  data <- timetk::m4_monthly %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::rename(Date = date) %>%
    dplyr::filter(
      id == "M2",
      Date >= "2012-01-01"
    )

  run_path <- tempfile("finnts-multistep-engines-")
  dir.create(run_path)
  on.exit(unlink(run_path, recursive = TRUE), add = TRUE)
  run_info <- set_run_info(
    project_name = "multistep_engine_test",
    run_name = "cubist_xgboost",
    path = run_path,
    add_unique_id = FALSE
  )

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
    back_test_scenarios = 1,
    models_to_run = c("cubist", "xgboost"),
    run_ensemble_models = FALSE,
    num_hyperparameters = 1,
    pca = TRUE
  )

  train_models(run_info = run_info)

  trained <- get_trained_models(run_info)
  cubist_fit <- trained %>%
    dplyr::filter(Model_Name == "cubist") %>%
    dplyr::pull(Model_Fit) %>%
    .[[1]] %>%
    workflows::extract_fit_engine()
  xgboost_fit <- trained %>%
    dplyr::filter(Model_Name == "xgboost") %>%
    dplyr::pull(Model_Fit) %>%
    .[[1]] %>%
    workflows::extract_fit_engine()

  expect_equal(length(cubist_fit$models), 2)
  expect_equal(length(xgboost_fit$models), 2)
  expect_true(all(grepl("^model_lag_\\d+$", names(xgboost_fit$models))))
  expect_true(is.numeric(xgboost_fit$data$.fitted))
  expect_true(all(!is.na(xgboost_fit$data$.fitted)))
  expect_true(is.numeric(xgboost_fit$data$.residuals))

  future_forecast <- read_unselected_forecast_data(run_info)$forecast %>%
    dplyr::filter(
      Model_Name == "xgboost",
      Run_Type == "Future_Forecast"
    )
  expect_equal(nrow(future_forecast), 2)
  expect_true(is.numeric(future_forecast$Forecast))
  expect_true(all(is.finite(future_forecast$Forecast)))

  final_models(run_info, average_models = FALSE)
  selected_forecast <- get_forecast_data(run_info) %>%
    dplyr::filter(Run_Type == "Future_Forecast", Best_Model == "Yes")
  expect_equal(nrow(selected_forecast), 2)
  expect_true(all(is.finite(selected_forecast$Forecast)))
  expect_true(all(selected_forecast$Model_Name %in% c("cubist", "xgboost")))
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

test_that("xgboost multistep fits one submodel per horizon through step 3", {
  rows <- 36
  outcome <- 100 + seq_len(rows)
  predictors <- tibble::tibble(
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = rows),
    Target_lag1 = outcome - 1,
    Target_lag2 = outcome - 2,
    Target_lag3 = outcome - 3
  )

  fit <- xgboost_multistep_fit_impl(
    x = predictors,
    y = outcome,
    max_depth = 2,
    nrounds = 1,
    lag_periods = c(1, 2, 3),
    forecast_horizon = 3,
    nthread = 1,
    verbose = 0
  )

  expect_equal(
    names(fit$models),
    c("model_lag_1", "model_lag_2", "model_lag_3")
  )
})

# Unit tests: selected_features and multistep spec classification ----

test_that("multistep specs are correctly identified by class", {
  multistep_specs <- list(
    cubist   = cubist_multistep(),
    mars     = mars_multistep(),
    glmnet   = glmnet_multistep(),
    svm_poly = svm_poly_multistep(),
    svm_rbf  = svm_rbf_multistep(),
    xgboost  = xgboost_multistep()
  )

  for (name in names(multistep_specs)) {
    spec <- multistep_specs[[name]]
    expect_true(
      any(grepl("_multistep", class(spec))),
      label = paste(name, "multistep spec has _multistep class")
    )
  }

  standard_specs <- list(
    cubist_rules = parsnip::cubist_rules(),
    mars         = parsnip::mars(),
    glmnet       = parsnip::linear_reg() %>% parsnip::set_engine("glmnet"),
    svm_poly     = parsnip::svm_poly(),
    svm_rbf      = parsnip::svm_rbf(),
    xgboost      = parsnip::boost_tree() %>% parsnip::set_engine("xgboost")
  )

  for (name in names(standard_specs)) {
    spec <- standard_specs[[name]]
    expect_false(
      any(grepl("_multistep", class(spec))),
      label = paste(name, "standard spec does not have _multistep class")
    )
  }
})

test_that("update(selected_features) works on multistep specs", {
  spec <- cubist_multistep(forecast_horizon = 3)

  updated <- update(spec, selected_features = list(model_lag_1 = c("a", "b")))
  expect_no_error(updated)
  expect_s3_class(updated, "cubist_multistep")

  updated_null <- update(spec, selected_features = NULL)
  expect_no_error(updated_null)
  expect_s3_class(updated_null, "cubist_multistep")
})

test_that("update(selected_features) errors on standard parsnip specs", {
  standard_spec <- parsnip::cubist_rules()

  expect_error(
    update(standard_spec, selected_features = list(model_lag_1 = c("a", "b")))
  )
})

test_that("automatic MARS grids exclude cross-validation pruning", {
  prune_methods <- dials::grid_regular(
    mars_automatic_prune_method(),
    levels = 10
  )$prune_method

  expect_setequal(
    prune_methods,
    c("backward", "none", "exhaustive", "forward", "seqrep")
  )
  expect_false("cv" %in% prune_methods)
})

test_that("multistep MARS supports explicit cross-validation pruning", {
  rows <- 36
  outcome <- 100 + seq_len(rows) + sin(seq_len(rows) / 3)
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = rows)
  predictors <- tibble::tibble(
    Date = dates,
    Date_index.num = as.numeric(dates),
    Target_lag1 = outcome - 1,
    Target_lag2 = outcome - 2
  )

  fit <- mars_multistep_fit_impl(
    x = predictors,
    y = outcome,
    nprune = 5,
    degree = 1,
    pmethod = "cv",
    lag_periods = c(1, 2),
    forecast_horizon = 2
  )

  expect_s3_class(fit, "mars_multistep_fit_impl")
  expect_equal(names(fit$models), c("model_lag_1", "model_lag_2"))
  expect_true(all(is.finite(fit$data$.fitted)))

  prediction <- predict(
    fit,
    new_data = predictors[seq_len(2), , drop = FALSE]
  )
  expect_equal(nrow(prediction), 2)
  expect_true(all(is.finite(prediction$.pred)))
})

test_that("multistep lag boundaries cover every supported date frequency", {
  cases <- tibble::tribble(
    ~date_type, ~forecast_horizon, ~expected_lags,
    "year", 2, list(c(1, 2)),
    "quarter", 3, list(c(1, 2, 3)),
    "month", 8, list(c(1, 2, 3, 6, 12)),
    "week", 13, list(c(4, 12, 24)),
    "day", 104, list(c(28, 90, 180))
  )

  for (case_index in seq_len(nrow(cases))) {
    lag_periods <- finnts:::get_lag_periods(
      lag_periods = NULL,
      date_type = cases$date_type[[case_index]],
      forecast_horizon = cases$forecast_horizon[[case_index]],
      multistep_horizon = TRUE
    )
    fitted_lags <- finnts:::get_multi_lags(
      lag_periods = lag_periods,
      forecast_horizon = cases$forecast_horizon[[case_index]]
    )

    expect_equal(
      fitted_lags,
      unlist(cases$expected_lags[[case_index]], use.names = FALSE),
      info = cases$date_type[[case_index]]
    )
    expect_gte(max(fitted_lags), cases$forecast_horizon[[case_index]])
  }

  expect_equal(
    finnts:::get_lag_periods(
      lag_periods = list(7, 14, 28),
      date_type = "day",
      forecast_horizon = 92,
      multistep_horizon = TRUE
    ),
    c(7, 14, 28, 92)
  )
})

test_that("multistep training features are horizon safe and preserve rows", {
  xreg_tbl <- tibble::tibble(
    Combo = rep("Synthetic", 4),
    Target = 101:104,
    Date_index.num = 1:4,
    Date_month.lbl = factor(rep("Jan", 4)),
    Target_lag1 = 11:14,
    Target_lag1_roll2_Avg = 21:24,
    Target_lag2 = 31:34,
    Target_lag2_roll2_Avg = 41:44,
    Target_lag4 = 51:54,
    Historical_Xreg_lag2 = 61:64,
    Historical_Xreg_lag4 = 71:74,
    Future_Xreg = 81:84,
    Historical_Xreg = 91:94,
    Unrelated = 201:204
  )

  selected <- finnts:::multi_feature_selection(
    xreg_tbl = xreg_tbl,
    future_xregs = "Future_Xreg",
    lag_periods = c(1, 2, 4),
    lag = 2,
    target = TRUE
  )

  expect_equal(
    colnames(selected),
    c(
      "Combo", "Target", "Date_index.num", "Date_month.lbl",
      "Target_lag2", "Target_lag2_roll2_Avg", "Historical_Xreg_lag2",
      "Target_lag4", "Historical_Xreg_lag4", "Future_Xreg"
    )
  )
  expect_equal(selected$Date_index.num, xreg_tbl$Date_index.num)
  expect_false(any(grepl("lag1|Historical_Xreg$|Unrelated", colnames(selected))))
})

test_that("multistep feature selection uses custom lag boundaries", {
  feature_names <- function(data) {
    setdiff(colnames(data), c("Combo", "Date", "Target"))
  }
  testthat::local_mocked_bindings(
    vip_available = function() TRUE,
    require_optional_package = function(...) invisible(TRUE),
    target_corr_fn = function(data, threshold) {
      tibble::tibble(term = feature_names(data))
    },
    vip_rf_fn = function(data, seed) {
      tibble::tibble(Variable = feature_names(data))
    },
    vip_cubist_fn = function(data, seed) {
      tibble::tibble(Variable = feature_names(data))
    },
    vip_lm_fn = function(data, seed) {
      tibble::tibble(Variable = feature_names(data))
    },
    .package = "finnts"
  )

  input_data <- tibble::tibble(
    Combo = "Synthetic",
    Date = seq(as.Date("2024-01-01"), by = "day", length.out = 20),
    Target = seq_len(20),
    Date_index.num = seq_len(20),
    Target_lag1 = seq_len(20),
    Target_lag2 = seq_len(20),
    Target_lag4 = seq_len(20)
  )

  selected_features <- finnts:::run_feature_selection(
    input_data = input_data,
    run_info = list(),
    train_test_data = tibble::tibble(),
    date_type = "day",
    fast = TRUE,
    forecast_horizon = 3,
    external_regressors = NULL,
    multistep_horizon = TRUE,
    lag_periods = c(1, 2, 4)
  )

  expect_setequal(names(selected_features), c("model_lag_1", "model_lag_2", "model_lag_4"))
  expect_false(any(grepl("lag1($|_)", selected_features$model_lag_2)))
  expect_false(any(grepl("lag(1|2)($|_)", selected_features$model_lag_4)))
})

test_that("multistep row mapping preserves fiscal date-index collisions", {
  make_case <- function(start_date, rows) {
    dates <- seq(as.Date(start_date), by = "day", length.out = rows)
    data <- tibble::tibble(
      Date = dates,
      Date_index.num = as.numeric(as.POSIXct(lubridate::`%m+%`(dates, lubridate::period(month = 6)), tz = "UTC")),
      marker = seq_along(dates)
    )

    xreg_recipe <- modeltime::create_xreg_recipe(
      data %>% dplyr::select(-Date),
      prepare = TRUE,
      one_hot = TRUE,
      clean_names = FALSE
    )

    object <- list(
      models = list(
        model_lag_28 = 28,
        model_lag_90 = 90,
        model_lag_180 = 180
      ),
      extras = list(xreg_recipe = xreg_recipe)
    )

    prediction <- finnts:::multistep_predict_rows(
      object = object,
      new_data = data,
      predict_model = function(model, new_data) {
        tibble::tibble(.pred = new_data$marker)
      }
    )

    expect_equal(nrow(prediction), rows)
    expect_equal(prediction$.pred, data$marker)
  }

  make_case("2025-10-04", 90)
  make_case("2025-07-06", 92)
  make_case("2025-09-29", 94)
  make_case("2025-06-24", 104)
})

test_that("multistep row mapping preserves repeated dates across combos", {
  dates <- seq(as.Date("2025-07-25"), by = "day", length.out = 35)
  data <- tidyr::crossing(
    Combo = c("Synthetic A", "Synthetic B"),
    Date = dates
  ) %>%
    dplyr::arrange(Date, Combo) %>%
    dplyr::mutate(
      Date_index.num = as.numeric(as.POSIXct(
        lubridate::`%m+%`(Date, lubridate::period(month = 6)),
        tz = "UTC"
      )),
      marker = dplyr::row_number()
    )

  xreg_recipe <- modeltime::create_xreg_recipe(
    data %>% dplyr::select(-Date),
    prepare = TRUE,
    one_hot = TRUE,
    clean_names = FALSE
  )
  object <- list(
    models = list(model_lag_28 = 28, model_lag_90 = 90),
    extras = list(xreg_recipe = xreg_recipe)
  )

  prediction <- finnts:::multistep_predict_rows(
    object = object,
    new_data = data,
    predict_model = function(model, new_data) {
      tibble::tibble(.pred = new_data$marker)
    }
  )

  expect_equal(nrow(prediction), nrow(data))
  expect_equal(prediction$.pred, data$marker)
})

test_that("multistep rows use the smallest fitted lag covering each horizon", {
  dates <- seq(as.Date("2025-06-24"), by = "day", length.out = 104)
  data <- tidyr::crossing(
    Combo = c("Synthetic A", "Synthetic B"),
    Date = dates
  ) %>%
    dplyr::arrange(Date, Combo) %>%
    dplyr::mutate(
      Date_index.num = as.numeric(as.POSIXct(
        lubridate::`%m+%`(Date, lubridate::period(month = 6)),
        tz = "UTC"
      ))
    )

  xreg_recipe <- modeltime::create_xreg_recipe(
    data %>% dplyr::select(-Date),
    prepare = TRUE,
    one_hot = TRUE,
    clean_names = FALSE
  )
  object <- list(
    models = list(model_lag_28 = 28, model_lag_90 = 90, model_lag_180 = 180),
    extras = list(xreg_recipe = xreg_recipe)
  )

  prediction <- finnts:::multistep_predict_rows(
    object = object,
    new_data = data,
    predict_model = function(model, new_data) {
      rep(model, nrow(new_data))
    }
  )

  expected_horizon <- ave(seq_len(nrow(data)), data$Combo, FUN = seq_along)
  expected_lag <- dplyr::case_when(
    expected_horizon <= 28 ~ 28,
    expected_horizon <= 90 ~ 90,
    TRUE ~ 180
  )

  expect_equal(prediction$.pred, expected_lag)
  lag_counts <- table(prediction$.pred)
  expect_equal(names(lag_counts), c("28", "90", "180"))
  expect_equal(as.integer(lag_counts), c(56L, 124L, 28L))
})

test_that("multistep row mapping rejects non-finite predictions", {
  dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 10)
  data <- tibble::tibble(
    Date = dates,
    Date_index.num = as.numeric(as.POSIXct(dates, tz = "UTC")),
    marker = seq_along(dates)
  )
  xreg_recipe <- modeltime::create_xreg_recipe(
    data %>% dplyr::select(-Date),
    prepare = TRUE,
    one_hot = TRUE,
    clean_names = FALSE
  )
  object <- list(
    models = list(model_lag_28 = 28),
    extras = list(xreg_recipe = xreg_recipe)
  )

  expect_error(
    finnts:::multistep_predict_rows(
      object = object,
      new_data = data,
      predict_model = function(model, new_data) {
        c(rep(1, nrow(new_data) - 1), Inf)
      }
    ),
    "non-finite predictions"
  )
})
