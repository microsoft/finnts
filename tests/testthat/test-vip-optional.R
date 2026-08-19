test_that("feature selection reports how to install vip when unavailable", {
  local_mocked_bindings(
    vip_available = function() FALSE,
    .package = "finnts"
  )

  input_data <- tibble::tibble(
    Date = as.Date(c("2025-01-01", "2025-02-01")),
    Combo = "A",
    Target = c(1, 2)
  )

  expect_error(
    run_feature_selection(
      input_data = input_data,
      run_info = NULL,
      train_test_data = NULL,
      date_type = "month",
      forecast_horizon = 1,
      external_regressors = NULL
    ),
    "bgreenwell\\.r-universe\\.dev"
  )
})

test_that("optional feature packages have actionable installation guidance", {
  expect_error(
    require_optional_package(
      "finntsPackageThatDoesNotExist",
      "feature selection"
    ),
    "install\\.packages\\(\\\"finntsPackageThatDoesNotExist\\\"\\)"
  )
})

test_that("feature selection preflights its complete optional package stack", {
  observed_packages <- character()
  local_mocked_bindings(
    vip_available = function() TRUE,
    require_optional_package = function(package, context) {
      observed_packages <<- c(observed_packages, package)
      invisible(TRUE)
    },
    multi_future_xreg_check = function(...) stop("preflight complete"),
    .package = "finnts"
  )
  input_data <- tibble::tibble(
    Date = as.Date(c("2025-01-01", "2025-02-01")),
    Combo = "A",
    Target = c(1, 2)
  )

  expect_error(
    run_feature_selection(
      input_data = input_data,
      run_info = NULL,
      train_test_data = NULL,
      date_type = "month",
      forecast_horizon = 1,
      external_regressors = NULL
    ),
    "preflight complete"
  )
  expect_identical(observed_packages, c("Boruta", "corrr", "ranger"))
})

test_that("Chronos2 importance is optional when vip is unavailable", {
  local_mocked_bindings(
    vip_available = function() FALSE,
    .package = "finnts"
  )

  expect_null(vip_vi(object = NULL))

  expect_null(
    chronos2_permutation_importance(
      chronos2_obj = NULL,
      mold = NULL
    )
  )
})

test_that("model summarization warns once when vip is unavailable", {
  local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    check_input_type = function(...) invisible(NULL),
    vip_available = function() FALSE,
    get_best_agent_run = function(...) stop("summary stopped after warning"),
    .package = "finnts"
  )
  observed_warnings <- character()

  expect_error(
    withCallingHandlers(
      summarize_models(
        agent_info = list(
          project_info = list(),
          run_id = "test-run"
        )
      ),
      warning = function(condition) {
        observed_warnings <<- c(observed_warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    ),
    "summary stopped after warning"
  )

  expect_length(observed_warnings, 1)
  expect_match(observed_warnings, "variable importance will be omitted")
})

test_that("vip 0.5 supports all feature-selection adapters", {
  skip_on_cran()
  skip_if_not_installed("vip", minimum_version = "0.5.0")
  skip_if_not_installed("ranger")

  feature_data <- tibble::tibble(
    Date = seq.Date(as.Date("2021-01-01"), by = "month", length.out = 48),
    Combo = "A",
    Feature_One = seq_len(48),
    Feature_Two = sin(seq_len(48) / 3),
    Target = 5 * Feature_One + 2 * Feature_Two
  )

  importance_results <- list(
    ranger = vip_rf_fn(feature_data),
    glmnet = vip_lm_fn(feature_data),
    cubist = vip_cubist_fn(feature_data)
  )

  for (model_name in names(importance_results)) {
    importance <- importance_results[[model_name]]

    expect_s3_class(importance, "data.frame")
    expect_true(
      all(c("Variable", "Importance") %in% colnames(importance)),
      info = model_name
    )
    expect_gt(nrow(importance), 0)
    expect_true(all(is.finite(importance$Importance)), info = model_name)
  }
})

test_that("Boruta feature selection uses the ranger importance adapter", {
  skip_if_not_installed("Boruta", minimum_version = "8.0.0")
  skip_if_not_installed("ranger")

  set.seed(123)
  feature_one <- seq_len(40)
  feature_data <- tibble::tibble(
    Target = 4 * feature_one + stats::rnorm(40, sd = 0.1),
    Feature_One = feature_one,
    Feature_Two = stats::rnorm(40)
  )

  result <- boruta_fn(
    data = feature_data,
    iterations = 11
  )

  expect_type(result, "character")
  expect_true(all(result %in% c("Feature_One", "Feature_Two")))
})

test_that("vip 0.5 supports multistep Cubist summary importance", {
  skip_on_cran()
  skip_if_not_installed("vip", minimum_version = "0.5.0")

  run_path <- withr::local_tempdir(pattern = "finnts-vip-cubist-")
  signal <- seq_len(60)
  input_data <- tibble::tibble(
    id = "A",
    Date = seq.Date(as.Date("2018-01-01"), by = "month", length.out = 60),
    value = ifelse(signal < 30, signal * 5, 150 + signal - 30) + sin(signal),
    signal = signal
  )
  run_info <- set_run_info(
    project_name = "vip_cubist",
    run_name = "summary",
    path = run_path,
    add_unique_id = FALSE
  )

  prep_data(
    run_info = run_info,
    input_data = input_data,
    combo_variables = "id",
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 2,
    external_regressors = "signal",
    lag_periods = c(1, 2),
    stationary = FALSE,
    recipes_to_run = "R1",
    multistep_horizon = TRUE
  )
  prep_models(
    run_info = run_info,
    back_test_scenarios = 1,
    models_to_run = "cubist",
    run_ensemble_models = FALSE,
    num_hyperparameters = 1,
    pca = FALSE
  )

  prepared_objects <- get_prepped_models(run_info)
  workflow_tbl <- prepared_objects$Data[[
    which(prepared_objects$Type == "Model_Workflows")
  ]]
  hyperparameter_tbl <- prepared_objects$Data[[
    which(prepared_objects$Type == "Model_Hyperparameters")
  ]]
  training_data <- get_prepped_data(run_info, recipe = "R1") %>%
    dplyr::filter(!is.na(Target))
  hyperparameters <- hyperparameter_tbl %>%
    dplyr::filter(Model == "cubist", Recipe == "R1") %>%
    dplyr::pull(Hyperparameters) %>%
    .[[1]]
  workflow <- workflow_tbl$Model_Workflow[[1]]

  if (ncol(hyperparameters) > 0) {
    workflow <- tune::finalize_workflow(
      workflow,
      hyperparameters[1, , drop = FALSE]
    )
  }

  fitted_workflow <- generics::fit(workflow, training_data)
  summary <- summarize_model_cubist(fitted_workflow)

  expect_importance_output(summary, "cubist-multistep")
})