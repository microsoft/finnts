# Build one monthly series where Signal drives the target and Noise does not,
# plus two validation folds shaped like prep_models() train/test splits.
# Returns list(data, splits, run_info); writes no files.
lofo_fixture <- function() {
  set.seed(42)
  dates <- seq(as.Date("2017-01-01"), by = "month", length.out = 48)
  index <- seq_along(dates)
  signal <- 20 * sin(2 * pi * index / 12)
  data <- tibble::tibble(
    Combo = "A", Date = dates, Target = 500 + 2 * index + signal + stats::rnorm(48),
    Date_index.num = index, Signal = signal, Noise = stats::rnorm(48)
  )
  splits <- tibble::tibble(
    Run_Type = c("Future_Forecast", "Back_Test", "Validation", "Validation"),
    Train_Test_ID = c(1, 2, 3, 4),
    Train_End = dates[c(48, 45, 42, 39)],
    Test_End = dates[c(48, 48, 45, 42)]
  )
  run_info <- list(storage_object = NULL, path = tempdir(), data_output = "csv", object_output = "rds")
  list(data = data, splits = splits, run_info = run_info)
}

# Check the LOFO contract: one row per candidate feature plus the baseline,
# finite RMSE, baseline importance of zero, and the informative feature
# ranked above noise. Input: lofo_fn() output.
expect_lofo_result <- function(result) {
  expect_setequal(result$LOFO_Var, c("Date_index.num", "Signal", "Noise", "Baseline_Model"))
  expect_equal(nrow(result), 4)
  expect_true(all(is.finite(result$Var_RMSE)))
  importance <- stats::setNames(result$Imp, result$LOFO_Var)
  expect_equal(importance[["Baseline_Model"]], 0)
  expect_gt(importance[["Signal"]], importance[["Noise"]])
}

test_that("LOFO feature importance scores every feature against the baseline", {
  skip_if_not_installed("xgboost")
  fixture <- lofo_fixture()
  result <- lofo_fn(fixture$run_info, fixture$data, fixture$splits, parallel_processing = NULL, seed = 123)

  expect_lofo_result(result)
})

test_that("installed LOFO workers match sequential feature importance", {
  skip_on_cran()
  skip_if_not_installed("xgboost")
  skip_if(parallel::detectCores() < 3, "Two local workers are required")
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(
    file.exists(file.path(namespace_path, "Meta", "package.rds")),
    "LOFO PSOCK workers are verified from the installed namespace"
  )
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  fixture <- lofo_fixture()

  sequential <- lofo_fn(fixture$run_info, fixture$data, fixture$splits, parallel_processing = NULL, seed = 123)
  parallel <- lofo_fn(fixture$run_info, fixture$data, fixture$splits, parallel_processing = "local_machine", seed = 123)

  expect_lofo_result(parallel)
  expect_equal(parallel[order(parallel$LOFO_Var), ], sequential[order(sequential$LOFO_Var), ])
})

test_that("train_models passes inner_parallel through to LOFO", {
  skip_on_cran()
  for (package in c("vip", "Boruta", "corrr", "ranger")) skip_if_not_installed(package)
  skip_if(parallel::detectCores() < 3, "Two local workers are required")
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(
    file.exists(file.path(namespace_path, "Meta", "package.rds")),
    "Inner PSOCK workers are verified from the installed namespace"
  )
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  dates <- seq(as.Date("2017-01-01"), by = "month", length.out = 48)
  input <- data.frame(Date = dates, id = "A", value = 500 + seq_along(dates) + 20 * sin(seq_along(dates)))
  modes <- list()
  testthat::local_mocked_bindings(
    lofo_fn = function(run_info, data, train_test_splits, parallel_processing, ...) {
      modes[[length(modes) + 1]] <<- if (is.null(parallel_processing)) "NULL" else parallel_processing
      tibble::tibble(LOFO_Var = "Baseline_Model", Var_RMSE = 1, Imp = 0)
    },
    .package = "finnts"
  )

  for (inner_parallel in c(FALSE, TRUE)) {
    info <- set_run_info(
      project_name = "lofo-inner", run_name = paste0("inner_", inner_parallel),
      path = withr::local_tempdir(), data_output = "csv", object_output = "rds", add_unique_id = FALSE
    )
    prep_data(info, input,
      combo_variables = "id", target_variable = "value", date_type = "month", forecast_horizon = 3,
      recipes_to_run = "R1", stationary = FALSE, box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE
    )
    prep_models(info, models_to_run = "glmnet", run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
    train_models(info, feature_selection = TRUE, inner_parallel = inner_parallel, num_cores = 2)
  }

  expect_identical(unlist(modes), c("NULL", "local_machine"))
})
