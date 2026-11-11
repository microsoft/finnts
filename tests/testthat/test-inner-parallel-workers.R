# Prepare one monthly multistep series once and copy its logs and prepared
# artifacts into an isolated run root, so sequential and inner-parallel runs
# start from identical inputs. Horizon 2 with back-test spacing 2 yields one
# validation fold: with one hyperparameter, tuning reaches only one inner worker
# while the back-test refit uses both. Input: destination path. Returns run info
# pointing there. The cached template never contains models or forecasts.
inner_parallel_run <- local({
  cache <- new.env(parent = emptyenv())
  function(path) {
    if (is.null(cache$info)) {
      info <- set_run_info(
        project_name = "inner-parallel", run_name = "test",
        path = withr::local_tempdir(.local_envir = cache),
        data_output = "csv", object_output = "rds", add_unique_id = FALSE
      )
      dates <- seq(as.Date("2016-01-01"), by = "month", length.out = 30)
      index <- seq_along(dates)
      prep_data(info,
        data.frame(Date = dates, id = "A", value = 500 + 2 * index + 25 * sin(2 * pi * index / 12)),
        combo_variables = "id", target_variable = "value", date_type = "month",
        forecast_horizon = 2, recipes_to_run = "R1", multistep_horizon = TRUE,
        stationary = FALSE, box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE
      )
      prep_models(info,
        models_to_run = c("glmnet", "snaive"), run_ensemble_models = TRUE,
        back_test_scenarios = 2, back_test_spacing = 2, num_hyperparameters = 1, pca = FALSE, seed = 123
      )
      cache$info <- info
    }
    info <- cache$info
    info$path <- path
    for (folder in c("logs", "prep_data", "prep_models")) {
      fs::dir_copy(fs::path(cache$info$path, folder), fs::path(path, folder))
    }
    fs::dir_create(fs::path(path, c("models", "forecasts")))
    info
  }
})

# Train, ensemble and select models for one copied run, returning every model's
# forecast rows (not only the winner) in a stable order for exact comparison.
inner_parallel_forecasts <- function(path, inner_parallel) {
  info <- inner_parallel_run(path)
  train_models(info, inner_parallel = inner_parallel, num_cores = 2, seed = 123)
  ensemble_models(info, inner_parallel = inner_parallel, num_cores = 2, seed = 123)
  final_models(info, average_models = TRUE, inner_parallel = inner_parallel, num_cores = 2)
  forecast <- get_forecast_data(info)
  forecast[order(forecast$Model_ID, forecast$Run_Type, forecast$Train_Test_ID, forecast$Date), ]
}

test_that("installed inner-parallel workers match sequential forecasts", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 3, "Two inner workers are required")
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(
    file.exists(file.path(namespace_path, "Meta", "package.rds")),
    "Inner PSOCK workers are verified from the installed namespace"
  )
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))

  sequential <- inner_parallel_forecasts(withr::local_tempdir(), inner_parallel = FALSE)
  parallel <- inner_parallel_forecasts(withr::local_tempdir(), inner_parallel = TRUE)

  expect_true(all(c("glmnet--local--R1", "snaive--local--R1") %in% sequential$Model_ID))
  expect_true(any(sequential$Recipe_ID == "ensemble"))
  expect_setequal(unique(parallel$Model_ID), unique(sequential$Model_ID))
  rownames(sequential) <- NULL
  rownames(parallel) <- NULL
  expect_equal(parallel, sequential)
})
