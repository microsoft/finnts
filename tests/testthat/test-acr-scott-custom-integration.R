test_that("custom preparation guards raw inputs and preserves R1 metadata", {
  data <- acr_scott_fixture()
  data$Combo <- NULL
  options <- list(pool_by = "Product", target_mode = "daily_rate", calendar_policy = "none")
  run <- set_run_info(project_name = "scott", run_name = "integration",
                      path = withr::local_tempdir(), add_unique_id = FALSE)
  arguments <- list(run_info = run, input_data = data,
                    combo_variables = c("Product", "Market"), target_variable = "Target",
                    date_type = "month", forecast_horizon = 2,
                    recipes_to_run = "R1", clean_missing_values = FALSE,
                    stationary = FALSE, acr_scott_custom_options = options)
  invalid <- arguments
  invalid$stationary <- TRUE
  expect_error(do.call(prep_data, invalid), "stationary")
  expect_no_error(do.call(prep_data, arguments))
  prepared <- get_prepped_data(run, recipe = "R1")
  expect_true(all(c(".acr_scott_dim_Product", ".acr_scott_dim_Market",
                    ".acr_scott_observed") %in% names(prepared)))
  expect_true(all(is.na(prepared$Target[prepared$Date > max(data$Date)])))
  restored <- acr_scott_restore_metadata(prepared, acr_scott_options(options))
  expect_setequal(unique(restored$Product), c("A", "B"))
  stripped <- acr_scott_strip_metadata(prepared)
  expect_false(any(startsWith(names(stripped), ".acr_scott_")))
  arguments$acr_scott_custom_options <- acr_scott_options(options)
  expect_no_error(do.call(prep_data, arguments))
  arguments$acr_scott_custom_options$calendar_policy <- "scott_main"
  expect_error(do.call(prep_data, arguments), "new run|changed")

  expect_false("acr-scott-custom" %in% list_models())
  expect_true("acr-scott-custom" %in% list_models(include_opt_in = TRUE))
  prep_models(run, models_to_run = c("acr-scott-custom", "snaive"),
               back_test_scenarios = 1, run_ensemble_models = FALSE,
               num_hyperparameters = 1)
  custom_run <- run
  custom_run$path <- withr::local_tempdir()
  fs::dir_create(fs::path(custom_run$path, c("forecasts", "models")))
  for (folder in c("logs", "prep_data", "prep_models")) {
    fs::dir_copy(fs::path(run$path, folder), fs::path(custom_run$path, folder))
  }
  prepared_models <- get_prepped_models(custom_run)
  workflow_rows <- prepared_models$Data[[which(prepared_models$Type == "Model_Workflows")]]
  write_data(workflow_rows[workflow_rows$Model_Name == "acr-scott-custom", ],
              combo = NULL, run_info = custom_run, output_type = "object",
              folder = "prep_models", suffix = "-model_workflows")
  expect_error(train_models(custom_run, run_global_models = FALSE), "run_global_models")
  train_models(custom_run, run_global_models = TRUE, debug = TRUE)
  expect_equal(nrow(get_trained_models(custom_run)), 1L)
  expect_no_error(train_models(custom_run, run_global_models = TRUE, debug = TRUE))

  train_models(run, run_global_models = TRUE, run_local_models = TRUE, debug = TRUE)
  trained <- get_trained_models(run)
  custom <- trained[trained$Model_Name == "acr-scott-custom", ]
  expect_equal(nrow(custom), 1L)
  expect_identical(custom$Model_Type, "global")
  expect_true(all(trained$Model_Type[trained$Model_Name == "snaive"] == "local"))
  fitted <- custom$Model_Fit[[1]]
  future <- subset(restored, Date > max(data$Date))
  expect_equal(nrow(predict(fitted, future)), nrow(future))
  summary <- summarize_model_acr_scott_custom(fitted)
  validate_summary_output(summary, "acr-scott-custom")
  final_models(run, average_models = FALSE)
  forecast <- get_forecast_data(run)
  expect_true(nrow(forecast) > 0)
})

test_that("high-level API forwards explicit custom raw-input settings", {
  calls <- list()
  testthat::local_mocked_bindings(
    prep_data = function(...) { calls$prep_data <<- list(...) },
    prep_models = function(...) { calls$prep_models <<- list(...) },
    train_models = function(...) { calls$train_models <<- list(...) },
    ensemble_models = function(...) NULL,
    final_models = function(...) NULL,
    .package = "finnts"
  )
  options <- list(pool_by = "Product")
  forecast_time_series(run_info = list(), input_data = data.frame(),
    combo_variables = c("Product", "Market"), target_variable = "Revenue",
    date_type = "month", forecast_horizon = 2, models_to_run = "acr-scott-custom",
    stationary = FALSE, box_cox = FALSE, clean_missing_values = FALSE,
    acr_scott_custom_options = options, run_global_models = TRUE, return_data = FALSE)
  expect_identical(calls$prep_data$acr_scott_custom_options, options)
  expect_false(calls$prep_data$stationary)
  expect_false(calls$prep_data$box_cox)
  expect_true(calls$train_models$run_global_models)
})

test_that("installed parallel preparation resolves custom metadata helpers", {
  data <- acr_scott_fixture(24L)
  data$Combo <- NULL
  # Match the corrected RStudio rscript launcher: give callr the installed build
  # before subprocess startup, then probe actual PSOCK worker namespaces.
  prepare_parallel <- function() {
    arguments <- commandArgs(trailingOnly = TRUE)
    .libPaths(c(arguments[[2]], .libPaths()))
    Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
    suppressPackageStartupMessages(library("finnts", character.only = TRUE))
    data <- readRDS(arguments[[1]])
    cluster <- parallel::makePSOCKcluster(2)
    probe <- function() {
      list(path = getNamespaceInfo(asNamespace("finnts"), "path"),
           helper = exists("acr_scott_attach_metadata", asNamespace("finnts"), inherits = FALSE),
           libraries = .libPaths(), profile = Sys.getenv("R_PROFILE_USER"))
    }
    environment(probe) <- baseenv()
    worker_info <- parallel::clusterCall(cluster, probe)
    parallel::stopCluster(cluster)
    saveRDS(worker_info, arguments[[4]])
    stopifnot(all(vapply(worker_info, function(worker) worker$helper, logical(1))))
    run <- finnts::set_run_info(project_name = "scott_parallel_metadata",
      run_name = "regression", path = tempfile(), add_unique_id = FALSE)
    finnts::prep_data(run_info = run, input_data = data,
      combo_variables = c("Product", "Market"), target_variable = "Target",
      date_type = "month", forecast_horizon = 2, recipes_to_run = "R1",
      stationary = FALSE, clean_missing_values = FALSE,
      acr_scott_custom_options = list(pool_by = "Product", target_mode = "daily_rate"),
      parallel_processing = "local_machine", num_cores = 2)
    saveRDS(finnts::get_prepped_data(run, recipe = "R1"), arguments[[3]])
  }
  script <- tempfile(fileext = ".R")
  input <- tempfile(fileext = ".rds")
  output <- tempfile(fileext = ".rds")
  worker_info <- tempfile(fileext = ".rds")
  saveRDS(data, input)
  writeLines(deparse(body(prepare_parallel)), script)
  library_path <- dirname(getNamespaceInfo("finnts", "path"))
  callr::rscript(script, cmdargs = c(input, library_path, output, worker_info),
    libpath = unique(c(library_path, .libPaths())), system_profile = FALSE, user_profile = FALSE)
  worker_paths <- vapply(readRDS(worker_info), function(worker) worker$path, character(1))
  expect_true(all(worker_paths == getNamespaceInfo("finnts", "path")))
  prepared <- readRDS(output)
  expect_equal(nrow(prepared), 6L * 26L)
  expect_true(all(c(".acr_scott_dim_Product", ".acr_scott_observed") %in% names(prepared)))
  restored <- acr_scott_restore_metadata(prepared, acr_scott_options(list(pool_by = "Product")))
  expect_setequal(unique(restored$Product), c("A", "B"))
  expect_true(all(is.na(prepared$Target[prepared$Date > max(data$Date)])))
})