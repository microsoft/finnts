test_that("completed local ensembles skip without enumerating forecasts", {
  run_info <- artifact_test_run(withr::local_tempdir())
  run_info$combo <- hash_data("A")
  artifact_test_model_log(run_info)
  artifact_test_splits(run_info)
  artifact_test_forecast(run_info)
  artifact_test_forecast(run_info, suffix = "ensemble_models")
  tracker <- local_artifact_spies()
  testthat::local_mocked_bindings(
    par_start = function(...) stop("completed ensemble was dispatched", call. = FALSE),
    .package = "finnts"
  )

  expect_no_error(ensemble_models(run_info))
  expect_equal(tracker$listings, 0L)
})

test_that("unfinished local ensembles dispatch without directory enumeration", {
  run_info <- artifact_test_run(withr::local_tempdir())
  run_info$combo <- hash_data("A")
  artifact_test_model_log(run_info)
  artifact_test_splits(run_info)
  artifact_test_forecast(run_info)
  tracker <- local_artifact_spies()
  testthat::local_mocked_bindings(
    par_start = function(...) stop("unfinished ensemble dispatched", call. = FALSE),
    .package = "finnts"
  )

  expect_error(ensemble_models(run_info), "unfinished ensemble dispatched")
  expect_equal(tracker$listings, 0L)
})

test_that("ensemble completion counts persisted outputs instead of worker returns", {
  run_info <- artifact_test_run(withr::local_tempdir())
  run_info$combo <- hash_data("A")
  artifact_test_model_log(run_info)
  artifact_test_splits(run_info)
  artifact_test_forecast(run_info)
  artifact_test_forecast(run_info, combo = "unrelated", suffix = "ensemble_models")
  tracker <- local_artifact_spies()
  testthat::local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(),
      foreach_operator = function(...) data.frame(Combo = hash_data("A"))),
    par_end = function(...) invisible(NULL),
    .package = "finnts"
  )

  expect_message(ensemble_models(run_info), "expected 1.*only 0")
  expect_equal(tracker$listings, 0L)
})

artifact_completed_final_outputs <- function(run_info) {
  log <- artifact_test_model_log(run_info, "standard_hierarchy")
  log$hist_end_date <- as.Date("2024-01-01")
  write_data(log, NULL, run_info, "log", "logs")
  history <- tibble::tibble(Combo = "A",
    Date = seq(as.Date("2023-01-01"), by = "month", length.out = 14),
    Target = c(rep(100, 13), NA_real_))
  write_data(history, "A", run_info, "data", "prep_data", "-R1")
  splits <- tibble::tibble(Run_Type = c("Future_Forecast", "Back_Test"), Train_Test_ID = c(1, 2),
    Train_End = as.Date(c("2024-01-01", "2023-12-01")),
    Test_End = as.Date(c("2024-02-01", "2024-01-01")))
  write_data(splits, NULL, run_info, "data", "prep_models", "-train_test_split")
  forecast <- artifact_test_forecast(run_info, write_output = FALSE)
  backtest <- forecast
  backtest$Train_Test_ID <- 2
  backtest$Date <- as.Date("2024-01-01")
  backtest$Target <- 100
  predictions <- dplyr::bind_rows(forecast, backtest)
  predictions$Horizon <- 1
  predictions$Run_Type <- c("Future_Forecast", "Back_Test")
  predictions$lo_80 <- 98
  predictions$lo_95 <- 95
  predictions$hi_80 <- 102
  predictions$hi_95 <- 105
  write_data(predictions, "A", run_info, "data", "forecasts", "-single_models")
  average <- predictions
  average$Model_ID <- "meanf--local--R1_snaive--local--R1"
  average$Model_Name <- "model_average"
  average$Best_Model <- "No"
  write_data(average, "A", run_info, "data", "forecasts", "-average_models")
  series <- list(history = history[history$Date <= log$hist_end_date, ], calendar = history$Date)
  expect_identical(completed_forecast_selection(dplyr::bind_rows(predictions, average), series, splits)$selected_id,
    "meanf--local--R1")
  write_data(list(original_combos = "A", hts_combos = "A"), NULL, run_info, "object", "prep_data", "-hts_info")
  predictions[, setdiff(names(predictions), "Run_Type")]
}

test_that("completed hierarchy restart reuses discovery and checks the exact reconciliation", {
  run_info <- artifact_test_run(withr::local_tempdir())
  predictions <- artifact_completed_final_outputs(run_info)
  write_data(predictions, "Best-Model", run_info, "data", "forecasts", "-reconciled")
  tracker <- local_artifact_spies(max_listings = 1L)
  testthat::local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    reconcile_hierarchical_data = function(...) stop("completed reconciliation reran", call. = FALSE),
    .package = "finnts"
  )

  expect_no_error(final_models(run_info))
  expect_equal(tracker$listings, 1L)
})

test_that("missing reconciliation is not satisfied by another run output", {
  run_info <- artifact_test_run(withr::local_tempdir())
  predictions <- artifact_completed_final_outputs(run_info)
  write_data(predictions, "unrelated", run_info, "data", "forecasts", "-reconciled")
  tracker <- local_artifact_spies(max_listings = 1L)
  testthat::local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    reconcile_hierarchical_data = function(...) stop("missing reconciliation dispatched", call. = FALSE),
    .package = "finnts"
  )

  expect_error(final_models(run_info), "missing reconciliation dispatched")
  expect_equal(tracker$listings, 1L)
})

local_artifact_logger <- function(agent_info, run_info, .env = parent.frame()) {
  log <- tibble::tibble(project_name = run_info$project_name, run_name = run_info$run_name,
    path = run_info$path, data_output = run_info$data_output, object_output = run_info$object_output,
    weighted_mape = 0.1, created = "2024-01-01 00:00:00"
  )
  forecasts <- tibble::tibble(Combo = c("A", "B"), Target = 100, Forecast = c(90, 110),
    Best_Model = "Yes", Run_Type = "Back_Test"
  )
  testthat::local_mocked_bindings(
    get_run_info = function(...) log,
    load_combo_forecast = function(...) forecasts,
    .package = "finnts", .env = .env
  )
}

test_that("global best-run verification reads the expected files directly", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$agent_version <- 1
  agent_info$forecast_approach <- "bottoms_up"
  run_info <- agent_info$project_info
  run_info$run_name <- "candidate"
  local_artifact_logger(agent_info, run_info)
  tracker <- local_artifact_spies()

  expect_identical(log_best_run(agent_info, run_info, 0.1, check_best_run = FALSE),
    "Run logged successfully."
  )
  expect_equal(tracker$listings, 0L)
  for (combo in c("A", "B")) {
    path <- artifact_test_path(
      agent_info$project_info, "logs", combo, "-agent_best_run", "csv"
    )
    expect_true(fs::file_exists(path))
    expect_equal(sum(tracker$metadata_paths == as.character(path)), 1L)
    expect_equal(sum(tracker$access_paths == as.character(path)), 1L)
  }
})

test_that("missing best-run writes cannot be masked by unrelated files", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$agent_version <- 1
  agent_info$forecast_approach <- "bottoms_up"
  run_info <- agent_info$project_info
  run_info$run_name <- "candidate"
  write_data(tibble::tibble(combo = "unrelated"), "unrelated", agent_info$project_info,
    "log", "logs", "-agent_best_run"
  )
  local_artifact_logger(agent_info, run_info)
  original_write <- write_data
  testthat::local_mocked_bindings(
    write_data = function(x, combo, run_info, output_type, folder = NULL, suffix = NULL) {
      if (identical(combo, "B") && identical(suffix, "-agent_best_run")) return(invisible(NULL))
      original_write(x, combo, run_info, output_type, folder, suffix)
    },
    .package = "finnts"
  )
  tracker <- local_artifact_spies()

  expect_error(log_best_run(agent_info, run_info, 0.1, check_best_run = FALSE),
    "Expected 2.*only found 1"
  )
  expect_equal(tracker$listings, 0L)
})