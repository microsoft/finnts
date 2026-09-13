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

artifact_completed_final_outputs <- function(run_info, combos = "A", date_type = "month") {
  log <- artifact_test_model_log(run_info, "standard_hierarchy")
  log$hist_end_date <- as.Date("2024-01-01")
  log$date_type <- date_type
  write_data(log, NULL, run_info, "log", "logs")
  future_date <- seq(log$hist_end_date, by = date_type, length.out = 2L)[2]
  history <- tibble::tibble(Combo = "A",
    Date = c(rev(seq(log$hist_end_date, by = paste("-1", date_type), length.out = 13L)), future_date),
    Target = c(rep(100, 13), NA_real_))
  write_data(history, "A", run_info, "data", "prep_data", "-R1")
  splits <- tibble::tibble(Run_Type = c("Future_Forecast", "Back_Test"), Train_Test_ID = c(1, 2),
    Train_End = c(log$hist_end_date, history$Date[12]),
    Test_End = c(future_date, log$hist_end_date))
  write_data(splits, NULL, run_info, "data", "prep_models", "-train_test_split")
  forecast <- artifact_test_forecast(run_info, write_output = FALSE)
  forecast$Date <- future_date
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
  predictions <- dplyr::bind_rows(lapply(combos, function(combo) {
    rows <- predictions
    rows$Combo <- rows$Combo_ID <- combo
    if (combo != "A") {
      history$Combo <- combo
      average$Combo <- average$Combo_ID <- combo
      write_data(history, combo, run_info, "data", "prep_data", "-R1")
      write_data(rows, combo, run_info, "data", "forecasts", "-single_models")
      write_data(average, combo, run_info, "data", "forecasts", "-average_models")
    }
    rows
  }))
  write_data(list(original_combos = combos, hts_combos = combos), NULL, run_info, "object", "prep_data", "-hts_info")
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

test_that("damaged reconciled future rows are rebuilt without reselecting sources", {
  for (defect in c("missing_future", "nonfinite_future")) {
    run_info <- artifact_test_run(withr::local_tempdir())
    expected <- artifact_completed_final_outputs(run_info)
    damaged <- expected
    if (defect == "missing_future") {
      damaged <- damaged[damaged$Train_Test_ID != 1, ]
    } else {
      damaged$Forecast[damaged$Train_Test_ID == 1] <- Inf
    }
    write_data(damaged, "Best-Model", run_info, "data", "forecasts", "-reconciled")
    reconciliations <- 0L
    local_mocked_bindings(
      par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
      train_models = function(...) stop("restart must not train models"),
      select_series_forecasts = function(...) stop("complete sources must not be reselected"),
      reconcile_hierarchical_data = function(run_info, ...) {
        reconciliations <<- reconciliations + 1L
        write_data(expected, "Best-Model", run_info, "data", "forecasts", "-reconciled")
      },
      .package = "finnts"
    )

    result <- final_models(run_info)

    expect_equal(reconciliations, 1L, info = defect)
    expect_equal(read_selection_file(run_info, "forecasts", "-reconciled", "Best-Model"),
      expected, info = defect)
    expect_length(result$rejected_combos, 0L)
  }
})

for (format in c("csv", "rds", "parquet")) {
  test_that(paste("reconciled restart repairs series and key coverage for", format), {
    if (format == "parquet") skip_if_not_installed("arrow")
    run_info <- artifact_test_run(withr::local_tempdir(), format)
    expected <- artifact_completed_final_outputs(run_info, combos = c("A", "B"))
    reconciliations <- 0L
    local_mocked_bindings(
      par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
      train_models = function(...) stop("restart must not train models"),
      select_series_forecasts = function(...) stop("complete sources must not be reselected"),
      reconcile_hierarchical_data = function(run_info, ...) {
        reconciliations <<- reconciliations + 1L
        write_data(expected, "Best-Model", run_info, "data", "forecasts", "-reconciled")
      },
      .package = "finnts"
    )
    for (defect in c("missing_series", "extra_series", "missing_backtest", "duplicate_future",
      "duplicate_backtest", "nonfinite_backtest", "missing_flag", "multiple_winners", "invalid_date", "empty")) {
      damaged <- expected
      if (defect == "missing_series") damaged <- damaged[damaged$Combo == "A", ]
      if (defect == "extra_series") {
        extra <- damaged[damaged$Combo == "A", ]
        extra$Combo <- extra$Combo_ID <- "Unexpected"
        damaged <- dplyr::bind_rows(damaged, extra)
      }
      if (defect == "missing_backtest") damaged <- damaged[-2, ]
      if (defect == "duplicate_future") damaged <- dplyr::bind_rows(damaged, damaged[1, ])
      if (defect == "duplicate_backtest") damaged <- dplyr::bind_rows(damaged, damaged[2, ])
      if (defect == "nonfinite_backtest") damaged$Forecast[2] <- NaN
      if (defect == "missing_flag") damaged$Best_Model <- NULL
      if (defect == "multiple_winners") damaged$Model_ID[1] <- "other--local--R1"
      if (defect == "invalid_date") {
        damaged$Date <- as.character(damaged$Date)
        damaged$Date[1] <- "invalid"
      }
      if (defect == "empty") damaged <- damaged[0, ]
      write_data(damaged, "Best-Model", run_info, "data", "forecasts", "-reconciled")
      reconciliations <- 0L

      result <- final_models(run_info)

      expect_equal(reconciliations, 1L, info = defect)
      expect_setequal(names(result$selections), c("A", "B"))
      expect_length(result$rejected_combos, 0L)
      saved <- if (format == "parquet") {
        arrow::read_parquet(artifact_test_path(run_info, "forecasts", "Best-Model", "-reconciled"), mmap = FALSE)
      } else read_selection_file(run_info, "forecasts", "-reconciled", "Best-Model")
      expect_equal(saved, expected, info = defect)
    }
  })
}

test_that("daily-expanded reconciliation requires every expected daily key", {
  run_info <- artifact_test_run(withr::local_tempdir())
  expected <- artifact_completed_final_outputs(run_info, combos = c("A", "B"), date_type = "week")
  expected <- convert_weekly_to_daily(expected, "week", TRUE)
  reconciliations <- 0L
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    select_series_forecasts = function(...) stop("complete sources must not be reselected"),
    reconcile_hierarchical_data = function(run_info, ...) {
      reconciliations <<- reconciliations + 1L
      write_data(expected, "Best-Model", run_info, "data", "forecasts", "-reconciled")
    },
    .package = "finnts"
  )
  for (defect in c("missing_day", "duplicate_day", "nonfinite_later_day", "missing_daily_column", "wrong_week")) {
    damaged <- expected
    if (defect == "missing_day") damaged <- damaged[-2, ]
    if (defect == "duplicate_day") damaged <- dplyr::bind_rows(damaged, damaged[2, ])
    if (defect == "nonfinite_later_day") damaged$Forecast[2] <- Inf
    if (defect == "missing_daily_column") damaged$Date_Day <- NULL
    if (defect == "wrong_week") damaged$Date[2] <- damaged$Date[2] + 7
    write_data(damaged, "Best-Model", run_info, "data", "forecasts", "-reconciled")
    reconciliations <- 0L

    result <- final_models(run_info, weekly_to_daily = TRUE)

    expect_equal(reconciliations, 1L, info = defect)
    expect_length(result$rejected_combos, 0L)
    expect_equal(read_selection_file(run_info, "forecasts", "-reconciled", "Best-Model"), expected,
      info = defect)
  }
})

test_that("valid reconciliation is read once and shares cached hierarchy history", {
  for (settings in list(
    list(date_type = "month", weekly_to_daily = FALSE),
    list(date_type = "week", weekly_to_daily = FALSE),
    list(date_type = "week", weekly_to_daily = TRUE)
  )) local({
    run_info <- artifact_test_run(withr::local_tempdir())
    expected <- artifact_completed_final_outputs(run_info, combos = c("A", "B"), date_type = settings$date_type)
    expected$Forecast[expected$Train_Test_ID == 1] <- 1e6
    expected <- convert_weekly_to_daily(expected, settings$date_type, settings$weekly_to_daily)
    write_data(expected, "Best-Model", run_info, "data", "forecasts", "-reconciled")
    tracker <- local_artifact_spies(max_listings = 1L)
    local_mocked_bindings(
      par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
      train_models = function(...) stop("restart must not train models"),
      select_series_forecasts = function(...) stop("complete sources must not be reselected"),
      reconcile_hierarchical_data = function(...) stop("valid reconciliation must be reused"),
      .package = "finnts"
    )

    result <- final_models(run_info, weekly_to_daily = settings$weekly_to_daily)

    expect_length(result$rejected_combos, 0L)
    expect_equal(tracker$listings, 1L)
    expect_equal(sum(tracker$payload_paths == artifact_test_path(run_info, "forecasts", "Best-Model", "-reconciled")), 1L)
    expect_equal(sum(tracker$payload_paths == artifact_test_path(run_info, "prep_data", suffix = "-hts_info", extension = "rds")), 1L)
    for (combo in c("A", "B")) {
      expect_equal(sum(tracker$payload_paths == artifact_test_path(run_info, "prep_data", combo, "-R1")), 2L)
    }
  })
})

test_that("invalid regenerated reconciliation cannot write a completion log", {
  for (cached in c(FALSE, TRUE)) local({
    run_info <- artifact_test_run(withr::local_tempdir())
    predictions <- artifact_completed_final_outputs(run_info)
    if (cached) {
      write_data(predictions[-1, ], "Best-Model", run_info, "data", "forecasts", "-reconciled")
    }
    previous_log <- read_selection_file(run_info, "logs")
    predictions$Forecast[1] <- Inf
    reconciliations <- 0L
    log_writes <- 0L
    original_writer <- write_data
    local_mocked_bindings(
      par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
      select_series_forecasts = function(...) stop("complete sources must not be reselected"),
      reconcile_hierarchical_data = function(run_info, ...) {
        reconciliations <<- reconciliations + 1L
        original_writer(predictions, "Best-Model", run_info, "data", "forecasts", "-reconciled")
      },
      write_data = function(x, combo, run_info, output_type, folder = NULL, suffix = NULL) {
        if (identical(output_type, "log")) log_writes <<- log_writes + 1L
        original_writer(x, combo, run_info, output_type, folder, suffix)
      },
      .package = "finnts"
    )

    expect_error(final_models(run_info), "Reconciled forecasts are incomplete or invalid", fixed = TRUE)
    expect_equal(reconciliations, 1L)
    expect_equal(log_writes, 0L)
    expect_equal(read_selection_file(run_info, "logs"), previous_log)
  })
})

test_that("reconciled artifact access and deserialization errors propagate", {
  for (failure in c("metadata", "payload", "deserialization")) local({
    run_info <- artifact_test_run(withr::local_tempdir(), "rds")
    predictions <- artifact_completed_final_outputs(run_info)
    write_data(predictions, "Best-Model", run_info, "data", "forecasts", "-reconciled")
    path <- artifact_test_path(run_info, "forecasts", "Best-Model", "-reconciled")
    reconciliations <- 0L
    original_reader <- read_file
    original_info <- fs::file_info
    local_mocked_bindings(
      par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
      reconcile_hierarchical_data = function(...) { reconciliations <<- reconciliations + 1L },
      .package = "finnts"
    )
    if (failure == "metadata") {
      local_mocked_bindings(file_info = function(path, ...) {
        if (any(grepl("-reconciled", path, fixed = TRUE))) {
          rlang::abort("reconciliation access denied", class = "reconciliation_storage_error")
        }
        original_info(path, ...)
      }, .package = "fs")
    } else if (failure == "payload") {
      local_mocked_bindings(read_file = function(run_info, path = NULL, file_list = NULL, ...) {
        if (any(grepl("-reconciled", c(path, file_list), fixed = TRUE))) {
          rlang::abort("reconciliation access denied", class = "reconciliation_storage_error")
        }
        original_reader(run_info, path, file_list, ...)
      }, .package = "finnts")
    } else writeLines("invalid serialized artifact", path)

    if (failure == "deserialization") {
      expect_error(final_models(run_info), "unknown input format|error reading from connection")
    } else {
      expect_error(final_models(run_info), class = "reconciliation_storage_error")
    }
    expect_equal(reconciliations, 0L)
  })
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