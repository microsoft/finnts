test_that("local EDA reads exact artifacts with independently expected metrics", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$external_regressors <- "price"
  artifact_test_eda(agent_info)
  tracker <- local_artifact_spies()
  tables <- list()
  original_table <- make_pipe_table
  testthat::local_mocked_bindings(
    make_pipe_table = function(data) {
      tables[[length(tables) + 1L]] <<- data
      original_table(data)
    },
    .package = "finnts"
  )

  result <- load_eda_results(agent_info, hash_data("A"))

  expect_equal(tables, list(
    tibble::tibble(Lag = 1, Value = 0.5),
    tibble::tibble(Lag = 1, Value = 0.4),
    tibble::tibble(stationary_adf = TRUE, stationary_kpss = TRUE),
    tibble::tibble(Lag = 1, Value = 0.2),
    tibble::tibble(Regressor = "price", Lag = 1, dCor = 0.25)
  ))
  expect_match(result, "price")
  expect_match(result, "None observed")
  expect_match(result, "Missing Count: 0", fixed = TRUE)
  expect_match(result, "Outlier Count: 0", fixed = TRUE)
  expect_equal(tracker$listings, 0L)
  expect_equal(tracker$directory_listings, 0L)
  expect_length(tracker$reads, 9L)
})

test_that("local EDA for a different combo cannot reuse the first series metrics", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$external_regressors <- "price"
  artifact_test_eda(agent_info)
  tracker <- local_artifact_spies()
  tables <- list()
  original_table <- make_pipe_table
  testthat::local_mocked_bindings(
    make_pipe_table = function(data) {
      tables[[length(tables) + 1L]] <<- data
      original_table(data)
    },
    .package = "finnts"
  )

  result <- load_eda_results(agent_info, hash_data("B"))

  expect_equal(tables, list(
    tibble::tibble(Lag = 1, Value = -0.3),
    tibble::tibble(Lag = 1, Value = 0.8),
    tibble::tibble(stationary_adf = FALSE, stationary_kpss = TRUE),
    tibble::tibble(Lag = 1, Value = 0.6),
    tibble::tibble(Regressor = "price", Lag = 1, dCor = 0.75)
  ))
  expect_match(result, "Missing Count: 2", fixed = TRUE)
  expect_match(result, "Missing Percent: 25%", fixed = TRUE)
  expect_match(result, "Outlier Count: 2", fixed = TRUE)
  expect_match(result, "First Outlier Date: 2020-03-01", fixed = TRUE)
  expect_match(result, "Last Outlier Date: 2020-06-01", fixed = TRUE)
  expect_equal(tracker$directory_listings, 0L)
})

test_that("local EDA does not require regressor artifacts when regressors are disabled", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  artifact_test_eda(agent_info, omit = "xreg_scan")
  tracker <- local_artifact_spies()

  result <- load_eda_results(agent_info, hash_data("A"))

  expect_match(result, "No external regressors")
  expect_equal(tracker$listings, 0L)
  expect_length(tracker$reads, 8L)
})

test_that("missing required local EDA is not mistaken for another combo", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  artifact_test_eda(agent_info, combos = "B")
  tracker <- local_artifact_spies()

  expect_error(load_eda_results(agent_info, hash_data("A")),
    "Missing required Finn artifact.*acf"
  )
  expect_equal(tracker$listings, 0L)
})

test_that("singleton EDA caches do not enumerate input or EDA folders", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$project_info$combo_variables <- c("Country", "Product")
  artifact_test_eda(agent_info)
  tracker <- local_artifact_spies()

  expect_match(data_profile(agent_info), "already exists")
  expect_match(hierarchy_detect(agent_info), "already exists")
  expect_equal(tracker$listings, 0L)
})

test_that("combo resolution reads only requested inputs and retains missing fallback", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  artifact_test_eda(agent_info)
  missing_hash <- hash_data("missing")
  tracker <- local_artifact_spies()

  result <- resolve_combo_hashes(agent_info, c(hash_data("A"), missing_hash))

  expect_identical(result, c("A", missing_hash))
  expect_equal(tracker$listings, 0L)
  expect_length(tracker$reads, 1L)
})

test_that("local submission reads its exact input before starting modeling", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  artifact_test_eda(agent_info)
  tracker <- local_artifact_spies()
  testthat::local_mocked_bindings(
    set_run_info = function(...) stop("submission input verified", call. = FALSE),
    .package = "finnts"
  )

  expect_error(submit_fcst_run(agent_info, list(), hash_data("A"), "test"),
    "submission input verified"
  )
  expect_equal(tracker$listings, 0L)
  expect_length(tracker$reads, 1L)
  expect_equal(tracker$reads[[1]]$file_list,
    as.character(artifact_test_path(agent_info$project_info, "input_data", "A"))
  )
})

for (provider in c("blob_container", "ms_drive")) {
  test_that(paste("best-run retrieval downloads CSV metadata independently of data format on", provider), {
    source <- artifact_test_agent(withr::local_tempdir(), "rds")
    expected <- tibble::tibble(combo = "A", model_type = "local", best_run_name = "selected", weighted_mape = 0.1)
    write_data(expected, "A", source$project_info, "log", "logs", "-agent_best_run")
    agent <- source
    agent$project_info$path <- "provider-artifacts"
    transport <- local_artifact_provider(provider)
    agent$project_info$storage_object <- transport$storage_object
    path <- artifact_test_path(agent$project_info, "logs", "A", "-agent_best_run", "csv")
    transport$files[[as.character(path)]] <- artifact_test_path(source$project_info, "logs", "A", "-agent_best_run", "csv")
    listings <- 0L
    local_mocked_bindings(list_files = function(storage_object, path, fail_on_error = FALSE) {
      listings <<- listings + 1L
      expect_true(fail_on_error)
      expect_match(path, "-agent_best_run[.]csv$")
      files <- names(transport$files)
      if (provider == "ms_drive") fs::path_file(files) else files
    })

    result <- load_best_agent_run(agent)

    expect_equal(result, expected)
    expect_equal(listings, 1L)
    expect_identical(transport$downloads, as.character(path))
  })
}

# Local best-run records for 150 series with the given `lag_periods` values
# (character, NA for none) and fixed metric, flag and timestamp columns, in
# series order S001 to S150. vroom guesses CSV column types from about 100
# evenly spaced rows plus the last one, so at 150 rows it never samples rows
# 100 to 149: settings placed there are the ones type guessing can miss.
best_run_type_records <- function(lag_periods) {
  tibble::tibble(
    combo = sprintf("S%03d", seq_along(lag_periods)),
    model_type = "local",
    agent_run_id = "007",
    best_run_name = sprintf("agent_007_%03d", seq_along(lag_periods)),
    weighted_mape = seq_along(lag_periods) / 1000,
    lag_periods = lag_periods,
    created = "2026-11-12 04:23:41",
    run_complete = TRUE,
    max_iterations = 3
  )
}

# Write each record as its own "-agent_best_run" log and return the exact
# paths in record order, so a mocked listing controls the row order read.
write_best_run_type_records <- function(agent_info, records) {
  vapply(seq_len(nrow(records)), function(index) {
    write_data(records[index, ], records$combo[[index]], agent_info$project_info, "log", "logs", "-agent_best_run")
    as.character(artifact_test_path(agent_info$project_info, "logs", records$combo[[index]], "-agent_best_run", "csv"))
  }, character(1))
}

test_that("best-run retrieval keeps settings values that sampled type guessing misses", {
  agent <- artifact_test_agent(withr::local_tempdir())
  lag_periods <- rep(NA_character_, 150)
  lag_periods[c(120, 135)] <- c("1---2", "1")
  records <- best_run_type_records(lag_periods)
  paths <- write_best_run_type_records(agent, records)
  local_mocked_bindings(list_files = function(storage_object, path, fail_on_error = FALSE) paths)

  result <- load_best_agent_run(agent)

  expect_identical(result$combo, records$combo)
  expect_identical(result$lag_periods, lag_periods)
  expect_equal(result$weighted_mape, records$weighted_mape)
  expect_identical(result$run_complete, records$run_complete)
})

test_that("best-run retrieval keeps column types for ordinary records", {
  agent <- artifact_test_agent(withr::local_tempdir())
  lag_periods <- rep(c("3", NA_character_), 75)
  records <- best_run_type_records(lag_periods)
  paths <- write_best_run_type_records(agent, records)
  local_mocked_bindings(list_files = function(storage_object, path, fail_on_error = FALSE) paths)

  result <- load_best_agent_run(agent)

  expect_identical(result$combo, records$combo)
  expect_identical(result$agent_run_id, records$agent_run_id)
  expect_identical(result$lag_periods, as.numeric(lag_periods))
  expect_equal(result$weighted_mape, records$weighted_mape)
  expect_identical(result$run_complete, records$run_complete)
  expect_identical(result$max_iterations, records$max_iterations)
  expect_s3_class(result$created, "POSIXct")
})

test_that("run metadata keeps settings values that sampled type guessing misses", {
  agent <- c(
    artifact_test_agent(withr::local_tempdir()),
    list(agent_version = 1, llm = NULL, combo_cleanup_date = NULL, overwrite = FALSE)
  )
  lag_periods <- rep(NA_character_, 150)
  lag_periods[c(120, 135)] <- c("1---2", "1")
  metadata <- best_run_type_records(lag_periods) %>%
    dplyr::select(-run_complete, -max_iterations)
  writer_info <- agent$project_info
  writer_info$run_name <- agent$run_id
  write_data(metadata, NULL, writer_info, "data", "final_output", "-run_metadata")

  result <- get_best_agent_run(agent)

  expect_identical(result$combo, metadata$combo)
  expect_identical(result$lag_periods, lag_periods)
  expect_equal(result$weighted_mape, metadata$weighted_mape)
})

for (provider in c("blob_container", "ms_drive")) {
  test_that(paste("hierarchical Agent publication downloads only its exact best artifact on", provider), {
    source <- artifact_test_run(withr::local_tempdir(), "rds")
    best <- artifact_test_forecast(source, write_output = FALSE)
    best$Model_ID <- "Best-Model"
    write_data(best, "Best-Model", source, "data", "forecasts", "-reconciled")
    transport <- local_artifact_provider(provider)
    project <- source
    project$path <- "provider-artifacts"
    project$storage_object <- transport$storage_object
    agent <- list(run_id = source$run_name, project_info = project, forecast_approach = "standard_hierarchy")
    best_path <- artifact_test_path(project, "forecasts", "Best-Model", "-reconciled")
    alternative_path <- artifact_test_path(project, "forecasts", "meanf--local--R1", "-reconciled")
    transport$files[[as.character(best_path)]] <- artifact_test_path(source, "forecasts", "Best-Model", "-reconciled")
    transport$files[[as.character(alternative_path)]] <- transport$files[[as.character(best_path)]]
    local_mocked_bindings(check_agent_info = function(...) NULL)

    result <- load_agent_forecast(agent, final_output = TRUE)

    expect_equal(result, best)
    expect_identical(transport$downloads, as.character(best_path))
    transport$files[[as.character(best_path)]] <- NULL
    expect_error(load_agent_forecast(agent, final_output = TRUE), class = "http_404")
  })
}

test_that("hierarchical Agent publication and getter ignore per-model reconciled alternatives", {
  agent <- artifact_test_agent(withr::local_tempdir())
  agent$forecast_approach <- "grouped_hierarchy"
  info <- agent$project_info
  info$run_name <- agent$run_id
  best <- artifact_test_forecast(info, write_output = FALSE)
  best$Model_ID <- "Best-Model"
  historical <- best
  historical$Train_Test_ID <- 2
  historical$Date <- as.Date("2024-01-01")
  historical$Target <- 100
  best <- dplyr::bind_rows(best, historical)
  write_data(best, "Best-Model", info, "data", "forecasts", "-reconciled")
  alternative <- best
  alternative$Model_ID <- "arima--local--R1"
  alternative$Best_Model <- "No"
  alternative$Forecast <- 1e6
  write_data(alternative, alternative$Model_ID, info, "data", "forecasts", "-reconciled")
  tracker <- local_artifact_spies()
  local_mocked_bindings(check_agent_info = function(...) NULL)

  save_agent_forecast(agent)
  result <- get_agent_forecast(agent)

  expect_equal(result, best)
  expect_identical(unique(result$Model_ID), "Best-Model")
  expect_true(all(result$Best_Model == "Yes"))
  expect_equal(tracker$listings, 0L)
  expect_false(any(grepl(hash_data(alternative$Model_ID), tracker$payload_paths, fixed = TRUE)))
})

for (format in c("csv", "rds", "parquet")) {
  test_that(paste("Agent forecast getter rejects missing final output and preserves", format), {
    if (format == "parquet") skip_if_not_installed("arrow")
    local_mocked_bindings(check_agent_info = function(...) NULL)
    agent <- artifact_test_agent(withr::local_tempdir(), format)
    info <- agent$project_info
    info$run_name <- agent$run_id
    path <- artifact_test_path(info, "final_output", suffix = "-forecast")

    expect_error(get_agent_forecast(agent), "Missing required Finn artifact")
    future <- artifact_test_forecast(info, write_output = FALSE)
    historical <- future
    historical$Train_Test_ID <- 2
    historical$Date <- as.Date("2024-01-01")
    historical$Target <- 100
    expected <- dplyr::bind_rows(future, historical)
    write_data(expected, NULL, info, "data", "final_output", "-forecast")
    original <- agent
    tracker <- local_artifact_spies()
    expect_equal(get_agent_forecast(agent), expected)
    expect_identical(agent, original)
    expect_equal(tracker$listings, 0L)
    expect_equal(tracker$directory_listings, 0L)
    if (format == "rds") {
      writeLines("not an RDS forecast", path)
      expect_error(get_agent_forecast(agent), "unknown input format")
    }
  })
}

for (provider in c("blob_container", "ms_drive")) {
  test_that(paste("Agent forecast getter propagates missing and failed downloads on", provider), {
    source <- artifact_test_agent(withr::local_tempdir(), "rds")
    info <- source$project_info
    info$run_name <- source$run_id
    expected <- artifact_test_forecast(info, write_output = FALSE)
    write_data(expected, NULL, info, "data", "final_output", "-forecast")
    source_path <- artifact_test_path(info, "final_output", suffix = "-forecast")
    agent <- source
    agent$project_info$path <- "provider-artifacts"
    agent$project_info$storage_object <- structure(list(), class = provider)
    downloads <- character()
    failure <- artifact_http_error()
    # Simulate only transport; exact-path lookup, strict deserialization and
    # provider condition propagation still execute through the shared reader.
    local_mocked_bindings(
      check_agent_info = function(...) NULL,
      list_files = function(...) stop("getter must not enumerate directories"),
      download_exact_artifact = function(storage_object, path, destination, allow_missing) {
        expect_false(allow_missing)
        downloads <<- c(downloads, as.character(path))
        if (!is.null(failure)) stop(failure)
        fs::file_copy(source_path, destination, overwrite = TRUE)
        TRUE
      }
    )
    expect_error(get_agent_forecast(agent), class = "http_404")
    failure <- artifact_http_error(403L, "access denied")
    expect_error(get_agent_forecast(agent), class = "http_403")
    failure <- simpleError("storage unavailable")
    expect_error(get_agent_forecast(agent), "storage unavailable")
    failure <- NULL
    expect_equal(get_agent_forecast(agent), expected)
    expect_identical(unique(downloads), as.character(fs::path(
      agent$project_info$path, "final_output", fs::path_file(source_path))))
    expect_length(downloads, 4L)
  })
}

test_that("reconciliation reads the selected run split by exact path", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  project_info <- agent_info$project_info
  selected <- project_info
  selected$project_name <- paste0(project_info$project_name, "_", hash_data("A"))
  selected$run_name <- "selected"
  write_data(tibble::tibble(Run_Type = "Future_Forecast", Train_Test_ID = 1),
    NULL, selected, "data", "prep_models", "-train_test_split"
  )
  tracker <- local_artifact_spies()
  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    get_best_agent_run = function(...) tibble::tibble(
      negative_forecast = FALSE, model_type = "local", combo = "A", best_run_name = "selected"
    ),
    load_agent_forecast = function(...) stop("reconciliation split verified", call. = FALSE),
    .package = "finnts"
  )

  expect_error(reconcile_agent_forecast(agent_info, project_info),
    "reconciliation split verified"
  )
  expect_equal(tracker$listings, 0L)
  expect_equal(tracker$reads[[1]]$file_list,
    as.character(artifact_test_path(selected, "prep_models", suffix = "-train_test_split"))
  )
})

# Agent run with one local series, "A", saved at two iterations whose forecasts
# differ (100 and 999). The saved run_metadata names "iteration-1" while the
# live best-run record names "iteration-2", as after a stray worker's late write.
# Returns the agent info for `forecast_approach`.
write_late_best_run_fixture <- function(path, forecast_approach) {
  agent <- artifact_test_agent(path)
  agent$forecast_approach <- forecast_approach
  agent$project_info$weekly_to_daily <- FALSE
  info <- agent$project_info
  info$run_name <- agent$run_id
  for (run in c("iteration-1", "iteration-2")) {
    selected <- agent$project_info
    selected$project_name <- paste0(selected$project_name, "_", hash_data("A"))
    selected$run_name <- run
    rows <- artifact_test_forecast(selected, "A", write_output = FALSE)
    rows$Forecast <- if (identical(run, "iteration-1")) 100 else 999
    write_data(rows, "A", selected, "data", "forecasts", "-single_models")
    artifact_test_splits(selected)
  }
  record <- function(run) {
    tibble::tibble(
      combo = "A", model_type = "local", agent_run_id = agent$run_id, best_run_name = run,
      weighted_mape = 0.1, recipes_to_run = "R1", models_to_run = "meanf",
      average_models = FALSE, negative_forecast = FALSE
    )
  }
  write_data(record("iteration-1"), NULL, info, "data", "final_output", "-run_metadata")
  write_data(dplyr::mutate(record("iteration-2"), max_iterations = 3, run_complete = TRUE),
    "A", info, "log", "logs", "-agent_best_run"
  )
  agent
}

test_that("saved Agent forecasts use the run_metadata iteration, not a later best-run record", {
  agent <- write_late_best_run_fixture(withr::local_tempdir(), "bottoms_up")
  info <- agent$project_info
  info$run_name <- agent$run_id
  local_mocked_bindings(check_agent_info = function(...) NULL)
  tracker <- local_artifact_spies(max_listings = 1L)

  save_agent_forecast(agent)

  saved <- read_exact_artifact(info, artifact_test_path(info, "final_output", suffix = "-forecast"))
  expect_identical(unique(saved$Forecast), 100)
  expect_equal(tracker$listings, 0L)
})

test_that("hierarchical reconciliation uses the run_metadata iteration, not a later best-run record", {
  agent <- write_late_best_run_fixture(withr::local_tempdir(), "standard_hierarchy")
  reconciled <- NULL
  local_mocked_bindings(
    check_agent_info = function(...) NULL,
    reconcile = function(forecasts, ...) {
      reconciled <<- forecasts
      stop("reconciliation input captured", call. = FALSE)
    }
  )
  tracker <- local_artifact_spies(max_listings = 1L)

  expect_error(reconcile_agent_forecast(agent, agent$project_info), "reconciliation input captured")
  expect_identical(unique(reconciled$Forecast), 100)
  expect_equal(tracker$listings, 0L)
})

for (provider in c("blob_container", "ms_drive")) {
  test_that(paste("outer Agent reconciliation transfers the required exact split on", provider), {
    agent <- artifact_test_agent(withr::local_tempdir(), "rds")
    selected <- agent$project_info
    selected$project_name <- paste0(selected$project_name, "_", hash_data("A"))
    selected$run_name <- "selected"
    artifact_test_splits(selected)
    source_path <- artifact_test_path(selected, "prep_models", suffix = "-train_test_split")
    agent$project_info$path <- "provider-artifacts"
    transport <- local_artifact_provider(provider)
    agent$project_info$storage_object <- transport$storage_object
    selected$path <- agent$project_info$path
    path <- artifact_test_path(selected, "prep_models", suffix = "-train_test_split")
    transport$files[[as.character(path)]] <- source_path
    local_mocked_bindings(
      check_agent_info = function(...) NULL,
      get_best_agent_run = function(...) tibble::tibble(negative_forecast = FALSE,
        model_type = "local", combo = "A", best_run_name = "selected"),
      load_agent_forecast = function(...) stop("exact split successfully loaded", call. = FALSE)
    )

    expect_error(reconcile_agent_forecast(agent, agent$project_info),
      "exact split successfully loaded", fixed = TRUE)
    expect_identical(transport$downloads, as.character(path))
  })
}

test_that("model summaries consolidate only expected combo artifacts", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  best_runs <- artifact_test_selected_models(agent_info)
  local_artifact_summary_mocks(best_runs)
  write_data(tibble::tibble(Combo = "unrelated", Best_Model = "Yes"), "unrelated",
    agent_info$project_info, "data", "models", "-model_summary"
  )
  tracker <- local_artifact_spies()

  summarize_models(agent_info)
  result <- get_summarized_models(agent_info)

  expect_setequal(result$Combo, c("A", "B"))
  expect_true(all(result$Best_Model == "Yes"))
  expect_equal(tracker$listings, 0L)
})

test_that("model summaries re-read model and forecast files not yet visible on mounted storage", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  best_runs <- artifact_test_selected_models(agent_info)
  local_artifact_summary_mocks(best_runs)
  original_read <- read_file
  failed <- character()
  # Each series' model and forecast files fail on their first read, as on the
  # Spark executor that reported "No trained models found" in runbook check A3.
  testthat::local_mocked_bindings(
    settle_waits = function(path) c(0, 0),
    read_file = function(run_info, path = NULL, ...) {
      if (!is.null(path) && grepl("-single_models.", path, fixed = TRUE) && !path %in% failed) {
        failed <<- c(failed, path)
        stop("cannot open file: not visible yet", call. = FALSE)
      }
      original_read(run_info, path = path, ...)
    },
    .package = "finnts"
  )

  messages <- character()
  withCallingHandlers(summarize_models(agent_info), message = function(condition) {
    messages <<- c(messages, conditionMessage(condition))
    invokeRestart("muffleMessage")
  })
  expect_length(failed, 4L)
  expect_identical(sum(grepl("^Re-reading (trained models|forecasts) for [AB]:", messages)), 4L)
  result <- get_summarized_models(agent_info)
  expect_setequal(result$Combo, c("A", "B"))
  expect_true(all(result$Best_Model == "Yes"))
})

test_that("model summaries re-read a stale forecast copy that has no selected model yet", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  best_runs <- artifact_test_selected_models(agent_info)
  local_artifact_summary_mocks(best_runs)
  original_read <- read_file
  stale <- character()
  # A mount can return a non-empty forecast copy without this series' winner:
  # A gets train_models()' copy, which has no Best_Model column, and B a copy
  # whose winning average file is not visible yet, so every row is "No".
  testthat::local_mocked_bindings(
    settle_waits = function(path) c(0, 0),
    read_file = function(run_info, path = NULL, ...) {
      value <- original_read(run_info, path = path, ...)
      if (!is.null(path) && grepl("/forecasts/.*-single_models\\.", path) && !path %in% stale) {
        stale <<- c(stale, path)
        if (identical(value$Combo[[1]], "A")) value$Best_Model <- NULL else value$Best_Model <- "No"
      }
      value
    },
    .package = "finnts"
  )

  messages <- character()
  withCallingHandlers(summarize_models(agent_info), message = function(condition) {
    messages <<- c(messages, conditionMessage(condition))
    invokeRestart("muffleMessage")
  })
  expect_length(stale, 2L)
  expect_identical(sum(grepl("^Re-reading forecasts for [AB]:", messages)), 2L)
  result <- get_summarized_models(agent_info)
  expect_setequal(result$Combo, c("A", "B"))
  expect_true(all(result$Best_Model == "Yes"))
})

test_that("model summary consolidation fails on a missing expected write", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  best_runs <- artifact_test_selected_models(agent_info)
  local_artifact_summary_mocks(best_runs)
  original_write <- write_data
  testthat::local_mocked_bindings(
    write_data = function(x, combo, run_info, output_type, folder = NULL, suffix = NULL) {
      if (identical(combo, "B") && identical(suffix, "-model_summary")) return(invisible(NULL))
      original_write(x, combo, run_info, output_type, folder, suffix)
    },
    .package = "finnts"
  )
  tracker <- local_artifact_spies()

  expect_error(summarize_models(agent_info), "Missing required Finn artifact.*model_summary")
  expect_equal(tracker$listings, 0L)
})

test_that("global-winner series share one summary of each global model per run", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  best_runs <- dplyr::bind_rows(artifact_test_global_models(agent_info), artifact_test_selected_models(agent_info))
  local_artifact_summary_mocks(best_runs)
  calls <- c(xgboost = 0L, chronos2 = 0L)
  model_reads <- 0L
  frame <- NULL
  original_read <- read_file
  local_mocked_bindings(
    summarize_model_xgboost = function(...) {
      calls[["xgboost"]] <<- calls[["xgboost"]] + 1L
      tibble::tibble(section = "model_arg", name = "trees", value = "15")
    },
    # chronos2 permutation importance is a random draw
    summarize_model_chronos2 = function(...) {
      calls[["chronos2"]] <<- calls[["chronos2"]] + 1L
      tibble::tibble(section = "importance", name = "x1", value = format(stats::runif(1), digits = 15))
    },
    read_file = function(run_info, path = NULL, ...) {
      if (!is.null(path) && grepl(paste0(hash_data("All-Data"), "-single_models."), path, fixed = TRUE)) {
        model_reads <<- model_reads + 1L
      }
      original_read(run_info, path = path, ...)
    },
    par_start = function(...) {
      frame <<- parent.frame()
      list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
    }
  )
  tracker <- local_artifact_spies()

  summarize_models(agent_info)
  result <- get_summarized_models(agent_info)

  expect_identical(model_reads, 1L)
  expect_identical(calls, c(xgboost = 1L, chronos2 = 1L))
  global <- result[result$Model_Type == "global", ]
  expect_setequal(unique(global$Combo), c("C", "D", "E"))
  expect_identical(unique(global$Best_Model[global$Model_Name == "xgboost"]), "Yes")
  expect_identical(unique(global$Best_Model[global$Model_Name == "chronos2"]), "No")
  expect_length(unique(global$value[global$Model_Name == "chronos2"]), 1L)
  expect_setequal(result$Combo[result$Model_Type == "local"], c("A", "B"))
  expect_equal(tracker$listings, 0L)
  # the trained global models are never bound in the frame a Spark loop ships to its tasks
  expect_false(any(vapply(as.list(frame), function(value) {
    is.data.frame(value) && "Model_Fit" %in% names(value)
  }, logical(1))))
})

test_that("a global run without readable trained models still names its first series", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  best_runs <- artifact_test_global_models(agent_info, combos = c("C", "D"))
  local_artifact_summary_mocks(best_runs)
  original_read <- read_file
  local_mocked_bindings(
    settle_waits = function(path) c(0, 0),
    read_file = function(run_info, path = NULL, ...) {
      if (!is.null(path) && grepl(paste0(hash_data("All-Data"), "-single_models."), path, fixed = TRUE)) {
        stop("cannot open file", call. = FALSE)
      }
      original_read(run_info, path = path, ...)
    }
  )

  expect_error(suppressMessages(summarize_models(agent_info)), "No trained models found for combo: C.", fixed = TRUE)
})

# Write a completed global run: log, splits, each series' global forecasts,
# the All-Data trained models and the prepared model objects. Returns run info.
write_checked_global_run <- function(path) {
  run_info <- artifact_test_run(path)
  artifact_test_model_log(run_info)
  artifact_test_splits(run_info)
  for (combo in c("A", "B")) artifact_test_forecast(run_info, combo, suffix = "global_models")
  artifact_test_forecast(run_info, "A")
  for (combo in c("All-Data", "A")) {
    write_data(tibble::tibble(Model_ID = "meanf--local--R1", Model_Fit = list(list(value = 1))),
      combo, run_info, "object", "models", "-single_models")
  }
  for (suffix in c("-model_hyperparameters", "-model_workflows")) {
    write_data(tibble::tibble(value = 1), NULL, run_info, "object", "prep_models", suffix)
  }
  run_info
}

test_that("a global run's output check returns the forecast its next step would read", {
  run_info <- write_checked_global_run(withr::local_tempdir())
  without_problems <- function(x) {
    attr(x, "problems") <- NULL
    x
  }
  expected <- without_problems(get_fcst_output(run_info))
  reads <- 0L
  original <- get_forecast_data
  local_mocked_bindings(get_forecast_data = function(...) {
    reads <<- reads + 1L
    original(...)
  })

  checked <- validate_run_outputs(run_info, NULL, keep_forecast = TRUE)

  expect_identical(without_problems(checked), expected)
  expect_identical(reads, 1L)
  expect_true(validate_run_outputs(run_info, NULL))
  # local series keep their own check and hand nothing on
  expect_true(validate_run_outputs(run_info, hash_data("A"), keep_forecast = TRUE))
})

test_that("a global iteration passes its checked forecast to the metrics step without reading it again", {
  path <- withr::local_tempdir()
  fs::dir_create(fs::path(path, "logs"))
  chat <- new.env(parent = emptyenv())
  chat$set_system_prompt <- function(prompt) chat
  agent <- list(
    project_info = list(project_name = "checked_forecast", path = path, storage_object = NULL, data_output = "csv",
      object_output = "rds", combo_variables = "id", target_variable = "Target", date_type = "month"),
    run_id = "agentrun", agent_version = 2, forecast_horizon = 6, external_regressors = NULL,
    hist_end_date = as.Date("2026-06-01"), forecast_approach = "bottoms_up", negative_forecast = FALSE,
    run_global_models = TRUE, run_local_models = FALSE, llm = chat
  )
  checked <- tibble::tibble(Combo = "A", Forecast = 1)
  state <- new.env(parent = emptyenv())
  state$outputs <- 0L
  local_mocked_bindings(
    new_llm_session = function(llm) llm,
    iterate_forecast_system_prompt = function(...) "system prompt",
    resolve_agent_global_forecast_approaches = function(...) "bottoms_up",
    get_foundation_model_suffix = function() "",
    wait_before_retry = function(...) invisible(NULL),
    load_reason_history = function(...) new_reason_history("No Previous Runs"),
    reason_inputs = function(...) list(models_to_run = "xgboost", reasoning = "next"),
    submit_fcst_run = function(agent_info, inputs, combo, timestamp, ...) {
      run_info <- list(project_name = "child", run_name = paste0("run_", timestamp), path = path, data_output = "csv")
      if (state$with_checked) run_info$checked_forecast <- checked
      run_info
    },
    get_fcst_output = function(run_info) {
      state$outputs <- state$outputs + 1L
      checked
    },
    calculate_fcst_metrics = function(run_info, fcst_tbl, ...) {
      state$metric_input <- fcst_tbl
      state$metric_fields <- names(run_info)
      0.01
    },
    log_best_run = function(agent_info, run_info, ...) {
      state$logged_fields <- names(run_info)
      TRUE
    },
    finalize_run = function(...) invisible(NULL)
  )
  run_iteration <- function() {
    testthat::capture_messages(fcst_agent_workflow(agent, NULL, 0.05, NULL, FALSE, 1, max_iter = 1,
      previous_run_results = "No Previous Runs", run_log_paths = character()))
  }

  state$with_checked <- TRUE
  run_iteration()
  expect_identical(state$outputs, 0L)
  expect_identical(state$metric_input, checked)
  expect_false("checked_forecast" %in% c(state$metric_fields, state$logged_fields))

  # without a checked forecast the output step reads it, as before
  state$with_checked <- FALSE
  run_iteration()
  expect_identical(state$outputs, 1L)
  expect_identical(state$metric_input, checked)
})

test_that("required local EDA propagates CSV failures after validation", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  artifact_test_eda(agent_info)
  original_vroom <- vroom::vroom
  original_info <- file.info
  testthat::local_mocked_bindings(
    vroom = function(file, ...) {
      if (any(endsWith(as.character(file), "-acf.csv"))) {
        stop("injected EDA read failure", call. = FALSE)
      }
      original_vroom(file, ...)
    },
    .package = "vroom"
  )
  testthat::local_mocked_bindings(
    file.info = function(path, ...) {
      if (any(endsWith(as.character(path), "-acf.csv"))) return(data.frame(size = NA_real_))
      original_info(path, ...)
    },
    .package = "base"
  )

  expect_error(load_eda_results(agent_info, hash_data("A")),
    "Cannot read Finn CSV artifact.*injected EDA read failure"
  )
})

test_that("reconciliation cannot proceed after an unreadable selected CSV split", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  selected <- agent_info$project_info
  selected$project_name <- paste0(selected$project_name, "_", hash_data("A"))
  selected$run_name <- "selected"
  artifact_test_splits(selected)
  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    get_best_agent_run = function(...) tibble::tibble(
      negative_forecast = FALSE, model_type = "local", combo = "A", best_run_name = "selected"
    ),
    load_agent_forecast = function(...) stop("unreadable split was accepted", call. = FALSE),
    .package = "finnts"
  )
  testthat::local_mocked_bindings(
    vroom = function(...) stop("injected split read failure", call. = FALSE),
    .package = "vroom"
  )
  testthat::local_mocked_bindings(
    file.info = function(...) data.frame(size = NA_real_),
    .package = "base"
  )

  expect_error(reconcile_agent_forecast(agent_info, agent_info$project_info),
    "Cannot read Finn CSV artifact.*injected split read failure"
  )
})