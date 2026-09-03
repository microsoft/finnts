# tests/testthat/test-finalize_run.R

# helper to build a minimal agent_info with a temp directory
make_finalize_agent_info <- function(project_name = "test_proj",
                                     run_id = "run_1",
                                     max_iter = 5) {
  tmp <- file.path(tempdir(), paste0("finalize_", project_name, "_", run_id))
  logs_dir <- file.path(tmp, "logs")
  fs::dir_create(logs_dir)

  project_info <- list(
    project_name = project_name,
    path = tmp,
    data_output = "csv",
    object_output = "rds",
    storage_object = NULL
  )

  list(
    project_info = project_info,
    run_id = run_id,
    max_iter = max_iter
  )
}

# helper to write a best_run csv file to the expected path
write_best_run_file <- function(agent_info, combo_name, model_type = "local",
                                run_complete = FALSE, max_iterations = 2) {
  project_info <- agent_info$project_info
  combo_hash <- hash_data(combo_name)

  best_run_tbl <- tibble::tibble(
    combo = combo_name,
    model_type = model_type,
    weighted_mape = 0.15,
    run_complete = run_complete,
    max_iterations = max_iterations,
    best_run_name = agent_info$run_id,
    models_to_run = "xgboost",
    recipes_to_run = "R1"
  )

  best_run_file <- file.path(
    project_info$path, "logs",
    paste0(
      hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "-",
      combo_hash, "-agent_best_run.csv"
    )
  )

  utils::write.csv(best_run_tbl, best_run_file, row.names = FALSE)

  invisible(best_run_file)
}

# helper to read back a best_run csv file
read_best_run_file <- function(agent_info, combo_name) {
  project_info <- agent_info$project_info
  combo_hash <- hash_data(combo_name)

  best_run_file <- file.path(
    project_info$path, "logs",
    paste0(
      hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "-",
      combo_hash, "-agent_best_run.csv"
    )
  )

  read.csv(best_run_file, stringsAsFactors = FALSE)
}

# * Local combo tests ----

test_that("finalize_run updates local combo best_run file", {
  agent_info <- make_finalize_agent_info()
  write_best_run_file(agent_info, "M750", model_type = "local")

  combo_hash <- hash_data("M750")
  result <- finalize_run(agent_info = agent_info, combo = combo_hash)

  expect_equal(result, "Run finalized successfully.")

  updated <- read_best_run_file(agent_info, "M750")
  expect_true(updated$run_complete)
  expect_equal(updated$max_iterations, 5)
})

test_that("finalize_run errors when local combo best_run file is missing", {
  agent_info <- make_finalize_agent_info(project_name = "missing_proj")

  expect_error(
    finalize_run(agent_info = agent_info, combo = "nonexistent_hash"),
    "No best run file found"
  )
})

# * Global combo tests ----

test_that("finalize_run updates global combo best_run files", {
  agent_info <- make_finalize_agent_info(project_name = "global_proj")
  write_best_run_file(agent_info, "M750", model_type = "global")
  write_best_run_file(agent_info, "M1", model_type = "global")

  result <- finalize_run(agent_info = agent_info, combo = NULL)

  expect_equal(result, "Run finalized successfully.")

  for (combo_name in c("M750", "M1")) {
    updated <- read_best_run_file(agent_info, combo_name)
    expect_true(updated$run_complete)
    expect_equal(updated$max_iterations, 5)
  }
})

test_that("finalize_run returns successfully when no global combos exist", {
  agent_info <- make_finalize_agent_info(project_name = "local_only_proj")
  write_best_run_file(agent_info, "M750", model_type = "local")

  result <- finalize_run(agent_info = agent_info, combo = NULL)

  expect_equal(result, "Run finalized successfully.")

  # verify the local file was NOT modified
  unchanged <- read_best_run_file(agent_info, "M750")
  expect_false(unchanged$run_complete)
  expect_equal(unchanged$max_iterations, 2)
})

test_that("finalize_run errors when no best_run files exist for global path", {
  agent_info <- make_finalize_agent_info(project_name = "empty_proj")

  expect_error(
    finalize_run(agent_info = agent_info, combo = NULL),
    "No best run files found"
  )
})

test_that("reasoning exhaustion preserves an existing local best run", {
  agent_info <- make_finalize_agent_info(project_name = "local_abort")
  original_path <- write_best_run_file(agent_info, "M750", model_type = "local")
  original <- read.csv(original_path, stringsAsFactors = FALSE)
  list_calls <- 0L

  testthat::local_mocked_bindings(
    list_files = function(...) {
      list_calls <<- list_calls + 1L
      stop("local finalization must use an exact file read")
    },
    .package = "finnts"
  )

  result <- finalize_run(
    agent_info = agent_info,
    combo = hash_data("M750"),
    completion_reason = "reasoning_exhausted",
    abort_reason = "invalid proposal",
    fallback_available = FALSE
  )

  updated <- read_best_run_file(agent_info, "M750")
  expect_equal(result, "Run finalized successfully.")
  expect_identical(list_calls, 0L)
  expect_equal(updated$weighted_mape, original$weighted_mape)
  expect_equal(updated$best_run_name, original$best_run_name)
  expect_true(updated$run_complete)
})

test_that("local reasoning exhaustion can preserve a global best for its combo", {
  agent_info <- make_finalize_agent_info(project_name = "local_uses_global")
  write_best_run_file(agent_info, "M750", model_type = "global")

  finalize_run(
    agent_info = agent_info,
    combo = hash_data("M750"),
    completion_reason = "reasoning_exhausted",
    abort_reason = "invalid local proposal",
    fallback_available = FALSE
  )

  updated <- read_best_run_file(agent_info, "M750")
  expect_identical(updated$model_type, "global")
  expect_true(updated$run_complete)
})

test_that("reasoning exhaustion remains hard without a local best run", {
  agent_info <- make_finalize_agent_info(project_name = "missing_local_abort")

  expect_error(
    finalize_run(
      agent_info = agent_info,
      combo = hash_data("M750"),
      completion_reason = "reasoning_exhausted",
      abort_reason = "invalid proposal",
      fallback_available = FALSE
    ),
    "invalid proposal.*No best run file found"
  )
})

test_that("global reasoning exhaustion skips only when local fallback exists", {
  agent_info <- make_finalize_agent_info(project_name = "global_fallback")

  skipped <- finalize_run(
    agent_info = agent_info,
    combo = NULL,
    completion_reason = "reasoning_exhausted",
    abort_reason = "invalid global proposal",
    fallback_available = TRUE
  )

  expect_identical(skipped$status, "skipped")
  expect_true(skipped$continue_to_local)
  expect_match(skipped$reason, "invalid global proposal", fixed = TRUE)

  expect_error(
    finalize_run(
      agent_info = agent_info,
      combo = NULL,
      completion_reason = "reasoning_exhausted",
      abort_reason = "invalid global proposal",
      fallback_available = FALSE
    ),
    "invalid global proposal.*No best run files found"
  )
})

test_that("global reasoning exhaustion handles a nonempty local-only best table", {
  agent_info <- make_finalize_agent_info(project_name = "global_local_only")
  write_best_run_file(agent_info, "M750", model_type = "local")

  skipped <- finalize_run(
    agent_info = agent_info,
    combo = NULL,
    completion_reason = "reasoning_exhausted",
    abort_reason = "invalid global proposal",
    fallback_available = TRUE
  )
  expect_identical(skipped$status, "skipped")
  expect_true(skipped$continue_to_local)

  expect_error(
    finalize_run(
      agent_info = agent_info,
      combo = NULL,
      completion_reason = "reasoning_exhausted",
      abort_reason = "invalid global proposal",
      fallback_available = FALSE
    ),
    "invalid global proposal.*No global best run files found"
  )
})

test_that("global storage failures remain hard during exhausted reasoning", {
  agent_info <- make_finalize_agent_info(project_name = "global_storage_failure")

  testthat::local_mocked_bindings(
    list_files = function(storage_object, path, fail_on_error = FALSE) {
      expect_true(fail_on_error)
      stop("ADLS unavailable")
    },
    .package = "finnts"
  )

  expect_error(
    finalize_run(
      agent_info = agent_info,
      combo = NULL,
      completion_reason = "reasoning_exhausted",
      abort_reason = "invalid global proposal",
      fallback_available = TRUE
    ),
    "ADLS unavailable"
  )
})

test_that("strict blob listing propagates storage provider failures", {
  blob <- structure(list(), class = "blob_container")

  testthat::local_mocked_bindings(
    list_storage_files = function(...) stop("ADLS unavailable"),
    .package = "AzureStor"
  )

  expect_error(
    list_files(blob, "/logs/*.csv", fail_on_error = TRUE),
    "ADLS unavailable"
  )
  expect_null(list_files(blob, "/logs/*.csv"))
})

test_that("global best-run write verification uses strict listing", {
  agent_info <- make_finalize_agent_info(project_name = "strict_best_run_write")
  agent_info$agent_version <- 2
  agent_info$forecast_approach <- "bottoms_up"
  run_info <- agent_info$project_info
  run_info$run_name <- "agent-run"

  testthat::local_mocked_bindings(
    get_run_info = function(...) tibble::tibble(
      project_name = run_info$project_name,
      run_name = run_info$run_name,
      path = run_info$path,
      data_output = run_info$data_output,
      object_output = run_info$object_output,
      weighted_mape = 0.2
    ),
    load_combo_forecast = function(...) tibble::tibble(
      Combo = c("A", "B"),
      Best_Model = "Yes",
      Run_Type = "Back_Test",
      Target = 100,
      Forecast = 110
    ),
    validate_best_model = function(...) invisible(TRUE),
    read_file = function(...) tibble::tibble(),
    write_data = function(...) invisible(NULL),
    list_files = function(storage_object, path, fail_on_error = FALSE) {
      expect_true(fail_on_error)
      stop("ADLS unavailable")
    },
    .package = "finnts"
  )

  expect_error(
    log_best_run(
      agent_info = agent_info,
      run_info = run_info,
      weighted_mape = 0.2,
      combo = NULL,
      check_best_run = FALSE
    ),
    "ADLS unavailable"
  )
})

test_that("global reasoning finalization performs at most one wildcard listing", {
  agent_info <- make_finalize_agent_info(project_name = "global_listing")
  files <- c(
    write_best_run_file(agent_info, "M750", model_type = "global"),
    write_best_run_file(agent_info, "M1", model_type = "global")
  )
  list_calls <- 0L

  testthat::local_mocked_bindings(
    list_files = function(...) {
      list_calls <<- list_calls + 1L
      files
    },
    .package = "finnts"
  )

  finalize_run(
    agent_info = agent_info,
    combo = NULL,
    completion_reason = "reasoning_exhausted",
    abort_reason = "invalid global proposal",
    fallback_available = TRUE
  )

  expect_identical(list_calls, 1L)
})
