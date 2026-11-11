# Build valid single-series Agent setup arguments in a caller-owned temporary
# project. The Chat stand-in is never called; setup writes only inputs and logs.
ledger_setup_args <- function(path, data_output = "csv") {
  project <- set_project_info(
    project_name = "ledger_tests", path = path, combo_variables = "Site",
    target_variable = "Revenue", date_type = "month", data_output = data_output,
    overwrite = TRUE
  )
  list(project_info = project, llm = structure(list(), class = "Chat"),
    input_data = data.frame(Site = "Seattle",
      Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 12),
      Revenue = seq_len(12)),
    forecast_horizon = 2, run_global_models = FALSE, run_local_models = TRUE)
}

# Give each new version a distinct, increasing creation time so run IDs never
# depend on how quickly the test runs. The binding is restored with the caller.
local_ledger_clock <- function(.local_envir = parent.frame()) {
  time <- as.POSIXct("2031-01-01", tz = "UTC")
  testthat::local_mocked_bindings(get_timestamp = function() {
    time <<- time + 1
    time
  }, .env = .local_envir)
}

# Minimal project storage metadata for writing run logs directly. Run logs are
# always CSV even when data_output is parquet.
ledger_project <- function(data_output = "csv") {
  list(project_name = "ledger_direct", path = withr::local_tempdir(.local_envir = parent.frame()),
    storage_object = NULL, data_output = data_output, object_output = "rds")
}

# Write one run log row through the production log writer for run_id.
write_ledger_row <- function(info, row) {
  info$run_name <- row$run_id
  write_data(row, NULL, info, "log", "logs", "-agent_run")
}

test_that("parquet projects find their CSV run logs", {
  info <- ledger_project("parquet")
  write_ledger_row(info, data.frame(agent_version = 1, run_id = "4282d17137126405",
    project_name = info$project_name, forecast_approach = "bottoms_up"))
  runs <- load_agent_runs(info)
  expect_identical(runs$run_id, "4282d17137126405")
  expect_equal(runs$agent_version, 1)
})

test_that("run logs are never read from parquet-named files", {
  skip_if_not_installed("arrow")
  info <- ledger_project("parquet")
  write_ledger_row(info, data.frame(agent_version = 1, run_id = "4282d17137126405",
    project_name = info$project_name, forecast_approach = "bottoms_up"))
  decoy <- fs::path(info$path, "logs", paste0(hash_data(info$project_name), "-",
    hash_data("decoy"), "-agent_run.parquet"))
  arrow::write_parquet(data.frame(agent_version = 9, run_id = "decoy"), decoy)
  runs <- load_agent_runs(info)
  expect_identical(runs$run_id, "4282d17137126405")
})

test_that("run log identifiers are read as exact text", {
  info <- ledger_project()
  # Every value looks numeric, so type guessing alone would parse the columns
  # as doubles and lose identifiers beyond double precision.
  run_ids <- c("6190327244019965", "9007199254740993", "1e10")
  request_ids <- c("20261110211115", "20261110211115123", "20261231235959")
  for (index in seq_along(run_ids)) {
    write_ledger_row(info, data.frame(agent_version = index, run_id = run_ids[[index]],
      project_name = info$project_name, forecast_approach = "bottoms_up",
      request_id = request_ids[[index]]))
  }
  runs <- load_agent_runs(info)
  runs <- runs[order(runs$agent_version), , drop = FALSE]
  expect_identical(runs$run_id, run_ids)
  expect_identical(runs$request_id, request_ids)
})

test_that("request_id stamps only the new version's own run log", {
  path <- withr::local_tempdir()
  local_ledger_clock()
  reference <- ledger_setup_args(withr::local_tempdir())
  reference_agent <- do.call(set_agent_info, reference)
  reference_info <- reference$project_info
  reference_info$run_name <- reference_agent$run_id
  reference_columns <- names(utils::read.csv(local_artifact_path(reference_info, "logs",
    "-agent_run", extension = "csv"), colClasses = "character"))

  args <- ledger_setup_args(path)
  legacy <- data.frame(agent_version = 1, run_id = "legacy-run",
    project_name = args$project_info$project_name, created = "2030-01-01 00:00:00",
    forecast_approach = "bottoms_up", custom_agent_update_id = "custom-update-legacy")
  write_ledger_row(args$project_info, legacy)
  log_files <- list.files(fs::path(path, "logs"), full.names = TRUE)
  before <- tools::md5sum(log_files)

  args$request_id <- "20261110211115"
  args$agent_action <- "iterate_forecast"
  args$overwrite <- TRUE
  agent <- do.call(set_agent_info_custom, args)
  info <- args$project_info
  info$run_name <- agent$run_id
  own <- utils::read.csv(local_artifact_path(info, "logs", "-agent_run", extension = "csv"),
    colClasses = "character")
  expect_identical(names(own), c(reference_columns, "request_id"))
  expect_identical(own$run_id, agent$run_id)
  expect_identical(own$request_id, "20261110211115")
  expect_identical(tools::md5sum(log_files), before)
})

test_that("numeric-looking run IDs keep their run log row and resume by request", {
  args <- ledger_setup_args(withr::local_tempdir())
  # This creation time hashes to a digits-only run ID beyond double precision,
  # so numeric parsing cannot reproduce it and the row would not be matched.
  stamp <- as.POSIXct("2030-06-01 00:00:00", tz = "UTC") + 6288
  expect_identical(hash_data(stamp), "9763547836502955")
  testthat::local_mocked_bindings(get_timestamp = function() stamp)
  args$request_id <- "20261110211115123"
  args$agent_action <- "iterate_forecast"
  first <- do.call(set_agent_info_custom, args)
  runs <- load_agent_runs(args$project_info)
  expect_identical(runs$run_id, hash_data(stamp))
  expect_identical(runs$request_id, "20261110211115123")
  testthat::local_mocked_bindings(write_data = function(...) stop("Unexpected retry write"))
  retry <- do.call(set_agent_info_custom, args)
  expect_identical(retry$run_id, first$run_id)
  expect_identical(retry$agent_version, first$agent_version)
})

test_that("parquet projects resume, number versions, and stamp CSV run logs", {
  skip_if_not_installed("arrow")
  path <- withr::local_tempdir()
  local_ledger_clock()
  args <- ledger_setup_args(path, data_output = "parquet")
  args$request_id <- "20261110211115"
  args$agent_action <- "iterate_forecast"
  first <- do.call(set_agent_info_custom, args)
  expect_equal(first$agent_version, 1)
  expect_identical(load_agent_runs(args$project_info)$request_id, "20261110211115")
  run_logs <- list.files(fs::path(path, "logs"), pattern = "agent_run")
  expect_true(length(run_logs) == 1L && all(grepl("\\.csv$", run_logs)))
  expect_identical(do.call(set_agent_info_custom, args)$run_id, first$run_id)

  public <- args[setdiff(names(args), c("request_id", "agent_action"))]
  public$overwrite <- FALSE
  expect_identical(do.call(set_agent_info, public)$run_id, first$run_id)
  public$overwrite <- TRUE
  second <- do.call(set_agent_info, public)
  expect_equal(second$agent_version, 2)
  expect_false(identical(second$run_id, first$run_id))
})
