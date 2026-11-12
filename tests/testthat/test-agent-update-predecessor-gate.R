# Build valid single-series Agent setup arguments in a caller-owned temporary
# project. The Chat stand-in is never called; setup writes only inputs and logs.
gate_setup_args <- function(path) {
  project <- set_project_info(
    project_name = "gate_tests", path = path, combo_variables = "Site",
    target_variable = "Revenue", date_type = "month", overwrite = TRUE
  )
  list(project_info = project, llm = structure(list(), class = "Chat"),
    input_data = data.frame(Site = "Seattle",
      Date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 12),
      Revenue = seq_len(12)),
    forecast_horizon = 2, run_global_models = FALSE, run_local_models = TRUE)
}

# Give each new version a distinct, increasing creation time so run IDs never
# depend on how quickly the test runs. The binding is restored with the caller.
local_gate_clock <- function(.local_envir = parent.frame()) {
  time <- as.POSIXct("2032-01-01", tz = "UTC")
  testthat::local_mocked_bindings(get_timestamp = function() {
    time <<- time + 1
    time
  }, .env = .local_envir)
}

# Checksum every file under a project so a test can prove setup wrote nothing.
project_file_hashes <- function(path) {
  tools::md5sum(sort(list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE)))
}

# Run Finn API setup for one request and action against the shared arguments.
gate_request <- function(args, request_id, action, overwrite = TRUE) {
  args$request_id <- request_id
  args$agent_action <- action
  args$overwrite <- overwrite
  do.call(set_agent_info_custom, args)
}

# Write nonempty, valid final outputs so a version counts as completed.
write_completed_outputs <- function(args, agent) {
  info <- args$project_info
  info$run_name <- agent$run_id
  outputs <- list(
    "-run_metadata" = data.frame(combo = "Seattle", agent_run_id = agent$run_id,
      best_run_name = "agent_Seattle", model_type = "local", weighted_mape = 0.1),
    "-forecast" = data.frame(Combo = "Seattle", Model_ID = "arima", Best_Model = "Yes"),
    "-model_summary" = data.frame(Combo = "Seattle", Model_ID = "arima", Best_Model = "Yes"),
    "-eda" = data.frame(Combo = "All", Metric = "Number_Series", Value = "1")
  )
  for (suffix in names(outputs)) {
    write_data(outputs[[suffix]], NULL, info, "data", "final_output", suffix)
  }
}

# Return a condition message, or an empty string when the call succeeded.
error_message <- function(expr) {
  result <- tryCatch(expr, error = identity)
  if (inherits(result, "error")) conditionMessage(result) else ""
}

test_that("updates with no Agent history fail before writing", {
  path <- withr::local_tempdir()
  args <- gate_setup_args(path)
  before <- project_file_hashes(path)
  message <- error_message(gate_request(args, "update-1", "update_forecast"))
  project_path <- args$project_info$path
  expect_match(message, "No previous agent runs found", fixed = TRUE)
  expect_match(message, "Project 'gate_tests'", fixed = TRUE)
  expect_match(message, paste0(project_path, "/logs"), fixed = TRUE)
  expect_match(message, "-*agent_run.csv", fixed = TRUE)
  expect_match(message, "Found 0 previous Agent versions; 0 completed", fixed = TRUE)
  expect_match(message, paste0("restore the Agent history under '", project_path,
    "' or start a new history with iterate_forecast()"), fixed = TRUE)
  expect_identical(project_file_hashes(path), before)
})

test_that("updates with only incomplete versions fail before writing", {
  path <- withr::local_tempdir()
  args <- gate_setup_args(path)
  local_gate_clock()
  gate_request(args, "iterate-1", "iterate_forecast")
  gate_request(args, "iterate-2", "iterate_forecast")
  before <- project_file_hashes(path)
  message <- error_message(gate_request(args, "update-1", "update_forecast"))
  expect_match(message, "No completed previous agent run found", fixed = TRUE)
  expect_match(message, "Found 2 previous Agent versions; 0 completed", fixed = TRUE)
  expect_match(message, "v2 lacks run_metadata, forecast, model_summary, eda", fixed = TRUE)
  expect_match(message, "v1 lacks run_metadata, forecast, model_summary, eda", fixed = TRUE)
  expect_match(message, "start a new history with iterate_forecast()", fixed = TRUE)
  expect_identical(project_file_hashes(path), before)
})

for (format in c("csv", "parquet")) {
  test_that(paste("empty required", format, "outputs cannot authorize a new update"), {
    if (format == "parquet") skip_if_not_installed("arrow")
    for (suffix in c("run_metadata", "forecast", "model_summary", "eda")) {
      path <- withr::local_tempdir()
      args <- gate_setup_args(path)
      args$project_info$data_output <- format
      local_gate_clock()
      first <- gate_request(args, "iterate-1", "iterate_forecast")
      write_completed_outputs(args, first)
      info <- args$project_info
      info$run_name <- first$run_id
      write_data(data.frame(Combo = character()), NULL, info, "data",
        "final_output", paste0("-", suffix))
      before <- project_file_hashes(path)

      status <- agent_predecessor_status(first)
      expect_false(status$complete, info = suffix)
      expect_match(paste(status$problems, collapse = "; "), paste("lacks", suffix), fixed = TRUE)
      expect_null(load_completed_agent_run_outputs(first))
      message <- error_message(gate_request(args, "update-1", "update_forecast"))
      expect_match(message, "No completed previous agent run found", fixed = TRUE)
      expect_match(message, "Found 1 previous Agent version; 0 completed", fixed = TRUE)
      expect_match(message, paste("v1 lacks", suffix), fixed = TRUE)
      expect_identical(project_file_hashes(path), before)
    }
  })
}

test_that("updates use the newest completed version and read each required output once", {
  path <- withr::local_tempdir()
  args <- gate_setup_args(path)
  local_gate_clock()
  first <- gate_request(args, "iterate-1", "iterate_forecast")
  write_completed_outputs(args, first)
  gate_request(args, "iterate-2", "iterate_forecast")
  original_reader <- read_file
  final_reads <- character()
  testthat::local_mocked_bindings(read_file = function(run_info, path = NULL, file_list = NULL, ...) {
    targets <- as.character(c(path, file_list))
    final_reads <<- c(final_reads, targets[grepl("final_output", targets, fixed = TRUE)])
    original_reader(run_info, path = path, file_list = file_list, ...)
  })
  expect_message(
    agent <- gate_request(args, "update-1", "update_forecast"),
    "Found 2 Agent versions; latest completed: v1"
  )
  expect_equal(agent$agent_version, 3)
  expect_length(final_reads, 4L)
  expect_true(all(table(final_reads) == 1L))
  expect_true(any(grepl(paste0(hash_data(first$run_id), "-forecast\\."), final_reads)))
  expect_true(any(grepl(paste0(hash_data(first$run_id), "-run_metadata\\."), final_reads)))
  previous <- load_agent_runs(args$project_info)
  previous <- previous[previous$agent_version < agent$agent_version, , drop = FALSE]
  completed <- find_completed_previous_agent_runs(agent, previous)
  expect_identical(completed[[1]]$agent_info$run_id, first$run_id)
})

for (provider in c("blob_container", "ms_drive")) {
  test_that(paste("predecessor checks read", provider, "outputs by exact path without listings"), {
    path <- withr::local_tempdir()
    args <- gate_setup_args(path)
    local_gate_clock()
    first <- gate_request(args, "iterate-1", "iterate_forecast")
    write_completed_outputs(args, first)
    source <- args$project_info
    source$run_name <- first$run_id
    transport <- local_artifact_provider(provider)
    remote <- first
    remote$project_info$path <- "provider-artifacts"
    remote$project_info$storage_object <- transport$storage_object
    target <- remote$project_info
    target$run_name <- first$run_id
    suffixes <- c("run_metadata", "forecast", "model_summary", "eda")
    remote_paths <- vapply(suffixes, function(suffix) {
      as.character(artifact_test_path(target, "final_output", suffix = paste0("-", suffix)))
    }, character(1))
    for (suffix in suffixes) {
      transport$files[[remote_paths[[suffix]]]] <-
        artifact_test_path(source, "final_output", suffix = paste0("-", suffix))
    }

    status <- agent_predecessor_status(remote, include_outputs = TRUE)
    expect_true(status$complete)
    expect_identical(status$outputs$run_metadata$agent_run_id, first$run_id)
    expect_setequal(transport$downloads, unname(remote_paths))

    transport$files[[remote_paths[["forecast"]]]] <- NULL
    expect_identical(agent_predecessor_status(remote)$problems, "lacks forecast")

    transport$lookup_error <- artifact_http_error(403L, "provider failure")
    expect_error(agent_predecessor_status(remote), class = "http_403")
  })
}

test_that("an empty hierarchy summary cannot authorize a hierarchical predecessor", {
  path <- withr::local_tempdir()
  args <- gate_setup_args(path)
  local_gate_clock()
  first <- gate_request(args, "iterate-1", "iterate_forecast")
  write_completed_outputs(args, first)
  info <- args$project_info
  info$run_name <- first$run_id
  log <- read_exact_artifact(info,
    local_artifact_path(info, "logs", "-agent_run", extension = "csv"),
    character_columns = c("run_id", "request_id"))
  log$forecast_approach <- "standard_hierarchy"
  write_data(log, NULL, info, "log", "logs", "-agent_run")
  write_data(data.frame(Hierarchy_Combo = character()), NULL, info, "data",
    "final_output", "-hierarchy_summary")
  first$forecast_approach <- "standard_hierarchy"
  before <- project_file_hashes(path)

  expect_false(agent_predecessor_status(first)$complete)
  expect_null(load_completed_agent_run_outputs(first))
  message <- error_message(gate_request(args, "update-1", "update_forecast"))
  expect_match(message, "v1 lacks hierarchy_summary", fixed = TRUE)
  expect_match(message, "Found 1 previous Agent version; 0 completed", fixed = TRUE)
  expect_identical(project_file_hashes(path), before)
})

test_that("same-request retries resume an interrupted update without the gate", {
  path <- withr::local_tempdir()
  args <- gate_setup_args(path)
  local_gate_clock()
  first <- gate_request(args, "iterate-1", "iterate_forecast")
  write_completed_outputs(args, first)
  update <- gate_request(args, "update-1", "update_forecast")
  testthat::local_mocked_bindings(
    require_completed_agent_predecessor = function(...) stop("Unexpected predecessor gate"),
    write_data = function(...) stop("Unexpected retry write")
  )
  retry <- gate_request(args, "update-1", "update_forecast")
  expect_identical(retry$run_id, update$run_id)
  expect_identical(retry$agent_version, update$agent_version)
  expect_true(retry$overwrite)
})

test_that("iterate requests still create the first version without history", {
  args <- gate_setup_args(withr::local_tempdir())
  agent <- gate_request(args, "iterate-1", "iterate_forecast", overwrite = FALSE)
  expect_equal(agent$agent_version, 1)
  expect_identical(load_agent_runs(args$project_info)$request_id, "iterate-1")
})

test_that("custom updates without a completed predecessor explain the next step", {
  args <- custom_agent_inputs(withr::local_tempdir())
  local_gate_clock()
  do.call(set_agent_info, args)
  args$overwrite <- TRUE
  current <- do.call(set_agent_info, args)
  message <- error_message(prepare_agent_custom_update(current, 123))
  expect_match(message, "Custom update requires a completed predecessor.", fixed = TRUE)
  expect_match(message, "No completed previous agent run found", fixed = TRUE)
  expect_match(message, "v1 lacks run_metadata", fixed = TRUE)
  expect_match(message, "start a new history with iterate_forecast()", fixed = TRUE)
})
