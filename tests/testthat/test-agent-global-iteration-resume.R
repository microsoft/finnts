# Agent with built-in global models and an underscore-free run ID, as Agent
# run-log names require. Like a real Agent project, its logs folder exists.
make_global_resume_run_agent <- function(path) {
  fs::dir_create(fs::path(path, "logs"))
  list(
    project_info = list(project_name = "global_resume", path = path, storage_object = NULL, data_output = "csv",
      object_output = "rds", combo_variables = "id", target_variable = "Target", date_type = "month"),
    run_id = "agentrun", agent_version = 2, forecast_horizon = 6, external_regressors = c("x1", "x2"),
    hist_end_date = as.Date("2026-06-01"), forecast_approach = "bottoms_up", negative_forecast = FALSE,
    run_global_models = TRUE, run_local_models = FALSE, llm = structure(list(), class = "Chat")
  )
}

# Write one global run log of the Agent run, named
# agent_<run_id>_<hash of "all">_<suffix>. `trained` adds the values
# train_models() writes when training finishes; `recorded` adds the values
# log_best_run() writes when the iteration is recorded. Returns the run name.
write_global_run_log <- function(agent, suffix, created, trained = TRUE, recorded = FALSE, seed = 123,
                                 run_id = agent$run_id, settings = list()) {
  child <- agent$project_info
  child$project_name <- paste0(child$project_name, "_", hash_data("all"))
  child$run_name <- paste0("agent_", run_id, "_", hash_data("all"), "_", suffix)
  row <- utils::modifyList(list(
    project_name = child$project_name, run_name = child$run_name, created = created,
    external_regressors = "x2---x1", clean_missing_values = TRUE, clean_outliers = FALSE, stationary = TRUE,
    forecast_approach = "bottoms_up", lag_periods = "6---3---1", rolling_window_periods = NA_character_,
    recipes_to_run = "R1", multistep_horizon = FALSE, models_to_run = "xgboost---chronos2",
    seasonal_period = NA_character_, run_global_models = NA, run_local_models = NA, global_model_recipes = NA_character_,
    feature_selection = NA, seed = NA_real_, negative_forecast = NA, weighted_mape = NA_real_,
    agent_version = NA_real_, agent_forecast_approach = NA_character_
  ), settings)
  if (trained) {
    row[c("run_global_models", "run_local_models", "global_model_recipes", "feature_selection", "seed",
      "negative_forecast")] <- list(TRUE, FALSE, "R1", FALSE, seed, FALSE)
  }
  if (recorded) {
    row[c("weighted_mape", "agent_version", "agent_forecast_approach")] <- list(0.2, agent$agent_version, "bottoms_up")
  }
  write_data(as.data.frame(row, stringsAsFactors = FALSE), NULL, child, "log", "logs")
  child$run_name
}

# Read one global run log back as the workflow does.
read_global_run_log <- function(agent, run_name) {
  child <- agent$project_info
  child$project_name <- paste0(child$project_name, "_", hash_data("all"))
  child$run_name <- run_name
  read_file(child, file_list = local_artifact_path(child, "logs", extension = "csv"), return_type = "df")
}

# Offline chat whose system prompt can be set; reasoning is stubbed separately.
global_resume_chat <- function() {
  chat <- new.env(parent = emptyenv())
  chat$set_system_prompt <- function(prompt) chat
  chat
}

# Stub the iteration steps so fcst_agent_workflow() runs without training:
# record each submission's timestamp and inputs, return the given accuracy and
# count reasoning calls. A submission with `fail_timestamp` always fails.
stub_global_iteration_steps <- function(state, wmape, fail_timestamp = NULL, env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)
  testthat::local_mocked_bindings(
    new_llm_session = function(llm) llm,
    iterate_forecast_system_prompt = function(...) "system prompt",
    resolve_agent_global_forecast_approaches = function(...) "bottoms_up",
    get_foundation_model_suffix = function() "---chronos2",
    wait_before_retry = function(...) invisible(NULL),
    load_reason_history = function(...) new_reason_history("No Previous Runs"),
    reason_inputs = function(...) {
      state$reasons <- state$reasons + 1L
      list(models_to_run = "xgboost", reasoning = "next")
    },
    submit_fcst_run = function(agent_info, inputs, combo, timestamp, ...) {
      state$submits <- c(state$submits, timestamp)
      state$inputs[[length(state$submits)]] <- inputs
      if (identical(timestamp, fail_timestamp)) stop("artifact unreadable", call. = FALSE)
      list(project_name = "child", run_name = paste0("run_", timestamp), path = path, data_output = "csv")
    },
    get_fcst_output = function(...) tibble::tibble(),
    calculate_fcst_metrics = function(...) wmape,
    log_best_run = function(...) TRUE,
    finalize_run = function(...) {
      state$finalized <- TRUE
      invisible(NULL)
    },
    .package = "finnts",
    .env = env
  )
}

# Fresh recording state for stub_global_iteration_steps().
new_iteration_state <- function() {
  state <- new.env(parent = emptyenv())
  state$reasons <- 0L
  state$submits <- character()
  state$inputs <- list()
  state$finalized <- FALSE
  state
}

test_that("the latest trained, unrecorded global run of this Agent run is chosen for resuming", {
  agent <- make_global_resume_run_agent(withr::local_tempdir())
  recorded <- write_global_run_log(agent, "20260101T090000Z_0", "2026-01-01 10:00:00", recorded = TRUE)
  write_global_run_log(agent, "20260101T090000Z_1", "2026-01-01 11:00:00")
  latest <- write_global_run_log(agent, "20260102T090000Z_0", "2026-01-02 10:00:00")
  write_global_run_log(agent, "20260102T090000Z_1", "2026-01-02 12:00:00", trained = FALSE)
  write_global_run_log(agent, "20260103T090000Z_0", "2026-01-03 10:00:00", run_id = "otherrun")
  paths <- agent_run_log_paths(agent, NULL)
  history <- load_run_results(agent, NULL, run_log_paths = paths)
  reads <- character()
  original_read <- read_file
  local_mocked_bindings(read_file = function(run_info, path = NULL, file_list = NULL, ...) {
    reads <<- c(reads, as.character(file_list))
    original_read(run_info, path = path, file_list = file_list, ...)
  })

  expect_identical(find_resumable_global_run(agent, paths, history, seed = 123)$run_name, latest)
  # the recorded iteration's log, already read with the history, isn't read again
  recorded_file <- paste0(hash_data(paste0("global_resume_", hash_data("all"))), "-", hash_data(recorded), ".csv")
  expect_false(recorded_file %in% fs::path_file(reads))
  # blob and OneDrive storage have no start listing and list the run logs once
  expect_identical(find_resumable_global_run(agent, NULL, history, seed = 123)$run_name, latest)
  # training compares the seed, so a run trained with another seed is not resumed
  expect_null(find_resumable_global_run(agent, paths, history, seed = 7))
  # custom-model runs re-run the iteration on restart, as before
  custom <- agent
  custom$custom_agent_contract <- list(id = "contract")
  expect_null(find_resumable_global_run(custom, paths, history, seed = 123))

  # a run older than the latest recorded iteration was superseded
  write_global_run_log(agent, "20260104T090000Z_0", "2026-01-04 10:00:00", recorded = TRUE,
    settings = list(models_to_run = "xgboost"))
  paths <- agent_run_log_paths(agent, NULL)
  history <- load_run_results(agent, NULL, run_log_paths = paths)
  expect_null(find_resumable_global_run(agent, paths, history, seed = 123))
})

test_that("a resumed run's settings are rebuilt in their logged order and checked again", {
  agent <- make_global_resume_run_agent(withr::local_tempdir())
  agent$global_forecast_approaches <- "bottoms_up"
  local_mocked_bindings(get_foundation_model_suffix = function() "---chronos2")
  row <- read_global_run_log(agent, write_global_run_log(agent, "20260102T090000Z_0", "2026-01-02 10:00:00"))

  inputs <- resume_global_run_inputs(agent, row, "No Previous Runs")

  expect_identical(inputs$models_to_run, c("xgboost", "chronos2"))
  expect_identical(inputs$external_regressors, c("x2", "x1"))
  expect_identical(inputs$lag_periods, c(6, 3, 1))
  expect_identical(inputs[c("clean_missing_values", "clean_outliers", "stationary", "feature_selection",
    "multistep_horizon", "negative_forecast")], list(clean_missing_values = TRUE, clean_outliers = FALSE,
    stationary = TRUE, feature_selection = FALSE, multistep_horizon = FALSE, negative_forecast = FALSE))
  # each stage's settings check compares these values, joined, with the run log
  joined <- function(value) {
    value <- null_converter(value)
    if (is.null(value)) NA_character_ else paste(value, collapse = "---")
  }
  for (field in c("models_to_run", "external_regressors", "forecast_approach", "lag_periods",
    "rolling_window_periods", "recipes_to_run", "seasonal_period")) {
    expect_identical(joined(inputs[[field]]), as.character(row[[field]][[1]]), label = field)
  }

  # a model that is no longer available is not resumed
  local_mocked_bindings(get_foundation_model_suffix = function() "")
  expect_message(expect_null(resume_global_run_inputs(agent, row, "No Previous Runs")), "Not resuming")

  # nor are settings a recorded iteration already ran, in any order
  local_mocked_bindings(get_foundation_model_suffix = function() "---chronos2")
  write_global_run_log(agent, "20260101T090000Z_0", "2026-01-01 10:00:00", recorded = TRUE,
    settings = list(models_to_run = "chronos2---xgboost", external_regressors = "x1---x2"))
  history <- load_run_results(agent, NULL, run_log_paths = agent_run_log_paths(agent, NULL))
  expect_message(expect_null(resume_global_run_inputs(agent, row, history)), "Not resuming")
})

test_that("a resumed global iteration runs first under its own run name and counts toward max_iter", {
  agent <- make_global_resume_run_agent(withr::local_tempdir())
  agent$llm <- global_resume_chat()
  resumed <- write_global_run_log(agent, "20260102T090000Z_0", "2026-01-02 10:00:00")
  row <- read_global_run_log(agent, resumed)
  state <- new_iteration_state()
  stub_global_iteration_steps(state, wmape = 0.5)

  messages <- testthat::capture_messages(fcst_agent_workflow(agent, NULL, 0.05, NULL, FALSE, 1, max_iter = 3,
    previous_run_results = "No Previous Runs", run_log_paths = character(), resume_run = row))

  expect_length(state$submits, 3L)
  expect_identical(state$submits[[1]], "20260102T090000Z_0")
  expect_true(all(endsWith(state$submits[-1], c("_1", "_2"))))
  expect_identical(state$inputs[[1]]$models_to_run, c("xgboost", "chronos2"))
  expect_identical(state$reasons, 2L)
  expect_true(any(grepl("Resuming global iteration '", messages, fixed = TRUE)))
  expect_true(any(grepl("Forecast Iteration 1/3 Complete", messages, fixed = TRUE)))

  # a resumed iteration that meets the goal finishes the search without reasoning
  state <- new_iteration_state()
  stub_global_iteration_steps(state, wmape = 0.01)
  testthat::capture_messages(fcst_agent_workflow(agent, NULL, 0.05, NULL, FALSE, 1, max_iter = 3,
    previous_run_results = "No Previous Runs", run_log_paths = character(), resume_run = row))
  expect_identical(state$submits, "20260102T090000Z_0")
  expect_identical(state$reasons, 0L)
  expect_true(state$finalized)
})

test_that("a resumed global iteration that keeps failing falls back to a new iteration", {
  agent <- make_global_resume_run_agent(withr::local_tempdir())
  agent$llm <- global_resume_chat()
  row <- read_global_run_log(agent, write_global_run_log(agent, "20260102T090000Z_0", "2026-01-02 10:00:00"))
  state <- new_iteration_state()
  stub_global_iteration_steps(state, wmape = 0.5, fail_timestamp = "20260102T090000Z_0")

  messages <- testthat::capture_messages(fcst_agent_workflow(agent, NULL, 0.05, NULL, FALSE, 1, max_iter = 2,
    previous_run_results = "No Previous Runs", run_log_paths = character(), resume_run = row))

  expect_true(any(grepl(paste0("Could not resume global iteration '", row$run_name[[1]], "'"), messages, fixed = TRUE)))
  expect_true(any(grepl("artifact unreadable", messages, fixed = TRUE)))
  expect_identical(sum(state$submits == "20260102T090000Z_0"), 4L)
  expect_identical(state$reasons, 2L)
  expect_true(all(endsWith(state$submits[state$submits != "20260102T090000Z_0"], c("_0", "_1"))))
  expect_true(state$finalized)
})

test_that("iterate_forecast() hands the global workflow the run to resume", {
  agent <- make_global_resume_run_agent(withr::local_tempdir())
  resumed <- write_global_run_log(agent, "20260102T090000Z_0", "2026-01-02 10:00:00")
  state <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    get_eda_data = function(...) tibble::tibble(Combo = "All", Analysis_Type = "Hierarchy",
      Metric = "hierarchy_type", Value = "none"),
    agent_input_series = function(...) c("A", "B"),
    load_best_agent_run = function(...) tibble::tibble(),
    fcst_agent_workflow = function(agent_info, combo, ..., resume_run = NULL) {
      state$resume_run <- resume_run
      list()
    },
    save_best_agent_run = function(...) invisible(NULL),
    save_agent_forecast = function(...) invisible(NULL),
    summarize_models = function(...) invisible(NULL),
    .package = "finnts"
  )

  iterate_forecast(agent, max_iter = 3, weighted_mape_goal = 0.05, seed = 123)
  expect_identical(state$resume_run$run_name, resumed)

  iterate_forecast(agent, max_iter = 3, weighted_mape_goal = 0.05, seed = 7)
  expect_null(state$resume_run)
})

test_that("a global iteration cut off after training resumes without training again", {
  skip_on_cran()
  data <- data.frame(Date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 24), 2),
    id = rep(c("east", "west"), each = 24), value = rep(c(100, 200), each = 24) + rep(seq_len(24), 2))
  tracker <- new.env(parent = emptyenv())
  path <- withr::local_tempdir()
  info <- set_agent_info(
    project_info = set_project_info(project_name = "global_cutoff", path = path, combo_variables = "id",
      target_variable = "value", date_type = "month"),
    llm = custom_agent_chat(custom_agent_response(models = "xgboost"), tracker), input_data = data,
    forecast_horizon = 1, run_global_models = TRUE, run_local_models = FALSE, back_test_scenarios = 1
  )
  testthat::local_mocked_bindings(
    get_eda_data = function(...) tibble::tibble(Combo = "All", Analysis_Type = "Hierarchy",
      Metric = "hierarchy_type", Value = "none"),
    load_eda_results = function(...) "Offline EDA",
    get_foundation_model_suffix = function() "",
    wait_before_retry = function(...) invisible(NULL),
    summarize_models = function(...) invisible(NULL),
    .package = "finnts"
  )
  child_hash <- hash_data(paste0("global_cutoff_", hash_data("all")))
  global_logs <- function() fs::dir_ls(fs::path(path, "logs"), glob = paste0("*", child_hash, "-*.csv"))

  # the first attempt is cut off after training and selection, before recording
  local({
    testthat::local_mocked_bindings(log_best_run = function(...) stop("cut off", call. = FALSE), .package = "finnts")
    expect_error(iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.05, num_cores = 1), "log_best_run")
  })
  expect_length(global_logs(), 1L)
  cut_off <- utils::read.csv(global_logs()[[1]], stringsAsFactors = FALSE)
  expect_false("agent_version" %in% names(cut_off))
  models_file <- fs::path(path, "models", paste0(child_hash, "-", hash_data(cut_off$run_name), "-",
    hash_data("All-Data"), "-single_models.rds"))
  trained_at <- fs::file_info(models_file)$modification_time

  messages <- testthat::capture_messages(iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.05, num_cores = 1))

  expect_true(any(grepl("Individual Models Already Trained", messages, fixed = TRUE)))
  expect_identical(fs::file_info(models_file)$modification_time, trained_at)
  expect_identical(tracker$calls, 1L)
  expect_length(global_logs(), 1L)
  recovered <- utils::read.csv(global_logs()[[1]], stringsAsFactors = FALSE)
  expect_identical(recovered$run_name, cut_off$run_name)
  expect_equal(recovered$agent_version, info$agent_version)
  best <- get_best_agent_run(info)
  expect_setequal(best$combo, c("east", "west"))
  expect_identical(unique(best$best_run_name), cut_off$run_name)
})
