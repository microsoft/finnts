# Coordinating (driver) code must not look up a per-series output by exact name
# before parallel workers write it. FUSE mounts such as Synapse /synfs can cache
# that "not found" answer and hide the file for the rest of the session.

# Record exact-path lookups made inside custom_run_audit() for paths that do not
# exist at lookup time. Returns an environment whose `missing` field accumulates
# those paths. Worker reads and other workflow lookups are deliberately ignored.
local_audit_missing_lookups <- function(.env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$active <- FALSE
  state$missing <- character()
  audit <- custom_run_audit
  file_info <- fs::file_info
  testthat::local_mocked_bindings(custom_run_audit = function(...) {
    state$active <- TRUE
    on.exit(state$active <- FALSE)
    audit(...)
  }, .package = "finnts", .env = .env)
  testthat::local_mocked_bindings(file_info = function(path, ...) {
    if (state$active) {
      paths <- as.character(path)
      state$missing <- c(state$missing, paths[!file.exists(paths)])
    }
    file_info(path, ...)
  }, .package = "fs", .env = .env)
  state
}

# Prepare custom-model workflows in a fresh standard run rooted at `path`.
# Returns the run info; writes only the normal prep_models artifacts.
prepare_custom_lookup_run <- function(path, model) {
  info <- standard_run(path)
  prep_models(info,
    models_to_run = c("business_mean", "meanf"), custom_models = list(business_mean = model),
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1
  )
  info
}

# Copy every artifact folder of `template` into the empty root `path` and return
# the template's run info re-rooted there. The template itself is never changed.
copy_lookup_run <- function(template, path) {
  for (folder in c("logs", "prep_data", "prep_models", "models", "forecasts")) {
    fs::dir_create(fs::path(path, folder))
    files <- fs::dir_ls(fs::path(template$path, folder), type = "file")
    if (length(files)) fs::file_copy(files, fs::path(path, folder, fs::path_file(files)))
  }
  info <- template
  info$path <- path
  info
}

# Build, once per test session, one prepared custom run and one copy of it with
# local models trained, so tests copy files instead of repeating prep_models()
# and train_models(). Returns list(prepared, trained, model); the cached files
# must stay unchanged, which every call verifies by hash.
lookup_templates <- local({
  cache <- new.env(parent = emptyenv())
  function() {
    if (is.null(cache$prepared)) {
      model <- standard_custom_model()
      prepared <- prepare_custom_lookup_run(withr::local_tempdir(.local_envir = cache), model)
      trained <- copy_lookup_run(prepared, withr::local_tempdir(.local_envir = cache))
      train_models(trained, run_global_models = FALSE, run_local_models = TRUE)
      cache$model <- model
      cache$prepared <- prepared
      cache$trained <- trained
      cache$hashes <- tools::md5sum(c(
        list.files(prepared$path, recursive = TRUE, full.names = TRUE),
        list.files(trained$path, recursive = TRUE, full.names = TRUE)
      ))
    }
    expect_identical(tools::md5sum(names(cache$hashes)), cache$hashes)
    list(prepared = cache$prepared, trained = cache$trained, model = cache$model)
  }
})

# Normalize artifact paths to the forward-slash form recorded by the I/O spies.
spy_path <- function(path) gsub("\\", "/", as.character(path), fixed = TRUE)

test_that("custom runs never probe per-series outputs before workers write them", {
  info <- copy_lookup_run(lookup_templates()$prepared, withr::local_tempdir())
  lookups <- local_audit_missing_lookups()

  train_models(info, run_global_models = TRUE, run_local_models = TRUE, negative_forecast = TRUE)
  ensemble_models(info)
  final_models(info)

  expect_identical(lookups$missing, character())
  combos <- vapply(c("A", "B"), hash_data, character(1))
  expect_true(all(file.exists(local_artifact_path(info, "forecasts", "-average_models", combos))))
  forecast <- get_forecast_data(info)
  expect_setequal(forecast$Combo[forecast$Best_Model == "Yes"], c("A", "B"))
  expect_true("business_mean" %in% forecast$Model_Name)
})

test_that("custom restarts audit and reuse only completed series outputs", {
  templates <- lookup_templates()
  full <- copy_lookup_run(templates$trained, withr::local_tempdir())
  partial <- copy_lookup_run(templates$prepared, withr::local_tempdir())
  combos <- vapply(c("A", "B"), hash_data, character(1))
  done <- c(
    local_artifact_path(full, "forecasts", "-single_models", combos[[1]]),
    local_artifact_path(full, "models", "-single_models", combos[[1]], "rds")
  )
  copied <- fs::path(partial$path, fs::path_rel(done, full$path))
  fs::file_copy(done, copied)
  before <- tools::md5sum(copied)
  lookups <- local_audit_missing_lookups()

  train_models(partial, run_global_models = FALSE, run_local_models = TRUE)

  expect_identical(lookups$missing, character())
  expect_identical(tools::md5sum(copied), before)
  b_forecast <- function(info) {
    read_exact_artifact(info, local_artifact_path(info, "forecasts", "-single_models", combos[[2]]))
  }
  expect_equal(b_forecast(partial), b_forecast(full))
  final_models(full, average_models = FALSE)
  final_models(partial, average_models = FALSE)
  expect_equal(get_forecast_data(partial), get_forecast_data(full))

  stale <- read_exact_artifact(full, done[[1]])
  stale$Model_ID[stale$Model_Name == "business_mean"] <- paste0("custom-", standard_custom_model(offset = 1)$definition$version_id, "--local--C1")
  tampered <- copy_lookup_run(templates$prepared, withr::local_tempdir())
  write_data(stale, "A", tampered, "data", "forecasts", "-single_models")
  expect_error(train_models(tampered, run_global_models = FALSE, run_local_models = TRUE), "identity|pool")
})

test_that("a saved custom fit without its forecast is retrained instead of audited", {
  templates <- lookup_templates()
  trained <- templates$trained
  fit_path <- local_artifact_path(trained, "models", "-single_models", hash_data("A"), "rds")
  fitted <- read_exact_artifact(trained, fit_path, return_type = "object")
  custom_index <- which(fitted$Model_Name == "business_mean")
  fitted$Model_Fit[[custom_index]]$fit$fit$fit$context$forecast_horizon <- 1
  interrupted <- copy_lookup_run(templates$prepared, withr::local_tempdir())
  write_data(fitted, "A", interrupted, "object", "models", "-single_models")
  orphan <- local_artifact_path(interrupted, "models", "-single_models", hash_data("A"), "rds")
  before <- tools::md5sum(orphan)

  expect_no_error(train_models(interrupted, run_global_models = FALSE, run_local_models = TRUE))
  expect_false(identical(tools::md5sum(orphan), before))
  retrained <- read_exact_artifact(interrupted, orphan, return_type = "object")
  expect_equal(retrained$Model_ID, read_exact_artifact(trained, fit_path, return_type = "object")$Model_ID)
})

test_that("custom audits read only listed outputs and keep required errors", {
  info <- copy_lookup_run(lookup_templates()$trained, withr::local_tempdir())
  custom <- custom_run_read(info, TRUE)
  combos <- vapply(c("A", "B"), hash_data, character(1))
  all_listed <- fs::path_file(fs::dir_ls(fs::path(info$path, "forecasts")))
  expected <- custom_run_audit(info, custom, TRUE, FALSE, combos, TRUE)
  listed <- fs::path_file(local_artifact_path(info, "forecasts", "-single_models", combos[[1]]))
  unlisted <- spy_path(c(
    local_artifact_path(info, "forecasts", "-single_models", combos[[2]]),
    local_artifact_path(info, "models", "-single_models", combos[[2]], "rds")
  ))
  tracker <- local_artifact_spies()

  saved <- custom_run_audit(info, custom, TRUE, FALSE, combos, present = listed)

  expect_true(is.data.frame(saved[[combos[[1]]]]$single))
  expect_null(saved[[combos[[2]]]]$single)
  expect_false(any(unlisted %in% c(tracker$metadata_paths, tracker$payload_paths)))
  expect_true(spy_path(local_artifact_path(info, "models", "-single_models", combos[[1]], "rds")) %in%
    tracker$payload_paths)
  expect_error(custom_run_audit(info, custom, TRUE, FALSE, combos, complete = TRUE, present = listed),
    "Missing required Finn artifact")
  expect_equal(custom_run_audit(info, custom, TRUE, FALSE, combos, complete = TRUE, present = all_listed), expected)
  expect_equal(tracker$listings, 0L)
})

test_that("hierarchical final_models finds Best-Model from its single listing", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_completed_final_outputs(run_info)
  best <- spy_path(local_artifact_path(run_info, "forecasts", "-reconciled", hash_data("Best-Model")))
  tracker <- local_artifact_spies(max_listings = 1L)
  probed <- NA
  testthat::local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    reconcile_hierarchical_data = function(...) {
      probed <<- best %in% tracker$metadata_paths
      stop("reconciliation dispatched", call. = FALSE)
    },
    .package = "finnts"
  )

  expect_error(final_models(run_info), "reconciliation dispatched")
  expect_false(probed)
  expect_equal(tracker$listings, 1L)
})

test_that("global best-run promotion reads only records known to exist, without listing", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  write_data(tibble::tibble(combo = "A", best_run_name = "candidate", weighted_mape = 0.1),
    "A", project_info, "log", "logs", "-agent_best_run")
  missing <- spy_path(fs::path(project_info$path, "logs", paste0(
    hash_data(project_info$project_name), "-", hash_data(project_info$run_name), "-",
    hash_data("B"), "-agent_best_run.csv"
  )))
  known <- new.env(parent = emptyenv())
  known$series <- "A"
  tracker <- local_artifact_spies()

  records <- read_agent_best_run_records(project_info, c("A", "B"), known)

  expect_identical(names(records), c("A", "B"))
  expect_equal(nrow(records$A), 1L)
  expect_identical(as.character(records$A$best_run_name), "candidate")
  expect_equal(nrow(records$B), 0L)
  expect_false(missing %in% c(tracker$metadata_paths, tracker$payload_paths))
  expect_equal(tracker$listings, 0L)
  expect_equal(tracker$directory_listings, 0L)

  # without the workflow's known set, each series keeps its original exact read
  fallback <- read_agent_best_run_records(project_info, c("A", "B"))
  expect_equal(fallback$A, records$A)
  expect_equal(nrow(fallback$B), 0L)
  expect_true(missing %in% tracker$metadata_paths)
  expect_equal(tracker$listings, 0L)
})

# Record the path of every list_files() call while delegating to the real
# function. Returns an environment whose `paths` field accumulates them.
local_listing_paths <- function(.env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$paths <- character()
  original <- list_files
  testthat::local_mocked_bindings(list_files = function(storage_object, path, ...) {
    state$paths <- c(state$paths, as.character(path))
    original(storage_object, path, ...)
  }, .package = "finnts", .env = .env)
  state
}

test_that("completed custom training and ensembles-off runs add no forecast listing", {
  info <- copy_lookup_run(lookup_templates()$trained, withr::local_tempdir())
  lookups <- local_audit_missing_lookups()
  listings <- local_listing_paths()

  train_models(info, run_global_models = FALSE, run_local_models = TRUE)
  ensemble_models(info)

  expect_false(any(grepl("/forecasts/", listings$paths, fixed = TRUE)))
  # global models are disabled here, so global forecasts and the shared fit are
  # never written later; the audit still checks them exactly (as before) to
  # catch foreign outputs
  global_fit <- as.character(local_artifact_path(info, "models", "-single_models", hash_data("All-Data"), "rds"))
  local_missing <- setdiff(lookups$missing, global_fit)
  expect_identical(local_missing[!grepl("-global_models", local_missing, fixed = TRUE)], character())
})

# Agent with an underscore-free run ID, as Agent run-log names require. Like a
# real Agent project, its logs folder exists before any series runs.
make_run_log_agent <- function(path) {
  fs::dir_create(fs::path(path, "logs"))
  list(
    project_info = list(project_name = "run_logs", path = path, storage_object = NULL, data_output = "csv",
      object_output = "rds", combo_variables = "id", target_variable = "Target", date_type = "month"),
    run_id = "agentrun", agent_version = 2, forecast_horizon = 6, hist_end_date = as.Date("2026-06-01"),
    external_regressors = NULL, forecast_approach = "bottoms_up", negative_forecast = FALSE,
    run_global_models = FALSE, run_local_models = TRUE, llm = structure(list(), class = "Chat")
  )
}

# Write one series' saved best-run record for the Agent run.
write_series_record <- function(agent, combo, model_type, weighted_mape, run_complete, max_iterations) {
  info <- agent$project_info
  info$run_name <- agent$run_id
  write_data(data.frame(project_name = info$project_name, agent_run_id = agent$run_id,
    best_run_name = paste0("agent_", agent$run_id, "_", hash_data(combo), "_1"), model_type = model_type,
    combo = combo, weighted_mape = weighted_mape, max_iterations = max_iterations,
    run_complete = run_complete, stringsAsFactors = FALSE), combo, info, "log", "logs", "-agent_best_run")
}

# A child project's run-log paths, as a sorted /logs listing returns them.
series_run_logs <- function(agent, combo) {
  pattern <- paste0("*", hash_data(paste0(agent$project_info$project_name, "_", combo)), "-*.csv")
  sort(as.character(fs::dir_ls(fs::path(agent$project_info$path, "logs"), glob = pattern)), method = "radix")
}

test_that("re-run local series tasks skip finished series before listing and list others once", {
  agent <- make_run_log_agent(withr::local_tempdir())
  write_series_record(agent, "done", "local", 0.2, TRUE, 3)
  write_series_record(agent, "goal", "local", 0.01, FALSE, 0)
  write_series_record(agent, "global_above", "global", 0.2, TRUE, 3)
  write_series_record(agent, "partial", "local", 0.2, FALSE, 0)
  child <- agent$project_info
  child$project_name <- paste0(child$project_name, "_", hash_data("partial"))
  child$run_name <- paste0("agent_", agent$run_id, "_", hash_data("partial"), "_20260101T000000Z_0")
  write_data(data.frame(project_name = child$project_name, run_name = child$run_name,
    created = "2026-01-01 10:00:00", agent_version = 2, agent_forecast_approach = "bottoms_up",
    weighted_mape = 0.2, models_to_run = "arima", stringsAsFactors = FALSE), NULL, child, "log", "logs")
  listings <- local_listing_paths()
  workflows <- list()
  testthat::local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    par_end = function(...) invisible(NULL),
    fcst_agent_workflow = function(agent_info, combo, max_iter, run_log_paths = NULL, ...) {
      workflows[[combo]] <<- list(max_iter = max_iter, run_log_paths = run_log_paths)
      list(warnings = list())
    },
    .package = "finnts"
  )

  # the alert names the completed count, from which a crash reports its iteration
  expect_message(results <- run_agent_local_series(agent, c("done", "goal", "global_above", "partial", "missing"),
    max_iter = 3, weighted_mape_goal = 0.05, parallel_processing = NULL, inner_parallel = FALSE,
    num_cores = 1, seed = 123), "Adjusting max iterations down due to 1 previously completed iteration.", fixed = TRUE)

  expect_setequal(vapply(results, function(result) result$Combo, character(1)),
    c("done", "goal", "global_above", "partial", "missing"))
  expect_setequal(names(workflows), vapply(c("global_above", "partial", "missing"), hash_data, character(1),
    USE.NAMES = FALSE))
  log_listings <- listings$paths[grepl("/logs/", listings$paths, fixed = TRUE)]
  expect_length(log_listings, 3L)
  for (finished in c("done", "goal")) {
    child_hash <- hash_data(paste0(agent$project_info$project_name, "_", hash_data(finished)))
    expect_false(any(grepl(child_hash, log_listings, fixed = TRUE)))
  }
  expect_identical(workflows[[hash_data("partial")]]$run_log_paths, series_run_logs(agent, hash_data("partial")))
  expect_length(workflows[[hash_data("partial")]]$run_log_paths, 1L)
  expect_equal(workflows[[hash_data("partial")]]$max_iter, 2)
  expect_identical(workflows[[hash_data("missing")]]$run_log_paths, character())
})

test_that("an unreadable best-run record stops a re-run local series task", {
  agent <- make_run_log_agent(withr::local_tempdir())
  record <- fs::path(agent$project_info$path, "logs", paste0(hash_data(agent$project_info$project_name), "-",
    hash_data(agent$run_id), "-", hash_data("broken"), "-agent_best_run.csv"))
  fs::dir_create(fs::path_dir(record))
  fs::file_create(record)
  started <- 0L
  testthat::local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    par_end = function(...) invisible(NULL),
    fcst_agent_workflow = function(...) {
      started <<- started + 1L
      list(warnings = list())
    },
    .package = "finnts"
  )

  expect_error(run_agent_local_series(agent, "broken", 3, 0.05, NULL, FALSE, 1, 123),
    "empty or unreadable", fixed = TRUE)
  expect_identical(started, 0L)
})

# Scripted chat answering each reasoning prompt with the next model setting.
retry_chat <- function(models) {
  chat <- new.env(parent = emptyenv())
  chat$prompts <- character()
  chat$responses <- vapply(models, function(models_to_run) {
    as.character(jsonlite::toJSON(list(
      models_to_run = models_to_run, external_regressors = "NULL", clean_missing_values = "FALSE",
      clean_outliers = "FALSE", forecast_approach = "bottoms_up", stationary = "FALSE",
      feature_selection = "FALSE", multistep_horizon = "FALSE", seasonal_period = "NULL",
      recipes_to_run = "R1", lag_periods = "NULL", rolling_window_periods = "NULL",
      reasoning = "test"
    ), auto_unbox = TRUE))
  }, character(1), USE.NAMES = FALSE)
  chat$chat <- function(prompt, ...) {
    chat$prompts <- c(chat$prompts, as.character(prompt))
    chat$responses[[length(chat$prompts)]]
  }
  chat$set_system_prompt <- function(prompt) chat
  chat
}

# Drop vroom's per-read `problems` pointer (also inside the `run_logs`
# attribute) so two reads of the same run logs compare as identical.
without_read_pointers <- function(history) {
  if (!is.data.frame(history)) return(history)
  attr(history, "problems") <- NULL
  if (!is.null(attr(history, "run_logs"))) attr(history, "run_logs") <- without_read_pointers(attr(history, "run_logs"))
  history
}

# The settings a submitted proposal records in its run log, in the form the
# duplicate check compares (multipart values joined, "NULL" as missing).
logged_run_settings <- function(inputs) {
  row <- inputs[setdiff(names(inputs), "reasoning")]
  for (field in intersect(c("models_to_run", "external_regressors", "recipes_to_run", "lag_periods",
    "rolling_window_periods", "seasonal_period"), names(row))) {
    row[[field]] <- collapse_or_na(row[[field]])
  }
  as.data.frame(lapply(row, function(value) if (length(value) == 1L) value else NA), stringsAsFactors = FALSE)
}

# Run three local iterations with the real reasoning step. The first proposal
# is invalid, the first training attempt of iteration 2 fails after starting
# its run log, iteration 2 is quality-rejected, and iteration 3 first repeats
# iteration 2's settings. `exact` passes the start listing as run-log paths.
run_logged_retry_workflow <- function(exact, envir = parent.frame()) {
  agent <- make_run_log_agent(withr::local_tempdir(.local_envir = envir))
  combo <- hash_data("series")
  chat <- retry_chat(c("unknown", "arima", "ets", "ets", "arima---ets"))
  agent$llm <- chat
  state <- new.env(parent = emptyenv())
  state$listings <- 0L
  state$checking <- TRUE
  state$matches <- logical()
  state$submits <- 0L
  state$failed <- FALSE
  original_list_files <- list_files
  original_reason <- reason_inputs
  testthat::local_mocked_bindings(
    new_llm_session = function(llm) llm,
    iterate_forecast_system_prompt = function(...) "system prompt",
    get_foundation_model_suffix = function() "",
    wait_before_retry = function(...) invisible(NULL),
    list_files = function(storage_object, path, ...) {
      if (!state$checking && grepl("/logs/", path, fixed = TRUE)) state$listings <- state$listings + 1L
      original_list_files(storage_object, path, ...)
    },
    reason_inputs = function(agent_info, combo = NULL, ...) {
      state$checking <- TRUE
      expected <- new_reason_history(load_run_results(agent_info, combo), agent_info$agent_version)
      state$checking <- FALSE
      state$matches <- c(state$matches, identical(without_read_pointers(list(...)$previous_run_results),
        without_read_pointers(expected$previous_run_results)))
      original_reason(agent_info, combo, ...)
    },
    submit_fcst_run = function(agent_info, inputs, combo, timestamp, ...) {
      iteration <- as.integer(sub(".*_", "", timestamp))
      info <- agent_info$project_info
      info$project_name <- paste0(info$project_name, "_", combo)
      info$run_name <- paste0("agent_", agent_info$run_id, "_", combo, "_", timestamp)
      row <- logged_run_settings(inputs)
      row$project_name <- info$project_name
      row$run_name <- info$run_name
      row$created <- paste0("2026-01-01 10:00:0", iteration)
      row$agent_version <- agent_info$agent_version
      row$agent_forecast_approach <- "bottoms_up"
      state$submits <- state$submits + 1L
      if (iteration == 1L && !state$failed) {
        state$failed <- TRUE
        row$weighted_mape <- NA_real_
        write_data(row, NULL, info, "log", "logs")
        stop("transient training failure", call. = FALSE)
      }
      row$weighted_mape <- c(0.5, NA, 0.4)[[iteration + 1L]]
      row$selection_status <- c("evaluated", "rejected", "evaluated")[[iteration + 1L]]
      write_data(row, NULL, info, "log", "logs")
      info
    },
    get_fcst_output = function(...) data.frame(),
    calculate_fcst_metrics = function(run_info, ...) {
      if (grepl("_1$", run_info$run_name)) structure(Inf, selection_ok = FALSE) else 0.5
    },
    log_best_run = function(...) "logged",
    finalize_run = function(...) "finalized",
    .package = "finnts"
  )
  paths <- if (exact) agent_run_log_paths(agent, combo)
  history <- if (exact) load_run_results(agent, combo, run_log_paths = paths) else load_run_results(agent, combo)
  state$checking <- FALSE
  args <- list(agent_info = agent, combo = combo, weighted_mape_goal = 0.05, parallel_processing = NULL,
    inner_parallel = FALSE, num_cores = 1, max_iter = 3, seed = 123, previous_run_results = history,
    fallback_available = FALSE)
  if (exact) args$run_log_paths <- paths
  ctx <- do.call(fcst_agent_workflow, args)
  list(ctx = ctx, chat = chat, state = state, log_files = series_run_logs(agent, combo))
}

# Both modes must retry an invalid proposal with its error, show the rejected
# iteration, and retry its repeated settings as a duplicate.
expect_retry_history <- function(result) {
  expect_length(result$chat$prompts, 5L)
  expect_match(result$chat$prompts[[1]], "No errors in previous input recommendation.", fixed = TRUE)
  expect_match(result$chat$prompts[[2]], "Invalid proposed", fixed = TRUE)
  expect_match(result$chat$prompts[[4]], "rejected", fixed = TRUE)
  expect_match(result$chat$prompts[[5]], "Duplicate parameter set", fixed = TRUE)
  expect_identical(result$state$submits, 4L)
  expect_length(result$state$matches, 5L)
  expect_true(all(result$state$matches))
  expect_identical(result$ctx$node, "stop")
  expect_length(result$log_files, 3L)
}

test_that("reasoning retries see the same run history with one listing per series", {
  listing <- run_logged_retry_workflow(exact = FALSE)
  expect_retry_history(listing)
  expect_identical(listing$state$listings, 2L)
  expect_null(listing$ctx$run_log_paths)

  exact <- run_logged_retry_workflow(exact = TRUE)
  expect_retry_history(exact)
  expect_identical(exact$state$listings, 0L)
  expect_identical(exact$ctx$run_log_paths, exact$log_files)
})

# Write one input file per series for the Agent run, as set_agent_info() does,
# and return their paths in listing order.
write_agent_inputs <- function(agent, series) {
  info <- agent$project_info
  info$run_name <- agent$run_id
  for (name in series) {
    write_data(data.frame(Combo = name, Date = as.Date("2026-01-01"), Target = 1), name, info, "data", "input_data")
  }
  list_files(NULL, paste0(info$path, "/input_data/*", hash_data(info$project_name), "-",
    hash_data(agent$run_id), "*.csv"))
}

# Consolidated EDA rows naming the given series, plus the run-level "All" row.
eda_naming <- function(series) {
  tibble::tibble(Combo = c("All", series), Analysis_Type = "Missing_Data", Metric = "total_rows", Value = "1")
}

test_that("the startup series list reads only input files whose series EDA doesn't name", {
  agent <- make_run_log_agent(withr::local_tempdir())
  listed <- write_agent_inputs(agent, c("west", "east", "001", "north"))
  info <- agent$project_info
  info$run_name <- agent$run_id
  full_read <- unique(read_file(info, file_list = listed, return_type = "df", character_columns = "Combo")$Combo)
  reads <- list()
  original_read <- read_file
  testthat::local_mocked_bindings(read_file = function(run_info, path = NULL, file_list = NULL, ...) {
    reads[[length(reads) + 1L]] <<- as.character(file_list)
    original_read(run_info, path = path, file_list = file_list, ...)
  }, .package = "finnts")
  input_file <- function(series) {
    paste0(hash_data("run_logs"), "-", hash_data("agentrun"), "-", vapply(series, hash_data, character(1),
      USE.NAMES = FALSE), ".csv")
  }

  # EDA names every series (plus one with no input file): no input file is read
  expect_identical(agent_input_series(agent, eda_naming(c("north", "001", "east", "west", "gone"))), full_read)
  expect_length(reads, 0L)

  # partial EDA: only the two uncovered files are read, in one read
  expect_identical(agent_input_series(agent, eda_naming(c("west", "north"))), full_read)
  expect_length(reads, 1L)
  expect_setequal(fs::path_file(reads[[1]]), input_file(c("east", "001")))

  # no EDA names: every file is read, as before
  reads <- list()
  expect_identical(agent_input_series(agent, tibble::tibble()), full_read)
  expect_length(reads, 1L)
  expect_length(reads[[1]], 4L)
  expect_true("001" %in% full_read)
})

test_that("iterate_forecast() builds its series list without reading inputs EDA covers", {
  agent <- make_run_log_agent(withr::local_tempdir())
  write_agent_inputs(agent, c("west", "east"))
  reads <- character()
  captured <- NULL
  original_read <- read_file
  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    get_eda_data = function(...) eda_naming("west"),
    read_file = function(run_info, path = NULL, file_list = NULL, ...) {
      reads <<- c(reads, as.character(file_list))
      original_read(run_info, path = path, file_list = file_list, ...)
    },
    load_best_agent_run = function(agent_info) {
      captured <<- agent_info$selection_combos
      stop("series list captured", call. = FALSE)
    },
    .package = "finnts"
  )

  expect_error(iterate_forecast(agent, max_iter = 1, num_cores = 1), "series list captured")
  expect_setequal(captured, c("west", "east"))
  input_reads <- reads[grepl("input_data", reads, fixed = TRUE)]
  expect_identical(fs::path_file(input_reads),
    paste0(hash_data("run_logs"), "-", hash_data("agentrun"), "-", hash_data("east"), ".csv"))
})

# Write one Agent local iteration's forecast artifacts: run log, splits, and
# the series' single and average forecasts. Returns run info for that run.
write_single_series_forecast <- function(path) {
  run_info <- artifact_test_run(path)
  run_info$combo <- hash_data("A")
  artifact_test_log(run_info, combo_variables = "ID", forecast_approach = "bottoms_up")
  artifact_test_splits(run_info)
  artifact_test_forecast(run_info)
  artifact_test_forecast(run_info, suffix = "average_models", model = "snaive", best = "No")
  run_info
}

test_that("an Agent local iteration reads its forecast without listing the forecasts folder", {
  run_info <- write_single_series_forecast(withr::local_tempdir())
  without_problems <- function(x) {
    attr(x, "problems") <- NULL
    x
  }
  listed <- without_problems(get_forecast_data(run_info))
  run_info$single_series_project <- TRUE
  tracker <- local_artifact_spies()

  result <- without_problems(get_forecast_data(run_info))

  expect_identical(result, listed)
  expect_equal(tracker$listings, 0L)
  expect_equal(tracker$directory_listings, 0L)
})

test_that("an Agent local iteration marks its run as one series; a global one does not", {
  agent <- artifact_test_agent(withr::local_tempdir())
  input_info <- agent$project_info
  input_info$run_name <- agent$run_id
  write_data(tibble::tibble(Combo = "A", Date = as.Date("2023-01-01") + c(0, 31, 59), Target = c(1, 2, 3)),
    "A", input_info, "data", "input_data")
  checked <- list()
  local_mocked_bindings(
    prep_data = function(...) invisible(NULL),
    prep_models = function(...) invisible(NULL),
    train_models = function(...) invisible(NULL),
    final_models = function(...) {
      list(selections = list(A = list(selected_id = "m")), source_selections = list(), rejected_combos = character())
    },
    validate_run_outputs = function(run_info, ...) {
      checked[[length(checked) + 1L]] <<- run_info
      invisible(TRUE)
    }
  )

  local <- submit_fcst_run(agent, list(models_to_run = "arima"), hash_data("A"), "t1")
  global <- submit_fcst_run(agent, list(models_to_run = "arima"), NULL, "t2")

  expect_true(isTRUE(local$single_series_project))
  expect_true(isTRUE(checked[[1]]$single_series_project))
  expect_null(global$single_series_project)
})
