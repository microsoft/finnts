# Independent trusted-envelope fixture for offline Agent integration. The literal
# mean rule has synthetic attestations and is never authored by a live provider.
custom_agent_model <- function(name = "business_mean", modes = c("local", "global")) {
  definition <- finnts:::new_custom_model_definition(
    name, "Use the historical mean.", "Use the analysis mean without transformations; {literal_contract_text} is descriptive text.",
    modes, c(fit = "function(data, context, parameters) { mean(data$Target) }", predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row,.pred=rep(object,nrow(new_data)))"),
    list(predictors = character(), recipes = "R1", target_scale = "original", date_types = "month", forecast_horizon = c(
      1,
      2
    ), missing_data = "error")
  )
  structure(list(schema_version = 1L, definition = definition, validation = list(
    version_id = definition$version_id, technical_passed = TRUE,
    checks = list(arithmetic = "Independent synthetic mean fixture")
  ), approval = list(
    version_id = definition$version_id,
    intent_confirmed = TRUE, allow_code = TRUE
  )), class = "finnts_custom_model")
}

# Create only input/project metadata, not a forecasting pipeline. The optional
# series count supports local and pooled fixtures using constant known targets.
custom_agent_inputs <- function(path, series = 1L) {
  data <- data.frame(Date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 24), series), id = rep(paste0(
    "series",
    seq_len(series)
  ), each = 24), value = rep(seq_len(series) * 100, each = 24))
  list(
    project_info = set_project_info(
      project_name = "custom-agent", path = path, combo_variables = "id", target_variable = "value",
      date_type = "month"
    ), llm = structure(list(), class = "Chat"), input_data = data, forecast_horizon = 1, negative_forecast = TRUE,
    run_global_models = FALSE, back_test_scenarios = 1, custom_models = list(business_mean = custom_agent_model()), models_to_run = "business_mean"
  )
}

test_that("public Agent setup pins approved custom enrollment without executing source", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- do.call(set_agent_info, args)
  expect_match(info$custom_agent_contract_id, "^custom-agent-[a-f0-9]{64}$")
  expect_identical(info$custom_agent_contract$selected, "business_mean")
  expect_false(info$allow_hierarchical_forecast)
  expect_identical(validate_agent_custom_models(info$custom_agent_contract), info$custom_agent_contract)
})

test_that("custom setup preserves exact enrollment and rejects omission or legacy adoption", {
  args <- custom_agent_inputs(withr::local_tempdir())
  args$models_to_run <- c("business_mean", "meanf")
  info <- do.call(set_agent_info, args)
  files <- list.files(args$project_info$path, recursive = TRUE, full.names = TRUE)
  hashes <- tools::md5sum(files)
  args$models_to_run <- rev(args$models_to_run)
  repeated <- do.call(set_agent_info, args)
  expect_identical(repeated$run_id, info$run_id)
  expect_identical(repeated$custom_agent_contract_id, info$custom_agent_contract_id)
  expect_identical(tools::md5sum(files), hashes)
  omitted <- args
  omitted$custom_models <- omitted$models_to_run <- NULL
  expect_error(do.call(set_agent_info, omitted), "omitted")
  changed <- args
  changed$custom_models$business_mean$validation$checks$arithmetic <- "Changed evidence"
  expect_error(do.call(set_agent_info, changed), "enrollment changed")
  changed <- args
  changed$forecast_horizon <- 2
  expect_error(do.call(set_agent_info, changed), "enrollment changed")
  expect_identical(tools::md5sum(files), hashes)
  legacy <- custom_agent_inputs(withr::local_tempdir())
  ordinary <- legacy
  ordinary$models_to_run <- ordinary$custom_models <- NULL
  do.call(set_agent_info, ordinary)
  expect_error(do.call(set_agent_info, legacy), "enrollment changed")
})

test_that("parent records are exact-path immutable and malformed content stays untouched", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- do.call(set_agent_info, args)
  parent <- agent_custom_parent_info(info)
  path <- local_artifact_path(parent, "logs", "-agent-custom-models", extension = "rds")
  testthat::local_mocked_bindings(list_files = function(...) stop("Unexpected listing"), .package = "finnts")
  expect_identical(read_agent_custom_record(info, TRUE), info$custom_agent_contract)
  original <- tools::md5sum(path)
  write_agent_custom_record(info, info$custom_agent_contract)
  expect_identical(tools::md5sum(path), original)
  saveRDS(NULL, path)
  corrupt <- tools::md5sum(path)
  expect_error(read_agent_custom_record(info), "record identity")
  expect_error(write_agent_custom_record(info, info$custom_agent_contract), "record identity")
  expect_identical(tools::md5sum(path), corrupt)
  other <- info
  other$run_id <- "not-yet-logged"
  write_agent_custom_record(other, info$custom_agent_contract)
  expect_identical(read_agent_custom_record(other, TRUE), info$custom_agent_contract)
})

test_that("execution entry restores saved identity and rejects conflicting context", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- do.call(set_agent_info, args)
  stripped <- info
  stripped$custom_agent_contract <- stripped$custom_agent_contract_id <- NULL
  restored <- load_agent_custom_state(stripped)
  expect_identical(restored$custom_agent_contract_id, info$custom_agent_contract_id)
  changed <- info
  changed$negative_forecast <- FALSE
  expect_error(load_agent_custom_state(changed), "negative_forecast|context")
  record_only <- info
  record_only$run_id <- "record-only"
  write_agent_custom_record(record_only, info$custom_agent_contract)
  expect_error(load_agent_custom_state(record_only), "parent log")
  expect_error(agent_custom_preflight(info, "spark", FALSE), "sequential|local_machine")
  expect_error(agent_custom_preflight(info, NULL, TRUE), "inner_parallel")
})

# Complete offline forecast proposal; the LLM selects fixed approved logic only.
custom_agent_response <- function(models = "business_mean", ...) {
  jsonlite::toJSON(utils::modifyList(
    list(
      models_to_run = models, external_regressors = "NULL", clean_missing_values = FALSE,
      clean_outliers = FALSE, forecast_approach = "bottoms_up", stationary = FALSE, feature_selection = FALSE, multistep_horizon = FALSE,
      seasonal_period = "NULL", recipes_to_run = "R1", lag_periods = "NULL", rolling_window_periods = "NULL", reasoning = "Use the approved fixed mean."
    ),
    list(...)
  ), auto_unbox = TRUE)
}

test_that("custom reasoning has no built-in defaults or source in its prompt", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- do.call(set_agent_info, args)
  info$custom_foundation_suffix <- ""
  info$llm <- list(chat = function(...) custom_agent_response())
  testthat::local_mocked_bindings(
    get_foundation_model_suffix = function(...) stop("Unselected foundation probe"), load_eda_results = function(...) "Offline EDA",
    .package = "finnts"
  )
  prompt <- iterate_forecast_system_prompt(info, "combo", 0.1)
  expect_match(prompt, "business_mean", fixed = TRUE)
  expect_false(grepl("arima---|function\\(data|feature_selection=\"TRUE\"", prompt))
  result <- reason_inputs(info, "combo", 0.1,
    previous_run_results = "No Previous Runs", previous_version_results = "No Previous Runs",
    total_runs = 0
  )
  expect_identical(result$models_to_run, "business_mean")
  expect_false(result$stationary)
  info$llm <- list(chat = function(...) "{\"models_to_run\":\"business_mean\"}")
  expect_error(reason_inputs(info, "combo", 0.1,
    previous_run_results = "No Previous Runs", previous_version_results = "No Previous Runs",
    total_runs = 0
  ), class = "finnts_reason_proposal_invalid")
})

test_that("custom Agent submission uses the real pinned standard runtime", {
  args <- custom_agent_inputs(withr::local_tempdir())
  args$models_to_run <- c("business_mean", "meanf")
  info <- do.call(set_agent_info, args)
  proposal <- validate_agent_custom_proposal(
    jsonlite::fromJSON(custom_agent_response("business_mean---meanf")), info$custom_agent_contract,
    "combo"
  )
  run <- submit_fcst_run(info, proposal, hash_data("series1"), "fixed", num_cores = 1)
  forecasts <- get_forecast_data(run)
  expect_true(nrow(forecasts) > 0)
  expect_equal(unique(forecasts$Forecast), 100)
  allowed <- c(paste0("custom-", args$custom_models$business_mean$definition$version_id, "--local--R1"), "meanf--local--R1")
  expect_true(all(unlist(strsplit(unique(forecasts$Model_ID), "_", fixed = TRUE)) %in% allowed))
  expect_true(any(forecasts$Recipe_ID == "simple_average"))
  log <- read_selection_file(run, "logs")
  expect_identical(log$custom_agent_contract_id, info$custom_agent_contract_id)
  expect_setequal(custom_run_load(run, log)$pool$selected, args$models_to_run)
  before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
  run_again <- submit_fcst_run(info, proposal, hash_data("series1"), "fixed", num_cores = 1)
  expect_equal(get_forecast_data(run_again), forecasts)
  expect_identical(tools::md5sum(names(before)), before)
  expect_no_error(audit_agent_custom_child(info, run, hash_data("series1")))
  builtin <- validate_agent_custom_proposal(
    jsonlite::fromJSON(custom_agent_response("meanf")), info$custom_agent_contract,
    "combo"
  )
  builtin_run <- submit_fcst_run(info, builtin, hash_data("series1"), "builtin-subset", num_cores = 1)
  expect_equal(unique(get_forecast_data(builtin_run)$Forecast), 100)
  expect_no_error(audit_agent_custom_child(info, builtin_run, hash_data("series1")))
  altered <- read_selection_file(builtin_run, "logs")
  altered$negative_forecast <- FALSE
  write_data(altered, NULL, builtin_run, "log", "logs")
  expect_error(audit_agent_custom_child(info, builtin_run, hash_data("series1")), "negative_forecast|controls")
  expect_error(submit_fcst_run(info, builtin, hash_data("series1"), "builtin-subset", num_cores = 1), "negative_forecast|controls")
  log$custom_agent_contract_id <- "custom-agent-wrong"
  write_data(log, NULL, run, "log", "logs")
  expect_error(audit_agent_custom_child(info, run, hash_data("series1")), "parent identity")
})

# Serializable offline Chat stand-in with independent session state per clone.
# The shared tracker observes calls only; no conversation state is shared.
custom_agent_chat <- function(response, tracker = new.env(parent = emptyenv()), observation_path = NULL) {
  if (is.null(tracker$calls))
    tracker$calls <- 0L
  factory <- function(response, tracker, observation_path) {
    session <- new.env(parent = emptyenv())
    class(session) <- "Chat"
    session$turns <- list()
    session$clone <- function(deep = FALSE) factory(response, tracker, observation_path)
    session$set_system_prompt <- function(prompt) {
      session$prompt <- prompt
      session
    }
    session$set_turns <- function(turns) {
      session$turns <- turns
      session
    }
    session$chat <- function(prompt, ...) {
      if (!is.null(observation_path))
        saveRDS(list(pid = Sys.getpid(), previous_turns = length(session$turns)), file.path(observation_path, paste0(
          "chat-",
          Sys.getpid(), ".rds"
        )))
      tracker$calls <- tracker$calls + 1L
      session$turns <- c(session$turns, list(prompt))
      response
    }
    session
  }
  factory(response, tracker, observation_path)
}

test_that("public custom iteration publishes summaries and restarts without new fitting", {
  args <- custom_agent_inputs(withr::local_tempdir())
  tracker <- new.env(parent = emptyenv())
  args$llm <- custom_agent_chat(custom_agent_response(), tracker)
  info <- do.call(set_agent_info, args)
  testthat::local_mocked_bindings(
    get_eda_data = function(...) data.frame(done = TRUE), load_eda_results = function(...) "Offline EDA",
    get_foundation_model_suffix = function(...) stop("Unselected foundation probe"), .package = "finnts"
  )
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  forecast <- get_agent_forecast(info)
  summaries <- get_summarized_models(info)
  expect_true(nrow(forecast) > 0)
  expect_equal(unique(forecast$Forecast), 100)
  expect_true(any(summaries$name == "version_id"))
  expect_false(any(summaries$section == "error"))
  expect_true(any(grepl("{literal_contract_text}", summaries$value, fixed = TRUE)))
  expect_equal(tracker$calls, 1)
  expect_length(args$llm$turns, 0L)
  files <- list.files(args$project_info$path, recursive = TRUE, full.names = TRUE)
  hashes <- tools::md5sum(files)
  testthat::local_mocked_bindings(train_models = function(...) stop("Unexpected refit"), .package = "finnts")
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  expect_equal(tracker$calls, 1)
  expect_equal(get_agent_forecast(info), forecast)
  expect_identical(tools::md5sum(files), hashes)
  corrupted <- get_best_agent_run(info)
  corrupted$custom_agent_contract_id <- "custom-agent-wrong"
  write_data(corrupted, "series1", agent_custom_parent_info(info), "log", "logs", "-agent_best_run")
  expect_error(iterate_forecast(info, max_iter = 1, num_cores = 1), "winner parent identity")
})

test_that("custom execution and update guards run before expensive work", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- do.call(set_agent_info, args)
  stripped <- info
  stripped$custom_agent_contract <- stripped$custom_agent_contract_id <- NULL
  testthat::local_mocked_bindings(
    get_eda_data = function(...) stop("Unexpected EDA"), update_fcst_agent_workflow = function(...) stop("Unexpected update graph"),
    load_update_runs = function(...) stop("Unexpected completion scan"), .package = "finnts"
  )
  expect_error(iterate_forecast(stripped, parallel_processing = "spark"), "sequential|local_machine")
  expect_error(iterate_forecast(info, inner_parallel = TRUE), "inner_parallel")
  expect_error(update_forecast(stripped), "Custom Agent.*update|custom.*update")
  stripped$overwrite <- TRUE
  expect_error(initial_checks(stripped), "Custom Agent.*update|custom.*update")
})

test_that("global-only custom iteration preserves one pinned global run and history", {
  args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  args$run_global_models <- TRUE
  args$run_local_models <- FALSE
  tracker <- new.env(parent = emptyenv())
  args$llm <- custom_agent_chat(custom_agent_response(), tracker)
  info <- do.call(set_agent_info, args)
  testthat::local_mocked_bindings(
    get_eda_data = function(...) data.frame(done = TRUE), load_eda_results = function(...) "Offline EDA",
    vip_available = function(...) FALSE, get_foundation_model_suffix = function(...) stop("Unselected foundation probe"),
    .package = "finnts"
  )
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 1, num_cores = 1)
  forecast <- get_agent_forecast(info)
  best <- get_best_agent_run(info)
  expect_equal(unique(forecast$Forecast), 150)
  expect_identical(unique(best$model_type), "global")
  expect_length(unique(best$best_run_name), 1L)
  expect_identical(unique(best$custom_agent_contract_id), info$custom_agent_contract_id)
  history <- load_run_results(info)
  expect_identical(history$models_to_run, "business_mean")
  expect_identical(history$custom_agent_contract_id, info$custom_agent_contract_id)
  expect_identical(history$model_avg_wmape, history$model_median_wmape)
  expect_equal(history$model_std_wmape, 0)
  expect_false(any(get_summarized_models(info)$section == "error"))
  testthat::local_mocked_bindings(train_models = function(...) stop("Unexpected global refit"), .package = "finnts")
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 1, num_cores = 1)
  expect_equal(tracker$calls, 1)
  parent <- info$project_info
  parent$project_name <- paste0(parent$project_name, "_", hash_data("all"))
  parent$run_name <- best$best_run_name[[1]]
  log <- read_selection_file(parent, "logs")
  log$custom_agent_contract_id <- "custom-agent-wrong"
  write_data(log, NULL, parent, "log", "logs")
  expect_error(load_run_results(info), "history.*identity")
  expect_error(iterate_forecast(info, max_iter = 1, num_cores = 1), "parent identity")
})

test_that("missing parent records and custom predecessors cannot enter update recovery", {
  args <- custom_agent_inputs(withr::local_tempdir())
  predecessor <- do.call(set_agent_info, args)
  current <- predecessor
  current$run_id <- "builtin-current"
  current$agent_version <- 2
  current$overwrite <- TRUE
  current$custom_agent_contract <- current$custom_agent_contract_id <- NULL
  original_read <- finnts:::read_file
  testthat::local_mocked_bindings(
    load_update_runs = function(...) tibble::tibble(), list_files = function(...) c(
      "first.csv",
      "second.csv"
    ), read_file = function(run_info, ...) {
      arguments <- list(...)
      if (identical(arguments$file_list, c("first.csv", "second.csv"))) {
        return(tibble::tibble(agent_version = 1, run_id = predecessor$run_id))
      }
      original_read(run_info, ...)
    }, find_completed_previous_agent_runs = function(...) list(list(agent_info = predecessor)), get_total_combos = function(...) stop("Unexpected recovery routing"),
    .package = "finnts"
  )
  expect_error(initial_checks(current), "Custom Agent updates are unsupported")
  marker_only <- current
  marker_only$run_id <- "marker-without-record"
  marker_only$custom_agent_contract_id <- predecessor$custom_agent_contract_id
  expect_error(load_agent_custom_state(marker_only))
  expect_error(reject_agent_custom_update(marker_only))
})

test_that("invalid custom setup fails before writing and metadata ownership stays exact", {
  args <- custom_agent_inputs(withr::local_tempdir())
  files <- list.files(args$project_info$path, recursive = TRUE, full.names = TRUE)
  hashes <- tools::md5sum(files)
  builtin <- args
  builtin$models_to_run <- "meanf"
  expect_error(do.call(set_agent_info, builtin), "at least one approved custom")
  wrong <- args
  wrong$negative_forecast <- FALSE
  expect_error(do.call(set_agent_info, wrong), "negative_forecast")
  wrong <- args
  wrong$project_info$data_output <- "parquet"
  expect_error(do.call(set_agent_info, wrong), "CSV")
  wrong <- args
  wrong$run_local_models <- FALSE
  wrong$run_global_models <- TRUE
  expect_error(do.call(set_agent_info, wrong), "at least two series")
  expect_identical(tools::md5sum(files), hashes)
  expect_identical(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE), files)
  info <- do.call(set_agent_info, args)
  path <- local_artifact_path(agent_custom_parent_info(info), "logs", "-agent-custom-models", extension = "rds")
  record <- readRDS(path)
  record$agent_run_id <- "wrong-owner"
  saveRDS(record, path)
  before <- tools::md5sum(path)
  expect_error(load_agent_custom_state(info), "record identity")
  expect_identical(tools::md5sum(path), before)
})

test_that("installed custom Agent forecasts dispatch to two independent real workers", {
  namespace <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace, "Meta", "package.rds")), "Real Agent worker dispatch is verified from the installed namespace")
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  observations <- withr::local_tempdir()
  args$llm <- custom_agent_chat(custom_agent_response(), observation_path = observations)
  info <- do.call(set_agent_info, args)
  original_parallel <- finnts:::par_start
  testthat::local_mocked_bindings(
    get_eda_data = function(...) data.frame(done = TRUE), load_eda_results = function(...) "Offline EDA",
    par_start = function(...) {
      parallel_info <- original_parallel(...)
      if (!is.null(parallel_info$cl))
        parallel::clusterCall(parallel_info$cl, function(expected_namespace) {
          stopifnot(identical(normalizePath(getNamespaceInfo(asNamespace("finnts"), "path")), normalizePath(expected_namespace)))
          testthat::local_mocked_bindings(load_eda_results = function(...) "Offline EDA", .package = "finnts", .env = globalenv())
          invisible(NULL)
        }, namespace)
      parallel_info
    }, .package = "finnts"
  )
  sequential_args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  sequential_args$llm <- custom_agent_chat(custom_agent_response())
  sequential <- do.call(set_agent_info, sequential_args)
  iterate_forecast(sequential, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  expected_forecast <- get_agent_forecast(sequential)
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, parallel_processing = "local_machine", num_cores = 2)
  forecast <- get_agent_forecast(info)
  expected <- ifelse(forecast$Combo == "series1", 100, 200)
  expect_equal(forecast$Forecast, expected)
  columns <- c("Combo", "Date", "Model_ID", "Train_Test_ID", "Forecast", "Best_Model")
  expect_equal(dplyr::arrange(forecast[, columns], Combo, Date, Train_Test_ID), dplyr::arrange(
    expected_forecast[, columns],
    Combo, Date, Train_Test_ID
  ))
  expect_identical(unique(forecast$Model_ID), paste0(
    "custom-", args$custom_models$business_mean$definition$version_id,
    "--local--R1"
  ))
  sessions <- lapply(list.files(observations, full.names = TRUE), readRDS)
  expect_length(sessions, 2L)
  expect_length(unique(vapply(sessions, function(session) session$pid, integer(1))), 2L)
  expect_true(all(vapply(sessions, function(session) session$previous_turns == 0L, logical(1))))
  expect_false(Sys.getpid() %in% vapply(sessions, function(session) session$pid, integer(1)))
  expect_false(any(get_summarized_models(info)$section == "error"))
  expect_length(args$llm$turns, 0L)
})

test_that("partial custom local restart does not repeat the global phase", {
  args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  args$run_global_models <- TRUE
  info <- do.call(set_agent_info, args)
  state <- new.env(parent = emptyenv())
  state$best <- data.frame(
    combo = c("series1", "series2"), model_type = c("local", "global"), weighted_mape = c(0.2, 0.2),
    run_complete = c(TRUE, FALSE), max_iterations = c(2, 0)
  )
  state$calls <- character()
  testthat::local_mocked_bindings(
    get_eda_data = function(...) data.frame(done = TRUE), load_best_agent_run = function(...) state$best,
    load_run_results = function(...) "No Previous Runs", fcst_agent_workflow = function(agent_info, combo, ...) {
      if (is.null(combo))
        stop("Unexpected repeated global phase")
      state$calls <- c(state$calls, combo)
      state$best$model_type[[2]] <- "local"
      state$best$run_complete[[2]] <- TRUE
      state$best$max_iterations[[2]] <- 2
      invisible(NULL)
    }, save_best_agent_run = function(...) invisible(NULL), save_agent_forecast = function(...) invisible(NULL), summarize_models = function(...) invisible(NULL),
    .package = "finnts"
  )
  iterate_forecast(info, max_iter = 2, weighted_mape_goal = 0.01, num_cores = 1)
  expect_identical(state$calls, hash_data("series2"))
  iterate_forecast(info, max_iter = 2, weighted_mape_goal = 0.01, num_cores = 1)
  expect_identical(state$calls, hash_data("series2"))
})
