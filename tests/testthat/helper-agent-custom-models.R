# Synthetic approved mean definition for offline replay tests. Source/evidence
# are fixed; this fixture never infers provider or authoring approval.
custom_update_model <- function(name = "replay_mean") {
  definition <- finnts:::new_custom_model_definition(
    name, "Use the historical mean.", "Use the analysis mean; {literal_update_text} is descriptive text.",
    c("local", "global"), c(fit = "function(data, context, parameters) mean(data$Target)", predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row,.pred=rep(object,nrow(new_data)))"),
    list(predictors = character(), recipes = "C1", target_scale = "original",
      date_types = "month", forecast_horizon = c(1, 2), missing_data = "error")
  )
  structure(list(definition = definition, validation = list(
    version_id = definition$version_id, technical_passed = TRUE,
    checks = list(arithmetic = "Independent synthetic mean fixture")
  ), approval = list(version_id = definition$version_id,
    intent_confirmed = TRUE, allow_code = TRUE)), class = "finnts_custom_model")
}

# Serializable offline Chat template; clones have independent histories.
# Its callbacks return one fixed complete proposal and never contact a provider.
custom_update_chat <- function(models = "replay_mean") {
  session <- new.env(parent = emptyenv())
  class(session) <- "Chat"
  session$turns <- list()
  session$clone <- function(deep = FALSE) custom_update_chat(models)
  session$set_system_prompt <- function(prompt) session
  session$set_turns <- function(turns) {
    session$turns <- turns
    session
  }
  session$chat <- function(prompt, ...) {
    session$turns <- c(session$turns, list(prompt))
    jsonlite::toJSON(list(
      models_to_run = models, external_regressors = "NULL",
      clean_missing_values = FALSE, clean_outliers = FALSE,
      forecast_approach = "bottoms_up", stationary = FALSE,
      feature_selection = FALSE, multistep_horizon = FALSE,
      seasonal_period = "NULL", recipes_to_run = "R1",
      lag_periods = "NULL", rolling_window_periods = "NULL",
      reasoning = "Use the approved fixed mean."), auto_unbox = TRUE)
  }
  session
}

# Write a completed synthetic EDA artifact into the caller's temporary project.
custom_update_eda <- function(agent_info, ...) {
  write_data(data.frame(Combo = "All", Analysis_Type = "Offline",
    Metric = "fixture", Value = "complete"), NULL,
    agent_custom_parent_info(agent_info), "data", "final_output", "-eda")
  invisible(NULL)
}

# Invocation-wide synthetic clock. Every mock installation continues beyond
# saved template versions rather than reusing their setup/run timestamps.
custom_update_timestamp <- local({
  time <- as.POSIXct("2030-01-01", tz = "UTC")
  function(...) {
    time <<- time + 1
    time
  }
})

# Install offline EDA, zero-wait retries and the monotonic fixture clock in the
# calling test scope. Retry counts and all production errors remain unchanged.
local_custom_update_runtime <- function(.local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    eda_agent_workflow = custom_update_eda,
    load_eda_results = function(...) "Offline EDA",
    wait_before_retry = function(...) invisible(NULL),
    get_timestamp = custom_update_timestamp,
    .package = "finnts", .env = .local_envir)
}

# Keep genuine default engines, tuning and predictions, using one small XGBoost
# configuration for recovery-routing tests. Other parameter domains are intact.
local_custom_default_grid <- function(.local_envir = parent.frame()) {
  original <- dials::grid_space_filling
  testthat::local_mocked_bindings(grid_space_filling = function(...) {
    grid <- original(...)
    if ("trees" %in% names(grid)) grid$trees <- rep(10L, nrow(grid))
    if ("tree_depth" %in% names(grid)) grid$tree_depth <- rep(2L, nrow(grid))
    grid
  }, .package = "dials", .env = .local_envir)
}

# Lazily train and optionally update genuine templates during measured tests.
# The cache belongs only to this test invocation. Each caller receives an
# isolated directory copy and fresh Chat templates; source bytes/identities are
# preserved. Never mutate cached projects or use mocked fitting/publication.
custom_update_fixture <- local({
  cache <- new.env(parent = emptyenv())
  function(global = FALSE, updated = TRUE, .local_envir = parent.frame()) {
    mode <- if (global) "global" else "local"
    key <- paste(mode, if (updated) "updated" else "initial", sep = "-")
    if (!exists(key, envir = cache, inherits = FALSE)) {
      local_custom_update_runtime()
      path <- withr::local_tempdir(.local_envir = cache)
      series <- if (global) c("first", "second") else "first"
      models <- if (global) "replay_mean---other_mean" else "replay_mean"
      data <- data.frame(Date = rep(seq(as.Date("2020-01-01"),
        by = "month", length.out = 24), length(series)),
        series = rep(series, each = 24), value = 100)
      envelopes <- list(replay_mean = custom_update_model())
      if (global) envelopes$other_mean <- custom_update_model("other_mean")
      args <- list(project_info = set_project_info(
        project_name = paste0("custom-update-", mode), path = path,
        combo_variables = "series", target_variable = "value", date_type = "month"),
        llm = custom_update_chat(models), input_data = data, forecast_horizon = 1,
        negative_forecast = TRUE, run_global_models = global,
        run_local_models = !global, back_test_scenarios = 1, custom_models = envelopes)
      previous <- load_agent_custom_state(do.call(set_agent_info, args))
      custom_update_eda(previous)
      iterate_forecast(previous, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
      if (global) {
        best <- get_best_agent_run(previous)
        child <- previous$project_info
        child$project_name <- paste0(child$project_name, "_", hash_data("all"))
        child$run_name <- best$best_run_name[[1]]
        ids <- paste0("custom-", vapply(envelopes, function(model)
          model$definition$version_id, character(1)), "--global--C1")
        for (combo in series) {
          rows <- read_selection_file(child, "forecasts", "-global_models", combo)
          rows$Best_Model <- if (combo == "first") "No" else
            ifelse(rows$Model_ID == ids[[1]], "Yes", "No")
          write_data(rows, combo, child, "data", "forecasts", "-global_models")
          average <- rows[rows$Model_ID == ids[[1]], , drop = FALSE]
          average$Model_ID <- paste(sort(ids), collapse = "_")
          average$Model_Name <- NA_character_
          average$Model_Type <- "local"
          average$Recipe_ID <- "simple_average"
          average$Best_Model <- if (combo == "first") "Yes" else "No"
          write_data(average, combo, child, "data", "forecasts", "-average_models")
        }
      }
      args$input_data <- rbind(data, data.frame(Date = as.Date("2022-01-01"),
        series = series, value = 125))
      args$overwrite <- TRUE
      current <- do.call(set_agent_info, args)
      testthat::local_mocked_bindings(
        create_custom_model = function(...) stop("Unexpected source regeneration"),
        forecast_new_combos = function(...) stop("Unexpected default fallback"),
        .package = "finnts")
      if (updated) update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
      files <- list.files(path, recursive = TRUE, full.names = TRUE)
      cache[[key]] <- list(args = args, previous = previous, current = current,
        forecast = get_agent_forecast(if (updated) current else previous), hashes = tools::md5sum(files))
    }
    template <- cache[[key]]
    expect_identical(tools::md5sum(names(template$hashes)), template$hashes)
    destination <- file.path(withr::local_tempdir(.local_envir = .local_envir), "project")
    fs::dir_copy(template$args$project_info$path, destination)
    result <- template[setdiff(names(template), "hashes")]
    models <- if (global) "replay_mean---other_mean" else "replay_mean"
    for (field in c("args", "previous", "current")) {
      result[[field]]$project_info$path <- destination
      result[[field]]$llm <- custom_update_chat(models)
    }
    result
  }
})

# Independent trusted-envelope fixture for offline Agent integration. The literal
# mean rule has synthetic attestations and is never authored by a live provider.
custom_agent_model <- function(name = "business_mean", modes = c("local", "global")) {
  definition <- finnts:::new_custom_model_definition(
    name, "Use the historical mean.", "Use the analysis mean without transformations; {literal_contract_text} is descriptive text.",
    modes, c(fit = "function(data, context, parameters) { mean(data$Target) }", predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row,.pred=rep(object,nrow(new_data)))"),
    list(predictors = character(), recipes = "C1", target_scale = "original",
      date_types = "month", forecast_horizon = c(1, 2), missing_data = "error"))
  structure(list(definition = definition, validation = list(
    version_id = definition$version_id, technical_passed = TRUE,
    checks = list(arithmetic = "Independent synthetic mean fixture")),
    approval = list(version_id = definition$version_id,
      intent_confirmed = TRUE, allow_code = TRUE)), class = "finnts_custom_model")
}

# Create only input/project metadata, not a forecasting pipeline. The optional
# series count supports local and pooled fixtures using constant known targets.
custom_agent_inputs <- function(path, series = 1L) {
  data <- data.frame(Date = rep(seq(as.Date("2020-01-01"), by = "month",
    length.out = 24), series), id = rep(paste0("series", seq_len(series)), each = 24),
    value = rep(seq_len(series) * 100, each = 24))
  list(project_info = set_project_info(
    project_name = "custom-agent", path = path, combo_variables = "id",
    target_variable = "value", date_type = "month"),
    llm = structure(list(), class = "Chat"), input_data = data,
    forecast_horizon = 1, negative_forecast = TRUE, run_global_models = FALSE,
    back_test_scenarios = 1, custom_models = list(business_mean = custom_agent_model()))
}

# Complete offline forecast proposal; no authoring or provider call is performed.
custom_agent_response <- function(models = "business_mean", ...) {
  jsonlite::toJSON(utils::modifyList(list(
    models_to_run = models, external_regressors = "NULL", clean_missing_values = FALSE,
    clean_outliers = FALSE, forecast_approach = "bottoms_up", stationary = FALSE,
    feature_selection = FALSE, multistep_horizon = FALSE, seasonal_period = "NULL",
    recipes_to_run = "R1", lag_periods = "NULL", rolling_window_periods = "NULL",
    reasoning = "Use the approved fixed mean."), list(...)), auto_unbox = TRUE)
}

# Serializable offline Chat stand-in with independent histories per clone.
# The nested factory/callbacks retain only a call tracker and optional worker
# observation directory. Resolve lazy inputs before serialization so workers
# never need the caller's test helpers; no callback contacts a provider.
custom_agent_chat <- function(response, tracker = new.env(parent = emptyenv()), observation_path = NULL) {
  force(response)
  force(observation_path)
  if (is.null(tracker$calls)) tracker$calls <- 0L
  factory <- function(response, tracker, observation_path) {
    force(response)
    force(tracker)
    force(observation_path)
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
        saveRDS(list(pid = Sys.getpid(), previous_turns = length(session$turns)),
          file.path(observation_path, paste0("chat-", Sys.getpid(), ".rds")))
      tracker$calls <- tracker$calls + 1L
      session$turns <- c(session$turns, list(prompt))
      response
    }
    session
  }
  factory(response, tracker, observation_path)
}
