# Background worker for one Finn run. Started by run_forecast.R; not meant to
# be called by hand.
# Usage:
#   Rscript run_worker.R --root=<root> --project=<name> --run_id=<id> [--log=<file>]
#
# With --log the worker appends its own output and messages to that file. The
# Windows launcher uses this because it starts the worker without inherited
# handles.
#
# Reads runs/<id>/state.json, runs finnts, keeps the state file current, and
# writes results to output/<id>/. finnts skips work it already saved, so a
# resumed run continues where it stopped. Nothing here deletes files.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Send the worker's output and messages to a log file
#'
#' Used when the worker was started without redirected standard streams.
#'
#' @param log Path to the log file, or NULL to leave output unchanged.
#' @return Invisible NULL.
worker_redirect_log <- function(log) {
  if (is.null(log) || !nzchar(log)) {
    return(invisible(NULL))
  }
  dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
  con <- file(log, open = "at")
  sink(con)
  sink(con, type = "message")
  invisible(NULL)
}

#' Whether an error message looks like running out of memory
#'
#' @param msg Error message.
#' @return Logical.
worker_is_memory_error <- function(msg) {
  grepl("cannot allocate|vector of size|out of memory|worker.*(died|terminated)|unserialize|connection to the worker|Error in serialize",
    msg,
    ignore.case = TRUE
  )
}

#' Convert NULL-or-value config entries into finnts arguments
#'
#' JSON stores single values as scalars and missing values as NULL; finnts
#' wants NULL for "use the default".
#'
#' @param x Value.
#' @return Value or NULL.
worker_opt <- function(x) {
  if (is.null(x) || length(x) == 0) NULL else unlist(x)
}

#' Coerce an optional whole-number setting
#'
#' finnts argument checks require class numeric, so whole numbers are
#' returned as doubles.
#'
#' @param x Value.
#' @return Whole-number double or NULL.
worker_int <- function(x) {
  x <- worker_opt(x)
  if (is.null(x)) NULL else round(as.numeric(x))
}

#' Write a data frame to CSV without replacing an existing file
#'
#' @param df Data frame.
#' @param path Target path.
#' @return Path written.
worker_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) {
    base <- tools::file_path_sans_ext(path)
    path <- paste0(base, "-", finn_stamp(), ".csv")
  }
  utils::write.csv(as.data.frame(df), path, row.names = FALSE, na = "")
  path
}

#' Save forecast outputs and summarize back-test accuracy
#'
#' Writes `future_forecast.csv` (selected model, future dates only) and
#' `all_forecasts.csv` (everything finnts returned).
#'
#' @param fcst Forecast data frame.
#' @param out_dir Output folder.
#' @return List with written file paths and overall back-test WMAPE.
worker_save_outputs <- function(fcst, out_dir) {
  fcst <- as.data.frame(fcst)
  best <- if ("Best_Model" %in% names(fcst)) fcst[fcst$Best_Model %in% "Yes", , drop = FALSE] else fcst
  future <- if ("Run_Type" %in% names(best)) best[best$Run_Type %in% "Future_Forecast", , drop = FALSE] else best
  back <- if ("Run_Type" %in% names(best)) best[best$Run_Type %in% "Back_Test", , drop = FALSE] else best[0, , drop = FALSE]
  wmape <- if (nrow(back) && all(c("Forecast", "Target") %in% names(back))) finn_wmape(back$Forecast, back$Target) else NA_real_
  list(
    future_forecast = worker_write_csv(future, file.path(out_dir, "future_forecast.csv")),
    all_forecasts = worker_write_csv(fcst, file.path(out_dir, "all_forecasts.csv")),
    back_test_wmape = if (is.na(wmape)) NULL else round(wmape, 4),
    future_rows = nrow(future)
  )
}

#' Run the standard finnts pipeline stage by stage
#'
#' @param paths Project paths.
#' @param run_paths Run paths.
#' @param state Run state.
#' @param data Prepared input data.
#' @return Forecast data frame.
worker_standard <- function(paths, run_paths, state, data) {
  cfg <- state$config
  plan <- state$parallel
  pp <- worker_opt(plan$parallel_processing)
  inner <- isTRUE(plan$inner_parallel)
  cores <- worker_int(plan$num_cores)
  seed <- worker_int(cfg$seed) %||% 123
  done <- unlist(state$stages_done %||% list())

  run_info <- finnts::set_run_info(
    project_name = paths$name, run_name = run_paths$id,
    path = paths$artifacts, add_unique_id = FALSE
  )

  stage <- function(name, expr) {
    finn_update_state(run_paths, stage = name)
    finn_log("Stage: ", name)
    force(expr)
    done <<- unique(c(done, name))
    finn_update_state(run_paths, stages_done = as.list(done))
  }

  stage("prep_data", finnts::prep_data(
    run_info = run_info,
    input_data = data,
    combo_variables = unlist(cfg$combo_variables),
    target_variable = cfg$target_variable,
    date_type = cfg$date_type,
    forecast_horizon = as.numeric(cfg$forecast_horizon),
    external_regressors = worker_opt(cfg$external_regressors),
    hist_start_date = if (is.null(cfg$hist_start_date)) NULL else as.Date(cfg$hist_start_date),
    hist_end_date = if (is.null(cfg$hist_end_date)) NULL else as.Date(cfg$hist_end_date),
    combo_cleanup_date = if (is.null(cfg$combo_cleanup_date)) NULL else as.Date(cfg$combo_cleanup_date),
    fiscal_year_start = as.numeric(cfg$fiscal_year_start %||% 1L),
    clean_missing_values = isTRUE(cfg$clean_missing_values),
    clean_outliers = isTRUE(cfg$clean_outliers),
    forecast_approach = cfg$forecast_approach %||% "bottoms_up",
    parallel_processing = pp,
    num_cores = cores,
    fourier_periods = worker_opt(cfg$fourier_periods),
    lag_periods = worker_opt(cfg$lag_periods),
    rolling_window_periods = worker_opt(cfg$rolling_window_periods),
    recipes_to_run = worker_opt(cfg$recipes_to_run)
  ))

  stage("prep_models", finnts::prep_models(
    run_info = run_info,
    back_test_scenarios = worker_int(cfg$back_test_scenarios),
    back_test_spacing = worker_int(cfg$back_test_spacing),
    models_to_run = worker_opt(cfg$models_to_run),
    models_not_to_run = worker_opt(cfg$models_not_to_run),
    run_ensemble_models = worker_opt(cfg$run_ensemble_models),
    pca = worker_opt(cfg$pca),
    num_hyperparameters = as.numeric(cfg$num_hyperparameters %||% 10L),
    seed = seed
  ))

  stage("train_models", finnts::train_models(
    run_info = run_info,
    run_global_models = worker_opt(cfg$run_global_models),
    run_local_models = finn_bool(cfg$run_local_models, TRUE),
    global_model_recipes = unlist(cfg$global_model_recipes %||% "R1"),
    feature_selection = isTRUE(cfg$feature_selection),
    negative_forecast = isTRUE(cfg$negative_forecast),
    parallel_processing = pp,
    inner_parallel = inner,
    num_cores = cores,
    seed = seed
  ))

  stage("ensemble_models", finnts::ensemble_models(
    run_info = run_info,
    parallel_processing = pp,
    inner_parallel = inner,
    num_cores = cores,
    seed = seed
  ))

  stage("final_models", finnts::final_models(
    run_info = run_info,
    average_models = finn_bool(cfg$average_models, TRUE),
    max_model_average = as.numeric(cfg$max_model_average %||% 3L),
    parallel_processing = pp,
    inner_parallel = inner,
    num_cores = cores
  ))

  finn_update_state(run_paths, stage = "collect_results")
  finnts::get_forecast_data(run_info = run_info)
}

#' Run an Agent iterate or update request
#'
#' Uses the internal `set_agent_info_custom()` with the run's saved
#' request id, so a restart reuses the same Agent run and version.
#'
#' @param paths Project paths.
#' @param run_paths Run paths.
#' @param state Run state.
#' @param data Prepared input data.
#' @return Forecast data frame.
worker_agent <- function(paths, run_paths, state, data) {
  cfg <- state$config
  plan <- state$parallel
  seed <- worker_int(cfg$seed) %||% 123
  agent_cfg <- cfg$agent %||% list()

  finn_update_state(run_paths, stage = "agent_setup")
  project_info <- finn_project_info(paths, cfg)
  llm <- finn_build_llm(agent_cfg$llm %||% list())

  agent_info <- finnts:::set_agent_info_custom(
    project_info = project_info,
    llm = llm,
    input_data = data,
    forecast_horizon = as.numeric(cfg$forecast_horizon),
    external_regressors = worker_opt(cfg$external_regressors),
    hist_end_date = if (is.null(cfg$hist_end_date)) NULL else as.Date(cfg$hist_end_date),
    hist_start_date = if (is.null(cfg$hist_start_date)) NULL else as.Date(cfg$hist_start_date),
    back_test_scenarios = worker_int(cfg$back_test_scenarios),
    back_test_spacing = worker_int(cfg$back_test_spacing),
    combo_cleanup_date = if (is.null(cfg$combo_cleanup_date)) NULL else as.Date(cfg$combo_cleanup_date),
    allow_hierarchical_forecast = isTRUE(agent_cfg$allow_hierarchical_forecast),
    negative_forecast = isTRUE(cfg$negative_forecast),
    run_global_models = worker_opt(cfg$run_global_models),
    run_local_models = finn_bool(cfg$run_local_models, TRUE),
    overwrite = isTRUE(state$overwrite),
    request_id = state$request_id,
    agent_action = state$agent_action
  )
  finn_update_state(run_paths,
    agent_run_id = agent_info$run_id, agent_version = agent_info$agent_version,
    stage = state$agent_action
  )

  common <- list(
    agent_info = agent_info,
    max_iter = as.numeric(agent_cfg$max_iter %||% 3L),
    weighted_mape_goal = as.numeric(agent_cfg$weighted_mape_goal %||%
      if (identical(state$agent_action, "update_forecast")) 0.1 else 0.03),
    parallel_processing = worker_opt(plan$parallel_processing),
    inner_parallel = isTRUE(plan$inner_parallel),
    num_cores = worker_int(plan$num_cores),
    seed = seed
  )
  if (identical(state$agent_action, "update_forecast")) {
    common$allow_iterate_forecast <- isTRUE(agent_cfg$allow_iterate_forecast)
    do.call(finnts::update_forecast, common)
  } else {
    do.call(finnts::iterate_forecast, common)
  }

  finn_update_state(run_paths, stage = "collect_results")
  list(forecast = finnts::get_agent_forecast(agent_info = agent_info), agent_info = agent_info)
}

#' Record the latest finished Agent version in project.json
#'
#' @param paths Project paths.
#' @param state Run state.
#' @param agent_info Agent info.
#' @return Invisible project list.
worker_record_agent <- function(paths, state, agent_info) {
  project <- finn_read_project(paths) %||% list()
  project$last_agent <- list(
    status = "completed",
    run_id = state$run_id,
    mode = state$mode,
    request_id = state$request_id,
    agent_run_id = agent_info$run_id,
    agent_version = agent_info$agent_version,
    completed_at = finn_now(),
    hist_end_date = state$config$hist_end_date %||% state$data_end,
    config_hash = state$config_hash,
    data_md5 = state$data_md5,
    finnts_version = state$finnts_version %||% finn_versions()$finnts_version,
    r_version = state$r_version %||% finn_versions()$r_version
  )
  finn_write_project(paths, project)
  invisible(project)
}

finn_main(function(args) {
  paths <- finn_project_from_args(args)
  run_id <- finn_arg(args, "run_id")
  if (is.null(run_id)) stop("--run_id is required.", call. = FALSE)
  run_paths <- finn_run_paths(paths, run_id)
  worker_redirect_log(finn_arg(args, "log"))
  state <- finn_read_state(run_paths)
  if (is.null(state)) stop("No state for run ", run_id, ".", call. = FALSE)

  if (identical(state$status, "cancelled")) {
    return(finn_result(FALSE, "cancelled", "Run was cancelled before it started."))
  }
  if (is.null(state$pid)) {
    h <- if (requireNamespace("ps", quietly = TRUE)) ps::ps_handle()
    finn_update_state(run_paths,
      pid = Sys.getpid(),
      pid_create_time = if (is.null(h)) NULL else as.numeric(ps::ps_create_time(h))
    )
  }
  state <- finn_update_state(run_paths, status = "running", started_at = finn_now(), error = NULL)
  finn_log("Finn run ", run_id, " (", state$mode, ") started.")

  outcome <- tryCatch(
    {
      suppressPackageStartupMessages(library(finnts))
      cfg <- state$config
      raw <- finn_load_data(finn_resolve_data_file(paths, cfg$data$file), cfg$data$sheet)
      data <- finn_prepare_input(raw, cfg)$data
      if (identical(state$mode, "standard")) {
        list(forecast = worker_standard(paths, run_paths, state, data))
      } else {
        worker_agent(paths, run_paths, state, data)
      }
    },
    error = function(e) e
  )

  latest <- finn_read_state(run_paths)
  if (identical(latest$status, "cancelled")) {
    return(finn_result(FALSE, "cancelled", "Run was cancelled."))
  }

  if (inherits(outcome, "error")) {
    msg <- finn_redact(conditionMessage(outcome))
    finn_log("Run failed: ", msg)
    memory <- worker_is_memory_error(msg)
    finn_update_state(run_paths,
      status = "failed", finished_at = finn_now(),
      error = msg, memory_error = memory,
      step_down = as.integer(latest$step_down %||% 0L) + if (memory) 1L else 0L
    )
    return(finn_result(FALSE, "failed", msg))
  }

  results <- worker_save_outputs(outcome$forecast, run_paths$output)
  if (!is.null(outcome$agent_info)) worker_record_agent(paths, latest, outcome$agent_info)
  finn_update_state(run_paths,
    status = "completed", stage = "done", finished_at = finn_now(),
    results = results
  )
  finn_log("Run completed. Outputs in ", run_paths$output)
  finn_result(TRUE, "completed", "Run completed.", results)
})
