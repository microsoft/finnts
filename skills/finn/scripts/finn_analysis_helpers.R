# Read-only helpers for free-form Finn analysis scripts.
#
# Coding agents write their own R analysis on the fly and source this file:
#   source("<skill>/scripts/finn_analysis_helpers.R")
#   ctx <- finn_open("My Project")
#   fc  <- finn_forecast(ctx)
#
# Nothing here trains models, calls an LLM, or changes saved run history.
# Save anything you produce with finn_save_analysis(), which writes only to
# the project's analysis/ folder and never replaces existing files.

local({
  here <- Sys.getenv("FINN_SKILL_SCRIPTS", "")
  here <- if (nzchar(here)) here else NULL
  for (i in rev(seq_len(sys.nframe()))) {
    if (!is.null(here)) break
    of <- sys.frame(i)$ofile
    if (!is.null(of)) {
      here <- dirname(normalizePath(of, winslash = "/", mustWork = FALSE))
      break
    }
  }
  if (is.null(here)) here <- getwd()
  sys.source(file.path(here, "finn_common.R"), envir = globalenv())
})

#' Open a Finn project for analysis
#'
#' @param project Project name.
#' @param root Optional storage root; defaults to the saved setting.
#' @return Context list with `paths`, `project`, `config`, and `runs`.
finn_open <- function(project, root = NULL) {
  args <- list(project = project)
  if (!is.null(root)) args$root <- root
  paths <- finn_project_from_args(args)
  cfg <- tryCatch(finn_read_project_config(paths), error = function(e) NULL)
  list(paths = paths, project = finn_read_project(paths), config = cfg, runs = finn_list_runs(paths))
}

#' Pick a run from a project context
#'
#' @param ctx Context from [finn_open()].
#' @param run_id Optional run id; defaults to the newest completed run.
#' @param mode Optional mode filter ("standard", "iterate", "update").
#' @return Run state list.
finn_pick_run <- function(ctx, run_id = NULL, mode = NULL) {
  runs <- finn_list_runs(ctx$paths)
  if (!is.null(run_id)) {
    for (r in runs) if (identical(r$run_id, run_id)) return(r)
    stop("No run named ", run_id, " in this project.", call. = FALSE)
  }
  for (r in runs) {
    if (identical(r$effective_status, "completed") && (is.null(mode) || r$mode %in% mode)) {
      return(r)
    }
  }
  stop("This project has no completed run", if (!is.null(mode)) paste0(" of mode ", paste(mode, collapse = "/")), ".", call. = FALSE)
}

#' Saved forecast table for a completed run
#'
#' Reads the CSV the run saved in `output/<run_id>/all_forecasts.csv`.
#' Columns follow finnts: Combo, Model_ID, Run_Type (Back_Test or
#' Future_Forecast), Date, Forecast, Target, Best_Model, and intervals.
#'
#' @param ctx Context from [finn_open()].
#' @param run_id Optional run id.
#' @param best_only Keep only the selected model per series.
#' @return Data frame.
finn_forecast <- function(ctx, run_id = NULL, best_only = TRUE) {
  run <- finn_pick_run(ctx, run_id)
  file <- run$results$all_forecasts %||% file.path(finn_run_paths(ctx$paths, run$run_id)$output, "all_forecasts.csv")
  if (!file.exists(file)) stop("Saved forecast not found: ", file, call. = FALSE)
  df <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  if ("Date" %in% names(df)) df$Date <- as.Date(df$Date)
  if (best_only && "Best_Model" %in% names(df)) df <- df[df$Best_Model %in% "Yes", , drop = FALSE]
  df
}

#' Input data prepared exactly as runs see it
#'
#' @param ctx Context from [finn_open()].
#' @return Data frame after the skill's date and number cleanup.
finn_input <- function(ctx) {
  if (is.null(ctx$config)) stop("This project has no saved config yet.", call. = FALSE)
  raw <- finn_load_data(ctx$config$data$file, ctx$config$data$sheet)
  finn_prepare_input(raw, ctx$config)$data
}

#' History in the same shape as the forecast table
#'
#' Builds `Combo` by joining the config's combo columns with `--`, as finnts
#' does, and renames the target column to `Target`, so history can be joined
#' or plotted next to [finn_forecast()] output.
#'
#' @param ctx Context from [finn_open()].
#' @return Data frame with Combo, Date, and Target, plus the combo columns.
finn_history <- function(ctx) {
  df <- finn_input(ctx)
  vars <- ctx$config$combo_variables
  combo <- do.call(paste, c(unname(as.list(df[vars])), sep = "--"))
  out <- data.frame(Combo = combo, Date = df$Date, Target = df[[ctx$config$target_variable]], stringsAsFactors = FALSE)
  out <- cbind(out, df[setdiff(vars, names(out))])
  out[order(out$Combo, out$Date), , drop = FALSE]
}

#' finnts project_info for a run, for use with finnts accessors
#'
#' @param ctx Context from [finn_open()].
#' @param run_id Optional run id.
#' @return finnts project_info list.
finn_project_handle <- function(ctx, run_id = NULL) {
  run <- finn_pick_run(ctx, run_id)
  finn_project_info(ctx$paths, run$config %||% ctx$config)
}

#' finnts run_info for a completed standard run
#'
#' Use with finnts::get_forecast_data(), get_prepped_data(),
#' get_prepped_models(), and get_trained_models().
#'
#' @param ctx Context from [finn_open()].
#' @param run_id Optional run id.
#' @return finnts run_info list, rebuilt from the saved run log without
#'   calling set_run_info(), so nothing is written.
finn_run_handle <- function(ctx, run_id = NULL) {
  run <- finn_pick_run(ctx, run_id, mode = "standard")
  log <- as.data.frame(finnts::get_run_info(
    project_name = ctx$paths$name, run_name = run$run_id, path = ctx$paths$artifacts
  ))
  if (!nrow(log)) stop("No saved finnts run log was found for run ", run$run_id, ".", call. = FALSE)
  pick <- function(col, default) if (col %in% names(log) && !is.na(log[[col]][1])) as.character(log[[col]][1]) else default
  list(
    project_name = ctx$paths$name, run_name = run$run_id,
    created = log$created[1], storage_object = NULL,
    path = ctx$paths$artifacts,
    data_output = pick("data_output", "csv"),
    object_output = pick("object_output", "rds")
  )
}

#' Read-only finnts agent_info for a completed Agent run
#'
#' Rebuilt from the Agent's saved log row without an LLM, so it supports
#' accessors such as finnts::get_agent_forecast(), get_best_agent_run(),
#' get_eda_data(), and get_summarized_models(). Do not pass it to
#' iterate_forecast() or update_forecast(); use run_forecast.R for runs.
#'
#' @param ctx Context from [finn_open()].
#' @param run_id Optional skill run id.
#' @return agent_info-like list.
finn_agent_handle <- function(ctx, run_id = NULL) {
  run <- finn_pick_run(ctx, run_id, mode = c("iterate", "update"))
  project_info <- finn_project_info(ctx$paths, run$config %||% ctx$config)
  rows <- as.data.frame(finnts:::load_agent_runs(project_info), stringsAsFactors = FALSE)
  if (!nrow(rows)) stop("No saved Agent runs were found for this project.", call. = FALSE)
  rows[] <- lapply(rows, as.character)
  row <- if (!is.null(run$agent_run_id)) rows[rows$run_id %in% run$agent_run_id, , drop = FALSE] else rows[0, , drop = FALSE]
  if (!nrow(row)) row <- rows[order(as.integer(rows$agent_version), decreasing = TRUE)[1], , drop = FALSE]
  na_null <- function(x) if (is.na(x) || !nzchar(x)) NULL else x
  list(
    agent_version = as.integer(row$agent_version[1]), run_id = row$run_id[1],
    project_info = project_info, llm = NULL,
    forecast_horizon = as.numeric(row$forecast_horizon[1]),
    external_regressors = if (is.null(na_null(row$external_regressors[1]))) NULL else strsplit(row$external_regressors[1], ",\\s*")[[1]],
    hist_end_date = if (is.null(na_null(row$hist_end_date[1]))) NULL else as.Date(row$hist_end_date[1]),
    back_test_scenarios = suppressWarnings(as.numeric(na_null(row$back_test_scenarios[1]))),
    back_test_spacing = suppressWarnings(as.numeric(na_null(row$back_test_spacing[1]))),
    combo_cleanup_date = if (is.null(na_null(row$combo_cleanup_date[1]))) NULL else as.Date(row$combo_cleanup_date[1]),
    overwrite = FALSE
  )
}

#' Weighted MAPE and accuracy by series
#'
#' @param df Back-test rows with Combo, Forecast, and Target.
#' @return Data frame with Combo, WMAPE, and Accuracy, worst first.
finn_accuracy_by_series <- function(df) {
  df <- df[df$Run_Type %in% "Back_Test", , drop = FALSE]
  out <- do.call(rbind, lapply(split(df, df$Combo), function(g) {
    w <- finn_wmape(g$Forecast, g$Target)
    data.frame(Combo = g$Combo[1], WMAPE = w, Accuracy = 1 - w, stringsAsFactors = FALSE)
  }))
  if (is.null(out)) {
    return(data.frame(Combo = character(), WMAPE = numeric(), Accuracy = numeric()))
  }
  out <- out[order(-out$WMAPE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Compare two runs' forecasts and accuracy by series
#'
#' Answers "what changed since last month?" for two completed runs of the
#' same project. Future forecasts are summed per series; back-test accuracy
#' uses [finn_accuracy_by_series()]. Series present in only one run are kept
#' with NA on the other side.
#'
#' @param ctx Context from [finn_open()].
#' @param run_a Older (baseline) run id.
#' @param run_b Newer run id.
#' @param forecasts Optional list of two forecast tables (`a`, `b`) to use
#'   instead of reading saved runs; mainly for tests.
#' @return Data frame with Combo, Forecast_A, Forecast_B, Forecast_Change,
#'   Forecast_Change_Pct, Accuracy_A, Accuracy_B, and Accuracy_Change,
#'   largest absolute forecast change first.
finn_compare_runs <- function(ctx, run_a, run_b, forecasts = NULL) {
  fa <- forecasts$a %||% finn_forecast(ctx, run_a)
  fb <- forecasts$b %||% finn_forecast(ctx, run_b)
  total <- function(df, col) {
    df <- df[df$Run_Type %in% "Future_Forecast", , drop = FALSE]
    if (!nrow(df)) {
      return(stats::setNames(data.frame(character(), numeric()), c("Combo", col)))
    }
    out <- stats::aggregate(df$Forecast, by = list(Combo = df$Combo), FUN = sum, na.rm = TRUE)
    stats::setNames(out, c("Combo", col))
  }
  acc <- function(df, col) {
    out <- finn_accuracy_by_series(df)[, c("Combo", "Accuracy")]
    stats::setNames(out, c("Combo", col))
  }
  out <- Reduce(
    function(x, y) merge(x, y, by = "Combo", all = TRUE),
    list(total(fa, "Forecast_A"), total(fb, "Forecast_B"), acc(fa, "Accuracy_A"), acc(fb, "Accuracy_B"))
  )
  out$Forecast_Change <- out$Forecast_B - out$Forecast_A
  out$Forecast_Change_Pct <- ifelse(is.na(out$Forecast_A) | out$Forecast_A == 0, NA_real_, out$Forecast_Change / abs(out$Forecast_A))
  out$Accuracy_Change <- out$Accuracy_B - out$Accuracy_A
  out <- out[order(-abs(out$Forecast_Change), na.last = TRUE), c(
    "Combo", "Forecast_A", "Forecast_B", "Forecast_Change", "Forecast_Change_Pct",
    "Accuracy_A", "Accuracy_B", "Accuracy_Change"
  ), drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Save an analysis result into the project's analysis/ folder
#'
#' Data frames become CSV, ggplot objects become PNG, and character vectors
#' become text. Existing files are kept; a timestamped name is used instead.
#'
#' @param x Object to save.
#' @param name File name without extension.
#' @param ctx Context from [finn_open()].
#' @return Path written.
finn_save_analysis <- function(x, name, ctx) {
  dir.create(ctx$paths$analysis, recursive = TRUE, showWarnings = FALSE)
  ext <- if (inherits(x, "ggplot")) "png" else if (is.data.frame(x)) "csv" else "txt"
  path <- file.path(ctx$paths$analysis, paste0(finn_sanitize_name(name), ".", ext))
  if (file.exists(path)) path <- file.path(ctx$paths$analysis, paste0(finn_sanitize_name(name), "-", finn_stamp(), ".", ext))
  if (identical(ext, "png")) {
    ggplot2::ggsave(path, x, width = 10, height = 6, dpi = 120)
  } else if (identical(ext, "csv")) {
    utils::write.csv(x, path, row.names = FALSE, na = "")
  } else {
    writeLines(as.character(x), path)
  }
  path
}
