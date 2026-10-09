# Show the status of Finn runs in a project.
# Usage:
#   Rscript run_status.R --project=<name> [--run_id=<id>] [--log_lines=20]
#   Rscript run_status.R --all=true
#
# With --run_id: stage, percent complete, elapsed time, recent log lines.
# Without: the active run (if any) plus a short list of recent runs.
# With --all: every queued or running run across all projects.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Friendly labels for run stages
#'
#' @param stage Stage key.
#' @return Label string.
status_stage_label <- function(stage) {
  labels <- c(
    starting = "Starting up",
    prep_data = "Preparing data",
    prep_models = "Setting up back tests and models",
    train_models = "Training models",
    ensemble_models = "Training ensemble models",
    final_models = "Selecting the best models",
    agent_setup = "Setting up the Agent",
    iterate_forecast = "Agent searching for the best forecast",
    update_forecast = "Agent updating the forecast with new data",
    collect_results = "Saving results",
    done = "Done"
  )
  if (is.null(stage)) {
    return("Unknown")
  }
  if (stage %in% names(labels)) labels[[stage]] else stage
}

#' Minutes between two ISO timestamps (or until now)
#'
#' @param start Start timestamp.
#' @param end Optional end timestamp.
#' @return Numeric minutes, or NULL.
status_minutes <- function(start, end = NULL) {
  parse <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S%z")
  if (is.null(start)) {
    return(NULL)
  }
  s <- parse(start)
  e <- if (is.null(end)) Sys.time() else parse(end)
  if (is.na(s) || is.na(e)) {
    return(NULL)
  }
  round(as.numeric(difftime(e, s, units = "mins")), 1)
}

#' Minutes since a running run last wrote to its log or state file
#'
#' finnts can go quiet for a long time inside one model, so this is only a
#' hint: a run is flagged when nothing changed for 60 minutes (standard) or
#' 120 minutes (Agent runs, which wait on the LLM).
#'
#' @param state Run state list.
#' @param paths Project paths.
#' @param status Effective status.
#' @return Idle minutes when the run looks stalled, otherwise NULL.
status_stall_minutes <- function(state, paths, status) {
  if (!identical(status, "running") || finn_other_host(state) || is.null(state$run_id)) {
    return(NULL)
  }
  rp <- finn_run_paths(paths, state$run_id)
  files <- c(rp$log, rp$state)
  files <- files[file.exists(files)]
  if (!length(files)) {
    return(NULL)
  }
  idle <- as.numeric(difftime(Sys.time(), max(file.mtime(files)), units = "mins"))
  limit <- if (identical(state$mode, "standard")) 60 else 120
  if (is.na(idle) || idle < limit) NULL else round(idle)
}

#' Compact summary of one run
#'
#' @param state Run state with `effective_status`.
#' @param paths Project paths.
#' @return List suitable for JSON output.
status_summary <- function(state, paths) {
  eff <- state$effective_status %||% finn_effective_status(state)
  view <- state
  view$status <- eff
  progress <- tryCatch(
    if (identical(state$mode, "standard")) {
      finn_standard_progress(view, paths$artifacts)
    } else {
      finn_agent_progress(view, paths$artifacts)
    },
    error = function(e) list(percent = NULL, detail = NULL)
  )
  if (eff %in% c("failed", "cancelled", "interrupted") && !is.null(progress$percent)) {
    progress$percent <- min(progress$percent, 99)
  }
  same_host <- !finn_other_host(state)
  list(
    run_id = state$run_id,
    mode = state$mode,
    status = eff,
    stage = state$stage,
    stage_label = status_stage_label(state$stage),
    percent_complete = progress$percent,
    detail = progress$detail,
    n_series = state$n_series,
    created_at = state$created_at,
    started_at = state$started_at,
    finished_at = state$finished_at,
    elapsed_minutes = status_minutes(state$started_at %||% state$created_at, state$finished_at),
    resume_count = state$resume_count,
    parallel = state$parallel$reason,
    agent_version = state$agent_version,
    error = state$error,
    results = state$results,
    host = state$host,
    other_host = !same_host,
    stalled_minutes = status_stall_minutes(state, paths, eff),
    liveness_unknown = eff %in% c("queued", "running") && same_host && !is.null(state$pid) &&
      is.na(finn_pid_alive(state$pid, state$pid_create_time))
  )
}

#' Next steps to suggest for a run status
#'
#' @param s Run summary.
#' @param project Project name.
#' @return Character vector.
status_next <- function(s, project) {
  arg <- sprintf("--project=\"%s\" --run_id=%s", project, s$run_id)
  if (isTRUE(s$other_host) && s$status %in% c("queued", "running")) {
    return(c(
      sprintf("This run was started on another computer (%s); its status here comes from synced files and may lag.", s$host %||% "unknown"),
      "Check or cancel it from that computer. Do not resume it here while it may still be running there.",
      "If the user confirms that computer stopped or is gone, run_forecast.R with the same mode and --takeover=true continues the run here."
    ))
  }
  extra <- c(
    if (!is.null(s$stalled_minutes)) {
      sprintf("Nothing has been written for %s minutes. Long model steps can be quiet, but it may be stuck: run diagnose_run.R %s and check the log before suggesting a cancel.", s$stalled_minutes, arg)
    },
    if (isTRUE(s$liveness_unknown)) "The ps package is missing, so Finn cannot confirm the run's process is alive. Install it with install_finnts.R or check the log's last update time."
  )
  steps <- switch(s$status,
    queued = ,
    running = c(
      "Check again in a few minutes with run_status.R.",
      paste("To stop it, confirm with the user, then run run_cancel.R", arg, "--confirm=true.")
    ),
    completed = c(
      "Summarize results from output/<run_id>/ for the user.",
      "Answer follow-up questions with free-form analysis (see references/analysis.md)."
    ),
    failed = ,
    interrupted = c(
      paste("Run diagnose_run.R", arg, "to find the cause."),
      sprintf("Resume with run_forecast.R --project=\"%s\" --mode=%s (picks up where it stopped).", project, s$mode)
    ),
    cancelled = sprintf("Resume later with run_forecast.R --project=\"%s\" --mode=%s.", project, s$mode),
    unreadable = c(
      "This run's state file could not be read. OneDrive may still be syncing it; wait a minute and check again.",
      paste("If it stays unreadable, run diagnose_run.R", arg, "and look for a state.json.corrupt-* copy in the run folder.")
    ),
    character()
  )
  c(extra, steps)
}

#' Warning when OneDrive conflict copies of Finn files exist
#'
#' @param paths Project paths.
#' @return Character vector with one warning, or empty.
status_conflict_note <- function(paths) {
  found <- tryCatch(finn_conflict_copies(paths), error = function(e) character())
  if (!length(found)) {
    return(character())
  }
  paste0(
    "OneDrive kept conflict copies of Finn files (", paste(basename(found), collapse = ", "),
    "). This happens when two computers edit the project at once. Ask the user which computer is the main one; Finn only reads the original file names."
  )
}

#' Status of active runs across every project
#'
#' @param args Parsed arguments (uses `--root`).
#' @return finn_result list.
status_all <- function(args) {
  root <- finn_resolve_root(args)
  if (is.null(root)) {
    return(finn_result(FALSE, "needs_setup", "No Finn folder is set.", NULL,
      "Run setup_project.R --action=status and confirm a location with the user."))
  }
  active <- finn_all_active_runs(root)
  summaries <- lapply(active, function(s) {
    out <- status_summary(s, finn_project_paths(root, s$project))
    out$project <- s$project
    out
  })
  msg <- if (length(summaries)) {
    paste0(length(summaries), " run(s) active: ", paste(vapply(summaries, function(s) {
      sprintf("%s/%s %s (%s%%)", s$project, s$run_id, s$status, s$percent_complete %||% "?")
    }, character(1)), collapse = "; "), ".")
  } else {
    "No runs are active in any project."
  }
  finn_result(TRUE, if (length(summaries)) "running" else "idle", msg,
    list(root = root, active = summaries),
    if (length(summaries)) "Use run_status.R --project=<name> --run_id=<id> for details on one run.")
}

finn_main(function(args) {
  if (finn_bool(finn_arg(args, "all"))) {
    return(status_all(args))
  }
  paths <- finn_project_from_args(args)
  run_id <- finn_arg(args, "run_id")
  n_lines <- as.integer(finn_arg(args, "log_lines", "20"))
  conflicts <- status_conflict_note(paths)

  if (!is.null(run_id)) {
    run_paths <- finn_run_paths(paths, run_id)
    state <- tryCatch(finn_read_state(run_paths), error = function(e) {
      list(run_id = run_id, status = "unreadable", effective_status = "unreadable", error = conditionMessage(e))
    })
    if (is.null(state)) {
      return(finn_result(FALSE, "not_found", paste0("No run named ", run_id, " in this project."),
        list(runs = vapply(finn_list_runs(paths), function(s) s$run_id, character(1))),
        "Run run_status.R without --run_id to list runs."
      ))
    }
    state$run_id <- state$run_id %||% run_id
    s <- status_summary(state, paths)
    s$log_tail <- finn_tail(run_paths$log, n_lines)
    s$log_file <- run_paths$log
    msg <- sprintf(
      "%s: %s%s - %s.", s$run_id, s$status,
      if (is.null(s$percent_complete)) "" else sprintf(" (%s%%)", s$percent_complete),
      s$stage_label
    )
    return(finn_result(TRUE, s$status, msg, s, c(conflicts, status_next(s, paths$name))))
  }

  runs <- finn_list_runs(paths)
  if (length(runs) == 0) {
    return(finn_result(TRUE, "no_runs", "This project has no runs yet.", list(project = paths$name),
      "Validate the config, then start a run with run_forecast.R."
    ))
  }
  summaries <- lapply(utils::head(runs, 10), status_summary, paths = paths)
  active <- Filter(function(s) s$status %in% c("queued", "running"), summaries)
  latest <- summaries[[1]]
  focus <- if (length(active)) active[[1]] else latest
  msg <- if (length(active)) {
    sprintf("Run %s is %s (%s%%) - %s.", focus$run_id, focus$status, focus$percent_complete %||% "?", focus$stage_label)
  } else {
    sprintf("No run is active. Latest run %s is %s.", latest$run_id, latest$status)
  }
  finn_result(TRUE, if (length(active)) "running" else "idle", msg,
    list(project = paths$name, active = if (length(active)) focus, runs = summaries),
    c(conflicts, status_next(focus, paths$name))
  )
})
