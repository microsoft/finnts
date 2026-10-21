# Start or resume a Finn forecast run in the background.
# Usage:
#   Rscript run_forecast.R --project=<name> [--mode=standard|iterate|update]
#       [--new=true] [--confirm=true] [--force=true] [--takeover=true] [--wait=true]
#       [--batch_size=<planned concurrent runs>] [--num_cores=<worker ceiling>]
#
# Default behavior picks up where the last unfinished run of the same mode
# left off: same run id, same Agent request id, same saved settings, and no
# new Agent version. --new=true starts a fresh run instead; for Agent runs that
# creates a new Agent version and needs --confirm=true after asking the user.
#
# Continuing an Agent version: after an iterate run finishes, review and save
# a higher agent.max_iter and/or a stricter agent.weighted_mape_goal, then
# launch --mode=iterate again (no extra flag). max_iter is the total number of
# iterations per search for the Agent version, so only the remaining
# iterations run, under the same request id, without overwriting anything.
# The result's data$continuation records the version, request id, and the
# previous and new values. A lower max_iter or only a looser goal returns
# already_complete; changed data or other settings return needs_new_version;
# different installed finnts or R returns needs_confirmation until rerun with
# --confirm=true. Stop results carry data$reason. A stopped iterate run whose
# only changes are those two settings continues with them the same way.
#
# Custom models: config.json `custom_models` lists the saved custom models
# (custom_models/<name>/<version>.rds) every run of the project uses. Standard
# runs train them with the built-in models. Agent runs enroll them in the
# Agent version; its iterate continuations and updates must enroll the same
# ones, so a changed enrollment returns needs_new_version (reason
# custom_models_changed for updates). Agent runs with custom models train
# series in parallel worker processes instead of within-series parallelism.
#
# --wait=true runs in the foreground (used by tests). Nothing here deletes
# files.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Statuses that a later launch can resume
#'
#' @return Character vector.
launch_resumable_statuses <- function() c("interrupted", "failed", "cancelled")

#' Map a launch mode to the config mode it needs
#'
#' @param mode "standard", "iterate", or "update".
#' @return "standard" or "agent".
launch_config_mode <- function(mode) if (identical(mode, "standard")) "standard" else "agent"

#' Stop a launch while another run of the project is still active
#'
#' A run recorded on another computer cannot be checked from here, so it can
#' only be taken over after the user confirms with `--takeover=true` that the
#' other computer is no longer running it. Takeover marks it interrupted so the
#' normal resume path continues it with its history intact.
#'
#' @param paths Project paths.
#' @param args Parsed script arguments.
#' @return NULL when the launch may proceed, otherwise a "busy" `finn_result`.
launch_active_guard <- function(paths, args) {
  active <- finn_active_run(paths)
  if (!is.null(active) && finn_other_host(active) && finn_bool(finn_arg(args, "takeover"))) {
    finn_update_state(finn_run_paths(paths, active$run_id), status = "interrupted")
    active <- finn_active_run(paths)
  }
  if (is.null(active)) {
    return(NULL)
  }
  other <- finn_other_host(active)
  finn_result(FALSE, "busy", paste0(
    "Run ", active$run_id, " is still ", active$effective_status,
    if (other) paste0(" on another computer (", active$host, ")") else "", "."
  ),
  list(run_id = active$run_id, mode = active$mode, host = active$host, other_host = other),
  if (other) {
    c("Ask the user to check or cancel it on that computer.",
      "Only if the user confirms that computer is no longer running it, rerun this command with --takeover=true to continue the run here; history is kept.")
  } else {
    "Check progress with run_status.R, or ask the user before cancelling it with run_cancel.R."
  })
}

#' Most recent run of a mode, regardless of status
#'
#' @param runs Runs from [finn_list_runs()].
#' @param mode Launch mode.
#' @return State list or NULL.
launch_latest_of_mode <- function(runs, mode) {
  for (s in runs) if (identical(s$mode, mode)) return(s)
  NULL
}

#' Read the config saved with a run
#'
#' @param paths Project paths.
#' @param run_paths Run paths.
#' @return Merged config list.
launch_run_config <- function(paths, run_paths) {
  finn_merge_config(finn_read_project_config(paths, file.path(run_paths$dir, "config.json")))
}

#' Start the worker for a prepared run
#'
#' In the background on Windows the worker is started with
#' `launch_worker_windows()` so the launcher returns at once; elsewhere, or if
#' that fails, a detached processx child is used.
#'
#' @param paths Project paths.
#' @param run_paths Run paths.
#' @param wait Run in the foreground and block until done.
#' @return List with `pid` and, when waiting, `exit_status`.
launch_worker <- function(paths, run_paths, wait = FALSE) {
  worker <- file.path(finn_script_dir(), "run_worker.R")
  wargs <- c(worker, paste0("--root=", paths$root), paste0("--project=", paths$name), paste0("--run_id=", run_paths$id))
  dir.create(run_paths$dir, recursive = TRUE, showWarnings = FALSE)
  if (wait) {
    status <- system2(finn_rscript(), shQuote(wargs), stdout = run_paths$log, stderr = run_paths$log)
    return(list(pid = NULL, exit_status = status))
  }
  finn_require("processx")
  pid <- if (.Platform$OS.type == "windows") launch_worker_windows(c(wargs, paste0("--log=", run_paths$log)))
  if (is.null(pid)) {
    proc <- processx::process$new(
      finn_rscript(), wargs,
      stdout = run_paths$log, stderr = "2>&1",
      cleanup = FALSE, cleanup_tree = FALSE,
      windows_detached_process = TRUE
    )
    pid <- proc$get_pid()
  }
  create_time <- if (requireNamespace("ps", quietly = TRUE)) {
    tryCatch(as.numeric(ps::ps_create_time(ps::ps_handle(as.integer(pid)))), error = function(e) NULL)
  }
  list(pid = pid, pid_create_time = create_time)
}

#' Quote one argument for a Windows command line
#'
#' Follows the CommandLineToArgvW rules so paths with spaces, quotes, or
#' trailing backslashes reach the worker unchanged.
#'
#' @param x Character vector of arguments.
#' @return Character vector of quoted arguments.
launch_win_quote <- function(x) {
  x <- gsub('(\\\\*)"', '\\1\\1\\\\"', x)
  x <- sub("(\\\\+)$", "\\1\\1", x)
  paste0('"', x, '"')
}

#' Start the worker on Windows without inheriting the caller's handles
#'
#' A processx child inherits every inheritable handle of the launcher,
#' including the pipe a coding agent reads the launcher's output from, so the
#' agent would wait for the whole run. PowerShell `Start-Process` starts the
#' worker without inherited handles, outside the caller's process tree, and
#' the worker writes its own log via `--log`.
#'
#' @param wargs Worker arguments, starting with the script path.
#' @return Worker process id, or NULL when PowerShell could not start it.
launch_worker_windows <- function(wargs) {
  ps_quote <- function(s) paste0("'", gsub("'", "''", s, fixed = TRUE), "'")
  script <- sprintf(
    "$p = Start-Process -FilePath %s -ArgumentList %s -WindowStyle Hidden -PassThru; $p.Id",
    ps_quote(finn_rscript()), ps_quote(paste(launch_win_quote(wargs), collapse = " "))
  )
  encoded <- jsonlite::base64_enc(iconv(script, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]])
  res <- tryCatch(
    processx::run("powershell.exe", c("-NoProfile", "-NonInteractive", "-EncodedCommand", encoded),
      error_on_status = FALSE, timeout = 60
    ),
    error = function(e) NULL
  )
  pid <- if (!is.null(res) && identical(res$status, 0L)) suppressWarnings(as.integer(trimws(res$stdout)))
  if (length(pid) != 1 || is.na(pid)) {
    return(NULL)
  }
  pid
}

#' Refuse a launch because other runs already use this computer's capacity
#'
#' The launch is refused, never queued, so the user stays in control of which
#' forecast runs next.
#'
#' @param claimed Result of [finn_claimed_resources()].
#' @param remaining Result of [finn_remaining_resources()].
#' @param data_gb Estimated data size in GB.
#' @return A failed `finn_result` with code "machine_busy".
launch_machine_busy <- function(claimed, remaining, data_gb) {
  n <- length(claimed$runs)
  projects <- unique(vapply(claimed$runs, function(r) as.character(r$project %||% ""), ""))
  finn_result(FALSE, "machine_busy",
    sprintf("This computer is busy with %d other Finn run(s) (%s); there are not enough free cores or memory to start another one now.",
      n, paste(projects, collapse = ", ")),
    list(
      active_runs = claimed$runs,
      remaining_cores = remaining$cpu_budget %||% remaining$cores,
      remaining_free_gb = remaining$free_gb,
      remaining_budget_gb = remaining$ram_budget_gb,
      needed_gb = round(finn_worker_gb(data_gb), 1)
    ),
    c(
      "Nothing was started. Tell the user which forecasts are already running.",
      "Check progress with run_status.R --all=true and relaunch this run after one of them finishes.",
      "Never cancel another run to make room unless the user explicitly asks for it."
    )
  )
}

#' Build a note for a launch that shares the computer with other runs
#'
#' @param claimed Result of [finn_claimed_resources()].
#' @param mode Launch mode.
#' @return Character vector of next actions (possibly empty).
launch_shared_note <- function(claimed, mode) {
  n <- length(claimed$runs)
  if (n == 0) {
    return(character())
  }
  out <- sprintf("This run shares the computer with %d other Finn run(s), so it uses fewer workers and may take longer.", n)
  if (!identical(mode, "standard")) {
    out <- c(out, "Several Agent runs at once each use GitHub Copilot requests; tell the user this adds to their premium request usage and may hit rate limits.")
  }
  out
}

#' Count large OneDrive runs and build a move suggestion
#'
#' @param paths Project paths.
#' @param n_series Number of series.
#' @return Character vector of next actions (possibly empty).
launch_onedrive_note <- function(paths, n_series) {
  if (!finn_is_onedrive(paths$root) || is.null(n_series) || n_series < 100) {
    return(character())
  }
  settings <- finn_load_settings()
  settings$large_onedrive_runs <- as.integer(settings$large_onedrive_runs %||% 0L) + 1L
  finn_save_settings(settings)
  out <- "This run has 100 or more series in a OneDrive folder. Tell the user syncing may slow down and they can pause OneDrive sync during the run."
  if (settings$large_onedrive_runs >= 3 && !isTRUE(settings$move_suggestion_dismissed)) {
    out <- c(out, "The user keeps running large forecasts on OneDrive. Offer to copy the Finn folder to a local folder they choose (setup_project.R --action=move_root), or dismiss with --action=dismiss_move_suggestion.")
  }
  out
}

#' Compare recorded finnts and R versions with the installed ones
#'
#' Missing recorded versions (runs from older skill releases) count as
#' unknown, not as a difference.
#'
#' @param recorded List with optional `finnts_version` and `r_version`.
#' @param current List from [finn_versions()].
#' @return Character vector describing each difference (possibly empty).
launch_version_changes <- function(recorded, current = finn_versions()) {
  out <- character()
  for (f in c("finnts_version", "r_version")) {
    old <- recorded[[f]]
    new <- current[[f]]
    if (!is.null(old) && !is.null(new) && !identical(as.character(old), as.character(new))) {
      out <- c(out, paste0(sub("_version$", "", f), " ", old, " -> ", new))
    }
  }
  out
}

#' Detect input changes that an update forecast should not absorb silently
#'
#' Compares the current data with the run that produced the last Agent
#' forecast, using only what that run already recorded.
#'
#' @param paths Project paths.
#' @param last_agent `last_agent` entry from project.json.
#' @param facts Current data facts.
#' @param cur_md5 Current input file MD5.
#' @param newer Whether the data has dates past the last forecast, or the
#'   user already confirmed revised history with `--force=true`.
#' @param cfg Current validated config, compared with the last Agent
#'   forecast's settings; NULL skips the settings comparison.
#' @return Character vector of drift descriptions (possibly empty).
launch_update_drift <- function(paths, last_agent, facts, cur_md5, newer, cfg = NULL) {
  prior <- if (!is.null(last_agent$run_id)) finn_read_state(finn_run_paths(paths, last_agent$run_id))
  out <- character()
  old_n <- prior$n_series
  if (!is.null(old_n) && !is.null(facts$n_series) && !identical(as.integer(old_n), as.integer(facts$n_series))) {
    out <- c(out, paste0("the number of time series changed from ", old_n, " to ", facts$n_series,
      " (series were added, dropped, or renamed)"))
  }
  if (!newer && !is.null(last_agent$data_md5) && !identical(last_agent$data_md5, cur_md5)) {
    out <- c(out, "the input file changed but has no newer dates, so earlier history may have been revised")
  }
  settings <- if (!is.null(cfg) && is.list(prior$config)) finn_settings_drift(prior$config, cfg) else character()
  if (length(settings)) {
    out <- c(out, paste0("these settings changed: ", paste(settings, collapse = ", ")))
  }
  out
}

#' Decide an iterate launch on a project whose last Agent forecast finished
#'
#' Implements the continuation contract: with the same input file and
#' forecast settings, a higher `agent.max_iter` and/or a stricter
#' `weighted_mape_goal` continue the last iterate request on the same Agent
#' version (the request id is reused, nothing is overwritten, and only the
#' remaining iterations run). `max_iter` is a total per search for the Agent
#' version, so a lower value or a looser goal alone is `already_complete`.
#' Changed data or other settings need a new Agent version, which the caller
#' must request with `--new=true --confirm=true`. A different installed
#' finnts or R needs `--confirm=true`. A newer unfinished update blocks
#' continuation until it is resumed.
#'
#' @param paths Project paths.
#' @param runs Runs from [finn_list_runs()], newest first.
#' @param last_agent `last_agent` entry from project.json.
#' @param cfg Current validated config.
#' @param cur_md5 Current input file MD5.
#' @param cur_hash Current [finn_config_hash()].
#' @param newer Whether the data has actuals past the last Agent forecast.
#' @param confirmed Whether `--confirm=true` was given.
#' @param stop_with Function building a "stop" decision from status, message,
#'   data, and next actions.
#' @return A "new" decision reusing the last request id, with `notes` and
#'   `continuation`, or a "stop" decision whose result data has `reason`.
launch_iterate_again <- function(paths, runs, last_agent, cfg, cur_md5, cur_hash, newer, confirmed, stop_with) {
  version <- last_agent$agent_version %||% "?"
  base <- list(agent_version = last_agent$agent_version, agent_run_id = last_agent$agent_run_id)
  refresh <- if (newer) "The data has newer dates. Suggest --mode=update to refresh the forecast without a full new search."
  fresh <- "To search again from scratch, ask the user to confirm a new Agent version, then rerun with --new=true --confirm=true."
  review <- "To review results instead, use export_results.R or a free-form analysis."
  stop_reason <- function(status, reason, message, actions, extra = list()) {
    stop_with(status, message, c(base, list(reason = reason), extra), actions)
  }

  ids <- vapply(runs, function(s) as.character(s$run_id %||% ""), character(1))
  agent_pos <- match(last_agent$run_id %||% "", ids)
  update <- launch_latest_of_mode(runs, "update")
  if (!is.null(update) && update$effective_status %in% launch_resumable_statuses() &&
    (is.na(agent_pos) || match(update$run_id, ids) < agent_pos)) {
    return(stop_reason("needs_new_version", "unfinished_update",
      paste0("Update run ", update$run_id, " (", update$effective_status, ") started after Agent version ", version,
        " finished and has not completed, so the search can't continue on that version."),
      c("Resume the update first: rerun with --mode=update (it picks up where it stopped).", fresh),
      list(run_id = update$run_id)))
  }

  same <- identical(last_agent$config_hash, cur_hash) && identical(last_agent$data_md5, cur_md5)
  if (!identical(last_agent$mode, "iterate")) {
    return(if (same) {
      stop_reason("already_complete", "unchanged", "The latest Agent forecast already used these settings and data.",
        c(refresh, fresh, review))
    } else {
      stop_reason("needs_new_version", "last_agent_not_iterate", paste0(
        "The latest Agent forecast (version ", version, ") was an update, so only a new Agent version can search with changed settings or data."
      ), c(refresh, fresh, review))
    })
  }
  if (!identical(last_agent$data_md5, cur_md5)) {
    return(stop_reason("needs_new_version", "data_changed",
      paste0("The input data changed since Agent version ", version, ", so continuing its search would mix two datasets."),
      c(refresh, fresh, review), list(changed = "input data")))
  }
  prior <- if (!is.null(last_agent$run_id)) finn_read_state(finn_run_paths(paths, last_agent$run_id))$config
  if (!is.list(prior)) {
    return(if (same) {
      stop_reason("already_complete", "unchanged", "The latest Agent forecast already used these settings and data.",
        c(refresh, fresh, review))
    } else {
      stop_reason("needs_new_version", "settings_changed", "Settings changed since the latest Agent forecast.",
        c(refresh, fresh, review))
    })
  }

  cont <- finn_iterate_continuation(prior, cfg)
  old_max <- format(cont$max_iter$previous)
  new_max <- format(cont$max_iter$new)
  limits <- list(max_iter = cont$max_iter, weighted_mape_goal = cont$weighted_mape_goal)
  more <- sprintf(paste(
    "To search longer on the same Agent version, show the review with a higher agent.max_iter (above %s), save it,",
    "then rerun this command; only the extra iterations run."
  ), old_max)
  stricter <- paste(
    "Or tighten agent.weighted_mape_goal: series whose best model is global and miss the new goal reopen",
    "(raise agent.max_iter too to search them longer)."
  )
  if (identical(cont$kind, "other_changes")) {
    changed <- if (length(cont$changed)) cont$changed else "forecast settings"
    return(stop_reason("needs_new_version", "settings_changed",
      paste0("Settings changed since Agent version ", version, " (", paste(changed, collapse = ", "),
        "). Only agent.max_iter and agent.weighted_mape_goal can change on the same Agent version."),
      c(refresh, fresh, review), c(list(changed = changed), limits)))
  }
  if (identical(cont$kind, "unchanged")) {
    return(stop_reason("already_complete", "unchanged", "The latest Agent forecast already used these settings and data.",
      c(refresh, more, stricter, fresh, review), limits))
  }
  if (identical(cont$kind, "lower_max_iter")) {
    return(stop_reason("already_complete", "lower_max_iter", paste0(
      "Agent version ", version, " already ran up to ", old_max, " iterations per search. A lower agent.max_iter (", new_max,
      ") can't undo iterations, so nothing new would run.",
      if (cont$goal_changed) paste0(" Keep agent.max_iter at ", old_max, " or higher for the goal change to apply.") else ""
    ), c(refresh, more, fresh, review), limits))
  }
  if (identical(cont$kind, "looser_goal_only")) {
    return(stop_reason("already_complete", "looser_goal_only", paste0(
      "A looser weighted MAPE goal (", format(cont$weighted_mape_goal$previous), " -> ", format(cont$weighted_mape_goal$new),
      ") alone runs nothing new on Agent version ", version, "."
    ), c(refresh, more, fresh, review), limits))
  }

  versions <- launch_version_changes(last_agent)
  if (length(versions) && !confirmed) {
    return(stop_reason("needs_confirmation", "software_changed", paste0(
      "Agent version ", version, " ran with different software (", paste(versions, collapse = "; "),
      "). Continuing it with the installed versions can mix results from two versions."
    ), c("Ask the user: continue the same Agent version anyway (rerun with --confirm=true), or start a new Agent version with the installed software (rerun with --new=true --confirm=true).",
      review), c(list(version_changes = versions), limits)))
  }
  goal_note <- if (cont$goal_changed) {
    paste0(" The weighted MAPE goal changes from ", format(cont$weighted_mape_goal$previous), " to ",
      format(cont$weighted_mape_goal$new), ".")
  } else {
    ""
  }
  list(action = "new", overwrite = FALSE, request_id = last_agent$request_id,
    continuation = c(list(agent_version = last_agent$agent_version, request_id = last_agent$request_id,
      from_run_id = last_agent$run_id, from_status = "completed", partial = cont$partial), limits),
    notes = c(
      if (cont$partial) {
        paste0("This continues Agent version ", version, " with a stricter weighted MAPE goal (",
          format(cont$weighted_mape_goal$previous), " -> ", format(cont$weighted_mape_goal$new), "). With agent.max_iter still ",
          old_max, ", only series whose best model is global and now miss the goal reopen; raise agent.max_iter to search longer.")
      } else {
        paste0("This continues Agent version ", version, ": it already ran up to ", old_max,
          " iterations per search, so only the iterations up to ", new_max, " run now.", goal_note)
      },
      if (length(versions)) {
        paste0("The last Agent forecast used different software (", paste(versions, collapse = "; "),
          "). Tell the user the extra iterations use the installed versions and results may shift slightly.")
      }
    ))
}

#' Rough runtime estimate for a launch
#'
#' @param mode Launch mode.
#' @param plan Parallel plan from [finn_choose_parallel()].
#' @param n_series Number of series.
#' @return List from [finn_estimate_runtime()].
launch_estimate <- function(mode, plan, n_series) {
  workers <- if (identical(plan$parallel_processing, "local_machine")) {
    plan$num_cores
  } else if (isTRUE(plan$inner_parallel)) {
    max(1, (plan$num_cores %||% 2) / 2)
  } else {
    1
  }
  finn_estimate_runtime(n_series, mode, workers)
}

#' Standard notes to pass on to the user at launch
#'
#' @param mode Launch mode.
#' @param plan Parallel plan.
#' @param n_series Number of series.
#' @return Character vector of next actions.
launch_run_notes <- function(mode, plan, n_series) {
  est <- launch_estimate(mode, plan, n_series)
  c(
    paste0("Tell the user the run may take ", est$text, " (a rough guide; data size and machine load change it)."),
    "Ask the user to keep the computer on, plugged in, and awake. Sleep, restarts, or closing the session stop the run; rerunning the same command resumes it.",
    if (!identical(mode, "standard")) {
      "Agent runs send many requests to GitHub Copilot, which can use the user's premium-request allowance. Mention this before long or repeated Agent runs."
    }
  )
}

#' Last date with actual values used for training
#'
#' Future driver rows (dates with no target) must not count as new actuals,
#' so this prefers `hist_end_date`, then the last date with a target value,
#' then the last date in the file.
#'
#' @param cfg Validated config.
#' @param facts Data facts from [finn_load_checked()].
#' @return Date string ("YYYY-MM-DD") or NULL.
launch_data_end <- function(cfg, facts) {
  end <- cfg$hist_end_date %||% facts$last_actual_date %||% facts$date_max
  if (is.null(end)) NULL else as.character(end)
}

#' Stop a new run whose mode differs from the reviewed config
#'
#' The settings review is rendered for the saved config's mode, so starting a
#' new run of the other kind would launch settings the user did not see.
#' Resumes keep their own saved settings and are never blocked.
#'
#' @param decision Result of [launch_decide()].
#' @param saved_mode Mode stored in the project config ("standard" or "agent").
#' @param mode Launch mode.
#' @return NULL when the launch may proceed, otherwise a "needs_review" `finn_result`.
launch_mode_mismatch <- function(decision, saved_mode, mode) {
  if (!identical(decision$action, "new") || identical(saved_mode, launch_config_mode(mode))) {
    return(NULL)
  }
  finn_result(FALSE, "needs_review", paste0(
    "The saved settings are for a ", if (identical(saved_mode, "agent")) "Agent" else "Standard",
    " forecast, but this launch asks for ", finn_run_type_label(mode), ". Nothing was started."
  ),
  list(saved_mode = saved_mode, mode = mode),
  c(paste0("Rerun validate_config.R with --run_type=", mode, ", show data.settings_review to the user, and save with --save=true once they agree."),
    paste0("Then rerun this command with --mode=", mode, ".")))
}

#' Decide whether to resume, refuse, or start fresh
#'
#' @param paths Project paths.
#' @param mode Launch mode.
#' @param cfg Current validated config.
#' @param facts Data facts.
#' @param args Parsed arguments.
#' @return List with `action` ("resume", "new", or "stop"), plus `prior`,
#'   `overwrite`, `notes`, and when continuing an Agent version a
#'   `request_id`, `continuation` and optionally `step_down`; a `result` when
#'   stopping (see [launch_iterate_again()]).
launch_decide <- function(paths, mode, cfg, facts, args) {
  runs <- finn_list_runs(paths)
  latest <- launch_latest_of_mode(runs, mode)
  want_new <- finn_bool(finn_arg(args, "new"))
  confirmed <- finn_bool(finn_arg(args, "confirm"))
  project <- finn_read_project(paths) %||% list()
  last_agent <- project$last_agent
  cur_hash <- finn_config_hash(cfg)
  cur_md5 <- finn_data_md5(cfg$data$file)
  data_end <- launch_data_end(cfg, facts)
  stop_with <- function(status, message, data = NULL, next_actions = character()) {
    list(action = "stop", result = finn_result(FALSE, status, message, data, next_actions))
  }

  if (!want_new && !is.null(latest) && latest$effective_status %in% launch_resumable_statuses()) {
    saved_config <- latest[["config", exact = TRUE]]
    changed <- c(
      if (!identical(if (!is.null(saved_config)) finn_config_hash(saved_config) else latest$config_hash, cur_hash)) "settings",
      if (!identical(latest$data_md5, cur_md5)) "input data"
    )
    revised <- if (identical(changed, "settings")) finn_stopped_iterate_revision(latest, cfg)
    if (length(changed) && is.null(revised) && !confirmed) {
      return(stop_with("needs_confirmation",
        paste0("Run ", latest$run_id, " stopped before finishing (", latest$effective_status, "). The ",
          paste(changed, collapse = " and "), " changed since it started."),
        list(run_id = latest$run_id, changed = changed),
        c("Ask the user: resume with the original settings and data (rerun with --confirm=true), or start fresh with the new ones (rerun with --new=true; Agent modes also need --confirm=true).")
      ))
    }
    versions <- launch_version_changes(latest)
    if (length(versions) && !confirmed) {
      return(stop_with("needs_confirmation",
        paste0("Run ", latest$run_id, " started with different software (", paste(versions, collapse = "; "),
          "). Finishing it with the installed versions can mix results from two versions."),
        list(run_id = latest$run_id, version_changes = versions, reason = "software_changed"),
        c("Ask the user: resume anyway (rerun with --confirm=true), or start fresh with the installed versions (rerun with --new=true; Agent modes also need --confirm=true).")
      ))
    }
    if (!is.null(revised)) {
      # A budget or goal revision runs as a new skill run under the stopped
      # run's request id, so the Agent version and its finished iterations
      # are kept while the revised values apply.
      return(list(action = "new", overwrite = latest$overwrite %||% FALSE, request_id = latest$request_id,
        step_down = latest$step_down,
        continuation = list(agent_version = last_agent$agent_version, request_id = latest$request_id,
          from_run_id = latest$run_id, from_status = latest$effective_status, partial = revised$partial,
          max_iter = revised$max_iter, weighted_mape_goal = revised$weighted_mape_goal),
        notes = paste0("Run ", latest$run_id, " stopped (", latest$effective_status, "). This continues its Agent search with ",
          "agent.max_iter ", format(revised$max_iter$new), " and weighted MAPE goal ", format(revised$weighted_mape_goal$new),
          "; iterations it already finished are kept.")))
    }
    return(list(action = "resume", prior = latest, overwrite = latest$overwrite %||% FALSE))
  }

  if (identical(mode, "standard")) {
    return(list(action = "new", overwrite = FALSE))
  }

  if (identical(mode, "iterate")) {
    has_version <- !is.null(last_agent$agent_run_id) || !is.null(latest) ||
      !is.null(launch_latest_of_mode(runs, "update"))
    if (!has_version) {
      return(list(action = "new", overwrite = FALSE))
    }
    if (!want_new && is.null(last_agent$agent_run_id)) {
      return(stop_with("needs_new_version",
        "An earlier Agent run exists for this project but never finished, and it cannot be resumed as-is.",
        list(run_id = latest$run_id),
        c("Run diagnose_run.R on that run. To start over, ask the user to confirm a new Agent version, then rerun with --new=true --confirm=true.")))
    }
    if (!want_new) {
      newer <- !is.null(last_agent$hist_end_date) && !is.null(data_end) && data_end > last_agent$hist_end_date
      return(launch_iterate_again(paths, runs, last_agent, cfg, cur_md5, cur_hash, newer, confirmed, stop_with))
    }
    if (!confirmed) {
      return(stop_with("needs_confirmation",
        "Starting fresh creates a new Agent version. Earlier versions and their history are kept.",
        list(agent_version = last_agent$agent_version),
        c("Confirm with the user, then rerun with --new=true --confirm=true.")))
    }
    return(list(action = "new", overwrite = TRUE))
  }

  # update
  if (is.null(last_agent) || !identical(last_agent$status, "completed")) {
    done <- Filter(function(s) identical(s$effective_status, "completed"), runs)
    standard_project <- length(done) && identical(done[[1]]$mode, "standard")
    return(stop_with("needs_agent_run",
      "Update needs a finished Agent forecast to build on, and none was found for this project.",
      NULL, if (standard_project) {
        c("This project's forecasts are standard runs. Refresh it with new actuals using a standard run: rerun validate_config.R with --run_type=standard, show the review, then launch with --mode=standard.",
          "To switch the project to the Agent instead, start with --mode=iterate after the user agrees.")
      } else {
        c("Run --mode=iterate first, or check run_status.R for an unfinished Agent run to resume.")
      }))
  }
  newer <- !is.null(last_agent$hist_end_date) && !is.null(data_end) && data_end > last_agent$hist_end_date
  if (!newer && !finn_bool(finn_arg(args, "force"))) {
    actual_end <- facts$last_actual_date %||% facts$date_max
    actual_end <- if (is.null(actual_end)) NULL else as.character(actual_end)
    pinned <- !is.null(cfg$hist_end_date) && !is.null(actual_end) && actual_end > as.character(cfg$hist_end_date)
    partial <- facts$incomplete_period
    open_end <- if (pinned && identical(partial$date, actual_end)) actual_end
    complete_end <- if (!is.null(open_end)) partial$suggested_hist_end_date else actual_end
    only_open <- !is.null(open_end) && !(is.character(complete_end) && complete_end > as.character(cfg$hist_end_date))
    return(stop_with("no_new_data",
      if (only_open) {
        paste0("The only actuals after the last Agent forecast (", last_agent$hist_end_date, ") are for ", open_end,
          ", a period that has not ended yet, and hist_end_date (", cfg$hist_end_date, ") leaves it out.")
      } else if (pinned) {
        paste0("hist_end_date is set to ", cfg$hist_end_date, ", so the update would stop at the same date as the last Agent forecast (",
          last_agent$hist_end_date, ") even though the file has actuals through ", actual_end, ".",
          if (!is.null(open_end)) paste0(" The ", open_end, " period has not ended yet; the last complete period is ", complete_end, "."))
      } else {
        paste0("The actuals end on ", data_end, ", the same as the last Agent forecast (", last_agent$hist_end_date, "). There is nothing new to update with.")
      },
      list(data_end = data_end, last_end = last_agent$hist_end_date, actual_end = actual_end, hist_end_pinned = pinned,
        incomplete_period = open_end),
      if (only_open) {
        c("Tell the user the newer period is still in progress. Usually wait until it ends and the actuals are complete; set hist_end_date to it only if the user wants to update with partial actuals.")
      } else if (pinned) {
        c(paste0("Ask the user to change hist_end_date to ", complete_end, " (or remove it): rerun validate_config.R with --run_type=update, show the review, save with --save=true, then rerun this command."))
      } else {
        c("Ask the user to add newer actuals to the input file, or rerun with --force=true if earlier values were revised.")
      }))
  }
  prior_cfg <- if (!is.null(last_agent$run_id)) finn_read_state(finn_run_paths(paths, last_agent$run_id))$config
  custom_old <- finn_custom_labels(prior_cfg)
  custom_new <- finn_custom_labels(cfg)
  if (is.list(prior_cfg) && !identical(custom_old, custom_new)) {
    return(stop_with("needs_new_version", paste0(
      "Agent update must reuse the custom models of Agent version ", last_agent$agent_version %||% "?", " (",
      finn_custom_label_text(custom_old), "), but this config enrolls ", finn_custom_label_text(custom_new), "."
    ), list(reason = "custom_models_changed", agent_version = last_agent$agent_version,
      previous = as.list(custom_old), new = as.list(custom_new)),
    c("Ask the user which they want: restore the previous custom models with custom_model.R --action=enroll or --action=unenroll, rerun validate_config.R --run_type=update, and save; or start a new Agent version that uses the current custom models with --mode=iterate --new=true --confirm=true after they agree.")))
  }
  drift <- launch_update_drift(paths, last_agent, facts, cur_md5, newer || finn_bool(finn_arg(args, "force")), cfg)
  if (length(drift) && !confirmed) {
    return(stop_with("needs_confirmation",
      paste0("Since the last Agent forecast, ", paste(drift, collapse = "; "), "."),
      list(changes = drift, agent_run_id = last_agent$agent_run_id),
      c("Confirm with the user that the change is intended, then rerun with --confirm=true (keep --force=true if it was used).",
        "If the series changed a lot, suggest a fresh Agent search instead: --mode=iterate --new=true --confirm=true.")))
  }
  versions <- launch_version_changes(last_agent)
  notes <- if (length(versions)) {
    paste0("The last Agent forecast used different software (", paste(versions, collapse = "; "),
      "). Tell the user the update uses the installed versions and results may shift slightly.")
  }
  list(action = "new", overwrite = TRUE, notes = notes)
}

finn_main(function(args) {
  machine_lock <- finn_machine_lock_acquire()
  if (is.null(machine_lock)) {
    return(finn_result(FALSE, "busy", "Another Finn launch or software change is in progress.",
      next_actions = "Wait for it to finish, then retry; nothing was started."))
  }
  on.exit(finn_machine_lock_release(machine_lock), add = TRUE)
  paths <- finn_project_from_args(args)
  cfg <- finn_read_project_config(paths)
  mode <- finn_arg(args, "mode") %||% (if (identical(cfg$mode, "agent")) "iterate" else "standard")
  if (!mode %in% c("standard", "iterate", "update")) stop("--mode must be standard, iterate, or update.", call. = FALSE)
  saved_mode <- cfg$mode %||% "standard"
  cfg$mode <- launch_config_mode(mode)

  # Hold a project launch lock so two agents cannot both pass the active-run
  # guard and start overlapping runs before either writes its state.
  lock_path <- file.path(paths$runs, "launch.lock")
  lock <- finn_lock_acquire(lock_path, timeout = 15, stale = 120)
  if (is.null(lock)) {
    return(finn_result(FALSE, "busy", "Another run is being started for this project right now.", NULL,
      c("Wait a few seconds, then check run_status.R before trying again.")))
  }
  on.exit(finn_lock_release(lock_path, lock), add = TRUE)

  guard <- launch_active_guard(paths, args)
  if (!is.null(guard)) {
    return(guard)
  }

  checked <- finn_load_checked(cfg, root = paths$root)
  if (length(checked$errors)) {
    return(finn_result(FALSE, "invalid", "The config has problems to fix before running.",
      list(errors = checked$errors, warnings = checked$warnings),
      c("Run validate_config.R and fix each error with the user.")))
  }
  cfg <- checked$config
  facts <- checked$facts

  if (!identical(mode, "standard") && identical(cfg$agent$llm$provider %||% "copilot", "copilot")) {
    copilot <- finn_copilot_status(cfg$agent$llm$command %||% "copilot")
    if (!copilot$ready) {
      return(finn_result(FALSE, "needs_setup", "Agent runs use GitHub Copilot, which is not ready on this machine.",
        copilot, c(unlist(copilot$problems), "Then rerun the same command; nothing was started.")))
    }
  }

  decision <- launch_decide(paths, mode, cfg, facts, args)
  if (identical(decision$action, "stop")) {
    return(decision$result)
  }
  mismatch <- launch_mode_mismatch(decision, saved_mode, mode)
  if (!is.null(mismatch)) {
    return(mismatch)
  }

  if (identical(decision$action, "resume")) {
    prior <- decision$prior
    run_paths <- finn_run_paths(paths, prior$run_id)
    run_cfg <- launch_run_config(paths, run_paths)
    run_cfg$parallel <- cfg$parallel %||% run_cfg$parallel
    step_down <- as.integer(prior$step_down %||% 0L)
    request_id <- prior$request_id
    resumed <- as.integer(prior$resume_count %||% 0L) + 1L
  } else {
    run_cfg <- cfg
    step_down <- as.integer(decision$step_down %||% 0L)
    request_id <- if (mode == "standard") NULL else decision$request_id %||% finn_new_request_id()
    resumed <- 0L
  }

  # The machine lock covers validation through registration, excluding software
  # changes and competing allocations. Always taken before the project lock.
  batch_size <- suppressWarnings(as.numeric(finn_arg(args, "batch_size", "1")))

  global <- run_cfg$run_global_models %||% finn_default_global_models(run_cfg$date_type)
  data_gb <- finn_data_gb(checked$data)
  claimed <- finn_claimed_resources(paths$root)
  sizing <- finn_plan_with_others(facts$n_series, data_gb, global, claimed,
    step_down = step_down, sequential = identical(run_cfg$parallel, "none"),
    batch_size = batch_size,
    num_cores = if (is.null(args$num_cores)) NULL else suppressWarnings(as.numeric(args$num_cores))
  )
  if (!sizing$fits) {
    return(launch_machine_busy(claimed, sizing$remaining, data_gb))
  }
  plan <- sizing$plan
  if (!identical(mode, "standard")) plan <- finn_custom_agent_plan(plan, run_cfg, data_gb, facts$n_series, step_down)

  if (!identical(decision$action, "resume")) {
    run_paths <- finn_run_paths(paths, finn_new_run_id(mode))
    while (dir.exists(run_paths$dir)) {
      Sys.sleep(1)
      run_paths <- finn_run_paths(paths, finn_new_run_id(mode))
    }
    dir.create(run_paths$dir, recursive = TRUE, showWarnings = FALSE)
    finn_save_project_config(paths, cfg, file.path(run_paths$dir, "config.json"))
  }

  base <- list(
    run_id = run_paths$id, mode = mode, project_name = paths$name,
    run_name = if (mode == "standard") run_paths$id else NULL,
    status = "queued", stage = "starting", host = Sys.info()[["nodename"]],
    n_series = facts$n_series, data_end = launch_data_end(run_cfg, facts),
    config = run_cfg, config_hash = finn_config_hash(run_cfg),
    data_md5 = finn_data_md5(run_cfg$data$file),
    parallel = plan, step_down = step_down, request_id = request_id,
    overwrite = isTRUE(decision$overwrite),
    agent_action = switch(mode, iterate = "iterate_forecast", update = "update_forecast", NULL),
    resume_count = resumed, error = NULL, skill_version = finn_skill_version,
    finnts_version = finn_versions()$finnts_version, r_version = finn_versions()$r_version,
    pid = NULL, pid_create_time = NULL, finished_at = NULL, cancelled_at = NULL,
    memory_error = NULL, results = NULL
  )
  if (identical(decision$action, "new")) base$created_at <- finn_now()
  do.call(finn_update_state, c(list(run_paths), base))
  finn_register_allocation(paths, run_paths, claimed)
  # The queued state now blocks other launches and records this run's claim,
  # so the locks can go before a possibly long --wait run.
  finn_machine_lock_release(machine_lock)
  finn_lock_release(lock_path, lock)

  wait <- finn_bool(finn_arg(args, "wait"))
  started <- tryCatch(launch_worker(paths, run_paths, wait = wait), error = function(e) {
    finn_update_state(run_paths, status = "failed", error = finn_redact(conditionMessage(e)), finished_at = finn_now())
    stop(e)
  })
  if (!wait) {
    finn_update_state(run_paths, pid = started$pid, pid_create_time = started$pid_create_time)
  }

  notes <- c(decision$notes, launch_shared_note(claimed, mode), launch_onedrive_note(paths, facts$n_series), finn_disk_warning(paths$root))
  if (!wait) notes <- c(notes, launch_run_notes(mode, plan, facts$n_series))
  estimate <- launch_estimate(mode, plan, facts$n_series)
  state <- finn_read_state(run_paths)
  data <- list(run_id = run_paths$id, mode = mode, resumed = identical(decision$action, "resume"),
    request_id = request_id, parallel = plan, log = run_paths$log, output = run_paths$output,
    estimated_runtime = estimate$text, warnings = c(checked$warnings, plan$warnings))
  if (!is.null(decision$continuation)) data$continuation <- decision$continuation
  if (wait) {
    data$status <- finn_effective_status(state)
    return(finn_result(identical(state$status, "completed"), state$status %||% "unknown",
      paste0("Run ", run_paths$id, " finished with status ", state$status, "."), data,
      c(if (identical(state$status, "completed")) {
        c("Summarize the results for the user; run export_results.R to give them a spreadsheet-friendly file.",
          "Answer follow-up questions with free-form analysis (see references/analysis.md).")
      } else {
        "Run diagnose_run.R to explain what went wrong."
      }, notes)))
  }
  verb <- if (data$resumed) "Resumed" else "Started"
  finn_result(TRUE, "started", paste0(verb, " ", mode, " run ", run_paths$id, " in the background."), data,
    c("Tell the user the run started and that they can keep working. Check progress with run_status.R when they ask.", notes))
})
