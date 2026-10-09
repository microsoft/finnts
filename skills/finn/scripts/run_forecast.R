# Start or resume a Finn forecast run in the background.
# Usage:
#   Rscript run_forecast.R --project=<name> [--mode=standard|iterate|update]
#       [--new=true] [--confirm=true] [--force=true] [--takeover=true] [--wait=true]
#
# Default behavior picks up where the last unfinished run of the same mode
# left off: same run id, same Agent request id, same saved settings, and no
# new Agent version. --new=true starts a fresh run instead; for Agent runs that
# creates a new Agent version and needs --confirm=true after asking the user.
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

#' Fingerprint of the settings that change forecast results
#'
#' Excludes fields that only affect speed or bookkeeping, so changing the
#' parallel setting or LLM model does not block a resume.
#'
#' @param cfg Merged config.
#' @return Hash string.
launch_config_hash <- function(cfg) {
  keep <- cfg
  keep$parallel <- NULL
  keep$project_name <- NULL
  keep$data$file <- basename(keep$data$file %||% "")
  keep$agent$llm <- NULL
  finn_hash(finn_to_json(keep))
}

#' MD5 of the input data file
#'
#' @param file Data file path.
#' @return Hash string or NA.
launch_data_md5 <- function(file) {
  tryCatch(unname(tools::md5sum(file)), error = function(e) NA_character_)
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
      remaining_cores = remaining$cores,
      remaining_free_gb = remaining$free_gb,
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
#' @return Character vector of drift descriptions (possibly empty).
launch_update_drift <- function(paths, last_agent, facts, cur_md5, newer) {
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
  out
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

#' Decide whether to resume, refuse, or start fresh
#'
#' @param paths Project paths.
#' @param mode Launch mode.
#' @param cfg Current validated config.
#' @param facts Data facts.
#' @param args Parsed arguments.
#' @return List with `action` ("resume", "new", or "stop"), plus `prior`,
#'   `overwrite`, `notes`, and a `result` when stopping.
launch_decide <- function(paths, mode, cfg, facts, args) {
  runs <- finn_list_runs(paths)
  latest <- launch_latest_of_mode(runs, mode)
  want_new <- finn_bool(finn_arg(args, "new"))
  confirmed <- finn_bool(finn_arg(args, "confirm"))
  project <- finn_read_project(paths) %||% list()
  last_agent <- project$last_agent
  cur_hash <- launch_config_hash(cfg)
  cur_md5 <- launch_data_md5(cfg$data$file)
  data_end <- launch_data_end(cfg, facts)
  stop_with <- function(status, message, data = NULL, next_actions = character()) {
    list(action = "stop", result = finn_result(FALSE, status, message, data, next_actions))
  }

  if (!want_new && !is.null(latest) && latest$effective_status %in% launch_resumable_statuses()) {
    changed <- c(
      if (!identical(latest$config_hash, cur_hash)) "settings",
      if (!identical(latest$data_md5, cur_md5)) "input data"
    )
    if (length(changed) && !confirmed) {
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
        list(run_id = latest$run_id, version_changes = versions),
        c("Ask the user: resume anyway (rerun with --confirm=true), or start fresh with the installed versions (rerun with --new=true; Agent modes also need --confirm=true).")
      ))
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
      same <- identical(last_agent$config_hash, cur_hash) && identical(last_agent$data_md5, cur_md5)
      newer <- !is.null(last_agent$hist_end_date) && !is.null(data_end) && data_end > last_agent$hist_end_date
      msg <- if (same) "The latest Agent forecast already used these settings and data." else "Settings or data changed since the latest Agent forecast."
      acts <- c(
        if (newer) "The data has newer dates. Suggest --mode=update to refresh the forecast without a full new search.",
        "To search again from scratch, ask the user to confirm a new Agent version, then rerun with --new=true --confirm=true.",
        "To review results instead, use export_results.R or a free-form analysis."
      )
      return(stop_with(if (same) "already_complete" else "needs_new_version", msg,
        list(agent_version = last_agent$agent_version, agent_run_id = last_agent$agent_run_id), acts))
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
    return(stop_with("needs_agent_run",
      "Update needs a finished Agent forecast to build on, and none was found for this project.",
      NULL, c("Run --mode=iterate first, or check run_status.R for an unfinished Agent run to resume.")))
  }
  newer <- !is.null(last_agent$hist_end_date) && !is.null(data_end) && data_end > last_agent$hist_end_date
  if (!newer && !finn_bool(finn_arg(args, "force"))) {
    return(stop_with("no_new_data",
      paste0("The actuals end on ", data_end, ", the same as the last Agent forecast (", last_agent$hist_end_date, "). There is nothing new to update with."),
      list(data_end = data_end, last_end = last_agent$hist_end_date),
      c("Ask the user to add newer actuals to the input file, or rerun with --force=true if earlier values were revised.")))
  }
  drift <- launch_update_drift(paths, last_agent, facts, cur_md5, newer || finn_bool(finn_arg(args, "force")))
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
  paths <- finn_project_from_args(args)
  cfg <- finn_read_project_config(paths)
  mode <- finn_arg(args, "mode") %||% (if (identical(cfg$mode, "agent")) "iterate" else "standard")
  if (!mode %in% c("standard", "iterate", "update")) stop("--mode must be standard, iterate, or update.", call. = FALSE)
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
    step_down <- 0L
    request_id <- if (mode == "standard") NULL else finn_new_request_id()
    resumed <- 0L
  }

  # A machine-wide lock keeps launches in different projects from claiming
  # the same cores and memory at once. Always taken after the project lock.
  machine_lock_path <- file.path(finn_settings_dir(), "launch.lock")
  machine_lock <- finn_lock_acquire(machine_lock_path, timeout = 30, stale = 120)
  if (is.null(machine_lock)) {
    return(finn_result(FALSE, "busy", "Another Finn run is being started on this computer right now.", NULL,
      c("Wait a few seconds, then try again; nothing was started.")))
  }
  on.exit(finn_lock_release(machine_lock_path, machine_lock), add = TRUE)

  global <- run_cfg$run_global_models %||% finn_default_global_models(run_cfg$date_type)
  data_gb <- finn_data_gb(checked$data)
  claimed <- finn_claimed_resources(paths$root, exclude_project = paths$name)
  sizing <- finn_plan_with_others(facts$n_series, data_gb, global, claimed,
    step_down = step_down, sequential = identical(run_cfg$parallel, "none")
  )
  if (!sizing$fits) {
    return(launch_machine_busy(claimed, sizing$remaining, data_gb))
  }
  plan <- sizing$plan

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
    config = run_cfg, config_hash = launch_config_hash(run_cfg),
    data_md5 = launch_data_md5(run_cfg$data$file),
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
  # The queued state now blocks other launches and records this run's claim,
  # so the locks can go before a possibly long --wait run.
  finn_lock_release(machine_lock_path, machine_lock)
  finn_lock_release(lock_path, lock)

  wait <- finn_bool(finn_arg(args, "wait"))
  started <- launch_worker(paths, run_paths, wait = wait)
  if (!wait) {
    finn_update_state(run_paths, pid = started$pid, pid_create_time = started$pid_create_time)
  }

  notes <- c(decision$notes, launch_shared_note(claimed, mode), launch_onedrive_note(paths, facts$n_series), finn_disk_warning(paths$root))
  if (!wait) notes <- c(notes, launch_run_notes(mode, plan, facts$n_series))
  estimate <- launch_estimate(mode, plan, facts$n_series)
  state <- finn_read_state(run_paths)
  data <- list(run_id = run_paths$id, mode = mode, resumed = identical(decision$action, "resume"),
    request_id = request_id, parallel = plan, log = run_paths$log, output = run_paths$output,
    estimated_runtime = estimate$text, warnings = checked$warnings)
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
