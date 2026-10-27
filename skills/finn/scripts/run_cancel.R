# Cancel a running Finn run without deleting anything.
# Usage:
#   Rscript run_cancel.R --project=<name> --run_id=<id> --confirm=true
#
# Marks the run cancelled, then stops the worker process and its children
# (for example parallel workers). Saved artifacts stay in place, so the run
# can be resumed later with run_forecast.R.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Stop a process and all its descendants
#'
#' Children are stopped first so parallel workers do not linger. Verifies the
#' process creation time to avoid stopping an unrelated process that reused
#' the pid.
#'
#' @param pid Process id.
#' @param create_time Recorded creation time in epoch seconds, or NULL.
#' @return List with `stopped` (count) and `note`.
cancel_kill_tree <- function(pid, create_time = NULL) {
  alive <- finn_pid_alive(pid, create_time)
  if (identical(alive, FALSE)) {
    return(list(stopped = 0L, confirmed = TRUE, note = "The worker process was no longer running."))
  }
  if (!isTRUE(alive)) {
    return(list(stopped = 0L, confirmed = FALSE, note = "Could not confirm whether the worker process is running."))
  }
  h <- ps::ps_handle(as.integer(pid))
  kids <- tryCatch(ps::ps_children(h, recursive = TRUE), error = function(e) list())
  stopped <- 0L
  for (k in rev(kids)) {
    ok <- tryCatch(
      {
        ps::ps_kill(k)
        TRUE
      },
      error = function(e) FALSE
    )
    stopped <- stopped + as.integer(ok)
  }
  ok <- tryCatch(
    {
      ps::ps_kill(h)
      TRUE
    },
    error = function(e) FALSE
  )
  stopped <- stopped + as.integer(ok)
  still_alive <- TRUE
  for (i in seq_len(20L)) {
    still_alive <- finn_pid_alive(pid, create_time)
    if (identical(still_alive, FALSE) || is.na(still_alive)) break
    Sys.sleep(0.1)
  }
  confirmed <- identical(still_alive, FALSE)
  note <- if (confirmed) {
    sprintf("Stopped %d process(es).", stopped)
  } else {
    "The worker process could not be confirmed stopped."
  }
  list(stopped = stopped, confirmed = confirmed, note = note)
}

finn_main(function(args) {
  finn_require("ps")
  paths <- finn_project_from_args(args)
  run_id <- finn_arg(args, "run_id")
  if (is.null(run_id)) {
    active <- finn_active_run(paths)
    return(finn_result(FALSE, "needs_run_id", "Say which run to cancel with --run_id.",
      list(active_run = active$run_id),
      if (!is.null(active)) sprintf("Confirm with the user, then rerun with --run_id=%s --confirm=true.", active$run_id)
    ))
  }
  run_paths <- finn_run_paths(paths, run_id)
  state <- finn_read_state(run_paths)
  if (is.null(state)) {
    return(finn_result(FALSE, "not_found", paste0("No run named ", run_id, " in this project.")))
  }
  status <- finn_effective_status(state)
  if (!status %in% c("queued", "running")) {
    return(finn_result(TRUE, status, sprintf("Run %s is already %s; nothing to cancel.", run_id, status)))
  }
  if (!isTRUE(finn_bool(finn_arg(args, "confirm"), FALSE))) {
    return(finn_result(FALSE, "needs_confirmation",
      sprintf("Run %s is %s at stage '%s'. Cancelling stops it; saved progress is kept and it can be resumed later.", run_id, status, state$stage %||% "unknown"),
      list(run_id = run_id, stage = state$stage),
      "Ask the user to confirm, then rerun with --confirm=true."
    ))
  }
  host <- state$host
  if (!is.null(host) && !identical(host, Sys.info()[["nodename"]])) {
    return(finn_result(FALSE, "other_machine", sprintf("Run %s was started on another computer (%s); cancel it there.", run_id, host)))
  }

  killed <- cancel_kill_tree(state$pid, state$pid_create_time)
  if (!isTRUE(killed$confirmed)) {
    finn_log("Could not confirm cancellation of run ", run_id, ". ", killed$note)
    return(finn_result(FALSE, "error",
      sprintf("Run %s was not marked cancelled. %s", run_id, killed$note),
      list(run_id = run_id, stage = state$stage, stopped_processes = killed$stopped),
      "Check run_status.R before retrying cancellation."
    ))
  }
  latest <- finn_read_state(run_paths)
  if (identical(finn_effective_status(latest), "completed")) {
    return(finn_result(TRUE, "completed", sprintf("Run %s completed before cancellation took effect.", run_id),
      list(run_id = run_id, stage = latest$stage)))
  }
  finn_update_state(run_paths, status = "cancelled", cancelled_at = finn_now(), finished_at = finn_now())
  finn_log("Cancelled run ", run_id, ". ", killed$note)
  finn_result(TRUE, "cancelled",
    sprintf("Run %s was cancelled. %s Nothing was deleted.", run_id, killed$note),
    list(run_id = run_id, stage = state$stage, stopped_processes = killed$stopped),
    sprintf("To pick up where it stopped, run run_forecast.R --project=\"%s\" --mode=%s.", paths$name, state$mode)
  )
})
