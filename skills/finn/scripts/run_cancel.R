# Cancel a running Finn run without deleting anything.
# Usage:
#   Rscript run_cancel.R --project=<name> --run_id=<id> --confirm=true
#
# Stops the worker process and its children (for example parallel workers),
# confirms they exited, then marks the run cancelled. Saved artifacts stay in
# place, so the run can be resumed later with run_forecast.R.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Stop a process and all its descendants
#'
#' Children are stopped first so parallel workers do not linger. Verifies the
#' process creation time to avoid stopping an unrelated process that reused
#' the pid, and confirms every captured descendant is gone.
#'
#' @param pid Process id.
#' @param create_time Recorded creation time in epoch seconds, or NULL.
#' @param process_api Process inspection API, normally the ps namespace.
#' @param is_alive Function checking a pid and creation time.
#' @return List with `stopped` (count), `confirmed`, and `note`. A run with no
#'   recorded worker pid is still starting, so it is never confirmed: its worker
#'   could start after a cancelled state is written and overwrite it.
cancel_kill_tree <- function(pid, create_time = NULL, process_api = asNamespace("ps"),
                             is_alive = finn_pid_alive) {
  if (is.null(pid) || length(pid) == 0 || is.na(pid)) {
    return(list(stopped = 0L, confirmed = FALSE,
      note = "The run is still starting and has no worker process yet. Retry cancellation in a few seconds."))
  }
  alive <- is_alive(pid, create_time)
  if (identical(alive, FALSE)) {
    return(list(stopped = 0L, confirmed = FALSE,
      note = "The worker was already gone, so its descendants could not be verified."))
  }
  if (!isTRUE(alive)) {
    return(list(stopped = 0L, confirmed = FALSE, note = "Could not confirm whether the worker process is running."))
  }
  h <- process_api$ps_handle(as.integer(pid))
  kids <- tryCatch(process_api$ps_children(h, recursive = TRUE), error = identity)
  if (inherits(kids, "error")) {
    return(list(stopped = 0L, confirmed = FALSE,
      note = paste0("Could not inspect worker descendants: ", conditionMessage(kids))))
  }
  child_ids <- lapply(kids, function(k) tryCatch(
    list(pid = process_api$ps_pid(k), create_time = as.numeric(process_api$ps_create_time(k))),
    error = identity
  ))
  stopped <- 0L
  for (k in rev(kids)) {
    ok <- tryCatch(
      {
        process_api$ps_kill(k)
        TRUE
      },
      error = function(e) FALSE
    )
    stopped <- stopped + as.integer(ok)
  }
  ok <- tryCatch(
    {
      process_api$ps_kill(h)
      TRUE
    },
    error = function(e) FALSE
  )
  stopped <- stopped + as.integer(ok)
  still_alive <- TRUE
  descendants_alive <- rep(NA, length(child_ids))
  for (i in seq_len(20L)) {
    still_alive <- is_alive(pid, create_time)
    descendants_alive <- vapply(child_ids, function(child) {
      if (inherits(child, "error")) return(NA)
      is_alive(child$pid, child$create_time)
    }, logical(1))
    if (identical(still_alive, FALSE) && all(vapply(descendants_alive, identical, logical(1), FALSE))) break
    if (is.na(still_alive) || anyNA(descendants_alive)) break
    Sys.sleep(0.1)
  }
  confirmed <- identical(still_alive, FALSE) &&
    all(vapply(descendants_alive, identical, logical(1), FALSE))
  note <- if (confirmed) {
    sprintf("Stopped %d process(es).", stopped)
  } else {
    "The worker and every captured descendant could not be confirmed stopped."
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
  # The worker may have finished while it was being stopped; report that outcome
  # instead of a cancellation error, and never overwrite a finished state.
  latest <- finn_read_state(run_paths)
  finished <- latest$status %||% ""
  if (finished %in% c("completed", "failed", "cancelled")) {
    return(finn_result(TRUE, finished,
      sprintf("Run %s was already %s before cancellation took effect.", run_id, finished),
      list(run_id = run_id, stage = latest$stage)))
  }
  if (!isTRUE(killed$confirmed)) {
    finn_log("Could not confirm cancellation of run ", run_id, ". ", killed$note)
    return(finn_result(FALSE, "error",
      sprintf("Run %s was not marked cancelled. %s", run_id, killed$note),
      list(run_id = run_id, stage = state$stage, stopped_processes = killed$stopped),
      "Check run_status.R before retrying cancellation."
    ))
  }
  finn_update_state(run_paths, status = "cancelled", cancelled_at = finn_now(), finished_at = finn_now())
  finn_log("Cancelled run ", run_id, ". ", killed$note)
  finn_result(TRUE, "cancelled",
    sprintf("Run %s was cancelled. %s Nothing was deleted.", run_id, killed$note),
    list(run_id = run_id, stage = state$stage, stopped_processes = killed$stopped),
    sprintf("To pick up where it stopped, run run_forecast.R --project=\"%s\" --mode=%s.", paths$name, state$mode)
  )
})
