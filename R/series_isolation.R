#' Attempts per series before a crash stops it
#'
#' Mirrors Spark's default `spark.task.maxFailures`, so an isolated series
#' tolerates as many crashes as one Spark task tolerated before finnts
#' isolated series. An internal constant, not an argument.
#' @noRd
SERIES_CRASH_ATTEMPTS <- 4L

#' Run one task per series, isolating crashes on Spark and local_machine
#'
#' Shared dispatcher of the Agent's per-series loops: local iteration, local
#' updates and default forecasts. Sequential runs call each task in this R
#' session, unchanged; a crash there still ends the session. Spark runs each
#' series in a forked child of its R worker ([isolate_series_fork()]) and
#' `local_machine` runs each series in a fresh R process
#' ([run_series_processes()]), so a series whose process dies (a segfault in
#' compiled code, or an out-of-memory kill) is named instead of failing the
#' whole loop. Crashed series are dispatched again, `SERIES_CRASH_ATTEMPTS`
#' attempts in total ([retry_crashed_series()]); each task's own restart checks
#' decide what a redispatched series skips. R errors keep the loop's policy.
#'
#' @param items Series to run, one task each.
#' @param task Name of a finnts function `function(item, context)`, or such a
#'   function. A name keeps Spark closures and process arguments small and is
#'   looked up in the namespace at run time.
#' @param context Inputs shared by every task, sent with each one. Keep lean.
#' @param labels Series names for crash messages, parallel to `items`.
#' @param parallel_processing `NULL`, `"local_machine"` or `"spark"`.
#' @param num_cores Cores for the pool or processes, or `NULL` for the default.
#' @param project_info Project info, for the pool and the process packages.
#' @param errorhandling `"stop"`: an R error, or a series that crashes on its
#'   last attempt, stops the loop; crashes carry class `finnts_series_crash`.
#'   `"pass"`: both come back as that series' error condition.
#' @param iterations Whether crash messages name the Agent iteration (the
#'   local iteration loop).
#'
#' @return One result per item, in item order: the task's value or, under
#'   `"pass"`, the series' error condition. On error a Spark or local pool is
#'   cancelled before the error propagates.
#' @noRd
run_series_tasks <- function(items,
                             task,
                             context,
                             labels = items,
                             parallel_processing,
                             num_cores,
                             project_info,
                             errorhandling = "stop",
                             iterations = FALSE) {
  force(items)
  force(task)
  force(context)
  force(labels)
  force(parallel_processing)
  force(num_cores)
  force(project_info)
  force(errorhandling)
  force(iterations)

  if (identical(parallel_processing, "local_machine")) {
    packages <- series_process_packages(project_info)
    workers <- max(1L, as.integer(min(get_cores(num_cores), length(items))))
    return(retry_crashed_series(labels, function(index) {
      run_series_processes(items[index], task, context, workers, packages, errorhandling)
    }, errorhandling, iterations))
  }

  par_info <- par_start(
    run_info = project_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    task_length = length(items)
  )
  par_end_on_exit(par_info$cl)
  isolate <- identical(parallel_processing, "spark")

  results <- tryCatch(
    {
      if (isolate) {
        retry_crashed_series(labels, function(index) {
          lapply(dispatch_series_foreach(items[index], task, context, par_info, TRUE, errorhandling), function(result) {
            if (inherits(result, "condition")) list(status = "error", condition = result) else result
          })
        }, errorhandling, iterations)
      } else {
        dispatch_series_foreach(items, task, context, par_info, FALSE, errorhandling)
      }
    },
    error = function(e) {
      # hard abort: cancel Spark jobs and stop local workers immediately
      cancel_parallel(par_info)
      stop(e)
    }
  )
  par_end(par_info$cl)
  results
}

#' Dispatch series tasks through the stage's foreach backend
#'
#' The foreach caller of sequential and Spark loops. sparklyr's doSpark
#' serializes this frame into every Spark task, so it holds only the items,
#' task name, lean context and pool info, all forced so that no promise
#' carries a caller's frame.
#'
#' @param items Series of this round.
#' @param task,context As in [run_series_tasks()].
#' @param par_info Pool from [par_start()].
#' @param isolate `TRUE` to run each series in a forked child.
#' @param errorhandling foreach error handling, `"stop"` or `"pass"`.
#'
#' @return foreach results in item order: task values, or outcomes from
#'   [isolate_series_fork()] when isolating. Under `"pass"`, R errors come
#'   back as conditions.
#' @noRd
dispatch_series_foreach <- function(items, task, context, par_info, isolate, errorhandling) {
  force(items)
  force(task)
  force(context)
  force(par_info)
  force(isolate)
  force(errorhandling)
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  foreach::foreach(
    item = items,
    .packages = packages,
    .errorhandling = errorhandling,
    .inorder = TRUE,
    .multicombine = TRUE
  ) %op%
    {
      if (isolate) isolate_series_fork(task, item, context) else run_series_task(task, item, context)
    } %>%
    base::suppressPackageStartupMessages()
}

#' Run one series task in this R session
#'
#' @param task Task name in the finnts namespace, or the task function.
#' @param item,context Task arguments.
#'
#' @return The task's value. Errors propagate.
#' @noRd
run_series_task <- function(task, item, context) {
  if (!is.function(task)) {
    task <- get(task, envir = asNamespace("finnts"), mode = "function")
  }
  task(item, context)
}

#' Dispatch crashed series again until they finish or run out of attempts
#'
#' Runs every series once, then only the series whose process crashed, up to
#' `SERIES_CRASH_ATTEMPTS` attempts. Each crash before the last attempt is
#' announced as a warning alert. After the last attempt, `"stop"` aborts with
#' every remaining series' message and `"pass"` returns each as a
#' `finnts_series_crash` condition, which the loop treats like an R error.
#'
#' @param labels Series names for messages, one per series.
#' @param run_round `function(index)` running the series at positions `index`
#'   and returning one outcome per series: `list(status = "ok", value)`,
#'   `list(status = "error", condition)` or `list(status = "crashed", details)`.
#' @param errorhandling `"stop"` or `"pass"`.
#' @param iterations Whether messages name the Agent iteration.
#'
#' @return One result per series in label order: the task value, or the error
#'   condition of a series that failed under `"pass"`.
#' @noRd
retry_crashed_series <- function(labels, run_round, errorhandling, iterations) {
  outcomes <- vector("list", length(labels))
  pending <- seq_along(labels)
  for (attempt in seq_len(SERIES_CRASH_ATTEMPTS)) {
    if (!length(pending)) break
    outcomes[pending] <- run_round(pending)
    crashed <- pending[vapply(outcomes[pending], function(outcome) identical(outcome$status, "crashed"), logical(1))]
    messages <- vapply(crashed, function(index) {
      describe_series_crash(labels[[index]], outcomes[[index]]$details, attempt, iterations)
    }, character(1))
    if (attempt < SERIES_CRASH_ATTEMPTS) {
      for (message in messages) cli::cli_alert_warning("{message}")
    } else if (length(crashed) && identical(errorhandling, "stop")) {
      rlang::abort(paste(messages, collapse = "\n"), class = "finnts_series_crash")
    } else {
      for (position in seq_along(crashed)) {
        outcomes[[crashed[[position]]]] <- list(status = "error",
          condition = rlang::error_cnd("finnts_series_crash", message = messages[[position]]))
      }
    }
    pending <- crashed
  }
  lapply(outcomes, function(outcome) if (identical(outcome$status, "ok")) outcome$value else outcome$condition)
}

#' Run one series in a forked child of this R process
#'
#' Used inside Spark tasks. The child shares the worker's loaded packages and
#' memory, and its RNG state, so results equal running in-process. Its R
#' messages, including R's crash report, go to a capture file in the task's
#' working folder (a YARN container folder, removed by YARN; finnts never
#' deletes it), which the parent then copies to its own log. A child that dies
#' without a result is reported as crashed; R errors are raised again here so
#' the loop's error handling applies. Where fork is unavailable the series
#' runs in-process.
#'
#' Side effect: after a crash, restores this process's temp folder, which R's
#' crash handler in the child deletes because parent and child share it.
#'
#' @param task Task name in the finnts namespace, or the task function.
#' @param item,context Task arguments.
#'
#' @return `list(status = "ok", value)` or
#'   `list(status = "crashed", details)` (see [series_crash_details()]).
#' @noRd
isolate_series_fork <- function(task, item, context) {
  if (!series_fork_supported()) {
    return(list(status = "ok", value = run_series_task(task, item, context)))
  }

  capture <- series_capture_file()
  result <- collect_series(fork_series(function() series_child(task, item, context, capture)))
  lines <- if (!is.null(capture) && file.exists(capture)) {
    tryCatch(readLines(capture, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  } else {
    character()
  }
  relay_series_output(lines)

  if (inherits(result, "try-error")) {
    condition <- attr(result, "condition")
    if (is.null(condition)) stop(as.character(result), call. = FALSE)
    stop(condition)
  }
  if (is.list(result) && identical(result$status, "error")) stop(result$condition)
  if (is.list(result) && identical(result$status, "ok")) {
    return(result)
  }

  restore_session_tempdir()
  list(status = "crashed", details = series_crash_details(lines))
}

#' Run one series task inside a forked child
#'
#' Sends the child's R messages (stderr) to the capture file, so the parent
#' can name the step and quote R's crash report. The sink is restored and the
#' file closed when the task returns.
#'
#' @param task,item,context Task and its arguments.
#' @param capture Capture file path, or `NULL` to leave messages as they are.
#'
#' @return `list(status = "ok", value)`, or `list(status = "error", condition)`
#'   for an R error.
#' @noRd
series_child <- function(task, item, context, capture) {
  if (!is.null(capture)) {
    connection <- tryCatch(file(capture, open = "wt"), error = function(e) NULL)
    if (!is.null(connection)) {
      sink(connection, type = "message")
      on.exit(
        {
          sink(type = "message")
          close(connection)
        },
        add = TRUE
      )
    }
  }
  tryCatch(
    list(status = "ok", value = run_series_task(task, item, context)),
    error = function(e) list(status = "error", condition = e)
  )
}

#' Whether this process can fork series children
#'
#' @return `TRUE` on Unix-alikes, where `parallel` can fork.
#' @noRd
series_fork_supported <- function() {
  .Platform$OS.type == "unix"
}

#' Create a capture file for a forked series' messages
#'
#' The file goes in the working folder, not R's temp folder: R's crash handler
#' in a forked child deletes the temp folder it shares with the parent, which
#' would lose the crash report. On Spark the working folder is the task's YARN
#' container folder.
#'
#' @return The created file's path, or `NULL` when it can't be created.
#' @noRd
series_capture_file <- function() {
  path <- tempfile("finnts-series-", tmpdir = getwd(), fileext = ".log")
  created <- tryCatch(file.create(path, showWarnings = FALSE), error = function(e) FALSE)
  if (isTRUE(created)) path else NULL
}

#' Fork a child that evaluates `fun()`
#'
#' Thin wrapper over `parallel::mcparallel()`, which exists only on
#' Unix-alikes and is looked up when called. The child keeps the parent's RNG
#' state (`mc.set.seed = FALSE`).
#'
#' @param fun Function of no arguments to run in the child.
#'
#' @return The child's job handle.
#' @noRd
fork_series <- function(fun) {
  mcparallel <- get("mcparallel", envir = asNamespace("parallel"))
  mcparallel(fun(), mc.set.seed = FALSE)
}

#' Wait for a forked child and return its result
#'
#' @param job Job handle from [fork_series()].
#'
#' @return The child's returned value, or `NULL` when it died without one.
#' @noRd
collect_series <- function(job) {
  mccollect <- get("mccollect", envir = asNamespace("parallel"))
  result <- mccollect(job, wait = TRUE)
  if (length(result)) result[[1]] else NULL
}

#' Restore this process's temp folder after a forked child crashed
#'
#' @return The temp folder path, invisibly.
#' @noRd
restore_session_tempdir <- function() {
  invisible(tempdir(check = TRUE))
}

#' Copy a forked series' captured messages to this process's stderr
#'
#' On Spark this is the executor log, so the series' progress shows there as
#' it did before series ran in children.
#'
#' @param lines Captured message lines.
#'
#' @return `NULL`, invisibly.
#' @noRd
relay_series_output <- function(lines) {
  if (length(lines)) cat(paste0(lines, "\n"), sep = "", file = stderr())
  invisible(NULL)
}

#' Run series tasks in fresh R processes, a few at a time
#'
#' `local_machine` isolation: each series runs in its own background R
#' process ([start_series_process()]), at most `workers` at once. Output is
#' kept in memory to describe a crash and is not shown, as with worker pools.
#' A process that exits without a result is reported as crashed with its exit
#' status. Under `"stop"` an R error ends the round: the other processes are
#' killed and the error is raised with its class. Processes still running when
#' this returns or fails are killed.
#'
#' @param items Series of this round.
#' @param task,context As in [run_series_tasks()].
#' @param workers Maximum processes at once.
#' @param packages Packages each process attaches before running its task.
#' @param errorhandling `"stop"` or `"pass"`.
#'
#' @return One outcome per item, in item order:
#'   `list(status = "ok", value)`, `list(status = "error", condition)` or
#'   `list(status = "crashed", details)`.
#' @noRd
run_series_processes <- function(items, task, context, workers, packages, errorhandling) {
  outcomes <- vector("list", length(items))
  running <- list()
  on.exit(for (job in running) try(job$process$kill(), silent = TRUE), add = TRUE)
  next_item <- 1L

  while (next_item <= length(items) || length(running)) {
    while (length(running) < workers && next_item <= length(items)) {
      running[[length(running) + 1L]] <- list(
        index = next_item, lines = character(),
        process = start_series_process(task, items[[next_item]], context, packages)
      )
      next_item <- next_item + 1L
    }

    active <- list()
    for (job in running) {
      alive <- job$process$is_alive()
      job$lines <- c(job$lines, job$process$read_output_lines())
      if (alive) {
        active[[length(active) + 1L]] <- job
        next
      }
      job$lines <- c(job$lines, job$process$read_all_output_lines())
      result <- tryCatch(job$process$get_result(), error = function(e) NULL)
      outcome <- if (is.list(result) && isTRUE(result$status %in% c("ok", "error"))) {
        result
      } else {
        list(status = "crashed", details = series_crash_details(job$lines, job$process$get_exit_status()))
      }
      if (identical(outcome$status, "error") && identical(errorhandling, "stop")) stop(outcome$condition)
      outcomes[[job$index]] <- outcome
    }
    running <- active
    if (length(running)) Sys.sleep(0.1)
  }
  outcomes
}

#' Start one series task in a background R process
#'
#' The process runs [series_process_main()] with this session's library
#' paths, sends stdout and stderr to one pipe, and is killed if this session
#' exits.
#'
#' @param task Task name in the finnts namespace, or the task function.
#' @param item,context Task arguments.
#' @param packages Packages to attach first.
#'
#' @return A `callr` background process.
#' @noRd
start_series_process <- function(task, item, context, packages) {
  callr::r_bg(
    series_process_main,
    args = list(task = task, item = item, context = context, packages = packages),
    libpath = .libPaths(),
    stdout = "|",
    stderr = "2>&1",
    supervise = TRUE
  )
}

#' Entry point of a series process
#'
#' Runs in a fresh R process, where callr gives it the global environment, so
#' it uses only base R. Attaches the worker packages as local workers do
#' (failures ignored, so a missing package raises its usual error where
#' needed), then runs the task. Every R error, including setup errors, comes
#' back as a result so it is not mistaken for a crash.
#'
#' @param task Task name in the finnts namespace, or the task function.
#' @param item,context Task arguments.
#' @param packages Packages to attach.
#'
#' @return `list(status = "ok", value)` or `list(status = "error", condition)`.
#' @noRd
series_process_main <- function(task, item, context, packages) {
  tryCatch(
    {
      for (package in packages) {
        try(suppressPackageStartupMessages(library(package, character.only = TRUE)), silent = TRUE)
      }
      if (!is.function(task)) {
        task <- get(task, envir = asNamespace("finnts"), mode = "function")
      }
      list(status = "ok", value = task(item, context))
    },
    error = function(e) list(status = "error", condition = e)
  )
}

#' Packages a series process attaches
#'
#' The same packages [par_start()] attaches on `local_machine` workers, plus
#' finnts itself. [par_start()] is called sequentially only to read its list;
#' that starts no pool.
#'
#' @param project_info Project info; storage and output format add packages.
#'
#' @return Character vector of package names.
#' @noRd
series_process_packages <- function(project_info) {
  base <- par_start(run_info = project_info, parallel_processing = NULL, num_cores = NULL, task_length = 1L)$packages
  unique(c(base, "hts", "magrittr", "methods", "rsample", "finnts"))
}

#' Summarize a crashed series' output
#'
#' Reads the progress the series printed before its process died: the last
#' started step (a cli info line such as "Training Individual Models"), the
#' Agent iterations finished in earlier attempts and in this one, and R's crash
#' report when R printed one (a segfault, bus error, illegal operation or C
#' stack overflow).
#'
#' @param lines Output lines of the series' process.
#' @param exit_status Process exit status, or `NA` when unknown (forked
#'   children).
#'
#' @return A list: `step` (or `NA`), `previous` (iterations finished before
#'   this attempt), `completed` (iterations finished in this attempt), `report`
#'   (up to five non-empty report lines of at most 200 characters) and
#'   `exit_status`.
#' @noRd
series_crash_details <- function(lines, exit_status = NA_integer_) {
  lines <- trimws(enc2utf8(as.character(lines)))

  start <- grep("caught segfault|caught (bus error|illegal operation)|irrecoverable exception|C stack usage", lines)
  report <- if (length(start)) {
    rest <- lines[start[[1]]:length(lines)]
    utils::head(substr(rest[nzchar(rest)], 1, 200), 5)
  } else {
    character()
  }

  # progress steps print as "<info symbol> Step name", sometimes several on one
  # line separated by "..."; alerts with punctuation or iteration counts are not steps
  segments <- trimws(unlist(strsplit(lines, "...", fixed = TRUE), use.names = FALSE))
  segments <- segments[grepl("^(\u2139|i) [A-Z]", segments)]
  segments <- sub("^(\u2139|i) ", "", segments)
  segments <- segments[nchar(segments) <= 60 & !grepl("[:{}'\"]|\\. |Forecast Iteration", segments)]

  previous <- regmatches(lines, regexpr("due to [0-9]+ previously completed iteration", lines))
  list(
    step = if (length(segments)) utils::tail(segments, 1) else NA_character_,
    previous = if (length(previous)) as.integer(gsub("[^0-9]", "", utils::tail(previous, 1))) else 0L,
    completed = sum(grepl("Forecast Iteration [0-9]+/[0-9]+ Complete", lines)),
    report = report,
    exit_status = exit_status
  )
}

#' Message for one series crash
#'
#' Format: "Series '<label>' crashed in iteration <n> (<step>): <reason>.
#' Retrying (attempt <k> of 4)." The iteration and step parts appear only when
#' known. The reason quotes R's crash report; without one, a kill signal or
#' unknown status reads as an out-of-memory kill, other exit statuses as an
#' unexpected exit.
#'
#' @param label Series name.
#' @param details Crash details from [series_crash_details()].
#' @param attempt Attempt that crashed, from 1.
#' @param iterations Whether to name the Agent iteration.
#'
#' @return A single string, ending with the retry or a final "Stopped after 4
#'   attempts."
#' @noRd
describe_series_crash <- function(label, details, attempt, iterations = FALSE) {
  where <- if (isTRUE(iterations)) paste0(" in iteration ", details$previous + details$completed + 1L) else ""
  if (!is.na(details$step)) where <- paste0(where, " (", details$step, ")")

  status <- details$exit_status
  reason <- if (length(details$report)) {
    paste0("R reported: ", paste(details$report, collapse = " | "))
  } else if (is.na(status) || status %in% c(-9, 137)) {
    "killed by the operating system, likely out of memory"
  } else {
    paste0("the R process stopped unexpectedly (exit status ", status, ")")
  }
  reason <- sub("[[:space:].]+$", "", reason)

  outcome <- if (attempt < SERIES_CRASH_ATTEMPTS) {
    paste0("Retrying (attempt ", attempt + 1L, " of ", SERIES_CRASH_ATTEMPTS, ").")
  } else {
    paste0("Stopped after ", SERIES_CRASH_ATTEMPTS, " attempts.")
  }
  paste0("Series '", label, "' crashed", where, ": ", reason, ". ", outcome)
}
