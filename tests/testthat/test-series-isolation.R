# Fake background R process for run_series_processes(): alive for `alive`
# polls, then its output lines and its result (or a crash) become available.
fake_series_process <- function(result = NULL, crash = FALSE, lines = character(), exit_status = 0L, alive = 0L) {
  state <- new.env(parent = emptyenv())
  state$polls <- 0L
  state$killed <- FALSE
  list(
    is_alive = function() {
      state$polls <- state$polls + 1L
      state$polls <= alive && !state$killed
    },
    read_output_lines = function() character(),
    read_all_output_lines = function() lines,
    get_result = function() if (crash) stop("callr subprocess failed: R session crashed", call. = FALSE) else result(),
    get_exit_status = function() exit_status,
    kill = function() {
      state$killed <- TRUE
      invisible(TRUE)
    },
    get_pid = function() 1L,
    state = state
  )
}

# Output of a series that segfaulted while training, after two iterations
# finished in an earlier attempt and one in this attempt.
segfault_lines <- c(
  "[agent] Running local model optimization for combo: west",
  "\u2139 Adjusting max iterations down due to 2 previously completed iterations.",
  "\u2139 Prepping Data",
  "\u2714 Prepping Data [1.2s]",
  "\u2139 Forecast Iteration 1/3 Complete",
  "\u2139 Running submit_fcst_run...",
  "\u2139 Training Individual Models",
  "",
  " *** caught segfault ***",
  "address 0x10, cause 'memory not mapped'",
  "",
  "Traceback:",
  " 1: .Call(\"XGBoosterUpdateOneIter_R\", handle, iteration, dtrain)",
  paste0(" 2: xgboost::xgb.train(", strrep("x", 300), ")"),
  " 3: fit_xgboost()",
  "An irrecoverable exception occurred. R is aborting now ..."
)

test_that("crash details name the step, iteration and R's crash report", {
  details <- series_crash_details(segfault_lines)
  expect_identical(details$step, "Training Individual Models")
  expect_identical(details$previous, 2L)
  expect_identical(details$completed, 1L)
  expect_length(details$report, 5L)
  expect_identical(details$report[1:4], c(
    "*** caught segfault ***", "address 0x10, cause 'memory not mapped'", "Traceback:",
    "1: .Call(\"XGBoosterUpdateOneIter_R\", handle, iteration, dtrain)"
  ))
  expect_identical(nchar(details$report[5]), 200L)
  expect_identical(
    describe_series_crash("west", details, attempt = 1L, iterations = TRUE),
    paste0(
      "Series 'west' crashed in iteration 4 (Training Individual Models): R reported: ",
      paste(details$report, collapse = " | "), ". Retrying (attempt 2 of 4)."
    )
  )
})

test_that("crashes without a report read as out-of-memory kills or unexpected exits", {
  training <- "\u2139 Training Individual Models"
  for (status in list(NA_integer_, -9L, 137L)) {
    expect_identical(
      describe_series_crash("east", series_crash_details(training, status), attempt = 2L),
      "Series 'east' crashed (Training Individual Models): killed by the operating system, likely out of memory. Retrying (attempt 3 of 4)."
    )
  }
  expect_identical(
    describe_series_crash("east", series_crash_details(character(), -1073741819L), attempt = 4L),
    "Series 'east' crashed: the R process stopped unexpectedly (exit status -1073741819). Stopped after 4 attempts."
  )
  ascii <- series_crash_details(c("i Creating Model Workflows...", "i Prepping Data..."))
  expect_identical(describe_series_crash("east", ascii, attempt = 1L, iterations = TRUE),
    "Series 'east' crashed in iteration 1 (Prepping Data): killed by the operating system, likely out of memory. Retrying (attempt 2 of 4).")
  alerts <- series_crash_details(c(
    "\u2139 Adjusting max iterations down due to 1 previously completed iteration.",
    "\u2139 Tool 'submit_fcst_run' failed (attempt 1/4). Let's try again...",
    "\u2139 Forecast Iteration 1/2 Complete"
  ))
  expect_identical(alerts$step, NA_character_)
  expect_identical(alerts$previous + alerts$completed, 2L)
})

test_that("crashed series are dispatched again until they finish, with a message per crash", {
  attempts <- c(a = 0L, b = 0L)
  local_mocked_bindings(
    series_process_packages = function(project_info) character(),
    start_series_process = function(task, item, context, packages) {
      attempts[[item]] <<- attempts[[item]] + 1L
      if (item == "b" && attempts[[item]] < 3L) {
        return(fake_series_process(crash = TRUE, exit_status = -11L, lines = c(
          "\u2139 Training Individual Models", " *** caught segfault ***", "address 0x0, cause 'memory not mapped'"
        )))
      }
      fake_series_process(function() list(status = "ok", value = paste0(item, "-done")))
    }
  )

  messages <- testthat::capture_messages(results <- run_series_tasks(c("a", "b"), "task", list(),
    parallel_processing = "local_machine", num_cores = 2, project_info = list()))

  expect_identical(results, list("a-done", "b-done"))
  expect_identical(attempts, c(a = 1L, b = 3L))
  report <- "R reported: *** caught segfault *** | address 0x0, cause 'memory not mapped'"
  expect_identical(messages, paste0("! Series 'b' crashed (Training Individual Models): ", report,
    ". Retrying (attempt ", 2:3, " of 4).\n"))
})

test_that("a series that crashes on every attempt stops the loop or comes back as a failure", {
  starts <- c(a = 0L, b = 0L)
  local_mocked_bindings(
    series_process_packages = function(project_info) character(),
    start_series_process = function(task, item, context, packages) {
      starts[[item]] <<- starts[[item]] + 1L
      if (item == "b") return(fake_series_process(crash = TRUE, exit_status = -9L))
      fake_series_process(function() list(status = "ok", value = item))
    }
  )
  expected <- "Series 'b' crashed: killed by the operating system, likely out of memory. Stopped after 4 attempts."

  messages <- testthat::capture_messages(error <- tryCatch(run_series_tasks(c("a", "b"), "task", list(),
    parallel_processing = "local_machine", num_cores = 2, project_info = list()), error = identity))
  expect_s3_class(error, "finnts_series_crash")
  expect_identical(conditionMessage(error), expected)
  expect_identical(starts, c(a = 1L, b = 4L))
  expect_length(messages, 3L)

  starts[] <- 0L
  testthat::capture_messages(passed <- run_series_tasks(c("a", "b"), "task", list(),
    parallel_processing = "local_machine", num_cores = 2, project_info = list(), errorhandling = "pass"))
  expect_identical(passed[[1]], "a")
  expect_s3_class(passed[[2]], "finnts_series_crash")
  expect_identical(conditionMessage(passed[[2]]), expected)
  expect_identical(starts, c(a = 1L, b = 4L))
})

test_that("R errors in a series process keep their class under both loop policies", {
  classed <- structure(class = c("finnts_update_artifact_error", "error", "condition"),
    list(message = "storage unavailable", call = NULL))
  processes <- list()
  local_mocked_bindings(
    get_cores = function(num_cores) 2L,
    series_process_packages = function(project_info) character(),
    start_series_process = function(task, item, context, packages) {
      processes[[item]] <<- if (item == "bad") {
        fake_series_process(function() list(status = "error", condition = classed))
      } else {
        fake_series_process(function() list(status = "ok", value = item), alive = 2L)
      }
      processes[[item]]
    }
  )

  passed <- run_series_tasks(c("slow", "bad"), "task", list(), parallel_processing = "local_machine",
    num_cores = 2, project_info = list(), errorhandling = "pass")
  expect_identical(passed[[1]], "slow")
  expect_identical(passed[[2]], classed)

  processes <- list()
  expect_error(run_series_tasks(c("slow", "bad"), "task", list(), parallel_processing = "local_machine",
    num_cores = 2, project_info = list()), "storage unavailable", class = "finnts_update_artifact_error")
  expect_true(processes$slow$state$killed)
})

test_that("a forked series reports crashes, re-raises R errors and restores the temp folder", {
  capture_dir <- withr::local_tempdir()
  restored <- 0L
  collect <- NULL
  local_mocked_bindings(
    series_fork_supported = function() TRUE,
    series_capture_file = function() tempfile("series-", tmpdir = capture_dir, fileext = ".log"),
    fork_series = function(fun) fun,
    collect_series = function(job) collect(job),
    restore_session_tempdir = function() restored <<- restored + 1L
  )

  collect <- function(job) {
    writeLines(segfault_lines, environment(job)$capture, useBytes = TRUE)
    NULL
  }
  relayed <- capture.output(crashed <- isolate_series_fork("task", "west", list()), type = "message")
  expect_identical(crashed, list(status = "crashed", details = series_crash_details(segfault_lines)))
  expect_length(relayed, length(segfault_lines))
  expect_identical(trimws(relayed[c(1, 9)]), trimws(segfault_lines[c(1, 9)]))
  expect_identical(restored, 1L)

  classed <- structure(class = c("finnts_forecast_selection_rejected", "error", "condition"),
    list(message = "rejected", call = NULL))
  collect <- function(job) list(status = "error", condition = classed)
  expect_error(isolate_series_fork("task", "west", list()), "rejected", class = "finnts_forecast_selection_rejected")
  collect <- function(job) structure("Error : boom\n", class = "try-error", condition = simpleError("boom"))
  expect_error(isolate_series_fork("task", "west", list()), "boom", fixed = TRUE)
  collect <- function(job) list(status = "ok", value = environment(job)$item)
  expect_identical(isolate_series_fork("task", "west", list()), list(status = "ok", value = "west"))
  expect_identical(restored, 1L)
})

test_that("a forked series runs without a capture file when none can be created", {
  child_capture <- "unset"
  local_mocked_bindings(
    series_fork_supported = function() TRUE,
    series_capture_file = function() NULL,
    fork_series = function(fun) fun,
    collect_series = function(job) {
      child_capture <<- environment(job)$capture
      NULL
    },
    restore_session_tempdir = function() NULL
  )
  crashed <- isolate_series_fork("task", "a", list())
  expect_null(child_capture)
  expect_identical(describe_series_crash("a", crashed$details, attempt = 1L),
    "Series 'a' crashed: killed by the operating system, likely out of memory. Retrying (attempt 2 of 4).")
})

test_that("series run in-process where fork is unavailable", {
  local_mocked_bindings(
    series_fork_supported = function() FALSE,
    fork_series = function(fun) stop("must not fork"),
    agent_local_series_task = function(item, context) paste(item, context$x)
  )
  expect_identical(isolate_series_fork("agent_local_series_task", "a", list(x = "b")),
    list(status = "ok", value = "a b"))
})

test_that("a series child sends its messages to the capture file and wraps its result", {
  capture <- withr::local_tempfile(fileext = ".log")
  task <- function(item, context) {
    cat("i Prepping Data\n", file = stderr())
    if (item == "bad") stop("bad input", call. = FALSE)
    toupper(item)
  }
  before <- sink.number(type = "message")
  expect_identical(series_child(task, "ok", list(), capture), list(status = "ok", value = "OK"))
  expect_identical(readLines(capture), "i Prepping Data")
  failed <- series_child(task, "bad", list(), capture)
  expect_identical(failed$status, "error")
  expect_identical(conditionMessage(failed$condition), "bad input")
  expect_identical(sink.number(type = "message"), before)
})

test_that("Spark series run in forked children and only crashed series are dispatched again", {
  attempts <- c(a = 0L, b = 0L, c = 0L)
  cancelled <- 0L
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`,
      parallel_processing = "spark"),
    par_end = function(...) invisible(NULL),
    cancel_parallel = function(par_info) cancelled <<- cancelled + 1L,
    series_fork_supported = function() TRUE,
    series_capture_file = function() NULL,
    fork_series = function(fun) fun,
    collect_series = function(job) {
      item <- environment(job)$item
      attempts[[item]] <<- attempts[[item]] + 1L
      if (item == "c" || (item == "b" && attempts[[item]] == 1L)) NULL else list(status = "ok", value = toupper(item))
    },
    restore_session_tempdir = function() NULL
  )

  messages <- testthat::capture_messages(results <- run_series_tasks(c("a", "b"), "task", list(),
    labels = c("Series A", "Series B"), parallel_processing = "spark", num_cores = NULL,
    project_info = list(), iterations = TRUE))
  expect_identical(results, list("A", "B"))
  expect_identical(attempts, c(a = 1L, b = 2L, c = 0L))
  expect_identical(messages, paste0("! Series 'Series B' crashed in iteration 1: killed by the operating system, ",
    "likely out of memory. Retrying (attempt 2 of 4).\n"))
  expect_identical(cancelled, 0L)

  testthat::capture_messages(expect_error(run_series_tasks(c("a", "c"), "task", list(),
    parallel_processing = "spark", num_cores = NULL, project_info = list()),
  "Series 'c' crashed", class = "finnts_series_crash"))
  expect_identical(attempts[["c"]], 4L)
  expect_identical(cancelled, 1L)
})

test_that("local_machine runs each dispatcher's series task in its own process with lean context", {
  started <- list()
  local_mocked_bindings(
    par_start = function(run_info, parallel_processing, ...) {
      if (!is.null(parallel_processing)) stop("local_machine series must not start a worker pool")
      list(cl = NULL, packages = "dplyr", foreach_operator = foreach::`%do%`)
    },
    start_series_process = function(task, item, context, packages) {
      started[[length(started) + 1L]] <<- list(task = task, item = item, context = context, packages = packages)
      fake_series_process(function() series_process_main(task, item, context, character()))
    },
    agent_local_series_task = function(item, context) list(Combo = item, warnings = list()),
    agent_local_update_task = function(item, context) data.frame(Combo = item),
    agent_default_forecast_task = function(item, context) list(Combo = item, warnings = list())
  )
  info <- list(project_name = "p", path = withr::local_tempdir(), storage_object = NULL, data_output = "csv")
  lean <- list(project_info = info, run_id = "r", agent_version = 1)

  iterated <- run_agent_local_series(lean, c("a", "b"), 3, 0.05, "local_machine", FALSE, 2, 123)
  updated <- run_agent_local_updates(lean, c("a", "b"), "prev", info, "local_machine", FALSE, 2, 123)
  defaulted <- run_agent_default_forecasts(lean, c("ha", "hb"), list(models_to_run = "arima"), info,
    "local_machine", FALSE, 2, 123)

  expect_identical(iterated, list(list(Combo = "a", warnings = list()), list(Combo = "b", warnings = list())))
  expect_identical(updated, list(data.frame(Combo = "a"), data.frame(Combo = "b")))
  expect_identical(defaulted, list(list(Combo = "ha", warnings = list()), list(Combo = "hb", warnings = list())))
  expect_identical(vapply(started, `[[`, character(1), "task"),
    rep(c("agent_local_series_task", "agent_local_update_task", "agent_default_forecast_task"), each = 2))
  expect_identical(vapply(started, `[[`, character(1), "item"), c("a", "b", "a", "b", "ha", "hb"))
  expect_identical(started[[1]]$context, list(agent_info_lean = lean, max_iter = 3, weighted_mape_goal = 0.05,
    inner_parallel = FALSE, num_cores = 2, seed = 123, task_cores = 2))
  expect_identical(started[[3]]$context, list(agent_info_lean = lean, prev_run_id = "prev",
    inner_parallel = FALSE, num_cores = 2, seed = 123, task_cores = 2))
  expect_identical(started[[5]]$context, list(agent_info_lean = lean, default_inputs = list(models_to_run = "arima"),
    inner_parallel = FALSE, num_cores = 2, seed = 123, task_cores = 2, timestamp = "default"))
  expect_true(all(vapply(started, function(start) all(c("dplyr", "finnts") %in% start$packages), logical(1))))
})

test_that("a real series process that dies is reported as crashed while the others finish", {
  skip_on_cran()
  task <- function(item, context) {
    cat("i Training Individual Models\n", file = stderr())
    if (identical(item, "boom")) quit(save = "no", status = 3L, runLast = FALSE)
    if (identical(item, "fail")) {
      stop(structure(class = c("series_test_error", "error", "condition"), list(message = "bad input", call = NULL)))
    }
    paste0(item, "-done")
  }
  environment(task) <- globalenv()

  outcomes <- run_series_processes(c("ok", "boom", "fail"), task, list(), workers = 2L,
    packages = character(), errorhandling = "pass")

  expect_identical(outcomes[[1]], list(status = "ok", value = "ok-done"))
  expect_identical(outcomes[[2]]$status, "crashed")
  expect_identical(outcomes[[2]]$details$step, "Training Individual Models")
  expect_identical(describe_series_crash("boom", outcomes[[2]]$details, attempt = 1L),
    "Series 'boom' crashed (Training Individual Models): the R process stopped unexpectedly (exit status 3). Retrying (attempt 2 of 4).")
  expect_identical(outcomes[[3]]$status, "error")
  expect_s3_class(outcomes[[3]]$condition, "series_test_error")
})

test_that("a real forked series that dies is reported as crashed", {
  skip_on_cran()
  skip_on_os("windows")
  withr::local_dir(withr::local_tempdir())
  task <- function(item, context) {
    cat("i Prepping Data\n", file = stderr())
    if (identical(item, "boom")) {
      tools::pskill(Sys.getpid(), tools::SIGKILL)
      Sys.sleep(30)
    }
    paste0(item, "-done")
  }

  relayed <- capture.output(crashed <- isolate_series_fork(task, "boom", list()), type = "message")
  expect_identical(crashed$status, "crashed")
  expect_identical(crashed$details$step, "Prepping Data")
  expect_identical(relayed, "i Prepping Data")
  capture.output(finished <- isolate_series_fork(task, "ok", list()), type = "message")
  expect_identical(finished, list(status = "ok", value = "ok-done"))
  expect_error(capture.output(isolate_series_fork(function(item, context) stop("bad input"), "x", list()),
    type = "message"), "bad input", fixed = TRUE)
})
