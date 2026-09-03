make_global_resume_agent_info <- function(run_local_models = TRUE) {
  list(
    agent_version = 1,
    run_id = "resume-run",
    project_info = list(
      project_name = "resume-project",
      path = tempdir(),
      storage_object = NULL,
      data_output = "csv"
    ),
    llm = structure(list(), class = "Chat"),
    forecast_approach = "bottoms_up",
    run_global_models = TRUE,
    run_local_models = run_local_models
  )
}

mock_global_resume_dependencies <- function(state, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    get_eda_data = function(...) data.frame(done = TRUE),
    list_files = function(...) "input.csv",
    read_file = function(...) data.frame(Combo = c("A", "B")),
    load_run_results = function(agent_info, combo = NULL, ...) {
      if (is.null(combo) &&
        exists("global_history", envir = state, inherits = FALSE)) {
        return(state$global_history)
      }
      if (is.null(combo) && state$global_calls > 0L) {
        return(tibble::tibble(agent_version = agent_info$agent_version))
      }
      "No Previous Runs"
    },
    load_best_agent_run = function(...) {
      if (exists("best_run_error", envir = state, inherits = FALSE)) {
        stop(state$best_run_error, call. = FALSE)
      }
      state$best_runs
    },
    par_start = function(...) list(
      cl = NULL,
      packages = character(0),
      foreach_operator = foreach::`%do%`
    ),
    par_end = function(...) invisible(NULL),
    fcst_agent_workflow = function(agent_info, combo, ...) {
      if (is.null(combo)) {
        state$global_calls <- state$global_calls + 1L
        if (nrow(state$best_runs) == 0L) {
          state$best_runs <- data.frame(
            combo = c("A", "B"),
            model_type = "global",
            weighted_mape = 0.2,
            run_complete = TRUE,
            max_iterations = 2,
            stringsAsFactors = FALSE
          )
        }
        return(list(status = "global complete"))
      }

      combo_name <- if (identical(combo, hash_data("A"))) "A" else "B"
      state$local_calls <- c(state$local_calls, combo_name)
      combo_index <- match(combo_name, state$best_runs$combo)
      state$best_runs$model_type[[combo_index]] <- "local"

      if (identical(combo_name, "B") && isTRUE(state$fail_local)) {
        state$best_runs$run_complete[[combo_index]] <- FALSE
        state$best_runs$max_iterations[[combo_index]] <- 0
        stop("local B failed", call. = FALSE)
      }

      state$best_runs$run_complete[[combo_index]] <- TRUE
      state$best_runs$max_iterations[[combo_index]] <- 2
      list(status = "local complete")
    },
    save_best_agent_run = function(...) invisible(NULL),
    save_agent_forecast = function(...) invisible(NULL),
    summarize_models = function(...) invisible(NULL),
    .package = "finnts",
    .env = .env
  )
}

test_that("restart skips global after one local combo completed", {
  state <- new.env(parent = emptyenv())
  state$best_runs <- data.frame()
  state$global_calls <- 0L
  state$local_calls <- character(0)
  state$fail_local <- TRUE
  mock_global_resume_dependencies(state)

  run_iteration <- function() {
    iterate_forecast(
      agent_info = make_global_resume_agent_info(),
      max_iter = 2,
      weighted_mape_goal = 0.05,
      parallel_processing = NULL,
      num_cores = 1
    )
  }

  expect_error(run_iteration(), "local B failed", fixed = TRUE)
  expect_identical(state$best_runs$model_type, c("local", "local"))
  expect_identical(state$best_runs$run_complete, c(TRUE, FALSE))

  state$fail_local <- FALSE
  run_iteration()

  expect_identical(state$global_calls, 1L)
  expect_identical(state$local_calls, c("A", "B", "B"))
})

test_that("completed global or local rows meet the iteration target", {
  completed_global <- data.frame(
    model_type = "global",
    run_complete = TRUE,
    max_iterations = 3
  )
  one_completed_local <- data.frame(
    model_type = c("global", "local", "local"),
    run_complete = c(FALSE, TRUE, FALSE),
    max_iterations = c(0, 3, 0)
  )

  expect_true(has_completed_iteration_target(completed_global, max_iter = 3))
  expect_true(has_completed_iteration_target(one_completed_local, max_iter = 3))
  expect_true(has_completed_iteration_target(completed_global, max_iter = 2))
  expect_true(has_completed_iteration_target(
    data.frame(run_complete = "TRUE", max_iterations = "3"),
    max_iter = 3
  ))
})

test_that("incomplete or unusable metadata does not meet the iteration target", {
  cases <- list(
    data.frame(),
    data.frame(model_type = "local"),
    data.frame(run_complete = NA, max_iterations = 3),
    data.frame(run_complete = FALSE, max_iterations = 3),
    data.frame(run_complete = TRUE, max_iterations = 2),
    data.frame(run_complete = TRUE, max_iterations = NA_real_),
    data.frame(run_complete = TRUE, max_iterations = NaN),
    data.frame(run_complete = TRUE, max_iterations = Inf),
    data.frame(run_complete = TRUE, max_iterations = -Inf)
  )

  for (best_runs in cases) {
    expect_false(has_completed_iteration_target(best_runs, max_iter = 3))
  }
})

test_that("incomplete metadata keeps global optimization resumable", {
  state <- new.env(parent = emptyenv())
  state$best_runs <- data.frame(
    combo = c("A", "B"),
    model_type = "local",
    weighted_mape = 0.2,
    run_complete = FALSE,
    max_iterations = 0,
    stringsAsFactors = FALSE
  )
  state$global_calls <- 1L
  state$local_calls <- character(0)
  state$fail_local <- FALSE
  mock_global_resume_dependencies(state)

  iterate_forecast(
    agent_info = make_global_resume_agent_info(run_local_models = FALSE),
    max_iter = 3,
    weighted_mape_goal = 0.05,
    parallel_processing = NULL,
    num_cores = 1
  )

  expect_identical(state$global_calls, 2L)
  expect_length(state$local_calls, 0L)
})

test_that("exhausted current-version global history skips optimization", {
  state <- new.env(parent = emptyenv())
  state$best_runs <- data.frame(
    combo = c("A", "B"),
    model_type = "global",
    weighted_mape = 0.2,
    run_complete = FALSE,
    max_iterations = 0
  )
  state$global_history <- tibble::tibble(agent_version = rep(1, 3))
  state$global_calls <- 0L
  state$local_calls <- character(0)
  state$fail_local <- FALSE
  mock_global_resume_dependencies(state)

  iterate_forecast(
    agent_info = make_global_resume_agent_info(run_local_models = FALSE),
    max_iter = 3,
    weighted_mape_goal = 0.05,
    parallel_processing = NULL,
    num_cores = 1
  )

  expect_identical(state$global_calls, 0L)
  expect_length(state$local_calls, 0L)
})

test_that("older-version global history does not consume the current budget", {
  state <- new.env(parent = emptyenv())
  state$best_runs <- data.frame(
    combo = c("A", "B"),
    model_type = "global",
    weighted_mape = 0.2,
    run_complete = FALSE,
    max_iterations = 0
  )
  state$global_history <- tibble::tibble(agent_version = rep(0, 3))
  state$global_calls <- 0L
  state$local_calls <- character(0)
  state$fail_local <- FALSE
  mock_global_resume_dependencies(state)

  iterate_forecast(
    agent_info = make_global_resume_agent_info(run_local_models = FALSE),
    max_iter = 3,
    weighted_mape_goal = 0.05,
    parallel_processing = NULL,
    num_cores = 1
  )

  expect_identical(state$global_calls, 1L)
  expect_length(state$local_calls, 0L)
})

test_that("metadata skip sends only above-goal global results to local fallback", {
  state <- new.env(parent = emptyenv())
  state$best_runs <- data.frame(
    combo = c("A", "B"),
    model_type = "global",
    weighted_mape = c(0.2, 0.01),
    run_complete = TRUE,
    max_iterations = 2,
    stringsAsFactors = FALSE
  )
  state$global_calls <- 0L
  state$local_calls <- character(0)
  state$fail_local <- FALSE
  mock_global_resume_dependencies(state)

  iterate_forecast(
    agent_info = make_global_resume_agent_info(),
    max_iter = 2,
    weighted_mape_goal = 0.05,
    parallel_processing = NULL,
    num_cores = 1
  )

  expect_identical(state$global_calls, 0L)
  expect_identical(state$local_calls, "A")
})

test_that("metadata skip avoids local optimization when all goals are met", {
  state <- new.env(parent = emptyenv())
  state$best_runs <- data.frame(
    combo = c("A", "B"),
    model_type = "global",
    weighted_mape = c(0.01, 0.02),
    run_complete = TRUE,
    max_iterations = 2,
    stringsAsFactors = FALSE
  )
  state$global_calls <- 0L
  state$local_calls <- character(0)
  state$fail_local <- FALSE
  mock_global_resume_dependencies(state)

  iterate_forecast(
    agent_info = make_global_resume_agent_info(),
    max_iter = 2,
    weighted_mape_goal = 0.05,
    parallel_processing = NULL,
    num_cores = 1
  )

  expect_identical(state$global_calls, 0L)
  expect_length(state$local_calls, 0L)
})

test_that("best-run read failures remain hard before global routing", {
  state <- new.env(parent = emptyenv())
  state$best_runs <- data.frame()
  state$best_run_error <- "ADLS unavailable"
  state$global_calls <- 0L
  state$local_calls <- character(0)
  state$fail_local <- FALSE
  mock_global_resume_dependencies(state)

  expect_error(
    iterate_forecast(
      agent_info = make_global_resume_agent_info(),
      max_iter = 2,
      weighted_mape_goal = 0.05,
      parallel_processing = NULL,
      num_cores = 1
    ),
    "ADLS unavailable",
    fixed = TRUE
  )

  expect_identical(state$global_calls, 0L)
  expect_length(state$local_calls, 0L)
})