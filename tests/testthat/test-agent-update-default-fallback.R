# Capture the real update graph without starting an LLM or running any nodes.
# Mock callbacks return only the constructed workflow; bindings restore on exit.
make_update_fallback_workflow <- function(parallel_processing = "spark", inner_parallel = TRUE) {
  local_mocked_bindings(
    new_llm_session = function(...) NULL,
    run_graph = function(chat, workflow, init_ctx) workflow,
    .package = "finnts"
  )
  update_fcst_agent_workflow(list(llm = NULL), list(), parallel_processing,
    inner_parallel, num_cores = 10)
}

test_that("local updates and default forecasts use a fourth local-machine attempt", {
  workflow <- make_update_fallback_workflow()
  local_mocked_bindings(wait_before_retry = function(...) invisible(NULL))
  for (tool in c("update_local_models", "forecast_new_combos")) {
    calls <- list()
    messages <- character()
    local_mocked_bindings(
      cli_alert_info = function(text, ...) {
        messages <<- c(messages, text)
        invisible(NULL)
      },
      .package = "cli"
    )
    # Simulate the worker reader error and succeed only on the fallback backend.
    operation <- function(...) {
      args <- list(...)
      calls[[length(calls) + 1L]] <<- args
      if (identical(args$parallel_processing, "spark")) {
        rlang::abort("unused argument (character_columns = ...)", class = "finnts_update_artifact_error")
      }
      "completed"
    }
    local_mocked_bindings(update_local_models = operation, forecast_new_combos = operation)
    node <- workflow[[tool]]
    context <- list(args = node$args, results = list(), attempts = list())
    result <- tryCatch(execute_node(node, context, NULL), error = identity)
    expect_true(isTRUE(result$ok), info = tool)
    expect_length(calls, 4L)
    if (length(calls) != 4L) next
    expect_identical(vapply(calls, `[[`, character(1), "parallel_processing"),
      c("spark", "spark", "spark", "local_machine"))
    expect_identical(vapply(calls, `[[`, logical(1), "inner_parallel"), c(TRUE, TRUE, TRUE, FALSE))
    expected <- node$args
    expected$parallel_processing <- "local_machine"
    expected$inner_parallel <- FALSE
    expect_identical(calls[[4]], expected)
    expect_identical(result$ctx$results[[tool]], "completed")
    expect_identical(result$ctx$attempts[[tool]], 0L)
    expect_identical(node$args, context$args)
    failover <- messages[startsWith(messages, "Failover:")]
    expect_length(failover, 1L)
    expect_match(failover, paste0("Failover: '", tool, "' failed with Spark."), fixed = TRUE)
  }
})

test_that("default fallback failure remains terminal after exactly four attempts", {
  workflow <- make_update_fallback_workflow()
  calls <- character()
  local_mocked_bindings(
    wait_before_retry = function(...) invisible(NULL),
    forecast_new_combos = function(parallel_processing, ...) {
      calls <<- c(calls, parallel_processing)
      stop(paste("unavailable backend", parallel_processing), call. = FALSE)
    }
  )
  node <- workflow$forecast_new_combos
  context <- list(args = node$args, results = list(), attempts = list())
  error <- expect_error(execute_node(node, context, NULL))
  expect_match(conditionMessage(error), "failed after 4 attempt(s)", fixed = TRUE)
  expect_match(conditionMessage(error), "unavailable backend local_machine", fixed = TRUE)
  expect_identical(calls, c("spark", "spark", "spark", "local_machine"))
})

test_that("default quality rejections bypass retries and backend fallback", {
  workflow <- make_update_fallback_workflow()
  calls <- 0L
  local_mocked_bindings(
    wait_before_retry = function(...) stop("Quality rejection must not wait or retry."),
    forecast_new_combos = function(...) {
      calls <<- calls + 1L
      rlang::abort("replacement is unusable", class = "finnts_forecast_selection_rejected")
    }
  )
  node <- workflow$forecast_new_combos
  context <- list(args = node$args, results = list(), attempts = list())
  expect_error(execute_node(node, context, NULL), class = "finnts_forecast_selection_rejected")
  expect_identical(calls, 1L)
})

test_that("default fallback recognizes Spark case-insensitively", {
  workflow <- make_update_fallback_workflow("SpArK", FALSE)
  calls <- character()
  local_mocked_bindings(
    wait_before_retry = function(...) invisible(NULL),
    forecast_new_combos = function(parallel_processing, inner_parallel, ...) {
      expect_false(inner_parallel)
      calls <<- c(calls, parallel_processing)
      if (parallel_processing != "local_machine") stop("worker failure", call. = FALSE)
      "completed"
    }
  )
  node <- workflow$forecast_new_combos
  result <- execute_node(node, list(args = node$args, results = list(), attempts = list()), NULL)
  expect_true(result$ok)
  expect_identical(calls, c("SpArK", "SpArK", "SpArK", "local_machine"))
})

test_that("successful Spark default attempts stop before fallback", {
  workflow <- make_update_fallback_workflow()
  local_mocked_bindings(wait_before_retry = function(...) invisible(NULL))
  for (succeed_on in 1:3) {
    calls <- 0L
    local_mocked_bindings(forecast_new_combos = function(parallel_processing, inner_parallel, ...) {
      expect_identical(parallel_processing, "spark")
      expect_true(inner_parallel)
      calls <<- calls + 1L
      if (calls < succeed_on) stop("transient worker failure", call. = FALSE)
      "completed"
    })
    node <- workflow$forecast_new_combos
    context <- list(args = node$args, results = list(), attempts = list())
    result <- execute_node(node, context, NULL)
    expect_true(result$ok)
    expect_identical(calls, succeed_on)
    expect_identical(result$ctx$args, node$args)
  }
})

test_that("non-Spark defaults retain their existing three-attempt budget", {
  local_mocked_bindings(wait_before_retry = function(...) invisible(NULL))
  for (backend in list(NULL, "local_machine")) {
    workflow <- make_update_fallback_workflow(backend, FALSE)
    calls <- 0L
    local_mocked_bindings(forecast_new_combos = function(parallel_processing, inner_parallel, ...) {
      expect_identical(parallel_processing, backend)
      expect_false(inner_parallel)
      calls <<- calls + 1L
      stop("unchanged backend failure", call. = FALSE)
    })
    node <- workflow$forecast_new_combos
    context <- list(args = node$args, results = list(), attempts = list())
    expect_error(execute_node(node, context, NULL), "failed after 3 attempt")
    expect_identical(calls, 3L)
  }
})

test_that("global updates do not gain the local/default backend fallback", {
  workflow <- make_update_fallback_workflow()
  calls <- 0L
  local_mocked_bindings(
    wait_before_retry = function(...) invisible(NULL),
    update_global_models = function(parallel_processing, inner_parallel, ...) {
      expect_identical(parallel_processing, "spark")
      expect_true(inner_parallel)
      calls <<- calls + 1L
      stop("global failure", call. = FALSE)
    }
  )
  node <- workflow$update_global_models
  context <- list(args = node$args, results = list(), attempts = list())
  expect_error(execute_node(node, context, NULL), "failed after 4 attempt")
  expect_identical(calls, 4L)
})

test_that("default forecasting independently falls back after local updates have recovered", {
  workflow <- make_update_fallback_workflow()
  workflow$update_local_models$`next` <- "forecast_new_combos"
  workflow$forecast_new_combos$`next` <- "stop"
  calls <- list(local = character(), default = character())
  local_mocked_bindings(
    wait_before_retry = function(...) invisible(NULL),
    update_local_models = function(parallel_processing, ...) {
      calls$local <<- c(calls$local, parallel_processing)
      if (parallel_processing == "spark") stop("local worker failure", call. = FALSE)
      list(status = "completed", failed_combos = character())
    },
    forecast_new_combos = function(parallel_processing, inner_parallel, new_combos, failed_combos, ...) {
      expect_identical(new_combos, "new-series")
      expect_identical(failed_combos, "rejected-series")
      calls$default <<- c(calls$default, parallel_processing)
      if (parallel_processing == "spark") stop("default worker failure", call. = FALSE)
      expect_false(inner_parallel)
      "default completed"
    }
  )
  context <- list(node = "update_local_models", attempts = list(), agent_info = list(),
    results = list(initial_checks = list(prev_best_runs_tbl = data.frame(), new_combos = "new-series"),
      check_update_failures = "rejected-series"))
  result <- tryCatch(run_graph(NULL, workflow, context), error = identity)
  expect_false(inherits(result, "error"))
  expect_identical(calls$local, c("spark", "spark", "spark", "local_machine"))
  expect_identical(calls$default, calls$local)
  if (!inherits(result, "error")) {
    expect_identical(result$results$forecast_new_combos, "default completed")
    expect_null(result$args)
  }
  expect_identical(workflow$forecast_new_combos$args$parallel_processing, "spark")
  expect_true(workflow$forecast_new_combos$args$inner_parallel)
})
