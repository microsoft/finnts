# tests/testthat/test-load_run_results.R
# Tests for iteration ranking and numeric type safety in load_run_results()
#
# Ranking tests call load_run_results() with an in-memory run-log source.
# The original coercion tests below retain their direct bind_rows coverage.

test_that("run history retains the original near-best model-pool signal", {
  cases <- list(
    list(wmape = c(0.1, 0.1), average = c(0.2, 0.15), winner = 2L),
    list(wmape = c(0.1, 0.105), average = c(0.2, 0.15), winner = 2L),
    list(wmape = c(0.1, 0.11), average = c(0.2, 0.15), winner = 2L),
    list(wmape = c(0.1, 0.1101), average = c(0.2, 0.15), winner = 1L),
    list(wmape = c(0.1, 0.105), average = c(0.2, 0.2), winner = 1L),
    list(wmape = c(0.1, 0.105), average = c(0.2, 0.25), winner = 1L),
    list(wmape = c(0.1, 0.105), average = c(0.2, NA_real_), winner = 1L),
    list(wmape = c(0.1, 0.105), average = c(NA_real_, 0.15), winner = 1L),
    list(wmape = c(0.12, 0.1, 0.108), average = c(0.05, 0.2, 0.15), winner = 3L),
    list(wmape = c(0, 0.0001), average = c(0.2, 0.15), winner = 1L)
  )
  for (case in cases) local({
    combo <- hash_data("North--Revenue")
    logs <- data.frame(
      project_name = paste0("project_", combo),
      run_name = paste0("agent_parent_", combo, "_", seq_along(case$wmape)),
      created = sprintf("2026-01-01 00:00:%02d", seq_along(case$wmape)),
      agent_version = 1, agent_forecast_approach = "bottoms_up",
      weighted_mape = case$wmape, model_avg_wmape = case$average,
      selection_status = "evaluated"
    )
    reads <- 0L
    local_mocked_bindings(
      get_run_info = function(...) { reads <<- reads + 1L; logs },
      assess_agent_run = function(...) stop("iteration ranking must not restore forecasts"),
      read_series_history = function(...) stop("iteration ranking must not reload prepared history")
    )
    agent <- list(agent_version = 1, forecast_approach = "bottoms_up",
      project_info = list(project_name = "project", path = tempdir()))

    result <- load_run_results(agent, combo)

    expect_identical(result$run_number[result$best_run == "yes"], case$winner)
    expect_equal(result$model_avg_wmape, case$average)
    expect_identical(reads, 1L)
  })
})

test_that("near-best ranking excludes other versions and incomplete iterations", {
  combo <- hash_data("North--Revenue")
  logs <- data.frame(
    project_name = paste0("project_", combo),
    run_name = paste0("agent_parent_", combo, "_", 1:5),
    created = sprintf("2026-01-01 00:00:%02d", 1:5),
    agent_version = c(1, 1, 1, 0, 1), agent_forecast_approach = "bottoms_up",
    weighted_mape = c(0.1, 0.105, 0.001, 0.0001, 0.106),
    model_avg_wmape = c(0.2, 0.18, 0.01, 0.001, 0.15),
    selection_status = c("evaluated", "evaluated", "rejected", "evaluated", "partial")
  )
  local_mocked_bindings(get_run_info = function(...) logs)
  agent <- list(agent_version = 1, forecast_approach = "bottoms_up",
    project_info = list(project_name = "project", path = tempdir()))

  result <- load_run_results(agent, combo)

  expect_identical(result$run_number[result$best_run == "yes"], 2L)
  expect_true(all(result$best_run[result$agent_version == 0] == "no"))
  expect_true(all(result$best_run[result$selection_status %in% c("partial", "rejected")] == "no"))
})

# helper: simulate the coercion + bind_rows logic from load_run_results
coerce_and_bind <- function(previous_runs, current_run_log) {
  numeric_cols <- c(
    "weighted_mape", "model_avg_wmape", "model_median_wmape",
    "model_std_wmape", "agent_version"
  )

  # remove matching run_name row (mirrors the real function)
  current_run_name <- current_run_log$run_name[[1]]
  previous_runs <- previous_runs[previous_runs$run_name != current_run_name, , drop = FALSE]

  # coerce shared columns
  common_cols <- intersect(names(previous_runs), names(current_run_log))
  for (col in common_cols) {
    prev_class <- class(previous_runs[[col]])[[1]]
    curr_class <- class(current_run_log[[col]])[[1]]
    if (prev_class != curr_class) {
      if (col %in% numeric_cols) {
        previous_runs[[col]] <- as.numeric(previous_runs[[col]])
        current_run_log[[col]] <- as.numeric(current_run_log[[col]])
      } else {
        previous_runs[[col]] <- as.character(previous_runs[[col]])
        current_run_log[[col]] <- as.character(current_run_log[[col]])
      }
    }
  }

  result <- dplyr::bind_rows(previous_runs, current_run_log)

  # enforce numeric type on key columns after merge
  for (col in intersect(numeric_cols, names(result))) {
    result[[col]] <- as.numeric(result[[col]])
  }

  result
}

# * Test: numeric columns stay numeric when previous_runs has character weighted_mape ----
test_that("coerce_and_bind keeps weighted_mape numeric when previous_runs has character type", {
  prev <- data.frame(
    run_name = c("run1", "run2"),
    weighted_mape = c("0.15", "0.20"),
    model_avg_wmape = c(0.10, 0.12),
    agent_version = c(1, 1),
    stringsAsFactors = FALSE
  )
  curr <- data.frame(
    run_name = "run3",
    weighted_mape = 0.12,
    model_avg_wmape = 0.08,
    agent_version = 1,
    stringsAsFactors = FALSE
  )

  result <- coerce_and_bind(prev, curr)

  expect_true(is.numeric(result$weighted_mape))
  expect_true(is.numeric(result$model_avg_wmape))
  expect_true(is.numeric(result$agent_version))
  expect_equal(nrow(result), 3)
  expect_equal(result$weighted_mape, c(0.15, 0.20, 0.12))
})

# * Test: numeric columns stay numeric when agent_version is character ----
test_that("coerce_and_bind keeps agent_version numeric when previous_runs has character type", {
  prev <- data.frame(
    run_name = c("run1"),
    weighted_mape = 0.15,
    agent_version = "1",
    stringsAsFactors = FALSE
  )
  curr <- data.frame(
    run_name = "run2",
    weighted_mape = 0.12,
    agent_version = 2,
    stringsAsFactors = FALSE
  )

  result <- coerce_and_bind(prev, curr)

  expect_true(is.numeric(result$agent_version))
  expect_equal(result$agent_version, c(1, 2))
})

# * Test: Inf weighted_mape read as character is coerced to numeric Inf ----
test_that("coerce_and_bind handles Inf values stored as character", {
  prev <- data.frame(
    run_name = c("run1", "run2"),
    weighted_mape = c("Inf", "0.15"),
    model_avg_wmape = c("0.10", "0.12"),
    agent_version = c("1", "1"),
    stringsAsFactors = FALSE
  )
  curr <- data.frame(
    run_name = "run3",
    weighted_mape = 0.12,
    model_avg_wmape = 0.08,
    agent_version = 1,
    stringsAsFactors = FALSE
  )

  result <- coerce_and_bind(prev, curr)

  expect_true(is.numeric(result$weighted_mape))
  expect_true(is.infinite(result$weighted_mape[[1]]))
  expect_equal(result$weighted_mape[[2]], 0.15)
  expect_equal(result$weighted_mape[[3]], 0.12)
})

# * Test: all-numeric columns produce no coercion issues (happy path) ----
test_that("coerce_and_bind works cleanly when all types already match", {
  prev <- data.frame(
    run_name = c("run1", "run2"),
    weighted_mape = c(0.15, 0.20),
    model_avg_wmape = c(0.10, 0.12),
    model_median_wmape = c(0.09, 0.11),
    model_std_wmape = c(0.01, 0.01),
    agent_version = c(1, 1),
    stringsAsFactors = FALSE
  )
  curr <- data.frame(
    run_name = "run3",
    weighted_mape = 0.12,
    model_avg_wmape = 0.08,
    model_median_wmape = 0.07,
    model_std_wmape = 0.005,
    agent_version = 1,
    stringsAsFactors = FALSE
  )

  result <- coerce_and_bind(prev, curr)

  expect_true(is.numeric(result$weighted_mape))
  expect_true(is.numeric(result$model_avg_wmape))
  expect_true(is.numeric(result$model_median_wmape))
  expect_true(is.numeric(result$model_std_wmape))
  expect_true(is.numeric(result$agent_version))
  expect_equal(nrow(result), 3)
})

# * Test: missing numeric columns filled by bind_rows remain numeric ----
test_that("coerce_and_bind keeps numeric type when previous_runs lacks some numeric columns", {
  prev <- data.frame(
    run_name = "run1",
    weighted_mape = 0.20,
    agent_version = 1,
    stringsAsFactors = FALSE
  )
  curr <- data.frame(
    run_name = "run2",
    weighted_mape = 0.12,
    model_avg_wmape = 0.08,
    agent_version = 1,
    stringsAsFactors = FALSE
  )

  result <- coerce_and_bind(prev, curr)

  expect_true(is.numeric(result$weighted_mape))
  expect_true(is.numeric(result$agent_version))
  # model_avg_wmape was filled with NA for run1 by bind_rows but should be numeric
  expect_true(is.numeric(result$model_avg_wmape))
  expect_true(is.na(result$model_avg_wmape[[1]]))
  expect_equal(result$model_avg_wmape[[2]], 0.08)
})

# * Test: arithmetic on weighted_mape works after coercion ----
test_that("arithmetic on weighted_mape works after coerce_and_bind (reproduces original bug)", {
  prev <- data.frame(
    run_name = c("run1", "run2"),
    weighted_mape = c("0.15", "0.20"),
    agent_version = c("1", "1"),
    stringsAsFactors = FALSE
  )
  curr <- data.frame(
    run_name = "run3",
    weighted_mape = 0.12,
    agent_version = 1,
    stringsAsFactors = FALSE
  )

  result <- coerce_and_bind(prev, curr)

  # this is the operation that was failing with "non-numeric argument to binary operator"
  best_wmape <- min(result$weighted_mape)
  expect_no_error(
    abs(result$weighted_mape - best_wmape) <= best_wmape * 0.10
  )
  expect_true(is.logical(abs(result$weighted_mape - best_wmape) <= best_wmape * 0.10))
})

# * Test: logical columns from vroom reading NA-only columns get coerced ----
test_that("coerce_and_bind handles logical-typed columns from vroom NA inference", {
  # vroom may infer an all-NA column as logical
  prev <- data.frame(
    run_name = "run1",
    weighted_mape = 0.20,
    model_avg_wmape = NA,
    agent_version = 1,
    stringsAsFactors = FALSE
  )
  # model_avg_wmape is logical (NA) in prev, numeric in curr
  curr <- data.frame(
    run_name = "run2",
    weighted_mape = 0.12,
    model_avg_wmape = 0.08,
    agent_version = 1,
    stringsAsFactors = FALSE
  )

  result <- coerce_and_bind(prev, curr)

  expect_true(is.numeric(result$model_avg_wmape))
  expect_true(is.na(result$model_avg_wmape[[1]]))
  expect_equal(result$model_avg_wmape[[2]], 0.08)
})
