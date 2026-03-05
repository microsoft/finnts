# tests/testthat/test-load_run_results.R
# Tests for numeric type safety in the type-coercion logic used by load_run_results()
#
# These tests exercise the coercion + bind_rows logic directly, without
# calling the full load_run_results() (which needs disk I/O via get_run_info).

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
