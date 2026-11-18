# Mock the Spark settings that spark_conf_values() reads from the connection.
# `values` is a named list of settings, or NULL for an unreadable connection.
local_spark_conf <- function(values, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    spark_conf_values = function(keys) {
      if (is.null(values)) {
        return(NULL)
      }
      stats::setNames(vapply(keys, function(key) {
        if (is.null(values[[key]])) NA_character_ else values[[key]]
      }, character(1)), keys)
    },
    .package = "finnts", .env = .env
  )
}

# Run "spark" stages sequentially on a one-slot Spark connection: foreach runs
# with %do%, and every pool's task count is recorded in `state$tasks`.
local_sequential_spark <- function(state, .env = parent.frame()) {
  state$tasks <- integer()
  state$started <- FALSE
  local_spark_conf(list(
    "spark.executor.cores" = "1", "spark.dynamicAllocation.enabled" = "true",
    "spark.dynamicAllocation.maxExecutors" = "1"
  ), .env = .env)
  testthat::local_mocked_bindings(
    check_parallel_processing = function(...) invisible(NULL),
    par_start = function(run_info, parallel_processing, num_cores, task_length, ...) {
      state$tasks <- c(state$tasks, as.integer(task_length))
      state$started <- TRUE
      list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
    },
    par_end = function(...) invisible(NULL),
    .package = "finnts", .env = .env
  )
}

test_that("short Spark stages group series into at most four tasks per slot", {
  items <- sprintf("s%03d", 1:100)
  local_spark_conf(list(
    "spark.executor.cores" = "2", "spark.dynamicAllocation.enabled" = "true",
    "spark.dynamicAllocation.maxExecutors" = "3"
  ))
  batches <- spark_stage_batches(items, "spark")
  expect_identical(lengths(batches), rep(5L, 20L))
  expect_identical(unlist(batches, use.names = FALSE), items)

  # task.cpus divides an executor's slots; static allocation counts executor.instances
  local_spark_conf(list("spark.executor.cores" = "4", "spark.task.cpus" = "2", "spark.executor.instances" = "2"))
  batches <- spark_stage_batches(items, "spark")
  expect_length(batches, 15L)
  expect_identical(unlist(batches, use.names = FALSE), items)

  # list items, such as summary jobs, keep their elements
  jobs <- lapply(1:40, function(i) tibble::tibble(combo = as.character(i)))
  grouped <- spark_stage_batches(jobs, "spark")
  expect_length(grouped, 14L)
  expect_identical(unlist(grouped, recursive = FALSE), jobs)
})

test_that("short Spark stages keep one series per task when slots are unknown or plentiful", {
  items <- sprintf("s%03d", 1:100)
  singles <- as.list(items)
  settings <- list(
    unreadable = NULL,
    cores_unset = list("spark.dynamicAllocation.enabled" = "true", "spark.dynamicAllocation.maxExecutors" = "3"),
    executors_unbounded = list("spark.executor.cores" = "2", "spark.dynamicAllocation.enabled" = "true"),
    executors_huge = list(
      "spark.executor.cores" = "2", "spark.dynamicAllocation.enabled" = "true",
      "spark.dynamicAllocation.maxExecutors" = "2147483647"
    )
  )
  for (name in names(settings)) {
    local_spark_conf(settings[[name]])
    expect_identical(spark_stage_batches(items, "spark"), singles, label = name)
  }
  local_spark_conf(list("spark.executor.cores" = "2", "spark.executor.instances" = "1"))
  expect_identical(spark_stage_batches(items, "local_machine"), singles)
  expect_identical(spark_stage_batches(items, NULL), singles)
  expect_identical(spark_stage_batches(character(), "spark"), list())
})

# Run the per-series EDA stage on six one-row-per-month input files with the
# analyses replaced by a recorder. `fail` names a series whose analysis
# errors. Returns the recording state.
eda_stage_run <- function(parallel_processing, fail = NULL) {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  series <- paste0("S", 1:6)
  input_info <- agent_info$project_info
  input_info$run_name <- agent_info$run_id
  for (name in series) {
    write_data(tibble::tibble(Combo = name, Date = as.Date("2023-01-01") + c(0, 31, 59), Target = c(1, 2, 3)),
      name, input_info, "data", "input_data")
  }
  hashes <- vapply(series, hash_data, character(1), USE.NAMES = FALSE)
  state <- new.env(parent = emptyenv())
  state$analysed <- character()
  local_sequential_spark(state)
  local_mocked_bindings(
    get_total_combos = function(...) hashes,
    get_finished_eda_combos = function(...) if (state$started) hashes else character(),
    run_acf_analysis = function(input_data, combo_name, ...) {
      if (identical(combo_name, fail)) stop("acf failed for ", combo_name, call. = FALSE)
      state$analysed <- c(state$analysed, combo_name)
    },
    run_pacf_analysis = function(...) NULL,
    run_stationarity_analysis = function(...) NULL,
    run_missing_analysis = function(...) NULL,
    run_outlier_analysis = function(...) NULL,
    run_seasonality_analysis = function(...) NULL,
    run_xreg_analysis = function(...) NULL
  )
  run_all_eda_per_combo(agent_info, parallel_processing, num_cores = 1)
  state
}

test_that("per-series EDA on Spark runs several series per task", {
  batched <- eda_stage_run("spark")
  expect_identical(batched$tasks, 3L)
  expect_setequal(batched$analysed, paste0("S", 1:6))
  expect_length(batched$analysed, 6L)

  sequential <- eda_stage_run(NULL)
  expect_identical(sequential$tasks, 6L)
  expect_setequal(sequential$analysed, paste0("S", 1:6))

  # a failing series still stops the stage
  expect_error(eda_stage_run("spark", fail = "S3"), "acf failed for S3")
})

# Summarize six selected local runs; returns the consolidated summary and the
# recorded pool task counts.
summary_stage_run <- function(parallel_processing) {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  best_runs <- artifact_test_selected_models(agent_info, paste0("S", 1:6))
  local_artifact_summary_mocks(best_runs)
  state <- new.env(parent = emptyenv())
  local_sequential_spark(state)
  summarize_models(agent_info, parallel_processing = parallel_processing, num_cores = 1)
  result <- get_summarized_models(agent_info)
  attr(result, "problems") <- NULL
  list(summary = result[order(result$Combo, result$Model_ID), ], tasks = state$tasks)
}

test_that("model summaries on Spark run several series per task with the same results", {
  batched <- summary_stage_run("spark")
  sequential <- summary_stage_run(NULL)

  expect_identical(batched$tasks, 3L)
  expect_identical(sequential$tasks, 6L)
  rownames(batched$summary) <- NULL
  rownames(sequential$summary) <- NULL
  expect_identical(batched$summary, sequential$summary)
})

# Train naive and snaive models for six monthly series once, then copy the
# logs and prepared and trained artifacts into an isolated run root, so each
# final_models() arm starts from identical inputs. Input: destination path.
# Returns run info pointing there.
final_stage_run_info <- local({
  cache <- new.env(parent = emptyenv())
  function(path) {
    if (is.null(cache$info)) {
      info <- set_run_info(
        project_name = "spark-stage-batches", run_name = "test",
        path = withr::local_tempdir(.local_envir = cache),
        data_output = "csv", object_output = "rds", add_unique_id = FALSE
      )
      dates <- seq(as.Date("2018-01-01"), by = "month", length.out = 30)
      index <- seq_along(dates)
      prep_data(info,
        data.frame(
          Date = rep(dates, 6), id = rep(paste0("S", 1:6), each = 30),
          value = unlist(lapply(1:6, function(i) 100 * i + index + 10 * sin(2 * pi * index / 12)))
        ),
        combo_variables = "id", target_variable = "value", date_type = "month",
        forecast_horizon = 2, recipes_to_run = "R1",
        clean_missing_values = FALSE, clean_outliers = FALSE
      )
      prep_models(info,
        models_to_run = c("naive", "snaive"), run_ensemble_models = FALSE,
        back_test_scenarios = 2, num_hyperparameters = 1, pca = FALSE
      )
      train_models(info, run_local_models = TRUE, run_global_models = FALSE)
      cache$info <- info
    }
    info <- cache$info
    info$path <- path
    for (folder in c("logs", "prep_data", "prep_models", "models", "forecasts")) {
      fs::dir_copy(fs::path(cache$info$path, folder), fs::path(path, folder))
    }
    info
  }
})

test_that("best-model selection on Spark runs several series per task with the same forecasts", {
  skip_on_cran()
  # Select models for a copied run; returns sorted forecasts and pool task counts.
  final_stage_run <- function(parallel_processing) {
    info <- final_stage_run_info(withr::local_tempdir())
    state <- new.env(parent = emptyenv())
    local_sequential_spark(state)
    final_models(info, average_models = TRUE, max_model_average = 2, parallel_processing = parallel_processing)
    forecast <- get_forecast_data(info)
    attr(forecast, "problems") <- NULL
    forecast <- forecast[order(forecast$Combo, forecast$Model_ID, forecast$Train_Test_ID, forecast$Date), ]
    rownames(forecast) <- NULL
    list(forecast = forecast, tasks = state$tasks)
  }
  batched <- final_stage_run("spark")
  sequential <- final_stage_run(NULL)

  expect_identical(batched$tasks[[1]], 3L)
  expect_identical(sequential$tasks[[1]], 6L)
  expect_identical(batched$forecast, sequential$forecast)
})
