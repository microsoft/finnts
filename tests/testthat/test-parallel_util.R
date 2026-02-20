# tests/testthat/test-parallel_util.R

test_that("get_cores returns cores minus 1 when num_cores is NULL", {
  result <- get_cores(NULL)

  expect_equal(result, parallel::detectCores() - 1)
})

test_that("get_cores respects num_cores limit", {
  result <- get_cores(2)

  expect_true(result <= 2)
  expect_true(result <= parallel::detectCores() - 1)
})

test_that("get_cores caps at available cores minus 1", {
  result <- get_cores(1000)

  expect_equal(result, parallel::detectCores() - 1)
})

test_that("par_start returns sequential operator when parallel_processing is NULL", {
  run_info <- list(
    storage_object = NULL,
    path = tempdir(),
    data_output = "csv",
    object_output = "rds"
  )

  par_info <- par_start(
    run_info = run_info,
    parallel_processing = NULL,
    num_cores = NULL,
    task_length = 5
  )

  expect_type(par_info, "list")
  expect_true("packages" %in% names(par_info))
  expect_true("foreach_operator" %in% names(par_info))
  expect_true("cl" %in% names(par_info))
  expect_null(par_info$cl)
  expect_true(is.function(par_info$foreach_operator))
})

test_that("par_start returns correct packages for sequential processing", {
  run_info <- list(
    storage_object = NULL,
    path = tempdir(),
    data_output = "csv",
    object_output = "rds"
  )

  par_info <- par_start(
    run_info = run_info,
    parallel_processing = NULL,
    num_cores = NULL,
    task_length = 5
  )

  expect_true("dplyr" %in% par_info$packages)
  expect_true("tibble" %in% par_info$packages)
  expect_true("recipes" %in% par_info$packages)
})

test_that("par_start with local_machine creates cluster", {
  run_info <- list(
    storage_object = NULL,
    path = tempdir(),
    data_output = "csv",
    object_output = "rds"
  )

  par_info <- par_start(
    run_info = run_info,
    parallel_processing = "local_machine",
    num_cores = 2,
    task_length = 5
  )

  expect_false(is.null(par_info$cl))
  expect_true(is.function(par_info$foreach_operator))

  # clean up
  par_end(par_info$cl)
})

test_that("par_end cleans up cluster", {
  run_info <- list(
    storage_object = NULL,
    path = tempdir(),
    data_output = "csv",
    object_output = "rds"
  )

  par_info <- par_start(
    run_info = run_info,
    parallel_processing = "local_machine",
    num_cores = 2,
    task_length = 3
  )

  # should not error
  expect_no_error(par_end(par_info$cl))
})

test_that("par_end handles NULL cluster gracefully", {
  expect_no_error(par_end(NULL))
})

test_that("cancel_parallel handles none parallel processing", {
  par_info <- list(
    parallel_processing = NULL,
    cl = NULL
  )

  expect_no_error(cancel_parallel(par_info))
})

test_that("cancel_parallel handles local_machine cleanup", {
  run_info <- list(
    storage_object = NULL,
    path = tempdir(),
    data_output = "csv",
    object_output = "rds"
  )

  par_info <- par_start(
    run_info = run_info,
    parallel_processing = "local_machine",
    num_cores = 2,
    task_length = 3
  )

  expect_no_error(cancel_parallel(par_info))
})

test_that("par_start errors on invalid parallel_processing input", {
  run_info <- list(
    storage_object = NULL,
    path = tempdir(),
    data_output = "csv",
    object_output = "rds"
  )

  expect_error(
    par_start(
      run_info = run_info,
      parallel_processing = "invalid",
      num_cores = NULL,
      task_length = 5
    ),
    "error"
  )
})

test_that("par_start adds parquet package when data_output is parquet", {
  run_info <- list(
    storage_object = NULL,
    path = tempdir(),
    data_output = "parquet",
    object_output = "rds"
  )

  par_info <- par_start(
    run_info = run_info,
    parallel_processing = NULL,
    num_cores = NULL,
    task_length = 5
  )

  expect_true("arrow" %in% par_info$packages)
})

test_that("par_start adds qs2 package when object_output is qs2", {
  run_info <- list(
    storage_object = NULL,
    path = tempdir(),
    data_output = "csv",
    object_output = "qs2"
  )

  par_info <- par_start(
    run_info = run_info,
    parallel_processing = NULL,
    num_cores = NULL,
    task_length = 5
  )

  expect_true("qs2" %in% par_info$packages)
})
