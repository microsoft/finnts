# tests/testthat/test-run_info.R

test_that("set_run_info creates run info with defaults", {
  run_info <- set_run_info()

  expect_type(run_info, "list")
  expect_true("project_name" %in% names(run_info))
  expect_true("run_name" %in% names(run_info))
  expect_true("created" %in% names(run_info))
  expect_true("path" %in% names(run_info))
  expect_true("data_output" %in% names(run_info))
  expect_true("object_output" %in% names(run_info))
  expect_equal(run_info$project_name, "finn_project")
  expect_equal(run_info$data_output, "csv")
  expect_equal(run_info$object_output, "rds")
})

test_that("set_run_info creates run info with custom values", {
  run_info <- set_run_info(
    project_name = "my_project",
    run_name = "test_run",
    data_output = "parquet",
    object_output = "qs2"
  )

  expect_equal(run_info$project_name, "my_project")
  expect_true(grepl("test_run", run_info$run_name))
  expect_equal(run_info$data_output, "parquet")
  expect_equal(run_info$object_output, "qs2")
})

test_that("set_run_info appends unique id by default", {
  run_info <- set_run_info(run_name = "test_run")

  # run_name should be test_run followed by a timestamp
  expect_true(grepl("^test_run-", run_info$run_name))
})

test_that("set_run_info without unique id", {
  run_info <- set_run_info(
    run_name = "fixed_run",
    add_unique_id = FALSE
  )

  expect_equal(run_info$run_name, "fixed_run")
})

test_that("set_run_info errors on invalid data_output", {
  expect_error(
    set_run_info(data_output = "json"),
    "invalid value for input name 'data_output'"
  )
})

test_that("set_run_info errors on invalid object_output", {
  expect_error(
    set_run_info(object_output = "pickle"),
    "invalid value for input name 'object_output'"
  )
})

test_that("set_run_info errors on invalid run_name type", {
  expect_error(
    set_run_info(run_name = 123),
    "`run_name` must either be a NULL or a string"
  )
})

test_that("set_run_info errors on invalid path type", {
  expect_error(
    set_run_info(path = 123),
    "`path` must either be a NULL or a string"
  )
})

test_that("set_run_info errors on invalid storage_object type", {
  expect_error(
    set_run_info(storage_object = "not_valid"),
    "`storage_object` must either be a NULL"
  )
})

test_that("set_run_info with explicit path creates correct directories", {
  temp_dir <- tempfile("run_info_test")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  run_info <- set_run_info(path = temp_dir)

  expect_true(dir.exists(file.path(temp_dir, "prep_data")))
  expect_true(dir.exists(file.path(temp_dir, "prep_models")))
  expect_true(dir.exists(file.path(temp_dir, "models")))
  expect_true(dir.exists(file.path(temp_dir, "forecasts")))
})

test_that("set_run_info creates storage_object in output list", {
  run_info <- set_run_info()

  expect_true("storage_object" %in% names(run_info))
  expect_null(run_info$storage_object)
})

test_that("get_run_info errors on invalid run_name type", {
  expect_error(
    get_run_info(run_name = 123),
    "`run_name` must either be a NULL or a string"
  )
})

test_that("get_run_info errors on invalid path type", {
  expect_error(
    get_run_info(path = 123),
    "`path` must either be a NULL or a string"
  )
})

test_that("get_run_info errors on invalid storage_object type", {
  expect_error(
    get_run_info(storage_object = "not_valid"),
    "`storage_object` must either be a NULL"
  )
})

test_that("get_run_info returns run data after set_run_info", {
  temp_dir <- tempfile("run_info_test")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  run_info <- set_run_info(
    project_name = "test_proj",
    run_name = "my_run",
    path = temp_dir,
    add_unique_id = FALSE
  )

  result <- get_run_info(
    project_name = "test_proj",
    run_name = "my_run",
    path = temp_dir
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_equal(result$project_name, "test_proj")
  expect_equal(result$run_name, "my_run")
})
