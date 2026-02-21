# tests/testthat/test-project_info.R

test_that("set_project_info creates project info with defaults", {
  project_info <- set_project_info(
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month"
  )

  expect_type(project_info, "list")
  expect_true("project_name" %in% names(project_info))
  expect_true("combo_variables" %in% names(project_info))
  expect_true("target_variable" %in% names(project_info))
  expect_true("date_type" %in% names(project_info))
  expect_true("path" %in% names(project_info))
  expect_true("created" %in% names(project_info))
  expect_true("data_output" %in% names(project_info))
  expect_true("object_output" %in% names(project_info))
  expect_equal(project_info$project_name, "finn_project")
  expect_equal(project_info$data_output, "csv")
  expect_equal(project_info$object_output, "rds")
  expect_equal(project_info$combo_variables, c("id"))
  expect_equal(project_info$target_variable, "value")
  expect_equal(project_info$date_type, "month")
})

test_that("set_project_info creates project info with custom values", {
  project_info <- set_project_info(
    project_name = "my_project",
    combo_variables = c("Store", "Product"),
    target_variable = "Sales",
    date_type = "week",
    fiscal_year_start = 7,
    weekly_to_daily = FALSE,
    data_output = "parquet",
    object_output = "qs2"
  )

  expect_equal(project_info$project_name, "my_project")
  expect_equal(project_info$combo_variables, c("Store", "Product"))
  expect_equal(project_info$target_variable, "Sales")
  expect_equal(project_info$date_type, "week")
  expect_equal(project_info$fiscal_year_start, 7)
  expect_equal(project_info$weekly_to_daily, FALSE)
  expect_equal(project_info$data_output, "parquet")
  expect_equal(project_info$object_output, "qs2")
})

test_that("set_project_info errors on invalid data_output", {
  expect_error(
    set_project_info(
      combo_variables = c("id"),
      target_variable = "value",
      date_type = "month",
      data_output = "json"
    ),
    "invalid value for input name 'data_output'"
  )
})

test_that("set_project_info errors on invalid object_output", {
  expect_error(
    set_project_info(
      combo_variables = c("id"),
      target_variable = "value",
      date_type = "month",
      object_output = "pickle"
    ),
    "invalid value for input name 'object_output'"
  )
})

test_that("set_project_info errors on invalid project_name type", {
  expect_error(
    set_project_info(
      project_name = 123,
      combo_variables = c("id"),
      target_variable = "value",
      date_type = "month"
    ),
    "`project_name` must either be a NULL or a string"
  )
})

test_that("set_project_info errors on invalid date_type", {
  expect_error(
    set_project_info(
      combo_variables = c("id"),
      target_variable = "value",
      date_type = "hourly"
    ),
    "invalid value for input name 'date_type'"
  )
})

test_that("set_project_info accepts all valid date types", {
  for (dt in c("year", "quarter", "month", "week", "day")) {
    project_info <- set_project_info(
      project_name = paste0("date_type_test_", dt),
      combo_variables = c("id"),
      target_variable = "value",
      date_type = dt
    )
    expect_equal(project_info$date_type, dt)
  }
})

test_that("set_project_info errors on invalid combo_variables type", {
  expect_error(
    set_project_info(
      combo_variables = 123,
      target_variable = "value",
      date_type = "month"
    ),
    "invalid type for input name 'combo_variables'"
  )
})

test_that("set_project_info errors on invalid target_variable type", {
  expect_error(
    set_project_info(
      combo_variables = c("id"),
      target_variable = 123,
      date_type = "month"
    ),
    "invalid type for input name 'target_variable'"
  )
})

test_that("set_project_info errors on invalid weekly_to_daily type", {
  expect_error(
    set_project_info(
      combo_variables = c("id"),
      target_variable = "value",
      date_type = "month",
      weekly_to_daily = "yes"
    ),
    "invalid type for input name 'weekly_to_daily'"
  )
})

test_that("set_project_info errors on invalid path type", {
  expect_error(
    set_project_info(
      combo_variables = c("id"),
      target_variable = "value",
      date_type = "month",
      path = 123
    ),
    "`path` must either be a NULL or a string"
  )
})

test_that("set_project_info errors on invalid storage_object type", {
  expect_error(
    set_project_info(
      combo_variables = c("id"),
      target_variable = "value",
      date_type = "month",
      storage_object = "invalid"
    ),
    "`storage_object` must either be a NULL"
  )
})

test_that("set_project_info with explicit path creates directories", {
  temp_dir <- tempfile("project_info_test")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  project_info <- set_project_info(
    path = temp_dir,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month"
  )

  expect_true(dir.exists(file.path(temp_dir, "eda")))
  expect_true(dir.exists(file.path(temp_dir, "input_data")))
  expect_true(dir.exists(file.path(temp_dir, "logs")))
  expect_true(dir.exists(file.path(temp_dir, "final_output")))
  expect_true(dir.exists(file.path(temp_dir, "prep_data")))
  expect_true(dir.exists(file.path(temp_dir, "prep_models")))
  expect_true(dir.exists(file.path(temp_dir, "models")))
  expect_true(dir.exists(file.path(temp_dir, "forecasts")))
})

test_that("set_project_info with overwrite re-creates project", {
  temp_dir <- tempfile("project_info_test")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  project_info1 <- set_project_info(
    project_name = "overwrite_test",
    path = temp_dir,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month"
  )

  # second call with same inputs should use existing
  project_info2 <- set_project_info(
    project_name = "overwrite_test",
    path = temp_dir,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month"
  )

  expect_equal(project_info2$project_name, "overwrite_test")
})

test_that("set_project_info errors when inputs change without overwrite", {
  temp_dir <- tempfile("project_info_test")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  set_project_info(
    project_name = "change_test",
    path = temp_dir,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month"
  )

  expect_error(
    set_project_info(
      project_name = "change_test",
      path = temp_dir,
      combo_variables = c("id"),
      target_variable = "revenue",
      date_type = "month"
    ),
    "Inputs have recently changed"
  )
})

test_that("set_project_info overwrite allows changing inputs", {
  temp_dir <- tempfile("project_info_test")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  set_project_info(
    project_name = "overwrite_change_test",
    path = temp_dir,
    combo_variables = c("id"),
    target_variable = "value",
    date_type = "month"
  )

  project_info <- set_project_info(
    project_name = "overwrite_change_test",
    path = temp_dir,
    combo_variables = c("id"),
    target_variable = "revenue",
    date_type = "month",
    overwrite = TRUE
  )

  expect_equal(project_info$target_variable, "revenue")
})
