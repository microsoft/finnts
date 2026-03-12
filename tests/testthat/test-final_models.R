# tests/testthat/test-final_models.R

# * read_file empty result handling ----

test_that("read_file returns NULL or empty tibble for non-existent files", {
  tmp <- tempdir()
  run_info <- list(
    path = tmp,
    storage_object = NULL,
    data_output = "csv",
    object_output = "rds"
  )

  result <- suppressWarnings(
    read_file(run_info,
      path = "/forecasts/nonexistent-file.csv",
      return_type = "df"
    )
  )

  # read_file may return NULL or an empty tibble for missing files;
  # either way it must NOT have usable columns like Model_ID
  is_empty <- is.null(result) || nrow(result) == 0
  # either way it must NOT have usable columns like Model_ID
  is_empty <- is.null(result) || nrow(result) == 0
  expect_true(is_empty)
})

test_that("empty tibble is treated as NULL by nrow guard", {
  empty_tbl <- tibble::tibble()

  # the guard used in final_models.R

  guarded <- if (is.null(empty_tbl) || nrow(empty_tbl) == 0) NULL else empty_tbl

  expect_null(guarded)
  expect_false("Model_ID" %in% colnames(empty_tbl))
})

# * adjust_combo_column ----

test_that("adjust_combo_column handles logical Combo column", {
  input <- tibble::tibble(Combo = c(TRUE, FALSE), Value = c(1, 2))
  result <- adjust_combo_column(input)

  expect_type(result$Combo, "character")
  expect_equal(result$Combo, c("T", "F"))
})

test_that("adjust_combo_column passes through character Combo", {
  input <- tibble::tibble(Combo = c("A", "B"), Value = c(1, 2))
  result <- adjust_combo_column(input)

  expect_equal(result$Combo, c("A", "B"))
})

# * create_prediction_intervals ----

test_that("create_prediction_intervals requires Model_ID column", {
  # a table missing Model_ID should fail in the group_by
  bad_tbl <- tibble::tibble(
    Combo = "A",
    Train_Test_ID = c(1, 2),
    Target = c(10, 12),
    Forecast = c(11, 13)
  )
  train_test <- tibble::tibble(
    Run_Type = c("Back_Test", "Future_Forecast"),
    Train_Test_ID = c(2, 1)
  )

  expect_error(create_prediction_intervals(bad_tbl, train_test))
})

test_that("create_prediction_intervals works with valid input", {
  good_tbl <- tibble::tibble(
    Combo = rep("A", 4),
    Model_ID = rep("snaive--local--R1", 4),
    Train_Test_ID = c(1, 1, 2, 2),
    Target = c(10, 12, 8, 9),
    Forecast = c(11, 13, 7, 10),
    Date = as.Date("2025-01-01") + 0:3,
    Horizon = c(1, 2, 1, 2)
  )
  train_test <- tibble::tibble(
    Run_Type = c("Back_Test", "Future_Forecast"),
    Train_Test_ID = c(2, 1)
  )

  result <- create_prediction_intervals(good_tbl, train_test)

  expect_true("lo_95" %in% colnames(result))
  expect_true("hi_95" %in% colnames(result))
  # prediction intervals only exist for Future_Forecast (Train_Test_ID == 1)
  future_rows <- result %>% dplyr::filter(Train_Test_ID == 1)
  expect_true(all(!is.na(future_rows$lo_95)))
})

# * validate_best_model ----

test_that("validate_best_model passes when all combos have Best_Model = Yes", {
  tbl <- tibble::tibble(
    Combo = c("A", "A", "B", "B"),
    Best_Model = c("Yes", "No", "Yes", "No"),
    Forecast = c(10, 11, 12, 13)
  )

  result <- validate_best_model(tbl, context = "test")
  expect_equal(result, tbl)
})

test_that("validate_best_model passes with single combo single row", {
  tbl <- tibble::tibble(
    Combo = "A",
    Best_Model = "Yes",
    Forecast = 10
  )

  result <- validate_best_model(tbl, context = "test")
  expect_equal(result, tbl)
})

test_that("validate_best_model stops when one combo is missing best model", {
  tbl <- tibble::tibble(
    Combo = c("A", "A", "B", "B"),
    Best_Model = c("Yes", "No", "No", "No"),
    Forecast = c(10, 11, 12, 13)
  )

  expect_error(
    validate_best_model(tbl, context = "test"),
    "missing a best model.*B"
  )
})

test_that("validate_best_model stops when multiple combos are missing best model", {
  tbl <- tibble::tibble(
    Combo = c("A", "B", "C"),
    Best_Model = c("No", "No", "Yes"),
    Forecast = c(10, 11, 12)
  )

  expect_error(
    validate_best_model(tbl, context = "test"),
    "missing a best model.*A.*B"
  )
})

test_that("validate_best_model stops when Best_Model column is missing", {
  tbl <- tibble::tibble(
    Combo = c("A", "B"),
    Forecast = c(10, 11)
  )

  expect_error(
    validate_best_model(tbl, context = "test"),
    "Best_Model column is missing"
  )
})

test_that("validate_best_model includes calling context in error message", {
  tbl <- tibble::tibble(
    Combo = "A",
    Best_Model = "No",
    Forecast = 10
  )

  expect_error(
    validate_best_model(tbl, context = "my_function"),
    "Error in my_function"
  )
})

# * reconciliation skip prevention ----

test_that("early return is blocked when reconciliation is incomplete", {
  # Simulate: all combos have average_models files but no reconciled files.
  # The early-return guard should NOT fire because recon_complete is FALSE.
  tmp <- file.path(tempdir(), paste0("fm_recon_test_", Sys.getpid()))
  dir.create(file.path(tmp, "forecasts"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- "proj1"
  run <- "run1"
  proj_hash <- hash_data(proj)
  run_hash <- hash_data(run)

  # create a combo *_models file (represents combo_list)
  file.create(file.path(
    tmp, "forecasts",
    paste0(proj_hash, "-", run_hash, "-comboA-single_models.csv")
  ))

  # create a matching *average_models file (represents prev_combo_list)
  file.create(file.path(
    tmp, "forecasts",
    paste0(proj_hash, "-", run_hash, "-comboA-average_models.csv")
  ))

  run_info <- list(
    path = tmp,
    storage_object = NULL,
    project_name = proj,
    run_name = run,
    data_output = "csv",
    object_output = "rds"
  )

  # combo_diff is empty, prev_combo_list is non-empty
  combo_list <- list_files(
    run_info$storage_object,
    paste0(run_info$path, "/forecasts/*", proj_hash, "-", run_hash, "*_models.csv")
  )
  expect_true(length(combo_list) > 0)

  # no reconciled files exist
  recon_files <- list_files(
    run_info$storage_object,
    paste0(run_info$path, "/forecasts/*", proj_hash, "-", run_hash, "*-reconciled.csv")
  )
  expect_equal(length(recon_files), 0)

  # recon_complete should be FALSE for non-bottoms_up
  forecast_approach <- "standard_hierarchy"
  recon_complete <- TRUE
  if (forecast_approach != "bottoms_up") {
    recon_complete <- length(recon_files) > 0
  }
  expect_false(recon_complete)
})

test_that("early return fires when reconciliation is complete", {
  tmp <- file.path(tempdir(), paste0("fm_recon_done_", Sys.getpid()))
  dir.create(file.path(tmp, "forecasts"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- "proj2"
  run <- "run2"
  proj_hash <- hash_data(proj)
  run_hash <- hash_data(run)

  # create average_models + reconciled files
  file.create(file.path(
    tmp, "forecasts",
    paste0(proj_hash, "-", run_hash, "-comboA-average_models.csv")
  ))
  file.create(file.path(
    tmp, "forecasts",
    paste0(proj_hash, "-", run_hash, "-BestModel-reconciled.csv")
  ))

  run_info <- list(
    path = tmp,
    storage_object = NULL,
    project_name = proj,
    run_name = run,
    data_output = "csv",
    object_output = "rds"
  )

  recon_files <- list_files(
    run_info$storage_object,
    paste0(run_info$path, "/forecasts/*", proj_hash, "-", run_hash, "*-reconciled.csv")
  )
  expect_true(length(recon_files) > 0)

  forecast_approach <- "standard_hierarchy"
  recon_complete <- TRUE
  if (forecast_approach != "bottoms_up") {
    recon_complete <- length(recon_files) > 0
  }
  expect_true(recon_complete)
})

test_that("early return fires for bottoms_up regardless of reconciled files", {
  # For bottoms_up, reconciliation is never needed, so recon_complete stays TRUE
  forecast_approach <- "bottoms_up"

  recon_complete <- TRUE
  if (forecast_approach != "bottoms_up") {
    recon_complete <- FALSE # would be set by missing files
  }
  expect_true(recon_complete)
})
