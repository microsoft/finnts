for (format in c("csv", "parquet", "rds")) {
  test_that(paste("single-combo prepared-data getters avoid discovery for", format), {
    if (format == "parquet") skip_if_not_installed("arrow")
    run_info <- artifact_test_run(withr::local_tempdir(), format)
    run_info$combo <- hash_data("A")
    artifact_test_log(run_info, "R2", combo_variables = "ID")
    expected <- artifact_test_recipe(run_info, "A", "R2")
    tracker <- local_artifact_spies()

    result <- get_prepped_data(run_info, "R2")

    expect_equal(result$Target, expected$Target)
    expect_identical(result$ID, rep("A", 3))
    expect_equal(tracker$listings, 0L)
  })
}

test_that("single-combo forecast getter retains every existing model output", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_log(run_info, combo_variables = "ID", forecast_approach = "bottoms_up")
  artifact_test_splits(run_info)
  artifact_test_forecast(run_info)
  artifact_test_forecast(run_info, suffix = "ensemble_models", model = "ensemble", best = "No")
  artifact_test_forecast(run_info, suffix = "average_models", model = "average", best = "No")
  expected <- get_forecast_data(run_info)
  run_info$combo <- hash_data("A")
  tracker <- local_artifact_spies(max_listings = 1L)

  result <- get_forecast_data(run_info)

  expect_equal(result, expected)
  expect_equal(tracker$listings, 1L)
})

test_that("trained model getter reads exactly the known combo model files", {
  run_info <- artifact_test_run(withr::local_tempdir())
  run_info$combo <- hash_data("A")
  for (suffix in c("single_models", "ensemble_models")) {
    write_data(tibble::tibble(Model_ID = suffix, Model_Fit = list(list(value = 1))),
      "A", run_info, "object", "models", paste0("-", suffix)
    )
  }
  write_data(tibble::tibble(Model_ID = "unrelated"), "B", run_info, "object", "models", "-single_models")
  tracker <- local_artifact_spies()

  result <- get_trained_models(run_info)

  expect_setequal(result$Model_ID, c("single_models", "ensemble_models"))
  expect_equal(tracker$listings, 0L)
  expect_length(tracker$reads, 1L)
})

test_that("temporary roots support named logs and prepared-data getters", {
  run_info <- artifact_test_run(NULL)
  run_info$combo <- hash_data("A")
  expected_log <- artifact_test_log(run_info, combo_variables = "ID")
  expected <- artifact_test_recipe(run_info, "A", "R1")
  tracker <- local_artifact_spies()

  result_log <- get_run_info(run_info$project_name, run_info$run_name, path = NULL)
  result <- get_prepped_data(run_info, "R1")

  expect_equal(result_log, expected_log)
  expect_equal(result$Target, expected$Target)
  expect_equal(tracker$listings, 0L)
})

for (format in c("csv", "parquet", "rds")) {
  test_that(paste("exact table reads combine only supplied files for", format), {
    if (format == "parquet") skip_if_not_installed("arrow")
    root <- fs::path(withr::local_tempdir(), "synfs", paste0("mount ", intToUtf8(233)))
    run_info <- artifact_test_run(root, format)
    expected <- dplyr::bind_rows(
      artifact_test_recipe(run_info, "A", "R1", 10),
      artifact_test_recipe(run_info, "B", "R1", 20)
    )
    paths <- vapply(c("A", "B"), function(combo) {
      as.character(artifact_test_path(run_info, "prep_data", combo, "-R1"))
    }, character(1))
    tracker <- local_artifact_spies()

    result <- read_local_artifacts(run_info, paths)

    expect_equal(result, expected)
    expect_equal(tracker$listings, 0L)
    expect_length(tracker$reads, 1L)
  })
}

test_that("missing required and optional artifacts remain distinct", {
  run_info <- artifact_test_run(withr::local_tempdir())
  missing <- artifact_test_path(run_info, "prep_data", "A", "-R1")
  tracker <- local_artifact_spies()

  expect_error(read_local_artifacts(run_info, missing), "Missing required Finn artifact")
  expect_error(read_local_artifacts(run_info, character()), "No files matched")
  expect_equal(nrow(read_local_artifacts(run_info, missing, allow_missing = TRUE)), 0L)
  expect_null(read_local_artifacts(run_info, missing, return_type = "object", allow_missing = TRUE))
  expect_equal(tracker$listings, 0L)
})

test_that("empty CSV artifacts remain readable", {
  run_info <- artifact_test_run(withr::local_tempdir())
  for (contents in list(character(), "value")) {
    path <- withr::local_tempfile(fileext = ".csv")
    writeLines(contents, path)
    expect_equal(nrow(suppressWarnings(read_local_artifacts(run_info, path))), 0L)
  }
})

for (extension in c("rds", "parquet")) {
  test_that(paste("corrupt binary artifacts fail for", extension), {
    if (extension == "parquet") skip_if_not_installed("arrow")
    run_info <- artifact_test_run(withr::local_tempdir())
    path <- withr::local_tempfile(fileext = paste0(".", extension))
    writeLines("not a valid binary artifact", path)
    expect_error(read_local_artifacts(run_info, path))
  })
}

test_that("optional artifacts do not hide read failures", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_recipe(run_info, "A", "R1")
  path <- artifact_test_path(run_info, "prep_data", "A", "-R1")
  testthat::local_mocked_bindings(
    read_file = function(...) stop("permission denied", call. = FALSE),
    .package = "finnts"
  )

  expect_error(read_local_artifacts(run_info, path, allow_missing = TRUE), "permission denied")
})

test_that("thousands of unrelated files do not affect known-combo reads", {
  run_info <- artifact_test_run(withr::local_tempdir())
  expected <- artifact_test_recipe(run_info, "A", "R1")
  unrelated <- fs::path(run_info$path, "prep_data", paste0("unrelated-", seq_len(2500), ".csv"))
  expect_true(all(file.create(unrelated)))
  tracker <- local_artifact_spies()

  result <- get_recipe_data(run_info, hash_data("A"), recipes = "R1")

  expect_equal(result$Data[[1]], expected)
  expect_equal(tracker$listings, 0L)
  expect_length(tracker$reads, 1L)
})

test_that("optional artifact checks propagate metadata failures", {
  run_info <- artifact_test_run(withr::local_tempdir())
  missing <- artifact_test_path(run_info, "prep_data", "A", "-R1")
  testthat::local_mocked_bindings(
    file_info = function(...) stop("metadata permission denied", call. = FALSE),
    .package = "fs"
  )

  expect_error(read_local_artifacts(run_info, missing, allow_missing = TRUE),
    "metadata permission denied"
  )
})

test_that("empty combo selections do not manufacture a singleton path", {
  run_info <- artifact_test_run(withr::local_tempdir())

  expect_length(local_artifact_path(run_info, "models", "-model_summary", character()), 0L)
})

test_that("prepared-model metadata is read correctly from the default temporary root", {
  run_info <- artifact_test_run(NULL, "rds")
  writer_info <- run_info
  writer_info$path <- tempdir()
  for (suffix in c("-train_test_split", "-model_hyperparameters", "-model_workflows")) {
    write_data(tibble::tibble(value = 1), NULL, writer_info, "object", "prep_models", suffix)
  }
  tracker <- local_artifact_spies()

  result <- get_prepped_models(run_info)

  expect_setequal(result$Type, c("Model_Workflows", "Model_Hyperparameters", "Train_Test_Splits"))
  expect_true(all(vapply(result$Data, function(data) identical(data$value, 1), logical(1))))
  expect_length(tracker$reads, 3L)
})

test_that("single-combo forecast getters preserve condensed-output precedence", {
  run_info <- artifact_test_run(withr::local_tempdir())
  run_info$combo <- hash_data("A")
  artifact_test_log(run_info, combo_variables = "ID", forecast_approach = "bottoms_up")
  artifact_test_splits(run_info)
  condensed <- artifact_test_forecast(run_info)
  condensed$Forecast <- 200
  write_data(condensed, "batch_1", run_info, "data", "forecasts", "-condensed")
  tracker <- local_artifact_spies(max_listings = 1L)

  result <- get_forecast_data(run_info)

  expect_equal(result$Forecast, 200)
  expect_equal(tracker$listings, 1L)
})

test_that("single-combo forecast getters can read condensed-only output", {
  run_info <- artifact_test_run(withr::local_tempdir())
  run_info$combo <- hash_data("A")
  artifact_test_log(run_info, combo_variables = "ID", forecast_approach = "bottoms_up")
  artifact_test_splits(run_info)
  source_run <- artifact_test_run(withr::local_tempdir())
  condensed <- artifact_test_forecast(source_run)
  write_data(condensed, "batch_1", run_info, "data", "forecasts", "-condensed")
  tracker <- local_artifact_spies(max_listings = 1L)

  result <- get_forecast_data(run_info)

  expect_equal(result$Forecast, condensed$Forecast)
  expect_equal(tracker$listings, 1L)
})

test_that("directories at artifact paths are hard errors even when optional", {
  run_info <- artifact_test_run(withr::local_tempdir())
  directory <- artifact_test_path(run_info, "prep_data", "A", "-R1")
  fs::dir_create(directory)

  expect_error(read_local_artifacts(run_info, directory, allow_missing = TRUE),
    "not a regular file"
  )
})

test_that("strict local CSV reads propagate unavailable metadata after preflight", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_recipe(run_info, "A", "R1")
  path <- artifact_test_path(run_info, "prep_data", "A", "-R1")
  testthat::local_mocked_bindings(
    vroom = function(...) stop("injected CSV read failure", call. = FALSE),
    .package = "vroom"
  )
  testthat::local_mocked_bindings(
    file.info = function(...) data.frame(size = NA_real_),
    .package = "base"
  )

  expect_error(read_local_artifacts(run_info, path), "Cannot read Finn CSV artifact")
  expect_error(read_local_artifacts(run_info, path, allow_missing = TRUE),
    "Cannot read Finn CSV artifact"
  )
  expect_equal(nrow(read_file(run_info, file_list = path)), 0L)
})

test_that("strict local CSV reads propagate fallback read errors", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_recipe(run_info, "A", "R1")
  path <- artifact_test_path(run_info, "prep_data", "A", "-R1")
  testthat::local_mocked_bindings(
    vroom = function(...) stop("injected CSV read failure", call. = FALSE),
    .package = "vroom"
  )
  testthat::local_mocked_bindings(
    read.csv = function(...) stop("injected fallback permission error", call. = FALSE),
    .package = "utils"
  )

  expect_error(read_local_artifacts(run_info, path), "injected fallback permission error")
  expect_warning(legacy <- read_file(run_info, file_list = path),
    "Skipping empty or unreadable file"
  )
  expect_equal(nrow(legacy), 0L)
})

test_that("strict local multi-file reads cannot silently lose a failed CSV", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_recipe(run_info, "A", "R1")
  artifact_test_recipe(run_info, "B", "R1")
  paths <- c(
    artifact_test_path(run_info, "prep_data", "A", "-R1"),
    artifact_test_path(run_info, "prep_data", "B", "-R1")
  )
  original_info <- file.info
  testthat::local_mocked_bindings(
    vroom = function(...) stop("injected multi-file read failure", call. = FALSE),
    .package = "vroom"
  )
  testthat::local_mocked_bindings(
    file.info = function(path, ...) {
      if (identical(as.character(path), as.character(paths[[2]]))) {
        return(data.frame(size = NA_real_))
      }
      original_info(path, ...)
    },
    .package = "base"
  )

  expect_error(read_local_artifacts(run_info, paths), "Cannot read Finn CSV artifact")
  expect_identical(unique(read_file(run_info, file_list = paths)$Combo), "A")
})

test_that("later condensed batches take precedence even when batch one is absent", {
  run_info <- artifact_test_run(withr::local_tempdir())
  run_info$combo <- hash_data("A")
  artifact_test_log(run_info, combo_variables = "ID", forecast_approach = "bottoms_up")
  artifact_test_splits(run_info)
  condensed <- artifact_test_forecast(run_info)
  condensed$Forecast <- 200
  write_data(condensed, "batch_2", run_info, "data", "forecasts", "-condensed")
  tracker <- local_artifact_spies(max_listings = 1L)

  result <- get_forecast_data(run_info)

  expect_equal(result$Forecast, 200)
  expect_equal(tracker$listings, 1L)
})

test_that("condensed batches are consumed once regardless of discovery order", {
  run_info <- artifact_test_run(withr::local_tempdir())
  run_info$combo <- hash_data("A")
  artifact_test_log(run_info, combo_variables = "ID", forecast_approach = "bottoms_up")
  artifact_test_splits(run_info)
  forecast <- artifact_test_forecast(run_info)
  paths <- character()
  for (batch in c(2L, 17L)) {
    forecast$Forecast <- batch * 100
    forecast$Date <- as.Date("2024-02-01") + batch
    write_data(forecast, paste0("batch_", batch), run_info, "data", "forecasts", "-condensed")
    paths <- c(paths, as.character(artifact_test_path(run_info, "forecasts",
      paste0("batch_", batch), "-condensed")))
  }
  listing_calls <- 0L
  testthat::local_mocked_bindings(
    list_files = function(...) {
      listing_calls <<- listing_calls + 1L
      rev(paths)
    },
    .package = "finnts"
  )
  original_read <- read_file
  read_paths <- character()
  testthat::local_mocked_bindings(
    read_file = function(run_info, path = NULL, file_list = NULL, ...) {
      if (!is.null(path) && grepl("train_test_split", path, fixed = TRUE)) {
        return(tibble::tibble(Run_Type = "Future_Forecast", Train_Test_ID = 1))
      }
      read_paths <<- c(read_paths, as.character(file_list))
      original_read(run_info, path, file_list, ...)
    },
    .package = "finnts"
  )

  result <- get_forecast_data(run_info)

  expect_equal(result$Forecast, c(200, 1700))
  expect_identical(listing_calls, 1L)
  expect_equal(sum(read_paths %in% paths), 2L)
  expect_equal(anyDuplicated(read_paths[read_paths %in% paths]), 0L)
})

test_that("known model candidates are validated once and read once", {
  run_info <- artifact_test_run(withr::local_tempdir())
  run_info$combo <- hash_data("A")
  write_data(tibble::tibble(Model_ID = "meanf", Model_Fit = list(list(value = 1))),
    "A", run_info, "object", "models", "-single_models"
  )
  model_path <- as.character(artifact_test_path(run_info, "models", "A", "-single_models", "rds"))
  tracker <- local_artifact_spies()

  result <- get_trained_models(run_info)

  expect_identical(result$Model_ID, "meanf")
  expect_equal(sum(tracker$metadata_paths == model_path), 1L)
  expect_equal(sum(tracker$access_paths == model_path), 1L)
  expect_equal(sum(tracker$payload_paths == model_path), 1L)
  expect_length(tracker$metadata_paths, 4L)
  expect_equal(tracker$directory_listings, 0L)
})

test_that("the I/O spy rejects direct directory enumeration outside list_files", {
  directory <- fs::path(withr::local_tempdir(), "prep_data")
  fs::dir_create(directory)
  tracker <- local_artifact_spies()

  expect_error(fs::dir_ls(directory), "Unexpected low-level directory enumeration")
  expect_error(list.files(directory), "Unexpected low-level directory enumeration")
  expect_error(dir(directory), "Unexpected low-level directory enumeration")
  expect_error(list.dirs(directory), "Unexpected low-level directory enumeration")
  expect_equal(tracker$listings, 0L)
  expect_equal(tracker$directory_listings, 4L)
})

test_that("the I/O spy counts wrapper and filesystem discovery separately", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_recipe(run_info, "A", "R1")
  tracker <- local_artifact_spies(max_listings = 1L)

  files <- local_artifact_inventory(run_info, "prep_data", "-*R*")

  expect_length(files, 1L)
  expect_equal(tracker$listings, 1L)
  expect_equal(tracker$directory_listings, 1L)
  expect_identical(tracker$directory_calls[[1]]$api, "fs::dir_ls")
})