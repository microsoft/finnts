test_that("read_file reports a missing artifact instead of subscripting an empty list", {
  run_info <- list(
    storage_object = NULL,
    path = withr::local_tempdir(),
    data_output = "csv",
    object_output = "rds"
  )

  expect_error(
    read_file(
      run_info = run_info,
      file_list = character(0),
      return_type = "df"
    ),
    paste0(
      "No files matched the requested Finn artifact.*",
      "missing, incomplete, or stored under an inconsistent cached identifier"
    )
  )
})

test_that("read_file retains matched empty CSV behavior", {
  output_path <- withr::local_tempfile(fileext = ".csv")
  file.create(output_path)
  run_info <- list(
    storage_object = NULL,
    path = fs::path_dir(output_path),
    data_output = "csv",
    object_output = "rds"
  )

  expect_no_error(
    result <- suppressWarnings(read_file(
      run_info = run_info,
      file_list = output_path,
      return_type = "df"
    ))
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("read_file permits an explicitly optional missing table", {
  run_info <- list(
    storage_object = NULL,
    path = withr::local_tempdir(),
    data_output = "csv",
    object_output = "rds"
  )

  result <- read_file(
    run_info = run_info,
    file_list = character(0),
    return_type = "df",
    allow_missing = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("strict CSV fallback preserves valid empty and populated files", {
  run_info <- artifact_test_run(withr::local_tempdir())
  paths <- file.path(run_info$path, paste0("fallback-", seq_len(3), ".csv"))
  file.create(paths[[1]])
  writeLines("value", paths[[2]])
  utils::write.csv(data.frame(value = c(2, 7)), paths[[3]], row.names = FALSE)
  testthat::local_mocked_bindings(
    vroom = function(...) stop("exercise compatible CSV fallback", call. = FALSE),
    .package = "vroom"
  )

  strict_result <- read_file(run_info, file_list = paths, strict = TRUE)
  legacy_result <- read_file(run_info, file_list = paths)

  expect_equal(strict_result, tibble::tibble(value = c(2L, 7L)))
  expect_equal(strict_result, legacy_result)
  expect_equal(nrow(read_file(run_info, file_list = paths[1:2], strict = TRUE)), 0L)
})

test_that("strict CSV fallback keeps metadata exceptions as errors", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_recipe(run_info, "A", "R1")
  path <- artifact_test_path(run_info, "prep_data", "A", "-R1")
  original_info <- file.info
  testthat::local_mocked_bindings(
    vroom = function(...) stop("injected CSV read failure", call. = FALSE),
    .package = "vroom"
  )
  testthat::local_mocked_bindings(
    file.info = function(files, ...) {
      if (any(as.character(files) == as.character(path))) {
        stop("injected metadata I/O failure", call. = FALSE)
      }
      original_info(files, ...)
    },
    .package = "base"
  )

  expect_error(read_local_artifacts(run_info, path), "injected metadata I/O failure")
  expect_error(read_local_artifacts(run_info, path, allow_missing = TRUE),
    "injected metadata I/O failure")
  expect_warning(legacy <- read_file(run_info, file_list = path),
    "Skipping empty or unreadable file")
  expect_equal(nrow(legacy), 0L)
})
