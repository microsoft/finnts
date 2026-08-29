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
