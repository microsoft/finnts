# tests/testthat/test-read_write_data.R
# Tests for read_write_data.R functions

# -- hash_data tests --

test_that("hash_data produces consistent hashes", {
  hash1 <- hash_data("test_string")
  hash2 <- hash_data("test_string")

  expect_equal(hash1, hash2)
  expect_type(hash1, "character")
  expect_true(nchar(hash1) > 0)
})

test_that("hash_data produces different hashes for different inputs", {
  hash1 <- hash_data("hello")
  hash2 <- hash_data("world")

  expect_false(hash1 == hash2)
})

test_that("hash_data works with various types", {
  expect_type(hash_data("string"), "character")
  expect_type(hash_data(42), "character")
  expect_type(hash_data(list(a = 1, b = 2)), "character")
  expect_type(hash_data(data.frame(x = 1:3)), "character")
})

# -- write_data_type tests --

test_that("write_data_type writes csv files", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  df <- tibble::tibble(a = 1:5, b = letters[1:5])
  write_data_type(df, temp_file, "csv")

  expect_true(file.exists(temp_file))
  read_back <- vroom::vroom(temp_file, delim = ",", show_col_types = FALSE)
  expect_equal(nrow(read_back), 5)
})

test_that("write_data_type writes rds files", {
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)

  df <- tibble::tibble(a = 1:5, b = letters[1:5])
  write_data_type(df, temp_file, "rds")

  expect_true(file.exists(temp_file))
  read_back <- readRDS(temp_file)
  expect_equal(nrow(read_back), 5)
})

test_that("write_data_type writes single-row csv as log format", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  df <- tibble::tibble(a = 1, b = "x")
  write_data_type(df, temp_file, "csv")

  expect_true(file.exists(temp_file))
  read_back <- utils::read.csv(temp_file)
  expect_equal(nrow(read_back), 1)
})

# -- write_data tests --

test_that("write_data writes to temp directory when path is NULL", {
  run_info <- list(
    project_name = "test_proj",
    run_name = "test_run",
    storage_object = NULL,
    path = NULL,
    data_output = "csv",
    object_output = "rds"
  )

  df <- tibble::tibble(a = 1:5, b = letters[1:5])

  expect_no_error(
    write_data(
      x = df,
      combo = "test_combo",
      run_info = run_info,
      output_type = "data",
      folder = "forecasts",
      suffix = "-test"
    )
  )
})

test_that("write_data writes to explicit path", {
  temp_dir <- tempfile("write_data_test")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  run_info <- list(
    project_name = "test_proj",
    run_name = "test_run",
    storage_object = NULL,
    path = temp_dir,
    data_output = "csv",
    object_output = "rds"
  )

  df <- tibble::tibble(a = 1:5, b = letters[1:5])

  write_data(
    x = df,
    combo = "test_combo",
    run_info = run_info,
    output_type = "data",
    folder = "forecasts",
    suffix = "-test"
  )

  files <- list.files(file.path(temp_dir, "forecasts"), pattern = "\\.csv$")
  expect_true(length(files) > 0)
})

test_that("write_data handles NULL combo", {
  run_info <- list(
    project_name = "test_proj",
    run_name = "test_run",
    storage_object = NULL,
    path = NULL,
    data_output = "csv",
    object_output = "rds"
  )

  df <- tibble::tibble(a = 1, b = "x")

  expect_no_error(
    write_data(
      x = df,
      combo = NULL,
      run_info = run_info,
      output_type = "log",
      folder = "logs",
      suffix = NULL
    )
  )
})

test_that("write_data handles object output type", {
  temp_dir <- tempfile("write_data_obj_test")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  run_info <- list(
    project_name = "test_proj",
    run_name = "test_run",
    storage_object = NULL,
    path = temp_dir,
    data_output = "csv",
    object_output = "rds"
  )

  df <- tibble::tibble(a = 1:5, b = letters[1:5])

  write_data(
    x = df,
    combo = "test_combo",
    run_info = run_info,
    output_type = "object",
    folder = "models",
    suffix = "-test"
  )

  files <- list.files(file.path(temp_dir, "models"), pattern = "\\.rds$")
  expect_true(length(files) > 0)
})

# -- custom_ls tests --

test_that("custom_ls lists files in local directory", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_custom_ls")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(test_dir, "file1.csv"))
  file.create(file.path(test_dir, "file2.csv"))
  file.create(file.path(test_dir, "file3.txt"))

  result <- custom_ls(file.path(test_dir, "*.csv"))
  expect_equal(length(result), 2)
  expect_true(all(grepl("\\.csv$", result)))
})

test_that("custom_ls returns empty for non-existent pattern", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_custom_ls2")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(test_dir, "file1.csv"))

  result <- custom_ls(file.path(test_dir, "*.parquet"))
  expect_equal(length(result), 0)
})

test_that("custom_ls with wildcard glob matches all files", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_custom_ls3")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(test_dir, "a.csv"))
  file.create(file.path(test_dir, "b.txt"))

  result <- custom_ls(file.path(test_dir, "*"))
  expect_equal(length(result), 2)
})

test_that("custom_ls validates input is character", {
  expect_error(custom_ls(123))
})

test_that("custom_ls validates input is length 1", {
  expect_error(custom_ls(c("a", "b")))
})

# -- list_files tests --

test_that("list_files with NULL storage_object lists local files", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_list_files")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(test_dir, "data1.csv"))
  file.create(file.path(test_dir, "data2.csv"))

  result <- list_files(NULL, file.path(test_dir, "*.csv"))
  expect_equal(length(result), 2)
  expect_true(all(grepl("\\.csv$", result)))
})

# -- write_data_folder tests --

test_that("write_data_folder writes to local with NULL storage", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  df <- tibble::tibble(x = 1:5, y = letters[1:5])
  write_data_folder(
    x = df,
    storage_object = NULL,
    final_dest = "NULL",
    temp_path = NULL,
    final_path = temp_file,
    type = "csv"
  )
  expect_true(file.exists(temp_file))
})
