# tests/testthat/test-write_data_type.R

test_that("write_data_type writes csv successfully", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(a = 1:3, b = letters[1:3])
  write_data_type(df, tmp, "csv")

  expect_true(file.exists(tmp))
  result <- vroom::vroom(tmp, delim = ",", show_col_types = FALSE)
  expect_equal(nrow(result), 3L)
  expect_equal(result$a, 1:3)
})

test_that("write_data_type writes single-row csv as log format", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(a = 1, b = "x")
  write_data_type(df, tmp, "csv")

  expect_true(file.exists(tmp))
  result <- utils::read.csv(tmp)
  expect_equal(nrow(result), 1L)
})

test_that("write_data_type writes rds successfully", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(a = 1:5)
  write_data_type(df, tmp, "rds")

  expect_true(file.exists(tmp))
  result <- readRDS(tmp)
  expect_equal(result, df)
})

test_that("write_data_type errors clearly after exhausting retries", {
  bad_path <- file.path(tempdir(), "no_such_dir_abc123", "sub", "file.csv")

  df <- data.frame(a = 1:3, b = letters[1:3])

  expect_error(
    write_data_type(df, bad_path, "csv", max_retries = 1L),
    "Failed to write.*after 1 attempt"
  )
})
