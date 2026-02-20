# tests/testthat/test-agent_helpers.R
# Tests for helper functions in agent_iterate_forecast.R that don't require LLM

test_that("collapse_or_na returns NA for 'NULL' string", {
  result <- collapse_or_na("NULL")
  expect_true(is.na(result))
  expect_type(result, "character")
})

test_that("collapse_or_na collapses multiple elements with ---", {
  result <- collapse_or_na(c("a", "b", "c"))
  expect_equal(result, "a---b---c")
})

test_that("collapse_or_na returns single element unchanged", {

  result <- collapse_or_na("hello")
  expect_equal(result, "hello")
})

test_that("collapse_or_na handles empty vector", {
  result <- collapse_or_na(character(0))
  expect_equal(result, "")
})

test_that("apply_column_types coerces numeric to integer", {
  target <- tibble::tibble(x = c(1.0, 2.0, 3.0))
  template <- tibble::tibble(x = 1L)
  result <- apply_column_types(target, template)
  expect_type(result$x, "integer")
})

test_that("apply_column_types coerces character to numeric", {
  target <- tibble::tibble(x = c("1", "2", "3"))
  template <- tibble::tibble(x = 1.0)
  result <- apply_column_types(target, template)
  expect_type(result$x, "double")
})

test_that("apply_column_types coerces to Date", {
  target <- tibble::tibble(x = c("2020-01-01", "2020-02-01"))
  template <- tibble::tibble(x = as.Date("2020-01-01"))
  result <- apply_column_types(target, template)
  expect_s3_class(result$x, "Date")
})

test_that("apply_column_types coerces to factor", {
  target <- tibble::tibble(x = c("a", "b", "c"))
  template <- tibble::tibble(x = factor("a", levels = c("a", "b", "c", "d")))
  result <- apply_column_types(target, template)
  expect_s3_class(result$x, "factor")
  expect_equal(levels(result$x), c("a", "b", "c", "d"))
})

test_that("apply_column_types coerces to logical", {
  target <- tibble::tibble(x = c(1, 0, 1))
  template <- tibble::tibble(x = TRUE)
  result <- apply_column_types(target, template)
  expect_type(result$x, "logical")
})

test_that("apply_column_types coerces to character", {
  target <- tibble::tibble(x = c(1, 2, 3))
  template <- tibble::tibble(x = "hello")
  result <- apply_column_types(target, template)
  expect_type(result$x, "character")
})

test_that("apply_column_types preserves columns not in template", {
  target <- tibble::tibble(x = 1, y = "extra")
  template <- tibble::tibble(x = 1L)
  result <- apply_column_types(target, template, drop_extra = FALSE)
  expect_true("y" %in% colnames(result))
})

test_that("apply_column_types drops extra columns when requested", {
  target <- tibble::tibble(x = 1, y = "extra")
  template <- tibble::tibble(x = 1L)
  result <- apply_column_types(target, template, drop_extra = TRUE)
  expect_false("y" %in% colnames(result))
  expect_equal(ncol(result), 1)
})

test_that("apply_column_types reorders columns when requested", {
  target <- tibble::tibble(z = "z", x = 1, y = 2)
  template <- tibble::tibble(x = 1L, y = 2L)
  result <- apply_column_types(target, template, reorder = TRUE)
  expect_equal(colnames(result)[1], "x")
  expect_equal(colnames(result)[2], "y")
})

test_that("apply_column_types coerces to POSIXct", {
  target <- tibble::tibble(x = c("2020-01-01 12:00:00", "2020-02-01 13:00:00"))
  template <- tibble::tibble(x = as.POSIXct("2020-01-01", tz = "UTC"))
  result <- apply_column_types(target, template)
  expect_s3_class(result$x, "POSIXct")
})

test_that("does_param_set_exist finds matching row", {
  df <- tibble::tibble(
    a = c(1, 2, 3),
    b = c("x", "y", "z")
  )
  
  result <- does_param_set_exist(list(a = 2, b = "y"), df)
  expect_true(result)
})

test_that("does_param_set_exist returns FALSE for no match", {
  df <- tibble::tibble(
    a = c(1, 2, 3),
    b = c("x", "y", "z")
  )
  
  result <- does_param_set_exist(list(a = 4, b = "w"), df)
  expect_false(result)
})

test_that("does_param_set_exist works with subset of columns", {
  df <- tibble::tibble(
    a = c(1, 2, 3),
    b = c("x", "y", "z"),
    c = c(10, 20, 30)
  )
  
  result <- does_param_set_exist(list(a = 1, b = "x"), df)
  expect_true(result)
})

test_that("extract_json_object parses simple JSON", {
  raw <- '{"key": "value", "num": 42}'
  result <- extract_json_object(raw)
  expect_equal(result$key, "value")
  expect_equal(result$num, 42)
})

test_that("extract_json_object strips code fences", {
  raw <- '```json\n{"key": "value"}\n```'
  result <- extract_json_object(raw)
  expect_equal(result$key, "value")
})

test_that("extract_json_object handles surrounding text", {
  raw <- 'Here is the JSON: {"answer": "yes"} and some more text.'
  result <- extract_json_object(raw)
  expect_equal(result$answer, "yes")
})

test_that("extract_json_object errors on non-character input", {
  expect_error(
    extract_json_object(123),
    "must be a single character string"
  )
})

test_that("extract_json_object errors on missing JSON", {
  expect_error(
    extract_json_object("no json here"),
    "No JSON object found"
  )
})

test_that("extract_json_object errors on invalid JSON", {
  expect_error(
    extract_json_object('{"key": }'),
    "Failed to parse JSON"
  )
})

test_that("null_converter returns NULL for 'NULL' string", {
  result <- null_converter("NULL")
  expect_null(result)
})

test_that("null_converter returns input for non-NULL string", {
  result <- null_converter("hello")
  expect_equal(result, "hello")
})

test_that("null_converter returns vector for multi-element input", {
  result <- null_converter(c("a", "b"))
  expect_equal(result, c("a", "b"))
})
