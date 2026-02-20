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

# -- clean_markdown tests --

test_that("clean_markdown removes bold markers", {
  result <- clean_markdown("This is **bold** text")
  expect_equal(result, "This is bold text")
})

test_that("clean_markdown removes underscore bold markers", {
  result <- clean_markdown("This is __bold__ text")
  expect_equal(result, "This is bold text")
})

test_that("clean_markdown removes italic markers", {
  result <- clean_markdown("This is *italic* text")
  expect_equal(result, "This is italic text")
})

test_that("clean_markdown removes header markers", {
  result <- clean_markdown("# Header text")
  expect_equal(result, "Header text")
})

test_that("clean_markdown removes multi-level headers", {
  result <- clean_markdown("### Third level header")
  expect_equal(result, "Third level header")
})

test_that("clean_markdown removes code blocks", {
  result <- clean_markdown("Use `code` in text")
  expect_equal(result, "Use code in text")
})

test_that("clean_markdown cleans excess whitespace", {
  result <- clean_markdown("Text\n\n\n\nMore text")
  expect_equal(result, "Text\n\nMore text")
})

test_that("clean_markdown returns empty string for empty input", {
  result <- clean_markdown("")
  expect_equal(result, "")
})

test_that("clean_markdown with keep_lists preserves bullet markers", {
  text <- "- Item 1\n- Item 2\n* Item 3"
  result <- clean_markdown(text, keep_lists = TRUE)
  expect_true(grepl("- Item 1", result))
  expect_true(grepl("- Item 2", result))
  expect_true(grepl("\\* Item 3", result))
})

test_that("clean_markdown with keep_lists=FALSE still removes list-like italics", {
  text <- "Some *italic* text"
  result <- clean_markdown(text, keep_lists = FALSE)
  expect_false(grepl("\\*", result))
})

# -- summarize_analysis_results tests --

test_that("summarize_analysis_results handles data.frame result", {
  results <- list(
    step_1 = data.frame(x = 1:3, y = c("a", "b", "c"))
  )
  output <- summarize_analysis_results(results, max_rows = 10)
  expect_type(output, "character")
  expect_true(grepl("3 rows total", output))
})

test_that("summarize_analysis_results handles numeric result", {
  results <- list(step_1 = 42.5)
  output <- summarize_analysis_results(results)
  expect_true(grepl("42.5", output))
})

test_that("summarize_analysis_results handles character result", {
  results <- list(step_1 = "some text result")
  output <- summarize_analysis_results(results)
  expect_true(grepl("some text result", output))
})

test_that("summarize_analysis_results handles list result", {
  results <- list(step_1 = list(a = 1, b = "text"))
  output <- summarize_analysis_results(results)
  expect_type(output, "character")
  expect_true(nchar(output) > 0)
})

test_that("summarize_analysis_results truncates long data frames", {
  results <- list(
    step_1 = data.frame(x = 1:100, y = letters[rep(1:26, length.out = 100)])
  )
  output <- summarize_analysis_results(results, max_rows = 5)
  expect_true(grepl("100 rows total, showing 5", output))
  expect_true(grepl("more rows omitted", output))
})

test_that("summarize_analysis_results handles multiple steps", {
  results <- list(
    step_1 = data.frame(x = 1:3),
    step_2 = 42,
    step_3 = "text"
  )
  output <- summarize_analysis_results(results)
  expect_true(grepl("1 result", output))
  expect_true(grepl("2 result", output))
  expect_true(grepl("3 result", output))
})

test_that("summarize_analysis_results handles empty results", {
  results <- list()
  output <- summarize_analysis_results(results)
  expect_equal(output, "")
})

# -- display_answer tests --

test_that("display_answer prints to console", {
  expect_output(display_answer("This is a test answer"), "This is a test answer")
})

test_that("display_answer cleans markdown in answer", {
  expect_output(
    display_answer("This is **bold** text"),
    "This is bold text"
  )
})

# -- sanitize_args tests (from agent_run.R) --

test_that("sanitize_args keeps atomic values", {
  result <- sanitize_args(list(a = "hello", b = 42, c = TRUE))
  expect_equal(result$a, "hello")
  expect_equal(result$b, 42)
  expect_equal(result$c, TRUE)
})

test_that("sanitize_args replaces non-atomic values with placeholder", {
  result <- sanitize_args(list(
    simple = "text",
    complex = data.frame(x = 1:3)
  ))
  expect_equal(result$simple, "text")
  expect_equal(result$complex, "<object:complex>")
})

test_that("sanitize_args handles empty list", {
  result <- sanitize_args(list())
  expect_equal(length(result), 0)
})

test_that("sanitize_args handles list values", {
  result <- sanitize_args(list(
    x = 1,
    y = list(a = 1, b = 2)
  ))
  expect_equal(result$x, 1)
  expect_type(result$y, "character")
  expect_true(grepl("object", result$y))
})

test_that("sanitize_args handles NULL values in list", {
  result <- sanitize_args(list(a = NULL, b = "text"))
  # NULL is non-atomic, so sanitize_args converts it to a description string
  expect_type(result$a, "character")
  expect_equal(result$b, "text")
})
