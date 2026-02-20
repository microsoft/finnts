# tests/testthat/test-utility.R

test_that("get_timestamp returns a POSIXct object in UTC", {
  ts <- get_timestamp()

  expect_s3_class(ts, "POSIXct")
  expect_equal(attr(ts, "tzone"), "UTC")
})

test_that("get_timestamp returns a timestamp close to current time", {
  ts <- get_timestamp()
  now_utc <- as.POSIXct(format(Sys.time(), tz = "UTC"), tz = "UTC")

  # should be within 5 seconds of now

  expect_true(abs(difftime(ts, now_utc, units = "secs")) < 5)
})

test_that("get_timestamp format is YYYYMMDDTHHMMSSZ", {
  ts <- get_timestamp()
  formatted <- format(ts, "%Y%m%dT%H%M%SZ")

  # should be parseable back
  parsed <- as.POSIXct(formatted, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  expect_false(is.na(parsed))
})
