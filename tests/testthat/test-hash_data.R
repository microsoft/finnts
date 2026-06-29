# tests/testthat/test-hash_data.R
# Regression tests for hash_data() encoding stability.
#
# Bug: digest::digest() is sensitive to the R string Encoding marker, so a
# non-ASCII combo read with read.csv (marker "unknown") hashed differently than
# the same bytes read with vroom (marker "UTF-8"). Input files were named with
# one hash but looked up with the other, causing "subscript out of bounds" in
# read_file(). hash_data() now normalizes the marker before hashing.

# helper: build a string with given bytes and a specific Encoding marker
make_str <- function(bytes, encoding) {
  s <- rawToChar(as.raw(bytes))
  Encoding(s) <- encoding
  s
}

# "Karén" in UTF-8 bytes (é = 0xC3 0xA9)
karen_bytes <- c(0x4b, 0x61, 0x72, 0xc3, 0xa9, 0x6e)

test_that("hash_data is stable across encoding markers for identical bytes", {
  combo_unknown <- make_str(karen_bytes, "unknown") # as read.csv reads it
  combo_utf8 <- make_str(karen_bytes, "UTF-8") # as vroom reads it
  combo_latin1 <- make_str(karen_bytes, "latin1")

  expect_identical(charToRaw(combo_unknown), charToRaw(combo_utf8))
  expect_equal(hash_data(combo_unknown), hash_data(combo_utf8))
  expect_equal(hash_data(combo_unknown), hash_data(combo_latin1))
})

test_that("hash_data keeps ASCII hashes unchanged (backward compatible file names)", {
  # these values must never change or existing run-logging files break
  expect_equal(hash_data("finn_forecast"), "2b8cd41e44e9eb3e")
  expect_equal(hash_data("all"), "5ec4d53eb467796e")
  expect_equal(hash_data("Microsoft AI--Rukmini I. (Ads)"), "025f819e516b2de2")
})

test_that("hash_data leaves non-character input untouched", {
  df <- data.frame(a = 1:2, b = c("x", "y"))
  expect_equal(
    hash_data(df),
    digest::digest(object = df, algo = "xxhash64")
  )
  expect_equal(hash_data(42L), digest::digest(object = 42L, algo = "xxhash64"))
})

test_that("non-ASCII combo file written via read.csv path is found via vroom path", {
  tmp <- withr::local_tempdir()

  project_info <- list(
    project_name = "verify_proj",
    storage_object = NULL,
    path = tmp,
    data_output = "csv",
    object_output = "rds",
    run_name = "runABC"
  )

  # combo as it arrives from read.csv of the user's data (marker "unknown")
  combo_written <- make_str(karen_bytes, "unknown")

  # write the input file exactly like set_agent_info() does
  write_data(
    x = data.frame(Combo = combo_written, Date = as.Date("2024-01-01"), Target = 10),
    combo = combo_written,
    run_info = project_info,
    output_type = "data",
    folder = "input_data",
    suffix = NULL
  )

  # combo as it is re-read at runtime via read_file()/vroom (marker "UTF-8")
  combo_runtime <- make_str(karen_bytes, "UTF-8")

  # build the per-combo lookup exactly like submit_fcst_run()
  lookup_glob <- paste0(
    project_info$path, "/input_data/*", hash_data(project_info$project_name), "-",
    hash_data(project_info$run_name), "-", hash_data(combo_runtime), ".",
    project_info$data_output
  )

  found <- list_files(NULL, lookup_glob)
  expect_length(found, 1)

  result <- read_file(
    run_info = project_info,
    file_list = found,
    return_type = "df"
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$Target, 10)
})
