# Check get_date_type() with different datasets

test_that("get_date_type() detects date types correctly", {
  # Using m750 dataset
  expect_equal("month", get_date_type(modeltime::m750$date))
  # Passing dates as integer values
  expect_equal("day", get_date_type((Sys.Date()-1):Sys.Date()))
  # Passing dates as Date values
  expect_equal("day", get_date_type(as.Date((Sys.Date()-1):Sys.Date())))
})
