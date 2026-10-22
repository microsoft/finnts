# tests/testthat/test-validate-prepared-input.R

# * Test data ----

validation_monthly_data <- function(ids = c("A", "B", "C"), months = 36) {
  dates <- seq.Date(as.Date("2021-01-01"), by = "month", length.out = months)
  data.frame(
    id = rep(ids, each = months),
    Date = rep(dates, times = length(ids)),
    value = as.numeric(seq_len(months * length(ids))) + 10,
    stringsAsFactors = FALSE
  )
}

validate_monthly <- function(data, ...) {
  args <- utils::modifyList(
    list(
      input_data = data,
      combo_variables = "id",
      target_variable = "value",
      date_type = "month",
      forecast_horizon = 3
    ),
    list(...)
  )
  do.call(validate_prepared_input, args)
}

check_status <- function(result, rule) {
  result$checks$Status[result$checks$Rule == rule]
}

check_message <- function(result, rule) {
  result$checks$Message[result$checks$Rule == rule]
}

# * Happy path ----

test_that("validate_prepared_input passes clean monthly data", {
  result <- validate_monthly(validation_monthly_data())

  expect_true(result$passed)
  expect_equal(result$checks$Rule, c(paste0("R", 1:11), paste0("P", 1:8)))
  expect_true(all(result$checks$Status == "pass"))
  expect_equal(result$summary$hist_end_date, as.Date("2023-12-01"))
  expect_equal(result$summary$n_series, 3)
  expect_equal(result$summary$n_future_rows, 0)
  expect_s3_class(result$details$splits, "tbl_df")
})

test_that("validate_prepared_input does not modify the input or write files", {
  data <- validation_monthly_data()
  original <- data
  before <- list.files(tempdir(), recursive = TRUE)

  validate_monthly(data)

  expect_identical(data, original)
  expect_identical(list.files(tempdir(), recursive = TRUE), before)
})

# * Input rules R1-R11 ----

test_that("R1 fails on missing columns with the prep_data message", {
  result <- validate_monthly(validation_monthly_data(), combo_variables = c("id", "region"))

  expect_false(result$passed)
  expect_equal(check_status(result, "R1"), "fail")
  expect_match(check_message(result, "R1"), "Missing columns: region")
  expect_equal(check_status(result, "R5"), "skipped")
  expect_true(all(result$checks$Status[result$checks$Rule %in% paste0("P", 1:8)] == "skipped"))

  missing_target <- validate_monthly(validation_monthly_data(), target_variable = "sales")
  expect_match(check_message(missing_target, "R1"), "target variable 'sales'")

  missing_xreg <- validate_monthly(validation_monthly_data(), external_regressors = "price")
  expect_match(check_message(missing_xreg, "R1"), "external regressors do not match")
})

test_that("R2 fails when Date is used as a regressor", {
  result <- validate_monthly(validation_monthly_data(), external_regressors = "Date")

  expect_equal(check_status(result, "R2"), "fail")
  expect_match(check_message(result, "R2"), "reserved for the time stamp")
})

test_that("R3 fails when Date is not a Date value", {
  data <- validation_monthly_data()
  data$Date <- as.character(data$Date)
  result <- validate_monthly(data)

  expect_equal(check_status(result, "R3"), "fail")
  expect_equal(check_status(result, "R7"), "skipped")

  renamed <- validation_monthly_data()
  names(renamed)[names(renamed) == "Date"] <- "date"
  expect_match(check_message(validate_monthly(renamed), "R3"), "named as 'Date'")
})

test_that("R4 fails when a combo variable is a date column", {
  data <- validation_monthly_data()
  data$order_date <- data$Date
  result <- validate_monthly(data, combo_variables = c("id", "order_date"))

  expect_equal(check_status(result, "R4"), "fail")
  expect_match(check_message(result, "R4"), "order_date")
})

test_that("R5 fails when the target is not numeric", {
  data <- validation_monthly_data()
  data$value <- as.character(data$value)
  result <- validate_monthly(data)

  expect_equal(check_status(result, "R5"), "fail")
  expect_match(check_message(result, "R5"), "needs to be numeric")
})

test_that("R6 fails on combo values that are blank after trimming", {
  data <- validation_monthly_data()
  data$id[1] <- "   "
  result <- validate_monthly(data)

  expect_equal(check_status(result, "R6"), "fail")
  expect_match(check_message(result, "R6"), "cannot be blank")
})

test_that("R7 fails on duplicates that differ only by whitespace", {
  data <- validation_monthly_data()
  extra <- data[1, ]
  extra$id <- " A "
  result <- validate_monthly(rbind(data, extra))

  expect_equal(check_status(result, "R7"), "fail")
  expect_match(check_message(result, "R7"), "Found 2 duplicate rows")
})

test_that("R8 fails when monthly dates use different days", {
  data <- validation_monthly_data()
  shifted <- format(data$Date, "%m") %in% c("02", "04")
  data$Date[shifted] <- data$Date[shifted] + 14
  result <- validate_monthly(data)

  expect_equal(check_status(result, "R9"), "pass")
  expect_equal(check_status(result, "R8"), "fail")
  expect_match(check_message(result, "R8"), "same day of the month")
})

test_that("R9 fails on invalid or mismatched date types", {
  invalid <- validate_monthly(validation_monthly_data(), date_type = "monthly")
  expect_equal(check_status(invalid, "R9"), "fail")
  expect_match(check_message(invalid, "R9"), "date_type must be one of")

  as_week <- validate_monthly(validation_monthly_data(), date_type = "week")
  expect_equal(check_status(as_week, "R9"), "fail")
  expect_match(check_message(as_week, "R9"), "whole number of weeks")

  quarterly <- validation_monthly_data()
  quarterly <- quarterly[format(quarterly$Date, "%m") %in% c("01", "04", "07", "10"), ]
  as_month <- validate_monthly(quarterly)
  expect_equal(check_status(as_month, "R9"), "fail")
  expect_match(check_message(as_month, "R9"), "coarser than 'month'")

  daily <- data.frame(
    id = "A",
    Date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 90),
    value = 1
  )
  daily_as_month <- validate_monthly(daily)
  expect_equal(check_status(daily_as_month, "R9"), "fail")

  missing_date <- validation_monthly_data()
  missing_date$Date[2] <- NA
  expect_match(check_message(validate_monthly(missing_date), "R9"), "missing Date")
})

test_that("R10 fails on an invalid fiscal year start", {
  result <- validate_monthly(validation_monthly_data(), fiscal_year_start = 13)

  expect_equal(check_status(result, "R10"), "fail")
  expect_match(check_message(result, "R10"), "1 to 12")
})

test_that("R11 fails when a Spark table runs without Spark processing", {
  data <- validation_monthly_data()
  class(data) <- c("tbl_spark", "data.frame")
  # The fake Spark table has no connection, so collecting it must not reach
  # sparklyr's method when another test has loaded that namespace.
  testthat::local_mocked_bindings(
    collect = function(x, ...) {
      class(x) <- setdiff(class(x), "tbl_spark")
      x
    },
    .package = "dplyr"
  )
  result <- validate_monthly(data, parallel_processing = "local_machine")

  expect_equal(check_status(result, "R11"), "fail")
  expect_match(check_message(result, "R11"), "spark parallel processing")
})

test_that("validator reports every failed input rule instead of stopping", {
  data <- validation_monthly_data()
  data$value <- as.character(data$value)
  data$id[1] <- " "
  result <- validate_monthly(data, fiscal_year_start = 0)

  expect_setequal(result$checks$Rule[result$checks$Status == "fail"], c("R5", "R6", "R10"))
})

test_that("check_input_data still stops on the first rule with the same message", {
  data <- validation_monthly_data()
  data$value <- as.character(data$value)

  expect_error(
    check_input_data(data, "id", "value", NULL, "month", 1, NULL),
    "Target variable in input data needs to be numeric"
  )
  result <- validate_monthly(data)
  expect_equal(check_message(result, "R5"), "Target variable in input data needs to be numeric")
})

# * Preparation checks P1-P8 ----

test_that("P1 reports future rows and fails on an impossible history window", {
  data <- validation_monthly_data()
  future <- data[data$Date >= as.Date("2023-10-01"), ]
  future$value <- NA_real_
  history <- data[data$Date < as.Date("2023-10-01"), ]
  result <- validate_monthly(rbind(history, future), hist_end_date = as.Date("2023-09-01"))

  expect_equal(check_status(result, "P1"), "report")
  expect_equal(result$summary$n_future_rows, 9)
  expect_match(check_message(result, "P1"), "9 rows fall after it")

  impossible <- validate_monthly(data, hist_start_date = as.Date("2030-01-01"))
  expect_equal(check_status(impossible, "P1"), "fail")
  expect_false(impossible$passed)
})

test_that("P2 reports future regressors and missing horizon coverage", {
  data <- validation_monthly_data()
  data$price <- 1
  future <- data[data$Date <= as.Date("2024-01-01"), ][1:3, ]
  future$Date <- as.Date(c("2024-01-01", "2024-02-01", "2024-03-01"))
  future$value <- NA_real_
  future$id <- "A"
  result <- validate_monthly(rbind(data, future),
    external_regressors = "price",
    hist_end_date = as.Date("2023-12-01")
  )

  expect_equal(check_status(result, "P2"), "report")
  expect_equal(result$details$future_regressors, "price")
  coverage <- result$details$future_regressor_coverage
  expect_equal(coverage$Missing_Future_Periods[coverage$Combo == "A"], 0)
  expect_equal(coverage$Missing_Future_Periods[coverage$Combo == "B"], 3)
})

test_that("P3 reports missing and leading periods", {
  data <- validation_monthly_data()
  data <- data[!(data$id == "A" & data$Date == as.Date("2022-03-01")), ]
  data <- data[!(data$id == "C" & data$Date < as.Date("2021-04-01")), ]
  result <- validate_monthly(data)

  expect_equal(check_status(result, "P3"), "report")
  gaps <- result$details$gaps
  expect_equal(gaps$Missing_Periods[gaps$Combo == "A"], 1)
  expect_equal(gaps$Leading_Periods[gaps$Combo == "C"], 3)
})

test_that("P4 reports missing and non-finite target values", {
  data <- validation_monthly_data()
  data$value[c(2, 3)] <- NA
  data$value[5] <- Inf
  result <- validate_monthly(data, clean_missing_values = FALSE)

  expect_equal(check_status(result, "P4"), "report")
  expect_match(check_message(result, "P4"), "2 missing target values \\(set to 0\\)")
  expect_equal(result$details$target_values$Infinite, 1)
})

test_that("P5 fails when series use different weekdays or quarter months", {
  weekly <- data.frame(
    id = rep(c("A", "B"), each = 20),
    Date = c(
      seq.Date(as.Date("2023-01-02"), by = "week", length.out = 20),
      seq.Date(as.Date("2023-01-04"), by = "week", length.out = 20)
    ),
    value = 1
  )
  week_result <- validate_monthly(weekly, date_type = "week")
  expect_equal(check_status(week_result, "R9"), "pass")
  expect_equal(check_status(week_result, "P5"), "fail")

  quarterly <- data.frame(
    id = rep(c("A", "B"), each = 12),
    Date = c(
      seq.Date(as.Date("2020-01-01"), by = "quarter", length.out = 12),
      seq.Date(as.Date("2020-02-01"), by = "quarter", length.out = 12)
    ),
    value = 1
  )
  quarter_result <- validate_monthly(quarterly, date_type = "quarter")
  expect_equal(check_status(quarter_result, "P5"), "fail")
})

test_that("P6 reports removed series and fails when every series is removed", {
  data <- validation_monthly_data()
  data$value[data$id == "B" & data$Date >= as.Date("2023-06-01")] <- 0
  result <- validate_monthly(data, combo_cleanup_date = as.Date("2023-06-01"))

  expect_equal(check_status(result, "P6"), "report")
  expect_equal(result$details$cleanup_combos, "B")

  data$value[data$Date >= as.Date("2023-06-01")] <- 0
  all_removed <- validate_monthly(data, combo_cleanup_date = as.Date("2023-06-01"))
  expect_equal(check_status(all_removed, "P6"), "fail")
})

test_that("P7 fails when a total series is mixed with its parts", {
  data <- validation_monthly_data()
  totals <- stats::aggregate(value ~ Date, data = data, FUN = sum)
  totals$id <- "Total"
  combined <- rbind(data, totals[, names(data)])
  combined <- combined[!(combined$id == "A" & combined$Date == as.Date("2022-03-01")), ]
  result <- validate_monthly(combined)

  expect_equal(check_status(result, "P7"), "fail")
  expect_match(check_message(result, "P7"), "id = 'Total'")

  named <- validation_monthly_data(ids = c("A", "B", "East Total"))
  named_result <- validate_monthly(named)
  expect_equal(check_status(named_result, "P7"), "report")
})

test_that("P7 detects subtotals within another combo variable", {
  data <- validation_monthly_data(ids = c("P1", "P2", "P3"))
  data$region <- "East"
  subtotal <- stats::aggregate(value ~ Date + region, data = data, FUN = sum)
  subtotal$id <- "Subtotal"
  combined <- rbind(data, subtotal[, names(data)])
  result <- validate_monthly(combined, combo_variables = c("region", "id"))

  expect_equal(check_status(result, "P7"), "fail")
  expect_equal(result$details$aggregate_levels$Level, "Subtotal")
})

test_that("P8 fails without enough history and reports late-starting series", {
  short <- validation_monthly_data(months = 6)
  short_result <- validate_monthly(short, forecast_horizon = 6)
  expect_equal(check_status(short_result, "P8"), "fail")
  expect_match(check_message(short_result, "P8"), "history has 6 periods, too few for a forecast_horizon of 6")
  expect_false(short_result$passed)

  data <- validation_monthly_data()
  data <- data[!(data$id == "C" & data$Date < as.Date("2023-11-01")), ]
  late_result <- validate_monthly(data)
  expect_equal(check_status(late_result, "P8"), "report")
  expect_equal(late_result$details$short_history_combos, "C")
})

# * Integration with prep_data and prep_models ----

test_that("a passing validation prepares and splits the same data", {
  skip_on_cran()

  data <- validation_monthly_data()
  data$price <- rep(seq_len(36), 3)
  result <- validate_monthly(data, external_regressors = "price")
  expect_true(result$passed)

  run_info <- set_run_info(
    project_name = "validate_prepared_input",
    run_name = "integration",
    path = withr::local_tempdir()
  )
  expect_no_error(suppressMessages(prep_data(
    run_info,
    input_data = data,
    combo_variables = "id",
    target_variable = "value",
    date_type = "month",
    forecast_horizon = 3,
    external_regressors = "price",
    recipes_to_run = "R1"
  )))
  expect_no_error(suppressMessages(prep_models(run_info, models_to_run = "snaive")))

  saved_splits <- get_prepped_models(run_info)$Data[[3]]
  saved_back_tests <- saved_splits[saved_splits$Run_Type == "Back_Test", ]
  planned_back_tests <- result$details$splits[result$details$splits$Run_Type == "Back_Test", ]
  expect_equal(as.Date(saved_back_tests$Train_End), planned_back_tests$Train_End)
  expect_equal(as.Date(saved_back_tests$Test_End), planned_back_tests$Test_End)
})
