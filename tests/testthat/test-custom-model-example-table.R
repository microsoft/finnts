# Simple validation-example tables convert into the strict internal examples.
# Expected values here are literal arithmetic, independent of any model code.

example_table_definition <- function(model_type = "local", horizon = 3) {
  finnts:::new_custom_model_definition(
    "last_value", "Repeat the last value.", "Repeat the last observed value.", model_type,
    c(
      fit = "function(data, context, parameters) tail(data$Target, 1)",
      predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row, .pred=rep(object,nrow(new_data)))"
    ),
    list(
      predictors = character(), recipes = "C1", target_scale = "original", date_types = "month",
      forecast_horizon = horizon, missing_data = "error"
    )
  )
}

test_that("the template converts into valid local examples", {
  template <- custom_model_examples_template()
  expect_identical(names(template), c("example", "Date", "Target", "expected", "how"))
  expect_identical(unique(template$example), c("last_value", "edge_zero_history"))
  examples <- custom_examples_from_table(template, "Target", ".finnts_single_series", character(), "local")
  expect_length(examples, 2L)
  expect_identical(examples[[1]]$model_type, "local")
  expect_false(examples[[1]]$edge_case)
  expect_true(examples[[2]]$edge_case)
  expect_identical(examples[[1]]$expected$.pred, c(120, 120, 120))
  expect_identical(examples[[1]]$rationale, "Repeat the last observed value.")
  expect_identical(unique(examples[[1]]$history$Combo), "example")
  expect_false("Target" %in% names(examples[[1]]$new_data))
  checked <- custom_author_examples(examples, example_table_definition(), list(date_type = "month", forecast_horizon = 3))
  expect_identical(checked[[1]]$expected$.pred, c(120, 120, 120))
  quarterly <- custom_model_examples_template(forecast_horizon = 2, date_type = "quarter")
  expect_identical(nrow(quarterly), 10L)
  expect_identical(quarterly$Date[1:2], as.Date(c("2024-01-01", "2024-04-01")))
  expect_error(custom_model_examples_template(forecast_horizon = 0), "forecast_horizon")
  expect_error(custom_model_examples_template(date_type = "hour"), "date_type")
})

test_that("series, drivers, error cases and target names convert correctly", {
  dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 3)
  pooled <- data.frame(
    example = "pooled", Date = rep(dates, 2), Region = rep(c("east", "west"), each = 3),
    Revenue = c(10, 20, NA, 30, 40, NA), expected = c(NA, NA, 25, NA, NA, 25), Units = 1:6
  )
  zero <- data.frame(
    example = "zero_series", Date = rep(dates, 2), Region = rep(c("east", "west"), each = 3),
    Revenue = c(0, 0, NA, 0, 0, NA), expected = NA_real_, Units = 1:6, expected_error = "fit:zero_history"
  )
  table <- dplyr::bind_rows(pooled, zero)
  examples <- custom_examples_from_table(table, "Revenue", "Region", "Units", "global")
  expect_identical(vapply(examples, `[[`, "", "model_type"), c("global", "global"))
  expect_setequal(unique(examples[[1]]$history$Combo), c("east", "west"))
  expect_identical(examples[[1]]$new_data$Units, c(3L, 6L))
  expect_identical(examples[[1]]$history$Target, c(10, 20, 30, 40))
  expect_true(examples[[2]]$edge_case)
  expect_null(examples[[2]]$expected)
  expect_identical(examples[[2]]$expected_error, list(phase = "fit", code = "zero_history"))
  expect_identical(examples[[2]]$outcome, "error")
  expect_error(custom_examples_from_table(table, "Revenue", "Region", "Units", "local"), "global examples")
  expect_error(custom_examples_from_table(table, "Revenue", "Region", "Units", c("local", "global")), "includes local")
  expect_error(custom_examples_from_table(table, "Revenue", "Region", character(), "global"), "unexpected column")
  expect_error(custom_examples_from_table(table[setdiff(names(table), "Units")], "Revenue", "Region", "Units", "global"), "Units")
})

test_that("table mistakes produce row-level actionable errors", {
  template <- custom_model_examples_template()
  convert <- function(table) custom_examples_from_table(table, "Target", ".finnts_single_series", character(), "local")
  expect_error(convert(list(1)), "data frame")
  expect_error(convert(template[setdiff(names(template), "expected")]), "missing column")
  both <- template
  both$expected[1] <- 5
  expect_error(convert(both), "last_value', row 1, date 2024-01-01")
  no_answer <- template
  no_answer$expected[4] <- NA
  expect_error(convert(no_answer), "row 4.*future rows need an expected")
  no_edge <- template
  no_edge$example[no_edge$example == "edge_zero_history"] <- "zero_history"
  expect_error(convert(no_edge), "edge case")
  expect_error(convert(template[template$example == "last_value", ]), "two to eight")
  duplicate <- rbind(template, template[1, ])
  expect_error(convert(duplicate), "duplicate example")
  bad_error <- transform(template, expected_error = ifelse(example == "edge_zero_history", "boom", NA))
  expect_error(convert(bad_error), "fit:code")
  wrong_order <- template
  wrong_order$Date[4] <- as.Date("2023-12-01")
  expect_error(convert(wrong_order), "history rows followed by later future rows")
  iso <- transform(template, Date = format(Date))
  expect_identical(convert(iso), convert(template))
})
