test_that("clamp_negative_target clamps negatives for croston", {
  df <- data.frame(Target = c(-5, 0, 3, -1, 10))

  expect_warning(
    result <- clamp_negative_target(df, "croston"),
    "requires non-negative Target values.*2 negative values clamped"
  )

  expect_equal(result$Target, c(0, 0, 3, 0, 10))
})

test_that("clamp_negative_target is a no-op for non-croston models", {
  df <- data.frame(Target = c(-5, 0, 3, -1, 10))

  result <- clamp_negative_target(df, "arima")
  expect_equal(result$Target, c(-5, 0, 3, -1, 10))
})

test_that("clamp_negative_target is a no-op when no negatives exist", {
  df <- data.frame(Target = c(0, 3, 10))

  # no warning when there are no negatives
  expect_silent(clamp_negative_target(df, "croston"))
  result <- clamp_negative_target(df, "croston")
  expect_equal(result$Target, c(0, 3, 10))
  expect_identical(result, df)
})

test_that("clamp_negative_target handles NA values without error", {
  df <- data.frame(Target = c(-2, NA, 5, -1))

  expect_warning(
    result <- clamp_negative_target(df, "croston"),
    "2 negative values clamped"
  )

  expect_equal(result$Target, c(0, NA, 5, 0))
})

test_that("Croston clamping preserves assessment actuals without changing training", {
  data <- tibble::tibble(
    Combo = "A",
    Date = as.Date("2024-01-01") + 0:6,
    Target = c(-5, 10, 0, -20, 0, 30, NA_real_)
  )
  splits <- tibble::tibble(
    Train_Test_ID = c(2, 3, 1),
    Train_End = data$Date[c(3, 2, 6)],
    Test_End = data$Date[c(6, 5, 7)]
  )
  for (with_original in c(FALSE, TRUE)) {
    input <- data
    if (with_original) {
      input$Target_Original <- c(-7, 12, 0, -25, 0, 35, NA_real_)
    }
    expected_actuals <- if (with_original) input$Target_Original else input$Target
    expect_warning(clamped <- clamp_negative_target(input, "croston"), "clamped")
    resamples <- create_splits(clamped, splits)

    for (i in seq_len(nrow(splits))) {
      analysis <- rsample::analysis(resamples$splits[[i]])
      assessment <- rsample::assessment(resamples$splits[[i]])
      expect_equal(analysis$Target, pmax(data$Target[match(analysis$Date, data$Date)], 0))
      expect_equal(assessment$Target, expected_actuals[match(assessment$Date, data$Date)])
    }
    expect_equal(input$Target, data$Target)
    expect_equal(clamp_negative_target(input, "meanf"), input)
    expect_silent(reclamped <- clamp_negative_target(clamped, "croston"))
    expect_identical(reclamped, clamped)
  }
})

test_that("Croston backtest CSVs and model averages retain historical actuals", {
  local_mocked_bindings(par_start = function(...) {
    list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`)
  })
  run_info <- set_run_info(
    project_name = "croston_actuals",
    run_name = "backtest",
    path = withr::local_tempdir(),
    data_output = "csv",
    add_unique_id = FALSE
  )
  input <- tibble::tibble(
    Series = "A",
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 48),
    Value = rep(c(100, 110, 0, 120, -20, 130), 8)
  )
  prep_data(run_info, input_data = input, combo_variables = "Series",
    target_variable = "Value", date_type = "month", forecast_horizon = 3,
    recipes_to_run = "R1", clean_outliers = FALSE, stationary = FALSE,
    box_cox = FALSE)
  prep_models(run_info, models_to_run = c("croston", "meanf"),
    num_hyperparameters = 1, back_test_scenarios = 2)
  expect_warning(
    train_models(run_info, run_global_models = FALSE, feature_selection = FALSE),
    "requires non-negative Target values"
  )

  raw_path <- locate_single_models_file(run_info)
  raw <- read_fcst_file(raw_path)
  backtests <- raw[raw$Train_Test_ID != 1, ]
  expect_setequal(backtests$Model_Name, c("croston", "meanf"))
  expect_true(any(backtests$Target < 0))
  expect_equal(backtests$Target, input$Value[match(backtests$Date, input$Date)])

  final_models(run_info, average_models = TRUE)
  output <- get_forecast_data(run_info)
  expect_true(any(output$Recipe_ID == "simple_average"))
  backtests <- output[output$Run_Type == "Back_Test", ]
  expect_equal(backtests$Target, input$Value[match(backtests$Date, input$Date)])
  expect_true(all(is.na(output$Target[output$Run_Type == "Future_Forecast"])))

  export_path <- file.path(run_info$path, "backtest-export.csv")
  vroom::vroom_write(output, export_path, delim = ",")
  exported <- read_fcst_file(export_path)
  expect_equal(exported$Target, output$Target)
  expect_setequal(names(read_fcst_file(raw_path)),
    c(names(raw), "Best_Model", "lo_95", "lo_80", "hi_80", "hi_95"))
  expect_false("Target_Original" %in% names(raw))
  expect_false("Target_Original" %in% names(output))
})
