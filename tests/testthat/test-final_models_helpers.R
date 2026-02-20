# tests/testthat/test-final_models_helpers.R

test_that("create_prediction_intervals adds interval columns", {
  fcst_tbl <- tibble::tibble(
    Combo = rep("A", 6),
    Model_ID = rep("M1", 6),
    Train_Test_ID = c(1, 1, 1, 2, 2, 2),
    Target = c(100, 200, 300, 100, 200, 300),
    Forecast = c(110, 190, 310, 105, 195, 305),
    Horizon = c(1, 2, 3, 1, 2, 3)
  )

  train_test_split <- tibble::tibble(
    Run_Type = c("Back_Test", "Future_Forecast"),
    Train_Test_ID = c(2, 1)
  )

  result <- create_prediction_intervals(fcst_tbl, train_test_split)

  expect_true("lo_80" %in% colnames(result))
  expect_true("lo_95" %in% colnames(result))
  expect_true("hi_80" %in% colnames(result))
  expect_true("hi_95" %in% colnames(result))

  # prediction intervals should only be non-NA for Train_Test_ID == 1
  future_rows <- result %>% dplyr::filter(Train_Test_ID == 1)
  expect_true(all(!is.na(future_rows$lo_80)))
  expect_true(all(!is.na(future_rows$hi_95)))

  back_test_rows <- result %>% dplyr::filter(Train_Test_ID == 2)
  expect_true(all(is.na(back_test_rows$lo_80)))
})

test_that("create_prediction_intervals calculates intervals correctly", {
  fcst_tbl <- tibble::tibble(
    Combo = rep("A", 4),
    Model_ID = rep("M1", 4),
    Train_Test_ID = c(1, 1, 2, 2),
    Target = c(100, 200, 100, 200),
    Forecast = c(110, 210, 100, 200),
    Horizon = c(1, 2, 1, 2)
  )

  train_test_split <- tibble::tibble(
    Run_Type = c("Back_Test", "Future_Forecast"),
    Train_Test_ID = c(2, 1)
  )

  result <- create_prediction_intervals(fcst_tbl, train_test_split)

  # residuals for back_test: 100-100=0, 200-200=0 -> std_dev = 0
  future_rows <- result %>% dplyr::filter(Train_Test_ID == 1)
  expect_equal(future_rows$lo_80, future_rows$Forecast)
  expect_equal(future_rows$hi_80, future_rows$Forecast)
})

test_that("convert_weekly_to_daily returns unchanged for non-weekly data", {
  fcst_tbl <- tibble::tibble(
    Combo_ID = "C1",
    Model_ID = "M1",
    Model_Name = "arima",
    Model_Type = "local",
    Recipe_ID = "R1",
    Train_Test_ID = 1,
    Hyperparameter_ID = "H1",
    Best_Model = "Yes",
    Combo = "A",
    Horizon = 1,
    Date = as.Date("2020-01-01"),
    Target = 100,
    Forecast = 110,
    lo_95 = 90,
    lo_80 = 95,
    hi_80 = 125,
    hi_95 = 130
  )

  result <- convert_weekly_to_daily(fcst_tbl, "month", FALSE)

  expect_equal(nrow(result), 1)
  expect_true("Date" %in% colnames(result))
  expect_false("Date_Day" %in% colnames(result))
})

test_that("convert_weekly_to_daily returns unchanged when weekly_to_daily is FALSE", {
  fcst_tbl <- tibble::tibble(
    Combo_ID = "C1",
    Model_ID = "M1",
    Model_Name = "arima",
    Model_Type = "local",
    Recipe_ID = "R1",
    Train_Test_ID = 1,
    Hyperparameter_ID = "H1",
    Best_Model = "Yes",
    Combo = "A",
    Horizon = 1,
    Date = as.Date("2020-01-06"),
    Target = 700,
    Forecast = 700,
    lo_95 = 600,
    lo_80 = 650,
    hi_80 = 750,
    hi_95 = 800
  )

  result <- convert_weekly_to_daily(fcst_tbl, "week", FALSE)

  expect_equal(nrow(result), 1)
})

test_that("convert_weekly_to_daily expands weekly to daily", {
  fcst_tbl <- tibble::tibble(
    Combo_ID = "C1",
    Model_ID = "M1",
    Model_Name = "arima",
    Model_Type = "local",
    Recipe_ID = "R1",
    Train_Test_ID = 1,
    Hyperparameter_ID = "H1",
    Best_Model = "Yes",
    Combo = "A",
    Horizon = 1,
    Date = as.Date("2020-01-06"),
    Target = 700,
    Forecast = 700,
    lo_95 = 630,
    lo_80 = 700,
    hi_80 = 700,
    hi_95 = 770
  )

  result <- convert_weekly_to_daily(fcst_tbl, "week", TRUE)

  expect_equal(nrow(result), 7)
  expect_true("Date_Day" %in% colnames(result))
  # daily values should be weekly / 7
  expect_equal(unique(result$Target), 100)
  expect_equal(unique(result$Forecast), 100)
})

test_that("remove_best_model removes Best_Model column", {
  df <- tibble::tibble(
    Model_ID = "M1",
    Forecast = 100,
    Best_Model = "Yes"
  )

  result <- remove_best_model(df)

  expect_false("Best_Model" %in% colnames(result))
  expect_true("Model_ID" %in% colnames(result))
})

test_that("remove_best_model leaves df unchanged without Best_Model", {
  df <- tibble::tibble(
    Model_ID = "M1",
    Forecast = 100
  )

  result <- remove_best_model(df)

  expect_equal(ncol(result), 2)
})

test_that("adjust_combo_column converts logical Combo to character", {
  df <- tibble::tibble(
    Combo = c(TRUE, FALSE, TRUE),
    Forecast = c(100, 200, 300)
  )

  result <- adjust_combo_column(df)

  expect_type(result$Combo, "character")
  expect_equal(result$Combo, c("T", "F", "T"))
})

test_that("adjust_combo_column leaves character Combo unchanged", {
  df <- tibble::tibble(
    Combo = c("A", "B", "C"),
    Forecast = c(100, 200, 300)
  )

  result <- adjust_combo_column(df)

  expect_equal(result$Combo, c("A", "B", "C"))
})

test_that("adjust_combo_column handles missing Combo column", {
  df <- tibble::tibble(
    Model_ID = "M1",
    Forecast = 100
  )

  result <- adjust_combo_column(df)

  expect_equal(ncol(result), 2)
})

# -- create_prediction_intervals with multiple models --

test_that("create_prediction_intervals handles multiple Model_IDs", {
  fcst_tbl <- tibble::tibble(
    Combo = rep("A", 8),
    Model_ID = c(rep("M1", 4), rep("M2", 4)),
    Train_Test_ID = rep(c(1, 1, 2, 2), 2),
    Target = c(100, 200, 100, 200, 100, 200, 100, 200),
    Forecast = c(110, 210, 95, 195, 120, 220, 105, 205),
    Horizon = rep(c(1, 2, 1, 2), 2)
  )

  train_test_split <- tibble::tibble(
    Run_Type = c("Back_Test", "Future_Forecast"),
    Train_Test_ID = c(2, 1)
  )

  result <- create_prediction_intervals(fcst_tbl, train_test_split)

  # Should have interval columns for both models
  m1_future <- result %>% dplyr::filter(Model_ID == "M1", Train_Test_ID == 1)
  m2_future <- result %>% dplyr::filter(Model_ID == "M2", Train_Test_ID == 1)

  expect_true(all(!is.na(m1_future$lo_80)))
  expect_true(all(!is.na(m2_future$lo_80)))
})

# -- convert_weekly_to_daily with multiple weeks --

test_that("convert_weekly_to_daily expands multiple weeks", {
  fcst_tbl <- tibble::tibble(
    Combo_ID = rep("C1", 2),
    Model_ID = rep("M1", 2),
    Model_Name = rep("model", 2),
    Model_Type = rep("type", 2),
    Recipe_ID = rep("R1", 2),
    Train_Test_ID = rep(1, 2),
    Hyperparameter_ID = rep("H1", 2),
    Best_Model = rep("M1", 2),
    Combo = rep("C1", 2),
    Horizon = c(1, 2),
    Date = as.Date(c("2020-01-06", "2020-01-13")),
    Target = c(70, 140),
    Forecast = c(70, 140),
    lo_80 = c(60, 130),
    lo_95 = c(50, 120),
    hi_80 = c(80, 150),
    hi_95 = c(90, 160)
  )

  result <- convert_weekly_to_daily(fcst_tbl, "week", weekly_to_daily = TRUE)

  # Each week should expand to 7 daily rows
  expect_equal(nrow(result), 14)
  # Daily values should be 1/7 of weekly
  expect_equal(result$Forecast[1], 10)
})
