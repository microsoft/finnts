# * custom test functions ----

check_exist <- function(to_check, ret) {
  for (elem in to_check) {
    testthat::expect_true(elem %in% ret)
  }
}

# * bottoms up forecast ----

# Basic forecast to test various parts
forecast_horizon <- 3
target_variable <- "value"
combo_variables <- c("id")
models_to_run <- c("snaive")
inp_data <- modeltime::m750 %>%
  dplyr::rename(Date = date) %>%
  dplyr::mutate(id = as.character(id)) %>%
  dplyr::filter(Date >= "2012-01-01")
dt_type <- "month"

finn_forecast <- forecast_time_series(
  input_data = inp_data,
  combo_variables = combo_variables,
  target_variable = target_variable,
  date_type = dt_type,
  forecast_horizon = forecast_horizon,
  run_model_parallel = FALSE,
  back_test_scenarios = 3,
  models_to_run = models_to_run,
  run_global_models = FALSE,
  run_ensemble_models = FALSE,
  average_models = FALSE
)

final_fcst <- finn_forecast$final_fcst %>%
  mutate(Date = as.Date(Date))

back_test_data <- finn_forecast$back_test_data
back_test_best_MAPE <- finn_forecast$back_test_best_MAPE


test_that("final forecast has all columns", {
  cols_to_check <- c(
    "Combo", "Date", "Type",
    "Model", "value", "lo.95",
    "lo.80", "hi.80", "hi.95"
  )
  retcols <- colnames(final_fcst)

  check_exist(cols_to_check, retcols)
})

test_that("back test data has all columns", {
  cols_to_check <- c(
    "Combo", "Date", "Model", "Horizon",
    "FCST", "Target", "Back_Test_Scenario",
    "MAPE", "Best_Model"
  )
  retcols <- colnames(back_test_data)

  check_exist(cols_to_check, retcols)
})

test_that("back test data rows are meaningful", {
  horizons <- unique(back_test_data$Horizon)
  testthat::expect_equal(min(horizons), 1)
  testthat::expect_equal(max(horizons), forecast_horizon)

  check_exist(1:forecast_horizon, horizons)

  best_model_res <- back_test_data %>% dplyr::filter(
    Horizon == 1,
    Best_Model == "Yes"
  )

  testthat::expect_lt(mean(best_model_res$MAPE) * 100, 3)
})

test_that("final forecast data rows are meaningful", {
  types <- unique(final_fcst$Type)

  to_check <- c("Historical", "Forecast")
  check_exist(to_check, types)

  final_fc_dt <- final_fcst %>%
    dplyr::filter(Type == "Historical") %>%
    dplyr::filter(Date == max(Date)) %>%
    dplyr::pull()

  print(paste("First Dt Val", final_fc_dt))

  future_frame <- final_fcst %>%
    dplyr::filter(
      Date > final_fc_dt,
      Model == "Best-Model"
    )

  testthat::expect_equal(nrow(future_frame), forecast_horizon)
})

test_that("back test best MAPE is as expected", {
  best_models <- unlist(strsplit(back_test_best_MAPE$Model, "_"))
  testthat::expect_equal(best_models, "snaive--local--R1")
})

rm(finn_forecast)

# * standard HTS forecast ----

# Basic forecast to test various parts
forecast_horizon <- 3
target_variable <- "value"
combo_variables <- c("ID1", "id")
models_to_run <- c("meanf", "snaive")

inp_data <- hts::allts(hts::htseg1) %>%
  timetk::tk_tbl() %>%
  dplyr::mutate(Date = as.Date(paste0(index, "-07-01"))) %>%
  tidyr::pivot_longer(-c("index", "Date"), names_to = "id") %>%
  dplyr::filter(
    id != "Total",
    id != "A",
    id != "B"
  ) %>%
  tidyr::separate(id, sep = 1, into = c("ID1", "ID2"), remove = FALSE)

inp_data_combos <- inp_data %>%
  dplyr::mutate(Combo = paste0(ID1, id))

dt_type <- "year"

finn_forecast <- forecast_time_series(
  input_data = inp_data,
  combo_variables = combo_variables,
  target_variable = target_variable,
  date_type = dt_type,
  forecast_horizon = forecast_horizon,
  forecast_approach = "standard_hierarchy",
  run_model_parallel = FALSE,
  back_test_scenarios = 3,
  models_to_run = models_to_run,
  recipes_to_run = "R1",
  run_global_models = FALSE,
  run_ensemble_models = FALSE,
  average_models = FALSE
)

final_fcst <- finn_forecast$final_fcst %>%
  mutate(Date = as.Date(Date))
back_test_data <- finn_forecast$back_test_data
back_test_best_MAPE <- finn_forecast$back_test_best_MAPE

# tests
test_that("final forecast has all columns", {
  cols_to_check <- c(
    "Combo", "Date", "Type",
    "Model", "value", "lo.95",
    "lo.80", "hi.80", "hi.95"
  )
  retcols <- colnames(final_fcst)

  check_exist(cols_to_check, retcols)
})

test_that("back test data has all columns", {
  cols_to_check <- c(
    "Combo", "Date", "Model", "Horizon",
    "FCST", "Target", "Back_Test_Scenario",
    "MAPE"
  )
  retcols <- colnames(back_test_data)

  check_exist(cols_to_check, retcols)
})

test_that("back test data rows are meaningful", {
  horizons <- unique(back_test_data$Horizon)
  expect_equal(min(horizons), 1)
  expect_equal(max(horizons), forecast_horizon)

  check_exist(1:forecast_horizon, horizons)
})

test_that("final forecast data rows are meaningful", {
  types <- unique(final_fcst$Type)

  to_check <- c("Historical", "Forecast")
  check_exist(to_check, types)

  final_fc_dt <- final_fcst %>%
    dplyr::filter(Type == "Historical") %>%
    dplyr::filter(Date == max(Date)) %>%
    dplyr::pull()

  print(paste("First Dt Val", final_fc_dt))

  future_frame <- final_fcst %>%
    dplyr::filter(
      Date > final_fc_dt,
      Model == "Best-Model"
    )

  testthat::expect_equal(nrow(future_frame), length(unique(inp_data_combos$Combo)) * forecast_horizon)
})

test_that("back test best MAPE is as expected", {
  best_models <- unlist(strsplit(back_test_best_MAPE$Model, "_"))
  testthat::expect_equal(unique(best_models), "meanf--local--R1")
})

rm(finn_forecast)

# * grouped HTS forecast ----

# Basic forecast to test various parts
forecast_horizon <- 3
target_variable <- "value"
combo_variables <- c("State", "Sex")
models_to_run <- c("meanf", "snaive")

inp_data <- hts::infantgts %>%
  hts::allts() %>%
  timetk::tk_tbl() %>%
  dplyr::mutate(Date = as.Date(paste0(index, "-07-01"))) %>%
  dplyr::select(-c("Total", "Sex/female", "Sex/male", dplyr::contains("State/"))) %>%
  tidyr::pivot_longer(-c("index", "Date"), names_to = "id") %>%
  tidyr::separate(id, sep = " ", into = c("State", "Sex"), remove = FALSE) %>%
  dplyr::filter(
    Date >= "1985-07-01",
    State %in% c("NSW", "VIC")
  )

inp_data_combos <- inp_data %>%
  dplyr::mutate(Combo = paste0(State, Sex))

dt_type <- "year"

finn_forecast <- forecast_time_series(
  input_data = inp_data,
  combo_variables = combo_variables,
  target_variable = target_variable,
  date_type = dt_type,
  forecast_horizon = forecast_horizon,
  forecast_approach = "grouped_hierarchy",
  run_model_parallel = FALSE,
  back_test_scenarios = 3,
  models_to_run = models_to_run,
  recipes_to_run = "R1",
  run_global_models = FALSE,
  run_ensemble_models = FALSE,
  average_models = FALSE
)

final_fcst <- finn_forecast$final_fcst %>%
  mutate(Date = as.Date(Date))
back_test_data <- finn_forecast$back_test_data
back_test_best_MAPE <- finn_forecast$back_test_best_MAPE

# tests
test_that("final forecast has all columns", {
  cols_to_check <- c(
    "Combo", "Date", "Type",
    "Model", "value", "lo.95",
    "lo.80", "hi.80", "hi.95"
  )
  retcols <- colnames(final_fcst)

  check_exist(cols_to_check, retcols)
})

test_that("back test data has all columns", {
  cols_to_check <- c(
    "Combo", "Date", "Model", "Horizon",
    "FCST", "Target", "Back_Test_Scenario",
    "MAPE"
  )
  retcols <- colnames(back_test_data)

  check_exist(cols_to_check, retcols)
})

test_that("back test data rows are meaningful", {
  horizons <- unique(back_test_data$Horizon)
  expect_equal(min(horizons), 1)
  expect_equal(max(horizons), forecast_horizon)

  check_exist(1:forecast_horizon, horizons)
})

test_that("final forecast data rows are meaningful", {
  types <- unique(final_fcst$Type)

  to_check <- c("Historical", "Forecast")
  check_exist(to_check, types)

  final_fc_dt <- final_fcst %>%
    dplyr::filter(Type == "Historical") %>%
    dplyr::filter(Date == max(Date)) %>%
    dplyr::pull()

  print(paste("First Dt Val", final_fc_dt))

  future_frame <- final_fcst %>%
    dplyr::filter(
      Date > final_fc_dt,
      Model == "Best-Model"
    )

  testthat::expect_equal(nrow(future_frame), length(unique(inp_data_combos$Combo)) * forecast_horizon)
})

test_that("back test best MAPE is as expected", {
  best_models <- unlist(strsplit(back_test_best_MAPE$Model, "_"))
  testthat::expect_equal(unique(best_models), "meanf--local--R1")
})
