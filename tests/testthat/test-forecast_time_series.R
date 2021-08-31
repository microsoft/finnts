#Basic forecast to test various parts
forecast_horizon <- 3
target_variable <- "value"
combo_variables <- c("id")
models_to_run <- c("arima", "ets", "snaive")
inp_data <- m750 %>% dplyr::rename(Date = date) %>% dplyr::mutate(id = as.character(id))
dt_type <- "month"

finn_forecast <- forecast_time_series(
  input_data = inp_data, 
  combo_variables = combo_variables, 
  target_variable = target_variable, 
  date_type = dt_type, 
  forecast_horizon = forecast_horizon, 
  run_model_parallel = FALSE,
  models_to_run = models_to_run, 
  run_global_models = FALSE)

final_fcst <- finn_forecast$final_fcst
back_test_data <- finn_forecast$back_test_data
back_test_best_MAPE <- finn_forecast$back_test_best_MAPE

check_exist <-function(to_check,ret){
  for(elem in to_check){
    expect_true(elem %in% ret)
  }
}

test_that("final forecast has all columns", {
  
  cols_to_check <- c("Combo","Date","Type",
                     "Model","value", "lo.95",
                     "lo.80","hi.80","hi.95")
  retcols <- colnames(final_fcst)
  
  check_exist(cols_to_check,retcols)
  
})


test_that("back test data has all columns", {
  
  cols_to_check <- c("Combo","Date","Model","Horizon",
                     "FCST", "Target","Back_Test_Scenario",
                     "MAPE","Best_Model")
  retcols <- colnames(back_test_data)
  
  check_exist(cols_to_check,retcols)
  
})

test_that("back test data rows are meaningful", {
  
  horizons <- unique(back_test_data$Horizon)
  expect_equal(min(horizons),1)
  expect_equal(max(horizons),forecast_horizon)
  
  check_exist(1:forecast_horizon,horizons)
  
  best_model_res <- back_test_data %>% dplyr::filter(Horizon==1,
                                              Best_Model=='Yes')
  
  expect_lt(mean(best_model_res$MAPE)*100,1.5)
  
})

test_that("final forecast data rows are meaningful", {
  
  types <- unique(final_fcst$Type)
  to_check <- c("Historical","Forecast")
  check_exist(to_check,types)
  
  final_fc_dt <- max(final_fcst[final_fcst$Type=="Historical",'Date'])
  
  future_frame <- final_fcst %>% dplyr::filter(Date>final_fc_dt,
                                    Model=='Best Model')
  
  expect_equal(nrow(future_frame),forecast_horizon)
  
})

test_that("back test best MAPE is as expected", {
  best_models <- unlist(strsplit(back_test_best_MAPE$Model, "_"))
  check_exist(best_models,models_to_run)
})