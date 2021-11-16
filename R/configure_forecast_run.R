#' Gets the right fourier periods
#' 
#' Checks if fourier_periods is set if not gets right one
#' 
#' 
#' @param fourier_periods fourier_periods override
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns fourier_periods
#' @noRd
get_fourier_periods <- function(fourier_periods,
                                date_type){
  
  if(!is.null(fourier_periods)) {
    return(fourier_periods)
  }
  
  fourier_periods <- switch(date_type,
                            "year" = c(1,2,3,4,5),
                            "quarter" = c(1,2,3,4),
                            "month" = c(3, 6, 9, 12),
                            "week" = c(2, 4, 8, 12, 24, 48, 52),
                            "day" = c(7, 14, 21, 28, 28*2, 
                                      28*3, 28*6, 28*9, 28*12, 365))  

  return(fourier_periods)
}

#' Gets the right lag periods
#' 
#' Checks if lag_periods is set if not gets right one
#' 
#' 
#' @param lag_periods lag_periods override
#' @param date_type year, quarter, month, week, day
#' @param forecast_horizon horizon input from user
#' 
#' @return Returns lag_periods
#' @noRd
get_lag_periods <- function(lag_periods, 
                            date_type,
                            forecast_horizon){
  
  if(!is.null(lag_periods)) {
    return(lag_periods)
  } 
  oplist <- switch (date_type,
                    "year" = c(1,2,3),
                    "quarter" = c(1,2,3,4),
                    "month" =  c(1, 2, 3, 6, 9, 12),
                    "week" = c(1, 2, 3, 4, 8, 12, 24, 48, 52),
                    "day" = c(7, 14, 21, 28, 60, 90, 180, 365)
                    )
  
  oplist <- c(oplist,forecast_horizon)
  lag_periods <- oplist[oplist >= forecast_horizon]
  lag_periods <- unique(lag_periods)
  
  return(lag_periods)
}

#' Gets the right rolling window periods
#' 
#' Checks if rolling_window_periods is set if not gets right one
#' 
#' 
#' @param rolling_window_periods rolling_window_periods override
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns rolling_window_periods
#' @noRd
get_rolling_window_periods <- function(rolling_window_periods,
                                date_type){
  
  if(!is.null(rolling_window_periods)) {
    return(rolling_window_periods)
  }
  
  rolling_window_periods <- switch(date_type,
                            "year" = c(2,3,4,5),
                            "quarter" = c(2,3,4),
                            "month" = c(3, 6, 9, 12),
                            "week" = c(2, 4, 8, 12, 24, 48, 52),
                            "day" = c(7, 14, 21, 28, 28*2, 
                                      28*3, 28*6, 28*9, 28*12, 365))  
  
  return(rolling_window_periods)
}

#' Gets the right frequency numbers
#' 
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns frequency_number
#' @noRd
get_frequency_number <- function(date_type){
  
  frequency_number <- switch(date_type,
                                   "year" = 1,
                                   "quarter" = 4,
                                   "month" = 12,
                                   "week" = 365.25/7,
                                   "day" = 365.25)  
  
  return(frequency_number)
}

#' Gets the right gluon ts frequency
#' 
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns gluon_ts_frequency
#' @noRd
get_gluon_ts_frequency <- function(date_type){
  
  gluon_ts_frequency <- switch(date_type,
                             "year" = "Y",
                             "quarter" = "Q",
                             "month" = "M",
                             "week" = "W",
                             "day" = "D")  
  
  return(gluon_ts_frequency)
}

#' Gets the seasonal periods
#' 
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns seasonal_periods
#' @noRd
get_seasonal_periods <- function(date_type){
  
  seasonal_periods <- switch(date_type,
                               "year" = c(1, 2, 3),
                               "quarter" = c(4, 6, 12),
                               "month" = c(12, 6, 4),
                               "week" = c(365.25/7, (365.25/7)/4, (365.25/7)/12),
                               "day" =  c(365.25, 365.25/4, 365.25/12))  
  
  return(seasonal_periods)
}

#' Gets the date regex for pasing
#' 
#' Checks if date_regex  is set if not gets right one
#' 
#' 
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns date_regex
#' @noRd
get_date_regex <- function(date_type){
  
  date_regex <- switch(date_type,
                             "year" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(half)|(quarter)|(month)|(week)|(day)",
                             "quarter" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(month)|(week)|(day)",
                             "month" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(day)",
                             "week" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(day)",
                             "day" =   "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")  
  
  return(date_regex)
}

#' Gets the back testing spacing
#' 
#' Checks if back_test_spacing is set to NULL and gets the right one
#' 
#' 
#' @param back_test_spacing back_test_spacing override
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns back_test_spacing
#' @noRd
get_back_test_spacing <- function(back_test_spacing,
                                       date_type){
  
  if(!is.null(back_test_spacing)) {
    return(back_test_spacing)
  }
  
  
  back_test_spacing <- switch (date_type,
                               "day" = 7,
                               "week" = 4,
                               1)
  return(back_test_spacing)
}


#' Gets the back testing scenarios
#' 
#' Gets back testing scenarios accounting for splits
#' 
#' 
#' @param full_data_tbl full data table
#' @param hist_end_date historical end date
#' @param back_test_scenarios back test scenarios
#' @param forecast_horizon forecast horizon
#' @param back_test_spacing back test spacing
#'  
#' @return Returns back_test_scenarios and hist_periods_80
#' @noRd
get_back_test_scenario_hist_periods<- function(full_data_tbl,
                                              hist_end_date,
                                              back_test_scenarios,
                                              forecast_horizon,
                                              back_test_spacing){
            
  historical_periods <- full_data_tbl %>%
    dplyr::filter(Date <= hist_end_date) %>%
    dplyr::select(Date) %>%
    unique() %>%
    nrow() %>%
    as.numeric()
  
  #historical_periods, back_test_scenarios, forecast_horizon,back_test_spacing
  
  hist_periods_80 <- floor(historical_periods*0.7) #used with time series CV in multivariate models
  
  if(is.null(back_test_scenarios)) {
    
    historical_periods_20 <- floor(historical_periods*0.2)
    
    #account for initial back tests that are smaller than the forecast horizon (1, 2, 3, etc up to fcst horizon)
    if(historical_periods_20 > forecast_horizon) {
      back_test_scenarios <- floor(historical_periods_20/back_test_spacing)
    } else {
      back_test_scenarios <- floor(forecast_horizon/back_test_spacing)
    }
  }
  
  back_test_scenarios <- back_test_scenarios + 1
  
  return (list(hist_periods_80=hist_periods_80,
               back_test_scenarios = back_test_scenarios))
}

#' Fetches a list of packages to be exported
#' 
#' Code gets used in multiple places where we need the packages list
#' 
#' @return Simple list of packages
#' @noRd
get_export_packages <- function(){
  c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
    'timetk', 'rlist', 'rules', 'Cubist', 'glmnet', 'earth', 'kernlab', 'xgboost',
    'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
    "doParallel", "parallel")
}

#' Fetches a list of parallel transfer functions
#' 
#' @return List of parallel transfer functions
#' @noRd
get_transfer_functions <- function(){
  c("arima", "arima_boost", "croston", "cubist", "deepar", "ets", "glmnet", "mars",
    "meanf", "nbeats", "nnetar", "nnetar_xregs", "prophet", "prophet_boost", "prophet_xregs", 
    "snaive", "stlm_arima", "stlm_ets", "svm_poly", "svm_rbf", "tbats", "tabnet", "theta", "xgboost", 
    "multivariate_prep_recipe_1", "multivariate_prep_recipe_2","combo_specific_filter",
    "construct_forecast_models", "get_model_functions", "get_not_all_data_models", 
    "get_r1_data_models", "get_r2_data_models", "get_deep_learning_models", "get_frequency_adjustment_models", 
    "invoke_forecast_function", "get_recipie_simple", "get_workflow_simple", "get_fit_simple", "get_freq_adjustment",
    "get_recipie_configurable", "get_fit_wkflw_nocombo", "get_latin_hypercube_grid", "get_resample_tune_grid", 
    "get_resample_tscv", "get_tune_grid", "get_not_all_data_models", "get_export_packages", "get_fit_wkflw_best",
    "get_kfold_tune_grid", "get_resample_kfold", "init_parallel_within","exit_parallel_within", "get_cores")
}

