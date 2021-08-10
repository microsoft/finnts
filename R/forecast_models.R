#' Gets combo specific filter
#' 
#' @param full_data_tbl Full Data Table
#' @param combo_value Combo Value
#' @param combo_variables Combo Variables List
#' 
#' @return combo_specific_filter function
combo_specific_filter <-function(full_data_tbl,
                                 combo_value,
                                 combo_variables){
  
  if(combo_value != "All-Data") {
    
    full_data_tbl %>%
      dplyr::filter(Combo == combo_value)
    
  } else {
    
    full_data_tbl %>%
      tidyr::separate(col = 'Combo', 
                      into = combo_variables,
                      remove=FALSE, 
                      sep= "--") 
  }
}



#' Get a Forecast Model Functions
#' 
#' @param full_data_tbl Full Data Table
#' @param external_regressors External Regressors
#' @param xregs_future_values_list External Regressors Future Values
#' @param fourier_periods Fourier Periods
#' @param combo_variables Combo Variables
#' @param lag_periods Lag Periods
#' @param rolling_window_periods Rolling window periods
#' @param hist_end_date Historical End Date
#' @param date_type Date Type
#' @param forecast_horizon Forecast Horizon
#' @param run_model_parallel Run Model in Parallel
#' @param parallel_processing Which parallel processing to use
#' @param parallel_init_func Init function for parallel module WITHIN
#' @param parallel_exit_func Exit fnction for parallel module WITHIN
#' @param frequency_number Frequency Number
#' @param models_to_run Models to Run
#' @param models_not_to_run Models not to run
#' @param hist_periods_80 Hist Periods 80
#' @param back_test_spacing Back Test Spacing
#' @param back_test_scenarios Back Test Scenarios
#' @param date_regex Date Regex
#' @param fiscal_year_start Fiscal Year Start
#' @param seasonal_periods Seasonal Periods
#' We need to move these out using a scheduler
#' 
#' @return a forecast_models function
construct_forecast_models <- function(full_data_tbl,
                                      external_regressors,
                                      xregs_future_values_list,
                                      fourier_periods,
                                      combo_variables,
                                      lag_periods,
                                      rolling_window_periods,
                                      hist_end_date,
                                      date_type,
                                      forecast_horizon,
                                      run_model_parallel,
                                      parallel_processing,
                                      parallel_init_func,
                                      parallel_exit_func,
                                      frequency_number,
                                      models_to_run,
                                      models_not_to_run,
                                      hist_periods_80,
                                      back_test_spacing,
                                      back_test_scenarios,
                                      date_regex,
                                      fiscal_year_start,
                                      seasonal_periods
                                      ){
  
  forecast_models <- function(combo_value) {
    
    print(combo_value)
    
    #filter on specific combo or all data
    model_name_suffix <-  ifelse(combo_value=="All-Data","-all","") 
    
    run_data_full_tbl <- full_data_tbl %>%
      combo_specific_filter(combo_value,
                            combo_variables)
    
    
    # recipe 1: standard feature engineering
    run_data_full_recipe_1 <- run_data_full_tbl %>%
      multivariate_prep_recipe_1(external_regressors = external_regressors, 
                                 xregs_future_values_list = xregs_future_values_list, 
                                 fourier_periods = fourier_periods, 
                                 lag_periods = lag_periods, 
                                 rolling_window_periods = rolling_window_periods)
    
    train_data_recipe_1 <- run_data_full_recipe_1 %>%
      dplyr::filter(Date <= hist_end_date)
    
    # return(train_data_recipe_1)
    
    test_data_recipe_1 <- run_data_full_recipe_1 %>%
      dplyr::filter(Date > hist_end_date)
    
    
    # recipe 2: custom horizon specific feature engineering
    run_data_full_recipe_2 <- run_data_full_tbl %>%
      multivariate_prep_recipe_2(external_regressors = external_regressors, 
                                 xregs_future_values_list = xregs_future_values_list, 
                                 fourier_periods = fourier_periods, 
                                 lag_periods = lag_periods, 
                                 rolling_window_periods = rolling_window_periods, 
                                 date_type = date_type, 
                                 forecast_horizon = forecast_horizon) %>%
      dplyr::mutate(Horizon_char = as.character(Horizon))
    
    train_data_recipe_2 <- run_data_full_recipe_2 %>%
      dplyr::filter(Date <= hist_end_date)
    
    train_origins <- train_data_recipe_2 %>%
      dplyr::filter(Horizon == 1)
    
    train_origin_max <- max(train_origins$Origin)
    
    test_data_recipe_2 <- run_data_full_recipe_2 %>%
      dplyr::filter(Date > hist_end_date,
                    Origin == train_origin_max+1)
    
    
    # create modeltime table to add single trained models to
    combined_models_recipe_1 <- modeltime::modeltime_table()
    combined_models_recipe_2 <- modeltime::modeltime_table()
    
    # parallel processing
    if(run_model_parallel==TRUE & parallel_processing!="local_machine") {
      
      parallel_args <- parallel_init_func
    }
    
    print("data_prepped")
    
    # Run Individual Time Series Models
    if(combo_value != "All-Data") {
      
      
      
      
      #Auto ARIMA
      #arima(train_data = train_data_recipe_1, frequency = frequency_number)
      try(arima <- arima(train_data = train_data_recipe_1,
                         frequency = frequency_number),
          silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, 
                                                                     arima, 
                                                                     location = "top") %>% 
            update_model_description(1, "arima"),
          silent = TRUE)
      
      
      #Arima with XGBoost applied to arima residuals
      try(arima_boost <- arima_boost(train_data = train_data_recipe_1, 
                                     frequency = frequency_number,
                                     parallel = run_model_parallel,
                                     horizon = forecast_horizon,
                                     tscv_initial = hist_periods_80,
                                     date_rm_regex = date_regex,
                                     back_test_spacing = back_test_spacing,
                                     fiscal_year_start = fiscal_year_start),
          silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1,
                                                                     arima_boost,
                                                                     location = "top") %>% 
            update_model_description(1, "arima-boost"),
          silent = TRUE)
      
      
      #Croston
      try(croston <- croston(train_data = train_data_recipe_1,
                             frequency = frequency_number),
          silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1,
                                                                     croston,
                                                                     location = "top") %>% 
            update_model_description(1, "croston"), 
          silent = TRUE)
      
      
      #ETS
      try(ets <- ets(train_data = train_data_recipe_1,
                     frequency = frequency_number),
          silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1,
                                                                     ets,
                                                                     location = "top") %>% 
            update_model_description(1, "ets"),
          silent = TRUE)
      
      
      #Simple average of latest year
      try(meanf <- meanf(train_data = train_data_recipe_1, frequency = ifelse(date_type == "week", 52, ifelse(date_type == "day", 365, frequency_number)), models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, meanf, location = "top") %>% update_model_description(1, "meanf"), silent = TRUE)
      
      #Neural Net Auto Regressive - NNETAR
      #nnetar <- nnetar(train_data = train_data, frequency = frequency_number, horizon = forecast_horizon, spacing = back_test_spacing, parallel = run_model_parallel)
      try(nnetar <- nnetar(train_data = train_data_recipe_1, frequency = frequency_number, horizon = forecast_horizon, parallel = run_model_parallel, models_to_run = models_to_run, models_not_to_run = models_not_to_run, tscv_initial = hist_periods_80, back_test_spacing = back_test_spacing), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, nnetar, location = "top") %>% update_model_description(1, "nnetar"), silent = TRUE)
      
      #Neural Net Auto Regressive - NNETAR with external regressors
      #nnetar_xregs <- nnetar_xregs(train_data = train_data_recipe_1, frequency = frequency_number, horizon = forecast_horizon, parallel = run_model_parallel, tscv_initial = hist_periods_80, date_rm_regex = date_regex, fiscal_year_start = fiscal_year_start, models_to_run = models_to_run, models_not_to_run = models_not_to_run, back_test_spacing = back_test_spacing)
      try(nnetar_xregs <- nnetar_xregs(train_data = train_data_recipe_1, frequency = frequency_number, horizon = forecast_horizon, parallel = run_model_parallel, tscv_initial = hist_periods_80, date_rm_regex = date_regex, fiscal_year_start = fiscal_year_start, models_to_run = models_to_run, models_not_to_run = models_not_to_run, back_test_spacing = back_test_spacing), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, nnetar_xregs, location = "top") %>% update_model_description(1, "nnetar-xregs"), silent = TRUE)
      
      #Prophet
      #prophet <- prophet(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, models_to_run = models_to_run, models_not_to_run = models_not_to_run)
      try(prophet <- prophet(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, prophet, location = "top") %>% update_model_description(1, "prophet"), silent = TRUE)
      
      #Prophet with XGBoost applied to Prophet residuals
      #prophet_boost <- prophet_boost(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex)
      try(prophet_boost <- prophet_boost(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, 
                                         back_test_spacing = back_test_spacing, fiscal_year_start = fiscal_year_start, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, prophet_boost, location = "top") %>% update_model_description(1, "prophet-boost"), silent = TRUE)
      
      #Prophet with external regressors
      #prophet_xregs <- prophet_xregs(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, models_to_run = models_to_run, models_not_to_run = models_not_to_run)
      try(prophet_xregs <- prophet_xregs(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, prophet_xregs, location = "top") %>% update_model_description(1, "prophet-xregs"), silent = TRUE)
      
      #Seasonal Naive
      #snaive <- snaive(train_data = train_data, frequency = frequency_number)
      #snaive <- snaive(train_data = train_data_recipe_1, frequency = ifelse(date_type == "week", 52, ifelse(date_type == "day", 365, frequency_number)))
      try(snaive <- snaive(train_data = train_data_recipe_1, frequency = ifelse(date_type == "week", 52, ifelse(date_type == "day", 365, frequency_number)), models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, snaive, location = "top") %>% modeltime::update_model_description(1, "snaive"), silent = TRUE)
      
      #STLM ARIMA
      try(stlm_arima <- stlm_arima(train_data = train_data_recipe_1, seasonal_period = seasonal_periods, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, stlm_arima, location = "top") %>% update_model_description(1, "stlm-arima"), silent = TRUE)
      
      #STLM ETS
      try(stlm_ets <- stlm_ets(train_data = train_data_recipe_1, seasonal_period = seasonal_periods, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, stlm_ets, location = "top") %>% update_model_description(1, "stlm-ets"), silent = TRUE)
      
      #TBATS
      try(tbats <- tbats(train_data = train_data_recipe_1, seasonal_period = seasonal_periods, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, tbats, location = "top") %>% update_model_description(1, "tbats"), silent = TRUE)
      
      #Theta
      #theta <- theta(train_data = train_data_recipe_1, frequency = frequency_number)
      try(theta <- theta(train_data = train_data_recipe_1, frequency = frequency_number, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
      try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, theta, location = "top") %>% update_model_description(1, "theta"), silent = TRUE)
    }
    
    #DeepAR Gluon TS 
    try(deepar <- deepar(train_data = train_data_recipe_1, horizon = forecast_horizon, frequency = gluon_ts_frequency, model_name = paste0("deepar", model_name_suffix), run_deep_learning = run_deep_learning, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent=TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, deepar, location = "top") %>% update_model_description(1, paste0("deepar", model_name_suffix)), silent=TRUE)
    
    #N-Beats Gluon TS 
    try(nbeats <- nbeats(train_data = train_data_recipe_1, horizon = forecast_horizon, frequency = gluon_ts_frequency, model_name = paste0("nbeats", model_name_suffix), run_deep_learning = run_deep_learning, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent=TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, nbeats, location = "top") %>% update_model_description(1, paste0("nbeats", model_name_suffix)), silent=TRUE)
    
    #Cubist Rules
    #cubist_r1 <- cubist(train_data = train_data_recipe_1, parallel = FALSE, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex)
    try(cubist_r1 <- cubist(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("cubist-R1", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, cubist_r1, location = "top") %>% update_model_description(1, paste0("cubist-R1", model_name_suffix)), silent = TRUE)
    
    try(cubist_r2 <- cubist(train_data = train_data_recipe_2, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("cubist-R2", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_2 <- modeltime::add_modeltime_model(combined_models_recipe_2, cubist_r2, location = "top") %>% update_model_description(1, paste0("cubist-R2", model_name_suffix)), silent = TRUE)
    
    #GLMNET
    try(glmnet_r1 <- glmnet(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("glmnet-R1", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, glmnet_r1, location = "top") %>% update_model_description(1, paste0("glmnet-R1", model_name_suffix)), silent = TRUE)
    
    try(glmnet_r2 <- glmnet(train_data = train_data_recipe_2, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("glmnet-R2", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_2 <- modeltime::add_modeltime_model(combined_models_recipe_2, glmnet_r2, location = "top") %>% update_model_description(1, paste0("glmnet-R2", model_name_suffix)), silent = TRUE)
    
    #MARS
    #mars_r1 <- mars(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("mars-R1", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run)
    try(mars_r1 <- mars(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("mars-R1", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, mars_r1, location = "top") %>% update_model_description(1, paste0("mars-R1", model_name_suffix)), silent = TRUE)
    
    #Support Vector Machine - Polynomial
    try(svm_poly_r1 <- svm_poly(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("svm-poly-R1", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, svm_poly_r1, location = "top") %>% update_model_description(1, paste0("svm-poly-R1", model_name_suffix)), silent = TRUE)
    
    try(svm_poly_r2 <- svm_poly(train_data = train_data_recipe_2, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("svm-poly-R2", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_2 <- modeltime::add_modeltime_model(combined_models_recipe_2, svm_poly_r2, location = "top") %>% update_model_description(1, paste0("svm-poly-R2", model_name_suffix)), silent = TRUE)
    
    #Support Vector Machine - Radial Basis Function
    try(svm_rbf_r1 <- svm_rbf(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("svm-rbf-R1", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, svm_rbf_r1, location = "top") %>% update_model_description(1, paste0("svm-rbf-R1", model_name_suffix)), silent = TRUE)
    
    try(svm_rbf_r2 <- svm_rbf(train_data = train_data_recipe_2, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("svm-rbf-R2", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_2 <- modeltime::add_modeltime_model(combined_models_recipe_2, svm_rbf_r2, location = "top") %>% update_model_description(1, paste0("svm-rbf-R2", model_name_suffix)), silent = TRUE)
    
    #XGBoost
    #xgboost_r1 <- xgboost(train_data = train_data_recipe_1, parallel = FALSE, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex)
    try(xgboost_r1 <- xgboost(train_data = train_data_recipe_1, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("xgboost-R1", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, xgboost_r1, location = "top") %>% update_model_description(1, paste0("xgboost-R1", model_name_suffix)), silent = TRUE)
    
    #xgboost_r2 <- xgboost(train_data = train_data_recipe_2, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex)
    try(xgboost_r2 <- xgboost(train_data = train_data_recipe_2, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, model_name = paste0("xgboost-R2", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_2 <- modeltime::add_modeltime_model(combined_models_recipe_2, xgboost_r2, location = "top") %>% update_model_description(1, paste0("xgboost-R2", model_name_suffix)), silent = TRUE)
    
    print(combined_models_recipe_1)
    print(combined_models_recipe_2)
    
    #create resamples for back testing forecasts and ensemble training data
    resamples_tscv_recipe_1 <- run_data_full_recipe_1 %>%
      timetk::time_series_cv(
        date_var = Date,
        initial = "1 year",
        assess = forecast_horizon,
        skip = back_test_spacing,
        cumulative = TRUE,
        slice_limit = 100)
    
    resamples_tscv_recipe_2 <- run_data_full_recipe_2 %>%
      timetk::time_series_cv(
        date_var = Date,
        initial = "1 year",
        assess = forecast_horizon,
        skip = back_test_spacing,
        cumulative = TRUE,
        slice_limit = 100) %>%
      timetk::tk_time_series_cv_plan()
    
    #correctly filter test split to correct horizons per date
    rsplit_function <- function(slice) {
      
      train <- resamples_tscv_recipe_2 %>%
        dplyr::filter(.id == slice, 
                      .key == "training")
      
      test <- resamples_tscv_recipe_2 %>%
        dplyr::filter(.id == slice, 
                      .key == "testing", 
                      Origin == max(train %>% dplyr::filter(Horizon == 1) %>% dplyr::select(Origin))+1)
      
      slice_tbl <- train %>%
        rbind(test) %>%
        dplyr::select(-.id, -.key)
      
      rsplit_obj <- slice_tbl %>%
        rsample::make_splits(ind = list(analysis = seq(nrow(train)), assessment = nrow(train) + seq(nrow(test))), class = "ts_cv_split")
      
      return(rsplit_obj)
    }
    
    split_objs <- purrr::map(unique(resamples_tscv_recipe_2$.id), .f=rsplit_function)
    
    resamples_tscv_recipe_2_final <- rsample::new_rset(splits = split_objs, ids = unique(resamples_tscv_recipe_2$.id), subclass = c("time_series_cv", "rset"))
    
    #refit models on resamples
    submodels_resample_tscv_recipe_1 <- tibble::tibble()
    submodels_resample_tscv_recipe_2 <- tibble::tibble()
    
    if(length(unique(combined_models_recipe_1$.model_desc)) > 0) {
      submodels_resample_tscv_recipe_1 <- combined_models_recipe_1 %>%
        modeltime.resample::modeltime_fit_resamples(
          resamples = resamples_tscv_recipe_1,
          control = tune::control_resamples(
            verbose = TRUE, 
            allow_par = run_model_parallel)) %>%
        modeltime.resample::unnest_modeltime_resamples() %>%
        dplyr::mutate(.id = .resample_id,
                      Model = .model_desc,
                      FCST = .pred) %>%
        dplyr::select(.id, Model, FCST, .row) %>%
        dplyr::left_join(
          resamples_tscv_recipe_1 %>%
            timetk::tk_time_series_cv_plan() %>%
            dplyr::group_by(.id) %>%
            dplyr::mutate(.row = dplyr::row_number()) %>%
            dplyr::ungroup()) %>%
        dplyr::select(.id, Combo, Model, FCST, Target, Date) %>%
        dplyr::group_by(.id, Combo, Model) %>%
        dplyr::mutate(Horizon = dplyr::row_number()) %>%
        dplyr::ungroup()
    }
    
    if(length(unique(combined_models_recipe_2$.model_desc)) > 0) {
      submodels_resample_tscv_recipe_2 <- combined_models_recipe_2 %>%
        modeltime.resample::modeltime_fit_resamples(
          resamples = resamples_tscv_recipe_2_final,
          control = tune::control_resamples(
            verbose = TRUE, 
            allow_par = run_model_parallel)) %>%
        modeltime.resample::unnest_modeltime_resamples() %>%
        dplyr::mutate(.id = .resample_id, 
                      Model = .model_desc, 
                      FCST = .pred) %>%
        dplyr::select(.id, Model, FCST, .row) %>%
        dplyr::left_join(
          resamples_tscv_recipe_2_final %>%
            timetk::tk_time_series_cv_plan() %>%
            dplyr::group_by(.id) %>%
            dplyr::mutate(.row = dplyr::row_number()) %>%
            dplyr::ungroup()) %>%
        dplyr::select(.id, Combo, Model, FCST, Target, Date, Horizon)
    }
    
    submodels_resample_tscv_tbl <- rbind(submodels_resample_tscv_recipe_1, submodels_resample_tscv_recipe_2)
    
    #Replace NaN/Inf with NA, then replace with zero
    is.na(submodels_resample_tscv_tbl) <- sapply(submodels_resample_tscv_tbl, is.infinite)
    is.na(submodels_resample_tscv_tbl) <- sapply(submodels_resample_tscv_tbl, is.nan)
    submodels_resample_tscv_tbl[is.na(submodels_resample_tscv_tbl)] = 0.01
    
    ensemble_train_data_initial <- submodels_resample_tscv_tbl %>% 
      dplyr::filter(Date <= hist_end_date) %>% 
      dplyr::select(-.id) %>%
      tidyr::pivot_wider(names_from = "Model", values_from = "FCST") %>%
      dplyr::mutate(Horizon_char = as.character(Horizon))
    
    #Replace NaN/Inf with NA, then replace with zero
    is.na(ensemble_train_data_initial) <- sapply(ensemble_train_data_initial, is.infinite)
    is.na(ensemble_train_data_initial) <- sapply(ensemble_train_data_initial, is.nan)
    ensemble_train_data_initial[is.na(ensemble_train_data_initial)] = 0.01
    
    print('prep_ensemble')
    
    #create modeltime table to add ensembled trained models to
    combined_ensemble_models <- modeltime::modeltime_table()
    
    #Cubist ensemble
    #cubist_ensemble <- cubist(train_data = submodels_resample_tscv_tbl %>% dplyr::filter(Date <= hist_end_date), parallel = run_model_parallel)
    try(cubist_ensemble <- cubist(train_data = ensemble_train_data_initial, parallel = run_model_parallel, model_type = 'ensemble', horizon = forecast_horizon, tscv_initial = "1 year", date_rm_regex = date_regex, model_name = paste0("cubist-ensemble", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_ensemble_models <- modeltime::add_modeltime_model(combined_ensemble_models, cubist_ensemble, location = "top") %>% update_model_description(1, paste0("cubist-ensemble", model_name_suffix)), silent = TRUE)
    
    #glmnet ensemble
    #glmnet_ensemble <- glmnet(train_data = ensemble_train_data_initial, parallel = FALSE, model_type = 'ensemble', horizon = forecast_horizon, tscv_initial = "1 year", date_rm_regex = date_regex)
    try(glmnet_ensemble <- glmnet(train_data = ensemble_train_data_initial, parallel = run_model_parallel, model_type = 'ensemble', horizon = forecast_horizon, tscv_initial = "1 year", date_rm_regex = date_regex, model_name = paste0("glmnet-ensemble", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_ensemble_models <- modeltime::add_modeltime_model(combined_ensemble_models, glmnet_ensemble, location = "top") %>% update_model_description(1, paste0("glmnet-ensemble", model_name_suffix)), silent = TRUE)
    
    #svm ensemble - polynomial
    try(svm_poly_ensemble <- svm_poly(train_data = ensemble_train_data_initial, parallel = run_model_parallel, model_type = 'ensemble', horizon = forecast_horizon, tscv_initial = "1 year", date_rm_regex = date_regex, model_name = paste0("svm-poly-ensemble", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_ensemble_models <- modeltime::add_modeltime_model(combined_ensemble_models, svm_poly_ensemble, location = "top") %>% update_model_description(1, paste0("svm-poly-ensemble", model_name_suffix)), silent = TRUE)
    
    #svm ensemble - radial basis function
    try(svm_rbf_ensemble <- svm_rbf(train_data = ensemble_train_data_initial, parallel = run_model_parallel, model_type = 'ensemble', horizon = forecast_horizon, tscv_initial = "1 year", date_rm_regex = date_regex, model_name = paste0("svm-rbf-ensemble", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_ensemble_models <- modeltime::add_modeltime_model(combined_ensemble_models, svm_rbf_ensemble, location = "top") %>% update_model_description(1, paste0("svm-rbf-ensemble", model_name_suffix)), silent = TRUE)
    
    #xgboost ensemble
    #xgboost_ensemble <- xgboost(train_data = ensemble_train_data_initial, parallel = run_model_parallel, model_type = 'ensemble', horizon = forecast_horizon, tscv_initial = "1 year")
    try(xgboost_ensemble <- xgboost(train_data = ensemble_train_data_initial, parallel = run_model_parallel, model_type = 'ensemble', horizon = forecast_horizon, tscv_initial = "1 year", date_rm_regex = date_regex, model_name = paste0("xgboost-ensemble", model_name_suffix), fiscal_year_start = fiscal_year_start, back_test_spacing = back_test_spacing, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_ensemble_models <- modeltime::add_modeltime_model(combined_ensemble_models, xgboost_ensemble, location = "top") %>% update_model_description(1, paste0("xgboost-ensemble", model_name_suffix)), silent = TRUE)
    
    print(combined_ensemble_models)
    
    #create ensemble resamples to train future and back test folds
    ensemble_tscv <- submodels_resample_tscv_tbl %>% 
      dplyr::select(-.id) %>%
      tidyr::pivot_wider(names_from = "Model", values_from = "FCST") %>%
      timetk::time_series_cv(
        date_var = Date,
        initial = "1 year",
        assess = forecast_horizon,
        skip = back_test_spacing,
        cumulative = TRUE,
        slice_limit = back_test_scenarios) %>%
      timetk::tk_time_series_cv_plan() %>%
      dplyr::mutate(Horizon_char = as.character(Horizon))
    
    #return(ensemble_tscv)
    
    #Replace NaN/Inf with NA, then replace with zero
    is.na(ensemble_tscv) <- sapply(ensemble_tscv, is.infinite)
    is.na(ensemble_tscv) <- sapply(ensemble_tscv, is.nan)
    ensemble_tscv[is.na(ensemble_tscv)] = 0.01
    
    #correctly filter test split to correct horizons per date
    rsplit_ensemble_function <- function(slice) {
      
      train <- ensemble_tscv %>%
        dplyr::filter(.id == slice, 
                      .key == "training")
      
      horizon <- 1
      
      test_dates <- ensemble_tscv %>%
        dplyr::filter(.id == slice, 
                      .key == "testing")
      
      test <- tibble::tibble()
      
      for(date in unique(test_dates$Date)) {
        
        date_horizon_tbl <- ensemble_tscv %>%
          dplyr::filter(.id == slice, 
                        .key == "testing", 
                        Date == date, 
                        Horizon == horizon)
        
        test <- rbind(test, date_horizon_tbl)
        
        horizon <- horizon+1
        
      }
      
      slice_tbl <- train %>%
        rbind(test) %>%
        dplyr::select(-.id, -.key)
      
      rsplit_obj <- slice_tbl %>%
        rsample::make_splits(ind = list(analysis = seq(nrow(train)), assessment = nrow(train) + seq(nrow(test))), class = "ts_cv_split")
      
      return(rsplit_obj)
    }
    
    ensemble_split_objs <- purrr::map(unique(ensemble_tscv$.id), .f=rsplit_ensemble_function)
    
    ensemble_tscv_final <- rsample::new_rset(splits = ensemble_split_objs, ids = unique(ensemble_tscv$.id), subclass = c("time_series_cv", "rset"))
    
    fcst_tbl <- tibble::tibble()
    
    if(length(unique(combined_ensemble_models$.model_desc)) > 0) {
      ensemble_fcst <- combined_ensemble_models %>%
        modeltime.resample::modeltime_fit_resamples(
          resamples = ensemble_tscv_final,
          control = tune::control_resamples(
            verbose = TRUE,
            allow_par = run_model_parallel)) %>%
        modeltime.resample::unnest_modeltime_resamples() %>%
        dplyr::mutate(.id = .resample_id, 
                      Model = .model_desc, 
                      FCST = .pred) %>%
        dplyr::select(.id, Model, FCST, .row) %>%
        dplyr::left_join(
          ensemble_tscv_final %>%
            timetk::tk_time_series_cv_plan() %>%
            dplyr::group_by(.id) %>%
            dplyr::mutate(.row = dplyr::row_number()) %>%
            dplyr::ungroup()) %>%
        dplyr::select(.id, Combo, Model, FCST, Target, Date, Horizon)
      
      fcst_tbl <- ensemble_fcst %>%
        tidyr::separate(col=.id, sep="Slice", into=c("Slice", "Number")) %>%
        dplyr::mutate(Number = as.numeric(Number)) %>%
        dplyr::filter(Number == 1) %>%
        dplyr::select(-Slice, -Number) %>%
        dplyr::mutate(.id = "Final_FCST") %>%
        rbind(
          ensemble_fcst %>%
            dplyr::filter(Date <= hist_end_date) %>%
            tidyr::separate(col=.id, sep="Slice", into=c("Slice", "Number")) %>%
            dplyr::mutate(Number = as.numeric(Number) - 1) %>%
            dplyr::mutate(Number_Char = ifelse(Number < 10, paste0("0", Number), paste0("", Number)), 
                          .id = paste0("Back_Test_", Number_Char)) %>%
            dplyr::select(-Slice, -Number, -Number_Char))
    }
    
    #stop parallel processing
    if(run_model_parallel==TRUE & parallel_processing!="local_machine"){
      parallel_exit_func(parallel_args)
    }
    
    #Create forecast output
    fcst_tbl <- fcst_tbl %>%
      rbind(
        submodels_resample_tscv_tbl %>%
          tidyr::separate(col=.id, sep="Slice", into=c("Slice", "Number")) %>%
          dplyr::mutate(Number = as.numeric(Number)) %>%
          dplyr::filter(Number == 1) %>%
          dplyr::select(-Slice, -Number) %>%
          dplyr::mutate(.id = "Final_FCST")) %>%
      rbind(
        submodels_resample_tscv_tbl %>%
          dplyr::filter(Date <= hist_end_date) %>%
          tidyr::separate(col=.id, sep="Slice", into=c("Slice", "Number")) %>%
          dplyr::mutate(Number = as.numeric(Number) - 1) %>%
          dplyr::filter(Number < back_test_scenarios) %>%
          dplyr::mutate(Number_Char = ifelse(Number < 10, paste0("0", Number), paste0("", Number)), 
                        .id = paste0("Back_Test_", Number_Char)) %>%
          dplyr::select(-Slice, -Number, -Number_Char))
    
    return(fcst_tbl)
  }
  
  return (forecast_models)
}


