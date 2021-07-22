forecast_models <- function(combo_value) {
  
  print(combo_value)
  
  #filter on specific combo or all data
  model_name_suffix <- ""
  
  if(combo_value != "All-Data") {
    
    run_data_full_tbl <- full_data_tbl %>%
      dplyr::filter(Combo == combo_value)
    
  } else {
    
    run_data_full_tbl <- full_data_tbl %>%
      tidyr::separate(col = 'Combo', 
                      into = combo_variables,
                      remove=FALSE, 
                      sep= "--") 
    
    model_name_suffix <- "-all"
  }
  
  
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
    
    cores <- parallel::detectCores()
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    #point to the correct libraries within Azure Batch
    if(parallel_processing=="azure_batch") {
      clusterEvalQ(cl, .libPaths("/mnt/batch/tasks/shared/R/packages"))
    }
  }
  
  print("data_prepped")
  
  # Run Individual Time Series Models
  if(combo_value != "All-Data") {
    
    #Auto ARIMA
    #arima(train_data = train_data_recipe_1, frequency = frequency_number)
    try(arima <- arima(train_data = train_data_recipe_1, frequency = frequency_number, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, arima, location = "top") %>% update_model_description(1, "arima"), silent = TRUE)
    
    #Arima with XGBoost applied to arima residuals
    #arima_boost <- arima_boost(train_data = train_data_recipe_1, frequency = frequency_number, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex)
    try(arima_boost <- arima_boost(train_data = train_data_recipe_1, frequency = frequency_number, parallel = run_model_parallel, horizon = forecast_horizon, tscv_initial = hist_periods_80, date_rm_regex = date_regex, back_test_spacing = back_test_spacing, fiscal_year_start = fiscal_year_start, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, arima_boost, location = "top") %>% update_model_description(1, "arima-boost"), silent = TRUE)
    
    #Croston
    #croston(train_data = train_data_recipe_1, frequency = frequency_number)
    try(croston <- croston(train_data = train_data_recipe_1, frequency = frequency_number, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, croston, location = "top") %>% update_model_description(1, "croston"), silent = TRUE)
    
    #ETS
    try(ets <- ets(train_data = train_data_recipe_1, frequency = frequency_number, models_to_run = models_to_run, models_not_to_run = models_not_to_run), silent = TRUE)
    try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1, ets, location = "top") %>% update_model_description(1, "ets"), silent = TRUE)
    
    #Simple average of latest year
    #meanf <- meanf(train_data = train_data, frequency = frequency_number)
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
  #deepar <- deepar(train_data = train_data_recipe_1, horizon = forecast_horizon, frequency = gluon_ts_frequency, model_name = paste0("deepar", model_name_suffix))
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
  
  #LightGBM
  #lightgbm <- lightgbm(train_data = train_data, parallel = run_model_parallel)
  #combined_models <- modeltime::add_modeltime_model(combined_models, lightgbm, location = "top") %>% update_model_description(1, paste0("lightgbm", model_name_suffix))
  
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
  
  #TabNet Deep Learning
  #tabnet <- tabnet(train_data = train_data, parallel = FALSE)
  #try(tabnet <- tabnet(train_data = train_data, parallel = run_model_parallel), silent = TRUE)
  #try(combined_models <- modeltime::add_modeltime_model(combined_models, tabnet, location = "top") %>% update_model_description(1, paste0("tabnet", model_name_suffix)), silent = TRUE)
  #print('tabnet')
  
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
  
  #Light GBM ensemble
  #lightgbm_ensemble <- lightgbm_ensemble(fitted_resamples = submodels_resample_tscv_tbl, parallel = run_model_parallel)
  #combined_models <- modeltime::add_modeltime_model(combined_models, lightgbm_ensemble, location = "top") %>% update_model_description(1, paste0("lightgbm-ensemble", model_name_suffix))
  
  #MARS ensemble
  #try(mars_ensemble <- mars_ensemble(fitted_resamples = submodels_resample_tscv_tbl, parallel = run_model_parallel), silent= TRUE)
  #try(combined_models <- modeltime::add_modeltime_model(combined_models, mars_ensemble, location = "top") %>% update_model_description(1, paste0("mars-ensemble", model_name_suffix)), silent = TRUE)
  #print('mars-ensemble')
  
  #nnet ensemble
  #nnet_ensemble <- nnet_ensemble(fitted_resamples = submodels_resample_tscv_tbl, parallel = run_model_parallel)
  #combined_models <- modeltime::add_modeltime_model(combined_models, nnet_ensemble, location = "top") %>% update_model_description(1, paste0("nnet-ensemble", model_name_suffix))
  
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
  
  #Average of all models
  #try(ensemble_fit_mean <- combined_models %>% modeltime.ensemble::ensemble_average(type = "mean"), silent = TRUE)
  #try(combined_models <- modeltime::add_modeltime_model(combined_models, ensemble_fit_mean, location = "top") %>% update_model_description(1, paste0("average-ensemble", model_name_suffix)), silent = TRUE)
  #print('avg-ensemble')
  
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
    #dplyr::mutate(.id = paste0("Back_Test_", as.numeric(Number)-1)) %>%
    #dplyr::select(-Number, -Slice))
  }
  
  #stop parallel processing
  if(run_model_parallel==TRUE & parallel_processing!="local_machine") {parallel::stopCluster(cl)}
  
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

# * Run Forecast ----
if(forecast_approach == "bottoms_up" & length(unique(full_data_tbl$Combo)) > 1 & run_all_data) {
  combo_list <- c('All-Data', unique(full_data_tbl$Combo))
} else{
  combo_list <- unique(full_data_tbl$Combo)
}

# no parallel processing
if(parallel_processing == "none") {
  
  fcst <- lapply(combo_list, forecast_models)
  fcst <- do.call(rbind, fcst)
}

# parallel run on local machine
if(parallel_processing=="local_machine") {
  
  cl <- parallel::makeCluster(detectCores())
  doParallel::registerDoParallel(cl)
  
  fcst <- foreach(i = combo_list, .combine = 'rbind',
                  .packages = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                'timetk', 'hts', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                "doParallel", "parallel"), 
                  .export = c("models", "arima", "arima_boost", "croston", "cubist", "deepar", "ets", "glmnet", "lightgbm", "mars",
                              "meanf", "nbeats", "nnetar", "prophet", "prophet_boost", "snaive", "stlm_arima", "stlm_ets", 
                              "svm_poly", "svm_rbf", "tbats", "tabnet", "theta", "xgboost", 
                              "multivariate_prep_recipe_1", "multivariate_prep_recipe_2")) %dopar% {forecast_models(i)}
  
  parallel::stopCluster(cl)
  
}

# parallel run within azure batch
if(parallel_processing=="azure_batch") {
  
  doAzureParallel::setCredentials(azure_batch_credentials)
  cluster <- doAzureParallel::makeCluster(azure_batch_clusterConfig)
  doAzureParallel::registerDoAzureParallel(cluster)
  
  
  fcst <- foreach(i = combo_list, .combine = 'rbind',
                  .packages = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                "doParallel", "parallel"), 
                  .export = c("models", "arima", "arima_boost", "croston", "cubist", "deepar", "ets", "glmnet", "lightgbm", "mars",
                              "meanf", "nbeats", "nnetar", "prophet", "prophet_boost", "snaive", "stlm_arima", "stlm_ets", 
                              "svm_poly", "svm_rbf", "tbats", "tabnet", "theta", "xgboost", 
                              "multivariate_prep_recipe_1", "multivariate_prep_recipe_2"),
                  .options.azure = list(maxTaskRetryCount = 0, autoDeleteJob = TRUE, 
                                        job = substr(paste0('finn-fcst-', strftime(Sys.time(), format="%H%M%S"), '-', 
                                                            tolower(gsub(" ", "-", trimws(gsub("\\s+", " ", gsub("[[:punct:]]", '', run_name)))))), 1, 63)),
                  .errorhandling = "remove") %dopar% {forecast_models(i)}
  
}

#Replace NaN/Inf with NA, then replace with zero
is.na(fcst) <- sapply(fcst, is.infinite)
is.na(fcst) <- sapply(fcst, is.nan)
fcst[is.na(fcst)] = 0

# convert negative forecasts to zero
if(negative_fcst == FALSE) {fcst$FCST <- replace(fcst$FCST, which(fcst$FCST < 0), 0)}

# * Create Average Ensembles ----

fcst_combination <- tibble::tibble(fcst)

#model average combinations
model_list <- unique(fcst$Model)

if(length(model_list) > 1 & average_models) {
  
  fcst_prep <- fcst %>%
    tidyr::pivot_wider(names_from = "Model", values_from = "FCST") %>%
    tidyr::pivot_longer(!c(".id", "Combo", "Target", "Date", "Horizon"), names_to='Model', values_to = "FCST") %>%
    dplyr::mutate(FCST = ifelse(is.na(FCST), 0, FCST), 
                  Target = ifelse(is.na(Target), 0, Target))
  
  
  create_model_averages <- function(combination) {
    
    model_combinations <- data.frame(gtools::combinations(v=model_list, n=length(model_list), r=combination))
    model_combinations$All <- model_combinations %>% tidyr::unite(All, colnames(model_combinations))
    model_combinations <- model_combinations$All
    
    
    #parallel processing
    if(run_model_parallel==TRUE & parallel_processing!="local_machine") {
      
      cores <- parallel::detectCores()
      cl <- parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)
      
      #point to the correct libraries within Azure Batch
      if(parallel_processing=="azure_batch") {
        clusterEvalQ(cl, .libPaths("/mnt/batch/tasks/shared/R/packages"))
      }
      
      combinations_tbl <-  foreach::foreach(i = model_combinations[[1]], .combine = 'rbind', 
                                            .packages = c('rlist', 'tidyverse', 'lubridate', 
                                                          "doParallel", "parallel", "gtools"), 
                                            .export = c("fcst_prep")) %dopar% {
                                              
                                              fcst_combination_temp <- fcst_prep %>%
                                                dplyr::filter(Model %in% strsplit(i, split = "_")[[1]]) %>%
                                                dplyr::group_by(.id, Combo, Date, Horizon) %>%
                                                dplyr::summarise(FCST = mean(FCST, na.rm=TRUE), 
                                                                 Target = mean(Target, nam.rm=FALSE)) %>%
                                                dplyr::ungroup() %>%
                                                dplyr::mutate(Model = i)
                                              
                                              return(fcst_combination_temp)
                                              
                                            }
      
      #stop parallel processing
      if(run_model_parallel==TRUE & parallel_processing!="local_machine") {parallel::stopCluster(cl)}
      
    } else {
      
      combinations_tbl <-  foreach::foreach(i = model_combinations[[1]], .combine = 'rbind') %do% {
        
        fcst_combination_temp <- fcst_prep %>%
          dplyr::filter(Model %in% strsplit(i, split = "_")[[1]]) %>%
          dplyr::group_by(.id, Combo, Date, Horizon) %>%
          dplyr::summarise(FCST = mean(FCST, na.rm=TRUE), 
                           Target = mean(Target, nam.rm=FALSE)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Model = i)
        
        return(fcst_combination_temp)
        
      }
    }
    
    return(combinations_tbl)
  }
  
  # no parallel processing
  if(parallel_processing == "none") {
    
    combinations_tbl_final <- lapply(2:min(max_model_average, length(model_list)), create_model_averages)
    combinations_tbl_final <- do.call(rbind, combinations_tbl_final)
  }
  
  # parallel run on local machine
  if(parallel_processing=="local_machine") {
    
    cl <- parallel::makeCluster(detectCores())
    doParallel::registerDoParallel(cl)
    
    combinations_tbl_final <- foreach(i = 2:min(max_model_average, length(model_list)), .combine = 'rbind',
                                      .packages = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                                    'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                                    'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                                    "doParallel", "parallel"), 
                                      .export = c("fcst_prep")) %dopar% {create_model_averages(i)}
    
    parallel::stopCluster(cl)
    
  }
  
  # parallel run within azure batch
  if(parallel_processing=="azure_batch") {
    
    
    combinations_tbl_final <- foreach(i = 2:min(max_model_average, length(model_list)), .combine = 'rbind',
                                      .packages = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                                    'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                                    'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                                    "doParallel", "parallel"), 
                                      .export = c("fcst_prep"),
                                      .options.azure = list(maxTaskRetryCount = 0, autoDeleteJob = TRUE, 
                                                            job = substr(paste0('finn-model-avg-combo-', strftime(Sys.time(), format="%H%M%S"), '-', 
                                                                                tolower(gsub(" ", "-", trimws(gsub("\\s+", " ", gsub("[[:punct:]]", '', run_name)))))), 1, 63)),
                                      .errorhandling = "remove") %dopar% {create_model_averages(i)}
    
  }
  
  if(parallel_processing == 'azure_batch' & azure_batch_cluster_delete == TRUE) {
    stopCluster(cluster)
  }
  
  # combine with individual model data
  fcst_combination <- rbind(fcst_combination, combinations_tbl_final)
}