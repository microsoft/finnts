#' Finn Forecast Framework
#' 
#' Calls the Finn forecast framework to automatically forecast any historical time series. 
#' 
#' @param input_data A data frame or tibble of historical time series data. Can also include external regressors for both 
#'   historical and future data. 
#' @param combo_variables List of column headers within input data to be used to separate individual time series. 
#' @param target_variable The column header formatted as a character value within input data you want to forecast.
#' @param date_type The date granularity of the input data. Finn accepts the following as a character string
#'   day, week, month, quarter, year.
#' @param forecast_horizon Number of periods to forecast into the future.
#' @param external_regressors List of column headers within input data to be used as features in multivariate models.
#' @param run_name Name used when submitting jobs to external compute like Azure Batch. Formatted as a character string. 
#' @param hist_start_date Date value of when your input_data starts. Default of NULL is to use earliest date value in 
#'   input_data.
#' @param hist_end_date Date value of when your input_data ends.Default of NULL is to use the latest date value in 
#'   input_data.
#' @param combo_cleanup_date Date value to remove individual time series that don't contain non-zero values after 
#'   that specified date. Default of NULL is to not remove any time series and attempt to forecast all of them. 
#' @param fiscal_year_start Month number of start of fiscal year of input data, aids in building out date features. 
#'   Formatted as a numeric value. Default of 1 assumes fiscal year starts in January. 
#' @param clean_missing_values Should missing values be inputted? Only inputes values for missing data within an 
#'   existing series, and does not add new values onto the beginning or end, but does provide a value of 0 for said 
#'   values. 
#' @param clean_outliers Should outliers be cleaned and inputted with values more in line with historical data?
#' @param back_test_scenarios Number of specific back test folds to run when determining the best model. 
#'   Default of 'auto' will automatially choose the number of back tests to run based on historical data size, 
#'   which tries to always use a minimum of 80% of the data when training a model. 
#' @param back_test_spacing Number of periods to move back for each back test scenario. Default of 'auto' moves back 1
#'   period at a time for year, quarter, and month data. Moves back 4 for week and 7 for day data. 
#' @param modeling_approach How Finn should approach your data. Current default and only option is 'accuracy'. In the 
#'   future this could evolve to other areas like optimizing for interpretability over accuracy. 
#' @param forecast_approach How the forecast is created. The default of 'bottoms_up' trains models for each individual 
#'   time series. 'grouped_hierarchy' creates a grouped time series to forecast at while 'standard_hierarchy' creates 
#'   a more traditional hierarchical time series to forecast, both based on the hts package.   
#' @param parallel_processing Default of 'none' runs no parallel processing and forecasts each individual time series
#'   one after another. 'local_machine' leverages all cores on current machine Finn is running on. 'azure_batch'
#'   runs time series in parallel on a remote compute cluster in Azure Batch. 
#' @param run_model_parallel Run model training in parallel, only works when parallel_processing is set to 
#'   'local_machine' or 'azure_batch'.
#' @param azure_batch_credentials Credentials to run parallel_processing in Azure Batch.
#' @param azure_batch_cluster_config Compute cluster specification to run parallel_processing in Azure Batch.
#' @param azure_batch_cluster_delete Delete the Azure Batch compute cluster after Finn finished running. 
#' @param target_log_transformation Log transform target variable before training models. 
#' @param negative_fcst Allow forecasts to dip below zero. 
#' @param fourier_periods List of values to use in creating fourier series as features. Default of NULL automatically chooses 
#'   these values based on the date_type. 
#' @param lag_periods List of values to use in creating lag features. Default of NULL automatically chooses these values 
#'   based on date_type. 
#' @param rolling_window_periods List of values to use in creating rolling window features. Default of NULL automatically 
#'   chooses these values based on date_type. 
#' @param reticulate_environment File path to python environment to use when training gluonts deep learning models. 
#'   Only important when parallel_processing is not set to 'azure_batch'. Azure Batch should use its own docker image 
#'   that has python environment already installed. 
#' @param models_to_run List of models to run. Default of NULL runs all models. 
#' @param models_not_to_run List of models not to run, overrides values in models_to_run. Default of NULL doesn't turn off 
#'   any model. 
#' @param run_deep_learning Run deep learning models from gluonts (deepar and nbeats). Overrides models_to_run and 
#'  models_not_to_run. 
#' @param run_global_models Run multivariate models on the entire data set (across all time series) as a global model. 
#'   Can be override by models_not_to_run. 
#' @param run_local_models Run models by individual time series as local models.
#' @param run_ensemble_models Run ensemble models 
#' @param average_models Create simple averages of individual models. 
#' @param max_model_average Max number of models to average together. Will create model averages for 2 models up until input value 
#'   or max number of models ran.
#' @param weekly_to_daily Convert a week forecast down to day by evenly splitting across each day of week. Helps when aggregating 
#'   up to higher temporal levels like month or quarter. 
#' 
#' @return A list of three separate data sets: the future forecast, the back test results, and the best model per time series.
#' 
#' @export
#' @examples
#' \dontrun{
#' finn_forecast <- forecast_time_series(
#'   input_data = m750 %>% dplyr::rename(Date = date), 
#'   combo_variables = c("id"), 
#'   target_variable = "value", 
#'   date_type = "month", 
#'   forecast_horizon = 3, 
#'   run_model_parallel = FALSE,
#'   models_to_run = c("arima", "ets", "snaive"))
#' }
forecast_time_series <- function(input_data,
  combo_variables,
  target_variable,
  date_type,
  forecast_horizon,
  external_regressors = NULL,
  run_name = "time_series_forecast",
  hist_start_date = NULL,
  hist_end_date = NULL,
  combo_cleanup_date = NULL,
  fiscal_year_start = 1,
  clean_missing_values = TRUE, 
  clean_outliers = FALSE, 
  back_test_scenarios = "auto",
  back_test_spacing = "auto",
  modeling_approach = "accuracy",
  forecast_approach = "bottoms_up",
  parallel_processing = 'none',
  run_model_parallel = TRUE,
  azure_batch_credentials = NULL, 
  azure_batch_cluster_config = NULL, 
  azure_batch_cluster_delete = FALSE, 
  target_log_transformation = FALSE,
  negative_fcst = FALSE,
  fourier_periods = NULL, 
  lag_periods = NULL, 
  rolling_window_periods = NULL, 
  reticulate_environment = NULL,
  models_to_run = NULL,
  models_not_to_run = NULL,
  run_deep_learning = FALSE, 
  run_global_models = TRUE,
  run_local_models = TRUE,
  run_ensemble_models = TRUE,
  average_models = TRUE,
  max_model_average = 4,
  weekly_to_daily = TRUE
) {

  # 1. Load Evironment Info:
  
  load_env_info(reticulate_environment)
  
  
  # 2. Initial Unit Tests:
  hist_dt <- validate_forecasting_inputs(input_data,
                                         combo_variables,
                                         target_variable,
                                         external_regressors,
                                         forecast_horizon,
                                         date_type,
                                         hist_start_date,
                                         hist_end_date,
                                         combo_cleanup_date,
                                         fiscal_year_start,
                                         clean_missing_values,
                                         clean_outliers,
                                         back_test_scenarios,
                                         back_test_spacing,
                                         modeling_approach,
                                         forecast_approach,
                                         parallel_processing,
                                         run_model_parallel,
                                         azure_batch_credentials,
                                         max_model_average)
  hist_start_date <- hist_dt$hist_start_date
  hist_end_date <- hist_dt$hist_end_date
  
  
  # 3. Update Input Values:
  
  #Select fourier values ----
  fourier_periods <- get_fourier_periods(fourier_periods,
                                         date_type)
  #Select lag values ----
  lag_periods <- get_lag_periods(lag_periods, 
                                 date_type,
                                 forecast_horizon)
  
  #Select rolling window values ----
  rolling_window_periods <- get_rolling_window_periods(rolling_window_periods,
                                                       date_type)
  
  #Missing values ----
  pad_value <- ifelse(clean_missing_values,NA,0)
  
  #Frequency number (year, quarter, month, etc) ----
  frequency_number <- get_frequency_number(date_type)
  
  #TS frequency (year, quarter, month, etc) ----
  gluon_ts_frequency <- get_gluon_ts_frequency(date_type)
  
  #Seasonal_periods (year, quarter, month, etc) ----
  seasonal_periods <- get_seasonal_periods(date_type)
  
  #Frequency number (year, quarter, month, etc) ----
  date_regex <- get_date_regex(date_type)
  
  # * back test spacing ----
  back_test_spacing <- get_back_test_spacing(back_test_spacing,
                                             date_type)
  
  # 4. Prep Data:
  
  cli::cli_h1("Prepping Data")
  
  #Get initial data table 
  data_tbl <- get_data_tbl(input_data,
                           combo_variables,
                           target_variable) 
  
  #Determine which xregs have future values or not
  xregs_future_values_list <- data_tbl %>%
    get_xreg_future_values_list(external_regressors,
                                hist_end_date)
  
  xregs_future_values_tbl <- data_tbl %>%
    dplyr::select(Combo, 
                  Date, 
                  xregs_future_values_list) %>%
    get_xregs_future_values_tbl(forecast_approach)
  
  external_regressors <- external_regressors %>%
    get_external_regressors(forecast_approach)
  
  #Select final data to be cleaned and prepped for modeling
  full_data_tbl <- get_full_data_tbl(data_tbl,
                                     combo_cleanup_date,
                                     combo_variables,
                                     clean_outliers,
                                     clean_missing_values,
                                     date_type,
                                     external_regressors,
                                     forecast_approach,
                                     frequency_number,
                                     forecast_horizon,
                                     hist_start_date,
                                     hist_end_date,
                                     pad_value,
                                     target_log_transformation,
                                     xregs_future_values_tbl)
  
  bt_conf <- get_back_test_scenario_hist_periods(full_data_tbl,
                                                 hist_end_date,
                                                 back_test_scenarios,
                                                 forecast_horizon,
                                                 back_test_spacing)
  
  #Back Testing and Future Forecast Splits
  #Calculate back test scenario number
  back_test_scenarios <- bt_conf$back_test_scenarios
  hist_periods_80 <- bt_conf$hist_periods_80
  
  # 5. Modeling ----
  
  cli::cli_h1("Kicking off Finn Modeling Process")
  
  # * Create and Run Modeling Function ----
  forecast_models_fn <- construct_forecast_models(full_data_tbl,
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
                                               run_deep_learning,
                                               frequency_number,
                                               models_to_run,
                                               models_not_to_run,
                                               run_ensemble_models,
                                               hist_periods_80,
                                               back_test_spacing,
                                               back_test_scenarios,
                                               date_regex,
                                               fiscal_year_start,
                                               seasonal_periods)
  
  # * Run Forecast ----
  if(forecast_approach == "bottoms_up" & length(unique(full_data_tbl$Combo)) > 1 & run_global_models & run_local_models) {
    
    combo_list <- c('All-Data', unique(full_data_tbl$Combo))
    
  } else if(forecast_approach == "bottoms_up" & length(unique(full_data_tbl$Combo)) > 1 & run_global_models == TRUE & run_local_models == FALSE) {
    
    combo_list <- c('All-Data')
    
  } else{
    
    combo_list <- unique(full_data_tbl$Combo)
  }
  
  # no parallel processing
  if(parallel_processing == "none") {
    
    fcst <- lapply(combo_list, forecast_models_fn)
    fcst <- do.call(rbind, fcst)
  }
  
  # parallel run on local machine
  if(parallel_processing=="local_machine") {
    
   fcst <- get_fcast_parallel(combo_list,
                              forecast_models_fn)
    
  }
  
  # parallel run within azure batch
  if(parallel_processing=="azure_batch") {

    fcst <- get_fcast_parallel_azure(combo_list,
                                     forecast_models_fn,
                                     azure_batch_credentials,
                                     azure_batch_cluster_config,
                                     run_name)
    
  }

  # Adjust for NaNs and Negative Forecasts
  fcst <- fcst %>%
    get_forecast_negative_adjusted(negative_fcst)
  
  # * Create Average Ensembles ----
  
  fcst_combination <- tibble::tibble(fcst)
  
  #model average combinations
  model_list <- unique(fcst$Model)
  
  if(length(model_list) > 1 & average_models){
    
    cli::cli_h1("Creating Simple Model Averages")
    
    fcst_prep <- fcst %>%
      tidyr::pivot_wider(names_from = "Model", values_from = "FCST") %>%
      tidyr::pivot_longer(!c(".id", "Combo", "Target", "Date", "Horizon"), 
                          names_to='Model', values_to = "FCST") %>%
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
      
      cl <- parallel::makeCluster(parallel::detectCores())
      doParallel::registerDoParallel(cl)
      
      combinations_tbl_final <- foreach(i = 2:min(max_model_average, length(model_list)), .combine = 'rbind',
                                        .packages = get_export_packages(), 
                                        .export = c("fcst_prep")) %dopar% {create_model_averages(i)}
      
      parallel::stopCluster(cl)
      
    }
    
    # parallel run within azure batch
    if(parallel_processing=="azure_batch") {
      
      
      combinations_tbl_final <- foreach(i = 2:min(max_model_average, length(model_list)), .combine = 'rbind',
                                        .packages = get_export_packages(), 
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
  
  # 6. Final Finn Outputs ----
  
  cli::cli_h1("Final Finn Outputs")
  
  cli::cli_h3("Selecting Best Model")
  
  #get back test results and replace missing model/back test scenario combos with zero
  back_test_initial <- fcst_combination %>%
    dplyr::filter(.id != "Final_FCST") %>%
    tidyr::pivot_wider(names_from = "Model", values_from = "FCST") %>%
    tidyr::pivot_longer(!c(".id", "Combo", "Target", "Date", "Horizon"), names_to='Model', values_to = "FCST") %>%
    dplyr::mutate(FCST = ifelse(is.na(FCST), 0, FCST))
  
  #classic weighted MAPE approach
  accuracy1 <- back_test_initial %>%
    dplyr::mutate(Target = ifelse(Target == 0, 0.1, Target)) %>%
    dplyr::mutate(MAPE = round(abs((FCST - Target) / Target), digits = 4)) %>%
    dplyr::group_by(Model, Combo) %>%
    dplyr::mutate(Combo_Total = sum(Target, na.rm = TRUE), 
                  weighted_MAPE = (Target/Combo_Total)*MAPE) %>%
    dplyr::summarise(Rolling_MAPE = sum(weighted_MAPE, na.rm=TRUE)) %>%
    dplyr::arrange(Rolling_MAPE) %>%
    dplyr::ungroup()
  
  accuracy_final <- accuracy1 %>% 
    dplyr::group_by(Combo) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Best_Model = "Yes") %>%
    dplyr::select(Combo, Model, Best_Model)
  
  #filter results on individual models and best model
  fcst_combination_final <- tibble::tibble()
  
  for(combo in unique(fcst_combination$Combo)) {
    
    temp_best_model <- accuracy_final %>%
      dplyr::filter(Combo == combo)
    
    temp <- fcst_combination %>%
      dplyr::filter(Combo == combo) %>%
      dplyr::filter(Model %in% unique(c(unique(fcst$Model), unique(temp_best_model$Model)[[1]])))
    
    fcst_combination_final <- rbind(fcst_combination_final, temp)
  }
  
  # inverse log transformation
  if(target_log_transformation) {
    
    fcst_final <- fcst_combination_final %>%
      dplyr::mutate(Target = expm1(Target), 
                    FCST = expm1(FCST))
    
    back_test_initial_final <- back_test_initial %>%
      dplyr::mutate(Target = expm1(Target), 
                    FCST = expm1(FCST))
    
  } else {
    
    fcst_final <- fcst_combination_final 
    back_test_initial_final <- back_test_initial 
  }
  
  # reconcile a hierarchical forecast
  if(forecast_approach != "bottoms_up") {
    
    cli::cli_h3("Reconciling Hierarchical Forecast")
    
    #create tibble to append reconciled fcsts to
    reconciled_fcst <- tibble::tibble()
    
    #extract best model and append to dataset
    fcst_unreconciled <- fcst_final %>%
      dplyr::left_join(accuracy_final %>%
                  dplyr::select(Combo, Model, Best_Model)) %>%
      dplyr::filter(Best_Model == "Yes") %>%
      dplyr::mutate(Model = "Best_Model") %>%
      dplyr::select(-Best_Model) %>%
      rbind(
        fcst_final %>%
          dplyr::filter(Model %in% unique(fcst$Model)))
    
    back_test_unreconciled <- back_test_initial_final %>%
      dplyr::left_join(accuracy_final %>%
                  dplyr::select(Combo, Model, Best_Model)) %>%
      dplyr::filter(Best_Model == "Yes") %>%
      dplyr::mutate(Model = "Best_Model") %>%
      dplyr::select(-Best_Model) %>%
      rbind(back_test_initial_final)
    
    #create iterator values
    model_test_date <- fcst_unreconciled %>% 
      dplyr::mutate(Model_Test_Date = paste0(Model, "---", .id)) 
    
    #fill missing combos in best model with seasonal naive results to ensure that we have all levels of the hierarchy filled out before allocating to lowest level
    combined_fcst <- data.frame()
    
    for(value in unique(model_test_date$Model_Test_Date)) {
      
      model <- strsplit(value, "---")[[1]][1]
      test_date <- strsplit(value, "---")[[1]][2]
      
      snaive_fcst <- fcst_unreconciled %>% 
        dplyr::filter(Model == 'snaive') %>%
        dplyr::mutate(Combo_Test_Date = paste0(Combo, "---", .id)) %>%
        dplyr::filter(.id == test_date)
      
      temp <- fcst_unreconciled %>%
        dplyr::filter(Model == model, 
                      .id == test_date) %>%
        dplyr::mutate(Combo_Test_Date = paste0(Combo, "---", .id)) 
      
      missing_combos <- snaive_fcst %>%
        dplyr::filter(!Combo_Test_Date %in% unique(temp$Combo_Test_Date)) %>%
        dplyr::mutate(Model=model)
      
      combined_fcst <- plyr::rbind.fill(combined_fcst, temp, missing_combos)
      
    }
    
    #get hierarchical ts info
    hts_gts_list <- data_tbl %>% 
      get_modelling_ready_tbl(external_regressors,
                              hist_end_date,
                              combo_cleanup_date,
                              combo_variables) %>%  
      get_data_tbl_final(combo_variables,
                         forecast_approach,
                         frequency_number, 
                         return = "hts_gts")
    
    hts_gts_df <- hts_gts_list$hts_gts %>%
      hts::allts() %>%
      data.frame()
    
    #reconcile forecasts
    for(value in unique(model_test_date$Model_Test_Date)) {
      print(value)
      tryCatch(
        expr = {
          
          model <- strsplit(value, "---")[[1]][1]
          test_date <- strsplit(value, "---")[[1]][2]
          
          temp <- combined_fcst %>%
            dplyr::filter(Model == model,
                          .id == test_date) %>%
            dplyr::select(Date, Combo, FCST) %>%
            tidyr::pivot_wider(names_from = Combo, values_from = FCST)
          
          Date <- temp$Date
          
          temp2 <- temp %>%
            dplyr::select(colnames(hts_gts_df),-Date)
          
          ts <- stats::ts(temp2, frequency = frequency_number)
          
          temp_residuals <- back_test_unreconciled %>%
            dplyr::filter(Model == model, 
                          Date <= max(Date)) %>% #only keep residuals that are equal or less than the forecast period
            dplyr::mutate(Residual = Target - FCST) %>%
            dplyr::select(-FCST, -Target) %>%
            tidyr::pivot_wider(names_from = Combo, values_from = Residual) %>%
            dplyr::select(colnames(hts_gts_df), -Date) %>%
            as.matrix()
          
          if(forecast_approach == "standard_hierarchy") {
            ts_combined <- data.frame(hts::combinef(ts, nodes = hts::get_nodes(hts_gts_list$hts_gts), weights = (1/colMeans(temp_residuals^2, na.rm = TRUE)), 
                                                    keep ="bottom", nonnegative = !negative_fcst))
            colnames(ts_combined) <- colnames(hts_gts_list$data_ts)
          } else if(forecast_approach == "grouped_hierarchy") {
            ts_combined <- data.frame(hts::combinef(ts, groups = hts::get_groups(hts_gts_list$hts_gts), weights = (1/colMeans(temp_residuals^2, na.rm = TRUE)), 
                                                    keep ="bottom", nonnegative = !negative_fcst))
            colnames(ts_combined) <- colnames(hts_gts_list$data_ts)
          }
          
          hts_final <- cbind(Date, ts_combined) %>%
            tidyr::pivot_longer(!Date, names_to = "Combo", values_to = "FCST") %>%
            dplyr::mutate(Model = model, 
                          .id = test_date)
          
          reconciled_fcst <- rbind(reconciled_fcst, hts_final)
          
        },
        error = function(e){ 
          print(e)
          print('skipping')
        }
      )
    }
    
    back_test_final <- reconciled_fcst %>%
      dplyr::filter(.id!="Final_FCST") %>%
      dplyr::left_join(data_tbl %>%
                         dplyr::select(Combo, Date, Target)) %>%
      dplyr::mutate(FCST = ifelse(is.na(FCST) | is.nan(FCST), 0, FCST),
                    Target = ifelse(is.na(Target) | is.nan(Target), 0, Target)) %>%
      dplyr::mutate(MAPE = abs((Target-FCST)/Target)) %>%
      dplyr::group_by(Combo, .id, Model) %>%
      dplyr::mutate(Horizon = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(Combo, .id, Date, Model, Horizon, FCST, Target, MAPE) %>%
      tidyr::separate(Combo, into = combo_variables, sep = '--', remove = FALSE) %>%
      dplyr::rename(Back_Test_Scenario = .id)
    
    prediction_intervals <- back_test_final %>%
      dplyr::mutate(Residual = Target - FCST) %>%
      dplyr::group_by(Combo, Model) %>%
      dplyr::summarise(Residual_Std_Dev = sd(Residual, na.rm=TRUE)) %>%
      dplyr::ungroup()
    
    future_fcst_final <- reconciled_fcst %>%
      dplyr::filter(.id == "Final_FCST") %>%
      dplyr::select(Combo, Date, Model, FCST) %>%
      dplyr::rename(Target = FCST) %>%
      tidyr::separate(Combo, into = combo_variables, sep = "--", remove = FALSE) %>%
      dplyr::left_join(prediction_intervals) %>%
      dplyr::mutate(lo.80 = Target - (1.28*Residual_Std_Dev), 
                    lo.95 = Target - (1.96*Residual_Std_Dev), 
                    hi.80 = Target + (1.28*Residual_Std_Dev), 
                    hi.95 = Target + (1.96*Residual_Std_Dev)) %>%
      dplyr::select(-Residual_Std_Dev) %>%
      rbind(
        data_tbl %>%
          dplyr::mutate(Model = "NA") %>%
          dplyr::mutate(lo.95 = Target, 
                        lo.80 = Target, 
                        hi.80 = Target, 
                        hi.95 = Target) %>%
          dplyr::select(Combo, combo_variables, Date, Model, Target, lo.95, lo.80, hi.80, hi.95)
      ) %>%
      dplyr::arrange(Date, Combo, Model) %>%
      dplyr::mutate(Type = ifelse(Model == "NA", "Historical", "Forecast")) %>%
      dplyr::select(Combo, dplyr::all_of(combo_variables), Date, Type, Model, Target, lo.95, lo.80, hi.80, hi.95)
    
    #colnames(future_fcst_final)[colnames(future_fcst_final)== 'Target'] <- target_variable
    
  } else if(forecast_approach == "bottoms_up") {
    
    back_test_final <- fcst_combination_final %>%
      dplyr::filter(.id != "Final_FCST") %>%
      dplyr::mutate(FCST = ifelse(is.na(FCST) | is.nan(FCST), 0, FCST)) %>%
      dplyr::left_join(accuracy_final) %>%
      dplyr::mutate(Best_Model = ifelse(is.na(Best_Model), "No", "Yes"), 
                    MAPE = abs((Target-FCST)/Target)) %>%
      dplyr::select(Combo, .id, Date, Model, Horizon, FCST, Target, MAPE, Best_Model) %>%
      tidyr::separate(Combo, into = combo_variables, sep = '--', remove = FALSE) %>%
      dplyr::rename(Back_Test_Scenario = .id)
    
    prediction_intervals <- back_test_final %>%
      dplyr::mutate(Residual = Target - FCST) %>%
      dplyr::group_by(Combo, Model) %>%
      dplyr::summarise(Residual_Std_Dev = sd(Residual, na.rm=TRUE)) %>%
      dplyr::ungroup()
    
    future_fcst_best_model <- fcst_final %>%
      dplyr::filter(.id == "Final_FCST") %>%
      dplyr::left_join(accuracy_final) %>%
      dplyr::filter(Best_Model == "Yes") %>%
      dplyr::left_join(prediction_intervals) %>%
      dplyr::mutate(lo.80 = FCST - (1.28*Residual_Std_Dev), 
                    lo.95 = FCST - (1.96*Residual_Std_Dev), 
                    hi.80 = FCST + (1.28*Residual_Std_Dev), 
                    hi.95 = FCST + (1.96*Residual_Std_Dev)) %>%
      dplyr::mutate(Model = "Best Model") %>%
      dplyr::select(Combo, Date, Model, FCST, lo.95, lo.80, hi.80, hi.95)
    
    future_fcst_final <- fcst_final %>%
      dplyr::filter(.id == "Final_FCST") %>%
      dplyr::select(Combo, Date, Model, FCST) %>%
      dplyr::left_join(prediction_intervals) %>%
      dplyr::mutate(lo.80 = FCST - (1.28*Residual_Std_Dev), 
                    lo.95 = FCST - (1.96*Residual_Std_Dev), 
                    hi.80 = FCST + (1.28*Residual_Std_Dev), 
                    hi.95 = FCST + (1.96*Residual_Std_Dev)) %>%
      dplyr::select(-Residual_Std_Dev) %>%
      rbind(future_fcst_best_model) %>%
      dplyr::rename(Target = FCST) %>%
      tidyr::separate(Combo, into = combo_variables, sep = "--", remove = FALSE) %>%
      plyr::rbind.fill(
        data_tbl %>%
          dplyr::mutate(Model = "NA") %>%
          dplyr::mutate(lo.95 = Target, 
                        lo.80 = Target, 
                        hi.80 = Target, 
                        hi.95 = Target)
      ) %>%
      dplyr::arrange(Date, Combo, Model) %>%
      dplyr::mutate(Type = ifelse(Model == "NA", "Historical", "Forecast")) %>%
      dplyr::select(Combo, dplyr::all_of(c(combo_variables, external_regressors)), Date, Type, Model, Target, 
                    lo.95, lo.80, hi.80, hi.95)
    
    #colnames(future_fcst_final)[colnames(future_fcst_final)== 'Target'] <- target_variable
  }
  
  if(date_type == "week" & weekly_to_daily) {
    
    future_fcst_final <- future_fcst_final %>%
      dplyr::group_by(Combo, Model) %>%
      dplyr::group_split() %>%
      purrr::map(.f = function(df) { #create polynomial transformations for numeric drivers, and rolling window calcs of Target, and create lags of each driver/target variable
        
        daily_tbl <- df %>%
          dplyr::mutate(Date_Day = Date) %>%
          timetk::pad_by_time(Date_Day, .by = "day", .pad_value = NA, .end_date = max(df$Date)+6) %>%
          tidyr::fill(tidyr::everything(), .direction = "down") %>%
          dplyr::mutate(Target = Target/7,
                        lo.95 = lo.95/7,
                        lo.80 = lo.80/7,
                        hi.80 = hi.80/7,
                        hi.95 = hi.95/7) %>%
          dplyr::select(Combo, dplyr::all_of(c(combo_variables, external_regressors)), Date, Date_Day, Type, Model, Target, 
                        lo.95, lo.80, hi.80, hi.95)
        
        return(daily_tbl)
        
      }) %>%
      dplyr::bind_rows()
  }
  
  
  colnames(future_fcst_final)[colnames(future_fcst_final)== 'Target'] <- target_variable
  
  return(list(final_fcst = tibble::tibble(future_fcst_final), back_test_data = back_test_final, back_test_best_MAPE = accuracy_final))
  
  # End ----
}