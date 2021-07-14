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
#' @param run_all_data Run multivariate models on the entire data set (across all time series). Can be override by 
#'  models_to_run and models_not_to_run.
#' @param average_models Create simple averages of individual models. 
#' @param max_model_average Max number of models to average together. Will create model averages for 2 models up until input value 
#'   or max number of models ran.
#' @param weekly_to_daily Convert a week forecast down to day by evenly splitting across each day of week. Helps when aggregating 
#'   up to higher temporal levels like month or quarter. 
#'   
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
#'   
#' @return A list of three separate data sets: the future forecast, the back test results, and the best model per time series.
#'   
#' @export
forecast_time_series <- function(
  input_data,
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
  run_all_data = TRUE,
  average_models = TRUE,
  max_model_average = 4,
  weekly_to_daily = TRUE
) {

  # 1. Load Evironment Info ----
  
  load_env_info(reticulate_environment)
  
  
  # 2. Initial Unit Tests ----
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
  
  
  # 3. Update Input Values ----
  
  # * select fourier values ----
  fourier_periods <- get_fourier_periods(fourier_periods,
                                         date_type)
  # * select lag values ----
  lag_periods <- get_lag_periods(lag_periods, 
                                 date_type,
                                 forecast_horizon)
  
  # * select rolling window values ----
  rolling_window_periods <- get_rolling_window_periods(rolling_window_periods,
                                                       date_type)
  
  # * missing values ----
  pad_value <- ifelse(clean_missing_values,NA,0)
  
  # * frequency number (year, quarter, month, etc) ----
  frequency_number <- get_frequency_number(date_type)
  
  # * ts frequency (year, quarter, month, etc) ----
  gluon_ts_frequency <- get_gluon_ts_frequency(date_type)
  
  # * seasonal_periods (year, quarter, month, etc) ----
  seasonal_periods <- get_seasonal_periods(date_type)
  
  # * frequency number (year, quarter, month, etc) ----
  date_regex <- get_date_regex(date_type)
  
  # * back test spacing ----
  back_test_spacing <- get_back_test_spacing(back_test_spacing,
                                             date_type)
  
  # 4. Prep Data ----
  
  # * Original Data ----
  data_tbl <- input_data %>%
    tibble()
  
  data_tbl$Combo <- do.call(paste, c(data_tbl[combo_variables], sep = "--"))
  
  colnames(data_tbl)[colnames(data_tbl) == target_variable] <- "Target"
  
  
  #Determine which xregs have future values or not
  xregs_future_values_list <- c()
  
  for(variable in external_regressors) {
    
    temp <- data_tbl %>%
      dplyr::filter(Date > hist_end_date) %>%
      dplyr::select(variable) %>%
      tidyr::drop_na()
    
    if(nrow(temp) > 0) {
      
      xregs_future_values_list <- append(xregs_future_values_list, variable)
    }
  }
  
  xregs_future_values_tbl <- data_tbl %>%
    dplyr::select(Combo, Date, xregs_future_values_list)
  
  #select final data to be cleaned and prepped for modeling
  data_tbl <- data_tbl %>%
    dplyr::select(c("Combo", all_of(combo_variables), all_of(external_regressors), "Date", "Target")) %>%
    dplyr::filter(Date <= hist_end_date) %>%
    dplyr::arrange(Combo, Date) 
  
  #remove data combos that do not contain data
  if(!is.null(combo_cleanup_date)) {
    
    combo_df <- data_tbl %>%
      dplyr::filter(Date >= combo_cleanup_date) %>%
      tidyr::drop_na(Target) %>%
      data.frame() %>%
      dplyr::group_by(Combo) %>%
      dplyr::summarise(Sum=sum(Target, na.rm = TRUE)) %>%
      data.frame() %>%
      dplyr::filter(Sum != 0)
    
    combo_df <- unique(as.character(combo_df$Combo))
    
    data_tbl <- data_tbl[data_tbl$Combo %in% combo_df,]
    
  }
  
  
  #set up data based on fcst approach
  if(fcst_approach != 'bottoms_up') {
    
    external_regressors <- NULL
    
    data_hts_gts_df <- data_tbl %>%
      dplyr::mutate(Target = tidyr::replace_na(Target, 0)) %>%
      dplyr::group_by(.dots = combo_variables) %>%
      #dplyr::group_by(combo_variables) %>%
      dplyr::summarise(Sum=sum(Target, na.rm=TRUE)) %>%
      data.frame()
    
    if(fcst_approach == "grouped_hierarchy") {
      
      group_list <- vector()
      
      for(variable in combo_variables) {
        
        var = data_hts_gts_df[[variable]]
        
        group_list = rbind(group_list, var)
      }
      
      rownames(group_list) <- combo_variables
      
    } else if(fcst_approach == "standard_hierarchy") {
      
      hierarchy_length_tbl <- tibble()
      
      node_list <- list()
      
      num <- 1
      
      for(variable in combo_variables) {
        
        hierarchy_length_tbl <- rbind(hierarchy_length_tbl, tibble(Variable = variable, Count = length(unique(data_tbl[[variable]]))))
        
      }
      
      hierarchy_combo_variables <- hierarchy_length_tbl %>%
        dplyr::arrange(Count) %>%
        dplyr::select(Variable) %>%
        unlist(use.names = FALSE)
      
      for(variable in hierarchy_combo_variables) {
        
        if(num == 1) {
          
          node_list = append(node_list, length(unique(data_tbl[[variable]])))
          
          num <- num+1
          
        } else {
          
          grouping_current <- variable
          
          grouping_minus_1 <- hierarchy_combo_variables[num-1]
          
          grouping_values <- data_hts_gts_df %>%
            dplyr::group_by(.dots = c(grouping_minus_1, grouping_current)) %>%
            dplyr::summarise(Sum = sum(Sum, na.rm=TRUE)) %>%
            dplyr::mutate(Sum = 1) %>%
            dplyr::group_by(.dots = grouping_minus_1) %>%
            dplyr::summarise(Count = sum(Sum)) %>%
            dplyr::select(Count) %>%
            unlist(use.names = FALSE)
          
          node_list = append(node_list, list(grouping_values))
          
          num <- num+1
        }
      }
      #node_list = list(11, c(6, 4, 2, 1, 6, 1, 4, 2, 8, 2, 5))
      #node_list = list(3, c(3, 5, 3), c(6, 4, 2, 1, 6, 1, 4, 2, 8, 2, 5))
    }
    
    data_cast <- data_tbl %>%
      dplyr::arrange(Combo, Date) %>%
      dplyr::select(-combo_variables) %>%
      tidyr::pivot_wider(names_from = Combo, values_from = Target)
    
    Date <- data_cast$Date
    
    data_cast <- data_cast %>%
      dplyr::select(-Date)
    
    col_names <- colnames(data_cast)
    
    data_ts <- ts(data_cast, frequency = frequency_number) 
    
    if(fcst_approach == "grouped_hierarchy") {
      
      hts_gts <- data_ts %>%
        hts::gts(groups = group_list)
      
    } else if(fcst_approach == "standard_hierarchy") {
      
      hts_gts <- data_ts %>%
        hts::hts(nodes = node_list)
    }
    
    
    hts_gts_df <- hts_gts %>%
      hts::allts() %>%
      data.frame()
    
    hts_gts_df <- cbind(Date, hts_gts_df)
    
    data_tbl_final <- hts_gts_df %>%
      tidyr::pivot_longer(!Date, names_to = "Combo", values_to = "Target")
    tibble()
    
    xregs_future_values_tbl <- xregs_future_values_tbl %>%
      tibble() %>%
      dplyr::select(Combo, Date)
    
  } else if(fcst_approach == 'bottoms_up') {
    data_tbl_final <- data_tbl
  }
  
  # * Prepped Data ----
  full_data_tbl <- data_tbl_final %>%
    dplyr::select(Combo, Date, Target, external_regressors) %>%
    dplyr::group_by(Combo) %>%
    timetk::pad_by_time(Date, .by = date_type, .pad_value = pad_value, .end_date = hist_end_date) %>% #fill in missing values in between existing data points
    timetk::pad_by_time(Date, .by = date_type, .pad_value = 0, .start_date = hist_start_date, .end_date = hist_end_date) %>% #fill in missing values at beginning of time series with zero
    dplyr::ungroup()
  
  #log transformation of target variable
  if(target_log_transformation) {
    
    full_data_tbl <- full_data_tbl %>%
      dplyr::mutate(Target = log1p(Target),
                    Target = ifelse(is.nan(Target), 0, Target))
    
  }
  
  #create future data and clean
  full_data_tbl <- full_data_tbl %>%
    dplyr::group_by(Combo) %>%
    timetk::future_frame(Date, .length_out = forecast_horizon, .bind_data = TRUE) %>% #add future data
    dplyr::left_join(xregs_future_values_tbl) %>% #join xregs that contain values given by user
    dplyr::ungroup() %>%
    dplyr::group_by(Combo) %>%
    dplyr::group_split() %>%
    purrr::map(.f = function(df) { #create polynomial transformations for numeric drivers, and rolling window calcs of Target, and create lags of each driver/target variable
      
      final_tbl <- df %>%
        dplyr::select(-Target, -external_regressors)
      
      #clean outliers and missing values, apply polynomial transformations
      numeric_xregs <- c()
      
      for(column in c("Target", external_regressors)) {
        
        if(is.numeric(dplyr::select(df, column)[[1]])) {
          
          column_names_final <- c(column)
          
          # if((column %in% xregs) & !(column %in% xregs_future_values_list)) {
          #   numeric_xregs <- c(numeric_xregs, str_c(column, c("", "_squared", "_cubed", "_log")))
          #   column_names_final <- str_c(column, c("", "_squared", "_cubed", "_log"))
          # }
          
          if(clean_outliers == TRUE) {
            
            colnames(df)[colnames(df)== column] <- "Column"
            
            df_clean <- df %>%
              dplyr::mutate(Column = timetk::ts_clean_vec(Column, period = frequency_number)) 
            
            colnames(df_clean)[colnames(df_clean)== "Column"] <- column
            colnames(df)[colnames(df)== "Column"] <- column
            
          } else if(clean_missing_values == TRUE) {
            
            colnames(df)[colnames(df)== column] <- "Column"
            
            df_clean <- df %>%
              dplyr::mutate(Column = timetk::ts_impute_vec(Column, period = frequency_number))
            
            colnames(df_clean)[colnames(df_clean)== "Column"] <- column
            colnames(df)[colnames(df)== "Column"] <- column
            
          } else {
            
            df_clean <- df
          }
        }
        
        final_tbl <- cbind(final_tbl, df_clean %>% dplyr::select(column_names_final)) %>% tibble()
      }
      
      return(tibble(final_tbl))
      
    }) %>%
    dplyr::bind_rows() #%>%
  #timetk::tk_augment_fourier(Date, .periods = fourier_periods, .K = 2) #add fourier series
  
  #Replace NaN/Inf with NA, then replace with zero
  is.na(full_data_tbl) <- sapply(full_data_tbl, is.infinite)
  is.na(full_data_tbl) <- sapply(full_data_tbl, is.nan)
  full_data_tbl[is.na(full_data_tbl)] = 0
  
  #replace future target variable values with NA
  full_data_tbl <- full_data_tbl %>%
    tibble() %>%
    dplyr::mutate(Target = ifelse(Date > hist_end_date, NA, Target))
  
  
  # * Back Testing and Future Forecast Splits ----
  
  #calculate back test scenario number
  historical_periods <- full_data_tbl %>%
    dplyr::filter(Date <= hist_end_date) %>%
    dplyr::select(Date) %>%
    unique() %>%
    nrow() %>%
    as.numeric()
  
  hist_periods_80 <- floor(historical_periods*0.7) #used with time series CV in multivariate models
  
  if(back_test_scenarios == "auto") {
    
    historical_periods_20 <- floor(historical_periods*0.2)
    
    #account for initial back tests that are smaller than the forecast horizon (1, 2, 3, etc up to fcst horizon)
    if(historical_periods_20 > forecast_horizon) {
      back_test_scenarios = floor(historical_periods_20/back_test_spacing)
    } else {
      back_test_scenarios = floor(forecast_horizon/back_test_spacing)
    }
  }
  
  back_test_scenarios <- back_test_scenarios + 1
  
  # 5. Modeling ----
  
  # * Create and Run Modeling Function ----
  
  forecast_models <- function(combo_value) {
    
    print(combo_value)
    
    #filter on specific combo or all data
    model_name_suffix <- ""
    
    if(combo_value != "All-Data") {
      
      run_data_full_tbl <- full_data_tbl %>%
        dplyr::filter(Combo == combo_value)
      
    } else {
      
      run_data_full_tbl <- full_data_tbl %>%
        tidyr::separate(col = 'Combo', into = combo_variables, remove=FALSE, sep= "--") 
      
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
    submodels_resample_tscv_recipe_1 <- tibble()
    submodels_resample_tscv_recipe_2 <- tibble()
    
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
      
      test <- tibble()
      
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
    
    fcst_tbl <- tibble()
    
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
  if(fcst_approach == "bottoms_up" & length(unique(full_data_tbl$Combo)) > 1 & run_all_data) {
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
  
  fcst_combination <- tibble(fcst)
  
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
  
  # 6. Final Finn Outputs ----
  
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
  
  #return(accuracy1)
  
  #new MASE approach
  # accuracy2 <- back_test_initial %>%
  #   dplyr::group_by(Combo, .id) %>%
  #   dplyr::group_split() %>%
  #   purrr::map(.f = function(df) {
  #     
  #     training_length <- full_data_tbl %>%
  #       timetk::tk_augment_lags(Target) %>%
  #       dplyr::filter(Date < min(df$Date), 
  #                     Combo == unique(df$Combo)[1]) %>%
  #       tidyr::drop_na(Target_lag1) %>%
  #       dplyr::mutate(Hist_Target = abs(Target - Target_lag1)) %>%
  #       dplyr::group_by(Combo) %>%
  #       dplyr::summarise(MASE_Denominator = mean(Hist_Target, na.rm=TRUE)) %>%
  #       dplyr::ungroup() %>%
  #       dplyr::mutate(.id = unique(df$.id)[1])
  #     
  #     return(training_length)
  #   }) %>%
  #   dplyr::bind_rows() %>%
  #   dplyr::right_join(
  #     back_test_initial %>%
  #       dplyr::mutate(MASE_Numerator = abs(Target - FCST)) %>%
  #       dplyr::group_by(.id, Combo, Model) %>%
  #       dplyr::summarise(MASE_Numerator = mean(MASE_Numerator, na.rm=TRUE)) %>%
  #       dplyr::ungroup()) %>%
  #   dplyr::mutate(MASE = MASE_Numerator/MASE_Denominator) %>%
  #   dplyr::group_by(Combo, Model) %>%
  #   dplyr::summarise(MASE = mean(MASE, na.rm=TRUE)) %>%
  #   dplyr::arrange(MASE) %>%
  #   dplyr::ungroup()
  # 
  # print(accuracy2)
  
  
  accuracy_final <- accuracy1 %>% 
    dplyr::group_by(Combo) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Best_Model = "Yes") %>%
    dplyr::select(Combo, Model, Best_Model)
  
  #filter results on individual models and best model
  fcst_combination_final <- tibble()
  
  for(combo in unique(fcst_combination$Combo)) {
    
    temp_best_model <- accuracy_final %>%
      dplyr::filter(Combo == combo)
    
    print(combo)
    
    temp <- fcst_combination %>%
      dplyr::filter(Combo == combo) %>%
      dplyr::filter(Model %in% unique(c(unique(fcst$Model), unique(temp_best_model$Model)[[1]])))
    
    fcst_combination_final <- rbind(fcst_combination_final, temp)
  }
  
  # inverse log transformation
  if(target_log_transformation) {
    
    fcst_final <- fcst_combination_final %>%
      #dplyr::filter(Model %in% unique(c(fcst$Model, accuracy_final$Model))) %>%
      dplyr::mutate(Target = expm1(Target), 
                    FCST = expm1(FCST))
    
    # resamples_tscv_final <- resamples_tscv %>%
    #   dplyr::mutate(Target = expm1(Target))
    
    back_test_initial_final <- back_test_initial %>%
      #dplyr::filter(Model %in% unique(c(fcst$Model, accuracy_final$Model))) %>%
      dplyr::mutate(Target = expm1(Target), 
                    FCST = expm1(FCST))
    
  } else {
    
    fcst_final <- fcst_combination_final 
    
    back_test_initial_final <- back_test_initial 
  }
  
  # reconcile a hierarchical forecast
  if(fcst_approach != "bottoms_up") {
    
    #create tibble to append reconciled fcsts to
    reconciled_fcst <- tibble()
    
    #extract best model and append to dataset
    fcst_unreconciled <- fcst_final %>%
      left_join(accuracy_final %>%
                  dplyr::select(Combo, Model, Best_Model)) %>%
      dplyr::filter(Best_Model == "Yes") %>%
      dplyr::mutate(Model = "Best_Model") %>%
      dplyr::select(-Best_Model) %>%
      rbind(
        fcst_final %>%
          dplyr::filter(Model %in% unique(fcst$Model)))
    
    back_test_unreconciled <- back_test_initial_final %>%
      left_join(accuracy_final %>%
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
    
    for(value in unique(model_test_date$Model_Test_Date)) {
      
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
          
          if(fcst_approach == "standard_hierarchy") {
            ts_combined <- data.frame(hts::combinef(ts, nodes = get_nodes(hts_gts), weights = (1/colMeans(temp_residuals^2, na.rm = TRUE)), 
                                                    keep ="bottom", nonnegative = !negative_fcst))
            colnames(ts_combined) <- colnames(data_ts)
          } else if(fcst_approach == "grouped_hierarchy") {
            ts_combined <- data.frame(hts::combinef(ts, groups = get_groups(hts_gts), weights = (1/colMeans(temp_residuals^2, na.rm = TRUE)), 
                                                    keep ="bottom", nonnegative = !negative_fcst))
            colnames(ts_combined) <- colnames(data_ts)
          }
          
          hts_final <- cbind(Date, ts_combined) %>%
            tidyr::pivot_longer(!Date, names_to = "Combo", values_to = "FCST") %>%
            dplyr::mutate(Model = model, 
                          .id = test_date)
          
          reconciled_fcst <- rbind(reconciled_fcst, hts_final)
          
        },
        error = function(e){ 
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
      #dplyr::left_join(accuracy_final) %>%
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
    
  } else if(fcst_approach == "bottoms_up") {
    
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
  
  return(list(final_fcst = future_fcst_final, back_test_data = back_test_final, back_test_best_MAPE = accuracy_final))
  
  # End ----
}
