#' Gets combo specific filter
#' 
#' @param full_data_tbl Full Data Table
#' @param combo_value Combo Value
#' @param combo_variables Combo Variables List
#' 
#' @return combo_specific_filter function
#' @noRd
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

#' Get list of all-data models
#' 
#' @return List of all data models
#' @noRd
get_not_all_data_models <- function(){
  c('arima','arima-boost','croston','ets','meanf','nnetar','nnetar-xregs',
    'prophet','prophet-xregs','snaive','stlm-ets','stlm-arima',
    'tbats','theta')
}

#' Get list of r1 only models
#' 
#' @return List of r1 only models
#' @noRd
get_r1_data_models <- function(){
  c('mars','deepar','nbeats')
}

#' Get list of r1 & r2 models
#' 
#' @return List of r1 & r2 models
#' @noRd
get_r2_data_models <- function(){
  c('cubist','glmnet','svm-poly','svm-rbf','xgboost')
}

#' Get list of deep learning models
#' 
#' @return List of deep learning models
#' @noRd
get_deep_learning_models <- function(){
  c('deepar','nbeats', 'tabnet')
}

#' Get list of seasonal correction
#' 
#' @return List of models that need seasonal adjustment
#' @noRd
get_frequency_adjustment_models <- function(){
  c('meanf','snaive')
}

#' Get change of frequency
#' 
#' @param date_type Date Type
#' @param frequency_number Original Frequency Number
#' 
#' @return adjusted frequency
#' @noRd
get_freq_adjustment <- function(date_type,
                                frequency_number){
  
  switch (date_type,
          "week" = 52,
          "day" = 365,
          frequency_number)
}

#' Gets list of pre-existing models
#' 
#' @param models_to_run List of models to run
#' @param model_not_to_run List of models NOT to run
#' @param run_deep_learning Deep Learning Models
#' 
#' @return uses models_to_run and models_not_to_run and returns correct list
#' @noRd
get_model_functions <- function(models_to_run,
                                model_not_to_run,
                                run_deep_learning){
  
  exhaustive_pre_load_list <- c(get_not_all_data_models(),
                                get_r1_data_models(),
                                get_r2_data_models())
  
  deep_learning_models <- get_deep_learning_models()
  
  fnlist <- list()
  
  if(is.null(models_to_run)){
    models_to_run <- exhaustive_pre_load_list
  }
  
  
  for(mr in models_to_run){
    
    if(mr %in% model_not_to_run){
      next
    }
    
    if((mr %in% deep_learning_models) & !run_deep_learning){
      next
    }
    
    fn_name <- as.character(stringr::str_replace(mr,"-","_"))
    
    if(mr %in% exhaustive_pre_load_list){
      cli::cli_alert_success(paste0("Known model:",
                                    mr,
                                    " function:",
                                    fn_name))
    }else{
      cli::cli_alert_warning(paste0("Custom model:",
                                    mr,
                                    " function:",
                                    fn_name))
    }
    
    fnlist[mr] <- fn_name
  }
  
  return(fnlist)
}

#' Invoke model function with params
#' 
#' @param fn_to_invoke Function to invoke
#' @param train_data Training Data
#' @param frequency Frequency used
#' @param horizon Horizon of forecast
#' @param parallel Is Include Parallel
#' @param seasonal_period Seasonal Peiod
#' @param tscv_inital Time Series CV Initialization
#' @param date_rm_regex Date Remove Regex
#' @param back_test_spacing Back Test Spacing
#' @param fiscal_year_start Fiscal Year Start
#' @param model_type Model Type
#' 
#' @return Forecast Object to be used down stream
#' @noRd
invoke_forecast_function <- function(fn_to_invoke,
                                     train_data,
                                     frequency,
                                     horizon,
                                     parallel,
                                     seasonal_period,
                                     tscv_inital,
                                     date_rm_regex,
                                     back_test_spacing,
                                     fiscal_year_start,
                                     model_type,
                                     pca){
  
  exp_arg_list <- formalArgs(fn_to_invoke)
  
  avail_arg_list <- list('train_data' = train_data,
                         'frequency' = frequency,
                         'horizon' = horizon,
                         'parallel' = parallel,
                         'seasonal_period' = seasonal_period,
                         'tscv_initial' = tscv_inital,
                         'date_rm_regex' = date_rm_regex,
                         'back_test_spacing' = back_test_spacing,
                         'fiscal_year_start' = fiscal_year_start,
                         'model_type' = model_type, 
                         "pca" = pca)
  
  avail_names <- names(avail_arg_list)
  
  inp_arg_list <- list()
  
  for(x in avail_names){
    
    if(x %in% exp_arg_list){
      inp_arg_list[x] <- avail_arg_list[x]
    }
  }
  
  do.call(fn_to_invoke,inp_arg_list, quote=TRUE)
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
#' @param num_cores number of cores for parallel processing
#' @param run_deep_learning Run Deep Learning model
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
#' @noRd
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
                                      num_cores,
                                      run_deep_learning,
                                      frequency_number,
                                      recipes_to_run,
                                      models_to_run,
                                      models_not_to_run,
                                      run_ensemble_models, 
                                      hist_periods_80,
                                      back_test_spacing,
                                      back_test_scenarios,
                                      date_regex,
                                      fiscal_year_start,
                                      seasonal_periods, 
                                      pca
                                      ){

  forecast_models <- function(combo_value) {
    
    cli::cli_h2("Running Combo: {combo_value}")
    
    # Copy functions into global environment within azure batch
    if(!is.null(parallel_processing)) {
      if(parallel_processing == "azure_batch") {
        
        global_env <- .GlobalEnv
        export_env <- global_env$azbatchenv$exportenv
        
        for(n in ls(export_env , all.names=TRUE)) {
          assign(n, get(n, export_env), global_env)
        }
        
      }
    }
    
    #filter on specific combo or all data
    model_name_suffix <-  ifelse(combo_value=="All-Data","-all","")
    
    run_data_full_tbl <- full_data_tbl %>%
      combo_specific_filter(combo_value,
                            combo_variables)

    cli::cli_h3("Initial Feature Engineering")
    
    # Run all recipes
    if(is.null(recipes_to_run)) {
      run_all_recipes_override <- FALSE
    } else if(recipes_to_run == "all") {
      run_all_recipes_override <- TRUE
    } else {
      run_all_recipes_override <- FALSE
    }
    
    # recipe 1: standard feature engineering
    run_data_full_recipe_1 <- NULL
    
    if(is.null(recipes_to_run) | "R1" %in% recipes_to_run | run_all_recipes_override) {
      
      run_data_full_recipe_1 <- run_data_full_tbl %>%
        multivariate_prep_recipe_1(external_regressors = external_regressors,
                                   xregs_future_values_list = xregs_future_values_list,
                                   fourier_periods = fourier_periods,
                                   lag_periods = lag_periods,
                                   rolling_window_periods = rolling_window_periods)
      
      train_data_recipe_1 <- run_data_full_recipe_1 %>%
        dplyr::filter(Date <= hist_end_date)
      
      
      test_data_recipe_1 <- run_data_full_recipe_1 %>%
        dplyr::filter(Date > hist_end_date)
      
    }
    
    # recipe 2: custom horizon specific feature engineering
    run_data_full_recipe_2 <- NULL

    if((is.null(recipes_to_run) & date_type %in% c("month", "quarter", "year")) | "R2" %in% recipes_to_run | run_all_recipes_override) {
      
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
      
    }
    
    # create modeltime table to add single trained models to
    combined_models_recipe_1 <- modeltime::modeltime_table()
    combined_models_recipe_2 <- modeltime::modeltime_table()
    
    # parallel processing
    if(run_model_parallel == TRUE) {
      parallel_args <- init_parallel_within(parallel_processing, num_cores)
    }
    
    cli::cli_h3("Individual Model Training")
    

    # models to run
    model_list <- get_model_functions(models_to_run,
                                      models_not_to_run,
                                      run_deep_learning)
    
    not_all_data_models <- get_not_all_data_models()
    r1_models <- get_r1_data_models()
    r2_models <- get_r2_data_models()
    freq_models <- get_frequency_adjustment_models()
    deep_nn_models <- get_deep_learning_models()
    
    models_to_go_over <- names(model_list)
    
    # PCA
    if((combo_value == "All-Data" & is.null(pca)) | (is.null(pca) & date_type %in% c("day", "week"))) {
      run_pca <- TRUE
    } else if(is.null(pca)) {
      run_pca <- FALSE
    } else if(pca == TRUE) {
      run_pca <- TRUE
    } else {
      run_pca <- FALSE
    }
    
    # train each model
    for(model_name in models_to_go_over){
      
      model_fn <- as.character(model_list[model_name])
      
      cli::cli_alert_info("Function being called: {model_fn}")
      
      if(model_name %in% not_all_data_models & combo_value != "All-Data"){
        if(model_name %in% freq_models){
          freq_val <- get_freq_adjustment(date_type,frequency_number)
        }
        else{
          freq_val <- frequency_number
        }
        
        try(mdl_called <- invoke_forecast_function(fn_to_invoke =  model_fn,
                                                   train_data = train_data_recipe_1,
                                                   frequency = freq_val,
                                                   parallel = run_model_parallel,
                                                   horizon = forecast_horizon,
                                                   seasonal_period =seasonal_periods,
                                                   back_test_spacing = back_test_spacing,
                                                   fiscal_year_start = fiscal_year_start,
                                                   tscv_inital = hist_periods_80,
                                                   date_rm_regex = date_regex,
                                                   model_type = "single", 
                                                   pca = run_pca))

        
        try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1,
                                                                       mdl_called,
                                                                       location = "top") %>%
              modeltime::update_model_description(1, model_name),
            silent = TRUE)
        
      }else{
        
        
        freq_val <- frequency
        
        if(((model_name %in% r1_models) | (model_name %in% r2_models)) & (is.null(recipes_to_run) | run_all_recipes_override | "R1" %in% recipes_to_run)){
          
          add_name <- paste0(model_name,"-R1",model_name_suffix)
          if(model_name %in% deep_nn_models){
            freq_val <- gluon_ts_frequency
            add_name <- paste0(model_name,model_name_suffix)
          }

            try(mdl_called <- invoke_forecast_function(fn_to_invoke =  model_fn,
                                                       train_data = train_data_recipe_1,
                                                       frequency = freq_val,
                                                       parallel = run_model_parallel,
                                                       horizon = forecast_horizon,
                                                       seasonal_period =seasonal_periods,
                                                       back_test_spacing = back_test_spacing,
                                                       fiscal_year_start = fiscal_year_start,
                                                       tscv_inital = hist_periods_80,
                                                       date_rm_regex = date_regex,
                                                       model_type = "single", 
                                                       pca = run_pca))

            try(combined_models_recipe_1 <- modeltime::add_modeltime_model(combined_models_recipe_1,
                                                                           mdl_called,
                                                                           location = "top") %>%
                  modeltime::update_model_description(1, add_name),
                silent = TRUE)

        }
        
        if(model_name %in% r2_models & ("R2" %in% recipes_to_run | run_all_recipes_override | (is.null(recipes_to_run) & date_type %in% c("month", "quarter", "year")))){

          add_name <- paste0(model_name,"-R2",model_name_suffix)
          try(mdl_called <- invoke_forecast_function(fn_to_invoke =  model_fn,
                                                     train_data = train_data_recipe_2,
                                                     frequency = freq_val,
                                                     parallel = run_model_parallel,
                                                     horizon = forecast_horizon,
                                                     seasonal_period =seasonal_periods,
                                                     back_test_spacing = back_test_spacing,
                                                     fiscal_year_start = fiscal_year_start,
                                                     tscv_inital = hist_periods_80,
                                                     date_rm_regex = date_regex,
                                                     model_type = "single", 
                                                     pca = run_pca))

          try(combined_models_recipe_2 <- modeltime::add_modeltime_model(combined_models_recipe_2,
                                                                         mdl_called,
                                                                         location = "top") %>%
                modeltime::update_model_description(1, add_name),
              silent = TRUE)
        }
        
      }
    }
    
    print(combined_models_recipe_1)
    print(combined_models_recipe_2)
    
    if(length(unique(combined_models_recipe_1$.model_desc))+length(unique(combined_models_recipe_2$.model_desc)) < 1) {
      stop("all individual models failed during initial training")
    }
    
    cli::cli_h3("Refitting Individual Models")
    
    #create resamples for back testing forecasts and ensemble training data
    
    # if multivariate models are chosen to run, ensemble models are turned on, and more than one individual model has been run, 
    # then create enough back test scenarios to train ensemble models, otherwise just run back test scenario input amount and turn ensembles off
    if(run_ensemble_models & (length(unique(combined_models_recipe_1$.model_desc))+length(unique(combined_models_recipe_2$.model_desc)))>1 & sum(grepl("-R", c(unique(combined_models_recipe_1$.model_desc), unique(combined_models_recipe_2$.model_desc)))) > 0) {
      slice_limit_amount <- 100
      run_ensemble_models <- TRUE
    } else {
      slice_limit_amount <- back_test_scenarios
      run_ensemble_models <- FALSE
    }
    
    if(!is.null(run_data_full_recipe_1)) {
      resamples_tscv_recipe_1 <- run_data_full_recipe_1 %>%
        timetk::time_series_cv(
          date_var = Date,
          initial = "1 year",
          assess = forecast_horizon,
          skip = back_test_spacing,
          cumulative = TRUE,
          slice_limit = slice_limit_amount)
    }

    if(!is.null(run_data_full_recipe_2)) {
      resamples_tscv_recipe_2 <- run_data_full_recipe_2 %>%
        timetk::time_series_cv(
          date_var = Date,
          initial = "1 year",
          assess = forecast_horizon,
          skip = back_test_spacing,
          cumulative = TRUE,
          slice_limit = slice_limit_amount) %>%
        timetk::tk_time_series_cv_plan()
      
      #correctly filter test split to correct horizons per date
      rsplit_function <- function(slice) {
        
        train <- resamples_tscv_recipe_2 %>%
          dplyr::filter(.id == slice,
                        .key == "training")
        
        test <- resamples_tscv_recipe_2 %>%
          dplyr::filter(.id == slice,
                        .key == "testing",
                        Origin == max(train %>%
                                        dplyr::filter(Horizon == 1) %>%
                                        dplyr::select(Origin))+1)
        
        slice_tbl <- train %>%
          rbind(test) %>%
          dplyr::select(-.id, -.key)
        
        rsplit_obj <- slice_tbl %>%
          rsample::make_splits(x = list(analysis = seq(nrow(train)),
                                        assessment = nrow(train) + seq(nrow(test))),
                               class = "ts_cv_split")
        
        return(rsplit_obj)
      }
      
      split_objs <- purrr::map(unique(resamples_tscv_recipe_2$.id), .f=rsplit_function)
      
      resamples_tscv_recipe_2_final <- rsample::new_rset(splits = split_objs,
                                                         ids = unique(resamples_tscv_recipe_2$.id),
                                                         subclass = c("time_series_cv", "rset"))
    }
    
    get_model_time_resample_fit<- function(combined_models_recipe,
                                           resamples_tscv_recipe){
      combined_models_recipe %>%
        modeltime.resample::modeltime_fit_resamples(
          resamples = resamples_tscv_recipe,
          control = tune::control_resamples(
            verbose = TRUE,
            allow_par = run_model_parallel)) %>%
        modeltime.resample::unnest_modeltime_resamples() %>%
        dplyr::mutate(.id = .resample_id,
                      Model = .model_desc,
                      FCST = .pred) %>%
        dplyr::select(.id, Model, FCST, .row) %>%
        dplyr::left_join(
          resamples_tscv_recipe %>%
            timetk::tk_time_series_cv_plan() %>%
            dplyr::group_by(.id) %>%
            dplyr::mutate(.row = dplyr::row_number()) %>%
            dplyr::ungroup())
    }
    
    #refit models on resamples
    submodels_resample_tscv_recipe_1 <- tibble::tibble()
    submodels_resample_tscv_recipe_2 <- tibble::tibble()
    
    if(length(unique(combined_models_recipe_1$.model_desc)) > 0) {
      submodels_resample_tscv_recipe_1 <- combined_models_recipe_1 %>%
        get_model_time_resample_fit(resamples_tscv_recipe_1)%>%
        dplyr::select(.id, Combo, Model, FCST, Target, Date) %>%
        dplyr::group_by(.id, Combo, Model) %>%
        dplyr::mutate(Horizon = dplyr::row_number()) %>%
        dplyr::ungroup()
    }
    
    if(length(unique(combined_models_recipe_2$.model_desc)) > 0) {
      submodels_resample_tscv_recipe_2 <- combined_models_recipe_2 %>%
        get_model_time_resample_fit(resamples_tscv_recipe_2_final) %>%
        dplyr::select(.id, Combo, Model, FCST, Target, Date, Horizon)
    }
    
    submodels_resample_tscv_tbl <- rbind(submodels_resample_tscv_recipe_1,
                                         submodels_resample_tscv_recipe_2)
    
    #Replace NaN/Inf with NA, then replace with zero
    is.na(submodels_resample_tscv_tbl) <- sapply(submodels_resample_tscv_tbl,
                                                 is.infinite)
    is.na(submodels_resample_tscv_tbl) <- sapply(submodels_resample_tscv_tbl,
                                                 is.nan)
    submodels_resample_tscv_tbl[is.na(submodels_resample_tscv_tbl)] = 0.01
    
    ensemble_train_data_initial <- submodels_resample_tscv_tbl %>%
      dplyr::filter(Date <= hist_end_date) %>%
      dplyr::select(-.id) %>%
      tidyr::pivot_wider(names_from = "Model", values_from = "FCST") %>%
      dplyr::mutate(Horizon_char = as.character(Horizon))
    
    #Replace NaN/Inf with NA, then replace with zero
    is.na(ensemble_train_data_initial) <- sapply(ensemble_train_data_initial,
                                                 is.infinite)
    is.na(ensemble_train_data_initial) <- sapply(ensemble_train_data_initial,
                                                 is.nan)
    ensemble_train_data_initial[is.na(ensemble_train_data_initial)] = 0.01
    
    
    # ensemble models
    get_final_fcst_slice <- function(df){
      df %>%
        tidyr::separate(col=.id, sep="Slice", into=c("Slice", "Number")) %>%
        dplyr::mutate(Number = as.numeric(Number)) %>%
        dplyr::filter(Number == 1) %>%
        dplyr::select(-Slice, -Number) %>%
        dplyr::mutate(.id = "Final_FCST")
    }
    
    get_final_fcst_back_test <- function(df){
      df %>%
        dplyr::filter(Date <= hist_end_date) %>%
        tidyr::separate(col=.id, sep="Slice", into=c("Slice", "Number")) %>%
        dplyr::mutate(Number = as.numeric(Number) - 1) %>%
        dplyr::filter(Number < back_test_scenarios) %>%
        dplyr::mutate(Number_Char = ifelse(Number < 10,
                                           paste0("0", Number),
                                           paste0("", Number)),
                      .id = paste0("Back_Test_", Number_Char)) %>%
        dplyr::select(-Slice, -Number, -Number_Char)
    }
    
    fcst_tbl <- tibble::tibble()
    
    if(run_ensemble_models) {
      
      cli::cli_h3("Ensemble Model Training")
      
      #create modeltime table to add ensembled trained models to
      combined_ensemble_models <- modeltime::modeltime_table()
      
      ensemble_models <- get_r2_data_models()
      
      models_to_go_over <- ensemble_models[ensemble_models %in% names(model_list)]
      
      for(model_name in models_to_go_over){
        
        #model_fn <- as.character(ensemble_models[model_name])
        model_fn <- gsub("-", "_", model_name)
        add_name <- paste0(model_name,"-ensemble",model_name_suffix)
        
        try(mdl_ensemble <- invoke_forecast_function(fn_to_invoke =  model_fn,
                                                     train_data = ensemble_train_data_initial,
                                                     frequency = frequency_number,
                                                     parallel = run_model_parallel,
                                                     horizon = forecast_horizon,
                                                     seasonal_period =seasonal_periods,
                                                     back_test_spacing = back_test_spacing,
                                                     fiscal_year_start = fiscal_year_start,
                                                     tscv_inital = "1 year",
                                                     date_rm_regex = date_regex,
                                                     model_type = "ensemble", 
                                                     pca = FALSE))
        
        try(combined_ensemble_models <- modeltime::add_modeltime_model(combined_ensemble_models,
                                                                       mdl_ensemble,
                                                                       location = "top") %>%
              modeltime::update_model_description(1, add_name),
            silent = TRUE)
      }
      
      print(combined_ensemble_models)
      
      #create ensemble resamples to train future and back test folds
      cli::cli_h3("Refitting Ensemble Models")
      
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
          rsample::make_splits(x = list(analysis = seq(nrow(train)),
                                        assessment = nrow(train) + seq(nrow(test))),
                               class = "ts_cv_split")
        
        return(rsplit_obj)
      }
      
      ensemble_split_objs <- purrr::map(unique(ensemble_tscv$.id),
                                        .f=rsplit_ensemble_function)
      
      ensemble_tscv_final <- rsample::new_rset(splits = ensemble_split_objs,
                                               ids = unique(ensemble_tscv$.id),
                                               subclass = c("time_series_cv",
                                                            "rset"))
      
      fcst_tbl <- tibble::tibble()
      
      if(length(unique(combined_ensemble_models$.model_desc)) > 0) {
        ensemble_fcst <- combined_ensemble_models %>%
          get_model_time_resample_fit(ensemble_tscv_final) %>%
          dplyr::select(.id, Combo, Model, FCST, Target, Date, Horizon)
        
        fcst_tbl <- ensemble_fcst  %>%
          get_final_fcst_slice() %>%
          rbind(
            ensemble_fcst %>%
              get_final_fcst_back_test())
      }
      
    }
    
    #stop parallel processing
    if(run_model_parallel==TRUE){
      exit_parallel_within(parallel_args)
    }
    
    cli::cli_h3("Forecast Output")
    
    #Create forecast output
    fcst_tbl <- fcst_tbl %>%
      rbind(
        submodels_resample_tscv_tbl %>%
          get_final_fcst_slice()) %>%
      rbind(
        submodels_resample_tscv_tbl %>%
          get_final_fcst_back_test())
    
    return(fcst_tbl)
  }
  
  return (forecast_models)
}
