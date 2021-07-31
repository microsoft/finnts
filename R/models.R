#' Gets a simple workflow from model
#' 
#' @param model_spec Model Spec A
#' @param recipe_spec year, quarter, month, week, day
#' 
#' @return dplyr workflow spec
get_workflow_simple <- function(model_spec,
                                recipe_spec){
    workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(recipe_spec)
}

#' Gets a simple recipe
#' 
#' @param train_data Training Data 
#' 
#' @return simple recipie
get_recipie_simple <- function(train_data){
  recipes::recipe(Target ~ Date, data = train_data %>% 
                    dplyr::select(-Combo))
}

#' Gets a recipe that adjusts for fiscal year start
#' 
#' @param train_data Training Data 
#' @param fiscal_year_start Start of Fiscal Year
#' @param date_rm_regex_final Date removal RegEx Final
#'  
#' @return simple recipie
get_recipie_fiscal_year_adj<- function(train_data,
                                       fiscal_year_start,
                                       date_rm_regex_final){
  
    recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recipes::step_rm(matches(date_rm_regex_final), Date) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(Date_Adj_index.num, Date_Adj_year) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
}

#' Gets a recipe that adjusts for fiscal year start
#' 
#' @param train_data Training Data 
#' @param fiscal_year_start Start of Fiscal Year
#' @param date_rm_regex_final Date removal RegEx Final
#' @param model_type Model Type
#'  
#' @return simple recipie
get_recipie_fiscal_quarter_adj<- function(train_data,
                                       fiscal_year_start,
                                       date_rm_regex_final,
                                       model_type){
  
  recepie_step_using_model_type <- function(df,model_type){
    
    if(model_type == "ensemble") {
      df %>%
        recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num)
    }else{
      df %>%
        recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj)
    }
  }
  
  #create recipe
  recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recepie_step_using_model_type(model_type)%>%
    recipes::step_nzv(recipes::all_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
}


#' Gets a simple fit
#' 
#' @param wflw_spec Worflow Spec
#' @param train_data Training Data 
#' 
#' @return simple recipie
get_fit_simple <- function(wflw_spec,
                           train_data){
  wflw_spec %>%
    generics::fit(train_data %>% 
                    dplyr::select(-Combo))
}

#' Gets a simple fit
#' 
#' @param train_data Training Data 
#' @param tune_results Tune results
#' @param wflw_spec_tune Worflow Spec after tuning
#' 
#' @return simple recipie
get_fit_wkflw_best <- function(train_data,
                               tune_results,
                               wflw_spec_tune){
  best_results <- tune_results %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_spec_tune %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
}


#' Get simple orchestration
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' @param fnToCall usually a modeltime function e.x. modeltime::arima_reg
#' @param parsnip_engine ex. 'auto_arima'
#' 
#' @return gives the model fit
get_simple_orchestration <- function(train_data,
                                     frequency,
                                     fnToCall,
                                     parsnip_engine){
  
  recipie_simple <- get_recipie_simple(train_data)
  model_spec_fn <- fnToCall(frequency) %>%
    parsnip::set_engine(parsnip_engine)
  
  wflw_spec <- get_workflow_simple(model_spec_fn,
                                   recipie_simple)
  
  model_fit_fn <- get_fit_simple(wflw_spec,
                                 train_data)
  
  return(model_fit_fn)
}

#' Get tuning grid with resample
#' 
#' @param train_data Training Data
#' @param tscv_initial TS Cross Validation Initalization
#' @param horizon Horizon
#' @param back_test_spacing Back Testing Spacing
#' @param wkflw Workflow Objet from previous stage
#' @param parallel Allow Parallal (Default False) 
#' 
#' @return gives the model fit
get_resample_tune_grid<- function(train_data,
                                  tscv_initial,
                                  horizon,
                                  back_test_spacing,
                                  wkflw,
                                  parallel = FALSE){
  set.seed(123)
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune::tune_grid(
    object     = wkflw,
    resamples  = resamples_tscv,
    param_info = dials::parameters(wkflw) %>%
      update(learn_rate = dials::learn_rate(range = c(0.15, 0.5), trans = NULL)),
    grid       = 10, 
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, 
                                    parallel_over = "everything", 
                                    pkgs = get_export_packages())
    )
}

#' ARIMA Model 
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get the ARIMA based model
arima <- function(train_data, 
                  frequency) {
  
  print("arima")
  
  model_fit_auto_arima <- train_data %>%
    get_simple_orchestration(frequency,
                             modeltime::arima_reg,
                             "auto_arima")
  
  return(model_fit_auto_arima)
}



#' ARIMA Boost 
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' @param parallel Parallel Version or not
#' @param horizon Horizon of model
#' @param tscv_initial tscv initialization
#' @param date_rm_regex Date removal Regex
#' @param back_test_spacing Back Testing Spacing
#' @param fiscal_year_start Fiscal Year Start
#' 
#' @return Get the ARIMA based model
arima_boost <- function(train_data,
                        frequency,
                        parallel,
                        horizon,
                        tscv_initial,
                        date_rm_regex,
                        back_test_spacing,
                        fiscal_year_start) {
  
  frequency_arima_boost <- frequency
  
  #create model recipe
  date_rm_regex_final <- paste0(date_rm_regex)
  
  recipe_spec_arima_boost <-  train_data %>%
    get_recipie_fiscal_year_adj(fiscal_year_start,
                                date_rm_regex_final)
  
  #create model spec
  model_spec_arima_boost_tune = modeltime::arima_boost(
    mode            = "regression",
    seasonal_period = frequency_arima_boost,
    mtry            = tune::tune(),
    trees           = tune::tune(),
    min_n           = tune::tune(),
    tree_depth      = tune::tune(),
    learn_rate      = tune::tune(),
    loss_reduction  = tune::tune()
  ) %>%
    parsnip::set_engine("auto_arima_xgboost")
  
  
  wflw_spec_tune_arima_boost <- get_workflow_simple(model_spec_arima_boost_tune,
                                                    recipe_spec_arima_boost)
  
  
  tune_results_arima_boost <- train_data%>%
    get_resample_tune_grid(tscv_initial,
                         horizon,
                         back_test_spacing,
                         wflw_spec_tune_arima_boost)
  
  
  wflw_fit_arima_boost<- train_data %>% 
    get_fit_wkflw_best(wflw_spec_tune_arima_boost,
                       tune_results_arima_boost)
  
  print("arima-boost")
  
  return(wflw_fit_arima_boost)
}

#' Cubist Function 
#' 
#' @param train_data Training Data
#' @param parallel Parallel Version or not
#' @param model_type "single" "ensemble" etc.
#' @param horizon Horizon of model
#' @param tscv_initial tscv initialization
#' @param date_rm_regex Date removal Regex
#' @param back_test_spacing Back Testing Spacing
#' @param fiscal_year_start Fiscal Year Start
#' 
#' @return Get the ARIMA based model
cubist <- function(train_data,
                  parallel,
                  model_type = "single",
                  horizon,
                  tscv_initial,
                  date_rm_regex,
                  back_test_spacing,
                  fiscal_year_start) {
  

  date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
  
  
  #create recipe
  recipe_spec_cubist <-train_data %>% 
    get_recipie_fiscal_quarter_adj(fiscal_year_start,
                                   date_rm_regex_final,
                                   model_type)

  
  model_spec_cubist <- rules::cubist_rules(
    mode = "regression", 
    committees = tune::tune(), 
    neighbors = tune::tune(), 
    max_rules = tune::tune()
  ) %>%
    parsnip::set_engine("Cubist")
  
  wflw_spec_tune_cubist <- get_workflow_simple(model_spec_cubist,
                                               recipe_spec_cubist)
  
  
  tune_results_cubist <- train_data%>%
    get_resample_tune_grid(tscv_initial,
                           horizon,
                           back_test_spacing,
                           wflw_spec_tune_cubist,
                           parallel)
  
  
  wflw_fit_cubist<- train_data %>% 
    get_fit_wkflw_best(wflw_spec_tune_cubist,
                       tune_results_cubist)
  
  
  print("cubist")
  
  return(wflw_fit_cubist)
  
}

#' Croston Model 
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get the Croston based model
croston <- function(train_data, 
                   frequency) {
  
  print("croston")
  
  model_fit_croston <- train_data %>%
    get_simple_orchestration(frequency,
                             modeltime::exp_smoothing,
                             "croston")
  
  return(model_fit_croston)
}

deepar = function(train_data, 
                  horizon, 
                  frequency){
  
  recipe_spec_gluon = recipes::recipe(
    Target ~ Date + Combo, 
    data = train_data)
  
  model_spec_1 = modeltime.gluonts::deep_ar(
    id = "Combo", 
    freq = frequency, 
    prediction_length = as.numeric(horizon), 
    epochs = 5, 
    num_batches_per_epoch = 5
  ) %>%
    parsnip::set_engine("gluonts_deepar")
  
  wflw_fit_deepar_1 = workflows::workflow() %>%
    workflows::add_model(model_spec_1) %>%
    workflows::add_recipe(recipe_spec_gluon) %>%
    generics::fit(train_data)
  
  print(model_name)
  
  return(wflw_fit_deepar_1)
}

ets = function(train_data, 
               frequency) {
  
  frequency_ets = frequency
  
  recipe_spec_ets = recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo)) 
  
  model_spec_ets =modeltime::exp_smoothing(
    error = "auto",
    trend = "auto",
    season = "auto", 
    seasonal_period = frequency_ets) %>%
    parsnip::set_engine("ets")
  
  wflw_spec_ets = workflows::workflow() %>%
    workflows::add_model(model_spec_ets) %>%
    workflows::add_recipe(recipe_spec_ets)
  
  model_fit_ets = wflw_spec_ets %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('ets')
  
  return(model_fit_ets)
}

glmnet = function(train_data,
                  parallel,
                  model_type = "single",
                  horizon, tscv_initial,
                  date_rm_regex,
                  fiscal_year_start,
                  back_test_spacing){
  
  #create model recipe
  if(model_type == 'ensemble') {
    
    date_rm_regex_final = paste0(date_rm_regex, '|(year)')
    
    recipe_spec_glmnet = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE) %>%
      recipes::step_center(recipes::all_predictors()) %>%
      recipes::step_scale(recipes::all_predictors())
    
    
  } else {
    
    date_rm_regex_final = paste0(date_rm_regex, '|(year)')
    
    recipe_spec_glmnet = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE) %>%
      recipes::step_center(recipes::all_predictors()) %>%
      recipes::step_scale(recipes::all_predictors())
    
    # print(recipe_spec_glmnet %>% prep() %>% juice() %>% glimpse())
    # return(recipe_spec_glmnet %>% prep() %>% juice())
  }
  
  model_spec_glmnet = parsnip::linear_reg(
    mode = "regression", 
    penalty = tune::tune(), 
    mixture = tune::tune()
  ) %>%
    parsnip::set_engine("glmnet")
  
  wflw_spec_tune_glmnet = workflows::workflow() %>%
    workflows::add_model(model_spec_glmnet) %>%
    workflows::add_recipe(recipe_spec_glmnet) 
  
  set.seed(123)
  
  #resamples_kfold = train_data %>% rsample::vfold_cv(v = 5)
  resamples_tscv = timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  
  tune_results_glmnet = tune::tune_grid(
    object     = wflw_spec_tune_glmnet,
    resamples  = resamples_tscv,
    param_info = dials::parameters(wflw_spec_tune_glmnet),
    grid       = 10, 
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results = tune_results_glmnet %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_glmnet = wflw_spec_tune_glmnet %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_glmnet)
}

lightgbm = function(train_data, 
                    parallel) {
  
  #create model recipe
  recipe_spec_lightgbm = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(day)|(week)"), Date, Date_Adj) %>%
    recipes::step_nzv(recipes::all_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  
  model_spec_lightgbm = parsnip::boost_tree(
    mode = "regression",
    #trees = tune::tune(),
    #min_n = tune::tune(),
    tree_depth = tune::tune()
  ) %>%
    parsnip::set_engine("lightgbm")
  
  wflw_spec_tune_lightgbm = workflows::workflow() %>%
    workflows::add_model(model_spec_lightgbm) %>%
    workflows::add_recipe(recipe_spec_lightgbm) 
  
  set.seed(123)
  resamples_kfold = train_data %>% rsample::vfold_cv(v = 5)
  
  tune_results_lightgbm = tune::tune_grid(
    object     = wflw_spec_tune_lightgbm,
    resamples  = resamples_kfold,
    param_info = dials::parameters(wflw_spec_tune_lightgbm),
    grid       = 10, 
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results = tune_results_lightgbm %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_lightgbm = wflw_spec_tune_lightgbm %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  return(wflw_fit_lightgbm)
}

mars = function(train_data, 
                parallel, 
                model_type = "single",
                horizon,
                tscv_initial,
                date_rm_regex,
                model_name, 
                fiscal_year_start,
                back_test_spacing) {
  
  recipe_spec_mars = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recipes::step_rm(matches(date_rm_regex), Date, Date_Adj) %>%
    recipes::step_nzv(recipes::all_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
  
  model_spec_mars = parsnip::mars(
    mode = "regression", 
    num_terms = tune::tune(), 
    prod_degree = tune::tune(),
    prune_method = tune::tune()
  ) %>%
    parsnip::set_engine("earth")
  
  wflw_spec_tune_mars = workflows::workflow() %>%
    workflows::add_model(model_spec_mars) %>%
    workflows::add_recipe(recipe_spec_mars)
  
  set.seed(123)
  resamples_kfold = train_data %>% rsample::vfold_cv(v = 5)
  resamples_tscv = timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_mars = tune::tune_grid(
    object     = wflw_spec_tune_mars,
    resamples  = resamples_kfold,
    param_info = dials::parameters(wflw_spec_tune_mars),
    grid       = 10, 
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results = tune_results_mars %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_mars = wflw_spec_tune_mars %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_mars)
}

meanf = function(train_data, 
                 frequency) {
  
  recipe_spec_meanf = recipes::recipe(Target ~ Date, 
                                      data = train_data %>% 
                                        dplyr::select(-Combo))
  
  model_spec_meanf = modeltime::window_reg(
    window_size = frequency
  ) %>%
    parsnip::set_engine(
      engine = "window_function", 
      window_function = mean, 
      na.rm = TRUE)
  
  wflw_spec_meanf = workflows::workflow() %>%
    workflows::add_model(model_spec_meanf) %>%
    workflows::add_recipe(recipe_spec_meanf)
  
  model_fit_meanf = wflw_spec_meanf %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('meanf')
  
  return(model_fit_meanf)
}

nbeats = function(train_data, 
                  horizon, 
                  frequency) {
  
  frequency_nbeats = frequency
  horizon_nbeats = horizon
  
  recipe_spec_gluon = recipes::recipe(
    Target ~ Date + Combo, 
    data = train_data)
  
  model_spec_nbeats = modeltime.gluonts::nbeats(
    id = "Combo", 
    freq = frequency_nbeats,
    prediction_length = horizon_nbeats, 
    epochs = 5, 
    num_batches_per_epoch = 5
  ) %>%
    parsnip::set_engine("gluonts_nbeats")
  
  wflw_fit_nbeats = workflows::workflow() %>%
    workflows::add_model(model_spec_nbeats) %>%
    workflows::add_recipe(recipe_spec_gluon) %>%
    generics::fit(train_data)
  
  print(model_name)
  
  return(wflw_fit_nbeats)
  
}

nnetar = function(train_data,
                  frequency,
                  horizon,
                  parallel,
                  tscv_initial,
                  back_test_spacing) {
  
  resamples_tscv_lag = timetk::time_series_cv(
    data = train_data, 
    cumulative = TRUE, 
    asses = horizon, 
    skip = back_test_spacing, 
    initial = tscv_initial, 
    slice_limit = 100
  )
  
  recipe_spec_nnetar = recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_nnetar = modeltime::nnetar_reg(
    seasonal_period = frequency, 
    non_seasonal_ar = tune::tune(id = "non_seasoanl_ar"), 
    seasonal_ar = tune::tune(), 
    hidden_units = tune::tune(), 
    num_networks = tune::tune(), 
    penalty = tune::tune(), 
    epochs = tune::tune()
  ) %>%
    parsnip::set_engine('nnetar')
  
  set.seed(123)
  
  grid_spec_nnetar = dials::grid_latin_hypercube(
    dials::parameters(model_spec_nnetar), 
    size = 10
  )
  
  
  wflw_tune_nnetar = workflows::workflow() %>%
    workflows::add_recipe(recipe_spec_nnetar) %>%
    workflows::add_model(model_spec_nnetar)
  
  set.seed(123)
  
  tune_results_nnetar = wflw_tune_nnetar %>%
    tune::tune_grid(
      resamples = resamples_tscv_lag, 
      grid = grid_spec_nnetar, 
      metrics = modeltime::default_forecast_accuracy_metric_set(), 
      control = tune::control_grid(verbose = FALSE, save_pred = TRUE, allow_par = parallel, parallel_over = "everything", 
                                   pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                            'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                            'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                            "doParallel", "parallel"))
    )
  
  set.seed(123)
  
  wflw_fit_nnetar_tscv = wflw_tune_nnetar %>%
    tune::finalize_workflow(
      tune_results_nnetar %>%
        tune::show_best(metric = "rmse", n = Inf) %>%
        dplyr::slice(1)
    ) %>%
    generics::fit(train_data)
  
  print('nnetar')
  
  return(wflw_fit_nnetar_tscv)
} 

nnetar_xregs = function(train_data, 
                        frequency, 
                        horizon, 
                        parallel, 
                        tscv_initial,
                        date_rm_regex,
                        fiscal_year_start,
                        back_test_spacing) {
  
  resamples_tscv_lag = timetk::time_series_cv(
    data = train_data, 
    cumulative = TRUE, 
    asses = horizon, 
    skip = back_test_spacing, 
    initial = tscv_initial, 
    slice_limit = 100
  )
  
  
  date_rm_regex_final = paste0(date_rm_regex)
  
  recipe_spec_nnetar = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recipes::step_rm(matches(date_rm_regex_final), Date) %>%
    recipes::step_normalize(Date_Adj_index.num, Date_Adj_year) %>%
    recipes::step_nzv(recipes::all_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  
  model_spec_nnetar = modeltime::nnetar_reg(
    seasonal_period = frequency, 
    non_seasonal_ar = tune::tune(id = "non_seasoanl_ar"), 
    seasonal_ar = tune::tune(), 
    hidden_units = tune::tune(), 
    num_networks = tune::tune(), 
    penalty = tune::tune(), 
    epochs = tune::tune()
  ) %>%
    parsnip::set_engine('nnetar')
  
  set.seed(123)
  
  grid_spec_nnetar = dials::grid_latin_hypercube(
    dials::parameters(model_spec_nnetar), 
    size = 10
  )
  
  wflw_tune_nnetar = workflows::workflow() %>%
    workflows::add_recipe(recipe_spec_nnetar) %>%
    workflows::add_model(model_spec_nnetar)
  
  set.seed(123)
  
  tune_results_nnetar = wflw_tune_nnetar %>%
    tune::tune_grid(
      resamples = resamples_tscv_lag, 
      grid = grid_spec_nnetar, 
      metrics = modeltime::default_forecast_accuracy_metric_set(), 
      control = tune::control_grid(verbose = FALSE, save_pred = TRUE, allow_par = parallel, parallel_over = "everything", 
                                   pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                            'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                            'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                            "doParallel", "parallel"))
    )
  
  set.seed(123)
  
  wflw_fit_nnetar_tscv = wflw_tune_nnetar %>%
    tune::finalize_workflow(
      tune_results_nnetar %>%
        tune::show_best(metric = "rmse", n = Inf) %>%
        dplyr::slice(1)
    ) %>%
    generics::fit(train_data)
  
  print('nnetar-xregs')
  
  return(wflw_fit_nnetar_tscv)
} 

prophet = function(train_data,
                   parallel,
                   horizon,
                   tscv_initial,
                   back_test_spacing) {
  
  recipe_spec_prophet = recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_prophet =modeltime::prophet_reg(
    growth = tune::tune(), 
    changepoint_num = tune::tune(), 
    changepoint_range = tune::tune(), 
    seasonality_yearly = tune::tune(), 
    seasonality_weekly = tune::tune(), 
    seasonality_daily = tune::tune(), 
    prior_scale_changepoints = tune::tune(), 
    prior_scale_seasonality = tune::tune()
  ) %>%
    parsnip::set_engine("prophet")
  
  wflw_spec_prophet = workflows::workflow() %>%
    workflows::add_model(model_spec_prophet) %>%
    workflows::add_recipe(recipe_spec_prophet)
  
  set.seed(123)
  resamples_tscv = timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_prophet = tune::tune_grid(
    object     = wflw_spec_prophet,
    resamples  = resamples_tscv,
    param_info = dials::parameters(wflw_spec_prophet),
    grid       = 10, 
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results = tune_results_prophet %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_prophet = wflw_spec_prophet %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("prophet")
  
  return(wflw_fit_prophet)
}

prophet_boost = function(train_data,
                         parallel,
                         horizon,
                         tscv_initial,
                         date_rm_regex,
                         back_test_spacing,
                         fiscal_year_start) {
  
  #create model recipe
  date_rm_regex_final = paste0(date_rm_regex)
  
  recipe_spec_prophet_boost = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recipes::step_rm(matches(date_rm_regex_final), Date) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(Date_Adj_index.num, Date_Adj_year) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  
  #create model spec
  model_spec_prophet_boost_tune = modeltime::prophet_boost(
    mode            = "regression",
    mtry            = tune::tune(),
    trees           = tune::tune(),
    min_n           = tune::tune(),
    tree_depth      = tune::tune(),
    learn_rate      = tune::tune(),
    loss_reduction  = tune::tune()
  ) %>%
    parsnip::set_engine("prophet_xgboost")
  
  
  wflw_spec_tune_prophet_boost = workflows::workflow() %>%
    workflows::add_model(model_spec_prophet_boost_tune) %>%
    workflows::add_recipe(recipe_spec_prophet_boost)
  
  set.seed(123)
  #resamples_kfold = train_data %>% rsample::vfold_cv(v = 5)
  resamples_tscv = timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_prophet_boost = tune::tune_grid(
    object     = wflw_spec_tune_prophet_boost,
    resamples  = resamples_tscv,
    param_info = dials::parameters(wflw_spec_tune_prophet_boost) %>%
      update(learn_rate = dials::learn_rate(range = c(0.15, 0.5), trans = NULL)),
    grid       = 10, 
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results = tune_results_prophet_boost %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_prophet_boost = wflw_spec_tune_prophet_boost %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("prophet-boost")
  
  return(wflw_fit_prophet_boost)
}

prophet_xregs = function(train_data,
                         parallel,
                         horizon,
                         tscv_initial,
                         date_rm_regex,
                         fiscal_year_start,
                         back_test_spacing) {
  
  date_rm_regex_final = paste0(date_rm_regex)
  
  recipe_spec_prophet_xregs = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recipes::step_rm(matches(date_rm_regex), Date) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_mutate_at(where(is.character), fn = ~as.factor(.))
  
  model_spec_prophet_xregs =modeltime::prophet_reg(
    growth = tune::tune(), 
    changepoint_num = tune::tune(), 
    changepoint_range = tune::tune(), 
    seasonality_yearly = tune::tune(), 
    seasonality_weekly = tune::tune(), 
    seasonality_daily = tune::tune(), 
    prior_scale_changepoints = tune::tune(), 
    prior_scale_seasonality = tune::tune()
  ) %>%
    parsnip::set_engine("prophet")
  
  wflw_spec_prophet_xregs = workflows::workflow() %>%
    workflows::add_model(model_spec_prophet_xregs) %>%
    workflows::add_recipe(recipe_spec_prophet_xregs)
  
  set.seed(123)
  resamples_tscv = timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_prophet_xregs = tune::tune_grid(
    object     = wflw_spec_prophet_xregs,
    resamples  = resamples_tscv,
    param_info = dials::parameters(wflw_spec_prophet_xregs),
    grid       = 10, 
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results = tune_results_prophet_xregs %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_prophet_xregs = wflw_spec_prophet_xregs %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("prophet-xregs")
  
  return(wflw_fit_prophet_xregs)
}

snaive = function(train_data,
                  frequency) {
  
  recipe_spec_snaive = recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_snaive = modeltime::naive_reg(
    seasonal_period = frequency
  ) %>%
    parsnip::set_engine("snaive")
  
  wflw_spec_snaive = workflows::workflow() %>%
    workflows::add_model(model_spec_snaive) %>%
    workflows::add_recipe(recipe_spec_snaive)
  
  model_fit_snaive = wflw_spec_snaive %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('snaive')
  
  return(model_fit_snaive)
}

stlm_arima = function(train_data, seasonal_period) {
  
  seasonal_period_stlm_arima = seasonal_period
  
  recipe_spec_stlm_arima = recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo)) 
  
  model_spec_stlm_arima = modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_stlm_arima[1],
    seasonal_period_2 = seasonal_period_stlm_arima[2],
    seasonal_period_3 = seasonal_period_stlm_arima[3]
  ) %>%
    parsnip::set_engine("stlm_arima")
  
  wflw_spec_stlm_arima = workflows::workflow() %>%
    workflows::add_model(model_spec_stlm_arima) %>%
    workflows::add_recipe(recipe_spec_stlm_arima)
  
  model_fit_stlm_arima = wflw_spec_stlm_arima %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('stlm-arima')
  
  return(model_fit_stlm_arima)
}

stlm_ets = function(train_data, seasonal_period) {
  
  seasonal_period_stlm_ets = seasonal_period
  
  recipe_spec_stlm_ets = recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo)) 
  
  model_spec_stlm_ets = modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_stlm_ets[1],
    seasonal_period_2 = seasonal_period_stlm_ets[2],
    seasonal_period_3 = seasonal_period_stlm_ets[3]
  ) %>%
    parsnip::set_engine("stlm_ets")
  
  wflw_spec_stlm_ets = workflows::workflow() %>%
    workflows::add_model(model_spec_stlm_ets) %>%
    workflows::add_recipe(recipe_spec_stlm_ets)
  
  model_fit_stlm_ets = wflw_spec_stlm_ets %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('stlm-ets')
  
  return(model_fit_stlm_ets)
}

svm_poly = function(train_data,
                    parallel,
                    model_type = "single",
                    horizon,
                    tscv_initial,
                    date_rm_regex,
                    fiscal_year_start,
                    back_test_spacing) {
  
  if(model_type == 'ensemble') {
    
    date_rm_regex_final = paste0(date_rm_regex, '|(year)')
    
    recipe_spec_svm = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
    
  } else {
    
    date_rm_regex_final = date_rm_regex
    
    recipe_spec_svm = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj) %>%
      recipes::step_normalize(Date_Adj_index.num, Date_Adj_year) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
  }
  
  model_spec_svm = parsnip::svm_poly(
    mode = "regression", 
    cost = tune::tune(), 
    degree = tune::tune(), 
    margin = tune::tune(), 
    scale_factor = tune::tune()
  ) %>%
    parsnip::set_engine("kernlab")
  
  wflw_spec_tune_svm = workflows::workflow() %>%
    workflows::add_model(model_spec_svm) %>%
    workflows::add_recipe(recipe_spec_svm)
  
  set.seed(123)
  
  
  resamples_tscv = timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_svm = tune::tune_grid(
    object     = wflw_spec_tune_svm,
    resamples  = resamples_tscv,
    param_info = dials::parameters(wflw_spec_tune_svm),
    grid       = 10, 
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results = tune_results_svm %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_svm = wflw_spec_tune_svm %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_svm)
  
}

svm_rbf = function(train_data,
                   parallel,
                   model_type = "single",
                   horizon,
                   tscv_initial,
                   date_rm_regex,
                   fiscal_year_start,
                   back_test_spacing) {
  
  if(model_type == 'ensemble') {
    
    date_rm_regex_final = paste0(date_rm_regex, '|(year)')
    
    recipe_spec_svm = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
    
  } else {
    
    date_rm_regex_final = date_rm_regex
    
    recipe_spec_svm = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj) %>%
      recipes::step_normalize(Date_Adj_index.num, Date_Adj_year) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
  }
  
  model_spec_svm = parsnip::svm_rbf(
    mode = "regression", 
    cost = tune::tune(), 
    rbf_sigma = tune::tune(), 
    margin = tune::tune()
  ) %>%
    parsnip::set_engine("kernlab")
  
  wflw_spec_tune_svm = workflows::workflow() %>%
    workflows::add_model(model_spec_svm) %>%
    workflows::add_recipe(recipe_spec_svm)
  
  set.seed(123)
  
  #resamples_kfold = train_data %>% rsample::vfold_cv(v = 5)
  
  resamples_tscv = timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_svm = tune::tune_grid(
    object     = wflw_spec_tune_svm,
    resamples  = resamples_tscv,
    param_info = dials::parameters(wflw_spec_tune_svm),
    grid       = 10, 
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results = tune_results_svm %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_svm = wflw_spec_tune_svm %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_svm)
  
}

tabnet = function(train_data,
                  parallel) {
  
  #create model recipe
  recipe_spec_tabnet = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(day)|(week)"), Date, Date_Adj) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  
  model_spec_tabnet = tabnet::tabnet(
    mode = "regression",
    batch_size = tune::tune(),
    virtual_batch_size = tune::tune(),
    epochs = tune::tune()
  ) %>%
    parsnip::set_engine("torch")
  
  wflw_spec_tune_tabnet = workflows::workflow() %>%
    workflows::add_model(model_spec_tabnet) %>%
    workflows::add_recipe(recipe_spec_tabnet) 
  
  set.seed(123)
  resamples_kfold = train_data %>% rsample::vfold_cv(v = 5)
  
  tune_results_tabnet = tune::tune_grid(
    object     = wflw_spec_tune_tabnet,
    resamples  = resamples_kfold,
    param_info = dials::parameters(wflw_spec_tune_tabnet),
    grid       = 3, 
    control    = tune::control_grid(verbose = TRUE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results = tune_results_tabnet %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_tabnet = wflw_spec_tune_tabnet %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  return(wflw_fit_tabnet)
  
}

tbats = function(train_data,
                 seasonal_period) {

  seasonal_period_tbats = seasonal_period
  
  recipe_spec_tbats = recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_tbats = modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_tbats[1],
    seasonal_period_2 = seasonal_period_tbats[2],
    seasonal_period_3 = seasonal_period_tbats[3]
  ) %>%
    parsnip::set_engine("tbats")
  
  wflw_spec_tbats = workflows::workflow() %>%
    workflows::add_model(model_spec_tbats) %>%
    workflows::add_recipe(recipe_spec_tbats)
  
  model_fit_tbats = wflw_spec_tbats %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('tbats')
  
  return(model_fit_tbats)
}

theta = function(train_data,
                 frequency) {
  
  recipe_spec_theta = recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_theta =modeltime::exp_smoothing(
    seasonal_period = frequency) %>%
    parsnip::set_engine("theta")
  
  wflw_spec_theta = workflows::workflow() %>%
    workflows::add_model(model_spec_theta) %>%
    workflows::add_recipe(recipe_spec_theta)
  
  model_fit_theta = wflw_spec_theta %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("theta")
  
  return(model_fit_theta)
}

xgboost = function(train_data,
                   parallel,
                   model_type = "single",
                   horizon,
                   tscv_initial,
                   date_rm_regex,
                   fiscal_year_start,
                   back_test_spacing) {
  
  #create model recipe
  if(model_type == 'ensemble') {
    
    date_rm_regex_final = paste0(date_rm_regex, '|(year)')
    
    recipe_spec_xgboost = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num) %>%
      recipes::step_zv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
    
  } else {
    
    date_rm_regex_final = paste0(date_rm_regex)
    
    recipe_spec_xgboost = recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj) %>%
      recipes::step_zv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  }
  
  model_spec_xgboost = parsnip::boost_tree(
    mode = "regression",
    trees = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = tune::tune(),
    loss_reduction = tune::tune()
  ) %>%
    parsnip::set_engine("xgboost")
  
  wflw_spec_tune_xgboost = workflows::workflow() %>%
    workflows::add_model(model_spec_xgboost) %>%
    workflows::add_recipe(recipe_spec_xgboost) 
  
  set.seed(123)
  
  #resamples_kfold = train_data %>% rsample::vfold_cv(v = 5)
  resamples_tscv = timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_xgboost = tune::tune_grid(
    object     = wflw_spec_tune_xgboost,
    resamples  = resamples_tscv,
    param_info = dials::parameters(wflw_spec_tune_xgboost),
    grid       = 10, 
    control    = tune::control_grid(verbose = TRUE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results = tune_results_xgboost %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_xgboost = wflw_spec_tune_xgboost %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_xgboost)
}