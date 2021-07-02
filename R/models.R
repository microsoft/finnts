# * Individual Model Functions ----

arima <- function(train_data, frequency, models_to_run, models_not_to_run) {
  
  if((!('arima' %in% models_to_run) & !is.null(models_to_run)) | ('arima' %in% models_not_to_run)) {return()}
  
  frequency_arima <- frequency
  
  recipe_spec_arima <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_arima <- modeltime::arima_reg(
    seasonal_period = frequency_arima
  ) %>%
    parsnip::set_engine("auto_arima")
  
  wflw_spec_arima <- workflows::workflow() %>%
    workflows::add_model(model_spec_arima) %>%
    workflows::add_recipe(recipe_spec_arima)
  
  model_fit_auto_arima <- wflw_spec_arima %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("arima")
  
  return(model_fit_auto_arima)
}

arima_boost <- function(train_data, frequency, parallel, horizon, tscv_initial, date_rm_regex, 
                        back_test_spacing, fiscal_year_start, models_to_run, models_not_to_run) {
  
  if((!('arima-boost' %in% models_to_run) & !is.null(models_to_run)) | ('arima-boost' %in% models_not_to_run)) {return()}
  
  frequency_arima_boost <- frequency
  
  #create model recipe
  date_rm_regex_final <- paste0(date_rm_regex)
  
  recipe_spec_arima_boost <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recipes::step_rm(matches(date_rm_regex_final), Date) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(Date_Adj_index.num, Date_Adj_year) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  
  #create model spec
  model_spec_arima_boost_tune <- modeltime::arima_boost(
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
  
  
  wflw_spec_tune_arima_boost <- workflows::workflow() %>%
    workflows::add_model(model_spec_arima_boost_tune) %>%
    workflows::add_recipe(recipe_spec_arima_boost)
  
  set.seed(123)
  #resamples_kfold <- train_data %>% rsample::vfold_cv(v = 5)
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_arima_boost <- tune::tune_grid(
    object     = wflw_spec_tune_arima_boost,
    resamples  = resamples_tscv,
    param_info = dials::parameters(wflw_spec_tune_arima_boost) %>%
      update(learn_rate = dials::learn_rate(range = c(0.15, 0.5), trans = NULL)),
    grid       = 10, 
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results <- tune_results_arima_boost %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_arima_boost <- wflw_spec_tune_arima_boost %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("arima-boost")
  
  return(wflw_fit_arima_boost)
}

cubist <- function(train_data, parallel, model_type = "single", horizon, tscv_initial, date_rm_regex, model_name, 
                   fiscal_year_start, back_test_spacing, models_to_run, models_not_to_run) {
  
  if((!(model_name %in% models_to_run) & !is.null(models_to_run)) | (model_name %in% models_not_to_run)) {return()}
  
  #create recipe
  if(model_type == "ensemble") {
    
    date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
    
    recipe_spec_cubist <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
    
  } else {
    
    date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
    
    recipe_spec_cubist <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
  }
  
  model_spec_cubist <- rules::cubist_rules(
    mode = "regression", 
    committees = tune::tune(), 
    neighbors = tune::tune(), 
    max_rules = tune::tune()
  ) %>%
    parsnip::set_engine("Cubist")
  
  wflw_spec_tune_cubist <- workflows::workflow() %>%
    workflows::add_model(model_spec_cubist) %>%
    workflows::add_recipe(recipe_spec_cubist)
  
  set.seed(123)
  
  #resamples_kfold <- train_data %>% rsample::vfold_cv(v = 5)
  
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_cubist <- tune::tune_grid(
    object     = wflw_spec_tune_cubist,
    resamples  = resamples_tscv,
    param_info = dials::parameters(wflw_spec_tune_cubist),
    grid       = 10, 
    metrics = modeltime::default_forecast_accuracy_metric_set(),
    control    = tune::control_grid(verbose = FALSE, allow_par = parallel, parallel_over = "everything", 
                                    pkgs = c('modeltime', 'modeltime.ensemble', 'modeltime.gluonts', 'modeltime.resample',
                                             'timetk', 'rlist', 'rules', 'Cubist', 'earth', 'kernlab', 'xgboost',
                                             'lightgbm', 'tidyverse', 'lubridate', 'prophet', 'torch', 'tabnet', 
                                             "doParallel", "parallel"))
  )
  
  best_results <- tune_results_cubist %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_cubist <- wflw_spec_tune_cubist %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_cubist)
  
}

croston <- function(train_data, frequency, models_to_run, models_not_to_run) {
  
  if((!('croston' %in% models_to_run) & !is.null(models_to_run)) | ('croston' %in% models_not_to_run)) {return()}
  
  recipe_spec_croston <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_croston <- modeltime::exp_smoothing(
    seasonal_period = frequency) %>%
    parsnip::set_engine("croston")
  
  wflw_spec_croston <- workflows::workflow() %>%
    workflows::add_model(model_spec_croston) %>%
    workflows::add_recipe(recipe_spec_croston)
  
  model_fit_croston <- wflw_spec_croston %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("croston")
  
  return(model_fit_croston)
}

deepar <- function(train_data, horizon, frequency, model_name, run_deep_learning, models_to_run, models_not_to_run) {
  
  if((!(model_name %in% models_to_run) & !is.null(models_to_run)) | run_deep_learning == FALSE | (model_name %in% models_not_to_run)) {return()}
  
  #horizon_deepar <- horizon 
  #frequency_deepar <- frequency
  
  recipe_spec_gluon <- recipes::recipe(
    Target ~ Date + Combo, 
    data = train_data)
  
  model_spec_1 <- modeltime.gluonts::deep_ar(
    id = "Combo", 
    freq = frequency, 
    prediction_length = as.numeric(horizon), 
    epochs = 5, 
    num_batches_per_epoch = 5
  ) %>%
    parsnip::set_engine("gluonts_deepar")
  
  wflw_fit_deepar_1 <- workflows::workflow() %>%
    workflows::add_model(model_spec_1) %>%
    workflows::add_recipe(recipe_spec_gluon) %>%
    generics::fit(train_data)
  
  print(model_name)
  
  return(wflw_fit_deepar_1)
}

ets <- function(train_data, frequency, models_to_run, models_not_to_run) {
  
  if((!('ets' %in% models_to_run) & !is.null(models_to_run)) | ('ets' %in% models_not_to_run)) {return()}
  
  frequency_ets <- frequency
  
  recipe_spec_ets <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo)) 
  
  model_spec_ets <-modeltime::exp_smoothing(
    error = "auto",
    trend = "auto",
    season = "auto", 
    seasonal_period = frequency_ets) %>%
    parsnip::set_engine("ets")
  
  wflw_spec_ets <- workflows::workflow() %>%
    workflows::add_model(model_spec_ets) %>%
    workflows::add_recipe(recipe_spec_ets)
  
  model_fit_ets <- wflw_spec_ets %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('ets')
  
  return(model_fit_ets)
}

glmnet <- function(train_data, parallel, model_type = "single", horizon, tscv_initial, date_rm_regex, model_name, 
                   fiscal_year_start, back_test_spacing, models_to_run, models_not_to_run) {
  
  if((!(model_name %in% models_to_run) & !is.null(models_to_run)) | (model_name %in% models_not_to_run)) {return()}
  
  #create model recipe
  if(model_type == 'ensemble') {
    
    date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
    
    recipe_spec_glmnet <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
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
    
    date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
    
    recipe_spec_glmnet <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
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
  
  model_spec_glmnet <- parsnip::linear_reg(
    mode = "regression", 
    penalty = tune::tune(), 
    mixture = tune::tune()
  ) %>%
    parsnip::set_engine("glmnet")
  
  wflw_spec_tune_glmnet <- workflows::workflow() %>%
    workflows::add_model(model_spec_glmnet) %>%
    workflows::add_recipe(recipe_spec_glmnet) 
  
  set.seed(123)
  
  #resamples_kfold <- train_data %>% rsample::vfold_cv(v = 5)
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  
  tune_results_glmnet <- tune::tune_grid(
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
  
  best_results <- tune_results_glmnet %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_glmnet <- wflw_spec_tune_glmnet %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_glmnet)
}

lightgbm <- function(train_data, parallel, models_to_run, models_not_to_run) {
  
  if((!('lightgbm' %in% models_to_run) & !is.null(models_to_run)) | (model_name %in% models_not_to_run)) {return()}
  
  #create model recipe
  recipe_spec_lightgbm <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(day)|(week)"), Date, Date_Adj) %>%
    recipes::step_nzv(recipes::all_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  
  model_spec_lightgbm <- parsnip::boost_tree(
    mode = "regression",
    #trees = tune::tune(),
    #min_n = tune::tune(),
    tree_depth = tune::tune()
  ) %>%
    parsnip::set_engine("lightgbm")
  
  wflw_spec_tune_lightgbm <- workflows::workflow() %>%
    workflows::add_model(model_spec_lightgbm) %>%
    workflows::add_recipe(recipe_spec_lightgbm) 
  
  set.seed(123)
  resamples_kfold <- train_data %>% rsample::vfold_cv(v = 5)
  
  tune_results_lightgbm <- tune::tune_grid(
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
  
  best_results <- tune_results_lightgbm %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_lightgbm <- wflw_spec_tune_lightgbm %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  return(wflw_fit_lightgbm)
}

mars <- function(train_data, parallel, model_type = "single", horizon, tscv_initial, date_rm_regex, model_name, 
                 fiscal_year_start, back_test_spacing, models_to_run, models_not_to_run) {
  
  if((!(model_name %in% models_to_run) & !is.null(models_to_run)) | (model_name %in% models_not_to_run)) {return()}
  
  
  #date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
  
  # recipe_spec_glmnet <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
  #   recipes::step_mutate(Date_Adj = Date %m-% months(6),
  #                        Horizon_Factor = as.factor(paste0('H', Horizon))) %>%
  #   timetk::step_timeseries_signature(Date_Adj) %>%
  #   recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
  #                        Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
  #   recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj) %>%
  #   recipes::step_nzv(recipes::all_predictors()) %>%
  #   recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE) %>%
  #   recipes::step_center(recipes::all_predictors()) %>%
  #   recipes::step_scale(recipes::all_predictors())
  
  recipe_spec_mars <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recipes::step_rm(matches(date_rm_regex), Date, Date_Adj) %>%
    recipes::step_nzv(recipes::all_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
  
  #write.csv(recipe_spec_mars %>% prep() %>% juice(), "\\\\fsu\\shares\\EDGFinBI\\EdgUsers\\Finn\\Mike Tokic\\Other\\mars_recipe.csv")
  
  
  model_spec_mars <- parsnip::mars(
    mode = "regression", 
    num_terms = tune::tune(), 
    prod_degree = tune::tune(),
    prune_method = tune::tune()
  ) %>%
    parsnip::set_engine("earth")
  
  wflw_spec_tune_mars <- workflows::workflow() %>%
    workflows::add_model(model_spec_mars) %>%
    workflows::add_recipe(recipe_spec_mars)
  
  set.seed(123)
  resamples_kfold <- train_data %>% rsample::vfold_cv(v = 5)
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_mars <- tune::tune_grid(
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
  
  best_results <- tune_results_mars %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_mars <- wflw_spec_tune_mars %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_mars)
}

meanf <- function(train_data, frequency, models_to_run, models_not_to_run) {
  
  if((!('meanf' %in% models_to_run) & !is.null(models_to_run)) | ('meanf' %in% models_not_to_run)) {return()}
  
  recipe_spec_meanf <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_meanf <- modeltime::window_reg(
    window_size = frequency
  ) %>%
    parsnip::set_engine(
      engine = "window_function", 
      window_function = mean, 
      na.rm = TRUE)
  
  wflw_spec_meanf <- workflows::workflow() %>%
    workflows::add_model(model_spec_meanf) %>%
    workflows::add_recipe(recipe_spec_meanf)
  
  model_fit_meanf <- wflw_spec_meanf %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('meanf')
  
  return(model_fit_meanf)
}

nbeats <- function(train_data, horizon, frequency, model_name, run_deep_learning, models_to_run, models_not_to_run) {
  
  if((!(model_name %in% models_to_run) & !is.null(models_to_run)) | run_deep_learning == FALSE | (model_name %in% models_not_to_run)) {return()}
  
  frequency_nbeats <- frequency
  horizon_nbeats <- horizon
  
  recipe_spec_gluon <- recipes::recipe(
    Target ~ Date + Combo, 
    data = train_data)
  
  model_spec_nbeats <- modeltime.gluonts::nbeats(
    id = "Combo", 
    freq = frequency_nbeats,
    prediction_length = horizon_nbeats, 
    epochs = 5, 
    num_batches_per_epoch = 5
  ) %>%
    parsnip::set_engine("gluonts_nbeats")
  
  wflw_fit_nbeats <- workflows::workflow() %>%
    workflows::add_model(model_spec_nbeats) %>%
    workflows::add_recipe(recipe_spec_gluon) %>%
    generics::fit(train_data)
  
  print(model_name)
  
  return(wflw_fit_nbeats)
  
}

nnetar <- function(train_data, frequency, horizon, parallel, tscv_initial,
                   back_test_spacing, models_to_run, models_not_to_run) {
  
  if((!('nnetar' %in% models_to_run) & !is.null(models_to_run)) | ('nnetar' %in% models_not_to_run)) {return()}
  
  resamples_tscv_lag <- timetk::time_series_cv(
    data = train_data, 
    cumulative = TRUE, 
    asses = horizon, 
    skip = back_test_spacing, 
    initial = tscv_initial, 
    slice_limit = 100
  )
  
  recipe_spec_nnetar <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_nnetar <- modeltime::nnetar_reg(
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
  
  grid_spec_nnetar <- dials::grid_latin_hypercube(
    dials::parameters(model_spec_nnetar), 
    size = 10
  )
  
  
  wflw_tune_nnetar <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec_nnetar) %>%
    workflows::add_model(model_spec_nnetar)
  
  set.seed(123)
  
  tune_results_nnetar <- wflw_tune_nnetar %>%
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
  
  wflw_fit_nnetar_tscv <- wflw_tune_nnetar %>%
    tune::finalize_workflow(
      tune_results_nnetar %>%
        tune::show_best(metric = "rmse", n = Inf) %>%
        dplyr::slice(1)
    ) %>%
    generics::fit(train_data)
  
  print('nnetar')
  
  return(wflw_fit_nnetar_tscv)
} 


nnetar_xregs <- function(train_data, frequency, horizon, parallel, tscv_initial, date_rm_regex, 
                         fiscal_year_start, back_test_spacing, models_to_run, models_not_to_run) {
  
  if((!('nnetar-xregs' %in% models_to_run) & !is.null(models_to_run)) | ('nnetar-xregs' %in% models_not_to_run)) {return()}
  
  resamples_tscv_lag <- timetk::time_series_cv(
    data = train_data, 
    cumulative = TRUE, 
    asses = horizon, 
    skip = back_test_spacing, 
    initial = tscv_initial, 
    slice_limit = 100
  )
  
  
  date_rm_regex_final <- paste0(date_rm_regex)
  
  recipe_spec_nnetar <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recipes::step_rm(matches(date_rm_regex_final), Date) %>%
    recipes::step_normalize(Date_Adj_index.num, Date_Adj_year) %>%
    recipes::step_nzv(recipes::all_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  
  model_spec_nnetar <- modeltime::nnetar_reg(
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
  
  grid_spec_nnetar <- dials::grid_latin_hypercube(
    dials::parameters(model_spec_nnetar), 
    size = 10
  )
  
  wflw_tune_nnetar <- workflows::workflow() %>%
    workflows::add_recipe(recipe_spec_nnetar) %>%
    workflows::add_model(model_spec_nnetar)
  
  set.seed(123)
  
  tune_results_nnetar <- wflw_tune_nnetar %>%
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
  
  wflw_fit_nnetar_tscv <- wflw_tune_nnetar %>%
    tune::finalize_workflow(
      tune_results_nnetar %>%
        tune::show_best(metric = "rmse", n = Inf) %>%
        dplyr::slice(1)
    ) %>%
    generics::fit(train_data)
  
  print('nnetar-xregs')
  
  return(wflw_fit_nnetar_tscv)
} 

prophet <- function(train_data, parallel, horizon, tscv_initial, back_test_spacing, models_to_run, models_not_to_run) {
  
  if((!('prophet' %in% models_to_run) & !is.null(models_to_run)) | ('prophet' %in% models_not_to_run)) {return()}
  
  recipe_spec_prophet <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_prophet <-modeltime::prophet_reg(
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
  
  wflw_spec_prophet <- workflows::workflow() %>%
    workflows::add_model(model_spec_prophet) %>%
    workflows::add_recipe(recipe_spec_prophet)
  
  set.seed(123)
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_prophet <- tune::tune_grid(
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
  
  best_results <- tune_results_prophet %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_prophet <- wflw_spec_prophet %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("prophet")
  
  return(wflw_fit_prophet)
  
  # model_fit_prophet <- wflw_spec_prophet %>%
  #   generics::fit(train_data %>% dplyr::select(-Combo))
  # 
  # print('prophet')
  # 
  # return(model_fit_prophet)
}

prophet_boost <- function(train_data, parallel, horizon, tscv_initial, date_rm_regex, 
                          back_test_spacing, fiscal_year_start, models_to_run, models_not_to_run) {
  
  if((!('prophet-boost' %in% models_to_run) & !is.null(models_to_run)) | ('prophet-boost' %in% models_not_to_run)) {return()}
  
  #create model recipe
  date_rm_regex_final <- paste0(date_rm_regex)
  
  recipe_spec_prophet_boost <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recipes::step_rm(matches(date_rm_regex_final), Date) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(Date_Adj_index.num, Date_Adj_year) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  
  #create model spec
  model_spec_prophet_boost_tune <- modeltime::prophet_boost(
    mode            = "regression",
    mtry            = tune::tune(),
    trees           = tune::tune(),
    min_n           = tune::tune(),
    tree_depth      = tune::tune(),
    learn_rate      = tune::tune(),
    loss_reduction  = tune::tune()
  ) %>%
    parsnip::set_engine("prophet_xgboost")
  
  
  wflw_spec_tune_prophet_boost <- workflows::workflow() %>%
    workflows::add_model(model_spec_prophet_boost_tune) %>%
    workflows::add_recipe(recipe_spec_prophet_boost)
  
  set.seed(123)
  #resamples_kfold <- train_data %>% rsample::vfold_cv(v = 5)
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_prophet_boost <- tune::tune_grid(
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
  
  best_results <- tune_results_prophet_boost %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_prophet_boost <- wflw_spec_tune_prophet_boost %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("prophet-boost")
  
  return(wflw_fit_prophet_boost)
}

prophet_xregs <- function(train_data, parallel, horizon, tscv_initial, date_rm_regex, 
                          fiscal_year_start, back_test_spacing, models_to_run, models_not_to_run) {
  
  if((!('prophet-xregs' %in% models_to_run) & !is.null(models_to_run)) | ('prophet-xregs' %in% models_not_to_run)) {return()}
  
  #recipe_spec_prophet <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  date_rm_regex_final <- paste0(date_rm_regex)
  
  #factor <- function(x) {as.factor(x)}
  
  recipe_spec_prophet_xregs <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                         Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
    recipes::step_rm(matches(date_rm_regex), Date) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_mutate_at(where(is.character), fn = ~as.factor(.))
  
  model_spec_prophet_xregs <-modeltime::prophet_reg(
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
  
  wflw_spec_prophet_xregs <- workflows::workflow() %>%
    workflows::add_model(model_spec_prophet_xregs) %>%
    workflows::add_recipe(recipe_spec_prophet_xregs)
  
  set.seed(123)
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_prophet_xregs <- tune::tune_grid(
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
  
  best_results <- tune_results_prophet_xregs %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_prophet_xregs <- wflw_spec_prophet_xregs %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("prophet-xregs")
  
  return(wflw_fit_prophet_xregs)
}


snaive <- function(train_data, frequency, models_to_run, models_not_to_run) {
  
  if((!('snaive' %in% models_to_run) & !is.null(models_to_run)) | ('snaive' %in% models_not_to_run)) {return()}
  
  recipe_spec_snaive <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_snaive <- modeltime::naive_reg(
    seasonal_period = frequency
  ) %>%
    parsnip::set_engine("snaive")
  
  wflw_spec_snaive <- workflows::workflow() %>%
    workflows::add_model(model_spec_snaive) %>%
    workflows::add_recipe(recipe_spec_snaive)
  
  model_fit_snaive <- wflw_spec_snaive %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('snaive')
  
  return(model_fit_snaive)
}

stlm_arima <- function(train_data, seasonal_period, models_to_run, models_not_to_run) {
  
  if((!('stlm-arima' %in% models_to_run) & !is.null(models_to_run)) | ('stlm-arima' %in% models_not_to_run)) {return()}
  
  seasonal_period_stlm_arima <- seasonal_period
  
  recipe_spec_stlm_arima <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo)) 
  
  model_spec_stlm_arima <- modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_stlm_arima[1],
    seasonal_period_2 = seasonal_period_stlm_arima[2],
    seasonal_period_3 = seasonal_period_stlm_arima[3]
  ) %>%
    parsnip::set_engine("stlm_arima")
  
  wflw_spec_stlm_arima <- workflows::workflow() %>%
    workflows::add_model(model_spec_stlm_arima) %>%
    workflows::add_recipe(recipe_spec_stlm_arima)
  
  model_fit_stlm_arima <- wflw_spec_stlm_arima %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('stlm-arima')
  
  return(model_fit_stlm_arima)
}

stlm_ets <- function(train_data, seasonal_period, models_to_run, models_not_to_run) {
  
  if((!('stlm-ets' %in% models_to_run) & !is.null(models_to_run)) | ('stlm-ets' %in% models_not_to_run)) {return()}
  
  seasonal_period_stlm_ets <- seasonal_period
  
  recipe_spec_stlm_ets <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo)) 
  
  model_spec_stlm_ets <- modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_stlm_ets[1],
    seasonal_period_2 = seasonal_period_stlm_ets[2],
    seasonal_period_3 = seasonal_period_stlm_ets[3]
  ) %>%
    parsnip::set_engine("stlm_ets")
  
  wflw_spec_stlm_ets <- workflows::workflow() %>%
    workflows::add_model(model_spec_stlm_ets) %>%
    workflows::add_recipe(recipe_spec_stlm_ets)
  
  model_fit_stlm_ets <- wflw_spec_stlm_ets %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('stlm-ets')
  
  return(model_fit_stlm_ets)
}

svm_poly <- function(train_data, parallel, model_type = "single", horizon, tscv_initial, date_rm_regex, 
                     fiscal_year_start, back_test_spacing, model_name, models_to_run, models_not_to_run) {
  
  if((!(model_name %in% models_to_run) & !is.null(models_to_run)) | (model_name %in% models_not_to_run)) {return()}
  
  
  if(model_type == 'ensemble') {
    
    date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
    
    recipe_spec_svm <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
    
  } else {
    
    date_rm_regex_final <- date_rm_regex
    
    recipe_spec_svm <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj) %>%
      recipes::step_normalize(Date_Adj_index.num, Date_Adj_year) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
  }
  
  model_spec_svm <- parsnip::svm_poly(
    mode = "regression", 
    cost = tune::tune(), 
    degree = tune::tune(), 
    margin = tune::tune(), 
    scale_factor = tune::tune()
  ) %>%
    parsnip::set_engine("kernlab")
  
  wflw_spec_tune_svm <- workflows::workflow() %>%
    workflows::add_model(model_spec_svm) %>%
    workflows::add_recipe(recipe_spec_svm)
  
  set.seed(123)
  
  #resamples_kfold <- train_data %>% rsample::vfold_cv(v = 5)
  
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_svm <- tune::tune_grid(
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
  
  best_results <- tune_results_svm %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_svm <- wflw_spec_tune_svm %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_svm)
  
}

svm_rbf <- function(train_data, parallel, model_type = "single", horizon, tscv_initial, date_rm_regex, 
                    fiscal_year_start, back_test_spacing, model_name, models_to_run, models_not_to_run) {
  
  if((!(model_name %in% models_to_run) & !is.null(models_to_run)) | (model_name %in% models_not_to_run)) {return()}
  
  if(model_type == 'ensemble') {
    
    date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
    
    recipe_spec_svm <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
    
  } else {
    
    date_rm_regex_final <- date_rm_regex
    
    recipe_spec_svm <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj) %>%
      recipes::step_normalize(Date_Adj_index.num, Date_Adj_year) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
  }
  
  model_spec_svm <- parsnip::svm_rbf(
    mode = "regression", 
    cost = tune::tune(), 
    rbf_sigma = tune::tune(), 
    margin = tune::tune()
  ) %>%
    parsnip::set_engine("kernlab")
  
  wflw_spec_tune_svm <- workflows::workflow() %>%
    workflows::add_model(model_spec_svm) %>%
    workflows::add_recipe(recipe_spec_svm)
  
  set.seed(123)
  
  #resamples_kfold <- train_data %>% rsample::vfold_cv(v = 5)
  
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_svm <- tune::tune_grid(
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
  
  best_results <- tune_results_svm %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_svm <- wflw_spec_tune_svm %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_svm)
  
}

tabnet <- function(train_data, parallel, models_to_run, models_not_to_run) {
  
  if((!('tabnet' %in% models_to_run) & !is.null(models_to_run)) | ('tabnet' %in% models_not_to_run)) {return()}
  
  #create model recipe
  recipe_spec_tabnet <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    recipes::step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(day)|(week)"), Date, Date_Adj) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  
  model_spec_tabnet <- tabnet::tabnet(
    mode = "regression",
    batch_size = tune::tune(),
    virtual_batch_size = tune::tune(),
    epochs = tune::tune()
  ) %>%
    parsnip::set_engine("torch")
  
  wflw_spec_tune_tabnet <- workflows::workflow() %>%
    workflows::add_model(model_spec_tabnet) %>%
    workflows::add_recipe(recipe_spec_tabnet) 
  
  set.seed(123)
  resamples_kfold <- train_data %>% rsample::vfold_cv(v = 5)
  
  tune_results_tabnet <- tune::tune_grid(
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
  
  best_results <- tune_results_tabnet %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_tabnet <- wflw_spec_tune_tabnet %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  return(wflw_fit_tabnet)
  
}

tbats <- function(train_data, seasonal_period, models_to_run, models_not_to_run) {
  
  if((!('tbats' %in% models_to_run) & !is.null(models_to_run)) | ('tbats' %in% models_not_to_run)) {return()}
  
  seasonal_period_tbats <- seasonal_period
  
  recipe_spec_tbats <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_tbats <- modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_tbats[1],
    seasonal_period_2 = seasonal_period_tbats[2],
    seasonal_period_3 = seasonal_period_tbats[3]
  ) %>%
    parsnip::set_engine("tbats")
  
  wflw_spec_tbats <- workflows::workflow() %>%
    workflows::add_model(model_spec_tbats) %>%
    workflows::add_recipe(recipe_spec_tbats)
  
  model_fit_tbats <- wflw_spec_tbats %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print('tbats')
  
  return(model_fit_tbats)
}

theta <- function(train_data, frequency, models_to_run, models_not_to_run) {
  
  if((!('theta' %in% models_to_run) & !is.null(models_to_run)) | ('theta' %in% models_not_to_run)) {return()}
  
  recipe_spec_theta <- recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
  
  model_spec_theta <-modeltime::exp_smoothing(
    seasonal_period = frequency) %>%
    parsnip::set_engine("theta")
  
  wflw_spec_theta <- workflows::workflow() %>%
    workflows::add_model(model_spec_theta) %>%
    workflows::add_recipe(recipe_spec_theta)
  
  model_fit_theta <- wflw_spec_theta %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print("theta")
  
  return(model_fit_theta)
}

xgboost <- function(train_data, parallel, model_type = "single", horizon, tscv_initial, date_rm_regex, 
                    fiscal_year_start, back_test_spacing, model_name, models_to_run, models_not_to_run) {
  
  if((!(model_name %in% models_to_run) & !is.null(models_to_run)) | (model_name %in% models_not_to_run)) {return()}
  
  #create model recipe
  if(model_type == 'ensemble') {
    
    date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
    
    recipe_spec_xgboost <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num) %>%
      recipes::step_zv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
    
  } else {
    
    date_rm_regex_final <- paste0(date_rm_regex)
    
    recipe_spec_xgboost <- recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
      recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1)) %>%
      timetk::step_timeseries_signature(Date_Adj) %>%
      recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                           Date_Adj_quarter_factor = as.factor(Date_Adj_quarter)) %>%
      recipes::step_rm(matches(date_rm_regex_final), Date, Date_Adj) %>%
      recipes::step_zv(recipes::all_predictors()) %>%
      recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
  }
  
  model_spec_xgboost <- parsnip::boost_tree(
    mode = "regression",
    trees = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = tune::tune(),
    loss_reduction = tune::tune()
  ) %>%
    parsnip::set_engine("xgboost")
  
  wflw_spec_tune_xgboost <- workflows::workflow() %>%
    workflows::add_model(model_spec_xgboost) %>%
    workflows::add_recipe(recipe_spec_xgboost) 
  
  set.seed(123)
  
  #resamples_kfold <- train_data %>% rsample::vfold_cv(v = 5)
  resamples_tscv <- timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
  
  tune_results_xgboost <- tune::tune_grid(
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
  
  best_results <- tune_results_xgboost %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_fit_xgboost <- wflw_spec_tune_xgboost %>%
    tune::finalize_workflow(parameters = best_results %>% dplyr::slice(1)) %>%
    generics::fit(train_data %>% dplyr::select(-Combo))
  
  print(model_name)
  
  return(wflw_fit_xgboost)
}