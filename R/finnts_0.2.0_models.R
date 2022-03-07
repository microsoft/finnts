#' Gets a simple recipe
#' 
#' @param train_data Training Data 
#' 
#' @return simple recipe
#' @noRd
get_recipie_simple <- function(train_data){
  recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
}

#' Gets a recipe with combo
#' 
#' @param train_data Training Data 
#' 
#' @return combo recipe
#' @noRd
get_recipie_combo <- function(train_data){
  recipes::recipe(Target ~  Date + Combo, data = train_data)
}

#' Gets a recipe that adjusts based on parameters
#' 
#' @param train_data Training Data 
#' @param mutate_adj_half parameter to add date adjustment
#' @param rm_date "plain", "with_adj", "with_adj_index"
#' @param step_nzv (Zero or NearZero Variance) "zv", "nzv", or none
#' @param norm_date_adj_year normalize date & year
#' @param dummy_one_hot one hot encoding
#' @param character_factor is character factor 
#' @param center_scale Center and scale 
#' @param one_hot True or False
#' @return configurable recipe
#' @noRd
get_recipie_configurable <- function(train_data,
                                     mutate_adj_half = FALSE, #todo Fix this. Should be true
                                     rm_date = "plain",
                                     step_nzv = "zv",
                                     norm_date_adj_year = FALSE,
                                     dummy_one_hot = TRUE,
                                     character_factor = FALSE,
                                     center_scale=FALSE,
                                     one_hot = FALSE, 
                                     pca = TRUE){
  
  mutate_adj_half_fn <- function(df){
    if(mutate_adj_half){
      df %>%
        recipes::step_mutate(Date_half_factor = as.factor(Date_half), 
                             Date_quarter_factor = as.factor(Date_quarter))
    }
    else{
      df
    }
  }
  
  rm_date_fn <- function(df){
    
    switch(rm_date,
           "with_adj" = df %>%
             recipes::step_rm(Date),
           "with_adj_index" = df %>%
             recipes::step_rm(Date, Date_index.num),
           df)

  }
  
  step_nz_fn <- function(df){
    
    switch(step_nzv,
           "zv" = df %>%
             recipes::step_zv(recipes::all_predictors()),
           "nzv" = df %>%
             recipes::step_nzv(recipes::all_predictors()),
           df)
  }
  
  norm_date_adj_year_fn <- function(df){
    if(norm_date_adj_year){
      df %>%
        recipes::step_normalize(Date_Adj_index.num, Date_Adj_year)
    }
    else{
      df
    }
  }
  
  dummy_one_hot_fn <- function(df){
    if(dummy_one_hot){
      df %>%
        recipes::step_dummy(recipes::all_nominal(), one_hot = one_hot)
    }
    else{
      df
    }
  }
  
  character_factor_fn <- function(df){
    if(character_factor){
      df %>%
        recipes::step_mutate_at(where(is.character), fn = ~as.factor(.))
    }
    else{
      df
    }
  }
  
  center_scale_fn <-function(df){
    if(center_scale){
      df %>%
        recipes::step_center(recipes::all_predictors()) %>%
        recipes::step_scale(recipes::all_predictors())
    }
    else{
      df
    }
  }
  
  pca_fn <-function(df){
    if(pca){
      df %>%
        recipes::step_pca(tidyselect::contains("lag"), threshold = .99, options = list(center = !center_scale, scale. = !center_scale))
    }
    else{
      df
    }
  }
  
  recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    mutate_adj_half_fn() %>%
    step_nz_fn() %>%
    rm_date_fn() %>%
    norm_date_adj_year_fn() %>%
    dummy_one_hot_fn() %>%
    character_factor_fn() %>%
    center_scale_fn() %>%
    pca_fn()
}


#' Gets a simple workflow from model
#' 
#' @param model_spec Model Spec A
#' @param recipe_spec year, quarter, month, week, day
#' 
#' @return dplyr workflow spec
#' @noRd
get_workflow_simple <- function(model_spec,
                                recipe_spec){
  workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(recipe_spec)
}

#' Gets a simple fit
#' 
#' @param train_data Training Data
#' @param wflw_spec Worflow Spec
#' 
#' @return simple recipe
#' @noRd
get_fit_simple <- function(train_data,
                           wflw_spec){
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
#' @noRd
get_fit_wkflw_best <- function(train_data,
                               tune_results,
                               wflw_spec_tune){

  best_results <- tune_results %>%
    tune::show_best(metric = "rmse", n = 10)
  
  wflw_spec_tune %>%
    tune::finalize_workflow(parameters = best_results %>%
                              dplyr::slice(1)) %>%
    generics::fit(train_data %>% 
                    dplyr::select(-Combo))
}

#' Gets a simple fit
#' 
#' @param train_data Training Data 
#' @param model_spec Model Spec
#' @param recipie_spec Recipe Spec
#' 
#' @return simple recipe
#' @noRd
get_fit_wkflw_nocombo <- function(train_data,
                                  model_spec,
                                  recipie_spec){
  get_workflow_simple(model_spec,recipie_spec) %>%
    generics::fit(train_data)
}

#' Get resample Time Series CV
#' 
#' @param train_data Training Data
#' @param tscv_initial TS Cross Validation Initialization
#' @param horizon Horizon
#' @param back_test_spacing Back Testing Spacing
#' 
#' @return gives the resample TS CV object
#' @noRd
get_resample_tscv <- function(train_data,
                              tscv_initial,
                              horizon,
                              back_test_spacing){

  timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
}

#' Get resample kFold CV
#' 
#' @param train_data Training Data
#' 
#' @return gives the resample kFold CV object
#' @noRd
get_resample_kfold <-function(train_data){

  train_data %>% 
    rsample::vfold_cv(v = 5)
}

#' Get tuning grid with resample
#' 
#' @param train_data Training Data
#' @param wkflw Workflow
#' @param isBoost isBoost params
#' @param resamples Resamples
#' @param parallel Parallel
#' @param isMetrics is metrics
#' 
#' @noRd
get_tune_grid <- function(train_data,
                          wkflw,
                          isBoost,
                          resamples,
                          parallel,
                          isMetrics
                          ){
  
  dial_by_boost <- function(wkflw,isBoost){
    
    params <- dials::parameters(wkflw)
    
    if(isBoost){
      params %>% stats::update(learn_rate = dials::learn_rate(range = c(0.15, 0.5), 
                                                       trans = NULL))
    }else
    {
      params
    }
  }
  
  tgCall <- list()
  tgCall$object <- wkflw
  tgCall$resamples <- resamples
  tgCall$param_info <- dial_by_boost(wkflw,isBoost)
  tgCall$grid <- 10
  tgCall$control <- tune::control_grid(verbose = FALSE, 
                                       allow_par = parallel, 
                                       parallel_over = NULL, 
                                       pkgs = get_export_packages())
  
  if(isMetrics){
    tgCall$metrics <- modeltime::default_forecast_accuracy_metric_set()
  }
  
  do.call(tune::tune_grid, tgCall, envir = globalenv())
}

#' Get tuning grid with resample
#' 
#' @param train_data Training Data
#' @param tscv_initial TS Cross Validation Initialization
#' @param horizon Horizon
#' @param back_test_spacing Back Testing Spacing
#' @param wkflw Workflow Objet from previous stage
#' @param parallel Allow Parallal (Default False) 
#' @param isBoost Add Boost
#' @param isMetrics Add Metrics
#' 
#' @return gives the model fit
#' @noRd
get_resample_tune_grid<- function(train_data,
                                  tscv_initial,
                                  horizon,
                                  back_test_spacing,
                                  wkflw,
                                  parallel = FALSE,
                                  isBoost = FALSE,
                                  isMetrics = FALSE){
  resamples_tscv <- train_data %>%
    get_resample_tscv( tscv_initial,
                       horizon,
                       back_test_spacing)
  
  train_data %>%
    get_tune_grid(wkflw,
                  isBoost,
                  resamples_tscv,
                  parallel,
                  isMetrics)
}

#' Get tuning grid k fold CV
#' 
#' @param train_data Training Data
#' @param wkflw Workflow Object from previous stage
#' @param parallel Allow Parallel (Default False) 
#' 
#' @return gives the model fit
#' @noRd
get_kfold_tune_grid<- function(train_data,
                               wkflw,
                               parallel = FALSE){
  
  resamples_cv <- train_data %>%
    get_resample_kfold()
  
  
  train_data %>%
    get_tune_grid(wkflw,
                  isBoost = FALSE,
                  resamples_cv,
                  parallel,
                  isMetrics = FALSE)
}

#' Get grid_latin_hypercube
#' 
#' @param model_spec Model Spec Obj
#' 
#' @return gives the latin hypercube grid
#' @noRd
get_latin_hypercube_grid<-function(model_spec){

  dials::grid_latin_hypercube(
    dials::parameters(model_spec), 
    size = 10
  )
}


#' ARIMA Model Spec
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get the ARIMA based model spec
#' @noRd
arima <- function(frequency) {

  modeltime::arima_reg(seasonal_period = frequency) %>%
    parsnip::set_engine("auto_arima")
}

#' ARIMA Boost Model Spec
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get the ARIMA based model spec
#' @noRd
arima_boost <- function(train_data,
                        frequency) {
  
  modeltime::arima_boost(
    mode            = "regression",
    seasonal_period = frequency,
    mtry            = tune::tune(),
    trees           = tune::tune(),
    min_n           = tune::tune(),
    tree_depth      = tune::tune(),
    learn_rate      = tune::tune(),
    loss_reduction  = tune::tune()
  ) %>%
    parsnip::set_engine("auto_arima_xgboost")
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
#' @param pca Run PCA
#' 
#' @return Get the cubist
#' @noRd
#' @examples
#' \donttest{
#' cubist_model <- cubist(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Date >= "2012-01-01")%>%
#'                  timetk::tk_augment_lags(.value = Target, .lags = c(3, 6, 12)), 
#'   parallel = FALSE, 
#'   horizon = 3, 
#'   tscv_initial = 24, 
#'   date_rm_regex = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(day)", 
#'   back_test_spacing = 3, 
#'   fiscal_year_start = 7, 
#'   pca = FALSE)
#' }
cubist <- function(train_data,
                  parallel,
                  model_type = "single",
                  horizon,
                  tscv_initial,
                  date_rm_regex,
                  back_test_spacing,
                  fiscal_year_start, 
                  pca) {
  
  rules::cubist_rules(
    mode = "regression", 
    committees = tune::tune(), 
    neighbors = tune::tune(), 
    max_rules = tune::tune()) %>%
    parsnip::set_engine("Cubist")
}

#' Croston Model 
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get the Croston based model
#' @noRd
#' @examples
#' \donttest{
#' croston_model <- croston(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   frequency = 12)
#' }
croston <- function(train_data, 
                   frequency) {
  
  modeltime::exp_smoothing(
    seasonal_period = frequency) %>%
    parsnip::set_engine("croston")
}

#' Deep AR Model Spec
#' 
#' @param train_data Training Data
#' @param horizon Horizon of forecast
#' @param frequency Frequency of Data
#' 
#' @return Get the DeepAR model spec
#' @noRd
deepar <- function(train_data, 
                   horizon, 
                   frequency){
  
  modeltime.gluonts::deep_ar(
    id = "Combo", 
    freq = frequency, 
    prediction_length = as.numeric(horizon), 
    epochs = 5, 
    num_batches_per_epoch = 5
  ) %>%
    parsnip::set_engine("gluonts_deepar")
}

#' ETS Model Spec
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get the ETS model spec
#' @noRd
ets <- function(train_data, 
               frequency) {
  
   modeltime::exp_smoothing(
    error = "auto",
    trend = "auto",
    season = "auto", 
    seasonal_period = frequency) %>%
    parsnip::set_engine("ets")
}

#' GLM Net Function 
#' 
#' @param train_data Training Data
#' @param parallel Parallel Version or not
#' @param model_type "single" "ensemble" etc.
#' @param horizon Horizon of model
#' @param tscv_initial tscv initialization
#' @param date_rm_regex Date removal Regex
#' @param back_test_spacing Back Testing Spacing
#' @param fiscal_year_start Fiscal Year Start
#' @param pca Run PCA
#' 
#' @return Get the GLM Net
#' @noRd
#' @examples
#' \donttest{
#' glmnet_model <- glmnet(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   parallel = FALSE, 
#'   horizon = 3, 
#'   tscv_initial = 12, 
#'   date_rm_regex = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(day)", 
#'   back_test_spacing = 1, 
#'   fiscal_year_start = 7, 
#'   pca = FALSE)
#' }
glmnet <- function(train_data,
                  parallel,
                  model_type = "single",
                  horizon,
                  tscv_initial,
                  date_rm_regex,
                  fiscal_year_start,
                  back_test_spacing, 
                  pca){

  parsnip::linear_reg(
    mode = "regression", 
    penalty = tune::tune(), 
    mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")
}

#' MARS Model Spec
#' 
#' @param train_data Training Data
#' @param parallel Parallel Version or not
#' @param model_type "single" "ensemble" etc.
#' @param date_rm_regex Date removal Regex
#' @param fiscal_year_start Fiscal Year Start
#' @param pca Run PCA
#' 
#' @return Get the Mars model spec
#' @noRd
mars <- function(train_data, 
                 pca) {
  
  parsnip::mars(
    mode = "regression", 
    num_terms = tune::tune(), 
    prod_degree = tune::tune(),
    prune_method = tune::tune()
  ) %>%
    parsnip::set_engine("earth")
}

#' Mean Forecast 
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get Mean Forecast Model
#' @noRd
#' @examples
#' \donttest{
#' meanf_model <- meanf(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   frequency = 12)
#' }
meanf <- function(train_data, 
                 frequency) {
  
  modeltime::window_reg(window_size = frequenc ) %>%
    parsnip::set_engine(
      engine = "window_function", 
      window_function = mean, 
      na.rm = TRUE)
}

#' nbeats model
#' 
#' @param train_data Training Data
#' @param horizon Horizon
#' @param frequency Frequency of Data
#' 
#' @return Get nbeats Model
#' @noRd
nbeats <- function(train_data, 
                  horizon, 
                  frequency) {
  
  recipe_spec_gluon <- train_data %>%
    get_recipie_combo()

  model_spec_nbeats <- modeltime.gluonts::nbeats(
    id = "Combo", 
    freq = frequency,
    prediction_length = horizon, 
    epochs = 5, 
    num_batches_per_epoch = 5
  ) %>%
    parsnip::set_engine("gluonts_nbeats")
  
  
  wflw_fit_nbeats<- train_data %>%
    get_fit_wkflw_nocombo(model_spec_nbeats,
                          recipe_spec_gluon)
  
  cli::cli_alert_success('nbeats')
  
  return(wflw_fit_nbeats)
}

#' nnetar model spec
#' 
#' @param frequency Frequency of Data
#' 
#' @return Get nnetar Model spec
#' @noRd
nnetar <- function(frequency) {
  
  modeltime::nnetar_reg(
    seasonal_period = frequency, 
    non_seasonal_ar = tune::tune(id = "non_seasoanl_ar"), 
    seasonal_ar = tune::tune(), 
    hidden_units = tune::tune(), 
    num_networks = tune::tune(), 
    penalty = tune::tune(), 
    epochs = tune::tune()
  ) %>%
    parsnip::set_engine('nnetar')
} 

#' nnetar xregs model spec
#' 
#' @param frequency Frequency of Data
#' 
#' @return Get nnetar xregs Model spec
#' @noRd
nnetar_xregs <- function(frequency) {
  
  modeltime::nnetar_reg(
    seasonal_period = frequency, 
    non_seasonal_ar = tune::tune(id = "non_seasoanl_ar"), 
    seasonal_ar = tune::tune(), 
    hidden_units = tune::tune(), 
    num_networks = tune::tune(), 
    penalty = tune::tune(), 
    epochs = tune::tune()
  ) %>%
    parsnip::set_engine('nnetar')
}

#' prophet model spec
#' 
#' @return Get prophet Model spec
#' @noRd
prophet <- function() {
  
  modeltime::prophet_reg(
    growth = tune::tune(), 
    changepoint_num = tune::tune(), 
    changepoint_range = tune::tune(), 
    seasonality_yearly = tune::tune(), 
    seasonality_weekly = tune::tune(), 
    seasonality_daily = tune::tune(), 
    prior_scale_changepoints = tune::tune(), 
    prior_scale_seasonality = tune::tune()) %>%
    parsnip::set_engine("prophet")
}

#' prophet boost model spec
#' 
#' @return Get prophet boost Model spec
#' @noRd
prophet_boost <- function(train_data,
                         horizon,
                         parallel,
                         tscv_initial,
                         date_rm_regex,
                         fiscal_year_start,
                         back_test_spacing, 
                         pca) {
  
  modeltime::prophet_boost(
    mode            = "regression",
    mtry            = tune::tune(),
    trees           = tune::tune(),
    min_n           = tune::tune(),
    tree_depth      = tune::tune(),
    learn_rate      = tune::tune(),
    loss_reduction  = tune::tune()) %>%
    parsnip::set_engine("prophet_xgboost")
}

#' prophet xregs model spec
#' 
#' @return Get prophet xregs Model spec
#' @noRd
prophet_xregs <- function() {
  
  modeltime::prophet_reg(
    growth = tune::tune(), 
    changepoint_num = tune::tune(), 
    changepoint_range = tune::tune(), 
    seasonality_yearly = tune::tune(), 
    seasonality_weekly = tune::tune(), 
    seasonality_daily = tune::tune(), 
    prior_scale_changepoints = tune::tune(), 
    prior_scale_seasonality = tune::tune()) %>%
    parsnip::set_engine("prophet")
}

#' SNaive model spec
#' 
#' @param frequency Frequency of Data
#' 
#' @return Get SNaive Forecast Model spec
#' @noRd
snaive <- function(frequency) {
  
  modeltime::naive_reg(seasonal_period = frequency) %>%
    parsnip::set_engine("snaive")
}

#' STLM Arima model spec
#' 
#' @param seasonal_period Seasonal Period
#' 
#' @return Get STLM Arima Forecast Model spec
#' @noRd
stlm_arima <- function(seasonal_period){
  
  modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_stlm_arima[1],
    seasonal_period_2 = seasonal_period_stlm_arima[2],
    seasonal_period_3 = seasonal_period_stlm_arima[3]
  ) %>%
    parsnip::set_engine("stlm_arima")
}

#' STLM ETS Model Spec
#' 
#' @param seasonal_period Seasonal Period
#' 
#' @return Get STLM ETS Forecast Model Spec
#' @noRd
stlm_ets <- function(seasonal_period) {
  
  modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_stlm_ets[1],
    seasonal_period_2 = seasonal_period_stlm_ets[2],
    seasonal_period_3 = seasonal_period_stlm_ets[3]
  ) %>%
    parsnip::set_engine("stlm_ets")
}

#' SVM Poly model spec
#' 
#' @return Get SVM Poly model spec
#' @noRd
svm_poly <- function() {
  
  parsnip::svm_poly(
    mode = "regression", 
    cost = tune::tune(), 
    degree = tune::tune(), 
    margin = tune::tune(), 
    scale_factor = tune::tune()
  ) %>%
    parsnip::set_engine("kernlab")
}

#' SVM RBF
#' 
#' @param train_data Training Data
#' @param horizon Horizon
#' @param parallel Parallel
#' @param model_type Type of Model
#' @param tscv_initial TS CV Initialization
#' @param date_rm_regex Date RM Regex
#' @param fiscal_year_start Fiscal Year Start
#' @param back_test_spacing Back Test Spacing
#' @param pca Run PCA
#' 
#' @return Get SVM RBF
#' @noRd
#' @examples
#' \donttest{
#' svm_rbf_model <- svm_rbf(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   parallel = FALSE, 
#'   horizon = 3, 
#'   tscv_initial = 12, 
#'   date_rm_regex = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(day)", 
#'   back_test_spacing = 1, 
#'   fiscal_year_start = 7, 
#'   pca = FALSE)
#' }
svm_rbf <- function() {
  
  parsnip::svm_rbf(
    mode = "regression", 
    cost = tune::tune(), 
    rbf_sigma = tune::tune(), 
    margin = tune::tune()) %>%
    parsnip::set_engine("kernlab")
}

#' Tbats Model Spec
#' 
#' @param seasonal_period Seasonal Period
#' 
#' @return Get TBats Model Spec
#' @noRd
tbats <- function(seasonal_period) {

  modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_tbats[1],
    seasonal_period_2 = seasonal_period_tbats[2],
    seasonal_period_3 = seasonal_period_tbats[3]
  ) %>%
    parsnip::set_engine("tbats")
}

#' Theta Model Spec
#' 
#' @param frequency Frequency of Data
#' 
#' @return Get the Theta model spec
#' @noRd
theta <- function(frequency) {
  
  modeltime::exp_smoothing(
    seasonal_period = frequency) %>%
    parsnip::set_engine("theta")
}

#' XGBoost
#' 
#' @return Get XGBoost model spec
#' @noRd
xgboost <-function() {
  
  parsnip::boost_tree(
    mode = "regression",
    trees = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = tune::tune(),
    loss_reduction = tune::tune()) %>%
    parsnip::set_engine("xgboost")
}