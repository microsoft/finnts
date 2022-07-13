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
#' @param fiscal_year_start Start of Fiscal Year
#' @param date_rm_regex_final Date removal RegEx Final
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
                                     fiscal_year_start,
                                     date_rm_regex_final,
                                     mutate_adj_half = TRUE,
                                     rm_date = "plain",
                                     step_nzv = "nzv",
                                     norm_date_adj_year = FALSE,
                                     dummy_one_hot = TRUE,
                                     character_factor = FALSE,
                                     center_scale=FALSE,
                                     one_hot = FALSE, 
                                     pca = TRUE){
  
  mutate_adj_half_fn <- function(df){
    if(mutate_adj_half){
      df %>%
        recipes::step_mutate(Date_Adj_half_factor = as.factor(Date_Adj_half), 
                             Date_Adj_quarter_factor = as.factor(Date_Adj_quarter))
    }
    else{
      df
    }
  }
  
  rm_date_fn <- function(df){
    
    switch(rm_date,
           "plain" = df %>%
             recipes::step_rm(tidyselect::matches(date_rm_regex_final), Date),
           "with_adj" = df %>%
             recipes::step_rm(tidyselect::matches(date_rm_regex_final), Date, Date_Adj),
           "with_adj_index" = df %>%
             recipes::step_rm(tidyselect::matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num),
           "ensemble" = df %>%
             recipes::step_rm(tidyselect::matches(date_rm_regex_final), Date, Date_Adj, Date_Adj_index.num, 
                              tidyselect::contains("Date"), tidyselect::contains("Horizon")),
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
    step_nz_fn() %>%
    recipes::step_mutate(Date_Adj = Date %m+% months(fiscal_year_start-1), 
                         Date_day_month_end = ifelse(lubridate::day(Date_Adj) == lubridate::days_in_month(Date_Adj), 1, 0)) %>%
    timetk::step_timeseries_signature(Date_Adj) %>%
    mutate_adj_half_fn() %>%
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
    rsample::vfold_cv(v = 10)
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
    
    params <- workflows::extract_parameter_set_dials(wkflw)
    
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

#' There are modeling functions in this section

#' ARIMA Model 
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get the ARIMA based model
#' @noRd
#' @examples
#' \donttest{
#' arima_model <- arima(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   frequency = 12)
#' }
arima <- function(train_data, 
                  frequency) {

  recipie_simple <- train_data %>%
    get_recipie_simple()
  
  model_spec_arima <- modeltime::arima_reg(
    seasonal_period = frequency
  ) %>%
    parsnip::set_engine("auto_arima")
  
  wflw_spec <- get_workflow_simple(model_spec_arima,
                                   recipie_simple)
  
  model_fit_auto_arima <- train_data %>%
    get_fit_simple(wflw_spec)
  
  cli::cli_alert_success("arima")
  
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
#' @param pca Run PCA
#' 
#' @return Get the ARIMA based model
#' @noRd
#' @examples
#' \donttest{
#' arima_boost_model <- arima_boost(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01") %>%
#'                  timetk::tk_augment_lags(.value = Target, .lags = c(3, 6, 12)), 
#'   frequency = 12, 
#'   parallel = FALSE, 
#'   horizon = 3, 
#'   tscv_initial = 24, 
#'   date_rm_regex = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(day)", 
#'   back_test_spacing = 3, 
#'   fiscal_year_start = 7, 
#'   pca = FALSE)
#' }
arima_boost <- function(train_data,
                        frequency,
                        parallel,
                        horizon,
                        tscv_initial,
                        date_rm_regex,
                        back_test_spacing,
                        fiscal_year_start, 
                        pca) {
  
  #create model recipe
  date_rm_regex_final <- paste0(date_rm_regex)
  
  recipe_spec_arima_boost <-  train_data %>%
    get_recipie_configurable(fiscal_year_start,
                             date_rm_regex_final,
                             step_nzv = "zv",
                             norm_date_adj_year = TRUE,
                             one_hot = TRUE, 
                             pca = pca)
  
  #create model spec
  model_spec_arima_boost_tune = modeltime::arima_boost(
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
  
  
  wflw_spec_tune_arima_boost <- get_workflow_simple(model_spec_arima_boost_tune,
                                                    recipe_spec_arima_boost)
  
  tune_results_arima_boost <- train_data%>%
    get_resample_tune_grid(tscv_initial,
                         horizon,
                         back_test_spacing,
                         wflw_spec_tune_arima_boost,
                         parallel,
                         TRUE)
  

  wflw_fit_arima_boost<- train_data %>% 
    get_fit_wkflw_best(tune_results_arima_boost,
                       wflw_spec_tune_arima_boost)
  
  cli::cli_alert_success("arima-boost")
  
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
  

  date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
  
  
  if(model_type=="ensemble"){
    #create recipe
    recipe_spec_cubist <-train_data %>% 
      get_recipie_configurable(fiscal_year_start,
                               date_rm_regex_final,
                               rm_date = "with_adj_index",
                               step_nzv = "nzv",
                               one_hot = FALSE, 
                               pca = pca)
  }else{
    recipe_spec_cubist <-train_data %>% 
      get_recipie_configurable(fiscal_year_start,
                               date_rm_regex_final,
                               rm_date = "with_adj",
                               step_nzv = "nzv",
                               one_hot = FALSE, 
                               pca = pca)
  }

  model_spec_cubist <- parsnip::cubist_rules(
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
                           parallel,
                           FALSE,
                           TRUE)
  
  wflw_fit_cubist<- train_data %>% 
    get_fit_wkflw_best(tune_results_cubist, 
                       wflw_spec_tune_cubist)
  
  
  cli::cli_alert_success("cubist")
  
  return(wflw_fit_cubist)
  
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
  
  recipie_simple <- train_data %>%
    get_recipie_simple()
  
  model_spec_croston <- modeltime::exp_smoothing(
    seasonal_period = frequency) %>%
    parsnip::set_engine("croston")
  
  wflw_spec <- get_workflow_simple(model_spec_croston,
                                   recipie_simple)
  
  model_fit_croston <- train_data %>%
    get_fit_simple(wflw_spec)
  
  cli::cli_alert_success("croston")
  
  return(model_fit_croston)
}

#' Deep AR Model 
#' 
#' @param train_data Training Data
#' @param horizon Horizon of forecast
#' @param frequency Frequency of Data
#' 
#' @return Get the DeepAR model
#' @noRd
deepar <- function(train_data, 
                  horizon, 
                  frequency){
  
  recipe_spec_gluon <- train_data %>%
    get_recipie_combo()
  
  model_spec_1 <- modeltime.gluonts::deep_ar(
    id = "Combo", 
    freq = frequency, 
    prediction_length = as.numeric(horizon), 
    epochs = 5, 
    num_batches_per_epoch = 5
  ) %>%
    parsnip::set_engine("gluonts_deepar")
  
  wflw_fit_deepar_1 <- train_data %>%
    get_fit_wkflw_nocombo(model_spec_1,
                          recipe_spec_gluon)
  
  cli::cli_alert_success("deepar")
  
  return(wflw_fit_deepar_1)
}

#' ETS Model 
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get the ETS model
#' @noRd
#' @examples
#' \donttest{
#' ets_model <- ets(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   frequency = 12)
#' }
ets <- function(train_data, 
               frequency) {
  
  recipie_simple <- train_data %>%
    get_recipie_simple()
  
  model_spec_ets <-  modeltime::exp_smoothing(
    error = "auto",
    trend = "auto",
    season = "auto", 
    seasonal_period = frequency) %>%
    parsnip::set_engine("ets")
  
  wflw_fit_deepar_1 <- train_data %>%
    get_fit_wkflw_nocombo(model_spec_ets,
                          recipie_simple)
  
  model_fit_ets <- train_data %>%
    get_fit_simple(wflw_fit_deepar_1)
  
  cli::cli_alert_success('ets')
  
  return(model_fit_ets)
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

  date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
  
  if(model_type=="ensemble"){
    recipe_spec_glmnet <- train_data %>%
      get_recipie_configurable(fiscal_year_start,
                               date_rm_regex_final,
                               rm_date = "ensemble",
                               step_nzv = "none",
                               one_hot = FALSE,
                               center_scale = FALSE, 
                               pca = pca)
    
    model_spec_glmnet <- parsnip::linear_reg(
      mode = "regression", 
      penalty = tune::tune(), 
      mixture = tune::tune()
    ) %>%
      parsnip::set_engine("glmnet", 
                          lower.limits = 0)
    
    wflw_spec_tune_glmnet <- get_workflow_simple(model_spec_glmnet,
                                                 recipe_spec_glmnet)
    
    tune_results_glmnet <- train_data %>%
      get_kfold_tune_grid(wflw_spec_tune_glmnet,
                          parallel)
    
  }else{
    recipe_spec_glmnet <- train_data %>%
      get_recipie_configurable(fiscal_year_start,
                               date_rm_regex_final,
                               rm_date = "with_adj",
                               step_nzv = "nzv",
                               one_hot = FALSE,
                               center_scale = TRUE, 
                               pca = pca)
    
    model_spec_glmnet <- parsnip::linear_reg(
      mode = "regression", 
      penalty = tune::tune(), 
      mixture = tune::tune()
    ) %>%
      parsnip::set_engine("glmnet")
    
    wflw_spec_tune_glmnet <- get_workflow_simple(model_spec_glmnet,
                                                 recipe_spec_glmnet)
    
    tune_results_glmnet <- train_data %>%
      get_resample_tune_grid(tscv_initial,
                             horizon,
                             back_test_spacing,
                             wflw_spec_tune_glmnet,
                             parallel)
  }
  
  wflw_fit_glmnet <- train_data %>%
    get_fit_wkflw_best(tune_results_glmnet,
                       wflw_spec_tune_glmnet)
  
  cli::cli_alert_success('glmnet')
  
  return(wflw_fit_glmnet)
}

#' MARS Model 
#' 
#' @param train_data Training Data
#' @param parallel Parallel Version or not
#' @param model_type "single" "ensemble" etc.
#' @param date_rm_regex Date removal Regex
#' @param fiscal_year_start Fiscal Year Start
#' @param pca Run PCA
#' 
#' @return Get the Mars model
#' @noRd
#' @examples
#' \donttest{
#' mars_model <- mars(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   parallel = FALSE, 
#'   date_rm_regex = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(day)", 
#'   fiscal_year_start = 7, 
#'   pca = FALSE)
#' }
mars <- function(train_data, 
                parallel, 
                model_type = "single",
                date_rm_regex,
                fiscal_year_start, 
                pca) {
  
  recipe_spec_mars <- train_data %>%
    get_recipie_configurable(fiscal_year_start,
                             date_rm_regex,
                             rm_date = "with_adj", 
                             pca = pca)

  model_spec_mars <- parsnip::mars(
    mode = "regression", 
    num_terms = tune::tune(), 
    prod_degree = tune::tune(),
    prune_method = tune::tune()
  ) %>%
    parsnip::set_engine("earth")
  
  wflw_spec_tune_mars <- get_workflow_simple(model_spec_mars,
                                             recipe_spec_mars)
  
  tune_results_mars <- train_data %>%
    get_kfold_tune_grid(wflw_spec_tune_mars,
                        parallel)
  
  wflw_fit_mars <- train_data %>%
    get_fit_wkflw_best(tune_results_mars,
                       wflw_spec_tune_mars)
  
  cli::cli_alert_success("mars")
  
  return(wflw_fit_mars)
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
  
  recipe_spec_meanf <-train_data %>% 
    get_recipie_simple()
  
  model_spec_meanf = modeltime::window_reg(
    window_size = frequency
  ) %>%
    parsnip::set_engine(
      engine = "window_function", 
      window_function = mean, 
      na.rm = TRUE)
  
  wflw_spec_meanf <-  get_workflow_simple(model_spec_meanf,
                                          recipe_spec_meanf)
  
  model_fit_meanf <- train_data %>%
    get_fit_simple(wflw_spec_meanf)
  
  cli::cli_alert_success('meanf')
  
  return(model_fit_meanf)
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

#' nnetar model
#' 
#' @param train_data Training Data
#' @param horizon Horizon
#' @param frequency Frequency of Data
#' @param parallel Parallel
#' @param tscv_initial TS CV Initialization
#' @param back_test_spacing Back Test Spacing
#' 
#' @return Get nnetar Model
#' @noRd
#' @examples
#' \donttest{
#' nnetar_model <- nnetar(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   parallel = FALSE, 
#'   horizon = 1, 
#'   tscv_initial = 12, 
#'   back_test_spacing = 1, 
#'   frequency = 12)
#' }
nnetar <- function(train_data,
                   horizon,
                   frequency,
                   parallel,
                   tscv_initial,
                   back_test_spacing) {
  
  recipe_spec_nnetar <- train_data %>%
    get_recipie_simple()
  
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
  
  
  grid_spec_nnetar <- get_latin_hypercube_grid(model_spec_nnetar)
  
  wflw_tune_nnetar <- get_workflow_simple(model_spec_nnetar,
                                          recipe_spec_nnetar)

  tune_results_nnetar <-  train_data%>%
    get_resample_tune_grid(tscv_initial,
                           horizon,
                           back_test_spacing,
                           wflw_tune_nnetar,
                           parallel,
                           FALSE,
                           TRUE)

  wflw_fit_nnetar_tscv <- train_data %>%
    get_fit_wkflw_best(tune_results_nnetar,
                       wflw_tune_nnetar)
  
  cli::cli_alert_success('nnetar')
  
  return(wflw_fit_nnetar_tscv)
} 

#' nnetar model
#' 
#' @param train_data Training Data
#' @param horizon Horizon
#' @param frequency Frequency of Data
#' @param parallel Parallel
#' @param tscv_initial TS CV Initialization
#' @param date_rm_regex Date RM Regex
#' @param fiscal_year_start Fiscal Year Start
#' @param back_test_spacing Back Test Spacing
#' @param pca Run PCA
#' 
#' @return Get nnetar Model
#' @noRd
#' @examples
#' \donttest{
#' nnetar_xregs_model <- nnetar_xregs(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   parallel = FALSE, 
#'   frequency = 12,
#'   horizon = 3, 
#'   tscv_initial = 12, 
#'   date_rm_regex = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(day)", 
#'   back_test_spacing = 1, 
#'   fiscal_year_start = 7, 
#'   pca = FALSE)
#' }
nnetar_xregs <- function(train_data, 
                        horizon, 
                        frequency,
                        parallel, 
                        tscv_initial,
                        date_rm_regex,
                        fiscal_year_start,
                        back_test_spacing, 
                        pca) {
  
  date_rm_regex_final = paste0(date_rm_regex)
  
  recipe_spec_nnetar <- train_data %>%
    get_recipie_configurable(fiscal_year_start,
                             date_rm_regex_final,
                             norm_date_adj_year = TRUE,
                             one_hot = TRUE, 
                             pca = pca)
  
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
  
  grid_spec_nnetar <- get_latin_hypercube_grid(model_spec_nnetar)
  
  
  wflw_tune_nnetar <- get_workflow_simple(model_spec_nnetar,
                                          recipe_spec_nnetar)
  
  tune_results_nnetar <-  train_data%>%
    get_resample_tune_grid(tscv_initial,
                           horizon,
                           back_test_spacing,
                           wflw_tune_nnetar,
                           parallel,
                           FALSE,
                           TRUE)
  
  wflw_fit_nnetar_tscv <- train_data %>%
    get_fit_wkflw_best(tune_results_nnetar,
                       wflw_tune_nnetar)
  
  cli::cli_alert_success('nnetar-xregs')
  
  return(wflw_fit_nnetar_tscv)
}

#' prophet model
#' 
#' @param train_data Training Data
#' @param horizon Horizon
#' @param parallel Parallel
#' @param tscv_initial TS CV Initialization
#' @param back_test_spacing Back Test Spacing
#' 
#' @return Get prophet Model
#' @noRd
#' @examples
#' \donttest{
#' prophet_model <- prophet(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   parallel = FALSE, 
#'   horizon = 1, 
#'   tscv_initial = 12, 
#'   back_test_spacing = 1)
#' }
prophet <- function(train_data,
                   horizon,
                   parallel,
                   tscv_initial,
                   back_test_spacing) {
  
  recipe_spec_prophet <- train_data %>%
    get_recipie_simple()
  
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
  
  wflw_spec_prophet<- get_workflow_simple(model_spec_prophet,
                                          recipe_spec_prophet)
  
  
  tune_results_prophet <- train_data %>%
    get_resample_tune_grid(tscv_initial,
                           horizon,
                           back_test_spacing,
                           wflw_spec_prophet,
                           parallel)
  
  
  
  wflw_fit_prophet <- train_data %>%
    get_fit_wkflw_best(tune_results_prophet,
                       wflw_spec_prophet)
  
  cli::cli_alert_success("prophet")
  
  return(wflw_fit_prophet)
}

#' prophet boost
#' 
#' @param train_data Training Data
#' @param horizon Horizon
#' @param parallel Parallel
#' @param tscv_initial TS CV Initialization
#' @param date_rm_regex Date RM Regex
#' @param fiscal_year_start Fiscal Year Start
#' @param back_test_spacing Back Test Spacing
#' @param pca Run PCA
#' 
#' @return Get prophet boost Model
#' @noRd
#' @examples
#' \donttest{
#' prophet_boost_model <- prophet_boost(
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
prophet_boost <- function(train_data,
                         horizon,
                         parallel,
                         tscv_initial,
                         date_rm_regex,
                         fiscal_year_start,
                         back_test_spacing, 
                         pca) {
  
  #create model recipe
  date_rm_regex_final = paste0(date_rm_regex)
  
  recipe_spec_prophet_boost <- train_data %>%
    get_recipie_configurable(fiscal_year_start,
                             date_rm_regex_final,
                             step_nzv = "zv",
                             norm_date_adj_year = TRUE,
                             one_hot = TRUE, 
                             pca = pca)
  
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
  
  
  wflw_spec_tune_prophet_boost <- get_workflow_simple(model_spec_prophet_boost_tune,
                                                      recipe_spec_prophet_boost)
    
  tune_results_prophet_boost <- train_data %>%
    get_resample_tune_grid(tscv_initial,
                          horizon,
                          back_test_spacing,
                          wflw_spec_tune_prophet_boost,
                          parallel,
                          TRUE,
                          FALSE)
  
  wflw_fit_prophet_boost <-train_data %>% 
    get_fit_wkflw_best(tune_results_prophet_boost,
                       wflw_spec_tune_prophet_boost)
  
  cli::cli_alert_success("prophet-boost")
  
  return(wflw_fit_prophet_boost)
}

#' prophet xregs
#' 
#' @param train_data Training Data
#' @param horizon Horizon
#' @param parallel Parallel
#' @param tscv_initial TS CV Initialization
#' @param date_rm_regex Date RM Regex
#' @param fiscal_year_start Fiscal Year Start
#' @param back_test_spacing Back Test Spacing
#' @param pca Run PCA
#' 
#' @return Get prophet xregs Model
#' @noRd
#' @examples
#' \donttest{
#' prophet_xregs_model <- prophet_xregs(
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
prophet_xregs <- function(train_data,
                         horizon,
                         parallel,
                         tscv_initial,
                         date_rm_regex,
                         fiscal_year_start,
                         back_test_spacing, 
                         pca) {
  
  date_rm_regex_final <- paste0(date_rm_regex)
  
  recipe_spec_prophet_xregs <- train_data %>%
    get_recipie_configurable(fiscal_year_start,
                             date_rm_regex_final,
                             step_nzv = "zv",
                             dummy_one_hot = FALSE,
                             character_factor = TRUE, 
                             pca = pca)
  
  model_spec_prophet_xregs <- modeltime::prophet_reg(
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
  
  wflw_spec_prophet_xregs <- get_workflow_simple(model_spec_prophet_xregs,
                                                 recipe_spec_prophet_xregs)
  
  tune_results_prophet_xregs <- train_data %>% 
    get_resample_tune_grid(tscv_initial,
                           horizon,
                           back_test_spacing,
                           wflw_spec_prophet_xregs,
                           parallel = parallel)
  
  wkflw__fit_prophet_xregs <- train_data %>% 
    get_fit_wkflw_best(tune_results_prophet_xregs,
                       wflw_spec_prophet_xregs)
  
  cli::cli_alert_success("prophet-xregs")
  
  return(wkflw__fit_prophet_xregs)
}

#' SNaive 
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get SNaive Forecast Model
#' @noRd
#' @examples
#' \donttest{
#' snaive_model <- snaive(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   frequency = 12)
#' }
snaive <- function(train_data,
                  frequency) {
  
  recipe_spec_snaive <- train_data %>%
    get_recipie_simple()
  
  model_spec_snaive <- modeltime::naive_reg(
    seasonal_period = frequency
  ) %>%
    parsnip::set_engine("snaive")
  
  wflw_spec_snaive <- get_workflow_simple(model_spec_snaive,
                                          recipe_spec_snaive)
    
  model_fit_snaive <- train_data %>% 
    get_fit_simple(wflw_spec_snaive) 
  
  cli::cli_alert_success('snaive')
  
  return(model_fit_snaive)
}

#' STLM Arima 
#' 
#' @param train_data Training Data
#' @param seasonal_period Seasonal Period
#' 
#' @return Get STLM Arima Forecast Model
#' @noRd
#' @examples
#' \donttest{
#' stlm_arima_model <- stlm_arima(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   seasonal_period = c(3, 6, 12))
#' }
stlm_arima <- function(train_data, 
                       seasonal_period){
  
  seasonal_period_stlm_arima <- seasonal_period
  
  recipe_spec_stlm_arima <- train_data %>%
    get_recipie_simple() 
  
  model_spec_stlm_arima = modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_stlm_arima[1],
    seasonal_period_2 = seasonal_period_stlm_arima[2],
    seasonal_period_3 = seasonal_period_stlm_arima[3]
  ) %>%
    parsnip::set_engine("stlm_arima")
  
  wflw_spec_stlm_arima <- get_workflow_simple(model_spec_stlm_arima,
                                              recipe_spec_stlm_arima)
  
  model_fit_stlm_arima <- train_data %>% 
    get_fit_simple(wflw_spec_stlm_arima)
  
  cli::cli_alert_success('stlm-arima')
  
  return(model_fit_stlm_arima)
}

#' STLM ETS 
#' 
#' @param train_data Training Data
#' @param seasonal_period Seasonal Period
#' 
#' @return Get STLM ETS Forecast Model
#' @noRd
#' @examples
#' \donttest{
#' stlm_ets_model <- stlm_ets(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   seasonal_period = c(3,6,12))
#' }
stlm_ets <- function(train_data, seasonal_period) {
  
  seasonal_period_stlm_ets <- seasonal_period
  
  recipe_spec_stlm_ets <- train_data %>%
    get_recipie_simple()
  
  model_spec_stlm_ets = modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_stlm_ets[1],
    seasonal_period_2 = seasonal_period_stlm_ets[2],
    seasonal_period_3 = seasonal_period_stlm_ets[3]
  ) %>%
    parsnip::set_engine("stlm_ets")
  
  wflw_spec_stlm_ets <- get_workflow_simple(model_spec_stlm_ets,
                                            recipe_spec_stlm_ets)
  
  model_fit_stlm_ets <- train_data %>% 
    get_fit_simple(wflw_spec_stlm_ets)
  
  cli::cli_alert_success('stlm-ets')
  
  return(model_fit_stlm_ets)
}

#' SVM Poly
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
#' @return Get SVM Poly
#' @noRd
#' @examples
#' \donttest{
#' svm_poly_model <- svm_poly(
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
svm_poly <- function(train_data,
                    horizon,
                    parallel,
                    model_type = "single",
                    tscv_initial,
                    date_rm_regex,
                    fiscal_year_start,
                    back_test_spacing, 
                    pca) {
  
  if(model_type == 'ensemble') {
    
    date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
    
    recipe_spec_svm <- train_data %>%
      get_recipie_configurable(fiscal_year_start,
                               date_rm_regex_final,
                               rm_date = "with_adj_index",
                               one_hot = FALSE, 
                               pca = pca)
    
  } else {
    
    date_rm_regex_final <- date_rm_regex
    
    recipe_spec_svm <- train_data %>%
      get_recipie_configurable(fiscal_year_start,
                               date_rm_regex_final,
                               rm_date = "with_adj",
                               norm_date_adj_year = TRUE,
                               one_hot = FALSE, 
                               pca = pca)
  }
  
  model_spec_svm <- parsnip::svm_poly(
    mode = "regression", 
    cost = tune::tune(), 
    degree = tune::tune(), 
    margin = tune::tune(), 
    scale_factor = tune::tune()
  ) %>%
    parsnip::set_engine("kernlab")
  
  wflw_spec_tune_svm <- get_workflow_simple(model_spec_svm,
                                            recipe_spec_svm)
  
  
  tune_results_svm <- train_data %>%
    get_resample_tune_grid(tscv_initial,
                           horizon,
                           back_test_spacing,
                           wflw_spec_tune_svm,
                           parallel)
  
  wflw_fit_svm <- train_data %>%
    get_fit_wkflw_best(tune_results_svm,
                       wflw_spec_tune_svm)
  
  
  cli::cli_alert_success("svm-poly")
  
  return(wflw_fit_svm)
  
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
svm_rbf <- function(train_data,
                   horizon,
                   parallel,
                   model_type = "single",
                   tscv_initial,
                   date_rm_regex,
                   fiscal_year_start,
                   back_test_spacing, 
                   pca) {
  
  if(model_type == 'ensemble') {
    
    date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
    
    recipe_spec_svm <- train_data %>%
      get_recipie_configurable(fiscal_year_start,
                               date_rm_regex_final,
                               rm_date = "with_adj_index",
                               one_hot = FALSE, 
                               pca = pca)
  }else{
    
    date_rm_regex_final = date_rm_regex
    
    recipe_spec_svm <- train_data %>%
      get_recipie_configurable(fiscal_year_start,
                               date_rm_regex_final,
                               norm_date_adj_year = TRUE,
                               rm_date = "with_adj",
                               one_hot = FALSE, 
                               pca = pca)
  }
  
  model_spec_svm = parsnip::svm_rbf(
    mode = "regression", 
    cost = tune::tune(), 
    rbf_sigma = tune::tune(), 
    margin = tune::tune()
  ) %>%
    parsnip::set_engine("kernlab")
  
  wflw_spec_tune_svm <- get_workflow_simple(model_spec_svm,
                                            recipe_spec_svm)
  
  
  tune_results_svm <- train_data %>%
    get_resample_tune_grid(tscv_initial,
                           horizon,
                           back_test_spacing,
                           wflw_spec_tune_svm,
                           parallel)
  
  wflw_fit_svm <- train_data %>%
    get_fit_wkflw_best(tune_results_svm,
                       wflw_spec_tune_svm)
  
  cli::cli_alert_success("svm-rbf")
  
  return(wflw_fit_svm)
  
}

#' Tbats
#' 
#' @param train_data Training Data
#' @param seasonal_period Seasonal Period
#' 
#' @return Get TBats
#' @noRd
#' @examples
#' \donttest{
#' tbats_model <- tbats(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   seasonal_period = c(3,6,12))
#' }
tbats <- function(train_data,
                 seasonal_period) {

  seasonal_period_tbats <- seasonal_period
  
  recipe_spec_tbats <- train_data %>%
    get_recipie_simple()
  
  model_spec_tbats <- modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_tbats[1],
    seasonal_period_2 = seasonal_period_tbats[2],
    seasonal_period_3 = seasonal_period_tbats[3]
  ) %>%
    parsnip::set_engine("tbats")
  
  wflw_spec_tbats <- get_workflow_simple(model_spec_tbats,
                                         recipe_spec_tbats)
  
  model_fit_tbats <- train_data %>%
    get_fit_simple(wflw_spec_tbats)
  
  cli::cli_alert_success('tbats')
  
  return(model_fit_tbats)
}

#' Theta Model 
#' 
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' 
#' @return Get the Theta based model
#' @noRd
#' @examples
#' \donttest{
#' theta_model <- theta(
#'   train_data = modeltime::m750 %>% 
#'                  dplyr::rename(Date = date, Combo = id, Target = value) %>% 
#'                  dplyr::mutate(Combo = as.character(Combo)) %>%
#'                  dplyr::filter(Combo == "M750", 
#'                                Date >= "2012-01-01"), 
#'   frequency = 12)
#' }
theta <- function(train_data,
                 frequency) {
  
  recipe_spec_theta <- train_data %>%
    get_recipie_simple()
  
  
  model_spec_theta <- modeltime::exp_smoothing(
    seasonal_period = frequency) %>%
    parsnip::set_engine("theta")
  
  wflw_spec_theta <- get_workflow_simple(model_spec_theta,
                                         recipe_spec_theta)
  
  model_fit_theta <- train_data %>%
    get_fit_simple(wflw_spec_theta)
  
  cli::cli_alert_success("theta")
  
  return(model_fit_theta)
}

#' XGBoost
#' 
#' @param train_data Training Data
#' @param horizon Horizon
#' @param parallel Parallel
#' @param model_type Type of Model
#' @param tscv_initial TS CV Initialization
#' @param date_rm_regex Date RM Regex
#' @param fiscal_year_start Fiscal Year start
#' @param back_test_spacing Back Test Spacing
#' @param pca Run PCA
#' 
#' @return Get XGBoost
#' @noRd
#' @examples
#' \donttest{
#' xgboost_model <- xgboost(
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
xgboost <-function(train_data,
                   horizon,
                   parallel,
                   model_type = "single",
                   tscv_initial,
                   date_rm_regex,
                   fiscal_year_start,
                   back_test_spacing, 
                   pca) {
  
  #create model recipe
  if(model_type == 'ensemble') {
    
    date_rm_regex_final <- paste0(date_rm_regex, '|(year)')
    
    recipe_spec_xgboost <- train_data %>%
      get_recipie_configurable(fiscal_year_start,
                               date_rm_regex_final,
                               rm_date = "with_adj_index",
                               step_nzv = "zv",
                               one_hot = TRUE, 
                               pca = pca)
    
    print(recipe_spec_xgboost %>% recipes::prep() %>% recipes::juice() %>% dplyr::glimpse())
    
  } else {
    
    date_rm_regex_final <- paste0(date_rm_regex)
    
    recipe_spec_xgboost <- train_data %>%
      get_recipie_configurable(fiscal_year_start,
                               date_rm_regex_final,
                               rm_date = "with_adj",
                               step_nzv = "zv",
                               one_hot = TRUE, 
                               pca = pca)
  }
  
  model_spec_xgboost <- parsnip::boost_tree(
    mode = "regression",
    trees = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = tune::tune(),
    loss_reduction = tune::tune()
  ) %>%
    parsnip::set_engine("xgboost")
  
  wflw_spec_tune_xgboost <- get_workflow_simple(model_spec_xgboost,
                                                recipe_spec_xgboost)
  
  
  tune_results_xgboost <- train_data %>%
    get_resample_tune_grid(tscv_initial,
                           horizon,
                           back_test_spacing,
                           wflw_spec_tune_xgboost,
                           parallel)
  
  wflw_fit_xgboost <- train_data %>%
    get_fit_wkflw_best(tune_results_xgboost,
                       wflw_spec_tune_xgboost)

  cli::cli_alert_success("xgboost")
  
  return(wflw_fit_xgboost)
}