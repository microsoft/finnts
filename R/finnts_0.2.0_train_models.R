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
#' @param input_tbl full data table
#' @param hist_end_date historical end date
#' @param forecast_horizon forecast horizon
#' @param back_test_scenarios back test scenarios
#' @param back_test_spacing back test spacing
#'  
#' @return Returns back_test_scenarios and hist_periods_80
#' @noRd
get_back_test_scenario_hist_periods <- function(input_tbl,
                                                hist_end_date,
                                                forecast_horizon, 
                                                back_test_scenarios,
                                                back_test_spacing){

  historical_periods <- input_tbl %>%
    dplyr::filter(Date <= hist_end_date) %>%
    dplyr::select(Date) %>%
    unique() %>%
    nrow() %>%
    as.numeric()

  hist_periods_80 <- floor(historical_periods*0.8) #used with time series CV in multivariate models
  
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

#' Gets the train test splits
#' 
#' @param input_tbl full data table
#' @param hist_end_date historical end date
#' @param date_type
#' @param forecast_horizon forecast horizon
#' @param back_test_scenarios back test scenarios
#' @param back_test_spacing back test spacing
#'  
#' @return Returns back_test_scenarios and hist_periods_80
#' @noRd
train_test_split <- function(input_tbl, 
                             hist_end_date, 
                             date_type, 
                             forecast_horizon,
                             back_test_scenarios = NULL, 
                             back_test_spacing = NULL){

  back_test_spacing_final <- get_back_test_spacing(back_test_spacing, 
                                                   date_type)
  
  # pull out first recipe data
  temp_tbl <- input_tbl %>% 
    dplyr::slice(1) %>%
    dplyr::select(Data) %>%
    tidyr::unnest(Data)
  
  # get back test info
  bt <- temp_tbl %>%
    get_back_test_scenario_hist_periods(hist_end_date,
                                        forecast_horizon, 
                                        back_test_scenarios,
                                        back_test_spacing_final)
  
  back_test_scenarios_final <- bt$back_test_scenarios
  back_test_initial <- bt$hist_periods_80
  
  # create train/test split info
  train_test_initial <- temp_tbl %>%
    timetk::time_series_cv(
      initial = "1 year", 
      asses = forecast_horizon, 
      skip = back_test_spacing_final, 
      cumulative = TRUE, 
      slice_limit = 100) %>%
    timetk::tk_time_series_cv_plan() %>%
    tidyr::separate(col = .id, into = c(NA, "Slice_ID"), sep = "Slice") 
  
  train_test_final <- tibble::tibble()

  for(id in unique(train_test_initial$Slice_ID)) {
    
    temp_tbl <- train_test_initial %>%
      dplyr::filter(Slice_ID == id)

    train_tbl <- temp_tbl %>%
      dplyr::filter(.key == "training") %>%
      dplyr::select(Date) 
    
    if(as.numeric(id) == 1) {
      
      run_type <- "Future_Forecast"
      
      test_tbl <- temp_tbl %>%
        dplyr::filter(.key == "testing") %>%
        dplyr::select(Date) 
      
    } else if(as.numeric(id) > 1 && as.numeric(id) <= back_test_scenarios_final){
      
      run_type <- "Back_Test"
      
      test_tbl <- temp_tbl %>%
        dplyr::filter(Date <= hist_end_date) 
      
    } else if (as.numeric(id) > back_test_scenarios_final & as.numeric(id) < max(back_test_scenarios_final + (forecast_horizon/back_test_spacing_final) + 1, back_test_scenarios_final*1.5)) {
      
      run_type <- "Validation"
      
      back_test_date <- train_test_final %>%
        dplyr::filter(Run_Type == "Back_Test")

      test_tbl <- temp_tbl %>%
        dplyr::filter(Date <= min(back_test_date$Train_End))
      
    } else {
      run_type <- "Ensemble"
      
      test_tbl <- temp_tbl %>%
        dplyr::filter(.key == "testing") %>%
        dplyr::select(Date) 
    }

    train_test_tbl <- tibble::tibble(Run_Type = run_type, 
                                     Run_ID = id, 
                                     Train_End = max(train_tbl$Date), 
                                     Test_End = max(test_tbl$Date))
    
    train_test_final <- rbind(train_test_final, train_test_tbl)
  }
  
  # check for back test and validation data

  if(!("Validation" %in% unique(train_test_final$Run_Type))) {
    stop("No validation data produced. Add more historical data, shorten the forecast horizon, or shorten the number of back test scenarios")
  } else if(!("Back_Test" %in% unique(train_test_final$Run_Type))) {
    stop("No back testing data produced. Shorten the forecast horizon, or shorten the number of back test scenarios or back test spacing")
  }
  
  return(train_test_final)
}

#' Gets model workflows
#' 
#' @param input_tbl full data table
#' @param hist_end_date historical end date
#'  
#' @return Returns back_test_scenarios and hist_periods_80
#' @noRd
model_workflows <- function(models_to_run = NULL, 
                            models_not_to_run = NULL, 
                            run_deep_learning = FALSE, 
                            pca = FALSE) {
  
  # get args to feel into model spec functions
  avail_arg_list <- list('train_data' = tibble::tibble(),
                         'frequency' = 12,
                         'horizon' = 12,
                         'seasonal_period' = c(1,2,3),
                         "pca" = TRUE)
  
  # tibble to add model workflows to
  model_workflow_tbl <- tibble::tibble()
  
  # univariate models
  univariate_models <- c("arima", "ets", "croston", "meanf", "snaive", "stlm-arima", "stlm-ets", "tbats",
                         "theta")
  
  for(model in univariate_models) {
    print(model)
    model_recipe <- tibble::tibble(Combo = "temp data", 
                                   Target = 1, 
                                   Date = as.Date("2020-01-01")) %>%
      get_recipie_simple()
    
    # get specific model spec
    fn_to_invoke <- get(gsub('-', '_', model))
    
    exp_arg_list <- formalArgs(fn_to_invoke)
    
    avail_names <- names(avail_arg_list)
    
    inp_arg_list <- list()
    
    for(x in avail_names){
      
      if(x %in% exp_arg_list){
        inp_arg_list[x] <- avail_arg_list[x]
      }
    }
    
    model_spec <- do.call(fn_to_invoke,inp_arg_list, quote=TRUE)
    
    model_workflow <- workflows::workflow() %>%
      workflows::add_model(model_spec) %>%
      workflows::add_recipe(model_recipe)
    
    workflow_tbl <- tibble::tibble(Model_Name = model, 
                                   Model_Workflow = list(model_workflow))
    
    model_workflow_tbl <- rbind(model_workflow_tbl, workflow_tbl)
  }
  
  # deep learning models
  deep_learning_models <- c("deepar", "nbeats")
  deep_learning_models <- c()
  
  for(model in deep_learning_models) {
    print(model)
    
    model_recipe <- tibble::tibble(Combo = "temp data", 
                                   Target = 1, 
                                   Date = as.Date("2020-01-01")) %>%
      get_recipie_combo()
    
    # get specific model spec
    fn_to_invoke <- get(gsub('-', '_', model))
    
    exp_arg_list <- formalArgs(fn_to_invoke)
    
    avail_names <- names(avail_arg_list)
    
    inp_arg_list <- list()
    
    for(x in avail_names){
      
      if(x %in% exp_arg_list){
        inp_arg_list[x] <- avail_arg_list[x]
      }
    }
    
    model_spec <- do.call(fn_to_invoke,inp_arg_list, quote=TRUE)
    
    model_workflow <- workflows::workflow() %>%
      workflows::add_model(model_spec) %>%
      workflows::add_recipe(model_recipe)
    
    workflow_tbl <- tibble::tibble(Model_Name = model, 
                                   Model_Workflow = list(model_workflow))
    
    model_workflow_tbl <- rbind(model_workflow_tbl, workflow_tbl)
  }
  
  # hyperparmeter models
  hyperparameter_models <- c("arima-boost", "cubist", "glmnet", "mars", "nnetar", "nnetar-xregs", "prophet", 
                             "prophet-boost", "prophet-xregs", "svm-poly", "svm-rbf", "xgboost")
  
  for(model in hyperparameter_models) {
    print(model)
    
    if(model %in% c("arima-boost")) {
      model_recipe <- tibble::tibble(Combo = "temp data", 
                                     Target = 1, 
                                     Date = as.Date("2020-01-01")) %>%
        get_recipie_configurable(step_nzv = "zv",
                                 norm_date_adj_year = TRUE,
                                 one_hot = TRUE, 
                                 pca = pca)
      
    } else if(model %in% c("cubist")) {
      model_recipe <- tibble::tibble(Combo = "temp data", 
                                     Target = 1, 
                                     Date = as.Date("2020-01-01")) %>%
        get_recipie_configurable(rm_date = "with_adj",
                                 step_nzv = "zv",
                                 one_hot = FALSE, 
                                 pca = pca)
      
    } else if(model %in% c("glmnet")) {
      model_recipe <- tibble::tibble(Combo = "temp data", 
                                     Target = 1, 
                                     Date = as.Date("2020-01-01")) %>%
        get_recipie_configurable(rm_date = "with_adj",
                                 step_nzv = "nzv",
                                 one_hot = FALSE,
                                 center_scale = TRUE, 
                                 pca = pca)
    }
    
    # get specific model spec
    fn_to_invoke <- get(gsub('-', '_', model))
    
    exp_arg_list <- formalArgs(fn_to_invoke)
    
    avail_names <- names(avail_arg_list)
    
    inp_arg_list <- list()
    
    for(x in avail_names){
      
      if(x %in% exp_arg_list){
        inp_arg_list[x] <- avail_arg_list[x]
      }
    }
    
    model_spec <- do.call(fn_to_invoke,inp_arg_list, quote=TRUE)
    
    model_workflow <- workflows::workflow() %>%
      workflows::add_model(model_spec) %>%
      workflows::add_recipe(model_recipe)
    
    workflow_tbl <- tibble::tibble(Model_Name = model, 
                                   Model_Workflow = list(model_workflow))
    
    model_workflow_tbl <- rbind(model_workflow_tbl, workflow_tbl)
  }
  
  return(model_workflow_tbl)
}
