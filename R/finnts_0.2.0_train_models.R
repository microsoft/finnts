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
#' @param model_recipe_tbl 
#' @param models_to_run
#' @param models_not_to_run
#' @param run_deep_learning
#' @param pca
#'  
#' @return Returns back_test_scenarios and hist_periods_80
#' @noRd
model_workflows <- function(model_recipe_tbl, 
                            models_to_run = NULL, 
                            models_not_to_run = NULL, 
                            run_deep_learning = FALSE, 
                            pca = FALSE) {
  
  # get recipe input data
  combos <- unique(model_recipe_tbl$Combo)
  
  input_tbl <- model_recipe_tbl %>%
    dplyr::filter(Combo == combos[[1]])
  
  # tibble to add model workflows to
  model_workflow_tbl <- tibble::tibble()
  
  # models to run
  ml_models <- c("arima", "arima-boost", "cubist", "croston", "ets", "glmnet", "mars", "meanf", 
                 "nnetar", "nnetar-xregs", "prophet", "prophet-boost", "prophet-xregs", "snaive", 
                 "stlm-arima", "stlm-ets", "svm-poly", "svm-rbf", "tbats", "theta", "xgboost")
  
  if(is.null(models_to_run) & is.null(models_not_to_run)) {
    
    # do nothing, using existing ml_models list
    
  } else if(is.null(models_to_run) & !is.null(models_not_to_run)) {
    
    ml_models <- setdiff(ml_models, models_not_to_run)
    
  } else {
    
    if(!is.null(models_not_to_run)) {
      cli::cli_alert_warning("Note: 'models_to_run' argument overrides the 'models_not_to_run' argument")
    }
    
    ml_models <- models_to_run
    
  }
    
  if(run_deep_learning) {
    ml_models <- c(ml_models, "nnetar", "nbeats")
  }
  
  r2_models <- c("cubist", "glmnet", "svm-poly", "svm-rbf", "xgboost")
  
  iter_tbl <- tibble::tibble()
  
  for(recipe in unique(input_tbl$Recipe)) {
    iter_tbl <- rbind(iter_tbl, 
                      tibble::tibble(Model = ml_models, 
                                     Recipe = recipe))
  } 

  for(x in iter_tbl %>% dplyr::group_split(dplyr::row_number(), .keep = FALSE)) {

    model <- x %>%
      dplyr::pull(Model)
    
    recipe <- x %>%
      dplyr::pull(Recipe)
    
    recipe_tbl <- input_tbl  %>%
      dplyr::filter(Recipe == recipe) %>%
      dplyr::select(Data) %>%
      tidyr::unnest(Data) 
    
    # get args to feel into model spec functions
    avail_arg_list <- list('train_data' = recipe_tbl,
                           'frequency' = 12,
                           'horizon' = 12,
                           'seasonal_period' = c(3,6,12),
                           'model_type' = "single",
                           "pca" = TRUE)
    
    # don't create workflows for models that only use R1 recipe
    if(recipe == "R2" & !(model %in% r2_models)) {
      next
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
    
    model_workflow <- do.call(fn_to_invoke,inp_arg_list, quote=TRUE)

    # model_workflow <- workflows::workflow() %>%
    #   workflows::add_model(model_spec) %>%
    #   workflows::add_recipe(model_recipe)
    
    workflow_tbl <- tibble::tibble(Model_Name = model, 
                                   Model_Recipe = recipe, 
                                   Model_Workflow = list(model_workflow))
    
    model_workflow_tbl <- rbind(model_workflow_tbl, workflow_tbl)
  }
  
  return(model_workflow_tbl %>% dplyr::arrange(Model_Name))
}

#' Get model hyperparameters
#' 
#' @param model_workflow_tbl model workflow table
#' @param model_recipe_tbl model recipe table
#' @param num_hyperparameters number of hyperparameter combinations
#'  
#' @return table of model hyperparameters
#' @noRd
model_hyperparameters <- function(model_workflow_tbl, 
                                  model_recipe_tbl,
                                  num_hyperparameters = 5) {
  
  # get recipe input data
  combos <- unique(model_recipe_tbl$Combo)
  
  input_tbl <- model_recipe_tbl %>%
    dplyr::filter(Combo == combos[[1]])
  
  iter_tbl <- model_workflow_tbl %>%
    dplyr::select(Model_Name, Model_Recipe)
  
  # get hyperparameters
  hyperparameters_tbl <- tibble::tibble()
  
  for(x in iter_tbl %>% dplyr::group_split(dplyr::row_number(), .keep = FALSE)) {

    model <- x %>%
      dplyr::pull(Model_Name)
    
    recipe <- x %>%
      dplyr::pull(Model_Recipe)
    
    temp_tbl <- model_workflow_tbl %>%
      dplyr::filter(Model_Name == model, 
                    Model_Recipe == recipe)
    
    model_workflow <- temp_tbl$Model_Workflow[[1]]
    
    model_spec <- model_workflow %>%
      workflows::extract_spec_parsnip()
    
    recipe_features <- input_tbl %>%
      dplyr::filter(Recipe == recipe) %>%
      dplyr::select(Data) %>%
      tidyr::unnest(Data)
    
    if(dials::parameters(model_spec) %>% nrow() > 0) {
      
      if(model=="svm-rbf") {
        parameters <- model_spec %>%
          dials::parameters()
      } else {
        parameters <- model_spec %>%
          dials::parameters() %>%
          dials::finalize(recipe_features, force = FALSE)
      }

      grid <- dials::grid_latin_hypercube(parameters, size = num_hyperparameters)

      hyperparameters_temp <- grid %>%
        dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
        purrr::map_df(tidyr::nest, data=tidyselect::everything()) %>%
        dplyr::rename(Hyperparameters = data) %>%
        tibble::rowid_to_column("Hyperparameter_Combo") %>%
        dplyr::mutate(Model = model, 
                      Recipe = recipe)

    } else{
      
      hyperparameters_temp <- tibble::tibble(Hyperparameter_Combo = 1, 
                                             Hyperparameters = list(tibble::tibble()), 
                                             Model = model, 
                                             Recipe = recipe)
    }
    
    hyperparameters_tbl <- rbind(hyperparameters_tbl, hyperparameters_temp)
  }
  return(hyperparameters_tbl %>% dplyr::select(Model, Recipe, Hyperparameter_Combo, Hyperparameters))
}

#' Tune hyperparameters
#' 
#' @param model_recipe_tbl model recipe table
#' @param model_workflow_tbl model workflow table
#' @param model_hyparameter_tbl model hyperparameter table
#' @param model_train_test_tbl model train test split table
#' @param run_global_models run global models
#' @param run_local_models run local models
#' @param global_model_recipes global model recipes
#' @param combo_variables combo variables
#' @param parallel_processing
#' @param num_cores
#' @param seed seed number
#'  
#' @return table
#' @noRd
tune_hyperparameters <- function(model_recipe_tbl, 
                                 model_workflow_tbl, 
                                 model_hyperparameter_tbl, 
                                 model_train_test_tbl, 
                                 run_global_models, 
                                 run_local_models, 
                                 global_model_recipes, 
                                 combo_variables, 
                                 parallel_processing, 
                                 num_cores,
                                 seed = 123) {
  
  # get list of tasks to run
  combo_list <- c()
  
  global_model_list <- c("cubist", "glmnet", "mars", "svm-poly", "svm-rbf", "xgboost")
  
  if(run_local_models) {
    combo_list <- c(combo_list, unique(model_recipe_tbl$Combo))
  }
  
  if(run_global_models) {
    combo_list <- c(combo_list, "All-Data")
  }
  
  iter_list <- purrr::map(combo_list, .f = function(x) {
    model_train_test_tbl %>%
      dplyr::mutate(Combo = x) %>%
      dplyr::rename(Train_Test_ID = Run_ID) %>%
      dplyr::filter(Run_Type == "Validation") %>%
      dplyr::select(Combo, Train_Test_ID)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
    purrr::map(.f = function(x) {
      temp <- model_hyperparameter_tbl %>%
        dplyr::select(Hyperparameter_Combo, Model, Recipe) %>%
        dplyr::rename(Hyperparameter_ID = Hyperparameter_Combo, 
                      Recipe_ID = Recipe) %>%
        dplyr::mutate(Combo = x$Combo, 
                      Train_Test_ID = x$Train_Test_ID)
      
      if(x$Combo == 'All-Data') {
        temp <- temp %>%
          dplyr::filter(Model %in% global_model_list, 
                        Recipe_ID %in% global_model_recipes)
      }
      
      return(temp)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(Combo, Model, Recipe_ID, Train_Test_ID, Hyperparameter_ID)
  
  #return(iter_list)
  
  # task run function 
  model_workflow_tbl <- model_workflow_tbl # prevent error in exporting tbl to compute cluster
  
  initial_tune_fn <- function(x) {

    # run input values
    param_combo <- x %>%
      dplyr::pull(Hyperparameter_ID)
    
    model <- x %>%
      dplyr::pull(Model)
    
    data_split <- x %>%
      dplyr::pull(Train_Test_ID)
    
    data_prep_recipe <- x %>%
      dplyr::pull(Recipe_ID)
    
    combo <- x %>%
      dplyr::pull(Combo)
    
    train_end_date <- model_train_test_tbl %>%
      dplyr::filter(Run_ID == data_split) %>%
      dplyr::pull(Train_End)
    
    test_end_date <- model_train_test_tbl %>%
      dplyr::filter(Run_ID == data_split) %>%
      dplyr::pull(Test_End)

    # get train/test data
    full_data <- model_recipe_tbl %>%
      dplyr::filter(Recipe == data_prep_recipe) %>%
      dplyr::select(Data) %>%
      tidyr::unnest(Data)
    
    if(combo != "All-Data") {
      
      full_data <- full_data %>%
        dplyr::filter(Combo == combo)
      
    } else {
      
      full_data <- full_data %>%
        tidyr::separate(col = Combo, 
                        into = combo_variables, 
                        sep = "---", 
                        remove = FALSE)
    }
    
    training <- full_data %>% 
      dplyr::filter(Date <= train_end_date)
    
    testing <- full_data %>% 
      dplyr::filter(Date > train_end_date, 
                    Date <= test_end_date)
    
    if(data_prep_recipe == "R2") {
      
      train_origin_max <- training %>%
        dplyr::filter(Horizon == 1) 
      
      testing <- testing %>%
        dplyr::filter(Origin == max(train_origin_max$Origin) + 1)
    }

    # get workflow
    workflow <- model_workflow_tbl %>%
      dplyr::filter(Model_Name == model, 
                    Model_Recipe == data_prep_recipe)

    workflow_final <- workflow$Model_Workflow[[1]]

    # get hyperparameters
    hyperparameters <- model_hyperparameter_tbl %>%
      dplyr::filter(Model == model,
                    Recipe == data_prep_recipe, 
                    Hyperparameter_Combo == param_combo) %>%
      dplyr::select(Hyperparameters) %>%
      tidyr::unnest(Hyperparameters)
    
    # fit model
    set.seed(seed)

    if(nrow(hyperparameters) > 0) {
      model_fit <- workflow_final %>%
        tune::finalize_workflow(parameters = hyperparameters) %>%
        generics::fit(data = training)
    } else {
      model_fit <- workflow_final %>%
        generics::fit(data = training)
    }

    # create prediction
    model_prediction <- testing %>%
      dplyr::bind_cols(
        predict(model_fit, new_data = testing)
      ) %>%
      dplyr::select(Combo, Date, Target, .pred) %>%
      dplyr::rename(Forecast = .pred)
    
    # finalize output tbl
    final_tbl <- tibble::tibble(
      Combo = combo, 
      Model = model, 
      Recipe_ID = data_prep_recipe,
      Train_Test_ID = data_split, 
      Hyperparameter_ID = param_combo, 
      Model_Fit = list(model_fit), 
      Prediction = list(model_prediction)
    )

    return(final_tbl)
  }
  
  initial_tuning_tbl <- submit_fn(model_workflow_tbl,
                                  parallel_processing,
                                  iter_list %>%
                                    dplyr::group_split(dplyr::row_number(), .keep = FALSE),
                                  initial_tune_fn,
                                  num_cores,
                                  package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                                                      'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                                                      'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                                                      'recipes', 'rules', 'modeltime'),
                                  function_exports = NULL)
  
  # select the best combination of hyperparameters
  iter_list2 <- iter_list %>%
    dplyr::select(Combo, Model, Recipe_ID) %>%
    dplyr::distinct()
  
  choose_hyperparameters_fn <- function(x) {

    combo <- x %>%
      dplyr::pull(Combo)
    
    model <- x %>%
      dplyr::pull(Model)
    
    recipe <- x %>%
      dplyr::pull(Recipe_ID)
    
    if(combo != "All-Data") {
      test_tbl <- initial_tuning_tbl %>%
        dplyr::filter(Combo == combo, 
                      Recipe_ID == recipe, 
                      Model == model) %>%
        dplyr::select(Model, Recipe_ID, Hyperparameter_ID, Prediction, Model_Fit)
    } else{
      test_tbl <- initial_tuning_tbl %>%
        dplyr::filter(Recipe_ID == recipe, 
                      Model == model) %>%
        dplyr::select(Model, Recipe_ID, Hyperparameter_ID, Prediction, Model_Fit)
    }
    
    best_param <- test_tbl %>%
      dplyr::select(-Model_Fit) %>%
      tidyr::unnest(Prediction) %>%
      dplyr::mutate(Combo = combo) %>%
      dplyr::group_by(Combo, Model, Recipe_ID, Hyperparameter_ID) %>%
      yardstick::rmse(truth = Target,
                      estimate = Forecast,
                      na_rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.estimate) %>%
      dplyr::slice(1) %>%
      dplyr::pull(Hyperparameter_ID)
    
    best_model_fit <- test_tbl %>%
      dplyr::filter(Hyperparameter_ID == best_param) %>%
      dplyr::slice(1)
    
    best_model_fit <- best_model_fit$Model_Fit[[1]]
    
    final_predictions <- test_tbl %>%
      dplyr::filter(Hyperparameter_ID == best_param) %>%
      dplyr::select(-Model_Fit) %>%
      tidyr::unnest(Prediction)
    
    return(tibble::tibble(Combo = combo, 
                          Model = model, 
                          Recipe_ID = recipe, 
                          Hyperparameter_ID = best_param, 
                          Model_Fit = list(best_model_fit), 
                          Prediction = list(final_predictions)))
  }
  
  final_tuning_tbl <- submit_fn(model_workflow_tbl,
                                parallel_processing,
                                iter_list2 %>%
                                  dplyr::group_split(dplyr::row_number(), .keep = FALSE),
                                choose_hyperparameters_fn,
                                num_cores,
                                package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                                                    'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                                                    'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                                                    'recipes', 'rules', 'modeltime', 'yardstick'),
                                function_exports = NULL)
  
  return(final_tuning_tbl)
}

#' Refit Models
#' 
#' @param model_recipe_tbl model recipe table
#' @param model_fit_tbl model fit table
#' @param model_train_test_tbl model train test split table
#' @param combo_variables combo variables
#' @param parallel_processing
#' @param num_cores
#' @param seed seed number
#'  
#' @return table
#' @noRd
refit_models <- function(model_fit_tbl, 
                         model_recipe_tbl, 
                         model_train_test_tbl = NULL,
                         combo_variables, 
                         parallel_processing = NULL, 
                         num_cores = NULL,
                         seed = 123) {
  
  iter_list <- model_train_test_tbl %>%
    dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test")) %>%
    dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
    purrr::map(.f = function(x) {
      model_fit_tbl %>%
        dplyr::mutate(Run_Type = x %>% dplyr::pull(Run_Type), 
                      Run_ID = x %>% dplyr::pull(Run_ID), 
                      Train_End = x %>% dplyr::pull(Train_End), 
                      Test_End = x %>% dplyr::pull(Test_End)) %>%
        dplyr::select(-Model_Fit, -Prediction)}) %>%
    dplyr::bind_rows()
  
  fit_model <- function(x) {
    print(x)
    combo <- x %>%
      dplyr::pull(Combo)
    
    model <- x %>%
      dplyr::pull(Model)
    
    recipe <- x %>%
      dplyr::pull(Recipe_ID)
    
    model_fit <- model_fit_tbl %>%
      dplyr::filter(Combo == combo, 
                    Model == model, 
                    Recipe_ID == recipe)
    
    model_fit <- model_fit$Model_Fit[[1]]

    run_type <- x %>%
      dplyr::pull(Run_Type)
    
    run_id <- x %>%
      dplyr::pull(Run_ID)
    
    train_end <- x %>%
      dplyr::pull(Train_End)
    
    test_end <- x %>%
      dplyr::pull(Test_End)
    
    if(combo != 'All-Data') {
      
      recipe_data <- model_recipe_tbl %>%
        dplyr::filter(Recipe == recipe, 
                      Combo == combo) %>%
        dplyr::select(Data) %>%
        tidyr::unnest(Data)
      
    } else {
      
      recipe_data <- model_recipe_tbl %>%
        dplyr::filter(Recipe == recipe) %>%
        tidyr::separate(col = Combo, 
                        into = combo_variables, 
                        sep = "---", 
                        remove = FALSE) %>%
        dplyr::select(Data) %>%
        tidyr::unnest(Data)
    }
    
    training <- recipe_data %>%
      dplyr::filter(Date <= train_end)
    
    testing <- recipe_data %>%
      dplyr::filter(Date > train_end, 
                    Date <= test_end)
    
    if(recipe == "R2") {
      
      train_origin_max <- training %>%
        dplyr::filter(Horizon == 1) 
      
      testing <- testing %>%
        dplyr::filter(Origin == max(train_origin_max$Origin) + 1)
    }

    # fit model
    set.seed(seed)
    
    model_fit <- model_fit %>%
      generics::fit(data = training)
    
    # create prediction
    model_prediction <- testing %>%
      dplyr::bind_cols(
        predict(model_fit, new_data = testing)
      ) %>%
      dplyr::select(Combo, Date, Target, .pred) %>%
      dplyr::rename(Forecast = .pred)
    
    # finalize output tbl
    final_tbl <- tibble::tibble(
      Combo = combo, 
      Model = model, 
      Recipe_ID = recipe,
      Train_Test_ID = run_id, 
      Model_Fit = list(model_fit), 
      Prediction = list(model_prediction)
    )
    
    return(final_tbl)
  }
  
  model_recipe_tbl <- model_recipe_tbl # prevent error in exporting tbl to compute cluster
  
  model_refit_final_tbl <- submit_fn(model_fit_tbl,
                                     parallel_processing,
                                     iter_list %>%
                                       dplyr::group_split(dplyr::row_number(), .keep = FALSE),
                                     fit_model,
                                     num_cores,
                                     package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                                                         'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                                                         'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                                                         'recipes', 'rules', 'modeltime'),
                                     function_exports = NULL)
  
  return(model_refit_final_tbl)
}


# To Do
# [ ] should the recipe column be removed from the output of model_hyperparameters? 
#     this would be important to have if the same models with two recipes had diff hyperparameters
#     but models with params that change based on predictor column number are only R1 recipes (boost and mars models)
# [ ] allow user to select accuracy metric for hyperparameter selection
# [x] allow users to turn models on/off in workflow function
# [ ] fix long running parallel process for 2nd function call in parameter tuning
# [x] filter test data for R2 recipes
# [ ] adjust tuning process if only models with no hyperparameters are selected
# [ ] ensure tuning function can work in spark
