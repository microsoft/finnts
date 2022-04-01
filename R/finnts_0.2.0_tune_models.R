#' Tune model hyperparameters
#' 
#' @param model_recipe_tbl model recipe table
#' @param model_workflow_tbl model workflow table
#' @param model_hyparameter_tbl model hyperparameter table
#' @param model_train_test_tbl model train test split table
#' @param run_global_models run global models
#' @param run_local_models run local models
#' @param global_model_recipes global model recipes
#' @param combo_variables combo variables
#' @param parallel_processing parallel processing
#' @param num_cores number of cores
#' @param seed seed number
#'  
#' @return table
#' @keywords internal
#' @export
tune_models <- function(model_recipe_tbl, 
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
    
    test_tbl <- initial_tuning_tbl %>%
      dplyr::filter(Combo == combo, 
                    Recipe_ID == recipe, 
                    Model == model) %>%
      dplyr::select(Model, Recipe_ID, Hyperparameter_ID, Train_Test_ID, Prediction, Model_Fit)
    
    # if(combo != "All-Data") {
    #   test_tbl <- initial_tuning_tbl %>%
    #     dplyr::filter(Combo == combo, 
    #                   Recipe_ID == recipe, 
    #                   Model == model) %>%
    #     dplyr::select(Model, Recipe_ID, Hyperparameter_ID, Train_Test_ID, Prediction, Model_Fit)
    # } else{
    #   test_tbl <- initial_tuning_tbl %>%
    #     dplyr::filter(Recipe_ID == recipe, 
    #                   Model == model) %>%
    #     dplyr::select(Model, Recipe_ID, Hyperparameter_ID, Train_Test_ID, Prediction, Model_Fit)
    # }
    
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
      tidyr::unnest(Prediction) %>%
      dplyr::select(Combo, Date, Train_Test_ID, Target, Forecast)
    
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