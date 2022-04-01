#' Refit Models
#' 
#' @param model_recipe_tbl model recipe table
#' @param model_fit_tbl model fit table
#' @param model_train_test_tbl model train test split table
#' @param combo_variables combo variables
#' @param parallel_processing parallel processing
#' @param num_cores number of cores
#' @param seed seed number
#'  
#' @return list of individual model predictions and fitted models
#' @keywords internal
#' @export
refit_models <- function(model_fit_tbl, 
                         model_recipe_tbl, 
                         model_train_test_tbl = NULL,
                         combo_variables, 
                         parallel_processing = NULL, 
                         num_cores = NULL,
                         seed = 123) {
  
  iter_list <- model_train_test_tbl %>%
    dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test", "Ensemble")) %>%
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
  
  fitted_models <- model_refit_final_tbl %>%
    dplyr::filter(Train_Test_ID == "01") %>%
    dplyr::select(Combo, Model, Recipe_ID, Model_Fit)
  
  return(list(Model_Predictions = model_refit_final_tbl %>% dplyr::select(-Model_Fit), Model_Fit = fitted_models))
}