#' Refit Models
#' 
#' @param model_recipe_tbl model recipe table
#' @param model_workflow_tbl model workflow table
#' @param model_hyperparameter_tbl model hyperparameter table
#' @param model_tune_tbl model tune table
#' @param model_train_test_tbl model train test split table
#' @param combo_variables combo variables
#' @param parallel_processing parallel processing
#' @param num_cores number of cores
#' @param seed seed number
#'  
#' @return list of individual model predictions and fitted models
#' @keywords internal
#' @export
refit_models <- function(model_tune_tbl, 
                         model_recipe_tbl, 
                         model_workflow_tbl, 
                         model_hyperparameter_tbl, 
                         model_train_test_tbl,
                         combo_variables, 
                         parallel_processing = NULL, 
                         num_cores = NULL,
                         seed = 123) {
  
  if(is.null(parallel_processing)) {
    
    `%op%` <- foreach::`%do%`
    
    packages <- c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                  'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                  'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                  'recipes', 'rules', 'modeltime')
    
  } else if(parallel_processing == "spark") {
    
    cli::cli_h2("Submitting Tasks to Spark")
    
    `%op%` <- foreach::`%dopar%`
    
    sparklyr::registerDoSpark(sc, parallelism = length(unique(model_tune_tbl$Combo)))
    
    packages <- NULL
    
  } else if(parallel_processing == "local_machine") {
    
    cli::cli_h2("Creating Parallel Processing")
    
    cores <- get_cores(num_cores)
    
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    cli::cli_alert_info("Running across {cores} cores")
    
    `%op%` <- foreach::`%dopar%`
    
    packages <- c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                  'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                  'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                  'recipes', 'rules', 'modeltime')
    
  } else {
    stop("error")
  }
  
  model_refit_final_tbl <- foreach::foreach(x = unique(model_tune_tbl$Combo), 
                                       .combine = 'rbind', 
                                       .packages = packages,
                                       .errorhandling = "stop", 
                                       .verbose = FALSE, 
                                       .inorder = FALSE, 
                                       .multicombine = TRUE, 
                                       .noexport = NULL) %op% {
                                         
                                         combo <- x
                                         
                                         if(!is.null(parallel_processing) & combo!= "All-Data") {
                                           model_tune_tbl <- model_tune_tbl %>%
                                             dplyr::filter(Combo == combo)
                                           
                                           model_recipe_tbl <- model_recipe_tbl %>%
                                             dplyr::filter(Combo == combo)
                                           
                                         } else if(!is.null(parallel_processing) & combo == 'All-Data') {
                                           model_tune_tbl <- model_tune_tbl %>%
                                             dplyr::filter(Combo == combo)
                                         }
                                         
                                         iter_list <- model_train_test_tbl %>%
                                           dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test", "Ensemble")) %>%
                                           dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
                                           purrr::map(.f = function(x) {
                                             model_tune_tbl %>%
                                               dplyr::filter(Combo == combo) %>%
                                               dplyr::mutate(Run_Type = x %>% dplyr::pull(Run_Type), 
                                                             Run_ID = x %>% dplyr::pull(Run_ID), 
                                                             Train_End = x %>% dplyr::pull(Train_End), 
                                                             Test_End = x %>% dplyr::pull(Test_End)) %>%
                                               dplyr::select(-Prediction)}) %>%
                                           dplyr::bind_rows()
                                         
                                         output_tbl <- iter_list %>%
                                           dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
                                           purrr::map_dfr(.f = function(x) {
                                             
                                             print(x)
                                             
                                             combo <- x %>%
                                               dplyr::pull(Combo)
                                             
                                             model <- x %>%
                                               dplyr::pull(Model)
                                             
                                             recipe <- x %>%
                                               dplyr::pull(Recipe_ID)
                                             
                                             param <- x %>%
                                               dplyr::pull(Hyperparameter_ID)
                                             
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
                                             
                                             # get workflow
                                             workflow <- model_workflow_tbl %>%
                                               dplyr::filter(Model_Name == model, 
                                                             Model_Recipe == recipe)
                                             
                                             workflow_final <- workflow$Model_Workflow[[1]]
                                             
                                             # get hyperparameters
                                             hyperparameters <- model_hyperparameter_tbl %>%
                                               dplyr::filter(Model == model,
                                                             Recipe == recipe, 
                                                             Hyperparameter_Combo == param) %>%
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
                                             if(run_id == "01") {
                                               model_fit <- model_fit
                                             } else {
                                               model_fit <- NULL
                                             }
                                             
                                             final_tbl <- tibble::tibble(
                                               Combo = combo, 
                                               Model = model, 
                                               Recipe_ID = recipe,
                                               Train_Test_ID = run_id, 
                                               Model_Fit = list(model_fit), 
                                               Prediction = list(model_prediction)
                                             )
                                             
                                             return(final_tbl)
                                             
                                           })
                                         
                                         return(output_tbl)
                                       }

  fitted_models <- model_refit_final_tbl %>%
    dplyr::filter(Train_Test_ID == "01") %>%
    dplyr::select(Combo, Model, Recipe_ID, Model_Fit)
  
  return(list(Model_Predictions = model_refit_final_tbl %>% dplyr::select(-Model_Fit), Model_Fit = fitted_models))
}