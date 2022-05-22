#' Ensemble Models
#' 
#' @param model_tune_tbl hyperparameter tuning predictions
#' @param model_refit_tbl individual model predictions
#' @param model_train_test_tbl model train test split table
#' @param date_type date type
#' @param num_hyperparameters number of hyperparameters
#' @param parallel_processing parallel processing
#' @param num_cores number of cores
#' @param seed seed number
#'  
#' @return list with ensemble predictions and fitted models
#' @keywords internal
#' @export
ensemble_models <- function(model_tune_tbl, 
                            model_refit_tbl, 
                            model_train_test_tbl,
                            date_type, 
                            num_hyperparameters = 5, 
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
    
    sparklyr::registerDoSpark(sc, parallelism = length(combo_list))
    
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
  
  
  # get individual prediction data
  initial_results_tbl <- model_tune_tbl %>%
    dplyr::select(Combo, Model, Recipe_ID, Prediction) %>%
    dplyr::rename(Combo_Key = Combo) %>%
    tidyr::unnest(Prediction) %>%
    rbind(
      model_refit_tbl %>%
        dplyr::select(Combo, Model, Recipe_ID, Train_Test_ID, Prediction) %>%
        dplyr::rename(Combo_Key = Combo) %>%
        tidyr::unnest(Prediction)
    ) 
  
  combo_list <- unique(initial_results_tbl$Combo)
  
  prep_ensemble_tbl <- foreach::foreach(x = combo_list, 
                                            .combine = 'rbind', 
                                            .packages = packages,
                                            .errorhandling = "stop", 
                                            .verbose = FALSE, 
                                            .inorder = FALSE, 
                                            .multicombine = TRUE, 
                                            .noexport = NULL) %op% {
                                              
                                              if(!is.null(parallel_processing)) {
                                                initial_results_tbl <- initial_results_tbl %>%
                                                  dplyr::filter(Combo == x)
                                              }
                                              
                                              output_tbl <- initial_results_tbl %>%
                                                dplyr::filter(Combo == x) %>%
                                                dplyr::mutate(Suffix = ifelse(Combo_Key == "All-Data", "Global", "Local")) %>%
                                                tidyr::unite(col= "Model_Key", 
                                                             c("Model", "Recipe_ID", "Suffix"),
                                                             sep="-",
                                                             remove=F) %>%
                                                tidyr::pivot_wider(names_from = Model_Key, values_from = Forecast, 
                                                                   id_cols = c("Combo", "Date", "Train_Test_ID", "Target"), values_fill = 0)
                                              
                                              return(output_tbl)
                                            }

  # ensemble models to run
  refit_models <- unique(model_refit_tbl$Model)
  
  ensemble_model_list <- refit_models[refit_models %in% c("cubist", "glmnet", "sv-poly", "svm-rbf", "xgboost")]
  
  if(length(ensemble_model_list) < 1) {
    stop("no ensemble models chosen to run")
  }
  
  model_workflow_tbl <- tibble::tibble()
  
  for(model in ensemble_model_list) {
    
    avail_arg_list <- list('train_data' = prep_ensemble_tbl %>% dplyr::select(-Train_Test_ID),
                           'model_type' = "ensemble",
                           'pca' = FALSE)
    
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
    
    workflow_tbl <- tibble::tibble(Model_Name = model,
                                   Model_Workflow = list(model_workflow))
    
    model_workflow_tbl <- rbind(model_workflow_tbl, workflow_tbl)
  }
  
  # get hyperparameters
  hyperparameters_tbl <- tibble::tibble()
  
  for(x in model_workflow_tbl %>% dplyr::group_split(dplyr::row_number(), .keep = FALSE)) {
    
    model <- x %>%
      dplyr::pull(Model_Name)
    
    temp_tbl <- model_workflow_tbl %>%
      dplyr::filter(Model_Name == model)
    
    model_workflow <- temp_tbl$Model_Workflow[[1]]
    
    model_spec <- model_workflow %>%
      workflows::extract_spec_parsnip()
    
    recipe_features <- prep_ensemble_tbl
    
    if(model=="svm-rbf") {
      parameters <- model_spec %>%
        workflows::extract_parameter_set_dials()
    } else {
      parameters <- model_spec %>%
        workflows::extract_parameter_set_dials() %>%
        dials::finalize(recipe_features, force = FALSE)
    }
    
    grid <- dials::grid_latin_hypercube(parameters, size = num_hyperparameters)
    
    hyperparameters_temp <- grid %>%
      dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
      purrr::map_df(tidyr::nest, data=tidyselect::everything()) %>%
      dplyr::rename(Hyperparameters = data) %>%
      tibble::rowid_to_column("Hyperparameter_Combo") %>%
      dplyr::mutate(Model = model)
    
    hyperparameters_tbl <- rbind(hyperparameters_tbl, hyperparameters_temp)
  }
  
  # fit models by hyperparameter
  initial_tuning_tbl <- foreach::foreach(x = combo_list, 
                                         .combine = 'rbind', 
                                         .packages = packages,
                                         .errorhandling = "stop", 
                                         .verbose = FALSE, 
                                         .inorder = FALSE, 
                                         .multicombine = TRUE, 
                                         .noexport = NULL) %op% {
                                           
                                           if(!is.null(parallel_processing)) {

                                             prep_ensemble_tbl <- prep_ensemble_tbl %>%
                                               dplyr::filter(Combo == x)
                                           }
                                           
                                           iter_list <- model_train_test_tbl %>%
                                             dplyr::mutate(Combo = x) %>%
                                             dplyr::rename(Train_Test_ID = Run_ID) %>%
                                             dplyr::filter(Run_Type == "Validation") %>%
                                             dplyr::select(Combo, Train_Test_ID) %>%
                                             dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
                                             purrr::map(.f = function(x) {
                                               hyperparameters_tbl %>%
                                                 dplyr::select(Hyperparameter_Combo, Model) %>%
                                                 dplyr::rename(Hyperparameter_ID = Hyperparameter_Combo) %>%
                                                 dplyr::mutate(Combo = x$Combo, 
                                                               Train_Test_ID = x$Train_Test_ID)
                                             }) %>%
                                             dplyr::bind_rows() %>%
                                             dplyr::select(Combo, Model, Train_Test_ID, Hyperparameter_ID)

                                           output_tbl <- iter_list %>%
                                             dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
                                             purrr::map_dfr(.f = function(x) {
                                               
                                               print(x)
                                               
                                               # run input values
                                               param_combo <- x %>%
                                                 dplyr::pull(Hyperparameter_ID)
                                               
                                               model <- x %>%
                                                 dplyr::pull(Model)
                                               
                                               data_split <- x %>%
                                                 dplyr::pull(Train_Test_ID)
                                               
                                               combo <- x %>%
                                                 dplyr::pull(Combo)
                                               
                                               train_end_date <- model_train_test_tbl %>%
                                                 dplyr::filter(Run_ID == data_split) %>%
                                                 dplyr::pull(Train_End)
                                               
                                               test_end_date <- model_train_test_tbl %>%
                                                 dplyr::filter(Run_ID == data_split) %>%
                                                 dplyr::pull(Test_End)
                                               
                                               # get train/test data
                                               full_data <- prep_ensemble_tbl %>%
                                                 dplyr::filter(Combo == combo) %>%
                                                 dplyr::mutate(Date_index.num = 0)
                                               
                                               training <- full_data %>% 
                                                 dplyr::filter(Date <= train_end_date) %>%
                                                 dplyr::select(-Train_Test_ID)
                                               
                                               testing <- full_data %>% 
                                                 dplyr::filter(Date > train_end_date, 
                                                               Date <= test_end_date, 
                                                               Train_Test_ID == data_split)
                                               
                                               # get workflow
                                               workflow <- model_workflow_tbl %>%
                                                 dplyr::filter(Model_Name == model)
                                               
                                               workflow_final <- workflow$Model_Workflow[[1]]
                                               
                                               # get hyperparameters
                                               hyperparameters <- hyperparameters_tbl %>%
                                                 dplyr::filter(Model == model, 
                                                               Hyperparameter_Combo == param_combo) %>%
                                                 dplyr::select(Hyperparameters) %>%
                                                 tidyr::unnest(Hyperparameters)
                                               
                                               # fit model
                                               set.seed(seed)
                                               
                                               model_fit <- workflow_final %>%
                                                 tune::finalize_workflow(parameters = hyperparameters) %>%
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
                                                 Train_Test_ID = data_split, 
                                                 Hyperparameter_ID = param_combo, 
                                                 Model_Fit = list(model_fit), 
                                                 Prediction = list(model_prediction)
                                               )
                                               
                                               return(final_tbl)
                                             })
                                           
                                           return(output_tbl)
                                         }
  
  final_tuning_tbl <- foreach::foreach(x = combo_list, 
                                         .combine = 'rbind', 
                                         .packages = packages,
                                         .errorhandling = "stop", 
                                         .verbose = FALSE, 
                                         .inorder = FALSE, 
                                         .multicombine = TRUE, 
                                         .noexport = NULL) %op% {
                                           
                                           if(!is.null(parallel_processing)) {
                                             
                                             initial_tuning_tbl <- initial_tuning_tbl %>%
                                               dplyr::filter(Combo == x)
                                           }
                                           
                                           iter_list <- model_train_test_tbl %>%
                                             dplyr::mutate(Combo = x) %>%
                                             dplyr::rename(Train_Test_ID = Run_ID) %>%
                                             dplyr::filter(Run_Type == "Validation") %>%
                                             dplyr::select(Combo, Train_Test_ID) %>%
                                             dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
                                             purrr::map(.f = function(x) {
                                               hyperparameters_tbl %>%
                                                 dplyr::select(Hyperparameter_Combo, Model) %>%
                                                 dplyr::rename(Hyperparameter_ID = Hyperparameter_Combo) %>%
                                                 dplyr::mutate(Combo = x$Combo, 
                                                               Train_Test_ID = x$Train_Test_ID)
                                             }) %>%
                                             dplyr::bind_rows() %>%
                                             dplyr::select(Combo, Model) %>%
                                             dplyr::distinct()
                                           
                                           output_tbl <- iter_list %>%
                                             dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
                                             purrr::map_dfr(.f = function(x) {
                                               
                                               print(x)
                                               
                                               combo <- x %>%
                                                 dplyr::pull(Combo)
                                               
                                               model <- x %>%
                                                 dplyr::pull(Model)
                                               
                                               test_tbl <- initial_tuning_tbl %>%
                                                 dplyr::filter(Combo == combo, 
                                                               Model == model) %>%
                                                 dplyr::select(Model, Hyperparameter_ID, Train_Test_ID, Prediction, Model_Fit)
                                               
                                               best_param <- test_tbl %>%
                                                 dplyr::select(-Model_Fit) %>%
                                                 tidyr::unnest(Prediction) %>%
                                                 dplyr::mutate(Combo = combo) %>%
                                                 dplyr::group_by(Combo, Model, Hyperparameter_ID) %>%
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
                                                                     Hyperparameter_ID = best_param, 
                                                                     Model_Fit = list(best_model_fit), 
                                                                     Prediction = list(final_predictions)))
                                             })
                                           
                                           return(output_tbl)
                                         }

  # refit ensemble models
  model_refit_final_tbl <- foreach::foreach(x = combo_list, 
                                       .combine = 'rbind', 
                                       .packages = packages,
                                       .errorhandling = "stop", 
                                       .verbose = FALSE, 
                                       .inorder = FALSE, 
                                       .multicombine = TRUE, 
                                       .noexport = NULL) %op% {
                                         
                                         combo <- x

                                         if(!is.null(parallel_processing)) {

                                           final_tuning_tbl <- final_tuning_tbl %>%
                                             dplyr::filter(Combo == combo)
                                         }
                                         
                                         iter_list <- model_train_test_tbl %>%
                                           dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test")) %>%
                                           dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
                                           purrr::map(.f = function(x) {
                                             final_tuning_tbl %>%
                                               dplyr::filter(Combo == combo) %>%
                                               dplyr::mutate(Run_Type = x %>% dplyr::pull(Run_Type), 
                                                             Run_ID = x %>% dplyr::pull(Run_ID), 
                                                             Train_End = x %>% dplyr::pull(Train_End), 
                                                             Test_End = x %>% dplyr::pull(Test_End)) %>%
                                               dplyr::select(-Model_Fit, -Prediction)}) %>%
                                           dplyr::bind_rows()

                                         output_tbl <- iter_list %>%
                                           dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
                                           purrr::map_dfr(.f = function(x) {
                                             
                                             print(x)
                                             
                                             combo <- x %>%
                                               dplyr::pull(Combo)
                                             
                                             model <- x %>%
                                               dplyr::pull(Model)
                                             
                                             model_fit <- final_tuning_tbl %>%
                                               dplyr::filter(Model == model, 
                                                             Combo == combo)
                                             
                                             model_fit <- model_fit$Model_Fit[[1]]
                                             
                                             run_type <- x %>%
                                               dplyr::pull(Run_Type)
                                             
                                             run_id <- x %>%
                                               dplyr::pull(Run_ID)
                                             
                                             train_end <- x %>%
                                               dplyr::pull(Train_End)
                                             
                                             test_end <- x %>%
                                               dplyr::pull(Test_End)
                                             
                                             full_data <- prep_ensemble_tbl %>%
                                               dplyr::filter(Combo == combo) %>%
                                               dplyr::mutate(Date_index.num = 0)
                                             
                                             training <- full_data %>% 
                                               dplyr::filter(Date <= train_end) %>%
                                               dplyr::select(-Train_Test_ID)
                                             
                                             testing <- full_data %>% 
                                               dplyr::filter(Date > train_end, 
                                                             Date <= test_end, 
                                                             Train_Test_ID == run_id)
                                             
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
                                               Recipe_ID = "Ensemble",
                                               Train_Test_ID = run_id,
                                               Model_Fit = list(model_fit), 
                                               Prediction = list(model_prediction)
                                             )
                                             
                                             return(final_tbl)
                                           })
                                         
                                         return(output_tbl)
                                       }
  
  #get final combined results and return final fitted models
  final_model_fit_tbl <- model_refit_final_tbl %>%
    dplyr::filter(Train_Test_ID == "01") %>%
    dplyr::select(Combo, Model, Recipe_ID, Model_Fit)
  
  final_ensemble_results_tbl <- model_refit_final_tbl %>%
    dplyr::select(-Model_Fit)
  
  return(list(Model_Predictions = final_ensemble_results_tbl, Model_Fit = final_model_fit_tbl))
}