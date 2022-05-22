#' Average Models
#' 
#' @param model_refit_tbl individual model predictions
#' @param model_ensemble_tbl ensemble model predictions
#' @param model_train_test_tbl train test splits
#' @param max_model_average max number of models to average
#' @param parallel_processing parallel processing
#' @param num_cores number of cores
#'  
#' @return list with average predictions and fitted models
#' @keywords internal
#' @export
average_models <- function(model_refit_tbl,
                           model_ensemble_tbl = NULL,
                           model_train_test_tbl, 
                           max_model_average = 3, 
                           parallel_processing = NULL, 
                           num_cores = NULL) {
  
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
  
  # get model list
  ind_model_list <- model_refit_tbl %>%
    dplyr::mutate(Combo = ifelse(Combo == "All-Data", "global", "local")) %>%
    tidyr::unite(col = "Model_Name", c("Model", "Recipe_ID", "Combo"), sep = "-") %>%
    dplyr::pull(Model_Name) %>%
    unique()
  
  if(!is.null(model_ensemble_tbl)) {
    ensemble_model_list <- model_ensemble_tbl %>%
      dplyr::mutate(Recipe_ID = "Ensemble-local") %>%
      tidyr::unite(col = "Model_Name", c("Model", "Recipe_ID"), sep = "-") %>%
      dplyr::pull(Model_Name) %>%
      unique()
  } else {
    ensemble_model_list <- NULL
  }
  
  final_model_list <- c(ind_model_list, ensemble_model_list)
  
  # create model combinations list
  model_combinations <- tibble::tibble()
  
  for(number in 2:min(length(final_model_list), max_model_average)) {
    
    temp <- data.frame(gtools::combinations(v=final_model_list, n=length(final_model_list), r=number))
    
    temp <- temp %>% 
      tidyr::unite(Model_Combo, colnames(temp)) %>%
      dplyr::select(Model_Combo) %>%
      tibble::tibble()
    
    model_combinations <- rbind(model_combinations, temp)
  }
  
  # prep prediction data
  train_test_id_list <- model_train_test_tbl %>%
    dplyr::filter(Run_Type != "Ensemble") %>%
    dplyr::pull(Run_ID) %>%
    unique()

  predictions_tbl <- model_refit_tbl %>%
    dplyr::mutate(Model_Suffix = ifelse(Combo == "All-Data", "global", "local")) %>%
    dplyr::select(Model_Suffix, Model, Recipe_ID, Train_Test_ID, Prediction) %>%
    tidyr::unite(col = 'Model_Name', c("Model", "Recipe_ID", "Model_Suffix"), sep= "-", remove = FALSE) %>%
    dplyr::select(-Model_Suffix) %>%
    dplyr::filter(Train_Test_ID %in% train_test_id_list) %>%
    tidyr::unnest(Prediction) %>%
    rbind(
      if(!is.null(model_ensemble_tbl)) {
        model_ensemble_tbl %>%
          dplyr::mutate(Model_Suffix = ifelse(Combo == "All-Data", "global", "local")) %>%
          dplyr::select(Model_Suffix, Model, Recipe_ID, Train_Test_ID, Prediction) %>%
          tidyr::unite(col = 'Model_Name', c("Model", "Recipe_ID", "Model_Suffix"), sep= "-", remove = FALSE) %>%
          dplyr::select(-Model_Suffix) %>%
          dplyr::filter(Train_Test_ID %in% train_test_id_list) %>%
          tidyr::unnest(Prediction)
      } else {
        tibble::tibble()
      }
    )
  
  # model average function
  model_average <- function(model) {
    
    # get list of models to average
    model_list <- strsplit(model, "_")[[1]]
    
    # create model average
    temp <- predictions_tbl %>%
      dplyr::filter(Model_Name %in% model_list) %>%
      dplyr::group_by(Combo, Train_Test_ID, Date) %>%
      dplyr::summarise(Target = mean(Target, na.rm = TRUE), 
                       Forecast = mean(Forecast, na.rm = TRUE)) %>%
      dplyr::mutate(Model = model) %>%
      dplyr::select(Combo, Model, Train_Test_ID, Date, Target, Forecast) %>%
      dplyr::ungroup()
    
    return(temp)
  }
  
  # model_avg_tbl <- submit_fn(predictions_tbl,
  #                            parallel_processing,
  #                            model_combinations %>%
  #                              dplyr::pull(Model_Combo),
  #                            model_average,
  #                            num_cores,
  #                            package_exports = c("tibble", "dplyr", "tidyselect", "stringr", "foreach",'doParallel', 'parallel'),
  #                            function_exports = NULL)
  
  model_avg_tbl <- foreach::foreach(x = unique(predictions_tbl$Combo), 
                                            .combine = 'rbind', 
                                            .packages = packages,
                                            .errorhandling = "stop", 
                                            .verbose = FALSE, 
                                            .inorder = FALSE, 
                                            .multicombine = TRUE, 
                                            .noexport = NULL) %op% {
                                              
                                              combo <- x
                                              
                                              if(!is.null(parallel_processing)) {
                                                
                                                predictions_tbl <- predictions_tbl %>%
                                                  dplyr::filter(Combo == combo)
                                              }
                                              
                                              iter_list <- model_combinations %>%
                                                dplyr::pull(Model_Combo)
                                              
                                              output_tbl <- iter_list %>%
                                                purrr::map_dfr(.f = function(x) {
                                                  
                                                  print(x)
                                                  
                                                  # get list of models to average
                                                  model_list <- strsplit(x, "_")[[1]]
                                                  
                                                  # create model average
                                                  final_tbl <- predictions_tbl %>%
                                                    dplyr::filter(Combo == combo) %>%
                                                    dplyr::filter(Model_Name %in% model_list) %>%
                                                    dplyr::group_by(Combo, Train_Test_ID, Date) %>%
                                                    dplyr::summarise(Target = mean(Target, na.rm = TRUE), 
                                                                     Forecast = mean(Forecast, na.rm = TRUE)) %>%
                                                    dplyr::mutate(Model = x) %>%
                                                    dplyr::select(Combo, Model, Train_Test_ID, Date, Target, Forecast) %>%
                                                    dplyr::ungroup()
                                                  
                                                  return(final_tbl)
                                                })
                                              
                                              return(output_tbl)
                                            }
  
  return(model_avg_tbl)
}