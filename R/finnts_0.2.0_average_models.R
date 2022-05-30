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
  
  combo_list <- unique(predictions_tbl$Combo)
  
  # parallel run info
  par_info <- par_start(parallel_processing = parallel_processing, 
                        num_cores = num_cores, 
                        task_length = length(combo_list))
  
  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  # submit tasks
  model_avg_tbl <- foreach::foreach(x = combo_list, 
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
                                              
                                              output_tbl <- foreach::foreach(x = iter_list, 
                                                                             .combine = 'rbind', 
                                                                             .packages = NULL,
                                                                             .errorhandling = "remove", 
                                                                             .verbose = FALSE, 
                                                                             .inorder = FALSE, 
                                                                             .multicombine = TRUE, 
                                                                             .noexport = NULL) %do% {
                                                                               
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
                                                                             }
                                              
                                              return(output_tbl)
                                            }
  
  # clean up any parallel run process
  par_end(cl)
  
  return(model_avg_tbl)
}