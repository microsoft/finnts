#' Select Best Models and Prep Final Outputs
#' 
#' @param refit_predictions_tbl individual model predictions
#' @param ensemble_predictions_tbl ensemble model predictions
#' @param average_predictions_tbl model average predictions
#' @param model_train_test_tbl train test splits
#' @param parallel_processing parallel processing
#' @param num_cores number of cores
#'  
#' @return tbl with best model flag
#' @keywords internal
#' @export
final_models <- function(run_info, 
                         average_models = TRUE, 
                         max_model_average = 3, 
                         parallel_processing = NULL, 
                         num_cores = NULL) {
  
  # get combos
  combo_list <- list_files(run_info$storage_object, 
                           paste0(run_info$path, "/forecasts/*", hash_data(run_info$experiment_name), '-', 
                                  hash_data(run_info$run_name), "*-single_models.", run_info$data_output)) %>%
    tibble::tibble(Path = .,
                   File = fs::path_file(.)) %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Type"), sep = '-', remove = TRUE) %>%
    dplyr::pull(Combo) %>%
    unique()
  
  # get run splits
  model_train_test_tbl <- read_file(run_info, 
                                    path = paste0('/model_utility/', hash_data(run_info$experiment_name), '-', hash_data(run_info$run_name), 
                                                  '-train_test_split.', run_info$data_output), 
                                    return_type = 'df')
  
  # parallel run info
  par_info <- par_start(parallel_processing = parallel_processing, 
                        num_cores = num_cores, 
                        task_length = length(combo_list))
  
  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator
  
  # submit tasks
  best_model_tbl <- foreach::foreach(x = combo_list, 
                                    .combine = 'rbind', 
                                    .packages = packages,
                                    .errorhandling = "stop", 
                                    .verbose = FALSE, 
                                    .inorder = FALSE, 
                                    .multicombine = TRUE, 
                                    .noexport = NULL) %op% {
                                      
                                      combo <- x
                                      
                                      # get individual and ensemble model predictions
                                      train_test_id_list <- model_train_test_tbl %>%
                                        dplyr::filter(Run_Type != "Ensemble") %>%
                                        dplyr::pull(Run_ID) %>%
                                        unique()
                                      
                                      single_model_tbl <- read_file(run_info, 
                                                                   path = paste0('/forecasts/', hash_data(run_info$experiment_name), '-', hash_data(run_info$run_name), 
                                                                                 '-', combo, '-single_models.', run_info$data_output), 
                                                                   return_type = 'df')
                                      
                                      ensemble_model_tbl <- NULL
                                      try(ensemble_model_tbl <- read_file(run_info, 
                                                                        path = paste0('/forecasts/', hash_data(run_info$experiment_name), '-', hash_data(run_info$run_name), 
                                                                                      '-', combo, '-ensemble_models.', run_info$data_output), 
                                                                        return_type = 'df'), 
                                          silent = TRUE)
                                      
                                      predictions_tbl <- single_model_tbl %>%
                                        dplyr::mutate(Model_Suffix = ifelse(Combo_ID == "All-Data", "global", "local")) %>%
                                        dplyr::select(Combo, Model_Suffix, Model, Recipe_ID, Train_Test_ID, Date, Forecast, Target) %>%
                                        tidyr::unite(col = 'Model_Name', c("Model", "Recipe_ID", "Model_Suffix"), sep= "-", remove = FALSE) %>%
                                        dplyr::select(-Model_Suffix) %>%
                                        dplyr::filter(Train_Test_ID %in% train_test_id_list) %>%
                                        #tidyr::unnest(Prediction) %>%
                                        rbind(
                                          if(!is.null(ensemble_model_tbl)) {
                                            ensemble_model_tbl %>%
                                              dplyr::mutate(Model_Suffix = ifelse(Combo_ID == "All-Data", "global", "local")) %>%
                                              dplyr::select(Combo, Model_Suffix, Model, Recipe_ID, Train_Test_ID, Date, Forecast, Target) %>%
                                              tidyr::unite(col = 'Model_Name', c("Model", "Recipe_ID", "Model_Suffix"), sep= "-", remove = FALSE) %>%
                                              dplyr::select(-Model_Suffix) %>%
                                              dplyr::filter(Train_Test_ID %in% train_test_id_list) #%>%
                                              #tidyr::unnest(Prediction)
                                          } else {
                                            tibble::tibble()
                                          }
                                        )
                                      
                                      # simple model averaging
                                      if(average_models) {
                                        
                                        # get model list
                                        single_model_list <- single_model_tbl %>%
                                          dplyr::mutate(Combo = ifelse(Combo == "All-Data", "global", "local")) %>%
                                          tidyr::unite(col = "Model_Name", c("Model", "Recipe_ID", "Combo"), sep = "-") %>%
                                          dplyr::pull(Model_Name) %>%
                                          unique()

                                        if(!is.null(ensemble_model_tbl)) {
                                          ensemble_model_list <- ensemble_model_tbl %>%
                                            dplyr::mutate(Recipe_ID = "Ensemble-local") %>%
                                            tidyr::unite(col = "Model_Name", c("Model", "Recipe_ID"), sep = "-") %>%
                                            dplyr::pull(Model_Name) %>%
                                            unique()
                                        } else {
                                          ensemble_model_list <- NULL
                                        }
                                        
                                        final_model_list <- c(single_model_list, ensemble_model_list)
 
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
                                        
                                        iter_list <- model_combinations %>%
                                          dplyr::pull(Model_Combo)

                                        averages_tbl <- foreach::foreach(x = iter_list, 
                                                                         .combine = 'rbind', 
                                                                         .packages = NULL,
                                                                         .errorhandling = "stop", 
                                                                         .verbose = FALSE, 
                                                                         .inorder = FALSE, 
                                                                         .multicombine = TRUE, 
                                                                         .noexport = NULL) %do% {
                                                                           
                                                                           # get list of models to average
                                                                           model_list <- strsplit(x, "_")[[1]]
                                                                           
                                                                           # create model average
                                                                           final_tbl <- predictions_tbl %>%
                                                                             #dplyr::filter(Combo == combo) %>%
                                                                             dplyr::filter(Model_Name %in% model_list) %>%
                                                                             dplyr::group_by(Combo, Train_Test_ID, Date) %>%
                                                                             dplyr::summarise(Target = mean(Target, na.rm = TRUE), 
                                                                                              Forecast = mean(Forecast, na.rm = TRUE)) %>%
                                                                             dplyr::mutate(Model_Name = x) %>%
                                                                             dplyr::select(Combo, Model_Name, Train_Test_ID, Date, Target, Forecast) %>%
                                                                             dplyr::ungroup()
                                                                           
                                                                           return(final_tbl)
                                                                         }
                                      } else {
                                        averages_tbl <- NULL
                                      }
                                      
                                      # choose best model
                                      final_predictions_tbl <- predictions_tbl %>%
                                        dplyr::select(Combo, Model_Name, Train_Test_ID, Date, Forecast, Target) %>%
                                        rbind(averages_tbl)
                                      
                                      back_test_mape <- final_predictions_tbl %>%
                                        dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID),
                                                      Target = ifelse(Target == 0, 0.1, Target)) %>%
                                        dplyr::filter(Train_Test_ID != 1) %>%
                                        dplyr::mutate(MAPE = round(abs((Forecast - Target) / Target), digits = 4))

                                      best_model_tbl <- back_test_mape %>%
                                        dplyr::group_by(Model_Name, Combo) %>%
                                        dplyr::mutate(Combo_Total = sum(abs(Target), na.rm = TRUE),
                                                      weighted_MAPE = (abs(Target)/Combo_Total)*MAPE) %>%
                                        dplyr::summarise(Rolling_MAPE = sum(weighted_MAPE, na.rm=TRUE)) %>%
                                        dplyr::arrange(Rolling_MAPE) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::group_by(Combo) %>%
                                        dplyr::slice(1) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::mutate(Best_Model = "Yes") %>%
                                        dplyr::select(Combo, Model_Name, Best_Model)

                                      back_test_mape_final <- back_test_mape %>%
                                        dplyr::left_join(best_model_tbl) %>%
                                        dplyr::mutate(Best_Model = ifelse(!is.na(Best_Model), "Yes", "No"),
                                                      Train_Test_ID = Train_Test_ID -1) %>%
                                        dplyr::rename(Back_Test_Scenario = Train_Test_ID,
                                                      Model = Model_Name) %>%
                                        dplyr::group_by(Combo, Model, Back_Test_Scenario) %>%
                                        dplyr::mutate(Horizon = dplyr::row_number()) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::select(Combo, Model, Back_Test_Scenario, Horizon, Date, Forecast, Target, MAPE, Best_Model)
                                      
                                      return(back_test_mape_final)
                                    }
  
  # clean up any parallel run process
  par_end(cl)
  
  return(best_model_tbl)
 
  # # get model predictions
  # train_test_id_list <- model_train_test_tbl %>%
  #   dplyr::filter(Run_Type != "Ensemble") %>%
  #   dplyr::pull(Run_ID) %>%
  #   unique()
  # 
  # predictions_tbl <- refit_predictions_tbl %>%
  #   dplyr::mutate(Model_Suffix = ifelse(Combo == "All-Data", "global", "local")) %>%
  #   dplyr::select(Model, Recipe_ID, Model_Suffix, Train_Test_ID, Prediction) %>%
  #   tidyr::unite(col = 'Model_Name', c("Model", "Recipe_ID", "Model_Suffix"), sep= "-", remove = FALSE) %>%
  #   dplyr::select(-Model_Suffix, -Model, -Recipe_ID) %>%
  #   dplyr::filter(Train_Test_ID %in% train_test_id_list) %>%
  #   tidyr::unnest(Prediction) %>%
  #   rbind(
  #     if(!is.null(ensemble_predictions_tbl)) {
  #       ensemble_predictions_tbl %>%
  #         dplyr::mutate(Model_Suffix = ifelse(Combo == "All-Data", "global", "local")) %>%
  #         dplyr::select(Model, Recipe_ID, Model_Suffix, Train_Test_ID, Prediction) %>%
  #         tidyr::unite(col = 'Model_Name', c("Model", "Recipe_ID", "Model_Suffix"), sep= "-", remove = FALSE) %>%
  #         dplyr::select(-Model_Suffix, -Model, -Recipe_ID) %>%
  #         dplyr::filter(Train_Test_ID %in% train_test_id_list) %>%
  #         tidyr::unnest(Prediction)
  #     } else {
  #       tibble::tibble()
  #     }
  #   ) %>%
  #   rbind(
  #     if(!is.null(average_predictions_tbl)) {
  #       average_predictions_tbl %>%
  #         dplyr::rename(Model_Name = Model)
  #     } else {
  #       tibble::tibble()
  #     }
  #   )
  # 
  # # calculate model accuracy
  # back_test_mape <- predictions_tbl %>%
  #   dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID), 
  #                 Target = ifelse(Target == 0, 0.1, Target)) %>%
  #   dplyr::filter(Train_Test_ID != 1) %>%
  #   dplyr::mutate(MAPE = round(abs((Forecast - Target) / Target), digits = 4)) 
  # 
  # best_model_tbl <- back_test_mape %>%
  #   dplyr::group_by(Model_Name, Combo) %>%
  #   dplyr::mutate(Combo_Total = sum(abs(Target), na.rm = TRUE), 
  #                 weighted_MAPE = (abs(Target)/Combo_Total)*MAPE) %>%
  #   dplyr::summarise(Rolling_MAPE = sum(weighted_MAPE, na.rm=TRUE)) %>%
  #   dplyr::arrange(Rolling_MAPE) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(Combo) %>% 
  #   dplyr::slice(1) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(Best_Model = "Yes") %>%
  #   dplyr::select(Combo, Model_Name, Best_Model)
  # 
  # back_test_mape_final <- back_test_mape %>%
  #   dplyr::left_join(best_model_tbl) %>%
  #   dplyr::mutate(Best_Model = ifelse(!is.na(Best_Model), "Yes", "No"), 
  #                 Train_Test_ID = Train_Test_ID -1) %>%
  #   dplyr::rename(Back_Test_Scenario = Train_Test_ID, 
  #                 Model = Model_Name) %>%
  #   dplyr::group_by(Combo, Model, Back_Test_Scenario) %>%
  #   dplyr::mutate(Horizon = dplyr::row_number()) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(Combo, Model, Back_Test_Scenario, Horizon, Date, Forecast, Target, MAPE, Best_Model)
  # 
  # return(back_test_mape_final)
}