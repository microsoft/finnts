#' Select Best Models and Prep Final Outputs
#' 
#' @param run_info run info
#' @param average_models average models
#' @param max_model_average max model average
#' @param parallel_processing parallel processing
#' @param num_cores number of cores
#'  
#' @return 
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
    dplyr::filter(Combo != hash_data("All-Data")) %>%
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
                                        dplyr::pull(Train_Test_ID) %>%
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
                                        #dplyr::mutate(Model_Suffix = ifelse(Combo_ID == "All-Data", "global", "local")) %>%
                                        dplyr::select(Combo, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Date, Forecast, Target) %>%
                                        #tidyr::unite(col = 'Model_Name', c("Model", "Recipe_ID", "Model_Suffix"), sep= "--", remove = FALSE) %>%
                                        #dplyr::select(-Model_Suffix) %>%
                                        dplyr::filter(Train_Test_ID %in% train_test_id_list) %>%
                                        #tidyr::unnest(Prediction) %>%
                                        rbind(
                                          if(!is.null(ensemble_model_tbl)) {
                                            ensemble_model_tbl %>%
                                              #dplyr::mutate(Model_Suffix = ifelse(Combo_ID == "All-Data", "global", "local")) %>%
                                              dplyr::select(Combo, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Date, Forecast, Target) %>%
                                              #tidyr::unite(col = 'Model_Name', c("Model", "Recipe_ID", "Model_Suffix"), sep= "--", remove = FALSE) %>%
                                              #dplyr::select(-Model_Suffix) %>%
                                              dplyr::filter(Train_Test_ID %in% train_test_id_list) #%>%
                                              #tidyr::unnest(Prediction)
                                          } else {
                                            tibble::tibble()
                                          }
                                        )
                                      
                                      # get model list
                                      single_model_list <- single_model_tbl %>%
                                        dplyr::mutate(Combo = ifelse(Combo == "All-Data", "global", "local")) %>%
                                        #tidyr::unite(col = "Model_Name", c("Model", "Recipe_ID", "Combo"), sep = "--") %>%
                                        dplyr::pull(Model_ID) %>%
                                        unique()
                                      
                                      if(!is.null(ensemble_model_tbl)) {
                                        ensemble_model_list <- ensemble_model_tbl %>%
                                          #dplyr::mutate(Recipe_ID = "ensemble-local") %>%
                                          #tidyr::unite(col = "Model_Name", c("Model", "Recipe_ID"), sep = "--") %>%
                                          dplyr::pull(Model_ID) %>%
                                          unique()
                                      } else {
                                        ensemble_model_list <- NULL
                                      }
                                      
                                      final_model_list <- c(single_model_list, ensemble_model_list)
                                      
                                      # simple model averaging
                                      if(average_models) {

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
                                                                             dplyr::filter(Model_ID %in% model_list) %>%
                                                                             dplyr::group_by(Combo, Train_Test_ID, Date) %>%
                                                                             dplyr::summarise(Target = mean(Target, na.rm = TRUE), 
                                                                                              Forecast = mean(Forecast, na.rm = TRUE)) %>%
                                                                             dplyr::mutate(Model_ID = x) %>%
                                                                             dplyr::select(Combo, Model_ID, Train_Test_ID, Date, Target, Forecast) %>%
                                                                             dplyr::ungroup()
                                                                           
                                                                           return(final_tbl)
                                                                         }
                                      } else {
                                        averages_tbl <- NULL
                                      }
                                      
                                      # choose best model
                                      final_predictions_tbl <- predictions_tbl %>%
                                        dplyr::select(Combo, Model_ID, Train_Test_ID, Date, Forecast, Target) %>%
                                        rbind(averages_tbl)
                                      
                                      back_test_mape <- final_predictions_tbl %>%
                                        dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID),
                                                      Target = ifelse(Target == 0, 0.1, Target)) %>%
                                        dplyr::filter(Train_Test_ID != 1) %>%
                                        dplyr::mutate(MAPE = round(abs((Forecast - Target) / Target), digits = 4))

                                      best_model_tbl <- back_test_mape %>%
                                        dplyr::group_by(Model_ID, Combo) %>%
                                        dplyr::mutate(Combo_Total = sum(abs(Target), na.rm = TRUE),
                                                      weighted_MAPE = (abs(Target)/Combo_Total)*MAPE) %>%
                                        dplyr::summarise(Rolling_MAPE = sum(weighted_MAPE, na.rm=TRUE)) %>%
                                        dplyr::arrange(Rolling_MAPE) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::group_by(Combo) %>%
                                        dplyr::slice(1) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::mutate(Best_Model = "Yes") %>%
                                        dplyr::select(Combo, Model_ID, Best_Model)

                                      back_test_mape_final <- back_test_mape %>%
                                        dplyr::left_join(best_model_tbl, 
                                                         by = c("Combo", "Model_ID")) %>%
                                        dplyr::mutate(Best_Model = ifelse(!is.na(Best_Model), "Yes", "No"),
                                                      Train_Test_ID = Train_Test_ID -1) %>%
                                        dplyr::rename(Back_Test_Scenario = Train_Test_ID) %>%
                                        dplyr::group_by(Combo, Model_ID, Back_Test_Scenario) %>%
                                        dplyr::mutate(Horizon = dplyr::row_number()) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::select(Combo, Model_ID, Back_Test_Scenario, Horizon, Date, Forecast, Target, MAPE, Best_Model)

                                      best_model_final_tbl <- tibble::tibble(Model_ID = stringr::str_split(best_model_tbl$Model_ID, "_")[[1]]) %>%
                                        dplyr::mutate(Combo = best_model_tbl$Combo, 
                                                      Best_Model = "Yes") %>%
                                        tidyr::separate(col = "Model_ID", into = c("Model_Name", "Recipe_ID", "Model_Type"), sep = "--", remove = FALSE)

                                      # if a simple model average is the most accurate store the results
                                      if(nrow(best_model_final_tbl) > 1) {
                                        
                                        model_avg_final_tbl <- final_predictions_tbl %>%
                                          dplyr::right_join(best_model_tbl, 
                                                            by = c("Combo", "Model_ID")) %>%
                                          dplyr::mutate(Combo_ID = Combo, 
                                                        Model_Name = 'NA', 
                                                        Model_Type = 'local',
                                                        Recipe_ID = "simple_average", 
                                                        Hyperparameter_ID = 'NA', 
                                                        Best_Model = "Yes") %>%
                                          dplyr::select(Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Hyperparameter_ID,
                                                        Best_Model, Combo, Date, Target, Forecast)
                                        
                                        single_model_final_tbl <- single_model_tbl %>%
                                          dplyr::mutate(Best_Model = "No") %>%
                                          dplyr::select(Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Hyperparameter_ID,
                                                        Best_Model, Combo, Date, Target, Forecast)
                                        
                                        if(!is.null(ensemble_model_tbl)) {
                                          ensemble_model_final_tbl <- ensemble_model_tbl %>%
                                            dplyr::mutate(Best_Model = "No") %>%
                                            dplyr::select(Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Hyperparameter_ID,
                                                          Best_Model, Combo, Date, Target, Forecast)
                                        }
                                        
                                        # write outputs
                                        write_data(x = model_avg_final_tbl,
                                                   combo = unique(model_avg_final_tbl$Combo_ID),
                                                   run_info = run_info,
                                                   output_type = 'data',
                                                   folder = "forecasts",
                                                   suffix = '-average_models')
                                        
                                        write_data(x = single_model_final_tbl,
                                                   combo = unique(single_model_final_tbl$Combo_ID),
                                                   run_info = run_info,
                                                   output_type = 'data',
                                                   folder = "forecasts",
                                                   suffix = '-single_models')
                                        
                                        if(!is.null(ensemble_model_tbl)) {
                                          write_data(x = ensemble_model_final_tbl,
                                                     combo = unique(ensemble_model_final_tbl$Combo_ID),
                                                     run_info = run_info,
                                                     output_type = 'data',
                                                     folder = "forecasts",
                                                     suffix = '-ensemble_models')
                                        } 
                                      } else { # choose the most accurate individual model and write outputs
                                        
                                        final_model_tbl <- tibble::tibble(Model_ID = final_model_list) %>%
                                          dplyr::left_join(
                                            best_model_final_tbl %>%
                                              dplyr::select(Model_ID, Best_Model), 
                                            by = "Model_ID"
                                          ) %>%
                                          dplyr::mutate(Best_Model = ifelse(!is.na(Best_Model), "Yes", "No"))
                                        
                                        single_model_final_tbl <- single_model_tbl %>%
                                          dplyr::left_join(final_model_tbl, 
                                                           by = "Model_ID") %>%
                                          dplyr::select(Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Hyperparameter_ID,
                                                        Best_Model, Combo, Date, Target, Forecast)
                                        
                                        if(!is.null(ensemble_model_tbl)) {
                                          ensemble_model_final_tbl <- ensemble_model_tbl %>%
                                            dplyr::left_join(final_model_tbl, 
                                                             by = "Model_ID") %>%
                                            dplyr::select(Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Hyperparameter_ID,
                                                          Best_Model, Combo, Date, Target, Forecast)
                                        }
                                        
                                        # write outputs
                                        write_data(x = single_model_final_tbl,
                                                   combo = unique(single_model_final_tbl$Combo_ID),
                                                   run_info = run_info,
                                                   output_type = 'data',
                                                   folder = "forecasts",
                                                   suffix = '-single_models')
                                        
                                        if(!is.null(ensemble_model_tbl)) {
                                          write_data(x = ensemble_model_final_tbl,
                                                     combo = unique(ensemble_model_final_tbl$Combo_ID),
                                                     run_info = run_info,
                                                     output_type = 'data',
                                                     folder = "forecasts",
                                                     suffix = '-ensemble_models')
                                        }
                                      }
                                      
                                      return(tibble::tibble())
                                    }
  
  # clean up any parallel run process
  par_end(cl)
  
  return(cli::cli_alert_success("Forecast Finished"))
}