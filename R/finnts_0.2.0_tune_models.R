
#' Tune model hyperparameters
#' 
#' @param run_info
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
tune_models <- function(run_info, 
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
    combo_temp <- list_files(run_info$storage_object, 
                             paste0(run_info$path, "/prep_data/*", hash_data(run_info$experiment_name), '-', 
                                    hash_data(run_info$run_name), "*.", run_info$data_output)) %>%
      tibble::tibble(Path = .,
                     File = fs::path_file(.)) %>%
      tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Recipe"), sep = '-', remove = TRUE) %>%
      dplyr::pull(Combo) %>%
      unique()
    
    combo_list <- c(combo_list, combo_temp)
  }
  
  if(run_global_models & (inherits(parallel_processing, "NULL") || parallel_processing == 'local_machine')) {
    combo_list <- c(combo_list, "All-Data")
  }
  print(combo_list)
  stop('stop')
  # get model utility info
  model_train_test_tbl <- read_file(run_info, 
                                    path = paste0('/model_utility/', hash_data(run_info$experiment_name), '-', hash_data(run_info$run_name), 
                                                  '-train_test_split.', run_info$data_output), 
                                    return_type = 'df')

  model_workflow_tbl <- read_file(run_info, 
                                  path = paste0('/model_utility/', hash_data(run_info$experiment_name), '-', hash_data(run_info$run_name), 
                                                '-model_workflows.', run_info$object_output), 
                                  return_type = 'df')
  
  model_hyperparameter_tbl <- read_file(run_info, 
                                        path = paste0('/model_utility/', hash_data(run_info$experiment_name), '-', hash_data(run_info$run_name), 
                                                      '-model_hyperparameters.', run_info$object_output), 
                                        return_type = 'df')
  
  # fix errors when submitting in parallel on local machine
  list_files <- get('list_files')
  get_recipe_data <- get('get_recipe_data')
  run_info <- run_info
  
  # parallel run info
  par_info <- par_start(parallel_processing = parallel_processing, 
                        num_cores = num_cores, 
                        task_length = length(combo_list))
  
  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  # submit tasks
  initial_tuning_tbl <- foreach::foreach(x = combo_list, 
                                         .combine = 'rbind', 
                                         .packages = packages,
                                         .errorhandling = "stop", 
                                         .verbose = FALSE, 
                                         .inorder = FALSE, 
                                         .multicombine = TRUE, 
                                         .export = c("list_files"),
                                         .noexport = NULL) %op% {

                                           model_recipe_tbl <- get_recipe_data(run_info, 
                                                                               combo = x)

                                           iter_list <- model_train_test_tbl %>%
                                             dplyr::mutate(Combo = x) %>%
                                             dplyr::rename(Train_Test_ID = Run_ID) %>%
                                             dplyr::filter(Run_Type == "Validation") %>%
                                             dplyr::select(Combo, Train_Test_ID) %>%
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

                                           output_tbl <- foreach::foreach(x = iter_list %>%
                                                                            dplyr::group_split(dplyr::row_number(), .keep = FALSE), 
                                                                          .combine = 'rbind', 
                                                                          .packages = NULL,
                                                                          .errorhandling = "stop", 
                                                                          .verbose = FALSE, 
                                                                          .inorder = FALSE, 
                                                                          .multicombine = TRUE, 
                                                                          #.export = c("get_recipe_data", "list_files"),
                                                                          .noexport = NULL) %do% {
                                                                            
                                                                            #print(x)
                                                                            
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
                                                                              
                                                                              # full_data <- full_data %>%
                                                                              #   dplyr::filter(Combo == combo)
                                                                              
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
                                                                              Prediction = list(model_prediction)
                                                                            )
                                                                            
                                                                            return(final_tbl)
                                                                          }
                                           
                                           test_tbl <- output_tbl %>%
                                             # dplyr::filter(Combo == combo,
                                             #               Recipe_ID == recipe,
                                             #               Model == model) %>%
                                             dplyr::select(Model, Recipe_ID, Hyperparameter_ID, Train_Test_ID, Prediction)

                                           best_param <- output_tbl %>%
                                             dplyr::rename(Combo_ID = Combo) %>%
                                             tidyr::unnest(Prediction) %>%
                                             #dplyr::mutate(Combo = combo) %>%
                                             dplyr::mutate(SE = (Target-Forecast)^2) %>%
                                             dplyr::group_by(Combo_ID, Model, Recipe_ID, Hyperparameter_ID) %>%
                                             #dplyr::mutate(SE = (Target-Forecast)^2) %>%
                                             dplyr::summarise(RMSE = sqrt(mean(SE, na.rm = TRUE))) %>%
                                             dplyr::arrange(RMSE) %>%
                                             dplyr::slice(1) %>%
                                             dplyr::ungroup()

                                           final_predictions <- test_tbl %>%
                                             #dplyr::filter(Hyperparameter_ID == best_param) %>%
                                             dplyr::right_join(best_param) %>%
                                             tidyr::unnest(Prediction) %>%
                                             dplyr::mutate(Combo_ID = ifelse(Combo_ID == "All-Data", "All-Data", Combo)) %>%
                                             dplyr::select(Combo_ID, Model, Recipe_ID, Train_Test_ID, Hyperparameter_ID, Combo, Date, Forecast, Target)
                                           
                                           # write outputs
                                           write_data(x = final_predictions, 
                                                      combo = unique(final_predictions$Combo_ID), 
                                                      run_info = run_info, 
                                                      output_type = 'data',
                                                      folder = "forecasts",
                                                      suffix = '-tune_models')
                                           
                                           return(final_predictions)
                                         }

  # clean up any parallel run process
  par_end(cl)
  
  # update logging file
  log_df <- read_file(run_info, 
                      path = paste0("logs/", hash_data(run_info$experiment_name), '-', hash_data(run_info$run_name), ".csv"), 
                      return_type = 'df') %>%
    dplyr::mutate(run_global_models = run_global_models, 
                  run_local_models = run_local_models, 
                  global_model_recipes = global_model_recipes, 
                  seed = seed)
  
  write_data(x = log_df, 
             combo = NULL, 
             run_info = run_info, 
             output_type = "log",
             folder = "logs", 
             suffix = NULL)
  
  return(cli::cli_alert_success("Models Tuned"))
}