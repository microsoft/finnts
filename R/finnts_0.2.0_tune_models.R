
construct_initial_tune_fn <- function(obj_list) {
  
  list2env(obj_list, envir = environment())
  
  initial_tune_fn <- function(x) {

    model_recipe_tbl_local <- large_tbl
    
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
    full_data <- model_recipe_tbl_local %>%
      dplyr::filter(Recipe == data_prep_recipe) #%>%
    #dplyr::select(Data) %>%
    #tidyr::unnest(Data)
    
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
      Prediction = list(model_prediction)
    )
    
    return(final_tbl)
  }
  
  return(initial_tune_fn)
}

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
#' @param batch_size batch size
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
                        seed = 123, 
                        batch_size = 10000) {
  
  # get list of tasks to run
  combo_list <- c()
  
  global_model_list <- c("cubist", "glmnet", "mars", "svm-poly", "svm-rbf", "xgboost")
  
  if(run_local_models) {
    combo_list <- c(combo_list, unique(model_recipe_tbl$Combo))
  }
  
  if(run_global_models) {
    combo_list <- c(combo_list, "All-Data")
  }
  
  # iter_list <- purrr::map(combo_list, .f = function(x) {
  #   model_train_test_tbl %>%
  #     dplyr::mutate(Combo = x) %>%
  #     dplyr::rename(Train_Test_ID = Run_ID) %>%
  #     dplyr::filter(Run_Type == "Validation") %>%
  #     dplyr::select(Combo, Train_Test_ID)
  # }) %>%
  #   dplyr::bind_rows() %>%
  #   dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
  #   purrr::map(.f = function(x) {
  #     temp <- model_hyperparameter_tbl %>%
  #       dplyr::select(Hyperparameter_Combo, Model, Recipe) %>%
  #       dplyr::rename(Hyperparameter_ID = Hyperparameter_Combo, 
  #                     Recipe_ID = Recipe) %>%
  #       dplyr::mutate(Combo = x$Combo, 
  #                     Train_Test_ID = x$Train_Test_ID)
  #     
  #     if(x$Combo == 'All-Data') {
  #       temp <- temp %>%
  #         dplyr::filter(Model %in% global_model_list, 
  #                       Recipe_ID %in% global_model_recipes)
  #     }
  #     
  #     return(temp)
  #   }) %>%
  #   dplyr::bind_rows() %>%
  #   dplyr::select(Combo, Model, Recipe_ID, Train_Test_ID, Hyperparameter_ID)
  
  #return(iter_list)
  
  # task run function 
  #model_workflow_tbl <- model_workflow_tbl # prevent error in exporting tbl to compute cluster
  
  # submit_initial_tune_fn <- function(combo) {
  # 
  #   combo_iter_list <- iter_list %>%
  #     dplyr::filter(Combo == combo)
  # 
  #   combo_initial_tuning_tbl <- submit_fn(model_workflow_tbl,
  #                                   NULL,
  #                                   combo_iter_list %>%
  #                                     dplyr::group_split(dplyr::row_number(), .keep = FALSE),
  #                                   initial_tune_fn,
  #                                   num_cores,
  #                                   package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
  #                                                       'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
  #                                                       'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
  #                                                       'recipes', 'rules', 'modeltime'),
  #                                   function_exports = NULL, 
  #                                   error_handling = "remove")
  # }
  
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
  
  initial_tuning_tbl <- foreach::foreach(x = combo_list, 
                                         .combine = 'rbind', 
                                         #.export = c(function_exports, "large_tbl"), 
                                         .packages = packages,
                                         .errorhandling = "stop", 
                                         .verbose = FALSE, 
                                         .inorder = FALSE, 
                                         .multicombine = TRUE, 
                                         .noexport = NULL) %op% {

                                           if(x != "All-Data") {
                                             
                                             model_recipe_tbl_local <- model_recipe_tbl %>%
                                               dplyr::filter(Combo == x)
                                             
                                           } else {
                                             
                                             model_recipe_tbl_local <- model_recipe_tbl
                                           }
    
                                           iter_list <- model_train_test_tbl %>%
                                             dplyr::mutate(Combo = x) %>%
                                             dplyr::rename(Train_Test_ID = Run_ID) %>%
                                             dplyr::filter(Run_Type == "Validation") %>%
                                             dplyr::select(Combo, Train_Test_ID) %>%
                                             #dplyr::bind_rows() %>%
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
                                           
                                           print("test111111111111111111111111111111111111111")
                                           
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
                                                            full_data <- model_recipe_tbl_local %>%
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
                                                              Prediction = list(model_prediction)
                                                            )
                                                            
                                                            return(final_tbl)
                                                          })
                                           
                                           # output_tbl <- foreach::foreach(x = iter_list %>%
                                           #                                  dplyr::group_split(dplyr::row_number(), .keep = FALSE),
                                           #                                .combine = 'rbind',
                                           #                                .packages = NULL,
                                           #                                .errorhandling = "remove",
                                           #                                .verbose = FALSE,
                                           #                                .inorder = FALSE,
                                           #                                .multicombine = TRUE,
                                           #                                .noexport = NULL) %do% {
                                           #                                  
                                           #                                  print(x)
                                           # 
                                           #                                  # run input values
                                           #                                  param_combo <- x %>%
                                           #                                    dplyr::pull(Hyperparameter_ID)
                                           # 
                                           #                                  model <- x %>%
                                           #                                    dplyr::pull(Model)
                                           # 
                                           #                                  data_split <- x %>%
                                           #                                    dplyr::pull(Train_Test_ID)
                                           # 
                                           #                                  data_prep_recipe <- x %>%
                                           #                                    dplyr::pull(Recipe_ID)
                                           # 
                                           #                                  combo <- x %>%
                                           #                                    dplyr::pull(Combo)
                                           # 
                                           #                                  train_end_date <- model_train_test_tbl %>%
                                           #                                    dplyr::filter(Run_ID == data_split) %>%
                                           #                                    dplyr::pull(Train_End)
                                           # 
                                           #                                  test_end_date <- model_train_test_tbl %>%
                                           #                                    dplyr::filter(Run_ID == data_split) %>%
                                           #                                    dplyr::pull(Test_End)
                                           # 
                                           #                                  # get train/test data
                                           #                                  full_data <- model_recipe_tbl %>%
                                           #                                    dplyr::filter(Recipe == data_prep_recipe) %>%
                                           #                                    dplyr::select(Data) %>%
                                           #                                    tidyr::unnest(Data)
                                           # 
                                           #                                  if(combo != "All-Data") {
                                           # 
                                           #                                    full_data <- full_data %>%
                                           #                                      dplyr::filter(Combo == combo)
                                           # 
                                           #                                  } else {
                                           # 
                                           #                                    full_data <- full_data %>%
                                           #                                      tidyr::separate(col = Combo,
                                           #                                                      into = combo_variables,
                                           #                                                      sep = "---",
                                           #                                                      remove = FALSE)
                                           #                                  }
                                           # 
                                           #                                  training <- full_data %>%
                                           #                                    dplyr::filter(Date <= train_end_date)
                                           # 
                                           #                                  testing <- full_data %>%
                                           #                                    dplyr::filter(Date > train_end_date,
                                           #                                                  Date <= test_end_date)
                                           # 
                                           #                                  if(data_prep_recipe == "R2") {
                                           # 
                                           #                                    train_origin_max <- training %>%
                                           #                                      dplyr::filter(Horizon == 1)
                                           # 
                                           #                                    testing <- testing %>%
                                           #                                      dplyr::filter(Origin == max(train_origin_max$Origin) + 1)
                                           #                                  }
                                           # 
                                           #                                  # get workflow
                                           #                                  workflow <- model_workflow_tbl %>%
                                           #                                    dplyr::filter(Model_Name == model,
                                           #                                                  Model_Recipe == data_prep_recipe)
                                           # 
                                           #                                  workflow_final <- workflow$Model_Workflow[[1]]
                                           # 
                                           #                                  # get hyperparameters
                                           #                                  hyperparameters <- model_hyperparameter_tbl %>%
                                           #                                    dplyr::filter(Model == model,
                                           #                                                  Recipe == data_prep_recipe,
                                           #                                                  Hyperparameter_Combo == param_combo) %>%
                                           #                                    dplyr::select(Hyperparameters) %>%
                                           #                                    tidyr::unnest(Hyperparameters)
                                           # 
                                           #                                  # fit model
                                           #                                  set.seed(seed)
                                           # 
                                           #                                  if(nrow(hyperparameters) > 0) {
                                           #                                    model_fit <- workflow_final %>%
                                           #                                      tune::finalize_workflow(parameters = hyperparameters) %>%
                                           #                                      generics::fit(data = training)
                                           #                                  } else {
                                           #                                    model_fit <- workflow_final %>%
                                           #                                      generics::fit(data = training)
                                           #                                  }
                                           # 
                                           #                                  # create prediction
                                           #                                  model_prediction <- testing %>%
                                           #                                    dplyr::bind_cols(
                                           #                                      predict(model_fit, new_data = testing)
                                           #                                    ) %>%
                                           #                                    dplyr::select(Combo, Date, Target, .pred) %>%
                                           #                                    dplyr::rename(Forecast = .pred)
                                           # 
                                           #                                  # finalize output tbl
                                           #                                  final_tbl <- tibble::tibble(
                                           #                                    Combo = combo,
                                           #                                    Model = model,
                                           #                                    Recipe_ID = data_prep_recipe,
                                           #                                    Train_Test_ID = data_split,
                                           #                                    Hyperparameter_ID = param_combo,
                                           #                                    Prediction = list(model_prediction)
                                           #                                  )
                                           # 
                                           #                                  return(final_tbl)
                                           #   }
                                           
                                           return(output_tbl)
                                         }
  
  return(initial_tuning_tbl)
  
  
  r1_tbl <- model_recipe_tbl %>%
    dplyr::filter(Recipe == "R1") %>%
    dplyr::select(Recipe, Data) %>%
    tidyr::unnest(Data)
  
  r1_obj_list <- list(
    input_data = r1_tbl, 
    #model_recipe_tbl = model_recipe_tbl, 
    model_workflow_tbl = model_workflow_tbl, 
    model_hyperparameter_tbl = model_hyperparameter_tbl, 
    model_train_test_tbl = model_train_test_tbl, 
    run_global_models = run_global_models, 
    run_local_models = run_local_models, 
    global_model_recipes = global_model_recipes, 
    combo_variables = combo_variables, 
    parallel_processing = parallel_processing, 
    num_cores = num_cores,
    seed = seed
  )
  
  r2_tbl <- model_recipe_tbl %>%
    dplyr::filter(Recipe == "R2") %>%
    dplyr::select(Recipe, Data) %>%
    tidyr::unnest(Data)
  
  r2_obj_list <- list(
    input_data = r2_tbl, 
    #model_recipe_tbl = model_recipe_tbl, 
    model_workflow_tbl = model_workflow_tbl, 
    model_hyperparameter_tbl = model_hyperparameter_tbl, 
    model_train_test_tbl = model_train_test_tbl, 
    run_global_models = run_global_models, 
    run_local_models = run_local_models, 
    global_model_recipes = global_model_recipes, 
    combo_variables = combo_variables, 
    parallel_processing = parallel_processing, 
    num_cores = num_cores,
    seed = seed
  )
  
  rm("model_recipe_tbl")
  
  initial_tuning_tbl_r1 <- submit_fn(r1_obj_list,
                                     parallel_processing,
                                     iter_list %>%
                                       dplyr::filter(Recipe_ID == "R1") %>%
                                       dplyr::group_split(dplyr::row_number(), .keep = FALSE),
                                     construct_initial_tune_fn,
                                     num_cores,
                                     package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                                                         'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                                                         'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                                                         'recipes', 'rules', 'modeltime', 'pryr'),
                                     #function_exports = NULL,
                                     error_handling = "stop", 
                                     batch_size = batch_size)
  
  rm("r1_tbl")

  initial_tuning_tbl_r2 <- submit_fn(r2_obj_list,
                                     parallel_processing,
                                     iter_list %>%
                                       dplyr::filter(Recipe_ID == "R2") %>%
                                       dplyr::group_split(dplyr::row_number(), .keep = FALSE),
                                     construct_initial_tune_fn,
                                     num_cores,
                                     package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                                                         'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                                                         'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                                                         'recipes', 'rules', 'modeltime'),
                                     #function_exports = NULL,
                                     error_handling = "stop", 
                                     batch_size = batch_size)
  
  initial_tuning_tbl <- rbind(initial_tuning_tbl_r1, initial_tuning_tbl_r2)
  
  rm("r2_tbl")
  rm("initial_tuning_tbl_r1")
  rm("initial_tuning_tbl_r2")
  
  return(initial_tuning_tbl)
  #submit_fn <- submit_fn # fix later
  
  # initial_tuning_tbl <- submit_fn(model_workflow_tbl,
  #                                 parallel_processing,
  #                                 combo_list,
  #                                 submit_initial_tune_fn,
  #                                 num_cores,
  #                                 package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
  #                                                     'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
  #                                                     'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
  #                                                     'recipes', 'rules', 'modeltime'),
  #                                 function_exports = c("submit_fn"), 
  #                                 error_handling = "remove")

  # select the best combination of hyperparameters
  iter_list2 <- iter_list %>%
    dplyr::select(Combo, Model, Recipe_ID) %>%
    dplyr::distinct()
  
  choose_hyperparameters_fn <- function(x) {
    
    initial_tuning_tbl_local <- large_tbl
    
    combo <- x %>%
      dplyr::pull(Combo)
    
    model <- x %>%
      dplyr::pull(Model)
    
    recipe <- x %>%
      dplyr::pull(Recipe_ID)
    
    test_tbl <- initial_tuning_tbl_local %>%
      dplyr::filter(Combo == combo, 
                    Recipe_ID == recipe, 
                    Model == model) %>%
      dplyr::select(Model, Recipe_ID, Hyperparameter_ID, Train_Test_ID, Prediction)
    
    best_param <- test_tbl %>%
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
    
    final_predictions <- test_tbl %>%
      dplyr::filter(Hyperparameter_ID == best_param) %>%
      tidyr::unnest(Prediction) %>%
      dplyr::select(Combo, Date, Train_Test_ID, Target, Forecast)
    
    return(tibble::tibble(Combo = combo, 
                          Model = model, 
                          Recipe_ID = recipe, 
                          Hyperparameter_ID = best_param, 
                          Prediction = list(final_predictions)))
  }
  
  submit_choose_hyperparameters_fn <- function(combo) {

    combo_iter_list <- iter_list2 %>%
      dplyr::filter(Combo == combo)
    
    combo_final_tuning_tbl <- submit_fn(model_workflow_tbl,
                                        NULL,
                                        combo_iter_list %>%
                                          dplyr::group_split(dplyr::row_number(), .keep = FALSE),
                                        choose_hyperparameters_fn,
                                        num_cores,
                                        package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                                                            'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                                                            'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                                                            'recipes', 'rules', 'modeltime', 'yardstick'),
                                        function_exports = NULL)
  }
  
  final_tuning_tbl <- submit_fn(initial_tuning_tbl,
                                parallel_processing,
                                iter_list2 %>%
                                  dplyr::group_split(dplyr::row_number(), .keep = FALSE),
                                choose_hyperparameters_fn,
                                num_cores,
                                package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                                                    'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                                                    'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                                                    'recipes', 'rules', 'modeltime', 'yardstick'),
                                function_exports = NULL, 
                                env = environment())
  
  # final_tuning_tbl <- submit_fn(model_workflow_tbl,
  #                               parallel_processing,
  #                               combo_list,
  #                               submit_choose_hyperparameters_fn,
  #                               num_cores,
  #                               package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
  #                                                   'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
  #                                                   'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
  #                                                   'recipes', 'rules', 'modeltime', 'yardstick'),
  #                               function_exports = NULL)
  
  return(final_tuning_tbl)
}