
update_forecast <- function(agent_info,
                             parallel_processing = NULL,
                             inner_parallel = FALSE,
                             num_cores = NULL,
                             max_iter = 3, 
                             seed = 123) {

  # check agent info
  check_agent_info(agent_info)
  
  # check parallel processing
  check_parallel_processing(
    run_info = agent_info$project_info,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel
  )
  
  # register tools
  register_update_fcst_tools(agent_info)
  
  # run the workflow
  results <- update_fcst_agent_workflow(
    agent_info = agent_info,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores,
    max_iter = max_iter, 
    seed = seed
  )
  
  message("[agent] ✅ Agent run completed successfully.")
}

update_fcst_agent_workflow <- function(agent_info,
                                       parallel_processing,
                                       inner_parallel,
                                       num_cores,
                                       max_iter = 3, 
                                       seed = 123) {

  # create a fresh session for the reasoning LLM
  if (!is.null(agent_info$reason_llm)) {
    agent_info$reason_llm <- agent_info$reason_llm$clone()
  }

  # construct the workflow
  workflow <- list(
    start = list(
      fn = "initial_checks",
      `next` = "update_global_models",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info
      )
    ),
    update_global_models = list(
      fn = "update_global_models",
      `next` = "update_local_models",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info, 
        previous_best_run_tbl = "{results$initial_checks}",
        parallel_processing = parallel_processing,
        inner_parallel = inner_parallel,
        num_cores = num_cores, 
        seed = seed
      )
    ), 
    update_local_models = list(
      fn = "update_local_models",
      `next` = "stop",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info, 
        previous_best_run_tbl = "{results$initial_checks}",
        parallel_processing = parallel_processing,
        inner_parallel = inner_parallel,
        num_cores = num_cores, 
        seed = seed
      )
    ), 
    stop = list(fn = NULL)
  )
  
  init_ctx <- list(
    node      = "start",
    iter      = 0, # iteration counter
    max_iter  = max_iter, # loop limit
    results   = list(), # where each tool’s output will be stored
    attempts  = list() # retry bookkeeping for execute_node()
  )
  
  # run the graph
  run_graph(agent_info$driver_llm, workflow, init_ctx)
}

register_update_fcst_tools <- function(agent_info) {

  # workflows
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "update_fcst_agent_workflow",
    .description = "Run the Finn update forecast agent workflow",
    .fun = update_fcst_agent_workflow
  ))
  
  # individual tools
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "initial_checks",
    .description = "Get previous agent information and check formatting of latest data/inputs",
    .fun = initial_checks
  ))
  
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "update_global_models",
    .description = "Update global model forecasts across all time series",
    .fun = update_forecast_combo
  ))
  
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "update_local_models",
    .description = "Update local mdoel forecasts for each time series",
    .fun = update_local_models
  ))
}

initial_checks <- function(agent_info) {
  
  # get metadata
  project_info <- agent_info$project_info
  
  # formatting checks
  check_agent_info(agent_info = agent_info)
  
  if(!agent_info$overwrite) {
    stop("Error in agent_info(). Set overwrite = TRUE to update the agent with latest data and inputs.",
         call. = FALSE)
  }
  
  # load all agent runs
  agent_runs_list <- list_files(
    project_info$storage_object,
    paste0(
      agent_info$project_info$path, "/logs/*", hash_data(project_info$project_name), "-",
      "*agent_run.csv"
    )
  )
  
  if(length(agent_runs_list) <= 1) {
    stop("Error in update_forecast(). No previous agent runs found.",
         call. = FALSE)
  }
  
  # load previous agent run results (excluding current run)
  prev_agent_run_tbl <- read_file(
    run_info = agent_info$project_info,
    file_list = agent_runs_list,
    return_type = "df"
  ) %>%
    dplyr::arrange(dplyr::desc(agent_version)) %>%
    dplyr::filter(agent_version < agent_info$agent_version)
  
  # loop through each agent version, until finding the last completed run with real results
  for(version in prev_agent_run_tbl$agent_version) {
    
    # create version specific agent info
    temp_agent_tbl <- prev_agent_run_tbl %>%
      dplyr::filter(agent_version == version)
    
    temp_agent_info <- list(project_info = agent_info$project_info, 
                            run_id = temp_agent_tbl$run_id[1], 
                            storage_object = project_info$storage_object,
                            path = project_info$path)
    
    # get best run results for version
    temp_run_results <- get_best_agent_run(agent_info = temp_agent_info)
    
    if(nrow(temp_run_results) > 0) {
      prev_agent_info <- temp_agent_info
      prev_best_run_results <- temp_run_results
      break
    }
  }
  
  # get time series from previous agent run
  prev_run_combos <- get_total_combos(agent_info = prev_agent_info)
  
  # get number of time series from current agent run
  current_run_combos <- get_total_combos(agent_info = agent_info)
  
  # check if number of time series has changed
  if (length(prev_run_combos) < length(current_run_combos)) {
    stop("Error in update_forecast(). The number of time series has grown since last complted agent run, please remove new time series.",
         call. = FALSE)
  } else {
    
    new_combos <- setdiff(current_run_combos, prev_run_combos)
    
    if(length(new_combos) > 0) {
      stop("Error in update_forecast(). The following time series have been added since last completed agent run: ",
           paste(new_combos, collapse = ", "), ". Please remove time series.",
           call. = FALSE)
    }
  }
  
  # get best runs from previous agent run
  prev_best_runs_tbl <- get_best_agent_run(agent_info = prev_agent_info)
  
  if(nrow(prev_best_runs_tbl) == 0) {
    stop("Error in update_forecast(). No best runs found in previous agent run.",
         call. = FALSE)
  }

  return(prev_best_runs_tbl)
}

update_global_models <- function(agent_info, 
                                 previous_best_run_tbl, 
                                 parallel_processing, 
                                 inner_parallel, 
                                 num_cores, 
                                 seed) {
  
  # get metadata
  project_info <- agent_info$project_info
  
  # check if global models are required to run
  previous_best_run_global_tbl <- previous_best_run_tbl %>%
    dplyr::filter(model_type == "global")
  
  if (nrow(previous_best_run_global_tbl) == 0) {
    cli::cli_alert_info("no global models to update")
    return("no global models to update")
  }
  
  # start forecast update process
  results <- update_forecast_combo(agent_info = agent_info, 
                                   prev_best_run_tbl = previous_best_run_global_tbl,
                                   parallel_processing = parallel_processing,
                                   num_cores = num_cores,
                                   inner_parallel = inner_parallel, 
                                   seed = seed)
  
  return("done")
}

update_local_models <- function(agent_info, 
                                previous_best_run_tbl, 
                                parallel_processing, 
                                inner_parallel, 
                                num_cores, 
                                seed) {
  
  # get metadata
  project_info <- agent_info$project_info
  
  # check if local models are required to run
  previous_best_run_local_tbl <- previous_best_run_tbl %>%
    dplyr::filter(model_type == "local")
  
  if (nrow(previous_best_run_local_tbl) == 0) {
    cli::cli_alert_info("no local models to update")
    return("no local models to update")
  }
  
  # get combos to run
  combos_to_run <- unique(previous_best_run_local_tbl$combo)
  
  # start forecast update process
  par_info <- par_start(
    run_info = project_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    task_length = num_cores
  )
  
  inner_cl <- par_info$cl
  inner_packages <- par_info$packages
  
  combo_tbl <- foreach::foreach(
    prev_run = previous_best_run_local_tbl %>%
      dplyr::group_split(dplyr::row_number(), .keep = FALSE),
    .combine = "rbind",
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .noexport = NULL
  ) %do% {

    results <- update_forecast_combo(agent_info = agent_info, 
                                     prev_best_run_tbl = prev_run,
                                     parallel_processing = NULL,
                                     num_cores = num_cores,
                                     inner_parallel = inner_parallel, 
                                     seed = seed)

    return(results)
  }
  
  par_end(inner_cl)
}

update_forecast_combo <- function(agent_info, 
                                  prev_best_run_tbl, 
                                  parallel_processing = NULL, 
                                  num_cores = NULL, 
                                  inner_parallel = FALSE, 
                                  seed = 123) {

  # get metadata
  project_info <- agent_info$project_info
  
  if(unique(prev_best_run_tbl$model_type) == "global") {
    combo <- "All-Data"
    combo_value <- "*"
  } else {
    combo <- prev_best_run_tbl$combo[1]
    combo_value <- hash_data(combo)
  }
  
  combo_list <- unique(prev_best_run_tbl$combo)
  
  prev_best_wmape <- mean(prev_best_run_tbl$weighted_mape, na.rm = TRUE)
  
  # get run info of previous best run
  prev_run_info <- list(project_name = project_info$project_name, 
                        run_name = prev_best_run_tbl$best_run_name[1],
                        storage_object = project_info$storage_object,
                        path = project_info$path, 
                        data_output = project_info$data_output, 
                        object_output = project_info$object_output)
  
  prev_run_log_tbl <- get_run_info(project_name = project_info$project_name,
                                   run_name = prev_best_run_tbl$best_run_name[1],
                                   storage_object = project_info$storage_object,
                                   path = project_info$path)
  
  # get previous forecast of best run
  prev_fcst_tbl <- get_forecast_data(run_info = prev_run_info) %>%
    dplyr::filter(Best_Model == "Yes")
  
  # get best model list from previous run
  model_id_list <- prev_fcst_tbl %>%
    dplyr::pull(Model_ID) %>%
    unique() %>%
    strsplit(split = "_") %>%
    unlist()
  
  prev_best_model_list <- model_id_list %>%
    stringr::str_replace("--.*$", "") %>%
    unique()
  
  # get best model recipes from previous run
  prev_best_recipes <- sub(".*--", "", model_id_list) %>%
    unique()
  
  # get trained models from previous run
  trained_models_tbl <- get_trained_models(run_info = prev_run_info) %>%
    dplyr::filter(Model_ID %in% model_id_list)
  
  # get external regressor info from previous run
  external_regressors <- adjust_inputs(prev_run_log_tbl$external_regressors)
  
  if(!is.null(external_regressors)) {
    # check that regressors from previous run are still present in the new run, if any are missing throw an error
    missing_xregs <- setdiff(external_regressors, agent_info$external_regressors)
    if(length(missing_xregs) > 0) {
      stop("Error in update_forecast(). The following external regressors are missing from the new run: ",
           paste(missing_xregs, collapse = ", "), ". Please add them back to the agent inputs.",
           call. = FALSE)
    }
  }
  
  # adjustments for hts with weekly data
  if (prev_run_log_tbl$forecast_approach != "bottoms_up" & project_info$date_type == "week") {
    # turn off daily conversion before hts recon
    initial_weekly_to_daily <- FALSE
  } else {
    initial_weekly_to_daily <- weekly_to_daily
  }
  
  # get input data for new run
  input_data <- read_file(
    run_info = project_info,
    file_list = list_files(
      project_info$storage_object,
      paste0(
        project_info$path, "/input_data/*", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", combo_value, ".", project_info$data_output
      )
    ),
    return_type = "df"
  )
  
  # create unique run name
  run_name <- paste0(
    "agent_",
    agent_info$run_id, "_",
    ifelse(combo == "All-Data", hash_data("all"), combo_value), "_",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  )
  
  # create new run
  new_run_info <- set_run_info(
    project_name = project_info$project_name,
    run_name = run_name,
    storage_object = project_info$storage_object,
    path = project_info$path,
    data_output = project_info$data_output,
    object_output = project_info$object_output,
    add_unique_id = FALSE
  )

  # clean and prepare data for training
  prep_data(
    run_info = new_run_info,
    input_data = input_data,
    combo_variables = project_info$combo_variables,
    target_variable = "Target",
    date_type = project_info$date_type,
    forecast_horizon = agent_info$forecast_horizon,
    external_regressors = external_regressors,
    hist_start_date = NULL,
    hist_end_date = agent_info$hist_end_date,
    combo_cleanup_date = agent_info$combo_cleanup_date,
    fiscal_year_start = project_info$fiscal_year_start,
    clean_missing_values = prev_run_log_tbl$clean_missing_values,
    clean_outliers = prev_run_log_tbl$clean_outliers,
    box_cox = prev_run_log_tbl$box_cox,
    stationary = prev_run_log_tbl$stationary,
    forecast_approach = prev_run_log_tbl$forecast_approach,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    fourier_periods = NULL,
    lag_periods = adjust_inputs(prev_run_log_tbl$lag_periods, convert_numeric = TRUE),
    rolling_window_periods = adjust_inputs(prev_run_log_tbl$rolling_window_periods, convert_numeric = TRUE),
    recipes_to_run = prev_best_recipes,
    multistep_horizon = prev_run_log_tbl$multistep_horizon
  )
  
  if (prev_run_log_tbl$box_cox || prev_run_log_tbl$stationary) {
    combo_info_tbl <- read_file(new_run_info,
                                 path = paste0(
                                   "/prep_data/", hash_data(new_run_info$project_name), "-", hash_data(new_run_info$run_name),
                                   "-orig_combo_info.", new_run_info$data_output
                                 ),
                                 return_type = "df"
    )
    
    if(combo != "All-Data") {
      combo_info_tbl <- combo_info_tbl %>%
        dplyr::filter(Combo == combo)
    }
  } else {
    combo_info_tbl <- tibble::tibble()
  }
  
  # prep models and train/test splits
  prep_models(run_info = new_run_info,
              back_test_scenarios = agent_info$back_test_scenarios,
              back_test_spacing = agent_info$back_test_spacing,
              models_to_run = prev_best_model_list,
              models_not_to_run = NULL,
              run_ensemble_models = FALSE,
              pca = prev_run_log_tbl$pca,
              num_hyperparameters = as.numeric(prev_run_log_tbl$num_hyperparameters),
              seasonal_period = adjust_inputs(prev_run_log_tbl$seasonal_period, convert_numeric = TRUE),
              seed = seed)
  
  model_train_test_tbl <- read_file(new_run_info,
                                    path = paste0(
                                      "/prep_models/", hash_data(new_run_info$project_name), "-", hash_data(new_run_info$run_name),
                                      "-train_test_split.", new_run_info$data_output
                                    ),
                                    return_type = "df"
  )
  
  model_hyperparameter_tbl <- read_file(new_run_info,
                                        path = paste0(
                                          "/prep_models/", hash_data(new_run_info$project_name), "-", hash_data(new_run_info$run_name),
                                          "-model_hyperparameters.", new_run_info$object_output
                                        ),
                                        return_type = "df"
  )

  # refit each model on previously selected hyperparameters
  cli::cli_alert_info("refitting models for {combo} with previously selected hyperparameters")
  
  refit_model_tbl <- fit_models(run_info = new_run_info, 
                                combo = combo, 
                                combo_info_tbl = combo_info_tbl, 
                                trained_models_tbl = trained_models_tbl,
                                model_train_test_tbl = model_train_test_tbl,
                                model_hyperparameter_tbl = model_hyperparameter_tbl,
                                prev_run_log_tbl = prev_run_log_tbl, 
                                retune_hyperparameters = FALSE, 
                                parallel_processing = parallel_processing,
                                num_cores = num_cores,
                                inner_parallel = inner_parallel,
                                seed = seed)
  
  # get forecast and calculate wmape
  refit_fcst_tbl <- refit_model_tbl %>%
    adjust_forecast()
  
  refit_wmape <- refit_fcst_tbl %>%
    dplyr::filter(Combo %in% combo_list) %>%
    calc_wmape()
  
  # retune hyperparameters if +10% worse than previous best
  if(refit_wmape > (prev_best_wmape*1.1)) {
    cli::cli_alert_info("retuning model hyperparameters")
    
    retune_model_tbl <- fit_models(run_info = new_run_info, 
                                   combo = combo, 
                                   combo_info_tbl = combo_info_tbl, 
                                   trained_models_tbl = trained_models_tbl,
                                   model_train_test_tbl = model_train_test_tbl,
                                   model_hyperparameter_tbl = model_hyperparameter_tbl,
                                   prev_run_log_tbl = prev_run_log_tbl, 
                                   retune_hyperparameters = TRUE, 
                                   parallel_processing = parallel_processing,
                                   num_cores = num_cores,
                                   inner_parallel = inner_parallel,
                                   seed = seed)
    
    retune_fcst_tbl <- retune_model_tbl %>%
      adjust_forecast()
    
    retune_wmape <- retune_fcst_tbl %>%
      dplyr::filter(Combo %in% combo_list) %>%
      calc_wmape()
  } else {
    retune_wmape <- Inf
  }

  # choose best results
  if(retune_wmape < refit_wmape) {
    final_wmape <- retune_wmape
    final_model_tbl <- retune_model_tbl
    final_fcst_tbl <- retune_fcst_tbl %>%
      create_prediction_intervals(model_train_test_tbl) %>%
      convert_weekly_to_daily(project_info$date_type, initial_weekly_to_daily)
  } else {
    final_wmape <- refit_wmape
    final_model_tbl <- refit_model_tbl
    final_fcst_tbl <- refit_fcst_tbl %>%
      create_prediction_intervals(model_train_test_tbl) %>%
      convert_weekly_to_daily(project_info$date_type, initial_weekly_to_daily)
  }

  # write final outputs
  fitted_models <- final_model_tbl %>%
    tidyr::unite(col = "Model_ID", c("Model_Name", "Model_Type", "Recipe_ID"), sep = "--", remove = FALSE) %>%
    dplyr::select(Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Model_Fit)
  
  write_data(
    x = fitted_models,
    combo = unique(fitted_models$Combo_ID),
    run_info = new_run_info,
    output_type = "object",
    folder = "models",
    suffix = "-single_models"
  )

  if("simple_average" %in% unique(final_fcst_tbl$Recipe_ID)) {
    write_data(
      x = final_fcst_tbl %>% 
        dplyr::filter(Recipe_ID != "simple_average") %>%
        dplyr::select(-Run_Type),
      combo = unique(fitted_models$Combo_ID),
      run_info = new_run_info,
      output_type = "data",
      folder = "forecasts",
      suffix = "-single_models"
    )
    
    write_data(
      x = final_fcst_tbl %>% 
        dplyr::filter(Recipe_ID == "simple_average") %>%
        dplyr::select(-Run_Type),
      combo = unique(fitted_models$Combo_ID),
      run_info = new_run_info,
      output_type = "data",
      folder = "forecasts",
      suffix = "-average_models"
    )
  } else {
    if (combo == "All-Data") {
      for (combo_name in combo_list) {
        write_data(
          x = final_fcst_tbl %>% 
            dplyr::filter(Combo == combo_name) %>%
            dplyr::select(-Run_Type),
          combo = combo_name,
          run_info = new_run_info,
          output_type = "data",
          folder = "forecasts",
          suffix = "-global_models"
        )
      }
    } else {
      write_data(
        x = final_fcst_tbl %>%
          dplyr::select(-Run_Type),
        combo = unique(fitted_models$Combo_ID),
        run_info = new_run_info,
        output_type = "data",
        folder = "forecasts",
        suffix = "-single_models"
      )
    }
  }
  
  # log run
  log_wmape <- final_fcst_tbl %>%
    dplyr::mutate(Combo = "Placeholder") %>% # calc wmape for a single combo or all combos
    calc_wmape()

  log_best_run(agent_info = agent_info,
               run_info = new_run_info,
               weighted_mape = log_wmape,
               combo = if(combo=="All-Data") {NULL} else {combo})

  new_log_tbl <- get_run_info(
    project_name = project_info$project_name,
    run_name = new_run_info$run_name,
    storage_object = project_info$storage_object,
    path = project_info$path
  ) %>%
    dplyr::mutate(run_global_models = ifelse(combo == "All-Data", TRUE, FALSE),
                  run_local_models = ifelse(combo != "All-Data", TRUE, FALSE),
                  global_model_recipes = prev_run_log_tbl$global_model_recipes,
                  feature_selection = prev_run_log_tbl$feature_selection,
                  seed = seed,
                  negative_forecast = prev_run_log_tbl$negative_forecast,
                  inner_parallel = inner_parallel, 
                  average_models = prev_run_log_tbl$average_models,
                  max_model_average = prev_run_log_tbl$max_model_average,
                  weighted_mape = log_wmape)
  
  write_data(
    x = new_log_tbl,
    combo = NULL,
    run_info = new_run_info,
    output_type = "log",
    folder = "logs",
    suffix = NULL
  )
  
  return("done")
}

fit_models <- function(run_info, 
                       combo, 
                       combo_info_tbl,
                       trained_models_tbl,
                       model_train_test_tbl,
                       model_hyperparameter_tbl,
                       prev_run_log_tbl, 
                       retune_hyperparameters = FALSE, 
                       parallel_processing = NULL,
                       num_cores = NULL,
                       inner_parallel = FALSE,
                       seed = 123) {
  
  # train each model
  par_info <- par_start(
    run_info = run_info,
    parallel_processing = if (inner_parallel) {
      "local_machine"
    } else {
      NULL
    },
    num_cores = num_cores,
    task_length = num_cores
  )
  
  inner_cl <- par_info$cl
  inner_packages <- par_info$packages
  
  model_tbl <- foreach::foreach(
    model_run = trained_models_tbl %>%
      dplyr::group_split(dplyr::row_number(), .keep = FALSE),
    .combine = "rbind",
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .noexport = NULL
  ) %do% {
    # get initial run info
    model <- model_run %>%
      dplyr::pull(Model_Name)
    
    data_prep_recipe <- model_run %>%
      dplyr::pull(Recipe_ID)
    
    prep_data <- get_prepped_data(run_info = run_info,
                                  recipe = data_prep_recipe)
    
    workflow <- model_run$Model_Fit[[1]]
    
    if (nrow(prep_data) > 500 & model == "xgboost") {
      # update xgboost model to use 'hist' tree method to speed up training
      workflow <- workflows::update_model(
        workflow,
        workflows::extract_spec_parsnip(workflow) %>%
          parsnip::set_args(tree_method = "hist")
      )
    }
    
    if (combo == "All-Data") {
      # adjust column types to match original data
      prep_data <- adjust_column_types(
        prep_data,
        workflows::extract_recipe(workflow, estimated = FALSE)
      )
    }

    # if (feature_selection & model %in% fs_model_list) {
    #   # update model workflow to only use features from feature selection process
    #   if (data_prep_recipe == "R1") {
    #     final_features_list <- fs_list$R1
    #   } else {
    #     final_features_list <- fs_list$R2
    #   }
    #   
    #   if (multistep_horizon & data_prep_recipe == "R1" & model %in% list_multistep_models()) {
    #     updated_model_spec <- workflow %>%
    #       workflows::extract_spec_parsnip() %>%
    #       update(selected_features = final_features_list)
    #     
    #     empty_workflow_final <- workflow %>%
    #       workflows::update_model(updated_model_spec)
    #   } else {
    #     final_features_list <- final_features_list[[paste0("model_lag_", forecast_horizon)]]
    #     
    #     updated_recipe <- workflow %>%
    #       workflows::extract_recipe(estimated = FALSE) %>%
    #       recipes::remove_role(tidyselect::everything(), old_role = "predictor") %>%
    #       recipes::update_role(tidyselect::any_of(unique(c(final_features_list, "Date"))), new_role = "predictor") %>%
    #       base::suppressWarnings()
    #     
    #     empty_workflow_final <- workflow %>%
    #       workflows::update_recipe(updated_recipe)
    #   }
    # } else {
    #   empty_workflow_final <- workflow
    # }

    hyperparameters <- model_hyperparameter_tbl %>%
      dplyr::filter(
        Model == model,
        Recipe == data_prep_recipe
      ) %>%
      dplyr::select(Hyperparameter_Combo, Hyperparameters) %>%
      tidyr::unnest(Hyperparameters)

    if (prev_run_log_tbl$stationary & !(model %in% list_multivariate_models())) {
      # undifference the data for a univariate model
      prep_data <- prep_data %>%
        undifference_recipe(
          combo_info_tbl,
          model_train_test_tbl %>% dplyr::slice(1) %>% dplyr::pull(Train_End)
        )
    }
    
    # tune hyperparameters
    if(retune_hyperparameters) {
      set.seed(seed)
      
      tune_results <- tune::tune_grid(
        object = workflow,
        resamples = create_splits(prep_data, model_train_test_tbl %>% dplyr::filter(Run_Type == "Validation")),
        grid = hyperparameters %>% dplyr::select(-Hyperparameter_Combo),
        control = tune::control_grid(
          allow_par = inner_parallel,
          pkgs = c(inner_packages, "finnts"),
          parallel_over = "everything"
        )
      ) %>%
        base::suppressMessages() %>%
        base::suppressWarnings()
      
      best_param <- tune::select_best(tune_results, metric = "rmse")
      
      if (length(colnames(best_param)) == 1) {
        hyperparameter_id <- 1
      } else {
        hyperparameter_id <- hyperparameters %>%
          dplyr::inner_join(best_param) %>%
          dplyr::select(Hyperparameter_Combo) %>%
          dplyr::pull() %>%
          base::suppressMessages()
      }
      
      finalized_workflow <- tune::finalize_workflow(workflow, best_param)
    } else {
      hyperparameter_id <- 1
      finalized_workflow <- workflow
    }
    
    # fit model on entire input data
    set.seed(seed)
    
    wflow_fit <- generics::fit(finalized_workflow, prep_data %>% tidyr::drop_na(Target)) %>%
      base::suppressMessages()
    
    # refit on all train test splits
    set.seed(seed)
    
    refit_tbl <- tune::fit_resamples(
      object = finalized_workflow,
      resamples = create_splits(prep_data, model_train_test_tbl),
      metrics = NULL,
      control = tune::control_resamples(
        allow_par = inner_parallel,
        save_pred = TRUE,
        pkgs = inner_packages,
        parallel_over = "everything"
      )
    ) %>%
      tune::collect_predictions() %>%
      base::suppressMessages() %>%
      base::suppressWarnings()
    
    # only predict on test sets, no refitting or tuning
    # splits_tbl <- create_splits(prep_data, model_train_test_tbl)
    # 
    # pred_tbl <- purrr::imap_dfr(
    #   splits_tbl$splits,
    #   function(spl, i) {
    #     
    #     assess_idx <- spl$out_id
    #     assess_dat <- rsample::assessment(spl)
    #     
    #     set.seed(seed)
    #     
    #     tibble::tibble(
    #       .pred   = predict(finalized_workflow, assess_dat)[[".pred"]],
    #       id      = splits_tbl$id[i],
    #       .row    = assess_idx,
    #       Target  = prep_data$Target[assess_idx],
    #       .config = "test"
    #     )
    #   }
    # ) %>%
    #   print(n=100)
    
    # finalize forecast
    final_fcst <- refit_tbl %>%
      dplyr::rename(
        Forecast = .pred,
        Train_Test_ID = id
      ) %>%
      dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID)) %>%
      dplyr::left_join(
        prep_data %>%
          dplyr::mutate(.row = dplyr::row_number()) %>%
          dplyr::select(Combo, Date, .row),
        by = ".row"
      ) %>%
      dplyr::mutate(Hyperparameter_ID = hyperparameter_id) %>%
      dplyr::select(-.row, -.config) %>%
      dplyr::left_join(
        model_train_test_tbl %>%
          dplyr::select(Train_Test_ID, Run_Type),
        by = "Train_Test_ID"
      )
  
    # check for future forecast
    if (as.numeric(min(unique(final_fcst$Train_Test_ID))) != 1) {
      stop("model is missing future forecast")
    }
    
    # undo differencing transformation
    if (prev_run_log_tbl$stationary & model %in% list_multivariate_models()) {
      if (combo == "All-Data") {
        final_fcst <- final_fcst %>%
          dplyr::group_by(Combo) %>%
          dplyr::group_split() %>%
          purrr::map(function(x) {
            combo <- unique(x$Combo)

            final_fcst_return <- x %>%
              undifference_forecast(
                prep_data %>% dplyr::filter(Combo == combo),
                combo_info_tbl %>% dplyr::filter(Combo == combo)
              )

            return(final_fcst_return)
          }) %>%
          dplyr::bind_rows()
      } else {
        final_fcst <- final_fcst %>%
          undifference_forecast(
            prep_data,
            combo_info_tbl
          )
      }
    }

    # undo box-cox transformation
    if (prev_run_log_tbl$box_cox) {
      if (combo == "All-Data") {
        final_fcst <- final_fcst %>%
          dplyr::group_by(Combo) %>%
          dplyr::group_split() %>%
          purrr::map(function(x) {
            combo <- unique(x$Combo)

            lambda <- combo_info_tbl %>%
              dplyr::filter(Combo == combo) %>%
              dplyr::select(Box_Cox_Lambda) %>%
              dplyr::pull(Box_Cox_Lambda)

            if (!is.na(lambda)) {
              final_fcst_return <- x %>%
                dplyr::mutate(
                  Forecast = timetk::box_cox_inv_vec(Forecast, lambda = lambda),
                  Target = timetk::box_cox_inv_vec(Target, lambda = lambda)
                )
            } else {
              final_fcst_return <- x
            }

            return(final_fcst_return)
          }) %>%
          dplyr::bind_rows()
      } else {
        lambda <- combo_info_tbl$Box_Cox_Lambda

        if (!is.na(lambda)) {
          final_fcst <- final_fcst %>%
            dplyr::mutate(
              Forecast = timetk::box_cox_inv_vec(Forecast, lambda = lambda),
              Target = timetk::box_cox_inv_vec(Target, lambda = lambda)
            )
        }
      }
    }

    # negative forecast adjustment
    final_fcst <- final_fcst %>%
      negative_fcst_adj(prev_run_log_tbl$negative_forecast)
    
    # return the forecast
    combo_id <- ifelse(combo == "All-Data", "All-Data", unique(final_fcst$Combo))
    
    final_return_tbl <- tibble::tibble(
      Combo_ID = combo_id,
      Model_Name = model,
      Model_Type = ifelse(combo_id == "All-Data", "global", "local"),
      Recipe_ID = data_prep_recipe,
      Forecast_Tbl = list(final_fcst),
      Model_Fit = list(wflow_fit)
    )
    
    return(final_return_tbl)
  }
  
  par_end(inner_cl)
  
  # ensure at least one model ran successfully
  if (is.null(model_tbl)) {
    stop("All models failed to train")
  }
  
  return(model_tbl)
}

adjust_forecast <- function(model_tbl) {
  
  # check if forecasts should be averaged
  if(nrow(model_tbl) > 1) {
    simple_average <- TRUE
  } else {
    simple_average <- FALSE
  }
  
  # extract out the forecast table
  forecast_tbl <- model_tbl %>%
    dplyr::select(-Model_Fit) %>%
    tidyr::unnest(Forecast_Tbl) %>%
    dplyr::arrange(Train_Test_ID) %>%
    tidyr::unite(col = "Model_ID", c("Model_Name", "Model_Type", "Recipe_ID"), sep = "--", remove = FALSE) %>%
    dplyr::group_by(Combo, Model_ID, Train_Test_ID) %>%
    dplyr::mutate(Horizon = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  if(simple_average) {
    # average the forecasts
    avg_forecast_tbl <- forecast_tbl %>%
      dplyr::group_by(Combo_ID, Combo, Run_Type, Train_Test_ID, Date, Horizon) %>%
      dplyr::summarise(
        Forecast = mean(Forecast, na.rm = TRUE),
        Target = mean(Target, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(Model_ID = paste(sort(unique(forecast_tbl$Model_ID)), collapse = "_"),
                    Model_Name = NA,
                    Model_Type = "local",
                    Recipe_ID = "simple_average",
                    Hyperparameter_ID = NA,
                    Best_Model = "Yes")
    
    final_fcst_tbl <- forecast_tbl %>%
      dplyr::mutate(Best_Model = "No") %>%
      dplyr::bind_rows(avg_forecast_tbl)
  } else {
    final_fcst_tbl <- forecast_tbl %>%
      dplyr::mutate(Best_Model = "Yes")
  }
  
  return(final_fcst_tbl)
}

calc_wmape <- function(forecast_tbl) {
  forecast_tbl %>%
    dplyr::filter(Run_Type == "Back_Test", 
                  Best_Model == "Yes") %>%
    dplyr::mutate(
      Target = ifelse(Target == 0, 0.1, Target)
    ) %>%
    dplyr::group_by(Combo) %>%
    dplyr::mutate(
      MAPE = round(abs((Forecast - Target) / Target), digits = 4),
      Total = sum(Target, na.rm = TRUE),
      Weight = (MAPE * Target) / Total
    ) %>%
    dplyr::summarise(
      weighted_mape = round(sum(Weight, na.rm = TRUE), digits = 4),
      .groups = "drop"
    ) %>%
    dplyr::pull(weighted_mape) %>%
    mean(na.rm = TRUE)
}

adjust_inputs <- function(x, 
                          convert_numeric = FALSE) {
  if(is.na(x)) {
    NULL
  } else if(is.character(x)) {
    # split string by --- and return as list
    if (convert_numeric) {
      as.numeric(strsplit(x, "---")[[1]])
    } else {
      strsplit(x, "---")[[1]]
    }
  } else {
    if(convert_numeric) {
      as.numeric(x)
    } else {
      x
    }
  }
}