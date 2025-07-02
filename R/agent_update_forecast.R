
update_forecast <- function(agent_info, 
                            parallel_processing = NULL, 
                            num_cores = NULL, 
                            inner_parallel = FALSE, 
                            seed = 123) {
  
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
  prev_best_runs_tbl <- get_best_agent_run(agent_info = prev_agent_info) %>%
    print()
  
  if(nrow(prev_best_runs_tbl) == 0) {
    stop("Error in update_forecast(). No best runs found in previous agent run.",
         call. = FALSE)
  }
  
  ################################################################
  ################################################################
  # for early dev purposes let's just get one local time series
  ################################################################
  ################################################################
  prev_best_runs_tbl <- prev_best_runs_tbl %>%
    dplyr::filter(model_type == "local") %>%
    dplyr::slice(1)
  
  combo <- prev_best_runs_tbl$combo[1]
  combo_value <- hash_data(combo)
  
  # get run info of previous best run
  prev_run_info_list <- list(project_name = project_info$project_name, 
                             run_name = prev_best_runs_tbl$best_run_name[1],
                             storage_object = project_info$storage_object,
                             path = prev_agent_info$project_info$path, 
                             data_output = project_info$data_output, 
                             object_output = project_info$object_output)
  
  
  prev_run_info <- get_run_info(project_name = project_info$project_name,
                                run_name = prev_best_runs_tbl$best_run_name[1],
                                storage_object = project_info$storage_object,
                                path = prev_agent_info$project_info$path)
  
  prev_run_info$stationary <- TRUE
  
  # get previous forecast of best run
  prev_fcst_tbl <- get_forecast_data(run_info = prev_run_info_list) %>%
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
  trained_models_tbl <- get_trained_models(run_info = prev_run_info_list) %>%
    dplyr::filter(Model_ID %in% model_id_list)
  
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
    ifelse(is.null(combo), hash_data("all"), combo_value), "_",
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
    external_regressors = agent_info$external_regressors,
    hist_start_date = NULL,
    hist_end_date = agent_info$hist_end_date,
    combo_cleanup_date = agent_info$combo_cleanup_date,
    fiscal_year_start = project_info$fiscal_year_start,
    clean_missing_values = prev_run_info$clean_missing_values,
    clean_outliers = prev_run_info$clean_outliers,
    box_cox = FALSE,
    stationary = prev_run_info$stationary,
    forecast_approach = prev_run_info$forecast_approach,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    fourier_periods = NULL,
    lag_periods = adjust_inputs(prev_run_info$lag_periods, convert_numeric = TRUE),
    rolling_window_periods = adjust_inputs(prev_run_info$rolling_window_periods, convert_numeric = TRUE),
    recipes_to_run = prev_best_recipes,
    multistep_horizon = prev_run_info$multistep_horizon
  )
  
  if (prev_run_info$box_cox || prev_run_info$stationary) {
    combo_info_tbl <- read_file(new_run_info,
                                 path = paste0(
                                   "/prep_data/", hash_data(new_run_info$project_name), "-", hash_data(new_run_info$run_name),
                                   "-orig_combo_info.", new_run_info$data_output
                                 ),
                                 return_type = "df"
    )
  } else {
    combo_info_tbl <- tibble::tibble()
  }
  
  # set train test splits
  model_train_test_tbl <- train_test_split(run_info = new_run_info,
                                           back_test_scenarios = agent_info$back_test_scenarios,
                                           back_test_spacing = agent_info$back_test_spacing,
                                           run_ensemble_models = FALSE, 
                                           model_list = prev_best_model_list, 
                                           return = TRUE)
  
  # train each model
  model_tbl <- fit_models(run_info = new_run_info, 
                          combo = combo, 
                          combo_info_tbl = combo_info_tbl, 
                          trained_models_tbl = trained_models_tbl,
                          model_train_test_tbl = model_train_test_tbl,
                          parallel_processing = parallel_processing,
                          num_cores = num_cores,
                          inner_parallel = inner_parallel,
                          seed = seed)
  
  # write outputs
  fitted_models <- model_tbl %>%
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

  final_forecast_tbl <- model_tbl %>%
    dplyr::select(-Model_Fit) %>%
    tidyr::unnest(Forecast_Tbl) %>%
    dplyr::arrange(Train_Test_ID) %>%
    tidyr::unite(col = "Model_ID", c("Model_Name", "Model_Type", "Recipe_ID"), sep = "--", remove = FALSE) %>%
    dplyr::group_by(Combo, Model_ID, Train_Test_ID) %>%
    dplyr::mutate(Horizon = dplyr::row_number()) %>%
    dplyr::ungroup()

  if (unique(final_forecast_tbl$Combo_ID) == "All-Data") {
    for (combo_name in unique(final_forecast_tbl$Combo)) {
      write_data(
        x = final_forecast_tbl %>% dplyr::filter(Combo == combo_name),
        combo = combo_name,
        run_info = new_run_info,
        output_type = "data",
        folder = "forecasts",
        suffix = "-global_models"
      )
    }
  } else {
    write_data(
      x = final_forecast_tbl,
      combo = unique(fitted_models$Combo_ID),
      run_info = new_run_info,
      output_type = "data",
      folder = "forecasts",
      suffix = "-single_models"
    )
  }

  print(model_tbl)
  print(final_forecast_tbl)
}

fit_models <- function(run_info, 
                       combo, 
                       combo_info_tbl,
                       trained_models_tbl,
                       model_train_test_tbl,
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
    # 
    # hyperparameters <- model_hyperparameter_tbl %>%
    #   dplyr::filter(
    #     Model == model,
    #     Recipe == data_prep_recipe
    #   ) %>%
    #   dplyr::select(Hyperparameter_Combo, Hyperparameters) %>%
    #   tidyr::unnest(Hyperparameters)

    if (nrow(combo_info_tbl) > 0 & !(model %in% list_multivariate_models())) {
      # undifference the data for a univariate model
      prep_data <- prep_data %>%
        undifference_recipe(
          combo_info_tbl,
          model_train_test_tbl %>% dplyr::slice(1) %>% dplyr::pull(Train_End)
        )
    }
    
    # tune hyperparameters
    # set.seed(seed)
    # 
    # tune_results <- tune::tune_grid(
    #   object = empty_workflow_final,
    #   resamples = create_splits(prep_data, model_train_test_tbl %>% dplyr::filter(Run_Type == "Validation")),
    #   grid = hyperparameters %>% dplyr::select(-Hyperparameter_Combo),
    #   control = tune::control_grid(
    #     allow_par = inner_parallel,
    #     pkgs = c(inner_packages, "finnts"),
    #     parallel_over = "everything"
    #   )
    # ) %>%
    #   base::suppressMessages() %>%
    #   base::suppressWarnings()
    # 
    # best_param <- tune::select_best(tune_results, metric = "rmse")
    # 
    # if (length(colnames(best_param)) == 1) {
    #   hyperparameter_id <- 1
    # } else {
    #   hyperparameter_id <- hyperparameters %>%
    #     dplyr::inner_join(best_param) %>%
    #     dplyr::select(Hyperparameter_Combo) %>%
    #     dplyr::pull() %>%
    #     base::suppressMessages()
    # }
    # 
    # finalized_workflow <- tune::finalize_workflow(empty_workflow_final, best_param)
    
    finalized_workflow <- workflow
    
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
      # dplyr::mutate(Hyperparameter_ID = hyperparameter_id) %>%
      dplyr::select(-.row, -.config)
    
    # check for future forecast
    if (as.numeric(min(unique(final_fcst$Train_Test_ID))) != 1) {
      stop("model is missing future forecast")
    }
    
    # undo differencing transformation
    if (nrow(combo_info_tbl) > 0 & model %in% list_multivariate_models()) {
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

    # # undo box-cox transformation
    # if (box_cox) {
    #   if (combo_hash == "All-Data") {
    #     final_fcst <- final_fcst %>%
    #       dplyr::group_by(Combo) %>%
    #       dplyr::group_split() %>%
    #       purrr::map(function(x) {
    #         combo <- unique(x$Combo)
    #         
    #         lambda <- filtered_combo_info_tbl %>%
    #           dplyr::filter(Combo == combo) %>%
    #           dplyr::select(Box_Cox_Lambda) %>%
    #           dplyr::pull(Box_Cox_Lambda)
    #         
    #         if (!is.na(lambda)) {
    #           final_fcst_return <- x %>%
    #             dplyr::mutate(
    #               Forecast = timetk::box_cox_inv_vec(Forecast, lambda = lambda),
    #               Target = timetk::box_cox_inv_vec(Target, lambda = lambda)
    #             )
    #         } else {
    #           final_fcst_return <- x
    #         }
    #         
    #         return(final_fcst_return)
    #       }) %>%
    #       dplyr::bind_rows()
    #   } else {
    #     lambda <- filtered_combo_info_tbl$Box_Cox_Lambda
    #     
    #     if (!is.na(lambda)) {
    #       final_fcst <- final_fcst %>%
    #         dplyr::mutate(
    #           Forecast = timetk::box_cox_inv_vec(Forecast, lambda = lambda),
    #           Target = timetk::box_cox_inv_vec(Target, lambda = lambda)
    #         )
    #     }
    #   }
    # }
    # 
    # # negative forecast adjustment
    # final_fcst <- final_fcst %>%
    #   negative_fcst_adj(negative_forecast)
    
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