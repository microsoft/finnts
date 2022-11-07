#' Ensemble Models
#'
#' Create ensemble model forecasts
#'
#' @param run_info run info using the [set_run_info()] function
#' @param parallel_processing Default of NULL runs no parallel processing and
#'   forecasts each individual time series one after another. 'local_machine'
#'   leverages all cores on current machine Finn is running on. 'spark'
#'   runs time series in parallel on a spark cluster in Azure Databricks or
#'   Azure Synapse.
#' @param inner_parallel Run components of forecast process inside a specific
#'   time series in parallel. Can only be used if parallel_processing is 
#'   set to NULL or 'spark'. 
#' @param num_cores Number of cores to run when parallel processing is set up.
#'   Used when running parallel computations on local machine or within Azure.
#'   Default of NULL uses total amount of cores on machine minus one. Can't
#'   be greater than number of cores on machine minus 1.
#' @param seed Set seed for random number generator. Numeric value.
#'
#' @return Ensemble model outputs are written to disk
#'
#' @examples
#' \donttest{
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(
#'     Date >= "2012-01-01",
#'     Date <= "2015-06-01",
#'     id == "M750"
#'   )
#'
#' run_info <- set_run_info()
#'
#' prep_data(run_info,
#'   input_data = data_tbl,
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month",
#'   forecast_horizon = 3
#' )
#'
#' prep_models(run_info,
#'   models_to_run = c("arima", "glmnet"),
#'   num_hyperparameters = 2
#' )
#'
#' train_models(run_info,
#'   run_global_models = FALSE
#' )
#'
#' ensemble_models(run_info)
#' }
#' @export
ensemble_models <- function(run_info,
                            parallel_processing = NULL,
                            inner_parallel = FALSE, 
                            num_cores = NULL,
                            seed = 123) {

  cli::cli_progress_step("Training Ensemble Models")
  
  # check input values
  check_input_type("run_info", run_info, "list")
  check_input_type("num_cores", num_cores, c("NULL", "numeric"))
  check_input_type("seed", seed, "numeric")
  check_parallel_processing(parallel_processing, inner_parallel)

  # get input and combo values
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  num_hyperparameters <- as.numeric(log_df$num_hyperparameters)
  negative_forecast <- log_df$negative_forecast

  if (log_df$run_ensemble_models == FALSE) {
    cli::cli_alert_info("Ensemble models not ran since no multivariate models were chosen to run.")
    return(cli::cli_progress_done())
  }

  combo_list <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*_models.", run_info$data_output
    )
  ) %>%
    tibble::tibble(
      Path = .,
      File = fs::path_file(.)
    ) %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Type"), sep = "-", remove = TRUE) %>%
    dplyr::filter(Combo != hash_data("All-Data")) %>%
    dplyr::pull(Combo) %>%
    unique()

  model_train_test_tbl <- read_file(run_info,
    path = paste0(
      "/prep_models/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
      "-train_test_split.", run_info$data_output
    ),
    return_type = "df"
  )

  # check if a previous run already has necessary outputs
  prev_combo_list <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*ensemble_models.", run_info$data_output
    )
  ) %>%
    tibble::tibble(
      Path = .,
      File = fs::path_file(.)
    ) %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Run_Type"), sep = "-", remove = TRUE) %>%
    dplyr::pull(Combo) %>%
    unique()

  current_combo_list <- combo_list

  current_combo_list_final <- setdiff(
    current_combo_list,
    prev_combo_list
  )

  if (length(current_combo_list_final) == 0 & length(prev_combo_list) > 0) {
    return(cli::cli_progress_done())
  }

  # parallel run info
  par_info <- par_start(
    run_info = run_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    task_length = length(combo_list)
  )

  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  # get ind model forecasts ready for ensemble models
  ensemble_tbl <- foreach::foreach(
    x = combo_list,
    .combine = "rbind",
    .packages = packages,
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .noexport = NULL
  ) %op% {
    combo <- x

    # model forecasts
    single_model_tbl <- NULL
    suppressWarnings(try(single_model_tbl <- read_file(run_info,
      path = paste0(
        "/forecasts/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
        "-", combo, "-single_models.", run_info$data_output
      ),
      return_type = "df"
    ),
    silent = TRUE
    ))

    global_model_tbl <- NULL
    suppressWarnings(try(global_model_tbl <- read_file(run_info,
      path = paste0(
        "/forecasts/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
        "-", combo, "-global_models.", run_info$data_output
      ),
      return_type = "df"
    ),
    silent = TRUE
    ))

    # combine model forecasts
    initial_results_final_tbl <- single_model_tbl %>%
      rbind(global_model_tbl)

    # create training data for ensemble
    prep_ensemble_tbl <- initial_results_final_tbl %>%
      dplyr::mutate(Suffix = ifelse(Combo_ID == "All-Data", "Global", "Local")) %>%
      tidyr::unite(
        col = "Model_Key",
        c("Model_Name", "Recipe_ID", "Suffix"),
        sep = "-",
        remove = F
      ) %>%
      tidyr::pivot_wider(
        names_from = Model_Key, values_from = Forecast,
        id_cols = c("Combo", "Date", "Train_Test_ID", "Target"), values_fill = 0
      )

    # ensemble models to run
    refit_models <- unique(initial_results_final_tbl$Model_Name)

    ensemble_model_list <- refit_models[refit_models %in% c("cubist", "glmnet", "svm-poly", "svm-rbf", "xgboost")]

    if (length(ensemble_model_list) < 1) {
      stop("no ensemble models chosen to run")
    }

    model_workflow_tbl <- tibble::tibble()

    for (model in ensemble_model_list) {
      avail_arg_list <- list(
        "train_data" = prep_ensemble_tbl %>% dplyr::select(-Train_Test_ID),
        "model_type" = "ensemble",
        "pca" = FALSE
      )

      # get specific model spec
      fn_to_invoke <- get(gsub("-", "_", model))

      exp_arg_list <- formalArgs(fn_to_invoke)

      avail_names <- names(avail_arg_list)

      inp_arg_list <- list()

      for (x in avail_names) {
        if (x %in% exp_arg_list) {
          inp_arg_list[x] <- avail_arg_list[x]
        }
      }

      model_workflow <- do.call(fn_to_invoke, inp_arg_list, quote = TRUE)

      workflow_tbl <- tibble::tibble(
        Model_Name = model,
        Model_Workflow = list(model_workflow)
      )

      model_workflow_tbl <- rbind(model_workflow_tbl, workflow_tbl)
    }

    # get hyperparameters
    hyperparameters_tbl <- tibble::tibble()

    for (x in model_workflow_tbl %>% dplyr::group_split(dplyr::row_number(), .keep = FALSE)) {
      model <- x %>%
        dplyr::pull(Model_Name)

      temp_tbl <- model_workflow_tbl %>%
        dplyr::filter(Model_Name == model)

      model_workflow <- temp_tbl$Model_Workflow[[1]]

      model_spec <- model_workflow %>%
        workflows::extract_spec_parsnip()

      recipe_features <- prep_ensemble_tbl

      if (model == "svm-rbf") {
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
        purrr::map_df(tidyr::nest, data = tidyselect::everything()) %>%
        dplyr::rename(Hyperparameters = data) %>%
        tibble::rowid_to_column("Hyperparameter_Combo") %>%
        dplyr::mutate(Model = model)

      hyperparameters_tbl <- rbind(hyperparameters_tbl, hyperparameters_temp)
    }
    
    if(inner_parallel) {
      # ensure variables get exported
      model_train_test_tbl <- model_train_test_tbl
      model_workflow_tbl <- model_workflow_tbl
      model_hyperparameter_tbl <- model_hyperparameter_tbl
      seed <- seed
      combo_variables <- combo_variables
      negative_fcst_adj <- negative_fcst_adj
      negative_forecast <- negative_forecast
      prep_ensemble_tbl <- prep_ensemble_tbl
    }

    # tune hyperparameters
    tune_iter_list <- model_train_test_tbl %>%
      dplyr::mutate(Combo = x) %>%
      dplyr::filter(Run_Type == "Validation") %>%
      dplyr::select(Combo, Train_Test_ID) %>%
      dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
      purrr::map(.f = function(x) {
        hyperparameters_tbl %>%
          dplyr::select(Hyperparameter_Combo, Model) %>%
          dplyr::rename(Hyperparameter_ID = Hyperparameter_Combo) %>%
          dplyr::mutate(
            Combo = x$Combo,
            Train_Test_ID = x$Train_Test_ID
          )
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::select(Combo, Model, Train_Test_ID, Hyperparameter_ID)

    par_info <- par_start(
      run_info = run_info,
      parallel_processing = if(inner_parallel) {"local_machine"} else {NULL},
      num_cores = num_cores,
      task_length = nrow(tune_iter_list)
    )
    
    inner_cl <- par_info$cl
    inner_packages <- par_info$packages
    `%op%` <- par_info$foreach_operator
    
    tune_output_tbl <- foreach::foreach(
      x = tune_iter_list %>%
        dplyr::group_split(dplyr::row_number(), .keep = FALSE),
      .combine = "rbind",
      .packages = inner_packages,
      .errorhandling = "stop",
      .verbose = FALSE,
      .inorder = FALSE,
      .multicombine = TRUE,
      .noexport = NULL
    ) %op% {

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
        dplyr::filter(Train_Test_ID == data_split) %>%
        dplyr::pull(Train_End)

      test_end_date <- model_train_test_tbl %>%
        dplyr::filter(Train_Test_ID == data_split) %>%
        dplyr::pull(Test_End)

      # get train/test data
      full_data <- prep_ensemble_tbl %>%
        dplyr::mutate(Date_index.num = 0)

      training <- full_data %>%
        dplyr::filter(Date <= train_end_date) %>%
        dplyr::select(-Train_Test_ID)

      testing <- full_data %>%
        dplyr::filter(
          Date > train_end_date,
          Date <= test_end_date,
          Train_Test_ID == data_split
        )

      # get workflow
      workflow <- model_workflow_tbl %>%
        dplyr::filter(Model_Name == model)

      workflow_final <- workflow$Model_Workflow[[1]]

      # get hyperparameters
      hyperparameters <- hyperparameters_tbl %>%
        dplyr::filter(
          Model == model,
          Hyperparameter_Combo == param_combo
        ) %>%
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
        dplyr::rename(Forecast = .pred) %>%
        negative_fcst_adj(negative_forecast)

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
    } %>%
      base::suppressPackageStartupMessages()
    
    par_end(inner_cl)

    final_tune_iter_list <- model_train_test_tbl %>%
      dplyr::mutate(Combo = x) %>%
      dplyr::filter(Run_Type == "Validation") %>%
      dplyr::select(Combo, Train_Test_ID) %>%
      dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
      purrr::map(.f = function(x) {
        hyperparameters_tbl %>%
          dplyr::select(Hyperparameter_Combo, Model) %>%
          dplyr::rename(Hyperparameter_ID = Hyperparameter_Combo) %>%
          dplyr::mutate(
            Combo = x$Combo,
            Train_Test_ID = x$Train_Test_ID
          )
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::select(Combo, Model) %>%
      dplyr::distinct()

    final_tune_output_tbl <- foreach::foreach(
      x = final_tune_iter_list %>%
        dplyr::group_split(dplyr::row_number(), .keep = FALSE),
      .combine = "rbind",
      .packages = NULL,
      .errorhandling = "stop",
      .verbose = FALSE,
      .inorder = FALSE,
      .multicombine = TRUE,
      .noexport = NULL
    ) %do% {
      combo <- x %>%
        dplyr::pull(Combo)

      model <- x %>%
        dplyr::pull(Model)

      test_tbl <- tune_output_tbl %>%
        dplyr::filter(
          Model == model
        ) %>%
        dplyr::select(Model, Hyperparameter_ID, Train_Test_ID, Prediction, Model_Fit)

      best_param <- test_tbl %>%
        dplyr::select(-Model_Fit) %>%
        tidyr::unnest(Prediction) %>%
        dplyr::mutate(Combo = combo) %>%
        dplyr::group_by(Combo, Model, Hyperparameter_ID) %>%
        yardstick::rmse(
          truth = Target,
          estimate = Forecast,
          na_rm = TRUE
        ) %>%
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

      return(tibble::tibble(
        Combo = unique(final_predictions$Combo),
        Model = model,
        Hyperparameter_ID = best_param,
        Model_Fit = list(best_model_fit),
        Prediction = list(final_predictions)
      ))
    } %>%
      base::suppressPackageStartupMessages()
    
    # refit models
    refit_iter_list <- model_train_test_tbl %>%
      dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test")) %>%
      dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
      purrr::map(.f = function(x) {
        final_tune_output_tbl %>%
          dplyr::mutate(
            Run_Type = x %>% dplyr::pull(Run_Type),
            Train_Test_ID = x %>% dplyr::pull(Train_Test_ID),
            Train_End = x %>% dplyr::pull(Train_End),
            Test_End = x %>% dplyr::pull(Test_End)
          ) %>%
          dplyr::select(-Model_Fit, -Prediction)
      }) %>%
      dplyr::bind_rows()
    
    par_info <- par_start(
      run_info = run_info,
      parallel_processing = if(inner_parallel) {"local_machine"} else {NULL},
      num_cores = num_cores,
      task_length = nrow(refit_iter_list)
    )
    
    inner_cl <- par_info$cl
    inner_packages <- par_info$packages
    `%op%` <- par_info$foreach_operator

    refit_tbl <- foreach::foreach(
      x = refit_iter_list %>%
        dplyr::group_split(dplyr::row_number(), .keep = FALSE),
      .combine = "rbind",
      .packages = inner_packages,
      .errorhandling = "stop",
      .verbose = FALSE,
      .inorder = FALSE,
      .multicombine = TRUE,
      .noexport = NULL
    ) %op% {
      combo <- x %>%
        dplyr::pull(Combo)

      model <- x %>%
        dplyr::pull(Model)

      model_fit <- final_tune_output_tbl %>%
        dplyr::filter(
          Model == model,
          Combo == combo
        )

      final_hyperparameters <- unique(model_fit$Hyperparameter_ID)

      model_fit <- model_fit$Model_Fit[[1]]

      run_type <- x %>%
        dplyr::pull(Run_Type)

      run_id <- x %>%
        dplyr::pull(Train_Test_ID)

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
        dplyr::filter(
          Date > train_end,
          Date <= test_end,
          Train_Test_ID == run_id
        )

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
        dplyr::rename(Forecast = .pred) %>%
        negative_fcst_adj(negative_forecast)

      # finalize output tbl
      final_tbl <- tibble::tibble(
        Combo_ID = combo,
        Model_Name = model,
        Model_Type = "local",
        Recipe_ID = "Ensemble",
        Train_Test_ID = run_id,
        Hyperparameter_ID = final_hyperparameters,
        Model_Fit = list(model_fit),
        Prediction = list(model_prediction)
      )

      return(final_tbl)
    } %>%
      base::suppressPackageStartupMessages()
    
    par_end(inner_cl)

    # get final combined results and final fitted models
    final_model_fit_tbl <- refit_tbl %>%
      dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID)) %>%
      dplyr::filter(Train_Test_ID == 1) %>%
      tidyr::unite(col = "Model_ID", c("Model_Name", "Model_Type", "Recipe_ID"), sep = "--", remove = FALSE) %>%
      dplyr::select(Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Model_Fit)

    final_ensemble_results_tbl <- refit_tbl %>%
      dplyr::select(-Model_Fit) %>%
      tidyr::unnest(Prediction) %>%
      tidyr::unite(col = "Model_ID", c("Model_Name", "Model_Type", "Recipe_ID"), sep = "--", remove = FALSE) %>%
      dplyr::group_by(Combo_ID, Model_ID, Train_Test_ID) %>%
      dplyr::mutate(Horizon = dplyr::row_number()) %>%
      dplyr::ungroup()

    # write outputs
    write_data(
      x = final_ensemble_results_tbl,
      combo = unique(final_ensemble_results_tbl$Combo_ID),
      run_info = run_info,
      output_type = "data",
      folder = "forecasts",
      suffix = "-ensemble_models"
    )

    write_data(
      x = final_model_fit_tbl,
      combo = unique(final_model_fit_tbl$Combo_ID),
      run_info = run_info,
      output_type = "object",
      folder = "models",
      suffix = "-ensemble_models"
    )

    return(tibble::tibble())
  } %>%
    base::suppressPackageStartupMessages()

  # clean up any parallel run process
  par_end(cl)
}
