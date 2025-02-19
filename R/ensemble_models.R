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
#'     Date >= "2013-01-01",
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
  check_parallel_processing(
    run_info,
    parallel_processing,
    inner_parallel
  )

  # get input and combo values
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  num_hyperparameters <- as.numeric(log_df$num_hyperparameters)
  negative_forecast <- log_df$negative_forecast
  run_global_models <- log_df$run_global_models
  run_local_models <- log_df$run_local_models
  models_to_run <- log_df$models_to_run
  models_not_to_run <- log_df$models_not_to_run

  if (log_df$run_ensemble_models == FALSE) {
    cli::cli_alert_info("Ensemble models have been turned off.")
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
    cli::cli_alert_info("Ensemble Models Already Trained")
    return(cli::cli_progress_done())
  }

  # get ensemble models to run
  ensemble_model_list <- list_ensemble_models()

  if (is.na(models_to_run) & is.na(models_not_to_run)) {
    # do nothing, using existing ml_models list
  } else if (is.na(models_to_run) & !is.na(models_not_to_run)) {
    ensemble_model_list <- setdiff(ensemble_model_list, stringr::str_split(models_not_to_run, "---")[[1]])
  } else {
    ensemble_model_list <- ensemble_model_list[list_ensemble_models() %in% stringr::str_split(models_to_run, "---")[[1]]]
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
    x = current_combo_list_final,
    .combine = "rbind",
    .packages = packages,
    .errorhandling = "remove",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .noexport = NULL
  ) %op%
    {
      set.seed(seed)

      combo <- x

      # model forecasts
      single_model_tbl <- NULL
      if (run_local_models) {
        suppressWarnings(try(
          single_model_tbl <- read_file(run_info,
            path = paste0(
              "/forecasts/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
              "-", combo, "-single_models.", run_info$data_output
            ),
            return_type = "df"
          ),
          silent = TRUE
        ))
      }

      global_model_tbl <- NULL
      if (run_global_models) {
        suppressWarnings(try(
          global_model_tbl <- read_file(run_info,
            path = paste0(
              "/forecasts/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
              "-", combo, "-global_models.", run_info$data_output
            ),
            return_type = "df"
          ),
          silent = TRUE
        ))
      }

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
      if (length(ensemble_model_list) < 1) {
        stop("no ensemble models chosen to run")
      }

      model_workflow_tbl <- tibble::tibble()

      for (model in ensemble_model_list) {
        avail_arg_list <- list(
          "train_data" = prep_ensemble_tbl %>% dplyr::select(-Train_Test_ID),
          "model_type" = "ensemble",
          "pca" = FALSE,
          "multistep" = FALSE
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
      model_hyperparameters_tbl <- tibble::tibble()

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

        set.seed(seed)

        grid <- dials::grid_latin_hypercube(parameters, size = num_hyperparameters)

        hyperparameters_temp <- grid %>%
          dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
          purrr::map_df(tidyr::nest, data = tidyselect::everything()) %>%
          dplyr::rename(Hyperparameters = data) %>%
          tibble::rowid_to_column("Hyperparameter_Combo") %>%
          dplyr::mutate(Model = model)

        model_hyperparameters_tbl <- rbind(model_hyperparameters_tbl, hyperparameters_temp)
      }

      if (inner_parallel) {
        # ensure variables get exported
        model_train_test_tbl <- model_train_test_tbl
        model_workflow_tbl <- model_workflow_tbl
        seed <- seed
        negative_fcst_adj <- negative_fcst_adj
        negative_forecast <- negative_forecast
      }

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
        model_run = model_workflow_tbl %>%
          dplyr::select(Model_Name) %>%
          dplyr::group_split(dplyr::row_number(), .keep = FALSE),
        .combine = "rbind",
        .errorhandling = "remove",
        .verbose = FALSE,
        .inorder = FALSE,
        .multicombine = TRUE,
        .noexport = NULL
      ) %do% {
        # get initial run info
        model <- model_run %>%
          dplyr::pull(Model_Name)

        workflow <- model_workflow_tbl %>%
          dplyr::filter(Model_Name == model) %>%
          dplyr::select(Model_Workflow)

        workflow <- workflow$Model_Workflow[[1]]

        hyperparameters <- model_hyperparameters_tbl %>%
          dplyr::filter(Model == model) %>%
          dplyr::select(Hyperparameter_Combo, Hyperparameters) %>%
          tidyr::unnest(Hyperparameters)

        # tune hyperparameters
        set.seed(seed)

        tune_results <- tune::tune_grid(
          object = workflow,
          resamples = create_splits(prep_ensemble_tbl, model_train_test_tbl %>% dplyr::filter(Run_Type == "Validation")),
          grid = hyperparameters %>% dplyr::select(-Hyperparameter_Combo),
          control = tune::control_grid(
            allow_par = inner_parallel,
            pkgs = inner_packages,
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

        final_wflow <- tune::finalize_workflow(workflow, best_param)
        set.seed(seed)
        wflow_fit <- generics::fit(final_wflow, prep_ensemble_tbl %>% tidyr::drop_na(Target))

        # refit on all train test splits
        set.seed(seed)

        refit_tbl <- tune::fit_resamples(
          object = final_wflow,
          resamples = create_splits(prep_ensemble_tbl, model_train_test_tbl %>% dplyr::filter(Run_Type %in% c("Back_Test", "Future_Forecast"))),
          metrics = NULL,
          control = tune::control_resamples(
            allow_par = inner_parallel,
            save_pred = TRUE,
            pkgs = inner_packages,
            parallel_over = "everything"
          )
        ) %>%
          base::suppressMessages() %>%
          base::suppressWarnings()

        final_fcst <- tune::collect_predictions(refit_tbl) %>%
          dplyr::rename(
            Forecast = .pred,
            Train_Test_ID = id
          ) %>%
          dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID)) %>%
          dplyr::left_join(
            prep_ensemble_tbl %>%
              dplyr::mutate(.row = dplyr::row_number()) %>%
              dplyr::select(Combo, Date, .row),
            by = ".row"
          ) %>%
          dplyr::mutate(Hyperparameter_ID = hyperparameter_id) %>%
          dplyr::select(-.row, -.config) %>%
          negative_fcst_adj(negative_forecast)

        combo_id <- unique(final_fcst$Combo)

        final_return_tbl <- tibble::tibble(
          Combo_ID = combo_id,
          Model_Name = model,
          Model_Type = "local",
          Recipe_ID = "ensemble",
          Forecast_Tbl = list(final_fcst),
          Model_Fit = list(wflow_fit)
        )

        return(final_return_tbl)
      }

      par_end(inner_cl)

      # ensure at least one model ran successfully
      if (nrow(model_tbl) < 1) {
        stop("All models failed to train")
      }

      # get final combined results and final fitted models
      final_model_fit_tbl <- model_tbl %>%
        tidyr::unite(col = "Model_ID", c("Model_Name", "Model_Type", "Recipe_ID"), sep = "--", remove = FALSE) %>%
        dplyr::select(Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Model_Fit)

      final_ensemble_results_tbl <- model_tbl %>%
        dplyr::select(-Model_Fit) %>%
        tidyr::unnest(Forecast_Tbl) %>%
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

      return(data.frame(Combo = combo))
    } %>%
    base::suppressPackageStartupMessages()

  # clean up any parallel run process
  par_end(cl)

  # check if all time series combos ran correctly
  successful_combos <- list_files(
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
    unique() %>%
    length()

  total_combos <- current_combo_list %>%
    unique() %>%
    length()

  if (successful_combos != total_combos) {
    cli::cli_alert_info(paste0(
      "Not all time series were completed within 'ensemble_models', expected ",
      total_combos, " time series but only ", successful_combos,
      " time series were ran. Some ran into errors."
    ))
    cli::cli_progress_update()
  }
}
