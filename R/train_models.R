#' Train Individual Models
#'
#' @param run_info run info using the [set_run_info()] function
#' @param run_global_models If TRUE, run multivariate models on the entire data
#'   set (across all time series) as a global model. Can be override by
#'   models_not_to_run. Default of NULL runs global models for all date types
#'   except week and day.
#' @param run_local_models If TRUE, run models by individual time series as
#'   local models.
#' @param global_model_recipes Recipes to use in global models.
#' @param feature_selection Implement feature selection before model training
#' @param negative_forecast If TRUE, allow forecasts to dip below zero.
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
#'   Default of NULL uses total amount of cores on machine minus one. Can't be
#'   greater than number of cores on machine minus 1.
#' @param seed Set seed for random number generator. Numeric value.
#'
#' @return trained model outputs are written to disk.
#' @export
#' @examples
#' \donttest{
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(
#'     Date >= "2013-01-01",
#'     Date <= "2015-06-01"
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
#'   num_hyperparameters = 2,
#'   back_test_scenarios = 6,
#'   run_ensemble_models = FALSE
#' )
#'
#' train_models(run_info)
#' }
train_models <- function(run_info,
                         run_global_models = FALSE,
                         run_local_models = TRUE,
                         global_model_recipes = c("R1"),
                         feature_selection = FALSE,
                         negative_forecast = FALSE,
                         parallel_processing = NULL,
                         inner_parallel = FALSE,
                         num_cores = NULL,
                         seed = 123) {
  cli::cli_progress_step("Training Individual Models")

  # check input values
  check_input_type("run_info", run_info, "list")
  check_input_type("run_global_models", run_global_models, c("NULL", "logical"))
  check_input_type("run_local_models", run_local_models, "logical")
  check_input_type("global_model_recipes", global_model_recipes, c("character", "list"))
  check_input_type("num_cores", num_cores, c("NULL", "numeric"))
  check_input_type("seed", seed, "numeric")
  check_parallel_processing(
    run_info,
    parallel_processing,
    inner_parallel
  )

  # get input values
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  combo_variables <- strsplit(log_df$combo_variables, split = "---")[[1]]
  date_type <- log_df$date_type
  forecast_approach <- log_df$forecast_approach
  stationary <- log_df$stationary
  box_cox <- log_df$box_cox
  multistep_horizon <- log_df$multistep_horizon
  forecast_horizon <- log_df$forecast_horizon
  external_regressors <- ifelse(log_df$external_regressors == "NULL", NULL, strsplit(log_df$external_regressors, split = "---")[[1]])

  if (is.null(run_global_models) & date_type %in% c("day", "week")) {
    run_global_models <- FALSE
  } else if (is.null(run_global_models)) {
    run_global_models <- TRUE
  } else {
    # do nothing
  }

  # get model prep info
  model_train_test_tbl <- read_file(run_info,
    path = paste0(
      "/prep_models/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
      "-train_test_split.", run_info$data_output
    ),
    return_type = "df"
  )

  model_workflow_tbl <- read_file(run_info,
    path = paste0(
      "/prep_models/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
      "-model_workflows.", run_info$object_output
    ),
    return_type = "df"
  )

  model_hyperparameter_tbl <- read_file(run_info,
    path = paste0(
      "/prep_models/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
      "-model_hyperparameters.", run_info$object_output
    ),
    return_type = "df"
  )

  # adjust based on models planned to run
  model_workflow_list <- model_workflow_tbl %>%
    dplyr::pull(Model_Name) %>%
    unique()

  global_model_list <- list_global_models()
  fs_model_list <- list_multivariate_models()

  if (sum(model_workflow_list %in% global_model_list) == 0 & run_global_models) {
    run_global_models <- FALSE
    cli::cli_alert_info("Turning global models off since no multivariate models were chosen to run.")
    cli::cli_progress_update()
  }

  # get other time series info
  if (box_cox || stationary) {
    orig_combo_info_tbl <- read_file(run_info,
      path = paste0(
        "/prep_data/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
        "-orig_combo_info.", run_info$data_output
      ),
      return_type = "df"
    )
  }

  # get list of tasks to run
  current_combo_list <- c()

  all_combo_list <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/prep_data/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*R*.", run_info$data_output
    )
  ) %>%
    tibble::tibble(
      Path = .,
      File = fs::path_file(.)
    ) %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Recipe"), sep = "-", remove = TRUE) %>%
    dplyr::pull(Combo) %>%
    unique()

  if (run_local_models) {
    current_combo_list <- all_combo_list
  }

  if (length(all_combo_list) == 1 & run_global_models) {
    run_global_models <- FALSE
    cli::cli_alert_info("Turning global models off since there is only a single time series.")
    cli::cli_progress_update()
  }

  if (run_global_models & length(all_combo_list) > 1) {
    current_combo_list <- c(current_combo_list, hash_data("All-Data"))
  }

  # check if a previous run already has necessary outputs
  prev_combo_tbl <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*.", run_info$data_output
    )
  ) %>%
    tibble::tibble(
      Path = .
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(File = ifelse(is.null(Path), "NA", fs::path_file(Path))) %>%
    dplyr::ungroup() %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Run_Type"), sep = "-", remove = TRUE) %>%
    base::suppressWarnings()

  prev_combo_list <- prev_combo_tbl %>%
    dplyr::filter(Run_Type != paste0("global_models.", run_info$data_output)) %>%
    dplyr::pull(Combo)

  if (sum(unique(prev_combo_tbl$Run_Type) %in% paste0("global_models.", run_info$data_output) == 1)) {
    prev_combo_list <- c(prev_combo_list, hash_data("All-Data"))
  }

  combo_diff <- setdiff(
    current_combo_list,
    prev_combo_list
  )

  current_combo_list_final <- combo_diff %>%
    stringr::str_replace(hash_data("All-Data"), "All-Data")

  if (length(combo_diff) == 0 & length(prev_combo_list) > 0) {
    # check if input values have changed
    current_log_df <- tibble::tibble(
      run_global_models = run_global_models,
      run_local_models = run_local_models,
      global_model_recipes = global_model_recipes,
      feature_selection = feature_selection,
      seed = seed
    ) %>%
      data.frame()

    prev_log_df <- log_df %>%
      dplyr::select(colnames(current_log_df)) %>%
      data.frame()

    if (hash_data(current_log_df) == hash_data(prev_log_df)) {
      cli::cli_alert_info("Individual Models Already Trained")
      return(cli::cli_progress_done())
    } else {
      stop("Inputs have recently changed in 'train_models', please revert back to original inputs or start a new run with 'set_run_info'",
        call. = FALSE
      )
    }
  }

  # parallel run info
  par_info <- par_start(
    run_info = run_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    task_length = length(current_combo_list_final)
  )

  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  # submit tasks
  train_models_tbl <- foreach::foreach(
    x = current_combo_list_final,
    .combine = "rbind",
    .packages = packages,
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .export = c("list_files"),
    .noexport = NULL
  ) %op%
    {
      # get time series
      combo_hash <- x

      model_recipe_tbl <- get_recipe_data(run_info,
        combo = x
      )

      if (combo_hash == "All-Data") {
        model_workflow_tbl <- model_workflow_tbl %>%
          dplyr::filter(
            Model_Name %in% list_global_models(),
            Model_Recipe %in% global_model_recipes
          )
      }

      # get other time series info
      if (box_cox || stationary) {
        if (combo_hash == "All-Data") {
          filtered_combo_info_tbl <- orig_combo_info_tbl
        } else {
          filtered_combo_info_tbl <- orig_combo_info_tbl %>%
            dplyr::filter(Combo_Hash == combo_hash)
        }
      }

      if (inner_parallel) {
        # ensure variables get exported
        model_train_test_tbl <- model_train_test_tbl
        model_workflow_tbl <- model_workflow_tbl
        model_hyperparameter_tbl <- model_hyperparameter_tbl
        seed <- seed
        combo_variables <- combo_variables
        negative_fcst_adj <- negative_fcst_adj
        negative_forecast <- negative_forecast
        stationary <- stationary
        box_cox <- box_cox
        undifference_forecast <- undifference_forecast
        undifference_recipe <- undifference_recipe
        list_global_models <- list_global_models
        list_multivariate_models <- list_multivariate_models
      }

      if (feature_selection) {
        # ensure feature selection objects get exported
        lofo_fn <- lofo_fn
        target_corr_fn <- target_corr_fn
        vip_rf_fn <- vip_rf_fn
        vip_lm_fn <- vip_lm_fn
        vip_cubist_fn <- vip_cubist_fn
        boruta_fn <- boruta_fn
        feature_selection <- feature_selection
        fs_model_list <- fs_model_list
      }

      # run feature selection
      if (feature_selection & sum(unique(model_workflow_tbl$Model_Name) %in% fs_model_list) > 0) {
        fs_list <- list()

        if ("R1" %in% unique(model_workflow_tbl$Model_Recipe)) {
          R1_fs_list <- model_recipe_tbl %>%
            dplyr::filter(Recipe == "R1") %>%
            dplyr::select(Data) %>%
            tidyr::unnest(Data) %>%
            run_feature_selection(
              run_info = run_info,
              train_test_data = model_train_test_tbl,
              parallel_processing = if (inner_parallel) {
                "local_machine"
              } else {
                NULL
              },
              date_type = date_type,
              fast = FALSE,
              forecast_horizon = forecast_horizon,
              external_regressors = external_regressors,
              multistep_horizon = multistep_horizon
            )

          fs_list <- append(fs_list, list(R1 = R1_fs_list))
        }

        if ("R2" %in% unique(model_workflow_tbl$Model_Recipe)) {
          R2_fs_list <- model_recipe_tbl %>%
            dplyr::filter(Recipe == "R2") %>%
            dplyr::select(Data) %>%
            tidyr::unnest(Data) %>%
            run_feature_selection(
              run_info = run_info,
              train_test_data = model_train_test_tbl,
              parallel_processing = if (inner_parallel) {
                "local_machine"
              } else {
                NULL
              },
              date_type = date_type,
              fast = FALSE,
              forecast_horizon = forecast_horizon,
              external_regressors = external_regressors,
              multistep_horizon = FALSE
            )

          fs_list <- append(fs_list, list(R2 = R2_fs_list))
        }
      }

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
        model_run = model_workflow_tbl %>%
          dplyr::select(Model_Name, Model_Recipe) %>%
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

        data_prep_recipe <- model_run %>%
          dplyr::pull(Model_Recipe)

        prep_data <- model_recipe_tbl %>%
          dplyr::filter(Recipe == data_prep_recipe) %>%
          dplyr::select(Data) %>%
          tidyr::unnest(Data)

        workflow <- model_workflow_tbl %>%
          dplyr::filter(
            Model_Name == model,
            Model_Recipe == data_prep_recipe
          ) %>%
          dplyr::select(Model_Workflow)

        workflow <- workflow$Model_Workflow[[1]]

        if (nrow(prep_data) > 500 & model == "xgboost") {
          # update xgboost model to use 'hist' tree method to speed up training
          workflow <- workflows::update_model(workflow,
                                              workflows::extract_spec_parsnip(workflow) %>%
                                                parsnip::set_args(tree_method = "hist"))
        }

        if (combo_hash == "All-Data") {
          # adjust column types to match original data
          prep_data <- adjust_column_types(
            prep_data,
            workflows::extract_recipe(workflow, estimated = FALSE)
          )
        }

        if (feature_selection & model %in% fs_model_list) {
          # update model workflow to only use features from feature selection process
          if (data_prep_recipe == "R1") {
            final_features_list <- fs_list$R1
          } else {
            final_features_list <- fs_list$R2
          }

          if (multistep_horizon & data_prep_recipe == "R1" & model %in% list_multistep_models()) {
            updated_model_spec <- workflow %>%
              workflows::extract_spec_parsnip() %>%
              update(selected_features = final_features_list)

            empty_workflow_final <- workflow %>%
              workflows::update_model(updated_model_spec)
          } else {
            final_features_list <- final_features_list[[paste0("model_lag_", forecast_horizon)]]

            updated_recipe <- workflow %>%
              workflows::extract_recipe(estimated = FALSE) %>%
              recipes::remove_role(tidyselect::everything(), old_role = "predictor") %>%
              recipes::update_role(tidyselect::any_of(unique(c(final_features_list, "Date"))), new_role = "predictor") %>%
              base::suppressWarnings()

            empty_workflow_final <- workflow %>%
              workflows::update_recipe(updated_recipe)
          }
        } else {
          empty_workflow_final <- workflow
        }

        hyperparameters <- model_hyperparameter_tbl %>%
          dplyr::filter(
            Model == model,
            Recipe == data_prep_recipe
          ) %>%
          dplyr::select(Hyperparameter_Combo, Hyperparameters) %>%
          tidyr::unnest(Hyperparameters)

        if (stationary & !(model %in% list_multivariate_models())) {
          # undifference the data for a univariate model
          prep_data <- prep_data %>%
            undifference_recipe(
              filtered_combo_info_tbl,
              model_train_test_tbl %>% dplyr::slice(1) %>% dplyr::pull(Train_End)
            )
        }

        # tune hyperparameters
        set.seed(seed)

        tune_results <- tune::tune_grid(
          object = empty_workflow_final,
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

        finalized_workflow <- tune::finalize_workflow(empty_workflow_final, best_param)

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
          dplyr::select(-.row, -.config)

        # check for future forecast
        if (as.numeric(min(unique(final_fcst$Train_Test_ID))) != 1) {
          stop("model is missing future forecast")
        }

        # undo differencing transformation
        if (stationary & model %in% list_multivariate_models()) {
          if (combo_hash == "All-Data") {
            final_fcst <- final_fcst %>%
              dplyr::group_by(Combo) %>%
              dplyr::group_split() %>%
              purrr::map(function(x) {
                combo <- unique(x$Combo)

                final_fcst_return <- x %>%
                  undifference_forecast(
                    prep_data %>% dplyr::filter(Combo == combo),
                    filtered_combo_info_tbl %>% dplyr::filter(Combo == combo)
                  )

                return(final_fcst_return)
              }) %>%
              dplyr::bind_rows()
          } else {
            final_fcst <- final_fcst %>%
              undifference_forecast(
                prep_data,
                filtered_combo_info_tbl
              )
          }
        }

        # undo box-cox transformation
        if (box_cox) {
          if (combo_hash == "All-Data") {
            final_fcst <- final_fcst %>%
              dplyr::group_by(Combo) %>%
              dplyr::group_split() %>%
              purrr::map(function(x) {
                combo <- unique(x$Combo)

                lambda <- filtered_combo_info_tbl %>%
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
            lambda <- filtered_combo_info_tbl$Box_Cox_Lambda

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
          negative_fcst_adj(negative_forecast)

        # return the forecast
        combo_id <- ifelse(x == "All-Data", "All-Data", unique(final_fcst$Combo))

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

      # write outputs
      fitted_models <- model_tbl %>%
        tidyr::unite(col = "Model_ID", c("Model_Name", "Model_Type", "Recipe_ID"), sep = "--", remove = FALSE) %>%
        dplyr::select(Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID, Model_Fit)

      write_data(
        x = fitted_models,
        combo = unique(fitted_models$Combo_ID),
        run_info = run_info,
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
            run_info = run_info,
            output_type = "data",
            folder = "forecasts",
            suffix = "-global_models"
          )
        }
      } else {
        write_data(
          x = final_forecast_tbl,
          combo = unique(fitted_models$Combo_ID),
          run_info = run_info,
          output_type = "data",
          folder = "forecasts",
          suffix = "-single_models"
        )
      }

      return(data.frame(Combo_Hash = combo_hash))
    } %>%
    base::suppressPackageStartupMessages()

  # clean up any parallel run process
  par_end(cl)

  # check if all time series combos ran correctly
  successful_combo_tbl <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*.", run_info$data_output
    )
  ) %>%
    tibble::tibble(
      Path = .
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(File = ifelse(is.null(Path), "NA", fs::path_file(Path))) %>%
    dplyr::ungroup() %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Run_Type"), sep = "-", remove = TRUE) %>%
    base::suppressWarnings()

  successful_combos <- 0

  if (run_local_models) {
    successful_combos <- successful_combo_tbl %>%
      dplyr::filter(Run_Type != paste0("global_models.", run_info$data_output)) %>%
      dplyr::pull(Combo) %>%
      unique() %>%
      length()
  }

  if (run_global_models) {
    successful_combos <- successful_combos + 1
  }

  total_combos <- current_combo_list %>%
    unique() %>%
    length()

  if (successful_combos != total_combos) {
    stop(
      paste0(
        "Not all time series were completed within 'train_models', expected ",
        total_combos, " time series but only ", successful_combos,
        " time series were ran. ", "Please run 'train_models' again."
      ),
      call. = FALSE
    )
  }

  # update logging file
  log_df <- log_df %>%
    dplyr::mutate(
      run_global_models = run_global_models,
      run_local_models = run_local_models,
      global_model_recipes = paste(unlist(global_model_recipes), collapse = "---"),
      feature_selection = feature_selection,
      seed = seed,
      negative_forecast = negative_forecast,
      inner_parallel = inner_parallel
    )

  write_data(
    x = log_df,
    combo = NULL,
    run_info = run_info,
    output_type = "log",
    folder = "logs",
    suffix = NULL
  )
}

#' Function to convert negative forecasts to zero
#'
#' @param data data frame
#' @param negative_forecast If TRUE, allow forecasts to dip below zero.
#'
#' @return tbl with adjusted
#' @noRd
negative_fcst_adj <- function(data,
                              negative_forecast) {
  fcst_final <- data %>%
    dplyr::mutate(Forecast = ifelse(is.finite(Forecast), Forecast, NA)) %>% # replace infinite values
    dplyr::mutate(Forecast = ifelse(is.nan(Forecast), NA, Forecast)) %>% # replace NaN values
    dplyr::mutate(Forecast = ifelse(is.na(Forecast), 0, Forecast)) # replace NA values

  # convert negative forecasts to zero
  if (negative_forecast == FALSE) {
    fcst_final$Forecast <- replace(
      fcst_final$Forecast,
      which(fcst_final$Forecast < 0),
      0
    )
  }

  return(fcst_final)
}

#' Function to get train test splits in rsample format
#'
#' @param data data frame
#' @param train_test_splits list of finnts train test splits df
#'
#' @return tbl with train test splits
#' @noRd
create_splits <- function(data, train_test_splits) {
  # Create the rsplit object
  analysis_split <- function(data, train_indices, test_indices) {
    rsplit_object <- rsample::make_splits(
      x = list(analysis = train_indices, assessment = test_indices),
      data = data
    )
  }

  # Create a list to store the splits and a vector to store the IDs
  splits <- list()
  ids <- character()

  # Loop over the rows of the split data frame
  for (i in seq_len(nrow(train_test_splits))) {
    # Get the train and test end dates
    train_end <- train_test_splits$Train_End[i]
    test_end <- train_test_splits$Test_End[i]
    train_test_id <- train_test_splits$Train_Test_ID[i]

    # Create the train and test indices
    train_indices <- which(data$Date <= train_end)

    if ("Train_Test_ID" %in% colnames(data)) {
      test_indices <- which(data$Train_Test_ID == train_test_id)
    } else if ("Horizon" %in% colnames(data)) {
      # adjust for the horizon in R2 recipe data
      train_data <- data %>%
        dplyr::filter(
          Horizon == 1,
          Date <= train_end
        )

      test_indices <- which(data$Date > train_end & data$Date <= test_end & data$Origin == max(train_data$Origin) + 1)
    } else {
      test_indices <- which(data$Date > train_end & data$Date <= test_end)
    }

    # Create the split and add it to the list
    splits[[i]] <- analysis_split(data, train_indices, test_indices)

    # Add the ID to the vector
    ids[i] <- as.character(train_test_splits$Train_Test_ID[i])
  }

  # Create the resamples
  resamples <- rsample::manual_rset(splits = splits, ids = ids)

  return(resamples)
}

#' Function to undifference forecast data
#'
#' @param forecast_data forecast data
#' @param recipe_data recipe data
#' @param diff_tbl diff table
#'
#' @return tbl with undifferenced forecast
#' @noRd
undifference_forecast <- function(forecast_data,
                                  recipe_data,
                                  diff_tbl) {
  # check if data needs to be undifferenced
  diff1 <- diff_tbl$Diff_Value1
  diff2 <- diff_tbl$Diff_Value2

  if (is.na(diff1) & is.na(diff2)) {
    return(forecast_data)
  }

  # return df
  return_tbl <- tibble::tibble()

  # train test id number
  train_test_id <- unique(forecast_data$Train_Test_ID)

  # non seasonal differencing
  if (!is.na(diff1)) {
    # loop through each back test split
    for (id in train_test_id) {
      # get specific train test split
      fcst_temp_tbl <- forecast_data %>%
        dplyr::filter(Train_Test_ID == id)

      fcst_start_date <- min(unique(fcst_temp_tbl$Date))

      # prep recipe data
      if ("Horizon" %in% colnames(recipe_data)) {
        filtered_recipe_data <- recipe_data %>%
          dplyr::filter(
            Date < fcst_start_date,
            Horizon == min(unique(recipe_data$Horizon))
          )
      } else {
        filtered_recipe_data <- recipe_data %>%
          dplyr::filter(Date < fcst_start_date)
      }

      # adjust recipe data
      filtered_recipe_data$Target[1] <- NA

      if (!is.na(diff2)) {
        filtered_recipe_data$Target[2] <- NA
      }

      # get number of differences and initial values
      if (!is.na(diff1) & !is.na(diff2)) {
        num_diffs <- 2
        initial_value <- c(diff1, diff2)
      } else {
        num_diffs <- 1
        initial_value <- diff1
      }

      # combine historical data with forecast, then undifference and return forecast
      combined_data <- filtered_recipe_data %>%
        dplyr::mutate(Forecast = Target) %>%
        dplyr::select(Date, Target, Forecast) %>%
        rbind(
          fcst_temp_tbl %>%
            dplyr::select(Date, Target, Forecast)
        ) %>%
        dplyr::arrange(Date)

      if (id == 1) {
        target_tbl <- combined_data %>%
          dplyr::select(-Forecast) %>%
          dplyr::filter(Date < fcst_start_date) %>%
          dplyr::mutate(Target = timetk::diff_inv_vec(Target, difference = num_diffs, initial_values = initial_value))
      } else {
        target_tbl <- combined_data %>%
          dplyr::select(-Forecast) %>%
          dplyr::mutate(Target = timetk::diff_inv_vec(Target, difference = num_diffs, initial_values = initial_value))
      }

      forecast_tbl <- combined_data %>%
        dplyr::select(-Target) %>%
        dplyr::mutate(Forecast = timetk::diff_inv_vec(Forecast, difference = num_diffs, initial_values = initial_value))

      final_forecast <- fcst_temp_tbl %>%
        dplyr::select(-Target, -Forecast) %>%
        dplyr::left_join(forecast_tbl, by = "Date") %>%
        dplyr::left_join(target_tbl, by = "Date")

      return_tbl <- return_tbl %>%
        rbind(final_forecast)
    }
  }

  return(return_tbl)
}

#' Function to undifference recipe data
#'
#' @param recipe_data recipe data
#' @param diff_tbl diff table
#' @param hist_end_date historical data end date
#'
#' @return tbl with undifferenced recipe
#' @noRd
undifference_recipe <- function(recipe_data,
                                diff_tbl,
                                hist_end_date) {
  # check if data needs to be undifferenced
  diff1 <- diff_tbl$Diff_Value1
  diff2 <- diff_tbl$Diff_Value2

  if (is.na(diff1) & is.na(diff2)) {
    return(recipe_data)
  }

  # adjust recipe data
  recipe_data$Target[1] <- NA

  if (!is.na(diff2)) {
    recipe_data$Target[2] <- NA
  }

  # get number of differences and initial values
  if (!is.na(diff1) & !is.na(diff2)) {
    num_diffs <- 2
    initial_value <- c(diff1, diff2)
  } else {
    num_diffs <- 1
    initial_value <- diff1
  }

  # undifference the data
  undiff_recipe_data <- recipe_data %>%
    dplyr::filter(Date <= hist_end_date) %>%
    dplyr::mutate(Target = timetk::diff_inv_vec(Target, difference = num_diffs, initial_values = initial_value))

  future_data <- recipe_data %>%
    dplyr::filter(Date > hist_end_date)

  final_recipe_data <- undiff_recipe_data %>%
    rbind(future_data)

  return(final_recipe_data)
}

#' Function to enforce correct column formatting
#'
#' @param data data
#' @param recipe recipe
#'
#' @return tbl with correct column types
#' @noRd
adjust_column_types <- function(data, recipe) {
  # Extract the required column types from the recipe
  expected_types <- recipe$var_info %>%
    dplyr::select(variable, type) %>%
    dplyr::mutate(type = purrr::map_chr(type, ~ .x[[1]]))

  # Identify and coerce mismatched columns
  for (i in seq_len(nrow(expected_types))) {
    col_name <- expected_types$variable[i]
    expected_type <- expected_types$type[i]

    # Check if column exists and type mismatch
    if (col_name %in% names(data)) {
      actual_type <- class(data[[col_name]])[1]

      # Convert if types are different
      if (expected_type == "string" && actual_type != "character") {
        data[[col_name]] <- as.character(data[[col_name]])
      } else if (expected_type %in% c("numeric", "double") && actual_type != "numeric") {
        data[[col_name]] <- as.numeric(data[[col_name]])
      } else if (expected_type == "date" && actual_type != "Date") {
        data[[col_name]] <- as.Date(data[[col_name]])
      }
    }
  }
  return(data)
}
