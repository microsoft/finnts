
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
#'     Date >= "2012-01-01",
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
#'   run_ensemble_models = FALSE
#' )
#'
#' train_models(run_info)
#' }
train_models <- function(run_info,
                         run_global_models = FALSE,
                         run_local_models = TRUE,
                         global_model_recipes = c("R1"),
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
  check_input_type("global_model_recipes", global_model_recipes, "character")
  check_input_type("num_cores", num_cores, c("NULL", "numeric"))
  check_input_type("seed", seed, "numeric")
  check_parallel_processing(run_info, 
                            parallel_processing, 
                            inner_parallel)

  # get input values
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  combo_variables <- strsplit(log_df$combo_variables, split = "---")[[1]]
  date_type <- log_df$date_type
  forecast_approach <- log_df$forecast_approach

  if (is.null(run_global_models) & date_type %in% c("day", "week")) {
    run_global_models <- FALSE
  } else if (forecast_approach != "bottoms_up") {
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

  ml_models <- c(
    "cubist", "glmnet", "mars",
    "svm-poly", "svm-rbf", "xgboost"
  )

  if (sum(model_workflow_list %in% ml_models) == 0 & run_global_models) {
    run_global_models <- FALSE
    cli::cli_alert_info("Turning global models off since no multivariate models were chosen to run.")
    cli::cli_progress_update()
  }

  # get list of tasks to run
  combo_list <- c()

  global_model_list <- c("cubist", "glmnet", "mars", "svm-poly", "svm-rbf", "xgboost")

  if (run_local_models) {
    combo_temp <- list_files(
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

    combo_list <- c(combo_list, combo_temp)
    combo_test <- combo_list
  }

  if (length(combo_list) == 1 & run_global_models) {
    run_global_models <- FALSE
    cli::cli_alert_info("Turning global models off since there is only a single time series.")
    cli::cli_progress_update()
  }

  if (run_global_models & length(combo_list) > 1) {
    combo_test <- c(combo_list, hash_data("All-Data"))
    combo_list <- c(combo_list, "All-Data")
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
      Path = .,
      File = ifelse(is.null(.), "NA", fs::path_file(.))
    ) %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Run_Type"), sep = "-", remove = TRUE) %>%
    base::suppressWarnings()

  prev_combo_list <- prev_combo_tbl %>%
    dplyr::filter(Run_Type != paste0("global_models.", run_info$data_output)) %>%
    dplyr::pull(Combo)

  if (sum(unique(prev_combo_tbl$Run_Type) %in% paste0("global_models.", run_info$data_output) == 1)) {
    prev_combo_list <- c(prev_combo_list, hash_data("All-Data"))
  }

  current_combo_list <- combo_test

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
      model_recipe_tbl <- get_recipe_data(run_info,
        combo = x
      )

      if (inner_parallel) {
        # ensure variables get exported
        model_train_test_tbl <- model_train_test_tbl
        model_workflow_tbl <- model_workflow_tbl
        model_hyperparameter_tbl <- model_hyperparameter_tbl
        seed <- seed
        combo_variables <- combo_variables
        negative_fcst_adj <- negative_fcst_adj
        negative_forecast <- negative_forecast
      }

      # tune models
      tune_iter_list <- model_train_test_tbl %>%
        dplyr::mutate(Combo = x) %>%
        dplyr::filter(Run_Type == "Validation") %>%
        dplyr::select(Combo, Train_Test_ID) %>%
        dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
        purrr::map(.f = function(x) {
          temp <- model_hyperparameter_tbl %>%
            dplyr::select(Hyperparameter_Combo, Model, Recipe) %>%
            dplyr::rename(
              Hyperparameter_ID = Hyperparameter_Combo,
              Recipe_ID = Recipe
            ) %>%
            dplyr::mutate(
              Combo = x$Combo,
              Train_Test_ID = x$Train_Test_ID
            )

          if (x$Combo == "All-Data") {
            temp <- temp %>%
              dplyr::filter(
                Model %in% global_model_list,
                Recipe_ID %in% global_model_recipes
              )
          }

          return(temp)
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::select(Combo, Model, Recipe_ID, Train_Test_ID, Hyperparameter_ID)

      par_info <- par_start(
        run_info = run_info,
        parallel_processing = if (inner_parallel) {
          "local_machine"
        } else {
          NULL
        },
        num_cores = num_cores,
        task_length = nrow(tune_iter_list)
      )

      inner_cl <- par_info$cl
      inner_packages <- par_info$packages
      `%op%` <- par_info$foreach_operator

      initial_tune_tbl <- foreach::foreach(
        x = tune_iter_list %>%
          dplyr::group_split(dplyr::row_number(), .keep = FALSE),
        .combine = "rbind",
        .packages = inner_packages,
        .errorhandling = "remove",
        .verbose = FALSE,
        .inorder = FALSE,
        .multicombine = TRUE,
        .noexport = NULL
      ) %op%
        {

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
            dplyr::filter(Train_Test_ID == data_split) %>%
            dplyr::pull(Train_End)

          test_end_date <- model_train_test_tbl %>%
            dplyr::filter(Train_Test_ID == data_split) %>%
            dplyr::pull(Test_End)

          # get train/test data
          full_data <- model_recipe_tbl %>%
            dplyr::filter(Recipe == data_prep_recipe) %>%
            dplyr::select(Data) %>%
            tidyr::unnest(Data)

          if (combo == "All-Data") {
            full_data <- full_data %>%
              tidyr::separate(
                col = Combo,
                into = combo_variables,
                sep = "---",
                remove = FALSE
              )
          }

          training <- full_data %>%
            dplyr::filter(Date <= train_end_date)

          testing <- full_data %>%
            dplyr::filter(
              Date > train_end_date,
              Date <= test_end_date
            )

          if (data_prep_recipe == "R2") {
            train_origin_max <- training %>%
              dplyr::filter(Horizon == 1)

            testing <- testing %>%
              dplyr::filter(Origin == max(train_origin_max$Origin) + 1)
          }

          # get workflow
          workflow <- model_workflow_tbl %>%
            dplyr::filter(
              Model_Name == model,
              Model_Recipe == data_prep_recipe
            )

          workflow_final <- workflow$Model_Workflow[[1]]

          # get hyperparameters
          hyperparameters <- model_hyperparameter_tbl %>%
            dplyr::filter(
              Model == model,
              Recipe == data_prep_recipe,
              Hyperparameter_Combo == param_combo
            ) %>%
            dplyr::select(Hyperparameters) %>%
            tidyr::unnest(Hyperparameters)

          # fit model
          set.seed(seed)

          if (nrow(hyperparameters) > 0) {
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
            dplyr::rename(Forecast = .pred) %>%
            negative_fcst_adj(negative_forecast)

          # finalize output tbl
          final_tbl <- tibble::tibble(
            Combo_ID = combo,
            Model_Name = model,
            Model_Type = ifelse(combo == "All-Data", "global", "local"),
            Recipe_ID = data_prep_recipe,
            Train_Test_ID = data_split,
            Hyperparameter_ID = param_combo,
            Prediction = list(model_prediction)
          )

          return(final_tbl)
        } %>%
        base::suppressPackageStartupMessages()

      par_end(inner_cl)

      best_param <- initial_tune_tbl %>%
        tidyr::unnest(Prediction) %>%
        dplyr::mutate(SE = (Target - Forecast)^2) %>%
        dplyr::group_by(Combo_ID, Model_Name, Model_Type, Recipe_ID, Hyperparameter_ID) %>%
        dplyr::summarise(RMSE = sqrt(mean(SE, na.rm = TRUE))) %>%
        dplyr::arrange(RMSE) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()

      model_tune_tbl <- initial_tune_tbl %>%
        dplyr::select(Model_Name, Model_Type, Recipe_ID, Hyperparameter_ID, Train_Test_ID, Prediction) %>%
        dplyr::right_join(best_param, by = c("Model_Name", "Model_Type", "Recipe_ID", "Hyperparameter_ID")) %>%
        tidyr::unnest(Prediction) %>%
        dplyr::mutate(
          Combo_Hash = Combo_ID,
          Combo_ID = ifelse(Combo_ID == "All-Data", "All-Data", Combo)
        ) %>%
        dplyr::select(Combo_Hash, Combo_ID, Model_Name, Model_Type, Recipe_ID, Train_Test_ID, Hyperparameter_ID, Combo, Date, Forecast, Target)

      # refit models
      refit_iter_list <- model_train_test_tbl %>%
        dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test", "Ensemble")) %>%
        dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
        purrr::map(.f = function(x) {
          model_tune_tbl %>%
            dplyr::mutate(
              Run_Type = x %>% dplyr::pull(Run_Type),
              Train_Test_ID = x %>% dplyr::pull(Train_Test_ID),
              Train_End = x %>% dplyr::pull(Train_End),
              Test_End = x %>% dplyr::pull(Test_End)
            ) %>%
            dplyr::select(
              Combo_ID, Run_Type, Train_Test_ID, Recipe_ID,
              Hyperparameter_ID, Train_End, Test_End, Model_Name, Model_Type
            ) %>%
            dplyr::distinct()
        }) %>%
        dplyr::bind_rows()

      par_info <- par_start(
        run_info = run_info,
        parallel_processing = if (inner_parallel) {
          "local_machine"
        } else {
          NULL
        },
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
        .errorhandling = "remove",
        .verbose = FALSE,
        .inorder = FALSE,
        .multicombine = TRUE,
        .noexport = NULL
      ) %op%
        {
          combo <- x %>%
            dplyr::pull(Combo_ID)

          model <- x %>%
            dplyr::pull(Model_Name)

          recipe <- x %>%
            dplyr::pull(Recipe_ID)

          param <- x %>%
            dplyr::pull(Hyperparameter_ID)

          run_type <- x %>%
            dplyr::pull(Run_Type)

          run_id <- x %>%
            dplyr::pull(Train_Test_ID)

          train_end <- x %>%
            dplyr::pull(Train_End)

          test_end <- x %>%
            dplyr::pull(Test_End)

          if (combo != "All-Data") {
            recipe_data <- model_recipe_tbl %>%
              dplyr::filter(
                Recipe == recipe,
              ) %>%
              dplyr::select(Data) %>%
              tidyr::unnest(Data)
          } else {
            recipe_data <- model_recipe_tbl %>%
              dplyr::filter(Recipe == recipe) %>%
              dplyr::select(Data) %>%
              tidyr::unnest(Data) %>%
              tidyr::separate(
                col = Combo,
                into = combo_variables,
                sep = "---",
                remove = FALSE
              )
          }

          training <- recipe_data %>%
            dplyr::filter(Date <= train_end)

          testing <- recipe_data %>%
            dplyr::filter(
              Date > train_end,
              Date <= test_end
            )

          if (recipe == "R2") {
            train_origin_max <- training %>%
              dplyr::filter(Horizon == 1)

            testing <- testing %>%
              dplyr::filter(Origin == max(train_origin_max$Origin) + 1)
          }

          # get workflow
          workflow <- model_workflow_tbl %>%
            dplyr::filter(
              Model_Name == model,
              Model_Recipe == recipe
            )

          workflow_final <- workflow$Model_Workflow[[1]]

          # get hyperparameters
          hyperparameters <- model_hyperparameter_tbl %>%
            dplyr::filter(
              Model == model,
              Recipe == recipe,
              Hyperparameter_Combo == param
            ) %>%
            dplyr::select(Hyperparameters) %>%
            tidyr::unnest(Hyperparameters)

          # fit model
          set.seed(seed)

          if (nrow(hyperparameters) > 0) {
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
            dplyr::rename(Forecast = .pred) %>%
            negative_fcst_adj(negative_forecast)

          # finalize output tbl
          if (run_id == 1) {
            model_fit <- model_fit
          } else {
            model_fit <- NULL
          }

          final_tbl <- tibble::tibble(
            Combo_ID = combo,
            Model_Name = model,
            Model_Type = ifelse(combo == "All-Data", "global", "local"),
            Recipe_ID = recipe,
            Train_Test_ID = run_id,
            Hyperparameter_ID = param,
            Model_Fit = list(model_fit),
            Prediction = list(model_prediction)
          )

          return(final_tbl)
        } %>%
        base::suppressPackageStartupMessages()

      par_end(inner_cl)

      # write outputs
      fitted_models <- refit_tbl %>%
        dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID)) %>%
        dplyr::filter(Train_Test_ID == 1) %>%
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

      final_forecast_tbl <- refit_tbl %>%
        dplyr::select(-Model_Fit) %>%
        tidyr::unnest(Prediction) %>%
        rbind(model_tune_tbl %>%
          dplyr::select(-Combo_Hash)) %>%
        dplyr::arrange(Train_Test_ID) %>%
        tidyr::unite(col = "Model_ID", c("Model_Name", "Model_Type", "Recipe_ID"), sep = "--", remove = FALSE) %>%
        dplyr::group_by(Combo_ID, Model_ID, Train_Test_ID) %>%
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

      return(tibble::tibble())
    } %>%
    base::suppressPackageStartupMessages()

  # clean up any parallel run process
  par_end(cl)

  # update logging file
  log_df <- log_df %>%
    dplyr::mutate(
      run_global_models = run_global_models,
      run_local_models = run_local_models,
      global_model_recipes = paste(unlist(global_model_recipes), collapse = "---"),
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
