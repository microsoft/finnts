#' Select Features
#'
#' @param input_data initial historical data
#' @param run_info run info
#' @param train_test_data train test data
#' @param parallel_processing parallel processing
#' @param date_type date_type
#' @param fast turns off lofo
#' @param seed seed
#' @param forecast_horizon forecast horizon
#' @param external_regressors external reressors
#' @param multistep_horizon multistep horizon forecast
#'
#' @return list of best features to use
#' @noRd
run_feature_selection <- function(input_data,
                                  run_info,
                                  train_test_data,
                                  parallel_processing = NULL,
                                  date_type,
                                  fast = FALSE,
                                  seed = 123,
                                  forecast_horizon,
                                  external_regressors,
                                  multistep_horizon = FALSE) {
  # check for more than one unique target value
  if (input_data %>% tidyr::drop_na(Target) %>% dplyr::pull(Target) %>% unique() %>% length() < 2) {
    # just return the date features
    fs_list <- input_data %>%
      dplyr::select(tidyselect::contains("Date"))

    return(fs_list)
  }

  # check for multiple time series
  if (length(unique(input_data$Combo)) > 1) {
    global <- TRUE
  } else {
    global <- FALSE
  }

  # check for external regressors future values
  future_xregs <- multi_future_xreg_check(
    input_data,
    external_regressors
  )

  # run feature selection
  if (multistep_horizon) {
    initial_lag_periods <- get_lag_periods(NULL, date_type, forecast_horizon, TRUE)

    iteration_list <- get_multi_lags(
      initial_lag_periods,
      forecast_horizon
    )
  } else {
    iteration_list <- (forecast_horizon)
  }

  fs_list_final <- foreach::foreach(
    lag = iteration_list,
    .combine = "c",
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .init = list(),
    .noexport = NULL
  ) %do% {
    # only keep historical data
    if (multistep_horizon) {
      input_data_lag <- multi_feature_selection(input_data,
        future_xregs,
        initial_lag_periods,
        lag,
        target = TRUE
      ) %>%
        tidyr::drop_na(Target)
    } else {
      input_data_lag <- input_data %>%
        tidyr::drop_na(Target)
    }

    # skip lofo if there are too many features
    if (ncol(input_data_lag) > 250) {
      fast <- TRUE
    }

    # run feature selection
    if (date_type %in% c("day", "week") | global) {
      # number of votes needed for feature to be selected
      votes_needed <- 3

      # don't run leave one feature out process for daily, weekly, or global model data
      lofo_results <- tibble::tibble()

      # target correlation
      target_corr_results <- target_corr_fn(input_data_lag, 0.2) %>%
        dplyr::rename(Feature = term) %>%
        dplyr::mutate(
          Vote = 1,
          Auto_Accept = 0
        ) %>%
        dplyr::select(Feature, Vote, Auto_Accept)

      # don't run boruta for daily, weekly, or global model data
      boruta_results <- tibble::tibble()
    } else {
      if (!fast) { # full implementation

        # votes needed for feature to be selected
        votes_needed <- 4

        # run leave one feature out selection
        lofo_results <- tryCatch(
          {
            lofo_fn(
              run_info = run_info,
              data = input_data_lag,
              train_test_splits = train_test_data,
              parallel_processing = parallel_processing,
              seed = seed
            ) %>%
              dplyr::filter(Imp >= 0) %>%
              dplyr::rename(Feature = LOFO_Var) %>%
              dplyr::mutate(
                Vote = 1,
                Auto_Accept = 0
              ) %>%
              dplyr::select(Feature, Vote, Auto_Accept)
          },
          error = function(e) {
            tibble::tibble()
          }
        )

        if (nrow(lofo_results) == 0) {
          votes_needed <- 3
        }
      } else { # fast implementation

        # votes needed for feature to be selected
        votes_needed <- 3

        # don't run lofo
        lofo_results <- tibble::tibble()
      }

      # correlation to target
      target_corr_results <- target_corr_fn(input_data_lag, 0.5) %>%
        dplyr::rename(Feature = term) %>%
        dplyr::mutate(
          Vote = 1,
          Auto_Accept = 0
        ) %>%
        dplyr::select(Feature, Vote, Auto_Accept)

      # botuta feature selection
      boruta_results <- tibble::tibble(
        Feature = boruta_fn(
          data = input_data_lag,
          seed = seed
        ),
        Vote = 1,
        Auto_Accept = 0
      )
    }

    # random forest feature importance
    vip_rf_results <- vip_rf_fn(
      input_data_lag,
      seed
    ) %>%
      dplyr::rename(Feature = Variable) %>%
      dplyr::mutate(
        Vote = 1,
        Auto_Accept = 0
      ) %>%
      dplyr::select(Feature, Vote, Auto_Accept)

    # cubist feature importance
    vip_cubist_results <- tryCatch(
      {
        vip_cubist_fn(
          input_data_lag,
          seed
        ) %>%
          dplyr::rename(Feature = Variable) %>%
          dplyr::mutate(
            Vote = 1,
            Auto_Accept = 0
          ) %>%
          dplyr::select(Feature, Vote, Auto_Accept)
      },
      warning = function(w) {
        # do nothing
      },
      error = function(e) {
        tibble::tibble()
      }
    )

    if (is.null(vip_cubist_results)) {
      votes_needed <- votes_needed - 1
    }

    # lasso regression feature importance
    vip_lm_initial <- vip_lm_fn(
      input_data_lag,
      seed
    )

    missing_cols <- setdiff(
      colnames(input_data_lag %>%
        dplyr::select(-Combo, -Date, -Target)),
      vip_lm_initial$Variable
    )

    cat_cols <- input_data_lag %>%
      dplyr::select_if(is.character) %>%
      dplyr::select(tidyselect::contains(missing_cols)) %>%
      colnames()

    vip_lm_cols <- input_data_lag %>%
      dplyr::select(
        tidyselect::contains(cat_cols),
        tidyselect::any_of(vip_lm_initial$Variable)
      ) %>%
      colnames()

    vip_lm_results <- tibble::tibble(
      Feature = vip_lm_cols,
      Vote = 1,
      Auto_Accept = 1
    )

    # consolidate results and create votes
    final_feature_votes <- rbind(
      target_corr_results,
      vip_rf_results,
      vip_cubist_results,
      vip_lm_results,
      boruta_results,
      lofo_results
    ) %>%
      dplyr::group_by(Feature) %>%
      dplyr::summarise(
        Votes = sum(Vote),
        Auto_Accept = sum(Auto_Accept)
      ) %>%
      dplyr::arrange(desc(Votes))

    # get final selected features list
    fs_list <- final_feature_votes %>%
      dplyr::filter(Votes >= votes_needed | Auto_Accept > 0) %>%
      dplyr::pull(Feature) %>%
      sort()

    element_name <- paste0("model_lag_", lag)

    return(setNames(list(fs_list), element_name))
  }
  return(fs_list_final)
}

#' Target Correlation Filter
#'
#' @param data data
#' @param threshold threshold
#'
#' @return list of features that are correlated to target variable
#' @noRd
target_corr_fn <- function(data,
                           threshold = 0.5) {
  data %>%
    corrr::correlate(quiet = TRUE) %>%
    dplyr::filter(abs(Target) > threshold) %>%
    dplyr::select(term, Target) %>%
    base::suppressWarnings()
}

#' Random Forest Variable Importance
#'
#' @param data data
#' @param seed seed
#'
#' @return list of most important features in random forest model
#' @noRd
vip_rf_fn <- function(data,
                      seed = 123) {
  rf_mod <- parsnip::rand_forest(mode = "regression", trees = 100) %>%
    parsnip::set_engine("ranger", importance = "impurity")

  rf_recipe <-
    recipes::recipe(Target ~ ., data = data %>% dplyr::select(-Date))

  rf_workflow <-
    workflows::workflow() %>%
    workflows::add_model(rf_mod) %>%
    workflows::add_recipe(rf_recipe)

  set.seed(seed)

  ranger_vip_fs <- rf_workflow %>%
    generics::fit(data) %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi() %>%
    dplyr::filter(Importance > 0)

  return(ranger_vip_fs)
}

#' Linear Regression Variable Importance
#'
#' @param data data
#' @param seed seed
#'
#' @return list of most important features in lasso regression model
#' @noRd
vip_lm_fn <- function(data,
                      seed = 123) {
  model_spec_lm <- parsnip::linear_reg(
    penalty = 0.01
  ) %>%
    parsnip::set_engine("glmnet")

  lm_recipe <-
    recipes::recipe(Target ~ ., data = data %>% dplyr::select(-Date, -Combo)) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_numeric_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal())

  lm_workflow <-
    workflows::workflow() %>%
    workflows::add_model(model_spec_lm) %>%
    workflows::add_recipe(lm_recipe)

  set.seed(seed)

  lm_vip_fs <- lm_workflow %>%
    generics::fit(data) %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi() %>%
    dplyr::filter(Importance > 0)

  return(lm_vip_fs)
}

#' Cubist Variable Importance
#'
#' @param data data
#' @param seed seed
#'
#' @return list of most important features in cubist rules model
#' @noRd
vip_cubist_fn <- function(data,
                          seed = 123) {
  model_spec_cubist <- parsnip::cubist_rules(
    mode = "regression",
    committees = 25
  ) %>%
    parsnip::set_engine("Cubist")

  cubist_recipe <-
    recipes::recipe(Target ~ ., data = data %>% dplyr::select(-Date)) %>%
    recipes::step_zv(recipes::all_predictors())

  cubist_workflow <-
    workflows::workflow() %>%
    workflows::add_model(model_spec_cubist) %>%
    workflows::add_recipe(cubist_recipe)

  set.seed(seed)

  cubist_vip_fs <- cubist_workflow %>%
    generics::fit(data) %>%
    workflows::extract_fit_parsnip() %>%
    vip::vi() %>%
    dplyr::filter(Importance > 0)

  return(cubist_vip_fs)
}

#' Feature Selection Using Boruta
#'
#' @param data data
#' @param iterations iterations
#' @param seed seed
#'
#' @return list of most important features in boruta selection process
#' @noRd
boruta_fn <- function(data,
                      iterations = 100,
                      seed = 123) {
  set.seed(seed)
  Boruta::Boruta(Target ~ ., data = data, maxRuns = iterations) %>%
    Boruta::getSelectedAttributes()
}

#' Leave One Feature Out Feature Importance
#'
#' @param run_info run info
#' @param data data
#' @param train_test_splits train test splits
#' @param parallel_processing parallel_processing
#' @param pca pca
#'
#' @return list of most important features using LOFO process
#' @noRd
lofo_fn <- function(run_info,
                    data,
                    train_test_splits,
                    parallel_processing,
                    pca = FALSE,
                    seed = 123) {
  # parallel run info
  par_info <- par_start(
    run_info = run_info,
    parallel_processing = parallel_processing,
    num_cores = NULL,
    task_length = data %>%
      dplyr::select(-Combo, -Target, -Date) %>%
      colnames() %>%
      c("Baseline_Model") %>%
      length()
  )

  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  # submit tasks
  final_results <- foreach::foreach(
    x = data %>%
      dplyr::select(-Combo, -Target, -Date) %>%
      colnames() %>%
      c("Baseline_Model"),
    .combine = "rbind",
    .errorhandling = "remove",
    .packages = packages
  ) %op% {
    col <- x

    if (col == "Baseline_Model") {
      data_lofo <- data
    } else {
      data_lofo <- data %>%
        dplyr::select(-col)
    }

    # xgboost model
    model_spec_xgboost <- parsnip::boost_tree(
      mode = "regression"
    ) %>%
      parsnip::set_engine("xgboost")

    recipe_spec_xgboost <- data_lofo %>%
      get_recipie_configurable(
        rm_date = "with_adj",
        step_nzv = "zv",
        one_hot = TRUE,
        pca = pca
      )

    wflw_spec_tune_xgboost <- get_workflow_simple(
      model_spec_xgboost,
      recipe_spec_xgboost
    )

    # glmnet model
    recipe_spec_glmnet <- data_lofo %>%
      get_recipie_configurable(
        rm_date = "with_adj",
        step_nzv = "zv",
        one_hot = FALSE,
        center_scale = TRUE,
        pca = pca
      )

    model_spec_glmnet <- parsnip::linear_reg(
      mode = "regression",
      penalty = double(1)
    ) %>%
      parsnip::set_engine("glmnet")

    wflw_spec_glmnet <- get_workflow_simple(
      model_spec_glmnet,
      recipe_spec_glmnet
    )

    # cubist model
    recipe_spec_cubist <- data_lofo %>%
      get_recipie_configurable(
        rm_date = "with_adj",
        step_nzv = "nzv",
        one_hot = FALSE,
        pca = pca
      )

    model_spec_cubist <- parsnip::cubist_rules(
      mode = "regression"
    ) %>%
      parsnip::set_engine("Cubist")

    wflw_spec_cubist <- get_workflow_simple(
      model_spec_cubist,
      recipe_spec_cubist
    )

    # run tests
    test_results <- foreach::foreach(
      x = train_test_splits %>%
        dplyr::filter(Run_Type == "Validation") %>%
        dplyr::pull(Train_Test_ID),
      .combine = "rbind"
    ) %do% {
      id <- x

      train_end <- train_test_splits %>%
        dplyr::filter(Train_Test_ID == id) %>%
        dplyr::pull(Train_End)

      test_end <- train_test_splits %>%
        dplyr::filter(Train_Test_ID == id) %>%
        dplyr::pull(Test_End)

      train_data <- data_lofo %>%
        dplyr::filter(Date <= train_end)

      test_data <- data_lofo %>%
        dplyr::filter(
          Date > train_end,
          Date <= test_end
        )

      set.seed(seed)

      xgb_model_fit <- wflw_spec_tune_xgboost %>%
        generics::fit(train_data)

      xgb_fcst <- test_data %>%
        dplyr::bind_cols(
          predict(xgb_model_fit, new_data = test_data)
        ) %>%
        dplyr::mutate(
          Forecast = .pred,
          Train_Test_ID = id,
          LOFO_Var = col
        ) %>%
        dplyr::select(Target, Forecast, Train_Test_ID, LOFO_Var)

      set.seed(seed)

      lr_model_fit <- wflw_spec_glmnet %>%
        generics::fit(train_data)

      lr_fcst <- test_data %>%
        dplyr::bind_cols(
          predict(lr_model_fit, new_data = test_data)
        ) %>%
        dplyr::mutate(
          Forecast = .pred,
          Train_Test_ID = id,
          LOFO_Var = col
        ) %>%
        dplyr::select(Target, Forecast, Train_Test_ID, LOFO_Var)

      set.seed(seed)

      cubist_model_fit <- wflw_spec_cubist %>%
        generics::fit(train_data)

      cubist_fcst <- test_data %>%
        dplyr::bind_cols(
          predict(cubist_model_fit, new_data = test_data)
        ) %>%
        dplyr::mutate(
          Forecast = .pred,
          Train_Test_ID = id,
          LOFO_Var = col
        ) %>%
        dplyr::select(Target, Forecast, Train_Test_ID, LOFO_Var)

      return(rbind(xgb_fcst, lr_fcst, cubist_fcst))
    }

    final_test_results <- test_results %>%
      dplyr::mutate(SE = (Target - Forecast)^2) %>%
      dplyr::group_by(LOFO_Var) %>%
      dplyr::summarise(RMSE = sqrt(mean(SE, na.rm = TRUE)))

    return(final_test_results)
  }

  par_end(cl)

  baseline_rmse <- final_results %>%
    dplyr::filter(LOFO_Var == "Baseline_Model") %>%
    dplyr::pull(RMSE)

  return_tbl <- final_results %>%
    dplyr::rename(Var_RMSE = RMSE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Imp = Var_RMSE - baseline_rmse
    ) %>%
    dplyr::ungroup()

  return(return_tbl)
}
