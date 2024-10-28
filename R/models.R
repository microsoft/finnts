#' List all available models
#'
#' @return list of models
#' @export
list_models <- function() {
  list <- c(
    "arima", "arima-boost", "arimax", "cubist", "croston", "ets", "glmnet", "mars", "meanf",
    "nnetar", "nnetar-xregs", "prophet", "prophet-boost", "prophet-xregs", "snaive",
    "stlm-arima", "stlm-ets", "svm-poly", "svm-rbf", "tbats", "theta", "xgboost"
  )

  return(list)
}

#' List models with hyperparameters
#'
#'
#' @return list of models
#' @noRd
list_hyperparmater_models <- function() {
  list <- c(
    "arima-boost", "cubist", "glmnet", "mars",
    "nnetar", "nnetar-xregs", "prophet", "prophet-boost",
    "prophet-xregs", "svm-poly", "svm-rbf", "xgboost"
  )

  return(list)
}

#' List ensemble models
#'
#'
#' @return list of models
#' @noRd
list_ensemble_models <- function() {
  list <- c(
    "cubist", "glmnet", "svm-poly", "svm-rbf", "xgboost"
  )

  return(list)
}

#' List models capable with R2 recipe
#'
#'
#' @return list of models
#' @noRd
list_r2_models <- function() {
  list <- c("cubist", "glmnet", "svm-poly", "svm-rbf", "xgboost")

  return(list)
}

#' List global models
#'
#'
#' @return list of models
#' @noRd
list_global_models <- function() {
  list <- c("xgboost")

  return(list)
}

#' List multivariate models
#'
#'
#' @return list of models
#' @noRd
list_multivariate_models <- function() {
  list <- c(
    "cubist", "glmnet", "mars", "svm-poly", "svm-rbf", "xgboost",
    "arima-boost", "arimax", "prophet-boost", "prophet-xregs",
    "nnetar-xregs"
  )

  return(list)
}

#' List multistep models
#'
#'
#' @return list of models
#' @noRd
list_multistep_models <- function() {
  list <- c(
    "cubist", "glmnet", "mars", "svm-poly", "svm-rbf", "xgboost"
  )

  return(list)
}

#' Gets a simple recipe
#'
#' @param train_data Training Data
#'
#' @return simple recipe
#' @noRd
get_recipe_simple <- function(train_data) {
  recipes::recipe(Target ~ Date, data = train_data %>% dplyr::select(-Combo))
}

#' Gets a recipe with combo
#'
#' @param train_data Training Data
#'
#' @return combo recipe
#' @noRd
get_recipe_combo <- function(train_data) {
  recipes::recipe(Target ~ Date + Combo, data = train_data)
}

#' Gets a recipe that adjusts based on parameters
#'
#' @param train_data Training Data
#' @param mutate_adj_half parameter to add date adjustment
#' @param rm_date "plain", "with_adj", "with_adj_index"
#' @param step_nzv (Zero or NearZero Variance) "zv", "nzv", or none
#' @param norm_date_adj_year normalize date & year
#' @param dummy_one_hot one hot encoding
#' @param character_factor is character factor
#' @param center_scale Center and scale
#' @param one_hot True or False
#' @param pca pca
#' @return configurable recipe
#' @noRd

get_recipe_configurable <- function(train_data,
                                    mutate_adj_half = FALSE,
                                    rm_date = "plain",
                                    step_nzv = "zv",
                                    norm_date_adj_year = FALSE,
                                    dummy_one_hot = TRUE,
                                    character_factor = FALSE,
                                    center_scale = FALSE,
                                    one_hot = FALSE,
                                    pca = TRUE,
                                    corr = FALSE,
                                    lincomb = FALSE) {
  mutate_adj_half_fn <- function(df) {
    if (mutate_adj_half) {
      df %>%
        recipes::step_mutate(
          Date_half_factor = as.factor(Date_half),
          Date_quarter_factor = as.factor(Date_quarter),
          id = "step_mutate_adj_half"
        )
    } else {
      df
    }
  }

  rm_date_fn <- function(df) {
    switch(rm_date,
      "with_adj" = df %>%
        recipes::step_rm(Date),
      "with_adj_index" = df %>%
        recipes::step_rm(Date, Date_index.num, id = "step_remove_date"),
      df,
      "none" = df
    )
  }

  corr_fn <- function(df) {
    if (corr) {
      df %>%
        recipes::step_corr(recipes::all_numeric_predictors(), threshold = .5, id = "remove_correlated_vars")
    } else {
      df
    }
  }

  step_nz_fn <- function(df) {
    switch(step_nzv,
      "zv" = df %>%
        recipes::step_zv(recipes::all_predictors(), id = "step_zv"),
      "nzv" = df %>%
        recipes::step_nzv(recipes::all_predictors(), id = "step_nzv"),
      df
    )
  }

  norm_date_adj_year_fn <- function(df) {
    if (norm_date_adj_year) {
      df %>%
        recipes::step_normalize(Date_index.num, Date_year, id = "step_normalize_date")
    } else {
      df
    }
  }

  dummy_one_hot_fn <- function(df) {
    if (dummy_one_hot) {
      df %>%
        recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = one_hot, id = "step_dummy")
    } else {
      df
    }
  }

  character_factor_fn <- function(df) {
    if (character_factor) {
      df %>%
        recipes::step_mutate_at(where(is.character), fn = ~ as.factor(.), id = "step_char_conv")
    } else {
      df
    }
  }

  center_scale_fn <- function(df) {
    if (center_scale) {
      df %>%
        recipes::step_center(recipes::all_numeric_predictors(), id = "step_center") %>%
        recipes::step_scale(recipes::all_numeric_predictors(), id = "step_scale")
    } else {
      df
    }
  }

  pca_fn <- function(df) {
    if (pca) {
      df %>%
        recipes::step_pca(tidyselect::contains("lag"), threshold = .99, options = list(center = !center_scale, scale. = !center_scale), id = "step_pca")
    } else {
      df
    }
  }

  rm_lincomb_fn <- function(df) {
    if (lincomb) {
      df %>%
        recipes::step_lincomb(recipes::all_numeric_predictors(), id = "remove_linear_combs")
    } else {
      df
    }
  }

  recipes::recipe(Target ~ ., data = train_data %>% dplyr::select(-Combo)) %>%
    mutate_adj_half_fn() %>%
    step_nz_fn() %>%
    rm_date_fn() %>%
    norm_date_adj_year_fn() %>%
    dummy_one_hot_fn() %>%
    character_factor_fn() %>%
    center_scale_fn() %>%
    pca_fn() %>%
    rm_lincomb_fn() %>%
    corr_fn()
}


#' Gets a simple workflow from model
#'
#' @param model_spec Model Spec A
#' @param recipe_spec year, quarter, month, week, day
#'
#' @return dplyr workflow spec
#' @noRd
get_workflow_simple <- function(model_spec,
                                recipe_spec) {
  workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(recipe_spec)
}

#' Gets a simple fit
#'
#' @param train_data Training Data
#' @param wflw_spec Worflow Spec
#'
#' @return simple recipe
#' @noRd
get_fit_simple <- function(train_data,
                           wflw_spec) {
  wflw_spec %>%
    generics::fit(train_data %>%
      dplyr::select(-Combo))
}

#' Gets a simple fit
#'
#' @param train_data Training Data
#' @param tune_results Tune results
#' @param wflw_spec_tune Worflow Spec after tuning
#'
#' @return simple recipe
#' @noRd
get_fit_wkflw_best <- function(train_data,
                               tune_results,
                               wflw_spec_tune) {
  best_results <- tune_results %>%
    tune::show_best(metric = "rmse", n = 10)

  wflw_spec_tune %>%
    tune::finalize_workflow(parameters = best_results %>%
      dplyr::slice(1)) %>%
    generics::fit(train_data %>%
      dplyr::select(-Combo))
}

#' Gets a simple fit
#'
#' @param train_data Training Data
#' @param model_spec Model Spec
#' @param recipe_spec Recipe Spec
#'
#' @return simple recipe
#' @noRd
get_fit_wkflw_nocombo <- function(train_data,
                                  model_spec,
                                  recipe_spec) {
  get_workflow_simple(model_spec, recipe_spec) %>%
    generics::fit(train_data)
}

#' Get resample Time Series CV
#'
#' @param train_data Training Data
#' @param tscv_initial TS Cross Validation Initialization
#' @param horizon Horizon
#' @param back_test_spacing Back Testing Spacing
#'
#' @return gives the resample TS CV object
#' @noRd
get_resample_tscv <- function(train_data,
                              tscv_initial,
                              horizon,
                              back_test_spacing) {
  timetk::time_series_cv(
    data = train_data,
    initial = tscv_initial,
    assess = horizon,
    skip = back_test_spacing,
    cumulative = TRUE,
    slice_limit = 100
  )
}

#' Get resample kFold CV
#'
#' @param train_data Training Data
#'
#' @return gives the resample kFold CV object
#' @noRd
get_resample_kfold <- function(train_data) {
  train_data %>%
    rsample::vfold_cv(v = 5)
}

#' Get tuning grid with resample
#'
#' @param train_data Training Data
#' @param wkflw Workflow
#' @param isBoost isBoost params
#' @param resamples Resamples
#' @param parallel Parallel
#' @param isMetrics is metrics
#'
#' @noRd
get_tune_grid <- function(train_data,
                          wkflw,
                          isBoost,
                          resamples,
                          parallel,
                          isMetrics) {
  dial_by_boost <- function(wkflw, isBoost) {
    params <- workflows::extract_parameter_set_dials(wkflw)

    if (isBoost) {
      params %>% stats::update(learn_rate = dials::learn_rate(
        range = c(0.15, 0.5),
        trans = NULL
      ))
    } else {
      params
    }
  }

  tgCall <- list()
  tgCall$object <- wkflw
  tgCall$resamples <- resamples
  tgCall$param_info <- dial_by_boost(wkflw, isBoost)
  tgCall$grid <- 10
  tgCall$control <- tune::control_grid(
    verbose = FALSE,
    allow_par = parallel,
    parallel_over = NULL,
    pkgs = get_export_packages()
  )

  if (isMetrics) {
    tgCall$metrics <- modeltime::default_forecast_accuracy_metric_set()
  }

  do.call(tune::tune_grid, tgCall, envir = globalenv())
}

#' Get tuning grid with resample
#'
#' @param train_data Training Data
#' @param tscv_initial TS Cross Validation Initialization
#' @param horizon Horizon
#' @param back_test_spacing Back Testing Spacing
#' @param wkflw Workflow Objet from previous stage
#' @param parallel Allow Parallal (Default False)
#' @param isBoost Add Boost
#' @param isMetrics Add Metrics
#'
#' @return gives the model fit
#' @noRd
get_resample_tune_grid <- function(train_data,
                                   tscv_initial,
                                   horizon,
                                   back_test_spacing,
                                   wkflw,
                                   parallel = FALSE,
                                   isBoost = FALSE,
                                   isMetrics = FALSE) {
  resamples_tscv <- train_data %>%
    get_resample_tscv(
      tscv_initial,
      horizon,
      back_test_spacing
    )

  train_data %>%
    get_tune_grid(
      wkflw,
      isBoost,
      resamples_tscv,
      parallel,
      isMetrics
    )
}

#' Get tuning grid k fold CV
#'
#' @param train_data Training Data
#' @param wkflw Workflow Object from previous stage
#' @param parallel Allow Parallel (Default False)
#'
#' @return gives the model fit
#' @noRd
get_kfold_tune_grid <- function(train_data,
                                wkflw,
                                parallel = FALSE) {
  resamples_cv <- train_data %>%
    get_resample_kfold()


  train_data %>%
    get_tune_grid(wkflw,
      isBoost = FALSE,
      resamples_cv,
      parallel,
      isMetrics = FALSE
    )
}

#' Get grid_latin_hypercube
#'
#' @param model_spec Model Spec Obj
#'
#' @return gives the latin hypercube grid
#' @noRd
get_latin_hypercube_grid <- function(model_spec) {
  dials::grid_latin_hypercube(
    dials::parameters(model_spec),
    size = 10
  )
}


#' ARIMA Model
#'
#' @param train_data Training Data
#' @param frequency Frequency of Data
#'
#' @return Get the ARIMA based model
#' @noRd
arima <- function(train_data,
                  frequency) {
  recipe_simple <- train_data %>%
    get_recipe_simple()

  model_spec_arima <- modeltime::arima_reg(
    seasonal_period = frequency
  ) %>%
    parsnip::set_engine("auto_arima")

  wflw_spec <- get_workflow_simple(
    model_spec_arima,
    recipe_simple
  )

  return(wflw_spec)
}


#' ARIMAX Model
#'
#' @param train_data Training Data
#' @param frequency Frequency of Data
#'
#' @return Get the ARIMAX based model
#' @noRd
arimax <- function(train_data,
                   frequency,
                   pca) {
  recipe_spec_arimax <- train_data %>%
    get_recipe_configurable(
      step_nzv = "zv",
      dummy_one_hot = TRUE,
      corr = TRUE,
      pca = pca,
      lincomb = TRUE
    )
  model_spec_arima <- modeltime::arima_reg(
    seasonal_period = frequency
  ) %>%
    parsnip::set_engine("auto_arima")

  wflw_spec <- get_workflow_simple(
    model_spec_arima,
    recipe_spec_arimax
  )

  return(wflw_spec)
}

#' ARIMA Boost Model
#'
#' @param train_data Training Data
#' @param frequency Frequency of Data
#' @param pca pca
#'
#' @return Get the ARIMA based model
#' @noRd
arima_boost <- function(train_data,
                        frequency,
                        pca) {
  recipe_spec_arima_boost <- train_data %>%
    get_recipe_configurable(
      step_nzv = "zv",
      norm_date_adj_year = TRUE,
      one_hot = TRUE,
      pca = pca
    )

  # create model spec
  model_spec_arima_boost_tune <- modeltime::arima_boost(
    mode            = "regression",
    seasonal_period = frequency,
    mtry            = tune::tune(),
    trees           = tune::tune(),
    min_n           = tune::tune(),
    tree_depth      = tune::tune(),
    learn_rate      = tune::tune(),
    loss_reduction  = tune::tune()
  ) %>%
    parsnip::set_engine("auto_arima_xgboost")


  wflw_spec_arima_boost <- get_workflow_simple(
    model_spec_arima_boost_tune,
    recipe_spec_arima_boost
  )

  return(wflw_spec_arima_boost)
}

#' Cubist Function
#'
#' @param train_data train data
#' @param model_type single or ensemble model
#' @param pca pca
#' @param multistep multistep horizon
#' @param horizon horizon
#' @param external_regressors external regressors
#' @param frequency frequency
#'
#' @return Get the cubist model
#' @noRd
cubist <- function(train_data,
                   pca,
                   multistep,
                   horizon,
                   external_regressors,
                   frequency) {
  if (multistep) {
    recipe_spec_cubist <- train_data %>%
      get_recipe_configurable(
        rm_date = "none",
        step_nzv = "nzv",
        one_hot = FALSE,
        pca = pca
      )

    model_spec_cubist <- cubist_multistep(
      mode = "regression",
      committees = tune::tune(),
      neighbors = tune::tune(),
      max_rules = tune::tune(),
      forecast_horizon = horizon,
      external_regressors = external_regressors,
      lag_periods = get_lag_periods(NULL, get_date_type(frequency), horizon, TRUE)
    ) %>%
      parsnip::set_engine("cubist_multistep_horizon")
  } else {
    recipe_spec_cubist <- train_data %>%
      get_recipe_configurable(
        rm_date = "with_adj",
        step_nzv = "nzv",
        one_hot = FALSE,
        pca = pca
      )

    model_spec_cubist <- parsnip::cubist_rules(
      mode = "regression",
      committees = tune::tune(),
      neighbors = tune::tune(),
      max_rules = tune::tune()
    ) %>%
      parsnip::set_engine("Cubist")
  }

  wflw_spec_cubist <- get_workflow_simple(
    model_spec_cubist,
    recipe_spec_cubist
  )

  return(wflw_spec_cubist)
}

#' Croston Model
#'
#' @param train_data input data
#' @param frequency Frequency of Data
#'
#' @return Get the Croston model
#' @noRd
croston <- function(train_data,
                    frequency) {
  recipe_simple <- train_data %>%
    get_recipe_simple()

  model_spec_croston <- modeltime::exp_smoothing(
    seasonal_period = frequency
  ) %>%
    parsnip::set_engine("croston")

  wflw_spec <- get_workflow_simple(
    model_spec_croston,
    recipe_simple
  )

  return(wflw_spec)
}

#' ETS Model
#'
#' @param train_data input data
#' @param frequency Frequency of Data
#'
#' @return Get the ETS model
#' @noRd
ets <- function(train_data,
                frequency) {
  recipe_simple <- train_data %>%
    get_recipe_simple()

  model_spec_ets <- modeltime::exp_smoothing(
    error = "auto",
    trend = "auto",
    season = "auto",
    seasonal_period = frequency
  ) %>%
    parsnip::set_engine("ets")

  wflw_spec <- get_workflow_simple(
    model_spec_ets,
    recipe_simple
  )

  return(wflw_spec)
}

#' GLM Net Function
#'
#' @param train_data input data
#' @param pca pca
#' @param multistep multistep horizon
#' @param horizon horizon
#' @param external_regressors external regressors
#' @param frequency frequency
#'
#' @return Get the GLM Net model
#' @noRd
glmnet <- function(train_data,
                   pca,
                   multistep,
                   horizon,
                   external_regressors,
                   frequency) {
  # create model recipe and spec
  if (multistep) {
    recipe_spec_glmnet <- train_data %>%
      get_recipe_configurable(
        rm_date = "none",
        step_nzv = "zv",
        one_hot = FALSE,
        center_scale = TRUE,
        pca = pca
      )

    model_spec_glmnet <- glmnet_multistep(
      mode = "regression",
      penalty = tune::tune(),
      mixture = tune::tune(),
      forecast_horizon = horizon,
      external_regressors = external_regressors,
      lag_periods = get_lag_periods(NULL, get_date_type(frequency), horizon, TRUE)
    ) %>%
      parsnip::set_engine("glmnet_multistep_horizon")
  } else {
    recipe_spec_glmnet <- train_data %>%
      get_recipe_configurable(
        rm_date = "with_adj",
        step_nzv = "zv",
        one_hot = FALSE,
        center_scale = TRUE,
        pca = pca
      )

    model_spec_glmnet <- parsnip::linear_reg(
      mode = "regression",
      penalty = tune::tune(),
      mixture = tune::tune()
    ) %>%
      parsnip::set_engine("glmnet")
  }

  # create final model workflow
  wflw_spec_glmnet <- get_workflow_simple(
    model_spec_glmnet,
    recipe_spec_glmnet
  )

  return(wflw_spec_glmnet)
}

#' MARS Model Spec
#'
#' @param train_data input data
#' @param model_type single or ensemble
#' @param pca pca
#' @param multistep multistep horizon
#' @param horizon horizon
#' @param external_regressors external regressors
#' @param frequency frequency
#'
#' @return Get the Mars model spec
#' @noRd
mars <- function(train_data,
                 pca,
                 multistep,
                 horizon,
                 external_regressors,
                 frequency) {
  if (multistep) {
    recipe_spec_mars <- train_data %>%
      get_recipe_configurable(
        rm_date = "none",
        pca = pca
      )

    model_spec_mars <- mars_multistep(
      mode = "regression",
      num_terms = tune::tune(),
      prod_degree = tune::tune(),
      prune_method = tune::tune(),
      forecast_horizon = horizon,
      external_regressors = external_regressors,
      lag_periods = get_lag_periods(NULL, get_date_type(frequency), horizon, TRUE)
    ) %>%
      parsnip::set_engine("mars_multistep_horizon")
  } else {
    recipe_spec_mars <- train_data %>%
      get_recipe_configurable(
        rm_date = "with_adj",
        pca = pca
      )

    model_spec_mars <- parsnip::mars(
      mode = "regression",
      num_terms = tune::tune(),
      prod_degree = tune::tune(),
      prune_method = tune::tune()
    ) %>%
      parsnip::set_engine("earth")
  }

  wflw_spec_mars <- get_workflow_simple(
    model_spec_mars,
    recipe_spec_mars
  )

  return(wflw_spec_mars)
}

#' Mean Forecast
#'
#' @param train_data input data
#' @param frequency Frequency of Data
#'
#' @return Get Mean Forecast Model
#' @noRd
meanf <- function(train_data,
                  frequency) {
  recipe_spec_meanf <- train_data %>%
    get_recipe_simple()

  model_spec_meanf <- modeltime::window_reg(
    window_size = round(frequency)
  ) %>%
    parsnip::set_engine(
      engine = "window_function",
      window_function = mean,
      na.rm = TRUE
    )

  wflw_spec_meanf <- get_workflow_simple(
    model_spec_meanf,
    recipe_spec_meanf
  )

  return(wflw_spec_meanf)
}

#' nnetar model
#'
#' @param train_data input data
#' @param horizon horizon
#' @param frequency Frequency of Data
#'
#' @return Get nnetar Model
#' @noRd
nnetar <- function(train_data,
                   horizon,
                   frequency) {
  recipe_spec_nnetar <- train_data %>%
    get_recipe_simple()

  model_spec_nnetar <- modeltime::nnetar_reg(
    seasonal_period = frequency,
    non_seasonal_ar = tune::tune(id = "non_seasoanl_ar"),
    seasonal_ar = tune::tune(),
    hidden_units = tune::tune(),
    num_networks = tune::tune(),
    penalty = tune::tune(),
    epochs = tune::tune()
  ) %>%
    parsnip::set_engine("nnetar")

  wflw_tune_nnetar <- get_workflow_simple(
    model_spec_nnetar,
    recipe_spec_nnetar
  )

  return(wflw_tune_nnetar)
}

#' nnetar xregs model
#'
#' @param train_data input data
#' @param frequency Frequency of Data
#' @param pca pca
#'
#' @return Get nnetar xregs Model
#' @noRd
nnetar_xregs <- function(train_data,
                         frequency,
                         pca) {
  recipe_spec_nnetar <- train_data %>%
    get_recipe_configurable(
      norm_date_adj_year = TRUE,
      one_hot = TRUE,
      pca = pca
    )

  model_spec_nnetar <- modeltime::nnetar_reg(
    seasonal_period = frequency,
    non_seasonal_ar = tune::tune(id = "non_seasoanl_ar"),
    seasonal_ar = tune::tune(),
    hidden_units = tune::tune(),
    num_networks = tune::tune(),
    penalty = tune::tune(),
    epochs = tune::tune()
  ) %>%
    parsnip::set_engine("nnetar")

  wflw_tune_nnetar <- get_workflow_simple(
    model_spec_nnetar,
    recipe_spec_nnetar
  )

  return(wflw_tune_nnetar)
}

#' prophet model
#'
#' @param train_data input data
#'
#' @return Get prophet Model
#' @noRd
prophet <- function(train_data) {
  recipe_spec_prophet <- train_data %>%
    get_recipe_simple()

  model_spec_prophet <- modeltime::prophet_reg(
    growth = tune::tune(),
    changepoint_num = tune::tune(),
    changepoint_range = tune::tune(),
    seasonality_yearly = tune::tune(),
    seasonality_weekly = tune::tune(),
    seasonality_daily = tune::tune(),
    prior_scale_changepoints = tune::tune(),
    prior_scale_seasonality = tune::tune()
  ) %>%
    parsnip::set_engine("prophet")

  wflw_spec_prophet <- get_workflow_simple(
    model_spec_prophet,
    recipe_spec_prophet
  )

  return(wflw_spec_prophet)
}

#' prophet boost model
#'
#' @param train_data input data
#' @param pca pca
#'
#' @return Get prophet boost Model
#' @noRd
prophet_boost <- function(train_data,
                          pca) {
  recipe_spec_prophet_boost <- train_data %>%
    get_recipe_configurable(
      step_nzv = "zv",
      norm_date_adj_year = TRUE,
      one_hot = TRUE,
      pca = pca
    )

  # create model spec
  model_spec_prophet_boost_tune <- modeltime::prophet_boost(
    mode            = "regression",
    mtry            = tune::tune(),
    trees           = tune::tune(),
    min_n           = tune::tune(),
    tree_depth      = tune::tune(),
    learn_rate      = tune::tune(),
    loss_reduction  = tune::tune()
  ) %>%
    parsnip::set_engine("prophet_xgboost")


  wflw_spec_tune_prophet_boost <- get_workflow_simple(
    model_spec_prophet_boost_tune,
    recipe_spec_prophet_boost
  )

  return(wflw_spec_tune_prophet_boost)
}

#' prophet xregs model
#'
#' @param train_data input data
#' @param pca pca
#'
#' @return Get prophet xregs Model
#' @noRd
prophet_xregs <- function(train_data,
                          pca) {
  recipe_spec_prophet_xregs <- train_data %>%
    get_recipe_configurable(
      step_nzv = "zv",
      dummy_one_hot = FALSE,
      character_factor = TRUE,
      pca = pca
    )

  model_spec_prophet_xregs <- modeltime::prophet_reg(
    growth = tune::tune(),
    changepoint_num = tune::tune(),
    changepoint_range = tune::tune(),
    seasonality_yearly = tune::tune(),
    seasonality_weekly = tune::tune(),
    seasonality_daily = tune::tune(),
    prior_scale_changepoints = tune::tune(),
    prior_scale_seasonality = tune::tune()
  ) %>%
    parsnip::set_engine("prophet")

  wflw_spec_prophet_xregs <- get_workflow_simple(
    model_spec_prophet_xregs,
    recipe_spec_prophet_xregs
  )

  return(wflw_spec_prophet_xregs)
}

#' SNaive model
#'
#' @param train_data input data
#' @param frequency Frequency of Data
#'
#' @return Get SNaive Forecast Model
#' @noRd
snaive <- function(train_data,
                   frequency) {
  recipe_spec_snaive <- train_data %>%
    get_recipe_simple()

  model_spec_snaive <- modeltime::naive_reg(
    seasonal_period = round(frequency)
  ) %>%
    parsnip::set_engine("snaive")

  wflw_spec_snaive <- get_workflow_simple(
    model_spec_snaive,
    recipe_spec_snaive
  )

  return(wflw_spec_snaive)
}

#' STLM Arima model
#'
#' @param train_data input data
#' @param seasonal_period Seasonal Period
#'
#' @return Get STLM Arima Forecast Model
#' @noRd
stlm_arima <- function(train_data,
                       seasonal_period) {
  seasonal_period_stlm_arima <- seasonal_period

  recipe_spec_stlm_arima <- train_data %>%
    get_recipe_simple()

  model_spec_stlm_arima <- modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_stlm_arima[1],
    seasonal_period_2 = seasonal_period_stlm_arima[2],
    seasonal_period_3 = seasonal_period_stlm_arima[3]
  ) %>%
    parsnip::set_engine("stlm_arima")

  wflw_spec_stlm_arima <- get_workflow_simple(
    model_spec_stlm_arima,
    recipe_spec_stlm_arima
  )

  return(wflw_spec_stlm_arima)
}

#' STLM ETS Model
#'
#' @param train_data input data
#' @param seasonal_period Seasonal Period
#'
#' @return Get STLM ETS Forecast Model
#' @noRd
stlm_ets <- function(train_data,
                     seasonal_period) {
  seasonal_period_stlm_ets <- seasonal_period

  recipe_spec_stlm_ets <- train_data %>%
    get_recipe_simple()

  model_spec_stlm_ets <- modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_stlm_ets[1],
    seasonal_period_2 = seasonal_period_stlm_ets[2],
    seasonal_period_3 = seasonal_period_stlm_ets[3]
  ) %>%
    parsnip::set_engine("stlm_ets")

  wflw_spec_stlm_ets <- get_workflow_simple(
    model_spec_stlm_ets,
    recipe_spec_stlm_ets
  )

  return(wflw_spec_stlm_ets)
}

#' SVM Poly model
#'
#' @param train_data input data
#' @param model_type single or ensemble
#' @param pca pca
#' @param multistep multistep horizon
#' @param horizon forecast horizon
#' @param external_regressors external regressors
#' @param frequency frequency
#'
#' @return Get SVM Poly model
#' @noRd
svm_poly <- function(train_data,
                     model_type = "single",
                     pca,
                     multistep,
                     horizon,
                     external_regressors,
                     frequency) {
  if (model_type == "ensemble") {
    recipe_spec_svm <- train_data %>%
      get_recipe_configurable(
        rm_date = "with_adj",
        one_hot = FALSE,
        pca = pca
      )

    model_spec_svm <- parsnip::svm_poly(
      mode = "regression",
      cost = tune::tune(),
      degree = tune::tune(),
      margin = tune::tune(),
      scale_factor = tune::tune()
    ) %>%
      parsnip::set_engine("kernlab")
  } else if (multistep) {
    recipe_spec_svm <- train_data %>%
      get_recipe_configurable(
        rm_date = "none",
        norm_date_adj_year = TRUE,
        one_hot = FALSE,
        pca = pca
      )

    model_spec_svm <- svm_poly_multistep(
      mode = "regression",
      cost = tune::tune(),
      degree = tune::tune(),
      margin = tune::tune(),
      scale_factor = tune::tune(),
      lag_periods = get_lag_periods(NULL, get_date_type(frequency), horizon, TRUE),
      external_regressors = external_regressors,
      forecast_horizon = horizon
    ) %>%
      parsnip::set_engine("svm_poly_multistep_horizon")
  } else {
    recipe_spec_svm <- train_data %>%
      get_recipe_configurable(
        rm_date = "with_adj",
        norm_date_adj_year = TRUE,
        one_hot = FALSE,
        pca = pca
      )

    model_spec_svm <- parsnip::svm_poly(
      mode = "regression",
      cost = tune::tune(),
      degree = tune::tune(),
      margin = tune::tune(),
      scale_factor = tune::tune()
    ) %>%
      parsnip::set_engine("kernlab")
  }

  wflw_spec_tune_svm <- get_workflow_simple(
    model_spec_svm,
    recipe_spec_svm
  )

  return(wflw_spec_tune_svm)
}

#' SVM RBF
#'
#' @param train_data input data
#' @param model_type single or ensemble
#' @param pca pca
#' @param multistep multistep horizon
#' @param horizon forecast horizon
#' @param external_regressors external regressors
#' @param frequency frequency
#'
#' @return Get SVM RBF model
#' @noRd
svm_rbf <- function(train_data,
                    model_type = "single",
                    pca,
                    multistep,
                    horizon,
                    external_regressors,
                    frequency) {
  if (model_type == "ensemble") {
    recipe_spec_svm <- train_data %>%
      get_recipe_configurable(
        rm_date = "with_adj",
        one_hot = FALSE,
        pca = pca
      )

    model_spec_svm <- parsnip::svm_rbf(
      mode = "regression",
      cost = tune::tune(),
      rbf_sigma = tune::tune(),
      margin = tune::tune()
    ) %>%
      parsnip::set_engine("kernlab")
  } else if (multistep) {
    recipe_spec_svm <- train_data %>%
      get_recipe_configurable(
        norm_date_adj_year = TRUE,
        rm_date = "none",
        one_hot = FALSE,
        pca = pca
      )

    model_spec_svm <- svm_rbf_multistep(
      mode = "regression",
      cost = tune::tune(),
      rbf_sigma = tune::tune(),
      margin = tune::tune(),
      lag_periods = get_lag_periods(NULL, get_date_type(frequency), horizon, TRUE),
      external_regressors = external_regressors,
      forecast_horizon = horizon
    ) %>%
      parsnip::set_engine("svm_rbf_multistep_horizon")
  } else {
    recipe_spec_svm <- train_data %>%
      get_recipe_configurable(
        norm_date_adj_year = TRUE,
        rm_date = "with_adj",
        one_hot = FALSE,
        pca = pca
      )

    model_spec_svm <- parsnip::svm_rbf(
      mode = "regression",
      cost = tune::tune(),
      rbf_sigma = tune::tune(),
      margin = tune::tune()
    ) %>%
      parsnip::set_engine("kernlab")
  }


  wflw_spec_tune_svm <- get_workflow_simple(
    model_spec_svm,
    recipe_spec_svm
  )

  return(wflw_spec_tune_svm)
}

#' Tbats Model
#'
#' @param train_data input data
#' @param seasonal_period Seasonal Period
#'
#' @return Get TBats Model
#' @noRd
tbats <- function(train_data,
                  seasonal_period) {
  seasonal_period_tbats <- seasonal_period

  recipe_spec_tbats <- train_data %>%
    get_recipe_simple()

  model_spec_tbats <- modeltime::seasonal_reg(
    seasonal_period_1 = seasonal_period_tbats[1],
    seasonal_period_2 = seasonal_period_tbats[2],
    seasonal_period_3 = seasonal_period_tbats[3]
  ) %>%
    parsnip::set_engine("tbats")

  wflw_spec_tbats <- get_workflow_simple(
    model_spec_tbats,
    recipe_spec_tbats
  )

  return(wflw_spec_tbats)
}

#' Theta Model
#'
#' @param train_data input data
#' @param frequency Frequency of Data
#'
#' @return Get the Theta model
#' @noRd
theta <- function(train_data,
                  frequency) {
  recipe_spec_theta <- train_data %>%
    get_recipe_simple()

  model_spec_theta <- modeltime::exp_smoothing(
    seasonal_period = frequency
  ) %>%
    parsnip::set_engine("theta")

  wflw_spec_theta <- get_workflow_simple(
    model_spec_theta,
    recipe_spec_theta
  )

  return(wflw_spec_theta)
}

#' XGBoost
#'
#' @param train_data input table
#' @param pca pca
#' @param multistep multistep horizon
#' @param horizon forecast horizon
#' @param external_regressors external regressors
#' @param frequency
#'
#' @return Get XGBoost model
#' @noRd
xgboost <- function(train_data,
                    pca,
                    multistep,
                    horizon,
                    external_regressors,
                    frequency) {
  # create model recipe and spec
  if (multistep) {
    recipe_spec_xgboost <- train_data %>%
      get_recipe_configurable(
        rm_date = "none",
        step_nzv = "zv",
        one_hot = TRUE,
        pca = pca
      )

    model_spec_xgboost <- xgboost_multistep(
      lag_periods = get_lag_periods(NULL, get_date_type(frequency), horizon, TRUE),
      external_regressors = external_regressors,
      forecast_horizon = horizon,
      mode = "regression",
      trees = tune::tune(),
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      min_n = tune::tune(),
      sample_size = tune::tune(),
      mtry = tune::tune(),
      loss_reduction = tune::tune()
    ) %>%
      parsnip::set_engine("xgboost_multistep_horizon")
  } else {
    recipe_spec_xgboost <- train_data %>%
      get_recipe_configurable(
        rm_date = "with_adj",
        step_nzv = "zv",
        one_hot = TRUE,
        pca = pca
      )

    model_spec_xgboost <- parsnip::boost_tree(
      mode = "regression",
      trees = tune::tune(),
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      loss_reduction = tune::tune()
    ) %>%
      parsnip::set_engine("xgboost")
  }

  # create model workflow
  wflw_spec_tune_xgboost <- get_workflow_simple(
    model_spec_xgboost,
    recipe_spec_xgboost
  )

  return(wflw_spec_tune_xgboost)
}
