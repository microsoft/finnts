# XGBOOST Multistep ----

#' Initialize custom xgboost parsnip model
#'
#'
#' @return NA
#' @noRd
make_xgboost_multistep <- function() {
  parsnip::set_new_model("xgboost_multistep")
  parsnip::set_model_mode("xgboost_multistep", "regression")

  # xgboost multistep ----

  # * Model ----
  parsnip::set_model_engine("xgboost_multistep", mode = "regression", eng = "xgboost_multistep_horizon")
  parsnip::set_dependency("xgboost_multistep", "xgboost_multistep_horizon", "xgboost")
  parsnip::set_dependency("xgboost_multistep", "xgboost_multistep_horizon", "modeltime")

  # * Args - Xgboost ----
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "tree_depth",
    original     = "max_depth",
    func         = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "trees",
    original     = "nrounds",
    func         = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "learn_rate",
    original     = "eta",
    func         = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "mtry",
    original     = "colsample_bynode",
    func         = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "min_n",
    original     = "min_child_weight",
    func         = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "loss_reduction",
    original     = "gamma",
    func         = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "sample_size",
    original     = "subsample",
    func         = list(pkg = "dials", fun = "sample_prop"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "stop_iter",
    original     = "early_stop",
    func         = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "lag_periods",
    original     = "lag_periods",
    func         = list(fun = "lag_periods"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "external_regressors",
    original     = "external_regressors",
    func         = list(fun = "external_regressors"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "forecast_horizon",
    original     = "forecast_horizon",
    func         = list(fun = "forecast_horizon"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "xgboost_multistep",
    eng          = "xgboost_multistep_horizon",
    parsnip      = "selected_features",
    original     = "selected_features",
    func         = list(fun = "selected_features"),
    has_submodel = FALSE
  )

  # * Encoding ----
  parsnip::set_encoding(
    model = "xgboost_multistep",
    eng = "xgboost_multistep_horizon",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept    = FALSE,
      remove_intercept     = FALSE,
      allow_sparse_x       = FALSE
    )
  )

  # * Fit ----
  parsnip::set_fit(
    model = "xgboost_multistep",
    eng = "xgboost_multistep_horizon",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect   = c("x", "y"),
      func      = c(fun = "xgboost_multistep_fit_impl"),
      defaults  = list(objective = "reg:squarederror", nthread = 1, verbose = 0)
    )
  )

  # * Predict ----
  parsnip::set_pred(
    model = "xgboost_multistep",
    eng = "xgboost_multistep_horizon",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object   = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )
}

#' XGBOOST Multistep Horizon
#'
#' @inheritParams parsnip::boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param mtry mtry
#' @param trees trees
#' @param min_n min_n
#' @param tree_depth tree depth
#' @param learn_rate learn rate
#' @param loss_reduction loss reduction
#' @param sample_size  number for the number (or proportion) of data that is exposed to the fitting routine.
#' @param stop_iter The number of iterations without improvement before stopping
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#'
#' @return Get Multistep Horizon XGBoost model
#' @keywords internal
#' @export
xgboost_multistep <- function(mode = "regression",
                              mtry = NULL, trees = NULL, min_n = NULL,
                              tree_depth = NULL, learn_rate = NULL,
                              loss_reduction = NULL,
                              sample_size = NULL, stop_iter = NULL,
                              lag_periods = NULL, external_regressors = NULL,
                              forecast_horizon = NULL, selected_features = NULL) {
  args <- list(
    # XGBoost
    mtry                      = rlang::enquo(mtry),
    trees                     = rlang::enquo(trees),
    min_n                     = rlang::enquo(min_n),
    tree_depth                = rlang::enquo(tree_depth),
    learn_rate                = rlang::enquo(learn_rate),
    loss_reduction            = rlang::enquo(loss_reduction),
    sample_size               = rlang::enquo(sample_size),
    stop_iter                 = rlang::enquo(stop_iter),
    # Custom
    lag_periods               = rlang::enquo(lag_periods),
    external_regressors       = rlang::enquo(external_regressors),
    forecast_horizon          = rlang::enquo(forecast_horizon),
    selected_features         = rlang::enquo(selected_features)
  )

  parsnip::new_model_spec(
    "xgboost_multistep",
    args     = args,
    eng_args = NULL,
    mode     = mode,
    method   = NULL,
    engine   = NULL
  )
}

#' Print custom xgboost model
#'
#'
#' @return Prints model info
#' @keywords internal
#' @export
print.xgboost_multistep <- function(x, ...) {
  cat("XGBoost Multistep Horizon (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' Update parameter in custom xgboost model
#'
#' @param object model object
#' @param parameters parameters
#' @param mtry mtry
#' @param trees trees
#' @param min_n min_n
#' @param tree_depth tree depth
#' @param learn_rate learn rate
#' @param loss_reduction loss reduction
#' @param sample_size  number for the number (or proportion) of data that is exposed to the fitting routine.
#' @param stop_iter The number of iterations without improvement before stopping
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#' @param fresh fresh
#' @param ... extra args passed to xgboost
#'
#' @return Updated model
#' @keywords internal
#' @importFrom stats update
#' @export
update.xgboost_multistep <- function(object,
                                     parameters = NULL,
                                     mtry = NULL, trees = NULL, min_n = NULL,
                                     tree_depth = NULL, learn_rate = NULL,
                                     loss_reduction = NULL,
                                     sample_size = NULL, stop_iter = NULL,
                                     lag_periods = NULL,
                                     external_regressors = NULL,
                                     forecast_horizon = NULL,
                                     selected_features = NULL,
                                     fresh = FALSE, ...) {
  eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh, ...)

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }

  args <- list(
    # XGBoost
    mtry                      = rlang::enquo(mtry),
    trees                     = rlang::enquo(trees),
    min_n                     = rlang::enquo(min_n),
    tree_depth                = rlang::enquo(tree_depth),
    learn_rate                = rlang::enquo(learn_rate),
    loss_reduction            = rlang::enquo(loss_reduction),
    sample_size               = rlang::enquo(sample_size),
    stop_iter                 = rlang::enquo(stop_iter),
    # Custom
    lag_periods               = rlang::enquo(lag_periods),
    external_regressors       = rlang::enquo(external_regressors),
    forecast_horizon          = rlang::enquo(forecast_horizon),
    selected_features         = rlang::enquo(selected_features)
  )

  args <- parsnip::update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- purrr::map_lgl(args, parsnip::null_value)
    if (any(null_args)) {
      args <- args[!null_args]
    }
    if (length(args) > 0) {
      object$args[names(args)] <- args
    }
    if (length(eng_args) > 0) {
      object$eng_args[names(eng_args)] <- eng_args
    }
  }

  parsnip::new_model_spec(
    "xgboost_multistep",
    args     = object$args,
    eng_args = object$eng_args,
    mode     = object$mode,
    method   = NULL,
    engine   = object$engine
  )
}


#' Translate custom xgboost model
#'
#'
#' @return translated model
#' @keywords internal
#' @importFrom parsnip translate
#' @export
translate.xgboost_multistep <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'xgboost_multistep_horizon'` for translation.")
    engine <- "xgboost_multistep_horizon"
  }
  x <- parsnip::translate.default(x, engine, ...)

  x
}

# FIT BRIDGE - XGBOOST Multistep ----

#' Bridge XGBOOST Multistep Modeling function
#'
#' @inheritParams parsnip::xgb_train
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param max_depth An integer for the maximum depth of the tree.
#' @param nrounds An integer for the number of boosting iterations.
#' @param eta A numeric value between zero and one to control the learning rate.
#' @param colsample_bytree Subsampling proportion of columns.
#' @param min_child_weight A numeric value for the minimum sum of instance
#'  weights needed in a child to continue to split.
#' @param gamma A number for the minimum loss reduction required to make a
#'  further partition on a leaf node of the tree
#' @param subsample Subsampling proportion of rows.
#' @param validation A positive number. If on `[0, 1)` the value, `validation`
#' is a random proportion of data in `x` and `y` that are used for performance
#' assessment and potential early stopping. If 1 or greater, it is the _number_
#' of training set samples use for these purposes.
#' @param early_stop An integer or `NULL`. If not `NULL`, it is the number of
#' training iterations without improvement before stopping. If `validation` is
#' used, performance is base on the validation set; otherwise the training set
#' is used.
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#' @param ... Other options to pass to `xgb.train`.
#' @param ... Additional arguments passed to `xgboost::xgb.train`
#'
#'
#' @keywords internal
#' @importFrom stats frequency
#' @export
xgboost_multistep_fit_impl <- function(x, y,
                                       # xgboost params
                                       max_depth = 6,
                                       nrounds = 15,
                                       eta = 0.3,
                                       colsample_bytree = NULL,
                                       colsample_bynode = NULL,
                                       min_child_weight = 1,
                                       gamma = 0,
                                       subsample = 1,
                                       validation = 0,
                                       early_stop = NULL,
                                       # custom params
                                       lag_periods = NULL,
                                       external_regressors = NULL,
                                       forecast_horizon = NULL,
                                       selected_features = NULL,
                                       ...) {
  # X & Y
  # Expect outcomes  = vector
  # Expect predictor = data.frame
  outcome <- y
  predictor <- x %>% dplyr::select(-Date)

  # INDEX
  index_tbl <- modeltime::parse_index_from_data(x)

  # XREGS
  # Clean names, get xreg recipe, process predictors
  xreg_recipe <- modeltime::create_xreg_recipe(predictor, prepare = TRUE, one_hot = FALSE, clean_names = FALSE)
  xreg_tbl <- modeltime::juice_xreg_recipe(xreg_recipe, format = "tbl")

  # See if external regressors have future values
  future_xregs <- multi_future_xreg_check(xreg_tbl, external_regressors)

  # fit multiple models
  models <- list()
  model_predictions <- list()

  for (lag in get_multi_lags(lag_periods, forecast_horizon)) {
    # get final features based on lag
    xreg_tbl_final <- multi_feature_selection(
      xreg_tbl,
      future_xregs,
      lag_periods,
      lag
    )

    if (!is.null(selected_features)) {
      element_name <- paste0("model_lag_", lag)

      xreg_tbl_final <- xreg_tbl_final %>%
        dplyr::select(
          tidyselect::any_of(selected_features[[element_name]]),
          tidyselect::contains(setdiff(selected_features[[element_name]], colnames(xreg_tbl_final)))
        )
    }

    # fit model
    fit_xgboost <- modeltime::xgboost_impl(
      x = xreg_tbl_final,
      y = outcome,
      max_depth = max_depth,
      nrounds = nrounds,
      eta = eta,
      colsample_bytree = colsample_bytree,
      colsample_bynode = colsample_bynode,
      min_child_weight = min_child_weight,
      gamma = gamma,
      subsample = subsample,
      validation = validation,
      early_stop = early_stop,
      ...
    )

    # create prediction
    xgboost_fitted <- modeltime::xgboost_predict(fit_xgboost, newdata = xreg_tbl_final)

    # append outputs
    element_name <- paste0("model_lag_", lag)
    models[[element_name]] <- fit_xgboost

    model_predictions <- c(model_predictions, list(xgboost_fitted))
  }

  # Create Final Predictions, Averaged Across Each Trained Model
  model_predictions <- do.call(rbind, model_predictions)
  model_predictions <- apply(model_predictions, 2, mean)

  # RETURN A NEW MODELTIME BRIDGE

  # Class - Add a class for the model
  class <- "xgboost_multistep_fit_impl"

  # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
  data <- index_tbl %>%
    dplyr::mutate(
      .actual = y,
      .fitted = model_predictions,
      .residuals = .actual - .fitted
    )

  # Extras - Pass on transformation recipe
  extras <- list(
    xreg_recipe = xreg_recipe
  )

  # Model Description - Gets printed to describe the high-level model structure
  desc <- "Multistep Horizon XGBOOST Model"

  # Create new model
  modeltime::new_modeltime_bridge(
    class  = class,
    models = models,
    data   = data,
    extras = extras,
    desc   = desc
  )
}

#' Print fitted custom xgboost model
#'
#'
#' @return prints custom model
#' @keywords internal
#' @export
print.xgboost_multistep_fit_impl <- function(x, ...) {
  if (!is.null(x$desc)) cat(paste0(x$desc, "\n"))
  cat("---\n")
  model_names <- names(x$models)
  for (model_name in model_names) {
    cat(paste("Model: ", model_name, "\n", sep = ""))
    print(x$models[[model_name]]$call)
    cat("---\n")
  }
  invisible(x)
}


# PREDICT BRIDGE ----

#' Predict custom xgboost model
#'
#' @param object model object
#' @param new_data input data to predict
#'
#' @return predictions
#' @keywords internal
#' @export
predict.xgboost_multistep_fit_impl <- function(object, new_data, ...) {
  xgboost_multistep_predict_impl(object, new_data, ...)
}

#' Bridge prediction Function for XGBOOST Multistep Horizon Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param object model object
#' @param new_data input data to predict
#' @param ... Additional arguments passed to `predict.xgb.Booster()`
#'
#' @return predictions
#' @keywords internal
#' @export
xgboost_multistep_predict_impl <- function(object, new_data, ...) {
  # Date Mapping Table
  date_tbl <- new_data %>%
    dplyr::select(Date, Date_index.num) %>%
    dplyr::distinct() %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(Run_Number = dplyr::row_number())

  # PREPARE INPUTS
  xreg_recipe <- object$extras$xreg_recipe
  h_horizon <- nrow(new_data)

  # XREG
  xreg_tbl <- modeltime::bake_xreg_recipe(xreg_recipe,
    new_data,
    format = "tbl"
  ) %>%
    dplyr::left_join(date_tbl, by = "Date_index.num") %>%
    dplyr::mutate(Row_Num = dplyr::row_number())

  # PREDICTIONS
  final_prediction <- tibble::tibble()
  start_val <- 1

  for (model_name in names(object$models)) {
    if (start_val > nrow(date_tbl)) {
      break
    }

    lag_number <- stringr::str_extract(model_name, "[0-9]+")

    xgboost_model <- object$models[[model_name]]

    xreg_tbl_temp <- xreg_tbl %>%
      dplyr::filter(
        Run_Number >= as.numeric(start_val),
        Run_Number <= as.numeric(lag_number)
      )

    xreg_tbl_final <- xreg_tbl_temp %>%
      dplyr::select(tidyselect::any_of(xgboost_model$feature_names))

    if (!is.null(xreg_tbl)) {
      preds_xgboost <- modeltime::xgboost_predict(xgboost_model,
        newdata = xreg_tbl_final,
        ...
      )
    } else {
      preds_xgboost <- rep(0, h_horizon)
    }

    preds_xgboost <- tibble::tibble(.pred = preds_xgboost) %>%
      dplyr::mutate(Row_Num = xreg_tbl_temp$Row_Num)

    start_val <- as.numeric(lag_number) + 1
    final_prediction <- rbind(final_prediction, preds_xgboost)
  }

  # Ensure it's sorted correctly for global models
  final_prediction <- final_prediction %>%
    dplyr::arrange(Row_Num) %>%
    dplyr::pull(.pred)

  return(final_prediction)
}
