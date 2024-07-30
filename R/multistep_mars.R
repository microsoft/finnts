# MARS Multistep ----

#' Initialize custom mars parsnip model
#'
#'
#' @return NA
#' @noRd
make_mars_multistep <- function() {
  parsnip::set_new_model("mars_multistep")
  parsnip::set_model_mode("mars_multistep", "regression")

  # * Model ----
  parsnip::set_model_engine("mars_multistep", mode = "regression", eng = "mars_multistep_horizon")
  parsnip::set_dependency("mars_multistep", "mars_multistep_horizon", "earth")
  parsnip::set_dependency("mars_multistep", "mars_multistep_horizon", "parsnip")

  # * Args - MARS ----
  parsnip::set_model_arg(
    model        = "mars_multistep",
    eng          = "mars_multistep_horizon",
    parsnip      = "num_terms",
    original     = "nprune",
    func         = list(pkg = "dials", fun = "max_num_terms"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "mars_multistep",
    eng          = "mars_multistep_horizon",
    parsnip      = "prod_degree",
    original     = "degree",
    func         = list(pkg = "dials", fun = "prod_degree"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "mars_multistep",
    eng          = "mars_multistep_horizon",
    parsnip      = "prune_method",
    original     = "pmethod",
    func         = list(pkg = "dials", fun = "prune_method"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "mars_multistep",
    eng          = "mars_multistep_horizon",
    parsnip      = "lag_periods",
    original     = "lag_periods",
    func         = list(fun = "lag_periods"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "mars_multistep",
    eng          = "mars_multistep_horizon",
    parsnip      = "external_regressors",
    original     = "external_regressors",
    func         = list(fun = "external_regressors"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "mars_multistep",
    eng          = "mars_multistep_horizon",
    parsnip      = "forecast_horizon",
    original     = "forecast_horizon",
    func         = list(fun = "forecast_horizon"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "mars_multistep",
    eng          = "mars_multistep_horizon",
    parsnip      = "selected_features",
    original     = "selected_features",
    func         = list(fun = "selected_features"),
    has_submodel = FALSE
  )

  # * Encoding ----
  parsnip::set_encoding(
    model = "mars_multistep",
    eng = "mars_multistep_horizon",
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
    model = "mars_multistep",
    eng = "mars_multistep_horizon",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect   = c("x", "y"),
      func      = c(fun = "mars_multistep_fit_impl"),
      defaults  = list()
    )
  )

  # * Predict ----
  parsnip::set_pred(
    model = "mars_multistep",
    eng = "mars_multistep_horizon",
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

#' MARS Multistep Horizon
#'
#' @inheritParams parsnip::mars
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param num_terms The number of features that will be retained in
#'  the final model, including the intercept.
#' @param prod_degree The highest possible interaction degree.
#' @param prune_method The pruning method.
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#'
#' @return Get Multistep Horizon MARS model
#' @keywords internal
#' @export
mars_multistep <- function(mode = "regression",
                           num_terms = NULL,
                           prod_degree = NULL,
                           prune_method = NULL,
                           lag_periods = NULL,
                           external_regressors = NULL,
                           forecast_horizon = NULL,
                           selected_features = NULL) {
  args <- list(
    # MARS
    prod_degree               = rlang::enquo(prod_degree),
    num_terms                 = rlang::enquo(num_terms),
    prune_method              = rlang::enquo(prune_method),
    # Custom
    lag_periods               = rlang::enquo(lag_periods),
    external_regressors       = rlang::enquo(external_regressors),
    forecast_horizon          = rlang::enquo(forecast_horizon),
    selected_features         = rlang::enquo(selected_features)
  )

  parsnip::new_model_spec(
    "mars_multistep",
    args     = args,
    eng_args = NULL,
    mode     = mode,
    method   = NULL,
    engine   = NULL
  )
}

#' Print custom mars model
#'
#'
#' @return Prints model info
#' @keywords internal
#' @export
print.mars_multistep <- function(x, ...) {
  cat("MARS Multistep Horizon (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' Update parameter in custom mars model
#'
#' @param object model object
#' @param parameters parameters
#' @param num_terms The number of features that will be retained in
#'  the final model, including the intercept.
#' @param prod_degree The highest possible interaction degree.
#' @param prune_method The pruning method.
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#' @param fresh fresh
#' @param ... extra args passed to mars
#'
#' @return Updated model
#' @keywords internal
#' @importFrom stats update
#' @export
update.mars_multistep <- function(object,
                                  parameters = NULL,
                                  num_terms = NULL,
                                  prod_degree = NULL,
                                  prune_method = NULL,
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
    # MARS
    prod_degree               = rlang::enquo(prod_degree),
    num_terms                 = rlang::enquo(num_terms),
    prune_method              = rlang::enquo(prune_method),
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
    "mars_multistep",
    args     = object$args,
    eng_args = object$eng_args,
    mode     = object$mode,
    method   = NULL,
    engine   = object$engine
  )
}

#' Translate custom mars model
#'
#'
#' @return translated model
#' @keywords internal
#' @importFrom parsnip translate
#' @export
translate.mars_multistep <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'mars_multistep_horizon'` for translation.")
    engine <- "mars_multistep_horizon"
  }
  x <- parsnip::translate.default(x, engine, ...)

  x
}

# FIT BRIDGE - MARS Multistep ----

#' Bridge MARS Multistep Modeling function
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param nprune The number of features that will be retained in
#'  the final model, including the intercept.
#' @param degree The highest possible interaction degree.
#' @param pmethod The pruning method.
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#'
#' @keywords internal
#' @importFrom stats frequency
#' @export
mars_multistep_fit_impl <- function(x, y,
                                    # mars params
                                    nprune = NULL,
                                    degree = 1L,
                                    pmethod = "backward",
                                    # custom params
                                    lag_periods = NULL,
                                    external_regressors = NULL,
                                    forecast_horizon = NULL,
                                    selected_features = NULL) {
  # X & Y
  # Expect outcomes  = vector
  # Expect predictor = data.frame
  outcome <- y
  predictor <- x %>% dplyr::select(-Date)

  # INDEX
  index_tbl <- modeltime::parse_index_from_data(x)

  # XREGS
  # Clean names, get xreg recipe, process predictors
  xreg_recipe <- modeltime::create_xreg_recipe(predictor, prepare = TRUE, one_hot = TRUE, clean_names = FALSE)
  xreg_tbl <- modeltime::juice_xreg_recipe(xreg_recipe, format = "tbl")

  # See if external regressors have future values
  future_xregs <- multi_future_xreg_check(xreg_tbl, external_regressors)

  # fit multiple models
  models <- list()
  model_predictions <- list()

  mars_reg_spec <- parsnip::mars(
    mode = "regression",
    num_terms = nprune,
    prod_degree = degree,
    prune_method = pmethod
  ) %>%
    parsnip::set_engine("earth")

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

    combined_df <- xreg_tbl_final %>%
      dplyr::mutate(Target = outcome)

    # fit model
    fit_mars <- mars_reg_spec %>%
      generics::fit(Target ~ ., data = combined_df)

    # create prediction
    mars_fitted <- predict(fit_mars, combined_df)

    # append outputs
    element_name <- paste0("model_lag_", lag)
    models[[element_name]] <- fit_mars

    model_predictions <- c(model_predictions, list(mars_fitted))
  }

  # Create Final Predictions, Averaged Across Each Trained Model
  model_predictions <- do.call(rbind, model_predictions)
  model_predictions <- apply(model_predictions, 2, mean)

  # RETURN A NEW MODELTIME BRIDGE

  # Class - Add a class for the model
  class <- "mars_multistep_fit_impl"

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
  desc <- "Multistep Horizon MARS Model"

  # Create new model
  modeltime::new_modeltime_bridge(
    class  = class,
    models = models,
    data   = data,
    extras = extras,
    desc   = desc
  )
}


#' Print fitted custom mars model
#'
#'
#' @return prints custom model
#' @keywords internal
#' @export
print.mars_multistep_fit_impl <- function(x, ...) {
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

#' Predict custom mars model
#'
#' @param object model object
#' @param new_data input data to predict
#'
#' @return predictions
#' @keywords internal
#' @export
predict.mars_multistep_fit_impl <- function(object, new_data, ...) {
  mars_multistep_predict_impl(object, new_data, ...)
}

#' Bridge prediction Function for mars Multistep Horizon Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param object model object
#' @param new_data input data to predict
#'
#' @return predictions
#' @keywords internal
#' @export
mars_multistep_predict_impl <- function(object, new_data, ...) {
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

    mars_model <- object$models[[model_name]]

    xreg_tbl_final <- xreg_tbl %>%
      dplyr::filter(
        Run_Number >= as.numeric(start_val),
        Run_Number <= as.numeric(lag_number)
      )

    if (!is.null(xreg_tbl)) {
      preds_mars <- predict(mars_model, xreg_tbl_final)
    } else {
      preds_mars <- rep(0, h_horizon)
    }

    preds_mars <- preds_mars %>%
      dplyr::mutate(Row_Num = xreg_tbl_final$Row_Num)

    start_val <- as.numeric(lag_number) + 1
    final_prediction <- rbind(final_prediction, preds_mars)
  }

  # Ensure it's sorted correctly for global models
  final_prediction <- final_prediction %>%
    dplyr::arrange(Row_Num) %>%
    dplyr::select(.pred)

  return(final_prediction)
}
