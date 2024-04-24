
# SVM-POLY Multistep ----

#' Initialize custom svm-poly parsnip model
#'
#'
#' @return NA
#' @noRd
make_svm_poly_multistep <- function() {
  parsnip::set_new_model("svm_poly_multistep")
  parsnip::set_model_mode("svm_poly_multistep", "regression")

  # * Model ----
  parsnip::set_model_engine("svm_poly_multistep", mode = "regression", eng = "svm_poly_multistep_horizon")
  parsnip::set_dependency("svm_poly_multistep", "svm_poly_multistep_horizon", "kernlab")
  parsnip::set_dependency("svm_poly_multistep", "svm_poly_multistep_horizon", "parsnip")

  # * Args - SVM-POLY ----
  parsnip::set_model_arg(
    model        = "svm_poly_multistep",
    eng          = "svm_poly_multistep_horizon",
    parsnip      = "cost",
    original     = "C",
    func         = list(pkg = "dials", fun = "cost", range = c(-10, 5)),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "svm_poly_multistep",
    eng          = "svm_poly_multistep_horizon",
    parsnip      = "degree",
    original     = "degree",
    func         = list(pkg = "dials", fun = "degree"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "svm_poly_multistep",
    eng          = "svm_poly_multistep_horizon",
    parsnip      = "scale_factor",
    original     = "scale",
    func         = list(pkg = "dials", fun = "scale_factor"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "svm_poly_multistep",
    eng          = "svm_poly_multistep_horizon",
    parsnip      = "margin",
    original     = "epsilon",
    func         = list(pkg = "dials", fun = "svm_margin"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "svm_poly_multistep",
    eng          = "svm_poly_multistep_horizon",
    parsnip      = "lag_periods",
    original     = "lag_periods",
    func         = list(fun = "lag_periods"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "svm_poly_multistep",
    eng          = "svm_poly_multistep_horizon",
    parsnip      = "external_regressors",
    original     = "external_regressors",
    func         = list(fun = "external_regressors"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "svm_poly_multistep",
    eng          = "svm_poly_multistep_horizon",
    parsnip      = "forecast_horizon",
    original     = "forecast_horizon",
    func         = list(fun = "forecast_horizon"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "svm_poly_multistep",
    eng          = "svm_poly_multistep_horizon",
    parsnip      = "selected_features",
    original     = "selected_features",
    func         = list(fun = "selected_features"),
    has_submodel = FALSE
  )

  # * Encoding ----
  parsnip::set_encoding(
    model = "svm_poly_multistep",
    eng = "svm_poly_multistep_horizon",
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
    model = "svm_poly_multistep",
    eng = "svm_poly_multistep_horizon",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(fun = "svm_poly_multistep_fit_impl"),
      defaults = list()
    )
  )

  # * Predict ----
  parsnip::set_pred(
    model = "svm_poly_multistep",
    eng = "svm_poly_multistep_horizon",
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

#' SVM-POLY Multistep Horizon
#'
#' @inheritParams parsnip::svm_poly
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param cost A positive number for the cost of predicting
#'   a sample within or on the wrong side of the margin.
#' @param degree A positive number for polynomial degree.
#' @param scale_factor A positive number for the polynomial
#'   scaling factor.
#' @param margin A positive number for the epsilon in the SVM
#'   insensitive loss function
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#'
#' @return Get Multistep Horizon SVM-POLY model
#' @keywords internal
#' @export
svm_poly_multistep <- function(mode = "regression",
                               cost = NULL,
                               degree = NULL,
                               scale_factor = NULL,
                               margin = NULL,
                               lag_periods = NULL,
                               external_regressors = NULL,
                               forecast_horizon = NULL,
                               selected_features = NULL) {
  args <- list(
    # SVM-Poly
    cost                      = rlang::enquo(cost),
    degree                    = rlang::enquo(degree),
    scale_factor              = rlang::enquo(scale_factor),
    margin                    = rlang::enquo(margin),
    # Custom
    lag_periods               = rlang::enquo(lag_periods),
    external_regressors       = rlang::enquo(external_regressors),
    forecast_horizon          = rlang::enquo(forecast_horizon),
    selected_features         = rlang::enquo(selected_features)
  )

  parsnip::new_model_spec(
    "svm_poly_multistep",
    args     = args,
    eng_args = NULL,
    mode     = mode,
    method   = NULL,
    engine   = NULL
  )
}

#' Print custom svm_poly model
#'
#'
#' @return Prints model info
#' @keywords internal
#' @export
print.svm_poly_multistep <- function(x, ...) {
  cat("SVM-POLY Multistep Horizon (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' Update parameter in custom svm_poly model
#'
#' @param object model object
#' @param parameters parameters
#' @param cost A positive number for the cost of predicting
#'   a sample within or on the wrong side of the margin.
#' @param degree A positive number for polynomial degree.
#' @param scale_factor A positive number for the polynomial
#'   scaling factor.
#' @param margin A positive number for the epsilon in the SVM
#'   insensitive loss function
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#' @param fresh fresh
#' @param ... extra args passed to svm_poly
#'
#' @return Updated model
#' @keywords internal
#' @importFrom stats update
#' @export
update.svm_poly_multistep <- function(object,
                                      parameters = NULL,
                                      cost = NULL,
                                      degree = NULL,
                                      scale_factor = NULL,
                                      margin = NULL,
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
    # SVM-Poly
    cost                      = rlang::enquo(cost),
    degree                    = rlang::enquo(degree),
    scale_factor              = rlang::enquo(scale_factor),
    margin                    = rlang::enquo(margin),
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
    "svm_poly_multistep",
    args     = object$args,
    eng_args = object$eng_args,
    mode     = object$mode,
    method   = NULL,
    engine   = object$engine
  )
}

#' Translate custom svm_poly model
#'
#'
#' @return translated model
#' @keywords internal
#' @importFrom parsnip translate
#' @export
translate.svm_poly_multistep <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'svm_poly_multistep_horizon'` for translation.")
    engine <- "svm_poly_multistep_horizon"
  }
  x <- parsnip::translate.default(x, engine, ...)

  x
}

# FIT BRIDGE - SVM-POLY Multistep ----

#' Bridge SVM-POLY Multistep Modeling function
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param C A positive number for the cost of predicting
#'   a sample within or on the wrong side of the margin.
#' @param degree A positive number for polynomial degree.
#' @param scale A positive number for the polynomial
#'   scaling factor.
#' @param epsilon A positive number for the epsilon in the SVM
#'   insensitive loss function
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#'
#' @keywords internal
#' @importFrom stats frequency
#' @export
svm_poly_multistep_fit_impl <- function(x, y,
                                        # svm_poly params
                                        C = double(1),
                                        degree = integer(1),
                                        scale = double(1),
                                        epsilon = double(1),
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

  svm_reg_spec <- parsnip::svm_poly(
    mode = "regression",
    cost = C,
    degree = degree,
    scale_factor = scale,
    margin = epsilon
  ) %>%
    parsnip::set_engine("kernlab")

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
    fit_svm <- svm_reg_spec %>%
      generics::fit(
        Target ~ .,
        combined_df
      )

    # create prediction
    svm_fitted <- predict(fit_svm, combined_df)

    # append outputs
    element_name <- paste0("model_lag_", lag)
    models[[element_name]] <- fit_svm

    model_predictions <- c(model_predictions, list(svm_fitted))
  }

  # Create Final Predictions, Averaged Across Each Trained Model
  model_predictions <- do.call(rbind, model_predictions)
  model_predictions <- apply(model_predictions, 2, mean)

  # RETURN A NEW MODELTIME BRIDGE

  # Class - Add a class for the model
  class <- "svm_poly_multistep_fit_impl"

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
  desc <- "Multistep Horizon SVM-POLY Model"

  # Create new model
  modeltime::new_modeltime_bridge(
    class  = class,
    models = models,
    data   = data,
    extras = extras,
    desc   = desc
  )
}


#' Print fitted custom svm_poly model
#'
#'
#' @return prints custom model
#' @keywords internal
#' @export
print.svm_poly_multistep_fit_impl <- function(x, ...) {
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

#' Predict custom svm_poly model
#'
#' @param object model object
#' @param new_data input data to predict
#'
#' @return predictions
#' @keywords internal
#' @export
predict.svm_poly_multistep_fit_impl <- function(object, new_data, ...) {
  svm_poly_multistep_predict_impl(object, new_data, ...)
}

#' Bridge prediction Function for SVM-POLY Multistep Horizon Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param object model object
#' @param new_data input data to predict
#'
#' @return predictions
#' @keywords internal
#' @export
svm_poly_multistep_predict_impl <- function(object, new_data, ...) {

  # PREPARE INPUTS
  xreg_recipe <- object$extras$xreg_recipe
  h_horizon <- nrow(new_data)

  # XREG
  xreg_tbl <- modeltime::bake_xreg_recipe(xreg_recipe,
    new_data,
    format = "tbl"
  )

  # PREDICTIONS
  final_prediction <- tibble::tibble()
  start_val <- 1

  for (model_name in names(object$models)) {
    if (start_val > nrow(xreg_tbl)) {
      break
    }

    lag_number <- stringr::str_extract(model_name, "[0-9]+")

    svm_poly_model <- object$models[[model_name]]

    xreg_tbl_final <- xreg_tbl %>%
      dplyr::slice(start_val:lag_number)

    if (!is.null(xreg_tbl)) {
      preds_svm_poly <- predict(svm_poly_model, xreg_tbl_final)
    } else {
      preds_svm_poly <- rep(0, h_horizon)
    }

    start_val <- as.numeric(lag_number) + 1
    final_prediction <- rbind(final_prediction, preds_svm_poly)
  }

  return(final_prediction)
}