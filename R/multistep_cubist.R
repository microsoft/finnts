# CUBIST Multistep ----

#' Initialize custom cubist parsnip model
#'
#'
#' @return NA
#' @noRd
make_cubist_multistep <- function() {
  parsnip::set_new_model("cubist_multistep")
  parsnip::set_model_mode("cubist_multistep", "regression")

  # * Model ----
  parsnip::set_model_engine("cubist_multistep", mode = "regression", eng = "cubist_multistep_horizon")
  parsnip::set_dependency("cubist_multistep", "cubist_multistep_horizon", "parsnip")
  parsnip::set_dependency("cubist_multistep", "cubist_multistep_horizon", "Cubist")
  parsnip::set_dependency("cubist_multistep", "cubist_multistep_horizon", "rules")

  # * Args - CUBIST ----
  parsnip::set_model_arg(
    model        = "cubist_multistep",
    eng          = "cubist_multistep_horizon",
    parsnip      = "committees",
    original     = "committees",
    func         = list(pkg = "rules", fun = "committees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "cubist_multistep",
    eng          = "cubist_multistep_horizon",
    parsnip      = "neighbors",
    original     = "neighbors",
    func         = list(pkg = "dials", fun = "neighbors"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model        = "cubist_multistep",
    eng          = "cubist_multistep_horizon",
    parsnip      = "max_rules",
    original     = "max_rules",
    func         = list(pkg = "rules", fun = "max_rules"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "cubist_multistep",
    eng          = "cubist_multistep_horizon",
    parsnip      = "lag_periods",
    original     = "lag_periods",
    func         = list(fun = "lag_periods"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "cubist_multistep",
    eng          = "cubist_multistep_horizon",
    parsnip      = "external_regressors",
    original     = "external_regressors",
    func         = list(fun = "external_regressors"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "cubist_multistep",
    eng          = "cubist_multistep_horizon",
    parsnip      = "forecast_horizon",
    original     = "forecast_horizon",
    func         = list(fun = "forecast_horizon"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = "cubist_multistep",
    eng          = "cubist_multistep_horizon",
    parsnip      = "selected_features",
    original     = "selected_features",
    func         = list(fun = "selected_features"),
    has_submodel = FALSE
  )

  # * Encoding ----
  parsnip::set_encoding(
    model = "cubist_multistep",
    eng = "cubist_multistep_horizon",
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
    model = "cubist_multistep",
    eng = "cubist_multistep_horizon",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect   = c("x", "y"),
      func      = c(fun = "cubist_multistep_fit_impl"),
      defaults  = list()
    )
  )

  # * Predict ----
  parsnip::set_pred(
    model = "cubist_multistep",
    eng = "cubist_multistep_horizon",
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

#' CUBIST Multistep Horizon
#'
#' @inheritParams parsnip::cubist_rules
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param committees committees
#' @param neighbors neighbors
#' @param max_rules max rules
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#'
#' @return Get Multistep Horizon CUBIST model
#' @keywords internal
#' @export
cubist_multistep <- function(mode = "regression",
                             committees = NULL, neighbors = NULL,
                             max_rules = NULL, lag_periods = NULL,
                             external_regressors = NULL,
                             forecast_horizon = NULL,
                             selected_features = NULL) {
  args <- list(
    # CUBIST
    committees                = rlang::enquo(committees),
    neighbors                 = rlang::enquo(neighbors),
    max_rules                 = rlang::enquo(max_rules),
    # Custom
    lag_periods               = rlang::enquo(lag_periods),
    external_regressors       = rlang::enquo(external_regressors),
    forecast_horizon          = rlang::enquo(forecast_horizon),
    selected_features         = rlang::enquo(selected_features)
  )

  parsnip::new_model_spec(
    "cubist_multistep",
    args     = args,
    eng_args = NULL,
    mode     = mode,
    method   = NULL,
    engine   = NULL
  )
}

#' Print custom cubist model
#'
#'
#' @return Prints model info
#' @keywords internal
#' @export
print.cubist_multistep <- function(x, ...) {
  cat("CUBIST Multistep Horizon (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' Update parameter in custom cubist model
#'
#' @param object model object
#' @param parameters parameters
#' @param committees committees
#' @param neighbors neighbors
#' @param max_rules max rules
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#' @param fresh fresh
#' @param ... extra args passed to cubist
#'
#' @return Updated model
#' @keywords internal
#' @importFrom stats update
#' @export
update.cubist_multistep <- function(object,
                                    parameters = NULL,
                                    committees = NULL,
                                    neighbors = NULL,
                                    max_rules = NULL,
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
    # CUBIST
    committees                = rlang::enquo(committees),
    neighbors                 = rlang::enquo(neighbors),
    max_rules                 = rlang::enquo(max_rules),
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
    "cubist_multistep",
    args     = object$args,
    eng_args = object$eng_args,
    mode     = object$mode,
    method   = NULL,
    engine   = object$engine
  )
}

#' Translate custom cubist model
#'
#'
#' @return translated model
#' @keywords internal
#' @importFrom parsnip translate
#' @export
translate.cubist_multistep <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'cubist_multistep_horizon'` for translation.")
    engine <- "cubist_multistep_horizon"
  }
  x <- parsnip::translate.default(x, engine, ...)

  x
}

# FIT BRIDGE - Cubist Multistep ----

#' Bridge CUBIST Multistep Modeling function
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param committees committees
#' @param neighbors neighbors
#' @param max_rules max rules
#' @param lag_periods lag periods
#' @param external_regressors external regressors
#' @param forecast_horizon forecast horizon
#' @param selected_features selected features
#'
#' @keywords internal
#' @importFrom stats frequency
#' @export
cubist_multistep_fit_impl <- function(x, y,
                                      # cubist params
                                      committees = 1,
                                      neighbors = 0,
                                      max_rules = 10,
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
    fit_cubist <- rules::cubist_fit(
      x = xreg_tbl_final,
      y = outcome,
      committees = committees,
      neighbors = neighbors,
      max_rules = max_rules
    )

    # create prediction
    cubist_fitted <- predict(fit_cubist, xreg_tbl_final)

    # append outputs
    element_name <- paste0("model_lag_", lag)
    models[[element_name]] <- fit_cubist

    model_predictions <- c(model_predictions, list(cubist_fitted))
  }

  # Create Final Predictions, Averaged Across Each Trained Model
  model_predictions <- do.call(rbind, model_predictions)
  model_predictions <- apply(model_predictions, 2, mean)

  # RETURN A NEW MODELTIME BRIDGE

  # Class - Add a class for the model
  class <- "cubist_multistep_fit_impl"

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
  desc <- "Multistep Horizon CUBIST Model"

  # Create new model
  modeltime::new_modeltime_bridge(
    class  = class,
    models = models,
    data   = data,
    extras = extras,
    desc   = desc
  )
}


#' Print fitted custom cubist model
#'
#'
#' @return prints custom model
#' @keywords internal
#' @export
print.cubist_multistep_fit_impl <- function(x, ...) {
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

#' Predict custom cubist model
#'
#' @param object model object
#' @param new_data input data to predict
#'
#' @return predictions
#' @keywords internal
#' @export
predict.cubist_multistep_fit_impl <- function(object, new_data, ...) {
  cubist_multistep_predict_impl(object, new_data, ...)
}

#' Bridge prediction Function for CUBIST Multistep Horizon Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param object model object
#' @param new_data input data to predict
#'
#' @return predictions
#' @keywords internal
#' @export
cubist_multistep_predict_impl <- function(object, new_data, ...) {
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

    cubist_model <- object$models[[model_name]]

    xreg_tbl_final <- xreg_tbl %>%
      dplyr::filter(
        Run_Number >= as.numeric(start_val),
        Run_Number <= as.numeric(lag_number)
      )

    if (!is.null(xreg_tbl)) {
      preds_cubist <- predict(cubist_model, xreg_tbl_final)
    } else {
      preds_cubist <- rep(0, h_horizon)
    }

    preds_cubist <- tibble::tibble(.pred = preds_cubist) %>%
      dplyr::mutate(Row_Num = xreg_tbl_final$Row_Num)

    start_val <- as.numeric(lag_number) + 1
    final_prediction <- rbind(final_prediction, preds_cubist)
  }

  # Ensure it's sorted correctly for global models
  final_prediction <- final_prediction %>%
    dplyr::arrange(Row_Num) %>%
    dplyr::pull(.pred)

  return(final_prediction)
}
