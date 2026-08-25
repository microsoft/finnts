# Helper Functions ----

#' Return xregs that contain future values for multistep horizon forecast
#'
#' @param input_data input data
#' @param external_regressors external regressors
#'
#' @return list of future xregs
#' @noRd
multi_future_xreg_check <- function(input_data,
                                    external_regressors) {
  if (is.null(external_regressors)) {
    future_xregs <- NULL
  } else {
    if (sum(external_regressors %in% colnames(input_data)) == 0) {
      future_xregs <- NULL
    } else {
      future_xregs <- unlist(intersect(external_regressors, colnames(input_data)), use.names = FALSE)
    }
  }

  return(future_xregs)
}

#' Get list of lags to use in multistep horizon forecast
#'
#' @param lag_periods lag periods
#' @param forecast_horizon forecast horizon
#'
#' @return list of lags
#' @noRd
get_multi_lags <- function(lag_periods,
                           forecast_horizon) {
  min_lag_above_horizon <- min(lag_periods[lag_periods >= forecast_horizon])

  final_lags <- lag_periods[lag_periods <= min_lag_above_horizon]

  return(final_lags)
}

#' Select correct features for multistep horizon forecast
#'
#' @param xreg_tbl table of feature data
#' @param future_xregs list of future xregs
#' @param lag_periods lag periods
#' @param lag lag
#' @param target should combo and target columns be kept
#'
#' @return df with correct columns
#' @noRd
multi_feature_selection <- function(xreg_tbl,
                                    future_xregs,
                                    lag_periods,
                                    lag,
                                    target = FALSE) {
  if (target) {
    if (is.null(future_xregs)) {
      xreg_tbl_final <- xreg_tbl %>%
        dplyr::select(
          Combo, Target,
          tidyselect::contains(c(
            "Date",
            paste0("lag", lag_periods[lag_periods >= lag])
          ))
        )
    } else {
      xreg_tbl_final <- xreg_tbl %>%
        dplyr::select(
          Combo, Target,
          tidyselect::contains(c(
            "Date",
            paste0("lag", lag_periods[lag_periods >= lag]),
            future_xregs
          ))
        )
    }
  } else {
    if (is.null(future_xregs)) {
      xreg_tbl_final <- xreg_tbl %>%
        dplyr::select(tidyselect::contains(c(
          "Date",
          paste0("lag", lag_periods[lag_periods >= lag])
        )))
    } else {
      xreg_tbl_final <- xreg_tbl %>%
        dplyr::select(tidyselect::contains(c(
          "Date",
          paste0("lag", lag_periods[lag_periods >= lag]),
          future_xregs
        )))
    }
  }

  return(xreg_tbl_final)
}

#' Predict multistep rows without losing assessment identity
#'
#' @param object fitted multistep model bridge
#' @param new_data assessment data
#' @param predict_model function that predicts from one fitted submodel
#' @param prepare_model_data optional function that selects engine-specific data
#' @param return_type prediction return type
#'
#' @return prediction vector or tibble with one row per assessment row
#' @noRd
multistep_predict_rows <- function(object,
                                   new_data,
                                   predict_model,
                                   prepare_model_data = function(model, data) data,
                                   return_type = c("tibble", "vector")) {
  return_type <- match.arg(return_type)

  required_columns <- c("Date", "Date_index.num")
  missing_columns <- setdiff(required_columns, colnames(new_data))
  if (length(missing_columns) > 0) {
    stop(
      "Multistep prediction data is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(new_data) == 0) {
    stop("Multistep prediction requires at least one assessment row.", call. = FALSE)
  }

  row_map <- new_data %>%
    dplyr::transmute(
      .finnts_row = dplyr::row_number(),
      .finnts_date = Date,
      .finnts_index = Date_index.num,
      .finnts_combo = if ("Combo" %in% colnames(new_data)) as.character(Combo) else ".single"
    ) %>%
    dplyr::group_by(.finnts_combo) %>%
    dplyr::mutate(.finnts_horizon = dplyr::dense_rank(.finnts_date)) %>%
    dplyr::ungroup()

  xreg_tbl <- modeltime::bake_xreg_recipe(
    object$extras$xreg_recipe,
    new_data,
    format = "tbl"
  )

  if (nrow(xreg_tbl) != nrow(row_map)) {
    stop(
      "Multistep recipe baking changed assessment rows: expected ",
      nrow(row_map), " rows but received ", nrow(xreg_tbl), ".",
      call. = FALSE
    )
  }

  if (!"Date_index.num" %in% colnames(xreg_tbl)) {
    stop(
      "Multistep recipe baking removed 'Date_index.num'; row identity cannot be verified.",
      call. = FALSE
    )
  }

  if (!identical(as.numeric(xreg_tbl$Date_index.num), as.numeric(row_map$.finnts_index))) {
    stop(
      "Multistep recipe baking changed assessment row order; predictions were not generated.",
      call. = FALSE
    )
  }

  xreg_tbl <- xreg_tbl %>%
    dplyr::mutate(
      .finnts_row = row_map$.finnts_row,
      .finnts_horizon = row_map$.finnts_horizon
    )

  model_names <- names(object$models)
  model_lags <- suppressWarnings(as.numeric(stringr::str_extract(model_names, "[0-9]+")))
  valid_models <- is.finite(model_lags) & model_lags > 0
  model_names <- model_names[valid_models]
  model_lags <- model_lags[valid_models]

  if (length(model_lags) == 0) {
    stop("Multistep model does not contain any fitted horizon submodels.", call. = FALSE)
  }

  model_order <- order(model_lags)
  model_lags <- model_lags[model_order]
  model_names <- model_names[model_order]

  if (anyDuplicated(model_lags)) {
    stop("Multistep model contains duplicate fitted horizon boundaries.", call. = FALSE)
  }

  max_horizon <- max(row_map$.finnts_horizon)
  if (max(model_lags) < max_horizon) {
    stop(
      "Multistep model coverage ends at horizon ", max(model_lags),
      " but the assessment requires horizon ", max_horizon, ".",
      call. = FALSE
    )
  }

  assigned_model <- vapply(
    row_map$.finnts_horizon,
    function(horizon) model_names[which(model_lags >= horizon)[1]],
    character(1)
  )

  prediction_parts <- lapply(unique(assigned_model), function(model_name) {
    row_ids <- row_map$.finnts_row[assigned_model == model_name]
    model <- object$models[[model_name]]
    model_data <- xreg_tbl %>%
      dplyr::filter(.finnts_row %in% row_ids) %>%
      dplyr::arrange(match(.finnts_row, row_ids)) %>%
      dplyr::select(-.finnts_row, -.finnts_horizon) %>%
      prepare_model_data(model, data = .)

    prediction <- predict_model(model, model_data)
    if (is.data.frame(prediction)) {
      if (!".pred" %in% colnames(prediction)) {
        stop("Multistep submodel prediction did not return a '.pred' column.", call. = FALSE)
      }
      prediction <- prediction$.pred
    }

    if (length(prediction) != length(row_ids)) {
      stop(
        "Multistep submodel '", model_name, "' returned ", length(prediction),
        " predictions for ", length(row_ids), " assessment rows.",
        call. = FALSE
      )
    }

    if (any(!is.finite(prediction))) {
      stop(
        "Multistep submodel '", model_name, "' returned non-finite predictions.",
        call. = FALSE
      )
    }

    tibble::tibble(.finnts_row = row_ids, .pred = as.numeric(prediction))
  })

  final_prediction <- dplyr::bind_rows(prediction_parts)
  if (nrow(final_prediction) != nrow(row_map) ||
        anyDuplicated(final_prediction$.finnts_row) ||
        !setequal(final_prediction$.finnts_row, row_map$.finnts_row)) {
    stop(
      "Multistep prediction did not preserve a one-to-one assessment row mapping.",
      call. = FALSE
    )
  }

  final_prediction <- final_prediction %>%
    dplyr::arrange(.finnts_row)

  if (return_type == "vector") {
    return(final_prediction$.pred)
  }

  final_prediction %>% dplyr::select(.pred)
}
