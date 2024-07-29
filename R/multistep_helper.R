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
      future_xregs <- external_regressors[external_regressors %in% colnames(input_data)][[1]]
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
