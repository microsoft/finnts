#' Get non-negative forecast
#' 
#' Function to rectify forecast non-negativity
#' 
#' @param fcst Input Data Frame
#' @param negative_forecast is negative forecast allowed
#' @noRd
get_forecast_negative_adjusted <- function(fcst,
                                           negative_forecast){
  
  
  fcst_final <- fcst %>%
    dplyr::mutate_if(is.numeric, list(~replace(., is.infinite(.), NA))) %>% # replace infinite values
    dplyr::mutate_if(is.numeric, list(~replace(., is.nan(.), NA))) %>% # replace NaN values
    dplyr::mutate_if(is.numeric, list(~replace(., is.na(.), 0))) # replace NA values
  
  # convert negative forecasts to zero
  if(negative_forecast == FALSE) {
    fcst_final$FCST <- replace(fcst_final$FCST, 
                               which(fcst_final$FCST < 0), 
                               0)
    }
  
  return(fcst_final)
}