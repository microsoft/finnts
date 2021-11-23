#' Get non-negative forecast
#' 
#' Function to rectify forecast non-negativity
#' 
#' @param fcst Input Data Frame
#' @param negative_forecast is negative forecast allowed
#' @noRd
get_forecast_negative_adjusted <- function(fcst,
                                           negative_forecast){
  
  #TODO: Should re-write this as dplyr
  
  #Replace NaN/Inf with NA, then replace with zero
  is.na(fcst) <- sapply(fcst, is.infinite)
  is.na(fcst) <- sapply(fcst, is.nan)
  fcst[is.na(fcst)] = 0
  
  # convert negative forecasts to zero
  if(negative_forecast == FALSE) {fcst$FCST <- replace(fcst$FCST, 
                                                   which(fcst$FCST < 0), 0)}
  
  return (fcst)
}