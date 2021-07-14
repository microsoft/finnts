#' Gets the right fourier periods
#' 
#' Checks if fourier_periods is set if not gets right one
#' 
#' 
#' @param fourier_periods fourier_periods override
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns fourier_periods
get_fourier_periods <- function(fourier_periods,
                                date_type){
  
  if(!is.null(fourier_periods)) {
    return(fourier_periods)
  }
  
  fourier_periods <- switch(date_type,
                            "year" = c(1,2,3,4,5),
                            "quarter" = c(1,2,3,4),
                            "month" = c(3, 6, 9, 12),
                            "week" = c(2, 4, 8, 12, 24, 48, 52),
                            "day" = c(7, 14, 21, 28, 28*2, 
                                      28*3, 28*6, 28*9, 28*12, 365))  

  return(fourier_periods)
}

#' Gets the right lag periods
#' 
#' Checks if lag_periods is set if not gets right one
#' 
#' 
#' @param lag_periods lag_periods override
#' @param date_type year, quarter, month, week, day
#' @param forecast_horizon 
#' 
#' @return Returns lag_periods
get_lag_periods <- function(lag_periods, 
                            date_type,
                            forecast_horizon){
  
  if(!is.null(lag_periods)) {
    return(lag_periods)
  } 
  oplist <- switch (date_type,
                    "year" = c(1,2,3),
                    "quarter" = c(1,2,3,4),
                    "month" =  c(1, 2, 3, 6, 9, 12),
                    "week" = c(1, 2, 3, 4, 8, 12, 24, 48, 52),
                    "day" = c(1, 2, 3, 4, 5, 6, 7, 14, 
                              21, 28, 28*2, 28*3, 28*6, 
                              28*9, 28*12, 365)
                    )
  
  oplist <- c(oplist,forecast_horizon)
  lag_periods <- oplist[oplist >= forecast_horizon]
  lag_periods <- unique(lag_periods)
  
  return(lag_periods)
}

#' Gets the right rolling window periods
#' 
#' Checks if rolling_window_periods is set if not gets right one
#' 
#' 
#' @param rolling_window_periods rolling_window_periods override
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns rolling_window_periods
get_rolling_window_periods <- function(rolling_window_periods,
                                date_type){
  
  if(!is.null(rolling_window_periods)) {
    return(rolling_window_periods)
  }
  
  rolling_window_periods <- switch(date_type,
                            "year" = c(2,3,4,5),
                            "quarter" = c(2,3,4),
                            "month" = c(3, 6, 9, 12),
                            "week" = c(2, 4, 8, 12, 24, 48, 52),
                            "day" = c(7, 14, 21, 28, 28*2, 
                                      28*3, 28*6, 28*9, 28*12, 365))  
  
  return(rolling_window_periods)
}

#' Gets the right frequency numbers
#' 
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns frequency_number
get_frequency_number <- function(date_type){
  
  frequency_number <- switch(date_type,
                                   "year" = 1,
                                   "quarter" = 4,
                                   "month" = 12,
                                   "week" = 365.25/7,
                                   "day" = 365.25)  
  
  return(frequency_number)
}

#' Gets the right gluon ts frequency
#' 
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns gluon_ts_frequency
get_gluon_ts_frequency <- function(date_type){
  
  gluon_ts_frequency <- switch(date_type,
                             "year" = "Y",
                             "quarter" = "Q",
                             "month" = "M",
                             "week" = "W",
                             "day" = "D")  
  
  return(gluon_ts_frequency)
}

#' Gets the seasonal periods
#' 
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns seasonal_periods
get_seasonal_periods <- function(date_type){
  
  seasonal_periods <- switch(date_type,
                               "year" = c(1, 2, 3),
                               "quarter" = c(4, 6, 12),
                               "month" = c(12, 6, 4),
                               "week" = c(365.25/7, (365.25/7)/4, (365.25/7)/12),
                               "day" =  c(365.25, 365.25/4, 365.25/12))  
  
  return(seasonal_periods)
}

#' Gets the date regex for pasing
#' 
#' Checks if date_regex  is set if not gets right one
#' 
#' 
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns date_regex
get_date_regex <- function(date_type){
  
  date_regex <- switch(date_type,
                             "year" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(half)|(quarter)|(month)|(week)|(day)",
                             "quarter" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(month)|(week)|(day)",
                             "month" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(day)",
                             "week" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(day)",
                             "day" =   "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")  
  
  return(date_regex)
}

#' Gets the back testing spacing
#' 
#' Checks if back_test_spacing is set to auto and gets the right one
#' 
#' 
#' @param back_test_spacing back_test_spacing override
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns back_test_spacing
get_back_test_spacing <- function(back_test_spacing,
                                       date_type){
  
  if(back_test_spacing != "auto") {
    return(back_test_spacing)
  }
  
  if(date_type == "day") {
    back_test_spacing <- 7
  } else if(date_type == "week") {
    back_test_spacing <- 4
  } else {
    back_test_spacing <- 1
  }
  
  return(back_test_spacing)
}

