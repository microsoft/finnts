#' multivariate_prep_recipe_1
#' 
#' @param data input data
#' @param external_regressors external regressors to build features with
#' @param xregs_future_values_list external regressors that have future values
#' @param fourier_periods list of periods when building fourier features 
#' @param lag_periods list of periods when building lag features
#' @param rolling_window_periods list of periods when building rolling window features
#' 
#' @return feature engineered data to input into models
#' @noRd
multivariate_prep_recipe_1 <- function(data, external_regressors, xregs_future_values_list, fourier_periods, 
                                       lag_periods, rolling_window_periods) {
  
  data_trans <- tibble::tibble()
  
  #apply polynomial transformations
  numeric_xregs <- c()
  
  df_poly <- data
  
  for(column in c("Target", external_regressors)) {
    
    if(is.numeric(dplyr::select(data, column)[[1]])) {
      
      column_names_final <- c(column)
      
      if((column %in% external_regressors) & !(column %in% xregs_future_values_list)) {
        numeric_xregs <- c(numeric_xregs, stringr::str_c(column, c("", "_squared", "_cubed", "_log")))
        column_names_final <- stringr::str_c(column, c("", "_squared", "_cubed", "_log"))
      }
      
      if(column %in% external_regressors) {
        
        df_poly_column <- data %>%
          dplyr::select(column)
        
        temp_squared <- df_poly_column^2
        temp_cubed <- df_poly_column^3
        temp_log <- log1p(df_poly_column)
        
        temp_final <- cbind(temp_squared, temp_cubed, temp_log)
        colnames(temp_final) <- c(paste0(column, '_squared'), paste0(column, '_cubed'), paste0(column, '_log'))
        
        df_poly <- cbind(df_poly, temp_final)
        
      } 
    }
  }
  
  #add lags and rolling window calcs
  data_period <- df_poly %>%
    dplyr::group_by(Combo) %>%
    dplyr::group_split() %>%
    purrr::map(.f = function(df) {
      
      
      df_lag_final <- df
      
      #apply lags
      for(column in colnames(df %>% dplyr::select(contains(c("Target", external_regressors))))) {
        
        df_lag <- df %>%
          timetk::tk_augment_lags(column, .lags = lag_periods) %>%
          tidyr::fill(stringr::str_c(column, "_lag", lag_periods), .direction = "up") %>%
          dplyr::select(stringr::str_c(column, "_lag", lag_periods))
        
        colnames(df_lag) <- stringr::str_c(column, "_lag", lag_periods)
        
        df_lag_final <- cbind(df_lag_final, df_lag)
      }
      
      # final_tbl <- final_tbl %>% 
      #   dplyr::select(-numeric_xregs)
      
      #apply rolling window calculations
      df_window_final <- df_lag_final
      
      for(period_num in lag_periods) {
        
        column <- paste0("Target_lag", period_num)
        
        df_roll <- df_lag_final %>%
          dplyr::arrange(Date) %>%
          tidyr::fill(column, .direction = "up") %>%
          timetk::tk_augment_slidify(
            column, 
            .f = ~mean(.x, na.rm = TRUE),
            .period = rolling_window_periods,
            .partial = TRUE, 
            .align = "right",
            .names = stringr::str_c(column, "_roll", rolling_window_periods, "_Avg")
          ) %>%
          timetk::tk_augment_slidify(
            column,
            .f = ~sum(.x, na.rm = TRUE),
            .period = rolling_window_periods,
            .partial = TRUE,
            .align = "right",
            .names = stringr::str_c(column, "_roll", rolling_window_periods, "_Sum")
          ) %>%
          timetk::tk_augment_slidify(
            column,
            .f = ~sd(.x, na.rm = TRUE),
            .period = rolling_window_periods,
            .partial = TRUE,
            .align = "right",
            .names = stringr::str_c(column, "_roll", rolling_window_periods, "_StdDev")
          )
        
        df_window_final <- cbind(df_window_final, df_roll %>% dplyr::select(c(stringr::str_c(column, "_roll", rolling_window_periods, "_Avg"), stringr::str_c(column, "_roll", rolling_window_periods, "_Sum"), stringr::str_c(column, "_roll", rolling_window_periods, "_StdDev"))))
      }
      
      df_window_final[is.na(df_window_final)] = 0.00
      
      return(df_window_final)
      
    }) %>%
    dplyr::bind_rows() %>%
    timetk::tk_augment_fourier(Date, .periods = fourier_periods, .K = 2) %>% #add fourier series
    tidyr::fill(contains("_roll"), .direction = "down")
  
  #drop xregs that do not contain future values
  data_period <- data_period %>%
    dplyr::select(-numeric_xregs)
  
  return(data_period)
}

#' multivariate prep recipe 2
#' 
#' @param data input data
#' @param external_regressors external regressors to build features with
#' @param xregs_future_values_list external regressors that have future values
#' @param fourier_periods list of periods when building fourier features 
#' @param lag_periods list of periods when building lag features
#' @param rolling_window_periods list of periods when building rolling window features
#' @param date_type date type
#' @param forecast_horizon forecast horizon
#' 
#' @return feature engineered data to input into models
#' @noRd
multivariate_prep_recipe_2 <- function(data, external_regressors, xregs_future_values_list, 
                                       fourier_periods, lag_periods, rolling_window_periods, 
                                       date_type, forecast_horizon) {
  
  data_trans <- tibble::tibble()
  
  #apply polynomial transformations
  numeric_xregs <- c()
  
  df_poly <- data
  
  for(column in c("Target", external_regressors)) {
    
    if(is.numeric(dplyr::select(data, column)[[1]])) {
      
      column_names_final <- c(column)
      
      if((column %in% external_regressors) & !(column %in% xregs_future_values_list)) {
        numeric_xregs <- c(numeric_xregs, stringr::str_c(column, c("", "_squared", "_cubed", "_log")))
        column_names_final <- stringr::str_c(column, c("", "_squared", "_cubed", "_log"))
      }
      
      if(column %in% external_regressors) {
        
        df_poly_column <- data %>%
          dplyr::select(column)
        
        temp_squared <- df_poly_column^2
        temp_cubed <- df_poly_column^3
        temp_log <- log1p(df_poly_column)
        
        temp_final <- cbind(temp_squared, temp_cubed, temp_log)
        colnames(temp_final) <- c(paste0(column, '_squared'), paste0(column, '_cubed'), paste0(column, '_log'))
        
        df_poly <- cbind(df_poly, temp_final)
        
      } 
    }
  }
  
  #add horizon specific features
  if(date_type == 'day') {
    lag_periods_r2 <- unique(c(1, 2, 3, 4, 5, 6, 7, 14, 21, 28, 28*2, 28*3, 28*6, 28*9, 28*12, 365, forecast_horizon))
  } else {
    lag_periods_r2 <- 1:forecast_horizon
  }
  
  
  for(period in 1:forecast_horizon) {
    
    #add horizon and origin components
    data_period <- df_poly %>%
      # dplyr::mutate(Horizon = period, 
      #               Origin = dplyr::row_number()-period) %>%
      dplyr::group_by(Combo) %>%
      dplyr::mutate(Horizon = period, 
                    Origin = dplyr::row_number()-period) %>%
      dplyr::group_split() %>%
      purrr::map(.f = function(df) {
        
        #apply lags
        df_lag_final <- df
        
        for(column in colnames(df %>% dplyr::select(contains(c("Target", external_regressors))))) {
          
          df_lag <- df %>%
            timetk::tk_augment_lags(column, .lags = unique(c(lag_periods_r2, lag_periods))+(period-1)) %>%
            tidyr::fill(stringr::str_c(column, "_lag", unique(c(lag_periods_r2, lag_periods))+(period-1)), .direction = "up") %>%
            dplyr::select(stringr::str_c(column, "_lag", unique(c(lag_periods_r2, lag_periods))+(period-1))) 
          
          colnames(df_lag) <- stringr::str_c(column, "_lag", unique(c(lag_periods_r2, lag_periods)))
          
          df_lag_final <- cbind(df_lag_final, df_lag)
        }
        
        #apply rolling window calculations
        df_window_final <- df_lag_final
        
        for(period_num in unique(c(lag_periods_r2, lag_periods))) {
          
          column <- paste0("Target_lag", period_num)
          
          df_roll <- df_lag_final %>%
            dplyr::arrange(Date) %>%
            tidyr::fill(column, .direction = "up") %>%
            timetk::tk_augment_slidify(
              column,
              .f = ~mean(.x, na.rm = TRUE),
              .period = rolling_window_periods,
              .partial = TRUE,
              .align = "right",
              .names = stringr::str_c(column, "_roll", rolling_window_periods, "_Avg")
            ) %>%
            timetk::tk_augment_slidify(
              column,
              .f = ~sum(.x, na.rm = TRUE),
              .period = rolling_window_periods,
              .partial = TRUE,
              .align = "right",
              .names = stringr::str_c(column, "_roll", rolling_window_periods, "_Sum")
            ) %>%
            timetk::tk_augment_slidify(
              column,
              .f = ~sd(.x, na.rm = TRUE),
              .period = rolling_window_periods,
              .partial = TRUE,
              .align = "right",
              .names = stringr::str_c(column, "_roll", rolling_window_periods, "_StdDev")
            )
          
          is.na(df_roll) <- sapply(df_roll, is.nan)
          
          df_roll <- df_roll %>%
            tidyr::fill(contains("_roll"), .direction = "down")
          
          df_window_final <- cbind(df_window_final, df_roll %>% dplyr::select(c(stringr::str_c(column, "_roll", rolling_window_periods, "_Avg"), stringr::str_c(column, "_roll", rolling_window_periods, "_Sum"), stringr::str_c(column, "_roll", rolling_window_periods, "_StdDev"))))
        }
        
        df_window_final[is.na(df_window_final)] = 0.00
        
        return(df_window_final)
        
      }) %>%
      dplyr::bind_rows() %>%
      timetk::tk_augment_fourier(Date, .periods = fourier_periods, .K = 2) #add fourier series
    
    #drop xregs that do not contain future values
    data_period <- data_period %>%
      dplyr::select(-numeric_xregs)
    
    #combine transformed data
    data_trans <- rbind(data_trans, data_period) 
  }
  
  return(data_trans)
}