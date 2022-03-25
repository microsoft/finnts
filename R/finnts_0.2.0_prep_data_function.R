
#' Function to perform log transformation
#' 
#' @param df data frame
#' @param target_log_transformation variable to indicate log transformation
#' 
#' @return tbl with or without log transformation
#' @noRd
get_log_transformation <- function(df,
                                   target_log_transformation){
  if(target_log_transformation){
    df %>%
      dplyr::mutate(Target = log1p(Target),
                    Target = ifelse(is.nan(Target), 0, Target))
  } else{
    df
  }
}

#' Function to remove time series that don't contain recent values
#' 
#' @param df data frame
#' @param combo_cleanup_date date value to test for non-zero values after
#' 
#' @return tbl with or without specific time series removed
#' @noRd
combo_cleanup_fn <- function(df,combo_cleanup_date){
  
  if(!is.null(combo_cleanup_date)) {
    
    combo_df <- df %>%
      dplyr::filter(Date >= combo_cleanup_date) %>%
      tidyr::drop_na(Target) %>%
      data.frame() %>%
      dplyr::group_by(Combo) %>%
      dplyr::summarise(Sum=sum(Target, na.rm = TRUE)) %>%
      data.frame() %>%
      dplyr::filter(Sum != 0)
    
    combo_df <- unique(as.character(combo_df$Combo))
    
    df %>% dplyr::filter(Combo %in% combo_df)
    
  }
  else{
    df
  }
}

#' Function to create hierarchical time series
#' 
#' @param data_tbl data frame
#' @param combo_variables list of unique time series combinations
#' @param forecast_approach indicates what type of hierarchy
#' @param frequency_number number of time series frequency
#' @param return_type whether to return a hts df or the hierarchy structure to use in forecast reconciliation 
#' 
#' @return tbl with or without a hierarchical structure
#' @noRd
get_hts <- function(data_tbl,
                    combo_variables,
                    forecast_approach,
                    frequency_number, 
                    return_type = "data"){
  
  # Group List for Grouped Hierarchy
  get_group_list <- function(data_hts_gts_df){
    
    group_list <- vector()
    
    for(variable in combo_variables) {
      
      var = data_hts_gts_df[[variable]]
      
      group_list = rbind(group_list, var)
    }
    rownames(group_list) <- combo_variables
    
    return(group_list)
  }
  
  # Node List for Standard Hierarchy
  get_node_list <- function(data_hts_gts_df){
    hierarchy_length_tbl <- tibble::tibble()
    
    node_list <- list()
    
    num <- 1
    
    for(variable in combo_variables) {
      
      hierarchy_length_tbl <- rbind(hierarchy_length_tbl, 
                                    tibble::tibble(Variable = variable, 
                                                   Count = length(unique(data_tbl[[variable]]))
                                    )
      )
      
    }
    
    hierarchy_combo_variables <- hierarchy_length_tbl %>%
      dplyr::arrange(Count) %>%
      dplyr::select(Variable) %>%
      unlist(use.names = FALSE)
    
    for(variable in hierarchy_combo_variables) {
      
      if(num == 1) {
        
        node_list = append(node_list, length(unique(data_tbl[[variable]])))
        
        num <- num+1
        
      } else {
        
        grouping_current <- variable
        
        grouping_minus_1 <- hierarchy_combo_variables[num-1]
        
        grouping_values <- data_hts_gts_df %>%
          dplyr::group_by(dplyr::across(tidyselect::all_of(c(grouping_minus_1, grouping_current)))) %>%
          dplyr::summarise(Sum = sum(Sum, na.rm=TRUE)) %>%
          dplyr::mutate(Sum = 1) %>%
          dplyr::group_by(dplyr::across(tidyselect::all_of(grouping_minus_1))) %>%
          dplyr::summarise(Count = sum(Sum)) %>%
          dplyr::select(Count) %>%
          unlist(use.names = FALSE)
        
        node_list = append(node_list, list(grouping_values))
        num <- num+1
      }
    }
    
    return (node_list)
  }
  
  # Pick between group_list and node_list
  pick_right_list <- function(data_hts_gts_df){
    if(forecast_approach == 'grouped_hierarchy'){
      data_hts_gts_df %>% get_group_list
    }else{
      data_hts_gts_df %>% get_node_list
    }
  }
  
  # getting the right hts
  get_hts <- function(data_ts,some_list){
    
    if(forecast_approach == "grouped_hierarchy") {
      
      data_ts %>%
        hts::gts(groups = some_list)
      
    } else{
      
      data_ts %>%
        hts::hts(nodes = some_list)
    }
  }
  
  # return correct hts info
  data_hts_return <- function(df, ret_obj, hts_list){
    if(ret_obj == "data"){
      
      Date = df$Date
      
      df %>%
        dplyr::select(-Date) %>%
        stats::ts(frequency = frequency_number)%>% 
        get_hts(hts_list)  %>%
        hts::allts() %>%
        data.frame() %>%
        tibble::add_column(Date = Date,
                           .before = 1)%>%
        tidyr::pivot_longer(!Date, 
                            names_to = "Combo", 
                            values_to = "Target") %>%
        tibble::tibble()
    } else if(ret_obj == "hts_gts") {
      data_ts <- df %>%
        dplyr::select(-Date) %>%
        stats::ts(frequency = frequency_number)
      
      hts_gts <- data_ts %>%
        get_hts(hts_list)
      
      return(list(data_ts = data_ts, hts_gts = hts_gts))
    } else{
      df
    }
  }
  
  # main data table function to produce our table
  data_tbl_func <- function(df, return_type = "data"){
    
    if(forecast_approach == 'bottoms_up'){
      df
    }
    else{
      
      some_list <- df %>%
        dplyr::mutate(Target = tidyr::replace_na(Target, 0)) %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(combo_variables))) %>%
        dplyr::summarise(Sum=sum(Target, na.rm=TRUE)) %>%
        data.frame() %>% 
        pick_right_list()
      
      data_cast <- df %>%
        dplyr::arrange(Combo, Date) %>%
        dplyr::select(-combo_variables) %>%
        tidyr::pivot_wider(names_from = Combo, 
                           values_from = Target) %>%
        dplyr::mutate_if(is.numeric, list(~replace(., is.na(.), 0))) # replace NA values with zero for hts aggregations
      
      data_cast %>%
        data_hts_return(ret_obj = return_type, hts_list = some_list)
      
    }
    
  }
  
  data_tbl %>% data_tbl_func(return_type = return_type)
  
}

#' Function to get external regressor features that contain future values after hist_end_date
#' 
#' @param data_tbl data frame
#' @param external_regressors list of external regressors
#' @param hist_end_date date of when your historical data ends
#' @param forecast_approach indicates what type of hierarchical time series method 
#' 
#' @return tbl with external regressors with future values
#' @noRd
get_xregs_future_values_tbl <- function(data_tbl, 
                                        external_regressors,
                                        hist_end_date, 
                                        forecast_approach){
  
  if(forecast_approach != 'bottoms_up'){
    
    data_tbl %>%
      tibble::tibble() %>%
      dplyr::select(Combo, Date)
    
  } else{
    
    xregs_future_values_list <- c()
    
    for(variable in external_regressors) {
      
      temp <- data_tbl %>%
        dplyr::filter(Date > hist_end_date) %>%
        dplyr::select(variable) %>%
        tidyr::drop_na()
      
      if(nrow(temp) > 0) {
        
        xregs_future_values_list <- append(xregs_future_values_list, 
                                           variable)
      }
    }
    
    data_tbl %>%
      dplyr::select(Combo, 
                    Date, 
                    tidyselect::all_of(xregs_future_values_list))
    
    
  }
  
}

#' Function to replace outliers and fill in missing values
#' 
#' @param df data frame
#' @param clean_outliers clean outliers or not
#' @param clean_missing_values clean missing values or not
#' @param frequency_number number of time series frequency
#' @param external_regressors list of external regressors
#' 
#' @return tbl with or without missing/outlier values replaced
#' @noRd
clean_outliers_missing_values <- function(df,
                                          clean_outliers,
                                          clean_missing_values,
                                          frequency_number,
                                          external_regressors){
  
  
  correct_clean_func <- function(col){
    if(clean_missing_values & sum(!is.na(col))<2){
      col
    }else if(clean_outliers){
      timetk::ts_clean_vec(col,period = frequency_number)
    }else if(clean_missing_values){
      timetk::ts_impute_vec(col,period = frequency_number)
    }else {
      col
    }
  }
  
  df %>%
    dplyr::mutate(
      dplyr::across(
        (where(is.numeric) & c("Target", external_regressors)),
        correct_clean_func
      )
    ) %>%
    tibble::tibble()
}

#' Function to get frequency number of time series
#' 
#' @param date_type date type
#' 
#' @return frequency number of time series
#' @noRd
get_frequency_number <- function(date_type){
  
  frequency_number <- switch(date_type,
                             "year" = 1,
                             "quarter" = 4,
                             "month" = 12,
                             "week" = 365.25/7,
                             "day" = 365.25)  
  
  return(frequency_number)
}

#' Function to get how many fourier periods to use
#' 
#' @param fourier_periods list of fourier periods 
#' @param date_type date type
#' 
#' @return list of fourier periods
#' @noRd
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

#' Function to get lag periods to use
#' 
#' @param lag_periods list of lag periods
#' @param date_type date type
#' @param forecast_horizon forecast horizon
#' 
#' @return list of lag periods
#' @noRd
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
                    "day" = c(7, 14, 21, 28, 60, 90, 180, 365)
  )
  
  oplist <- c(oplist,forecast_horizon)
  lag_periods <- oplist[oplist >= forecast_horizon]
  lag_periods <- unique(lag_periods)
  
  return(lag_periods)
}

#' Function to get rolling window periods
#' 
#' @param rolling_window_periods list of rolling window periods
#' @param date_type date type
#' 
#' @return list of rolling window periods
#' @noRd
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

#' Function to get recipes to use in feature engineering
#' 
#' @param recipes_to_run list of recipes to run
#' @param date_type date type
#' 
#' @return list of recipes to run
#' @noRd
get_recipes_to_run <- function(recipes_to_run,
                               date_type){
  
  if(is.null(recipes_to_run)) {
    switch(date_type,
           "year" = c("R1", "R2"),
           "quarter" = c("R1", "R2"),
           "month" = c("R1", "R2"),
           "week" = c("R1"),
           "day" = c("R1")) 
  } else {
    recipes_to_run
  }
}

#' Gets the date regex for removing unneeded date features
#' 
#' @param date_type year, quarter, month, week, day
#' 
#' @return Returns date_regex
#' @noRd
get_date_regex <- function(date_type){
  
  date_regex <- switch(date_type,
                       "year" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(half)|(quarter)|(month)|(week)|(day)",
                       "quarter" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(month)|(week)|(day)",
                       "month" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(day)",
                       "week" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(day)",
                       "day" =   "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")  
  
  return(date_regex)
}

#' Function to perform feature engineering according to R1 recipe
#' 
#' @param data data frame
#' @param external_regressors list of external regressors
#' @param xregs_future_values_list list of external regressors that contain future values
#' @param fourier_periods list of fourier periods
#' @param lag_periods list of lag periods
#' @param rolling_window_periods list of rolling window periods
#' 
#' @return tbl with R1 feature engineering applied
#' @noRd
multivariate_prep_recipe_1 <- function(data, 
                                       external_regressors, 
                                       xregs_future_values_list, 
                                       fourier_periods, 
                                       lag_periods, 
                                       rolling_window_periods) {
  
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
  #add lags, rolling window calcs, and fourier periods
  data_lag_window <- df_poly %>%
    timetk::tk_augment_lags(tidyselect::contains(c("Target", external_regressors)), .lags = lag_periods) %>% # create lags
    tidyr::fill(tidyselect::contains(c("Target", external_regressors)), .direction = "up") %>%
    timetk::tk_augment_slidify( # create rolling windows
      tidyselect::any_of(stringr::str_c("Target_lag", lag_periods)),
      .f = ~mean(.x, na.rm = TRUE),
      .period = rolling_window_periods,
      .partial = TRUE,
      .align = "right", 
      .names = lapply(rolling_window_periods, 
                      FUN = function(x) {
                        stringr::str_c(stringr::str_c("Target_lag", lag_periods, "_roll"), x, "_Avg")
                      }) %>%
        unlist()
    ) %>%
    timetk::tk_augment_slidify(
      tidyselect::any_of(stringr::str_c("Target_lag", lag_periods)),
      .f = ~sum(.x, na.rm = TRUE),
      .period = rolling_window_periods,
      .partial = TRUE,
      .align = "right", 
      .names = lapply(rolling_window_periods, 
                      FUN = function(x) {
                        stringr::str_c(stringr::str_c("Target_lag", lag_periods, "_roll"), x, "_Sum")
                      }) %>%
        unlist()
    ) %>%
    timetk::tk_augment_slidify(
      tidyselect::any_of(stringr::str_c("Target_lag", lag_periods)),
      .f = ~sd(.x, na.rm = TRUE),
      .period = rolling_window_periods,
      .partial = TRUE,
      .align = "right", 
      .names = lapply(rolling_window_periods, 
                      FUN = function(x) {
                        stringr::str_c(stringr::str_c("Target_lag", lag_periods, "_roll"), x, "_StdDev")
                      }) %>%
        unlist()
    ) %>%
    timetk::tk_augment_fourier(Date, .periods = fourier_periods, .K = 2) %>% #add fourier series
    tidyr::fill(tidyselect::contains("_roll"), .direction = "down") %>%
    dplyr::select(-numeric_xregs)
  
  data_lag_window[is.na(data_lag_window)] = 0.00
  
  return(data_lag_window)
}

#' Function to perform feature engineering according to R2 recipe
#' 
#' @param data data frame
#' @param external_regressors list of external regressors
#' @param xregs_future_values_list list of external regressors that contain future values
#' @param fourier_periods list of fourier periods
#' @param lag_periods list of lag periods
#' @param rolling_window_periods list of rolling window periods
#' @param date_type date type
#' @param forecast_horizon forecast horizon
#' 
#' @return tbl with R2 feature engineering applied
#' @noRd
multivariate_prep_recipe_2 <- function(data, 
                                       external_regressors, 
                                       xregs_future_values_list, 
                                       fourier_periods, 
                                       lag_periods, 
                                       rolling_window_periods, 
                                       date_type, 
                                       forecast_horizon) {
  
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
    lag_periods_r2 <- unique(c(7, 14, 21, 30, 90, 180, 365, forecast_horizon))
  } else {
    lag_periods_r2 <- 1:forecast_horizon
  }
  
  for(period in 1:forecast_horizon) {
    
    #add horizon and origin components
    data_lag_window <- df_poly %>%
      #dplyr::group_by(Combo) %>%
      dplyr::mutate(Horizon = period, 
                    Origin = dplyr::row_number()-period) %>%
      timetk::tk_augment_lags(tidyselect::contains(c("Target", external_regressors)), 
                              .lags = unique(c(lag_periods_r2, lag_periods))+(period-1),
                              .names = stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)))) %>%
      tidyr::fill(tidyselect::contains(c("Target", external_regressors)), .direction = "up") %>%
      timetk::tk_augment_slidify(
        tidyselect::any_of(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)))),
        .f = ~mean(.x, na.rm = TRUE),
        .period = rolling_window_periods,
        .partial = TRUE,
        .align = "right",
        .names = lapply(rolling_window_periods, 
                        FUN = function(x) {
                          stringr::str_c(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)), "_roll"), x, "_Avg")
                        }) %>%
          unlist()
      ) %>%
      timetk::tk_augment_slidify(
        tidyselect::any_of(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)))),
        .f = ~sum(.x, na.rm = TRUE),
        .period = rolling_window_periods,
        .partial = TRUE,
        .align = "right",
        .names = lapply(rolling_window_periods, 
                        FUN = function(x) {
                          stringr::str_c(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)), "_roll"), x, "_Sum")
                        }) %>%
          unlist()
      ) %>%
      timetk::tk_augment_slidify(
        tidyselect::any_of(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)))),
        .f = ~sd(.x, na.rm = TRUE),
        .period = rolling_window_periods,
        .partial = TRUE,
        .align = "right",
        .names = lapply(rolling_window_periods, 
                        FUN = function(x) {
                          stringr::str_c(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)), "_roll"), x, "_StdDev")
                        }) %>%
          unlist()
      ) %>%
      tidyr::fill(tidyselect::contains("_roll"), .direction = "down") %>%
      timetk::tk_augment_fourier(Date, .periods = fourier_periods, .K = 2) %>% #add fourier series
      dplyr::select(-numeric_xregs) #drop xregs that do not contain future values
    
    is.na(data_lag_window) <- sapply(data_lag_window, is.nan)
    data_lag_window[is.na(data_lag_window)] = 0.00

    #combine transformed data
    data_trans <- rbind(data_trans, data_lag_window) 
  }
  
  return(data_trans)
}

#' Prep Data
#' 
#' Preps data with various feature engineering recipes to create features before training models
#' 
#' @param input_data A data frame or tibble of historical time series data. Can also include external regressors for both 
#'   historical and future data. 
#' @param combo_variables List of column headers within input data to be used to separate individual time series. 
#' @param target_variable The column header formatted as a character value within input data you want to forecast.
#' @param date_type The date granularity of the input data. Finn accepts the following as a character string
#'   day, week, month, quarter, year.
#' @param forecast_horizon Number of periods to forecast into the future.
#' @param external_regressors List of column headers within input data to be used as features in multivariate models.
#' @param hist_start_date Date value of when your input_data starts. Default of NULL is to use earliest date value in 
#'   input_data.
#' @param hist_end_date Date value of when your input_data ends.Default of NULL is to use the latest date value in 
#'   input_data.
#' @param combo_cleanup_date Date value to remove individual time series that don't contain non-zero values after 
#'   that specified date. Default of NULL is to not remove any time series and attempt to forecast all of them. 
#' @param fiscal_year_start Month number of start of fiscal year of input data, aids in building out date features. 
#'   Formatted as a numeric value. Default of 1 assumes fiscal year starts in January. 
#' @param clean_missing_values If TRUE, cleans missing values. Only impute values for missing data within an 
#'   existing series, and does not add new values onto the beginning or end, but does provide a value of 0 for said 
#'   values. 
#' @param clean_outliers If TRUE, outliers are cleaned and inputted with values more in line with historical data
#' @param forecast_approach How the forecast is created. The default of 'bottoms_up' trains models for each individual 
#'   time series. 'grouped_hierarchy' creates a grouped time series to forecast at while 'standard_hierarchy' creates 
#'   a more traditional hierarchical time series to forecast, both based on the hts package.   
#' @param parallel_processing Default of NULL runs no parallel processing and forecasts each individual time series
#'   one after another. 'local_machine' leverages all cores on current machine Finn is running on. 'azure_batch'
#'   runs time series in parallel on a remote compute cluster in Azure Batch. 
#' @param num_cores Number of cores to run when parallel processing is set up. Used when running parallel computations 
#'   on local machine or within Azure. Default of NULL uses total amount of cores on machine minus one. Can't be greater 
#'   than number of cores on machine minus 1.
#' @param target_log_transformation If TRUE, log transform target variable before training models. 
#' @param fourier_periods List of values to use in creating fourier series as features. Default of NULL automatically chooses 
#'   these values based on the date_type. 
#' @param lag_periods List of values to use in creating lag features. Default of NULL automatically chooses these values 
#'   based on date_type.
#' @param rolling_window_periods List of values to use in creating rolling window features. Default of NULL automatically 
#'   chooses these values based on date type.
#' @param recipes_to_run List of recipes to run on multivariate models that can run different recipes. A value of NULL runs 
#'   all recipes, but only runs the R1 recipe for weekly and daily date types. A value of "all" runs all recipes, regardless 
#'   of date type. A list like c("R1") or c("R2") would only run models with the R1 or R2 recipe.  
#' 
#' @return A dataframe with nested list values for feature engineering applied by recipe
#' @noRd
construct_prep_time_series <- function(input_data,
                                       combo_variables,
                                       target_variable,
                                       date_type,
                                       forecast_horizon,
                                       external_regressors,
                                       hist_start_date,
                                       hist_end_date,
                                       combo_cleanup_date,
                                       fiscal_year_start,
                                       clean_missing_values,
                                       clean_outliers,
                                       forecast_approach,
                                       parallel_processing,
                                       num_cores, 
                                       target_log_transformation, 
                                       fourier_periods,
                                       lag_periods,
                                       rolling_window_periods,
                                       recipes_to_run, 
                                       get_xregs_future_values_tbl, 
                                       clean_outliers_missing_values, 
                                       get_frequency_number, 
                                       multivariate_prep_recipe_1, 
                                       multivariate_prep_recipe_2, 
                                       get_fourier_periods, 
                                       get_lag_periods, 
                                       get_rolling_window_periods, 
                                       get_date_regex) {
  
  
  
  prep_time_series <- function(combo) {

    xregs_future_tbl <- get_xregs_future_values_tbl(input_data,
                                                    external_regressors,
                                                    hist_end_date,
                                                    forecast_approach)
    
    if(length(colnames(xregs_future_tbl)) > 2) {
      xregs_future_list <- xregs_future_tbl %>% dplyr::select(-Date, -Combo) %>% colnames()
    } else {
      xregs_future_list <- NULL
    }
    
    initial_tbl <- input_data %>%
      dplyr::filter(Combo == combo) %>%
      dplyr::select(Combo,
                    Date,
                    Target,
                    tidyselect::all_of(external_regressors)) %>%
      dplyr::group_by(Combo) %>%
      timetk::pad_by_time(Date,
                          .by = date_type,
                          .pad_value = ifelse(clean_missing_values, NA, 0),
                          .end_date = hist_end_date) %>% #fill in missing values in between existing data points
      timetk::pad_by_time(Date,
                          .by = date_type,
                          .pad_value = 0,
                          .start_date = hist_start_date,
                          .end_date = hist_end_date) %>% #fill in missing values at beginning of time series with zero
      timetk::future_frame(Date,
                           .length_out = forecast_horizon,
                           .bind_data = TRUE) %>% #add future data
      dplyr::ungroup() %>%
      dplyr::left_join(xregs_future_tbl) %>% #join xregs that contain values given by user
      clean_outliers_missing_values(clean_outliers,
                                    clean_missing_values,
                                    get_frequency_number(date_type),
                                    external_regressors) %>% # clean outliers and missing values
      dplyr::mutate_if(is.numeric, list(~replace(., is.infinite(.), NA))) %>% # replace infinite values
      dplyr::mutate_if(is.numeric, list(~replace(., is.nan(.), NA))) %>% # replace NaN values
      dplyr::mutate_if(is.numeric, list(~replace(., is.na(.), 0))) %>% # replace NA values
      dplyr::mutate(Target = ifelse(Date > hist_end_date,
                                    NA,
                                    Target))
    
    date_features <- initial_tbl %>%
      dplyr::select(Date) %>%
      dplyr::mutate(Date_Adj = Date %m+% months(fiscal_year_start-1), 
                    Date_day_month_end = ifelse(lubridate::day(Date_Adj) == lubridate::days_in_month(Date_Adj), 1, 0)) %>%
      timetk::tk_augment_timeseries_signature(Date_Adj) %>%
      dplyr::select(!tidyselect::matches(get_date_regex(date_type)), -Date_Adj, -Date)
    
    names(date_features) <- stringr::str_c("Date_", names(date_features))
    
    initial_tbl <- initial_tbl %>%
      cbind(date_features)
    
    # Run Recipes
    if(is.null(recipes_to_run)) {
      run_all_recipes_override <- FALSE
    } else if(recipes_to_run == "all") {
      run_all_recipes_override <- TRUE
    } else {
      run_all_recipes_override <- FALSE
    }
    
    output_tbl <- NULL
    
    if(is.null(recipes_to_run) | "R1" %in% recipes_to_run | run_all_recipes_override) {
      
      R1 <- initial_tbl %>%
        multivariate_prep_recipe_1(external_regressors,
                                   xregs_future_values_list = xregs_future_list,
                                   get_fourier_periods(fourier_periods, date_type),
                                   get_lag_periods(lag_periods, date_type,forecast_horizon),
                                   get_rolling_window_periods(rolling_window_periods, date_type))
      
      output_tbl <- output_tbl %>%
        rbind(tibble::tibble(Combo = combo, 
                            Recipe = "R1",
                            Data = list(R1)))
      
    }
    
    if((is.null(recipes_to_run) & date_type %in% c("month", "quarter", "year")) | "R2" %in% recipes_to_run | run_all_recipes_override) {
      
      R2 <- initial_tbl %>%
        multivariate_prep_recipe_2(external_regressors,
                                   xregs_future_values_list = xregs_future_list,
                                   get_fourier_periods(fourier_periods, date_type),
                                   get_lag_periods(lag_periods, date_type,forecast_horizon),
                                   get_rolling_window_periods(rolling_window_periods, date_type),
                                   date_type,
                                   forecast_horizon)
      
      output_tbl <- output_tbl %>%
        rbind(tibble::tibble(Combo = combo, 
                            Recipe = "R2",
                            Data = list(R2)))
      
    }

    if(is.null(output_tbl)) {
      stop("Error in Running Feature Engineering Recipes")
    }
    
    return(output_tbl)
  }
  
  return(prep_time_series)
}

#' Prep Data
#' 
#' Preps data with various feature engineering reciepes to create features before training models
#' 
#' @param input_data A data frame or tibble of historical time series data. Can also include external regressors for both 
#'   historical and future data. 
#' @param combo_variables List of column headers within input data to be used to separate individual time series. 
#' @param target_variable The column header formatted as a character value within input data you want to forecast.
#' @param date_type The date granularity of the input data. Finn accepts the following as a character string
#'   day, week, month, quarter, year.
#' @param forecast_horizon Number of periods to forecast into the future.
#' @param external_regressors List of column headers within input data to be used as features in multivariate models.
#' @param hist_start_date Date value of when your input_data starts. Default of NULL is to use earliest date value in 
#'   input_data.
#' @param hist_end_date Date value of when your input_data ends.Default of NULL is to use the latest date value in 
#'   input_data.
#' @param combo_cleanup_date Date value to remove individual time series that don't contain non-zero values after 
#'   that specified date. Default of NULL is to not remove any time series and attempt to forecast all of them. 
#' @param fiscal_year_start Month number of start of fiscal year of input data, aids in building out date features. 
#'   Formatted as a numeric value. Default of 1 assumes fiscal year starts in January. 
#' @param clean_missing_values If TRUE, cleans missing values. Only impute values for missing data within an 
#'   existing series, and does not add new values onto the beginning or end, but does provide a value of 0 for said 
#'   values. 
#' @param clean_outliers If TRUE, outliers are cleaned and inputted with values more in line with historical data
#' @param forecast_approach How the forecast is created. The default of 'bottoms_up' trains models for each individual 
#'   time series. 'grouped_hierarchy' creates a grouped time series to forecast at while 'standard_hierarchy' creates 
#'   a more traditional hierarchical time series to forecast, both based on the hts package.   
#' @param parallel_processing Default of NULL runs no parallel processing and forecasts each individual time series
#'   one after another. 'local_machine' leverages all cores on current machine Finn is running on. 'azure_batch'
#'   runs time series in parallel on a remote compute cluster in Azure Batch. 
#' @param num_cores Number of cores to run when parallel processing is set up. Used when running parallel computations 
#'   on local machine or within Azure. Default of NULL uses total amount of cores on machine minus one. Can't be greater 
#'   than number of cores on machine minus 1.
#' @param target_log_transformation If TRUE, log transform target variable before training models. 
#' @param fourier_periods List of values to use in creating fourier series as features. Default of NULL automatically chooses 
#'   these values based on the date_type. 
#' @param lag_periods List of values to use in creating lag features. Default of NULL automatically chooses these values 
#'   based on date_type.
#' @param rolling_window_periods List of values to use in creating rolling window features. Default of NULL automatically 
#'   chooses these values based on date type.
#' @param recipes_to_run List of recipes to run on multivariate models that can run different recipes. A value of NULL runs 
#'   all recipes, but only runs the R1 recipe for weekly and daily date types. A value of "all" runs all recipes, regardless 
#'   of date type. A list like c("R1") or c("R2") would only run models with the R1 or R2 recipe.  
#' 
#' @return A dataframe with nested list values for feature engineering applied by recipe
#' 
#' @keywords internal
#' @export
#' @examples
#' feature_engineering <- prep_data( 
#'   input_data = timetk::m4_monthly %>% dplyr::rename(Date = date) %>% dplyr::mutate(id = as.character(id)), 
#'   combo_variables = c("id"), 
#'   target_variable = "value", 
#'   date_type = "month", 
#'   forecast_horizon = 3)
#' 
#' recipe_1 <- feature_engineering %>% 
#'   dplyr::select(-Combo) %>% 
#'   tidyr::unnest(R1)
#'   
#' recipe_2 <- feature_engineering %>%
#'   dplyr::select(-Combo) %>%
#'   tidyr::unnest(R2)
#' 
prep_data <- function(
  input_data,
  combo_variables,
  target_variable,
  date_type,
  forecast_horizon,
  external_regressors = NULL,
  hist_start_date = NULL,
  hist_end_date = NULL,
  combo_cleanup_date = NULL,
  fiscal_year_start = 1,
  clean_missing_values = TRUE,
  clean_outliers = FALSE,
  forecast_approach = "bottoms_up",
  parallel_processing = NULL,
  num_cores = NULL, 
  target_log_transformation = FALSE, 
  fourier_periods = NULL,
  lag_periods = NULL,
  rolling_window_periods = NULL,
  recipes_to_run = NULL) {

  # get hist data start and end date
  if(is.null(hist_end_date)) {
    hist_end_date <- max(input_data$Date)
  }
  
  if(is.null(hist_start_date)) {
    hist_start_date <- min(input_data$Date)
  }

  initial_prep_tbl <- input_data %>%
    tibble::tibble() %>%
    tidyr::unite("Combo",
                 combo_variables,
                 sep="--",
                 remove=F) %>%
    dplyr::rename("Target" = target_variable) %>%
    dplyr::select(c("Combo", 
                    tidyselect::all_of(combo_variables), 
                    tidyselect::all_of(external_regressors), 
                    "Date", "Target")) %>%
    dplyr::arrange(Combo, Date) %>%
    combo_cleanup_fn(combo_cleanup_date) %>%
    get_hts(combo_variables,
            forecast_approach,
            frequency_number)
  
  # finalize function to call
  prep_time_series_fn <- construct_prep_time_series(initial_prep_tbl,
                                                    combo_variables,
                                                    target_variable,
                                                    date_type,
                                                    forecast_horizon,
                                                    external_regressors,
                                                    hist_start_date,
                                                    hist_end_date,
                                                    combo_cleanup_date,
                                                    fiscal_year_start,
                                                    clean_missing_values,
                                                    clean_outliers,
                                                    forecast_approach,
                                                    parallel_processing,
                                                    num_cores, 
                                                    target_log_transformation, 
                                                    fourier_periods,
                                                    lag_periods,
                                                    rolling_window_periods,
                                                    recipes_to_run, 
                                                    get_xregs_future_values_tbl, 
                                                    clean_outliers_missing_values, 
                                                    get_frequency_number, 
                                                    multivariate_prep_recipe_1, 
                                                    multivariate_prep_recipe_2, 
                                                    get_fourier_periods, 
                                                    get_lag_periods, 
                                                    get_rolling_window_periods, 
                                                    get_date_regex)

  # submit data to create features
  assign("get_xregs_future_values_tbl", get_xregs_future_values_tbl)
  assign("clean_outliers_missing_values", clean_outliers_missing_values)
  assign("get_frequency_number", get_frequency_number)
  assign("multivariate_prep_recipe_1", multivariate_prep_recipe_1)
  assign("multivariate_prep_recipe_2", multivariate_prep_recipe_2)
  assign("get_fourier_periods", get_fourier_periods)
  assign("get_lag_periods", get_lag_periods)
  assign("get_rolling_window_periods", get_rolling_window_periods)
  assign("get_date_regex", get_date_regex)
  
  final_data <- submit_fn(initial_prep_tbl,
                          parallel_processing,
                          unique(initial_prep_tbl$Combo),
                          prep_time_series_fn,
                          num_cores,
                          package_exports = c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach", 
                                              'doParallel', 'parallel', "lubridate"),
                          function_exports = c("get_log_transformation", "combo_cleanup_fn",
                                               "get_hts", "clean_outliers_missing_values", "get_xregs_future_values_tbl",
                                               "get_frequency_number", "get_fourier_periods", "get_lag_periods",
                                               "get_rolling_window_periods", "get_recipes_to_run",
                                               "multivariate_prep_recipe_1", "multivariate_prep_recipe_2"))
  
  return(final_data)
}

# test <- prep_data(
#   input_data = timetk::m4_monthly %>% dplyr::rename(Date = date) %>% dplyr::mutate(id = as.character(id)), 
#   combo_variables = c("id"), 
#   target_variable = "value", 
#   date_type = "month", 
#   forecast_horizon = 3, 
#   hist_start_date = as.Date("2010-01-01")
# )
# 
# test %>% dplyr::select(-Combo) %>% tidyr::unnest(R2)

# Things to Do ----
# [x] add date features from timetk into function and away from model workflows with their own recipes
# [x] run specific recipes based on inputs
# [ ] all for custom recipes to be provided 
# [ ] unit tests to check data validation, maybe make a separate file that has reproducible data validation functions
# [x] standard submission functions to run no parallel processing, on local machine, in spark, or azure batch.
#       could benefit from a standard function that lives in another file that takes in processing type, 
#       what function to call, and what list of iterators to run through it. 
# [ ] Fix function export to parallel cluster
