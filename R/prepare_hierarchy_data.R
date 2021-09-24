#' Function to create the data_tbl_final
#' 
#' @param data_tbl standardized data table
#' @param combo_variables list of combo variables
#' @param forecast_approach forecasting approach
#' @param frequency_number frequency number 
#' 
#' @return data_tbl_final
#' @noRd
get_data_tbl_final <- function(data_tbl,
                          combo_variables,
                          forecast_approach,
                          frequency_number){
  
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
  
  # main data table function to produce our table
  data_tbl_func <- function(df){
    
    if(forecast_approach == 'bottoms_up'){
      df
    }
    else{
      
      some_list <- df %>%
        dplyr::mutate(Target = tidyr::replace_na(Target, 0)) %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(combo_variables))) %>%
        dplyr::summarise(Sum=sum(Target, na.rm=TRUE)) %>%
        data.frame() %>% pick_right_list()
        
      data_cast <- df %>%
        dplyr::arrange(Combo, Date) %>%
        dplyr::select(-combo_variables) %>%
        tidyr::pivot_wider(names_from = Combo, 
                           values_from = Target)
      
      Date = data_cast$Date
      
      data_cast %>%
        dplyr::select(-Date) %>%
        ts(frequency = frequency_number)%>% 
        get_hts(some_list)  %>%
        hts::allts() %>%
        data.frame() %>%
        tibble::add_column(Date = Date,
                   .before = 1)%>%
        tidyr::pivot_longer(!Date, 
                          names_to = "Combo", 
                          values_to = "Target") %>%
        tibble::tibble()
    }
    
  }
  
  data_tbl %>% data_tbl_func
  
}

#' Function to perform log transformation
#' 
#' @param df data frame
#' @param target_log_transformation variable to indicate log transformation
#' 
#' @return full_data_tbl with or without log transformation
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

#' Function to perform outlier cleaning and polynomial transformation
#' 
#' @param df data frame
#' @param clean_outliers clean_outliers
#' @param clean_missing_values clean missing values
#' @param frequency_number frequency number
#' @param external_regressors external regressors
#' 
#' @return cleaned_data 
#' @noRd
get_poly_trans_clean <- function(df,
                                 clean_outliers,
                                 clean_missing_values,
                                 frequency_number,
                                 external_regressors){
  
  
  correct_clean_func <- function(col){
    if(clean_outliers){
      timetk::ts_clean_vec(col,period = frequency_number)
    }else if(clean_missing_values){
      timetk::ts_impute_vec(col,period = frequency_number)
    }
    col
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


#' Function to create the external_regressors
#' 
#' @param external_regressors existing external_regressors
#' @param forecast_approach forecasting approach 
#' 
#' @return external_regressors
#' @noRd
get_external_regressors <- function(external_regressors,
                               forecast_approach){
  if(forecast_approach != 'bottoms_up'){
    return(NULL)
  }
  
  return(external_regressors)
}


#' Function to create the xregs_future_values_tbl
#' 
#' @param xregs_future_values_tbl existing external_regressors
#' @param forecast_approach forecasting approach 
#' 
#' @return xregs_future_values_tbl
#' @noRd
get_xregs_future_values_tbl <- function(xregs_future_values_tbl,
                                    forecast_approach){
  if(forecast_approach != 'bottoms_up'){
    xregs_future_values_tbl %>%
      tibble::tibble() %>%
      dplyr::select(Combo, Date)
  }
  
  return(xregs_future_values_tbl)
}

#' Final function to create the full_data_tbl
#' 
#' @param data_tbl data table from previous pipe
#' @param combo_cleanup_date Date to clean up combo after
#' @param combo_variables List of combo variables
#' @param clean_outliers Is clean outliers out of data
#' @param clean_missing_values Is clean missing values
#' @param date_type date type from previous stage
#' @param external_regressors existing external_regressors
#' @param forecast_approach forecasting approach 
#' @param frequency_number frequency number
#' @param forecast_horizon forecast horizon
#' @param hist_start_date historical start date
#' @param hist_end_date historical end date
#' @param pad_value Value for padding
#' @param target_log_transformation Log transformation
#' @param xregs_future_values_tbl external regressor table
#' 
#' @return full_data_tbl
#' @noRd
get_full_data_tbl <- function(data_tbl,
                              combo_cleanup_date,
                              combo_variables,
                              clean_outliers,
                              clean_missing_values,
                              date_type,
                              external_regressors,
                              forecast_approach,
                              frequency_number,
                              forecast_horizon,
                              hist_start_date,
                              hist_end_date,
                              pad_value,
                              target_log_transformation,
                              xregs_future_values_tbl){
  data_tbl %>% 
    get_modelling_ready_tbl(external_regressors,
                            hist_end_date,
                            combo_cleanup_date,
                            combo_variables) %>%  
    get_data_tbl_final(combo_variables,
                       forecast_approach,
                       frequency_number) %>%
    dplyr::select(Combo, 
                  Date, 
                  Target, 
                  external_regressors) %>%
    dplyr::group_by(Combo) %>%
    timetk::pad_by_time(Date, 
                        .by = date_type, 
                        .pad_value = pad_value, 
                        .end_date = hist_end_date) %>% #fill in missing values in between existing data points
    timetk::pad_by_time(Date, 
                        .by = date_type, 
                        .pad_value = 0, 
                        .start_date = hist_start_date, 
                        .end_date = hist_end_date) %>% #fill in missing values at beginning of time series with zero
    dplyr::ungroup()%>%
    get_log_transformation(target_log_transformation) %>%
    dplyr::group_by(Combo) %>%
    timetk::future_frame(Date, 
                         .length_out = forecast_horizon, 
                         .bind_data = TRUE) %>% #add future data
    dplyr::left_join(xregs_future_values_tbl) %>% #join xregs that contain values given by user
    dplyr::ungroup() %>%
    dplyr::group_by(Combo) %>%
    dplyr::group_split() %>%
    purrr::map(.f = function(df) { get_poly_trans_clean(df,
                                                        clean_outliers,
                                                        clean_missing_values,
                                                        frequency_number,
                                                        external_regressors) }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), 
      list(~ifelse(is.infinite(.),NA, .)))) %>%
    dplyr::mutate(dplyr::across(where(is.numeric),
      list(~ifelse(is.nan(.),NA, .)))) %>%
    dplyr::mutate(dplyr::across(where(is.numeric),
      list(~ifelse(is.na(.),0,.)))) %>%
    dplyr::mutate(Target = ifelse(Date > hist_end_date,
                                  NA,
                                  Target))
}