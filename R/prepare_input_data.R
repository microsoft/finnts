#' Gets the tibble version of data
#' 
#' Converts input data into tibble
#' 
#' 
#' @param input_data Original input data to function
#' @param combo_variables List of Combo variables
#' @param target_variable Variable to forecast
#' 
#' @return Returns Standardized data_tbl (a data table a.k.a. tibble)
#' @noRd
get_data_tbl<- function(input_data,
                        combo_variables,
                        target_variable){
  input_data %>%
    tibble::tibble() %>%
    tidyr::unite("Combo",
          combo_variables,
          sep="--",
          remove=F) %>%
    dplyr::rename("Target" =target_variable)
}

#' Gets the list of external regressors with future values
#' 
#' @param data_tbl standardized data table
#' @param external_regressors all external regressors
#' @param hist_end_date historical end date of data
#' 
#' @return list of external regressors with future values
#' @noRd
get_xreg_future_values_list <- function(data_tbl,
                                        external_regressors,
                                        hist_end_date){
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
  
  return (xregs_future_values_list)
}

#' Gets the modelling ready table
#' 
#' @param data_tbl standardized data table
#' @param external_regressors all external regressors
#' @param hist_end_date historical end date of data
#' @param combo_cleanup_date date to remove combos without value
#' @param combo_variables list of combo variables
#' 
#' @return list of external regressors with future values
#' @noRd
get_modelling_ready_tbl<-function(data_tbl,
                                  external_regressors,
                                  hist_end_date,
                                  combo_cleanup_date,
                                  combo_variables){
  
  combo_cleanup_func <- function(df,combo_cleanup_date){
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
  
  data_tbl %>%
    dplyr::select(c("Combo", 
                    tidyselect::all_of(combo_variables), 
                    tidyselect::all_of(external_regressors), 
                    "Date", "Target")) %>%
    dplyr::filter(Date <= hist_end_date) %>%
    dplyr::arrange(Combo, Date) %>%
    combo_cleanup_func(combo_cleanup_date)
}
