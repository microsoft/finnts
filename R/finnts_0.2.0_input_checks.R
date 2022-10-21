
#' Check input vales
#'
#' @param input_name input name
#' @param input_value input value
#' @param type type
#' @param expected_value expected value
#'
#' @return nothing
#' @noRd
check_input_type <- function(input_name, 
                             input_value, 
                             type, 
                             expected_value = NULL) {
  
  if(!inherits(input_value, type)) {
    
    stop(paste0("invalid type for input name '", input_name, "', needs to be of type ", 
                glue::glue_collapse(type, " or ")), 
         call. = FALSE)
  }
  
  if(!is.null(expected_value) & !is.null(input_value)) {
    
    if(!(input_value %in% expected_value)) {
      
      stop(paste0("invalid value for input name '", input_name, "', value needs to equal ", 
                  glue::glue_collapse(expected_value, " or ")), 
           call. = FALSE)
    }
  }
}

#' Check input data
#'
#' @param input_data input name
#' @param combo_variables combo variables
#' @param target_variable target variable
#' @param external_regressors external regressors
#' @param date_type date type
#' @param fiscal_year_start fiscal year start
#' @param max_model_average max model average
#' 
#' @return nothing
#' @noRd
check_input_data <- function(input_data, 
                             combo_variables, 
                             target_variable, 
                             external_regressors, 
                             date_type, 
                             fiscal_year_start, 
                             max_model_average) {
  
  #data combo names match the input data
  if(sum(combo_variables %in% colnames(input_data)) != length(combo_variables)) {
    stop("combo variables do not match column headers in input data")
  }
  
  #target variable name matches the input data
  if(!(target_variable %in% colnames(input_data))) {
    stop("target variable does not match a column header in input data")
  }
  
  #target variable is numeric
  if(!input_data %>% dplyr::rename(Target = target_variable) %>% dplyr::pull(Target) %>% is.numeric()) {
    stop("Target variable in input data needs to be numeric")
  }
  
  #external regressors match the input data
  if(!is.null(external_regressors) & sum(external_regressors %in% colnames(input_data)) != length(external_regressors)) {
    stop("external regressors do not match column headers in input data")
  }
  
  #date column is labeled as "Date"
  if(!("Date" %in% colnames(input_data))) {
    stop("date column in input data needs to be named as 'Date'")
  }
  
  #ensure month, quarter, year data repeats on the same day of each period
  if((date_type != "day" & date_type != "week") & length(unique(format(input_data$Date, format = "%d"))) != 1) {
    stop("historical date values are not evenly spaced")
  }
  
  # fiscal year start formatting
  if(!is.numeric(fiscal_year_start) | fiscal_year_start < 1 | fiscal_year_start > 12) {
    stop("fiscal year start should be a number from 1 to 12")
  }
  
  # duplicate rows
  dup_col_check <- c(combo_variables, "Date")
  print(dup_col_check)
  duplicate_tbl <- input_data %>% 
    dplyr::group_by(dplyr::across(dup_col_check)) %>%
    #dplyr::group_by(dplyr::across(tidyselect::all_of(c(combo_variables, "Date")))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(Date) %>%
    dplyr::collect()
  
  if(nrow(duplicate_tbl) > 1) {
    stop("duplicate rows have been detected in the input data", 
         call. = FALSE)
  }
}

#' Check parallel processing set up
#'
#' @param parallel_processing parallel processing
#' @param run_model_parallel run model parallel
#' 
#' @return nothing
#' @noRd
check_parallel_processing <- function(parallel_processing, 
                                      run_model_parallel = FALSE) {
  
  # parallel processing formatting
  if(is.null(parallel_processing)) {
    
    # no further checks needed
    
  } else if(parallel_processing %in% c("local_machine", "azure_batch", "spark") == FALSE) {
    
    stop("parallel processing input must be one of these values: NULL, 'local_machine', 'azure_batch', 'spark'")
    
  } else if(parallel_processing == "local_machine" & run_model_parallel) {
    
    stop("cannot run parallel process (run model parallel input) within another parallel process (parallel processing input). Please set run_model_parallel to FALSE")
    
  } else if(parallel_processing == "azure_batch") {
    
    message("NOTE: Ensure that Azure Batch parallel back-end has been registered before calling 'forecast_time_series' function")
    warning("The azure batch parallel compute method is now deprecated, please use the new spark option in Azure", 
            call. = FALSE)
    
  } else if(parallel_processing == "spark") {
    
    if(!exists("sc")) {
      stop("Ensure that you are connected to a spark cluster using an object called 'sc'", 
           call. = FALSE)
    }
    
  } else {
    
    # no further checks needed
    
  }
  
}
