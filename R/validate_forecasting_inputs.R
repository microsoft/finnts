#' Validate Forecasting Inputs
#' 
#' Validates if all the forecasting inputs are as expected
#' 
#' Function will stop execution and send an error otherwise
#' 
#' @param input_data Input Dataframe or Tibble
#' @param combo_variables Column headers forming a tuple
#' @param target_variable Column header to forecast
#' @param external_regressors Column header used as regressors
#' @param forecast_horizon Number of periods to forecast
#' @param date_type Type of the time variable
#' @param hist_start_date Input data start, default is earliest
#' @param hist_end_date Input data end, default is latest
#' @param combo_cleanup_date Date after which to trim zero series
#' @param fiscal_year_start Month number of start of fiscal year
#' @param clean_missing_values Cleaning missing values
#' @param clean_outliers  Cleaning outliers in data
#' @param back_test_scenarios NULL or 1,2,3, etc.
#' @param back_test_spacing NULL or 1,2,3, etc.
#' @param modeling_approach Currently only accuracy is supported
#' @param forecast_approach bottoms_up, grouped_hierarchy, standard_hierarchy
#' @param parallel_processing azure_batch, local_machine, NULL
#' @param num_cores number of cores for parallel processing
#' @param run_model_parallel run hyperparameter search and model in parallel
#' @param max_model_average Maximum number of models to average
#' 
#' @return Returns hist_start_date and hist_end_date
#' @noRd
validate_forecasting_inputs<-function(input_data,
                                      combo_variables,
                                      target_variable,
                                      external_regressors,
                                      forecast_horizon,
                                      date_type,
                                      hist_start_date,
                                      hist_end_date,
                                      combo_cleanup_date,
                                      fiscal_year_start,
                                      clean_missing_values,
                                      clean_outliers,
                                      back_test_scenarios,
                                      back_test_spacing,
                                      modeling_approach,
                                      forecast_approach,
                                      parallel_processing,
                                      run_model_parallel,
                                      num_cores,
                                      max_model_average){
  
  retlist = list("hist_start_date" = hist_start_date,
                 "hist_end_date" = hist_end_date)
  
  #input data is a data frame or tibble
  if(!is.data.frame(input_data) | !tibble::is_tibble(input_data)) {
    stop("input data is not a data frame or tibble")
  }
  
  #data combo names match the input data
  if(sum(combo_variables %in% colnames(input_data)) != length(combo_variables)) {
    stop("combo variables do not match column headers in input data")
  }
  
  #target variable name matches the input data
  if(!(target_variable %in% colnames(input_data))) {
    stop("target variable does not match a column header in input data")
  }
  
  #external regressors match the input data
  if(!is.null(external_regressors) & sum(external_regressors %in% colnames(input_data)) != length(external_regressors)) {
    stop("external regressors do not match column headers in input data")
  }
  
  #forecast horizon is a number
  if(!is.numeric(forecast_horizon)){
    stop("forecast horizon should be a number")
  } else if(forecast_horizon > (length(unique(input_data$Date))*0.25)) {
    warning("consider getting more historical data based on forecast horizon length")
  }
  
  #date type is a valid value
  if(!(date_type %in% c("day", "week", "month", "quarter", 'year'))) {
    stop("invalid date type input value")
  }
  
  #date column is labeled as "Date"
  if(!("Date" %in% colnames(input_data))) {
    stop("date column in input data needs to be named as 'Date'")
  }
  
  #date column is formatted as a date
  if(!lubridate::is.Date(input_data$Date)) {
    stop("date column in input data needs to be formatted as a date value")
  }
  
  #ensure month, quarter, year data repeats on the same day of each period
  if((date_type != "day" & date_type != "week") & length(unique(format(input_data$Date, format = "%d"))) != 1) {
    stop("historical date values are not evenly spaced")
  }
  
  #historical data start date formatting
  if(is.null(hist_start_date)) {
    retlist["hist_start_date"] = min(input_data$Date)
  }else if(!lubridate::is.Date(hist_start_date)) {
    stop("historical data start date input value is not a valid date")
  }
  
  #historical data end date formatting
  if(is.null(hist_end_date)) {
    retlist["hist_end_date"] = max(input_data$Date)
  }else if(!lubridate::is.Date(hist_end_date)) {
    stop("historical data end date input value not a valid date")
  }
  
  #combo cleanup date formatting
  if(!is.null(combo_cleanup_date) & !lubridate::is.Date(combo_cleanup_date)) {
    stop("combo cleanup date input value is not a valid date")
  }
  
  #fiscal year start formatting
  if(!is.numeric(fiscal_year_start) | fiscal_year_start < 1 | fiscal_year_start > 12) {
    stop("fiscal year start should be a number from 1 to 12")
  }
  
  #clean missing values formatting
  if(clean_missing_values != TRUE & clean_missing_values != FALSE) {
    stop("clean missing value input must be either TRUE or FALSE")
  }
  
  #clean outlier values formatting
  if(clean_outliers != TRUE & clean_outliers != FALSE) {
    stop("clean outliers input must be either TRUE or FALSE")
  }
  
  #back test scenarios formatting
  if(!is.numeric(back_test_scenarios) & !is.null(back_test_scenarios)) {
    stop("back test scenarios input value must be either a number greater than 0 or set to NULL")
  } else if(is.null(back_test_scenarios)) {
    # do nothing
  } else if(back_test_scenarios < 1) {
    stop("back test scenarios input value must be either a number greater than 0 or set to NULL")
  }
  
  #back test spacing
  if((!is.numeric(back_test_spacing) & !is.null(back_test_spacing)) | sum(back_test_spacing < 1) == 1) {
    stop("back test spacing input value must be either a number greater than 0 or set to NULL")
  }
  
  #modeling approach formatting
  if(modeling_approach != "accuracy") {
    stop("modeling approach input value needs to be set to 'accruacy'")
  }
  
  #forecast approach formatting
  if(!(forecast_approach %in% c("bottoms_up",
                                "standard_hierarchy",
                                "grouped_hierarchy"))) {
    stop("forecast approach input must be one of these values: 'bottoms_up', 'standard_hierarchy', 'grouped_hierarchy'")
  }
  
  if(forecast_approach != "bottoms_up") {
    
    combo_amount <- c()
    
    for(column in combo_variables) {
      
      temp1 <- input_data %>%
        dplyr::select(column)
      
      temp2 <- length(unique(temp1[[1]]))
      
      
      combo_amount <- append(combo_amount, temp2)
    }
    
    if(forecast_approach == "grouped_hierarchy" & sum(combo_amount %in% c("1")) > 0) {
      stop("there needs to be more than one unique value per data combo column for a grouped hierarchy forecast")
    } else if(forecast_approach == "standard_hierarchy" & length(combo_amount) != length(unique(combo_amount))) {
      stop("each data combo column needs to contain a different amount of unqiue values compared to other data combo columns")
    }
    
  }
  
  # parallel processing formatting
  if(is.null(parallel_processing)) {
    
    # no further checks needed
  
  } else if(parallel_processing %in% c("local_machine", "azure_batch", "spark") == FALSE) {
    
    stop("parallel processing input must be one of these values: NULL, 'local_machine', 'azure_batch', 'spark'")
    
  } else if((parallel_processing == "local_machine" & run_model_parallel) | (parallel_processing == "spark" & run_model_parallel)) {
    
    stop("cannot run parallel process (run model parallel input) within another parallel process (parallel processing input). Please set run_model_parallel to FALSE")
    
  } else if(parallel_processing == "azure_batch") {
    
    message("NOTE: Ensure that Azure Batch parallel back-end has been registered before calling 'forecast_time_series' function")
    warning("The azure batch parallel compute method is now deprecated, please use the new spark option in Azure", 
            call. = FALSE)
    
  } else {
    
    # no further checks needed
    
  }
  
  if(run_model_parallel) {
    warning("run_model_parallel argument in forecast_time_series is deprecated as Finn transitions to other parallel compute options like spark", 
            call. = FALSE)
  }
  
  #number of cores formatting
  if(!is.numeric(num_cores) & !is.null(num_cores)) {
    stop("num_cores should be NULL or a numeric value")
  }
  
  
  #max model average formatting
  if(!is.numeric(max_model_average)) {
    stop("max model average input needs to be a number")
  } else if(max_model_average < 2) {
    stop("max model average input needs to be a number greater than 2")
  }
  
  return (retlist)
}