
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

check_input_data <- function(input_data, 
                             combo_variables, 
                             target_variable, 
                             external_regressors) {
  
  # column names
  
  if(sum(cols %in% col_names) != length(cols)) {
    stop("data column names don't match input column names")
  }
  
  # column types
  
}

