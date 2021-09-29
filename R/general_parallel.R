#' Call back function within forecast for init
#' 
#' Sets the right configuration during init
#' 
#' @param type Type of parallel processing being done
#' @param num_cores number of cores for parallel processing
#' @noRd 
init_parallel_within <-function(type, num_cores){
  
  cli::cli_h3("Creating Parallel Processing")
  
  if(is.null(num_cores)) {
    cores <- parallel::detectCores()-1
  } else {
    cores <- min(num_cores, parallel::detectCores()-1)
  }
  
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  
  #point to the correct libraries within Azure Batch
  if(type == "azure_batch") {
    parallel::clusterEvalQ(cl, .libPaths("/mnt/batch/tasks/shared/R/packages")) 
  }
  
  cli::cli_alert_info("Running across {cores} cores")
  
  return(cl)
}

#' Call back function within forecast for exit
#' 
#' @param cl Cluster variable from init
#' 
#' Sets the right configuration during exit
#' @noRd
exit_parallel_within <-function(cl){
  parallel::stopCluster(cl)
}



#' Call parallel forecast model
#' 
#' Local machine parallelization of forecast model
#' 
#' @param combo_list Combo List
#' @param call_back_fn Call Back Function
#' @param num_cores number of cores for parallel processing
#' 
#' @return Forecast Object
#' @noRd
get_fcast_parallel<- function(combo_list,
                              call_back_fn, 
                              num_cores){
  
  cli::cli_h2("Creating Parallel Processing")
  
  if(is.null(num_cores)) {
    cores <- parallel::detectCores()-1
  } else {
    cores <- min(num_cores, parallel::detectCores()-1)
  }
  
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  
  cli::cli_alert_info("Running across {cores} cores")
  
  fcst <- foreach(i = combo_list, 
                  .combine = 'rbind',
                  .packages = get_export_packages(),
                  .export = get_transfer_functions()
                  ) %dopar% {call_back_fn(i)}
  
  parallel::stopCluster(cl)
  
  return(fcst)
}