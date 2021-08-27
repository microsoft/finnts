#' Call back function within forecast for init
#' 
#' Sets the right configuration during init
#' 
#' @param type Type of parallel processing being done 
init_parallel_within <-function(type){
  
  cores <- parallel::detectCores()
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  
  #point to the correct libraries within Azure Batch
  if(type == "azure_batch") {
    clusterEvalQ(cl, .libPaths("/mnt/batch/tasks/shared/R/packages"))  
  }
  
  return(cl)
}

#' Call back function within forecast for exit
#' 
#' @param cl Cluster variable from init
#' 
#' Sets the right configuration during exit
exit_parallel_within <-function(cl){
  parallel::stopCluster(cl)
}



#' Call parallel forecast model
#' 
#' Local machine parallelization of forecast model
#' 
#' @param combo_list Combo List
#' @param call_back_fn Call Back Function
#' 
#' @return Forecast Object
get_fcast_parallel<- function(combo_list,
                              call_back_fn){
  
  cl <- parallel::makeCluster(parallel::detectCores(), outfile = "doParallel.txt")
  doParallel::registerDoParallel(cl)
  
  parallel::clusterExport(cl, get_transfer_functions())
  #parallel::clusterExport(cl, "combo_specific_filter")
  
  fcst <- foreach(i = combo_list, 
                  .combine = 'rbind',
                  .packages = get_export_packages(),
                  .export = get_transfer_functions()
                  ) %dopar% {call_back_fn(i)}
  
  parallel::stopCluster(cl)
  
  return(fcst)
}