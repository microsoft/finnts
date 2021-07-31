#' Call back function within forecast for init
#' 
#' Sets the right configuration during init
init_azure_parallel_within <-function(){
  cores <- parallel::detectCores()
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  
  #point to the correct libraries within Azure Batch
  clusterEvalQ(cl, .libPaths("/mnt/batch/tasks/shared/R/packages"))
  
  return(cl)
}

#' Call back function within forecast for exit
#' 
#' @param cl Cluster variable from init
#' 
#' Sets the right configuration during exit
exit_azure_parallel_within <-function(cl){
  parallel::stopCluster(cl)
}

