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

#' Call parallel forecast model for azure
#' 
#' Azure Batch parallelization of forecast model
#' 
#' @param combo_list Combo List
#' @param call_back_fn Call Back Function
#' @param azure_batch_credentials Azure Batch Credentials
#' @param azure_batch_clusterConfig Azure Batch Cluster Config
#' @param run_name Run Name
#' 
#' @return Forecast Object
get_fcast_parallel_azure <- function(combo_list,
                                     call_back_fn,
                                     azure_batch_credentials,
                                     azure_batch_clusterConfig,
                                     run_name){
  
  doAzureParallel::setCredentials(azure_batch_credentials)
  cluster <- doAzureParallel::makeCluster(azure_batch_clusterConfig)
  doAzureParallel::registerDoAzureParallel(cluster)
  
  
  fcst <- foreach(i = combo_list, .combine = 'rbind',
                  .packages = get_export_packages(), 
                  .export = get_transfer_functions(),
                  .options.azure = list(maxTaskRetryCount = 0, 
                                        autoDeleteJob = TRUE, 
                                        job = substr(paste0('finn-fcst-', 
                                                            strftime(Sys.time(),format="%H%M%S"), '-', 
                                                            tolower(gsub(" ", "-", trimws(gsub("\\s+", " ", gsub("[[:punct:]]", '', run_name)))))), 1, 63)),
                  .errorhandling = "remove") %dopar% {call_back_fn(i)}
}