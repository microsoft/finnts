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
  
  #parallel::clusterExport(varlist = get_transfer_functions())
  #assign("combo_specific_filter", combo_specific_filter, .GlobalEnv)
  print(ls(globalenv()))
  
  #stop()
  
  fcst <- foreach(i = combo_list, .combine = 'rbind',
                  .packages = get_export_packages(), 
                  .export = ls(globalenv()),
                  .options.azure = list(maxTaskRetryCount = 0, 
                                        autoDeleteJob = TRUE, 
                                        job = substr(paste0('finn-fcst-', 
                                                            strftime(Sys.time(),format="%H%M%S"), '-', 
                                                            tolower(gsub(" ", "-", trimws(gsub("\\s+", " ", gsub("[[:punct:]]", '', run_name)))))), 1, 63)),
                  .errorhandling = "remove") %dopar% {call_back_fn(i)}
}