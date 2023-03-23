#' Call parallel forecast model for azure batch
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
#' @noRd
get_fcast_parallel_azure_batch <- function(combo_list,
                                           call_back_fn,
                                           run_name, 
                                           models_to_run,
                                           models_not_to_run,
                                           recipes_to_run, 
                                           pca){
  
  cli::cli_h2("Submitting Tasks to Azure Batch")
  
  # add specific forecast_time_series function arguments to local env before submitting to Azure
  # Helps prevent errors when function arguments are referenced in another variable
  models_to_run <- models_to_run
  models_not_to_run <- models_not_to_run
  recipes_to_run <- recipes_to_run
  pca <- pca
  
  fcst <- foreach::foreach(i = combo_list, .combine = 'rbind',
                  .packages = get_export_packages(), 
                  .export = get_transfer_functions(),
                  .options.azure = list(maxTaskRetryCount = 0, 
                                        autoDeleteJob = TRUE,
                                        timeout = 60 * 60 * 24 * 7, # timeout after a week
                                        job = substr(paste0('finn-fcst-', 
                                                            strftime(Sys.time(),format="%H%M%S"), '-', 
                                                            tolower(gsub(" ", "-", trimws(gsub("\\s+", " ", gsub("[[:punct:]]", '', run_name)))))), 1, 63)),
                  .errorhandling = "remove") %dopar% {call_back_fn(i)}
}

#' Call parallel forecast model for azure with spark
#' 
#' Azure Spark parallelization of forecast model
#' 
#' @param combo_list Combo List
#' @param call_back_fn Call Back Function
#' @param azure_batch_credentials Azure Batch Credentials
#' @param azure_batch_clusterConfig Azure Batch Cluster Config
#' @param run_name Run Name
#' 
#' @return Forecast Object
#' @noRd
get_fcast_parallel_azure_spark <- function(combo_list,
                                           call_back_fn,
                                           run_name, 
                                           models_to_run,
                                           models_not_to_run,
                                           recipes_to_run, 
                                           pca){
  
  cli::cli_h2("Submitting Tasks to Spark Cluster")
  
  sparklyr::registerDoSpark(sc, parallelism = length(combo_list))
  
  # add specific forecast_time_series function arguments to local env before submitting to Azure
  # Helps prevent errors when function arguments are referenced in another variable
  models_to_run <- models_to_run
  models_not_to_run <- models_not_to_run
  recipes_to_run <- recipes_to_run
  pca <- pca
  
  fcst <- foreach::foreach(i = combo_list,
                           .combine = 'rbind',
                           .errorhandling = "remove") %dopar% {call_back_fn(i)}
}