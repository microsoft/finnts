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
#' @noRd
get_fcast_parallel_azure <- function(combo_list,
                                     call_back_fn,
                                     run_name){
  
  cli::cli_h2("Submitting Tasks to Azure Batch")
  
  fcst <- foreach::foreach(i = combo_list, .combine = 'rbind',
                  .packages = get_export_packages(), 
                  .export = get_transfer_functions(),
                  .options.azure = list(maxTaskRetryCount = 0, 
                                        autoDeleteJob = TRUE, 
                                        job = substr(paste0('finn-fcst-', 
                                                            strftime(Sys.time(),format="%H%M%S"), '-', 
                                                            tolower(gsub(" ", "-", trimws(gsub("\\s+", " ", gsub("[[:punct:]]", '', run_name)))))), 1, 63)),
                  .errorhandling = "remove") %dopar% {call_back_fn(i)}
}