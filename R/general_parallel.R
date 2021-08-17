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
  
  cl <- parallel::makeCluster(detectCores())
  doParallel::registerDoParallel(cl)
  
  fcst <- foreach(i = combo_list, 
                  .combine = 'rbind',
                  .packages = get_export_packages(), 
                  .export = get_transfer_functions()) %dopar% {call_back_fn(i)}
  
  parallel::stopCluster(cl)
  
  return(fcst)
}