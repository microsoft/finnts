#' Get number of cores to use when registering parallel back end
#' 
#' @param num_cores number of cores for parallel processing
#' 
#' @noRd 
get_cores <-function(num_cores){
  
  if(is.null(num_cores)) {
    parallel::detectCores()-1
  } else {
    min(num_cores, parallel::detectCores()-1)
  }
}


#' Function to submit various functions in spark, azure batch, or parallel on local machine
#' 
#' @param parallel_processing type of parallel processing to run
#' @param iterator list of values to run through
#' @param fn function to run
#' @param num_cores number of cores to use on local machine
#' @param package_exports packages to export
#' @param function_exports functions to export
#' 
#' @return tbl with function output
#' @noRd
submit_fn <- function(input_data,
                      parallel_processing, 
                      iterator, 
                      fn, 
                      num_cores = NULL, 
                      package_exports = NULL, 
                      function_exports = NULL){
  
  if(is.null(parallel_processing)) {
    
    final_data <- lapply(iterator, fn)
    final_data <- do.call(rbind, final_data)
    
    # final_data <- foreach::foreach(i = iterator, 
    #                                .combine = 'rbind',
    #                                .errorhandling = "stop"
    # ) %dopar% {fn(i)}
    
    return(final_data)
    
  } else if(parallel_processing == "local_machine") {
    
    cli::cli_h2("Creating Parallel Processing")
    
    cores <- get_cores(num_cores)
    
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    # parallel::clusterExport(cl=cl, 
    #                         function_exports,
    #                         envir=environment())

    cli::cli_alert_info("Running across {cores} cores")
    
    final_data <- foreach::foreach(i = iterator, 
                                   .combine = 'rbind',
                                   .packages = package_exports,
                                   .export = function_exports, 
                                   .errorhandling = "stop", 
                                   .verbose = FALSE
    ) %dopar% {fn(i)}

    parallel::stopCluster(cl)
    
    return(final_data)
    
  } else if(parallel_processing == "spark") {
    
    cli::cli_h2("Submitting Tasks to Spark")
    
    sparklyr::registerDoSpark(sc, parallelism = length(iterator))
    final_data <- foreach(i = iterator, 
                          .combine = 'rbind', 
                          .errorhandling = "stop") %dopar% {fn(i)}
    
    return(final_data)
    
  } else {
    stop("error during function submission")
  }
}