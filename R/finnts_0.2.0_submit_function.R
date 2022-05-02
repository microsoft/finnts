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
                      function_exports = NULL, 
                      error_handling = "stop", 
                      batch_size = 10000){
  
  num_rounds <- ceiling(length(iterator)/batch_size)
  final_data <- NULL
  
  cli::cli_alert_info("Running {length(iterator)} total tasks in {num_rounds} separate batches of {min(batch_size, length(iterator))} tasks")
  
  if(is.null(parallel_processing)) {

    for(round in 1:num_rounds) {

      iterator_max <- min(round * batch_size, length(iterator))
      iterator_min <- ((round-1) * batch_size) + 1

      iterator_round <- iterator[iterator_min:iterator_max]
      
      temp <- lapply(iterator_round, fn)
      temp <- do.call(rbind, temp)
      
      final_data <- rbind(final_data, temp)
      
      cli::cli_alert_success("Finished batch {round} out of {num_rounds} total")
    }

    return(final_data)
    
  } else if(parallel_processing == "local_machine") {
    
    cli::cli_h2("Creating Parallel Processing")
    
    cores <- get_cores(num_cores)
    
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)

    cli::cli_alert_info("Running across {cores} cores")
    
    for(round in 1:num_rounds) {
      
      iterator_max <- min(round * batch_size, length(iterator))
      iterator_min <- ((round-1) * batch_size) + 1
      
      iterator_round <- iterator[iterator_min:iterator_max]
      
      temp <- foreach::foreach(i = iterator_round, 
                                   .combine = 'rbind',
                                   .packages = package_exports,
                                   .export = function_exports, 
                                   .errorhandling = error_handling, 
                                   .verbose = FALSE
      ) %dopar% {fn(i)}
      
      final_data <- rbind(final_data, temp)
      
      cli::cli_alert_success("Finished batch {round} out of {num_rounds} total")
    }

    parallel::stopCluster(cl)
    
    return(final_data)
    
  } else if(parallel_processing == "spark") {
    
    cli::cli_h2("Submitting Tasks to Spark")
    
    for(round in 1:num_rounds) {
      
      iterator_max <- min(round * batch_size, length(iterator))
      iterator_min <- ((round-1) * batch_size) + 1
      
      iterator_round <- iterator[iterator_min:iterator_max]
      
      sparklyr::registerDoSpark(sc, parallelism = length(iterator_round))
      
      temp <- foreach(i = iterator_round, 
                      .combine = 'rbind', 
                      .errorhandling = error_handling) %dopar% {fn(i)}
      
      final_data <- rbind(final_data, temp)
      
      cli::cli_alert_success("Finished batch {round} out of {num_rounds} total")
    }

    return(final_data)
    
  } else {
    stop("error during function submission")
  }
}