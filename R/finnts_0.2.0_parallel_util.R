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

#' Function to submit tasks sequentially, in parallel on local machine, or in spark
#' 
#' @param parallel_processing type of parallel processing to run
#' @param num_cores number of cores
#' @param task_length number of time series to submit to parallel cluster
#' 
#' @noRd
par_start <- function(parallel_processing, 
                      num_cores, 
                      task_length){
  
  cl <- NULL
  
  if(is.null(parallel_processing)) {
    
    `%op%` <- foreach::`%do%`

    packages <- c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                  'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                  'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                  'recipes', 'rules', 'modeltime', "fs", "digest", "AzureStor", "Microsoft365R", 
                  "arrow", "qs", "tidyr", "sparklyr", "vroom", "utils", "cli")
    
  } else if(parallel_processing == "spark") {
    
    if(!exists("sc")) {
      stop("Ensure that you are connected to a spark cluster using an object called 'sc'")
    }
    
    cli::cli_h2("Connecting to Spark Cluster")
    
    `%op%` <- foreach::`%dopar%`
    
    sparklyr::registerDoSpark(sc, parallelism = task_length)
    
    packages <- NULL
    
  } else if(parallel_processing == "local_machine") {
    
    cli::cli_h2("Connecting to Local Parallel Cluster")
    
    cores <- get_cores(num_cores)
    
    cl <- parallel::makeCluster(min(cores, task_length))
    doParallel::registerDoParallel(cl)
    
    cli::cli_alert_info("Running across {cores} cores")
    
    `%op%` <- foreach::`%dopar%`
    
    packages <- c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                  'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                  'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                  'recipes', 'rules', 'modeltime', "fs", "digest", "AzureStor", "Microsoft365R", 
                  "arrow", "qs", "tidyr", "sparklyr", "vroom", "utils", "cli", "generics", 
                  "gtools", "hts", "magrittr", "methods", "base", "modeltime.resample", 
                  "plyr", "rsample")
    
  } else {
    stop("error")
  }
  
  return(list(packages = packages, foreach_operator = `%op%`, cl = cl))
  
}

#' Function to clean up after submiting tasks sequentially, in parallel on local machine, or in spark
#' 
#' @param cl cluster object
#' 
#' @noRd
par_end <- function(cl){
  foreach::registerDoSEQ()
  
  if(!is.null(cl)) {
    parallel::stopCluster(cl)
  }
}

