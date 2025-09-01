#' Get number of cores to use when registering parallel back end
#'
#' @param num_cores number of cores for parallel processing
#'
#' @noRd
get_cores <- function(num_cores) {
  if (is.null(num_cores)) {
    parallel::detectCores() - 1
  } else {
    min(num_cores, parallel::detectCores() - 1)
  }
}

#' Function to submit tasks sequentially, in parallel on local machine, or in spark
#'
#' @param run_info run info
#' @param parallel_processing type of parallel processing to run
#' @param num_cores number of cores
#' @param task_length number of time series to submit to parallel cluster
#'
#' @noRd
par_start <- function(run_info,
                      parallel_processing,
                      num_cores,
                      task_length) {
  
  # always drop any previous foreach backend
  try(foreach::registerDoSEQ(), silent = TRUE)
  try(doParallel::stopImplicitCluster(), silent = TRUE)  # harmless if none
  cl <- NULL

  # packages to export to cluster
  base_packages <- c(
    "tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
    "doParallel", "parallel", "lubridate", "parsnip", "tune", "dials", "workflows",
    "Cubist", "earth", "glmnet", "kernlab", "purrr",
    "recipes", "rules", "modeltime", "fs", "digest", "tidyr",
    "vroom", "utils", "cli"
  )

  parallel_packages <- c(
    "gtools", "hts", "magrittr", "methods", "base",
    "plyr", "rsample"
  )

  add_packages <- NULL

  if (inherits(run_info$storage_object, "blob_container")) {
    add_packages <- c(add_packages, "AzureStor")
  } else if (inherits(run_info$storage_object, "ms_drive")) {
    add_packages <- c(add_packages, "Microsoft365R")
  }

  if (run_info$data_output == "parquet") {
    add_packages <- c(add_packages, "arrow")
  }

  if (run_info$object_output == "qs") {
    add_packages <- c(add_packages, "qs")
  }

  # register cluster
  if (is.null(parallel_processing)) {
    `%op%` <- foreach::`%do%`

    packages <- c(base_packages, add_packages)
  } else if (parallel_processing == "spark") {
    if (!exists("sc")) {
      stop("Ensure that you are connected to a spark cluster using an object called 'sc'")
    }
    
    reset_spark()

    `%op%` <- foreach::`%dopar%`

    sparklyr::registerDoSpark(sc, parallelism = task_length)
    
    if (!identical(foreach::getDoParName(), "doSpark")) {
      stop("Failed to register doSpark backend (got: ", foreach::getDoParName(), ")")
    }

    packages <- NULL
  } else if (parallel_processing == "local_machine") {
    cores <- get_cores(num_cores)

    cl <- parallel::makeCluster(min(cores, task_length))

    if (substr(run_info$path, start = 1, stop = 6) == "/synfs") {
      # make sure libraries are exported properly in Azure Synapse
      e <- new.env()
      e$libs <- .libPaths()
      parallel::clusterExport(cl, "libs", envir = e)
      parallel::clusterEvalQ(cl, .libPaths(libs))
    }

    doParallel::registerDoParallel(cl)

    `%op%` <- foreach::`%dopar%`

    packages <- c(base_packages, add_packages, parallel_packages)
  } else {
    stop("error")
  }

  return(list(packages = packages, foreach_operator = `%op%`, cl = cl, parallel_processing = parallel_processing))
}

#' Function to clean up after submitting tasks sequentially, in parallel on local machine, or in spark
#'
#' @param cl cluster object
#'
#' @noRd
par_end <- function(cl) {
  if (!is.null(cl)) {
    try(parallel::stopCluster(cl), silent = TRUE)
  }
  foreach::registerDoSEQ()
}


#' Function to clean up after a foreach error when submitting tasks sequentially, in parallel on local machine, or in spark
#'
#' @param par_info parallel info
#'
#' @noRd
cancel_parallel <- function(par_info) {
  
  if(is.null(par_info$parallel_processing)) {
    parallel_processing <- "none"
  } else {
    parallel_processing <- par_info$parallel_processing
  }
  
  # Spark: cancel active jobs immediately (best-effort)
  if (parallel_processing == "spark") {
    # drop foreach backend first
    try(foreach::registerDoSEQ(), silent = TRUE)
    try(parallel::stopCluster(par_info$cl), silent = TRUE)
    
    # cancel running jobs
    try({
      sctx <- sparklyr::spark_context(sc)
      sparklyr::invoke(sctx, "cancelAllJobs")
    }, silent = TRUE)
    
    # reset spark connection
    # conf       <- sc$config
    # master     <- sc$master
    # ver        <- sc$version
    # spark_home <- sc$spark_home
    # 
    # sparklyr::spark_disconnect(sc)
    # 
    # assign("sc", sparklyr::spark_connect(master=master, version=ver, spark_home=spark_home, config=conf),
    #        envir = .GlobalEnv)
    # 
    # # check spark session is running
    # if (!sparklyr::connection_is_open(sc)) {
    #   stop("Spark session is not open")
    # }
    
    reset_spark()
  }
  
  # PSOCK: tear down cluster
  if (parallel_processing == "local_machine") {
    try(parallel::stopCluster(par_info$cl), silent = TRUE)
  }
  
  try(foreach::registerDoSEQ(), silent = TRUE)  # final safeguard
}

reset_spark <- function() {
  # reset spark connection
  conf       <- sc$config
  master     <- sc$master
  ver        <- sc$version
  spark_home <- sc$spark_home
  
  sparklyr::spark_disconnect(sc)
  
  assign("sc", sparklyr::spark_connect(master=master, version=ver, spark_home=spark_home, config=conf),
         envir = .GlobalEnv)
  
  # check spark session is running
  if (!sparklyr::connection_is_open(sc)) {
    stop("Spark session is not open")
  }
}
