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
submit_fn <- function(obj_list,
                      parallel_processing, 
                      iterator, 
                      fn, 
                      num_cores = NULL, 
                      package_exports, 
                      error_handling = "stop", 
                      batch_size = 1000){

  num_rounds <- ceiling(length(iterator)/batch_size)
  final_data <- NULL
  
  cli::cli_alert_info("Running {length(iterator)} total tasks in {num_rounds} separate batches of {min(batch_size, length(iterator))} tasks")
  
  if(is.null(parallel_processing)) {

    for(round in 1:num_rounds) {

      iterator_max <- min(round * batch_size, length(iterator))
      iterator_min <- ((round-1) * batch_size) + 1

      iterator_round <- iterator[iterator_min:iterator_max]

      combos <- iterator_round %>%
        dplyr::bind_rows() %>%
        dplyr::select(Combo) %>%
        dplyr::distinct() %>%
        dplyr::pull(Combo)
      
      if('All-Data' %in% combos) {
        large_tbl <- obj_list$input_data
      } else {
        large_tbl <- obj_list$input_data %>%
          dplyr::filter(Combo %in% combos)
      }
      
      obj_list_final <- append(obj_list, list(large_tbl = large_tbl))
      obj_list_final <- obj_list_final[names(obj_list_final) != "input_data"]    

      run_fn <- fn(obj_list_final)
    
      # temp <- lapply(iterator_round, fn)
      # temp <- do.call(rbind, temp)
      
      temp <- foreach::foreach(i = iterator_round, 
                               .combine = 'rbind',
                               .packages = package_exports,
                               #.export = c(function_exports, "large_tbl"), 
                               .errorhandling = error_handling, 
                               .verbose = FALSE, 
                               .inorder = FALSE, 
                               .multicombine = TRUE, 
                               .noexport = NULL
      ) %do% {run_fn(i)}
      
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
      
      combos <- iterator_round %>%
        dplyr::bind_rows() %>%
        dplyr::select(Combo) %>%
        dplyr::distinct() %>%
        dplyr::pull(Combo)
      
      if('All-Data' %in% combos) {
        large_tbl <- obj_list$input_data
      } else {
        large_tbl <- obj_list$input_data %>%
          dplyr::filter(Combo %in% combos)
      }
      
      obj_list_final <- append(obj_list, list(large_tbl = large_tbl))
      obj_list_final <- obj_list_final[names(obj_list_final) != "input_data"] 
      
      run_fn <- fn(obj_list_final)

      temp <- foreach::foreach(i = iterator_round, 
                                   .combine = 'rbind',
                                   .packages = package_exports,
                                   #.export = c(function_exports, "large_tbl"), 
                                   .errorhandling = error_handling, 
                                   .verbose = FALSE, 
                                   .inorder = FALSE, 
                                   .multicombine = TRUE, 
                                   .noexport = NULL
      ) %dopar% {run_fn(i)}
      
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
      
      combos <- iterator_round %>%
        dplyr::bind_rows() %>%
        dplyr::select(Combo) %>%
        dplyr::distinct() %>%
        dplyr::pull(Combo)
      
      if('All-Data' %in% combos) {
        large_tbl <- obj_list$input_data
      } else {
        large_tbl <- obj_list$input_data %>%
          dplyr::filter(Combo %in% combos)
      }
      
      obj_list_final <- append(obj_list, list(large_tbl = large_tbl))
      obj_list_final <- obj_list_final[names(obj_list_final) != "input_data"]    
      
      run_fn <- fn(obj_list_final)
      
      #sparklyr::registerDoSpark(sc, parallelism = length(iterator_round))
      sparklyr::registerDoSpark(sc)
      
      temp <- foreach::foreach(i = iterator_round, 
                               .combine = 'rbind', 
                               #.export = c(function_exports, "large_tbl"), 
                               .packages = package_exports,
                               .errorhandling = error_handling, 
                               .verbose = FALSE, 
                               .inorder = FALSE, 
                               .multicombine = TRUE, 
                               .noexport = NULL) %dopar% {run_fn(i)}
      
      final_data <- rbind(final_data, temp)
      
      cli::cli_alert_success("Finished batch {round} out of {num_rounds} total")
    }

    return(final_data)
    
  } else {
    stop("error during function submission")
  }
}


#' Function to prep for tasks to be submitted either sequentially, in parallel on local machine, or spark
#' 
#' @param parallel_processing type of parallel processing to run
#' @param num_cores number of cores to use on local machine
#' @param package_exports packages to export
#' 
#' @return parallel run objects
#' 
#' @noRd
run_start <- function(parallel_processing,
                      num_cores, 
                      package_exports){
  
  if(is.null(parallel_processing)) {
    
    `%op%` <- foreach::`%do%`
    
    packages <- package_exports
    
  } else if(parallel_processing == "spark") {
    
    cli::cli_h2("Submitting Tasks to Spark")
    
    `%op%` <- foreach::`%dopar%`
    
    sparklyr::registerDoSpark(sc, parallelism = length(combo_list))
    
    packages <- NULL
    
  } else if(parallel_processing == "local_machine") {
    
    cli::cli_h2("Creating Parallel Processing")
    
    cores <- get_cores(num_cores)
    
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    cli::cli_alert_info("Running across {cores} cores")
    
    `%op%` <- foreach::`%dopar%`
    
    packages <- c("tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
                  'doParallel', 'parallel', "lubridate", 'parsnip', 'tune', 'dials', 'workflows',
                  'Cubist', 'earth', 'glmnet', 'kernlab', 'modeltime.gluonts', 'purrr',
                  'recipes', 'rules', 'modeltime')
    
  } else {
    stop("error")
  }
  
  
}

#' Function to clean up after submitting tasks sequentially, in parallel on local machine, or spark
#' 
#' @param parallel_processing type of parallel processing to run
#' 
#' @noRd
run_start <- function(parallel_processing){
  
  
  
}

