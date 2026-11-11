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

# Per-process record of the future plan finnts registered for its active local
# PSOCK pool: `cluster` is that pool and `previous` is the caller's plan to
# restore. One slot suffices because a process runs at most one finnts pool at a
# time; par_start() already replaces any earlier foreach backend.
par_future_state <- new.env(parent = emptyenv())

#' Share a local PSOCK pool with tune through a future plan
#'
#' tune 2.x no longer uses foreach backends; it parallelizes only through a
#' registered future plan (or mirai daemons). Registering the pool foreach
#' already uses lets `tune_grid()` and `fit_resamples()` run on the same
#' workers, so inner parallelism needs no second pool or extra memory.
#' A registration left by an earlier error is restored first, so the saved
#' caller plan is never a stale finnts pool.
#'
#' @param cl PSOCK cluster created by `par_start()`.
#'
#' @return `cl`, invisibly. Side effect: changes the session's future plan and
#'   records the caller's plan in `par_future_state`.
#' @noRd
par_future_register <- function(cl) {
  par_future_restore()
  previous <- future::plan(future::cluster, workers = cl)
  par_future_state$cluster <- cl
  par_future_state$previous <- previous
  invisible(cl)
}

#' Restore the caller's future plan after a finnts pool
#'
#' @param cl Optional PSOCK cluster. When supplied, only that pool's
#'   registration is restored, so ending one pool never undoes another's.
#'   `NULL` restores any registration, for error cleanup.
#'
#' @return `TRUE` invisibly when a plan was restored, otherwise `FALSE`.
#'   Side effect: resets the session's future plan and clears
#'   `par_future_state`.
#' @noRd
par_future_restore <- function(cl = NULL) {
  if (!exists("previous", envir = par_future_state, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  if (!is.null(cl) && !identical(par_future_state$cluster, cl)) {
    return(invisible(FALSE))
  }
  previous <- par_future_state$previous
  rm(list = c("cluster", "previous"), envir = par_future_state)
  future::plan(previous)
  invisible(TRUE)
}

#' Attach worker packages on every PSOCK worker at once
#'
#' future's cluster backend attaches a future's packages on its worker with a
#' blocking call while launching it, so tune's first futures on a fresh pool
#' start one worker after another (about 8 s each for finnts' dependencies).
#' Attaching them here runs on all workers concurrently. foreach `.packages`
#' and tune `pkgs` then find them attached. Failures are ignored, so a missing
#' package still raises the existing foreach or tune error when it is needed.
#'
#' @param cl PSOCK cluster.
#' @param packages Character vector of package names to attach.
#'
#' @return `NULL`, invisibly. Side effect: attaches packages on each worker.
#' @noRd
par_attach_worker_packages <- function(cl, packages) {
  attach_packages <- function(packages) {
    for (package in packages) {
      try(suppressPackageStartupMessages(library(package, character.only = TRUE)), silent = TRUE)
    }
    NULL
  }
  # keep the worker function free of this frame so only package names are sent
  environment(attach_packages) <- baseenv()
  parallel::clusterCall(cl, attach_packages, unique(packages))
  invisible(NULL)
}

#' Function to submit tasks sequentially, in parallel on local machine, or in spark
#'
#' A `"local_machine"` pool attaches its worker packages up front, then is
#' registered both as the foreach backend and as the session's future plan, so
#' tune 2.x tunes and refits on the same, already loaded workers. `par_end()`
#' or `cancel_parallel()` restores the caller's plan.
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
  try(doParallel::stopImplicitCluster(), silent = TRUE) # harmless if none
  try(foreach::registerDoSEQ(), silent = TRUE)
  cl <- NULL

  # packages to export to cluster
  base_packages <- c(
    "tibble", "dplyr", "timetk", "hts", "tidyselect", "stringr", "foreach",
    "doParallel", "parallel", "lubridate", "parsnip", "tune", "dials", "workflows",
    "Cubist", "earth", "glmnet", "kernlab", "purrr",
    "recipes", "rules", "modeltime", "fs", "digest", "tidyr",
    "vroom", "utils", "cli", "tseries"
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

    packages <- c(base_packages, add_packages, parallel_packages)
    par_attach_worker_packages(cl, c(packages, "finnts"))

    doParallel::registerDoParallel(cl)
    par_future_register(cl)

    `%op%` <- foreach::`%dopar%`
  } else {
    stop("error")
  }

  return(list(packages = packages, foreach_operator = `%op%`, cl = cl, parallel_processing = parallel_processing))
}

#' Function to clean up after submitting tasks sequentially, in parallel on local machine, or in spark
#'
#' Restores the caller's future plan if `cl` was registered by `par_start()`,
#' then stops the cluster and resets foreach to sequential.
#'
#' @param cl cluster object
#'
#' @noRd
par_end <- function(cl) {
  if (!is.null(cl)) {
    try(par_future_restore(cl), silent = TRUE)
    try(parallel::stopCluster(cl), silent = TRUE)
  }
  foreach::registerDoSEQ()
}


#' Function to clean up after a foreach error when submitting tasks sequentially, in parallel on local machine, or in spark
#'
#' Also restores any future plan a finnts pool registered, including one left
#' behind by an inner pool that the error skipped.
#'
#' @param par_info parallel info
#'
#' @noRd
cancel_parallel <- function(par_info) {
  if (is.null(par_info$parallel_processing)) {
    parallel_processing <- "none"
  } else {
    parallel_processing <- par_info$parallel_processing
  }

  try(par_future_restore(), silent = TRUE)

  # Spark: cancel active jobs immediately (best-effort)
  if (parallel_processing == "spark") {
    # drop foreach backend first
    try(foreach::registerDoSEQ(), silent = TRUE)
    try(parallel::stopCluster(par_info$cl), silent = TRUE)

    # cancel running jobs
    try(
      {
        sctx <- sparklyr::spark_context(sc)
        sparklyr::invoke(sctx, "cancelAllJobs")
      },
      silent = TRUE
    )

    # reset spark connection
    reset_spark()
  }

  # PSOCK: tear down cluster
  if (parallel_processing == "local_machine") {
    try(parallel::stopCluster(par_info$cl), silent = TRUE)
  }

  try(foreach::registerDoSEQ(), silent = TRUE) # final safeguard
}

reset_spark <- function() {
  # reset spark connection
  conf <- sc$config
  master <- sc$master
  ver <- sc$version
  spark_home <- sc$spark_home

  sparklyr::spark_disconnect(sc)

  fn_env <- .GlobalEnv

  assign("sc", sparklyr::spark_connect(master = master, version = ver, spark_home = spark_home, config = conf),
    envir = fn_env
  )

  # check spark session is running
  if (!sparklyr::connection_is_open(sc)) {
    stop("Spark session is not open")
  }
}
