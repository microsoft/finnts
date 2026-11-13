#' Get number of cores to use when registering parallel back end
#'
#' Uses `parallelly::availableCores(omit = 1)`, which respects container CPU
#' limits, scheduler allocations and `mc.cores`, and leaves one core for the
#' main process. An explicit `num_cores` is capped at that limit, so a request
#' can never oversubscribe the cores this process may use.
#'
#' @param num_cores number of cores for parallel processing, or `NULL` to use
#'   every available core but one.
#'
#' @return A single integer, never below 1.
#' @noRd
get_cores <- function(num_cores) {
  available <- as.integer(unname(parallelly::availableCores(omit = 1)))
  if (is.null(num_cores)) {
    return(max(1L, available))
  }
  max(1L, min(as.integer(num_cores), available))
}

#' Cores one Spark task's inner pool may use
#'
#' `spark.executor.cores` is a scheduling count: how many tasks (series) Spark
#' runs at once on an executor, divided by `spark.task.cpus`. It doesn't limit
#' the CPUs an R process can use, so each task takes an equal share of the
#' node, minus one core for the task's own R process. This assumes one
#' executor per node, the usual setup on Synapse and Fabric. When
#' `spark.executor.cores` is unset (Databricks runs one task per core), no
#' spare cores exist and the share is 1.
#'
#' @param executor_cores Value of `spark.executor.cores`, or `NA` when unset.
#' @param task_cpus Value of `spark.task.cpus`, or `NA` for Spark's default 1.
#' @param node_cores Cores visible on a node.
#'
#' @return A single integer, at least 1.
#' @noRd
spark_task_cores <- function(executor_cores, task_cpus, node_cores) {
  executor_cores <- suppressWarnings(as.numeric(executor_cores))
  if (length(executor_cores) != 1 || is.na(executor_cores) || executor_cores < 1) {
    return(1L)
  }
  task_cpus <- suppressWarnings(as.numeric(task_cpus))
  if (length(task_cpus) != 1 || is.na(task_cpus) || task_cpus < 1) {
    task_cpus <- 1
  }
  tasks <- max(1, floor(executor_cores / task_cpus))
  as.integer(max(1, floor(as.numeric(node_cores) / tasks) - 1))
}

#' Read Spark settings from the active connection
#'
#' Reads the running SparkContext's configuration through the session's `sc`
#' connection. A key that isn't set is returned as `NA`.
#'
#' @param keys Spark configuration keys.
#'
#' @return A named character vector, or `NULL` when there's no readable
#'   connection.
#' @noRd
spark_conf_values <- function(keys) {
  tryCatch(
    {
      conn <- get("sc", envir = globalenv())
      conf <- sparklyr::invoke(sparklyr::spark_context(conn), "getConf")
      values <- vapply(keys, function(key) {
        value <- as.character(sparklyr::invoke(conf, "get", key, ""))
        if (identical(value, "")) NA_character_ else value
      }, character(1))
      stats::setNames(values, keys)
    },
    error = function(e) NULL
  )
}

#' Inner-pool core budget for Spark tasks (D13)
#'
#' With `parallel_processing = "spark"`, `inner_parallel = TRUE` and no
#' `num_cores`, every task would otherwise start a pool of all node cores but
#' one, so two tasks per executor put twice the node's cores to work. The
#' driver reads the executor settings once and sizes each task's inner pool to
#' its share with `spark_task_cores()`. Node cores are taken from the driver,
#' which on Synapse and Fabric has the executors' node size; the pool is still
#' capped at the cores each executor reports. An explicit `num_cores` is kept.
#' The result goes to inner pools only, never into `num_cores` arguments that
#' are saved in run logs.
#'
#' @param num_cores The caller's `num_cores`.
#' @param parallel_processing The caller's `parallel_processing`.
#' @param inner_parallel The caller's `inner_parallel`.
#'
#' @return `num_cores` unchanged, or an integer core budget on Spark. Also
#'   unchanged when the Spark settings can't be read.
#' @noRd
spark_inner_cores <- function(num_cores, parallel_processing, inner_parallel) {
  if (!is.null(num_cores) || !identical(parallel_processing, "spark") || !isTRUE(inner_parallel)) {
    return(num_cores)
  }
  conf <- spark_conf_values(c("spark.executor.cores", "spark.task.cpus"))
  if (is.null(conf)) {
    return(num_cores)
  }
  spark_task_cores(conf[["spark.executor.cores"]], conf[["spark.task.cpus"]], parallelly::availableCores())
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

#' Count the parallel tasks one inner pool can use
#'
#' An inner pool serves `tune_grid()` over every hyperparameter combination and
#' validation split (`parallel_over = "everything"`), then `fit_resamples()`
#' over the refit splits. Workers beyond the larger of those two counts only
#' start up, load packages and hold memory, so pools are capped at this count.
#'
#' @param hyperparameter_tbl Hyperparameter table with one row per combination,
#'   or `NULL` when no tuning runs. Its `Model` column (plus `Recipe` for
#'   individual models) identifies each tuning grid; the largest grid sets the
#'   tuning work.
#' @param train_test_tbl Train/test split table with a `Run_Type` column.
#' @param refit_types `Run_Type` values refit by `fit_resamples()`, or `NULL`
#'   when every split is refit.
#'
#' @return A single integer, at least 1.
#' @noRd
inner_task_count <- function(hyperparameter_tbl, train_test_tbl, refit_types = NULL) {
  keys <- intersect(c("Model", "Recipe"), names(hyperparameter_tbl))
  grid <- if (is.data.frame(hyperparameter_tbl) && nrow(hyperparameter_tbl) > 0 && length(keys) > 0) {
    max(table(do.call(paste, unname(as.list(hyperparameter_tbl[keys])))))
  } else {
    1L
  }
  run_type <- if (is.data.frame(train_test_tbl)) as.character(train_test_tbl$Run_Type) else character()
  validation <- sum(run_type == "Validation")
  refits <- if (is.null(refit_types)) length(run_type) else sum(run_type %in% refit_types)
  max(1L, as.integer(grid) * max(1L, validation), as.integer(refits))
}

# Per-process state of the inner-pool scope opened by with_inner_pool():
# `scope` is TRUE while a scope is open, `cl` is its shared PSOCK pool (NULL
# until the first inner request), `size` that pool's worker count and `cores`
# the scope's core budget for requests without `num_cores`.
par_inner_state <- new.env(parent = emptyenv())

#' Run code with one reusable inner pool
#'
#' Inner pools (`par_start(..., inner = TRUE)`) normally start and stop for
#' every series and stage, paying package loading each time. Inside this scope
#' they share one pool per process: the first request creates it, `par_end()`
#' keeps it, and the scope's end stops it and restores the caller's future
#' plan, including after an error. Nested scopes use the outermost one.
#'
#' @param code Code to evaluate.
#' @param active When `FALSE`, `code` runs without opening a scope.
#' @param cores Core budget for inner requests with `num_cores = NULL`, such
#'   as a Spark task's share of its node from `spark_inner_cores()`. `NULL`
#'   keeps the default of every available core but one. Carried here rather
#'   than in `num_cores` so saved run logs keep the caller's setting.
#'
#' @return The value of `code`.
#' @noRd
with_inner_pool <- function(code, active = TRUE, cores = NULL) {
  local_inner_pool(active, cores)
  code
}

#' Open an inner-pool scope until the calling function exits
#'
#' Function-level form of `with_inner_pool()` for stage functions whose whole
#' body should share one inner pool. Does nothing when `active` is `FALSE` or a
#' scope is already open, so only the outermost caller owns the pool.
#'
#' @param active `TRUE` to open the scope.
#' @param cores Core budget for automatic inner requests; see
#'   `with_inner_pool()`.
#' @param env Frame whose exit closes the scope; defaults to the caller's.
#'
#' @return `TRUE` invisibly when this call opened the scope, otherwise `FALSE`.
#'   Side effect: registers `par_inner_close()` on exit of `env`.
#' @noRd
local_inner_pool <- function(active = TRUE, cores = NULL, env = parent.frame()) {
  if (!isTRUE(active) || isTRUE(par_inner_state$scope)) {
    return(invisible(FALSE))
  }
  par_inner_state$scope <- TRUE
  par_inner_state$cores <- cores
  do.call(base::on.exit, list(quote(par_inner_close()), TRUE, FALSE), envir = env)
  invisible(TRUE)
}

#' Close the inner-pool scope
#'
#' Clears the scope state, then restores the caller's future plan and stops the
#' shared pool when one was created. Cleanup errors are ignored so the original
#' error of a failed stage is never masked.
#'
#' @return `NULL`, invisibly.
#' @noRd
par_inner_close <- function() {
  cl <- par_inner_state$cl
  par_inner_state$scope <- FALSE
  par_inner_state$cl <- NULL
  par_inner_state$size <- 0L
  par_inner_state$cores <- NULL
  if (!is.null(cl)) {
    try(par_future_restore(cl), silent = TRUE)
    try(parallel::stopCluster(cl), silent = TRUE)
    try(foreach::registerDoSEQ(), silent = TRUE)
  }
  invisible(NULL)
}

#' Check that every worker of a PSOCK pool still answers
#'
#' @param cl PSOCK cluster.
#'
#' @return `TRUE` when all workers respond, otherwise `FALSE`.
#' @noRd
par_pool_alive <- function(cl) {
  ping <- function() TRUE
  environment(ping) <- baseenv()
  isTRUE(tryCatch(all(vapply(parallel::clusterCall(cl, ping), isTRUE, logical(1))),
    error = function(e) FALSE
  ))
}

#' Start a local PSOCK pool with finnts' worker packages attached
#'
#' On Synapse (`/synfs` paths) workers first receive the session's library
#' paths so they find the same packages.
#'
#' @param run_info Run info; `path` decides the Synapse library export.
#' @param size Number of workers.
#' @param packages Packages to attach on every worker, besides finnts.
#'
#' @return The PSOCK cluster.
#' @noRd
par_make_pool <- function(run_info, size, packages) {
  cl <- parallel::makeCluster(size)

  if (substr(run_info$path, start = 1, stop = 6) == "/synfs") {
    # make sure libraries are exported properly in Azure Synapse
    e <- new.env()
    e$libs <- .libPaths()
    parallel::clusterExport(cl, "libs", envir = e)
    parallel::clusterEvalQ(cl, .libPaths(libs))
  }

  par_attach_worker_packages(cl, c(packages, "finnts"))
  cl
}

#' Get the shared inner pool of the open scope
#'
#' Reuses the scope's pool when it has at least `size` workers and all of them
#' answer. A smaller or broken pool is stopped and replaced, so a worker lost
#' to an earlier failure never breaks later stages.
#'
#' @inheritParams par_make_pool
#'
#' @return The shared PSOCK cluster. Side effect: updates `par_inner_state`.
#' @noRd
par_inner_pool <- function(run_info, size, packages) {
  cl <- par_inner_state$cl
  if (!is.null(cl) && (par_inner_state$size < size || !par_pool_alive(cl))) {
    try(par_future_restore(cl), silent = TRUE)
    try(parallel::stopCluster(cl), silent = TRUE)
    cl <- NULL
  }
  if (is.null(cl)) {
    cl <- par_make_pool(run_info, size, packages)
    par_inner_state$cl <- cl
    par_inner_state$size <- size
  }
  cl
}

#' Function to submit tasks sequentially, in parallel on local machine, or in spark
#'
#' A `"local_machine"` pool attaches its worker packages up front, then is
#' registered both as the foreach backend and as the session's future plan, so
#' tune 2.x tunes and refits on the same, already loaded workers. `par_end()`
#' or `cancel_parallel()` restores the caller's plan. The pool has
#' `min(get_cores(num_cores), task_length)` workers. An inner pool requested
#' inside `with_inner_pool()` is shared for the whole scope.
#'
#' @param run_info run info
#' @param parallel_processing type of parallel processing to run
#' @param num_cores number of cores
#' @param task_length number of tasks the pool can run at once, or `NULL` for
#'   no limit beyond the cores
#' @param inner `TRUE` for a pool started inside one series' work
#'   (`inner_parallel`), which may be reused within `with_inner_pool()`.
#'
#' @noRd
par_start <- function(run_info,
                      parallel_processing,
                      num_cores,
                      task_length,
                      inner = FALSE) {
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
    in_scope <- isTRUE(inner) && isTRUE(par_inner_state$scope)
    if (in_scope && is.null(num_cores)) {
      num_cores <- par_inner_state$cores
    }
    size <- max(1L, as.integer(min(c(get_cores(num_cores), task_length))))
    packages <- c(base_packages, add_packages, parallel_packages)

    cl <- if (in_scope) {
      par_inner_pool(run_info, size, packages)
    } else {
      par_make_pool(run_info, size, packages)
    }

    doParallel::registerDoParallel(cl)
    if (!identical(par_future_state$cluster, cl)) {
      par_future_register(cl)
    }

    `%op%` <- foreach::`%dopar%`
  } else {
    stop("error")
  }

  return(list(packages = packages, foreach_operator = `%op%`, cl = cl, parallel_processing = parallel_processing))
}

#' Function to clean up after submitting tasks sequentially, in parallel on local machine, or in spark
#'
#' Restores the caller's future plan if `cl` was registered by `par_start()`,
#' then stops the cluster and resets foreach to sequential. The shared pool of
#' an open `with_inner_pool()` scope is kept for reuse; the scope stops it.
#'
#' @param cl cluster object
#'
#' @noRd
par_end <- function(cl) {
  if (!is.null(cl)) {
    if (identical(cl, par_inner_state$cl)) {
      foreach::registerDoSEQ()
      return(invisible(NULL))
    }
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

#' Arguments that reconnect Spark the way the user first connected
#'
#' sparklyr records `method = "gateway"` and a local `sparklyr://` master for
#' connections opened with `method = "synapse"` or `"databricks"`; only the
#' connection class keeps the platform. That gateway belongs to the backend
#' `spark_disconnect()` stops, so these platforms reconnect through their own
#' method and let it start a new backend. Other connections (for example
#' `"shell"` with master `"yarn"`, or `"livy"`) keep their master, method,
#' app name and Spark home. The original `config` is always kept.
#'
#' @param sc An open sparklyr connection.
#' @return A named list of `sparklyr::spark_connect()` arguments.
#' @noRd
spark_reconnect_args <- function(sc) {
  platforms <- c(synapse_connection = "synapse", databricks_connection = "databricks")
  platform <- platforms[vapply(names(platforms), function(cls) inherits(sc, cls), logical(1))]
  if (length(platform)) {
    return(list(method = unname(platform[[1]]), config = sc$config))
  }
  list(
    master = sc$master, method = sc$method %||% "shell", app_name = sc$app_name %||% "sparklyr",
    version = sc$version, spark_home = sc$spark_home, config = sc$config
  )
}

#' Disconnect and reconnect the global Spark connection `sc`
#'
#' Called at the start of every Spark `par_start()` stage and by
#' `cancel_parallel()` after cancelling jobs, so each stage starts on a fresh
#' Spark application. Reconnects with [spark_reconnect_args()] and replaces
#' `sc` in the global environment, because finnts and doSpark look it up there.
#' Errors if the new connection is not open.
#'
#' @return Invisible `NULL`; called for its side effect on the global `sc`.
#' @noRd
reset_spark <- function() {
  args <- spark_reconnect_args(sc)

  sparklyr::spark_disconnect(sc)

  assign("sc", do.call(sparklyr::spark_connect, args), envir = .GlobalEnv)

  # check spark session is running
  if (!sparklyr::connection_is_open(sc)) {
    stop("Spark session is not open")
  }
  invisible(NULL)
}
