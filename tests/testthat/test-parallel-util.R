# Build the run_info fields par_start() reads for a local PSOCK pool: no remote
# storage, csv/rds outputs and a non-Synapse path. Returns a plain list; no
# files are written.
parallel_util_run_info <- function() {
  list(storage_object = NULL, path = tempdir(), data_output = "csv", object_output = "rds")
}

# Return the process IDs of every worker in a PSOCK cluster as an integer vector.
parallel_util_worker_pids <- function(cl) {
  unlist(parallel::clusterEvalQ(cl, Sys.getpid()))
}

# Report which of `pids` still run, waiting up to `wait` seconds for stopped
# workers to exit. A stopped cluster's connection numbers can be reused by the
# next pool, so process state is the reliable check. Returns a logical vector.
parallel_util_running <- function(pids, wait = 10) {
  testthat::skip_if_not_installed("ps")
  running <- function() vapply(pids, function(pid) {
    isTRUE(tryCatch(ps::ps_is_running(ps::ps_handle(pid)), error = function(e) FALSE))
  }, logical(1))
  deadline <- Sys.time() + wait
  state <- running()
  while (any(state) && Sys.time() < deadline) {
    Sys.sleep(0.25)
    state <- running()
  }
  state
}

test_that("sequential par_start leaves the caller's future plan untouched", {
  before <- future::plan()
  info <- par_start(parallel_util_run_info(), NULL, num_cores = NULL, task_length = 1)
  expect_null(info$cl)
  expect_identical(future::plan(), before)
  par_end(info$cl)
  expect_identical(future::plan(), before)
})

test_that("tune runs on the local pool and par_end restores the caller's plan", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 3, "Two local workers are required")
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))

  info <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2)
  withr::defer(try(parallel::stopCluster(info$cl), silent = TRUE))
  expect_equal(future::nbrOfWorkers(), 2)

  worker_pids <- parallel_util_worker_pids(info$cl)
  resamples <- rsample::vfold_cv(mtcars, v = 4)
  fitted <- tune::fit_resamples(
    workflows::workflow(mpg ~ wt, parsnip::linear_reg()),
    resamples,
    control = tune::control_resamples(allow_par = TRUE, extract = function(x) Sys.getpid())
  )
  tune_pids <- unlist(lapply(fitted$.extracts, function(extract) extract$.extracts))
  expect_length(tune_pids, 4)
  expect_true(all(tune_pids %in% worker_pids))

  par_end(info$cl)
  expect_true(inherits(future::plan(), "sequential"))
  expect_error(parallel::clusterEvalQ(info$cl, 1))
})

test_that("par_end restores a non-sequential caller plan", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 3, "Two local workers are required")
  old_plan <- future::plan(future::multisession, workers = 2)
  withr::defer(future::plan(old_plan))

  info <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2)
  withr::defer(try(parallel::stopCluster(info$cl), silent = TRUE))
  expect_false(inherits(future::plan(), "multisession"))
  expect_equal(future::nbrOfWorkers(), 2)

  par_end(info$cl)
  expect_true(inherits(future::plan(), "multisession"))
  expect_equal(future::nbrOfWorkers(), 2)
})

test_that("cancel_parallel restores the caller's plan and stops the pool", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 3, "Two local workers are required")
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))

  info <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2)
  withr::defer(try(parallel::stopCluster(info$cl), silent = TRUE))
  expect_equal(future::nbrOfWorkers(), 2)

  cancel_parallel(info)
  expect_true(inherits(future::plan(), "sequential"))
  expect_error(parallel::clusterEvalQ(info$cl, 1))
})

test_that("a pool abandoned by an error is never restored as the caller's plan", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 3, "Two local workers are required")
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))

  abandoned <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2)
  withr::defer(try(parallel::stopCluster(abandoned$cl), silent = TRUE))
  current <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2)
  withr::defer(try(parallel::stopCluster(current$cl), silent = TRUE))
  expect_true(future::value(future::future(Sys.getpid())) %in% parallel_util_worker_pids(current$cl))

  par_end(current$cl)
  expect_true(inherits(future::plan(), "sequential"))
})

# Mock parallelly::availableCores() as a host that allows `limit` cores and
# honours `omit` the way parallelly does (never below one). Returns nothing;
# the mock lasts until the calling test ends.
parallel_util_mock_cores <- function(limit, env = parent.frame()) {
  testthat::local_mocked_bindings(
    availableCores = function(..., omit = 0L) max(1L, as.integer(limit) - as.integer(omit)),
    .package = "parallelly", .env = env
  )
}

test_that("get_cores follows the detected core limit and is never below one", {
  parallel_util_mock_cores(2)
  expect_identical(get_cores(NULL), 1L)
  expect_identical(get_cores(8), 1L)
  expect_identical(get_cores(1), 1L)

  parallel_util_mock_cores(1)
  expect_identical(get_cores(NULL), 1L)
  expect_identical(get_cores(4), 1L)
  expect_identical(get_cores(0), 1L)

  parallel_util_mock_cores(16)
  expect_identical(get_cores(NULL), 15L)
  expect_identical(get_cores(4), 4L)
  expect_identical(get_cores(64), 15L)
})

# Build a hyperparameter table with `sizes` rows per named model, keyed like
# prep_models() (Model, Recipe, Hyperparameter_Combo). Returns a tibble.
parallel_util_grid <- function(sizes, recipe = "R1") {
  dplyr::bind_rows(lapply(names(sizes), function(model) {
    tibble::tibble(Model = model, Recipe = recipe, Hyperparameter_Combo = seq_len(sizes[[model]]))
  }))
}

# Build a train/test table with the given number of splits per Run_Type.
parallel_util_splits <- function(validation = 1, back_test = 3, future = 1, ensemble = 1) {
  tibble::tibble(Run_Type = c(
    rep("Validation", validation), rep("Back_Test", back_test),
    rep("Future_Forecast", future), rep("Ensemble", ensemble)
  ))
}

test_that("inner_task_count sizes pools to the larger of tuning and refit work", {
  splits <- parallel_util_splits(validation = 1, back_test = 3, future = 1, ensemble = 1)
  expect_identical(inner_task_count(parallel_util_grid(c(arima = 1, xgboost = 10)), splits), 10L)
  expect_identical(inner_task_count(parallel_util_grid(c(arima = 1, xgboost = 10)),
    parallel_util_splits(validation = 2)), 20L)
  expect_identical(inner_task_count(parallel_util_grid(c(arima = 1)), splits), 6L)
  expect_identical(inner_task_count(parallel_util_grid(c(arima = 1)), splits,
    refit_types = c("Back_Test", "Future_Forecast")), 4L)
  expect_identical(inner_task_count(parallel_util_grid(c(arima = 1))[0, ], parallel_util_splits(0, 0, 0, 0)), 1L)

  ensemble_grid <- parallel_util_grid(c(cubist = 4, glmnet = 6))
  ensemble_grid$Recipe <- NULL
  expect_identical(inner_task_count(ensemble_grid, parallel_util_splits(validation = 2)), 12L)
})

# Skip attaching finnts' worker packages on new pools for tests that only
# check pool lifecycle and future plans, saving about 6 s per pool. Lasts
# until the calling test ends.
parallel_util_light_pools <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(par_attach_worker_packages = function(cl, packages) invisible(NULL), .env = env)
}

test_that("an inner pool scope reuses one pool and stops it when the scope ends", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 3, "Two local workers are required")
  parallel_util_light_pools()
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))
  pools <- list()

  with_inner_pool({
    first <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2, inner = TRUE)
    pools[[1]] <- first$cl
    expect_equal(future::nbrOfWorkers(), 2)
    par_end(first$cl)
    expect_equal(unlist(parallel::clusterEvalQ(first$cl, 1)), c(1, 1))

    second <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2, inner = TRUE)
    expect_identical(second$cl, first$cl)
    expect_equal(future::nbrOfWorkers(), 2)
    par_end(second$cl)
  })

  expect_true(inherits(future::plan(), "sequential"))
  expect_error(parallel::clusterEvalQ(pools[[1]], 1))
})

test_that("an inner pool scope replaces a dead or too small pool and nests", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 4, "Three local workers are required")
  parallel_util_light_pools()
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))
  pools <- list()

  with_inner_pool({
    small <- par_start(parallel_util_run_info(), "local_machine", num_cores = 3, task_length = 2, inner = TRUE)
    small_pids <- parallel_util_worker_pids(small$cl)
    par_end(small$cl)
    large <- par_start(parallel_util_run_info(), "local_machine", num_cores = 3, task_length = 3, inner = TRUE)
    expect_false(identical(large$cl, small$cl))
    expect_length(large$cl, 3)
    expect_false(any(parallel_util_worker_pids(large$cl) %in% small_pids))
    expect_false(any(parallel_util_running(small_pids)))
    par_end(large$cl)

    reused <- par_start(parallel_util_run_info(), "local_machine", num_cores = 3, task_length = 2, inner = TRUE)
    expect_identical(reused$cl, large$cl)
    par_end(reused$cl)

    with_inner_pool({
      nested <- par_start(parallel_util_run_info(), "local_machine", num_cores = 3, task_length = 2, inner = TRUE)
      expect_identical(nested$cl, large$cl)
      par_end(nested$cl)
    })
    expect_equal(length(unlist(parallel::clusterEvalQ(large$cl, 1))), 3)

    parallel::stopCluster(large$cl)
    replaced <- par_start(parallel_util_run_info(), "local_machine", num_cores = 3, task_length = 2, inner = TRUE)
    pools[[1]] <- replaced$cl
    expect_false(identical(replaced$cl, large$cl))
    expect_equal(length(unlist(parallel::clusterEvalQ(replaced$cl, 1))), 2)
    par_end(replaced$cl)
  })

  expect_error(parallel::clusterEvalQ(pools[[1]], 1))
  expect_true(inherits(future::plan(), "sequential"))
})

test_that("an inner pool scope cleans up after an error and after cancel_parallel", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 3, "Two local workers are required")
  parallel_util_light_pools()
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))
  pools <- list()

  expect_error(with_inner_pool({
    info <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2, inner = TRUE)
    pools[[1]] <- info$cl
    stop("stage failed")
  }), "stage failed")
  expect_error(parallel::clusterEvalQ(pools[[1]], 1))
  expect_true(inherits(future::plan(), "sequential"))

  with_inner_pool({
    info <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2, inner = TRUE)
    cancel_parallel(list(parallel_processing = NULL, cl = NULL))
    expect_true(inherits(future::plan(), "sequential"))
    again <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2, inner = TRUE)
    expect_identical(again$cl, info$cl)
    expect_equal(future::nbrOfWorkers(), 2)
    par_end(again$cl)
  })
  expect_true(inherits(future::plan(), "sequential"))
})

test_that("inner pools outside a scope are started and stopped per call", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 3, "Two local workers are required")
  parallel_util_light_pools()
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))

  first <- par_start(parallel_util_run_info(), "local_machine", num_cores = 2, task_length = 2, inner = TRUE)
  par_end(first$cl)
  expect_error(parallel::clusterEvalQ(first$cl, 1))
  expect_true(inherits(future::plan(), "sequential"))
})

test_that("spark_task_cores gives each Spark task its share of the node", {
  expect_identical(spark_task_cores("2", NA, 32), 15L)
  expect_identical(spark_task_cores("1", NA, 32), 31L)
  expect_identical(spark_task_cores("2", "2", 32), 31L)
  expect_identical(spark_task_cores(NA, NA, 32), 1L)
  expect_identical(spark_task_cores("8", "1", 8), 1L)
  expect_identical(spark_task_cores("4", "1", 8), 1L)
})

test_that("spark_inner_cores sizes only automatic inner pools on Spark", {
  testthat::local_mocked_bindings(
    spark_conf_values = function(keys) c(spark.executor.cores = "2", spark.task.cpus = NA_character_)
  )
  parallel_util_mock_cores(32)
  expect_identical(spark_inner_cores(NULL, "spark", TRUE), 15L)
  expect_identical(spark_inner_cores(4, "spark", TRUE), 4)
  expect_null(spark_inner_cores(NULL, "spark", FALSE))
  expect_null(spark_inner_cores(NULL, NULL, TRUE))

  testthat::local_mocked_bindings(spark_conf_values = function(keys) NULL)
  expect_null(spark_inner_cores(NULL, "spark", TRUE))
})

# Build a fake sparklyr connection holding the fields reset_spark() reads.
# `class` adds the platform subclass sparklyr gives Synapse and Databricks
# connections; those record method "gateway" and a local gateway master.
parallel_util_fake_sc <- function(method, master, class = character()) {
  structure(
    list(
      method = method, master = master, app_name = "finn_app",
      config = list(spark.executor.cores = "2"), spark_home = "/opt/spark"
    ),
    class = c("spark_connection", class, "spark_shell_connection", "DBIConnection")
  )
}

# Run reset_spark() on `old_sc` with sparklyr's connect, disconnect and
# open-check mocked. Restores any global `sc` afterwards and returns the
# arguments reset_spark() passed to sparklyr::spark_connect().
parallel_util_reset_args <- function(old_sc) {
  testthat::skip_if_not_installed("sparklyr")
  had_sc <- exists("sc", envir = .GlobalEnv, inherits = FALSE)
  previous <- if (had_sc) get("sc", envir = .GlobalEnv)
  withr::defer(if (had_sc) {
    assign("sc", previous, envir = .GlobalEnv)
  } else if (exists("sc", envir = .GlobalEnv, inherits = FALSE)) {
    rm("sc", envir = .GlobalEnv)
  })
  assign("sc", old_sc, envir = .GlobalEnv)

  connect_args <- NULL
  testthat::local_mocked_bindings(
    spark_disconnect = function(sc, ...) invisible(NULL),
    spark_connect = function(...) {
      connect_args <<- list(...)
      "new connection"
    },
    connection_is_open = function(sc) TRUE,
    .package = "sparklyr"
  )
  reset_spark()
  expect_identical(get("sc", envir = .GlobalEnv), "new connection")
  connect_args
}

test_that("reset_spark reconnects Synapse and Databricks through their own method", {
  gateway <- "sparklyr://localhost:8880/8880"
  for (platform in c("synapse", "databricks")) {
    old_sc <- parallel_util_fake_sc("gateway", gateway,
      class = c(paste0(platform, "_connection"), "spark_gateway_connection"))
    args <- parallel_util_reset_args(old_sc)
    expect_identical(args$method, platform)
    expect_identical(args$config, old_sc$config)
    expect_null(args$master)
  }
})

test_that("reset_spark keeps the method, master and settings of other connections", {
  old_sc <- parallel_util_fake_sc("shell", "yarn")
  args <- parallel_util_reset_args(old_sc)
  expect_identical(args$method, "shell")
  expect_identical(args$master, "yarn")
  expect_identical(args$spark_home, "/opt/spark")
  expect_identical(args$app_name, "finn_app")
  expect_identical(args$config, old_sc$config)

  livy <- parallel_util_reset_args(parallel_util_fake_sc("livy", "http://livy:8998"))
  expect_identical(livy$method, "livy")
  expect_identical(livy$master, "http://livy:8998")
})

test_that("an inner pool scope applies its core budget to automatic requests only", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 4, "Three local workers are required")
  parallel_util_light_pools()
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))

  with_inner_pool({
    automatic <- par_start(parallel_util_run_info(), "local_machine", num_cores = NULL, task_length = 10, inner = TRUE)
    expect_length(automatic$cl, 2)
    par_end(automatic$cl)
  }, cores = 2)

  with_inner_pool({
    explicit <- par_start(parallel_util_run_info(), "local_machine", num_cores = 3, task_length = 10, inner = TRUE)
    expect_length(explicit$cl, 3)
    par_end(explicit$cl)
  }, cores = 2)
  expect_true(inherits(future::plan(), "sequential"))
})
