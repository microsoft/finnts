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
