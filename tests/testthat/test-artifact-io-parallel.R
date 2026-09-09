test_that("PSOCK workers read independent exact recipe paths without enumeration", {
  run_info <- artifact_test_run(withr::local_tempdir(), "rds")
  for (combo in c("A", "B")) {
    for (recipe in c("R1", "R2")) artifact_test_recipe(run_info, combo, recipe)
  }
  jobs <- lapply(c("A", "B"), function(combo) {
    list(run_info = run_info, combo = hash_data(combo), recipes = c("R1", "R2"))
  })
  before <- serialize(jobs, NULL)
  expected <- lapply(jobs, function(job) {
    get_recipe_data(job$run_info, job$combo, recipes = job$recipes)
  })
  cluster <- parallel::makePSOCKcluster(2)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  package_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  definitions <- NULL
  if (!file.exists(file.path(package_path, "Meta", "package.rds"))) {
    symbols <- c("get_recipe_data", "read_local_artifacts", "local_artifact_files",
      "local_artifact_path", "read_file", "hash_data", "get_recipes_to_run")
    definitions <- setNames(lapply(symbols, function(symbol) {
      definition <- getFromNamespace(symbol, "finnts")
      environment(definition) <- baseenv()
      definition
    }), symbols)
  }
  bootstrap <- function(libraries, definitions) {
    .libPaths(libraries)
    worker_environment <- asNamespace("finnts")
    if (!is.null(definitions)) {
      worker_environment <- new.env(parent = worker_environment)
      for (symbol in names(definitions)) {
        definition <- definitions[[symbol]]
        environment(definition) <- worker_environment
        assign(symbol, definition, envir = worker_environment)
      }
      worker_environment$list_files <- function(...) {
        stop("worker enumerated a directory", call. = FALSE)
      }
    }
    assign("finnts_artifact_test_environment", worker_environment, envir = globalenv())
    TRUE
  }
  environment(bootstrap) <- baseenv()
  parallel::clusterCall(cluster, bootstrap, .libPaths(), definitions)

  result <- parallel::parLapply(cluster, jobs, function(job) {
    testthat::local_mocked_bindings(
      list_files = function(...) stop("worker enumerated a directory", call. = FALSE),
      .package = "finnts"
    )
    worker_environment <- get("finnts_artifact_test_environment", envir = globalenv())
    worker_environment$get_recipe_data(
      job$run_info, job$combo, recipes = job$recipes
    )
  })

  expect_equal(result, expected)
  expect_identical(serialize(jobs, NULL), before)
  expect_setequal(result[[1]]$Data[[1]]$Combo, "A")
  expect_setequal(result[[2]]$Data[[1]]$Combo, "B")
  missing_job <- jobs[[1]]
  missing_job$combo <- hash_data("missing")
  expect_error(parallel::parLapply(cluster, list(missing_job), function(job) {
    worker_environment <- get("finnts_artifact_test_environment", envir = globalenv())
    worker_environment$get_recipe_data(
      job$run_info, job$combo, recipes = job$recipes
    )
  }), "Missing required Finn artifact")
})

test_that("Spark prepared-data routing continues to use path instead of file_list", {
  run_info <- artifact_test_run(withr::local_tempdir(), "parquet")
  run_info$combo <- hash_data("A")
  observed <- list()
  testthat::local_mocked_bindings(
    read_file = function(run_info, path = NULL, file_list = NULL, return_type = "df", ...) {
      observed[[length(observed) + 1L]] <<- list(path = path, file_list = file_list, return_type = return_type)
      if (!is.null(file_list)) return(tibble::tibble(combo_variables = "ID"))
      tibble::tibble(Combo = "A", Target = 1)
    },
    .package = "finnts"
  )

  result <- get_prepped_data(run_info, "R1", return_type = "sdf")

  expect_identical(result$ID, "A")
  expect_identical(observed[[2]]$return_type, "sdf")
  expect_null(observed[[2]]$file_list)
  expect_match(observed[[2]]$path, "*", fixed = TRUE)
})

test_that("Spark forecast routing retains its bulk path contract", {
  run_info <- artifact_test_run(withr::local_tempdir(), "parquet")
  run_info$combo <- hash_data("A")
  forecast <- artifact_test_forecast(run_info, write_output = FALSE)
  observed <- list()
  testthat::local_mocked_bindings(
    list_files = function(...) character(),
    read_file = function(run_info, path = NULL, file_list = NULL, return_type = "df", ...) {
      observed[[length(observed) + 1L]] <<- list(path = path, file_list = file_list, return_type = return_type)
      if (!is.null(file_list)) {
        return(tibble::tibble(combo_variables = "ID", forecast_approach = "bottoms_up"))
      }
      if (grepl("train_test_split", path, fixed = TRUE)) {
        return(tibble::tibble(Run_Type = "Future_Forecast", Train_Test_ID = 1))
      }
      forecast
    },
    .package = "finnts"
  )

  result <- get_forecast_data(run_info, return_type = "sdf")

  expect_identical(result$Combo, "A")
  expect_identical(observed[[3]]$return_type, "sdf")
  expect_null(observed[[3]]$file_list)
  expect_match(observed[[3]]$path, "*", fixed = TRUE)
})

test_that("provider recipe routing retains its original discovery and download path", {
  run_info <- artifact_test_run("provider-root")
  run_info$storage_object <- structure(list(), class = "blob_container")
  recipe_path <- artifact_test_path(run_info, "prep_data", "A", "-R1")
  listings <- 0L
  observed <- NULL
  testthat::local_mocked_bindings(
    list_files = function(...) {
      listings <<- listings + 1L
      recipe_path
    },
    read_file = function(run_info, path = NULL, file_list = NULL, ...) {
      observed <<- list(path = path, file_list = file_list)
      tibble::tibble(Combo = "A", Target = 1)
    },
    .package = "finnts"
  )

  result <- get_recipe_data(run_info, hash_data("A"))

  expect_identical(result$Recipe, "R1")
  expect_equal(listings, 1L)
  expect_null(observed$file_list)
  expect_match(observed$path, "/prep_data/", fixed = TRUE)
})

test_that("installed train_models dispatches exact recipe reads to real PSOCK workers", {
  package_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(package_path, "Meta", "package.rds")),
    "Actual training dispatch is verified from the installed namespace"
  )
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  template <- set_run_info(project_name = "artifact_dispatch", run_name = "test",
    path = withr::local_tempdir(), data_output = "csv", object_output = "rds", add_unique_id = FALSE
  )
  data <- tibble::tibble(
    id = rep(c("A", "B"), each = 36),
    Date = rep(seq.Date(as.Date("2020-01-01"), by = "month", length.out = 36), 2),
    value = c(100 + seq_len(36), 300 + 3 * seq_len(36))
  )
  prep_data(template, data, combo_variables = "id", target_variable = "value",
    date_type = "month", forecast_horizon = 2, recipes_to_run = "R1"
  )
  prep_models(template, models_to_run = "meanf", back_test_scenarios = 1,
    num_hyperparameters = 1, run_ensemble_models = FALSE
  )
  copy_template <- function(path, include_recipes = TRUE) {
    info <- template
    info$path <- path
    folders <- c("logs", "prep_models", if (include_recipes) "prep_data")
    for (folder in folders) fs::dir_copy(fs::path(template$path, folder), fs::path(path, folder))
    fs::dir_create(fs::path(path, c("forecasts", "models", "prep_data")))
    if (!include_recipes) {
      fs::file_copy(artifact_test_path(template, "prep_data", suffix = "-orig_combo_info"),
        artifact_test_path(info, "prep_data", suffix = "-orig_combo_info"))
    }
    info
  }
  sequential_info <- copy_template(withr::local_tempdir())
  parallel_info <- copy_template(withr::local_tempdir())
  missing_info <- copy_template(withr::local_tempdir(), include_recipes = FALSE)
  missing_info$combo <- hash_data("A")
  train_models(sequential_info, run_global_models = FALSE, run_local_models = TRUE, num_cores = 2)

  install_guard <- function(root) {
    asNamespace("finnts")
    tracker <- new.env(parent = emptyenv())
    tracker$recipe_reads <- character()
    tracker$listings <- 0L
    root <- paste0(gsub("\\", "/", root, fixed = TRUE), "/")
    original_dir_ls <- fs::dir_ls
    original_list_files <- list.files
    original_read <- vroom::vroom
    record_directory <- function(path) {
      if (any(startsWith(paste0(gsub("\\", "/", as.character(path), fixed = TRUE), "/"), root))) {
        tracker$listings <- tracker$listings + 1L
        stop("training worker enumerated an artifact directory", call. = FALSE)
      }
    }
    testthat::local_mocked_bindings(
      dir_ls = function(path = ".", ...) {
        record_directory(path)
        original_dir_ls(path, ...)
      },
      .package = "fs", .env = globalenv()
    )
    testthat::local_mocked_bindings(
      list.files = function(path = ".", ...) {
        record_directory(path)
        original_list_files(path, ...)
      },
      .package = "base", .env = globalenv()
    )
    testthat::local_mocked_bindings(
      vroom = function(file, ...) {
        if (is.character(file)) {
          paths <- gsub("\\", "/", as.character(file), fixed = TRUE)
          tracker$recipe_reads <- c(tracker$recipe_reads,
            paths[startsWith(paths, root) & grepl("-R[12]\\.csv$", paths)])
        }
        original_read(file, ...)
      },
      .package = "vroom", .env = globalenv()
    )
    assign("finnts_training_io", tracker, envir = globalenv())
    TRUE
  }
  environment(install_guard) <- baseenv()
  collect_guard <- function() {
    tracker <- get("finnts_training_io", envir = globalenv())
    list(pid = Sys.getpid(), recipe_reads = tracker$recipe_reads, listings = tracker$listings)
  }
  environment(collect_guard) <- baseenv()
  original_start <- par_start
  original_end <- par_end
  clusters <- list()
  reports <- list()
  on.exit({
    for (cluster in clusters) try(parallel::stopCluster(cluster), silent = TRUE)
    foreach::registerDoSEQ()
  }, add = TRUE)
  testthat::local_mocked_bindings(
    par_start = function(run_info, ...) {
      info <- original_start(run_info, ...)
      if (inherits(info$cl, "cluster")) {
        clusters[[length(clusters) + 1L]] <<- info$cl
        parallel::clusterCall(info$cl, install_guard, run_info$path)
      }
      info
    },
    par_end = function(cl) {
      if (inherits(cl, "cluster")) reports <<- c(reports, parallel::clusterCall(cl, collect_guard))
      original_end(cl)
    },
    .package = "finnts"
  )

  train_models(parallel_info, run_global_models = FALSE, run_local_models = TRUE,
    parallel_processing = "local_machine", num_cores = 2
  )

  expect_length(reports, 2L)
  expect_equal(length(unique(vapply(reports, function(report) report$pid, integer(1)))), 2L)
  expect_equal(sum(vapply(reports, function(report) report$listings, integer(1))), 0L)
  recipe_reads <- unlist(lapply(reports, function(report) report$recipe_reads), use.names = FALSE)
  expect_setequal(recipe_reads, vapply(c("A", "B"), function(combo) {
    as.character(artifact_test_path(parallel_info, "prep_data", combo, "-R1"))
  }, character(1)))
  expect_length(recipe_reads, 2L)
  for (combo in c("A", "B")) {
    expected <- read_file(sequential_info,
      file_list = artifact_test_path(sequential_info, "forecasts", combo, "-single_models"), strict = TRUE)
    actual <- read_file(parallel_info,
      file_list = artifact_test_path(parallel_info, "forecasts", combo, "-single_models"), strict = TRUE)
    expect_equal(actual, expected)
    expected_models <- readRDS(artifact_test_path(sequential_info, "models", combo, "-single_models", "rds"))
    actual_models <- readRDS(artifact_test_path(parallel_info, "models", combo, "-single_models", "rds"))
    expect_equal(dplyr::select(actual_models, -Model_Fit), dplyr::select(expected_models, -Model_Fit))
  }
  expect_error(train_models(missing_info, run_global_models = FALSE, run_local_models = TRUE,
    parallel_processing = "local_machine", num_cores = 2
  ), "Missing required Finn artifact.*R1")
})