# tests/testthat/test-finalize_run.R

# helper to build a minimal agent_info with a temp directory
make_finalize_agent_info <- function(project_name = "test_proj",
                                     run_id = "run_1",
                                     max_iter = 5) {
  tmp <- file.path(tempdir(), paste0("finalize_", project_name, "_", run_id))
  logs_dir <- file.path(tmp, "logs")
  fs::dir_create(logs_dir)

  project_info <- list(
    project_name = project_name,
    path = tmp,
    data_output = "csv",
    object_output = "rds",
    storage_object = NULL
  )

  list(
    project_info = project_info,
    run_id = run_id,
    max_iter = max_iter
  )
}

# helper to write a best_run csv file to the expected path
write_best_run_file <- function(agent_info, combo_name, model_type = "local",
                                run_complete = FALSE, max_iterations = 2) {
  project_info <- agent_info$project_info
  combo_hash <- hash_data(combo_name)

  best_run_tbl <- tibble::tibble(
    combo = combo_name,
    model_type = model_type,
    weighted_mape = 0.15,
    run_complete = run_complete,
    max_iterations = max_iterations,
    best_run_name = agent_info$run_id,
    models_to_run = "xgboost",
    recipes_to_run = "R1"
  )

  best_run_file <- file.path(
    project_info$path, "logs",
    paste0(
      hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "-",
      combo_hash, "-agent_best_run.csv"
    )
  )

  utils::write.csv(best_run_tbl, best_run_file, row.names = FALSE)

  invisible(best_run_file)
}

# helper to read back a best_run csv file
read_best_run_file <- function(agent_info, combo_name) {
  project_info <- agent_info$project_info
  combo_hash <- hash_data(combo_name)

  best_run_file <- file.path(
    project_info$path, "logs",
    paste0(
      hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "-",
      combo_hash, "-agent_best_run.csv"
    )
  )

  read.csv(best_run_file, stringsAsFactors = FALSE)
}

# * Local combo tests ----

test_that("finalize_run updates local combo best_run file", {
  agent_info <- make_finalize_agent_info()
  write_best_run_file(agent_info, "M750", model_type = "local")

  combo_hash <- hash_data("M750")
  result <- finalize_run(agent_info = agent_info, combo = combo_hash)

  expect_equal(result, "Run finalized successfully.")

  updated <- read_best_run_file(agent_info, "M750")
  expect_true(updated$run_complete)
  expect_equal(updated$max_iterations, 5)
})

test_that("finalize_run errors when local combo best_run file is missing", {
  agent_info <- make_finalize_agent_info(project_name = "missing_proj")

  expect_error(
    finalize_run(agent_info = agent_info, combo = "nonexistent_hash"),
    "No best run file found"
  )
})

# * Global combo tests ----

test_that("finalize_run updates global combo best_run files", {
  agent_info <- make_finalize_agent_info(project_name = "global_proj")
  write_best_run_file(agent_info, "M750", model_type = "global")
  write_best_run_file(agent_info, "M1", model_type = "global")

  result <- finalize_run(agent_info = agent_info, combo = NULL)

  expect_equal(result, "Run finalized successfully.")

  for (combo_name in c("M750", "M1")) {
    updated <- read_best_run_file(agent_info, combo_name)
    expect_true(updated$run_complete)
    expect_equal(updated$max_iterations, 5)
  }
})

test_that("finalize_run returns successfully when no global combos exist", {
  agent_info <- make_finalize_agent_info(project_name = "local_only_proj")
  write_best_run_file(agent_info, "M750", model_type = "local")

  result <- finalize_run(agent_info = agent_info, combo = NULL)

  expect_equal(result, "Run finalized successfully.")

  # verify the local file was NOT modified
  unchanged <- read_best_run_file(agent_info, "M750")
  expect_false(unchanged$run_complete)
  expect_equal(unchanged$max_iterations, 2)
})

test_that("finalize_run errors when no best_run files exist for global path", {
  agent_info <- make_finalize_agent_info(project_name = "empty_proj")

  expect_error(
    finalize_run(agent_info = agent_info, combo = NULL),
    "No best run files found"
  )
})
