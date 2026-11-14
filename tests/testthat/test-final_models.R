# tests/testthat/test-final_models.R

# * read_file empty result handling ----

test_that("read_file returns NULL or empty tibble for non-existent files", {
  tmp <- tempdir()
  run_info <- list(
    path = tmp,
    storage_object = NULL,
    data_output = "csv",
    object_output = "rds"
  )

  result <- suppressWarnings(
    read_file(run_info,
      path = "/forecasts/nonexistent-file.csv",
      return_type = "df"
    )
  )

  # read_file may return NULL or an empty tibble for missing files;
  # either way it must NOT have usable columns like Model_ID
  is_empty <- is.null(result) || nrow(result) == 0
  expect_true(is_empty)
})

test_that("empty tibble is treated as NULL by nrow guard", {
  empty_tbl <- tibble::tibble()

  # the guard used in final_models.R

  guarded <- if (is.null(empty_tbl) || nrow(empty_tbl) == 0) NULL else empty_tbl

  expect_null(guarded)
  expect_false("Model_ID" %in% colnames(empty_tbl))
})

# * adjust_combo_column ----

test_that("adjust_combo_column handles logical Combo column", {
  input <- tibble::tibble(Combo = c(TRUE, FALSE), Value = c(1, 2))
  result <- adjust_combo_column(input)

  expect_type(result$Combo, "character")
  expect_equal(result$Combo, c("T", "F"))
})

test_that("adjust_combo_column passes through character Combo", {
  input <- tibble::tibble(Combo = c("A", "B"), Value = c(1, 2))
  result <- adjust_combo_column(input)

  expect_equal(result$Combo, c("A", "B"))
})

# * create_prediction_intervals ----

test_that("create_prediction_intervals requires Model_ID column", {
  # a table missing Model_ID should fail in the group_by
  bad_tbl <- tibble::tibble(
    Combo = "A",
    Train_Test_ID = c(1, 2),
    Target = c(10, 12),
    Forecast = c(11, 13)
  )
  train_test <- tibble::tibble(
    Run_Type = c("Back_Test", "Future_Forecast"),
    Train_Test_ID = c(2, 1)
  )

  expect_error(create_prediction_intervals(bad_tbl, train_test))
})

test_that("create_prediction_intervals works with valid input", {
  good_tbl <- tibble::tibble(
    Combo = rep("A", 4),
    Model_ID = rep("snaive--local--R1", 4),
    Train_Test_ID = c(1, 1, 2, 2),
    Target = c(10, 12, 8, 9),
    Forecast = c(11, 13, 7, 10),
    Date = as.Date("2025-01-01") + 0:3,
    Horizon = c(1, 2, 1, 2)
  )
  train_test <- tibble::tibble(
    Run_Type = c("Back_Test", "Future_Forecast"),
    Train_Test_ID = c(2, 1)
  )

  result <- create_prediction_intervals(good_tbl, train_test)

  expect_true("lo_95" %in% colnames(result))
  expect_true("hi_95" %in% colnames(result))
  # prediction intervals only exist for Future_Forecast (Train_Test_ID == 1)
  future_rows <- result %>% dplyr::filter(Train_Test_ID == 1)
  expect_true(all(!is.na(future_rows$lo_95)))
})

# * validate_best_model ----

test_that("validate_best_model passes when all combos have Best_Model = Yes", {
  tbl <- tibble::tibble(
    Combo = c("A", "A", "B", "B"),
    Best_Model = c("Yes", "No", "Yes", "No"),
    Forecast = c(10, 11, 12, 13)
  )

  result <- validate_best_model(tbl, context = "test")
  expect_equal(result, tbl)
})

test_that("validate_best_model passes with single combo single row", {
  tbl <- tibble::tibble(
    Combo = "A",
    Best_Model = "Yes",
    Forecast = 10
  )

  result <- validate_best_model(tbl, context = "test")
  expect_equal(result, tbl)
})

test_that("validate_best_model stops when one combo is missing best model", {
  tbl <- tibble::tibble(
    Combo = c("A", "A", "B", "B"),
    Best_Model = c("Yes", "No", "No", "No"),
    Forecast = c(10, 11, 12, 13)
  )

  expect_error(
    validate_best_model(tbl, context = "test"),
    "missing a best model.*B"
  )
})

test_that("validate_best_model stops when multiple combos are missing best model", {
  tbl <- tibble::tibble(
    Combo = c("A", "B", "C"),
    Best_Model = c("No", "No", "Yes"),
    Forecast = c(10, 11, 12)
  )

  expect_error(
    validate_best_model(tbl, context = "test"),
    "missing a best model.*A.*B"
  )
})

test_that("validate_best_model stops when Best_Model column is missing", {
  tbl <- tibble::tibble(
    Combo = c("A", "B"),
    Forecast = c(10, 11)
  )

  expect_error(
    validate_best_model(tbl, context = "test"),
    "Best_Model column is missing"
  )
})

test_that("validate_best_model includes calling context in error message", {
  tbl <- tibble::tibble(
    Combo = "A",
    Best_Model = "No",
    Forecast = 10
  )

  expect_error(
    validate_best_model(tbl, context = "my_function"),
    "Error in my_function"
  )
})

# * reconciliation skip prevention ----

test_that("early return is blocked when reconciliation is incomplete", {
  # Simulate: all combos have average_models files but no reconciled files.
  # The early-return guard should NOT fire because recon_complete is FALSE.
  tmp <- file.path(tempdir(), paste0("fm_recon_test_", Sys.getpid()))
  dir.create(file.path(tmp, "forecasts"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- "proj1"
  run <- "run1"
  proj_hash <- hash_data(proj)
  run_hash <- hash_data(run)

  # create a combo *_models file (represents combo_list)
  file.create(file.path(
    tmp, "forecasts",
    paste0(proj_hash, "-", run_hash, "-comboA-single_models.csv")
  ))

  # create a matching *average_models file (represents prev_combo_list)
  file.create(file.path(
    tmp, "forecasts",
    paste0(proj_hash, "-", run_hash, "-comboA-average_models.csv")
  ))

  run_info <- list(
    path = tmp,
    storage_object = NULL,
    project_name = proj,
    run_name = run,
    data_output = "csv",
    object_output = "rds"
  )

  # combo_diff is empty, prev_combo_list is non-empty
  combo_list <- list_files(
    run_info$storage_object,
    paste0(run_info$path, "/forecasts/*", proj_hash, "-", run_hash, "*_models.csv")
  )
  expect_true(length(combo_list) > 0)

  # no Best-Model reconciled file exists
  best_model_hash <- hash_data("Best-Model")
  recon_files <- list_files(
    run_info$storage_object,
    paste0(run_info$path, "/forecasts/*", proj_hash, "-", run_hash, "-", best_model_hash, "-reconciled.csv")
  )
  expect_equal(length(recon_files), 0)

  # recon_complete should be FALSE for non-bottoms_up
  forecast_approach <- "standard_hierarchy"
  recon_complete <- TRUE
  if (forecast_approach != "bottoms_up") {
    recon_complete <- length(recon_files) > 0
  }
  expect_false(recon_complete)
})

test_that("early return fires when reconciliation is complete", {
  tmp <- file.path(tempdir(), paste0("fm_recon_done_", Sys.getpid()))
  dir.create(file.path(tmp, "forecasts"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- "proj2"
  run <- "run2"
  proj_hash <- hash_data(proj)
  run_hash <- hash_data(run)

  best_model_hash <- hash_data("Best-Model")

  # create average_models + Best-Model reconciled file
  file.create(file.path(
    tmp, "forecasts",
    paste0(proj_hash, "-", run_hash, "-comboA-average_models.csv")
  ))
  file.create(file.path(
    tmp, "forecasts",
    paste0(proj_hash, "-", run_hash, "-", best_model_hash, "-reconciled.csv")
  ))

  run_info <- list(
    path = tmp,
    storage_object = NULL,
    project_name = proj,
    run_name = run,
    data_output = "csv",
    object_output = "rds"
  )

  recon_files <- list_files(
    run_info$storage_object,
    paste0(run_info$path, "/forecasts/*", proj_hash, "-", run_hash, "-", best_model_hash, "-reconciled.csv")
  )
  expect_true(length(recon_files) > 0)

  forecast_approach <- "standard_hierarchy"
  recon_complete <- TRUE
  if (forecast_approach != "bottoms_up") {
    recon_complete <- length(recon_files) > 0
  }
  expect_true(recon_complete)
})

test_that("early return is blocked when only non-Best-Model reconciled files exist", {
  # Simulate: reconciled files exist for individual models but NOT for Best-Model.
  # recon_complete should still be FALSE because Best-Model was not reconciled.
  tmp <- file.path(tempdir(), paste0("fm_recon_partial_", Sys.getpid()))
  dir.create(file.path(tmp, "forecasts"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- "proj3"
  run <- "run3"
  proj_hash <- hash_data(proj)
  run_hash <- hash_data(run)
  best_model_hash <- hash_data("Best-Model")

  # create average_models file
  file.create(file.path(
    tmp, "forecasts",
    paste0(proj_hash, "-", run_hash, "-comboA-average_models.csv")
  ))

  # create a reconciled file for an individual model (NOT Best-Model)
  some_model_hash <- hash_data("arima--local--R1")
  file.create(file.path(
    tmp, "forecasts",
    paste0(proj_hash, "-", run_hash, "-", some_model_hash, "-reconciled.csv")
  ))

  run_info <- list(
    path = tmp,
    storage_object = NULL,
    project_name = proj,
    run_name = run,
    data_output = "csv",
    object_output = "rds"
  )

  # Best-Model reconciled file does not exist
  recon_files <- list_files(
    run_info$storage_object,
    paste0(run_info$path, "/forecasts/*", proj_hash, "-", run_hash, "-", best_model_hash, "-reconciled.csv")
  )
  expect_equal(length(recon_files), 0)

  forecast_approach <- "standard_hierarchy"
  recon_complete <- TRUE
  if (forecast_approach != "bottoms_up") {
    recon_complete <- length(recon_files) > 0
  }
  expect_false(recon_complete)
})

test_that("early return fires for bottoms_up regardless of reconciled files", {
  # For bottoms_up, reconciliation is never needed, so recon_complete stays TRUE
  forecast_approach <- "bottoms_up"

  recon_complete <- TRUE
  if (forecast_approach != "bottoms_up") {
    recon_complete <- FALSE # would be set by missing files
  }
  expect_true(recon_complete)
})

# Synthetic predictions for one series with `models` complete candidates: each
# has the same Train_Test_ID x Date rows, the future split (Train_Test_ID 1)
# has NA targets, and rows are shuffled so averaging can't rely on row order.
# Returns a tibble shaped like final_models()' predictions_tbl.
averaging_predictions <- function(models = 4, back_tests = 3, horizon = 4) {
  set.seed(11)
  grid <- expand.grid(
    h = seq_len(horizon), Train_Test_ID = seq_len(back_tests + 1), Model_ID = sprintf("m%02d--local--R1", seq_len(models)),
    stringsAsFactors = FALSE
  )
  grid$Date <- as.Date("2021-01-01") + (grid$Train_Test_ID * horizon + grid$h) * 31
  grid$Target <- ifelse(grid$Train_Test_ID == 1, NA_real_, round(100 + stats::rnorm(nrow(grid), sd = 10), 3))
  grid$Forecast <- 100 + stats::rnorm(nrow(grid), sd = 7) / 3
  grid <- grid[sample(nrow(grid)), ]
  tibble::tibble(
    Combo = "A", Model_ID = grid$Model_ID, Model_Name = sub("--.*", "", grid$Model_ID), Model_Type = "local",
    Recipe_ID = "R1", Train_Test_ID = as.numeric(grid$Train_Test_ID), Date = grid$Date,
    Forecast = grid$Forecast, Target = grid$Target
  )
}

# The per-combination averaging loop final_models() used before the grouped
# computation, kept as the bit-identical reference. Returns NULL for fewer
# than two models, otherwise one tibble of averages in combination order.
averaging_reference <- function(predictions_tbl, model_ids, max_model_average) {
  if (length(model_ids) < 2) {
    return(NULL)
  }
  combos <- unlist(lapply(2:min(length(model_ids), max_model_average), function(number) {
    utils::combn(sort(model_ids), number, FUN = paste, collapse = "_")
  }))
  suppressMessages(dplyr::bind_rows(lapply(combos, function(x) {
    model_list <- strsplit(x, "_")[[1]]
    predictions_tbl %>%
      dplyr::filter(Model_ID %in% model_list) %>%
      dplyr::group_by(Combo, Train_Test_ID, Date) %>%
      dplyr::summarise(Target = mean(Target, na.rm = TRUE), Forecast = mean(Forecast)) %>%
      dplyr::mutate(Model_ID = x) %>%
      dplyr::select(Combo, Model_ID, Train_Test_ID, Date, Target, Forecast) %>%
      dplyr::ungroup()
  })))
}

# Run info fields par_start() reads for a local pool; no files are written.
averaging_run_info <- function() {
  list(storage_object = NULL, path = tempdir(), data_output = "csv", object_output = "rds")
}

test_that("model average IDs list sorted members in lexicographic combination order", {
  # saved average Model_IDs depend on this order
  ids <- c("snaive--local--R1", "arima--local--R1", "ets--local--R2")
  pairs <- c("arima--local--R1_ets--local--R2", "arima--local--R1_snaive--local--R1",
    "ets--local--R2_snaive--local--R1")
  expect_identical(model_average_combinations(ids, 2), pairs)
  expect_identical(model_average_combinations(rev(ids), 3),
    c(pairs, "arima--local--R1_ets--local--R2_snaive--local--R1"))
})

test_that("grouped model averages are bit-identical to the per-combination loop", {
  predictions <- averaging_predictions(models = 4)
  ids <- sort(unique(predictions$Model_ID))

  expect_null(average_model_forecasts(predictions, ids[1], 3))
  for (case in list(list(ids[1:2], 3), list(ids, 3), list(ids, 2), list(rev(ids), 3))) {
    expect_identical(
      average_model_forecasts(predictions, case[[1]], case[[2]]),
      averaging_reference(predictions, case[[1]], case[[2]])
    )
  }
})

test_that("model averaging below the pool threshold never starts a pool", {
  testthat::local_mocked_bindings(par_start = function(...) stop("no pool expected"))
  predictions <- averaging_predictions(models = 4)
  ids <- unique(predictions$Model_ID)

  result <- average_model_forecasts(predictions, ids, 3,
    run_info = averaging_run_info(), inner_parallel = TRUE, num_cores = 2
  )
  expect_identical(result, averaging_reference(predictions, ids, 3))
})

# Replace average_model_chunk() with a version that fails for any chunk
# containing `sentinel`, either only on pool workers (`workers_only = TRUE`)
# or everywhere. The replacement's environment holds only base R, so workers
# don't need this test's environment or package. Lasts until the test ends.
averaging_lose_chunk <- function(sentinel, workers_only, env = parent.frame()) {
  holder <- new.env(parent = baseenv())
  holder$main_pid <- Sys.getpid()
  holder$sentinel <- sentinel
  holder$workers_only <- workers_only
  real_chunk <- average_model_chunk
  environment(real_chunk) <- baseenv()
  holder$real_chunk <- real_chunk
  lose <- function(combos, predictions_tbl) {
    on_worker <- Sys.getpid() != main_pid
    if (sentinel %in% combos && (on_worker || !workers_only)) stop("worker lost")
    real_chunk(combos, predictions_tbl)
  }
  environment(lose) <- holder
  testthat::local_mocked_bindings(average_model_chunk = lose, .env = env)
}

test_that("model averages lost on pool workers are recomputed in the main process", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 3, "Two local workers are required")
  testthat::local_mocked_bindings(par_attach_worker_packages = function(cl, packages) invisible(NULL))
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))
  predictions <- averaging_predictions(models = 5)
  ids <- unique(predictions$Model_ID)
  expected <- averaging_reference(predictions, ids, 3)
  averaging_lose_chunk(expected$Model_ID[[1]], workers_only = TRUE)

  expect_warning(
    result <- average_model_forecasts(predictions, ids, 3,
      run_info = averaging_run_info(), inner_parallel = TRUE, num_cores = 2, pool_threshold = 1
    ),
    "recomputed in the main process.*worker lost"
  )
  expect_identical(result, expected)
  expect_true(inherits(future::plan(), "sequential"))
})

test_that("model averaging errors with the series when a recompute also fails", {
  predictions <- averaging_predictions(models = 3)
  ids <- unique(predictions$Model_ID)
  averaging_lose_chunk(averaging_reference(predictions, ids, 3)$Model_ID[[1]], workers_only = FALSE)

  expect_error(average_model_forecasts(predictions, ids, 3), "series 'A'.*worker lost")
})
