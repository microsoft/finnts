# Coordinating (driver) code must not look up a per-series output by exact name
# before parallel workers write it. FUSE mounts such as Synapse /synfs can cache
# that "not found" answer and hide the file for the rest of the session.

# Record exact-path lookups made inside custom_run_audit() for paths that do not
# exist at lookup time. Returns an environment whose `missing` field accumulates
# those paths. Worker reads and other workflow lookups are deliberately ignored.
local_audit_missing_lookups <- function(.env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$active <- FALSE
  state$missing <- character()
  audit <- custom_run_audit
  file_info <- fs::file_info
  testthat::local_mocked_bindings(custom_run_audit = function(...) {
    state$active <- TRUE
    on.exit(state$active <- FALSE)
    audit(...)
  }, .package = "finnts", .env = .env)
  testthat::local_mocked_bindings(file_info = function(path, ...) {
    if (state$active) {
      paths <- as.character(path)
      state$missing <- c(state$missing, paths[!file.exists(paths)])
    }
    file_info(path, ...)
  }, .package = "fs", .env = .env)
  state
}

# Prepare custom-model workflows in a fresh standard run rooted at `path`.
# Returns the run info; writes only the normal prep_models artifacts.
prepare_custom_lookup_run <- function(path, model) {
  info <- standard_run(path)
  prep_models(info,
    models_to_run = c("business_mean", "meanf"), custom_models = list(business_mean = model),
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1
  )
  info
}

# Normalize artifact paths to the forward-slash form recorded by the I/O spies.
spy_path <- function(path) gsub("\\", "/", as.character(path), fixed = TRUE)

test_that("custom runs never probe per-series outputs before workers write them", {
  info <- prepare_custom_lookup_run(withr::local_tempdir(), standard_custom_model())
  lookups <- local_audit_missing_lookups()

  train_models(info, run_global_models = TRUE, run_local_models = TRUE, negative_forecast = TRUE)
  ensemble_models(info)
  final_models(info)

  expect_identical(lookups$missing, character())
  combos <- vapply(c("A", "B"), hash_data, character(1))
  expect_true(all(file.exists(local_artifact_path(info, "forecasts", "-average_models", combos))))
  forecast <- get_forecast_data(info)
  expect_setequal(forecast$Combo[forecast$Best_Model == "Yes"], c("A", "B"))
  expect_true("business_mean" %in% forecast$Model_Name)
})

test_that("custom restarts audit and reuse only completed series outputs", {
  model <- standard_custom_model()
  full <- prepare_custom_lookup_run(withr::local_tempdir(), model)
  partial <- prepare_custom_lookup_run(withr::local_tempdir(), model)
  train_models(full, run_global_models = FALSE, run_local_models = TRUE)
  combos <- vapply(c("A", "B"), hash_data, character(1))
  done <- c(
    local_artifact_path(full, "forecasts", "-single_models", combos[[1]]),
    local_artifact_path(full, "models", "-single_models", combos[[1]], "rds")
  )
  copied <- fs::path(partial$path, fs::path_rel(done, full$path))
  fs::file_copy(done, copied)
  before <- tools::md5sum(copied)
  lookups <- local_audit_missing_lookups()

  train_models(partial, run_global_models = FALSE, run_local_models = TRUE)

  expect_identical(lookups$missing, character())
  expect_identical(tools::md5sum(copied), before)
  b_forecast <- function(info) {
    read_exact_artifact(info, local_artifact_path(info, "forecasts", "-single_models", combos[[2]]))
  }
  expect_equal(b_forecast(partial), b_forecast(full))
  final_models(full, average_models = FALSE)
  final_models(partial, average_models = FALSE)
  expect_equal(get_forecast_data(partial), get_forecast_data(full))

  stale <- read_exact_artifact(full, done[[1]])
  stale$Model_ID[stale$Model_Name == "business_mean"] <- paste0("custom-", standard_custom_model(offset = 1)$definition$version_id, "--local--C1")
  tampered <- prepare_custom_lookup_run(withr::local_tempdir(), model)
  write_data(stale, "A", tampered, "data", "forecasts", "-single_models")
  expect_error(train_models(tampered, run_global_models = FALSE, run_local_models = TRUE), "identity|pool")
})

test_that("a saved custom fit without its forecast is retrained instead of audited", {
  model <- standard_custom_model()
  trained <- prepare_custom_lookup_run(withr::local_tempdir(), model)
  train_models(trained, run_global_models = FALSE, run_local_models = TRUE)
  fit_path <- local_artifact_path(trained, "models", "-single_models", hash_data("A"), "rds")
  fitted <- read_exact_artifact(trained, fit_path, return_type = "object")
  custom_index <- which(fitted$Model_Name == "business_mean")
  fitted$Model_Fit[[custom_index]]$fit$fit$fit$context$forecast_horizon <- 1
  interrupted <- prepare_custom_lookup_run(withr::local_tempdir(), model)
  write_data(fitted, "A", interrupted, "object", "models", "-single_models")
  orphan <- local_artifact_path(interrupted, "models", "-single_models", hash_data("A"), "rds")
  before <- tools::md5sum(orphan)

  expect_no_error(train_models(interrupted, run_global_models = FALSE, run_local_models = TRUE))
  expect_false(identical(tools::md5sum(orphan), before))
  retrained <- read_exact_artifact(interrupted, orphan, return_type = "object")
  expect_equal(retrained$Model_ID, read_exact_artifact(trained, fit_path, return_type = "object")$Model_ID)
})

test_that("custom audits read only listed outputs and keep required errors", {
  info <- prepare_custom_lookup_run(withr::local_tempdir(), standard_custom_model())
  train_models(info, run_global_models = FALSE, run_local_models = TRUE)
  custom <- custom_run_read(info, TRUE)
  combos <- vapply(c("A", "B"), hash_data, character(1))
  all_listed <- fs::path_file(fs::dir_ls(fs::path(info$path, "forecasts")))
  expected <- custom_run_audit(info, custom, TRUE, FALSE, combos, TRUE)
  listed <- fs::path_file(local_artifact_path(info, "forecasts", "-single_models", combos[[1]]))
  unlisted <- spy_path(c(
    local_artifact_path(info, "forecasts", "-single_models", combos[[2]]),
    local_artifact_path(info, "models", "-single_models", combos[[2]], "rds")
  ))
  tracker <- local_artifact_spies()

  saved <- custom_run_audit(info, custom, TRUE, FALSE, combos, present = listed)

  expect_true(is.data.frame(saved[[combos[[1]]]]$single))
  expect_null(saved[[combos[[2]]]]$single)
  expect_false(any(unlisted %in% c(tracker$metadata_paths, tracker$payload_paths)))
  expect_true(spy_path(local_artifact_path(info, "models", "-single_models", combos[[1]], "rds")) %in%
    tracker$payload_paths)
  expect_error(custom_run_audit(info, custom, TRUE, FALSE, combos, complete = TRUE, present = listed),
    "Missing required Finn artifact")
  expect_equal(custom_run_audit(info, custom, TRUE, FALSE, combos, complete = TRUE, present = all_listed), expected)
  expect_equal(tracker$listings, 0L)
})

test_that("hierarchical final_models finds Best-Model from its single listing", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_completed_final_outputs(run_info)
  best <- spy_path(local_artifact_path(run_info, "forecasts", "-reconciled", hash_data("Best-Model")))
  tracker <- local_artifact_spies(max_listings = 1L)
  probed <- NA
  testthat::local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    reconcile_hierarchical_data = function(...) {
      probed <<- best %in% tracker$metadata_paths
      stop("reconciliation dispatched", call. = FALSE)
    },
    .package = "finnts"
  )

  expect_error(final_models(run_info), "reconciliation dispatched")
  expect_false(probed)
  expect_equal(tracker$listings, 1L)
})

test_that("global best-run promotion lists records once instead of probing each series", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  write_data(tibble::tibble(combo = "A", best_run_name = "candidate", weighted_mape = 0.1),
    "A", project_info, "log", "logs", "-agent_best_run")
  missing <- spy_path(fs::path(project_info$path, "logs", paste0(
    hash_data(project_info$project_name), "-", hash_data(project_info$run_name), "-",
    hash_data("B"), "-agent_best_run.csv"
  )))
  tracker <- local_artifact_spies(max_listings = 1L)

  records <- read_agent_best_run_records(project_info, c("A", "B"))

  expect_identical(names(records), c("A", "B"))
  expect_equal(nrow(records$A), 1L)
  expect_identical(as.character(records$A$best_run_name), "candidate")
  expect_equal(nrow(records$B), 0L)
  expect_false(missing %in% c(tracker$metadata_paths, tracker$payload_paths))
  expect_equal(tracker$listings, 1L)
})
