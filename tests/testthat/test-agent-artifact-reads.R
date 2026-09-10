test_that("local EDA reads exact artifacts with independently expected metrics", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$external_regressors <- "price"
  artifact_test_eda(agent_info)
  tracker <- local_artifact_spies()
  tables <- list()
  original_table <- make_pipe_table
  testthat::local_mocked_bindings(
    make_pipe_table = function(data) {
      tables[[length(tables) + 1L]] <<- data
      original_table(data)
    },
    .package = "finnts"
  )

  result <- load_eda_results(agent_info, hash_data("A"))

  expect_equal(tables, list(
    tibble::tibble(Lag = 1, Value = 0.5),
    tibble::tibble(Lag = 1, Value = 0.4),
    tibble::tibble(stationary_adf = TRUE, stationary_kpss = TRUE),
    tibble::tibble(Lag = 1, Value = 0.2),
    tibble::tibble(Regressor = "price", Lag = 1, dCor = 0.25)
  ))
  expect_match(result, "price")
  expect_match(result, "None observed")
  expect_match(result, "Missing Count: 0", fixed = TRUE)
  expect_match(result, "Outlier Count: 0", fixed = TRUE)
  expect_equal(tracker$listings, 0L)
  expect_equal(tracker$directory_listings, 0L)
  expect_length(tracker$reads, 9L)
})

test_that("local EDA for a different combo cannot reuse the first series metrics", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$external_regressors <- "price"
  artifact_test_eda(agent_info)
  tracker <- local_artifact_spies()
  tables <- list()
  original_table <- make_pipe_table
  testthat::local_mocked_bindings(
    make_pipe_table = function(data) {
      tables[[length(tables) + 1L]] <<- data
      original_table(data)
    },
    .package = "finnts"
  )

  result <- load_eda_results(agent_info, hash_data("B"))

  expect_equal(tables, list(
    tibble::tibble(Lag = 1, Value = -0.3),
    tibble::tibble(Lag = 1, Value = 0.8),
    tibble::tibble(stationary_adf = FALSE, stationary_kpss = TRUE),
    tibble::tibble(Lag = 1, Value = 0.6),
    tibble::tibble(Regressor = "price", Lag = 1, dCor = 0.75)
  ))
  expect_match(result, "Missing Count: 2", fixed = TRUE)
  expect_match(result, "Missing Percent: 25%", fixed = TRUE)
  expect_match(result, "Outlier Count: 2", fixed = TRUE)
  expect_match(result, "First Outlier Date: 2020-03-01", fixed = TRUE)
  expect_match(result, "Last Outlier Date: 2020-06-01", fixed = TRUE)
  expect_equal(tracker$directory_listings, 0L)
})

test_that("local EDA does not require regressor artifacts when regressors are disabled", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  artifact_test_eda(agent_info, omit = "xreg_scan")
  tracker <- local_artifact_spies()

  result <- load_eda_results(agent_info, hash_data("A"))

  expect_match(result, "No external regressors")
  expect_equal(tracker$listings, 0L)
  expect_length(tracker$reads, 8L)
})

test_that("missing required local EDA is not mistaken for another combo", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  artifact_test_eda(agent_info, combos = "B")
  tracker <- local_artifact_spies()

  expect_error(load_eda_results(agent_info, hash_data("A")),
    "Missing required Finn artifact.*acf"
  )
  expect_equal(tracker$listings, 0L)
})

test_that("singleton EDA caches do not enumerate input or EDA folders", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$project_info$combo_variables <- c("Country", "Product")
  artifact_test_eda(agent_info)
  tracker <- local_artifact_spies()

  expect_match(data_profile(agent_info), "already exists")
  expect_match(hierarchy_detect(agent_info), "already exists")
  expect_equal(tracker$listings, 0L)
})

test_that("combo resolution reads only requested inputs and retains missing fallback", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  artifact_test_eda(agent_info)
  missing_hash <- hash_data("missing")
  tracker <- local_artifact_spies()

  result <- resolve_combo_hashes(agent_info, c(hash_data("A"), missing_hash))

  expect_identical(result, c("A", missing_hash))
  expect_equal(tracker$listings, 0L)
  expect_length(tracker$reads, 1L)
})

test_that("local submission reads its exact input before starting modeling", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  artifact_test_eda(agent_info)
  tracker <- local_artifact_spies()
  testthat::local_mocked_bindings(
    set_run_info = function(...) stop("submission input verified", call. = FALSE),
    .package = "finnts"
  )

  expect_error(submit_fcst_run(agent_info, list(), hash_data("A"), "test"),
    "submission input verified"
  )
  expect_equal(tracker$listings, 0L)
  expect_length(tracker$reads, 1L)
  expect_equal(tracker$reads[[1]]$file_list,
    as.character(artifact_test_path(agent_info$project_info, "input_data", "A"))
  )
})

test_that("reconciliation reads the selected run split by exact path", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  project_info <- agent_info$project_info
  selected <- project_info
  selected$project_name <- paste0(project_info$project_name, "_", hash_data("A"))
  selected$run_name <- "selected"
  write_data(tibble::tibble(Run_Type = "Future_Forecast", Train_Test_ID = 1),
    NULL, selected, "data", "prep_models", "-train_test_split"
  )
  tracker <- local_artifact_spies()
  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    get_best_agent_run = function(...) tibble::tibble(
      negative_forecast = FALSE, model_type = "local", combo = "A", best_run_name = "selected"
    ),
    load_agent_forecast = function(...) stop("reconciliation split verified", call. = FALSE),
    .package = "finnts"
  )

  expect_error(reconcile_agent_forecast(agent_info, project_info),
    "reconciliation split verified"
  )
  expect_equal(tracker$listings, 0L)
  expect_equal(tracker$reads[[1]]$file_list,
    as.character(artifact_test_path(selected, "prep_models", suffix = "-train_test_split"))
  )
})

test_that("model summaries consolidate only expected combo artifacts", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  best_runs <- artifact_test_selected_models(agent_info)
  local_artifact_summary_mocks(best_runs)
  write_data(tibble::tibble(Combo = "unrelated", Best_Model = "Yes"), "unrelated",
    agent_info$project_info, "data", "models", "-model_summary"
  )
  tracker <- local_artifact_spies()

  summarize_models(agent_info)
  result <- get_summarized_models(agent_info)

  expect_setequal(result$Combo, c("A", "B"))
  expect_true(all(result$Best_Model == "Yes"))
  expect_equal(tracker$listings, 0L)
})

test_that("model summary consolidation fails on a missing expected write", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  best_runs <- artifact_test_selected_models(agent_info)
  local_artifact_summary_mocks(best_runs)
  original_write <- write_data
  testthat::local_mocked_bindings(
    write_data = function(x, combo, run_info, output_type, folder = NULL, suffix = NULL) {
      if (identical(combo, "B") && identical(suffix, "-model_summary")) return(invisible(NULL))
      original_write(x, combo, run_info, output_type, folder, suffix)
    },
    .package = "finnts"
  )
  tracker <- local_artifact_spies()

  expect_error(summarize_models(agent_info), "Missing required Finn artifact.*model_summary")
  expect_equal(tracker$listings, 0L)
})

test_that("required local EDA propagates CSV failures after validation", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  artifact_test_eda(agent_info)
  original_vroom <- vroom::vroom
  original_info <- file.info
  testthat::local_mocked_bindings(
    vroom = function(file, ...) {
      if (any(endsWith(as.character(file), "-acf.csv"))) {
        stop("injected EDA read failure", call. = FALSE)
      }
      original_vroom(file, ...)
    },
    .package = "vroom"
  )
  testthat::local_mocked_bindings(
    file.info = function(path, ...) {
      if (any(endsWith(as.character(path), "-acf.csv"))) return(data.frame(size = NA_real_))
      original_info(path, ...)
    },
    .package = "base"
  )

  expect_error(load_eda_results(agent_info, hash_data("A")),
    "Cannot read Finn CSV artifact.*injected EDA read failure"
  )
})

test_that("reconciliation cannot proceed after an unreadable selected CSV split", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  selected <- agent_info$project_info
  selected$project_name <- paste0(selected$project_name, "_", hash_data("A"))
  selected$run_name <- "selected"
  artifact_test_splits(selected)
  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    get_best_agent_run = function(...) tibble::tibble(
      negative_forecast = FALSE, model_type = "local", combo = "A", best_run_name = "selected"
    ),
    load_agent_forecast = function(...) stop("unreadable split was accepted", call. = FALSE),
    .package = "finnts"
  )
  testthat::local_mocked_bindings(
    vroom = function(...) stop("injected split read failure", call. = FALSE),
    .package = "vroom"
  )
  testthat::local_mocked_bindings(
    file.info = function(...) data.frame(size = NA_real_),
    .package = "base"
  )

  expect_error(reconcile_agent_forecast(agent_info, agent_info$project_info),
    "Cannot read Finn CSV artifact.*injected split read failure"
  )
})