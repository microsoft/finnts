test_that("global EDA reasoning uses one artifact inventory", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$external_regressors <- "price"
  artifact_test_eda(agent_info)
  tracker <- local_artifact_spies(max_listings = 1L)
  tables <- list()
  original_table <- make_pipe_table
  testthat::local_mocked_bindings(
    make_pipe_table = function(data) {
      tables[[length(tables) + 1L]] <<- data
      original_table(data)
    },
    .package = "finnts"
  )

  result <- load_eda_results(agent_info)

  expect_equal(tables, list(
    tibble::tibble(Lag = 1, Combo_Count = 2L, Combo_Percent = 100, Avg_Value = 0.1, Mean_Abs_Value = 0.4),
    tibble::tibble(Lag = 1, Combo_Count = 2L, Combo_Percent = 100, Avg_Value = 0.6, Mean_Abs_Value = 0.6),
    tibble::tibble(Stationary = c("Non-Stationary", "Stationary"), Count = c(1L, 1L), Percent = c(50, 50)),
    tibble::tibble(Lag = 1, Combo_Count = 2L, Combo_Percent = 100, Avg_ACF_Value = 0.4, Mean_Abs_ACF_Value = 0.4),
    tibble::tibble(Regressor = "price", Lag = 1, Avg_dCor = 0.5, Median_dCor = 0.5, Max_dCor = 0.75)
  ))
  expect_match(result, "Total Rows: 14", fixed = TRUE)
  expect_match(result, "Missing Count: 2", fixed = TRUE)
  expect_match(result, "Missing Percent: 12%", fixed = TRUE)
  expect_match(result, "Longest Gap: 2", fixed = TRUE)
  expect_match(result, "Outlier Count: 2", fixed = TRUE)
  expect_match(result, "Outlier Percent: 14%", fixed = TRUE)
  expect_equal(tracker$listings, 1L)
  expect_equal(tracker$directory_listings, 1L)
})

test_that("EDA consolidation reuses one discovery for every analysis type", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$external_regressors <- "price"
  artifact_test_eda(agent_info)
  testthat::local_mocked_bindings(check_agent_info = function(...) invisible(NULL), .package = "finnts")
  expected <- artifact_expected_eda()
  tracker <- local_artifact_spies(max_listings = 1L)

  save_eda_data(agent_info)
  result <- get_eda_data(agent_info) %>% dplyr::arrange(Combo, Analysis_Type, Metric)

  expect_equal(result, expected)
  expect_equal(nrow(result), 38L)
  expect_equal(anyDuplicated(result[c("Combo", "Analysis_Type", "Metric")]), 0L)
  expect_equal(tracker$listings, 1L)
  expect_equal(tracker$directory_listings, 1L)
})

for (storage_class in c("blob_container", "ms_drive")) {
  test_that(paste("provider EDA consolidation avoids input discovery for", storage_class), {
    agent_info <- artifact_test_agent(withr::local_tempdir())
    agent_info$external_regressors <- "price"
    artifact_test_eda(agent_info)
    agent_info$project_info$storage_object <- structure(list(), class = storage_class)
    original_list <- list_files
    original_read <- read_file
    original_write <- write_data
    provider_listings <- character()
    testthat::local_mocked_bindings(
      check_agent_info = function(...) invisible(NULL),
      list_files = function(storage_object, path, ...) {
        if (!is.null(storage_object)) provider_listings <<- c(provider_listings, path)
        if (grepl("/input_data/", path, fixed = TRUE)) {
          stop("Unexpected input-data listing during EDA consolidation", call. = FALSE)
        }
        original_list(NULL, path, ...)
      },
      read_file = function(run_info, ...) {
        run_info$storage_object <- NULL
        original_read(run_info, ...)
      },
      write_data = function(x, combo, run_info, ...) {
        run_info$storage_object <- NULL
        original_write(x, combo, run_info, ...)
      },
      download_file = function(...) stop("Unexpected provider download", call. = FALSE),
      .package = "finnts"
    )

    save_eda_data(agent_info)
    result <- get_eda_data(agent_info) %>% dplyr::arrange(Combo, Analysis_Type, Metric)

    expect_equal(result, artifact_expected_eda())
    expect_equal(nrow(result), 38L)
    expect_equal(anyDuplicated(result[c("Combo", "Analysis_Type", "Metric")]), 0L)
    expect_length(provider_listings, 7L)
    expect_false(any(grepl("/input_data/", provider_listings, fixed = TRUE)))
  })
}

test_that("independent EDA expectations reject a dropped-series mutation", {
  agent_info <- artifact_test_agent(withr::local_tempdir())
  agent_info$external_regressors <- "price"
  artifact_test_eda(agent_info)
  original_read <- read_file
  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    read_file = function(run_info, path = NULL, file_list = NULL, ...) {
      data <- original_read(run_info, path = path, file_list = file_list, ...)
      if (!is.null(file_list) && is.data.frame(data) && "Combo" %in% names(data)) {
        data <- dplyr::filter(data, Combo != "B")
      }
      data
    },
    .package = "finnts"
  )

  save_eda_data(agent_info)
  result <- get_eda_data(agent_info) %>% dplyr::arrange(Combo, Analysis_Type, Metric)

  expect_false("B" %in% result$Combo)
  expect_failure(expect_equal(result, artifact_expected_eda()))
})

test_that("forecast getter reads discovered condensed files without listing again", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_log(run_info, combo_variables = "ID", forecast_approach = "bottoms_up")
  artifact_test_splits(run_info)
  forecast <- artifact_test_forecast(run_info)
  write_data(forecast, "batch_1", run_info, "data", "forecasts", "-condensed")
  tracker <- local_artifact_spies(max_listings = 1L)

  result <- get_forecast_data(run_info)

  expect_equal(result$Forecast, forecast$Forecast)
  expect_identical(result$Combo, "A")
  expect_equal(tracker$listings, 1L)
})

test_that("hierarchy consumes its discovered condensed filenames directly", {
  run_info <- artifact_test_run(withr::local_tempdir())
  artifact_test_splits(run_info)
  forecast <- artifact_test_forecast(run_info)
  write_data(forecast, "batch_1", run_info, "data", "forecasts", "-condensed")
  write_data(list(nodes = 1, original_combos = "A", hts_combos = "A"),
    NULL, run_info, "object", "prep_data", "-hts_info"
  )
  tracker <- local_artifact_spies(max_listings = 1L)
  testthat::local_mocked_bindings(
    validate_best_model = function(...) stop("hierarchy inputs verified", call. = FALSE),
    .package = "finnts"
  )

  expect_error(reconcile_hierarchical_data(run_info, NULL, "standard_hierarchy",
    date_type = "month", num_cores = 1
  ), "hierarchy inputs verified")
  expect_equal(tracker$listings, 1L)
})

test_that("model preparation shares one lazily discovered recipe inventory", {
  run_info <- set_run_info(
    project_name = "artifact_prep", run_name = "test",
    path = withr::local_tempdir(), add_unique_id = FALSE
  )
  data <- tibble::tibble(
    id = "A", Date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 36),
    value = 100 + seq_len(36)
  )
  prep_data(run_info, data, combo_variables = "id", target_variable = "value",
    date_type = "month", forecast_horizon = 2, recipes_to_run = "R1"
  )
  tracker <- local_artifact_spies(max_listings = 1L)

  prep_models(run_info, models_to_run = "meanf", back_test_scenarios = 1,
    num_hyperparameters = 1, run_ensemble_models = FALSE
  )
  expect_equal(tracker$listings, 1L)
  expect_setequal(get_prepped_models(run_info)$Type,
    c("Model_Workflows", "Model_Hyperparameters", "Train_Test_Splits")
  )
  prep_models(run_info, models_to_run = "meanf", back_test_scenarios = 1,
    num_hyperparameters = 1, run_ensemble_models = FALSE
  )
  expect_equal(tracker$listings, 1L)
})

test_that("supplied aggregate inventory is filtered without rediscovery or extra recipes", {
  run_info <- artifact_test_run(withr::local_tempdir())
  paths <- character()
  for (combo in c("A", "B")) {
    for (recipe in c("R1", "R2")) {
      artifact_test_recipe(run_info, combo, recipe)
      paths <- c(paths, as.character(artifact_test_path(run_info, "prep_data", combo, paste0("-", recipe))))
    }
  }
  tracker <- local_artifact_spies()

  result <- get_recipe_data(run_info, "All-Data", recipes = "R2", file_list = paths)

  expect_identical(result$Recipe, "R2")
  expect_setequal(result$Data[[1]]$Combo, c("A", "B"))
  expect_length(tracker$reads, 1L)
  expect_equal(tracker$listings, 0L)
})