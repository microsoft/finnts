test_that("custom reasoning has no built-in defaults or source in its prompt", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  info$custom_foundation_suffix <- ""
  info$llm <- list(chat = function(...) custom_agent_response())
  testthat::local_mocked_bindings(
    get_foundation_model_suffix = function(...) stop("Unselected foundation probe"), load_eda_results = function(...) "Offline EDA",
    .package = "finnts"
  )
  prompt <- iterate_forecast_system_prompt(info, "combo", 0.1)
  expect_match(prompt, "business_mean", fixed = TRUE)
  expect_false(grepl("arima---|function\\(data|feature_selection=\"TRUE\"", prompt))
  result <- reason_inputs(info, "combo", 0.1,
    previous_run_results = "No Previous Runs", previous_version_results = "No Previous Runs",
    total_runs = 0
  )
  expect_identical(result$models_to_run, "business_mean")
  expect_false(result$stationary)
  info$llm <- list(chat = function(...) "{\"models_to_run\":\"business_mean\"}")
  expect_error(reason_inputs(info, "combo", 0.1,
    previous_run_results = "No Previous Runs", previous_version_results = "No Previous Runs",
    total_runs = 0
  ), class = "finnts_reason_proposal_invalid")
})

test_that("custom Agent submission uses the real pinned standard runtime", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  proposal <- validate_agent_custom_proposal(
    jsonlite::fromJSON(custom_agent_response("business_mean---meanf")), info$custom_agent_contract,
    "combo"
  )
  run <- submit_fcst_run(info, proposal, hash_data("series1"), "fixed", num_cores = 1)
  forecasts <- get_forecast_data(run)
  expect_true(nrow(forecasts) > 0)
  expect_equal(unique(forecasts$Forecast), 100)
  allowed <- c(paste0("custom-", args$custom_models$business_mean$definition$version_id, "--local--C1"), "meanf--local--R1")
  expect_true(all(unlist(strsplit(unique(forecasts$Model_ID), "_", fixed = TRUE)) %in% allowed))
  expect_true(any(forecasts$Recipe_ID == "simple_average"))
  log <- read_selection_file(run, "logs")
  expect_identical(log$custom_agent_contract_id, info$custom_agent_contract_id)
  expect_setequal(custom_run_load(run, log)$pool$selected, c("business_mean", "meanf"))
  before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
  run_again <- submit_fcst_run(info, proposal, hash_data("series1"), "fixed", num_cores = 1)
  expect_equal(get_forecast_data(run_again), forecasts)
  expect_identical(tools::md5sum(names(before)), before)
  expect_no_error(audit_agent_custom_child(info, run, hash_data("series1")))
  builtin <- validate_agent_custom_proposal(
    jsonlite::fromJSON(custom_agent_response("meanf")), info$custom_agent_contract,
    "combo"
  )
  expect_identical(builtin$models_to_run, proposal$models_to_run)
  builtin_run <- run_again
  expect_equal(unique(get_forecast_data(builtin_run)$Forecast), 100)
  expect_no_error(audit_agent_custom_child(info, builtin_run, hash_data("series1")))
  altered <- read_selection_file(builtin_run, "logs")
  altered$negative_forecast <- FALSE
  write_data(altered, NULL, builtin_run, "log", "logs")
  expect_error(audit_agent_custom_child(info, builtin_run, hash_data("series1")), "negative_forecast|controls")
  expect_error(submit_fcst_run(info, builtin, hash_data("series1"), "fixed", num_cores = 1), "negative_forecast|controls")
  log$custom_agent_contract_id <- "custom-agent-wrong"
  write_data(log, NULL, run, "log", "logs")
  expect_error(audit_agent_custom_child(info, run, hash_data("series1")), "parent identity")
})

test_that("custom execution and update guards run before expensive work", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  stripped <- info
  stripped$custom_agent_contract <- stripped$custom_agent_contract_id <- NULL
  testthat::local_mocked_bindings(
    get_eda_data = function(...) stop("Unexpected EDA"),
    eda_agent_workflow = function(...) stop("Unexpected EDA"), update_fcst_agent_workflow = function(...) stop("Unexpected update graph"),
    load_update_runs = function(...) stop("Unexpected completion scan"), .package = "finnts"
  )
  expect_error(iterate_forecast(stripped, parallel_processing = "spark"), "Unexpected EDA")
  expect_error(iterate_forecast(info, inner_parallel = TRUE), "Unexpected EDA")
  expect_error(update_forecast(stripped), "Custom Agent.*update|custom.*update")
  stripped$overwrite <- TRUE
  expect_error(initial_checks(stripped), "Custom Agent.*update|custom.*update")
})

test_that("missing parent records and custom predecessors cannot enter update recovery", {
  args <- custom_agent_inputs(withr::local_tempdir())
  predecessor <- load_agent_custom_state(do.call(set_agent_info, args))
  current <- predecessor
  current$run_id <- "builtin-current"
  current$agent_version <- 2
  current$overwrite <- TRUE
  current$custom_agent_contract <- current$custom_agent_contract_id <- NULL
  original_read <- finnts:::read_file
  testthat::local_mocked_bindings(
    load_update_runs = function(...) tibble::tibble(), list_files = function(...) c(
      "first.csv",
      "second.csv"
    ), read_file = function(run_info, ...) {
      arguments <- list(...)
      if (identical(arguments$file_list, c("first.csv", "second.csv"))) {
        return(tibble::tibble(agent_version = 1, run_id = predecessor$run_id))
      }
      original_read(run_info, ...)
    }, find_completed_previous_agent_runs = function(...) list(list(agent_info = predecessor)), get_total_combos = function(...) stop("Unexpected recovery routing"),
    .package = "finnts"
  )
  expect_error(initial_checks(current), "Custom Agent updates are unsupported")
  marker_only <- current
  marker_only$run_id <- "marker-without-record"
  marker_only$custom_agent_contract_id <- predecessor$custom_agent_contract_id
  expect_error(load_agent_custom_state(marker_only))
  expect_error(reject_agent_custom_update(marker_only))
})

test_that("custom Agent setup pins negative_forecast = FALSE into the contract", {
  args <- custom_agent_inputs(withr::local_tempdir())
  args$negative_forecast <- FALSE
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  expect_false(info$custom_agent_contract$context$negative_forecast)
  expect_identical(load_agent_custom_state(info)$custom_agent_contract_id, info$custom_agent_contract_id)
})

test_that("invalid custom setup fails before writing and metadata ownership stays exact", {
  args <- custom_agent_inputs(withr::local_tempdir())
  files <- list.files(args$project_info$path, recursive = TRUE, full.names = TRUE)
  hashes <- tools::md5sum(files)
  builtin <- args
  builtin$models_to_run <- "meanf"
  expect_error(do.call(set_agent_info, builtin), "unused argument")
  wrong <- args
  wrong$negative_forecast <- NA
  expect_error(do.call(set_agent_info, wrong), "negative_forecast")
  wrong <- args
  wrong$project_info$data_output <- "parquet"
  expect_error(do.call(set_agent_info, wrong), "CSV")
  wrong <- args
  wrong$run_local_models <- FALSE
  wrong$run_global_models <- TRUE
  expect_error(do.call(set_agent_info, wrong), "at least two series")
  expect_identical(tools::md5sum(files), hashes)
  expect_identical(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE), files)
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  path <- local_artifact_path(agent_custom_parent_info(info), "prep_models", "-custom-models", extension = "rds")
  record <- readRDS(path)
  record$agent_run_id <- "wrong-owner"
  saveRDS(record, path)
  before <- tools::md5sum(path)
  expect_error(load_agent_custom_state(info), "record identity")
  expect_identical(tools::md5sum(path), before)
})

test_that("custom candidates execute before built-ins regardless of alias ordering", {
  args <- custom_agent_inputs(withr::local_tempdir())
  args$custom_models <- list(zz_rule = custom_agent_model("zz_rule"))
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  observed <- character()
  original_fit <- tune::fit_resamples
  # Observe real fold execution without replacing fits, predictions or selection.
  local_mocked_bindings(fit_resamples = function(object, ...) {
    spec <- workflows::extract_spec_parsnip(object)
    observed <<- c(observed, if (inherits(spec, "finnts_custom")) "zz_rule" else "builtin")
    original_fit(object, ...)
  }, .package = "tune")
  inputs <- validate_agent_custom_proposal(
    jsonlite::fromJSON(custom_agent_response("meanf")), info$custom_agent_contract, "local")
  child <- submit_fcst_run(info, inputs, hash_data("series1"), "custom-first", num_cores = 1)
  expect_identical(observed, c("zz_rule", "builtin"))
  expect_equal(unique(get_forecast_data(child)$Forecast), 100)
})
