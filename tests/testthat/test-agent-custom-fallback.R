test_that("all-replaced custom series retain typed replay metadata and enter defaults", {
  fixture <- custom_update_fixture(updated = FALSE)
  local_custom_update_runtime()
  other <- fixture$args
  other$input_data$series <- "renamed"
  changed <- do.call(set_agent_info, other)
  prepared <- prepare_agent_custom_update(changed, 123)
  routed <- initial_agent_custom_update(prepared)
  expect_equal(nrow(routed$prev_best_runs_tbl), 0L)
  expect_true(all(c("combo", "model_type", "best_run_name") %in% names(routed$prev_best_runs_tbl)))
  expect_identical(routed$new_combos, hash_data("renamed"))
})

test_that("custom quality failures enter recovery but source failures stay hard", {
  fixture <- custom_update_fixture(updated = FALSE)
  local_custom_update_runtime()
  prepared <- prepare_agent_custom_update(fixture$current, 123)
  record <- read_agent_custom_update(fixture$current, TRUE)
  source <- dplyr::bind_rows(lapply(record$winners, function(row)
    row[setdiff(names(row), c("components", "selected_id"))]))
  expect_identical(check_update_failures(prepared, source, hash_data("first"),
    character(), character(), local_quality_rejected = hash_data("first")), hash_data("first"))
  local_mocked_bindings(
    update_forecast_combo = function(...) stop("Deliberate custom source failure"),
    .package = "finnts")
  expect_error(update_local_models(prepared, source, NULL, FALSE, 1, 123), "Deliberate custom source failure")
})

test_that("typed model execution and quality failures permit custom update recovery", {
  fixture <- custom_update_fixture(updated = FALSE)
  local_custom_update_runtime()
  prepared <- prepare_agent_custom_update(fixture$current, 123)
  record <- read_agent_custom_update(prepared, TRUE)
  source <- dplyr::bind_rows(lapply(record$winners, function(row)
    row[setdiff(names(row), c("components", "selected_id"))]))
  for (class in c("finnts_update_model_error", "finnts_forecast_selection_rejected")) {
    testthat::local_mocked_bindings(
      update_forecast_combo = function(...) rlang::abort("Recoverable model failure", class = class),
      .package = "finnts")
    local <- update_local_models(prepared, source, NULL, FALSE, 1, 123)
    expected <- if (class == "finnts_update_model_error") local$failed_combos else local$quality_rejected_combos
    expect_identical(expected, hash_data("first"))
    source$model_type <- "global"
    global <- update_global_models(prepared, source, NULL, FALSE, 1, 123)
    expected <- if (class == "finnts_update_model_error") global$failed_combos else global$quality_rejected_combos
    expect_identical(expected, hash_data("first"))
    source$model_type <- "local"
  }
})
test_that("custom update recovery retains failure limits and mandatory default candidates", {
  args <- custom_agent_inputs(withr::local_tempdir())
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  observed <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    resolve_combo_hashes = function(agent_info, combos) combos,
    get_foundation_model_suffix = function(...) "",
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    par_end = function(...) invisible(NULL),
    submit_fcst_run = function(agent_info, inputs, ...) {
      expect_null(agent_info$custom_agent_contract)
      expect_null(agent_info$custom_agent_update)
      observed$inputs <- inputs
      list(forecast_selection = list())
    },
    get_fcst_output = function(...) tibble::tibble(),
    calculate_fcst_metrics = function(...) 0.1,
    agent_selection_summary = function(...) list(acceptable = TRUE),
    log_best_run = function(...) invisible(NULL),
    .package = "finnts"
  )
  combo <- hash_data("series1")
  previous <- tibble::tibble(combo = "series1")
  expect_identical(check_update_failures(info, previous, combo, character(), combo), combo)
  expect_error(check_update_failures(info, tibble::tibble(combo = letters[1:20]),
    letters[1:11], character(), letters[1:11]), "exceeds.*limit")
  forecast_new_combos(info, combo, parallel_processing = NULL, inner_parallel = FALSE,
    num_cores = 1, seed = 123)
  expect_identical(observed$inputs$models_to_run[[1]], "business_mean")
  expect_true(all(c("arima", "ets", "xgboost", "glmnet") %in% observed$inputs$models_to_run))
  expect_identical(observed$inputs$recipes_to_run, c("R1", "C1"))
  expect_true(observed$inputs$stationary)
  expect_true(observed$inputs$clean_missing_values)
})

test_that("custom update provenance routes added series to default recovery", {
  local_custom_update_runtime()
  args <- custom_agent_inputs(withr::local_tempdir())
  previous <- load_agent_custom_state(do.call(set_agent_info, args))
  added <- args$input_data
  added$id <- "series2"
  args$input_data <- rbind(args$input_data, added)
  args$overwrite <- TRUE
  current <- load_agent_custom_state(do.call(set_agent_info, args))
  model_id <- paste0("custom-", args$custom_models$business_mean$definition$version_id, "--local--C1")
  best <- tibble::tibble(combo = "series1", best_run_name = "saved", model_type = "local",
    weighted_mape = 0.1, agent_run_id = previous$run_id,
    custom_agent_contract_id = previous$custom_agent_contract_id)
  testthat::local_mocked_bindings(
    load_update_runs = function(...) tibble::tibble(),
    find_completed_previous_agent_runs = function(...) list(list(agent_info = previous, best_runs_tbl = best)),
    audit_agent_custom_best = function(...) invisible(NULL),
    read_update_result = function(...) list(
      forecasts = tibble::tibble(Model_ID = model_id, Best_Model = "Yes"),
      models = tibble::tibble(Model_ID = model_id, Model_Name = "business_mean")),
    .package = "finnts"
  )
  prepared <- prepare_agent_custom_update(current, 123)
  routed <- initial_agent_custom_update(prepared)
  expect_identical(routed$prev_best_runs_tbl$combo, "series1")
  expect_identical(routed$new_combos, hash_data("series2"))
  expect_setequal(routed$current_run_combos, vapply(c("series1", "series2"), hash_data, character(1)))
  args$input_data <- added
  replaced <- do.call(set_agent_info, args)
  replacement <- initial_agent_custom_update(prepare_agent_custom_update(replaced, 123))
  expect_equal(nrow(replacement$prev_best_runs_tbl), 0L)
  expect_true(all(c("combo", "model_type", "best_run_name") %in% names(replacement$prev_best_runs_tbl)))
  expect_identical(replacement$new_combos, hash_data("series2"))
})

test_that("accepted custom defaults retain provenance and restart without refitting", {
  fixture <- custom_update_fixture(updated = FALSE)
  local_custom_update_runtime()
  args <- fixture$args
  args$input_data$series <- "new-series"
  current <- prepare_agent_custom_update(do.call(set_agent_info, args), 123)
  current$default_reforecast <- TRUE
  inputs <- validate_agent_custom_proposal(
    jsonlite::fromJSON(custom_agent_response("replay_mean")),
    current$custom_agent_contract, "local")
  combo <- hash_data("new-series")
  child <- submit_fcst_run(current, inputs, combo, "default", num_cores = 1)
  expect_identical(read_selection_file(child, "logs")[["default_reforecast_status"]], "accepted")
  log_best_run(current, child, calculate_fcst_metrics(child, get_fcst_output(child)),
    combo, check_best_run = FALSE)
  best <- load_best_agent_run(current)
  log <- read_selection_file(child, "logs")
  expect_identical(log[["custom_agent_update_id"]], current$custom_agent_update_id)
  expect_identical(best[["custom_agent_update_id"]], current$custom_agent_update_id)
  expect_identical(best[["default_reforecast_status"]], "accepted")
  forecasts <- get_fcst_output(child)
  expect_equal(unique(forecasts$Forecast[forecasts$Train_Test_ID == 1]), 101)
  expect_no_error(audit_agent_custom_best(current, best))
  local_mocked_bindings(
    train_models = function(...) stop("Unexpected accepted-default refit"),
    fit_models = function(...) stop("Unexpected accepted-default refit"),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  expect_identical(initial_agent_custom_update(current), "no updates required")
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_identical(get_best_agent_run(current)$custom_agent_update_id, current$custom_agent_update_id)
  before <- tools::md5sum(list.files(args$project_info$path,
    recursive = TRUE, full.names = TRUE))
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_identical(tools::md5sum(names(before)), before)
  wrong <- best
  wrong$best_run_name <- sub("_default$", "_foreign", wrong$best_run_name)
  expect_error(audit_agent_custom_best(current, wrong), "winner choice")
})
