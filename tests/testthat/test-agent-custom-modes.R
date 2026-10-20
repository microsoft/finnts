# Return every fenced JSON template in a custom reasoning prompt, parsed without
# simplification so each value keeps its JSON type. Returns list() when absent.
custom_prompt_templates <- function(prompt) {
  blocks <- regmatches(prompt, gregexpr("(?s)```json\\s*\\{.*?\\}\\s*```", prompt, perl = TRUE))[[1]]
  lapply(blocks, function(block) {
    json <- sub("\\s*```$", "", sub("^```json\\s*", "", block, perl = TRUE), perl = TRUE)
    jsonlite::fromJSON(json, simplifyVector = FALSE)
  })
}

# Fill a typed template as an obedient model would: first listed alternatives,
# FALSE switches, one candidate model and brief reasoning. Returns a passive
# proposal list for the real validators; it performs no validation itself.
custom_prompt_instance <- function(template, model) {
  instance <- lapply(template, function(value) strsplit(value, "|", fixed = TRUE)[[1]][[1]])
  instance[c("clean_missing_values", "clean_outliers", "stationary", "feature_selection",
    "multistep_horizon")] <- "FALSE"
  instance$models_to_run <- model
  instance$reasoning <- "Offline typed-template proposal."
  instance
}

test_that("local built-in-only children skip global-only custom envelopes", {
  args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  args$run_global_models <- TRUE
  args$custom_models <- list(business_mean = custom_agent_model(modes = "global"))
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  proposal <- validate_agent_custom_proposal(
    jsonlite::fromJSON(custom_agent_response("meanf")), info$custom_agent_contract, "combo"
  )
  expect_identical(proposal$models_to_run, "meanf")
  expect_identical(proposal$recipes_to_run, "R1")
  run <- submit_fcst_run(info, proposal, hash_data("series1"), "builtin", num_cores = 1)
  forecasts <- get_forecast_data(run)
  expect_true(nrow(forecasts) > 0)
  expect_equal(unique(forecasts$Forecast), 100)
  expect_true(all(unlist(strsplit(unique(forecasts$Model_ID), "_", fixed = TRUE)) %in% "meanf--local--R1"))
  log <- read_selection_file(run, "logs")
  expect_identical(log$custom_agent_contract_id, info$custom_agent_contract_id)
  expect_null(custom_run_load(run, log))
  expect_no_error(audit_agent_custom_child(info, run, hash_data("series1")))
  before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
  run_again <- submit_fcst_run(info, proposal, hash_data("series1"), "builtin", num_cores = 1)
  expect_equal(get_forecast_data(run_again), forecasts)
  expect_identical(tools::md5sum(names(before)), before)
})

test_that("global built-in-only children prepare without local-only custom envelopes", {
  args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  args$run_global_models <- TRUE
  args$custom_models <- list(business_mean = custom_agent_model(modes = "local"))
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  contract <- info$custom_agent_contract
  candidates <- agent_custom_candidates(contract, NULL, "")$models
  builtin <- setdiff(candidates, names(contract$envelopes))
  expect_true(length(builtin) > 0L)
  expect_true("xgboost" %in% builtin)
  expect_false("business_mean" %in% candidates)
  proposal <- validate_agent_custom_proposal(
    jsonlite::fromJSON(custom_agent_response("xgboost")), contract, NULL
  )
  captured <- new.env(parent = emptyenv())
  captured$called <- FALSE
  testthat::local_mocked_bindings(
    prep_models = function(run_info, ..., models_to_run = NULL, custom_models = NULL) {
      captured$called <- TRUE
      captured$models_to_run <- models_to_run
      captured$custom_models <- custom_models
      stop("prep_models captured")
    },
    .package = "finnts"
  )
  expect_error(submit_fcst_run(info, proposal, NULL, "global", num_cores = 1), "prep_models captured")
  expect_true(captured$called)
  expect_identical(captured$models_to_run, proposal$models_to_run)
  expect_null(captured$custom_models)
})

test_that("custom reasoning prompts show one typed proposal template per mode", {
  args <- custom_agent_inputs(withr::local_tempdir(), series = 2L)
  args$run_global_models <- TRUE
  info <- load_agent_custom_state(do.call(set_agent_info, args))
  info$custom_foundation_suffix <- ""
  testthat::local_mocked_bindings(
    get_foundation_model_suffix = function(...) stop("Unselected foundation probe"),
    load_eda_results = function(...) "Offline EDA",
    .package = "finnts"
  )
  fields <- c("models_to_run", "external_regressors", "clean_missing_values", "clean_outliers",
    "forecast_approach", "stationary", "feature_selection", "multistep_horizon", "seasonal_period",
    "recipes_to_run", "lag_periods", "rolling_window_periods", "reasoning")
  switches <- c("clean_missing_values", "clean_outliers", "stationary", "feature_selection", "multistep_horizon")
  for (combo in list("combo", NULL)) {
    prompt <- iterate_forecast_system_prompt(info, combo, 0.1)
    templates <- custom_prompt_templates(prompt)
    expect_length(templates, 1L)
    template <- if (length(templates)) templates[[1L]] else list()
    expect_named(template, fields)
    for (field in switches) expect_identical(template[[field]], "TRUE|FALSE")
    expect_identical(template$forecast_approach, "bottoms_up")
    expect_identical(template$recipes_to_run, "R1")
    expect_identical(template$external_regressors, "NULL")
    expect_identical(substr(c(template$lag_periods, template$rolling_window_periods), 1L, 5L), c("NULL|", "NULL|"))
    if (is.null(combo)) {
      expect_identical(template$seasonal_period, "NULL")
    } else {
      expect_identical(substr(template$seasonal_period, 1L, 5L), "NULL|")
    }
    expect_false(grepl("arima---|function\\(data|feature_selection=\"TRUE\"", prompt))
    model <- agent_custom_candidates(info$custom_agent_contract, combo, "")$models[[1L]]
    expect_no_error(validate_agent_custom_proposal(
      custom_prompt_instance(template, model), info$custom_agent_contract, combo
    ))
  }
  regressors <- c("Driver", "Other")
  contract <- resolve_agent_custom_models(list(
    project_info = list(date_type = "month", storage_object = NULL, object_output = "rds", data_output = "csv"),
    forecast_horizon = 2, external_regressors = regressors, run_local_models = TRUE, run_global_models = TRUE,
    negative_forecast = TRUE, forecast_approach = "bottoms_up", allow_hierarchical_forecast = FALSE
  ), NULL, list(business_mean = custom_agent_model()))
  prompt <- agent_custom_prompt(list(
    custom_agent_contract = contract, custom_foundation_suffix = "", external_regressors = regressors
  ), "combo", 0.1)
  templates <- custom_prompt_templates(prompt)
  template <- if (length(templates)) templates[[1L]] else list()
  expect_identical(substr(template$external_regressors, 1L, 5L), "NULL|")
  instance <- custom_prompt_instance(template, agent_custom_candidates(contract, "combo", "")$models[[1L]])
  instance$external_regressors <- "Driver"
  expect_no_error(validate_agent_custom_proposal(instance, contract, "combo"))
})
