
# Return passive arguments for an independently specified per-node mean rule.
# NULL approach retains the legacy contract; no source executes in construction.
hierarchy_definition_args <- function(approach = "standard_hierarchy") {
  requirements <- list(predictors = character(), recipes = "R1", target_scale = "original",
    date_types = "month", forecast_horizon = 1:3, missing_data = "error")
  if (!is.null(approach)) requirements$hierarchy <- list(forecast_approach = approach,
    application = "each_prepared_node", reconciliation = "finnts_reconciliation")
  list(name = "node_rule", instructions = "Use each node's historical mean plus ten.",
    interpretation = "Fit nodes independently; reconcile base forecasts to the bottom level.",
    model_type = "local", source = c(
      fit = "function(data, context, parameters) { stop('inert contract fixture') }",
      predict = "function(object, new_data, context) { stop('inert contract fixture') }"),
    requirements = requirements, fixed_parameters = list(offset = 10))
}

# Build an explicitly synthetic trusted attestation. Executable fixtures use a
# fixed mean-plus-ten rule; consent is test data, never claimed human approval.
# Executable cases opt into the temporal runtime; inert cases retain schema 2.
hierarchy_model <- function(approach = "standard_hierarchy", executable = FALSE) {
  args <- hierarchy_definition_args(approach)
  if (executable) args$requirements$runtime <- list(version = 1L, prediction_scope = "complete_horizon")
  if (executable) args$source <- c(
    fit = "function(data, context, parameters) { stopifnot(length(unique(data$Combo)) == 1L, max(data$Date) == context$cutoff); list(value = mean(data$Target) + parameters$offset, pid = Sys.getpid(), namespace = normalizePath(getNamespaceInfo(asNamespace('finnts'), 'path'))) }",
    predict = "function(object, new_data, context) { stopifnot(!'Target' %in% names(new_data)); data.frame(.finnts_row = new_data$.finnts_row, .pred = rep(object$value, nrow(new_data))) }")
  definition <- do.call(new_custom_model_definition, args)
  structure(list(schema_version = 1L, definition = definition,
    validation = list(version_id = definition$version_id, technical_passed = TRUE,
      checks = list(arithmetic = "Synthetic passive fixture")),
    approval = list(version_id = definition$version_id, intent_confirmed = TRUE, allow_code = TRUE)),
    class = "finnts_custom_model")
}

# Build a balanced small panel; standard leaves have unique labels under their
# parent, while grouped leaves cross two dimensions. Values are deterministic.
hierarchy_input <- function(approach = "standard_hierarchy", date_type = "month") {
  dates <- seq(as.Date("2020-01-01"), by = date_type, length.out = 24)
  data <- expand.grid(Date = dates, Region = c("North", "South"), Product = c("A", "B"), stringsAsFactors = FALSE)
  if (approach == "standard_hierarchy") data$Product <- paste(data$Region, data$Product, sep = "_")
  data$value <- 100 + match(data$Date, dates) + 10 * match(data$Product, unique(data$Product))
  data
}

# Return explicit public forecast controls for the narrow mixed hierarchy profile.
# The caller owns the temporary root; neither helper nor fixture calls a provider.
hierarchy_forecast_args <- function(path, approach = "standard_hierarchy") {
  info <- set_run_info(project_name = "hierarchy-forecast", run_name = "test", path = path,
    data_output = "csv", object_output = "rds", add_unique_id = FALSE)
  list(run_info = info, input_data = hierarchy_input(approach), combo_variables = c("Region", "Product"),
    target_variable = "value", date_type = "month", forecast_horizon = 2, forecast_approach = approach,
    models_to_run = c("node_rule", "meanf"), custom_models = list(node_rule = hierarchy_model(approach, TRUE)),
    recipes_to_run = "R1", stationary = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE,
    run_global_models = FALSE, run_local_models = TRUE, run_ensemble_models = FALSE,
    negative_forecast = TRUE, average_models = FALSE, weekly_to_daily = FALSE,
    back_test_scenarios = 1, num_cores = 1, return_data = FALSE)
}

# Exercise one complete mixed hierarchy forecast and restart, retaining exact
# arithmetic, identity, coverage and installed-worker assertions for its approach.
hierarchy_expect_forecast <- function(approach) {
  args <- hierarchy_forecast_args(withr::local_tempdir(), approach)
  do.call(forecast_time_series, args)
  info <- args$run_info
  result <- get_forecast_data(info)
  version <- args$custom_models$node_rule$definition$version_id
  expect_true("node_rule" %in% result$Model_Name, info = approach)
  expect_true(paste0("custom-", version, "--local--R1") %in% result$Model_ID)
  expect_true(all(is.finite(result$Forecast)))
  expect_equal(length(unique(result$Combo[result$Best_Model == "Yes"])), 4L)
  metadata <- read_exact_artifact(info, local_artifact_path(info, "prep_data", "-hts_info", extension = "rds"), return_type = "object")
  base <- read_exact_artifact(info, local_artifact_path(info, "forecasts", "-single_models",
    vapply(metadata$hts_combos, hash_data, character(1))))
  expect_setequal(unique(base$Model_Name), c("node_rule", "meanf"))
  expect_setequal(unique(base$Combo[base$Model_Name == "node_rule"]), metadata$hts_combos)
  files <- list.files(file.path(info$path, "forecasts"), full.names = TRUE)
  before <- tools::md5sum(files)
  expect_error(final_models(info, average_models = TRUE), "average_models = FALSE")
  expect_identical(tools::md5sum(files), before)
  local({
    local_mocked_bindings(custom_model_functions = function(...) stop("Unexpected source execution on restart"), .package = "finnts")
    do.call(forecast_time_series, args)
  })
  expect_identical(tools::md5sum(files), before)
  namespace <- getNamespaceInfo(asNamespace("finnts"), "path")
  if (approach == "grouped_hierarchy" && file.exists(file.path(namespace, "Meta", "package.rds"))) {
    withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
    parallel_args <- args
    parallel_args$run_info <- set_run_info(project_name = info$project_name, run_name = info$run_name,
      path = withr::local_tempdir(), data_output = "csv", object_output = "rds", add_unique_id = FALSE)
    parallel_args$parallel_processing <- "local_machine"
    parallel_args$num_cores <- 2
    do.call(forecast_time_series, parallel_args)
    parallel_result <- get_forecast_data(parallel_args$run_info)
    columns <- c("Combo", "Model_ID", "Model_Name", "Train_Test_ID", "Date", "Target", "Forecast")
    expected <- result[do.call(order, result[c("Combo", "Model_ID", "Train_Test_ID", "Date")]), columns]
    actual <- parallel_result[do.call(order, parallel_result[c("Combo", "Model_ID", "Train_Test_ID", "Date")]), columns]
    expect_equal(as.data.frame(actual), as.data.frame(expected), tolerance = 1e-8)
    fit_paths <- local_artifact_path(parallel_args$run_info, "models",
      "-single_models", vapply(metadata$hts_combos, hash_data, character(1)), "rds")
    fits <- dplyr::bind_rows(lapply(fit_paths, function(path) {
      read_exact_artifact(parallel_args$run_info, path, return_type = "object")
    }))
    states <- lapply(fits$Model_Fit[fits$Model_Name == "node_rule"], function(workflow) workflows::extract_fit_parsnip(workflow)$fit$state)
    pids <- vapply(states, function(state) state$pid, integer(1))
    expect_length(unique(pids), 2L)
    expect_false(Sys.getpid() %in% pids)
    expect_true(all(vapply(states, function(state) identical(state$namespace, normalizePath(namespace)), logical(1))))
  }
}
