
test_that("explicit per-node hierarchy capabilities are version-bound and inert", {
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    args <- hierarchy_definition_args(approach)
    definition <- do.call(new_custom_model_definition, args)
    expect_identical(definition$schema_version, 2L)
    expect_identical(definition$requirements$hierarchy, args$requirements$hierarchy)
    expect_identical(definition$source, args$source)
    expect_identical(custom_model_current_definition(definition), definition)
  }
})

test_that("legacy definitions retain their original schema without hierarchy permission", {
  definition <- do.call(new_custom_model_definition, hierarchy_definition_args(NULL))
  expect_identical(definition$schema_version, 1L)
  expect_identical(definition$version_id, "70e432a0c3182b6f0228f35989c069a937c37ab5859be264abe6c4645e1a2d85")
  expect_null(definition$requirements$hierarchy)
  expect_identical(custom_model_current_definition(definition), definition)
})

test_that("hierarchy declarations are canonical and cannot widen capabilities", {
  definition <- do.call(new_custom_model_definition, hierarchy_definition_args())
  grouped <- do.call(new_custom_model_definition, hierarchy_definition_args("grouped_hierarchy"))
  expect_false(identical(definition$version_id, grouped$version_id))
  reordered <- definition
  reordered$requirements$hierarchy <- rev(reordered$requirements$hierarchy)
  expect_identical(custom_model_definition_digest(reordered), definition$version_id)
  for (field in c("forecast_approach", "application", "reconciliation")) {
    invalid <- hierarchy_definition_args()
    invalid$requirements$hierarchy[[field]] <- "unsupported"
    expect_error(do.call(new_custom_model_definition, invalid), "hierarchy")
  }
  for (field in c("model_type", "recipes", "target_scale", "predictors")) {
    invalid <- hierarchy_definition_args()
    if (field == "model_type") invalid$model_type <- c("local", "global") else
      invalid$requirements[[field]] <- switch(field, recipes = "R2", target_scale = "prepared", predictors = "promotion")
    expect_error(do.call(new_custom_model_definition, invalid), "hierarchy requires local")
  }
  invalid <- definition
  invalid$schema_version <- 1L
  expect_error(validate_custom_model_definition(invalid), "requirements")
})

test_that("hierarchy definitions round-trip through the unchanged passive registry", {
  info <- list(project_name = "hierarchy-registry", run_name = "test", path = withr::local_tempdir(),
    storage_object = NULL, object_output = "rds", data_output = "csv")
  definition <- do.call(new_custom_model_definition, hierarchy_definition_args())
  reference <- write_custom_model_definition(definition, info)
  expect_identical(reference$registry_schema_version, 1L)
  expect_identical(read_custom_model_definition(reference, info), definition)
  expect_identical(write_custom_model_definition(definition, info), reference)
})



test_that("hierarchy preflight requires an explicitly mixed shared pool before writes", {
  info <- list(project_name = "hierarchy-preflight", run_name = "test", path = withr::local_tempdir(),
    storage_object = NULL, data_output = "csv", object_output = "rds")
  args <- list(run_info = info, input_data = hierarchy_input(), combo_variables = c("Region", "Product"),
    target_variable = "value", date_type = "month", forecast_horizon = 2,
    forecast_approach = "standard_hierarchy", recipes_to_run = "R1", stationary = FALSE,
    clean_missing_values = FALSE, clean_outliers = FALSE, negative_forecast = TRUE,
    run_global_models = FALSE, run_local_models = TRUE, run_ensemble_models = FALSE,
    average_models = FALSE, weekly_to_daily = FALSE, models_to_run = c("node_rule", "meanf"),
    custom_models = list(node_rule = hierarchy_model()))
  local_mocked_bindings(prep_data = function(...) stop("Reached approved preparation"), .package = "finnts")
  expect_error(do.call(forecast_time_series, args), "Reached approved preparation")
  args$models_to_run <- "node_rule"
  expect_error(do.call(forecast_time_series, args), "mixed.*bottoms_up")
  expect_length(list.files(info$path, recursive = TRUE), 0L)
  args$models_to_run <- c("node_rule", "meanf")
  args$models_not_to_run <- "meanf"
  expect_error(do.call(forecast_time_series, args), "Reached approved preparation")
  args$custom_models <- list(node_rule = hierarchy_model(NULL))
  expect_error(do.call(forecast_time_series, args), "hierarchy.*approval|hierarchy.*capability")
  args$custom_models <- list(node_rule = hierarchy_model())
  args$input_data <- args$input_data[-1, ]
  expect_error(do.call(forecast_time_series, args), "balanced")
  expect_length(list.files(info$path, recursive = TRUE), 0L)
  args$input_data <- hierarchy_input()
  local({
    local_mocked_bindings(custom_model_package_available = function(package) package != "modeltime", .package = "finnts")
    expect_error(do.call(forecast_time_series, args), "unavailable package 'modeltime'")
  })
  args$models_to_run <- c("node_rule", "timegpt")
  expect_error(do.call(forecast_time_series, args), "not qualified")
  expect_length(list.files(info$path, recursive = TRUE), 0L)
  args$models_to_run <- c("node_rule", "meanf")
  args$hist_start_date <- min(args$input_data$Date)
  args$input_data <- args$input_data[args$input_data$Date > args$hist_start_date, ]
  expect_error(do.call(forecast_time_series, args), "balanced")
})

test_that("staged hierarchy rejection cannot register a custom-only pool", {
  info <- list(project_name = "hierarchy-staged", run_name = "test", path = withr::local_tempdir(),
    storage_object = NULL, data_output = "csv", object_output = "rds")
  settings <- list(forecast_approach = "standard_hierarchy", recipes_to_run = "R1", stationary = FALSE,
    box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE, multistep_horizon = FALSE,
    date_type = "month", forecast_horizon = 2, external_regressors = NA_character_)
  pool <- custom_run_pool("node_rule", NULL, list(node_rule = hierarchy_model()))
  expect_error(custom_run_prepare(info, pool, settings, FALSE), "mixed.*bottoms_up")
  expect_length(list.files(info$path, recursive = TRUE), 0L)
  pool <- custom_run_pool(c("node_rule", "meanf"), NULL, list(node_rule = hierarchy_model()))
  settings$forecast_approach <- "bottoms_up"
  expect_error(custom_run_prepare(info, pool, settings, FALSE), "hierarchy-only")
  expect_length(list.files(info$path, recursive = TRUE), 0L)
})

# Prepare one real immutable hierarchy in a caller-owned temporary directory.
# Tests reuse the resulting files rather than repeat training to create fixtures.
hierarchy_prepare <- function(path, approach = "standard_hierarchy", date_type = "month", data_output = "csv") {
  info <- set_run_info(project_name = "custom-hierarchy", run_name = "test", path = path,
    data_output = if (data_output == "rds") "csv" else data_output, object_output = "rds", add_unique_id = FALSE)
  info$data_output <- data_output
  prep_data(info, hierarchy_input(approach, date_type), c("Region", "Product"), "value", date_type, 2,
    forecast_approach = approach, recipes_to_run = "R1", stationary = FALSE, box_cox = FALSE,
    clean_missing_values = FALSE, clean_outliers = FALSE, multistep_horizon = FALSE)
  info
}

test_that("prepared hierarchy manifests bind the exact topology before restart", {
  info <- hierarchy_prepare(withr::local_tempdir())
  model <- hierarchy_model()
  prep_models(info, models_to_run = c("node_rule", "meanf"), custom_models = list(node_rule = model),
    run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
  saved <- custom_run_read(info, TRUE)
  expect_match(saved$manifest$context$hierarchy_id, "^[a-f0-9]{64}$")
  log <- read_exact_artifact(info, local_artifact_path(info, "logs", extension = "csv"))
  expect_identical(custom_run_load(info, log)$manifest$manifest_id, saved$manifest$manifest_id)
  path <- local_artifact_path(info, "prep_data", "-hts_info", extension = "rds")
  metadata <- readRDS(path)
  metadata$hts_combos <- rev(metadata$hts_combos)
  saveRDS(metadata, path)
  expect_error(custom_run_load(info, log), "topology|manifest")
})

test_that("hierarchy contexts preserve RDS and Parquet data without discovery on reload", {
  for (format in c("rds", "parquet")) local({
    if (format == "parquet") skip_if_not_installed("arrow")
    info <- hierarchy_prepare(withr::local_tempdir(), data_output = format)
    prep_models(info, models_to_run = c("node_rule", "meanf"), custom_models = list(node_rule = hierarchy_model()),
      run_ensemble_models = FALSE, back_test_scenarios = 1, num_hyperparameters = 1)
    files <- list.files(info$path, recursive = TRUE, full.names = TRUE)
    before <- tools::md5sum(files)
    local_mocked_bindings(list_files = function(...) stop("Unexpected hierarchy listing"),
      local_artifact_inventory = function(...) stop("Unexpected hierarchy discovery"), .package = "finnts")
    log <- read_exact_artifact(info, local_artifact_path(info, "logs", extension = "csv"))
    loaded <- custom_run_load(info, log)
    captured <- custom_run_hierarchy_context(info, custom_run_context(info, log, FALSE, allow_hierarchy = TRUE), log,
      return_data = TRUE, max_rows = 10000)
    expect_identical(loaded$manifest$context$hierarchy_id, captured$context$hierarchy_id)
    expect_equal(nrow(captured$data), 24 * length(captured$topology$hts_combos))
    expect_true(all(is.finite(captured$data$Target)))
    expect_identical(tools::md5sum(files), before)
  })
})



# Adapt the existing small reconciliation fixture to a synthetic mixed pool.
# Source never runs: these rows exercise coverage/publication and real hts only.
hierarchy_reconciliation_fixture <- function(approach = "standard_hierarchy", date_type = "month") {
  fixture <- make_reconciled_selection_fixture(approach, date_type)
  args <- hierarchy_definition_args(approach)
  args$requirements$date_types <- date_type
  args$requirements$forecast_horizon <- 6
  definition <- do.call(new_custom_model_definition, args)
  fixture$custom <- list(pool = list(selected = c("node_rule", "meanf"), built_in = "meanf",
    custom = list(node_rule = definition)), manifest = list(context = list(
      forecast_approach = approach, date_type = date_type, forecast_horizon = 6)))
  custom <- fixture$forecasts$Model_ID == "accurate"
  fixture$forecasts$Model_ID <- ifelse(custom, paste0("custom-", definition$version_id, "--local--R1"), "meanf--local--R1")
  fixture$forecasts$Model_Name <- ifelse(custom, "node_rule", "meanf")
  fixture$forecasts$Model_Type <- "local"
  fixture$forecasts$Recipe_ID <- "R1"
  fixture$forecasts$Run_Type <- NULL
  fixture$project_info$path <- tempdir()
  fixture
}

# Replace artifact transport only while running real hts reconciliation. Writes
# are captured in a caller-visible environment, including on error; no files,
# models or forecasts are generated outside this small deterministic fixture.
hierarchy_reconcile <- function(fixture, forecasts = fixture$forecasts, written = new.env(parent = emptyenv())) {
  testthat::local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    list_files = function(...) character(),
    read_file = function(run_info, path, return_type = "df", ...) {
      if (return_type == "object") return(fixture$metadata)
      if (grepl("train_test_split", path, fixed = TRUE)) return(fixture$splits)
      if (grepl("hts_data", path, fixed = TRUE)) return(fixture$history)
      forecasts
    },
    write_data = function(x, combo, ...) { written[[combo]] <- x }, .package = "finnts")
  reconcile_hierarchical_data(fixture$project_info, NULL, fixture$custom$manifest$context$forecast_approach,
    TRUE, FALSE, fixture$project_info$date_type, 1, custom_run = fixture$custom)
  as.list(written)
}

test_that("custom reconciliation rejects incomplete candidates before publication", {
  fixture <- hierarchy_reconciliation_fixture()
  for (defect in c("missing", "duplicate", "nonfinite", "foreign", "missing_builtin")) {
    forecasts <- fixture$forecasts
    index <- which(forecasts$Model_Name == "node_rule" & forecasts$Train_Test_ID != 1)[[1]]
    if (defect == "missing") forecasts <- forecasts[-index, ]
    if (defect == "duplicate") forecasts <- rbind(forecasts, forecasts[index, ])
    if (defect == "nonfinite") forecasts$Forecast[index] <- Inf
    if (defect == "foreign") forecasts$Model_ID[index] <- "foreign--local--R1"
    if (defect == "missing_builtin") forecasts <- forecasts[forecasts$Model_Name == "node_rule", ]
    written <- new.env(parent = emptyenv())
    expect_error(hierarchy_reconcile(fixture, forecasts, written), "coverage|identity|pool")
    expect_length(as.list(written), 0L)
  }
  written <- new.env(parent = emptyenv())
  local_mocked_bindings(combinef = function(...) stop("Deliberate reconciliation failure"), .package = "hts")
  expect_error(hierarchy_reconcile(fixture, written = written), "Deliberate reconciliation failure")
  expect_length(as.list(written), 0L)
})

test_that("mixed hierarchy reconciliation matches an independent projection at every cadence", {
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    summing <- rbind(rep(1, 4), c(1, 1, 0, 0), c(0, 0, 1, 1))
    if (approach == "grouped_hierarchy") summing <- rbind(summing, c(1, 0, 1, 0), c(0, 1, 0, 1))
    summing <- rbind(summing, diag(4))
    for (date_type in c("year", "quarter", "month", "week", "day")) {
      fixture <- hierarchy_reconciliation_fixture(approach, date_type)
      forecasts <- fixture$forecasts
      values <- as.numeric(summing %*% unname(fixture$values))
      node_values <- values[match(forecasts$Combo, fixture$metadata$hts_combos)]
      custom <- forecasts$Model_Name == "node_rule"
      future <- forecasts$Train_Test_ID == 1
      forecasts$Forecast <- ifelse(future, node_values + ifelse(custom, 10, 20),
        forecasts$Target - ifelse(custom, 1, 2))
      written <- hierarchy_reconcile(fixture, forecasts[rev(seq_len(nrow(forecasts))), ])
      custom_id <- unique(forecasts$Model_ID[custom])
      expected <- as.numeric(solve(crossprod(summing), crossprod(summing, values + 10)))
      for (model in c(custom_id, "Best-Model")) {
        result <- written[[model]]
        result <- result[result$Train_Test_ID == 1, ]
        expect_equal(result$Forecast, expected[match(result$Combo, fixture$metadata$original_combos)],
          tolerance = 1e-7, info = paste(approach, date_type, model))
        expect_equal(sort(unique(result$Date)), sort(unique(forecasts$Date[future])))
      }
      expect_true(any(abs(expected - (unname(fixture$values) + 10)) > 1e-6))
      expect_identical(unique(written[[custom_id]]$Model_Name), "node_rule")
      expect_setequal(forecasts$Model_Name[forecasts$Best_Model == "Yes"], "node_rule")
    }
  }
})
