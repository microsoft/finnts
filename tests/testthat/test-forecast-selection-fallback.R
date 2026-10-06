test_that("plausibility fallback is explicit and retains strict failure evidence", {
  fixture <- make_selection_case(actuals = 100 + seq_len(36) %% 5,
    futures = list(less = rep(30000, 6), worse = rep(60000, 6)),
    errors = c(less = 0.03, worse = 0.01))
  strict <- do.call(select_forecast_candidate, fixture)
  expect_true(is.na(strict$selected_id))
  fixture$context$allow_plausibility_fallback <- TRUE
  result <- do.call(select_forecast_candidate, fixture)
  expect_identical(result$selected_id, "less")
  expect_identical(result$fallback_id, "less")
  expect_false(any(result$rankings$Eligible))
  expect_true(all(vapply(result$rankings$Reasons,
    function(reasons) "catastrophic_magnitude" %in% reasons, logical(1))))
  expect_true(all(result$rankings$Risk > 0))
})

test_that("fallback ranking counts issues before severity accuracy and stable identity", {
  fixture <- make_selection_case(futures = list(a = rep(30000, 6),
    b = rep(40000, 6), broken = rep(Inf, 6)), errors = c(a = 0.1, b = 0.01, broken = 0))
  rankings <- do.call(evaluate_forecast_candidates, fixture)
  rankings$Reasons <- list(c("catastrophic_magnitude", "level_deviation"),
    "catastrophic_magnitude", "nonfinite_forecast")
  rankings$Risk <- c(1, 10, 0)
  for (order in list(1:3, 3:1)) {
    result <- rank_forecast_candidates(rankings[order, ], allow_plausibility_fallback = TRUE)
    expect_identical(result$selected_id, "b")
  }
  rankings$Reasons[[1]] <- "catastrophic_magnitude"
  rankings$Risk[1:2] <- 10
  expect_identical(rank_forecast_candidates(rankings, allow_plausibility_fallback = TRUE)$selected_id, "b")
  rankings$WMAPE[1:2] <- 0.1
  expect_identical(rank_forecast_candidates(rankings, allow_plausibility_fallback = TRUE)$selected_id, "a")
})

test_that("fallback never repairs structurally unusable candidates", {
  for (defect in c("nonfinite", "missing", "duplicate", "accuracy", "no-accuracy")) {
    fixture <- make_selection_case(futures = list(only = rep(30000, 6)), errors = c(only = 0.02))
    fixture$context$allow_plausibility_fallback <- TRUE
    if (defect == "nonfinite") fixture$forecasts$Forecast[1] <- Inf
    if (defect == "missing") fixture$forecasts <- fixture$forecasts[-1, ]
    if (defect == "duplicate") fixture$forecasts <- rbind(fixture$forecasts, fixture$forecasts[1, ])
    if (defect == "accuracy") fixture$backtests$Forecast[1] <- NA_real_
    if (defect == "no-accuracy") fixture$history$Target[31:36] <- NA_real_
    result <- do.call(select_forecast_candidate, fixture)
    expect_true(is.na(result$selected_id), info = defect)
    expect_null(result$fallback_id)
  }
})

test_that("normal selection stays unchanged and fresh defaults can rank soft-only failures", {
  fixture <- make_selection_case(actuals = 100 + seq_len(36) %% 5,
    futures = list(accurate = rep(600, 6), safer = rep(300, 6)),
    errors = c(accurate = 0.01, safer = 0.1))
  strict <- do.call(select_forecast_candidate, fixture)
  fixture$context$allow_plausibility_fallback <- TRUE
  ordinary <- do.call(select_forecast_candidate, fixture)
  expect_identical(ordinary$selected_id, strict$selected_id)
  expect_null(ordinary$fallback_id)
  fixture$context$require_clean_selection <- TRUE
  fallback <- do.call(select_forecast_candidate, fixture)
  expect_identical(fallback$selected_id, "safer")
  expect_identical(fallback$fallback_id, "safer")
})

test_that("finalization stamps a fallback without adding saved fields and restart does not refit", {
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    par_end = function(...) NULL
  )
  control <- make_best_models_fixture()
  final_models(control, weekly_to_daily = FALSE)
  control_rows <- read_selection_file(control, "forecasts", "-single_models", "Synthetic")
  control_log <- read_selection_file(control, "logs")
  info <- make_best_models_fixture()
  rows <- read_selection_file(info, "forecasts", "-single_models", "Synthetic")
  rows$Forecast[rows$Train_Test_ID == 1] <- ifelse(
    rows$Model_Name[rows$Train_Test_ID == 1] == "meanf", 30000, 60000)
  write_data(rows, "Synthetic", info, "data", "forecasts", "-single_models")
  expect_warning(result <- final_models(info, weekly_to_daily = FALSE),
    class = "finnts_forecast_selection_fallback")
  saved <- read_selection_file(info, "forecasts", "-single_models", "Synthetic")
  expect_length(unique(saved$Model_ID[saved$Best_Model == "Yes"]), 1L)
  expect_identical(names(saved), names(control_rows))
  expect_identical(names(read_selection_file(info, "logs")), names(control_log))
  expect_true(agent_selection_summary(result)$acceptable)
  before <- tools::md5sum(locate_single_models_file(info))
  local_mocked_bindings(
    select_series_forecasts = function(...) stop("completed winner must not be reselected"),
    train_models = function(...) stop("unexpected fitting")
  )
  expect_no_warning(final_models(info, weekly_to_daily = FALSE))
  expect_identical(tools::md5sum(locate_single_models_file(info)), before)
})

test_that("single-series fallback repair preserves the empty optional average contract", {
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    par_end = function(...) NULL,
    train_models = function(...) stop("unexpected fitting")
  )
  info <- make_best_models_fixture()
  info$combo <- hash_data("Synthetic")
  info$rebuild_update_models <- TRUE
  rows <- read_selection_file(info, "forecasts", "-single_models", "Synthetic")
  rows <- rows[rows$Model_Name == "meanf", ]
  rows$Forecast[rows$Train_Test_ID == 1] <- 30000
  write_data(rows, "Synthetic", info, "data", "forecasts", "-single_models")
  expect_warning(result <- final_models(info, weekly_to_daily = FALSE),
    class = "finnts_forecast_selection_fallback")
  expect_true(agent_selection_summary(result)$acceptable)
  expect_equal(nrow(read_selection_file(info, "forecasts", "-average_models",
    "Synthetic", optional = TRUE)), 0L)
  info$rebuild_update_models <- NULL
  before <- tools::md5sum(locate_single_models_file(info))
  expect_no_warning(final_models(info, weekly_to_daily = FALSE))
  expect_identical(tools::md5sum(locate_single_models_file(info)), before)
})

test_that("fresh defaults consider every usable individual without averaging rejected components", {
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    par_end = function(...) NULL
  )
  info <- make_best_models_fixture()
  info$default_reforecast <- TRUE
  rows <- read_selection_file(info, "forecasts", "-single_models", "Synthetic")
  rows$Forecast[rows$Train_Test_ID != 1 & rows$Model_Name == "meanf"] <- 30000
  rows$Forecast[rows$Train_Test_ID == 1 & rows$Model_Name == "snaive"] <- 600
  write_data(rows, "Synthetic", info, "data", "forecasts", "-single_models")
  expected <- select_series_forecasts(rows, read_series_history(info, "Synthetic"),
    read_selection_file(info, "prep_models", "-train_test_split"),
    allow_plausibility_fallback = TRUE, require_clean_selection = TRUE)
  expect_warning(result <- final_models(info, weekly_to_daily = FALSE),
    class = "finnts_forecast_selection_fallback")
  selected <- result$selections$Synthetic
  expect_setequal(selected$rankings$Model_ID, unique(rows$Model_ID))
  expect_identical(selected$selected_id, expected$selected_id)
  expect_equal(nrow(read_selection_file(info, "forecasts", "-average_models",
    "Synthetic", optional = TRUE)), 0L)
})

test_that("fallback warning explains a clean winner outside the normal accuracy shortlist", {
  fixture <- make_selection_case(actuals = 100 + seq_len(36) %% 5,
    futures = list(accurate = rep(600, 6), safe = rep(102, 6)),
    errors = c(accurate = 0.01, safe = 0.1))
  fixture$context$allow_plausibility_fallback <- TRUE
  fixture$context$require_clean_selection <- TRUE
  selection <- do.call(select_forecast_candidate, fixture)
  expect_identical(selection$fallback_id, "safe")
  expect_warning(warn_forecast_fallback(selection, "Synthetic"),
    "none.*outside the normal accuracy shortlist",
    class = "finnts_forecast_selection_fallback")
})

test_that("hierarchical finalization reconciles usable fallback sources without another quality veto", {
  local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    par_end = function(...) NULL,
    train_models = function(...) stop("unexpected fitting")
  )
  fixture <- make_hierarchical_selection_artifacts(horizon = 3L, backtest_scenarios = 1L)
  info <- fixture$run_info
  for (combo in fixture$metadata$hts_combos) {
    rows <- read_selection_file(info, "forecasts", "-single_models", combo)
    rows$Forecast[rows$Train_Test_ID == 1] <- 1000 * max(abs(fixture$contexts[[combo]]$history$Target))
    write_data(rows, combo, info, "data", "forecasts", "-single_models")
  }
  captured <- capture_forecast_warnings(final_models(info, average_models = FALSE, weekly_to_daily = FALSE))
  expect_length(captured$warnings, length(fixture$metadata$hts_combos))
  expect_true(agent_selection_summary(captured$value)$acceptable)
  expect_true(all(vapply(captured$value$source_selections,
    function(selection) !is.null(selection$fallback_id), logical(1))))
  for (combo in fixture$metadata$hts_combos) {
    rows <- read_selection_file(info, "forecasts", "-single_models", combo)
    expect_length(unique(rows$Model_ID[rows$Best_Model == "Yes"]), 1L)
  }
  delivered <- get_forecast_data(info)
  expect_true(all(is.finite(delivered$Forecast[delivered$Best_Model == "Yes"])))
})

# Build local saved-default artifacts, including historical rejected metadata
# that predates the retired-field filter. No real forecasting engine is called.
make_rejected_default_fixture <- function(kind = "extreme", selected = FALSE, .env = parent.frame()) {
  info <- make_best_models_fixture(.local_envir = .env)
  info$combo <- hash_data("Synthetic")
  rows <- read_selection_file(info, "forecasts", "-single_models", "Synthetic")
  rows$Forecast[rows$Train_Test_ID == 1] <- if (kind == "nonfinite") Inf else
    if (kind == "soft") 0 else 30000
  splits <- read_selection_file(info, "prep_models", "-train_test_split")
  rows$Best_Model <- ifelse(selected & rows$Model_Name == "meanf", "Yes", "No")
  rows <- create_prediction_intervals(rows, splits)
  write_data(rows, "Synthetic", info, "data", "forecasts", "-single_models")
  models <- unique(rows[, c("Combo_ID", "Model_ID", "Model_Name", "Model_Type", "Recipe_ID")])
  models$Model_Fit <- rep(list(stats::lm(mpg ~ wt, mtcars)), nrow(models))
  write_data(models, "Synthetic", info, "object", "models", "-single_models")
  log <- read_selection_file(info, "logs")
  log$average_models <- TRUE
  log$max_model_average <- 3
  log$weekly_to_daily <- FALSE
  log$weighted_mape <- NA_real_
  log$selection_status <- "rejected"
  log$default_reforecast_status <- "rejected"
  utils::write.csv(log, local_artifact_path(info, "logs", extension = "csv"), row.names = FALSE)
  history <- read_selection_file(info, "prep_data", "-R1", "Synthetic")
  history <- history[!is.na(history$Target), ]
  agent <- list(run_id = "parent", agent_version = 2, forecast_horizon = 3,
    default_reforecast = TRUE, project_info = info, forecast_approach = "bottoms_up")
  agent$project_info$date_type <- "month"
  agent$project_info$weekly_to_daily <- FALSE
  list(info = info, agent = agent, input = history)
}

test_that("legacy rejected defaults recover from saved forecasts without any new fit or fields", {
  for (kind in c("extreme", "soft")) for (selected in c(FALSE, TRUE)) local({
    fixture <- make_rejected_default_fixture(kind, selected)
    original_local_reader <- read_local_artifacts
    local_mocked_bindings(
      set_run_info = function(...) fixture$info,
      read_local_artifacts = function(run_info, file_list, ...) {
        if (any(grepl("input_data", file_list, fixed = TRUE))) return(fixture$input)
        original_local_reader(run_info, file_list, ...)
      },
      prep_data = function(...) stop("unexpected preparation"),
      prep_models = function(...) stop("unexpected model preparation"),
      train_models = function(...) stop("unexpected refit"),
      list_files = function(...) stop("unexpected discovery"),
      par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
      par_end = function(...) NULL,
      validate_run_outputs = function(...) TRUE
    )
    expect_warning(result <- submit_fcst_run(fixture$agent, list(models_to_run = "meanf"),
      hash_data("Synthetic"), "default"), class = "finnts_forecast_selection_fallback")
    expect_true(agent_selection_summary(result$forecast_selection, check_quality = TRUE,
      allow_plausibility_fallback = TRUE)$acceptable)
    saved <- read_selection_file(fixture$info, "forecasts", "-single_models", "Synthetic")
    expect_length(unique(saved$Model_ID[saved$Best_Model == "Yes"]), 1L)
    log <- read_selection_file(fixture$info, "logs")
    expect_false(any(c("fallback_id", "default_reforecast", "default_reforecast_status") %in% names(log)))
    splits <- read_selection_file(fixture$info, "prep_models", "-train_test_split")
    delivered <- dplyr::left_join(saved, splits[, c("Train_Test_ID", "Run_Type")], by = "Train_Test_ID")
    log_best_run(fixture$agent, result, calculate_fcst_metrics(result, delivered),
      combo = hash_data("Synthetic"), check_best_run = FALSE)
    parent <- fixture$agent$project_info
    parent$run_name <- fixture$agent$run_id
    best <- read_selection_file(parent, "logs", "-agent_best_run", "Synthetic")
    expect_identical(best$selection_status, "evaluated")
    expect_true(is.finite(best$weighted_mape))
    expect_false(any(c("fallback_id", "default_reforecast", "default_reforecast_status") %in% names(best)))
  })
})

test_that("legacy nonfinite defaults retain the explicit rejection without fitting", {
  fixture <- make_rejected_default_fixture("nonfinite")
  local_mocked_bindings(
    set_run_info = function(...) fixture$info,
    read_local_artifacts = function(...) fixture$input,
    prep_data = function(...) stop("unexpected preparation"),
    train_models = function(...) stop("unexpected refit")
  )
  expect_error(submit_fcst_run(fixture$agent, list(models_to_run = "meanf"),
    hash_data("Synthetic"), "default"), class = "finnts_forecast_selection_rejected")
})

test_that("fallback acceptance never weakens strict update acceptance", {
  fixture <- make_selection_case(futures = list(only = rep(30000, 6)), errors = c(only = 0.01))
  fixture$context$allow_plausibility_fallback <- TRUE
  selected <- do.call(select_forecast_candidate, fixture)
  result <- list(selections = list(series = selected))
  expect_true(agent_selection_summary(result)$acceptable)
  expect_false(agent_selection_summary(result, check_quality = TRUE)$acceptable)
  expect_true(agent_selection_summary(result, check_quality = TRUE,
    allow_plausibility_fallback = TRUE)$acceptable)
  expect_error(validate_forecast_selection(selected, "only"), "ineligible")
  expect_identical(validate_forecast_selection(selected, "only", TRUE), selected)
  fixture$context$allow_plausibility_fallback <- FALSE
  expect_true(is.na(do.call(select_forecast_candidate, fixture)$selected_id))
})

test_that("both fresh default routes relay fallback warnings and publish once", {
  fixture <- make_selection_case(futures = list(only = rep(30000, 6)), errors = c(only = 0.01))
  fixture$context$allow_plausibility_fallback <- TRUE
  selected <- do.call(select_forecast_candidate, fixture)
  for (failed in c(FALSE, TRUE)) local({
    logged <- 0L
    local_mocked_bindings(
      get_foundation_model_suffix = function() "",
      par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
      par_end = function(...) NULL,
      read_exact_artifact = function(...) NULL,
      read_file = function(...) data.frame(),
      submit_fcst_run = function(...) {
        warn_forecast_fallback(selected, "series")
        list(forecast_selection = list(selections = list(series = selected)))
      },
      get_fcst_output = function(...) data.frame(),
      calculate_fcst_metrics = function(...) 0.01,
      log_best_run = function(...) { logged <<- logged + 1L }
    )
    hash <- hash_data("series")
    agent <- list(run_id = "parent", project_info = list(project_name = "project", path = tempdir()),
      quality_rejected_combos = if (failed) hash else character())
    captured <- capture_forecast_warnings(forecast_new_combos(agent,
      if (failed) character() else hash, if (failed) hash else character(), NULL, FALSE, 1, 123))
    expect_identical(captured$value, "Finished Forecasting New Time Series")
    expect_length(captured$warnings, 1L)
    expect_s3_class(captured$warnings[[1]], "finnts_forecast_selection_fallback")
    expect_identical(logged, 1L)
  })
})
