test_that("custom updates always refit enrolled custom models beside a built-in winner", {
  fixture <- custom_update_fixture(builtin_winner = TRUE)
  args <- fixture$args
  current <- fixture$current
  forecast <- fixture$forecast
  local_custom_update_runtime()
  winner <- "meanf--local--R1"
  custom_id <- paste0("custom-", args$custom_models$replay_mean$definition$version_id, "--local--C1")
  expect_identical(read_agent_custom_update(current, TRUE)$winners$first$components, winner)
  expect_setequal(unique(forecast$Model_ID), c(winner, custom_id))
  expect_identical(unique(forecast$Model_ID[forecast$Best_Model == "Yes"]), winner)
  expect_true(all(forecast$Best_Model[forecast$Model_ID == custom_id] == "No"))
  expect_equal(unique(forecast$Forecast[forecast$Model_ID == custom_id & forecast$Train_Test_ID == 1]), 101)
  # The comparison is never averaged into the winner or its model-pool spread;
  # one winning model has no spread, whereas a refit comparison would add one.
  expect_false(any(forecast$Recipe_ID == "simple_average"))
  best <- get_best_agent_run(current)
  expect_true(is.na(best$model_std_wmape))
  expect_equal(as.numeric(best$model_avg_wmape), as.numeric(best$weighted_mape), tolerance = 1e-4)
  expect_setequal(strsplit(best$models_to_run, "---", fixed = TRUE)[[1]], c("meanf", "replay_mean"))

  local({
    local_mocked_bindings(
      fit_models = function(...) stop("Unexpected update refit"),
      summarize_models = function(...) invisible(NULL), .package = "finnts")
    before <- tools::md5sum(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE))
    update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
    expect_identical(tools::md5sum(names(before)), before)
  })

  local_mocked_bindings(
    forecast_new_combos = function(...) stop("Unexpected default fallback"),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  args$input_data <- rbind(args$input_data, data.frame(Date = as.Date("2022-02-01"), series = "first", value = 101))
  next_run <- do.call(set_agent_info, args)
  update_forecast(next_run, allow_iterate_forecast = FALSE, num_cores = 1)
  chained <- get_agent_forecast(next_run)
  expect_identical(read_agent_custom_update(next_run, TRUE)$predecessor$run_id, current$run_id)
  expect_setequal(unique(chained$Model_ID), c(winner, custom_id))
  expect_identical(unique(chained$Model_ID[chained$Best_Model == "Yes"]), winner)
  expect_equal(unique(chained$Forecast[chained$Model_ID == custom_id & chained$Train_Test_ID == 1]), 101)
})

test_that("interrupted completion logging keeps winner-only metrics beside comparisons", {
  fixture <- custom_update_fixture(builtin_winner = TRUE)
  current <- fixture$current
  local_custom_update_runtime()
  metrics <- c("combo", "best_run_name", "weighted_mape", "model_avg_wmape", "model_median_wmape", "model_std_wmape")
  best <- get_best_agent_run(current)
  # Move the series' current best-run record aside, as an update interrupted
  # before logging leaves it, so the saved update rebuilds its completion
  # metrics from outputs that also hold the custom comparison.
  parent <- current$project_info
  parent$run_name <- current$run_id
  record <- local_artifact_path(parent, "logs", "-agent_best_run", hash_data("first"), extension = "csv")
  expect_true(file.rename(record, file.path(withr::local_tempdir(), basename(record))))
  expect_false(file.exists(record))
  local_mocked_bindings(
    fit_models = function(...) stop("Unexpected interrupted-tail refit"),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_true(file.exists(record))
  rebuilt <- get_best_agent_run(current)
  expect_true(is.na(rebuilt$model_std_wmape))
  expect_equal(as.data.frame(rebuilt[, metrics]), as.data.frame(best[, metrics]))
})

test_that("updates saved without custom comparisons require a new Agent version", {
  fixture <- custom_update_fixture(builtin_winner = TRUE)
  args <- fixture$args
  current <- fixture$current
  local_custom_update_runtime()
  custom_id <- paste0("custom-", args$custom_models$replay_mean$definition$version_id, "--local--C1")
  best <- get_best_agent_run(current)
  # Recreate an update child saved before custom comparisons were always refit.
  child <- current$project_info
  child$project_name <- paste0(child$project_name, "_", hash_data("first"))
  child$run_name <- best$best_run_name[[1]]
  models <- readRDS(local_artifact_path(child, "models", "-single_models", hash_data("first"), "rds"))
  write_data(models[models$Model_ID != custom_id, ], "first", child, "object", "models", "-single_models")
  rows <- read_selection_file(child, "forecasts", "-single_models", "first")
  write_data(rows[rows$Model_ID != custom_id, ], "first", child, "data", "forecasts", "-single_models")
  stale_error <- "every enrolled custom model for comparison"
  expect_error(audit_agent_custom_best(load_agent_custom_state(current), best), stale_error)

  args$input_data <- rbind(args$input_data, data.frame(Date = as.Date("2022-02-01"), series = "first", value = 101))
  next_run <- do.call(set_agent_info, args)
  files <- list.files(args$project_info$path, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(files)
  expect_error(update_forecast(next_run, allow_iterate_forecast = FALSE, num_cores = 1), stale_error)
  expect_setequal(list.files(args$project_info$path, recursive = TRUE, full.names = TRUE), files)
  expect_identical(tools::md5sum(files), before)
})

test_that("damaged saved custom comparisons are hard errors, not reused output", {
  # Each case damages only the non-winning comparison: a missing future
  # forecast, a dropped backtest date and a duplicated row.
  damage <- list(
    missing_value = function(rows, id) {
      rows$Forecast[rows$Model_ID == id & rows$Train_Test_ID == 1] <- NA
      rows
    },
    missing_date = function(rows, id) rows[!(rows$Model_ID == id & rows$Train_Test_ID == 2), ],
    duplicate_row = function(rows, id) rbind(rows, rows[which(rows$Model_ID == id)[1], ])
  )
  for (case in names(damage)) local({
    fixture <- custom_update_fixture(builtin_winner = TRUE)
    current <- fixture$current
    local_custom_update_runtime()
    custom_id <- paste0("custom-", fixture$args$custom_models$replay_mean$definition$version_id, "--local--C1")
    best <- get_best_agent_run(current)
    child <- current$project_info
    child$project_name <- paste0(child$project_name, "_", hash_data("first"))
    child$run_name <- best$best_run_name[[1]]
    rows <- read_selection_file(child, "forecasts", "-single_models", "first")
    write_data(damage[[case]](rows, custom_id), "first", child, "data", "forecasts", "-single_models")
    expect_error(audit_agent_custom_best(load_agent_custom_state(current), best),
      "damaged or foreign fitted outputs", info = case)
    files <- list.files(fixture$args$project_info$path, recursive = TRUE, full.names = TRUE)
    before <- tools::md5sum(files)
    local_mocked_bindings(fit_models = function(...) stop("Unexpected refit"), .package = "finnts")
    expect_error(update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1),
      "damaged or foreign fitted outputs", info = case)
    expect_identical(tools::md5sum(files), before, info = case)
  })
})
