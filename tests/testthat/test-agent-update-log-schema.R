test_that("metadata writers omit withdrawn tracking fields without changing other values", {
  retired <- c("global_update_previous_selection", "global_update_selected",
    "global_update_recovered_sources", "global_update_concerned_sources", "global_update_status",
    "default_reforecast_status")
  expected <- data.frame(combo = "00042", weighted_mape = 0.125,
    average_models = TRUE, max_model_average = 3L)
  inherited <- expected
  inherited[retired] <- "withdrawn"
  cases <- list(
    list(folder = "logs", suffix = NULL, combo = NULL, type = "log"),
    list(folder = "logs", suffix = "-agent_best_run", combo = "00042", type = "log"),
    list(folder = "final_output", suffix = "-run_metadata", combo = NULL, type = "data")
  )
  for (format in c("csv", "rds", "parquet")) {
    info <- list(project_name = "schema", run_name = "update", path = withr::local_tempdir(),
      storage_object = NULL, data_output = format, object_output = "rds")
    for (case in cases) {
      write_data(inherited, case$combo, info, case$type, case$folder, case$suffix)
      path <- local_artifact_path(info, case$folder, case$suffix,
        if (is.null(case$combo)) NULL else hash_data(case$combo),
        if (case$type == "log") "csv" else format)
      saved <- read_exact_artifact(info, path, character_columns = "combo")
      expect_identical(names(saved), names(expected))
      expect_identical(saved$combo, expected$combo)
      expect_equal(saved$weighted_mape, expected$weighted_mape)
      expect_identical(saved$average_models, expected$average_models)
      expect_equal(saved$max_model_average, expected$max_model_average)
    }
    write_data(inherited, "00042", info, "data", "forecasts", "-global_models")
    other <- read_exact_artifact(info,
      local_artifact_path(info, "forecasts", "-global_models", hash_data("00042")),
      character_columns = "combo")
    expect_identical(names(other), names(inherited))
  }
  expect_true(all(retired %in% names(inherited)))
})

test_that("recovery and final Agent metadata require no new log columns", {
  path <- withr::local_tempdir()
  previous <- make_update_chain_case(path, uniform = TRUE, data_output = "csv")
  previous$log$average_models <- FALSE
  previous$log$max_model_average <- 1L
  write_data(previous$log, NULL, previous$info, "log", "logs")
  current <- make_update_chain_case(path, "schema-recovery", legacy = TRUE, data_output = "csv")
  changed <- current$hierarchy$hts_combos[2]
  current$fitted$Forecast_Tbl[[1]]$Forecast[
    current$fitted$Forecast_Tbl[[1]]$Combo == changed &
      current$fitted$Forecast_Tbl[[1]]$Run_Type == "Future_Forecast"] <- Inf
  step <- run_update_chain_step(previous, current)
  expect_identical(step$result$status, "done")
  log <- read_selection_file(current$info, "logs")
  expect_false(any(startsWith(names(log), "global_update_")))
  expect_true(log$average_models)
  expect_equal(log$max_model_average, length(previous$ids))
  parent <- step$agent$project_info
  parent$run_name <- step$agent$run_id
  for (combo in previous$hierarchy$original_combos) {
    record <- read_selection_file(parent, "logs", "-agent_best_run", combo)
    expect_false(any(startsWith(names(record), "global_update_")))
    expect_identical(record$best_run_name, current$info$run_name)
  }
  # Only outer input validation/discovery is substituted; aggregation and
  # writing use the actual per-series completion records produced above.
  local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    get_total_combos = function(...) vapply(previous$hierarchy$original_combos, hash_data, character(1))
  )
  save_best_agent_run(step$agent)
  metadata <- read_selection_file(parent, "final_output", "-run_metadata")
  expect_false(any(startsWith(names(metadata), "global_update_")))
  expect_setequal(metadata$combo, previous$hierarchy$original_combos)
  expect_length(run_update_chain_step(previous, current)$fits, 0L)
})
