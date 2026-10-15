test_that("public custom update replays the exact approved version on new data", {
  fixture <- custom_update_fixture()
  args <- fixture$args
  previous <- fixture$previous
  current <- fixture$current
  forecast <- fixture$forecast
  local_custom_update_runtime()
  expect_equal(unique(get_agent_forecast(previous)$Forecast), 100)
  local_mocked_bindings(
    create_custom_model = function(...) stop("Unexpected source regeneration"), forecast_new_combos = function(...) stop("Unexpected default fallback"),
    .package = "finnts"
  )
  expect_equal(unique(forecast$Forecast[forecast$Train_Test_ID == 1]), 101)
  expect_identical(unique(forecast$Model_ID), paste0("custom-", args$custom_models$replay_mean$definition$version_id, "--local--C1"))
  expect_identical(read_agent_custom_record(current, TRUE)$envelopes, previous$custom_agent_contract$envelopes)
  expect_length(args$llm$turns, 0L)
  local_mocked_bindings(summarize_models = function(...) invisible(NULL), .package = "finnts")
  record <- read_agent_custom_update(current, TRUE)
  expect_identical(record$predecessor$run_id, previous$run_id)
  expect_identical(record$winners$first$components, unique(forecast$Model_ID))
  local({
    next_args <- args
    next_args$input_data <- rbind(next_args$input_data, data.frame(Date = as.Date("2022-02-01"), series = "first", value = 101))
    next_run <- do.call(set_agent_info, next_args)
    update_forecast(next_run, allow_iterate_forecast = FALSE, num_cores = 1)
    expect_identical(read_agent_custom_update(next_run, TRUE)$predecessor$run_id, current$run_id)
    expect_equal(unique(get_agent_forecast(next_run)$Forecast[get_agent_forecast(next_run)$Train_Test_ID == 1]), 101)
  })
})

test_that("finalization preserves numeric-looking identities through both CSV rewrites", {
  for (mode in c("local", "global")) for (run_id in c("40607e5321822937", "4282d17137126405", "ordinary-run")) local({
    info <- list(project_info = list(
      project_name = "finalize-identity", path = withr::local_tempdir(), storage_object = NULL,
      data_output = "csv", object_output = "rds"
    ), run_id = run_id, max_iter = 3)
    parent <- info$project_info
    parent$run_name <- run_id
    row <- data.frame(
      combo = "00017", agent_run_id = run_id, best_run_name = "4282d17137126405", model_type = mode,
      weighted_mape = 0.125, max_iterations = 0, run_complete = FALSE
    )
    write_data(row, "00017", parent, "log", "logs", "-agent_best_run")
    path <- local_artifact_path(parent, "logs", "-agent_best_run", hash_data("00017"), "csv")
    before <- read_exact_artifact(parent, path, character_columns = c("combo", "agent_run_id", "best_run_name"))
    expect_identical(before$agent_run_id, run_id)
    finalize_run(info, combo = if (mode == "local")
      hash_data("00017")
    else NULL)
    after <- read_exact_artifact(parent, path, character_columns = c("combo", "agent_run_id", "best_run_name"))
    expect_identical(after$combo, "00017")
    expect_identical(after$agent_run_id, run_id)
    expect_identical(after$best_run_name, "4282d17137126405")
    expect_true(after$run_complete)
    expect_equal(after$max_iterations, 3)
    expect_equal(after$weighted_mape, 0.125)
    published <- load_best_agent_run(info)
    expect_equal(nrow(published), 1L)
    expect_identical(published$agent_run_id, run_id)
  })
})
