test_that("global custom replay preserves saved component subsets from one shared run", {
  fixture <- custom_update_fixture(TRUE)
  args <- fixture$args
  current <- fixture$current
  local_custom_update_runtime()
  expect_length(unique(get_best_agent_run(fixture$previous)$best_run_name), 1L)
  ids <- paste0("custom-", vapply(args$custom_models, function(model)
    model$definition$version_id, character(1)), "--global--C1")
  forecast <- get_agent_forecast(current)
  expect_equal(unique(forecast$Forecast[forecast$Train_Test_ID == 1]), 101)
  expect_setequal(unique(forecast$Model_ID[forecast$Combo == "first"]), c(ids, paste(sort(ids), collapse = "_")))
  expect_identical(unique(forecast$Model_ID[forecast$Combo == "second"]), ids[[1]])
  expect_length(unique(get_best_agent_run(current)$best_run_name), 1L)
  expect_false(any(grepl("^meanf--", forecast$Model_ID)))
  local_mocked_bindings(
    forecast_new_combos = function(...) stop("Unexpected global fallback"),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  args$input_data <- rbind(args$input_data, data.frame(
    Date = as.Date("2022-02-01"), series = c("first", "second"), value = 101))
  next_run <- do.call(set_agent_info, args)
  update_forecast(next_run, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_identical(read_agent_custom_update(next_run, TRUE)$predecessor$run_id, current$run_id)
  expect_length(unique(get_best_agent_run(next_run)$best_run_name), 1L)
})
test_that("new peers train global winners without inheriting global publication", {
  fixture <- custom_update_fixture(TRUE, updated = FALSE)
  local_custom_update_runtime()
  args <- fixture$args
  added <- args$input_data[args$input_data$series == "first", ]
  added$series <- "new-peer"
  added$value <- 110
  args$input_data <- rbind(args$input_data, added)
  current <- do.call(set_agent_info, args)
  prepared <- prepare_agent_custom_update(current, 123)
  routed <- initial_agent_custom_update(prepared)
  expect_setequal(routed$prev_best_runs_tbl$combo, c("first", "second"))
  expect_identical(routed$new_combos, hash_data("new-peer"))
  original_listing <- finnts:::list_files
  local_mocked_bindings(list_files = function(storage_object, path, ...) {
    if (any(grepl("input_data", path, fixed = TRUE))) stop("Known input paths must not be listed")
    original_listing(storage_object, path, ...)
  }, .package = "finnts")
  update_forecast_combo(prepared, routed$prev_best_runs_tbl, NULL, 1, FALSE, 123)
  best <- load_update_runs(prepared)
  expect_setequal(best$combo, c("first", "second"))
  expect_true(all(best$model_type == "global"))
  child <- current$project_info
  child$project_name <- paste0(child$project_name, "_", hash_data("all"))
  child$run_name <- best$best_run_name[[1]]
  rows <- read_update_result(child, c("first", "second"), TRUE, current$forecast_horizon)$forecasts
  expect_false("new-peer" %in% rows$Combo)
  expect_equal(unique(rows$Forecast[rows$Train_Test_ID == 1]), mean(args$input_data$value))
})

test_that("updates refit at a new horizon and audit predecessor with its own horizon", {
  fixture <- custom_update_fixture(TRUE, updated = FALSE)
  local_custom_update_runtime()
  args <- fixture$args
  args$forecast_horizon <- 3L
  current <- do.call(set_agent_info, args)
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  rows <- get_agent_forecast(current)
  future <- rows[rows$Train_Test_ID == 1 & rows$Best_Model == "Yes", ]
  expect_true(all(table(future$Combo) == 3L))
  expect_equal(unique(future$Forecast), mean(args$input_data$value))
  expect_no_error(audit_agent_custom_best(load_agent_custom_state(current), get_best_agent_run(current)))
  expect_no_error(update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1))
})
