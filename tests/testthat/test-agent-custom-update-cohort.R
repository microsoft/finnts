test_that("a changed global cohort recovers through genuine compatible custom defaults", {
  fixture <- custom_update_fixture(TRUE, updated = FALSE)
  local_custom_update_runtime()
  local_custom_default_grid()
  args <- fixture$args
  args$input_data$series[args$input_data$series == "first"] <- "replacement"
  args$input_data$value <- 100
  current <- do.call(set_agent_info, args)
  original_prep <- finnts:::prep_models
  original_submit <- finnts:::submit_fcst_run
  submitted <- NULL
  # Bound only the tuning grid; all default/custom engines still fit and predict.
  local_mocked_bindings(get_foundation_model_suffix = function(...) "",
    summarize_models = function(...) invisible(NULL),
    prep_models = function(..., num_hyperparameters = 20) original_prep(..., num_hyperparameters = 1),
    submit_fcst_run = function(agent_info, inputs, ...) {
      submitted <<- inputs
      original_submit(agent_info, inputs, ...)
    },
    update_forecast_combo = function(...) stop("Changed global cohort must not replay saved fits"),
    .package = "finnts")
  prepared <- prepare_agent_custom_update(current, 123)
  routed <- initial_agent_custom_update(prepared)
  expect_equal(nrow(routed$prev_best_runs_tbl), 0L)
  expect_setequal(routed$new_combos, vapply(c("replacement", "second"), hash_data, character(1)))
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_identical(submitted$models_to_run[1:2], names(args$custom_models))
  actual <- get_agent_forecast(current)
  ids <- paste0("custom-", vapply(args$custom_models, function(model)
    model$definition$version_id, character(1)), "--local--C1")
  custom <- actual[actual$Model_ID %in% ids & actual$Train_Test_ID == 1, ]
  expect_setequal(custom$Model_ID, ids)
  expect_equal(custom$Forecast, rep(100, nrow(custom)))
  expect_true(all(get_best_agent_run(current)$model_type == "local"))
  expect_no_error(audit_agent_custom_best(load_agent_custom_state(current), get_best_agent_run(current)))
})
