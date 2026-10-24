test_that("normal local/global workflows predict without fit-only columns after RDS", {
  for (mode in c("local", "global")) {
    definition <- historical_driver_definition(mode)
    panel <- historical_driver_panel(mode == "global")
    context <- list(date_type = "month", forecast_horizon = 2)
    workflow <- custom_run_workflows(definition, panel$history, context)$Model_Workflow[[1]]
    fitted <- generics::fit(workflow, panel$history)
    expect_equal(predict(fitted, panel$future)$.pred, panel$expected, tolerance = 1e-8)
    supplied <- panel$future
    supplied$Units <- seq_len(nrow(supplied)) * 999
    expect_identical(predict(fitted, supplied), predict(fitted, panel$future))
    path <- withr::local_tempfile(fileext = ".rds")
    saveRDS(fitted, path)
    expect_identical(predict(readRDS(path), panel$future), predict(fitted, panel$future))
    order <- rev(seq_len(nrow(panel$future)))
    expect_equal(predict(fitted, panel$future[order, ])$.pred, panel$expected[order], tolerance = 1e-8)
  }
})

test_that("four finance rules author, fit and roundtrip through real workflows", {
  local_mocked_bindings(check_agent_ellmer_version = function() invisible(NULL),
    new_llm_session = function(llm) list(), .package = "finnts")
  for (family in c("customer_growth", "unit_trend", "productivity", "integrated")) {
    for (global in c(FALSE, TRUE)) {
      case <- historical_finance_case(family, global)
      proposal <- author_proposal()
      proposal$name <- paste0("finance_", family)
      proposal$interpretation <- case$instructions
      proposal$historical_only_predictors <- case$historical
      proposal$defaults_used <- character()
      if (global) proposal$prediction_cohort <- list(scope = "series", group_columns = character())
      source <- list(status = "candidate", conflict = "", fit_body = case$fit,
        predict_body = case$predict, helpers = list())
      local_mocked_bindings(custom_author_request = function(session, stage, payload)
        if (stage == "contract") proposal else source, .package = "finnts")
      raw <- case$history
      future <- case$future
      future$Target <- NA_real_
      raw <- dplyr::bind_rows(raw, future)
      model <- suppressMessages(create_custom_model(case$instructions, list(), input_data = raw,
        combo_variables = "Combo", target_variable = "Target", date_type = "month",
        forecast_horizon = 2, external_regressors = case$predictors,
        model_type = case$mode, approval = "automatic",
        validation_examples = custom_examples_table(historical_finance_examples(case)),
        control = custom_model_control(package_policy = "base_only")))
      expect_s3_class(model, "finnts_custom_model")
      expect_setequal(model$definition$requirements$historical_only_predictors, case$historical)
      workflow <- custom_run_workflows(model$definition, case$history,
        list(date_type = "month", forecast_horizon = 2))$Model_Workflow[[1]]
      fitted <- generics::fit(workflow, case$history)
      expect_equal(predict(fitted, case$future)$.pred, case$expected, tolerance = 1e-8)
      path <- withr::local_tempfile(fileext = ".rds")
      saveRDS(fitted, path)
      expect_identical(predict(readRDS(path), case$future), predict(fitted, case$future))
    }
  }
})

test_that("real preparation, resampling and saved Agent updates work without future units", {
  args <- custom_driver_agent(withr::local_tempdir())
  args$custom_models <- list(unit_growth = historical_driver_model())
  args$input_data$Units <- c(rep(110, 24), NA, NA)
  args$input_data$Price <- c(rep(10, 24), 12, 13)
  args$input_data$value <- c(rep(1100, 24), NA, NA)
  args$llm <- custom_agent_chat(custom_agent_response("unit_growth"))
  args$external_regressors <- NULL
  local_custom_update_runtime()
  local_mocked_bindings(get_eda_data = function(...) data.frame(done = TRUE),
    .package = "finnts")
  previous <- do.call(set_agent_info, args)
  custom_update_eda(previous)
  iterate_forecast(previous, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  best <- get_best_agent_run(previous)
  child <- previous$project_info
  child$project_name <- paste0(child$project_name, "_", hash_data("series1"))
  child$run_name <- best$best_run_name[[1]]
  models <- read_exact_artifact(child, local_artifact_path(child, "models",
    "-single_models", hash_data("series1"), extension = "rds"), return_type = "object")
  expect_identical(unique(models$Model_Name), "unit_growth")
  expect_true(all(vapply(models$Model_Fit, inherits, logical(1), "finnts_custom_workflow")))
  initial <- get_agent_forecast(previous)
  rows <- initial[which(initial$Model_Name == "unit_growth" & initial$Train_Test_ID == 1), ]
  expect_equal(rows$Forecast, c(1452, 1730.3), tolerance = 1e-8)
  local_mocked_bindings(summarize_models = function(...) invisible(NULL), .package = "finnts")
  args$overwrite <- TRUE
  current <- do.call(set_agent_info, args)
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  forecast <- get_agent_forecast(current)
  rows <- forecast[which(forecast$Model_Name == "unit_growth" & forecast$Train_Test_ID == 1), ]
  expect_equal(rows$Forecast, c(1452, 1730.3), tolerance = 1e-8)
  expect_no_error(audit_agent_custom_best(load_agent_custom_state(current), get_best_agent_run(current)))
  local_mocked_bindings(fit_models = function(...) stop("Completed update must not refit"),
    .package = "finnts")
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_identical(get_agent_forecast(current), forecast)
  args$input_data$Units <- NULL
  expect_error(do.call(set_agent_info, args), "unit_growth.*Units")
})
