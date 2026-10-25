test_that("installed custom driver summaries match sequential and PSOCK execution", {
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace_path, "Meta", "package.rds")),
    "Custom summary workers require the installed namespace")
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  args <- custom_driver_agent(withr::local_tempdir(), series = 2L)
  args$run_global_models <- TRUE
  args$run_local_models <- FALSE
  model <- args$custom_models$driver_rule
  model$definition$requirements$historical_only_predictors <- "Driver"
  model$definition$source[["fit"]] <- paste0(
    "function(data, context, parameters) lapply(split(data, data$Combo), ",
    "function(x) x$Driver[[nrow(x)]])")
  model$definition$source[["predict"]] <- paste0(
    "function(object, new_data, context) data.frame(.finnts_row = new_data$.finnts_row, ",
    ".pred = vapply(new_data$Combo, function(id) object[[id]], numeric(1)))")
  model$definition$version_id <- custom_model_definition_digest(model$definition)
  model$approval$version_id <- model$validation$version_id <- model$definition$version_id
  args$custom_models$driver_rule <- model
  local_custom_update_runtime()
  info <- do.call(set_agent_info, args)
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  expected <- get_summarized_models(info)
  expect_true(any(expected$section == "importance"))
  expect_true(any(expected$section == "historical_sensitivity"))
  expect_true(any(expected$section == "historical_sensitivity_metadata"))
  expect_identical(names(expected), c("Combo", "Model_ID", "Model_Name", "Model_Type",
    "Best_Model", "section", "name", "value"))
  expect_setequal(unique(expected$Combo[expected$section == "historical_sensitivity"]),
    c("series1", "series2"))
  expect_true(any(expected$name == "method:Driver" & expected$section == "importance_metadata"))
  summarize_models(info, parallel_processing = "local_machine", num_cores = 2)
  actual <- get_summarized_models(info)
  # Canonicalize summary row order only for the sequential/worker comparison.
  # Input/output columns and values are preserved; no artifacts are modified.
  order_rows <- function(data) {
    data <- data[do.call(order, data), , drop = FALSE]
    rownames(data) <- NULL
    data
  }
  expect_equal(order_rows(actual), order_rows(expected))
})
