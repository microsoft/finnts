test_that("installed custom driver summaries match sequential and PSOCK execution", {
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace_path, "Meta", "package.rds")),
    "Custom summary workers require the installed namespace")
  withr::local_envvar(c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  args <- custom_driver_agent(withr::local_tempdir(), series = 2L)
  args$run_global_models <- TRUE
  args$run_local_models <- FALSE
  local_custom_update_runtime()
  info <- do.call(set_agent_info, args)
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  expected <- get_summarized_models(info)
  expect_true(any(expected$section == "importance"))
  summarize_models(info, parallel_processing = "local_machine", num_cores = 2)
  actual <- get_summarized_models(info)
  # Compare summary rows independently of worker completion order, without
  # changing any score, identity, column or stored value.
  order_rows <- function(data) {
    data <- data[do.call(order, data), , drop = FALSE]
    rownames(data) <- NULL
    data
  }
  expect_equal(order_rows(actual), order_rows(expected))
})
