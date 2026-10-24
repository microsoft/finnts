test_that("historical-driver workflow dispatch survives installed two-worker replay", {
  namespace <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace, "Meta", "package.rds")),
    "Historical workers require the installed namespace")
  panel <- historical_driver_panel(TRUE)
  workflows <- lapply(c("local", "global"), function(mode) {
    definition <- historical_driver_definition(mode)
    history <- if (mode == "local") panel$history[panel$history$Combo == "A", ] else panel$history
    workflow <- custom_run_workflows(definition, history,
      list(date_type = "month", forecast_horizon = 2))$Model_Workflow[[1]]
    generics::fit(workflow, history)
  })
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(workflows, path)
  # Workers read the serialized workflows and dispatch through the installed
  # namespace, without copying development functions into their global scope.
  replay <- function(path, future, namespace, libraries) {
    .libPaths(libraries)
    loadNamespace("finnts")
    stopifnot(normalizePath(getNamespaceInfo(asNamespace("finnts"), "path")) == normalizePath(namespace))
    objects <- readRDS(path)
    list(local = stats::predict(objects[[1]], future[future$Combo == "A", ])$.pred,
      global = stats::predict(objects[[2]], future)$.pred)
  }
  environment(replay) <- baseenv()
  cluster <- parallel::makePSOCKcluster(2L)
  withr::defer(parallel::stopCluster(cluster))
  results <- parallel::clusterCall(cluster, replay, path, panel$future, namespace, .libPaths())
  expect_length(results, 2)
  for (result in results) {
    expect_equal(result$local, c(1452, 1730.3), tolerance = 1e-8)
    expect_equal(result$global, c(1452, 1730.3, 2904, 3460.6), tolerance = 1e-8)
  }
})
