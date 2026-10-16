test_that("completed custom updates restart without new fitting or artifact changes", {
  fixture <- custom_update_fixture()
  current <- fixture$current
  local_custom_update_runtime()
  local_mocked_bindings(
    fit_models = function(...) stop("Unexpected update refit"),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  before <- tools::md5sum(list.files(fixture$args$project_info$path,
    recursive = TRUE, full.names = TRUE))
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_equal(get_agent_forecast(current), fixture$forecast)
  expect_identical(tools::md5sum(names(before)), before)
})

test_that("interrupted custom completion reuses fitted and forecast artifacts", {
  fixture <- custom_update_fixture()
  current <- fixture$current
  local_custom_update_runtime()
  best <- get_best_agent_run(current)
  original_read <- finnts:::read_selection_file
  calls <- 0L
  local_mocked_bindings(
    load_update_runs = function(...) tibble::tibble(),
    read_selection_file = function(run_info, folder, suffix = NULL,
      combo = NULL, optional = FALSE, ...) {
      if (identical(suffix, "-agent_best_run") &&
        identical(run_info$run_name, current$run_id) && optional && calls == 0L) {
        calls <<- calls + 1L
        return(tibble::tibble())
      }
      original_read(run_info, folder, suffix, combo, optional, ...)
    },
    fit_models = function(...) stop("Unexpected interrupted-tail refit"),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  artifacts <- list.files(fixture$args$project_info$path, recursive = TRUE, full.names = TRUE)
  artifacts <- artifacts[grepl("/(models|forecasts)/", gsub("\\\\", "/", artifacts))]
  before <- tools::md5sum(artifacts)
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_equal(calls, 1L)
  expect_identical(tools::md5sum(artifacts), before)
  expect_equal(get_best_agent_run(current), best)
})

test_that("partial global completion reuses the one shared fit for all series", {
  fixture <- custom_update_fixture(TRUE)
  current <- fixture$current
  local_custom_update_runtime()
  prepared <- prepare_agent_custom_update(current, 123)
  best <- get_best_agent_run(current)
  original_load <- finnts:::load_update_runs
  local_mocked_bindings(
    load_update_runs = function(...) original_load(...)[1, , drop = FALSE],
    fit_models = function(...) stop("Unexpected shared refit"),
    summarize_models = function(...) invisible(NULL), .package = "finnts")
  routed <- initial_agent_custom_update(prepared)
  expect_setequal(routed$prev_best_runs_tbl$combo, c("first", "second"))
  update_forecast(current, allow_iterate_forecast = FALSE, num_cores = 1)
  expect_equal(get_agent_forecast(current), fixture$forecast)
  wrong <- best
  wrong$combo[[1]] <- "foreign-series"
  expect_error(audit_agent_custom_best(prepared, wrong), "winner choice")
})
