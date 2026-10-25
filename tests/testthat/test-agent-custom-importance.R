# One fitted proportional rule with two held-out dates. Swapping drivers changes
# each prediction by two; five of ten cyclic permutations swap and five do not.
custom_importance_fixture <- function(mode = "local", model = custom_driver_model(), data = NULL) {
  if (is.null(data)) data <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 8),
    Combo = "one", Target = 2 * seq_len(8), Driver = seq_len(8))
  context <- list(date_type = "month", forecast_horizon = 2L)
  rows <- custom_run_workflows(model$definition, data, context)
  workflow <- rows$Model_Workflow[[match(mode, rows$Model_Type)]]
  list(model = model, workflow = generics::fit(workflow, data), data = data)
}

test_that("custom summaries report independently checked backtest permutation importance", {
  fixture <- custom_importance_fixture()
  before <- serialize(fixture$workflow, NULL)
  summary <- summarize_model_custom(fixture$workflow, fixture$model, "local")
  scores <- summary[summary$section == "importance", ]
  expect_identical(scores$name, "Driver")
  expect_equal(as.numeric(scores$value), 1)
  expect_true(any(grepl("rmse", summary$value, ignore.case = TRUE)))
  expect_false(any(grepl("historical_sensitivity", summary$section, fixed = TRUE)))
  expect_setequal(summary$name[summary$section == "importance_metadata"], c("method", "status"))
  expect_identical(serialize(fixture$workflow, NULL), before)
})

test_that("diagnostic fitting excludes holdout targets and preserves the RNG", {
  withr::local_preserve_seed()
  fixture <- custom_importance_fixture()
  original <- custom_model_fit_impl
  calls <- 0L
  local_mocked_bindings(custom_model_fit_impl = function(x, y, ...) {
    calls <<- calls + 1L
    expect_equal(nrow(x), 6L)
    expect_false("Target" %in% names(x))
    expect_equal(y, 2 * (1:6))
    original(x, y, ...)
  }, .package = "finnts")
  set.seed(19)
  before <- .Random.seed
  summarize_model_custom(fixture$workflow, fixture$model, "local")
  expect_identical(.Random.seed, before)
  expect_equal(calls, 1L)
})

test_that("unestimable custom importance is explicit and does not invent zero", {
  fixture <- custom_importance_fixture()
  short <- custom_importance_fixture(data = fixture$data[1:2, ])$workflow
  expect_warning(summary <- summarize_model_custom(short, fixture$model, "local"), "insufficient")
  expect_false(any(summary$section == "importance"))
  data <- fixture$data
  data$Driver <- 1
  constant <- custom_importance_fixture(data = data)
  local_mocked_bindings(custom_model_fit_impl = function(...) stop("unexpected diagnostic fit"), .package = "finnts")
  expect_warning(summary <- summarize_model_custom(constant$workflow, constant$model, "local"), "constant")
  expect_false(any(summary$section == "importance"))
})

test_that("custom models with no drivers keep metadata without diagnostic execution", {
  model <- standard_custom_model()
  data <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 8),
    Combo = "one", Target = 1:8)
  fixture <- custom_importance_fixture(model = model, data = data)
  local_mocked_bindings(custom_model_fit_impl = function(...) stop("unexpected fit"),
    custom_model_predict_impl = function(...) stop("unexpected prediction"), .package = "finnts")
  summary <- summarize_model_custom(fixture$workflow, model, "local")
  expect_true("custom_model" %in% summary$section)
  expect_false(any(grepl("importance", summary$section)))
})

test_that("negative importance is retained instead of clipped or omitted", {
  model <- custom_driver_model()
  model$definition$source[["predict"]] <- "function(object,new_data,context) data.frame(.finnts_row=new_data$.finnts_row,.pred=-2*new_data$Driver)"
  model$definition$version_id <- custom_model_definition_digest(model$definition)
  model$approval$version_id <- model$validation$version_id <- model$definition$version_id
  fixture <- custom_importance_fixture(model = model)
  summary <- summarize_model_custom(fixture$workflow, fixture$model, "local")
  expect_equal(as.numeric(summary$value[summary$section == "importance"]), (30 - sqrt(904)) / 2)
})

test_that("grouped importance preserves full cohorts and never permutes group keys", {
  model <- custom_driver_model()
  model$definition$requirements$predictors <- c("Driver", "Group")
  model$definition$requirements$runtime$cohort <- "groups"
  model$definition$requirements$runtime$group_columns <- "Group"
  model$definition$source[["predict"]] <- paste0(
    "function(object,new_data,context) { stopifnot(setequal(unique(new_data$Combo),c('one','two','three'))); ",
    "stopifnot(all(as.character(new_data$Group)==ifelse(new_data$Combo=='three','second','first'))); ",
    "data.frame(.finnts_row=new_data$.finnts_row,.pred=object*new_data$Driver) }")
  model$definition$version_id <- custom_model_definition_digest(model$definition)
  model$approval$version_id <- model$validation$version_id <- model$definition$version_id
  data <- custom_importance_fixture()$data
  data$Group <- "first"
  data <- rbind(data, transform(data, Combo = "two"), transform(data, Combo = "three", Group = "second"))
  fixture <- custom_importance_fixture("global", model, data)
  expect_warning(summary <- summarize_model_custom(fixture$workflow, model, "global", combo = "one"), "cohort-key")
  expect_identical(summary$name[summary$section == "importance"], "Driver")
  expect_equal(as.numeric(summary$value[summary$section == "importance"]), 1)
  wrong <- model
  wrong$approval$allow_code <- FALSE
  local_mocked_bindings(custom_summary_importance = function(...) stop("unexpected execution"), .package = "finnts")
  expect_error(summarize_model_custom(fixture$workflow, wrong, "global"), "approval|consent|allow_code")
})

test_that("unavailable holdout lags and gaps are disclosed before source execution", {
  model <- custom_driver_model()
  model$definition$requirements$history$required_lag_periods <- 1L
  model$definition$version_id <- custom_model_definition_digest(model$definition)
  model$approval$version_id <- model$validation$version_id <- model$definition$version_id
  fixture <- custom_importance_fixture(model = model)
  expect_warning(summary <- summarize_model_custom(fixture$workflow, model, "local"), "lags")
  expect_false(any(summary$section == "importance"))
  fixture <- custom_importance_fixture(data = custom_importance_fixture()$data[-6, ])
  expect_warning(summary <- summarize_model_custom(fixture$workflow, fixture$model, "local"), "cadence")
  expect_false(any(summary$section == "importance"))
})

test_that("global custom importance is reused only within the supplied memory cache", {
  fixture <- custom_importance_fixture("global")
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  original <- custom_model_fit_impl
  local_mocked_bindings(custom_model_fit_impl = function(...) {
    calls <<- calls + 1L
    original(...)
  }, .package = "finnts")
  first <- summarize_model_custom(fixture$workflow, fixture$model, "global", combo = "one", cache = cache)
  second <- summarize_model_custom(fixture$workflow, fixture$model, "global", combo = "one", cache = cache)
  expect_equal(first, second)
  expect_equal(calls, 1L)
})

test_that("public global summaries share one diagnostic fit across recipients", {
  args <- custom_driver_agent(withr::local_tempdir(), series = 2L)
  args$run_global_models <- TRUE
  args$run_local_models <- FALSE
  local_custom_update_runtime()
  info <- do.call(set_agent_info, args)
  iterate_forecast(info, max_iter = 1, weighted_mape_goal = 0.01, num_cores = 1)
  calls <- 0L
  original <- custom_model_fit_impl
  local_mocked_bindings(custom_model_fit_impl = function(...) {
    calls <<- calls + 1L
    original(...)
  }, .package = "finnts")
  summarize_models(info, num_cores = 1)
  summary <- get_summarized_models(info)
  expect_setequal(unique(summary$Combo[summary$section == "importance"]), c("series1", "series2"))
  expect_equal(calls, 1L)
  expect_identical(names(summary), c("Combo", "Model_ID", "Model_Name", "Model_Type", "Best_Model",
    "section", "name", "value"))
})
