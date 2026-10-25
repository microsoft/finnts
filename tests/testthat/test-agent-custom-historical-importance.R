# Fit a pinned last/mean historical rule on three analysis and two holdout rows.
# Literal holdout targets are independent of source execution; price is optional.
historical_importance_fixture <- function(rule = "last", values = c(1, 2, 3, 3, 3),
    target = rep(3, 5), price = NULL, mode = "local", peer = FALSE,
    type = "number", fit_source = NULL, predict_expression = NULL, cohort_key = FALSE,
    required_lags = integer()) {
  if (is.null(fit_source)) fit_source <- paste0(
    "function(data, context, parameters) lapply(split(data, data$Combo), function(x) ",
    switch(rule, last = "x$History[[nrow(x)]]", mean = "mean(x$History)",
      lag = "x$History[[nrow(x) - 1L]]",
      irrelevant = "3", categorical = "as.numeric(x$History[[nrow(x)]] == 'high')"), ")")
  if (is.null(predict_expression)) predict_expression <- paste0(
    "vapply(new_data$Combo, function(id) object[[id]], numeric(1))",
    if (!is.null(price)) " + new_data$Price" else "")
  predictors <- c("History", if (!is.null(price)) "Price")
  definition <- new_custom_model_definition("historical_importance",
    "Fixed historical rule for independent importance arithmetic.",
    "History is historical-only. Price, if declared, is supplied in the future.",
    c("local", "global"), c(fit = fit_source, predict = paste0(
      "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row, .pred=",
      predict_expression, ")")),
    list(predictors = predictors, historical_only_predictors = "History",
      predictor_types = c(list(History = type), if (!is.null(price)) list(Price = "number")),
      recipes = "C1", target_scale = "original", date_types = "month",
      forecast_horizon = 2, missing_data = "error",
      history = list(minimum_rows_per_series = 1L, required_lag_periods = required_lags),
      runtime = list(prediction_scope = "complete_horizon",
        cohort = if (cohort_key) "groups" else "series",
        group_columns = if (cohort_key) "History" else character())))
  model <- structure(list(definition = definition,
    validation = list(version_id = definition$version_id, technical_passed = TRUE,
      checks = list(arithmetic = "Literal holdout arithmetic")),
    approval = list(version_id = definition$version_id, intent_confirmed = TRUE,
      allow_code = TRUE)), class = "finnts_custom_model")
  data <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 5),
    Combo = "one", Target = target, History = values)
  if (!is.null(price)) data$Price <- price
  if (peer) data <- rbind(data, transform(data, Combo = "two"))
  rows <- custom_run_workflows(definition, data, list(date_type = "month", forecast_horizon = 2L))
  workflow <- generics::fit(rows$Model_Workflow[[match(mode, rows$Model_Type)]], data)
  list(model = model, workflow = workflow, data = data)
}

# Select a single numeric diagnostic by section/driver, retaining missing rows.
historical_importance_value <- function(summary, section, driver = "History") {
  as.numeric(summary$value[summary$section == section & summary$name == driver])
}

test_that("historical last-value importance refits history, not discarded future cells", {
  fixture <- historical_importance_fixture()
  future <- data.frame(Date = as.Date(c("2020-06-01", "2020-07-01")), Combo = "one")
  expect_equal(predict(fixture$workflow, future)$.pred, c(3, 3))
  before <- serialize(fixture$workflow, NULL)
  summary <- summarize_model_custom(fixture$workflow, fixture$model, "local")
  expect_equal(historical_importance_value(summary, "importance"), 1.1)
  expect_equal(historical_importance_value(summary, "historical_sensitivity"), 0.3)
  expect_match(summary$value[summary$section == "importance_metadata" &
    summary$name == "method:History"], "historical|history", ignore.case = TRUE)
  expect_identical(serialize(fixture$workflow, NULL), before)
})

test_that("history mean dependence is distinguished from history order dependence", {
  fixture <- historical_importance_fixture("mean", target = rep(2, 5))
  future <- data.frame(Date = as.Date(c("2020-06-01", "2020-07-01")), Combo = "one")
  expect_equal(predict(fixture$workflow, future)$.pred, c(2.4, 2.4))
  summary <- summarize_model_custom(fixture$workflow, fixture$model, "local")
  expect_equal(historical_importance_value(summary, "importance"), 0)
  expect_equal(historical_importance_value(summary, "historical_sensitivity"), 0.2)
})

test_that("constant and zero history have honest diagnostic applicability", {
  fixture <- historical_importance_fixture(values = rep(5, 5), target = rep(5, 5))
  expect_warning(summary <- summarize_model_custom(fixture$workflow, fixture$model, "local"), "constant")
  expect_length(historical_importance_value(summary, "importance"), 0)
  expect_equal(historical_importance_value(summary, "historical_sensitivity"), 0.5)
  fixture <- historical_importance_fixture(values = rep(0, 5), target = rep(0, 5))
  expect_warning(expect_warning(
    summary <- summarize_model_custom(fixture$workflow, fixture$model, "local"),
    "all-zero"), "constant")
  expect_length(historical_importance_value(summary, "historical_sensitivity"), 0)
  expect_true(any(grepl("zero", summary$value)))
})

test_that("mixed custom drivers retain assessment permutation and separate historical scores", {
  fixture <- historical_importance_fixture(price = 1:5, target = c(4, 5, 6, 7, 8))
  summary <- summarize_model_custom(fixture$workflow, fixture$model, "local")
  expect_equal(historical_importance_value(summary, "importance", "Price"), 0.5)
  expect_equal(historical_importance_value(summary, "importance"), 1.1)
  expect_equal(historical_importance_value(summary, "historical_sensitivity"), 0.3)
  expect_length(historical_importance_value(summary, "historical_sensitivity", "Price"), 0)
})

test_that("historical diagnostics preserve analysis targets, RNG and cached global cohorts", {
  fixture <- historical_importance_fixture(mode = "global", peer = TRUE)
  original_fit <- custom_model_fit_impl
  original_predict <- custom_model_predict_impl
  calls <- 0L
  local_mocked_bindings(custom_model_fit_impl = function(x, y, ...) {
    calls <<- calls + 1L
    expect_setequal(unique(x$Combo), c("one", "two"))
    expect_equal(x$Date, fixture$data$Date[c(1:3, 6:8)])
    expect_equal(y, rep(3, 6))
    expect_false("Target" %in% names(x))
    original_fit(x, y, ...)
  }, custom_model_predict_impl = function(object, new_data) {
    expect_setequal(unique(new_data$Combo), c("one", "two"))
    expect_false("Target" %in% names(new_data))
    original_predict(object, new_data)
  }, .package = "finnts")
  withr::local_preserve_seed()
  set.seed(81)
  seed <- .Random.seed
  cache <- new.env(parent = emptyenv())
  first <- summarize_model_custom(fixture$workflow, fixture$model, "global", "one", cache)
  second <- summarize_model_custom(fixture$workflow, fixture$model, "global", "two", cache)
  expect_equal(first, second)
  expect_identical(.Random.seed, seed)
  expect_lte(calls, 13L)
  expect_equal(calls, 5L)
})

test_that("historical permutation supports categorical drivers without numeric coercion", {
  fixture <- historical_importance_fixture("categorical",
    values = c("low", "low", "high", "high", "high"), target = rep(1, 5), type = "string")
  expect_warning(summary <- summarize_model_custom(fixture$workflow, fixture$model, "local"), "numeric|scaling")
  expect_equal(historical_importance_value(summary, "importance"), 0.7)
  expect_length(historical_importance_value(summary, "historical_sensitivity"), 0)
})

test_that("historical perturbation errors remain visible instead of selective scores", {
  source <- paste0("function(data, context, parameters) { ",
    "if (any(data$History > 3)) stop('History exceeds approved domain'); ",
    "lapply(split(data, data$Combo), function(x) x$History[[nrow(x)]]) }")
  fixture <- historical_importance_fixture(values = c(1, 2, 3, 3, 3), fit_source = source)
  expect_error(summarize_model_custom(fixture$workflow, fixture$model, "local"), "approved domain")
})

test_that("historical irrelevant and negative scores are not fabricated or clipped", {
  fixture <- historical_importance_fixture("irrelevant")
  summary <- summarize_model_custom(fixture$workflow, fixture$model, "local")
  expect_equal(historical_importance_value(summary, "importance"), 0)
  expect_equal(historical_importance_value(summary, "historical_sensitivity"), 0)
  fixture <- historical_importance_fixture(target = rep(0, 5))
  summary <- summarize_model_custom(fixture$workflow, fixture$model, "local")
  expect_equal(historical_importance_value(summary, "importance"), -1.1)
})

test_that("Agent column cards distinguish historical sensitivity and RMSE importance", {
  cards <- get_column_cards("get_summarized_models")
  expect_match(cards, "historical_sensitivity", fixed = TRUE)
  expect_match(cards, "method:<driver>", fixed = TRUE)
  expect_match(cards, "not.*causal|not causal", ignore.case = TRUE)
})

test_that("historical assessment values do not enter historical diagnostics", {
  fixture <- historical_importance_fixture(values = c(1, 2, 3, 999, 888),
    predict_expression = paste0("{ stopifnot(!('History' %in% names(new_data))); ",
      "vapply(new_data$Combo, function(id) object[[id]], numeric(1)) }"))
  summary <- summarize_model_custom(fixture$workflow, fixture$model, "local")
  expect_equal(historical_importance_value(summary, "importance"), 1.1)
  expect_equal(historical_importance_value(summary, "historical_sensitivity"), 0.3)
})

test_that("historical holdout guards disclose both diagnostic methods before refitting", {
  fixture <- historical_importance_fixture()
  data <- fixture$data[1:2, ]
  rows <- custom_run_workflows(fixture$model$definition, data,
    list(date_type = "month", forecast_horizon = 2L))
  short <- generics::fit(rows$Model_Workflow[[1]], data)
  data <- fixture$data[-3, ]
  rows <- custom_run_workflows(fixture$model$definition, data,
    list(date_type = "month", forecast_horizon = 2L))
  gap <- generics::fit(rows$Model_Workflow[[1]], data)
  lagged <- historical_importance_fixture(required_lags = 1L)
  local_mocked_bindings(custom_model_fit_impl = function(...) stop("unexpected diagnostic fit"),
    .package = "finnts")
  expect_warning(summary <- summarize_model_custom(short, fixture$model, "local"), "insufficient")
  expect_true("historical_sensitivity_metadata" %in% summary$section)
  expect_false(any(summary$section %in% c("importance", "historical_sensitivity")))
  expect_warning(summary <- summarize_model_custom(gap, fixture$model, "local"), "cadence")
  expect_false(any(summary$section %in% c("importance", "historical_sensitivity")))
  expect_warning(summary <- summarize_model_custom(lagged$workflow, lagged$model, "local"), "lags")
  expect_false(any(summary$section %in% c("importance", "historical_sensitivity")))
})

test_that("cohort keys cannot be declared historical-only diagnostic drivers", {
  local_mocked_bindings(custom_model_fit_impl = function(...) stop("unexpected diagnostic fit"),
    .package = "finnts")
  expect_error(historical_importance_fixture(values = rep(5, 5), target = rep(5, 5),
    mode = "global", peer = TRUE, cohort_key = TRUE), "nongroup drivers")
})

test_that("historical nonfinite predictions and missing consent remain errors", {
  fixture <- historical_importance_fixture(predict_expression = paste0(
    "{ value <- vapply(new_data$Combo, function(id) object[[id]], numeric(1)); ",
    "ifelse(value > 3, Inf, value) }"))
  expect_error(summarize_model_custom(fixture$workflow, fixture$model, "local"), "finite")
  fixture <- historical_importance_fixture()
  fixture$model$approval$allow_code <- FALSE
  expect_error(summarize_model_custom(fixture$workflow, fixture$model, "local"), "approval|consent|allow_code")
})

test_that("stored observed lag dependence has an independent history-refit oracle", {
  fixture <- historical_importance_fixture("lag", target = rep(2, 5))
  summary <- summarize_model_custom(fixture$workflow, fixture$model, "local")
  expect_equal(historical_importance_value(summary, "importance"), 0.7)
  expect_equal(historical_importance_value(summary, "historical_sensitivity"), 0.2)
})

test_that("grouped global historical diagnostics keep distinct peer histories and group keys", {
  fixture <- historical_importance_fixture(mode = "global", peer = TRUE)
  definition <- fixture$model$definition
  definition$requirements$predictors <- c("History", "Group")
  definition$requirements$predictor_types$Group <- "string"
  definition$requirements$runtime$cohort <- "groups"
  definition$requirements$runtime$group_columns <- "Group"
  definition$source[["predict"]] <- paste0(
    "function(object,new_data,context) { ",
    "stopifnot(setequal(unique(new_data$Combo),c('one','two'))); ",
    "stopifnot(all(new_data$Group == 'region')); ",
    "data.frame(.finnts_row=new_data$.finnts_row, ",
    ".pred=vapply(new_data$Combo,function(id) object[[id]],numeric(1))) }")
  definition$version_id <- custom_model_definition_digest(definition)
  model <- fixture$model
  model$definition <- definition
  model$approval$version_id <- model$validation$version_id <- definition$version_id
  data <- fixture$data
  data$Group <- "region"
  data$History[data$Combo == "two"] <- data$History[data$Combo == "two"] * 10
  data$Target[data$Combo == "two"] <- 30
  rows <- custom_run_workflows(definition, data,
    list(date_type = "month", forecast_horizon = 2L))
  workflow <- generics::fit(rows$Model_Workflow[[match("global", rows$Model_Type)]], data)
  expect_warning(summary <- summarize_model_custom(workflow, model, "global"), "cohort-key")
  expect_equal(historical_importance_value(summary, "importance"), c(1.1, 11))
  expect_equal(historical_importance_value(summary, "historical_sensitivity"), c(0.3, 3))
  expect_length(historical_importance_value(summary, "importance", "Group"), 0)
})
