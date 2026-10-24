test_that("historical-only definitions are explicit, immutable and hash bound", {
  definition <- historical_driver_definition()
  expect_identical(definition$requirements$historical_only_predictors, "Units")
  expect_no_error(custom_model_current_definition(definition))
  legacy <- historical_driver_definition(historical = FALSE)
  expect_false(identical(legacy$version_id, definition$version_id))
  original <- serialize(legacy, NULL)
  expect_no_error(custom_model_current_definition(legacy))
  expect_identical(serialize(legacy, NULL), original)
  invalid <- definition
  invalid$requirements$historical_only_predictors <- "Unknown"
  expect_error(validate_custom_model_definition(invalid), "historical")
  invalid$requirements$historical_only_predictors <- c("Units", "Units")
  expect_error(validate_custom_model_definition(invalid), "historical")
})

test_that("phase-aware checks retain fit drivers and require only real future inputs", {
  definition <- historical_driver_definition()
  panel <- historical_driver_panel()
  context <- list(model_type = "local", recipe_id = "C1", date_type = "month",
    forecast_horizon = 2, target_scale = "original")
  fit <- custom_model_fit_impl(panel$history[c("Date", "Combo", "Units", "Price")],
    panel$history$Target, definition, context, TRUE)
  expect_equal(custom_model_predict_impl(fit, panel$future), panel$expected, tolerance = 1e-8)
  supplied <- panel$future
  supplied$Units <- c("not a future input", "ignored")
  expect_identical(custom_model_predict_impl(fit, supplied),
    custom_model_predict_impl(fit, panel$future))
  bad <- panel$future
  bad$Price <- NULL
  expect_error(custom_model_predict_impl(fit, bad), "required predictors")
  bad$Price <- c(NA_real_, 13)
  expect_error(custom_model_predict_impl(fit, bad), "Price")
  bad$Price <- c("12", "13")
  expect_error(custom_model_predict_impl(fit, bad), "type.*Price")
  history <- panel$history
  history$Units[[1]] <- NA_real_
  expect_error(custom_model_fit_impl(history[c("Date", "Combo", "Units", "Price")],
    history$Target, definition, context, TRUE), "Units")
})

test_that("legacy drivers still require future values", {
  panel <- historical_driver_panel()
  legacy <- historical_driver_definition(historical = FALSE)
  expect_error(custom_author_examples(list(), legacy, list()), "two to eight")
  context <- list(model_type = "local", recipe_id = "C1", date_type = "month",
    forecast_horizon = 2, target_scale = "original")
  fit <- custom_model_fit_impl(panel$history[c("Date", "Combo", "Units", "Price")],
    panel$history$Target, legacy, context, TRUE)
  expect_error(custom_model_predict_impl(fit, panel$future), "required predictors")
})

test_that("availability-aware table parsing permits blank future history-only cells", {
  panel <- historical_driver_panel()
  example <- list(name = "ordinary", model_type = "local", history = panel$history,
    new_data = transform(panel$future, Units = NA_real_), expected = transform(panel$future, .pred = panel$expected)[c("Date", "Combo", ".pred")],
    tolerance = 1e-8, edge_case = FALSE, rationale = "Literal ten percent growth calculation.",
    outcome = "predictions", expected_error = NULL)
  edge <- example
  edge$name <- "edge"
  edge$edge_case <- TRUE
  table <- custom_examples_table(list(example, edge))
  expect_error(custom_examples_from_table(table, "Target", NULL, c("Units", "Price"), "local"),
    "Units.*filled")
  parsed <- custom_examples_from_table(table, "Target", NULL, c("Units", "Price"), "local",
    historical_only_predictors = "Units")
  expect_false("Units" %in% names(parsed[[1]]$new_data))
  expect_identical(parsed[[1]]$history$Units, c(100, 110))
  expect_equal(parsed[[1]]$expected$.pred, panel$expected)
  bad <- table
  bad$Price[is.na(bad$Target)] <- NA_real_
  expect_error(custom_examples_from_table(bad, "Target", NULL, c("Units", "Price"), "local",
    historical_only_predictors = "Units"), "Price.*filled")
})

test_that("new authoring freezes availability before validating future inputs", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  panel <- historical_driver_panel()
  raw <- panel$history
  names(raw)[names(raw) == "Target"] <- "value"
  metadata <- list(date_type = "month", forecast_horizon = 2,
    schema = list(Units = "numeric", Price = "numeric"), available_packages = "base")
  protocol <- custom_author_protocol(metadata, "local", c("Units", "Price"), "base_only", TRUE)
  example <- list(name = "ordinary", model_type = "local", history = panel$history,
    new_data = panel$future, expected = transform(panel$future, .pred = panel$expected)[c("Date", "Combo", ".pred")],
    tolerance = 1e-8, edge_case = FALSE, rationale = "Independent growth oracle.",
    outcome = "predictions", expected_error = NULL)
  edge <- example
  edge$name <- "edge"
  edge$edge_case <- TRUE
  proposal <- author_proposal()
  proposal$name <- "unit_growth"
  proposal$historical_only_predictors <- "Units"
  contract <- custom_author_contract(proposal, "Forecast units internally at ten percent; use supplied future price.",
    metadata, NULL, "local", list(example, edge), protocol)
  expect_identical(contract$requirements$historical_only_predictors, "Units")
  expect_error(custom_author_contract(within(proposal, historical_only_predictors <- character()),
    "Use supplied future units.", metadata, NULL, "local", list(example, edge), protocol),
    "columns.*match")
  history_only <- custom_author_data(raw, NULL, "Combo", "value", "month", 2,
    c("Units", "Price"), NULL, NULL, defer_future = TRUE)
  expect_identical(history_only$future_valid, c(Units = FALSE, Price = FALSE))
  expect_false("future_valid" %in% names(history_only$metadata))
})

test_that("driver prompts distinguish observed, supplied and internally generated values", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  metadata <- list(date_type = "month", forecast_horizon = 2,
    schema = list(Units = "numeric", Price = "numeric"), available_packages = "base")
  payload <- list(instructions = "Use units and prices.", metadata = metadata, name = "rule",
    model_type = "local", protocol = custom_author_protocol(metadata, "local",
      c("Units", "Price"), "base_only", TRUE))
  captured <- NULL
  session <- list(chat_structured = function(prompt, type, echo, convert) {
    captured <<- prompt
    list()
  })
  custom_author_request(session, "contract", payload)
  expect_match(captured, "historical_only_predictors", fixed = TRUE)
  expect_match(captured, "unspecified lag/window/date alignment", fixed = TRUE)
  expect_match(captured, "unresolved circular dependencies", fixed = TRUE)
  expect_match(captured, "business clarification questions", fixed = TRUE)
  expect_match(captured, "REMOVED from new_data", fixed = TRUE)
})

test_that("observed lags cannot silently become generated future driver values", {
  definition <- historical_driver_definition()
  definition$source[["fit"]] <- "function(data, context, parameters) data$Units[(nrow(data)-1):nrow(data)]"
  definition$source[["predict"]] <- paste0("function(object, new_data, context) { ",
    "if (any(context$forecast_step > length(object))) stop('observed_lag_exhausted'); ",
    "data.frame(.finnts_row = new_data$.finnts_row, .pred = object[context$forecast_step] * new_data$Price) }")
  definition$version_id <- custom_model_definition_digest(definition)
  panel <- historical_driver_panel()
  context <- list(model_type = "local", recipe_id = "C1", date_type = "month",
    forecast_horizon = 2, target_scale = "original")
  fit <- custom_model_fit_impl(panel$history[c("Date", "Combo", "Units", "Price")],
    panel$history$Target, definition, context, TRUE)
  expect_equal(custom_model_predict_impl(fit, panel$future), c(1200, 1430), tolerance = 1e-8)
  context$forecast_horizon <- 3
  fit <- custom_model_fit_impl(panel$history[c("Date", "Combo", "Units", "Price")],
    panel$history$Target, definition, context, TRUE)
  request <- rbind(panel$future, data.frame(Date = as.Date("2020-05-01"), Combo = "A", Price = 14))
  expect_error(custom_model_predict_impl(fit, request), "observed_lag_exhausted")
})

test_that("ambiguous driver logic stops both approval modes before source execution", {
  issues <- c("Which drivers require caller future values?",
    "What lag/window and date alignment should apply?",
    "How should driver values beyond the observed lag be generated?",
    "What method, starting state and order resolves the circular driver dependency?",
    "Which instruction or contradictory worked example is authoritative?")
  for (approval in c("manual", "automatic")) local({
    requests <- character()
    local_custom_console(function(request) {
      expect_identical(request$stage, "clarify")
      expect_length(requests, 1)
      custom_author_abort("clarification", "Await explicit driver logic.", code = "business_clarification")
    })
    local_mocked_bindings(check_agent_ellmer_version = function() invisible(NULL),
      new_llm_session = function(llm) list(),
      custom_author_request = function(session, stage, payload) {
        requests <<- c(requests, stage)
        if (stage != "contract") stop("Source must not be generated or executed")
        proposal <- author_proposal()
        if (approval == "automatic") proposal$defaults_used <- character()
        proposal$historical_only_predictors <- character()
        proposal$questions <- lapply(issues, function(question)
          list(kind = "business", topic = "business_context", question = question))
        proposal
      }, custom_author_validate = function(...) stop("Source must not execute"),
      .package = "finnts")
    case <- historical_finance_case()
    result <- tryCatch(create_custom_model("Use the drivers to predict revenue.", list(),
      input_data = case$history, combo_variables = "Combo", target_variable = "Target",
      date_type = "month", forecast_horizon = 2, external_regressors = case$predictors,
      model_type = "local", approval = approval,
      validation_examples = custom_examples_table(historical_finance_examples(case)),
      control = custom_model_control(package_policy = "base_only")), error = identity)
    expect_identical(requests, "contract")
    expect_identical(result$code, "business_clarification")
    expect_identical(custom_model_feedback(result)$status, "needs_input")
    expect_length(custom_model_feedback(result)$questions, length(issues))
  })
})
