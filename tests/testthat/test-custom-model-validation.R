# Independent local/global mean fixture with passive expected predictions.
# The business oracle is literal arithmetic, never derived from candidate output.
validation_fixture <- function(wrong = FALSE) {
  definition <- finnts:::new_custom_model_definition(
    "validated_mean", "Use mean.", "Use the analysis mean.", c(
      "local",
      "global"
    ), c(
      fit = if (wrong) "function(data, context, parameters) 999" else "function(data, context, parameters) mean(data$Target)",
      predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row, .pred=rep(object,nrow(new_data)))"
    ),
    list(
      predictors = character(), recipes = "R1", target_scale = "original", date_types = "month", forecast_horizon = 1,
      missing_data = "error"
    )
  )
  history <- data.frame(Date = rep(as.Date(c("2020-01-01", "2020-02-01")), 2), Combo = rep(c("example-a", "example-b"),
    each = 2
  ), Target = c(10, 20, 30, 40))
  request <- data.frame(Date = as.Date("2020-03-01"), Combo = c("example-a", "example-b"))
  global <- list(name = "pooled", model_type = "global", history = history, new_data = request, expected = transform(request,
    .pred = 25
  ), tolerance = 1e-08, edge_case = FALSE, rationale = "100 / 4 = 25")
  local <- global
  local$name <- "zero"
  local$model_type <- "local"
  local$history <- history[1:2, ]
  local$history$Target <- 0
  local$new_data <- request[1, , drop = FALSE]
  local$expected <- transform(local$new_data, .pred = 0)
  local$edge_case <- TRUE
  local$rationale <- "The mean of zeros is zero."
  data <- data.frame(Date = rep(seq(as.Date("2021-01-01"), by = "month", length.out = 6), 2), Combo = rep(c(
    "private-a",
    "private-b"
  ), each = 6), Target = rep(c(100, 200), each = 6))
  list(definition = definition, examples = list(local, global), data = list(data = data, metadata = list(
    date_type = "month",
    forecast_horizon = 1
  )))
}

test_that("example construction preserves independent answers and row keys", {
  history <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")), Revenue = c(10, 20))
  future <- data.frame(Date = as.Date(c("2020-04-01", "2020-03-01")), Revenue = NA_real_)
  before <- serialize(list(history, future), NULL)
  example <- custom_model_example(history, future, c(40, 30), target_variable = "Revenue")
  expect_identical(example$expected$.pred, c(30, 40))
  expect_identical(example$expected$Date, sort(future$Date))
  expect_identical(example$history$Target, c(10, 20))
  expect_identical(unique(example$history$Combo), "example")
  expect_false("Target" %in% names(example$new_data))
  expect_identical(custom_model_example(history, future, c(40, 30), target_variable = "Revenue"), example)
  expect_identical(serialize(list(history, future), NULL), before)
  expect_error(custom_model_example(history, future, 30, target_variable = "Revenue"), "one independent")
  expect_error(custom_model_example(history, transform(future, Revenue = 4), c(40, 30), target_variable = "Revenue"), "observed target")
  error <- custom_model_example(transform(history, Revenue = 0), future, target_variable = "Revenue", expected_error = list(
    phase = "fit",
    code = "zero_denominator"
  ))
  expect_true(error$edge_case)
  expect_null(error$expected)
  expect_identical(error$expected_error, list(phase = "fit", code = "zero_denominator"))
  keyed_history <- rbind(transform(history, id = "first"), transform(history, id = "second"))
  keyed_future <- rbind(transform(future, id = "first"), transform(future, id = "second"))
  global <- custom_model_example(keyed_history, keyed_future, c(40, 30, 80, 60),
    target_variable = "Revenue", combo_variables = "id",
    model_type = "global"
  )
  expect_identical(global$expected$.pred, c(30, 40, 60, 80))
  expect_setequal(unique(global$history$Combo), c("first", "second"))
  answer <- data.frame(Date = keyed_future$Date, id = keyed_future$id, .pred = c(40, 30, 80, 60))
  keyed <- custom_model_example(keyed_history, keyed_future, answer,
    target_variable = "Revenue", combo_variables = "id",
    model_type = "global"
  )
  expect_equal(keyed$expected, global$expected)
  expect_error(custom_model_example(history, future, c(NA_real_, 30), target_variable = "Revenue"), "finite")
  expect_error(custom_model_example(history, rbind(future, future), rep(30, 4), target_variable = "Revenue"), "unique and nonmissing")
  expect_error(custom_model_example(history, future, c(40, 30), target_variable = "Revenue", expected_error = list(
    phase = "fit",
    code = "zero_denominator"
  )), "no numerical answer")
})

test_that("raw authoring infers only single-series keys and unambiguous cadences", {
  for (cadence in c("day", "week", "month", "quarter", "year")) {
    data <- data.frame(Date = seq(as.Date("2020-01-01"), by = cadence, length.out = 8), Revenue = 1:8)
    before <- serialize(data, NULL)
    resolved <- custom_author_input_metadata(data, NULL, "Revenue", NULL, NULL, NULL)
    expect_identical(resolved$date_type, cadence)
    expect_length(unique(resolved$input_data[[resolved$combo_variables]]), 1L)
    expect_identical(serialize(data, NULL), before)
    explicit <- custom_author_input_metadata(transform(data, id = "one"), "id", "Revenue", cadence, NULL, NULL)
    expect_identical(explicit$combo_variables, "id")
    expect_identical(explicit$date_type, cadence)
  }
  data <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01", "2020-04-01")), Revenue = 1:3)
  expect_error(custom_author_input_metadata(data, NULL, "Revenue", NULL, NULL, NULL), "date_type")
  expect_error(custom_author_input_metadata(data[1:2, ], NULL, "Revenue", NULL, NULL, NULL), "date_type")
  expect_error(custom_author_input_metadata(rbind(data, data), NULL, "Revenue", "month", NULL, NULL), "combo_variables")
  expect_error(
    custom_author_input_metadata(transform(data, Date = as.character(Date)), NULL, "Revenue", NULL, NULL, NULL),
    "Date column"
  )
  panel <- rbind(transform(data, id = "one"), transform(data, id = "two"))
  expect_identical(custom_author_input_metadata(panel, "id", "Revenue", "month", NULL, NULL)$input_data, panel)
  interleaved <- data.frame(
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 6), id = rep(c("a", "b"), 3),
    Revenue = 1
  )
  expect_error(custom_author_input_metadata(interleaved, NULL, "Revenue", NULL, NULL, NULL), "ambiguous")
  expect_error(custom_author_input_metadata(interleaved, "id", "Revenue", NULL, NULL, NULL), "cadence")
  expect_error(custom_author_input_metadata(interleaved, "id", "Revenue", NULL, "2020-01-01", NULL), "Date values")
  drivers <- transform(interleaved[c("Date", "Revenue")], Units = seq_len(6))
  expect_identical(custom_author_input_metadata(drivers, NULL, "Revenue", NULL, NULL, NULL, "Units")$date_type, "month")
})

test_that("UX raw defaults and constructed examples pass real manual validation", {
  history <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")), Revenue = c(10, 20))
  future <- data.frame(Date = as.Date("2020-03-01"))
  examples <- list(custom_model_example(history, future, 15, target_variable = "Revenue"), custom_model_example(transform(history,
    Revenue = 0
  ), future, 0, target_variable = "Revenue", edge_case = TRUE))
  original <- serialize(examples, NULL)
  requests <- list()
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_request = function(session, stage, payload) {
      requests[[length(requests) + 1L]] <<- payload
      if (stage == "contract")
        return(list(
          questions = character(), interpretation = "Use the historical mean.", parameters_json = "{}",
          parameter_schema = list(), error_policy = list(), validation_properties = character(), history_requirements = list(
            minimum_rows_per_series = 1L,
            required_lag_periods = integer()
          )
        ))
      list(
        status = "candidate", conflict = "", fit_body = "mean(data$Target)", predict_body = "rep(object, nrow(new_data))",
        helpers = list()
      )
    }, .package = "finnts"
  )
  raw <- data.frame(Date = seq(as.Date("2021-01-01"), by = "month", length.out = 12), Revenue = 12345)
  before <- serialize(raw, NULL)
  draft <- create_custom_model("Use the historical mean.", list(),
    input_data = raw, target_variable = "Revenue", forecast_horizon = 1,
    name = "default_mean", validation_examples = examples, control = custom_model_control(package_policy = "base_only")
  )
  expect_identical(draft$definition$model_type, "local")
  expect_identical(draft$metadata$date_type, "month")
  expect_identical(requests[[1L]]$model_type, "local")
  expect_identical(serialize(examples, NULL), original)
  expect_identical(serialize(raw, NULL), before)
  expect_false(grepl("12345", jsonlite::toJSON(requests), fixed = TRUE))
  expect_identical(custom_model_feedback(draft)$resolved_inputs$combo_variables, ".finnts_single_series")
  model <- create_custom_model(
    draft = draft, responses = list(request_id = draft$pending$request_id, answer = "yes"),
    control = custom_model_control()
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_equal(custom_model_feedback(model)$resolved_inputs, custom_model_feedback(draft)$resolved_inputs)
  expect_identical(model$definition$model_type, "local")
  expect_identical(custom_run_envelope(model), model)
  expect_error(
    create_custom_model(draft = draft, control = custom_model_control(package_policy = "installed_finnts")),
    "policy"
  )
  local_mocked_bindings(custom_author_request = function(session, stage, payload) {
    requests[[length(requests) + 1L]] <<- payload
    stop("scaffold_probe")
  }, .package = "finnts")
  expect_error(
    create_custom_model("Use mean.", list(), input_data = raw, target_variable = "Revenue", forecast_horizon = 1),
    "scaffold_probe"
  )
  scaffold <- tail(requests, 1L)[[1L]]$protocol$scaffold
  expect_true(all(vapply(scaffold$scenarios, function(scenario) identical(scenario$model_type, "local"), logical(1))))
  request_count <- length(requests)
  for (field in c("target_variable", "forecast_horizon")) {
    arguments <- list(instructions = "Use mean.", llm = list(), input_data = raw, target_variable = "Revenue", forecast_horizon = 1)
    arguments[field] <- list(NULL)
    failure <- tryCatch(do.call(create_custom_model, arguments), error = identity)
    expect_s3_class(failure, "finnts_custom_model_authoring_error")
    expect_identical(custom_model_feedback(failure)$stage, "input")
  }
  expect_identical(length(requests), request_count)
})

test_that("missing candidate variables receive source-verified repair diagnostics", {
  fixture <- validation_fixture()
  fixture$definition$source[["fit"]] <- "function(data, context, parameters) list(history = data)"
  fixture$definition$source[["predict"]] <- paste0("function(object, new_data, context) with(object$history, { ", "data.frame(.finnts_row = new_data$.finnts_row, .pred = rep(mean(history$Target), nrow(new_data))) })")
  fixture$definition$version_id <- custom_model_definition_digest(fixture$definition)
  before <- serialize(fixture, NULL)
  report <- custom_author_child(fixture$definition, fixture$examples, fixture$data)
  expect_false(report$passed)
  expect_identical(report$failure$reason, "unbound_symbol")
  expect_identical(report$failure$unbound_symbol, "history")
  expect_identical(report$failure$phase, "example_predict")
  expect_null(report$failure$source_message)
  expect_identical(validate_custom_author_failure(report$failure, fixture$definition), report$failure)
  expect_identical(serialize(fixture, NULL), before)
})

test_that("missing-variable evidence excludes literals, member names and sensitive identifiers", {
  error <- simpleError("object 'history' not found")
  reference <- c(predict = "function(object, new_data, context) with(object$history, mean(history$Target))")
  expect_identical(custom_author_unbound_symbol(error, reference), "history")
  for (body in c(
    "object$history", "object[['history']]", "'history'", "quote(history)", "base::quote(history)", "expression(history)",
    "~history", "history <- 1", "1 -> history", "list(history = 1)", "function(history) NULL", "get('history')", "for (history in 1:3) NULL"
  )) {
    source <- c(predict = paste0("function(object, new_data, context) { ", body, " }"))
    expect_null(custom_author_unbound_symbol(error, source))
  }
  expect_null(custom_author_unbound_symbol(error, c(predict = "function(")))
  expect_null(custom_author_unbound_symbol(simpleError("object 'absent_value' not found"), reference))
  expect_null(custom_author_unbound_symbol(simpleError("could not find function 'history'"), reference))
  expect_null(custom_author_unbound_symbol(simpleError("object 'history' not found: PRIVATE_VALUE"), reference))
  coded <- structure(list(message = "object 'history' not found", call = NULL, code = "zero_denominator"), class = c(
    "finnts_custom_rule_error",
    "error", "condition"
  ))
  expect_null(custom_author_unbound_symbol(coded, reference))
  for (symbol in c("private_history", "api_key", "password", "token123", "C:/history", "a\nb", paste(rep("a", 121), collapse = ""))) {
    source <- c(predict = paste0("function(...) `", symbol, "`"))
    expect_null(custom_author_unbound_symbol(simpleError(paste0("object '", symbol, "' not found")), source))
    expect_error(validate_custom_author_unbound_symbol(symbol), "missing-variable")
  }
  fixture <- validation_fixture()
  fixture$definition$source[["predict"]] <- "function(object, new_data, context) history$Target"
  fixture$definition$version_id <- custom_model_definition_digest(fixture$definition)
  failure <- custom_author_failure(fixture$definition, "example_predict", "unbound_symbol", unbound_symbol = "history")
  expect_identical(validate_custom_author_failure(failure), failure)
  for (field in c("reason", "phase", "source_message", "unbound_symbol")) {
    changed <- failure
    changed[[field]] <- switch(field,
      reason = "candidate_error",
      phase = "source_guard",
      source_message = "Hidden detail",
      unbound_symbol = "absent_value"
    )
    expect_error(validate_custom_author_failure(changed, fixture$definition), class = "finnts_custom_model_authoring_error")
  }
  changed <- failure
  changed$unbound_symbol <- NULL
  expect_error(validate_custom_author_failure(changed), "missing-variable")
  changed <- failure
  changed["phase"] <- list(NULL)
  expect_error(validate_custom_author_failure(changed), class = "finnts_custom_model_authoring_error")
  legacy <- custom_author_failure(fixture$definition, "example_predict", "candidate_error")
  expect_null(legacy$unbound_symbol)
  expect_identical(validate_custom_author_failure(legacy), legacy)
})

test_that("valid data-mask and nested alias bindings retain their calculations", {
  fixture <- validation_fixture()
  for (calculation in c("with(object$history, mean(Target))", "{ history <- object$history; mean(history$Target) }", "{ calculate <- function(history) mean(history$Target); calculate(object$history) }")) {
    definition <- fixture$definition
    definition$source[["fit"]] <- "function(data, context, parameters) list(history = data)"
    definition$source[["predict"]] <- paste0("function(object, new_data, context) { value <- ", calculation, "; data.frame(.finnts_row = new_data$.finnts_row, .pred = rep(value, nrow(new_data))) }")
    definition$version_id <- custom_model_definition_digest(definition)
    expect_true(custom_author_child(definition, fixture$examples, fixture$data)$passed)
  }
})

test_that("unbound runtime feedback covers fit and holdout without leaking private values", {
  fixture <- validation_fixture()
  for (scope in c("fit", "holdout", "private")) {
    definition <- fixture$definition
    definition$source[["fit"]] <- switch(scope,
      fit = "function(data, context, parameters) missing_cache",
      holdout = "function(data, context, parameters) { if (nrow(data) > 4) missing_cache; mean(data$Target) }",
      private = "function(data, context, parameters) PRIVATE_BINDING"
    )
    definition$version_id <- custom_model_definition_digest(definition)
    report <- custom_author_child(definition, fixture$examples, fixture$data)
    expect_false(report$passed)
    expect_identical(report$failure$reason, if (scope == "private")
      "candidate_error"
    else "unbound_symbol")
    expect_identical(report$failure$unbound_symbol, if (scope == "private")
      NULL
    else "missing_cache")
    expect_identical(report$failure$phase, if (scope == "holdout")
      "holdout_fit"
    else "example_fit")
    expect_null(report$failure$source_message)
    expect_false(grepl("PRIVATE_BINDING|private-a|private-b", jsonlite::toJSON(report)))
  }
})

test_that("caller history feedback excludes intentional error cases and preserves inputs", {
  fixture <- validation_fixture()
  examples <- fixture$examples
  error <- examples[[1]]
  error$name <- "short_history"
  error$history <- error$history[nrow(error$history), , drop = FALSE]
  error["expected"] <- list(NULL)
  error$outcome <- "error"
  error$expected_error <- list(phase = "fit", code = "insufficient_history")
  examples[[3]] <- error
  before <- serialize(examples, NULL)
  expect_length(custom_author_examples(examples, fixture$definition, fixture$data$metadata), 3L)
  protocol <- custom_author_protocol(fixture$data$metadata, c("local", "global"), character(), "base_only", TRUE)
  proposal <- list(history_requirements = list(minimum_rows_per_series = 3L, required_lag_periods = integer()))
  feedback <- custom_author_history_feedback(proposal, protocol, examples, fixture$data$metadata)
  expect_length(feedback$examples, 3L)
  expect_identical(vapply(feedback$examples, `[[`, integer(1), "history_rows"), c(2L, 2L, 2L))
  expect_false("short_history" %in% vapply(feedback$examples, `[[`, character(1), "scenario"))
  expect_match(feedback$guidance, "Caller examples and expected outcomes are fixed", fixed = TRUE)
  expect_identical(serialize(examples, NULL), before)
  expect_null(custom_author_history_feedback(proposal, protocol))
})

test_that("typed fixtures preserve fractional future drivers and reject lossy input", {
  metadata <- list(date_type = "month", forecast_horizon = 1, schema = list(Discount_Rate = "numeric"))
  protocol <- custom_author_protocol(metadata, "local", "Discount_Rate", "base_only", FALSE)
  values <- lapply(protocol$scaffold$scenarios, function(scenario) list(
    id = scenario$id, rationale = "Ten times one minus the supplied discount.",
    series = list(list(
      combo = "example_a", history = list(Target = list(10L), Discount_Rate = list(0L)), future = list(Discount_Rate = list(0.1)),
      expected = list(9)
    )), outcome = "predictions", error_phase = "none", error_code = ""
  ))
  saved <- serialize(values, NULL)
  legacy <- custom_author_scaffold_examples(values, protocol$scaffold, "local", "Discount_Rate", protocol$predictor_types)
  legacy[[1]]$history$Discount_Rate <- as.integer(legacy[[1]]$history$Discount_Rate)
  expect_type(legacy[[1]]$history$Discount_Rate, "integer")
  normalized <- custom_author_scaffold_examples(values, protocol$scaffold, "local", "Discount_Rate", protocol$predictor_types)
  expect_type(normalized[[1]]$history$Discount_Rate, "double")
  expect_type(normalized[[1]]$new_data$Discount_Rate, "double")
  expect_identical(serialize(values, NULL), saved)
  definition <- new_custom_model_definition(
    "discount_rule", "Ten times one minus discount.", "Use the supplied fractional discount.",
    "local", c(fit = "function(data, context, parameters) list()", predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row, .pred=10*(1-new_data$Discount_Rate))"),
    list(
      predictors = "Discount_Rate", recipes = "R1", target_scale = "original", date_types = "month", forecast_horizon = 1,
      missing_data = "error"
    )
  )
  workflow <- custom_run_workflows(definition, normalized[[1]]$history, metadata)$Model_Workflow[[1]]
  fitted <- generics::fit(workflow, normalized[[1]]$history)
  expect_equal(predict(fitted, normalized[[1]]$new_data)$.pred, 9)
  failure <- custom_author_child(definition, legacy, list(metadata = metadata), contract = list(
    authoring_protocol = 5L,
    validation_properties = character()
  ))
  expect_false(failure$passed)
  expect_identical(failure$failure$reason, "input_type_mismatch")
  expect_identical(failure$failure$phase, "example_predict")
})

test_that("declared parameter types fix numeric arrays without flattening structured values", {
  values <- custom_author_json("{\"weights\":[0.5,0.3,0.2],\"window\":3,\"settings\":{\"items\":[1,2]},\"records\":[{\"weight\":1}]}")
  schema <- list(list(name = "weights", type = "numeric_vector", units = "fractions, newest first"), list(
    name = "window",
    type = "integer", units = "months"
  ), list(name = "settings", type = "object", units = "configuration"), list(
    name = "records",
    type = "records", units = "configuration"
  ))
  original <- serialize(values, NULL)
  result <- custom_author_typed_parameters(values, schema)
  expect_identical(result$values$weights, c(0.5, 0.3, 0.2))
  expect_identical(result$values$window, 3L)
  expect_identical(result$values$settings, values$settings)
  expect_identical(result$values$records, values$records)
  expect_identical(serialize(values, NULL), original)
  expect_identical(custom_author_typed_parameters(result$values, result$schema), result)
  expect_equal(sum(c(30, 20, 10) * result$values$weights), 23)
  invalid <- values
  invalid$weights <- list("0.5", "0.3", "0.2")
  expect_error(custom_author_typed_parameters(invalid, schema), "declared types")
  invalid <- values
  invalid$window <- 3.5
  expect_error(custom_author_typed_parameters(invalid, schema), "declared types")
  expect_error(custom_author_typed_parameters(values, schema[-1]), "declared types")
  expect_error(custom_author_typed_parameters(values, c(schema[1], schema[1], schema[3:4])), "declared types")
})

test_that("protocol-six transport distinguishes questions defaults and conflicts", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  metadata <- list(date_type = "month", forecast_horizon = 1, schema = list())
  protocol <- custom_author_protocol(metadata, "local", character(), "base_only", FALSE)
  policy <- custom_author_automatic_policy(
    list(metadata = metadata), "local", character(), "bottoms_up", "rule", NULL,
    3L, 60
  )
  payload <- list(
    metadata = metadata, protocol = protocol, name = "rule", model_type = "local", approval_mode = "automatic",
    automatic_policy = policy, contract = list(model_type = "local")
  )
  captured <- NULL
  session <- list(chat_structured = function(prompt, type, echo, convert) {
    captured <<- type
    list()
  })
  custom_author_request(session, "contract", payload)
  fields <- attr(captured, "properties")
  expect_setequal(attr(attr(fields$defaults_used, "items"), "values"), setdiff(names(policy$catalog), policy$supplied))
  questions <- attr(attr(fields$questions, "items"), "properties")
  expect_setequal(names(questions), c("kind", "topic", "question"))
  expect_setequal(attr(questions$kind, "values"), c("business", "protocol"))
  expect_error(
    custom_author_questions(list(list(kind = "protocol", topic = "fiscal_calendar", question = "Which calendar?"))),
    "topic"
  )
  custom_author_request(session, "source", payload)
  fields <- attr(captured, "properties")
  expect_setequal(names(fields), c("status", "conflict", "fit_body", "predict_body", "helpers"))
  expect_setequal(attr(fields$status, "values"), c("candidate", "contract_conflict", "implementation_difficulty"))
  expect_error(custom_author_source_response(list(
    status = "contract_conflict", conflict = "Contradiction.", fit_body = "stop('must not execute')",
    predict_body = "", helpers = list()
  )), "omit executable")
})

test_that("new parameter diagnostics identify fields without inventing units", {
  values <- list(method = "first observation", alpha = 0.3)
  schema <- list(list(name = "method", type = "string", units = ""), list(name = "alpha", type = "number", units = "fraction"))
  original <- serialize(schema, NULL)
  result <- custom_author_typed_parameters(values, schema)
  expect_identical(result$schema[[1]]$units, "not_applicable")
  expect_identical(serialize(schema, NULL), original)
  expect_identical(custom_author_typed_parameters(values, schema), result)
  schema[[2]]$units <- ""
  error <- tryCatch(custom_author_typed_parameters(values, schema), error = identity)
  expect_identical(error$parameter_issue$parameter, "alpha")
  expect_identical(error$parameter_issue$field, "units")
  expect_match(conditionMessage(error), "alpha.units", fixed = TRUE)
})

# Build a new-protocol ratio contract with a positive control and a required
# zero-denominator error. Inputs and expected values are independent fixed data.
validation_error_contract <- function() {
  metadata <- list(date_type = "month", forecast_horizon = 2, schema = list(Price = "numeric"))
  protocol <- custom_author_protocol(metadata, "local", "Price", "base_only", FALSE)
  values <- lapply(protocol$scaffold$scenarios, function(scenario) list(
    id = scenario$id, rationale = if (scenario$edge_case) "Zero price must raise the declared error, not return zero." else "100/2=50 and 100/4=25.",
    outcome = if (scenario$edge_case) "error" else "predictions", error_phase = if (scenario$edge_case) "predict" else "none",
    error_code = if (scenario$edge_case) "zero_denominator" else "", series = list(list(combo = "example_a", history = list(
      Target = 100L,
      Price = 1L
    ), future = list(Price = if (scenario$edge_case) c(0, 4) else c(2, 4)), expected = if (scenario$edge_case) list() else c(
      50,
      25
    )))
  ))
  proposal <- list(
    questions = character(), interpretation = "Divide 100 by future Price; reject a zero denominator.",
    parameters_json = "{}", parameter_schema = list(), history_requirements = list(minimum_rows_per_series = 0L, required_lag_periods = integer()),
    error_policy = list(list(code = "zero_denominator", phase = "predict", description = "Reject zero Price.")), validation_properties = c(
      "independent_series",
      "fixed_window"
    ), examples = values
  )
  contract <- custom_author_contract(
    proposal, "Divide 100 by Price; reject zero denominators.", metadata, "price_ratio",
    "local", NULL, protocol
  )
  source <- list(
    fit_body = "list()", predict_body = "if (any(new_data$Price == 0)) finntsRuleError('zero_denominator'); 100 / new_data$Price",
    helpers = list(), status = "candidate", conflict = ""
  )
  list(
    contract = contract, definition = custom_author_definition(contract, source), source = source, proposal = proposal,
    protocol = protocol, metadata = metadata, data = list(data = data.frame(Date = seq(as.Date("2021-01-01"),
      by = "month",
      length.out = 8
    ), Combo = "private", Target = 100, Price = 2), metadata = metadata)
  )
}

test_that("expected errors reject silent zero forecasts and unrelated exceptions", {
  fixture <- validation_error_contract()
  contract <- fixture$contract
  expect_identical(contract$history_requirements$minimum_rows_per_series, 0L)
  expect_identical(contract$history_requirements$workflow_minimum_rows_per_series, 1L)
  expect_identical(contract$expectation_provenance, "llm_proposed_unverified")
  saved <- serialize(contract, NULL)
  report <- custom_author_validate(fixture$definition, contract$examples, fixture$data, 60,
    package_policy = contract$package_policy,
    contract = contract
  )
  expect_true(report$passed)
  expect_identical(report$examples[[2]]$error_code, "zero_denominator")
  expect_identical(report$verification$properties$row_order, 1L)
  checks <- custom_author_checks(report, fixture$definition, digest::digest(contract, algo = "sha256", serializeVersion = 2),
    contract$examples, fixture$metadata,
    contract = contract
  )
  expect_match(checks$expectation_provenance, "not independently", fixed = TRUE)
  for (body in c(
    "if (any(new_data$Price == 0)) return(rep(0, nrow(new_data))); 100/new_data$Price", "if (any(new_data$Price == 0)) stop('Unrelated error'); 100/new_data$Price",
    "if (any(new_data$Price == 0)) finntsRuleError('different_error'); 100/new_data$Price"
  )) {
    source <- fixture$source
    source$predict_body <- body
    definition <- custom_author_definition(contract, source)
    failed <- custom_author_validate(definition, contract$examples, fixture$data, 60,
      package_policy = contract$package_policy,
      contract = contract
    )
    expect_false(failed$passed)
    expect_true(failed$failure$reason %in% c("expected_error_not_raised", "wrong_expected_error"))
  }
  expect_identical(serialize(contract, NULL), saved)
  invalid <- fixture$proposal
  invalid$examples[[2]]$series[[1]]$expected <- c(0, 0)
  expect_error(
    custom_author_contract(invalid, "Reject zero price.", fixture$metadata, "price_ratio", "local", NULL, fixture$protocol),
    "must not contain numerical"
  )
  invalid <- fixture$proposal
  invalid$error_policy <- list()
  expect_error(
    custom_author_contract(invalid, "Reject zero price.", fixture$metadata, "price_ratio", "local", NULL, fixture$protocol),
    "contradict"
  )
  source <- fixture$source
  source$predict_body <- "if (any(new_data$Price == 0)) finntsRuleError('zero_denominator'); 100 / new_data$Price[order(new_data$.finnts_row)]"
  wrong_order <- custom_author_validate(custom_author_definition(contract, source), contract$examples, fixture$data, 60,
    package_policy = contract$package_policy, contract = contract
  )
  expect_identical(wrong_order$failure$reason, "property_mismatch")
  expect_identical(wrong_order$failure$property, "row_order")
  altered_report <- report
  altered_report$verification$properties$row_order <- 0L
  expect_error(custom_author_checks(altered_report, fixture$definition, "unused", contract$examples, fixture$metadata,
    contract = contract
  ), "Property authority")
  review <- custom_draft_review(contract, fixture$definition, fixture$metadata)
  output <- capture.output(custom_author_review(review))
  expect_true(any(grepl("zero_denominator", output, fixed = TRUE)))
  expect_true(any(grepl("not independently", output, fixed = TRUE)))
})

test_that("multiple declared errors fit bounded generated scenarios", {
  fixture <- validation_error_contract()
  proposal <- fixture$proposal
  proposal$error_policy[[2]] <- list(code = "negative_price", phase = "predict", description = "Reject negative Price.")
  extra <- proposal$examples[[2]]
  extra$id <- "local_error_1"
  extra$error_code <- "negative_price"
  extra$series[[1]]$future$Price <- c(-1, 4)
  proposal$examples[[3]] <- extra
  contract <- custom_author_contract(
    proposal, "Reject zero and negative Price.", fixture$metadata, "price_ratio", "local",
    NULL, fixture$protocol
  )
  source <- fixture$source
  source$predict_body <- paste("if (any(new_data$Price < 0)) finntsRuleError('negative_price');", source$predict_body)
  definition <- custom_author_definition(contract, source)
  report <- custom_author_validate(definition, contract$examples, fixture$data, 60,
    package_policy = contract$package_policy,
    contract = contract
  )
  expect_true(report$passed)
  expect_identical(report$verification$expected_errors, 2L)
  invalid <- proposal
  invalid$examples[[3]]$id <- "global_error_1"
  expect_error(custom_author_contract(
    invalid, "Reject invalid prices.", fixture$metadata, "price_ratio", "local", NULL,
    fixture$protocol
  ), "scenario")
  expect_error(custom_author_scaffold_examples(
    rep(proposal$examples, 3), fixture$protocol$scaffold, "local", "Price",
    fixture$protocol$predictor_types
  ), "coverage")
})

test_that("generated numerical mismatches are advisory while caller answers stay mandatory", {
  fixture <- validation_error_contract()
  contract <- fixture$contract
  contract$authoring_protocol <- 6L
  contract$requirements$runtime <- list(version = 2L, prediction_scope = "complete_horizon", cohort = "series", group_columns = character())
  contract$required_properties <- character()
  contract$examples[[1]]$expected$.pred <- contract$examples[[1]]$expected$.pred + 1
  before <- serialize(contract, NULL)
  definition <- custom_author_definition(contract, fixture$source)
  report <- custom_author_validate(definition, contract$examples, fixture$data, 60, contract = contract)
  expect_true(report$passed)
  if (isTRUE(report$passed)) {
    checks <- custom_author_checks(report, definition, "fixed_contract", contract$examples, contract = contract)
    expect_match(checks$llm_example_warning, "1 LLM-generated", fixed = TRUE)
    expect_match(checks$llm_example_warning, "validation_examples", fixed = TRUE)
    expect_identical(report$verification$provenance, "llm_proposed_unverified")
    expect_equal(report$examples[[1]]$maximum_error, 1)
    corrupt <- report
    corrupt$verification$provenance <- "caller_supplied_unverified"
    expect_error(
      custom_author_checks(corrupt, definition, "fixed_contract", contract$examples, contract = contract),
      "behavioral evidence"
    )
  }
  caller <- contract
  caller$expectation_provenance <- "caller_supplied_unverified"
  expect_false(custom_author_validate(definition, caller$examples, fixture$data, 60, contract = caller)$passed)
  source <- fixture$source
  source$predict_body <- "if (any(new_data$Price == 0)) return(rep(0, nrow(new_data))); 100/new_data$Price"
  failed <- custom_author_validate(custom_author_definition(contract, source), contract$examples, fixture$data, 60, contract = contract)
  expect_false(failed$passed)
  expect_identical(failed$failure$reason, "expected_error_not_raised")
  expect_identical(serialize(contract, NULL), before)
})

test_that("new property authority distinguishes suggestions from caller requirements", {
  fixture <- validation_error_contract()
  contract <- fixture$contract
  contract$authoring_protocol <- 6L
  contract$requirements$runtime <- list(version = 2L, prediction_scope = "complete_horizon", cohort = "series", group_columns = character())
  contract$validation_properties <- "target_scale_equivariant"
  contract$required_properties <- character()
  definition <- custom_author_definition(contract, fixture$source)
  report <- custom_author_validate(definition, contract$examples, fixture$data, 60, contract = contract)
  expect_true(report$passed)
  expect_identical(report$verification$schema_version, 2L)
  expect_identical(report$verification$properties$target_scale_equivariant, 0L)
  expect_identical(report$verification$diagnostics[[1]]$property, "target_scale_equivariant")
  checks <- custom_author_checks(report, definition, "fixed_contract", contract$examples, contract = contract)
  expect_match(checks$property_diagnostics, "1 suggested-property", fixed = TRUE)
  required <- contract
  required$required_properties <- "target_scale_equivariant"
  failed <- custom_author_validate(definition, required$examples, fixture$data, 60, contract = required)
  expect_false(failed$passed)
  expect_identical(failed$failure$reason, "property_mismatch")
  expect_error(custom_author_checks(report, definition, "fixed_contract", contract$examples, contract = required), "authority")
  altered <- report
  altered$verification$diagnostics <- list()
  expect_error(custom_author_checks(altered, definition, "fixed_contract", contract$examples, contract = contract), "coverage")
})

test_that("declared invariants catch clipping, history leakage and calendar dependence", {
  fixture <- validation_error_contract()
  for (property in c("constant_forecast", "target_scale_equivariant")) {
    contract <- fixture$contract
    contract$validation_properties <- property
    contract$required_properties <- property
    report <- custom_author_validate(fixture$definition, contract$examples, fixture$data, 60,
      package_policy = contract$package_policy,
      contract = contract
    )
    expect_false(report$passed)
    expect_identical(report$failure$reason, "property_mismatch")
  }
  for (cadence in c("day", "week", "month", "quarter", "year")) {
    metadata <- list(date_type = cadence, forecast_horizon = 2)
    protocol <- custom_author_protocol(metadata, c("local", "global"), character(), "base_only", FALSE)
    reference <- custom_author_prompt_examples("source", list(protocol = protocol, metadata = metadata, model_type = c(
      "local",
      "global"
    ), name = "reference_mean"))[[1]]
    contract <- reference$request$contract
    contract$validation_properties <- c(
      "constant_forecast", "target_scale_equivariant", "fixed_window",
      "relative_calendar"
    )
    reference$response$fit_body <- paste(
      "if (identical(context$model_type, 'global') && length(unique(data$Combo)) < 2) stop('Global training requires two series.');",
      reference$response$fit_body
    )
    definition <- custom_author_definition(contract, reference$response)
    data <- data.frame(Date = rep(seq(as.Date("2021-01-01"), by = cadence, length.out = 8), 2), Combo = rep(c(
      "private_a",
      "private_b"
    ), each = 8), Target = rep(1:8, 2))
    report <- custom_author_validate(definition, contract$examples, list(data = data, metadata = metadata), 60,
      package_policy = contract$package_policy,
      contract = contract
    )
    expect_true(report$passed)
    expect_true(all(vapply(report$verification$properties, function(count) count > 0L, logical(1))))
  }
})

test_that("declared history rejects short or absent lag coverage without changing examples", {
  requirements <- list(minimum_rows_per_series = 48, required_lag_periods = c(12, 24, 36, 48))
  normalized <- custom_author_history_requirements(requirements)
  expect_identical(normalized, list(minimum_rows_per_series = 48L, required_lag_periods = c(12L, 24L, 36L, 48L), workflow_minimum_rows_per_series = 48L))
  history <- data.frame(Date = seq(as.Date("2020-06-01"), by = "month", length.out = 7), Combo = "example_a", Target = 100)
  request <- data.frame(Date = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01")), Combo = "example_a")
  examples <- list(list(history = history, new_data = request, expected = transform(request, .pred = 231.6666667)))
  saved <- serialize(examples, NULL)
  error <- tryCatch(custom_author_check_history(examples, normalized, "month"), error = identity)
  expect_identical(error$code, "insufficient_example_history")
  expect_identical(error$field, "examples.history")
  expect_identical(serialize(examples, NULL), saved)
  examples[[1]]$history <- data.frame(
    Date = seq(as.Date("2017-01-01"), by = "month", length.out = 48), Combo = "example_a",
    Target = 100
  )
  expect_null(custom_author_check_history(examples, normalized, "month"))
  examples[[1]]$history$Date[1] <- as.Date("2016-12-01")
  error <- tryCatch(custom_author_check_history(examples, normalized, "month"), error = identity)
  expect_identical(error$code, "insufficient_example_history")
  expect_match(conditionMessage(error), "lag", fixed = TRUE)
  expect_identical(custom_author_history_requirements(list(minimum_rows_per_series = 0L, required_lag_periods = integer()))$workflow_minimum_rows_per_series, 1L)
  for (minimum in list(-1, 1.5, Inf, 2001, "48")) {
    expect_error(custom_author_history_requirements(list(minimum_rows_per_series = minimum, required_lag_periods = integer())),
      class = "finnts_custom_model_authoring_error"
    )
  }
  for (lags in list(c(12, 12), 0, -1, 1.5, 2001, "12")) {
    expect_error(custom_author_history_requirements(list(minimum_rows_per_series = 1, required_lag_periods = lags)),
      class = "finnts_custom_model_authoring_error"
    )
  }
  expect_identical(
    custom_author_history_requirements(list(minimum_rows_per_series = 1, required_lag_periods = list()))$required_lag_periods,
    integer()
  )
})

# Script proposal transport only. The captured six-row/1:6 declaration is
# rejected before execution; correction changes only the unaccepted declaration.
# Literal median oracles, real holdouts and later forecasts remain independent.
test_that("six-month fit-window median repairs declarations without changing answers", {
  protocol_builder <- custom_author_protocol
  invisible(NULL)
  instructions <- "Use the median of the last six actual months for every forecast month."
  values <- lapply(c("local_basic", "local_edge"), function(id) list(
    id = id, rationale = if (id == "local_basic") "The middle values five and seven average to six." else "The middle values minus one and one average to zero.",
    series = list(list(combo = "example_a", history = list(Target = if (id == "local_basic") c(1, 3, 5, 7, 9, 11) else c(
      -9,
      -3, -1, 1, 3, 9
    )), future = list(), expected = if (id == "local_basic") rep(6, 3) else rep(0, 3))), outcome = "predictions",
    error_phase = "none", error_code = ""
  ))
  bad <- list(
    questions = character(), interpretation = "Fit the median once to the last six actual rows and repeat it.",
    parameters_json = "{\"window\":6}", history_requirements = list(minimum_rows_per_series = 6L, required_lag_periods = as.list(1:6)),
    examples = values, defaults_used = character(), parameter_schema = list(list(name = "window", type = "number", units = "test quantity")),
    error_policy = list(), validation_properties = character()
  )
  corrected <- bad
  corrected$history_requirements$required_lag_periods <- integer()
  original <- serialize(bad, NULL)
  data <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 24), Combo = "PRIVATE_MEDIAN", Target = seq_len(24))
  metadata <- list(date_type = "month", forecast_horizon = 3, available_packages = c("base", "stats", "utils"))
  protocol <- custom_author_protocol(metadata, "local", character(), "installed_finnts", FALSE)
  examples <- custom_author_scaffold_examples(values, protocol$scaffold, "local", character())
  expect_error(custom_author_check_history(examples, bad$history_requirements, "month"), "declared lag date")
  longer <- examples
  for (index in seq_along(longer)) {
    longer[[index]]$history <- data.frame(
      Date = seq(as.Date("2011-01-01"), by = "month", length.out = 120), Combo = "example_a",
      Target = 1
    )
  }
  expect_error(custom_author_check_history(longer, bad$history_requirements, "month"), "declared lag date")
  expect_null(custom_author_check_history(examples, corrected$history_requirements, "month"))
  requests <- list()
  contracts <- validations <- 0L
  validate <- custom_author_validate
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data, metadata = metadata), custom_author_validate = function(...) {
      validations <<- validations + 1L
      validate(...)
    }, custom_author_request = function(session, stage, payload) {
      requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
      if (stage == "contract") {
        contracts <<- contracts + 1L
        return(if (contracts == 1L) bad else corrected)
      }
      list(
        fit_body = "stats::median(utils::tail(data$Target, parameters$window))", predict_body = "rep(object, nrow(new_data))",
        helpers = list(), status = "candidate", conflict = ""
      )
    }, .package = "finnts"
  )
  model <- create_custom_model(instructions, list(),
    input_data = data.frame(), name = "median_six", model_type = "local",
    approval = "automatic", control = custom_model_control(max_attempts = 2L)
  )
  expect_true(model$validation$technical_passed)
  expect_identical(validations, 1L)
  expect_identical(vapply(requests, `[[`, character(1), "stage"), c("contract", "contract", "source"))
  expect_identical(requests[[2]]$payload$feedback$history$unobserved_lag_periods, 1:2)
  expect_identical(requests[[3]]$payload$contract$history_requirements, list(minimum_rows_per_series = 6L, required_lag_periods = integer(), workflow_minimum_rows_per_series = 6L))
  expect_equal(requests[[3]]$payload$contract$examples, examples)
  expect_identical(serialize(bad, NULL), original)
  expect_false(grepl("PRIVATE_MEDIAN", jsonlite::toJSON(requests), fixed = TRUE))
  workflow <- custom_model_workflow(model$definition, recipes::recipe(Target ~ Date + Combo, data = data), "local", "R1",
    "month", 3, "original",
    allow_code = TRUE
  )
  fitted <- generics::fit(workflow, data)
  future <- data.frame(Date = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01")), Combo = "PRIVATE_MEDIAN")
  expect_equal(predict(fitted, future)$.pred, rep(21.5, 3))
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fitted, path)
  expect_identical(predict(readRDS(path), future), predict(fitted, future))
  requests_before <- length(requests)
  validations_before <- validations
  local_mocked_bindings(custom_author_request = function(session, stage, payload) {
    requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
    bad
  }, .package = "finnts")
  error <- tryCatch(create_custom_model(instructions, list(),
    input_data = data.frame(), name = "median_six", model_type = "local",
    approval = "automatic", control = custom_model_control(max_attempts = 1L)
  ), error = identity)
  expect_identical(error$code, "insufficient_example_history")
  expect_match(conditionMessage(error), "Forecast-relative lags reaching unobserved periods: 1, 2", fixed = TRUE)
  expect_match(conditionMessage(error), "Adding older history cannot", fixed = TRUE)
  expect_identical(error$diagnostics$rejected_contract$proposal$history_requirements, bad$history_requirements)
  expect_identical(length(requests), requests_before + 1L)
  expect_identical(validations, validations_before)
})

test_that("declared history checks every cadence and global series independently", {
  requirements <- list(minimum_rows_per_series = 4L, required_lag_periods = c(2L, 4L))
  for (cadence in c("day", "week", "month", "quarter", "year")) {
    metadata <- list(date_type = cadence, forecast_horizon = 2)
    scaffold <- custom_author_example_scaffold(metadata, c("local", "global"))
    values <- lapply(scaffold$scenarios, function(scenario) list(
      id = scenario$id, rationale = "A constant series stays constant.",
      series = lapply(scenario$combos, function(combo) list(
        combo = combo, history = list(Target = rep(10, 4)), future = list(),
        expected = rep(10, 2)
      )), outcome = "predictions", error_phase = "none", error_code = ""
    ))
    examples <- custom_author_scaffold_examples(values, scaffold, c("local", "global"), character())
    expect_null(custom_author_check_history(examples, requirements, cadence))
    global <- examples[[3]]
    global$history <- global$history[-nrow(global$history), ]
    expect_error(custom_author_check_history(list(global), requirements, cadence), "rows")
    expect_null(custom_author_check_history(
      examples, list(minimum_rows_per_series = 1L, required_lag_periods = integer()),
      cadence
    ))
    expect_error(custom_author_check_history(
      examples, list(minimum_rows_per_series = 4L, required_lag_periods = 1L),
      cadence
    ), "lag")
  }
})

test_that("compact synthetic values use deterministic example grids", {
  for (cadence in c("day", "week", "month", "quarter", "year")) {
    metadata <- list(date_type = cadence, forecast_horizon = 2)
    scaffold <- custom_author_example_scaffold(metadata, c("local", "global"), "driver")
    values <- lapply(scaffold$scenarios, function(scenario) {
      list(id = scenario$id, rationale = "Targets two and four average to three.", series = lapply(
        scenario$combos,
        function(combo) list(combo = combo, history = list(Target = c(2, 4), driver = c(1, 1)), future = list(driver = c(
          1,
          1
        )), expected = c(3, 3))
      ), outcome = "predictions", error_phase = "none", error_code = "")
    })
    examples <- custom_author_scaffold_examples(values, scaffold, c("local", "global"), "driver")
    expect_length(examples, 4)
    expect_true(all(vapply(examples, function(example) identical(example$tolerance, 1e-08), logical(1))))
    expect_equal(vapply(examples, function(example) nrow(example$new_data), integer(1)), c(2, 2, 4, 4))
    for (example in examples) {
      expect_identical(example$new_data[c("Date", "Combo")], example$expected[c("Date", "Combo")])
      for (combo in unique(example$history$Combo)) {
        cutoff <- max(example$history$Date[example$history$Combo == combo])
        expect_identical(example$new_data$Date[example$new_data$Combo == combo], seq(cutoff, by = cadence, length.out = 3)[-1])
      }
    }
    invalid <- values
    invalid[[1]]$series[[1]]$expected <- 3
    expect_error(custom_author_scaffold_examples(invalid, scaffold, c("local", "global"), "driver"), "length")
    invalid <- values
    invalid[[1]]$series[[1]]$history$driver <- 1
    expect_error(custom_author_scaffold_examples(invalid, scaffold, c("local", "global"), "driver"), "length")
    expect_error(custom_author_scaffold_examples(c(values, values[1]), scaffold, c("local", "global"), "driver"), "scenario")
    altered <- scaffold
    altered$cutoff <- "2099-01-01"
    expect_error(custom_author_scaffold_examples(values, altered, c("local", "global"), "driver"), "scaffold")
  }
})

test_that("typed example validation preserves driver types and precise fields", {
  metadata <- list(date_type = "month", forecast_horizon = 1, schema = list(amount = "numeric", enabled = "logical", segment = "factor"))
  predictors <- c("amount", "enabled", "segment")
  protocol <- custom_author_protocol(metadata, "local", predictors, "base_only", FALSE)
  values <- lapply(protocol$scaffold$scenarios, function(scenario) list(
    id = scenario$id, rationale = "The mean of two and four is three.",
    series = list(list(combo = "example_a", history = list(
      Target = c(2, 4), amount = c(1, 2), enabled = c(TRUE, FALSE),
      segment = c("A", "B")
    ), future = list(amount = 1, enabled = TRUE, segment = "A"), expected = 3)), outcome = "predictions",
    error_phase = "none", error_code = ""
  ))
  materialized <- custom_author_scaffold_examples(values, protocol$scaffold, "local", predictors, predictor_types = protocol$predictor_types)
  expect_type(materialized[[1]]$history$amount, "double")
  expect_type(materialized[[1]]$history$enabled, "logical")
  expect_type(materialized[[1]]$history$segment, "character")
  for (field in c("history", "future", "expected")) {
    invalid <- values
    if (field == "history")
      invalid[[1]]$series[[1]]$history$amount <- c("A", "B")
    if (field == "future")
      invalid[[1]]$series[[1]]$future$enabled <- 1
    if (field == "expected")
      invalid[[1]]$series[[1]]$expected <- c(3, 3)
    error <- tryCatch(custom_author_scaffold_examples(invalid, protocol$scaffold, "local", predictors, predictor_types = protocol$predictor_types),
      error = identity
    )
    expect_identical(error$code, "invalid_examples_shape")
    expect_identical(error$field, paste0("examples.", field))
  }
})

test_that("consented dependency checks precede candidate execution", {
  fixture <- validation_fixture()
  definition <- fixture$definition
  definition$packages <- "stats"
  definition$source[["fit"]] <- "function(data, context, parameters) stats::not_a_real_export(data$Target)"
  definition$version_id <- custom_model_definition_digest(definition)
  policy <- list(mode = "installed_finnts", available = c("base", "stats"))
  report <- custom_author_validate(definition, fixture$examples, fixture$data, 60, package_policy = policy)
  expect_false(report$passed)
  expect_identical(report$failure$reason, "source_dependency")
  expect_identical(report$failure$dependency$references[[1]]$reason, "unexported_member")
  expect_false(grepl("private-a|private-b", jsonlite::toJSON(report)))
  testthat::local_mocked_bindings(custom_model_package_available = function(package) FALSE, .package = "finnts")
  report <- custom_author_validate(definition, fixture$examples, fixture$data, 60, package_policy = policy)
  expect_identical(report$failure$dependency$references[[1]]$reason, "unavailable_namespace")
})

test_that("new protocol fits and serialized forecasts reuse only pinned dependencies", {
  fixture <- validation_fixture()
  contract <- fixture$definition[c(
    "name", "instructions", "interpretation", "model_type", "requirements", "fixed_parameters",
    "packages"
  )]
  contract$authoring_protocol <- 2L
  contract$package_policy <- list(mode = "installed_finnts", available = c("base", "dplyr"))
  definition <- custom_author_definition(contract, list(
    fit_body = "mean(dplyr::pull(data, 'Target'))", predict_body = "rep(object, nrow(new_data))",
    helpers = list(), status = "candidate", conflict = ""
  ))
  normalized <- custom_author_definition(contract, list(
    fit_body = "function(data, context, parameters) mean(dplyr::pull(data, 'Target'))",
    predict_body = "function(object, new_data, context) rep(object, nrow(new_data))", helpers = list(), status = "candidate",
    conflict = ""
  ))
  expect_identical(normalized$source, definition$source)
  expect_identical(normalized$version_id, definition$version_id)
  definition <- normalized
  expect_identical(definition$packages, "dplyr")
  report <- custom_author_validate(definition, fixture$examples, fixture$data, 60, package_policy = contract$package_policy)
  expect_true(report$passed)
  testthat::local_mocked_bindings(
    custom_author_request = function(...) stop("Unexpected provider"), custom_author_assemble = function(...) stop("Unexpected source normalization"),
    custom_author_package_policy = function(...) stop("Unexpected package discovery"), custom_author_example_scaffold = function(...) stop("Unexpected example generation"),
    .package = "finnts"
  )
  training <- fixture$data$data
  workflow <- custom_model_workflow(definition, recipes::recipe(Target ~ Date + Combo, data = training), "global", "R1",
    "month", 1, "original",
    allow_code = TRUE
  )
  fitted <- generics::fit(workflow, data = training)
  request <- data.frame(Date = as.Date("2021-07-01"), Combo = c("private-b", "private-a"))
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fitted, path)
  expect_identical(predict(fitted, request)$.pred, c(150, 150))
  expect_identical(predict(readRDS(path), request), predict(fitted, request))
})

# Build a local monthly percentage-growth rule with independent literal oracles.
# Four annual observations supply three rates; the unequal rates distinguish an
# arithmetic mean from CAGR. Separate positive history supplies a real holdout.
percentage_growth_fixture <- function() {
  metadata <- list(date_type = "month", forecast_horizon = 3)
  protocol <- custom_author_protocol(metadata, "local", character(), "base_only", FALSE)
  values <- lapply(protocol$scaffold$scenarios, function(scenario) list(
    id = scenario$id, rationale = if (scenario$edge_case) "Constant fifty has three zero percentage changes and remains fifty." else "100, 120, 150, 180 have rates 0.2, 0.25, 0.2; 180 times (1 + their arithmetic mean) is 219.",
    series = list(list(combo = "example_a", history = list(Target = if (scenario$edge_case) rep(50, 48) else rep(c(
      100,
      120, 150, 180
    ), each = 12)), future = list(), expected = if (scenario$edge_case) rep(50, 3) else rep(219, 3))),
    outcome = "predictions", error_phase = "none", error_code = ""
  ))
  proposal <- list(
    questions = character(), interpretation = "Prior-year seasonal value multiplied by one plus the arithmetic mean of three annual percentage changes.",
    parameters_json = "{\"growth_periods\":3}", examples = values, history_requirements = list(
      minimum_rows_per_series = 48L,
      required_lag_periods = c(12L, 24L, 36L, 48L)
    ), parameter_schema = list(list(
      name = "growth_periods", type = "number",
      units = "test quantity"
    )), error_policy = list(), validation_properties = character()
  )
  instructions <- "Use the prior yearly seasonal value plus the average growth over the last three years."
  contract <- custom_author_contract(proposal, instructions, metadata, "percentage_seasonal", "local", NULL, protocol)
  source <- list(
    fit_body = "function(data, context, parameters) list(history = data, periods = parameters$growth_periods)",
    predict_body = paste0(
      "function(object, new_data, context) vapply(seq_len(nrow(new_data)), function(row) { ", "annual <- vapply(seq_len(object$periods + 1L), function(lag_years) { ",
      "date <- as.POSIXlt(new_data$Date[row]); date$year <- date$year - lag_years; ", "position <- match(as.Date(date), object$history$Date); ",
      "if (is.na(position)) stop('Missing annual history'); object$history$Target[position] }, numeric(1)); ", "older <- annual[-1L]; if (any(older == 0)) stop('Growth denominator must be nonzero'); ",
      "annual[1L] * (1 + mean(annual[seq_len(object$periods)] / older - 1)) }, numeric(1))"
    ), helpers = list(), status = "candidate",
    conflict = ""
  )
  data <- list(data = data.frame(
    Date = seq(as.Date("2013-01-01"), by = "month", length.out = 96), Combo = "PRIVATE_PERCENTAGE_SERIES",
    Target = rep(c(100, 120, 150, 180, 225, 270, 337.5, 405), each = 12)
  ), metadata = metadata)
  list(contract = contract, proposal = proposal, protocol = protocol, source = source, data = data)
}

test_that("percentage examples retain signed values and reject undefined rates or wrong oracles", {
  fixture <- percentage_growth_fixture()
  definition <- custom_author_definition(fixture$contract, fixture$source)
  original <- serialize(fixture$contract$examples, NULL)
  report <- custom_author_validate(definition, fixture$contract$examples, fixture$data, 60, package_policy = fixture$protocol$package_policy)
  expect_true(report$passed)
  expect_true(report$serialization)
  expect_equal(fixture$contract$examples[[1]]$expected$.pred, rep(219, 3))
  expect_equal(fixture$contract$examples[[2]]$expected$.pred, rep(50, 3))
  for (variant in c("signed", "zero_numerator", "zero_denominator", "wrong_oracle")) {
    examples <- fixture$contract$examples
    if (variant == "signed") {
      examples[[1]]$history$Target <- -examples[[1]]$history$Target
      examples[[1]]$expected$.pred <- -219
    } else if (variant == "zero_numerator") {
      examples[[1]]$history$Target[37:48] <- 0
      examples[[1]]$expected$.pred <- 0
    } else if (variant == "zero_denominator")
      examples[[1]]$history$Target[1:12] <- 0
    else examples[[1]]$expected$.pred <- 220
    saved <- serialize(examples, NULL)
    result <- custom_author_validate(definition, examples, fixture$data, 60, package_policy = fixture$protocol$package_policy)
    expect_identical(result$passed, variant %in% c("signed", "zero_numerator"))
    if (variant == "zero_denominator")
      expect_identical(result$failure$phase, "example_predict")
    if (variant == "wrong_oracle")
      expect_identical(result$code, "intent_mismatch")
    expect_identical(serialize(examples, NULL), saved)
  }
  expect_identical(serialize(fixture$contract$examples, NULL), original)
})

# Mock only provider transport and input sampling; the complete automatic
# authoring, validation, persistence, workflow fit and RDS prediction are real.
test_that("automatic percentage growth records its default and reuses pinned forecasts", {
  fixture <- percentage_growth_fixture()
  protocol_builder <- custom_author_protocol
  invisible(NULL)
  requests <- list()
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) fixture$data, custom_author_request = function(session, stage, payload) {
      requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
      if (stage == "source")
        return(fixture$source)
      expect_identical(payload$automatic_policy$catalog_version, 2L)
      expect_match(payload$automatic_policy$catalog$growth_basis, "percentage", fixed = TRUE)
      c(fixture$proposal, list(defaults_used = c("growth_basis", "averaging_weights")))
    }, .package = "finnts"
  )
  root <- withr::local_tempdir()
  original <- serialize(fixture$proposal, NULL)
  output <- capture.output(model <- create_custom_model(fixture$contract$instructions, list(),
    input_data = data.frame(),
    name = fixture$contract$name, model_type = "local", approval = "automatic", control = custom_model_control(
      package_policy = "base_only",
      draft_path = root
    )
  ))
  expect_length(requests, 2L)
  expect_identical(vapply(requests, `[[`, character(1), "stage"), c("contract", "source"))
  contract <- requests[[2]]$payload$contract
  expect_identical(contract$examples, fixture$contract$examples)
  expect_identical(serialize(fixture$proposal, NULL), original)
  expect_true(model$validation$technical_passed)
  expect_identical(model$approval$mode, "automatic")
  expect_false(model$approval$intent_confirmed)
  expect_true(any(grepl("growth_basis", output, fixed = TRUE)))
  evidence <- jsonlite::fromJSON(model$validation$checks$automatic_defaults, simplifyVector = FALSE)
  expect_equal(evidence$catalog_version, 2)
  expect_identical(evidence$defaults_used$growth_basis, custom_author_defaults()$growth_basis)
  expect_false(grepl("PRIVATE_PERCENTAGE_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
  pointer <- file.path(list.dirs(root, recursive = FALSE), "current.rds")
  state <- custom_draft_load(pointer)$state
  expect_length(state$receipts, 1L)
  expect_identical(state$receipts[[1]]$identity, model$definition$version_id)
  expect_identical(create_custom_model(draft = pointer), model)
  defaults <- contract$automatic_defaults
  checks <- custom_author_checks(state$report, state$definition, state$contract_id, state$contract$examples,
    approval_mode = "automatic",
    automatic_defaults = defaults, contract = state$contract
  )
  expect_identical(checks$automatic_defaults, as.character(jsonlite::toJSON(defaults, auto_unbox = TRUE, null = "null")))
  for (version in list(1L, 0L, 3L, "2", c(2L, 2L))) {
    defaults <- contract$automatic_defaults
    defaults$catalog_version <- version
    expect_error(custom_author_checks(state$report, state$definition, state$contract_id, state$contract$examples,
      approval_mode = "automatic",
      automatic_defaults = defaults, contract = state$contract
    ), "catalog|fields", class = "finnts_custom_model_authoring_error")
  }
  defaults <- contract$automatic_defaults
  defaults$catalog_version <- NA_integer_
  expect_error(custom_author_checks(state$report, state$definition, state$contract_id, state$contract$examples,
    approval_mode = "automatic",
    automatic_defaults = defaults, contract = state$contract
  ), "finite and nonmissing", fixed = TRUE)
  local_mocked_bindings(
    custom_author_request = function(...) stop("Unexpected provider"), custom_author_assemble = function(...) stop("Unexpected normalization"),
    custom_author_automatic_policy = function(...) stop("Unexpected policy rebuild"), custom_author_validate = function(...) stop("Unexpected authoring validation"),
    .package = "finnts"
  )
  training <- fixture$data$data
  workflow <- custom_model_workflow(model$definition, recipes::recipe(Target ~ Date + Combo, data = training), "local",
    "R1", "month", 3, "original",
    allow_code = TRUE
  )
  fitted <- generics::fit(workflow, data = training)
  request <- data.frame(Date = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01")), Combo = "PRIVATE_PERCENTAGE_SERIES")
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fitted, path)
  expect_equal(predict(fitted, request)$.pred, rep(492.75, 3))
  expect_identical(predict(readRDS(path), request), predict(fitted, request))
})

test_that("typed seasonal examples preserve independent arithmetic and required history", {
  metadata <- list(date_type = "month", forecast_horizon = 3)
  protocol <- custom_author_protocol(metadata, "local", character(), "base_only", FALSE)
  values <- lapply(protocol$scaffold$scenarios, function(scenario) list(
    id = scenario$id, rationale = if (scenario$edge_case) "A constant fifty has zero annual change." else "Annual observations ninety, one hundred, one hundred ten and one hundred twenty give mean change ten and forecast one hundred thirty.",
    series = list(list(combo = "example_a", history = list(Target = if (scenario$edge_case) rep(50, 48) else rep(c(
      90,
      100, 110, 120
    ), each = 12)), future = list(), expected = if (scenario$edge_case) rep(50, 3) else rep(130, 3))),
    outcome = "predictions", error_phase = "none", error_code = ""
  ))
  proposal <- list(
    questions = character(), interpretation = "Prior-year value plus the mean of three absolute annual changes.",
    parameters_json = "{\"growth_periods\":3}", examples = values, parameter_schema = list(list(
      name = "growth_periods",
      type = "number", units = "test quantity"
    )), error_policy = list(), validation_properties = character(), history_requirements = list(
      minimum_rows_per_series = 1L,
      required_lag_periods = integer()
    )
  )
  contract <- custom_author_contract(
    proposal, "Test explicit absolute annual differences.", metadata, "typed_seasonal",
    "local", NULL, protocol
  )
  source <- list(
    fit_body = "list(history = data, growth_periods = parameters$growth_periods)", predict_body = paste0(
      "vapply(seq_len(nrow(new_data)), function(row) { ",
      "values <- vapply(seq_len(object$growth_periods + 1L), function(lag_years) { ", "date <- as.POSIXlt(new_data$Date[row]); date$year <- date$year - lag_years; ",
      "position <- match(as.Date(date), object$history$Date); ", "if (is.na(position)) stop('Missing annual history'); object$history$Target[position] }, numeric(1)); ",
      "positions <- seq_len(object$growth_periods); values[1L] + mean(values[positions] - values[positions + 1L]) }, numeric(1))"
    ),
    helpers = list(), status = "candidate", conflict = ""
  )
  definition <- custom_author_definition(contract, source)
  data <- list(data = data.frame(
    Date = seq(as.Date("2015-01-01"), by = "month", length.out = 96), Combo = "PRIVATE_SEASONAL",
    Target = rep(seq(90, 160, by = 10), each = 12)
  ), metadata = metadata)
  valid <- custom_author_validate(definition, contract$examples, data, 60, package_policy = protocol$package_policy)
  expect_true(valid$passed)
  expect_true(valid$serialization)
  history_protocol <- custom_author_protocol(metadata, "local", character(), "base_only", FALSE)
  history_proposal <- proposal
  history_proposal$history_requirements <- list(minimum_rows_per_series = 48L, required_lag_periods = c(
    12L, 24L, 36L,
    48L
  ))
  history_contract <- custom_author_contract(
    history_proposal, contract$instructions, metadata, contract$name, "local",
    NULL, history_protocol
  )
  history_definition <- custom_author_definition(history_contract, source)
  expect_identical(history_definition$version_id, definition$version_id)
  history_result <- custom_author_validate(history_definition, history_contract$examples, data, 60, package_policy = history_protocol$package_policy)
  expect_true(history_result$passed)
  expect_true(history_result$serialization)
  expect_identical(history_contract$examples, contract$examples)
  inconsistent <- proposal
  inconsistent$examples[[1]]$series[[1]]$expected <- rep(140, 3)
  wrong <- custom_author_contract(inconsistent, contract$instructions, metadata, contract$name, "local", NULL, protocol)
  rejected <- custom_author_validate(definition, wrong$examples, data, 60, package_policy = protocol$package_policy)
  expect_false(rejected$passed)
  expect_identical(rejected$failure$phase, "example_compare")
  expect_identical(rejected$failure$comparisons[[1]]$expected, 140)
  expect_identical(rejected$failure$comparisons[[1]]$actual, 130)
  inconsistent$examples[[1]]$series[[1]]$history$Target <- rep(c(100, 110, 120), each = 12)
  short <- custom_author_contract(inconsistent, contract$instructions, metadata, contract$name, "local", NULL, protocol)
  insufficient <- custom_author_validate(definition, short$examples, data, 60, package_policy = protocol$package_policy)
  expect_false(insufficient$passed)
  expect_identical(insufficient$failure$phase, "example_predict")
  expect_identical(insufficient$failure$source_message, "Missing annual history")
  expect_identical(short$examples[[1]]$expected$.pred, rep(140, 3))
})

# Portable reproduction of the captured month/year bug. The lookup and literal
# failure match the saved candidate; expected forecasts are independent literals.
validation_seasonal_failure_fixture <- function(correct = FALSE) {
  source <- c(
    lag_date_years = "function(dates, years) { if (!inherits(dates, 'Date')) dates <- as.Date(dates); lt <- as.POSIXlt(dates); lt$year <- lt$year - years; as.Date(lt) }",
    fit = "function(data, context, parameters) { stopifnot(all(c('Date', 'Combo', 'Target') %in% names(data)), !anyNA(data), all(is.finite(data$Target)), !anyDuplicated(data[c('Date', 'Combo')])); list(history = data, lag = parameters$lag) }",
    predict = paste0(
      "function(object, new_data, context) { ", "target_dates <- lag_date_years(new_data$Date, object$lag",
      if (correct) " / 12" else "", "); ", "lookup <- data.frame(.finnts_row = new_data$.finnts_row, target_date = target_dates, Combo = as.character(new_data$Combo)); ",
      "hist <- object$history; merged <- merge(lookup, hist, by.x = c('target_date', 'Combo'), by.y = c('Date', 'Combo'), all.x = TRUE, sort = FALSE); ",
      "if (any(is.na(merged$Target))) stop('Missing historical observation for required lag date'); ", "out <- data.frame(.finnts_row = merged$.finnts_row, .pred = merged$Target); out[order(out$.finnts_row), , drop = FALSE] }"
    )
  )
  definition <- new_custom_model_definition("seasonal_lookup", "Use the prior yearly seasonal value.", "Use the same calendar month one year earlier; lag is measured in months.",
    "local", source, list(
      predictors = character(), recipes = "R1", target_scale = "original", date_types = "month",
      forecast_horizon = 3, missing_data = "error"
    ),
    fixed_parameters = list(lag = 12)
  )
  history <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 12), Combo = "A", Target = seq(
    10,
    120, 10
  ))
  request <- data.frame(Date = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01")), Combo = "A")
  basic <- list(name = "basic_cycle", model_type = "local", history = history, new_data = request, expected = transform(request,
    .pred = c(10, 20, 30)
  ), tolerance = 1e-08, edge_case = FALSE, rationale = "Prior January, February and March targets are 10, 20 and 30.")
  edge <- basic
  edge$name <- "zero_negative"
  edge$history$Target <- c(-5, 0, -2, rep(1, 9))
  edge$expected$.pred <- c(-5, 0, -2)
  edge$edge_case <- TRUE
  edge$rationale <- "Prior targets -5, 0 and -2 stay unchanged."
  data <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 36), Combo = "PRIVATE_HOLDOUT", Target = rep(seq(
    10,
    120, 10
  ), 3))
  list(definition = definition, examples = list(basic, edge), data = list(data = data, metadata = list(
    date_type = "month",
    forecast_horizon = 3
  )))
}

test_that("captured seasonal failure retains phase and safe source evidence", {
  fixture <- validation_seasonal_failure_fixture()
  failed <- custom_author_validate(fixture$definition, fixture$examples, fixture$data, 60)
  expect_false(failed$passed)
  expect_identical(failed$code, "candidate_validation_error")
  expect_identical(failed$failure$phase, "example_predict")
  expect_identical(failed$failure$example_index, 1L)
  expect_identical(failed$failure$model_type, "local")
  expect_identical(failed$failure$version_id, fixture$definition$version_id)
  expect_identical(failed$failure$source_message, "Missing historical observation for required lag date")
  expect_false(grepl("PRIVATE_HOLDOUT", jsonlite::toJSON(failed), fixed = TRUE))
})

test_that("candidate failure taxonomy preserves privacy and arithmetic evidence", {
  fixture <- validation_fixture()
  for (phase in c("fit", "predict")) {
    definition <- fixture$definition
    definition$source[[phase]] <- if (phase == "fit")
      "function(data, context, parameters) stop(as.character(data$Target[[1]]))"
    else "function(object, new_data, context) stop(new_data$Combo[[1]])"
    definition$version_id <- custom_model_definition_digest(definition)
    report <- custom_author_validate(definition, fixture$examples, fixture$data, 60)
    expect_identical(report$failure$phase, paste0("example_", phase))
    expect_null(report$failure$source_message)
    expect_length(report$failure$comparisons, 0L)
  }
  for (defect in c("rows", "nonfinite", "state")) {
    definition <- fixture$definition
    if (defect == "rows")
      definition$source[["predict"]] <- "function(object, new_data, context) data.frame(.finnts_row=0,.pred=0)"
    if (defect == "nonfinite")
      definition$source[["predict"]] <- "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row,.pred=Inf)"
    if (defect == "state")
      definition$source[["fit"]] <- "function(data, context, parameters) new.env()"
    definition$version_id <- custom_model_definition_digest(definition)
    report <- custom_author_validate(definition, fixture$examples, fixture$data, 60)
    expect_identical(report$failure$reason, if (defect == "state")
      "nonportable_state"
    else "prediction_shape")
  }
  wrong <- validation_fixture(TRUE)
  report <- custom_author_validate(wrong$definition, wrong$examples, wrong$data, 60)
  expect_identical(report$code, "intent_mismatch")
  expect_identical(report$failure$phase, "example_compare")
  expect_identical(report$failure$comparisons[[1]]$expected, 0)
  expect_identical(report$failure$comparisons[[1]]$actual, 999)
  expect_lte(length(report$failure$comparisons), 4L)
  expect_false(grepl("private-a|private-b", jsonlite::toJSON(report)))
  for (message in c("private account", "token abc", "C:/private/file", "secret123", "person@example.com")) {
    expect_false(custom_author_safe_message(message))
  }
  expect_null(custom_author_source_message(simpleError("Unrelated failure"), c(fit = "function(...) stop('Different failure')")))
  altered <- report$failure
  altered$message <- "Invented guidance"
  expect_error(validate_custom_author_failure(altered), "Invalid bounded")
  altered <- report$failure
  altered$comparisons <- rep(altered$comparisons, 5L)
  expect_error(validate_custom_author_failure(altered), "Invalid bounded")
})

test_that("workflow and serialization I/O failures are not candidate repairs", {
  fixture <- validation_fixture()
  local({
    local_mocked_bindings(custom_run_workflows = function(...) stop("Workflow infrastructure failure"), .package = "finnts")
    expect_error(custom_author_validate(fixture$definition, fixture$examples, fixture$data, 60), "Workflow infrastructure failure")
  })
  for (operation in c("saveRDS", "readRDS")) local({
    bindings <- list(function(...) stop("Serialization I/O failure"))
    names(bindings) <- operation
    do.call(local_mocked_bindings, c(bindings, list(.package = "base", .env = environment())))
    expect_error(custom_author_validate(fixture$definition, fixture$examples, fixture$data, 60), "Serialization I/O failure")
  })
  definition <- fixture$definition
  definition$source[["predict"]] <- "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row,.pred=stats::runif(nrow(new_data)))"
  definition$version_id <- custom_model_definition_digest(definition)
  report <- custom_author_validate(definition, fixture$examples, fixture$data, 60)
  expect_identical(report$failure$phase, "example_roundtrip")
  expect_identical(report$failure$reason, "serialization_mismatch")
  definition <- fixture$definition
  definition$source[["fit"]] <- "function(data, context, parameters) { if (nrow(data) > 4) stop('Training window unsupported'); mean(data$Target) }"
  definition$version_id <- custom_model_definition_digest(definition)
  report <- custom_author_validate(definition, fixture$examples, fixture$data, 60)
  expect_identical(report$failure$phase, "holdout_fit")
  expect_null(report$failure$example_index)
  expect_length(report$failure$comparisons, 0L)
  expect_false(grepl("private-a|private-b", jsonlite::toJSON(report)))
})

# Small independent oracles span the ordinary-R rules promised by this repair.
# Real fits/holdouts/RDS checks run for every row; no mock success reports or tuning.
validation_rule_matrix <- function() {
  scalar_predict <- "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row,.pred=rep(object,nrow(new_data)))"
  seasonal_fit <- "function(data, context, parameters) list(data=data, growth=parameters$growth)"
  seasonal_predict <- paste0(
    "function(object, new_data, context) { keys <- paste0(as.integer(format(new_data$Date,'%Y'))-1L,format(new_data$Date,'-%m')); ",
    "values <- object$data$Target[match(keys,format(object$data$Date,'%Y-%m'))]; data.frame(.finnts_row=new_data$.finnts_row,.pred=values*object$growth) }"
  )
  list(last_value = list(
    fit = "function(data, context, parameters) data$Target[[nrow(data)]]", predict = scalar_predict,
    expected = rep(48, 3)
  ), mean_offset = list(
    fit = "function(data, context, parameters) mean(data$Target)+parameters$offset",
    predict = scalar_predict, parameters = list(offset = 5), expected = rep(29.5, 3)
  ), rolling_mean = list(
    fit = "function(data, context, parameters) mean(data$Target[(nrow(data)-2L):nrow(data)])",
    predict = scalar_predict, expected = rep(47, 3)
  ), weighted_mean = list(
    fit = "function(data, context, parameters) sum(data$Target[(nrow(data)-2L):nrow(data)]*parameters$weights)/sum(parameters$weights)",
    predict = scalar_predict, parameters = list(weights = c(1, 2, 3)), expected = rep(142 / 3, 3)
  ), seasonal = list(
    fit = seasonal_fit,
    predict = seasonal_predict, parameters = list(growth = 1), expected = c(37, 38, 39)
  ), seasonal_growth = list(
    fit = seasonal_fit,
    predict = seasonal_predict, parameters = list(growth = 1.1), expected = c(40.7, 41.8, 42.9)
  ), mean_yoy_growth = list(
    fit = "function(data, context, parameters) data",
    predict = paste0(
      "function(object, new_data, context) { values <- vapply(new_data$Date, function(date) { date <- as.Date(date,origin='1970-01-01'); ",
      "keys <- paste0(as.integer(format(date,'%Y'))-(1:4),format(date,'-%m')); prior <- object$Target[match(keys,format(object$Date,'%Y-%m'))]; ",
      "if (any(prior[-1] == 0)) stop('Growth denominator must be nonzero'); prior[[1]]*(1+mean(prior[1:3]/prior[2:4]-1)) },numeric(1)); ",
      "data.frame(.finnts_row=new_data$.finnts_row,.pred=values) }"
    ), expected = rep(205.92, 3)
  ), driver_ratio = list(
    fit = "function(data, context, parameters) { if (any(data$Driver == 0)) stop('Driver denominator must be nonzero'); mean(data$Target/data$Driver) }",
    predict = "function(object, new_data, context) data.frame(.finnts_row=new_data$.finnts_row,.pred=object*new_data$Driver)",
    predictors = "Driver", expected = c(50, 100, 0)
  ), conditional = list(
    fit = "function(data, context, parameters) if (data$Target[[nrow(data)]] >= parameters$threshold) 100 else 0",
    predict = scalar_predict, parameters = list(threshold = 47), expected = rep(100, 3)
  ))
}

test_that("ordinary R business rule matrix passes real numerical and holdout validation", {
  rules <- validation_rule_matrix()
  for (name in names(rules)) {
    rule <- rules[[name]]
    history <- data.frame(Date = seq(as.Date("2019-01-01"), by = "month", length.out = 48), Combo = "example", Target = as.numeric(1:48))
    data <- data.frame(Date = seq(as.Date("2019-01-01"), by = "month", length.out = 60), Combo = "private", Target = as.numeric(1:60))
    request <- data.frame(Date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")), Combo = "example")
    if (name == "mean_yoy_growth") {
      history$Target <- rep(c(100, 110, 132, 171.6), each = 12)
      data$Target <- rep(c(100, 110, 132, 171.6, 205.92), each = 12)
    }
    if (name == "driver_ratio") {
      history$Driver <- 2 * history$Target
      data$Driver <- 2 * data$Target
      request$Driver <- c(100, 200, 0)
    }
    expected <- request[c("Date", "Combo")]
    expected$.pred <- rule$expected
    example <- list(
      name = "ordinary", model_type = "local", history = history, new_data = request, expected = expected,
      tolerance = 1e-08, edge_case = FALSE, rationale = "Independent arithmetic constants for the fixed rule."
    )
    edge <- example
    edge$name <- "constant_history"
    edge$history$Target <- if (name == "mean_yoy_growth")
      100
    else 0
    edge$expected$.pred <- if (name == "mean_offset")
      5
    else if (name == "mean_yoy_growth")
      100
    else 0
    edge$edge_case <- TRUE
    definition <- new_custom_model_definition(name, "Use the specified fixed business rule.", "Keep the explicit calculation and parameters unchanged.",
      "local", c(fit = rule$fit, predict = rule$predict), list(
        predictors = rule$predictors %||% character(), recipes = "R1",
        target_scale = "original", date_types = "month", forecast_horizon = 3, missing_data = "error"
      ),
      fixed_parameters = rule$parameters %||%
        list()
    )
    report <- custom_author_validate(definition, list(example, edge), list(data = data, metadata = list(
      date_type = "month",
      forecast_horizon = 3
    )), 60)
    expect_true(report$passed, info = name)
    expect_true(report$serialization, info = name)
    expect_length(report$holdouts, 1L)
    expect_true(all(vapply(report$examples, function(result) result$maximum_error <= result$tolerance, logical(1))),
      info = name
    )
    if (name == "driver_ratio") {
      example$history$Driver[[1]] <- 0
      failed <- custom_author_validate(definition, list(example, edge), list(data = data, metadata = list(
        date_type = "month",
        forecast_horizon = 3
      )), 60)
      expect_false(failed$passed)
      expect_identical(failed$failure$phase, "example_fit")
    }
  }
  fixture <- validation_fixture()
  expect_true(custom_author_validate(fixture$definition, fixture$examples, fixture$data, 60)$passed)
  quarterly <- validation_seasonal_failure_fixture(TRUE)
  quarterly$definition$requirements$date_types <- "quarter"
  quarterly$definition$version_id <- custom_model_definition_digest(quarterly$definition)
  quarterly$data$metadata$date_type <- "quarter"
  quarterly$data$data$Date <- seq(as.Date("2014-01-01"), by = "quarter", length.out = 36)
  for (index in seq_along(quarterly$examples)) {
    quarterly$examples[[index]]$history$Date <- seq(as.Date("2020-01-01"), by = "quarter", length.out = 12)
    quarterly$examples[[index]]$new_data$Date <- as.Date(c("2023-01-01", "2023-04-01", "2023-07-01"))
    quarterly$examples[[index]]$expected$Date <- quarterly$examples[[index]]$new_data$Date
    quarterly$examples[[index]]$expected$.pred <- if (index == 1L)
      c(90, 100, 110)
    else rep(1, 3)
  }
  expect_true(custom_author_validate(quarterly$definition, quarterly$examples, quarterly$data, 60)$passed)
})

test_that("cutoff preflight preserves explicit and inferred bounds across cadences", {
  for (cadence in c("day", "week", "month", "quarter", "year")) {
    dates <- seq(as.Date("2020-01-01"), by = cadence, length.out = 7)
    raw <- data.frame(Date = rep(dates, 2), id = rep(c("A", "B"), each = 7), value = rep(c(
      0, -5, 10, 20, NA_real_, NA_real_,
      NA_real_
    ), 2), Driver = 0)
    before <- serialize(raw, NULL)
    inferred <- custom_author_preflight(raw, "id", "value", cadence, 2, "Driver", NULL, NULL)
    expect_identical(inferred$hist_end_date, dates[[4]])
    expect_identical(inferred$cutoff_source, "inferred")
    expect_identical(inferred$series_checked, 2L)
    changed <- raw
    changed$value[changed$Date > dates[[4]]] <- 999999
    explicit <- custom_author_preflight(changed, "id", "value", cadence, 2, "Driver", dates[[1]], dates[[4]])
    expect_identical(explicit$hist_end_date, dates[[4]])
    expect_identical(explicit$cutoff_source, "explicit")
    expect_identical(serialize(raw, NULL), before)
    no_future <- raw[raw$Date <= dates[[4]], ]
    expect_error(custom_author_preflight(no_future, "id", "value", cadence, 2, "Driver", NULL, dates[[4]]), "Future drivers must cover")
    expect_identical(custom_author_preflight(no_future, "id", "value", cadence, 2, NULL, NULL, NULL)$hist_end_date, dates[[4]])
    ragged <- raw[!(raw$id == "A" & raw$Date == dates[[1]]), ]
    expect_error(custom_author_preflight(ragged, "id", "value", cadence, 2, "Driver", NULL, dates[[4]]), "complete historical window")
    expect_identical(
      custom_author_preflight(ragged, "id", "value", cadence, 2, "Driver", dates[[2]], dates[[4]])$hist_start_date,
      dates[[2]]
    )
    raw$value <- NA_real_
    expect_error(custom_author_preflight(raw, "id", "value", cadence, 2, "Driver", NULL, NULL), "observed historical targets")
  }
})

# Four series ensure malformed rows outside the three-series authoring sample
# are rejected before any temporary run is created. Every mutation breaks one
# raw-history/cutoff/future-coverage invariant without changing valid column types.
test_that("raw preflight rejects incomplete panels before sampling or preparation", {
  dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 15)
  raw <- data.frame(Date = rep(dates, 4), id = rep(paste0("private-", 1:4), each = 15), value = rep(c(rep(100, 12), rep(
    NA_real_,
    3
  )), 4), Driver = seq_len(60))
  cutoff <- dates[[12]]
  labels <- unique(raw$id)
  omitted <- labels[order(vapply(labels, hash_data, character(1)), method = "radix")][[4]]
  historical_row <- which(raw$id == omitted & raw$Date == dates[[6]])
  last_row <- which(raw$id == omitted & raw$Date == cutoff)
  future_row <- which(raw$id == omitted & raw$Date == dates[[14]])
  local_mocked_bindings(set_run_info = function(...) stop("Unexpected preparation"), .package = "finnts")
  for (case in c(
    "missing_actual", "missing_period", "lagging_series", "missing_future_row", "missing_future_driver", "infinite_future_driver",
    "misaligned_cutoff", "future_only_series", "inferred_ragged"
  )) {
    changed <- raw
    end <- cutoff
    if (case == "missing_actual")
      changed$value[historical_row] <- NA_real_
    if (case == "missing_period")
      changed <- changed[-historical_row, ]
    if (case == "lagging_series")
      changed$value[last_row] <- NA_real_
    if (case == "missing_future_row")
      changed <- changed[-future_row, ]
    if (case == "missing_future_driver")
      changed$Driver[future_row] <- NA_real_
    if (case == "infinite_future_driver")
      changed$Driver[future_row] <- Inf
    if (case == "misaligned_cutoff")
      end <- as.Date("2024-12-31")
    if (case == "future_only_series")
      changed <- rbind(changed, transform(raw[raw$id == omitted & raw$Date > cutoff, ], id = "private-new"))
    if (case == "inferred_ragged") {
      changed$value[last_row] <- NA_real_
      end <- NULL
    }
    error <- tryCatch(custom_author_data(changed, NULL, "id", "value", "month", 3, "Driver", NULL, end), error = identity)
    expect_s3_class(error, "finnts_custom_model_authoring_error")
    expect_false(grepl("Unexpected preparation", conditionMessage(error), fixed = TRUE), info = case)
  }
})

# One real preparation supplies four stored series. Intercepted reads simulate
# a missing future driver outside the sample, then count reads on the valid path
# to ensure complete coverage does not reload the selected artifacts.
test_that("prepared driver preflight covers unsampled series and reuses exact reads", {
  dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 15)
  raw <- data.frame(Date = rep(dates, 4), id = rep(paste0("private-", 1:4), each = 15), value = rep(c(rep(100, 12), rep(
    NA_real_,
    3
  )), 4), Driver = seq_len(60))
  info <- set_run_info(project_name = "prepared-driver-preflight", path = withr::local_tempdir(), add_unique_id = FALSE)
  prep_data(info, raw, "id", "value", "month", 3,
    external_regressors = "Driver", hist_end_date = dates[[12]], recipes_to_run = "R1",
    stationary = FALSE, box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE, multistep_horizon = FALSE
  )
  paths <- sort(as.character(local_artifact_inventory(info, "prep_data", "-*-R1")))
  expect_length(paths, 4L)
  read <- read_exact_artifact
  reads <- character()
  corrupt <- TRUE
  local_mocked_bindings(read_exact_artifact = function(run_info, path, ...) {
    reads <<- c(reads, as.character(path))
    data <- read(run_info, path, ...)
    if (corrupt && identical(as.character(path), paths[[4]])) {
      data$Driver[which(data$Date > dates[[12]])[[1]]] <- NA_real_
    }
    data
  }, .package = "finnts")
  expect_error(custom_author_data(NULL, info, NULL, NULL, NULL, NULL, NULL, NULL, NULL), "Future driver values", class = "finnts_custom_model_authoring_error")
  corrupt <- FALSE
  reads <- character()
  sampled <- custom_author_data(NULL, info, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capture_context = TRUE)
  expect_identical(sampled$metadata$total_series, 4L)
  expect_equal(sampled$metadata$sampled_series, 3)
  expect_true(all(paths %in% names(sampled$sources)))
  expect_identical(as.integer(table(factor(reads[reads %in% paths], levels = paths))), rep(1L, 4))
})

test_that("raw authoring retains declared raw drivers without exposing future rows", {
  dates <- seq(as.Date("2021-01-01"), by = "month", length.out = 27)
  cutoff <- dates[[24]]
  raw <- data.frame(Date = rep(dates, 2), id = rep(c("private-a", "private-b"), each = 27), value = rep(c(
    rep(100, 24),
    rep(NA_real_, 3)
  ), 2), Units = seq_len(54) + 10, Price = rep(c(2.5, 3.5), each = 27), Flag = rep(
    c(TRUE, FALSE),
    27
  ), Channel = rep(c("retail", "wholesale"), each = 27))
  predictors <- c("Units", "Price", "Flag", "Channel")
  before <- serialize(raw, NULL)
  sampled <- custom_author_data(raw, NULL, "id", "value", "month", 3, predictors, NULL, cutoff)
  expected <- raw[raw$Date <= cutoff, ]
  expect_equal(sampled$data$Units, expected$Units)
  expect_equal(sampled$data$Price, expected$Price)
  expect_identical(sampled$data$Flag, expected$Flag)
  expect_identical(sampled$data$Channel, expected$Channel)
  expect_true(all(predictors %in% names(sampled$metadata$schema)))
  expect_identical(custom_author_predictor_types(sampled$metadata, predictors), list(
    Units = "number", Price = "number",
    Flag = "boolean", Channel = "string"
  ))
  expect_true(all(sampled$data$Date <= cutoff))
  expect_equal(nrow(sampled$data), 48)
  expect_identical(serialize(raw, NULL), before)
  expect_false(grepl("private-a|private-b", jsonlite::toJSON(sampled$metadata)))
  info <- set_run_info(project_name = "authoring-drivers", path = withr::local_tempdir(), add_unique_id = FALSE)
  prep_data(info, raw, "id", "value", "month", 3,
    external_regressors = predictors, hist_end_date = cutoff, recipes_to_run = "R1",
    stationary = FALSE, box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE, multistep_horizon = FALSE
  )
  prepared <- custom_author_data(NULL, info, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
  for (predictor in predictors) expect_equal(sampled$data[[predictor]], prepared$data[[predictor]])
  changed <- raw
  changed$value[changed$Date > cutoff] <- 999999
  changed$Units[changed$Date > cutoff] <- 999999
  repeated <- custom_author_data(changed, NULL, "id", "value", "month", 3, predictors, NULL, cutoff)
  expect_identical(repeated$data, sampled$data)
  for (invalid in c(NA_real_, Inf)) {
    missing <- raw
    missing$Units[[1]] <- invalid
    expect_error(custom_author_data(missing, NULL, "id", "value", "month", 3, predictors, NULL, cutoff), "complete finite observed driver history")
  }
})

# Mock only proposal transport; raw sampling, model creation, worked examples,
# chronological holdouts and fitted-state serialization use the actual code.
test_that("raw driver models can be authored before a forecast run exists", {
  history <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")), Combo = "example", Target = c(20, 40), Units = c(
    10,
    20
  ))
  future <- data.frame(Date = as.Date(c("2020-03-01", "2020-04-01", "2020-05-01")), Combo = "example", Units = c(
    15, 16,
    17
  ))
  basic <- list(name = "units", model_type = "local", history = history, new_data = future, expected = transform(future[c(
    "Date",
    "Combo"
  )], .pred = c(30, 32, 34)), tolerance = 1e-08, edge_case = FALSE, rationale = "Each unit earns two: 15,16,17 produce 30,32,34.")
  edge <- basic
  edge$name <- "zero_units"
  edge$history$Units <- edge$history$Target <- 0
  edge$new_data$Units <- edge$expected$.pred <- 0
  edge$edge_case <- TRUE
  edge$rationale <- "Zero units earn zero."
  requests <- list()
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_request = function(session, stage, payload) {
      requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
      if (stage == "contract")
        return(list(
          questions = character(), interpretation = "Two revenue units per unit sold.", parameters_json = "{\"rate\":2}",
          parameter_schema = list(list(name = "rate", type = "number", units = "revenue per unit")), error_policy = list(),
          validation_properties = c("independent_series", "fixed_window"), history_requirements = list(
            minimum_rows_per_series = 0L,
            required_lag_periods = integer()
          ), defaults_used = character()
        ))
      list(
        status = "candidate", conflict = "", fit_body = "parameters$rate", predict_body = "object * new_data$Units",
        helpers = list()
      )
    }, .package = "finnts"
  )
  raw <- data.frame(Date = seq(as.Date("2021-01-01"), by = "month", length.out = 27), id = "PRIVATE_DRIVER_SERIES", value = c(2 *
    seq_len(24), rep(NA_real_, 3)), Units = seq_len(27))
  original <- serialize(raw, NULL)
  model <- create_custom_model("Revenue equals Units times two.", list(),
    input_data = raw, combo_variables = "id", target_variable = "value",
    date_type = "month", forecast_horizon = 3, external_regressors = "Units", model_type = "local", name = "raw_driver",
    approval = "automatic", validation_examples = list(basic, edge)
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_true(model$validation$technical_passed)
  expect_identical(requests[[1]]$payload$protocol$version, 6L)
  expect_identical(model$definition$requirements$predictors, "Units")
  expect_match(model$validation$checks$driver_conditioning, "conditional on supplied driver values", fixed = TRUE)
  expect_match(model$validation$checks$authoring_cutoff, "2022-12-01 (inferred)", fixed = TRUE)
  expect_identical(serialize(raw, NULL), original)
  expect_identical(vapply(requests, `[[`, character(1), "stage"), c("contract", "source"))
  expect_identical(requests[[1]]$payload$protocol$predictor_types, list(Units = "number"))
  expected_inputs <- c("Date", "Combo", "Target", "Units")
  expect_setequal(names(requests[[1]]$payload$metadata$schema), expected_inputs)
  workflows <- custom_run_workflows(model$definition, history, list(date_type = "month", forecast_horizon = 3))
  recipe <- workflows::extract_recipe(workflows$Model_Workflow[[1]], estimated = FALSE)
  expect_setequal(names(requests[[1]]$payload$metadata$schema), recipe$var_info$variable)
  expect_false(grepl("PRIVATE_DRIVER_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
  approved <- serialize(model, NULL)
  local_mocked_bindings(
    custom_author_request = function(...) stop("Unexpected reauthoring"), custom_author_data = function(...) stop("Unexpected authoring sample"),
    .package = "finnts"
  )
  for (months in c(12L, 15L)) {
    training <- data.frame(
      Date = seq(as.Date("2025-01-01"), by = "month", length.out = months), Combo = "later-series",
      Units = seq_len(months), Target = 2 * seq_len(months)
    )
    request <- data.frame(Date = seq(max(training$Date), by = "month", length.out = 4)[-1], Combo = "later-series", Units = months +
      1:3)
    workflow <- custom_model_workflow(model$definition, recipes::recipe(Target ~ Date + Combo + Units, data = training),
      "local", "R1", "month", 3, "original",
      allow_code = TRUE
    )
    fitted <- generics::fit(workflow, training)
    context <- workflows::extract_fit_parsnip(fitted)$fit$context
    expect_identical(context$cutoff, max(training$Date))
    expect_identical(context$series_cutoffs[["later-series"]], max(training$Date))
    expect_equal(predict(fitted, request)$.pred, 2 * request$Units)
    expect_identical(serialize(model, NULL), approved)
  }
})

test_that("raw and prepared authoring data preserve inputs and existing artifacts", {
  raw <- data.frame(Date = seq(as.Date("2021-01-01"), by = "month", length.out = 12), id = "private-id", value = 100)
  before <- raw
  sampled <- custom_author_data(raw, NULL, "id", "value", "month", 1, NULL, NULL, NULL)
  expect_equal(unique(sampled$data$Target[!is.na(sampled$data$Target)]), 100)
  expect_identical(raw, before)
  expect_false(grepl("private-id", jsonlite::toJSON(sampled$metadata), fixed = TRUE))
  info <- set_run_info(project_name = "authoring-data", path = withr::local_tempdir(), add_unique_id = FALSE)
  prep_data(info, raw, "id", "value", "month", 1,
    recipes_to_run = "R1", stationary = FALSE, clean_missing_values = FALSE,
    clean_outliers = FALSE
  )
  files <- list.files(info$path, recursive = TRUE, full.names = TRUE)
  hashes <- tools::md5sum(files)
  inventory <- finnts:::local_artifact_inventory
  discoveries <- 0L
  read_artifact <- read_exact_artifact
  reads <- character()
  testthat::local_mocked_bindings(local_artifact_inventory = function(...) {
    discoveries <<- discoveries + 1L
    inventory(...)
  }, read_exact_artifact = function(...) {
    reads <<- c(reads, as.character(list(...)[[2L]]))
    read_artifact(...)
  }, .package = "finnts")
  prepared <- custom_author_data(NULL, info, NULL, NULL, NULL, NULL, NULL, NULL, NULL, forecast_approach = NULL)
  expect_identical(prepared$metadata$resolved_inputs$forecast_approach, "bottoms_up")
  expect_identical(sum(reads == local_artifact_path(info, "logs", extension = "csv")), 1L)
  expect_equal(prepared$data[c("Date", "Combo", "Target")], sampled$data[c("Date", "Combo", "Target")])
  expect_identical(discoveries, 1L)
  info$combo <- hash_data("private-id")
  testthat::local_mocked_bindings(local_artifact_inventory = function(...) stop("Unexpected known-path discovery"), .package = "finnts")
  known <- custom_author_data(NULL, info, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
  expect_identical(known$data, prepared$data)
  request <- NULL
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_request = function(session, stage, payload) {
      request <<- payload
      fields <- custom_author_contract_fields(payload$protocol, payload$name, payload$model_type)
      response <- stats::setNames(rep(list(NULL), length(fields)), fields)
      response$questions <- list(list(kind = "business", topic = "business_context", question = "Which multiplier?"))
      response
    }, .package = "finnts"
  )
  draft <- create_custom_model("Use the specified multiplier.", list(),
    run_info = info, name = "prepared_schema", model_type = "local",
    control = custom_model_control(draft_path = withr::local_tempdir())
  )
  expect_setequal(names(request$metadata$schema), c("Date", "Combo", "Target"))
  expect_true("Date_index.num" %in% names(known$metadata$schema))
  expect_identical(draft$metadata$schema, known$metadata$schema)
  expect_identical(draft$pending$stage, "clarify")
  expect_identical(tools::md5sum(files), hashes)
  expect_error(custom_author_data(NULL, info, NULL, NULL, "week", NULL, NULL, NULL, NULL), "metadata")
})

test_that("reviewed example predictions align by keys regardless of row names", {
  fixture <- validation_fixture()
  fixture$examples[[2]]$expected <- fixture$examples[[2]]$expected[2:1, ]
  examples <- custom_author_examples(fixture$examples, fixture$definition, fixture$data$metadata)
  expect_identical(examples[[2]]$expected[c("Combo", "Date")], examples[[2]]$new_data[c("Combo", "Date")])
})

test_that("local seasonal contracts identify undeclared global examples specifically", {
  fixture <- validation_fixture()
  fixture$definition$model_type <- "local"
  fixture$definition$requirements$forecast_horizon <- 3
  fixture$definition$version_id <- custom_model_definition_digest(fixture$definition)
  metadata <- list(date_type = "month", forecast_horizon = 3)
  history <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-12-01")), Combo = "A", Target = c(
    100,
    200, 300, 1200
  ))
  request <- data.frame(Date = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01")), Combo = "A")
  local <- list(name = "local_basic", model_type = "local", history = history, new_data = request, expected = transform(request,
    .pred = c(111, 222, 333)
  ), tolerance = 1e-08, edge_case = FALSE, rationale = "Prior-year 100, 200, 300 times 1.11 are 111, 222, 333.")
  global <- local
  global$name <- "global_basic"
  global$model_type <- "global"
  global$history <- rbind(transform(history, Target = c(10, 20, 30, 120)), transform(history, Combo = "B", Target = c(
    5,
    15, 25, 75
  )))
  global$new_data <- rbind(request, transform(request, Combo = "B"))
  global$expected <- transform(global$new_data, .pred = c(11.1, 22.2, 33.3, 5.55, 16.65, 27.75))
  global$rationale <- "Each series uses its own prior-year month times 1.11."
  edge <- local
  edge$name <- "edge_negative_zero"
  edge$history$Target <- c(0, -50, 100, 0)
  edge$expected$.pred <- c(0, -55.5, 111)
  edge$edge_case <- TRUE
  edge$rationale <- "Prior-year 0, -50, 100 times 1.11 are 0, -55.5, 111."
  examples <- list(local, global, edge)
  before <- examples
  error <- tryCatch(custom_author_examples(examples, fixture$definition, metadata), error = identity)
  expect_s3_class(error, "finnts_custom_model_authoring_error")
  expect_identical(error$stage, "examples")
  expect_identical(error$code, "example_mode")
  expect_match(conditionMessage(error), "declared model modes")
  expect_identical(examples, before)
  expect_length(custom_author_examples(list(local, edge), fixture$definition, metadata), 2L)
})

test_that("example mode diagnostics preserve local global and mixed capability boundaries", {
  fixture <- validation_fixture()
  for (modes in list("local", "global", c("local", "global"))) {
    definition <- fixture$definition
    definition$model_type <- modes
    definition$version_id <- custom_model_definition_digest(definition)
    examples <- fixture$examples
    if (length(modes) == 1L) {
      selected <- examples[[if (modes == "local")
        1L
      else 2L]]
      second <- selected
      second$name <- "second"
      second$edge_case <- TRUE
      examples <- list(selected, second)
    }
    expect_length(custom_author_examples(examples, definition, fixture$data$metadata), 2L)
    before <- examples
    invalid <- examples
    invalid[[1]]$model_type <- if (identical(modes, "local"))
      "global"
    else if (identical(modes, "global"))
      "local"
    else "pooled"
    error <- tryCatch(custom_author_examples(invalid, definition, fixture$data$metadata), error = identity)
    expect_identical(error$code, "example_mode")
    expect_identical(error$stage, "examples")
    expect_identical(examples, before)
  }
  for (mode in list(NULL, NA_character_, "", c("local", "global"), 1, list("local"))) {
    invalid <- fixture$examples
    invalid[[1]]$model_type <- mode
    error <- tryCatch(custom_author_examples(invalid, fixture$definition, fixture$data$metadata), error = identity)
    expect_s3_class(error, "finnts_custom_model_authoring_error")
    expect_null(error$code)
    expect_match(conditionMessage(error), "metadata or tolerance")
  }
  for (tolerance in list(-1, 0.02, NA_real_, Inf, "1e-8")) {
    invalid <- fixture$examples
    invalid[[1]]$tolerance <- tolerance
    expect_error(custom_author_examples(invalid, fixture$definition, fixture$data$metadata), "metadata or tolerance")
  }
  invalid <- fixture$examples
  invalid[[1]]$model_type <- "global"
  expect_error(custom_author_examples(invalid, fixture$definition, fixture$data$metadata), "declared series mode")
  invalid <- fixture$examples
  invalid[[2]]$model_type <- "local"
  expect_error(custom_author_examples(invalid, fixture$definition, fixture$data$metadata), "declared series mode")
  invalid <- fixture$examples
  invalid[[2]]$name <- invalid[[1]]$name
  expect_error(custom_author_examples(invalid, fixture$definition, fixture$data$metadata), "unique names")
  invalid <- fixture$examples
  invalid[[1]]$edge_case <- FALSE
  expect_error(custom_author_examples(invalid, fixture$definition, fixture$data$metadata), "edge case")
})

test_that("horizon diagnostics preserve strict complete consecutive example dates", {
  fixture <- validation_fixture()
  fixture$definition$model_type <- "local"
  fixture$definition$requirements$forecast_horizon <- 3
  fixture$definition$version_id <- custom_model_definition_digest(fixture$definition)
  metadata <- list(date_type = "month", forecast_horizon = 3)
  example <- fixture$examples[[1]]
  example$new_data <- data.frame(Date = as.Date(c("2020-03-01", "2020-04-01", "2020-05-01")), Combo = "example-a")
  example$expected <- transform(example$new_data, .pred = 0)
  other <- example
  other$name <- "second"
  examples <- list(example, other)
  original <- examples
  expect_length(custom_author_examples(examples, fixture$definition, metadata), 2L)
  bad_dates <- list(
    missing = c("2020-03-01", "2020-04-01"), extra = c("2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01"),
    skipped_start = c("2020-04-01", "2020-05-01", "2020-06-01"), nonconsecutive = c("2020-03-01", "2020-05-01", "2020-06-01"),
    seasonal_jump = c("2021-03-01", "2021-04-01", "2021-05-01")
  )
  for (dates in bad_dates) {
    invalid <- examples
    invalid[[1]]$new_data <- data.frame(Date = as.Date(dates), Combo = "example-a")
    invalid[[1]]$expected <- transform(invalid[[1]]$new_data, .pred = 0)
    error <- tryCatch(custom_author_examples(invalid, fixture$definition, metadata), error = identity)
    expect_s3_class(error, "finnts_custom_model_authoring_error")
    expect_identical(error$stage, "examples")
    expect_identical(error$code, "example_horizon")
    expect_identical(conditionMessage(error), "Custom model examples: Worked example must cover the full declared horizon after its history.")
  }
  expect_identical(examples, original)
})

test_that("validation executes in the current R process without staging", {
  fixture <- validation_fixture()
  fixture$definition$fixed_parameters <- list(pid = as.numeric(Sys.getpid()))
  fixture$definition$source[["fit"]] <- paste0("function(data, context, parameters) { ", "stopifnot(Sys.getpid() == parameters$pid); mean(data$Target) }")
  fixture$definition$version_id <- custom_model_definition_digest(fixture$definition)
  local_mocked_bindings(
    r = function(...) stop("Unexpected validation child"), rcmd = function(...) stop("Unexpected package installation"),
    .package = "callr"
  )
  report <- custom_author_validate(fixture$definition, fixture$examples, fixture$data, 120)
  expect_true(report$passed)
  expect_length(report$holdouts, 3L)
  expect_identical(report$version_id, fixture$definition$version_id)
})

# Use the unchanged validator body with a private monotonic test clock. Resetting
# its bytecode avoids compiler-inlined base primitives; no global clock changes.
validation_timed_validator <- function() {
  validator <- custom_author_validate
  clock <- new.env(parent = environment(validator))
  clock$ticks <- 0
  clock$proc.time <- function() {
    clock$ticks <- clock$ticks + 1
    c(elapsed = clock$ticks)
  }
  environment(validator) <- clock
  body(validator) <- body(custom_author_validate)
  validator
}

test_that("validation checks its elapsed budget only after evaluation returns", {
  fixture <- validation_fixture()
  evaluated <- FALSE
  validator <- validation_timed_validator()
  local_mocked_bindings(custom_author_child = function(...) {
    evaluated <<- TRUE
    list(passed = TRUE, code = "passed")
  }, .package = "finnts")
  report <- validator(fixture$definition, fixture$examples, fixture$data, .Machine$double.eps)
  expect_true(evaluated)
  expect_identical(report[c("passed", "code")], list(passed = FALSE, code = "validation_timeout"))
  expect_identical(report$failure$phase, "validation_budget")
  expect_identical(report$failure$reason, "validation_timeout")
})

test_that("validation rejects wrong arithmetic and preserves ordinary session state", {
  fixture <- validation_fixture()
  report <- custom_author_validate(fixture$definition, fixture$examples, fixture$data, 60)
  expect_true(report$passed)
  expect_length(report$holdouts, 3L)
  wrong <- validation_fixture(TRUE)
  failed <- custom_author_validate(wrong$definition, wrong$examples, wrong$data, 60)
  expect_false(failed$passed)
  expect_identical(failed$code, "intent_mismatch")
  expect_false(custom_author_validate(fixture$definition, fixture$examples, fixture$data, 0.01)$passed)
  fixture$definition$source[["fit"]] <- paste0(
    "function(data, context, parameters) { ", "stopifnot(Sys.getenv('FINNTS_AUTHOR_SECRET_SENTINEL') == 'not-a-real-secret'); ",
    "options(finnts_child_mutation = TRUE); mean(data$Target) }"
  )
  fixture$definition$version_id <- finnts:::custom_model_definition_digest(fixture$definition)
  withr::local_envvar(c(FINNTS_AUTHOR_SECRET_SENTINEL = "not-a-real-secret"))
  withr::local_options(list(finnts_child_mutation = FALSE))
  expect_true(custom_author_validate(fixture$definition, fixture$examples, fixture$data, 60)$passed)
  expect_false(getOption("finnts_child_mutation"))
  expect_identical(Sys.getenv("FINNTS_AUTHOR_SECRET_SENTINEL"), "not-a-real-secret")
  fixture$definition$packages <- "stats"
  fixture$definition$version_id <- finnts:::custom_model_definition_digest(fixture$definition)
  testthat::local_mocked_bindings(custom_model_package_available = function(package) FALSE, .package = "finnts")
  expect_error(custom_author_validate(fixture$definition, fixture$examples, fixture$data, 60), "declared package is unavailable",
    class = "finnts_custom_model_authoring_error"
  )
})

# The synthetic evaluator exercises wrapper restoration on every exit path;
# numerical validation and same-PID execution are covered by the real fits above.
test_that("validation restores options RNG directory and libraries on every exit", {
  fixture <- validation_fixture()
  directory <- withr::local_tempdir()
  original_directory <- getwd()
  original_libraries <- .libPaths()
  original_options <- options(finnts_validation_existing = FALSE, finnts_validation_expression = quote(stop("Must not evaluate this option")))
  withr::defer(options(original_options))
  withr::local_seed(431)
  original_seed <- .Random.seed
  expression_option <- getOption("finnts_validation_expression")
  for (outcome in c("success", "error", "budget")) local({
    validator <- if (outcome == "budget")
      validation_timed_validator()
    else custom_author_validate
    local_mocked_bindings(custom_author_child = function(...) {
      options(finnts_validation_existing = TRUE, finnts_validation_added = "temporary")
      stats::runif(1)
      setwd(directory)
      .libPaths(c(directory, original_libraries))
      if (outcome == "error")
        stop("Synthetic evaluator error")
      list(passed = TRUE, code = "passed")
    }, .package = "finnts")
    if (outcome == "error") {
      expect_error(custom_author_validate(fixture$definition, fixture$examples, fixture$data, 60), "Synthetic evaluator error")
    } else {
      budget <- if (outcome == "budget")
        .Machine$double.eps
      else 60
      report <- validator(fixture$definition, fixture$examples, fixture$data, budget)
      expect_identical(report$passed, outcome == "success")
    }
    expect_identical(getOption("finnts_validation_existing"), FALSE)
    expect_null(getOption("finnts_validation_added"))
    expect_identical(getOption("finnts_validation_expression"), expression_option)
    expect_identical(.Random.seed, original_seed)
    expect_identical(getwd(), original_directory)
    expect_identical(.libPaths(), original_libraries)
  })
})
