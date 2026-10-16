# Independent numerical examples for a fixed mean rule; no candidate source is
# used to calculate the oracle. The zero-history example supplies the edge case.
draft_examples <- function() {
  first <- list(name = "mean", model_type = "local", history = data.frame(
    Date = as.Date(c("2020-01-01", "2020-02-01")),
    Combo = "example", Target = c(10, 20)
  ), new_data = data.frame(Date = as.Date("2020-03-01"), Combo = "example"), expected = data.frame(
    Date = as.Date("2020-03-01"),
    Combo = "example", .pred = 15
  ), tolerance = 1e-08, edge_case = FALSE, rationale = "(10 + 20) / 2 = 15",
    outcome = "predictions", expected_error = NULL)
  second <- first
  second$name <- "zero"
  second$history$Target <- c(0, 0)
  second$expected$.pred <- 0
  second$edge_case <- TRUE
  second$rationale <- "Mean of zeros is zero."
  list(first, second)
}

# Current native generated examples use literal arithmetic, never candidate
# output. The supplied scaffold owns their scenario and series identities.
draft_generated_examples <- function(scaffold) {
  lapply(scaffold$scenarios, function(scenario) list(
    id = scenario$id,
    rationale = "Ten and twenty average to fifteen; zero history averages to zero.",
    outcome = "predictions", error_phase = "none", error_code = "",
    series = lapply(scenario$combos, function(combo) list(
      combo = combo,
      history = list(Target = if (scenario$edge_case) c(0, 0) else c(10, 20)), future = list(),
      expected = rep(if (scenario$edge_case) 0 else 15, scaffold$forecast_horizon)
    ))
  ))
}

test_that("global implementation difficulty preserves cohort examples and source budgets", {
  for (approval in c("manual", "automatic")) for (limit in c(1L, 3L)) local({
    panel <- custom_global_case("pooled_mean", custom_global_panel(series = 2L, months = 8L, horizon = 1L))
    example <- list(name = "pooled", model_type = "global", history = panel$history,
      new_data = panel$future, expected = transform(panel$future, .pred = custom_global_reference("pooled_mean", panel$history, panel$future)),
      tolerance = 1e-08, edge_case = FALSE, rationale = "Independent pooled arithmetic.")
    edge <- example
    edge$name <- "zero_edge"
    edge$history$Target <- 0
    edge$expected$.pred <- 0
    edge$edge_case <- TRUE
    examples <- list(example, edge)
    before <- serialize(examples, NULL)
    requests <- list()
    reviews <- list()
    local_custom_console(function(request) {
      expect_identical(validations, 0L)
      reviews[[length(reviews) + 1L]] <<- request
      "yes"
    })

    sources <- validations <- 0L
    validate <- custom_author_validate
    metadata <- list(resolved_inputs = list(combo_variables = "Combo"), date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(
      Date = "Date",
      Combo = "character", Target = "numeric"
    ))
    local_mocked_bindings(
      check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
      custom_author_data = function(...) list(data = panel$history, metadata = metadata), custom_author_validate = function(...) {
        validations <<- validations + 1L
        validate(...)
      }, custom_author_request = function(session, stage, payload) {
        requests[[length(requests) + 1L]] <<- payload
        if (stage == "contract") {
          proposal <- list(
            questions = list(), interpretation = "Use one pooled historical mean.", parameters_json = "{}",
            parameter_schema = list(), history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer()),
            error_policy = list(), validation_properties = character(), prediction_cohort = list(scope = "all", group_columns = list())
          )
          if (approval == "automatic")
            proposal$defaults_used <- character()
          return(proposal)
        }
        sources <<- sources + 1L
        if (sources == 1L)
          return(list(
            status = "implementation_difficulty", conflict = "Need helper decomposition.", fit_body = "",
            predict_body = "", helpers = list()
          ))
        list(
          status = "candidate", conflict = "", fit_body = "mean(data$Target)", predict_body = "rep(object, nrow(new_data))",
          helpers = list()
        )
      }, .package = "finnts"
    )

    result <- tryCatch(create_custom_model("Use one pooled mean for the complete cohort.", list(),
      input_data = data.frame(),
      name = "global_retry", model_type = "global", validation_examples = custom_examples_table(examples), approval = approval, control = custom_model_control(
        max_attempts = limit
      )
    ), error = identity)

    expect_identical(serialize(examples, NULL), before)
    if (limit == 1L) {
      expect_s3_class(result, "finnts_custom_model_authoring_error")
      expect_identical(result$code, "implementation_difficulty")
      expect_match(conditionMessage(result), "Provider explanation: Need helper decomposition.", fixed = TRUE)
      expect_identical(sources, 1L)
      expect_identical(validations, 0L)
    } else {
      expect_s3_class(result, "finnts_custom_model")
      expect_identical(sources, 2L)
      expect_identical(validations, 1L)
      expect_identical(requests[[2L]]$contract, requests[[3L]]$contract)
      expect_identical(requests[[3L]]$failure$reason, "implementation_difficulty")
      expect_identical(requests[[3L]]$failure$provider_explanation, "Need helper decomposition.")
      contract <- requests[[3L]]$contract
      expect_identical(contract$requirements$runtime, list(
        prediction_scope = "complete_horizon", cohort = "all", group_columns = character()
      ))
      expect_true("finntsRowsDate" %in% names(result$definition$source))
      expect_false(any(c("date_helpers", "global_support") %in% names(contract)))
      if (approval == "manual") {
        expect_length(reviews, 1L)
        expect_identical(reviews[[1L]]$identity, result$definition$version_id)
      } else expect_length(reviews, 0L)
    }
  })
})

test_that("date repairs preserve caller examples, bounded attempts and fresh consent", {
  for (approval in c("manual", "automatic")) for (defect in c("static", "runtime")) local({
    examples <- draft_examples()
    examples[[1]]$expected$.pred <- 20
    examples[[1]]$rationale <- "The latest actual is 20."
    before <- serialize(examples, NULL)
    requests <- list()
    reviews <- list()
    local_custom_console(function(request) {
      reviews[[length(reviews) + 1L]] <<- request
      "yes"
    })

    sources <- 0L
    metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(
      Date = "Date",
      Combo = "character", Target = "numeric"
    ))
    local_mocked_bindings(
      check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
      custom_author_data = function(...) list(data = data.frame(
        Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
        Combo = "PRIVATE_DATE_SERIES", Target = 5
      ), metadata = metadata), custom_author_request = function(session,
                                                                stage, payload) {
        requests[[length(requests) + 1L]] <<- payload
        if (stage == "contract") {
          proposal <- list(
            questions = list(), interpretation = "Use the most recent actual.", parameters_json = "{}",
            parameter_schema = list(), history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer()),
            error_policy = list(), validation_properties = character()
          )
          if (approval == "automatic")
            proposal$defaults_used <- character()
          return(proposal)
        }
        sources <<- sources + 1L
        prediction <- if (sources == 1L && defect == "static")
          "vapply(new_data$Date, function(date) as.Date(date, origin='1970-01-01'), as.Date(NA))"
        else if (sources == 1L)
          "object$Target[finntsMatchDate(as.numeric(new_data$Date), object$Date)]"
        else "object$Target[finntsMatchDate(finntsShiftDate(new_data$Date, -1, context$date_type), object$Date, new_data$Combo, object$Combo)]"
        list(status = "candidate", conflict = "", fit_body = "data", predict_body = prediction, helpers = list())
      }, .package = "finnts"
    )

    first <- create_custom_model("Use the most recent actual.", list(),
      input_data = data.frame(), name = "date_repair",
      model_type = "local", validation_examples = custom_examples_table(examples), approval = approval, control = custom_model_control()
    )
    model <- first
    expect_s3_class(model, "finnts_custom_model")
    expect_identical(sources, 2L)
    expect_length(requests, 3L)
    expect_identical(requests[[3]]$failure$reason, if (defect == "static")
      "date_class_loss"
    else "date_key_type")
    expect_identical(requests[[2]]$contract$examples, requests[[3]]$contract$examples)
    expect_identical(serialize(examples, NULL), before)
    expect_true("finntsRowsDate" %in% names(model$definition$source))
    if (approval == "manual") {
      expect_length(reviews, if (defect == "runtime") 2L else 1L)
      expect_identical(tail(reviews, 1L)[[1L]]$identity, model$definition$version_id)
      if (defect == "runtime") {
        expect_false(identical(reviews[[1L]]$identity, reviews[[2L]]$identity))
        expect_identical(reviews[[1L]]$review$contract, reviews[[2L]]$review$contract)
      }
    } else expect_length(reviews, 0L)
    expect_false(grepl("PRIVATE_DATE_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
  })
})

test_that("missing-variable repairs preserve examples, consent and attempt limits", {
  for (approval in c("manual", "automatic")) for (limit in c(1L, 3L)) local({
    examples <- draft_examples()
    before <- serialize(examples, NULL)
    requests <- list()
    reviews <- list()
    local_custom_console(function(request) {
      reviews[[length(reviews) + 1L]] <<- request
      "yes"
    })

    sources <- 0L
    metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(
      Date = "Date",
      Combo = "character", Target = "numeric"
    ))
    local_mocked_bindings(
      check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
      custom_author_data = function(...) list(data = data.frame(
        Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
        Combo = "PRIVATE_BINDING_SERIES", Target = 5
      ), metadata = metadata), custom_author_request = function(session,
                                                                stage, payload) {
        requests[[length(requests) + 1L]] <<- payload
        if (stage == "contract") {
          proposal <- list(
            questions = list(), interpretation = "Use the historical mean.", parameters_json = "{}",
            parameter_schema = list(), history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer()),
            error_policy = list(), validation_properties = character()
          )
          if (approval == "automatic")
            proposal$defaults_used <- character()
          return(proposal)
        }
        sources <<- sources + 1L
        body <- if (sources == 1L)
          "with(object$history, rep(mean(history$Target), nrow(new_data)))"
        else "history <- object$history; rep(mean(history$Target), nrow(new_data))"
        list(status = "candidate", conflict = "", fit_body = "list(history = data)", predict_body = body, helpers = list())
      }, .package = "finnts"
    )

    result <- tryCatch(
      {
        first <- create_custom_model("Use the historical mean.", list(),
          input_data = data.frame(), name = "binding_repair",
          model_type = "local", validation_examples = custom_examples_table(examples), approval = approval, control = custom_model_control(
            max_attempts = limit
          )
        )
        first
      },
      error = identity
    )
    expect_identical(serialize(examples, NULL), before)
    if (limit == 1L) {
      expect_s3_class(result, "finnts_custom_model_authoring_error")
      expect_identical(result$code, "unbound_symbol")
      expect_match(conditionMessage(result), "Missing variable: history", fixed = TRUE)
      expect_identical(sources, 1L)
      expect_length(requests, 2L)
    } else {
      expect_s3_class(result, "finnts_custom_model")
      expect_identical(sources, 2L)
      expect_length(requests, 3L)
      expect_identical(requests[[3]]$failure$reason, "unbound_symbol")
      expect_identical(requests[[3]]$failure$unbound_symbol, "history")
      expect_null(requests[[3]]$failure$source_message)
      expect_identical(requests[[2]]$contract, requests[[3]]$contract)
      if (approval == "manual") {
        expect_length(reviews, 2L)
        expect_identical(reviews[[1L]]$review$contract, reviews[[2L]]$review$contract)
        expect_false(identical(reviews[[1L]]$identity, reviews[[2L]]$identity))
        expect_identical(reviews[[2L]]$identity, result$definition$version_id)
        expect_identical(reviews[[2L]]$review$repair$failure$unbound_symbol, "history")
        review <- capture.output(custom_author_review(reviews[[2L]]$review))
        expect_true(any(grepl("Missing variable: history", review, fixed = TRUE)))
      } else expect_length(reviews, 0L)
    }
    expect_false(grepl("PRIVATE_BINDING_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
  })
})

test_that("current authoring advertises only workflow inputs without changing input metadata", {
  for (drivers in list(character(), "Price", "Date_index.num")) local({
    data <- data.frame(
      Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8), Combo = "PRIVATE_SCHEMA", Target = 100,
      Price = 2, Date_index.num = seq_len(8), Target_lag3 = 90
    )
    metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = lapply(data, function(column) paste(class(column),
      collapse = "/"
    )), input_context = list(external_regressors = drivers))
    before <- serialize(list(data = data, metadata = metadata), NULL)
    requests <- list()
    local_mocked_bindings(
      check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
      custom_author_data = function(...) list(data = data, metadata = metadata), custom_author_request = function(session,
                                                                                                                  stage, payload) {
        requests[[length(requests) + 1L]] <<- payload
        fields <- custom_author_contract_fields(payload$protocol, payload$name, payload$model_type)
        response <- stats::setNames(rep(list(NULL), length(fields)), fields)
        response$questions <- list(list(kind = "business", topic = "business_context", question = "Which business multiplier applies?"))
        response
      }, .package = "finnts"
    )
    reviews <- list()
    local_custom_console(function(request) {
      reviews[[length(reviews) + 1L]] <<- request
      "no"
    })
    error <- tryCatch(create_custom_model("Apply the specified business multiplier.", list(),
      input_data = data.frame(), external_regressors = drivers,
      name = "schema_rule", model_type = "local", control = custom_model_control()
    ), error = identity)
    expected <- c("Target", "Date", "Combo", drivers)
    expect_setequal(names(requests[[1]]$metadata$schema), expected)
    expect_identical(requests[[1]]$metadata$schema, metadata$schema[names(requests[[1]]$metadata$schema)])
    expect_identical(error$stage, "clarify")
    expect_length(reviews, 1L)
    expect_identical(reviews[[1L]]$stage, "clarify")
    expect_identical(serialize(list(data = data, metadata = metadata), NULL), before)
    expect_false(grepl("PRIVATE_SCHEMA", jsonlite::toJSON(requests), fixed = TRUE))
  })
})

# Return native contract examples for a fixed 100/Price rule and zero-price error.
draft_typed_proposal <- function() {
  list(
    questions = character(), interpretation = "Divide 100 by Price; reject zero Price.", parameters_json = "{}", parameter_schema = list(),
    history_requirements = list(minimum_rows_per_series = 0L, required_lag_periods = integer()), error_policy = list(list(
      code = "zero_denominator",
      phase = "predict", description = "Reject zero Price."
    )), validation_properties = character(), examples = lapply(c(
      FALSE,
      TRUE
    ), function(edge) list(
      id = if (edge) "local_edge" else "local_basic", rationale = if (edge) "Zero price must error." else "100/2=50.",
      outcome = if (edge) "error" else "predictions", error_phase = if (edge) "predict" else "none", error_code = if (edge) "zero_denominator" else "",
      series = list(list(
        combo = "example_a", history = list(Target = 100L, Price = 1L), future = list(Price = if (edge) 0 else 2),
        expected = if (edge) list() else 50
      ))
    ))
  )
}

test_that("generated numerical warnings preserve source and exact manual approval", {
  for (approval in c("manual", "automatic")) local({
    metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"), input_context = list(external_regressors = "Price"))
    requests <- list()
    reviews <- list()
    local_custom_console(function(request) {
      reviews[[length(reviews) + 1L]] <<- request
      "yes"
    })

    sources <- 0L
    local_mocked_bindings(
      check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
      custom_author_data = function(...) list(data = data.frame(
        Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
        Combo = "PRIVATE_WARNING", Target = 100, Price = 2
      ), metadata = metadata), custom_author_request = function(session,
                                                                stage, payload) {
        requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
        if (stage == "source") {
          sources <<- sources + 1L
          if (sources > 1L)
            stop("Generated expected values must not request a source rewrite")
          return(list(
            status = "candidate", conflict = "", fit_body = "list()", predict_body = "if (any(new_data$Price == 0)) finntsRuleError('zero_denominator'); 100/new_data$Price",
            helpers = list()
          ))
        }
        proposal <- draft_typed_proposal()
        proposal$examples[[1]]$series[[1]]$expected <- 51
        if (approval == "automatic")
          proposal$defaults_used <- character()
        proposal
      }, .package = "finnts"
    )

    warnings <- character()
    output <- capture.output(model <- withCallingHandlers(
      {
        first <- create_custom_model("Divide 100 by Price; reject zero.", list(),
          input_data = data.frame(), external_regressors = "Price",
          name = "warning_ratio", model_type = "local", approval = approval, control = custom_model_control()
        )
        first
      },
      warning = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    ))
    expect_s3_class(model, "finnts_custom_model")
    expect_identical(sources, 1L)
    expect_length(requests, 2L)
    expect_length(warnings, 1L)
    expect_match(warnings[[1]], "1 LLM-generated", fixed = TRUE)
    expect_match(warnings[[1]], "validation_examples", fixed = TRUE)
    expect_true(any(grepl("1/2 matched", output, fixed = TRUE)))
    expect_identical(model$validation$checks$llm_example_warning, warnings[[1]])
    contract <- requests[[2L]]$payload$contract
    expect_equal(contract$examples[[1L]]$expected$.pred, 51)
    expect_identical(contract$expectation_provenance, "llm_proposed_unverified")
    if (approval == "manual") {
      expect_length(reviews, 1L)
      expect_identical(reviews[[1L]]$identity, model$definition$version_id)
      expect_identical(reviews[[1L]]$review$contract, contract)
    } else expect_length(reviews, 0L)
    expect_false(grepl("PRIVATE_WARNING", jsonlite::toJSON(requests), fixed = TRUE))
  })
})

test_that("caller numerical examples can request repair under the new policy", {
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list())
  sources <- 0L
  requests <- list()
  examples <- draft_examples()
  before <- serialize(examples, NULL)
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
      Combo = "private", Target = 100
    ), metadata = metadata), custom_author_request = function(session, stage, payload) {
      requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
      if (stage == "source") {
        sources <<- sources + 1L
        return(list(
          status = "candidate", conflict = "", fit_body = if (sources == 1L) "999" else "mean(data$Target)",
          predict_body = "rep(object, nrow(new_data))", helpers = list()
        ))
      }
      list(
        questions = list(), interpretation = "Use the historical mean.", parameters_json = "{}", parameter_schema = list(),
        error_policy = list(), validation_properties = character(), history_requirements = list(
          minimum_rows_per_series = 1L,
          required_lag_periods = integer()
        ), defaults_used = character()
      )
    }, .package = "finnts"
  )
  expect_no_warning(model <- create_custom_model("Use the historical mean.", list(),
    input_data = data.frame(), name = "caller_mean",
    model_type = "local", approval = "automatic", validation_examples = custom_examples_table(examples)
  ))
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(sources, 2L)
  expect_identical(requests[[1]]$payload$validation_examples, examples)
  expect_identical(requests[[3]]$payload$failure$reason, "intent_mismatch")
  expect_identical(requests[[2]]$payload$contract, requests[[3]]$payload$contract)
  expect_match(model$validation$checks$expectation_provenance, "caller_supplied", fixed = TRUE)
  expect_null(model$validation$checks$llm_example_warning)
  expect_identical(serialize(examples, NULL), before)
})

test_that("invalid proposed history can be corrected without replacing caller examples", {
  examples <- draft_examples()
  for (index in seq_along(examples)) {
    examples[[index]]$history <- data.frame(
      Date = seq(as.Date("2020-01-01"), by = "month", length.out = 4), Combo = "example",
      Target = if (index == 1L)
        c(10, 20, 30, 40)
      else 0
    )
    examples[[index]]$new_data <- data.frame(Date = seq(as.Date("2020-05-01"), by = "month", length.out = 3), Combo = "example")
    examples[[index]]$expected <- transform(examples[[index]]$new_data, .pred = if (index == 1L)
      25
    else 0)
    examples[[index]]$rationale <- if (index == 1L)
      "100 / 4 = 25."
    else "The mean of zeros is zero."
  }
  original <- serialize(examples, NULL)
  requests <- list()
  contracts <- sources <- 0L
  correct <- TRUE
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
      Combo = "PRIVATE_HISTORY", Target = 100
    ), metadata = list(
      date_type = "month", forecast_horizon = 3, available_packages = "base",
      schema = list()
    )), custom_author_request = function(session, stage, payload) {
      requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
      if (stage == "source") {
        sources <<- sources + 1L
        return(list(
          status = "candidate", conflict = "", fit_body = "mean(data$Target)", predict_body = "rep(object, nrow(new_data))",
          helpers = list()
        ))
      }
      contracts <<- contracts + 1L
      list(
        questions = list(), interpretation = "Fit the historical mean once and hold it constant.", parameters_json = "{}",
        parameter_schema = list(), error_policy = list(), validation_properties = character(), defaults_used = character(),
        history_requirements = list(minimum_rows_per_series = 4L, required_lag_periods = if (correct && contracts >
          1L) integer() else 1:3)
      )
    }, .package = "finnts"
  )
  model <- create_custom_model("Fit the historical mean once.", list(),
    input_data = data.frame(), name = "caller_history",
    model_type = "local", approval = "automatic", validation_examples = custom_examples_table(examples), control = custom_model_control(max_attempts = 2)
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(contracts, 2L)
  expect_identical(sources, 1L)
  expect_identical(requests[[1]]$payload$validation_examples, examples)
  expect_identical(requests[[2]]$payload$validation_examples, examples)
  expect_identical(requests[[2]]$payload$feedback$code, "insufficient_example_history")
  expect_false(requests[[2]]$payload$feedback$history$observed_history_possible)
  expect_identical(requests[[2]]$payload$feedback$history$unobserved_lag_periods, 1:2)
  expect_equal(vapply(requests[[2]]$payload$feedback$history$examples, `[[`, integer(1), "history_rows"), c(4L, 4L))
  expect_identical(serialize(examples, NULL), original)
  expect_false(grepl("PRIVATE_HISTORY", jsonlite::toJSON(requests), fixed = TRUE))
  correct <- FALSE
  contracts <- sources <- 0L
  error <- tryCatch(
    create_custom_model("Fit the historical mean once.", list(),
      input_data = data.frame(), name = "caller_history",
      model_type = "local", approval = "automatic", validation_examples = custom_examples_table(examples), control = custom_model_control(max_attempts = 2)
    ),
    error = identity
  )
  expect_identical(error$code, "insufficient_example_history")
  expect_identical(error$stage, "clarification")
  expect_identical(contracts, 2L)
  expect_identical(sources, 0L)
  expect_false(grepl("Correct those examples", conditionMessage(error), fixed = TRUE))
  expect_identical(serialize(examples, NULL), original)
  for (defect in c("horizon", "tolerance")) {
    invalid <- examples
    if (defect == "horizon") {
      invalid[[1]]$new_data$Date <- invalid[[1]]$new_data$Date + 1
      invalid[[1]]$expected$Date <- invalid[[1]]$new_data$Date
    } else invalid[[1]]$tolerance <- 1
    before <- serialize(invalid, NULL)
    contracts <- sources <- 0L
    error <- tryCatch(
      create_custom_model("Fit the historical mean once.", list(),
        input_data = data.frame(), name = "caller_history",
        model_type = "local", approval = "automatic", validation_examples = custom_examples_table(invalid), control = custom_model_control(max_attempts = 2)
      ),
      error = identity
    )
    expect_identical(error$stage, "examples")
    if (defect == "horizon") {
      expect_identical(error$code, "example_horizon")
      expect_identical(contracts, 1L)
    } else {
      expect_s3_class(error, "finnts_custom_model_authoring_error")
      expect_match(conditionMessage(error), "tolerance", fixed = TRUE)
      expect_identical(contracts, 0L)
    }
    expect_identical(sources, 0L)
    expect_identical(serialize(invalid, NULL), before)
  }
})

test_that("protocol corrections preserve explicit settings and property authority", {
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"), input_context = list(external_regressors = "Price"))
  requests <- list()
  contract_calls <- 0L
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
      Combo = "PRIVATE_PROTOCOL", Target = 100, Price = 2
    ), metadata = metadata), custom_author_request = function(session,
                                                              stage, payload) {
      requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
      if (stage == "source")
        return(list(
          status = "candidate", conflict = "", fit_body = "list()", predict_body = "if (any(new_data$Price == 0)) finntsRuleError('zero_denominator'); 100/new_data$Price",
          helpers = list()
        ))
      contract_calls <<- contract_calls + 1L
      proposal <- draft_typed_proposal()
      proposal$validation_properties <- "target_scale_equivariant"
      proposal$defaults_used <- character()
      if (contract_calls == 1L)
        proposal$questions <- list(list(kind = "protocol", topic = "history_layout", question = "Do I include intervening periods?"))
      if (contract_calls == 2L)
        proposal$defaults_used <- "external_regressors"
      proposal
    }, .package = "finnts"
  )

  model <- create_custom_model("Divide 100 by future Price; reject zero Price.", list(),
    input_data = data.frame(), external_regressors = "Price",
    name = "protocol_ratio", model_type = "local", approval = "automatic", control = custom_model_control()
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_false("schema_version" %in% names(model$definition))
  expect_length(requests, 4L)
  expect_identical(requests[[2]]$payload$feedback$code, "protocol_clarification")
  expect_match(requests[[2]]$payload$feedback$facts$history_layout, "consecutive", fixed = TRUE)
  expect_identical(requests[[3]]$payload$feedback$code, "invalid_defaults_used")
  expect_identical(requests[[4]]$payload$contract$required_properties, character())
  expect_match(model$validation$checks$property_diagnostics, "1 suggested-property", fixed = TRUE)
  expect_identical(requests[[4L]]$payload$contract$required_properties, character())
  expect_false(grepl("PRIVATE_PROTOCOL", jsonlite::toJSON(requests), fixed = TRUE))
})

test_that("protocol-six business questions and source conflicts stop without guessing", {
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"), input_context = list(external_regressors = "Price"))
  requests <- 0L
  conflict <- FALSE
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
      Combo = "private", Target = 100, Price = 2
    ), metadata = metadata), custom_author_validate = function(...) stop("Candidate must not execute"),
    custom_author_request = function(session, stage, payload) {
      requests <<- requests + 1L
      if (stage == "source")
        return(list(
          status = "contract_conflict", conflict = "The fixed case contradicts the rule.", fit_body = "",
          predict_body = "", helpers = list()
        ))
      proposal <- draft_typed_proposal()
      proposal$defaults_used <- character()
      if (!conflict)
        proposal$questions <- list(list(kind = "business", topic = "business_context", question = "Which fiscal calendar applies?"))
      proposal
    }, .package = "finnts"
  )
  question <- tryCatch(create_custom_model("Use the specified fiscal rule.", list(),
    input_data = data.frame(), external_regressors = "Price",
    name = "fiscal_rule", model_type = "local", approval = "automatic"
  ), error = identity)
  expect_identical(question$code, "business_clarification")
  expect_identical(requests, 1L)
  conflict <- TRUE
  error <- tryCatch(create_custom_model("Divide 100 by Price.", list(),
    input_data = data.frame(), external_regressors = "Price",
    name = "conflicting_rule", model_type = "local", approval = "automatic"
  ), error = identity)
  expect_identical(error$code, "contract_conflict")
  expect_identical(requests, 3L)
})

test_that("parameter feedback and malformed source repairs preserve fixed authority", {
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"), input_context = list(external_regressors = "Price"))
  requests <- list()
  contracts <- sources <- validations <- 0L
  validate <- custom_author_validate
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
      Combo = "PRIVATE_PARAMETER", Target = 100, Price = 2
    ), metadata = metadata), custom_author_validate = function(...) {
      validations <<- validations + 1L
      validate(...)
    }, custom_author_request = function(session, stage, payload) {
      requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
      if (stage == "source") {
        sources <<- sources + 1L
        result <- list(
          fit_body = "parameters$numerator", predict_body = "if (any(new_data$Price == 0)) finntsRuleError('zero_denominator'); object/new_data$Price",
          helpers = list()
        )
        return(if (sources == 1L) result else c(list(status = "candidate", conflict = ""), result))
      }
      contracts <<- contracts + 1L
      proposal <- draft_typed_proposal()
      proposal$defaults_used <- character()
      proposal$parameters_json <- "{\"numerator\":100}"
      proposal$parameter_schema <- list(list(name = "numerator", type = "number", units = if (contracts == 1L) "" else "currency"))
      proposal
    }, .package = "finnts"
  )
  model <- create_custom_model("Divide 100 currency by Price, rejecting zero.", list(),
    input_data = data.frame(), external_regressors = "Price",
    name = "parameter_ratio", model_type = "local", approval = "automatic", control = custom_model_control(required_properties = "constant_forecast")
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_length(requests, 4L)
  expect_identical(validations, 1L)
  expect_identical(requests[[2]]$payload$feedback$parameter_issue$parameter, "numerator")
  expect_identical(requests[[2]]$payload$feedback$parameter_issue$field, "units")
  expect_identical(requests[[3]]$payload$contract$required_properties, "constant_forecast")
  expect_identical(requests[[4]]$payload$contract, requests[[3]]$payload$contract)
  expect_identical(requests[[4]]$payload$failure$reason, "source_contract")
  expect_false(grepl("PRIVATE_PARAMETER", jsonlite::toJSON(requests), fixed = TRUE))
})

test_that("typed repair stops unchanged failures and requires exact candidate approval", {
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base",
    schema = list(Price = "numeric"), input_context = list(external_regressors = "Price"))
  requests <- 0L
  reviews <- list()
  source <- list(fit_body = "list()", predict_body = "if (any(new_data$Price == 0)) return(0); 100/new_data$Price",
    helpers = list(), status = "candidate", conflict = "")
  local_custom_console(function(request) {
    reviews[[length(reviews) + 1L]] <<- request
    "yes"
  })
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
      Combo = "PRIVATE_TYPED", Target = 100, Price = 2
    ), metadata = metadata), custom_author_request = function(session, stage, payload) {
      requests <<- requests + 1L
      if (stage == "contract") return(draft_typed_proposal())
      source
    }, .package = "finnts"
  )
  error <- tryCatch(create_custom_model("Divide 100 by Price; reject zero Price.", list(),
    input_data = data.frame(), external_regressors = "Price", name = "typed_ratio", model_type = "local"
  ), error = identity)
  expect_identical(error$code, "no_repair_progress")
  expect_identical(requests, 3L)
  expect_length(reviews, 2L)
  expect_identical(reviews[[1L]]$review$contract, reviews[[2L]]$review$contract)
  expect_identical(reviews[[1L]]$identity, reviews[[2L]]$identity)
  source$predict_body <- "if (any(new_data$Price == 0)) finntsRuleError('zero_denominator'); 100/new_data$Price"
  approved <- create_custom_model("Divide 100 by Price; reject zero Price.", list(),
    input_data = data.frame(), external_regressors = "Price", name = "typed_ratio", model_type = "local")
  expect_s3_class(approved, "finnts_custom_model")
  expect_identical(tail(reviews, 1L)[[1L]]$identity, approved$definition$version_id)
  expect_match(approved$validation$checks$expectation_provenance, "unverified", fixed = TRUE)
})

test_that("automatic typed validation stops preprocessing failures without source repairs", {
  requests <- validations <- 0L
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"), input_context = list(external_regressors = "Price"))
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
      Combo = "PRIVATE_TYPE", Target = 100, Price = 2
    ), metadata = metadata), custom_author_request = function(session,
                                                              stage, payload) {
      requests <<- requests + 1L
      if (stage == "source")
        return(list(
          status = "candidate", conflict = "", fit_body = "list()", predict_body = "if (any(new_data$Price == 0)) finntsRuleError('zero_denominator'); 100/new_data$Price",
          helpers = list()
        ))
      c(draft_typed_proposal(), list(defaults_used = character()))
    }, custom_author_validate = function(definition, ...) {
      validations <<- validations + 1L
      list(passed = FALSE, code = "candidate_validation_error", failure = custom_author_failure(
        definition, "example_predict",
        "input_type_mismatch", 1L, "local"
      ))
    }, .package = "finnts"
  )
  error <- tryCatch(create_custom_model("Divide 100 by Price; reject zero Price.", list(),
    input_data = data.frame(), external_regressors = "Price",
    name = "typed_ratio", model_type = "local", approval = "automatic"
  ), error = identity)
  expect_identical(error$stage, "environment")
  expect_identical(error$code, "input_type_mismatch")
  expect_identical(requests, 2L)
  expect_identical(validations, 1L)
  expect_match(conditionMessage(error), "No further source repair", fixed = TRUE)
})

# Mock transport and bounded validation while recording in-memory states, reviews,
# candidate identities and provider payloads. No draft is stored or resumed.
draft_harness <- function(question = FALSE, repair = FALSE, respond = NULL, .env = parent.frame()) {
  tracker <- new.env(parent = emptyenv())
  tracker$states <- list()
  tracker$reviews <- list()
  tracker$feedback <- list()
  tracker$review_validations <- integer()
  save <- custom_draft_save
  local_custom_console(function(request) {
    tracker$reviews[[length(tracker$reviews) + 1L]] <- request
    tracker$feedback[[length(tracker$feedback) + 1L]] <- custom_author_feedback(state = tracker$state)
    tracker$review_validations <- c(tracker$review_validations, tracker$validations)
    if (!is.null(respond)) return(respond(request))
    if (request$stage == "clarify") list(question_1 = "All history.") else "yes"
  }, .env = .env)
  tracker$requests <- 0L
  tracker$validations <- 0L
  tracker$preparations <- 0L
  tracker$payloads <- list()
  tracker$examples <- list()
  tracker$versions <- character()
  testthat::local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_draft_save = function(state) {
      state <- save(state)
      tracker$state <- state
      tracker$states[[length(tracker$states) + 1L]] <- state
      state
    }, custom_author_data = function(...) {
      tracker$preparations <- tracker$preparations + 1L
      list(data = data.frame(
        Date = seq(as.Date("2020-01-01"), by = "month", length.out = 4), Combo = "PRIVATE_DRAFT_SERIES",
        Target = c(12, 24, 12, 24)
      ), metadata = list(date_type = "month", forecast_horizon = 1))
    }, custom_author_request = function(session, stage, payload) {
      tracker$requests <- tracker$requests + 1L
      tracker$payloads[[tracker$requests]] <- payload
      if (stage == "contract")
        return(c(
          list(
            questions = if (question && tracker$requests == 1L) list(list(
              kind = "business", topic = "business_context",
              question = "Which window?"
            )) else list(), name = "draft_mean", interpretation = "Use the historical mean.",
            parameters_json = "{}", parameter_schema = list(), error_policy = list(), validation_properties = character(),
            history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer())
          ),
          if (!is.null(payload$protocol$scaffold)) list(examples = draft_generated_examples(payload$protocol$scaffold)), if (identical(
            payload$approval_mode,
            "automatic"
          )) list(defaults_used = c("history_window", "averaging_weights"))
        ))
      list(
        status = "candidate", conflict = "", fit_body = if (repair && tracker$validations == 0L) "999" else "mean(data$Target)",
        predict_body = "rep(object,nrow(new_data))", helpers = list()
      )
    }, custom_author_validate = function(definition, examples, data, timeout, package_policy, contract) {
      tracker$validations <- tracker$validations + 1L
      tracker$examples[[tracker$validations]] <- examples
      tracker$versions <- c(tracker$versions, definition$version_id)
      if (repair && tracker$validations == 1L) {
        if (identical(contract$expectation_provenance, "llm_proposed_unverified")) return(list(
          passed = FALSE,
          code = "candidate_validation_error", failure = custom_author_failure(definition, "example_predict", "prediction_shape", 1L, "local")
        ))
        return(list(passed = FALSE, code = "intent_mismatch"))
      }
      list(
        passed = TRUE, code = "passed", version_id = definition$version_id, examples_id = digest::digest(examples,
          algo = "sha256", serializeVersion = 2
        ), examples = lapply(examples, function(example) list(
          name = example$name,
          model_type = example$model_type, maximum_error = 0, tolerance = example$tolerance
        )), holdouts = list(list(
          model_type = "local",
          rows = 1L, training_rows = 3L, cutoff = "2020-03-01", mae = 1, wmape = 0.01
        )), serialization = TRUE, r_version = as.character(getRversion()),
        package_version = "synthetic", elapsed = 1, verification = list(
          provenance = contract$expectation_provenance,
          properties = list(row_order = as.integer(length(examples))), expected_errors = 0L, required_properties = character(),
          diagnostics = list()
        )
      )
    }, .package = "finnts", .env = .env
  )
  tracker
}

# Replies are synthetic test approvals, not a production autoapproval
# callback. Production callers must present the whole review to a human.
test_that("UX feedback is passive bounded and never changes approval or retry state", {
  tracker <- draft_harness(question = TRUE)
  model <- create_custom_model("Use mean.", list(), input_data = data.frame(),
    validation_examples = custom_examples_table(draft_examples()))
  feedback <- tracker$feedback[[1L]]
  expect_identical(feedback$status, "needs_input")
  expect_identical(feedback$next_action, "ask_user")
  expect_identical(unname(feedback$questions), list("Which window?"))
  expect_identical(feedback$attempts$remaining, list(contract = 2L, source = 3L))
  expect_false(grepl("PRIVATE_DRAFT_SERIES", jsonlite::toJSON(feedback), fixed = TRUE))
  expect_error(custom_run_envelope(feedback), "approved")
  review <- tracker$feedback[[2L]]
  expect_identical(review$status, "review_required")
  expect_true(review$needs_user_input)
  expect_identical(review$next_action, "review_candidate")
  expect_identical(review$version_id, model$definition$version_id)
  expect_identical(tracker$review_validations, c(0L, 0L))
  success <- custom_model_feedback(model)
  expect_identical(success$status, "approved")
  expect_true(success$validation$technical_passed)
  expect_false(success$validation$business_correctness_guaranteed)
  expect_null(success$attempts$limit_per_phase)
  expect_identical(success$version_id, model$definition$version_id)
  expect_identical(custom_model_feedback(unserialize(serialize(model, NULL))), success)
  expect_identical(names(model), c("definition", "validation", "approval"))
  failure <- tryCatch(create_custom_model("", list(), input_data = data.frame()), error = identity)
  expect_s3_class(failure, "finnts_custom_model_authoring_error")
  expect_identical(failure$feedback, custom_model_feedback(failure))
  expect_identical(failure$feedback$next_action, "correct_inputs")
  expect_identical(failure$feedback$stage, "input")
  expect_null(failure$feedback$approval_mode)
  withr::local_options(rlang_interactive = FALSE)
  interaction <- tryCatch(create_custom_model("Use mean.", list(), input_data = data.frame()), error = identity)
  expect_identical(custom_model_feedback(interaction)$stage, "interaction")
  generic <- custom_model_feedback(simpleError("PRIVATE_PROVIDER_SECRET"))
  expect_identical(generic$stage, "unknown")
  expect_null(generic$approval_mode)
  foreign <- simpleError("PRIVATE_PROVIDER_SECRET")
  foreign$feedback <- list(status = "approved", private = "PRIVATE_PROVIDER_SECRET")
  safe <- custom_model_feedback(foreign)
  expect_identical(safe$status, "failed")
  expect_false("schema_version" %in% names(safe))
  expect_false(grepl("PRIVATE_PROVIDER_SECRET", jsonlite::toJSON(safe), fixed = TRUE))
  expect_identical(foreign$feedback$status, "approved")
  changed <- failure
  changed$feedback$status <- "approved"
  expect_identical(custom_model_feedback(changed)$status, "failed")
  expect_identical(class(custom_author_error_feedback(foreign)), class(foreign))
  bounded <- custom_author_feedback(state = list(stage = "clarify", pending = list(questions = rep(strrep("x", 1000), 10))))
  expect_length(bounded$questions, 8L)
  expect_true(all(nchar(unlist(bounded$questions)) == 500L))
  expect_identical(bounded$questions_provenance, "provider_proposed")
  expect_error(custom_model_feedback(list()), "requires a custom model")
})

test_that("complete source functions normalize before review and preserve repair receipts", {
  validate <- custom_author_validate
  for (approval in c("manual", "automatic")) {
    reviews <- list()
    local_custom_console(function(request) {
      expect_identical(validations, as.integer(length(reviews)))
      reviews[[length(reviews) + 1L]] <<- request
      "yes"
    })
    requests <- list()
    sources <- validations <- 0L
    local_mocked_bindings(
      check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
      custom_author_data = function(...) list(data = data.frame(
        Date = seq(as.Date("2020-01-01"), by = "month", length.out = 6),
        Combo = "PRIVATE_FUNCTION_SERIES", Target = rep(c(2, 4), 3)
      ), metadata = list(
        date_type = "month", forecast_horizon = 1,
        available_packages = "base", input_context = list(external_regressors = character())
      )), custom_author_validate = function(...) {
        validations <<- validations + 1L
        validate(...)
      }, custom_author_request = function(session, stage, payload) {
        requests[[length(requests) + 1L]] <<- payload
        if (stage == "source") {
          sources <<- sources + 1L
          return(list(
            fit_body = if (sources == 1L) "function(data, wrong_context, parameters) list(value = 3)" else paste0(
              "function(data, context, parameters) { list(value = mean(data$Target)",
              if (sources == 2L) " + 1" else "", ") }"
            ), predict_body = "function(object, new_data, context) { return(rep(object$value, nrow(new_data))) }",
            helpers = list(), status = "candidate", conflict = ""
          ))
        }
        values <- lapply(payload$protocol$scaffold$scenarios, function(scenario) list(
          id = scenario$id, rationale = if (scenario$edge_case) "Zero observations average to zero." else "Two and four average to three.",
          series = list(list(combo = "example_a", history = list(Target = if (scenario$edge_case) c(0, 0) else c(
            2,
            4
          )), future = list(), expected = if (scenario$edge_case) 0 else 3)), outcome = "predictions", error_phase = "none",
          error_code = ""
        ))
        reply <- list(
          questions = character(), interpretation = "Use the historical mean.", parameters_json = "{}",
          history_requirements = list(minimum_rows_per_series = 2L, required_lag_periods = integer()), examples = values,
          parameter_schema = list(), error_policy = list(), validation_properties = character()
        )
        if (is.null(payload$protocol$scaffold)) reply$examples <- NULL
        if (identical(payload$approval_mode, "automatic"))
          reply$defaults_used <- character()
        reply
      }, .package = "finnts"
    )
    first <- create_custom_model("Use the historical mean.", list(),
      input_data = data.frame(), name = "normalized_mean",
      model_type = "local", approval = approval, validation_examples = custom_examples_table(draft_examples()), control = custom_model_control()
    )
    model <- first
    if (approval == "manual") {
      expect_length(reviews, 2L)
      expect_false(identical(reviews[[1L]]$identity, reviews[[2L]]$identity))
      expect_identical(reviews[[1L]]$review$contract, reviews[[2L]]$review$contract)
      expect_identical(reviews[[2L]]$identity, model$definition$version_id)
    } else expect_length(reviews, 0L)
    expect_identical(validations, 2L)
    expect_identical(sources, 3L)
    expect_length(requests, 4)
    expect_identical(requests[[3]]$failure$reason, "source_body_format")
    expect_identical(requests[[4]]$failure$reason, "intent_mismatch")
    expect_match(requests[[4]]$previous_source[["fit"]], "mean(data$Target) + 1", fixed = TRUE)
    expect_identical(requests[[2]]$contract, requests[[4]]$contract)
    expect_s3_class(model, "finnts_custom_model")
    expect_identical(model$approval$intent_confirmed, approval == "manual")
    expect_false(grepl("PRIVATE_FUNCTION_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
  }
})

test_that("history and binding failures repair before exact candidate authorization", {
  validate <- custom_author_validate
  for (approval in c("manual", "automatic")) {
    reviews <- list()
    local_custom_console(function(request) {
      reviews[[length(reviews) + 1L]] <<- request
      "yes"
    })
    requests <- list()
    contracts <- sources <- validations <- 0L
    local_mocked_bindings(
      check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
      custom_author_data = function(...) list(data = data.frame(
        Date = seq(as.Date("2017-01-01"), by = "month", length.out = 72),
        Combo = "PRIVATE_HISTORY_SERIES", Target = 100
      ), metadata = list(
        date_type = "month", forecast_horizon = 3,
        available_packages = "base", input_context = list(external_regressors = character())
      )), custom_author_validate = function(...) {
        validations <<- validations + 1L
        validate(...)
      }, custom_author_request = function(session, stage, payload) {
        requests[[length(requests) + 1L]] <<- payload
        if (stage == "source") {
          sources <<- sources + 1L
          return(list(
            fit_body = if (sources == 1L) "object$history <- data; object" else "list(history = data)",
            predict_body = paste0(
              "vapply(seq_len(nrow(new_data)), function(row) { ", "dates <- seq.Date(new_data$Date[row], by = '-12 months', length.out = 5)[2:5]; ",
              "values <- object$history$Target[match(dates, object$history$Date)]; ", "values[1L] + mean(values[1:3] - values[2:4]) }, numeric(1))"
            ),
            helpers = list(), status = "candidate", conflict = ""
          ))
        }
        contracts <<- contracts + 1L
        values <- lapply(payload$protocol$scaffold$scenarios, function(scenario) list(
          id = scenario$id, rationale = "Constant history has zero annual differences.",
          series = list(list(combo = "example_a", history = list(Target = rep(
            if (scenario$edge_case) 0 else 100,
            if (contracts == 1L) 7 else 48
          )), future = list(), expected = rep(
            if (scenario$edge_case) 0 else 100,
            3
          ))), outcome = "predictions", error_phase = "none", error_code = ""
        ))
        reply <- list(
          questions = character(), interpretation = "Prior-year value plus three absolute annual differences averaged equally.",
          parameters_json = "{}", history_requirements = list(minimum_rows_per_series = 48L, required_lag_periods = c(
            12L,
            24L, 36L, 48L
          )), examples = values, parameter_schema = list(), error_policy = list(), validation_properties = character()
        )
        if (identical(payload$approval_mode, "automatic"))
          reply$defaults_used <- character()
        reply
      }, .package = "finnts"
    )
    result <- create_custom_model("Use the explicit seasonal differences rule.", list(),
      input_data = data.frame(), name = "history_rule",
      model_type = "local", approval = approval, control = custom_model_control()
    )
    expect_identical(sort(names(requests[[1]]$protocol)), c("missing_data", "package_policy", "predictor_types", "predictors", "scaffold"))
    expect_identical(requests[[2]]$feedback$code, "insufficient_example_history")
    expect_identical(requests[[2]]$feedback$field, "examples.history")
    expect_identical(requests[[4]]$failure$reason, "source_binding")
    expect_identical(requests[[4]]$failure$bindings$references[[1]], list(function_name = "fit", symbol = "object"))
    expect_identical(requests[[3]]$contract, requests[[4]]$contract)
    expect_identical(contracts, 2L)
    expect_identical(sources, 2L)
    model <- result
    if (approval == "manual") {
      expect_length(reviews, 1L)
      expect_identical(reviews[[1L]]$identity, model$definition$version_id)
      expect_identical(reviews[[1L]]$review$contract$history_requirements$minimum_rows_per_series, 48L)
    } else expect_length(reviews, 0L)
    expect_s3_class(model, "finnts_custom_model")
    expect_identical(validations, 1L)
    expect_identical(model$approval$intent_confirmed, approval == "manual")
    expect_false(grepl("PRIVATE_HISTORY_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
  }
})

test_that("typed authoring retries native examples and validates both approval policies", {
  for (approval in c("manual", "automatic")) {
    reviews <- list()
    local_custom_console(function(request) {
      reviews[[length(reviews) + 1L]] <<- request
      "yes"
    })
    requests <- list()
    contracts <- 0L
    local_mocked_bindings(
      check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
      custom_author_data = function(...) list(data = data.frame(
        Date = seq(as.Date("2021-01-01"), by = "month", length.out = 6),
        Combo = "PRIVATE_TYPED_SERIES", Target = rep(c(2, 4), 3)
      ), metadata = list(
        date_type = "month", forecast_horizon = 1,
        available_packages = "base", input_context = list(external_regressors = character())
      )), custom_author_request = function(session,
                                           stage, payload) {
        requests[[length(requests) + 1L]] <<- payload
        if (stage == "source")
          return(list(
            fit_body = "mean(data$Target)", predict_body = "rep(object, nrow(new_data))", helpers = list(),
            status = "candidate", conflict = ""
          ))
        contracts <<- contracts + 1L
        values <- lapply(payload$protocol$scaffold$scenarios, function(scenario) list(
          id = scenario$id, rationale = if (scenario$edge_case) "The mean of zeros is zero." else "The mean of two and four is three.",
          series = list(list(combo = "example_a", history = list(Target = if (scenario$edge_case) c(0, 0) else c(
            2,
            4
          )), future = list(), expected = if (scenario$edge_case) 0 else 3)), outcome = "predictions", error_phase = "none",
          error_code = ""
        ))
        if (contracts == 1L)
          values[[1]]$series <- values[[1]]$series[[1]]
        result <- list(
          questions = character(), interpretation = "Use the historical mean.", parameters_json = "{}",
          examples = values, parameter_schema = list(), error_policy = list(), validation_properties = character(),
          history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer())
        )
        if (identical(payload$approval_mode, "automatic"))
          result$defaults_used <- character()
        result
      }, .package = "finnts"
    )
    draft <- create_custom_model("Use mean.", list(),
      input_data = data.frame(), name = "typed_mean", model_type = "local",
      approval = approval, control = custom_model_control()
    )
    expect_identical(sort(names(requests[[1]]$protocol)), c("missing_data", "package_policy", "predictor_types", "predictors", "scaffold"))
    expect_identical(requests[[2]]$feedback$field, "examples.series")
    expect_identical(requests[[2]]$feedback$code, "invalid_examples_shape")
    expect_length(requests, 3)
    model <- draft
    if (approval == "manual") {
      expect_length(reviews, 1L)
      expect_identical(reviews[[1L]]$review$contract$requirements$missing_data, "error")
      expect_identical(reviews[[1L]]$identity, model$definition$version_id)
    } else expect_length(reviews, 0L)
    expect_s3_class(model, "finnts_custom_model")
    expect_identical(model$approval$intent_confirmed, approval == "manual")
    expect_length(requests, 3)
    expect_false(grepl("PRIVATE_TYPED_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
  }
})

test_that("rejected contract snapshots are whitelisted bounded and tamper checked", {
  protocol <- custom_author_protocol(
    list(date_type = "month", forecast_horizon = 1), "local", character(), "base_only",
    TRUE
  )
  state <- list(stage = "contract", attempts = list(contract = 1L, source = 0L), settings = list(
    name = "typed_mean", model_type = "local",
    approval_mode = "automatic", authoring_protocol = protocol
  ))
  proposal <- list(
    questions = character(), interpretation = "Use mean.", parameters_json = "{", defaults_used = character(),
    private_extra = "PRIVATE_SENTINEL", parameter_schema = list(), error_policy = list(), validation_properties = character(),
    history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer())
  )
  failure <- custom_author_failure(NULL, "contract", "invalid_parameters_json")
  recorded <- custom_draft_record(state, "rejected", failure, proposal, field = "parameters_json")
  snapshot <- recorded$diagnostics$rejected_contract
  expect_true(snapshot$omitted)
  expect_false("private_extra" %in% names(snapshot$proposal))
  expect_false(grepl("PRIVATE_SENTINEL", jsonlite::toJSON(snapshot), fixed = TRUE))
  corrupted <- snapshot
  corrupted$proposal$parameters_json <- "{}"
  expect_error(validate_custom_draft_contract_rejection(corrupted), "identity")
  corrupted <- snapshot
  corrupted["field"] <- list(NULL)
  expect_error(validate_custom_draft_contract_rejection(corrupted), "contract")
  proposal$parameters_json <- paste(rep("x", 1048577), collapse = "")
  oversized <- custom_draft_record(state, "rejected", failure, proposal, field = "parameters_json")
  expect_null(oversized$diagnostics$rejected_contract$proposal)
  expect_true(oversized$diagnostics$rejected_contract$omitted)
  expect_match(oversized$diagnostics$rejected_contract$proposal_id, "^[a-f0-9]{64}$")
  expect_lte(custom_draft_diagnostic_size(oversized$diagnostics), 1048576)
})

test_that("manual contract correction retains feedback before review and freezes repaired examples", {
  tracker <- draft_harness(repair = TRUE)
  request <- custom_author_request

  saved <- NULL
  local_mocked_bindings(custom_author_request = function(session, stage, payload) {
    proposal <- request(session, stage, payload)
    if (stage == "contract") {
      examples <- proposal$examples
      if (tracker$requests == 1L) {
        examples[[1]]$series[[1]]$expected <- c(15, 15)
      } else {
        saved <<- tracker$state
      }
      proposal$examples <- examples
    }
    proposal
  }, .package = "finnts")
  model <- create_custom_model("Use mean.", list(), input_data = data.frame(), approval = "manual", control = custom_model_control())
  expect_identical(saved$diagnostic, "invalid_examples_shape")
  expect_identical(saved$attempts, list(contract = 2L, source = 0L))
  expect_identical(saved$consent, list(intent = NULL, test = NULL))
  expect_identical(unserialize(serialize(saved, NULL)), saved)
  expect_identical(tracker$payloads[[2]]$feedback$code, "invalid_examples_shape")
  expect_identical(tracker$payloads[[2]]$feedback$field, "examples.expected")
  expect_identical(tracker$review_validations, c(0L, 1L))
  expect_length(tracker$reviews, 2L)
  expect_identical(tracker$reviews[[1L]]$review$contract, tracker$reviews[[2L]]$review$contract)
  expect_false(identical(tracker$reviews[[1L]]$identity, tracker$reviews[[2L]]$identity))
  expect_identical(tracker$reviews[[2L]]$identity, model$definition$version_id)
  expect_null(tracker$payloads[[4L]]$feedback)
  expect_identical(tracker$payloads[[4L]]$diagnostic, "candidate_validation_error")
  expect_s3_class(model, "finnts_custom_model")
  expect_true(model$approval$intent_confirmed)
  expect_identical(tracker$examples[[1]], tracker$examples[[2]])
})

test_that("manual authoring preserves mode feedback without broadening the reviewed contract", {
  tracker <- draft_harness()
  request <- custom_author_request

  saved <- NULL
  local_mocked_bindings(custom_author_request = function(session, stage, payload) {
    proposal <- request(session, stage, payload)
    if (stage == "contract") {
      examples <- proposal$examples
      if (tracker$requests == 1L) {
        extra <- examples[[1]]
        extra$id <- "global_basic"
        examples <- list(examples[[1]], extra, examples[[2]])
      } else {
        saved <<- tracker$state
      }
      proposal$examples <- examples
    }
    proposal
  }, .package = "finnts")
  model <- create_custom_model("Use mean.", list(), input_data = data.frame(), approval = "manual", control = custom_model_control())
  expect_identical(saved$diagnostic, "invalid_examples_shape")
  expect_identical(saved$attempts, list(contract = 2L, source = 0L))
  expect_identical(saved$consent, list(intent = NULL, test = NULL))
  expect_identical(unserialize(serialize(saved, NULL)), saved)
  expect_identical(tracker$payloads[[2]]$feedback$code, "invalid_examples_shape")
  expect_identical(tracker$payloads[[2]]$feedback$field, "examples.id")
  expect_identical(tracker$review_validations, 0L)
  expect_length(tracker$reviews, 1L)
  contract <- tracker$reviews[[1L]]$review$contract
  expect_length(contract$examples, 2L)
  expect_true(all(vapply(contract$examples, function(example) identical(example$model_type, "local"), logical(1))))
  expect_true(model$approval$intent_confirmed)
  expect_identical(model$definition$model_type, "local")
  expect_identical(tracker$examples[[1]], contract$examples)
})

test_that("required diagnostic history rejects missing and corrupt evidence", {
  tracker <- draft_harness(repair = TRUE)
  model <- create_custom_model("Use mean.", list(), input_data = data.frame(),
    validation_examples = custom_examples_table(draft_examples()))
  expect_error(validate_custom_draft_diagnostics(NULL), "diagnostic")
  failed <- Filter(function(state) identical(state$stage, "source") &&
    identical(state$diagnostic, "intent_mismatch"), tracker$states)[[1L]]
  failure_index <- length(failed$diagnostics$history)
  expect_identical(failed$diagnostics$history[[failure_index]]$failure$reason, "intent_mismatch")
  expect_identical(failed$consent, list(intent = NULL, test = NULL))
  expect_identical(unserialize(serialize(failed, NULL)), failed)
  expect_identical(tracker$reviews[[1L]]$review$contract, tracker$reviews[[2L]]$review$contract)
  expect_identical(tracker$payloads[[3L]]$failure$reason, "intent_mismatch")
  expect_identical(tracker$reviews[[2L]]$review$repair$changed_functions, "fit")
  output <- capture.output(custom_author_review(tracker$reviews[[2L]]$review))
  expect_true(any(grepl("Source changes: fit", output, fixed = TRUE)))
  expect_identical(tracker$reviews[[2L]]$identity, model$definition$version_id)
  for (defect in c("version", "message", "history", "source")) {
    changed <- failed$diagnostics
    if (defect == "version") changed$history[[failure_index]]$failure$version_id <- strrep("a", 64)
    if (defect == "message") changed$history[[failure_index]]$failure$message <- "Forged text"
    if (defect == "history") changed$history <- rep(changed$history, 13L)
    if (defect == "source") changed$snapshot <- list(
      contract_id = NULL, version_id = NULL, source_id = NULL, contract = NULL, source = "Changed")
    expect_error(validate_custom_draft_diagnostics(changed), class = "finnts_custom_model_authoring_error")
  }
})

test_that("diagnostic capture bounds history and omits oversized snapshots without changing contracts", {
  tracker <- draft_harness()
  create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = custom_examples_table(draft_examples()))
  draft <- Filter(function(state) identical(state$stage, "review_create"), tracker$states)[[1L]]
  state <- draft
  state$stage <- "source"
  state$contract$interpretation <- strrep("Long confidential rule. ", 60000L)
  state$contract_id <- digest::digest(state$contract, algo = "sha256", serializeVersion = 2)
  before <- state$contract
  for (index in 1:15) state <- custom_draft_record(state, "accepted")
  expect_length(state$diagnostics$history, 12L)
  expect_true(state$diagnostics$omitted)
  expect_null(state$diagnostics$snapshot$contract)
  expect_identical(state$diagnostics$snapshot$contract_id, state$contract_id)
  expect_identical(state$contract, before)
  expect_lte(custom_draft_diagnostic_size(state$diagnostics), 1048576L)
  state <- draft
  state$stage <- "source"
  proposal <- list(source = list(list(name = "fit", code = "function(...) NULL"), list(name = "fit", code = "function(...) NULL")))
  captured <- custom_draft_record(state, "rejected", proposal = proposal)
  expect_true(captured$diagnostics$omitted)
  expect_null(captured$diagnostics$snapshot$source)
  expect_identical(tracker$validations, 1L)
})

test_that("authoring preserves language-valued options without evaluating them", {
  previous <- options(finnts_draft_option = quote(stop("Language-valued option was executed")))
  withr::defer(options(previous))
  tracker <- draft_harness()
  before <- getOption("finnts_draft_option")
  model <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = custom_examples_table(draft_examples()))
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(getOption("finnts_draft_option"), before)
})

test_that("edit text cannot widen existing model capabilities", {
  tracker <- draft_harness(respond = function(request) list(action = "edit", instructions = "Pool the series."))
  contract <- custom_author_contract
  local_mocked_bindings(custom_author_contract = function(...) {
    result <- contract(...)
    if (tracker$requests > 2L) result$model_type <- "global"
    result
  }, .package = "finnts")
  expect_error(create_custom_model("Use mean.", list(), input_data = data.frame(),
    validation_examples = custom_examples_table(draft_examples())),
    "Edits cannot change model modes or predictors")
  expect_identical(tracker$validations, 0L)
  expect_identical(tracker$requests, 3L)
})

test_that("automatic repairs preserve fixed examples and exact authorization policy", {
  tracker <- draft_harness(repair = TRUE)
  local_mocked_bindings(custom_author_interact = function(...) stop("Unexpected human interaction"), .package = "finnts")

  examples <- draft_examples()
  examples[[1]]$tolerance <- 0
  output <- capture.output(model <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = custom_examples_table(examples),
    approval = "automatic", control = custom_model_control()
  ))
  state <- tracker$state
  expect_identical(state$status, "approved")
  expect_null(state$pending)
  expect_length(state$answers, 0L)
  expect_identical(state$attempts, list(contract = 1L, source = 2L))
  expect_identical(tracker$examples[[1]], tracker$examples[[2]])
  expect_identical(tracker$examples[[1]][[1]]$tolerance, 0)
  expect_length(unique(tracker$versions), 2L)
  expect_length(state$receipts, 2L)
  expect_true(all(vapply(state$receipts, function(receipt) receipt$stage == "automatic_authorization", logical(1))))
  expect_identical(unname(vapply(state$receipts, function(receipt) receipt$identity, character(1))), tracker$versions)
  expect_true(any(grepl("automatic; no manual review", output, fixed = TRUE)))
  expect_false(any(grepl("Follows the reviewed rule|Create model|PRIVATE_DRAFT_SERIES", output)))
  expect_identical(tracker$validations, 2L)
  expect_identical(tracker$requests, 3L)
  expect_true(any(grepl("not manually reviewed", capture.output(print(model)), fixed = TRUE)))
})

test_that("the current automatic catalog records only resolved defaults", {
  tracker <- draft_harness()
  model <- create_custom_model("Use the historical mean.", list(), input_data = data.frame(),
    validation_examples = custom_examples_table(draft_examples()), approval = "automatic")
  state <- tracker$state
  expect_identical(state$settings$automatic_policy$catalog, custom_author_defaults())
  expect_false("catalog_version" %in% names(state$settings$automatic_policy))
  expect_false("catalog_version" %in% names(state$contract$automatic_defaults))
  expect_null(state$contract$automatic_defaults$defaults_used$growth_basis)
  evidence <- jsonlite::fromJSON(model$validation$checks$automatic_defaults, simplifyVector = FALSE)
  expect_null(evidence$catalog_version)
  expect_null(evidence$defaults_used$growth_basis)
  expect_identical(tracker$requests, 2L)
  expect_identical(tracker$validations, 1L)
  expect_identical(state$definition$source, model$definition$source)
  expect_identical(state$definition$version_id, model$definition$version_id)
})

test_that("automatic cannot ask questions or exceed budgets and manual retains approval policy", {
  tracker <- draft_harness(question = TRUE)
  expect_error(create_custom_model("Use weighted mean.", list(),
    input_data = data.frame(), validation_examples = custom_examples_table(draft_examples()),
    approval = "automatic"
  ), "business context", class = "finnts_custom_model_authoring_error")
  expect_identical(tracker$requests, 1L)
  expect_identical(tracker$validations, 0L)
  tracker <- draft_harness(repair = TRUE)

  expect_error(create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = custom_examples_table(draft_examples()),
    approval = "automatic", control = custom_model_control(max_attempts = 1L)
  ), "max_attempts")
  expect_identical(tracker$validations, 1L)
  expect_identical(tracker$requests, 2L)
  expect_null(tracker$state$result)
  tracker <- draft_harness()
  approved <- create_custom_model("Use mean.", list(), input_data = data.frame(),
    validation_examples = custom_examples_table(draft_examples()))
  expect_identical(names(approved$approval), c("version_id", "intent_confirmed", "allow_code"))
  expect_true(approved$approval$intent_confirmed)
})

test_that("authoring environment fails explicitly when package metadata is unavailable", {
  original <- base::system.file
  local_mocked_bindings(system.file = function(..., package = "base", lib.loc = NULL, mustWork = FALSE) {
    if (package == "stats")
      return("")
    original(..., package = package, lib.loc = lib.loc, mustWork = mustWork)
  }, .package = "base")
  expect_error(custom_draft_environment(list(available_packages = "stats")), "metadata is unavailable for stats", class = "finnts_custom_model_authoring_error")
})
