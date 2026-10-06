# Independent numerical examples for a fixed mean rule; no candidate source is
# used to calculate the oracle. The zero-history example supplies the edge case.
draft_examples <- function() {
  first <- list(name = "mean", model_type = "local", history = data.frame(
    Date = as.Date(c("2020-01-01", "2020-02-01")),
    Combo = "example", Target = c(10, 20)
  ), new_data = data.frame(Date = as.Date("2020-03-01"), Combo = "example"), expected = data.frame(
    Date = as.Date("2020-03-01"),
    Combo = "example", .pred = 15
  ), tolerance = 1e-08, edge_case = FALSE, rationale = "(10 + 20) / 2 = 15")
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
    example <- custom_model_example(panel$history, panel$future, expected = custom_global_reference(
      "pooled_mean", panel$history,
      panel$future
    ), model_type = "global")
    edge <- example
    edge$name <- "zero_edge"
    edge$history$Target <- 0
    edge$expected$.pred <- 0
    edge$edge_case <- TRUE
    examples <- list(example, edge)
    before <- serialize(examples, NULL)
    requests <- list()
    sources <- validations <- 0L
    validate <- custom_author_validate
    metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(
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
    root <- withr::local_tempdir()
    result <- tryCatch(create_custom_model("Use one pooled mean for the complete cohort.", list(),
      input_data = data.frame(),
      name = "global_retry", model_type = "global", validation_examples = examples, approval = approval, control = custom_model_control(
        max_attempts = limit,
        draft_path = root
      )
    ), error = identity)
    if (approval == "manual" && !inherits(result, "error")) {
      expect_identical(validations, 0L)
      result <- tryCatch(create_custom_model(draft = result, llm = list(), responses = list(
        request_id = result$pending$request_id,
        answer = "yes"
      )), error = identity)
    }
    expect_identical(serialize(examples, NULL), before)
    if (limit == 1L) {
      expect_s3_class(result, "finnts_custom_model_authoring_error")
      expect_identical(result$code, "implementation_difficulty")
      expect_identical(sources, 1L)
      expect_identical(validations, 0L)
    } else {
      expect_s3_class(result, "finnts_custom_model")
      expect_identical(sources, 2L)
      expect_identical(validations, 1L)
      expect_identical(requests[[2L]]$contract, requests[[3L]]$contract)
      expect_identical(requests[[3L]]$failure$reason, "implementation_difficulty")
      state <- custom_draft_load(file.path(list.dirs(root, recursive = FALSE), "current.rds"))$state
      expect_identical(state$contract$requirements$runtime, list(
        version = 2L, prediction_scope = "complete_horizon",
        cohort = "all", group_columns = character()
      ))
      expect_true("finntsRowsDate" %in% names(state$definition$source))
      expect_false(any(c("date_helpers", "global_support") %in% names(state$contract)))
      expect_identical(create_custom_model(draft = state), result)
      changed <- state
      changed$settings$authoring_protocol$global_support <- "obsolete"
      changed$state_digest <- custom_draft_digest(changed)
      expect_error(validate_custom_model_draft(changed), "protocol")
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
    root <- withr::local_tempdir()
    first <- create_custom_model("Use the most recent actual.", list(),
      input_data = data.frame(), name = "date_repair",
      model_type = "local", validation_examples = examples, approval = approval, control = custom_model_control(draft_path = root)
    )
    if (approval == "manual") {
      next_result <- create_custom_model(draft = first, llm = list(), responses = list(
        request_id = first$pending$request_id,
        answer = "yes"
      ))
      if (defect == "runtime") {
        expect_identical(next_result$stage, "review_create")
        expect_null(next_result$consent$test)
        expect_false(identical(first$definition$version_id, next_result$definition$version_id))
        expect_identical(first$contract_id, next_result$contract_id)
        next_result <- create_custom_model(draft = next_result, responses = list(
          request_id = next_result$pending$request_id,
          answer = "yes"
        ))
      }
      model <- next_result
    } else model <- first
    expect_s3_class(model, "finnts_custom_model")
    expect_identical(sources, 2L)
    expect_length(requests, 3L)
    expect_identical(requests[[3]]$failure$reason, if (defect == "static")
      "date_class_loss"
    else "date_key_type")
    expect_identical(requests[[2]]$contract$examples, requests[[3]]$contract$examples)
    expect_identical(serialize(examples, NULL), before)
    state <- custom_draft_load(file.path(list.dirs(root, recursive = FALSE), "current.rds"))$state
    expect_true("finntsRowsDate" %in% names(state$definition$source))
    expect_identical(state$diagnostics$snapshot$source, model$definition$source)
    changed <- state
    changed$settings$authoring_protocol$date_helpers <- "obsolete"
    changed$state_digest <- custom_draft_digest(changed)
    expect_error(validate_custom_model_draft(changed), "protocol")
    expect_false(grepl("PRIVATE_DATE_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
    local_mocked_bindings(custom_author_request = function(...) stop("Unexpected reauthoring"), .package = "finnts")
    expect_identical(create_custom_model(draft = state), model)
  })
})

test_that("missing-variable repairs preserve examples, consent and attempt limits", {
  for (approval in c("manual", "automatic")) for (limit in c(1L, 3L)) local({
    examples <- draft_examples()
    before <- serialize(examples, NULL)
    requests <- list()
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
    root <- withr::local_tempdir()
    result <- tryCatch(
      {
        first <- create_custom_model("Use the historical mean.", list(),
          input_data = data.frame(), name = "binding_repair",
          model_type = "local", validation_examples = examples, approval = approval, control = custom_model_control(
            max_attempts = limit,
            draft_path = root
          )
        )
        if (approval == "manual") {
          repaired <- create_custom_model(draft = first, llm = list(), responses = list(
            request_id = first$pending$request_id,
            answer = "yes"
          ))
          expect_identical(repaired$stage, "review_create")
          expect_identical(repaired$contract_id, first$contract_id)
          expect_false(identical(repaired$definition$version_id, first$definition$version_id))
          expect_null(repaired$consent$test)
          expect_identical(repaired$pending$review$repair$failure$unbound_symbol, "history")
          review <- capture.output(custom_author_review(repaired$pending$review))
          expect_true(any(grepl("Missing variable: history", review, fixed = TRUE)))
          create_custom_model(draft = repaired, responses = list(request_id = repaired$pending$request_id, answer = "yes"))
        } else first
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
      state <- custom_draft_load(file.path(list.dirs(root, recursive = FALSE), "current.rds"))$state
      expect_identical(state$diagnostics$snapshot$source, result$definition$source)
      local_mocked_bindings(custom_author_request = function(...) stop("Unexpected reauthoring"), .package = "finnts")
      expect_identical(create_custom_model(draft = state), result)
    }
    expect_false(grepl("PRIVATE_BINDING_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
  })
})

test_that("current authoring advertises only workflow inputs without changing saved metadata", {
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
    draft <- create_custom_model("Apply the specified business multiplier.", list(),
      input_data = data.frame(), external_regressors = drivers,
      name = "schema_rule", model_type = "local", control = custom_model_control(draft_path = withr::local_tempdir())
    )
    expected <- c("Target", "Date", "Combo", drivers)
    expect_setequal(names(requests[[1]]$metadata$schema), expected)
    expect_identical(requests[[1]]$metadata$schema, metadata$schema[names(requests[[1]]$metadata$schema)])
    expect_identical(draft$metadata, metadata)
    expect_identical(readRDS(draft$context$snapshot)$data, data)
    expect_identical(serialize(list(data = data, metadata = metadata), NULL), before)
    expect_false(grepl("PRIVATE_SCHEMA", jsonlite::toJSON(requests), fixed = TRUE))
    expect_identical(draft$pending$stage, "clarify")
  })
})

test_that("current drafts freeze deterministic authoring and replay without discovery", {
  requests <- list()
  protocol_builder <- custom_author_protocol
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2021-01-01"), by = "month", length.out = 6),
      Combo = "PRIVATE_NEW_PROTOCOL", Target = rep(c(2, 4), 3)
    ), metadata = list(
      date_type = "month", forecast_horizon = 1,
      available_packages = c("base", "lubridate"), input_context = list(external_regressors = character())
    )), custom_author_request = function(session,
                                         stage, payload) {
      requests[[length(requests) + 1L]] <<- payload
      if (stage == "source")
        return(list(
          fit_body = "mean(data$Target) + 0 * sum(lubridate::month(data$Date))", predict_body = "rep(object, nrow(new_data))",
          helpers = list(), status = "candidate", conflict = ""
        ))
      values <- lapply(payload$protocol$scaffold$scenarios, function(scenario) list(
        id = scenario$id, rationale = if (scenario$edge_case) "Mean of zeros is zero." else "Two and four average to three.",
        series = list(list(combo = "example_a", history = list(Target = if (scenario$edge_case) c(0, 0) else c(
          2,
          4
        )), future = list(), expected = if (scenario$edge_case) 0 else 3)), outcome = "predictions", error_phase = "none",
        error_code = ""
      ))
      reply <- list(
        questions = character(), interpretation = "Use the historical mean.", parameters_json = "{}",
        examples = values, parameter_schema = list(), error_policy = list(),
        validation_properties = character(), history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer())
      )
      if (identical(payload$approval_mode, "automatic"))
        reply$defaults_used <- character()
      reply
    }, .package = "finnts"
  )
  draft <- create_custom_model("Use the historical mean.",
    llm = list(), input_data = data.frame(), name = "frozen_mean",
    model_type = "local", control = custom_model_control(package_policy = "installed_finnts", draft_path = withr::local_tempdir())
  )
  expect_identical(draft$settings$authoring_protocol$version, 6L)
  expect_identical(draft$contract$authoring_protocol, 6L)
  expect_identical(draft$definition$packages, "lubridate")
  expect_length(requests, 2)
  expect_identical(draft$attempts, list(contract = 1L, source = 1L))
  expect_identical(draft$contract$examples[[1]]$expected$.pred, 3)
  expect_false(grepl("PRIVATE_NEW_PROTOCOL", jsonlite::toJSON(requests), fixed = TRUE))
  response <- list(request_id = draft$pending$request_id, answer = "yes")
  expect_error(
    create_custom_model(draft = draft, responses = response, control = custom_model_control(package_policy = "base_only")),
    "policy"
  )
  altered <- draft
  altered$settings$authoring_protocol$version <- 99L
  altered$state_digest <- custom_draft_digest(altered)
  expect_error(validate_custom_model_draft(altered), "recreate")
  altered <- draft
  altered$settings$authoring_protocol$package_policy$available <- c("base", "foreignpkg", "lubridate")
  altered$state_digest <- custom_draft_digest(altered)
  expect_error(validate_custom_model_draft(altered), "policy")
  result <- create_custom_model(draft = draft, responses = response)
  expect_s3_class(result, "finnts_custom_model")
  expect_identical(result$definition$version_id, draft$definition$version_id)
  automatic <- create_custom_model("Use the historical mean.",
    llm = list(), input_data = data.frame(), name = "frozen_mean",
    model_type = "local", approval = "automatic"
  )
  expect_s3_class(automatic, "finnts_custom_model")
  expect_identical(automatic$approval$mode, "automatic")
  expect_false(automatic$approval$intent_confirmed)
  expect_identical(automatic$definition$version_id, result$definition$version_id)
  expect_length(requests, 4)
  local_mocked_bindings(
    custom_author_request = function(...) stop("Unexpected provider"), custom_author_package_policy = function(...) stop("Unexpected discovery"),
    custom_model_package_available = function(...) stop("Unexpected namespace load"), .package = "finnts"
  )
  expect_identical(create_custom_model(draft = draft, responses = response), result)
})

test_that("reference prompts survive repair and resume without changing saved consent", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  metadata <- list(date_type = "month", forecast_horizon = 2, available_packages = "base", schema = list(
    Date = "Date",
    Combo = "character", Target = "numeric"
  ))
  protocol <- custom_author_protocol(metadata, "local", character(), "base_only", TRUE)
  examples <- custom_author_prompt_examples("source", list(
    metadata = metadata, protocol = protocol,
    name = "reference_resume", model_type = "local"
  ))[[1]]$request$contract$examples
  prompts <- character()
  sources <- 0L
  local_mocked_bindings(check_agent_ellmer_version = function() invisible(NULL), custom_author_data = function(...) list(data = data.frame(Date = seq(as.Date("2021-01-01"),
    by = "month", length.out = 8
  ), Combo = "PRIVATE_REFERENCE_SERIES", Target = seq_len(8)), metadata = metadata), new_llm_session = function(llm) list(chat_structured = function(prompt,
                                                                                                                                                     ...) {
    prompts <<- c(prompts, prompt)
    actual <- jsonlite::fromJSON(strsplit(prompt, "\n[ACTUAL_REQUEST]\n", fixed = TRUE)[[1]][[2]], simplifyVector = FALSE)
    stage <- if (is.null(actual$contract)) "contract" else "source"
    response <- custom_author_prompt_examples(stage, list(
      metadata = metadata, protocol = protocol, name = "reference_resume",
      model_type = "local"
    ))[[1]]$response
    if (stage == "source") {
      sources <<- sources + 1L
      if (sources == 1L) response$predict_body <- "rep(999, nrow(new_data))"
    }
    response
  }), .package = "finnts")
  first <- create_custom_model("Use each series' last three actual periods' mean, held constant.",
    llm = list(), input_data = data.frame(),
    name = "reference_resume", model_type = "local", validation_examples = examples,
    control = custom_model_control(package_policy = "base_only", draft_path = withr::local_tempdir())
  )
  expect_length(prompts, 2L)
  repaired <- create_custom_model(draft = first, llm = list(), responses = list(
    request_id = first$pending$request_id,
    answer = "yes"
  ))
  expect_length(prompts, 3L)
  expect_identical(repaired$contract_id, first$contract_id)
  expect_identical(repaired$contract$examples, first$contract$examples)
  expect_false(identical(repaired$definition$version_id, first$definition$version_id))
  expect_identical(repaired$stage, "review_create")
  expect_null(repaired$consent$test)
  expect_true(all(grepl("[REFERENCE_DEMONSTRATIONS]", prompts, fixed = TRUE)))
  expect_false(any(grepl("PRIVATE_REFERENCE_SERIES", prompts, fixed = TRUE)))
  expect_true(grepl("previous_source", prompts[[3]], fixed = TRUE))
  response <- list(request_id = repaired$pending$request_id, answer = "yes")
  approved <- create_custom_model(draft = repaired, responses = response)
  expect_s3_class(approved, "finnts_custom_model")
  expect_identical(approved$definition$version_id, repaired$definition$version_id)
  expect_identical(create_custom_model(draft = repaired, responses = response), approved)
  expect_length(prompts, 3L)
  expect_false("reference_demonstrations" %in% names(approved$definition))
})

# Provide a stateless new-protocol numerical/error proposal for lifecycle tests.
# Literal values exercise frozen behavior; the provider is replaced only in tests.
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

test_that("generated numerical warnings preserve source and survive approved replay", {
  for (approval in c("manual", "automatic")) local({
    metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"))
    requests <- list()
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
    root <- withr::local_tempdir()
    warnings <- character()
    output <- capture.output(model <- withCallingHandlers(
      {
        first <- create_custom_model("Divide 100 by Price; reject zero.", list(),
          input_data = data.frame(), external_regressors = "Price",
          name = "warning_ratio", model_type = "local", approval = approval, control = custom_model_control(draft_path = root)
        )
        if (approval == "manual")
          create_custom_model(draft = first, responses = list(request_id = first$pending$request_id, answer = "yes"))
        else first
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
    state <- custom_draft_load(file.path(list.dirs(root, recursive = FALSE), "current.rds"))$state
    expect_equal(state$contract$examples[[1]]$expected$.pred, 51)
    expect_equal(state$report$examples[[1]]$maximum_error, 1)
    expect_identical(state$contract$expectation_provenance, "llm_proposed_unverified")
    expect_identical(state$definition$version_id, model$definition$version_id)
    expect_identical(state$report$verification$provenance, state$contract$expectation_provenance)
    changed <- state
    changed$settings$authoring_protocol$example_comparison <- "obsolete"
    changed$state_digest <- custom_draft_digest(changed)
    expect_error(validate_custom_model_draft(changed), "protocol")
    local_mocked_bindings(custom_author_request = function(...) stop("Unexpected reauthoring"), .package = "finnts")
    expect_no_warning(restored <- create_custom_model(draft = state))
    expect_identical(restored, model)
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
    model_type = "local", approval = "automatic", validation_examples = examples
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
    model_type = "local", approval = "automatic", validation_examples = examples, control = custom_model_control(max_attempts = 2)
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
      model_type = "local", approval = "automatic", validation_examples = examples, control = custom_model_control(max_attempts = 2)
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
        model_type = "local", approval = "automatic", validation_examples = invalid, control = custom_model_control(max_attempts = 2)
      ),
      error = identity
    )
    expect_identical(error$stage, "examples")
    expect_identical(error$code, if (defect == "horizon")
      "example_horizon"
    else "invalid_examples")
    expect_identical(contracts, 1L)
    expect_identical(sources, 0L)
    expect_identical(serialize(invalid, NULL), before)
  }
})

test_that("protocol corrections preserve explicit settings and property authority", {
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"))
  builder <- custom_author_protocol
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
  root <- withr::local_tempdir()
  model <- create_custom_model("Divide 100 by future Price; reject zero Price.", list(),
    input_data = data.frame(), external_regressors = "Price",
    name = "protocol_ratio", model_type = "local", approval = "automatic", control = custom_model_control(draft_path = root)
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(model$definition$schema_version, 3L)
  expect_length(requests, 4L)
  expect_identical(requests[[2]]$payload$feedback$code, "protocol_clarification")
  expect_match(requests[[2]]$payload$feedback$facts$history_layout, "consecutive", fixed = TRUE)
  expect_identical(requests[[3]]$payload$feedback$code, "invalid_defaults_used")
  expect_identical(requests[[4]]$payload$contract$required_properties, character())
  expect_match(model$validation$checks$property_diagnostics, "1 suggested-property", fixed = TRUE)
  state <- custom_draft_load(file.path(list.dirs(root, recursive = FALSE), "current.rds"))$state
  expect_identical(state$settings$required_properties, character())
  expect_identical(create_custom_model(draft = state), model)
  changed <- state
  changed$settings$required_properties <- "target_scale_equivariant"
  changed$state_digest <- custom_draft_digest(changed)
  expect_error(validate_custom_model_draft(changed), "authority")
  expect_false(grepl("PRIVATE_PROTOCOL", jsonlite::toJSON(requests), fixed = TRUE))
})

test_that("protocol-six business questions and source conflicts stop without guessing", {
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"))
  builder <- custom_author_protocol
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
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"))
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

test_that("typed draft repair stops unchanged failures and preserves approved replay", {
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"))
  builder <- custom_author_protocol
  requests <- 0L
  source <- list(
    fit_body = "list()", predict_body = "if (any(new_data$Price == 0)) return(0); 100/new_data$Price", helpers = list(),
    status = "candidate", conflict = ""
  )
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2021-01-01"), by = "month", length.out = 8),
      Combo = "PRIVATE_TYPED", Target = 100, Price = 2
    ), metadata = metadata), custom_author_request = function(session,
                                                              stage, payload) {
      requests <<- requests + 1L
      if (stage == "contract")
        return(draft_typed_proposal())
      source
    }, .package = "finnts"
  )
  draft <- create_custom_model("Divide 100 by Price; reject zero Price.", list(),
    input_data = data.frame(), external_regressors = "Price",
    name = "typed_ratio", model_type = "local", control = custom_model_control(draft_path = withr::local_tempdir())
  )
  expect_identical(draft$settings$authoring_protocol$version, 6L)
  original_contract <- draft$contract
  repair <- create_custom_model(draft = draft, llm = list(), responses = list(request_id = draft$pending$request_id, answer = "yes"))
  expect_identical(repair$contract, original_contract)
  expect_identical(repair$definition$version_id, draft$definition$version_id)
  error <- tryCatch(create_custom_model(draft = repair, llm = list(), responses = list(
    request_id = repair$pending$request_id,
    answer = "yes"
  )), error = identity)
  expect_identical(error$code, "no_repair_progress")
  expect_identical(requests, 3L)
  source$predict_body <- "if (any(new_data$Price == 0)) finntsRuleError('zero_denominator'); 100/new_data$Price"
  fresh <- create_custom_model("Divide 100 by Price; reject zero Price.", list(),
    input_data = data.frame(), external_regressors = "Price",
    name = "typed_ratio", model_type = "local", control = custom_model_control(draft_path = withr::local_tempdir())
  )
  answer <- list(request_id = fresh$pending$request_id, answer = "yes")
  approved <- create_custom_model(draft = fresh, responses = answer)
  expect_s3_class(approved, "finnts_custom_model")
  count <- requests
  expect_identical(create_custom_model(draft = fresh, responses = answer), approved)
  expect_identical(requests, count)
  expect_match(approved$validation$checks$expectation_provenance, "unverified", fixed = TRUE)
})

test_that("automatic typed validation stops preprocessing failures without source repairs", {
  requests <- validations <- 0L
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list(Price = "numeric"))
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

test_that("deferred authoring returns a passive review before any source execution", {
  validations <- 0L
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = as.Date(c("2020-01-01", "2020-02-01")), Combo = "private-series",
      Target = c(12, 24)
    ), metadata = list(date_type = "month", forecast_horizon = 1)), custom_author_request = function(session,
                                                                                                     stage, payload) {
      if (stage == "source")
        return(list(status = "candidate", conflict = "", fit_body = "mean(data$Target)", predict_body = "rep(object,nrow(new_data))", helpers = list()))
      list(
        questions = character(), name = "draft_mean", interpretation = "Use the historical mean.", parameters_json = "{}",
        parameter_schema = list(), error_policy = list(), validation_properties = character(), history_requirements = list(
          minimum_rows_per_series = 1L,
          required_lag_periods = integer()
        )
      )
    }, custom_author_validate = function(...) {
      validations <<- validations + 1L
      stop("Unexpected execution")
    }, .package = "finnts"
  )
  draft <- create_custom_model("Use the historical mean.",
    llm = list(), input_data = data.frame(), validation_examples = draft_examples(),
    approval = "manual", control = custom_model_control(draft_path = withr::local_tempdir())
  )
  expect_s3_class(draft, "finnts_custom_model_draft")
  expect_false(inherits(draft, "finnts_custom_model"))
  expect_identical(draft$status, "review_required")
  expect_identical(draft$pending$stage, "review_create")
  expect_identical(draft$schema_version, 2L)
  expect_identical(validations, 0L)
  expect_identical(draft$settings$authoring_protocol$version, 6L)
  expect_error(
    create_custom_model(draft = draft, control = custom_model_control(package_policy = "base_only")),
    "policy"
  )
  expect_identical(unserialize(serialize(draft, NULL)), draft)
  expect_false(grepl("private-series", paste(unlist(draft, use.names = FALSE), collapse = " "), fixed = TRUE))
  expect_error(custom_run_envelope(draft), "approved")
})

# Script current provider boundaries while retaining real draft storage and
# state transitions. Arithmetic evidence is synthetic, never live validation.
draft_harness <- function(question = FALSE, repair = FALSE, .env = parent.frame()) {
  tracker <- new.env(parent = emptyenv())
  tracker$requests <- 0L
  tracker$validations <- 0L
  tracker$preparations <- 0L
  tracker$payloads <- list()
  tracker$examples <- list()
  tracker$versions <- character()
  testthat::local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) {
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
          schema_version = 2L, provenance = contract$expectation_provenance,
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
draft_reply <- function(draft, answer = "yes") {
  list(request_id = draft$pending$request_id, answer = answer)
}

test_that("UX controls preserve explicit settings and durable callback consent", {
  tracker <- draft_harness()
  expect_error(
    create_custom_model("Use mean.", list(), input_data = data.frame(), max_attempts = 1L, control = custom_model_control(max_attempts = 2L)),
    "unused argument"
  )
  expect_identical(tracker$requests, 0L)
  malformed <- custom_model_control()
  malformed$max_attempts <- 4L
  expect_error(create_custom_model("Use mean.", list(), input_data = data.frame(), control = malformed), "max_attempts")
  draft <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    control = custom_model_control(max_attempts = 2L, package_policy = "base_only", draft_path = withr::local_tempdir())
  )
  expect_identical(draft$settings$max_attempts, 2L)
  expect_identical(tracker$validations, 0L)
  expect_error(create_custom_model(draft = draft, control = custom_model_control(max_attempts = 2L)), "saved inputs and limits")
  expect_error(
    create_custom_model(draft = draft, control = custom_model_control(package_policy = "installed_finnts")),
    "policy"
  )
  model <- create_custom_model(draft = draft, responses = draft_reply(draft), control = custom_model_control())
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(tracker$validations, 1L)
  expect_identical(
    create_custom_model(draft = draft, responses = draft_reply(draft), control = custom_model_control()),
    model
  )
  root <- withr::local_tempdir()
  reviews <- list()
  callback <- function(request) {
    reviews[[length(reviews) + 1L]] <<- request
    "yes"
  }
  approved <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    control = custom_model_control(max_attempts = 2L, draft_path = root, interact = callback)
  )
  expect_s3_class(approved, "finnts_custom_model")
  expect_length(reviews, 1L)
  expect_identical(reviews[[1L]]$identity, approved$definition$version_id)
  saved <- custom_draft_load(file.path(list.dirs(root, recursive = FALSE), "current.rds"))$state
  expect_identical(saved$result, approved)
  expect_null(saved$settings$interact)
  expect_null(saved$settings$control)
  expect_identical(unserialize(serialize(saved, NULL)), saved)
  expect_error(create_custom_model("Use mean.", list(), input_data = data.frame(), interact = callback), "unused argument")
})

test_that("UX persistence does not force deferred interactive review", {
  tracker <- draft_harness()
  reviews <- 0L
  implementation <- create_custom_model
  body(implementation) <- body(implementation)
  local_mocked_bindings(interactive = function() TRUE, .package = "base")
  local_mocked_bindings(create_custom_model = implementation, custom_author_interact = function(...) {
    reviews <<- reviews + 1L
    "yes"
  }, .package = "finnts")
  model <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    control = custom_model_control(draft_path = withr::local_tempdir())
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(reviews, 1L)
  expect_identical(tracker$validations, 1L)
  local_mocked_bindings(interactive = function() FALSE, .package = "base")
  draft <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    control = custom_model_control(draft_path = withr::local_tempdir())
  )
  expect_s3_class(draft, "finnts_custom_model_draft")
  expect_identical(reviews, 1L)
  expect_identical(tracker$validations, 1L)
  expect_identical(custom_draft_load(file.path(draft$store, "current.rds"))$state, draft)
})

test_that("UX feedback is passive bounded and never changes approval or retry state", {
  tracker <- draft_harness(question = TRUE)
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  before <- serialize(draft, NULL)
  feedback <- custom_model_feedback(draft)
  expect_identical(feedback$status, "needs_input")
  expect_identical(feedback$next_action, "ask_user")
  expect_identical(unname(feedback$questions), list("Which window?"))
  expect_identical(feedback$request_id, draft$pending$request_id)
  expect_identical(feedback$attempts$remaining, list(contract = 2L, source = 3L))
  expect_identical(serialize(draft, NULL), before)
  expect_false(grepl("PRIVATE_DRAFT_SERIES", jsonlite::toJSON(feedback), fixed = TRUE))
  expect_error(custom_run_envelope(feedback), "approved")
  draft <- create_custom_model(draft = draft, responses = draft_reply(draft, list(question_1 = "All history.")), llm = list())
  review <- custom_model_feedback(draft)
  expect_identical(review$status, "review_required")
  expect_true(review$needs_user_input)
  expect_identical(review$next_action, "review_candidate")
  expect_identical(review$version_id, draft$definition$version_id)
  expect_identical(tracker$validations, 0L)
  model <- create_custom_model(draft = draft, responses = draft_reply(draft))
  success <- custom_model_feedback(model)
  expect_identical(success$status, "approved")
  expect_true(success$validation$technical_passed)
  expect_false(success$validation$business_correctness_guaranteed)
  expect_null(success$attempts$limit_per_phase)
  expect_identical(success$version_id, model$definition$version_id)
  expect_identical(custom_model_feedback(unserialize(serialize(model, NULL))), success)
  expect_identical(names(model), c("schema_version", "definition", "validation", "approval"))
  failure <- tryCatch(create_custom_model("", list(), input_data = data.frame()), error = identity)
  expect_s3_class(failure, "finnts_custom_model_authoring_error")
  expect_identical(failure$feedback, custom_model_feedback(failure))
  expect_identical(failure$feedback$next_action, "correct_inputs")
  expect_identical(failure$feedback$stage, "input")
  expect_null(failure$feedback$approval_mode)
  interaction <- tryCatch(create_custom_model("Use mean.", list(), input_data = data.frame(), control = custom_model_control(interact = TRUE)),
    error = identity
  )
  expect_identical(custom_model_feedback(interaction)$stage, "interaction")
  generic <- custom_model_feedback(simpleError("PRIVATE_PROVIDER_SECRET"))
  expect_identical(generic$stage, "unknown")
  expect_null(generic$approval_mode)
  foreign <- simpleError("PRIVATE_PROVIDER_SECRET")
  foreign$feedback <- list(status = "approved", private = "PRIVATE_PROVIDER_SECRET")
  safe <- custom_model_feedback(foreign)
  expect_identical(safe$status, "failed")
  expect_identical(safe$schema_version, 1L)
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
  expect_false(grepl("PRIVATE_PROVIDER_SECRET", jsonlite::toJSON(custom_model_feedback(simpleError("PRIVATE_PROVIDER_SECRET"))),
    fixed = TRUE
  ))
  expect_error(custom_model_feedback(list()), "requires a custom model")
})

test_that("complete source functions normalize before review and preserve repair receipts", {
  validate <- custom_author_validate
  protocol_builder <- custom_author_protocol
  invisible(NULL)
  for (approval in c("manual", "automatic")) {
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
      model_type = "local", approval = approval, validation_examples = draft_examples(), control = custom_model_control(draft_path = withr::local_tempdir())
    )
    if (approval == "manual") {
      expect_identical(validations, 0L)
      expect_identical(first$diagnostics$snapshot$source, first$definition$source)
      second <- create_custom_model(draft = first, responses = draft_reply(first), llm = list())
      expect_identical(validations, 1L)
      expect_false(identical(first$definition$version_id, second$definition$version_id))
      expect_false(identical(first$pending$request_id, second$pending$request_id))
      expect_identical(first$contract, second$contract)
      expect_identical(second$diagnostics$snapshot$source, second$definition$source)
      response <- draft_reply(second)
      model <- create_custom_model(draft = second, responses = response)
      expect_identical(create_custom_model(draft = second, responses = response), model)
    } else model <- first
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
  protocol_builder <- custom_author_protocol
  invisible(NULL)
  for (approval in c("manual", "automatic")) {
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
      model_type = "local", approval = approval, control = custom_model_control(draft_path = withr::local_tempdir())
    )
    expect_identical(requests[[1]]$protocol$version, 6L)
    expect_identical(requests[[2]]$feedback$code, "insufficient_example_history")
    expect_identical(requests[[2]]$feedback$field, "examples.history")
    expect_identical(requests[[4]]$failure$reason, "source_binding")
    expect_identical(requests[[4]]$failure$bindings$references[[1]], list(function_name = "fit", symbol = "object"))
    expect_identical(requests[[3]]$contract, requests[[4]]$contract)
    expect_identical(contracts, 2L)
    expect_identical(sources, 2L)
    if (approval == "manual") {
      expect_identical(validations, 0L)
      corrupted <- result
      corrupted$contract$history_requirements$minimum_rows_per_series <- 49L
      corrupted$contract_id <- digest::digest(corrupted$contract, algo = "sha256", serializeVersion = 2)
      corrupted$state_digest <- custom_draft_digest(corrupted)
      expect_error(validate_custom_model_draft(corrupted), "history")
      response <- draft_reply(result)
      model <- create_custom_model(draft = result, responses = response)
      expect_identical(create_custom_model(draft = result, responses = response), model)
    } else model <- result
    expect_s3_class(model, "finnts_custom_model")
    expect_identical(validations, 1L)
    expect_identical(model$approval$intent_confirmed, approval == "manual")
    expect_false(grepl("PRIVATE_HISTORY_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
  }
})

test_that("typed drafts retry native examples and validate both approval policies", {
  protocol_builder <- custom_author_protocol
  for (approval in c("manual", "automatic")) {
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
      approval = approval, control = custom_model_control(draft_path = withr::local_tempdir())
    )
    expect_identical(requests[[1]]$protocol$version, 6L)
    expect_identical(requests[[2]]$feedback$field, "examples.series")
    expect_identical(requests[[2]]$feedback$code, "invalid_examples_shape")
    expect_length(requests, 3)
    if (approval == "manual") {
      expect_identical(draft$contract$requirements$missing_data, "error")
      expect_identical(draft$diagnostics$rejected_contract$field, "examples.series")
      response <- draft_reply(draft)
      model <- create_custom_model(draft = draft, responses = response)
      expect_identical(create_custom_model(draft = draft, responses = response), model)
    } else model <- draft
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

test_that("manual contract correction persists feedback before review and freezes repaired examples", {
  tracker <- draft_harness(repair = TRUE)
  request <- custom_author_request
  root <- withr::local_tempdir()
  saved <- NULL
  local_mocked_bindings(custom_author_request = function(session, stage, payload) {
    proposal <- request(session, stage, payload)
    if (stage == "contract") {
      examples <- proposal$examples
      if (tracker$requests == 1L) {
        examples[[1]]$series[[1]]$expected <- c(15, 15)
      } else {
        pointer <- file.path(list.dirs(root, recursive = FALSE), "current.rds")
        saved <<- custom_draft_load(pointer)$state
      }
      proposal$examples <- examples
    }
    proposal
  }, .package = "finnts")
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), approval = "manual", control = custom_model_control(draft_path = root))
  expect_identical(saved$diagnostic, "invalid_examples_shape")
  expect_identical(saved$attempts, list(contract = 2L, source = 0L))
  expect_identical(saved$consent, list(intent = NULL, test = NULL))
  expect_identical(unserialize(serialize(saved, NULL)), saved)
  expect_identical(tracker$payloads[[2]]$feedback$code, "invalid_examples_shape")
  expect_identical(tracker$payloads[[2]]$feedback$field, "examples.expected")
  expect_identical(draft$stage, "review_create")
  expect_identical(draft$attempts, list(contract = 2L, source = 1L))
  expect_identical(tracker$validations, 0L)
  failed <- create_custom_model(draft = draft, responses = draft_reply(draft))
  expect_identical(failed$diagnostic, "candidate_validation_error")
  repaired <- create_custom_model(draft = failed, llm = list())
  expect_identical(repaired$attempts, list(contract = 2L, source = 2L))
  expect_identical(repaired$contract, draft$contract)
  expect_null(repaired$consent$test)
  expect_null(tracker$payloads[[4]]$feedback)
  expect_identical(tracker$payloads[[4]]$diagnostic, "candidate_validation_error")
  model <- create_custom_model(draft = repaired, responses = draft_reply(repaired))
  expect_s3_class(model, "finnts_custom_model")
  expect_true(model$approval$intent_confirmed)
  expect_identical(tracker$examples[[1]], tracker$examples[[2]])
})

test_that("manual drafts preserve mode feedback without broadening the reviewed contract", {
  tracker <- draft_harness()
  request <- custom_author_request
  root <- withr::local_tempdir()
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
        saved <<- custom_draft_load(file.path(list.dirs(root, recursive = FALSE), "current.rds"))$state
      }
      proposal$examples <- examples
    }
    proposal
  }, .package = "finnts")
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), approval = "manual", control = custom_model_control(draft_path = root))
  expect_identical(saved$diagnostic, "invalid_examples_shape")
  expect_identical(saved$attempts, list(contract = 2L, source = 0L))
  expect_identical(saved$consent, list(intent = NULL, test = NULL))
  expect_identical(unserialize(serialize(saved, NULL)), saved)
  expect_identical(tracker$payloads[[2]]$feedback$code, "invalid_examples_shape")
  expect_identical(tracker$payloads[[2]]$feedback$field, "examples.id")
  expect_identical(draft$definition$model_type, "local")
  expect_identical(draft$pending$stage, "review_create")
  expect_identical(tracker$validations, 0L)
  expect_length(draft$contract$examples, 2L)
  expect_true(all(vapply(draft$contract$examples, function(example) identical(example$model_type, "local"), logical(1))))
  expect_error(create_custom_model(draft = draft, approval = "automatic"), "policy cannot change")
  model <- create_custom_model(draft = draft, responses = draft_reply(draft))
  expect_true(model$approval$intent_confirmed)
  expect_identical(model$definition$model_type, "local")
  expect_identical(tracker$examples[[1]], draft$contract$examples)
})

test_that("required diagnostic history rejects missing and corrupt evidence", {
  tracker <- draft_harness(repair = TRUE)
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  legacy <- draft
  legacy$diagnostics <- NULL
  legacy$state_digest <- custom_draft_digest(legacy)
  expect_error(validate_custom_model_draft(legacy), "schema")
  failed <- create_custom_model(draft = draft, responses = draft_reply(draft))
  failure_index <- length(failed$diagnostics$history)
  expect_identical(failed$diagnostics$history[[failure_index]]$failure$reason, "intent_mismatch")
  expect_identical(failed$consent, list(intent = NULL, test = NULL))
  expect_identical(unserialize(serialize(failed, NULL)), failed)
  repaired <- create_custom_model(draft = failed, llm = list())
  expect_identical(repaired$contract, draft$contract)
  expect_identical(tracker$payloads[[3]]$failure$reason, "intent_mismatch")
  expect_identical(repaired$pending$review$repair$changed_functions, "fit")
  output <- capture.output(custom_author_review(repaired$pending$review))
  expect_true(any(grepl("Source changes: fit", output, fixed = TRUE)))
  for (defect in c("version", "message", "history", "source")) {
    changed <- failed
    if (defect == "version")
      changed$diagnostics$history[[failure_index]]$failure$version_id <- paste(rep("a", 64), collapse = "")
    if (defect == "message")
      changed$diagnostics$history[[failure_index]]$failure$message <- "Forged text"
    if (defect == "history")
      changed$diagnostics$history <- rep(changed$diagnostics$history, 13L)
    if (defect == "source")
      changed$diagnostics$snapshot <- list(
        contract_id = NULL, version_id = NULL, source_id = NULL, contract = NULL,
        source = "Changed"
      )
    changed$state_digest <- custom_draft_digest(changed)
    expect_error(validate_custom_model_draft(changed), class = "finnts_custom_model_authoring_error")
  }
})

test_that("diagnostic capture bounds history and omits oversized snapshots without changing contracts", {
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
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
  expect_identical(tracker$validations, 0L)
})

test_that("serialized deferred gates preserve completed work and exact approval", {
  tracker <- draft_harness(question = TRUE)
  start <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    approval = "manual", control = custom_model_control(draft_path = withr::local_tempdir())
  )
  expect_identical(start$pending$stage, "clarify")
  clarified <- create_custom_model(draft = unserialize(serialize(start, NULL)), llm = list(), responses = draft_reply(
    start,
    list(question_1 = "All history.")
  ))
  expect_identical(clarified$pending$stage, "review_create")
  source <- clarified
  expect_identical(source$environment$validation_execution, "current_session")
  expect_match(source$pending$review$warning, "current R session")
  expect_match(source$pending$review$warning, "no sandbox or reliable automatic interruption")
  expect_identical(tracker$validations, 0L)
  model <- create_custom_model(draft = source, responses = draft_reply(source))
  tested <- custom_draft_load(file.path(source$store, "current.rds"))$state
  expect_identical(tested$stage, "approved")
  expect_identical(sum(vapply(tested$receipts, function(receipt) receipt$stage == "review_create", logical(1))), 1L)
  expect_identical(tracker$validations, 1L)
  expect_identical(create_custom_model(draft = source, responses = draft_reply(source, " YeS ")), model)
  expect_error(create_custom_model(draft = source), "Stale draft")
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(model$definition$version_id, source$definition$version_id)
  expect_identical(
    create_custom_model(draft = file.path(tested$store, "current.rds"), responses = draft_reply(source)),
    model
  )
  expect_identical(tracker$requests, 3L)
  expect_identical(tracker$validations, 1L)
  expect_identical(tracker$preparations, 1L)
})

test_that("isolated-execution drafts cannot reuse consent for the current session", {
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  source <- draft
  legacy <- source
  legacy$environment$validation_execution <- NULL
  legacy <- custom_draft_gate(
    legacy, source$pending$stage, source$pending$prompt, source$pending$identity, source$pending$review,
    source$pending$questions
  )
  expect_error(create_custom_model(draft = legacy, responses = draft_reply(legacy)), "obsolete validation execution contract; start a new draft",
    class = "finnts_custom_model_authoring_error"
  )
  expect_identical(tracker$validations, 0L)
  legacy <- source
  legacy$schema_version <- 1L
  legacy$state_digest <- custom_draft_digest(legacy)
  expect_error(create_custom_model(draft = legacy), "obsolete approval workflow; start a new draft")
})

test_that("wrong responses and missing LLM leave valid revisions untouched", {
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    approval = "manual"
  )
  files <- list.files(draft$store, full.names = TRUE)
  before <- tools::md5sum(files)
  expect_error(create_custom_model(draft = draft, llm = list(), responses = draft_reply(draft, TRUE)), "Use y/yes")
  expect_error(
    create_custom_model(draft = draft, llm = list(), responses = list(request_id = "stale", answer = "yes")),
    "request_id"
  )
  expect_error(create_custom_model(draft = draft, instructions = "changed"), "saved inputs")
  expect_identical(tools::md5sum(files), before)
  expect_identical(list.files(draft$store, full.names = TRUE), files)
  expect_identical(tracker$validations, 0L)
})

test_that("clarification cannot resume without its provider", {
  tracker <- draft_harness(question = TRUE)
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  expect_error(create_custom_model(draft = draft, responses = draft_reply(draft, list(question_1 = "All history."))), "llm")
  expect_identical(tracker$validations, 0L)
})

test_that("deferred repair preserves examples and cumulative source limits", {
  tracker <- draft_harness(repair = TRUE)
  draft <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    approval = "manual"
  )
  source <- draft
  failed <- create_custom_model(draft = source, responses = draft_reply(source))
  expect_identical(failed$stage, "source")
  expect_null(failed$pending)
  repaired <- create_custom_model(draft = failed, llm = list())
  expect_false(identical(repaired$definition$version_id, source$definition$version_id))
  expect_identical(repaired$contract, source$contract)
  expect_identical(repaired$attempts$source, 2L)
  expect_identical(tracker$validations, 1L)
  expect_error(create_custom_model(draft = repaired, responses = draft_reply(source, "stale")), "changed|Stale|request")
  tested <- create_custom_model(draft = repaired, responses = draft_reply(repaired))
  expect_s3_class(tested, "finnts_custom_model")
  expect_identical(tracker$validations, 2L)
})

test_that("default deferred mode and synchronous mode share the confirmed model", {
  tracker <- draft_harness()
  deferred <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  expect_identical(deferred$status, "review_required")
  expect_false(deferred$context$durable)
  approved <- create_custom_model(draft = deferred, responses = draft_reply(deferred))
  synchronous <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    control = custom_model_control(interact = function(request) "yes")
  )
  expect_identical(approved, synchronous)
  expect_identical(tracker$validations, 2L)
})

test_that("draft corruption and changed context cannot advance approval", {
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  for (defect in c("status", "callback", "review", "identity", "limit")) local({
    altered <- draft
    if (defect == "status")
      altered$status <- "approved"
    if (defect == "callback")
      altered$answers <- list(function() stop("must not execute"))
    if (defect == "review")
      altered$pending$review$contract$interpretation <- "Changed"
    if (defect == "identity")
      altered$pending$request_id <- "stale"
    if (defect == "limit")
      altered$settings$max_attempts <- 4L
    altered$state_digest <- custom_draft_digest(altered)
    expect_error(create_custom_model(draft = altered, llm = list(), responses = draft_reply(altered)), class = "finnts_custom_model_authoring_error")
  })
  snapshot <- readRDS(draft$context$snapshot)
  snapshot$data$Target[[1]] <- 999
  saveRDS(snapshot, draft$context$snapshot)
  before <- tools::md5sum(list.files(draft$store, full.names = TRUE))
  expect_error(create_custom_model(draft = draft, llm = list(), responses = draft_reply(draft)), "context.*changed")
  expect_identical(tools::md5sum(names(before)), before)
  expect_identical(tracker$validations, 0L)
})

test_that("corrupt pointer and interrupted provider actions are never replayed", {
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  pointer <- file.path(draft$store, "current.rds")
  saveRDS(NULL, pointer)
  before <- tools::md5sum(pointer)
  expect_error(create_custom_model(draft = draft), class = "finnts_custom_model_authoring_error")
  expect_identical(tools::md5sum(pointer), before)
  other <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  local_mocked_bindings(custom_author_request = function(...) stop("Interrupted provider"), .package = "finnts")
  expect_error(
    create_custom_model(draft = other, llm = list(), responses = draft_reply(other, list(action = "edit", instructions = "Use a different fixed offset."))),
    "Interrupted provider"
  )
  expect_error(create_custom_model(draft = file.path(other$store, "current.rds"), llm = list()), "Interrupted authoring")
  expect_identical(tracker$validations, 0L)
})

test_that("deferred exhaustion cannot reset the candidate budget", {
  tracker <- draft_harness(repair = TRUE)
  draft <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    control = custom_model_control(max_attempts = 1)
  )
  source <- draft
  expect_error(create_custom_model(draft = source, responses = draft_reply(source)), "max_attempts")
  expect_identical(tracker$validations, 1L)
  expect_error(create_custom_model(draft = source, control = custom_model_control(max_attempts = 3)), "saved inputs")
})

test_that("authoring preserves language-valued options without evaluating them", {
  previous <- options(finnts_draft_option = quote(stop("Language-valued option was executed")))
  withr::defer(options(previous))
  tracker <- draft_harness()
  before <- getOption("finnts_draft_option")
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  expect_identical(draft$stage, "review_create")
  expect_identical(getOption("finnts_draft_option"), before)
})

test_that("stale writes and changed environment cannot reuse saved approval", {
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  source <- custom_draft_gate(
    draft, draft$pending$stage, draft$pending$prompt, draft$pending$identity, draft$pending$review,
    draft$pending$questions
  )
  expect_error(custom_draft_save(draft), "newer draft")
  files <- list.files(source$store, full.names = TRUE)
  before <- tools::md5sum(files)
  local({
    local_mocked_bindings(custom_draft_environment = function(...) list(changed = TRUE), .package = "finnts")
    expect_error(create_custom_model(draft = source, responses = draft_reply(source)), "environment changed")
  })
  expect_identical(tools::md5sum(files), before)
  expect_identical(tracker$validations, 0L)
  expect_error(create_custom_model(draft = source, responses = draft_reply(source, "no")), "declined")
  model <- create_custom_model(draft = source, responses = draft_reply(source))
  tested <- custom_draft_load(file.path(source$store, "current.rds"))$state
  bad <- tested
  bad$report$examples[[1]]$maximum_error <- 1
  bad$state_digest <- custom_draft_digest(bad)
  expect_error(create_custom_model(draft = bad, responses = draft_reply(bad)), "comparison failed")
  expect_identical(create_custom_model(draft = source, responses = draft_reply(source)), model)
  expect_identical(tracker$validations, 1L)
})

test_that("deferred details preserve revision and receipts without provider or execution", {
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  files <- list.files(draft$store, full.names = TRUE)
  before <- tools::md5sum(files)
  output <- capture.output(details <- create_custom_model(draft = draft, responses = draft_reply(draft, " D ")))
  expect_identical(details, draft)
  expect_identical(tools::md5sum(files), before)
  expect_identical(list.files(draft$store, full.names = TRUE), files)
  expect_identical(tracker$requests, 2L)
  expect_identical(tracker$validations, 0L)
  expect_true(grepl(draft$definition$source[["fit"]], paste(output, collapse = "\n"), fixed = TRUE))
})

# Exercise stored manual repair reviews with scripted proposals/reports. Reading
# details and replay must be passive; version-specific consent is still required
# even when the provider resubmits exactly the same failed source.
test_that("deferred repair reviews survive replay and explain unchanged candidates", {
  for (unchanged in c(FALSE, TRUE)) local({
    tracker <- draft_harness(repair = TRUE)
    request <- custom_author_request
    local_mocked_bindings(custom_author_request = function(session, stage, payload) {
      proposal <- request(session, stage, payload)
      if (unchanged && stage == "source")
        proposal$fit_body <- "999"
      proposal
    }, .package = "finnts")
    draft <- create_custom_model("Use mean.", list(),
      input_data = data.frame(), validation_examples = draft_examples(),
      control = custom_model_control(draft_path = withr::local_tempdir())
    )
    expect_null(draft$pending$review$repair)
    failed <- create_custom_model(draft = draft, responses = draft_reply(draft))
    repaired <- create_custom_model(draft = failed, llm = list())
    repair <- repaired$pending$review$repair
    expect_identical(repair$failure$version_id, draft$definition$version_id)
    expect_identical(repair$failure$phase, "example_compare")
    expect_identical(repair$changed_functions, if (unchanged)
      character()
    else "fit")
    expect_identical(repaired$contract, draft$contract)
    expect_identical(repaired$consent, list(intent = NULL, test = NULL))
    expect_identical(tracker$validations, 1L)
    expect_identical(tracker$requests, 3L)
    expect_false(identical(draft$pending$request_id, repaired$pending$request_id))
    expect_identical(create_custom_model(draft = repaired, responses = draft_reply(draft)), repaired)
    expect_identical(tracker$validations, 1L)
    expect_error(
      create_custom_model(draft = repaired, responses = list(request_id = "unknown-request", answer = "yes")),
      "Stale"
    )
    pointer <- file.path(repaired$store, "current.rds")
    expect_identical(custom_draft_load(pointer)$state, repaired)
    output <- capture.output(custom_author_review(repaired$pending$review))
    if (unchanged)
      expect_true(any(grepl("same failed candidate", output, fixed = TRUE)))
    else expect_true(any(grepl("Source changes: fit", output, fixed = TRUE)))
    expect_false(any(grepl("PRIVATE_DRAFT_SERIES|function\\(", output)))
    files <- list.files(repaired$store, full.names = TRUE)
    hashes <- tools::md5sum(files)
    details <- capture.output(replayed <- create_custom_model(draft = pointer, responses = draft_reply(repaired, "details")))
    expect_identical(replayed, repaired)
    expect_identical(tools::md5sum(files), hashes)
    expect_true(any(grepl(draft$definition$version_id, details, fixed = TRUE)))
    expect_true(any(grepl(repaired$definition$version_id, details, fixed = TRUE)))
    expect_true(grepl(repaired$definition$source[["fit"]], paste(details, collapse = "\n"), fixed = TRUE))
    expect_identical(tracker$requests, 3L)
    expect_identical(tracker$validations, 1L)
    source_entries <- which(vapply(
      repaired$diagnostics$history, function(entry) !is.null(entry$source_fingerprints),
      logical(1)
    ))
    for (fingerprints in list(
      c(fit = "bad-hash"), unname(custom_draft_source_fingerprints(repaired$definition$source)),
      c(fit = strrep("a", 64)), stats::setNames(rep(strrep("a", 64), 33L), paste0("helper_", seq_len(33L)))
    )) {
      diagnostics <- repaired$diagnostics
      diagnostics$history[[tail(source_entries, 1L)]]$source_fingerprints <- fingerprints
      expect_error(validate_custom_draft_diagnostics(diagnostics), "fingerprints")
    }
    diagnostics <- repaired$diagnostics
    diagnostics$history[[tail(source_entries, 1L)]]$source_fingerprints <- rep(
      custom_draft_source_fingerprints(repaired$definition$source),
      2L
    )
    expect_error(validate_custom_draft_diagnostics(diagnostics), "duplicate fields")
    candidate <- repaired$definition
    candidate$source[["new_helper"]] <- "function(value) value"
    candidate$source <- candidate$source[names(candidate$source) != "predict"]
    comparison <- custom_draft_review(repaired$contract, candidate, repaired$metadata, repaired$diagnostics)
    expect_identical(comparison$repair$changed_functions, if (unchanged)
      c("new_helper", "predict")
    else c("fit", "new_helper", "predict"))
    altered <- repaired
    altered$pending$review$repair$changed_functions <- "predict"
    altered$pending$request_id <- custom_draft_request_id(altered)
    altered$state_digest <- custom_draft_digest(altered)
    expect_error(validate_custom_model_draft(altered), "Review content changed")
    legacy <- repaired
    legacy$pending$review$repair <- NULL
    legacy$diagnostics$history <- lapply(legacy$diagnostics$history, function(entry) {
      entry$source_fingerprints <- NULL
      entry
    })
    legacy$pending$request_id <- custom_draft_request_id(legacy)
    legacy$state_digest <- custom_draft_digest(legacy)
    expect_identical(validate_custom_model_draft(legacy), legacy)
    model <- create_custom_model(draft = repaired, responses = draft_reply(repaired))
    expect_identical(model$definition$version_id, repaired$definition$version_id)
    expect_identical(tracker$validations, 2L)
    expect_identical(create_custom_model(draft = repaired, responses = draft_reply(repaired)), model)
    expect_identical(tracker$validations, 2L)
  })
})

test_that("deferred edits without a provider wait and cannot reset budgets", {
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  response <- draft_reply(draft, list(action = "edit", instructions = "Clarify equal weighting."))
  waiting <- create_custom_model(draft = draft, responses = response)
  expect_identical(waiting$stage, "contract")
  expect_null(waiting$pending)
  expect_identical(tracker$requests, 2L)
  expect_identical(create_custom_model(draft = draft, responses = response), waiting)
  revised <- create_custom_model(draft = waiting, llm = list())
  expect_identical(revised$stage, "review_create")
  expect_identical(revised$attempts, list(contract = 2L, source = 2L))
  expect_identical(tracker$preparations, 1L)
  expect_identical(tracker$validations, 0L)
  limited <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    control = custom_model_control(max_attempts = 1)
  )
  expect_error(
    create_custom_model(draft = limited, responses = draft_reply(limited, list(action = "edit", instructions = "Change the rule."))),
    "max_attempts"
  )
  expect_identical(tracker$validations, 0L)
})

test_that("edit text cannot widen existing model capabilities", {
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  changed <- draft$contract
  changed$model_type <- "global"
  local_mocked_bindings(custom_author_contract = function(...) changed, .package = "finnts")
  expect_error(
    create_custom_model(draft = draft, llm = list(), responses = draft_reply(draft, list(action = "edit", instructions = "Pool the series."))),
    "Edits cannot change model modes or predictors"
  )
  expect_identical(tracker$validations, 0L)
})

test_that("deferred review can switch explicitly to a synchronous trusted callback", {
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  stages <- character()
  model <- create_custom_model(draft = draft, llm = list(), approval = "manual", control = custom_model_control(interact = function(request) {
    stages <<- c(stages, request$stage)
    "yes"
  }))
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(stages, "review_create")
  expect_identical(tracker$preparations, 1L)
})

test_that("automatic repairs preserve fixed examples and exact policy on replay", {
  tracker <- draft_harness(repair = TRUE)
  local_mocked_bindings(custom_author_interact = function(...) stop("Unexpected human interaction"), .package = "finnts")
  root <- withr::local_tempdir()
  examples <- draft_examples()
  examples[[1]]$tolerance <- 0
  output <- capture.output(model <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = examples,
    approval = "automatic", control = custom_model_control(draft_path = root)
  ))
  pointer <- file.path(list.dirs(root, recursive = FALSE), "current.rds")
  state <- custom_draft_load(pointer)$state
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
  expect_identical(create_custom_model(draft = pointer), model)
  expect_identical(tracker$validations, 2L)
  expect_identical(tracker$requests, 3L)
  expect_error(create_custom_model(draft = pointer, approval = "manual"), "policy cannot change")
  expect_error(create_custom_model(draft = pointer, responses = list(request_id = "ignored", answer = "yes")), "manual responses")
  expect_error(
    create_custom_model(draft = pointer, control = custom_model_control(interact = function(request) "yes")),
    "manual responses"
  )
  expect_true(any(grepl("not manually reviewed", capture.output(print(model)), fixed = TRUE)))
  altered <- state
  altered$result$approval$mode <- NULL
  altered$result$approval$intent_confirmed <- TRUE
  altered$state_digest <- custom_draft_digest(altered)
  expect_error(validate_custom_model_draft(altered), "Result approval policy changed")
  altered <- state
  altered$receipts[[2]]$stage <- "review_create"
  altered$state_digest <- custom_draft_digest(altered)
  expect_error(validate_custom_model_draft(altered), "authorization receipt")
  altered <- state
  altered$contract$automatic_defaults$defaults_used$history_window <- "last two years"
  altered$contract_id <- digest::digest(altered$contract, algo = "sha256", serializeVersion = 2)
  altered$state_digest <- custom_draft_digest(altered)
  expect_error(validate_custom_model_draft(altered), "assumptions.*changed")
})

# Construct historical/current policies through the normal authoring lifecycle.
# Transport and numeric reports use the existing synthetic harness; persisted
# source, receipts and replay are real, and replay must not regenerate anything.
test_that("the current automatic catalog replays exactly and rejects policy tampering", {
  policy_builder <- custom_author_automatic_policy
  for (version in 2L) local({
    tracker <- draft_harness()
    local_mocked_bindings(custom_author_automatic_policy = function(...) {
      policy <- policy_builder(...)
      policy$catalog_version <- version
      policy$catalog <- custom_author_defaults()
      policy
    }, .package = "finnts")
    root <- withr::local_tempdir()
    model <- create_custom_model("Use the historical mean.", list(),
      input_data = data.frame(), validation_examples = draft_examples(),
      approval = "automatic", control = custom_model_control(draft_path = root)
    )
    pointer <- file.path(list.dirs(root, recursive = FALSE), "current.rds")
    state <- custom_draft_load(pointer)$state
    original <- serialize(state, NULL)
    expect_identical(state$settings$automatic_policy$catalog_version, version)
    expect_identical(state$settings$automatic_policy$catalog, custom_author_defaults())
    expect_identical(state$contract$automatic_defaults$catalog_version, version)
    expect_null(state$contract$automatic_defaults$defaults_used$growth_basis)
    evidence <- jsonlite::fromJSON(model$validation$checks$automatic_defaults, simplifyVector = FALSE)
    expect_equal(evidence$catalog_version, version)
    expect_null(evidence$defaults_used$growth_basis)
    expect_identical(tracker$requests, 2L)
    expect_identical(tracker$validations, 1L)
    for (changed_version in c(3L - version, 3L)) {
      altered <- state
      altered$settings$automatic_policy$catalog_version <- changed_version
      altered$state_digest <- custom_draft_digest(altered)
      expect_error(validate_custom_model_draft(altered), "defaults policy changed")
    }
    altered <- state
    altered$settings$automatic_policy$catalog$growth_basis <- "Use absolute differences instead."
    altered$state_digest <- custom_draft_digest(altered)
    expect_error(validate_custom_model_draft(altered), "defaults policy changed")
    local_mocked_bindings(
      custom_author_request = function(...) stop("Unexpected authoring"), custom_author_automatic_policy = function(...) stop("Unexpected policy rebuild"),
      custom_author_validate = function(...) stop("Unexpected revalidation"), .package = "finnts"
    )
    expect_identical(create_custom_model(draft = pointer), model)
    expect_identical(create_custom_model(draft = unserialize(original)), model)
    reloaded <- custom_draft_load(pointer)$state
    expect_identical(serialize(reloaded, NULL), original)
    expect_identical(reloaded$receipts, state$receipts)
    expect_identical(reloaded$definition$source, model$definition$source)
    expect_identical(reloaded$definition$version_id, model$definition$version_id)
  })
})

test_that("automatic cannot ask questions, exceed budgets or promote manual drafts", {
  tracker <- draft_harness(question = TRUE)
  expect_error(create_custom_model("Use weighted mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    approval = "automatic"
  ), "business context", class = "finnts_custom_model_authoring_error")
  expect_identical(tracker$requests, 1L)
  expect_identical(tracker$validations, 0L)
  tracker <- draft_harness(repair = TRUE)
  root <- withr::local_tempdir()
  expect_error(create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = draft_examples(),
    approval = "automatic", control = custom_model_control(max_attempts = 1L, draft_path = root)
  ), "max_attempts")
  expect_identical(tracker$validations, 1L)
  expect_identical(tracker$requests, 2L)
  state <- custom_draft_load(file.path(list.dirs(root, recursive = FALSE), "current.rds"))$state
  expect_null(state$result)
  tracker <- draft_harness()
  draft <- create_custom_model("Use mean.", list(), input_data = data.frame(), validation_examples = draft_examples())
  expect_error(create_custom_model(draft = draft, approval = "automatic"), "policy cannot change")
  legacy <- draft
  legacy$settings$approval_mode <- NULL
  legacy$state_digest <- custom_draft_digest(legacy)
  expect_error(validate_custom_model_draft(legacy), "fields")
  approved <- create_custom_model(draft = draft, responses = draft_reply(draft))
  expect_identical(names(approved$approval), c("version_id", "intent_confirmed", "allow_code"))
  expect_true(approved$approval$intent_confirmed)
})

test_that("printed summaries exclude private review payloads and return invisibly", {
  tracker <- draft_harness()
  draft <- create_custom_model("CONFIDENTIAL RULE", list(), input_data = data.frame(), validation_examples = draft_examples())
  output <- capture.output(printed <- withVisible(print.finnts_custom_model_draft(draft)))
  expect_false(printed$visible)
  expect_identical(printed$value, draft)
  expect_false(any(grepl("CONFIDENTIAL|PRIVATE_DRAFT_SERIES|Target|context.rds|function\\(", output)))
  approved <- create_custom_model(draft = draft, responses = draft_reply(draft))
  output <- capture.output(printed <- withVisible(print.finnts_custom_model(approved)))
  expect_false(printed$visible)
  expect_identical(printed$value, approved)
  expect_true(any(grepl(substr(approved$definition$version_id, 1L, 12L), output, fixed = TRUE)))
  expect_false(any(grepl(approved$definition$version_id, output, fixed = TRUE)))
  expect_false(any(grepl("CONFIDENTIAL|PRIVATE_DRAFT_SERIES|Target|context.rds|function\\(", output)))
})

# Offline proposals for real context/child tests. Provider transport is replaced,
# not model fitting, data preparation, draft storage or measured validation.
draft_provider_reply <- function(session, stage, payload) {
  if (stage == "contract")
    return(c(list(
      questions = character(), interpretation = "Use the historical mean.", parameters_json = "{}", parameter_schema = list(),
      error_policy = list(), validation_properties = character(), history_requirements = list(
        minimum_rows_per_series = 1L,
        required_lag_periods = integer()
      )
    ), if (is.null(payload$name)) list(name = "draft_mean")))
  list(status = "candidate", conflict = "", fit_body = "mean(data$Target)", predict_body = "rep(object, nrow(new_data))", helpers = list())
}

test_that("draft environment fails explicitly when package metadata is unavailable", {
  original <- base::system.file
  local_mocked_bindings(system.file = function(..., package = "base", lib.loc = NULL, mustWork = FALSE) {
    if (package == "stats")
      return("")
    original(..., package = package, lib.loc = lib.loc, mustWork = mustWork)
  }, .package = "base")
  expect_error(custom_draft_environment(list(available_packages = "stats")), "metadata is unavailable for stats", class = "finnts_custom_model_authoring_error")
})

test_that("prepared draft resumes use exact references without changing the original run", {
  raw <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 12), id = "private-id", value = 100)
  info <- set_run_info(project_name = "draft-prepared", path = withr::local_tempdir(), add_unique_id = FALSE)
  prep_data(info, raw, "id", "value", "month", 1,
    recipes_to_run = "R1", stationary = FALSE, clean_missing_values = FALSE,
    clean_outliers = FALSE
  )
  files <- list.files(info$path, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(files)
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_request = draft_provider_reply, .package = "finnts"
  )
  library <- withr::local_tempdir()
  package <- file.path(library, "finntsDraftDummy")
  dir.create(file.path(package, "Meta"), recursive = TRUE)
  metadata <- readRDS(base::system.file("Meta", "package.rds", package = "stats"))
  metadata$DESCRIPTION[c("Package", "Priority")] <- c("finntsDraftDummy", "recommended")
  write.dcf(as.data.frame(as.list(metadata$DESCRIPTION)), file.path(package, "DESCRIPTION"))
  saveRDS(metadata, file.path(package, "Meta", "package.rds"))
  file.create(file.path(package, "dummy_for_check"))
  withr::local_libpaths(c(library, .libPaths()))
  expect_true("finntsDraftDummy" %in% rownames(utils::installed.packages()))
  expect_identical(base::system.file("DESCRIPTION", package = "finntsDraftDummy"), "")
  draft <- create_custom_model("Use mean.", list(), run_info = info, validation_examples = draft_examples(), control = custom_model_control(draft_path = withr::local_tempdir()))
  expect_false("finntsDraftDummy" %in% draft$metadata$available_packages)
  expect_true(length(draft$context$sources) >= 2L)
  local_mocked_bindings(
    custom_author_data = function(...) stop("Unexpected repreparation"), local_artifact_inventory = function(...) stop("Unexpected rediscovery"),
    .package = "finnts"
  )
  invisible(capture.output(source <- create_custom_model(draft = draft, responses = draft_reply(draft, "details"))))
  expect_identical(source$stage, "review_create")
  expect_identical(tools::md5sum(files), before)
  path <- names(source$context$sources)[grepl("-R1\\.csv$", names(source$context$sources))][[1]]
  rows <- utils::read.csv(path, colClasses = c(Combo = "character"))
  rows$Target[[1]] <- 101
  utils::write.csv(rows, path, row.names = FALSE)
  expect_error(create_custom_model(draft = source, responses = draft_reply(source)), "Prepared source context changed")
})

test_that("durable drafts resume validation and approval in exact fresh installed processes", {
  namespace <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace, "Meta", "package.rds")), "Actual deferred validation processes are verified from the installed namespace")
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  raw <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 24), id = "PRIVATE_DURABLE_SERIES", value = 12345)
  original <- raw
  chat <- ellmer::chat_openai(model = "offline-fixture", credentials = function() "synthetic-not-a-credential")
  chat$set_system_prompt("Unchanged private template")
  payloads <- list()
  local_mocked_bindings(custom_author_request = function(session, stage, payload) {
    payloads[[length(payloads) + 1L]] <<- payload
    draft_provider_reply(session, stage, payload)
  }, .package = "finnts")
  draft <- create_custom_model("Use mean.", chat, raw,
    combo_variables = "id", target_variable = "value", date_type = "month",
    forecast_horizon = 1, validation_examples = draft_examples(), control = custom_model_control(draft_path = withr::local_tempdir())
  )
  source <- draft
  expect_false(grepl("PRIVATE_DURABLE_SERIES|12345", jsonlite::toJSON(payloads)))
  expect_false(grepl("PRIVATE_DURABLE_SERIES|12345", paste(unlist(source, use.names = FALSE), collapse = " ")))
  expect_identical(raw, original)
  snapshot_hash <- tools::md5sum(source$context$snapshot)
  pointer <- file.path(source$store, "current.rds")
  worker <- function(pointer, response, namespace, final = FALSE) {
    loadNamespace("finnts")
    stopifnot(identical(normalizePath(getNamespaceInfo(asNamespace("finnts"), "path")), normalizePath(namespace)))
    testthat::local_mocked_bindings(
      custom_author_request = function(...) stop("Unexpected provider request"), custom_author_data = function(...) stop("Unexpected repreparation"),
      .package = "finnts"
    )
    if (final)
      testthat::local_mocked_bindings(custom_author_validate = function(...) stop("Unexpected revalidation"), .package = "finnts")
    model <- finnts::create_custom_model(draft = pointer, responses = response, control = finnts::custom_model_control())
    feedback <- finnts::custom_model_feedback(model)
    stopifnot(identical(feedback$status, "approved"), identical(feedback$version_id, model$definition$version_id))
    model
  }
  environment(worker) <- baseenv()
  tested <- callr::r(worker,
    args = list(pointer = pointer, response = draft_reply(source), namespace = namespace), libpath = .libPaths(),
    system_profile = FALSE, user_profile = FALSE
  )
  expect_s3_class(tested, "finnts_custom_model")
  completed <- custom_draft_load(pointer)$state
  expect_identical(completed$stage, "approved")
  expect_true(completed$report$passed)
  expect_equal(completed$report$examples[[1]]$maximum_error, 0)
  model <- callr::r(worker,
    args = list(pointer = pointer, response = draft_reply(source), namespace = namespace, final = TRUE),
    libpath = .libPaths(), system_profile = FALSE, user_profile = FALSE
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(model$definition$version_id, source$definition$version_id)
  expect_identical(tools::md5sum(source$context$snapshot), snapshot_hash)
  expect_equal(chat$get_system_prompt(), "Unchanged private template")
  expect_length(chat$get_turns(), 0L)
  info <- set_run_info(project_name = "resumed-draft", path = withr::local_tempdir(), add_unique_id = FALSE)
  forecast_time_series(info, raw, "id", "value", "month", 1,
    custom_models = list(draft_mean = model), models_to_run = "draft_mean",
    stationary = FALSE, clean_missing_values = FALSE, recipes_to_run = "R1", run_ensemble_models = FALSE, run_global_models = FALSE,
    negative_forecast = TRUE, average_models = FALSE, back_test_scenarios = 1, return_data = FALSE
  )
  expect_equal(unique(get_forecast_data(info)$Forecast), 12345)
  contract <- resolve_agent_custom_models(list(
    project_info = list(date_type = "month", data_output = "csv", object_output = "rds"),
    forecast_horizon = 1, external_regressors = NULL, run_local_models = TRUE, run_global_models = FALSE, negative_forecast = TRUE,
    allow_hierarchical_forecast = FALSE, forecast_approach = "bottoms_up"
  ), "draft_mean", list(draft_mean = model))
  expect_identical(contract$envelopes$draft_mean, model)
  expect_error(resolve_agent_custom_models(list(), "draft_mean", list(draft_mean = source)), "approved")
  files <- list.files(info$path, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(files)
  prepared <- create_custom_model("Use mean.", chat, run_info = info, validation_examples = draft_examples(), control = custom_model_control(draft_path = withr::local_tempdir()))
  prepared_source <- prepared
  prepared_pointer <- file.path(prepared_source$store, "current.rds")
  prepared_tested <- callr::r(worker, args = list(
    pointer = prepared_pointer, response = draft_reply(prepared_source),
    namespace = namespace
  ), libpath = .libPaths(), system_profile = FALSE, user_profile = FALSE)
  prepared_model <- callr::r(worker, args = list(
    pointer = prepared_pointer, response = draft_reply(prepared_source), namespace = namespace,
    final = TRUE
  ), libpath = .libPaths(), system_profile = FALSE, user_profile = FALSE)
  expect_identical(prepared_model$definition$version_id, model$definition$version_id)
  expect_identical(tools::md5sum(files), before)
  expect_equal(chat$get_system_prompt(), "Unchanged private template")
  expect_length(chat$get_turns(), 0L)
})
