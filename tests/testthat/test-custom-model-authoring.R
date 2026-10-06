test_that("custom model controls validate defaults and retain explicit settings", {
  control <- custom_model_control()
  expect_s3_class(control, "finnts_custom_model_control")
  expect_identical(names(control), c(
    "max_attempts", "validation_budget", "package_policy", "required_properties", "forecast_approach",
    "draft_path", "interact"
  ))
  expect_identical(control$max_attempts, 3L)
  expect_identical(control$validation_budget, 60)
  expect_null(control$forecast_approach)
  expect_identical(attr(control, "supplied"), character())
  explicit <- custom_model_control(max_attempts = 2L, draft_path = NULL)
  expect_identical(attr(explicit, "supplied"), c("max_attempts", "draft_path"))
  for (arguments in list(
    list(max_attempts = 4), list(validation_budget = Inf), list(validation_budget = 121), list(package_policy = "anything"),
    list(required_properties = "unknown"), list(forecast_approach = "unknown"), list(draft_path = "https://example.test"),
    list(interact = TRUE)
  )) {
    expect_error(do.call(custom_model_control, arguments), class = "finnts_custom_model_authoring_error")
  }
  expect_error(custom_model_control(unknown = 1), "unused argument")
  for (field in c("max_attempts", "package_policy")) {
    changed <- custom_model_control()
    changed[[field]] <- if (field == "max_attempts")
      2L
    else "base_only"
    expect_error(create_custom_model(control = changed), "Rebuild changed defaults")
  }
  malformed <- custom_model_control()
  attr(malformed, "unknown") <- TRUE
  expect_error(create_custom_model(control = malformed), "control must be created")
})

test_that("public authoring signature consolidates advanced settings into control", {
  expect_identical(names(formals(create_custom_model)), c(
    "instructions", "llm", "input_data", "run_info", "combo_variables",
    "target_variable", "date_type", "forecast_horizon", "external_regressors", "hist_start_date", "hist_end_date", "name",
    "model_type", "validation_examples", "approval", "control", "draft", "responses"
  ))
})

test_that("current creation API uses controls without legacy forwarding", {
  local_mocked_bindings(custom_draft_resume = function(draft, responses, llm, deferred, interact, approval, package_policy) {
    list(draft = draft, approval = approval, package_policy = package_policy, interact = interact)
  }, check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) stop("no-provider"), .package = "finnts")
  expect_false("..." %in% names(formals(create_custom_model)))
  expect_false(exists("custom_model_authoring_impl", envir = asNamespace("finnts"), inherits = FALSE))
  expect_null(create_custom_model(draft = list())$approval)
  expect_identical(create_custom_model(draft = list(), approval = "manual")$approval, "manual")
  expect_identical(
    create_custom_model(draft = list(), control = custom_model_control(package_policy = "base_only"))$package_policy,
    "base_only"
  )
  expect_error(create_custom_model(draft = list(), control = custom_model_control(max_attempts = 1L)), "Resume uses saved")
  expect_error(create_custom_model(max_attempts = 1L), "unused argument")
  expect_error(create_custom_model(draft_path = "old-path"), "unused argument")
  evaluations <- 0L
  expect_error(create_custom_model(instructions = {
    evaluations <- evaluations + 1L
    "Use mean."
  }, llm = list(), input_data = data.frame()), "no-provider")
  expect_identical(evaluations, 1L)
  expect_error(create_custom_model(unknown_setting = TRUE), "unused argument")
})

test_that("legacy approval delivery values require explicit migration", {
  for (policy in list(NULL, "interactive", "deferred", "unknown")) {
    expect_error(create_custom_model("Use the historical mean.", llm = NULL, input_data = data.frame(), approval = policy),
      "approval must be manual or automatic",
      class = "finnts_custom_model_authoring_error"
    )
  }
})

# Small provider-visible examples have independently specified expected values.
# The second is a zero-valued edge case; neither oracle executes model source.
author_examples <- function() {
  history <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")), Combo = "sample", Target = c(10, 20))
  request <- data.frame(Date = as.Date("2020-03-01"), Combo = "sample")
  expected <- request
  expected$.pred <- 15
  first <- list(
    name = "ordinary", model_type = "local", history = history, new_data = request, expected = expected, tolerance = 1e-08,
    edge_case = FALSE, rationale = "(10 + 20) / 2 = 15", outcome = "predictions", expected_error = NULL
  )
  second <- first
  second$name <- "zero"
  second$history$Target <- c(0, 0)
  second$expected$.pred <- 0
  second$edge_case <- TRUE
  second$rationale <- "The mean of zeros is zero."
  list(first, second)
}

# Return a scripted proposal and source with no provider, session or credentials.
author_proposal <- function() {
  list(
    questions = list(), name = "human_mean", interpretation = "Use the mean of historical targets.", parameters_json = "{}",
    parameter_schema = list(), history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer()),
    error_policy = list(), validation_properties = character()
  )
}

# Return fixed source text; tests deliberately replace entries for repair cases.
author_source <- function() {
  list(
    status = "candidate", conflict = "", fit_body = "mean(data$Target)", predict_body = "rep(object, nrow(new_data))",
    helpers = list()
  )
}

test_that("authoring prompts separate reference demonstrations from the actual request", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  prompts <- character()
  session <- list(chat_structured = function(prompt, type, echo, convert) {
    prompts <<- c(prompts, prompt)
    list()
  })
  metadata <- list(date_type = "month", forecast_horizon = 3, schema = list())
  protocol <- custom_author_protocol(metadata, "local", character(), "base_only", FALSE)
  for (stage in c("contract", "source")) {
    payload <- list(
      instructions = "ACTUAL_RULE_SENTINEL", metadata = metadata, name = "actual_rule", model_type = "local",
      protocol = protocol, contract = list(instructions = "ACTUAL_RULE_SENTINEL")
    )
    expect_identical(custom_author_request(session, stage, payload), list())
    prompt <- tail(prompts, 1L)
    expect_true(grepl("[REFERENCE_DEMONSTRATIONS]", prompt, fixed = TRUE))
    expect_true(grepl("[ACTUAL_REQUEST]", prompt, fixed = TRUE))
    if (!grepl("[REFERENCE_DEMONSTRATIONS]", prompt, fixed = TRUE))
      next
    sections <- strsplit(prompt, "[REFERENCE_DEMONSTRATIONS]\n", fixed = TRUE)[[1]][[2]]
    sections <- strsplit(sections, "\n[ACTUAL_REQUEST]\n", fixed = TRUE)[[1]]
    references <- jsonlite::fromJSON(sections[[1]], simplifyVector = FALSE)
    expect_true(length(references) %in% 1:2)
    expect_false(grepl("ACTUAL_RULE_SENTINEL", sections[[1]], fixed = TRUE))
    expect_identical(sections[[2]], as.character(jsonlite::toJSON(payload,
      auto_unbox = TRUE, null = "null", dataframe = "rows",
      Date = "ISO8601", digits = NA
    )))
    expect_true(all(vapply(references, function(example) all(c("request", "response") %in% names(example)), logical(1))))
  }
  expect_length(prompts, 2L)
})


test_that("contract prompts expose exact caller error codes and phases without changing examples", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  examples <- author_examples()
  examples[[2]]["expected"] <- list(NULL)
  examples[[2]]$outcome <- "error"
  examples[[2]]$expected_error <- list(phase = "fit", code = "caller_zero")
  original <- serialize(examples, NULL)
  metadata <- list(date_type = "month", forecast_horizon = 1, schema = list())
  payload <- list(
    instructions = "Reject the caller's zero case.", metadata = metadata, name = "caller_rule", model_type = "local",
    protocol = custom_author_protocol(metadata, "local", character(), "base_only", TRUE), validation_examples = examples
  )
  prompt <- NULL
  session <- list(chat_structured = function(prompt, type, echo, convert) {
    assign("prompt", prompt, envir = parent.env(environment()))
    list()
  })
  custom_author_request(session, "contract", payload)
  sections <- strsplit(prompt, "\n[ACTUAL_REQUEST]\n", fixed = TRUE)[[1]]
  actual <- jsonlite::fromJSON(sections[[2]], simplifyVector = FALSE)
  expect_identical(actual$validation_examples[[2]]$expected_error, list(phase = "fit", code = "caller_zero"))
  expect_match(sections[[1]], "preserve each supplied expected_error code and phase exactly", fixed = TRUE)
  expect_match(sections[[1]], "do not invent extra error_policy entries", fixed = TRUE)
  expect_false(grepl("caller_zero", sections[[1]], fixed = TRUE))
  expect_identical(serialize(examples, NULL), original)
})

test_that("reference JSON preserves typed arrays, policies and retry payloads", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  metadata <- list(date_type = "quarter", forecast_horizon = 37, schema = list(
    amount = "numeric", enabled = "logical",
    .description = "character"
  ))
  for (supplied in c(FALSE, TRUE)) {
    for (catalog in 2L) {
      protocol <- custom_author_protocol(metadata, "local", names(metadata$schema), "base_only", supplied)
      policy <- custom_author_automatic_policy(
        list(metadata = metadata), "local", names(metadata$schema), "bottoms_up",
        "actual_name", NULL, 3L, 60
      )
      policy$catalog_version <- as.integer(catalog)
      policy$catalog <- custom_author_defaults()
      payload <- list(
        name = "actual_name", model_type = "local", metadata = metadata, protocol = protocol, approval_mode = "automatic",
        automatic_policy = policy, answers = list(list(multiplier = "PRIVATE_ANSWER")), feedback = list(code = "invalid_contract")
      )
      for (stage in c("contract", "source")) {
        if (stage == "source")
          payload$contract <- list(model_type = "local", instructions = "PRIVATE_RULE", requirements = list(
            date_types = "quarter",
            predictors = names(metadata$schema)
          ), examples = list("PRIVATE_EXAMPLE"))
        original <- serialize(payload, NULL)
        prompt <- NULL
        session <- list(chat_structured = function(prompt, type, echo, convert) {
          assign("prompt", prompt, envir = parent.env(environment()))
          list()
        })
        custom_author_request(session, stage, payload)
        sections <- strsplit(prompt, "[REFERENCE_DEMONSTRATIONS]\n", fixed = TRUE)[[1]][[2]]
        sections <- strsplit(sections, "\n[ACTUAL_REQUEST]\n", fixed = TRUE)[[1]]
        references <- jsonlite::fromJSON(sections[[1]], simplifyVector = FALSE)
        expect_length(references, 2L)
        expect_false(grepl("PRIVATE_", sections[[1]], fixed = TRUE))
        expect_identical(serialize(payload, NULL), original)
        expect_identical(references[[1]]$request$automatic_policy$catalog_version, as.integer(catalog))
        expect_equal(references[[1]]$request$automatic_policy$effective$forecast_horizon, 2)
        if (stage == "contract") {
          expect_setequal(names(references[[1]]$response), custom_author_contract_fields(
            protocol, "actual_name",
            "local", TRUE
          ))
          if (supplied) {
            expect_null(references[[1]]$response$examples)
            expect_true(length(references[[1]]$request$validation_examples) >= 2L)
          } else {
            example <- references[[1]]$response$examples[[1]]$series[[1]]
            expect_type(example$history$enabled[[1]], "logical")
            expect_type(example$history$.description[[1]], "character")
            expect_type(example$expected, "list")
          }
        } else {
          expect_setequal(names(references[[1]]$response), c("status", "conflict", "fit_body", "predict_body", "helpers"))
          expect_type(references[[1]]$response$helpers, "list")
        }
      }
    }
  }
  plain <- custom_author_prompt_examples("contract", list(protocol = custom_author_protocol(
    list(date_type = "month", forecast_horizon = 1),
    "local", character(), "base_only", FALSE
  ), model_type = "local"))
  expect_match(as.character(jsonlite::toJSON(plain, auto_unbox = TRUE)), "\"future\":{}", fixed = TRUE)
})

test_that("missing-variable source guidance retains the actual bounded evidence", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list())
  payload <- list(
    protocol = custom_author_protocol(metadata, "local", character(), "base_only", TRUE), contract = list(instructions = "Use the historical mean."),
    failure = custom_author_failure(NULL, "example_predict", "unbound_symbol", unbound_symbol = "history")
  )
  before <- serialize(payload, NULL)
  prompt <- NULL
  session <- list(chat_structured = function(prompt, type, echo, convert) {
    assign("prompt", prompt, envir = parent.env(environment()))
    list()
  })
  custom_author_request(session, "source", payload)
  sections <- strsplit(prompt, "\n[ACTUAL_REQUEST]\n", fixed = TRUE)[[1]]
  actual <- jsonlite::fromJSON(sections[[2]], simplifyVector = FALSE)
  expect_identical(actual$failure$reason, "unbound_symbol")
  expect_identical(actual$failure$unbound_symbol, "history")
  expect_null(actual$failure$source_message)
  expect_match(sections[[1]], "Bind fitted-state aliases explicitly", fixed = TRUE)
  expect_match(sections[[1]], "with(object$history, ...) exposes its columns", fixed = TRUE)
  expect_match(sections[[1]], "failure.unbound_symbol", fixed = TRUE)
  expect_identical(serialize(payload, NULL), before)
})

test_that("authoring has one current protocol and a shared package boundary", {
  metadata <- list(date_type = "month", forecast_horizon = 1, available_packages = "base", schema = list())
  protocol <- custom_author_protocol(metadata, "local", character(), "base_only", TRUE)
  expect_named(protocol, c("version", "package_policy", "predictors", "scaffold", "missing_data", "predictor_types"))
  expect_identical(validate_custom_author_protocol(protocol, metadata, "local", TRUE), protocol)
  expect_identical(custom_author_protocol(metadata, "global", character(), "base_only", TRUE), protocol)
  for (field in c("date_helpers", "global_support", "example_comparison")) {
    old <- protocol
    old[[field]] <- "obsolete"
    expect_error(validate_custom_author_protocol(old, metadata, "local", TRUE), "Unexpected or missing")
  }
  for (version in 1:5) {
    old <- protocol
    old$version <- version
    expect_error(validate_custom_author_protocol(old, metadata, "local", TRUE), "recreate")
  }
  expect_error(custom_author_protocol(metadata, "local", character(), "base_only", TRUE, version = 5L), "unused argument")
  metadata$available_packages <- c("base", "stats", "not_a_finnts_dependency")
  expect_identical(custom_author_package_policy(metadata, "installed_finnts")$available, c("base", "stats"))
})

test_that("date safety rejects Date simplification without blocking numeric iteration", {
  source <- c(fit = "function(data, context, parameters) data", predict = paste0(
    "function(object, new_data, context) { ",
    "lag_dates <- vapply(1:3, function(offset) as.Date('2020-01-01'), as.Date(NA)); ", "object$Target[match(lag_dates, object$Date)] }"
  ))
  expect_null(custom_author_source_preflight(source))
  error <- tryCatch(custom_author_source_preflight(source, check_dates = TRUE), error = identity)
  expect_s3_class(error, "finnts_custom_model_authoring_error")
  expect_identical(error$code, "date_class_loss")
  for (body in c(
    "base::vapply(1:3, identity, FUN.VALUE = as.Date(NA))", "sapply(1:3, function(offset) as.Date('2020-01-01'))",
    "sapply(1:3, adjust_year)"
  )) {
    rejected <- c(predict = paste0("function(object, new_data, context) { ", body, " }"), adjust_year = "function(offset) { calendar <- as.POSIXlt('2020-01-01'); as.Date(calendar) }")
    rejection <- tryCatch(custom_author_source_preflight(rejected, check_dates = TRUE), error = identity)
    expect_identical(rejection$code, "date_class_loss")
  }
  for (body in c(
    "vapply(1:3, function(value) value * 2, numeric(1))", "as.Date(vapply(1:3, function(value) as.Date('2020-01-01'), as.Date(NA)), origin = '1970-01-01')",
    "as.numeric(vapply(1:3, identity, as.Date(NA)))", "sapply(1:3, function(value) value * 2)", "sapply(1:3, function(value) as.Date('2020-01-01'), simplify = FALSE)",
    "vapply <- function(...) 1; vapply(1:3, identity, as.Date(NA))", "as.Date <- function(value) 1; vapply(1:3, identity, as.Date(NA))",
    "as.Date <- function(value) 1; sapply(1:3, as.Date)", "quote(vapply(1:3, function(value) as.Date('2020-01-01'), as.Date(NA)))"
  )) {
    allowed <- c(predict = paste0("function(object, new_data, context) { ", body, " }"))
    expect_null(custom_author_source_preflight(allowed, check_dates = TRUE))
  }
  shadow <- c(
    predict = "function(object, new_data, context) { adjust_year <- function(value) value; sapply(1:3, adjust_year) }",
    adjust_year = "function(value) as.Date(value, origin = '1970-01-01')"
  )
  expect_null(custom_author_source_preflight(shadow, check_dates = TRUE))
})

test_that("protocol-state binding checks are passive and conservative", {
  source <- c(fit = "function(data, context, parameters) { object$history <- data; object }", predict = "function(object, new_data, context) parameters$multiplier")
  expect_null(custom_author_source_preflight(source))
  error <- tryCatch(custom_author_source_preflight(source, check_bindings = TRUE), error = identity)
  expect_s3_class(error, "finnts_custom_model_authoring_error")
  expect_identical(error$code, "source_binding")
  expect_identical(error$bindings$references[[1]], list(function_name = "fit", symbol = "object"))
  expect_identical(error$bindings$references[[2]], list(function_name = "predict", symbol = "parameters"))
  failure <- custom_author_failure(NULL, "source_signature", "source_binding", bindings = error$bindings)
  expect_identical(validate_custom_author_failure(failure), failure)
  corrupt <- error$bindings
  corrupt$references[[1]]$symbol <- "PRIVATE_VALUE"
  expect_error(validate_custom_author_bindings(corrupt), "binding")
  corrupt <- error$bindings
  corrupt$references <- rep(corrupt$references, 5)
  expect_error(validate_custom_author_bindings(corrupt), "binding")
  expect_lte(nchar(jsonlite::toJSON(error$bindings, auto_unbox = TRUE), type = "bytes"), 8192)
  for (body in c(
    "object <- list(); object$history <- data; object", "data -> object; object", "helper <- function(object) object$history; list(history = data)",
    "quote(object$history)", "~object$history", "dplyr::mutate(data, value = object)", "with(data, object)"
  )) {
    allowed <- c(fit = paste0("function(data, context, parameters) { ", body, " }"))
    expect_null(custom_author_source_preflight(allowed, check_bindings = TRUE))
  }
  for (body in c("helper <- function(object) object; object$history <- data; object", "quote(object <- list()); object$history <- data; object")) {
    invalid <- c(fit = paste0("function(data, context, parameters) { ", body, " }"))
    expect_error(custom_author_source_preflight(invalid, check_bindings = TRUE), class = "finnts_custom_model_authoring_error")
  }
})

test_that("protocol-four contracts require history coverage before binding-checked source", {
  metadata <- list(date_type = "month", forecast_horizon = 3)
  protocol <- custom_author_protocol(metadata, "local", character(), "base_only", FALSE)
  expect_identical(protocol$version, 6L)
  values <- lapply(protocol$scaffold$scenarios, function(scenario) list(
    id = scenario$id, rationale = "A constant series has zero annual change.",
    series = list(list(combo = "example_a", history = list(Target = rep(100, 7)), future = list(), expected = rep(
      100,
      3
    ))), outcome = "predictions", error_phase = "none", error_code = ""
  ))
  proposal <- list(
    questions = character(), interpretation = "Use prior-year values and three annual differences.", parameters_json = "{}",
    history_requirements = list(minimum_rows_per_series = 48L, required_lag_periods = c(12L, 24L, 36L, 48L)), examples = values,
    parameter_schema = list(), error_policy = list(), validation_properties = character()
  )
  error <- tryCatch(custom_author_contract(
    proposal, "Use the explicit seasonal rule.", metadata, "history_rule", "local",
    NULL, protocol
  ), error = identity)
  expect_identical(error$code, "insufficient_example_history")
  expect_identical(error$field, "examples.history")
  for (index in seq_along(proposal$examples)) proposal$examples[[index]]$series[[1]]$history$Target <- rep(100, 48)
  contract <- custom_author_contract(
    proposal, "Use the explicit seasonal rule.", metadata, "history_rule", "local", NULL,
    protocol
  )
  expect_identical(contract$history_requirements, custom_author_history_requirements(proposal$history_requirements))
  bad <- list(
    fit_body = "object$history <- data; object", predict_body = "rep(100, nrow(new_data))", helpers = list(),
    status = "candidate", conflict = ""
  )
  error <- tryCatch(custom_author_definition(contract, bad), error = identity)
  expect_identical(error$code, "source_binding")
  expect_identical(error$bindings$references[[1]]$symbol, "object")
  bad$fit_body <- "list(history = data)"
  expect_s3_class(custom_author_definition(contract, bad), "finnts_custom_model_definition")
})

test_that("contract JSON errors identify the field without leaking parser text", {
  error <- tryCatch(custom_author_json("{\"PRIVATE_VALUE\":[1}", field = "parameters_json"), error = identity)
  expect_s3_class(error, "finnts_custom_model_authoring_error")
  expect_identical(error$code, "invalid_parameters_json")
  expect_match(conditionMessage(error), "parameters_json", fixed = TRUE)
  expect_false(grepl("PRIVATE_VALUE", conditionMessage(error), fixed = TRUE))
  expect_identical(custom_author_contract_feedback(error$code)$code, error$code)
  expect_error(custom_author_json("[]", field = "examples_json"), "Unknown JSON field")
  parameters <- custom_author_json("{\"lag\":12,\"units\":{\"lag\":\"months\"}}", field = "parameters_json")
  expect_identical(parameters$units$lag, "months")
})

test_that("new contracts accept typed examples and own missing data policy", {
  metadata <- list(date_type = "month", forecast_horizon = 3)
  protocol <- custom_author_protocol(metadata, "local", character(), "base_only", FALSE)
  expect_identical(protocol$version, 6L)
  expect_identical(protocol$missing_data, "error")
  values <- lapply(protocol$scaffold$scenarios, function(scenario) list(
    id = scenario$id, rationale = "The mean of two and four is three.",
    series = list(list(combo = "example_a", history = list(Target = c(2, 4)), future = list(), expected = c(3, 3, 3))),
    outcome = "predictions", error_phase = "none", error_code = ""
  ))
  proposal <- list(
    questions = character(), interpretation = "Use the historical mean.", parameters_json = "{}", examples = values,
    parameter_schema = list(), error_policy = list(), validation_properties = character(), history_requirements = list(
      minimum_rows_per_series = 1L,
      required_lag_periods = integer()
    )
  )
  contract <- custom_author_contract(proposal, "Use mean.", metadata, "typed_mean", "local", NULL, protocol)
  expect_identical(contract$requirements$missing_data, "error")
  expect_identical(contract$examples[[1]]$expected$.pred, c(3, 3, 3))
  expect_identical(custom_author_contract_fields(protocol, "typed_mean", "local"), c(
    "questions", "interpretation", "parameters_json",
    "history_requirements", "parameter_schema", "error_policy", "validation_properties", "examples"
  ))
  malformed <- proposal
  malformed$examples[[1]]$series <- malformed$examples[[1]]$series[[1]]
  error <- tryCatch(custom_author_contract(malformed, "Use mean.", metadata, "typed_mean", "local", NULL, protocol), error = identity)
  expect_identical(error$code, "invalid_examples_shape")
  expect_match(conditionMessage(error), "series", fixed = TRUE)
})

test_that("source dependencies are derived from a frozen eligible pool", {
  contract <- custom_author_contract(author_proposal(), "Use mean.", list(date_type = "month", forecast_horizon = 1), NULL,
    "local", author_examples(),
    protocol = custom_author_protocol(
      list(date_type = "month", forecast_horizon = 1), "local",
      character(), "base_only", TRUE
    )
  )
  proposal <- author_source()
  proposal$fit_body <- paste0("function(data, context, parameters) { ", "months <- lubridate::month(data$Date); mean(data$Target) + 0 * sum(months) }")
  expect_error(custom_author_definition(contract, proposal), "unsupported namespace")
  contract$package_policy <- list(mode = "installed_finnts", available = c("base", "dplyr", "lubridate"))
  candidate <- custom_author_definition(contract, proposal)
  expect_identical(candidate$packages, "lubridate")
  expect_identical(contract$packages, character())
  expect_identical(custom_author_definition(contract, proposal)$version_id, candidate$version_id)
  proposal$fit_body <- paste0("function(data, context, parameters) { ", "months <- lubridate::month(data$Date); mean(dplyr::pull(data, 'Target')) + 0 * sum(months) }")
  changed <- custom_author_definition(contract, proposal)
  expect_identical(changed$packages, c("dplyr", "lubridate"))
  expect_false(identical(changed$version_id, candidate$version_id))
  contract$package_policy <- list(mode = "base_only", available = "base")
  expect_error(custom_author_definition(contract, proposal), "unsupported namespace")
})

test_that("complete provider functions normalize to the same canonical body source", {
  contract <- custom_author_contract(author_proposal(), "Use mean.", list(date_type = "month", forecast_horizon = 1), NULL,
    "local", author_examples(),
    protocol = custom_author_protocol(
      list(date_type = "month", forecast_horizon = 1), "local",
      character(), "base_only", TRUE
    )
  )
  contract$package_policy <- list(mode = "base_only", available = "base")
  bodies <- list(
    fit_body = "mean(data$Target)", predict_body = "return(rep(object, nrow(new_data)))", helpers = list(),
    status = "candidate", conflict = ""
  )
  complete <- list(
    fit_body = "function(data, context, parameters) { mean(data$Target) }", predict_body = "function(object, new_data, context) { return(rep(object, nrow(new_data))) }",
    helpers = list(), status = "candidate", conflict = ""
  )
  for (protocol in 6L) {
    contract$authoring_protocol <- as.integer(protocol)
    body_model <- custom_author_definition(contract, bodies)
    full_model <- custom_author_definition(contract, complete)
    expect_identical(full_model$source, body_model$source)
    expect_identical(full_model$version_id, body_model$version_id)
    functions <- custom_model_functions(full_model, TRUE)
    fitted <- functions$fit(data.frame(Target = c(2, 4)), list(), list())
    expect_identical(fitted, 3)
    expect_identical(functions$finntsPredictBody(3, data.frame(.finnts_row = c(9L, 2L)), list()), c(3, 3))
  }
})

test_that("complete-function normalization rejects ambiguous entrypoints without execution", {
  correct <- list(
    fit_body = "mean(data$Target)", predict_body = "rep(object, nrow(new_data))", helpers = list(), status = "candidate",
    conflict = ""
  )
  bad_fit <- c(
    "function(context, data, parameters) 1", "function(data, context, parameters = NULL) 1", "function(data, context, parameters, extra) 1",
    "function(...) 1", "function(data, context, parameters) 1; 2", "function(data, context, parameters) { function(data, context, parameters) 1 }",
    "{ function(data, context, parameters) 1 }", "return(function(data, context, parameters) 1)"
  )
  for (code in bad_fit) {
    proposal <- correct
    proposal$fit_body <- code
    error <- tryCatch(custom_author_assemble(proposal[c("fit_body", "predict_body", "helpers")]), error = identity)
    expect_identical(error$code, "source_body_format")
  }
  correct$fit_body <- "mapper <- function(value) value; list(value = mapper(mean(data$Target)))"
  correct$predict_body <- "vapply(seq_len(nrow(new_data)), function(row) object$value, numeric(1))"
  expect_type(custom_author_assemble(correct[c("fit_body", "predict_body", "helpers")]), "list")
  for (code in c(
    "function(data, new_data, context) 1", "function(object, new_data, context = list()) 1", "function(object, new_data, context, ...) 1",
    "(function(object, new_data, context) 1)", "function(object, new_data, context) { return(function(...) 1) }"
  )) {
    correct$predict_body <- code
    error <- tryCatch(custom_author_assemble(correct[c("fit_body", "predict_body", "helpers")]), error = identity)
    expect_identical(error$code, "source_body_format")
  }
})

test_that("canonical saved source is never normalized on replay", {
  contract <- custom_author_contract(author_proposal(), "Use mean.", list(date_type = "month", forecast_horizon = 1), NULL,
    "local", author_examples(),
    protocol = custom_author_protocol(
      list(date_type = "month", forecast_horizon = 1), "local",
      character(), "base_only", TRUE
    )
  )
  contract$package_policy <- list(mode = "base_only", available = "base")
  complete <- list(
    fit_body = "function(data, context, parameters) { mean(data$Target) }", predict_body = "function(object, new_data, context) { rep(object, nrow(new_data)) }",
    helpers = list(), status = "candidate", conflict = ""
  )
  historical <- custom_author_assemble(complete[c("fit_body", "predict_body", "helpers")])
  source <- stats::setNames(vapply(historical$source, `[[`, character(1), "code"), vapply(
    historical$source, `[[`, character(1),
    "name"
  ))
  definition <- new_custom_model_definition(
    contract$name, contract$instructions, contract$interpretation, contract$model_type,
    source, contract$requirements, contract$fixed_parameters, contract$packages
  )
  local_mocked_bindings(custom_model_package_available = function(...) stop("Unexpected package load"), .package = "finnts")
  for (protocol in 6L) {
    contract$authoring_protocol <- as.integer(protocol)
    replayed <- custom_author_definition(contract, historical, assembled = TRUE)
    expect_identical(replayed$source, definition$source)
    expect_identical(replayed$version_id, definition$version_id)
    corrected <- custom_author_definition(contract, complete)
    expect_identical(corrected$version_id, replayed$version_id)
    normalized <- custom_author_assemble(complete[c("fit_body", "predict_body", "helpers")])
    expect_identical(custom_author_assemble(normalized, assembled = TRUE), normalized)
  }
  tampered <- historical
  tampered$source[[2L]]$code <- "function(object, new_data, context) rep(1, nrow(new_data))"
  expect_error(custom_author_definition(contract, tampered, assembled = TRUE), "wrapper")
})

test_that("body-only source has fixed signatures and protected row identities", {
  contract <- custom_author_contract(author_proposal(), "Use mean.", list(date_type = "month", forecast_horizon = 1), NULL,
    "local", author_examples(),
    protocol = custom_author_protocol(
      list(date_type = "month", forecast_horizon = 1), "local",
      character(), "base_only", TRUE
    )
  )
  contract$authoring_protocol <- 2L
  contract$package_policy <- list(mode = "base_only", available = "base")
  proposal <- list(
    fit_body = "mean(data$Target)", predict_body = "return(rep(object, nrow(new_data)))", helpers = list(),
    status = "candidate", conflict = ""
  )
  definition <- custom_author_definition(contract, proposal)
  functions <- custom_model_functions(definition, TRUE)
  expect_identical(names(formals(functions$fit)), c("data", "context", "parameters"))
  expect_identical(names(formals(functions$predict)), c("object", "new_data", "context"))
  request <- data.frame(.finnts_row = c(7L, 2L, 5L))
  fitted <- functions$fit(data.frame(Target = c(2, 4)), list(), list())
  expect_identical(functions$predict(fitted, request, list()), data.frame(.finnts_row = request$.finnts_row, .pred = rep(
    3,
    3
  )))
  saved <- list(source = lapply(names(definition$source), function(name) list(name = name, code = definition$source[[name]])))
  expect_identical(custom_author_definition(contract, saved, assembled = TRUE)$version_id, definition$version_id)
  saved$source[[which(vapply(saved$source, `[[`, character(1), "name") == "predict")]]$code <- "function(object, new_data, context) 1"
  expect_error(custom_author_definition(contract, saved, assembled = TRUE), "wrapper")
  for (body in c("return(1)", "rep(Inf, nrow(new_data))", "matrix(1, nrow(new_data), 1)", "rep('1', nrow(new_data))")) {
    proposal$predict_body <- body
    invalid <- custom_model_functions(custom_author_definition(contract, proposal), TRUE)
    expect_error(invalid$predict(fitted, request, list()), "numeric prediction")
  }
  proposal$predict_body <- "rep(object, nrow(new_data))"
  proposal$helpers <- list(list(name = "finntsPredictBody", code = "function(...) 1"))
  expect_error(custom_author_definition(contract, proposal), "reserved")
})

test_that("package policy and syntax inspection are deterministic and bounded", {
  policy <- custom_author_package_policy(list(available_packages = c("stats", "Rcpp", "lubridate", "stats")), "installed_finnts")
  expect_identical(policy, list(mode = "installed_finnts", available = c("base", "lubridate", "stats")))
  expect_identical(custom_author_package_policy(list(), "base_only"), list(mode = "base_only", available = "base"))
  source <- c(
    fit = "function(data, context, parameters) { helper <- function(value = stats::median(data$Target)) value; helper() }",
    predict = "function(object, new_data, context) base::rep(object, base::nrow(new_data))"
  )
  expect_identical(custom_author_source_guard(source, policy$available, collect = TRUE, strict = TRUE), "stats")
  rejected <- tryCatch(custom_author_source_guard(c(fit = "function(...) stats:::internal()", predict = "function(...) foreignpkg::calculate()"),
    policy$available,
    strict = TRUE
  ), error = identity)
  expect_identical(rejected$code, "source_dependency")
  expect_identical(vapply(rejected$dependency$references, `[[`, character(1), "reason"), c("internal_namespace", "undeclared_package"))
  expect_identical(rejected$dependency$allowed_packages, policy$available)
  expect_error(custom_author_source_guard(c(fit = "function(...) missing_calculation()"), "base", strict = TRUE), class = "finnts_custom_model_authoring_error")
  expect_error(custom_author_source_guard(c(fit = "function(data) lapply(data, missing_calculation)"), "base", strict = TRUE),
    class = "finnts_custom_model_authoring_error"
  )
  expect_error(custom_author_source_guard(c(fit = "function(...) getExportedValue('unknownpkg', 'calculate')()"), "base",
    strict = TRUE
  ), class = "finnts_custom_model_authoring_error")
  expect_error(custom_author_source_guard(c(fit = "function(...) base::system('DO_NOT_ECHO_SECRET')"), "base"), class = "finnts_custom_model_authoring_error")
  expect_false(grepl("DO_NOT_ECHO_SECRET", conditionMessage(rejected), fixed = TRUE))
  failure <- custom_author_failure(NULL, "source_guard", "source_dependency", dependency = rejected$dependency)
  expect_identical(validate_custom_author_failure(failure), failure)
  failure$dependency$references[[1]]$reason <- "invented_reason"
  expect_error(validate_custom_author_failure(failure), "dependency")
  many <- stats::setNames(rep("function(...) unknownpkg::calculate()", 16), paste0("helper", seq_len(16)))
  bounded <- tryCatch(custom_author_source_guard(many, "base", strict = TRUE), error = identity)$dependency
  expect_length(bounded$references, 1)
  expect_lte(nchar(jsonlite::toJSON(bounded, auto_unbox = TRUE), type = "bytes"), 8192)
  references <- lapply(seq_len(20), function(index) list(
    namespace = paste0("package", index), member = "calculate", access = "::",
    reason = "undeclared_package"
  ))
  bounded <- custom_author_dependency(references, paste0("allowed", seq_len(40)))
  expect_length(bounded$references, 8)
  expect_length(bounded$allowed_packages, 32)
  expect_identical(bounded$omitted_references, 12L)
  expect_identical(bounded$omitted_packages, 8L)
  expect_lte(nchar(jsonlite::toJSON(bounded, auto_unbox = TRUE), type = "bytes"), 8192)
  bounded$references[[1]]$member <- function() NULL
  expect_error(validate_custom_author_dependency(bounded))
})

# Independent three-month seasonal oracles use a full year of history. The
# second example checks zero/negative values without clipping or inferred rates.
author_seasonal_examples <- function() {
  history <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 12), Combo = "example", Target = seq(10,
    120,
    by = 10
  ))
  request <- data.frame(Date = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01")), Combo = "example")
  ordinary <- list(name = "seasonal", model_type = "local", history = history, new_data = request, expected = transform(request,
    .pred = c(11.1, 22.2, 33.3)
  ), tolerance = 1e-08, edge_case = FALSE, rationale = "Prior January, February and March values 10, 20, 30 times 1.11 are 11.1, 22.2, 33.3.")
  edge <- ordinary
  edge$name <- "seasonal_zero_negative"
  edge$history$Target <- c(-10, 0, 20, rep(0, 9))
  edge$expected$.pred <- c(-11.1, 0, 22.2)
  edge$edge_case <- TRUE
  edge$rationale <- "Prior-year values -10, 0, 20 times 1.11 are -11.1, 0, 22.2 without clipping."
  list(ordinary, edge)
}

# Materialize current native value fixtures from independently specified rows.
# Preserve all supplied values and ordering; scenario keys are test-only labels.
author_native_examples <- function(examples) {
  lapply(examples, function(example) list(
    id = paste0(example$model_type, if (example$edge_case) "_edge" else "_basic"),
    rationale = example$rationale, outcome = "predictions", error_phase = "none", error_code = "", series = lapply(
      seq_along(unique(example$history$Combo)),
      function(index) {
        combo <- unique(example$history$Combo)[index]
        history <- example$history[example$history$Combo == combo, ]
        future <- example$new_data[example$new_data$Combo == combo, , drop = FALSE]
        expected <- example$expected$.pred[example$expected$Combo == combo]
        list(combo = c("example_a", "example_b")[index], history = as.list(history[setdiff(names(history), c(
          "Date",
          "Combo"
        ))]), future = as.list(future[setdiff(names(future), c("Date", "Combo"))]), expected = expected)
      }
    )
  ))
}

# Offline contract fixture optionally violates the expected forecast length.
# extra_global reproduces the captured undeclared-mode pattern without private data.
author_seasonal_proposal <- function(valid = TRUE, extra_global = FALSE) {
  examples <- author_seasonal_examples()
  if (!valid) {
    examples[[1]]$expected <- rbind(examples[[1]]$expected, examples[[1]]$expected[1L, ])
  }
  if (extra_global) {
    global <- examples[[1]]
    global$name <- "global_basic"
    global$model_type <- "global"
    global$history <- rbind(global$history, transform(global$history, Combo = "other"))
    global$new_data <- rbind(global$new_data, transform(global$new_data, Combo = "other"))
    global$expected <- rbind(global$expected, transform(global$expected, Combo = "other"))
    examples <- list(examples[[1]], global, examples[[2]])
  }
  proposal <- author_proposal()
  proposal$interpretation <- "Use the same calendar month in the prior year times the fixed multiplier 1.11."
  proposal$parameters_json <- "{\"multiplier\":1.11}"
  proposal$parameter_schema <- list(list(name = "multiplier", type = "number", units = "multiplier"))
  proposal$history_requirements <- list(minimum_rows_per_series = 12L, required_lag_periods = 12L)
  proposal$examples <- author_native_examples(examples)
  proposal$defaults_used <- character()
  proposal
}

# Fixed ordinary-R seasonal source for real validation; no oracle is derived
# from this source. Fit sees analysis history only and prediction uses no Target.
author_seasonal_source <- function() {
  list(
    status = "candidate", conflict = "", fit_body = paste0("stopifnot(max(data$Date) == context$cutoff); ", "list(months = format(data$Date, '%Y-%m'), targets = data$Target, multiplier = parameters$multiplier)"),
    predict_body = paste0(
      "stopifnot(!'Target' %in% names(new_data)); ", "prior_months <- paste0(as.integer(format(new_data$Date, '%Y')) - 1L, '-', format(new_data$Date, '%m')); ",
      "object$targets[match(prior_months, object$months)] * object$multiplier"
    ), helpers = list()
  )
}

# Reproduce the captured month/year confusion with existing seasonal oracles.
# The corrected candidate converts only implementation units, never parameters.
author_unit_source <- function(correct = FALSE) {
  list(
    status = "candidate", conflict = "", fit_body = "list(history=data,lag=parameters$lag)", predict_body = paste0(
      "prior <- lag_date_years(new_data$Date,object$lag",
      if (correct) "/12" else "", "); values <- object$history$Target[match(prior,object$history$Date)]; ", "if (anyNA(values)) stop('Missing historical observation for required lag date'); values"
    ),
    helpers = list(list(name = "lag_date_years", code = "function(dates, years) { value <- as.POSIXlt(dates); value$year <- value$year-years; as.Date(value) }"))
  )
}

test_that("source repair uses previous candidate and phase evidence without changing the oracle", {
  invisible(NULL)
  requests <- list()
  examples <- author_seasonal_examples()
  examples[[1]]$expected$.pred <- c(10, 20, 30)
  examples[[2]]$expected$.pred <- c(-10, 0, 20)
  examples[[1]]$rationale <- "Same prior-year observations are 10, 20 and 30."
  examples[[2]]$rationale <- "Same prior-year observations are -10, 0 and 20."
  proposal <- author_seasonal_proposal()
  proposal$interpretation <- "Same calendar month last year; lag is 12 monthly periods."
  proposal$parameters_json <- "{\"lag\":12}"
  proposal$parameter_schema <- list(list(name = "lag", type = "number", units = "months"))
  proposal$examples <- NULL
  root <- withr::local_tempdir()
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2020-01-01"), by = "month", length.out = 36),
      Combo = "PRIVATE_REPAIR_DATA", Target = rep(seq(10, 120, 10), 3)
    ), metadata = list(date_type = "month", forecast_horizon = 3)),
    custom_author_request = function(session, stage, payload) {
      requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
      if (stage == "contract")
        return(proposal)
      correct <- identical(payload$failure$phase, "example_predict") && identical(payload$failure$source_message, "Missing historical observation for required lag date") &&
        !is.null(payload$previous_source) && identical(payload$failure$example_index, 1L)
      author_unit_source(correct)
    }, custom_author_interact = function(...) stop("Unexpected prompt"), .package = "finnts"
  )
  model <- create_custom_model("Use the prior yearly seasonal value.", list(),
    input_data = data.frame(), approval = "automatic",
    validation_examples = examples, control = custom_model_control(draft_path = root)
  )
  saved <- custom_draft_load(file.path(list.dirs(root, recursive = FALSE), "current.rds"))$state
  expect_true(model$validation$technical_passed)
  expect_equal(model$definition$fixed_parameters$lag, 12)
  expect_identical(saved$attempts, list(contract = 1L, source = 2L))
  expect_identical(requests[[2]]$payload$contract, requests[[3]]$payload$contract)
  expect_match(requests[[3]]$payload$previous_source[["finntsPredictBody"]], "lag_date_years(new_data$Date, object$lag)",
    fixed = TRUE
  )
  expect_identical(requests[[3]]$payload$previous_version_id, requests[[3]]$payload$failure$version_id)
  expect_identical(requests[[3]]$payload$contract_id, saved$contract_id)
  expect_length(saved$receipts, 2L)
  expect_length(saved$diagnostics$history, 5L)
  expect_identical(saved$diagnostics$history[[3]]$failure$phase, "example_predict")
  expect_false(grepl("PRIVATE_REPAIR_DATA", jsonlite::toJSON(saved$diagnostics), fixed = TRUE))
  expect_identical(create_custom_model(draft = file.path(saved$store, "current.rds")), model)
  expect_length(requests, 3L)
})

test_that("source preflight accepts named protocols and rejects incompatible signatures passively", {
  for (source in list(
    c(fit = "function(data, context, parameters) NULL", predict = "function(object, new_data, context) NULL"),
    c(fit = "function(...) NULL", predict = "function(object, ..., extra = NULL) NULL")
  )) {
    expect_invisible(custom_author_source_preflight(source))
  }
  for (source in list(
    c(fit = "function(x, context, parameters) NULL"), c(predict = "function(object, new_data, context, extra) NULL"),
    c(fit = "function(data, ..., extra) NULL")
  )) {
    error <- tryCatch(custom_author_source_preflight(source), error = identity)
    expect_identical(error$code, "source_signature")
  }
  error <- tryCatch(custom_author_source_preflight(c(fit = "function(")), error = identity)
  expect_identical(error$code, "source_syntax")
  expect_error(custom_author_source_guard(c(fit = "function(...) base::system('never')"), character()), "unsupported namespace")
  expect_error(custom_author_source_guard(c(fit = "function(...) stats::median(1)"), character()), "unsupported namespace")
})

test_that("contract retries explain horizon rejection before seasonal source generation", {
  invisible(NULL)
  requests <- list()
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2020-01-01"), by = "month", length.out = 36),
      Combo = "PRIVATE_SEASONAL_SERIES", Target = rep(seq(10, 120, by = 10), 3)
    ), metadata = list(
      date_type = "month",
      forecast_horizon = 3
    )), custom_author_request = function(session, stage, payload) {
      requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
      if (stage == "contract")
        return(author_seasonal_proposal(identical(payload$feedback$code, "invalid_examples_shape")))
      author_seasonal_source()
    }, custom_author_interact = function(...) stop("Unexpected manual review"), .package = "finnts"
  )
  model <- create_custom_model("Use the prior yearly seasonal value, plus an 11% increase.", list(),
    input_data = data.frame(),
    forecast_horizon = 3, approval = "automatic"
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(vapply(requests, function(request) request$stage, character(1)), c("contract", "contract", "source"))
  expect_identical(requests[[2]]$payload$feedback$code, "invalid_examples_shape")
  expect_identical(requests[[2]]$payload$feedback$field, "examples.expected")
  expect_identical(model$definition$fixed_parameters$multiplier, 1.11)
  expect_true(model$validation$technical_passed)
  expect_false(model$approval$intent_confirmed)
  expect_match(model$validation$checks$example_1, "maximum error")
  expect_match(model$validation$checks$example_2, "maximum error")
  expect_match(model$validation$checks$holdout_1, "rows 3")
  expect_match(model$validation$checks$serialization, "matched")
  expect_false(grepl("PRIVATE_SEASONAL_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
})

test_that("mode feedback corrects generated examples before real seasonal validation", {
  invisible(NULL)
  requests <- list()
  validations <- 0L
  validator <- custom_author_validate
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2020-01-01"), by = "month", length.out = 36),
      Combo = "PRIVATE_MODE_SERIES", Target = rep(seq(10, 120, by = 10), 3)
    ), metadata = list(
      date_type = "month",
      forecast_horizon = 3
    )), custom_author_request = function(session, stage, payload) {
      requests[[length(requests) + 1L]] <<- list(stage = stage, payload = payload)
      if (stage == "contract") {
        expect_identical(validations, 0L)
        return(author_seasonal_proposal(extra_global = !identical(payload$feedback$code, "invalid_examples_shape")))
      }
      author_seasonal_source()
    }, custom_author_validate = function(...) {
      validations <<- validations + 1L
      validator(...)
    }, custom_author_interact = function(...) stop("Unexpected manual review"), .package = "finnts"
  )
  model <- create_custom_model("Use the prior yearly seasonal value, plus an 11% increase.", list(),
    input_data = data.frame(),
    forecast_horizon = 3, approval = "automatic"
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(vapply(requests, function(request) request$stage, character(1)), c("contract", "contract", "source"))
  expect_identical(requests[[2]]$payload$feedback$code, "invalid_examples_shape")
  expect_identical(requests[[2]]$payload$feedback$field, "examples.id")
  expect_identical(model$definition$model_type, "local")
  expect_identical(model$definition$fixed_parameters$multiplier, 1.11)
  expect_true(model$validation$technical_passed)
  expect_identical(validations, 1L)
  expect_identical(
    vapply(requests[[3]]$payload$contract$examples, function(example) example$model_type, character(1)),
    rep("local", 2)
  )
  expect_identical(requests[[3]]$payload$contract$examples[[2]]$expected$.pred, c(-11.1, 0, 22.2))
  expect_false(model$approval$intent_confirmed)
  expect_match(model$validation$checks$holdout_1, "rows 3")
  expect_match(model$validation$checks$serialization, "matched")
  expect_false(grepl("PRIVATE_MODE_SERIES", jsonlite::toJSON(requests), fixed = TRUE))
})

# Script only proposal transport and sample preparation; numerical validation is
# real. One user decision must bind the candidate and its conditional approval.
test_that("approval has two explicit policies and automatic mode needs no review", {
  invisible(NULL)
  expect_identical(formals(create_custom_model)$approval, "manual")
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
      Combo = "private", Target = 100
    ), metadata = list(date_type = "month", forecast_horizon = 1)), custom_author_request = function(session,
                                                                                                     stage, payload) {
      if (stage == "contract")
        c(author_proposal(), list(defaults_used = c("history_window", "averaging_weights")))
      else author_source()
    }, custom_author_interact = function(...) stop("Unexpected manual review"), .package = "finnts"
  )
  local_mocked_bindings(readline = function(...) stop("Unexpected console question"), .package = "base")
  model <- create_custom_model("Use the historical mean.", list(),
    input_data = data.frame(), validation_examples = author_examples(),
    approval = "automatic"
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(model$approval$mode, "automatic")
  expect_identical(model$approval$intent_confirmed, FALSE)
  expect_true(model$approval$allow_code)
  expect_true(model$validation$technical_passed)
  expect_false(any(grepl("Confirmed intent|Confirmed examples", unlist(model$validation$checks))))
})

test_that("one affirmative review creates a validated model", {
  invisible(NULL)
  stages <- character()
  proposals <- 0L
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
      Combo = "private", Target = 100
    ), metadata = list(date_type = "month", forecast_horizon = 1)), custom_author_request = function(session,
                                                                                                     stage, payload) {
      proposals <<- proposals + 1L
      if (stage == "contract")
        author_proposal()
      else author_source()
    }, .package = "finnts"
  )
  model <- create_custom_model("Use the historical mean.", list(),
    input_data = data.frame(), validation_examples = author_examples(),
    control = custom_model_control(interact = function(request) {
      stages <<- c(stages, request$stage)
      " y "
    })
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(stages, "review_create")
  expect_identical(proposals, 2L)
  expect_true(model$validation$technical_passed)
  expect_identical(model$approval$version_id, model$definition$version_id)
})

test_that("the current automatic catalog keeps explicit defaults and growth basis", {
  legacy <- list(
    model_type = "local", forecast_approach = "bottoms_up", history_window = "All supplied training history within historical bounds and the current fold cutoff.",
    averaging_weights = "Equal weight for an otherwise unspecified arithmetic average.", external_regressors = "None unless explicitly declared in arguments or prepared-run metadata.",
    missing_values = "Reject empty, missing or nonfinite required history/predictors and duplicate series/date rows; no dropping or imputation.",
    zero_negative_values = "Zero and negative observations are valid; no implicit clipping or rounding.", zero_denominator = "Reject undefined rule arithmetic; do not invent epsilon, growth rates or caps.",
    implicit_structure = "No implicit trend, seasonality, transformations or tuning.", packages = "Prefer base R and existing declared installed dependencies; never install packages.",
    worked_examples = "Use 2-8 independently calculated examples, every declared mode and an edge case; generated tolerance is 1e-8.",
    name = "Generate a safe descriptive model name when no name is supplied."
  )
  expect_identical(custom_author_defaults()[names(legacy)], legacy)
  current <- custom_author_defaults()
  expect_identical(current[names(legacy)], legacy)
  expect_identical(setdiff(names(current), names(legacy)), "growth_basis")
  expect_identical(custom_author_defaults(), current)
  expect_error(custom_author_defaults(version = 1L), "unused argument")
})

test_that("automatic defaults are closed and cannot override explicit capabilities", {
  metadata <- list(
    date_type = "month", forecast_horizon = 1, date_start = "2020-01-01", date_end = "2021-12-01", sampled_series = 2L,
    sampled_rows = 48L, input_context = list(external_regressors = "Driver", hist_start_date = "2020-06-01", hist_end_date = "2021-12-01")
  )
  policy <- custom_author_automatic_policy(
    list(metadata = metadata), "global", NULL, "bottoms_up", "fixed_name", author_examples(),
    2, 45
  )
  expect_identical(policy$catalog_version, 2L)
  expect_true("growth_basis" %in% names(policy$catalog))
  if ("growth_basis" %in% names(policy$catalog)) {
    expect_match(policy$catalog$growth_basis, "percentage", fixed = TRUE)
    expect_match(policy$catalog$growth_basis, "newer / older - 1", fixed = TRUE)
  }
  expect_identical(policy$effective$model_type, "global")
  expect_identical(policy$effective$predictors, "Driver")
  expect_identical(policy$effective$hist_start_date, "2020-06-01")
  expect_identical(policy$effective$max_attempts, 2L)
  expect_identical(policy$effective$validation_timeout, 45)
  proposal <- c(author_proposal(), list(defaults_used = "history_window"))
  proposal$model_type <- "global"
  proposal$predictors <- "Driver"
  assumptions <- custom_author_assumptions(proposal, policy)
  expect_identical(assumptions$defaults_used$history_window, custom_author_defaults()$history_window)
  expect_false(any(c("model_type", "external_regressors", "name", "worked_examples") %in% names(assumptions$defaults_used)))
  expect_null(assumptions$defaults_used$growth_basis)
  growth <- proposal
  growth$defaults_used <- "growth_basis"
  expect_identical(custom_author_assumptions(growth, policy)$defaults_used$growth_basis, policy$catalog$growth_basis)
  for (interpretation in c("Use absolute annual differences.", "Use CAGR.", "Use percentage-point changes.", "Use the mean.")) {
    explicit <- proposal
    explicit$interpretation <- interpretation
    explicit$defaults_used <- character()
    expect_null(custom_author_assumptions(explicit, policy)$defaults_used$growth_basis)
  }
  for (keys in list("growth_rate", "model_type", "name", "external_regressors", c("history_window", "history_window"))) {
    changed <- proposal
    changed$defaults_used <- keys
    expect_error(custom_author_assumptions(changed, policy), "unknown|contradict", class = "finnts_custom_model_authoring_error")
  }
  changed <- proposal
  changed$model_type <- "local"
  expect_error(custom_author_assumptions(changed, policy), "conflict")
  changed <- proposal
  changed$predictors <- c("Driver", "GeneratedFeature")
  expect_error(custom_author_assumptions(changed, policy), "conflict")
  changed$predictors <- character()
  expect_error(custom_author_assumptions(changed, policy), "conflict")
  for (mode in list(NULL, NA_character_, c("manual", "automatic"), "other")) {
    expect_error(custom_author_checks(NULL, NULL, NULL, NULL, approval_mode = mode), "Unknown approval policy", class = "finnts_custom_model_authoring_error")
  }
})

test_that("automatic rejects manual input before any preparation or provider call", {
  local_mocked_bindings(
    check_agent_ellmer_version = function() stop("Unexpected provider setup"), custom_author_data = function(...) stop("Unexpected preparation"),
    .package = "finnts"
  )
  expect_error(
    create_custom_model("Use mean.", approval = "automatic", control = custom_model_control(interact = function(request) "yes")),
    "cannot use interact"
  )
  expect_error(create_custom_model("Use mean.", approval = "automatic", responses = list(answer = "yes")), "manual responses")
})

test_that("deferred hierarchy authoring reviews all derived nodes without forecast enrollment", {
  invisible(NULL)
  raw <- expand.grid(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 12), Region = c(
    "PRIVATE_HIER_NORTH",
    "PRIVATE_HIER_SOUTH"
  ), Product = c("A", "B"), stringsAsFactors = FALSE)
  raw$Product <- paste(raw$Region, raw$Product, sep = "_")
  raw$value <- 12345
  payloads <- list()
  local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_request = function(session, stage, payload) {
      payloads[[length(payloads) + 1L]] <<- payload
      if (stage == "contract")
        author_proposal()
      else author_source()
    }, .package = "finnts"
  )
  draft <- create_custom_model("Use each node's historical mean, then reconcile.", list(), raw,
    combo_variables = c(
      "Region",
      "Product"
    ), target_variable = "value", date_type = "month", forecast_horizon = 1, validation_examples = author_examples(),
    approval = "manual", control = custom_model_control(forecast_approach = "standard_hierarchy", draft_path = withr::local_tempdir())
  )
  expect_identical(draft$pending$stage, "review_create")
  expect_identical(draft$settings$forecast_approach, "standard_hierarchy")
  expect_identical(draft$contract$requirements$hierarchy$application, "each_prepared_node")
  expect_identical(draft$contract$requirements$hierarchy$reconciliation, "finnts_reconciliation")
  expect_match(draft$pending$review$reconciliation, "may adjust")
  expect_equal(draft$metadata$hierarchy$sampled_leaves, 3)
  expect_gt(draft$metadata$hierarchy$node_count, 3)
  snapshot <- readRDS(draft$context$snapshot)
  expect_equal(length(unique(snapshot$data$Combo)), draft$metadata$hierarchy$node_count)
  expect_false(grepl("PRIVATE_HIER|12345", jsonlite::toJSON(payloads)))
  expect_error(
    create_custom_model(draft = draft, control = custom_model_control(forecast_approach = "grouped_hierarchy")),
    "saved inputs"
  )
  source <- draft
  expect_identical(source$definition$schema_version, 3L)
  expect_identical(source$pending$stage, "review_create")
  info <- set_run_info(
    project_name = "prepared-authoring-hierarchy", path = withr::local_tempdir(), data_output = "csv",
    object_output = "rds", add_unique_id = FALSE
  )
  prep_data(info, raw, c("Region", "Product"), "value", "month", 1,
    forecast_approach = "standard_hierarchy", recipes_to_run = "R1",
    stationary = FALSE, box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE, multistep_horizon = FALSE
  )
  files <- list.files(info$path, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(files)
  local_mocked_bindings(
    prep_data = function(...) stop("Unexpected hierarchy repreparation"), local_artifact_inventory = function(...) stop("Unexpected hierarchy discovery"),
    .package = "finnts"
  )
  prepared <- create_custom_model("Use each node's historical mean, then reconcile.", list(),
    run_info = info, validation_examples = author_examples(),
    approval = "manual", control = custom_model_control(forecast_approach = "standard_hierarchy", draft_path = withr::local_tempdir())
  )
  expect_equal(prepared$metadata$hierarchy$sampled_leaves, 4)
  expect_equal(length(unique(readRDS(prepared$context$snapshot)$data$Combo)), prepared$metadata$hierarchy$node_count)
  expect_true(any(grepl("-hts_info[.]rds$", names(prepared$context$sources))))
  expect_true(any(grepl("-hts_data[.]csv$", names(prepared$context$sources))))
  expect_identical(tools::md5sum(files), before)
  log <- read_exact_artifact(info, local_artifact_path(info, "logs", extension = "csv"))
  expect_error(custom_run_hierarchy_context(info, custom_run_context(info, log, FALSE, allow_hierarchy = TRUE), log,
    return_data = TRUE,
    max_rows = 1
  ), "row limit")
  path <- local_artifact_path(info, "prep_data", "-hts_info", extension = "rds")
  metadata <- readRDS(path)
  metadata$hts_combos <- rev(metadata$hts_combos)
  saveRDS(metadata, path)
  expect_error(create_custom_model(draft = prepared, llm = list(), responses = list(
    request_id = prepared$pending$request_id,
    answer = prepared$pending$prompt
  )), "Prepared source context changed")
})

test_that("confirmed contracts and source reject unexpected or altered fields", {
  metadata <- list(date_type = "month", forecast_horizon = 1)
  contract <- custom_author_contract(author_proposal(), "Use mean.", metadata, NULL, "local", author_examples(), protocol = custom_author_protocol(
    metadata,
    "local", character(), "base_only", TRUE
  ))
  definition <- custom_author_definition(contract, author_source())
  expect_identical(definition$name, "human_mean")
  expect_identical(definition$fixed_parameters, list())
  expect_equal(contract$examples[[1]]$expected$.pred, 15)
  expect_true(custom_run_envelope(structure(list(schema_version = 1L, definition = definition, validation = list(
    version_id = definition$version_id,
    technical_passed = TRUE, checks = list(test = "synthetic")
  ), approval = list(
    version_id = definition$version_id,
    intent_confirmed = TRUE, allow_code = TRUE
  )), class = "finnts_custom_model"))$approval$intent_confirmed)
  invalid <- author_proposal()
  invalid$approved <- TRUE
  expect_error(custom_author_contract(invalid, "Use mean.", metadata, NULL, "local", author_examples(), protocol = custom_author_protocol(
    metadata,
    "local", character(), "base_only", TRUE
  )), "fields")
  expect_error(custom_author_contract(author_proposal(), "Use mean.", metadata, "other", "local", author_examples(), protocol = custom_author_protocol(
    metadata,
    "local", character(), "base_only", TRUE
  )), "fields")
  expect_error(custom_author_contract(author_proposal(), "Use mean.", metadata, NULL, "global", author_examples(), protocol = custom_author_protocol(
    metadata,
    "global", character(), "base_only", TRUE
  )), "fields")
  invalid <- author_source()
  invalid$approval <- TRUE
  expect_error(custom_author_definition(contract, invalid), "fields")
  invalid <- author_source()
  invalid$fit_body <- "install.packages('unrequested')"
  expect_error(custom_author_definition(contract, invalid), "unsupported")
})

test_that("hierarchy evidence must cover every captured node", {
  metadata <- list(date_type = "month", forecast_horizon = 1, forecast_approach = "standard_hierarchy", hierarchy = list(node_count = 2L))
  contract <- custom_author_contract(author_proposal(), "Use mean.", metadata, NULL, "local", author_examples(), protocol = custom_author_protocol(
    metadata,
    "local", character(), "base_only", TRUE
  ))
  definition <- custom_author_definition(contract, author_source())
  report <- list(
    passed = TRUE, code = "passed", version_id = definition$version_id, examples_id = digest::digest(contract$examples,
      algo = "sha256", serializeVersion = 2
    ), examples = lapply(contract$examples, function(example) list(
      name = example$name,
      model_type = example$model_type, maximum_error = 0, tolerance = example$tolerance
    )), holdouts = list(list(
      model_type = "local",
      rows = 1L, training_rows = 2L, cutoff = "2020-02-01", mae = 1, wmape = 0.01
    )), serialization = TRUE, r_version = as.character(getRversion()),
    package_version = "synthetic", elapsed = 1, verification = list(
      schema_version = 2L,
      provenance = contract$expectation_provenance, properties = list(row_order = 2L), expected_errors = 0L,
      required_properties = character(), diagnostics = list()
    )
  )
  expect_error(custom_author_checks(report, definition, "contract", contract$examples, metadata, contract = contract), "hierarchy node")
  report$holdouts <- rep(report$holdouts, 2)
  checks <- custom_author_checks(report, definition, "contract", contract$examples, metadata, contract = contract)
  expect_match(checks$hierarchy, "2 independent node holdouts")
})

# Only provider replies and synthetic human responses are supplied here. All
# validation, persistence and forecast steps are real and stay in the caller PID.
test_that("same-session authoring and deferred approval feed a staged standard forecast", {
  invisible(NULL)
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  raw <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 24), id = "private-local", value = 100 +
    seq_len(24))
  chat <- ellmer::chat_openai(model = "offline-fixture", credentials = function() "synthetic-not-a-credential")
  requests <- 0L
  validations <- 0L
  validator <- custom_author_validate
  local_mocked_bindings(custom_author_request = function(session, stage, payload) {
    requests <<- requests + 1L
    if (stage == "contract")
      author_proposal()
    else author_source()
  }, custom_author_validate = function(...) {
    validations <<- validations + 1L
    validator(...)
  }, .package = "finnts")
  local_mocked_bindings(
    r = function(...) stop("Unexpected authoring subprocess"), rcmd = function(...) stop("Unexpected authoring installation"),
    .package = "callr"
  )
  stages <- character()
  model <- create_custom_model("Use each series' historical mean.", chat, raw,
    combo_variables = "id", target_variable = "value",
    date_type = "month", forecast_horizon = 1, model_type = "local", validation_examples = author_examples(), control = custom_model_control(interact = function(request) {
      stages <<- c(stages, request$stage)
      if (request$stage == "review_create") {
        expect_identical(validations, 0L)
        expect_match(request$review$warning, "current R session")
      }
      "yes"
    })
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(stages, "review_create")
  expect_identical(validations, 1L)
  info <- set_run_info(project_name = "same-session-authoring", path = withr::local_tempdir(), add_unique_id = FALSE)
  prep_data(info, raw, "id", "value", "month", 1,
    recipes_to_run = "R1", stationary = FALSE, box_cox = FALSE, clean_missing_values = FALSE,
    clean_outliers = FALSE, multistep_horizon = FALSE
  )
  draft <- create_custom_model("Use each series' historical mean.", chat,
    run_info = info, model_type = "local", validation_examples = author_examples(),
    approval = "manual"
  )
  expect_identical(validations, 1L)
  approved <- create_custom_model(draft = draft, responses = list(request_id = draft$pending$request_id, answer = "yes"))
  expect_identical(approved$definition$version_id, model$definition$version_id)
  expect_identical(validations, 2L)
  expect_identical(requests, 4L)
  local_mocked_bindings(custom_author_request = function(...) stop("Unexpected forecast-time provider"), .package = "finnts")
  prep_models(info,
    custom_models = list(human_mean = approved), models_to_run = c("human_mean", "meanf"), run_ensemble_models = FALSE,
    back_test_scenarios = 1, num_hyperparameters = 1, pca = FALSE
  )
  train_models(info,
    run_local_models = TRUE, run_global_models = FALSE, feature_selection = FALSE, negative_forecast = TRUE,
    parallel_processing = NULL, inner_parallel = FALSE, num_cores = 1
  )
  final_models(info,
    average_models = FALSE, weekly_to_daily = FALSE, parallel_processing = NULL, inner_parallel = FALSE,
    num_cores = 1
  )
  forecasts <- get_forecast_data(info)
  custom <- forecasts[forecasts$Model_Name == "human_mean" & forecasts$Date > max(raw$Date), ]
  expect_equal(nrow(custom), 1L)
  expect_equal(custom$Forecast, mean(raw$value))
  expect_identical(validations, 2L)
})

test_that("installed hierarchy authoring resumes real validation and produces a mixed forecast", {
  invisible(NULL)
  namespace <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace, "Meta", "package.rds")), "Actual hierarchy authoring children are verified from the installed namespace")
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  raw <- expand.grid(
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 24), Region = c("PRIVATE_NORTH", "PRIVATE_SOUTH"),
    Product = c("A", "B"), stringsAsFactors = FALSE
  )
  raw$Product <- paste(raw$Region, raw$Product, sep = "_")
  raw$value <- 100 + 10 * match(raw$Product, unique(raw$Product))
  chat <- ellmer::chat_openai(model = "offline-fixture", credentials = function() "synthetic-not-a-credential")
  chat$set_system_prompt("Unchanged hierarchy template")
  payloads <- list()
  local_mocked_bindings(custom_author_request = function(session, stage, payload) {
    payloads[[length(payloads) + 1L]] <<- payload
    if (stage == "contract")
      author_proposal()
    else author_source()
  }, .package = "finnts")
  prepared_info <- set_run_info(
    project_name = "installed-authoring-hierarchy", path = withr::local_tempdir(), data_output = "csv",
    object_output = "rds", add_unique_id = FALSE
  )
  prep_data(prepared_info, raw, c("Region", "Product"), "value", "month", 1,
    forecast_approach = "standard_hierarchy",
    recipes_to_run = "R1", stationary = FALSE, box_cox = FALSE, clean_missing_values = FALSE, clean_outliers = FALSE,
    multistep_horizon = FALSE
  )
  files <- list.files(prepared_info$path, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(files)
  worker <- function(pointer, response, namespace, final = FALSE) {
    loadNamespace("finnts")
    stopifnot(identical(normalizePath(getNamespaceInfo(asNamespace("finnts"), "path")), normalizePath(namespace)))
    testthat::local_mocked_bindings(
      custom_author_request = function(...) stop("Unexpected provider request"), custom_author_data = function(...) stop("Unexpected repreparation"),
      .package = "finnts"
    )
    if (final)
      testthat::local_mocked_bindings(custom_author_validate = function(...) stop("Unexpected revalidation"), .package = "finnts")
    finnts::create_custom_model(draft = pointer, responses = response)
  }
  environment(worker) <- baseenv()
  versions <- character()
  for (input in c("raw", "prepared")) {
    arguments <- list(
      instructions = "Use each node's historical mean, then reconcile.", llm = chat, forecast_approach = "standard_hierarchy",
      validation_examples = author_examples(), approval = "manual", draft_path = withr::local_tempdir(), validation_timeout = 120
    )
    if (input == "raw")
      arguments <- c(arguments, list(
        input_data = raw, combo_variables = c("Region", "Product"), target_variable = "value",
        date_type = "month", forecast_horizon = 1
      ))
    else arguments$run_info <- prepared_info
    draft <- do.call(create_custom_model, arguments)
    source <- draft
    pointer <- file.path(source$store, "current.rds")
    tested <- callr::r(worker, args = list(pointer = pointer, response = list(
      request_id = source$pending$request_id,
      answer = "yes"
    ), namespace = namespace), libpath = .libPaths(), system_profile = FALSE, user_profile = FALSE)
    expect_s3_class(tested, "finnts_custom_model")
    completed <- custom_draft_load(pointer)$state
    expect_identical(completed$stage, "approved")
    expect_length(completed$report$holdouts, completed$metadata$hierarchy$node_count)
    expect_match(completed$checks$hierarchy, "independent node holdouts")
    model <- callr::r(worker, args = list(pointer = pointer, response = list(
      request_id = source$pending$request_id,
      answer = "yes"
    ), namespace = namespace, final = TRUE), libpath = .libPaths(), system_profile = FALSE, user_profile = FALSE)
    expect_s3_class(model, "finnts_custom_model")
    expect_identical(model$definition$version_id, source$definition$version_id)
    versions <- c(versions, model$definition$version_id)
  }
  expect_length(unique(versions), 1L)
  expect_identical(tools::md5sum(files), before)
  expect_false(grepl("PRIVATE_NORTH|PRIVATE_SOUTH", jsonlite::toJSON(payloads)))
  expect_equal(chat$get_system_prompt(), "Unchanged hierarchy template")
  expect_length(chat$get_turns(), 0L)
  info <- set_run_info(project_name = "authored-hierarchy-forecast", path = withr::local_tempdir(), add_unique_id = FALSE)
  arguments <- list(
    run_info = info, input_data = raw, combo_variables = c("Region", "Product"), target_variable = "value",
    date_type = "month", forecast_horizon = 1, forecast_approach = "standard_hierarchy", custom_models = list(human_mean = model),
    models_to_run = c("human_mean", "meanf"), recipes_to_run = "R1", stationary = FALSE, clean_missing_values = FALSE,
    clean_outliers = FALSE, run_global_models = FALSE, run_ensemble_models = FALSE, negative_forecast = TRUE, average_models = FALSE,
    weekly_to_daily = FALSE, back_test_scenarios = 1, return_data = FALSE
  )
  do.call(forecast_time_series, arguments)
  forecasts <- get_forecast_data(info)
  future <- forecasts[forecasts$Train_Test_ID == 1 & forecasts$Best_Model == "Yes", ]
  expect_equal(sort(unique(future$Forecast)), sort(unique(raw$value)), tolerance = 1e-07)
  arguments$models_to_run <- "human_mean"
  expect_error(do.call(forecast_time_series, arguments), "mixed.*bottoms_up")
})

# Install current offline provider boundaries for lifecycle tests. Captures requests and
# consent events; measured report is synthetic and never used as live evidence.
# Real same-session validation is covered by the end-to-end tests above.
# Optional metadata permits alternate horizons without preparing new run artifacts.
author_harness <- function(replies = list(author_proposal(), author_source()), .env = parent.frame(), metadata = list(
                             date_type = "month",
                             forecast_horizon = 1
                           )) {
  tracker <- new.env(parent = emptyenv())
  tracker$requests <- list()
  tracker$validations <- 0L
  tracker$stages <- character()
  tracker$reports <- list()
  testthat::local_mocked_bindings(
    check_agent_ellmer_version = function() invisible(NULL), new_llm_session = function(llm) list(),
    custom_author_data = function(...) list(data = data.frame(
      Date = seq(as.Date("2020-01-01"), by = "month", length.out = 4),
      Combo = "PRIVATE_AUTHOR_SERIES", Target = 100
    ), metadata = metadata), custom_author_request = function(session,
                                                              stage, payload) {
      tracker$requests[[length(tracker$requests) + 1L]] <- list(stage = stage, payload = payload)
      replies[[length(tracker$requests)]]
    }, custom_author_validate = function(definition, examples, data, timeout, package_policy, contract) {
      tracker$validations <- tracker$validations + 1L
      if (length(tracker$reports) >= tracker$validations)
        return(tracker$reports[[tracker$validations]])
      list(
        passed = TRUE, code = "passed", version_id = definition$version_id, examples_id = digest::digest(examples,
          algo = "sha256", serializeVersion = 2
        ), examples = lapply(examples, function(example) list(
          name = example$name,
          model_type = example$model_type, maximum_error = 0, tolerance = example$tolerance
        )), holdouts = list(list(
          model_type = "local",
          rows = 1L, training_rows = 2L, cutoff = "2020-02-01", mae = 1, wmape = 0.01
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

test_that("typed contract failures retain exact bounded proposal evidence", {
  proposal <- list(
    questions = character(), name = "typed_mean", interpretation = "Use the historical mean.", parameters_json = "{\"lag\":",
    defaults_used = character(), parameter_schema = list(), error_policy = list(), validation_properties = character(),
    history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer())
  )
  tracker <- author_harness(list(proposal, proposal))
  error <- tryCatch(create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = author_examples(),
    approval = "automatic", control = custom_model_control(max_attempts = 2)
  ), error = identity)
  expect_identical(error$code, "invalid_parameters_json")
  expect_identical(tracker$requests[[2]]$payload$feedback$field, "parameters_json")
  expect_identical(error$diagnostics$rejected_contract$proposal$parameters_json, proposal$parameters_json)
  expect_identical(error$diagnostics$rejected_contract$field, "parameters_json")
  expect_identical(error$diagnostics$rejected_contract$code, "invalid_parameters_json")
  expect_identical(error$diagnostics$rejected_contract$proposal_id, digest::digest(error$diagnostics$rejected_contract$proposal,
    algo = "sha256", serializeVersion = 2
  ))
  expect_identical(tracker$validations, 0L)
  expect_length(tracker$requests, 2)
})

test_that("unexpected typed contract validator failures are not LLM repairs", {
  proposal <- list(
    questions = character(), name = "typed_mean", interpretation = "Use the historical mean.", parameters_json = "{}",
    defaults_used = character(), parameter_schema = list(), error_policy = list(), validation_properties = character(),
    history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer())
  )
  tracker <- author_harness(list(proposal, proposal, proposal))
  local_mocked_bindings(custom_author_contract = function(...) stop("PRIVATE_IMPLEMENTATION_FAILURE"), .package = "finnts")
  error <- tryCatch(create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = author_examples(),
    approval = "automatic"
  ), error = identity)
  expect_identical(error$code, "contract_validation_error")
  expect_identical(error$stage, "environment")
  expect_length(tracker$requests, 1)
  expect_false(grepl("PRIVATE_IMPLEMENTATION_FAILURE", conditionMessage(error), fixed = TRUE))
})

test_that("dependency repairs use loaded eligible export locations without widening policy", {
  references <- list(list(namespace = "base", member = "setNames", access = "::", reason = "unexported_member"))
  dependency <- custom_author_dependency(references, c("base", "stats"))
  before <- loadedNamespaces()
  candidates <- custom_author_dependency_candidates(dependency, c("base", "stats"))
  expect_identical(candidates, list(list(member = "setNames", exported_by = "stats", omitted_packages = 0L)))
  expect_setequal(loadedNamespaces(), before)
  expect_identical(custom_author_dependency_candidates(dependency, "base")[[1]]$exported_by, character())
  expect_identical(custom_author_dependency_candidates(NULL, "base"), list())
})

test_that("automatic clarification retains bounded questions before stopping", {
  proposal <- list(
    questions = list(list(kind = "business", topic = "business_context", question = "Which fiscal calendar should the rule use?")),
    name = "history_rule", interpretation = "", parameters_json = "{}", history_requirements = list(), defaults_used = character(),
    parameter_schema = list(), error_policy = list(), validation_properties = character()
  )
  tracker <- author_harness(list(proposal))
  error <- tryCatch(create_custom_model("Use the fiscal seasonal value.", list(),
    input_data = data.frame(), validation_examples = author_examples(),
    approval = "automatic"
  ), error = identity)
  expect_identical(error$code, "business_clarification")
  expect_match(conditionMessage(error), proposal$questions[[1]]$question, fixed = TRUE)
  expect_match(conditionMessage(error), "Provider clarification", fixed = TRUE)
  expect_identical(error$diagnostics$rejected_contract$proposal$questions, proposal$questions)
  expect_identical(error$diagnostics$rejected_contract$field, "questions")
  expect_length(tracker$requests, 1L)
  expect_identical(tracker$validations, 0L)
})

test_that("clarification error text is bounded without changing retained questions", {
  questions <- list("Which calendar?\r\nPlease specify.\t", paste(rep("Long question", 600), collapse = " "))
  questions <- lapply(questions, function(question) list(kind = "business", topic = "business_context", question = question))
  proposal <- list(
    questions = questions, name = "history_rule", interpretation = "PRIVATE_NONQUESTION_VALUE", parameters_json = "{}",
    history_requirements = list(), defaults_used = character(), parameter_schema = list(), error_policy = list(), validation_properties = character()
  )
  tracker <- author_harness(list(proposal))
  error <- tryCatch(create_custom_model("Use a fiscal rule.", list(),
    input_data = data.frame(), validation_examples = author_examples(),
    approval = "automatic"
  ), error = identity)
  message <- conditionMessage(error)
  expect_match(message, "Which calendar?", fixed = TRUE)
  expect_match(message, "Long question", fixed = TRUE)
  expect_match(message, "truncated", fixed = TRUE)
  expect_false(grepl("[\r\t]", message))
  expect_false(grepl("PRIVATE_NONQUESTION_VALUE", message, fixed = TRUE))
  expect_lt(nchar(message), 1600L)
  expect_identical(error$stage, "clarification")
  expect_identical(error$code, "business_clarification")
  expect_identical(error$diagnostics$rejected_contract$proposal$questions, questions)
  expect_length(tracker$requests, 1L)
  expect_identical(tracker$validations, 0L)
})

test_that("history repair receives structural counts without changing the rejected oracle", {
  examples <- lapply(c("local_basic", "local_edge"), function(id) list(
    id = id, rationale = "A constant one hundred stays constant.",
    series = list(list(combo = "example_a", history = list(Target = rep(100, 7)), future = list(), expected = 100)),
    outcome = "predictions", error_phase = "none", error_code = ""
  ))
  proposal <- list(
    questions = character(), name = "history_rule", interpretation = "Use the prior annual value.", parameters_json = "{}",
    history_requirements = list(minimum_rows_per_series = 48L, required_lag_periods = c(12L, 24L, 36L, 48L)), examples = examples,
    defaults_used = character(), parameter_schema = list(), error_policy = list(), validation_properties = character()
  )
  original <- serialize(proposal, NULL)
  tracker <- author_harness(list(proposal, proposal))
  error <- tryCatch(create_custom_model("Use the prior annual value.", list(),
    input_data = data.frame(), approval = "automatic",
    control = custom_model_control(max_attempts = 2L)
  ), error = identity)
  facts <- tracker$requests[[2]]$payload$feedback$history
  expect_identical(facts$minimum_history_rows, 48L)
  expect_identical(facts$required_lag_periods, c(12L, 24L, 36L, 48L))
  expect_identical(facts$forecast_horizon, 1L)
  expect_identical(facts$observed_history_possible, TRUE)
  expect_identical(facts$examples[[1]]$history_rows, 7L)
  expect_identical(facts$examples[[2]]$history_rows, 7L)
  expect_identical(error$code, "insufficient_example_history")
  expect_identical(error$diagnostics$rejected_contract$proposal, proposal[setdiff(names(proposal), "defaults_used")])
  expect_identical(serialize(proposal, NULL), original)
  expect_identical(tracker$validations, 0L)
  protocol <- custom_author_protocol(
    list(date_type = "month", forecast_horizon = 3), "local", character(), "base_only",
    FALSE
  )
  changed <- proposal
  changed$history_requirements$required_lag_periods <- c(1L, 48L)
  changed$history_requirements$minimum_rows_per_series <- 1L
  changed$examples[[1]]$series[[1]]$history$Target <- rep("PRIVATE_SENTINEL", 7)
  facts <- custom_author_history_feedback(changed, protocol)
  expect_false(facts$observed_history_possible)
  expect_identical(facts$lag_reference, "forecast_date")
  expect_identical(facts$unobserved_lag_periods, 1L)
  expect_identical(facts$minimum_history_rows, 48L)
  expect_false(grepl("PRIVATE_SENTINEL", jsonlite::toJSON(facts), fixed = TRUE))
  expect_null(custom_author_history_feedback(list(), protocol))
  changed$history_requirements <- list(minimum_rows_per_series = 6L, required_lag_periods = as.list(1:6))
  facts <- custom_author_history_feedback(changed, protocol)
  expect_identical(facts$unobserved_lag_periods, 1:2)
  expect_match(facts$guidance, "Adding older history cannot", fixed = TRUE)
  expect_match(facts$guidance, "minimum_rows_per_series", fixed = TRUE)
  expect_match(facts$guidance, "required_lag_periods = []", fixed = TRUE)
})

test_that("protocol-four exhaustion and caller examples cannot bypass history or binding gates", {
  proposal <- list(
    questions = character(), name = "history_rule", interpretation = "Use the explicit historical rule.",
    parameters_json = "{}", history_requirements = list(minimum_rows_per_series = 48L, required_lag_periods = c(
      12L,
      24L, 36L, 48L
    )), defaults_used = character(), parameter_schema = list(), error_policy = list(), validation_properties = character()
  )
  examples <- author_examples()
  original <- serialize(examples, NULL)
  tracker <- author_harness(rep(list(proposal), 3L))
  error <- tryCatch(create_custom_model("Use the explicit historical rule.", list(),
    input_data = data.frame(), validation_examples = examples,
    approval = "automatic"
  ), error = identity)
  expect_identical(error$code, "insufficient_example_history")
  expect_match(conditionMessage(error), "history", fixed = TRUE)
  expect_identical(serialize(examples, NULL), original)
  expect_length(tracker$requests, 3)
  expect_identical(tracker$validations, 0L)
})

test_that("protocol-four binding exhaustion preserves the attempt budget", {
  examples <- author_examples()
  proposal <- list(
    questions = character(), name = "history_rule", interpretation = "Use the explicit historical rule.",
    parameters_json = "{}", history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer()),
    defaults_used = character(), parameter_schema = list(), error_policy = list(), validation_properties = character()
  )
  bad <- list(
    fit_body = "object$history <- data; object", predict_body = "rep(0, nrow(new_data))", helpers = list(), status = "candidate",
    conflict = ""
  )
  tracker <- author_harness(list(proposal, bad))
  error <- tryCatch(create_custom_model("Use the explicit historical rule.", list(),
    input_data = data.frame(), validation_examples = examples,
    approval = "automatic", control = custom_model_control(max_attempts = 1)
  ), error = identity)
  expect_identical(error$code, "source_binding")
  expect_identical(custom_model_feedback(error)$status, "failed")
  expect_identical(custom_model_feedback(error)$attempts$remaining$source, 0L)
  expect_identical(custom_model_feedback(error)$version_id, tail(error$diagnostics$history, 1)[[1]]$failure$version_id)
  expect_identical(custom_model_feedback(error)$details$bindings$references[[1]]$symbol, "object")
  expect_match(conditionMessage(error), "fit: unbound object", fixed = TRUE)
  expect_identical(tail(error$diagnostics$history, 1)[[1]]$failure$bindings$references[[1]]$symbol, "object")
  expect_length(tracker$requests, 2)
  expect_identical(tracker$validations, 0L)
})

test_that("function-format rejection preserves source retry limits and safe evidence", {
  for (limit in c(1L, 3L)) local({
    proposal <- list(
      questions = character(), name = "full_mean", interpretation = "Use the historical mean.", parameters_json = "{}",
      history_requirements = list(minimum_rows_per_series = 2L, required_lag_periods = integer()), defaults_used = character(),
      parameter_schema = list(), error_policy = list(), validation_properties = character()
    )
    bad <- list(
      fit_body = "function(data, context, parameters, extra) mean(data$Target)", predict_body = "function(object, new_data, context) rep(object, nrow(new_data))",
      helpers = list(), status = "candidate", conflict = ""
    )
    tracker <- author_harness(c(list(proposal), rep(list(bad), limit)))
    error <- tryCatch(create_custom_model("Use mean.", list(),
      input_data = data.frame(), validation_examples = author_examples(),
      approval = "automatic", control = custom_model_control(max_attempts = limit)
    ), error = identity)
    expect_identical(error$code, "source_body_format")
    expect_match(conditionMessage(error), "exact required fit/predict signature", fixed = TRUE)
    expect_length(tracker$requests, 1L + limit)
    expect_identical(tracker$validations, 0L)
    expect_identical(tail(error$diagnostics$history, 1)[[1]]$failure$reason, "source_body_format")
    if (limit > 1L)
      expect_identical(tracker$requests[[3]]$payload$failure$reason, "source_body_format")
    expect_true(error$diagnostics$omitted)
  })
})

test_that("automatic protocol repairs use exact dependency evidence without changing examples", {
  proposal <- list(
    questions = character(), name = "human_mean", interpretation = "Use the historical mean.", parameters_json = "{}",
    defaults_used = character(), parameter_schema = list(), error_policy = list(), validation_properties = character(),
    history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer())
  )
  bad <- list(
    fit_body = "foreignpkg::calculate(data$Target)", predict_body = "rep(object, nrow(new_data))", helpers = list(),
    status = "candidate", conflict = ""
  )
  good <- bad
  good$fit_body <- "mean(data$Target)"
  tracker <- author_harness(list(proposal, bad, good))
  result <- create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = author_examples(),
    approval = "automatic"
  )
  expect_s3_class(result, "finnts_custom_model")
  expect_false(result$approval$intent_confirmed)
  expect_length(tracker$requests, 3)
  repair <- tracker$requests[[3]]$payload
  expect_identical(repair$failure$dependency$references[[1]]$namespace, "foreignpkg")
  expect_identical(repair$failure$dependency$references[[1]]$reason, "undeclared_package")
  expect_match(repair$previous_source[["fit"]], "foreignpkg", fixed = TRUE)
  expect_identical(repair$contract$examples, author_examples())
  expect_identical(result$definition$packages, character())
  expect_identical(tracker$validations, 1L)
  expect_identical(custom_author_contract_fields(repair$protocol, "known_name", "local", TRUE), c(
    "questions", "interpretation",
    "parameters_json", "history_requirements", "parameter_schema", "error_policy", "validation_properties", "defaults_used"
  ))
})

test_that("new protocol dependency exhaustion retains exact bounded evidence", {
  proposal <- list(
    questions = character(), name = "human_mean", interpretation = "Use the historical mean.", parameters_json = "{}",
    defaults_used = character(), parameter_schema = list(), error_policy = list(), validation_properties = character(),
    history_requirements = list(minimum_rows_per_series = 1L, required_lag_periods = integer())
  )
  source <- list(
    fit_body = "foreignpkg::calculate(data$Target)", predict_body = "rep(object, nrow(new_data))", helpers = list(),
    status = "candidate", conflict = ""
  )
  tracker <- author_harness(list(proposal, source))
  error <- tryCatch(create_custom_model("Use mean.", list(),
    input_data = data.frame(), validation_examples = author_examples(),
    approval = "automatic", control = custom_model_control(max_attempts = 1)
  ), error = identity)
  expect_s3_class(error, "finnts_custom_model_authoring_error")
  expect_identical(error$code, "source_dependency")
  expect_match(conditionMessage(error), "foreignpkg::calculate (undeclared_package)", fixed = TRUE)
  expect_length(tracker$requests, 2)
  expect_identical(tracker$validations, 0L)
  expect_identical(error$diagnostics$snapshot$contract$examples, author_examples())
  expect_identical(tail(error$diagnostics$history, 1)[[1]]$failure$dependency$references[[1]]$namespace, "foreignpkg")
})

test_that("mode exhaustion and caller failures preserve intent without silent repairs", {
  metadata <- list(date_type = "month", forecast_horizon = 3)
  invalid <- author_seasonal_proposal(extra_global = TRUE)
  for (limit in c(1L, 3L)) {
    tracker <- author_harness(rep(list(invalid), limit), metadata = metadata)
    error <- tryCatch(create_custom_model("Use prior year times 1.11.", list(),
      input_data = data.frame(), approval = "automatic",
      control = custom_model_control(max_attempts = limit)
    ), error = identity)
    expect_s3_class(error, "finnts_custom_model_authoring_error")
    expect_identical(error$code, "invalid_examples_shape")
    expect_identical(error$stage, "clarification")
    expect_match(conditionMessage(error), "Last contract rejection:.*scenario records")
    expect_false(grepl("global_basic", conditionMessage(error), fixed = TRUE))
    expect_length(tracker$requests, limit)
    expect_identical(tracker$validations, 0L)
    expect_true(all(vapply(tracker$requests, function(request) request$stage == "contract", logical(1))))
    if (limit > 1L)
      expect_identical(tracker$requests[[2]]$payload$feedback$code, "invalid_examples_shape")
  }
  examples <- author_seasonal_examples()
  extra <- examples[[1]]
  extra$name <- "global_basic"
  extra$model_type <- "global"
  extra$history <- rbind(extra$history, transform(extra$history, Combo = "other"))
  extra$new_data <- rbind(extra$new_data, transform(extra$new_data, Combo = "other"))
  extra$expected <- rbind(extra$expected, transform(extra$expected, Combo = "other"))
  examples <- list(examples[[1]], extra, examples[[2]])
  before <- examples
  proposal <- author_seasonal_proposal()
  proposal$examples <- NULL
  tracker <- author_harness(list(proposal), metadata = metadata)
  error <- tryCatch(create_custom_model("Use prior year times 1.11.", list(),
    input_data = data.frame(), approval = "automatic",
    validation_examples = examples
  ), error = identity)
  expect_identical(error$code, "example_mode")
  expect_identical(error$stage, "examples")
  expect_match(conditionMessage(error), "Caller-provided validation_examples failed", fixed = TRUE)
  expect_identical(examples, before)
  expect_length(tracker$requests, 1L)
  expect_identical(tracker$validations, 0L)
  expect_match(custom_author_contract_feedback("example_mode")$message, "Do not enable another mode")
  expect_identical(custom_author_contract_feedback("example_mode_PRIVATE"), custom_author_contract_feedback("invalid_contract"))
})

test_that("source exhaustion exposes bounded reusable failure diagnostics", {
  broken <- author_source()
  broken$fit_body <- "function(x, context, parameters) NULL"
  tracker <- author_harness(c(list(c(author_proposal(), list(defaults_used = character()))), rep(list(broken), 3)))
  error <- tryCatch(create_custom_model("Use mean.", list(), input_data = data.frame(), approval = "automatic", validation_examples = author_examples()),
    error = identity
  )
  expect_s3_class(error, "finnts_custom_model_authoring_error")
  expect_identical(error$code, "source_body_format")
  expect_match(conditionMessage(error), "Last failure: source_signature")
  expect_length(error$diagnostics$history, 4L)
  expect_null(error$diagnostics$snapshot$source)
  expect_true(error$diagnostics$omitted)
  expect_lte(custom_draft_diagnostic_size(error$diagnostics), 1048576L)
  expect_identical(tracker$validations, 0L)
  expect_length(tracker$requests, 4L)
  expect_identical(tracker$requests[[3]]$payload$failure$reason, "source_body_format")
  expect_null(tracker$requests[[3]]$payload$previous_source)
})

test_that("contract exhaustion retains safe horizon feedback and exact attempt limits", {
  for (limit in c(1L, 3L)) {
    examples <- author_examples()
    examples[[1]]$new_data$Date <- as.Date("2021-03-01")
    examples[[1]]$expected$Date <- examples[[1]]$new_data$Date
    invalid <- c(author_proposal(), list(defaults_used = character()))
    invalid$examples <- author_native_examples(examples)
    invalid$examples[[1]]$series[[1]]$expected <- c(15, 15)
    tracker <- author_harness(rep(list(invalid), limit))
    error <- tryCatch(create_custom_model("Use mean.", list(), input_data = data.frame(), approval = "automatic", control = custom_model_control(max_attempts = limit)),
      error = identity
    )
    expect_s3_class(error, "finnts_custom_model_authoring_error")
    expect_identical(error$stage, "clarification")
    expect_identical(error$code, "invalid_examples_shape")
    expect_match(conditionMessage(error), "No candidate passed required checks within max_attempts", fixed = TRUE)
    expect_match(conditionMessage(error), "Last contract rejection:.*exact value-array lengths")
    expect_length(tracker$requests, limit)
    expect_true(all(vapply(tracker$requests, function(request) request$stage == "contract", logical(1))))
    expect_identical(tracker$validations, 0L)
    if (limit > 1L)
      expect_identical(tracker$requests[[2]]$payload$feedback$code, "invalid_examples_shape")
  }
})

test_that("invalid caller examples fail without replacement or further proposals", {
  for (defect in c("horizon", "tolerance")) {
    examples <- author_examples()
    if (defect == "horizon") {
      examples[[1]]$new_data$Date <- as.Date("2021-03-01")
      examples[[1]]$expected$Date <- examples[[1]]$new_data$Date
    } else examples[[1]]$tolerance <- 1
    before <- examples
    valid <- c(author_proposal(), list(defaults_used = character()))
    invisible(NULL)
    tracker <- author_harness(list(valid))
    error <- tryCatch(create_custom_model("Use mean.", list(), input_data = data.frame(), approval = "automatic", validation_examples = examples),
      error = identity
    )
    expect_s3_class(error, "finnts_custom_model_authoring_error")
    expect_identical(error$stage, "examples")
    expect_identical(error$code, if (defect == "horizon")
      "example_horizon"
    else "invalid_examples")
    expect_match(conditionMessage(error), "Caller-provided validation_examples failed", fixed = TRUE)
    expect_match(conditionMessage(error), "they were not changed", fixed = TRUE)
    expect_identical(examples, before)
    expect_length(tracker$requests, 1L)
    expect_identical(tracker$validations, 0L)
  }
})

test_that("contract feedback never exposes arbitrary exception text or unknown codes", {
  valid <- c(author_proposal(), list(defaults_used = character()))
  tracker <- author_harness(rep(list(valid), 3))
  local_mocked_bindings(custom_author_contract = function(...) stop("PRIVATE_ERROR_VALUE_SECRET_PATH"), .package = "finnts")
  error <- tryCatch(create_custom_model("Use mean.", list(), input_data = data.frame(), approval = "automatic", validation_examples = author_examples()),
    error = identity
  )
  expect_identical(error$code, "contract_validation_error")
  expect_length(tracker$requests, 1L)
  expect_identical(tracker$validations, 0L)
  feedback <- error$diagnostics$history[[1]]$failure
  expect_identical(feedback$reason, "contract_validation_error")
  expect_false(grepl("PRIVATE_ERROR_VALUE_SECRET_PATH", paste(conditionMessage(error), jsonlite::toJSON(feedback)), fixed = TRUE))
  for (code in list(NULL, character(), NA_character_, c("example_horizon", "PRIVATE"), "PRIVATE")) {
    expect_identical(custom_author_contract_feedback(code), custom_author_contract_feedback("invalid_contract"))
  }
})

test_that("structured contract feedback is separate from source repair diagnostics", {
  valid <- c(author_proposal(), list(defaults_used = character()))
  malformed <- valid
  malformed$unexpected <- "PRIVATE_RESPONSE_SENTINEL"
  tracker <- author_harness(list(malformed, valid, author_source()))
  model <- create_custom_model("Use mean.", list(), input_data = data.frame(), approval = "automatic", validation_examples = author_examples())
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(tracker$requests[[2]]$payload$feedback$code, "invalid_contract_response")
  expect_false(grepl("PRIVATE_RESPONSE_SENTINEL", jsonlite::toJSON(tracker$requests), fixed = TRUE))
  expect_null(tracker$requests[[3]]$payload$feedback)
  expect_identical(tracker$requests[[3]]$payload$diagnostic, "initial")
})

test_that("contract retry feedback cannot consume provider or capability failures", {
  tracker <- author_harness()
  local({
    calls <- 0L
    local_mocked_bindings(custom_author_request = function(...) {
      calls <<- calls + 1L
      custom_author_abort("provider", "Structured provider request failed; no candidate was executed.")
    }, .package = "finnts")
    expect_error(create_custom_model("Use mean.", list(), input_data = data.frame(), approval = "automatic"), "provider")
    expect_identical(calls, 1L)
  })
  for (field in c("model_type", "predictors")) {
    invalid <- c(author_proposal(), list(defaults_used = character()))
    invalid[[field]] <- if (field == "model_type")
      "global"
    else "InventedDriver"
    tracker <- author_harness(list(invalid))
    expect_error(create_custom_model("Use mean.", list(),
      input_data = data.frame(), approval = "automatic", validation_examples = author_examples(),
      control = custom_model_control(max_attempts = 1L)
    ), "invalid fields")
    expect_length(tracker$requests, 1L)
    expect_identical(tracker$validations, 0L)
  }
})

test_that("generated automatic examples use fixed tolerance before source generation", {
  proposal <- c(author_proposal(), list(defaults_used = c("history_window", "averaging_weights")))
  examples <- author_examples()
  examples[[1]]$tolerance <- 0.01
  proposal$examples <- author_native_examples(examples)
  tracker <- author_harness(list(proposal, author_source()))
  model <- create_custom_model("Use the mean.", list(), input_data = data.frame(), approval = "automatic")
  contract <- tracker$requests[[2]]$payload$contract
  expect_true(all(vapply(contract$examples, function(example) identical(example$tolerance, 1e-08), logical(1))))
  expect_identical(contract$examples[[1]]$expected$.pred, 15)
  expect_identical(contract$examples[[2]]$expected$.pred, 0)
  expect_true(contract$examples[[2]]$edge_case)
  expect_identical(tracker$validations, 1L)
  evidence <- jsonlite::fromJSON(model$validation$checks$automatic_defaults, simplifyVector = FALSE)
  expect_equal(evidence$catalog_version, 2)
  expect_identical(evidence$effective$model_type, "local")
  expect_identical(evidence$defaults_used$worked_examples, custom_author_defaults()$worked_examples)
  expect_false(grepl("PRIVATE_AUTHOR_SERIES|100", model$validation$checks$contract))
})

test_that("console review is concise and details do not execute or regenerate", {
  tracker <- author_harness()
  draft <- create_custom_model("PRIVATE_INSTRUCTION_SENTINEL", NULL,
    input_data = data.frame(), validation_examples = author_examples(),
    approval = "manual"
  )
  request <- draft$pending
  prompts <- character()
  answers <- "Y"
  local_mocked_bindings(readline = function(prompt = "") {
    prompts <<- c(prompts, prompt)
    answer <- answers[[1]]
    answers <<- answers[-1]
    answer
  }, .package = "base")
  output <- capture.output(answer <- custom_author_interact(
    NULL, request$stage, request$prompt, request$identity, request$review,
    request$questions
  ))
  expect_identical(answer, "yes")
  expect_length(prompts, 1L)
  expect_lt(length(output), 30L)
  expect_false(any(grepl("PRIVATE_INSTRUCTION_SENTINEL|available_packages|\\$contract|function\\(", output)))
  expect_false(any(grepl(draft$definition$version_id, output, fixed = TRUE)))
  expect_true(any(grepl("Examples?", output)))
  prompts <- character()
  answers <- c("invalid", "D", "e", "", "yes")
  detailed <- capture.output(answer <- custom_author_interact(
    NULL, request$stage, request$prompt, request$identity, request$review,
    request$questions
  ))
  expect_identical(answer, "yes")
  expect_length(prompts, 5L)
  expect_true(any(grepl("Choose y/yes", detailed, fixed = TRUE)))
  expect_true(grepl(draft$definition$source[["fit"]], paste(detailed, collapse = "\n"), fixed = TRUE))
  expect_true(any(grepl("PRIVATE_INSTRUCTION_SENTINEL", detailed, fixed = TRUE)))
  expect_identical(tracker$validations, 0L)
  expect_length(tracker$requests, 2L)
  answers <- ""
  expect_error(invisible(capture.output(custom_author_interact(
    NULL, request$stage, request$prompt, request$identity, request$review,
    request$questions
  ))), "declined")
  expect_identical(tracker$validations, 0L)
})

test_that("edits require new candidate consent without changing data capabilities", {
  tracker <- author_harness(list(author_proposal(), author_source(), author_proposal(), author_source()))
  draft <- create_custom_model("Use mean.", NULL, input_data = data.frame(), validation_examples = author_examples(), approval = "manual")
  edited <- create_custom_model(draft = draft, llm = list(), responses = list(request_id = draft$pending$request_id, answer = list(
    action = "EDIT",
    instructions = "Clarify that all history rows receive equal weight."
  )))
  expect_identical(edited$stage, "review_create")
  expect_false(identical(edited$definition$version_id, draft$definition$version_id))
  expect_identical(edited$contract$requirements, draft$contract$requirements)
  expect_null(edited$consent$test)
  expect_identical(tracker$validations, 0L)
  expect_length(tracker$requests, 4L)
  expect_error(
    create_custom_model(draft = edited, responses = list(request_id = draft$pending$request_id, answer = "yes")),
    "changed|Stale"
  )
  model <- create_custom_model(draft = edited, responses = list(request_id = edited$pending$request_id, answer = "yes"))
  expect_identical(model$definition$version_id, edited$definition$version_id)
  expect_identical(tracker$validations, 1L)
})

test_that("quiet preparation retains warnings and does not hide validation failures", {
  tracker <- author_harness()
  prepare <- custom_author_data
  local_mocked_bindings(custom_author_data = function(...) {
    cat("ROUTINE_PREPARATION_OUTPUT\n")
    message("ROUTINE_PREPARATION_MESSAGE")
    warning("Important preparation warning")
    prepare(...)
  }, .package = "finnts")
  expect_warning(output <- capture.output(draft <- create_custom_model("Use mean.", NULL,
    input_data = data.frame(), validation_examples = author_examples(),
    approval = "manual"
  )), "Important preparation warning")
  expect_false(any(grepl("ROUTINE_PREPARATION", output, fixed = TRUE)))
  expect_identical(draft$stage, "review_create")
  expect_identical(tracker$validations, 0L)
})

test_that("only explicit candidate approval can produce an enrolled envelope", {
  tracker <- author_harness()
  respond <- function(request) {
    tracker$stages <- c(tracker$stages, request$stage)
    "yes"
  }
  result <- create_custom_model("Use mean.", NULL,
    input_data = data.frame(), validation_examples = author_examples(),
    control = custom_model_control(interact = respond)
  )
  expect_s3_class(result, "finnts_custom_model")
  expect_identical(tracker$stages, "review_create")
  expect_identical(tracker$validations, 1L)
  expect_identical(result$definition$version_id, result$approval$version_id)
  expect_identical(result, unserialize(serialize(result, NULL)))
  expect_identical(names(result), c("schema_version", "definition", "validation", "approval"))
})

test_that("declined and stale replies never validate or approve source", {
  for (answer in c("", "no", "APPROVE CUSTOM MODEL wrong-version")) local({
    tracker <- author_harness()
    respond <- function(request) {
      answer
    }
    expect_error(create_custom_model("Use mean.", NULL,
      input_data = data.frame(), validation_examples = author_examples(),
      control = custom_model_control(interact = respond)
    ), "declined|Invalid response", class = "finnts_custom_model_authoring_error")
    expect_identical(tracker$validations, 0L)
  })
})

test_that("repair keeps the confirmed oracle and obtains fresh source consent", {
  broken <- author_source()
  broken$fit_body <- "function(data, context, parameters) 999"
  tracker <- author_harness(list(author_proposal(), broken, author_source()))
  tracker$reports <- list(list(passed = FALSE, code = "intent_mismatch"))
  ids <- character()
  reviews <- list()
  output <- list()
  respond <- function(request) {
    if (request$stage == "review_create") {
      ids <<- c(ids, request$identity)
      reviews[[length(reviews) + 1L]] <<- request$review
      output[[length(output) + 1L]] <<- capture.output(custom_author_review(request$review))
    }
    "yes"
  }
  result <- create_custom_model("Use mean.", NULL,
    input_data = data.frame(), validation_examples = author_examples(),
    control = custom_model_control(interact = respond)
  )
  expect_length(unique(ids), 2L)
  expect_identical(result$definition$version_id, ids[[2]])
  expect_identical(tracker$requests[[2]]$payload$contract, tracker$requests[[3]]$payload$contract)
  expect_identical(tracker$requests[[3]]$payload$diagnostic, "intent_mismatch")
  expect_null(reviews[[1]]$repair)
  expect_match(paste(output[[2]], collapse = " "), "Previous candidate failed validation", fixed = TRUE)
  expect_true(any(grepl("example_compare", output[[2]], fixed = TRUE)))
  expect_true(any(grepl(substr(ids[[1]], 1, 12), output[[2]], fixed = TRUE)))
  expect_true(any(grepl(substr(ids[[2]], 1, 12), output[[2]], fixed = TRUE)))
  expect_true(any(grepl("Source changes: fit", output[[2]], fixed = TRUE)))
  expect_true(any(grepl("Rule and worked examples: unchanged", output[[2]], fixed = TRUE)))
  expect_false(any(grepl("function\\(|PRIVATE_AUTHOR_SERIES", output[[2]])))
  expect_identical(tracker$validations, 2L)
})

test_that("malformed clarification cannot bypass strict proposal fields", {
  invalid <- author_proposal()
  invalid$questions <- "Which window?"
  invalid$approved <- TRUE
  tracker <- author_harness(list(invalid))
  expect_error(create_custom_model("Use mean.", NULL, input_data = data.frame(), control = custom_model_control(
    max_attempts = 1,
    interact = function(request) stop("Unexpected callback")
  )), "fields|contract|clarification")
  expect_identical(tracker$validations, 0L)
})

test_that("current global guidance and demonstrations genuinely pool series", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  metadata <- list(date_type = "month", forecast_horizon = 2, available_packages = "base", schema = list(Units = "numeric"))
  protocol <- custom_author_protocol(metadata, "global", "Units", "base_only", TRUE)
  expect_false("global_support" %in% names(protocol))
  legacy <- protocol
  legacy$global_support <- NULL
  expect_identical(validate_custom_author_protocol(legacy, metadata, "global", TRUE), legacy)
  expect_null(custom_author_protocol(metadata, "local", "Units", "base_only", TRUE)$global_support)
  payload <- list(protocol = protocol, model_type = "global", metadata = metadata, name = "pooled_rule")
  examples <- custom_author_prompt_examples("source", payload)
  for (example in examples) {
    execution <- example$execution
    source <- custom_author_assemble(example$response[c("fit_body", "predict_body", "helpers")])
    functions <- new.env(parent = baseenv())
    for (entry in source$source) assign(entry$name, eval(parse(text = entry$code), functions), functions)
    fitted <- functions$fit(execution$history, execution$context, execution$parameters)
    future <- execution$new_data
    future$.finnts_row <- seq_len(nrow(future))
    expect_equal(fitted, execution$fitted_state)
    expect_equal(functions$predict(fitted, future, execution$context)$.pred, execution$predictions)
    changed <- execution$history
    changed$Target[changed$Combo == tail(unique(changed$Combo), 1L)] <- 2 * changed$Target[changed$Combo == tail(
      unique(changed$Combo),
      1L
    )]
    peer_fit <- functions$fit(changed, execution$context, execution$parameters)
    selected <- future$Combo == unique(execution$history$Combo)[[1L]]
    expect_true(any(functions$predict(peer_fit, future, execution$context)$.pred[selected] != execution$predictions[selected]))
  }
  proposal <- list(
    status = "implementation_difficulty", conflict = "Need several helpers.", fit_body = "", predict_body = "",
    helpers = list()
  )
  error <- tryCatch(custom_author_source_response(proposal), error = identity)
  expect_identical(error$code, "implementation_difficulty")
  expect_error(custom_author_source_response(proposal), "Implementation difficulty")
  proposal$status <- "contract_conflict"
  expect_error(custom_author_source_response(proposal), "conflict in the fixed contract")
  prompt <- NULL
  session <- list(chat_structured = function(prompt, ...) {
    assign("prompt", prompt, parent.env(environment()))
    list()
  })
  custom_author_request(session, "source", payload)
  expect_match(prompt, "Only duplicate (Combo, Date) observations", fixed = TRUE)
  expect_match(prompt, "not restricted to tiny expressions", fixed = TRUE)
  expect_match(prompt, "business operations are allowed", fixed = TRUE)
})

test_that("provider failures retain only bounded typed diagnostics", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  cases <- list(
    httr2_http_429 = "rate_limit", httr2_http_401 = "authentication", httr2_http_503 = "server", httr2_failure = "transport",
    curl_error_operation_timedout = "timeout", simpleError = "unknown"
  )
  for (kind in names(cases)) {
    original <- structure(list(message = "PRIVATE_TOKEN and PRIVATE_RESPONSE", call = NULL, body = "PRIVATE_BODY", headers = list(Authorization = "PRIVATE_SECRET")),
      class = c(kind, "error", "condition")
    )
    calls <- 0L
    session <- list(chat_structured = function(...) {
      calls <<- calls + 1L
      stop(original)
    })
    error <- tryCatch(custom_author_request(session, "contract", list(instructions = "Use mean.", metadata = list(forecast_approach = "bottoms_up"))),
      error = identity
    )
    expect_s3_class(error, "finnts_custom_model_authoring_error")
    expect_identical(error$code, "provider_failure")
    expect_identical(error$provider$kind, cases[[kind]])
    expect_identical(error$provider$phase, "contract")
    expect_true(error$provider$prompt_bytes > 0)
    expect_gte(error$provider$elapsed_seconds, 0)
    expect_identical(calls, 1L)
    expect_false(grepl("PRIVATE_", jsonlite::toJSON(unclass(error)), fixed = TRUE))
    feedback <- custom_model_feedback(error)
    expect_identical(feedback$stage, "provider")
    expect_identical(feedback$next_action, "inspect_operation")
    expect_identical(feedback$details$provider, error$provider)
  }
})

test_that("structured authoring disables inherited tools before offline transport", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  current_protocol <- custom_author_protocol(list(date_type = "month", forecast_horizon = 1), "local", character(), "base_only", FALSE)
  chat <- ellmer::chat_openai(model = "offline-fixture", credentials = function() "synthetic-not-a-credential")
  called <- FALSE
  chat$register_tool(ellmer::tool(function() {
    called <<- TRUE
    "never"
  }, name = "must_not_execute", description = "Test sentinel", arguments = list()))
  chat$set_system_prompt("private template prompt")
  session <- new_llm_session(chat)
  request <- NULL
  original <- get("chat_request", envir = asNamespace("ellmer"))
  testthat::local_mocked_bindings(chat_perform = function(provider, mode, turns, tools = NULL, type = NULL, ...) {
    request <<- original(provider = provider, stream = FALSE, turns = turns, tools = if (is.null(tools))
      list()
    else tools, type = type)
    stop("offline transport intercepted")
  }, .package = "ellmer")
  expect_error(
    custom_author_request(session, "contract", list(instructions = "Use mean.", protocol = current_protocol, metadata = list(forecast_approach = "bottoms_up"))),
    "provider"
  )
  expect_false(is.null(request))
  expect_false(called)
  expect_null(request$body$data$tools)
  expect_equal(chat$get_system_prompt(), "private template prompt")
  expect_length(chat$get_tools(), 1L)
  expect_false(grepl("every prepared node", jsonlite::toJSON(request$body), fixed = TRUE))
  expect_error(
    custom_author_request(session, "contract", list(instructions = "Use each node's mean.", protocol = current_protocol, metadata = list(forecast_approach = "standard_hierarchy"))),
    "provider"
  )
  expect_true(grepl("every prepared node", jsonlite::toJSON(request$body), fixed = TRUE))
  automatic_policy <- custom_author_automatic_policy(
    list(metadata = list(date_type = "month", forecast_horizon = 1)),
    NULL, NULL, "bottoms_up", NULL, NULL, 3L, 60
  )
  expect_error(custom_author_request(session, "contract", list(
    instructions = "Use mean.", approval_mode = "automatic",
    automatic_policy = automatic_policy, protocol = current_protocol, metadata = list(forecast_approach = "bottoms_up")
  )), "provider")
  automatic <- jsonlite::toJSON(request$body)
  expect_true(grepl("defaults_used", automatic, fixed = TRUE))
  expect_true(grepl("1e-8", automatic, fixed = TRUE))
  expect_true(grepl("consecutive forecast dates immediately after cutoff", automatic, fixed = TRUE))
  expect_true(grepl("Seasonal lookup dates are in history", automatic, fixed = TRUE))
  expect_true(grepl("Only unaccepted generated examples may be replaced", automatic, fixed = TRUE))
  expect_true(grepl("small integers, simple ratios and exact finite-decimal results", automatic, fixed = TRUE))
  expect_true(grepl("Do not guess answers", automatic, fixed = TRUE))
  expect_false(grepl("Ask questions for every unresolved assumption", automatic, fixed = TRUE))
  expect_null(request$body$data$tools)
  legacy_policy <- automatic_policy
  legacy_policy$catalog_version <- 1L
  legacy_policy$catalog$growth_basis <- NULL
  protocol <- custom_author_protocol(list(date_type = "month", forecast_horizon = 1), "local", character(), "base_only", TRUE)
  expect_error(custom_author_request(session, "contract", list(
    instructions = "Use average seasonal growth.", name = "growth_test",
    model_type = "local", approval_mode = "automatic", automatic_policy = automatic_policy, protocol = protocol,
    metadata = list(forecast_approach = "bottoms_up")
  )), "provider")
  schema <- request$body$data$text$format$schema %||% request$body$data$response_format$json_schema$schema
  expect_true("growth_basis" %in% schema$properties$defaults_used$items$enum)
  prompt <- jsonlite::toJSON(request$body)
  expect_true(grepl("Default unspecified growth to percentage changes", prompt, fixed = TRUE))
  expect_true(grepl("Explicit absolute differences, percentage-point changes, CAGR", prompt, fixed = TRUE))
  expect_true(grepl("omit it for explicit growth bases and non-growth rules", prompt, fixed = TRUE))
  expect_true(grepl("Other unresolved or contradictory essential business choices still require questions", prompt, fixed = TRUE))
  for (invalid in list(NULL, list(catalog_version = 3L, catalog = automatic_policy$catalog), list(
    catalog_version = 1L,
    catalog = automatic_policy$catalog
  ), list(catalog_version = 2L, catalog = legacy_policy$catalog))) {
    request <- NULL
    expect_error(custom_author_request(session, "contract", list(approval_mode = "automatic", automatic_policy = invalid)),
      "catalog|policy",
      class = "finnts_custom_model_authoring_error"
    )
    expect_null(request)
  }
  expect_error(custom_author_request(session, "source", list(approval_mode = "automatic", protocol = current_protocol)), "provider")
  expect_true(grepl("not manually reviewed", jsonlite::toJSON(request$body), fixed = TRUE))
  expect_false(grepl("human will review", jsonlite::toJSON(request$body), fixed = TRUE))
  expect_true(grepl("Twelve monthly periods are one year", jsonlite::toJSON(request$body), fixed = TRUE))
  expect_true(grepl("previous_source and failure", jsonlite::toJSON(request$body), fixed = TRUE))
  expect_true(grepl("Preserve Date classes for lookup keys", jsonlite::toJSON(request$body), fixed = TRUE))
  expect_true(grepl("hist_end_date is not a permanent model cutoff", jsonlite::toJSON(request$body), fixed = TRUE))
  for (modes in list("local", "global", c("local", "global"))) {
    expect_error(custom_author_request(session, "contract", list(
      instructions = "Use the supplied rule.", model_type = modes,
      protocol = custom_author_protocol(list(date_type = "month", forecast_horizon = 1), modes, character(), "base_only", FALSE),
      metadata = list(forecast_approach = "bottoms_up")
    )), "provider")
    body <- jsonlite::toJSON(request$body)
    expect_true(grepl("exactly the captured horizon and declared modes", body, fixed = TRUE))
    expect_true(grepl("A local-only contract contains only local examples", body, fixed = TRUE))
    expect_true(grepl("Only when global is declared, global examples use at least two series", body, fixed = TRUE))
    expect_true(grepl("Even a single declared mode requires at least two examples", body, fixed = TRUE))
    expect_null(request$body$data$tools)
  }
  protocol <- custom_author_protocol(
    list(date_type = "month", forecast_horizon = 1), "local", character(), "base_only",
    TRUE
  )
  expect_error(
    custom_author_request(session, "contract", list(
      protocol = protocol, name = "fixed_name", model_type = "local",
      approval_mode = "automatic", automatic_policy = automatic_policy, metadata = list(forecast_approach = "bottoms_up")
    )),
    "provider"
  )
  schema <- request$body$data$text$format$schema %||% request$body$data$response_format$json_schema$schema
  expect_setequal(names(schema$properties), c("questions", "interpretation", "parameters_json", "history_requirements", "parameter_schema", "error_policy", "validation_properties", "defaults_used"))
  expect_null(request$body$data$tools)
  expect_error(custom_author_request(session, "source", list(protocol = protocol, approval_mode = "automatic")), "provider")
  schema <- request$body$data$text$format$schema %||% request$body$data$response_format$json_schema$schema
  expect_setequal(names(schema$properties), c("status", "conflict", "fit_body", "predict_body", "helpers"))
  expect_null(request$body$data$tools)
  expect_true(grepl("Preserve Date classes for lookup keys", jsonlite::toJSON(request$body), fixed = TRUE))
  expect_true(grepl("hist_end_date is not a permanent model cutoff", jsonlite::toJSON(request$body), fixed = TRUE))
  typed <- custom_author_protocol(list(date_type = "month", forecast_horizon = 3), "local", character(), "base_only", FALSE)
  expect_error(
    custom_author_request(session, "contract", list(
      protocol = typed, name = "typed_mean", model_type = "local",
      approval_mode = "automatic", automatic_policy = automatic_policy, metadata = list(forecast_approach = "bottoms_up")
    )),
    "provider"
  )
  schema <- request$body$data$text$format$schema %||% request$body$data$response_format$json_schema$schema
  expect_setequal(names(schema$properties), c("questions", "interpretation", "parameters_json", "history_requirements", "parameter_schema", "error_policy", "validation_properties", "examples", "defaults_used"))
  expect_identical(schema$properties$examples$type, "array")
  expect_identical(schema$properties$examples$items$properties$series$type, "array")
  expect_identical(
    schema$properties$examples$items$properties$series$items$properties$history$properties$Target$type,
    "array"
  )
  expect_null(request$body$data$tools)
  driver_metadata <- list(date_type = "month", forecast_horizon = 1, schema = list(
    amount = "numeric", enabled = "logical",
    segment = "factor", .description = "numeric"
  ))
  driver_protocol <- custom_author_protocol(driver_metadata, "local", names(driver_metadata$schema), "base_only", FALSE)
  expect_error(custom_author_request(session, "contract", list(
    protocol = driver_protocol, name = "typed_drivers", model_type = "local",
    metadata = driver_metadata
  )), "provider")
  schema <- request$body$data$text$format$schema %||% request$body$data$response_format$json_schema$schema
  properties <- schema$properties$examples$items$properties$series$items$properties$history$properties
  expect_setequal(names(properties), c("Target", names(driver_metadata$schema)))
  expect_identical(properties$amount$items$type, "number")
  expect_identical(properties$enabled$items$type, "boolean")
  expect_identical(properties$segment$items$type, "string")
  expect_identical(properties$.description$items$type, "number")
  expect_identical(schema$properties$history_requirements$type, "object")
  expect_identical(schema$properties$history_requirements$properties$minimum_rows_per_series$type, "integer")
  expect_identical(schema$properties$history_requirements$properties$required_lag_periods$type, "array")
  expect_true(grepl("fit cutoff, not each forecast date", jsonlite::toJSON(request$body), fixed = TRUE))
  expect_true(grepl("minimum_rows_per_series = 6 and required_lag_periods = []", jsonlite::toJSON(request$body), fixed = TRUE))
})

test_that("installed authoring returns a reusable model without exposing private data", {
  invisible(NULL)
  namespace_path <- getNamespaceInfo(asNamespace("finnts"), "path")
  skip_if_not(file.exists(file.path(namespace_path, "Meta", "package.rds")), "End-to-end authoring children are checked from the installed namespace")
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  chat <- ellmer::chat_openai(model = "offline-fixture", credentials = function() "synthetic-not-a-credential")
  chat$set_system_prompt("unchanged template")
  payloads <- list()
  testthat::local_mocked_bindings(custom_author_request = function(session, stage, payload) {
    payloads[[length(payloads) + 1L]] <<- payload
    if (stage == "contract")
      author_proposal()
    else author_source()
  }, .package = "finnts")
  raw <- data.frame(Date = seq(as.Date("2021-01-01"), by = "month", length.out = 24), id = "PRIVATE_SERIES_SENTINEL", value = 12345)
  reviewed <- character()
  respond <- function(request) {
    reviewed <<- c(reviewed, request$stage)
    "yes"
  }
  model <- create_custom_model("Use mean.", chat, raw,
    combo_variables = "id", target_variable = "value", date_type = "month",
    forecast_horizon = 1, validation_examples = author_examples(), control = custom_model_control(interact = respond)
  )
  expect_s3_class(model, "finnts_custom_model")
  expect_identical(reviewed, "review_create")
  expect_false(grepl("PRIVATE_SERIES_SENTINEL|12345", jsonlite::toJSON(payloads)))
  expect_false(any(grepl("PRIVATE_SERIES_SENTINEL", unlist(model, use.names = FALSE), fixed = TRUE)))
  expect_equal(chat$get_system_prompt(), "unchanged template")
  expect_length(chat$get_turns(), 0L)
  info <- set_run_info(path = withr::local_tempdir(), project_name = "authored-run", add_unique_id = FALSE)
  forecast_time_series(info, raw, "id", "value", "month", 1,
    custom_models = list(human_mean = model), models_to_run = "human_mean",
    stationary = FALSE, clean_missing_values = FALSE, recipes_to_run = "R1", run_ensemble_models = FALSE, run_global_models = FALSE,
    negative_forecast = TRUE, average_models = FALSE, back_test_scenarios = 1, return_data = FALSE
  )
  result <- get_forecast_data(info)
  expect_equal(result$Forecast, rep(12345, nrow(result)))
  expect_length(payloads, 2L)
})

test_that("measured report schemas cannot omit holdout evidence", {
  contract <- custom_author_contract(author_proposal(), "Use mean.", list(date_type = "month", forecast_horizon = 1), NULL,
    "local", author_examples(),
    protocol = custom_author_protocol(
      list(date_type = "month", forecast_horizon = 1), "local",
      character(), "base_only", TRUE
    )
  )
  definition <- custom_author_definition(contract, author_source())
  report <- list(
    passed = TRUE, code = "passed", version_id = definition$version_id, examples_id = digest::digest(contract$examples,
      algo = "sha256", serializeVersion = 2
    ), examples = list(list(
      name = "ordinary", model_type = "local", maximum_error = 0,
      tolerance = 1e-08
    )), holdouts = list(list(model_type = "local")), serialization = TRUE, r_version = "test", package_version = "test",
    elapsed = 1
  )
  report$examples <- lapply(contract$examples, function(example) list(
    name = example$name, model_type = example$model_type,
    maximum_error = 0, tolerance = example$tolerance
  ))
  expect_error(custom_author_checks(report, definition, "contract", contract$examples), "fields|holdout")
})

test_that("clarification and exhausted repair never manufacture approval", {
  question <- author_proposal()
  question$questions <- list(list(kind = "business", topic = "business_context", question = "Which history window?"))
  tracker <- author_harness(list(question, author_proposal(), author_source()))
  respond <- function(request) {
    if (request$stage == "clarify")
      list(question_1 = "All available history.")
    else "yes"
  }
  model <- create_custom_model("Use mean.", NULL, input_data = data.frame(), validation_examples = author_examples(), control = custom_model_control(interact = respond))
  expect_s3_class(model, "finnts_custom_model")
  expect_equal(tracker$requests[[2]]$payload$answers[[1]]$question_1, "All available history.")
})

test_that("exhaustion and stale reports cannot reach final approval", {
  tracker <- author_harness()
  tracker$reports <- list(list(passed = FALSE, code = "intent_mismatch"))
  stages <- character()
  respond <- function(request) {
    stages <<- c(stages, request$stage)
    "yes"
  }
  expect_error(create_custom_model("Use mean.", NULL,
    input_data = data.frame(), validation_examples = author_examples(),
    control = custom_model_control(max_attempts = 1, interact = respond)
  ), "No candidate")
  expect_false("approve" %in% stages)
})

test_that("a report for different examples is never accepted", {
  tracker <- author_harness()
  tracker$reports <- list(list(passed = TRUE, examples_id = paste(rep("a", 64), collapse = "")))
  expect_error(create_custom_model("Use mean.", NULL,
    input_data = data.frame(), validation_examples = author_examples(),
    control = custom_model_control(interact = function(request) "yes")
  ), "examples.*confirmed")
})

test_that("measured comparisons must cover the exact confirmed examples", {
  contract <- custom_author_contract(author_proposal(), "Use mean.", list(date_type = "month", forecast_horizon = 1), NULL,
    "local", author_examples(),
    protocol = custom_author_protocol(
      list(date_type = "month", forecast_horizon = 1), "local",
      character(), "base_only", TRUE
    )
  )
  definition <- custom_author_definition(contract, author_source())
  report <- list(
    passed = TRUE, code = "passed", version_id = definition$version_id, examples_id = digest::digest(contract$examples,
      algo = "sha256", serializeVersion = 2
    ), examples = lapply(contract$examples, function(example) list(
      name = example$name,
      model_type = example$model_type, maximum_error = 0, tolerance = example$tolerance
    )), holdouts = list(list(
      model_type = "local",
      rows = 1L, training_rows = 2L, cutoff = "2020-02-01", mae = 1, wmape = 0.01
    )), serialization = TRUE, r_version = "test",
    package_version = "test", elapsed = 1, verification = list(
      schema_version = 2L,
      provenance = contract$expectation_provenance, properties = list(row_order = 2L), expected_errors = 0L,
      required_properties = character(), diagnostics = list()
    )
  )
  for (defect in c("missing", "renamed", "tolerance")) local({
    tracker <- author_harness()
    altered <- report
    if (defect == "missing")
      altered$examples <- altered$examples[1]
    if (defect == "renamed")
      altered$examples[[2]]$name <- "unconfirmed"
    if (defect == "tolerance")
      altered$examples[[2]]$tolerance <- 0.01
    tracker$reports <- list(altered)
    expect_error(create_custom_model("Use mean.", NULL,
      input_data = data.frame(), validation_examples = author_examples(),
      control = custom_model_control(interact = function(request) "yes")
    ), "confirmed examples")
  })
})

test_that("invalid authoring inputs stop before any provider request", {
  tracker <- author_harness()
  respond <- function(request) "yes"
  expect_error(create_custom_model("Use mean.", NULL, control = custom_model_control(interact = respond)), "exactly one")
  expect_error(
    create_custom_model("Use mean.", NULL, input_data = data.frame(), run_info = list(), control = custom_model_control(interact = respond)),
    "exactly one"
  )
  for (attempts in c(0, 1.5, 4, NA_real_)) {
    expect_error(create_custom_model("Use mean.", NULL, input_data = data.frame(), control = custom_model_control(
      max_attempts = attempts,
      interact = respond
    )), "max_attempts")
  }
  for (timeout in c(0, Inf, 121)) {
    expect_error(create_custom_model("Use mean.", NULL, input_data = data.frame(), control = custom_model_control(
      validation_budget = timeout,
      interact = respond
    )), "validation_budget")
  }
  expect_error(
    create_custom_model("Use mean.", NULL, input_data = data.frame(), model_type = c("local", "local"), control = custom_model_control(interact = respond)),
    "model_type"
  )
  expect_length(tracker$requests, 0L)
  expect_identical(tracker$validations, 0L)
})

test_that("authoring restores caller RNG and existing options", {
  tracker <- author_harness()
  withr::local_preserve_seed()
  withr::local_options(list(finnts_authoring_option = "original"))
  set.seed(42)
  before <- .Random.seed
  respond <- function(request) {
    stats::runif(1)
    options(finnts_authoring_option = "changed")
    "yes"
  }
  create_custom_model("Use mean.", NULL, input_data = data.frame(), validation_examples = author_examples(), control = custom_model_control(interact = respond))
  expect_identical(.Random.seed, before)
  expect_identical(getOption("finnts_authoring_option"), "original")
  expect_length(tracker$requests, 2L)
})
