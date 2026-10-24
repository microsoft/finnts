# Capture the existing typed contract error without changing caller examples.
error_contract_condition <- function(policy, examples = author_examples(), modes = "local") {
  tryCatch(custom_author_behavior_contract(policy, character(), examples, modes), error = identity)
}

# Return a fixed error declaration and an independently supplied error example.
error_contract_fixture <- function() {
  entry <- list(code = "zero_units", phase = "fit", description = "Units must be positive.")
  examples <- author_examples()
  examples[[2]]$outcome <- "error"
  examples[[2]]$expected <- NULL
  examples[[2]]$expected_error <- entry[c("phase", "code")]
  list(entry = entry, examples = examples)
}

test_that("error contracts identify missing examples and numerical controls", {
  fixture <- error_contract_fixture()
  failure <- error_contract_condition(list(fixture$entry))
  expect_s3_class(failure, "finnts_custom_model_authoring_error")
  expect_identical(failure$code, "invalid_error_contract")
  expect_identical(failure$error_contract$reason, "missing_error_example")
  expect_identical(failure$error_contract$error_code, "zero_units")
  expect_identical(failure$error_contract$expected_phase, "fit")
  expect_identical(failure$error_contract$model_type, "local")
  expect_true(failure$error_contract$requires_caller_input)
  error_only <- fixture$examples[c(2, 2)]
  error_only[[2]]$name <- "another_error"
  missing <- error_contract_condition(list(fixture$entry), error_only)
  expect_identical(missing$error_contract$reason, "missing_numerical_control")
})

test_that("error contracts prioritize undeclared codes and phase mismatches", {
  fixture <- error_contract_fixture()
  undeclared <- error_contract_condition(list(), fixture$examples)
  expect_identical(undeclared$error_contract$reason, "undeclared_example_error")
  expect_identical(undeclared$error_contract$example_index, 2L)
  fixture$examples[[2]]$expected_error$phase <- "predict"
  mismatch <- error_contract_condition(list(fixture$entry), fixture$examples)
  expect_identical(mismatch$error_contract$reason, "error_phase_mismatch")
  expect_identical(mismatch$error_contract$expected_phase, "fit")
  expect_identical(mismatch$error_contract$actual_phase, "predict")
})

test_that("malformed declarations are bounded and valid mixtures remain unchanged", {
  fixture <- error_contract_fixture()
  duplicate <- error_contract_condition(rep(list(fixture$entry), 2))
  expect_identical(duplicate$error_contract$reason, "invalid_error_declaration")
  malformed <- fixture$entry
  malformed$code <- "PRIVATE_TOKEN /secret/path"
  condition <- error_contract_condition(list(malformed))
  expect_identical(condition$error_contract$reason, "invalid_error_declaration")
  expect_false(grepl("PRIVATE_TOKEN", jsonlite::toJSON(condition$error_contract), fixed = TRUE))
  before <- serialize(fixture$examples, NULL)
  expect_identical(custom_author_behavior_contract(list(fixture$entry), character(),
    fixture$examples, "local")$error_policy, list(fixture$entry))
  expect_identical(serialize(fixture$examples, NULL), before)
})

test_that("contract mismatch details survive validated failures and direct feedback", {
  fixture <- error_contract_fixture()
  condition <- error_contract_condition(list(fixture$entry))
  feedback <- custom_model_feedback(condition)
  expect_identical(feedback$code, "invalid_error_contract")
  expect_identical(feedback$details$error_contract, condition$error_contract)
  expect_identical(feedback$status, "needs_input")
  expect_true(length(feedback$questions) > 0L)
  failure <- custom_author_failure(NULL, "contract", condition$code,
    error_contract = condition$error_contract)
  expect_identical(validate_custom_author_failure(failure)$error_contract, condition$error_contract)
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(failure, path)
  expect_identical(validate_custom_author_failure(readRDS(path)), failure)
})

test_that("source contract conflicts require input rather than guessed driver paths", {
  condition <- tryCatch(custom_author_abort("source", "Driver forecast order contradicts the examples.",
    code = "contract_conflict"), error = identity)
  feedback <- custom_model_feedback(condition)
  expect_identical(feedback$status, "needs_input")
  expect_identical(feedback$next_action, "ask_user")
  expect_true(length(feedback$questions) > 0L)
})

test_that("fixed caller error-contract failures preserve diagnostics without execution", {
  requests <- 0L
  local_mocked_bindings(check_agent_ellmer_version = function() invisible(NULL),
    new_llm_session = function(llm) list(),
    custom_author_request = function(session, stage, payload) {
      requests <<- requests + 1L
      expect_identical(stage, "contract")
      proposal <- author_proposal()
      proposal$defaults_used <- character()
      proposal$error_policy <- list(error_contract_fixture()$entry)
      proposal
    }, custom_author_validate = function(...) stop("Source must not execute"),
    .package = "finnts")
  examples <- custom_examples_table(author_examples())
  before <- serialize(examples, NULL)
  result <- tryCatch(create_custom_model("Use a mean; reject zero units.", list(),
    input_data = author_examples()[[1]]$history, combo_variables = "Combo",
    target_variable = "Target", date_type = "month", forecast_horizon = 1,
    model_type = "local", approval = "automatic", validation_examples = examples,
    control = custom_model_control(package_policy = "base_only")), error = identity)
  expect_identical(requests, 1L)
  expect_identical(result$code, "invalid_error_contract")
  feedback <- custom_model_feedback(result)
  expect_identical(feedback$status, "needs_input")
  expect_identical(feedback$details$error_contract$reason, "missing_error_example")
  expect_identical(serialize(examples, NULL), before)
})

test_that("generated contract details reach repair prompts and final exhaustion", {
  for (limit in c(1L, 2L)) local({
    calls <- list()
    local_mocked_bindings(check_agent_ellmer_version = function() invisible(NULL),
      new_llm_session = function(llm) list(),
      custom_author_request = function(session, stage, payload) {
        calls[[length(calls) + 1L]] <<- list(stage = stage, payload = payload)
        if (stage == "source") return(author_source())
        proposal <- author_proposal()
        proposal$defaults_used <- character()
        proposal$examples <- lapply(payload$protocol$scaffold$scenarios, function(scenario) list(
          id = scenario$id, rationale = "Independent mean of ten and twenty or zeros.",
          outcome = "predictions", error_phase = "none", error_code = "",
          series = lapply(scenario$combos, function(combo) list(combo = combo,
            history = list(Target = if (scenario$edge_case) c(0, 0) else c(10, 20)),
            future = list(), expected = rep(if (scenario$edge_case) 0 else 15,
              payload$protocol$scaffold$forecast_horizon)))))
        if (length(calls) == 1L) proposal$error_policy <- list(error_contract_fixture()$entry)
        proposal
      }, .package = "finnts")
    result <- tryCatch(create_custom_model("Use a historical mean.", list(),
      input_data = data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 8),
        Combo = "example", Target = rep(15, 8)), combo_variables = "Combo",
      target_variable = "Target", date_type = "month", forecast_horizon = 1,
      model_type = "local", approval = "automatic",
      control = custom_model_control(max_attempts = limit, package_policy = "base_only")),
      error = identity)
    if (limit == 1L) {
      expect_identical(result$code, "invalid_error_contract")
      feedback <- custom_model_feedback(result)
      expect_identical(feedback$details$error_contract$reason, "missing_error_example")
      expect_false(feedback$details$error_contract$requires_caller_input)
      expect_identical(feedback$status, "failed")
    } else {
      expect_true(!inherits(result, "error"),
        info = if (inherits(result, "error")) conditionMessage(result) else NULL)
      expect_s3_class(result, "finnts_custom_model")
      expect_identical(calls[[2]]$payload$feedback$error_contract$reason, "missing_error_example")
      expect_false(calls[[2]]$payload$feedback$error_contract$requires_caller_input)
    }
  })
})
