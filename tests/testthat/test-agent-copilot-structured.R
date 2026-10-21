# Inspect the conversation sent through the mocked CLI boundary, not a live provider.
copilot_structured_payload <- function(prompt) {
  jsonlite::fromJSON(substring(prompt, regexpr("\n", prompt) + 1L), simplifyVector = FALSE)
}

test_that("Copilot structured replies preserve native nested JSON shapes and history", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  payloads <- list()
  responses <- c(
    '{"rows":[{"name":"a","values":[2],"enabled":true}],"empty":{},"kind":"mean"}',
    "```json\n[1,2]\n```"
  )
  local_mocked_bindings(
    check_copilot_runtime = function(...) invisible(NULL),
    request_copilot = function(command, model, timeout, prompt) {
      payloads[[length(payloads) + 1L]] <<- copilot_structured_payload(prompt)
      responses[[length(payloads)]]
    }
  )
  chat <- chat_copilot("synthetic-model")
  chat$set_system_prompt("Keep the business rule fixed.")
  type <- ellmer::type_object(
    rows = ellmer::type_array(ellmer::type_object(
      name = ellmer::type_string(), values = ellmer::type_array(ellmer::type_number()),
      enabled = ellmer::type_boolean()
    )),
    empty = ellmer::TypeObject(properties = list(), additional_properties = FALSE),
    kind = ellmer::type_enum(c("mean", "last")),
    optional = ellmer::type_integer(required = FALSE)
  )
  result <- chat$chat_structured("Return the rule.", type = type, convert = FALSE, echo = "none")
  expect_identical(result, jsonlite::fromJSON(responses[[1]], simplifyVector = FALSE))
  expect_length(result$rows[[1]]$values, 1L)
  expect_identical(names(result$empty), character())
  schema <- payloads[[1]]$response_format
  expect_identical(schema$type, "object")
  expect_false(schema$additionalProperties)
  expect_setequal(unlist(schema$required), c("rows", "empty", "kind"))
  expect_identical(unlist(schema$properties$kind$enum), c("mean", "last"))
  expect_identical(names(schema$properties$empty$properties), character())
  expect_identical(payloads[[1]]$system_prompt, "Keep the business rule fixed.")
  expect_identical(chat$chat_structured("Return numbers.", type = ellmer::type_array(ellmer::type_integer()),
    convert = FALSE), list(1L, 2L))
  expect_length(payloads[[2]]$messages, 3L)
  expect_identical(payloads[[2]]$messages[[2]]$content, responses[[1]])
  expect_length(chat$get_turns(), 4L)
  copy <- unserialize(serialize(chat, NULL))
  fresh <- new_llm_session(copy)
  expect_true(is.function(fresh$chat_structured))
  expect_length(fresh$get_turns(), 0L)
  expect_null(fresh$get_system_prompt())
  expect_length(chat$get_turns(), 4L)
})

test_that("Copilot rejects invalid structured responses without retries or committed turns", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  response <- ""
  calls <- 0L
  local_mocked_bindings(
    check_copilot_runtime = function(...) invisible(NULL),
    request_copilot = function(...) {
      calls <<- calls + 1L
      response
    }
  )
  chat <- chat_copilot()
  type <- ellmer::type_object(
    count = ellmer::type_integer(), values = ellmer::type_array(ellmer::type_number()),
    kind = ellmer::type_enum("mean"), enabled = ellmer::type_boolean()
  )
  invalid <- c(
    "PRIVATE_RESPONSE", 'Some prose: {"count":1}', '[]', '{}',
    '{"count":1.5,"values":[],"kind":"mean","enabled":true}',
    '{"count":"1","values":[],"kind":"mean","enabled":true}',
    '{"count":1,"values":{},"kind":"mean","enabled":true}',
    '{"count":1,"values":[null],"kind":"mean","enabled":true}',
    '{"count":1,"values":[1e999],"kind":"mean","enabled":true}',
    '{"count":1,"values":[],"kind":"other","enabled":true}',
    '{"count":1,"values":[],"kind":"mean","enabled":1}',
    '{"count":1,"values":[],"kind":"mean","enabled":null}',
    '{"count":1,"values":[],"kind":"mean","enabled":true,"PRIVATE_EXTRA":0}',
    '{"count":1,"count":2,"values":[],"kind":"mean","enabled":true}'
  )
  for (response in invalid) {
    error <- tryCatch(chat$chat_structured("Keep private.", type = type, convert = FALSE), error = identity)
    expect_s3_class(error, "finnts_copilot_structured_error")
    expect_false(grepl("PRIVATE", conditionMessage(error), fixed = TRUE))
    expect_length(chat$get_turns(), 0L)
  }
  expect_identical(calls, length(invalid))
  response <- '{"count":1,"values":[],"kind":"mean","enabled":true}'
  expect_identical(chat$chat_structured("Valid.", type = type)$count, 1L)
  saved <- chat$get_turns()
  response <- "PRIVATE_RESPONSE"
  expect_error(chat$chat_structured("Bad.", type = type), class = "finnts_copilot_structured_error")
  expect_identical(chat$get_turns(), saved)
})

test_that("Copilot structured argument errors fail before transport and transport stays hard", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  calls <- 0L
  local_mocked_bindings(
    check_copilot_runtime = function(...) invisible(NULL),
    request_copilot = function(...) {
      calls <<- calls + 1L
      abort_copilot_transport("Synthetic transport failure.")
    }
  )
  chat <- chat_copilot()
  type <- ellmer::type_string()
  expect_error(chat$chat_structured("", type = type), "prompt")
  expect_error(chat$chat_structured("Rule", type = type, convert = TRUE), "convert")
  expect_error(chat$chat_structured("Rule", type = type, echo = TRUE), "echo")
  expect_error(chat$chat_structured("Rule", type = list(type = "string")), "type")
  expect_error(chat$chat_structured("Rule", type = ellmer::TypeJsonSchema(json = list(type = "string"))), "type")
  expect_identical(calls, 0L)
  expect_error(chat$chat_structured("Rule", type = type), class = "finnts_copilot_transport_error")
  expect_identical(calls, 1L)
  expect_length(chat$get_turns(), 0L)
})

test_that("Copilot response parsing never reads a file or URL and optional S7 fails early", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  input <- tempfile(fileext = ".json")
  writeLines('{"ok":true}', input)
  calls <- 0L
  local_mocked_bindings(
    check_copilot_runtime = function(...) invisible(NULL),
    request_copilot = function(...) {
      calls <<- calls + 1L
      input
    }
  )
  chat <- chat_copilot()
  expect_error(chat$chat_structured("Rule", type = ellmer::type_object(ok = ellmer::type_boolean())),
    class = "finnts_copilot_structured_error")
  original <- requireNamespace
  # Isolate dependency availability in this method, without changing base R.
  environment(chat$chat_structured) <- list2env(list(requireNamespace = function(package, ...) {
    if (identical(package, "S7")) FALSE else original(package, ...)
  }), parent = environment(chat$chat_structured))
  expect_error(chat$chat_structured("Rule", type = ellmer::type_string()), "Install")
  expect_identical(calls, 1L)
  expect_identical(chat$chat("Text still works."), input)
})
