test_that("initial authoring instructions have a generous UTF-8 byte boundary", {
  limit <- 1048576L
  calls <- 0L
  local_mocked_bindings(check_agent_ellmer_version = function() invisible(NULL),
    new_llm_session = function(...) {
    calls <<- calls + 1L
    stop("provider sentinel", call. = FALSE)
  }, .package = "finnts")
  # The input boundary must run before session creation or provider calls.
  large <- paste(rep("Explain this rule.\nx <- function(y) y * 2\n", 5000L), collapse = "")
  for (text in c(large, strrep("x", limit), paste0(strrep("x", limit - 2L), "\u00e9"))) {
    expect_error(create_custom_model(instructions = text, llm = list(),
      input_data = data.frame(), approval = "automatic"), "provider sentinel")
  }
  before <- calls
  expect_error(create_custom_model(instructions = strrep("x", limit + 1L),
    llm = list(), input_data = data.frame(), approval = "automatic"), "1048576")
  expect_equal(calls, before)
})

test_that("accumulated edits cannot bypass the limit or save a receipt", {
  state <- list(pending = list(request_id = "request", stage = "review_create", questions = NULL),
    attempts = list(contract = 0L, source = 0L),
    settings = list(max_attempts = 3L, instructions = strrep("x", 1048560L)))
  before <- serialize(state, NULL)
  local_mocked_bindings(custom_draft_save = function(...) stop("unexpected draft save"), .package = "finnts")
  expect_error(custom_draft_answer(state, list(request_id = "request",
    answer = list(action = "edit", instructions = "One more complete rule."))), "1048576")
  expect_identical(serialize(state, NULL), before)
  expect_identical(custom_author_instructions(paste0(strrep("x", 1048574L), "\u00e9")),
    paste0(strrep("x", 1048574L), "\u00e9"))
  expect_error(custom_author_instructions(paste0(strrep("x", 1048575L), "\u00e9")), "1048576")
})

test_that("review edits reject oversized input and preserve large text", {
  request <- list(stage = "review_create", questions = NULL)
  text <- paste(rep("Keep this code:\nf <- function(x) x + 1\n", 5000L), collapse = "")
  expect_identical(custom_author_response(request, list(action = "edit", instructions = text))$instructions,
    trimws(text))
  expect_error(custom_author_response(request,
    list(action = "edit", instructions = strrep("x", 1048577L))), "1048576")
})

test_that("large code-bearing instructions reach the provider boundary unchanged", {
  text <- paste(rep("Preserve the rule.\nf <- function(x) { x * 2 }\n", 5000L), collapse = "")
  calls <- 0L
  local_mocked_bindings(check_agent_ellmer_version = function() invisible(NULL),
    new_llm_session = function(...) list(),
    custom_author_request = function(session, stage, payload) {
      calls <<- calls + 1L
      expect_identical(stage, "contract")
      expect_identical(payload$instructions, text)
      stop("large request sentinel", call. = FALSE)
    }, .package = "finnts")
  data <- data.frame(Date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    id = "one", value = 10)
  expect_error(create_custom_model(text, llm = list(), input_data = data,
    combo_variables = "id", target_variable = "value", date_type = "month",
    forecast_horizon = 2, approval = "automatic"), "large request sentinel")
  expect_equal(calls, 1L)
})
