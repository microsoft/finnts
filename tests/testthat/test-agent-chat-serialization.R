# tests/testthat/test-agent-chat-serialization.R

create_serializable_agent_chat <- function(api_key, model) {
  withr::local_envvar(c(
    AZURE_TENANT_ID = NA,
    AZURE_CLIENT_ID = NA,
    AZURE_CLIENT_SECRET = NA,
    AZURE_OPENAI_API_KEY = api_key
  ))

  ellmer::chat_azure_openai(
    endpoint = "https://example.openai.azure.com",
    model = model
  )
}

test_that("new LLM sessions are clean and independent", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")

  llm <- create_serializable_agent_chat("template-test-key", "template-model")
  llm$set_system_prompt("template prompt")
  first_session <- new_llm_session(llm)
  second_session <- new_llm_session(llm)

  first_session$set_model("first-session-model")
  first_session$set_system_prompt("first session prompt")

  expect_false(identical(first_session, second_session))
  expect_equal(second_session$get_model(), "template-model")
  expect_null(second_session$get_system_prompt())
  expect_length(second_session$get_turns(), 0)
  expect_identical(
    second_session$get_provider()@credentials(),
    "template-test-key"
  )
  expect_equal(llm$get_model(), "template-model")
  expect_equal(llm$get_system_prompt(), "template prompt")
})

test_that("EDA and update graphs receive isolated LLM sessions", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")

  llm <- create_serializable_agent_chat("template-test-key", "template-model")
  llm$set_system_prompt("template prompt")
  observed_sessions <- list()

  local_mocked_bindings(
    run_graph = function(chat, workflow, init_ctx = list(node = "start")) {
      observed_sessions[[length(observed_sessions) + 1L]] <<- list(
        is_template = identical(chat, llm),
        system_prompt = chat$get_system_prompt(),
        turn_count = length(chat$get_turns())
      )
      chat$set_system_prompt("workflow prompt")
      invisible(init_ctx)
    }
  )

  agent_info <- list(llm = llm)
  eda_agent_workflow(
    agent_info = agent_info,
    parallel_processing = NULL,
    num_cores = 1
  )
  update_fcst_agent_workflow(
    agent_info = agent_info,
    project_info = list(),
    parallel_processing = NULL,
    inner_parallel = FALSE,
    num_cores = 1
  )

  expect_length(observed_sessions, 2)
  expect_false(any(vapply(observed_sessions, function(session) session$is_template, logical(1))))
  expect_true(all(vapply(observed_sessions, function(session) is.null(session$system_prompt), logical(1))))
  expect_equal(vapply(observed_sessions, function(session) session$turn_count, integer(1)), c(0L, 0L))
  expect_equal(llm$get_system_prompt(), "template prompt")
  expect_length(llm$get_turns(), 0)
})

test_that("ellmer LLM template survives RDS serialization", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")

  llm <- create_serializable_agent_chat("template-test-key", "template-model")
  chat_path <- tempfile(fileext = ".rds")
  on.exit(unlink(chat_path), add = TRUE)

  saveRDS(llm, chat_path)
  restored_llm <- readRDS(chat_path)

  expect_s3_class(restored_llm, "Chat")
  expect_equal(restored_llm$get_model(), "template-model")
  expect_identical(
    restored_llm$get_provider()@credentials(),
    "template-test-key"
  )
})

test_that("each PSOCK series receives a fresh LLM session", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")

  llm <- create_serializable_agent_chat("template-test-key", "template-model")
  llm$set_system_prompt("template prompt")
  cluster <- parallel::makePSOCKcluster(1)
  doParallel::registerDoParallel(cluster)
  on.exit(
    {
      parallel::stopCluster(cluster)
      foreach::registerDoSEQ()
    },
    add = TRUE
  )
  `%op%` <- foreach::`%dopar%`

  worker_results <- foreach::foreach(
    combo = c("series-a", "series-b"),
    # load_all() needs this even though the installed namespace does not.
    .export = "new_llm_session",
    .errorhandling = "stop"
  ) %op%
    {
      session <- new_llm_session(llm)
      initial_model <- session$get_model()
      initial_system_prompt <- session$get_system_prompt()
      initial_turn_count <- length(session$get_turns())
      session$set_model(paste0(combo, "-model"))
      session$set_system_prompt(paste0(combo, " prompt"))

      list(
        initial_model = initial_model,
        initial_system_prompt = initial_system_prompt,
        initial_turn_count = initial_turn_count,
        session_model = session$get_model(),
        session_credentials = session$get_provider()@credentials(),
        template_model = llm$get_model(),
        template_system_prompt = llm$get_system_prompt()
      )
    }

  expect_equal(
    vapply(worker_results, function(result) result$initial_model, character(1)),
    rep("template-model", 2)
  )
  expect_true(all(vapply(worker_results, function(result) is.null(result$initial_system_prompt), logical(1))))
  expect_equal(vapply(worker_results, function(result) result$initial_turn_count, integer(1)), c(0L, 0L))
  expect_equal(
    vapply(worker_results, function(result) result$session_model, character(1)),
    c("series-a-model", "series-b-model")
  )
  expect_true(all(vapply(
    worker_results,
    function(result) identical(result$session_credentials, "template-test-key"),
    logical(1)
  )))
  expect_equal(
    vapply(worker_results, function(result) result$template_model, character(1)),
    rep("template-model", 2)
  )
  expect_equal(
    vapply(worker_results, function(result) result$template_system_prompt, character(1)),
    rep("template prompt", 2)
  )
  expect_equal(llm$get_model(), "template-model")
  expect_equal(llm$get_system_prompt(), "template prompt")
})