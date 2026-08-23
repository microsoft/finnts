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

test_that("ellmer chats retain distinct credentials after RDS serialization", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")

  agent_chats <- list(
    driver_llm = create_serializable_agent_chat("driver-test-key", "driver-model"),
    reason_llm = create_serializable_agent_chat("reason-test-key", "reason-model")
  )
  chat_path <- tempfile(fileext = ".rds")
  on.exit(unlink(chat_path), add = TRUE)

  saveRDS(agent_chats, chat_path)
  restored_chats <- readRDS(chat_path)

  expect_s3_class(restored_chats$driver_llm, "Chat")
  expect_s3_class(restored_chats$reason_llm, "Chat")
  expect_equal(restored_chats$driver_llm$get_model(), "driver-model")
  expect_equal(restored_chats$reason_llm$get_model(), "reason-model")
  expect_identical(
    restored_chats$driver_llm$get_provider()@credentials(),
    "driver-test-key"
  )
  expect_identical(
    restored_chats$reason_llm$get_provider()@credentials(),
    "reason-test-key"
  )
})

test_that("ellmer chats survive PSOCK transport and clone independently", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")

  agent_info <- list(
    driver_llm = create_serializable_agent_chat("driver-test-key", "driver-model"),
    reason_llm = create_serializable_agent_chat("reason-test-key", "reason-model")
  )
  agent_info$driver_llm$set_system_prompt("driver history")
  agent_info$reason_llm$set_system_prompt("reason history")
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

  worker_result <- foreach::foreach(
    combo = "worker-combo",
    # load_all() needs this even though the installed namespace does not.
    .export = "clone_agent_info_chats",
    .errorhandling = "stop"
  ) %op%
    {
      worker_agent_info <- clone_agent_info_chats(agent_info)
      worker_agent_info$driver_llm$set_model("worker-driver-model")

      list(
        driver_model = worker_agent_info$driver_llm$get_model(),
        reason_model = worker_agent_info$reason_llm$get_model(),
        driver_system_prompt = worker_agent_info$driver_llm$get_system_prompt(),
        reason_system_prompt = worker_agent_info$reason_llm$get_system_prompt(),
        driver_credentials = worker_agent_info$driver_llm$get_provider()@credentials(),
        reason_credentials = worker_agent_info$reason_llm$get_provider()@credentials()
      )
    }
  worker_result <- worker_result[[1]]

  expect_equal(worker_result$driver_model, "worker-driver-model")
  expect_equal(worker_result$reason_model, "reason-model")
  expect_null(worker_result$driver_system_prompt)
  expect_null(worker_result$reason_system_prompt)
  expect_identical(worker_result$driver_credentials, "driver-test-key")
  expect_identical(worker_result$reason_credentials, "reason-test-key")
  expect_equal(agent_info$driver_llm$get_model(), "driver-model")
  expect_equal(agent_info$driver_llm$get_system_prompt(), "driver history")
})

test_that("each combo receives fresh independent chats", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")

  agent_info <- list(
    driver_llm = create_serializable_agent_chat("driver-test-key", "driver-model"),
    reason_llm = create_serializable_agent_chat("reason-test-key", "reason-model")
  )
  agent_info$driver_llm$set_system_prompt("driver history")
  agent_info$reason_llm$set_system_prompt("reason history")

  first_combo <- clone_agent_info_chats(agent_info)
  second_combo <- clone_agent_info_chats(agent_info)
  first_combo$driver_llm$set_model("first-combo-model")
  first_combo$reason_llm$set_system_prompt("first combo prompt")

  expect_false(identical(first_combo$driver_llm, second_combo$driver_llm))
  expect_false(identical(first_combo$reason_llm, second_combo$reason_llm))
  expect_equal(second_combo$driver_llm$get_model(), "driver-model")
  expect_null(second_combo$driver_llm$get_system_prompt())
  expect_null(second_combo$reason_llm$get_system_prompt())
  expect_equal(agent_info$driver_llm$get_model(), "driver-model")
  expect_equal(agent_info$driver_llm$get_system_prompt(), "driver history")
  expect_equal(agent_info$reason_llm$get_system_prompt(), "reason history")
})