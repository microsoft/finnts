test_that("Copilot creates reusable custom models through the real authoring gates", {
  skip_if_not_installed("ellmer", minimum_version = "0.4.0")
  raw <- data.frame(
    Date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    id = "PRIVATE_COPILOT_SERIES", value = seq_len(12) + 12345
  )
  for (approval in c("manual", "automatic")) local({
    calls <- list()
    proposal <- author_proposal()
    if (approval == "automatic") proposal$defaults_used <- list()
    replies <- list(proposal, author_source())
    local_custom_console()
    local_mocked_bindings(
      check_copilot_runtime = function(...) invisible(NULL),
      request_copilot = function(command, model, timeout, prompt) {
        calls[[length(calls) + 1L]] <<- prompt
        as.character(jsonlite::toJSON(replies[[length(calls)]], auto_unbox = TRUE))
      }
    )
    llm <- chat_copilot("synthetic-model")
    llm$set_system_prompt("PRIVATE_TEMPLATE_PROMPT")
    result <- create_custom_model("Use the mean of historical targets independently for each series.",
      llm = llm, input_data = raw, combo_variables = "id", target_variable = "value",
      date_type = "month", forecast_horizon = 1, model_type = "local",
      validation_examples = custom_examples_table(author_examples()), approval = approval,
      control = custom_model_control(package_policy = "base_only")
    )
    expect_s3_class(result, "finnts_custom_model")
    expect_identical(custom_model_feedback(result)$status, "approved")
    expect_identical(result$validation$technical_passed, TRUE)
    expect_identical(result$approval$intent_confirmed, approval == "manual")
    expect_identical(names(result), c("definition", "validation", "approval"))
    restored <- unserialize(serialize(result, NULL))
    expect_identical(custom_run_envelope(restored), result)
    expect_length(calls, 2L)
    expect_false(any(grepl("PRIVATE_|12345", unlist(calls))))
    expect_length(llm$get_turns(), 0L)
    expect_identical(llm$get_system_prompt(), "PRIVATE_TEMPLATE_PROMPT")
  })
})
