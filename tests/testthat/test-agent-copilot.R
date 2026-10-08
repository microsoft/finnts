# Build a synthetic CLI JSONL transcript with one response and terminal result.
copilot_test_output <- function(content = "answer", exit_code = 0L) {
  paste(
    jsonlite::toJSON(
      list(type = "assistant.message", data = list(content = content, toolRequests = list())),
      auto_unbox = TRUE
    ),
    jsonlite::toJSON(list(type = "result", exitCode = exit_code), auto_unbox = TRUE),
    sep = "\n"
  )
}

# Mock inspection only, retaining the actual public constructor and sessions.
mock_copilot_runtime <- function() {
  testthat::local_mocked_bindings(
    check_copilot_runtime = function(...) invisible("copilot"),
    .env = parent.frame()
  )
}

test_that("Copilot templates accept only valid configuration", {
  mock_copilot_runtime()
  for (value in list(NULL, NA_character_, "", " ", c("a", "b"), 1)) {
    expect_error(chat_copilot(value), "model")
  }
  for (value in list(NA_real_, Inf, 0, -1, "1", c(1, 2))) {
    expect_error(chat_copilot("model", timeout = value), "timeout")
  }
  expect_error(chat_copilot("model", command = ""), "command")
  automatic <- chat_copilot()
  expect_identical(automatic$get_model(), "auto")
  expect_true("--model=auto" %in% copilot_cli_args(automatic$get_model()))
  expect_identical(new_llm_session(automatic)$get_model(), "auto")
  llm <- chat_copilot("model")
  expect_identical(llm$get_model(), "model")
  expect_s3_class(llm, "finnts_copilot_chat")
  expect_invisible(check_agent_llm(llm))
  expect_invisible(check_agent_llm(structure(list(), class = "Chat")))
  expect_error(check_agent_llm(list()), "invalid type")
})

test_that("set_agent_info accepts Copilot without changing artifact schemas", {
  mock_copilot_runtime()
  writes <- list()
  local_mocked_bindings(
    load_agent_runs = function(...) tibble::tibble(),
    write_data = function(x, ...) {
      writes[[length(writes) + 1L]] <<- x
      invisible(NULL)
    }
  )
  project <- set_project_info(
    project_name = "copilot-schema-test", path = tempdir(),
    combo_variables = "id", target_variable = "value", date_type = "month"
  )
  writes <- list()
  input <- data.frame(
    id = "series-a",
    Date = seq(as.Date("2022-01-01"), by = "month", length.out = 36),
    value = seq_len(36)
  )
  copilot <- chat_copilot("model")
  result <- set_agent_info(
    project, copilot, input, forecast_horizon = 6,
    run_global_models = FALSE, overwrite = TRUE
  )
  expect_identical(result$llm, copilot)
  copilot_columns <- lapply(writes, names)
  writes <- list()
  set_agent_info(
    project, structure(list(), class = "Chat"), input, forecast_horizon = 6,
    run_global_models = FALSE, overwrite = TRUE
  )
  expect_identical(lapply(writes, names), copilot_columns)
  expect_false(any(c("llm", "provider", "session_id", "transcript") %in% unlist(copilot_columns)))
})

test_that("Copilot histories are isolated and failures do not commit turns", {
  mock_copilot_runtime()
  payloads <- list()
  fail <- FALSE
  local_mocked_bindings(
    request_copilot = function(command, model, timeout, prompt) {
      payloads[[length(payloads) + 1L]] <<- jsonlite::fromJSON(
        substring(prompt, regexpr("\n", prompt) + 1L), simplifyVector = FALSE
      )
      if (fail) abort_copilot_transport("synthetic transport failure")
      "answer"
    },
    check_agent_ellmer_version = function() stop("ellmer must not be required")
  )
  template <- chat_copilot("model")
  template$set_system_prompt("template prompt")
  template$chat("template history")
  first <- new_llm_session(template)
  second <- new_llm_session(template)
  expect_null(first$get_system_prompt())
  expect_length(first$get_turns(), 0)
  expect_length(second$get_turns(), 0)
  expect_invisible(first$set_system_prompt("workflow instructions"))
  expect_identical(first$chat("first", echo = FALSE), "answer")
  expect_identical(first$chat("second"), "answer")
  expect_length(payloads[[3]]$messages, 3)
  expect_identical(payloads[[3]]$system_prompt, "workflow instructions")
  expect_identical(payloads[[3]]$messages[[2]]$role, "assistant")
  expect_identical(payloads[[3]]$messages[[2]]$content, "answer")
  expect_length(second$get_turns(), 0)
  expect_length(template$get_turns(), 2)
  expect_identical(template$get_system_prompt(), "template prompt")
  fail <- TRUE
  expect_error(first$chat("failed"), class = "finnts_copilot_transport_error")
  expect_length(first$get_turns(), 4)
  expect_error(first$chat(NA_character_), "prompt")
  expect_error(first$chat("prompt", echo = NA), "echo")
})

test_that("Copilot sessions serialize without credentials or process handles", {
  mock_copilot_runtime()
  withr::local_envvar(COPILOT_GITHUB_TOKEN = "synthetic-secret-do-not-serialize")
  llm <- chat_copilot("model", command = "copilot", timeout = 42)
  restored <- unserialize(serialize(llm, NULL))
  expect_identical(restored$get_model(), "model")
  expect_identical(restored$timeout, 42)
  restored$set_system_prompt("restored")
  expect_null(llm$get_system_prompt())
  expect_identical(restored$get_system_prompt(), "restored")
  expect_length(restored$get_turns(), 0)
  expect_false(any(grepl("synthetic-secret", rawToChar(serialize(llm, NULL, ascii = TRUE)), fixed = TRUE)))
})

test_that("Copilot transport output is validated separately from response text", {
  for (text in c(
    '{"ok":true}', '[{"output_name":"data"}]', "result <- 1",
    "Finance-friendly answer", paste0("Unicode: ", intToUtf8(0x20ac), "\nsecond line")
  )) {
    expect_identical(parse_copilot_response(copilot_test_output(text)), text)
  }
  for (output in c("", "not JSON", "[]", '{"type":1}', '{"type":"result","exitCode":1}')) {
    expect_error(parse_copilot_response(output), class = "finnts_copilot_transport_error")
  }
  expect_error(parse_copilot_response(copilot_test_output("")), "empty")
  expect_error(
    parse_copilot_response(paste(
      '{"type":"assistant.message","data":1}', '{"type":"result","exitCode":0}',
      sep = "\n"
    )),
    "message envelope"
  )
  expect_error(parse_copilot_response(copilot_test_output(exit_code = 1L)), "successful")
  expect_error(
    parse_copilot_response(paste('{"type":"session.error"}', copilot_test_output(), sep = "\n")),
    "session error"
  )
  expect_error(
    parse_copilot_response(paste('{"type":"tool.execution_start"}', copilot_test_output(), sep = "\n")),
    "tool execution"
  )
  expect_error(
    parse_copilot_response(paste(copilot_test_output(), '{"type":"other"}', sep = "\n")),
    "terminal result"
  )
  expect_error(
    parse_copilot_response(paste(copilot_test_output(), copilot_test_output(), sep = "\n")),
    "terminal result"
  )
  expect_error(
    parse_copilot_response(paste(
      '{"type":"assistant.message","data":{"content":"answer","toolRequests":[{"name":"write"}]}}',
      '{"type":"result","exitCode":0}', sep = "\n"
    )),
    "tool request"
  )
})

test_that("Copilot requests disable tools and isolate CLI state", {
  captured <- NULL
  local_mocked_bindings(
    resolve_copilot_command = function(command) command,
    copilot_auth_env = function(timeout) c(COPILOT_GITHUB_TOKEN = "synthetic-token"),
    run_copilot_process = function(command, args, input, wd, env, timeout) {
      captured <<- list(command = command, args = args, input = input, wd = wd, env = env)
      list(status = 0L, stdout = copilot_test_output(), stderr = "")
    }
  )
  withr::local_envvar(c(
    COPILOT_ALLOW_ALL = "true",
    COPILOT_CUSTOM_INSTRUCTIONS_DIRS = "untrusted",
    COPILOT_PROVIDER_BASE_URL = "https://example.invalid"
  ))
  prompt <- paste0(strrep("large prompt with quotes ' \" & | \n", 3000), intToUtf8(0x20ac))
  expect_identical(request_copilot("path with spaces", "model", 10, prompt), "answer")
  expect_identical(captured$input, prompt)
  expect_false(any(grepl("large prompt", captured$args, fixed = TRUE)))
  expect_true(all(c(
    "--available-tools=", "--disable-builtin-mcps", "--no-custom-instructions",
    "--no-auto-update", "--no-ask-user", "--output-format=json"
  ) %in% captured$args))
  expect_false(any(c("--allow-all", "--allow-all-tools", "--continue", "--resume") %in% captured$args))
  expect_identical(unname(captured$env["COPILOT_ALLOW_ALL"]), "false")
  expect_identical(unname(captured$env["COPILOT_CUSTOM_INSTRUCTIONS_DIRS"]), "")
  expect_true(is.na(captured$env["COPILOT_PROVIDER_BASE_URL"]))
  settings <- jsonlite::read_json(file.path(captured$env[["COPILOT_HOME"]], "settings.json"))
  expect_true(settings$disableAllHooks)
  expect_false(settings$memory)
  expect_false(settings$ide$autoConnect)
  expect_true(dir.exists(captured$wd))
  expect_identical(basename(captured$wd), "work")
})

test_that("Copilot subprocess failure is hard and does not leak diagnostics", {
  local_mocked_bindings(
    resolve_copilot_command = function(command) command,
    copilot_auth_env = function(timeout) c(COPILOT_GITHUB_TOKEN = "synthetic-token"),
    run_copilot_process = function(...) {
      list(status = 1L, stdout = "secret prompt", stderr = "synthetic-token")
    }
  )
  error <- tryCatch(request_copilot("copilot", "model", 10, "prompt"), error = identity)
  expect_s3_class(error, "finnts_copilot_transport_error")
  expect_match(conditionMessage(error), "exit status 1")
  expect_false(grepl("synthetic-token", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("secret prompt", conditionMessage(error), fixed = TRUE))
  expect_null(error$call)
  expect_null(error$trace)
  expect_false(is_graceful_reason_failure(error))
})

test_that("environment authentication respects precedence without recording tokens", {
  withr::local_envvar(c(
    COPILOT_GITHUB_TOKEN = "first", GH_TOKEN = "second", GITHUB_TOKEN = "third"
  ))
  expect_identical(copilot_auth_env(1), c(COPILOT_GITHUB_TOKEN = "first"))
  Sys.unsetenv("COPILOT_GITHUB_TOKEN")
  expect_identical(copilot_auth_env(1), c(COPILOT_GITHUB_TOKEN = "second"))
  Sys.unsetenv("GH_TOKEN")
  expect_identical(copilot_auth_env(1), c(COPILOT_GITHUB_TOKEN = "third"))
  Sys.unsetenv("GITHUB_TOKEN")
  withr::local_envvar(PATH = "")
  expect_error(copilot_auth_env(1), "No Copilot authentication")
})

test_that("graph routing accepts text and legacy content responses", {
  workflow <- list(
    start = list(fn = "llm_decide", `next` = "stop"),
    stop = list(fn = NULL)
  )
  for (response in list("stop", list(content = "stop"))) {
    chat <- list(chat = function(...) response)
    expect_identical(run_graph(chat, workflow)$node, "stop")
  }
  expect_error(agent_chat_text(list(other = "text")), "one character")
  expect_error(agent_chat_text(c("a", "b")), "one character")
})

test_that("EDA update and Q&A graphs get clean Copilot sessions", {
  mock_copilot_runtime()
  template <- chat_copilot("model")
  template$set_system_prompt("template")
  sessions <- list()
  local_mocked_bindings(
    run_graph = function(chat, workflow, init_ctx = list(node = "start")) {
      sessions[[length(sessions) + 1L]] <<- chat
      list(results = list(synthesize_answer = list(answer = "answer")))
    },
    get_tool_spec = function(...) "synthetic tool specification"
  )
  info <- list(llm = template)
  eda_agent_workflow(info, NULL, 1)
  update_fcst_agent_workflow(info, list(), NULL, FALSE, 1)
  ask_agent_workflow(info, "question")
  expect_length(sessions, 3)
  expect_true(all(vapply(sessions, function(session) length(session$get_turns()) == 0, logical(1))))
  expect_null(sessions[[1]]$get_system_prompt())
  expect_null(sessions[[2]]$get_system_prompt())
  expect_match(sessions[[3]]$get_system_prompt(), "ROLE")
  expect_identical(template$get_system_prompt(), "template")
})

test_that("Copilot executable lookup fails early and rejects batch shims", {
  expect_error(resolve_copilot_command("finnts-nonexistent-copilot-executable"), "not found")
  if (.Platform$OS.type == "windows") {
    shim <- tempfile(fileext = ".cmd")
    writeLines("@echo off", shim)
    expect_error(resolve_copilot_command(shim), "standalone copilot.exe")
  }
})

test_that("runtime preflight rejects old or incompatible CLI versions", {
  skip_if_not_installed("processx", minimum_version = "3.9.0")
  local_mocked_bindings(resolve_copilot_command = function(...) "copilot")
  version <- "GitHub Copilot CLI 1.0.93"
  help <- paste(sub("=.*$", "", copilot_cli_args("model")), collapse = "\n")
  local_mocked_bindings(
    run = function(command, args, ...) {
      list(status = 0L, stdout = if ("--version" %in% args) version else help)
    },
    .package = "processx"
  )
  expect_invisible(check_copilot_runtime("copilot", 10))
  version <- "GitHub Copilot CLI 1.0.92"
  expect_error(check_copilot_runtime("copilot", 10), "1.0.93")
  version <- "unknown version"
  expect_error(check_copilot_runtime("copilot", 10), "1.0.93")
  version <- "GitHub Copilot CLI 1.0.93"
  help <- "--version --help"
  expect_error(check_copilot_runtime("copilot", 10), "lacks required options")
})

test_that("gh authentication fallback is private and host-aware", {
  skip_if_not_installed("processx", minimum_version = "3.9.0")
  skip_if(!nzchar(Sys.which("gh")), "GitHub CLI not installed")
  withr::local_envvar(c(
    COPILOT_GITHUB_TOKEN = NA, GH_TOKEN = NA, GITHUB_TOKEN = NA,
    COPILOT_GH_HOST = "github.example.invalid", GH_HOST = "ignored.invalid"
  ))
  captured <- NULL
  status <- 0L
  local_mocked_bindings(
    run = function(command, args, ...) {
      captured <<- args
      list(status = status, stdout = "synthetic-gh-token\n", stderr = "private diagnostics")
    },
    .package = "processx"
  )
  expect_identical(copilot_auth_env(10), c(COPILOT_GITHUB_TOKEN = "synthetic-gh-token"))
  expect_identical(captured, c("auth", "token", "--hostname", "github.example.invalid"))
  status <- 1L
  expect_error(copilot_auth_env(10), "No Copilot authentication")
})

test_that("piped transport handles large UTF-8 input and drains both outputs", {
  skip_if_not_installed("processx", minimum_version = "3.9.0")
  script <- tempfile("copilot pipe test ", fileext = ".R")
  writeLines(c(
    'con <- file("stdin")',
    'input <- paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\\n")',
    'close(con)',
    'cat(jsonlite::toJSON(input, auto_unbox = TRUE))',
    'cat(strrep("diagnostic", 10000), file = stderr())'
  ), script)
  executable <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  input <- paste0(strrep("line with ' quotes \" and & | ;\n", 6000), intToUtf8(0x20ac))
  result <- run_copilot_process(
    executable, c("--vanilla", script), input, tempdir(), NULL, 20
  )
  expect_identical(result$status, 0L)
  expect_identical(jsonlite::fromJSON(result$stdout), input)
  expect_identical(result$stderr, strrep("diagnostic", 10000))
})

test_that("transport timeout and early process exit terminate without hanging", {
  skip_if_not_installed("processx", minimum_version = "3.9.0")
  script <- tempfile(fileext = ".R")
  executable <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  writeLines("Sys.sleep(30)", script)
  start <- proc.time()[["elapsed"]]
  expect_error(
    run_copilot_process(executable, c("--vanilla", script), "prompt", tempdir(), NULL, 1),
    "timed out", class = "finnts_copilot_transport_error"
  )
  expect_lt(proc.time()[["elapsed"]] - start, 5)
  writeLines('cat("synthetic failure", file = stderr()); quit(status = 7)', script)
  result <- run_copilot_process(executable, c("--vanilla", script), "prompt", tempdir(), NULL, 10)
  expect_identical(result$status, 7L)
  expect_identical(result$stderr, "synthetic failure")
})

test_that("each PSOCK worker recreates independent Copilot history", {
  skip_if_not_installed("processx", minimum_version = "3.9.0")
  template <- new_copilot_chat("model", "copilot", 10)
  template$set_system_prompt("template instructions")
  cluster <- parallel::makePSOCKcluster(1)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  results <- parallel::parLapply(cluster, c("series-a", "series-b"), function(combo, llm, create, validate) {
    # load_all() helpers may not exist in a worker's installed namespace.
    environment(create) <- list2env(
      list(check_copilot_string = validate), parent = environment(create)
    )
    session <- create(llm$model, llm$command, llm$timeout)
    before <- length(session$get_turns())
    session$set_system_prompt(combo)
    list(before = before, prompt = session$get_system_prompt(), template = llm$get_system_prompt())
  }, llm = template, create = new_copilot_chat, validate = check_copilot_string)
  expect_identical(vapply(results, function(x) x$before, integer(1)), c(0L, 0L))
  expect_identical(vapply(results, function(x) x$prompt, character(1)), c("series-a", "series-b"))
  expect_true(all(vapply(results, function(x) x$template == "template instructions", logical(1))))
})

test_that("live Copilot returns JSON and preserves conversation context", {
  skip_on_cran()
  skip_if(
    nzchar(Sys.getenv("CI")) || identical(Sys.getenv("GITHUB_ACTIONS"), "true"),
    "Live Copilot tests are local-only"
  )
  skip_if(
    !identical(Sys.getenv("FINNTS_TEST_COPILOT_LIVE"), "true"),
    "Set FINNTS_TEST_COPILOT_LIVE=true to run locally"
  )
  skip_if_not_installed("processx", minimum_version = "3.9.0")
  command <- Sys.getenv("FINNTS_COPILOT_COMMAND", unset = unname(Sys.which("copilot")))
  skip_if(!nzchar(command), "Copilot CLI not installed")
  # Resolve credentials without printing them; configured provider failures fail.
  authenticated <- tryCatch(
    { copilot_auth_env(10); TRUE },
    finnts_copilot_transport_error = function(error) {
      if (startsWith(conditionMessage(error), "No Copilot authentication")) return(FALSE)
      stop(error)
    }
  )
  skip_if_not(authenticated, "Copilot credentials unavailable")
  llm <- chat_copilot(
    model = Sys.getenv("FINNTS_COPILOT_MODEL", unset = "auto"),
    command = command
  )
  llm$set_system_prompt("Follow the user's output format exactly. Do not use tools.")
  response <- llm$chat('Return exactly {"test_value":"synthetic-417"}')
  expect_identical(jsonlite::fromJSON(response)$test_value, "synthetic-417")
  expect_identical(
    trimws(llm$chat("Return only the test_value from your preceding response.")),
    "synthetic-417"
  )
})
