#' Use GitHub Copilot CLI as the Finn Agent LLM
#'
#' Creates a serializable configuration template for isolated Finn Agent
#' sessions. Unlike an ellmer Chat, this adapter replays system instructions
#' and conversation history as text on each request. Finn remains responsible
#' for tools, proposal validation, and retries.
#'
#' Requires GitHub Copilot CLI 1.0.93 or later, an entitled GitHub account, and
#' the optional \pkg{processx} package. Authentication uses
#' `COPILOT_GITHUB_TOKEN`, `GH_TOKEN`, or `GITHUB_TOKEN` (in that order), or
#' credentials retrieved privately with `gh auth token`. Tokens are resolved
#' in the process making the request, not stored in the template.
#'
#' Every request uses a new CLI home and working directory under `tempdir()`.
#' The CLI saves local configuration, transcripts, and session state there;
#' these can contain sensitive prompt and response data.
#' Only use data approved for GitHub Copilot.
#' Repository instructions, hooks, IDE connections, built-in MCP servers, and
#' model tools are disabled. Cached Copilot-only logins are not copied into
#' the isolated home; use an environment token or `gh auth login` instead.
#'
#' @param model A nonempty Copilot model identifier available to your account.
#'   Defaults to `"auto"`, letting Copilot select the model for each request.
#' @param command Copilot executable name or path. Defaults to `"copilot"`.
#'   On Windows, prefer the standalone `copilot.exe` rather than an npm
#'   `.cmd` shim.
#' @param timeout Positive finite timeout in seconds for each CLI request.
#'
#' @return A `finnts_copilot_chat` environment with `$chat(prompt, echo = FALSE)`,
#'   `$set_system_prompt(prompt)`, `$get_system_prompt()`, `$get_model()`, and
#'   `$get_turns()` methods. `$chat()` returns one character string. Successful
#'   exchanges update in-memory history; failed requests do not.
#' @examples
#' \dontrun{
#' llm <- chat_copilot()
#' agent <- set_agent_info(
#'   project_info = project, llm = llm,
#'   input_data = hist_data, forecast_horizon = 6
#' )
#' }
#' @export
chat_copilot <- function(model = "auto", command = "copilot", timeout = 120) {
  check_copilot_string(model, "model")
  check_copilot_string(command, "command")
  if (!is.numeric(timeout) || length(timeout) != 1 ||
    is.na(timeout) || !is.finite(timeout) || timeout <= 0) {
    stop("`timeout` must be one positive finite number of seconds.", call. = FALSE)
  }
  check_copilot_runtime(command, timeout)
  new_copilot_chat(model, command, timeout)
}

# Validate a single nonblank configuration string or prompt; returns invisibly
# and errors before spawning a process for invalid input.
check_copilot_string <- function(value, name) {
  if (!is.character(value) || length(value) != 1 ||
    is.na(value) || !nzchar(trimws(value))) {
    stop("`", name, "` must be one nonempty character string.", call. = FALSE)
  }
  invisible(NULL)
}

# Construct an independent session from serializable configuration only.
# Method closures own this session's prompt and turns, never another session's.
new_copilot_chat <- function(model, command, timeout) {
  session <- new.env(parent = emptyenv())
  class(session) <- "finnts_copilot_chat"
  session$model <- model
  session$command <- command
  session$timeout <- timeout
  session$system_prompt <- NULL
  session$turns <- list()
  session$get_model <- function() session$model
  session$get_system_prompt <- function() session$system_prompt
  session$get_turns <- function() session$turns
  # Set or clear instructions for subsequent requests; return the session.
  session$set_system_prompt <- function(prompt) {
    if (!is.null(prompt)) check_copilot_string(prompt, "system_prompt")
    session$system_prompt <- prompt
    invisible(session)
  }
  # Send one text prompt, committing both turns only after transport success.
  session$chat <- function(prompt, echo = FALSE) {
    check_copilot_string(prompt, "prompt")
    if (!is.logical(echo) || length(echo) != 1 || is.na(echo)) {
      stop("`echo` must be TRUE or FALSE.", call. = FALSE)
    }
    turns <- c(session$turns, list(list(role = "user", content = prompt)))
    payload <- paste0(
      "You are the language-model backend for FinnTS. Do not use tools. ",
      "The JSON below represents a conversation. Follow its system_prompt ",
      "as instructions, use messages as conversation history, and answer ",
      "only the final user message in its requested format. ",
      "Do not describe this envelope or add CLI commentary.\n",
      jsonlite::toJSON(
        list(system_prompt = session$system_prompt, messages = turns),
        auto_unbox = TRUE, null = "null"
      )
    )
    response <- request_copilot(session$command, session$model, session$timeout, payload)
    session$turns <- c(turns, list(list(role = "assistant", content = response)))
    if (echo) cat(response, "\n")
    response
  }
  session
}

# Resolve an executable without a shell. Reject Windows batch shims, whose
# quoting and stdin behavior differ from native executables.
resolve_copilot_command <- function(command) {
  resolved <- unname(Sys.which(command))
  if (!nzchar(resolved) && file.exists(command) && !dir.exists(command)) {
    resolved <- normalizePath(command, mustWork = TRUE)
  }
  if (!nzchar(resolved)) {
    stop(
      "GitHub Copilot CLI was not found. Install it and put `copilot` on PATH, ",
      "or supply its executable path to chat_copilot(command = ...).",
      call. = FALSE
    )
  }
  if (.Platform$OS.type == "windows" &&
    grepl("\\.(cmd|bat)$", resolved, ignore.case = TRUE)) {
    stop("Use the standalone copilot.exe, not a Windows .cmd or .bat shim.", call. = FALSE)
  }
  resolved
}

# Successful inspections only, held in memory and never in chat templates.
copilot_runtime_cache <- new.env(parent = emptyenv())

# Verify dependencies, CLI version and flags; reuse successful inspections only
# for the same process, resolved path, and executable size/change timestamps.
# Workers must inspect independently, even if a cache was serialized to them.
# No authentication or model request is made; failures are never cached.
check_copilot_runtime <- function(command, timeout) {
  if (!requireNamespace("processx", quietly = TRUE) ||
    utils::compareVersion(as.character(utils::packageVersion("processx")), "3.9.0") < 0) {
    stop(
      "Copilot Agent workflows require 'processx' version 3.9.0 or later. ",
      "Install or upgrade it with install.packages(\"processx\").",
      call. = FALSE
    )
  }
  executable <- resolve_copilot_command(command)
  identity <- file.info(executable)[, c("size", "mtime", "ctime"), drop = FALSE]
  cacheable <- !anyNA(identity)
  key <- paste(Sys.getpid(), executable, sep = ":")
  if (cacheable && identical(copilot_runtime_cache[[key]], identity)) {
    return(invisible(executable))
  }
  # Capture help/version only; suppress processx errors that can expose args.
  inspect_cli <- function(flag) {
    result <- tryCatch(
      processx::run(
        executable, c("--no-auto-update", flag), timeout = min(timeout, 30),
        error_on_status = FALSE, cleanup_tree = TRUE,
        env = c(COPILOT_AUTO_UPDATE = "false")
      ),
      error = function(error) {
        abort_copilot_transport("Could not inspect Copilot CLI. Check its executable and permissions.")
      }
    )
    if (result$status != 0) {
      abort_copilot_transport("Copilot CLI inspection failed. Install GitHub Copilot CLI 1.0.93 or later.")
    }
    result$stdout
  }
  version <- inspect_cli("--version")
  match <- regmatches(version, regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", version))
  if (length(match) != 1 ||
    utils::compareVersion(match, "1.0.93") < 0) {
    stop("Copilot Agent workflows require GitHub Copilot CLI 1.0.93 or later.", call. = FALSE)
  }
  help <- inspect_cli("--help")
  flags <- copilot_cli_args("model")
  flags <- sub("=.*$", "", flags)
  missing <- flags[!vapply(flags, grepl, logical(1), x = help, fixed = TRUE)]
  if (length(missing)) {
    stop(
      "Copilot CLI lacks required options: ", paste(missing, collapse = ", "),
      ". Install a compatible GitHub Copilot CLI.",
      call. = FALSE
    )
  }
  if (cacheable) copilot_runtime_cache[[key]] <- identity
  invisible(executable)
}

# Fixed, least-privilege arguments. An empty available-tools list disables all
# model tools; hooks and IDE connections are disabled separately in settings.
copilot_cli_args <- function(model) {
  c(
    "--available-tools=", "--disable-builtin-mcps",
    "--no-custom-instructions", "--no-ask-user", "--no-auto-update",
    "--no-experimental", "--no-remote", "--no-remote-export", "--no-bash-env",
    "--stream=off", "--silent", "--output-format=json", "--log-level=none",
    paste0("--model=", model)
  )
}

# Resolve authentication privately for this request. Environment tokens take
# precedence over gh's existing account; no token enters the serializable chat.
copilot_auth_env <- function(timeout) {
  variables <- c("COPILOT_GITHUB_TOKEN", "GH_TOKEN", "GITHUB_TOKEN")
  tokens <- Sys.getenv(variables)
  present <- which(nzchar(trimws(tokens)))
  if (length(present)) {
    return(c(COPILOT_GITHUB_TOKEN = unname(tokens[[present[[1]]]])))
  }
  gh <- unname(Sys.which("gh"))
  if (nzchar(gh)) {
    host <- Sys.getenv("COPILOT_GH_HOST", unset = Sys.getenv("GH_HOST"))
    args <- c("auth", "token", if (nzchar(host)) c("--hostname", host))
    result <- tryCatch(
      processx::run(
        gh, args, error_on_status = FALSE, timeout = min(timeout, 30),
        cleanup_tree = TRUE
      ),
      error = function(error) {
        abort_copilot_transport("Could not read GitHub CLI credentials. Run `gh auth login` or set COPILOT_GITHUB_TOKEN.")
      }
    )
    if (result$status == 0 && nzchar(trimws(result$stdout))) {
      return(c(COPILOT_GITHUB_TOKEN = trimws(result$stdout)))
    }
  }
  abort_copilot_transport(
    paste0(
      "No Copilot authentication is available. Set COPILOT_GITHUB_TOKEN ",
      "(or GH_TOKEN/GITHUB_TOKEN), or run `gh auth login`. ",
      "The account needs Copilot access; cached Copilot-only logins are not reused."
    )
  )
}

# Raise a hard provider/transport error without including prompts, tokens or
# raw subprocess diagnostics in the condition. Never a proposal-exhaustion error.
abort_copilot_transport <- function(message) {
  stop(structure(
    list(message = message, call = NULL),
    class = c("finnts_copilot_transport_error", "error", "condition")
  ))
}

# Start one CLI in an isolated temporary home, saving only CLI-local state.
# Instructions/history travel through stdin, not argv or a prompt file.
request_copilot <- function(command, model, timeout, prompt) {
  executable <- resolve_copilot_command(command)
  auth <- copilot_auth_env(timeout)
  home <- tempfile("finnts-copilot-")
  wd <- file.path(home, "work")
  if (!dir.create(wd, recursive = TRUE)) {
    abort_copilot_transport("Could not create the isolated Copilot working directory.")
  }
  jsonlite::write_json(
    list(disableAllHooks = TRUE, memory = FALSE, ide = list(autoConnect = FALSE)),
    file.path(home, "settings.json"), auto_unbox = TRUE
  )
  provider_vars <- grep("^COPILOT_PROVIDER_", names(Sys.getenv()), value = TRUE)
  env <- c(
    auth, COPILOT_HOME = home, COPILOT_ALLOW_ALL = "false",
    COPILOT_AUTO_UPDATE = "false", COPILOT_CUSTOM_INSTRUCTIONS_DIRS = "",
    USE_TGREP = "false", stats::setNames(rep(NA_character_, length(provider_vars)), provider_vars)
  )
  result <- run_copilot_process(executable, copilot_cli_args(model), prompt, wd, env, timeout)
  if (result$status != 0) {
    abort_copilot_transport(paste0(
      "Copilot CLI failed with exit status ", result$status,
      ". Verify GitHub authentication, Copilot entitlement, model availability, ",
      "and network connectivity. Raw diagnostics are withheld to protect prompt data and credentials."
    ))
  }
  parse_copilot_response(result$stdout)
}

# Drain stdout/stderr while writing nonblocking stdin, closing it at EOF.
# The deadline covers process startup, input and output. Kill only this process
# tree on timeout, interruption or error; retain CLI-local files for OS cleanup.
run_copilot_process <- function(command, args, input, wd, env, timeout) {
  start <- proc.time()[["elapsed"]]
  process <- tryCatch(
    processx::process$new(
      command, args, stdin = "|", stdout = "|", stderr = "|",
      wd = wd, env = env, encoding = "UTF-8", cleanup_tree = TRUE,
      windows_hide_window = TRUE
    ),
    error = function(error) {
      abort_copilot_transport("Could not start Copilot CLI. Check its executable and permissions.")
    }
  )
  on.exit(process$kill_tree(), add = TRUE)
  pending <- charToRaw(enc2utf8(paste0(input, "\n")))
  closed <- FALSE
  stdout <- stderr <- list()
  repeat {
    if (proc.time()[["elapsed"]] - start >= timeout) {
      abort_copilot_transport(paste0("Copilot CLI timed out after ", timeout, " seconds."))
    }
    tryCatch(
      {
        if (length(pending) && process$is_alive()) pending <- process$write_input(pending)
        if (!length(pending) && !closed) {
          processx::processx_conn_close(process$get_input_connection())
          closed <- TRUE
        }
        process$poll_io(50)
        out <- process$read_output()
        err <- process$read_error()
        if (nzchar(out)) stdout[[length(stdout) + 1L]] <- out
        if (nzchar(err)) stderr[[length(stderr) + 1L]] <- err
      },
      error = function(error) {
        abort_copilot_transport("Copilot CLI pipe communication failed. Check CLI compatibility and authentication.")
      }
    )
    if (!process$is_alive() &&
      !process$is_incomplete_output() && !process$is_incomplete_error()) break
  }
  list(
    status = process$get_exit_status(),
    stdout = paste0(unlist(stdout), collapse = ""),
    stderr = paste0(unlist(stderr), collapse = "")
  )
}

# Parse the verified CLI JSONL envelope, not the model's requested JSON schema.
# Require a successful terminal result and exactly one nonblank text response;
# reject tool requests/execution and session errors even with process exit zero.
parse_copilot_response <- function(output) {
  lines <- strsplit(output, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  events <- lapply(lines, function(line) {
    event <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(error) {
        abort_copilot_transport("Copilot CLI returned malformed JSONL output.")
      }
    )
    if (!is.list(event) || !is.character(event$type) || length(event$type) != 1) {
      abort_copilot_transport("Copilot CLI returned an invalid event envelope.")
    }
    event
  })
  types <- vapply(events, function(event) event$type, character(1))
  if (any(types == "session.error") || any(startsWith(types, "tool."))) {
    abort_copilot_transport("Copilot CLI reported a session error or attempted tool execution.")
  }
  results <- events[types == "result"]
  if (length(results) != 1 || utils::tail(types, 1) != "result" ||
    !identical(results[[1]]$exitCode, 0L)) {
    abort_copilot_transport("Copilot CLI did not return a successful terminal result.")
  }
  messages <- events[types == "assistant.message"]
  if (length(messages) != 1) {
    abort_copilot_transport("Copilot CLI must return exactly one assistant message.")
  }
  data <- messages[[1]]$data
  if (!is.list(data)) {
    abort_copilot_transport("Copilot CLI returned an invalid assistant message envelope.")
  }
  if (length(data$toolRequests)) {
    abort_copilot_transport("Copilot CLI attempted a tool request despite disabled tools.")
  }
  content <- data$content
  if (!is.character(content) || length(content) != 1 ||
    is.na(content) || !nzchar(trimws(content))) {
    abort_copilot_transport("Copilot CLI returned an empty or invalid assistant response.")
  }
  content
}

# Check supported templates without imposing ellmer on a Copilot workflow.
check_agent_llm <- function(llm) {
  check_input_type("llm", llm, c("Chat", "finnts_copilot_chat"))
  invisible(NULL)
}

# Normalize text/list chat responses for graph routing, proposal retries, and
# Q&A. Preserve plain character responses and explicitly reject invalid shapes.
agent_chat_text <- function(response) {
  if (is.list(response) && !is.null(response$content)) response <- response$content
  if (!is.character(response) || length(response) != 1 || is.na(response)) {
    stop("The LLM must return one character response.", call. = FALSE)
  }
  as.character(response)
}
