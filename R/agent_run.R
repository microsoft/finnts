#' Run the agent workflow graph
#'
#' @param chat A chat object for interacting with the LLM.
#' @param workflow A list representing the workflow graph.
#' @param init_ctx Initial context for the workflow.
#'
#' @return The final context after running the workflow.
#' @noRd
run_graph <- function(chat,
                      workflow,
                      init_ctx = list(node = "start")) {
  # get context
  ctx <- init_ctx

  # iterate through the workflow nodes
  repeat {
    # safety: unknown node?
    node <- workflow[[ctx$node]]
    if (is.null(node)) {
      stop("Unknown node: ", ctx$node, call. = FALSE)
    }

    # termination
    if (ctx$node == "stop") break

    # LLM-decision node
    if (identical(node$fn, "llm_decide")) {
      decision <- chat$chat(
        glue::glue(
          "Context: {jsonlite::toJSON(ctx, auto_unbox=TRUE)}\n",
          "You can choose one of: {paste(node$`next`, collapse=', ')}\n",
          "Reply ONLY with the chosen node name."
        )
      )
      ctx$node <- trimws(decision$content)
      next
    }

    # normal tool node
    resolve_args <- function(arg_template, ctx) {
      # if no args, return empty list
      if (is.null(arg_template)) {
        return(list())
      }

      # Build a data-mask so rlang/glue can safely evaluate curly expressions.
      # flatten_ctx() makes every nested element of `ctx` accessible by name.
      mask_env <- rlang::env(parent = emptyenv())

      flatten_ctx <- function(x, env) {
        if (is.list(x)) {
          purrr::imap(x, function(val, nm) {
            assign(nm, val, envir = env)
            flatten_ctx(val, env) # recurse so results$foo -> foo
          })
        }
      }
      flatten_ctx(ctx, mask_env)
      mask <- rlang::new_data_mask(mask_env) # << key line (no warning)

      # walk through the template and resolve expressions
      walk_template <- function(x) {
        # recurse into sub-lists first
        if (is.list(x)) {
          return(purrr::modify(x, walk_template))
        }
        if (!is.character(x) || length(x) != 1) {
          return(x)
        }

        # eval simple expressions
        if (stringr::str_detect(x, "^\\{[^{}]+\\}$")) {
          expr <- stringr::str_sub(x, 2, -2) # remove outer braces
          out <- tryCatch(
            rlang::eval_tidy(rlang::parse_expr(expr), mask),
            error = function(e) {
              stop(
                sprintf("Failed to eval '%s': %s", expr, conditionMessage(e)),
                call. = FALSE
              )
            }
          )
          return(out)
        }

        # mixed glue string
        val <- glue::glue_data(
          mask,
          x,
          .open  = "{",
          .close = "}"
        )
        if (identical(val, "NULL")) NULL else as.character(val)
      }

      purrr::modify(arg_template, walk_template)
    }

    # run the node
    ctx$args <- resolve_args(node$args, ctx)
    res <- execute_node(node, ctx, chat)
    ctx <- res$ctx

    # prevent arg-bleed into the next node's templates
    ctx$args <- NULL # clears retry-modified args

    # choose next step
    if (!is.null(node$branch)) {
      # branch node: use the branch function to get next node
      br <- node$branch(ctx)
      ctx <- br$ctx
      ctx$node <- br$`next`
    } else {
      # normal node: just go to the next node
      ctx$node <- node$`next`[1]
    }
  }

  return(ctx)
}

#' Execute a node in the workflow
#'
#' @param node The node to execute.
#' @param ctx The current context containing arguments and results.
#' @param chat The chat object for interacting with the LLM.
#'
#' @return A list containing the updated context and a success flag.
#' @noRd
execute_node <- function(node, ctx, chat) {
  # get tool name and retry settings
  tool_name <- node$fn
  max_try <- node$max_retry %||% 0L
  retry_mode <- node$retry_mode %||% "llm" # "llm" (default) or "plain"
  attempt <- 0L
  registry <- chat$get_tools()

  cli::cli_progress_step(sprintf("Running %s...", tool_name))

  repeat {
    # look up tool and call it
    if (!tool_name %in% names(registry)) {
      stop(sprintf("Tool '%s' not registered.", tool_name), call. = FALSE)
    }

    tool_fn <- registry[[tool_name]]

    # call the function object directly
    try(doParallel::stopImplicitCluster(), silent = TRUE)
    try(foreach::registerDoSEQ(), silent = TRUE)

    err <- NULL
    result <- tryCatch(
      rlang::exec(tool_fn@fun, !!!(ctx$args %||% list())),
      error = function(e) {
        err <<- e
        NULL
      }
    )
    if (!is.null(err)) {
      print(err)
    }
    # success
    if (is.null(err)) {
      ctx$results[[tool_name]] <- result
      ctx$attempts[[tool_name]] <- 0L
      return(list(ctx = ctx, ok = TRUE))
    }

    # pause for 5 seconds
    Sys.sleep(5)

    # failure bookkeeping
    attempt <- attempt + 1L
    ctx$attempts[[tool_name]] <- attempt
    ctx$last_error <- as.character(err)

    if (attempt > max_try) {
      stop(sprintf(
        "Tool '%s' failed after %d attempt(s):\n%s",
        tool_name, attempt, err
      ), call. = FALSE)
    }

    # Two retry strategies
    if (identical(retry_mode, "plain")) {
      cli::cli_alert_info(
        sprintf(
          "Tool '%s' failed (attempt %d/%d). Let's try again...",
          tool_name, attempt, max_try + 1L
        )
      )

      # pass LLM error to next run of reason inputs tool call
      if (tool_name == "reason_inputs") {
        ctx$args$last_error <- paste0(ctx$args$last_error, ", ", ctx$last_error)
      }

      # SPECIAL FAILOVER: update_local_models spark -> local_machine
      if (tool_name == "update_local_models" &&
        attempt == 3L &&
        is.character(ctx$args$parallel_processing) &&
        identical(tolower(ctx$args$parallel_processing), "spark")) {
        cli::cli_alert_info(
          "Failover: 'update_local_models' failed with Spark. Switching parallel_processing to 'local_machine' for the next retry."
        )

        # Flip the arg for the next loop iteration
        ctx$args$parallel_processing <- "local_machine"
        ctx$args$inner_parallel <- FALSE
      }

      next # loop again with same args
    }

    # LLM-guided retry
    cli::cli_alert_info(
      sprintf(
        "Tool '%s' failed. Asking LLM to suggest a fix (attempt %d/%d).",
        tool_name, attempt, max_try + 1L
      ),
      "\nError message:\n",
      as.character(err)
    )

    prompt <- paste0(
      "The tool '", tool_name, "' failed with this error:\n",
      ctx$last_error, "\n\n",
      "Its last arguments were:\n",
      jsonlite::toJSON(sanitize_args(ctx$args %||% list()),
        auto_unbox = TRUE
      ), "\n\n",
      "Suggest a valid tool call with ONLY json like:\n",
      '{ "tool": "tool_name", "arguments": { "arg1": ..., "arg2": ... } }'
    )

    raw_response <- chat$chat(prompt, echo = FALSE)
    clean_json <- gsub("(?s)```.*?\\n|\\n```", "", raw_response, perl = TRUE)
    tool_call <- try(jsonlite::fromJSON(clean_json), silent = TRUE)

    if (inherits(tool_call, "try-error") || is.null(tool_call$tool)) {
      stop("LLM response lacked a valid 'tool' field:\n", raw_response,
        call. = FALSE
      )
    }

    tool_name <- tool_call$tool
    ctx$args <- tool_call$arguments %||% list()

    cli::cli_alert_info(
      sprintf(
        "Retrying with tool '%s' and args: %s",
        tool_name, paste(deparse(ctx$args), collapse = " ")
      )
    )
  }
}

#' Register tools for the agent
#'
#' @param agent_info A list containing the agent information, including project info and LLMs.
#'
#' @return NULL
#' @noRd
register_tools <- function(agent_info) {
  # register the tools for the agent
  register_eda_tools(agent_info)
  register_fcst_tools(agent_info)
}

#' Sanitize arguments for display
#'
#' @param arg_list A list of arguments to sanitize.
#'
#' @return A sanitized list where atomic values are kept as is, and non-atomic values are replaced with a placeholder.
#' @noRd
sanitize_args <- function(arg_list) {
  purrr::imap(arg_list, function(val, nm) {
    if (is.atomic(val)) val else sprintf("<object:%s>", nm)
  })
}
