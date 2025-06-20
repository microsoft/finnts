#' Run the Finn Agent
#'
#' @param agent_info A list containing the agent information, including project info and LLMs.
#' @param max_iter Maximum number of iterations for the global model optimization.
#' @param weighted_mape_goal The goal for the weighted MAPE metric.
#' @param parallel_processing Logical indicating whether to use parallel processing.
#' @param inner_parallel Logical indicating whether to use inner parallel processing.
#' @param num_cores Number of cores to use for parallel processing.
#'
#' @return NULL
#' @export
run_agent <- function(agent_info,
                      max_iter = 3,
                      weighted_mape_goal = 0.03,
                      parallel_processing = NULL,
                      inner_parallel = FALSE,
                      num_cores = NULL) {
  # get project info
  project_info <- agent_info$project_info

  # register tools
  register_tools(agent_info)

  # run exploratory data analysis
  eda_results <- eda_agent_workflow(
    agent_info = agent_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores
  )

  # optimize global models
  message("[agent] 🌎 Starting Global Model Iteration Workflow")

  fcst_results <- fcst_agent_workflow(
    agent_info = agent_info,
    combo = NULL,
    weighted_mape_goal = weighted_mape_goal,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores,
    max_iter = max_iter
  )

  # filter out which time series met the mape goal after global models
  local_combo_list <- get_best_agent_run(agent_info = agent_info) %>%
    dplyr::filter(weighted_mape > weighted_mape_goal) %>%
    dplyr::pull(combo) %>%
    unique()

  # optimize local models
  if (length(local_combo_list) == 0) {
    message("[agent] All time series met the MAPE goal after global models. Skipping local model optimization.")
    return(invisible())
  } else {
    message("[agent] 📊 Starting Local Model Iteration Workflow")

    # parallel setup
    par_info <- par_start(
      run_info            = project_info,
      parallel_processing = parallel_processing,
      num_cores           = num_cores,
      task_length         = length(local_combo_list)
    )

    cl <- par_info$cl
    packages <- par_info$packages
    `%op%` <- par_info$foreach_operator

    # foreach over each combo file
    foreach::foreach(
      x               = local_combo_list,
      .packages       = packages,
      .errorhandling  = "stop",
      .inorder        = FALSE,
      .multicombine   = TRUE
    ) %op%
      {
        message("[agent] 🔄 Running local model optimization for combo: ", x)

        # run the local model workflow
        fcst_agent_workflow(
          agent_info = agent_info,
          combo = hash_data(x),
          weighted_mape_goal = weighted_mape_goal,
          parallel_processing = NULL,
          inner_parallel = inner_parallel,
          num_cores = num_cores,
          max_iter = max_iter
        )
      } %>% base::suppressPackageStartupMessages()

    par_end(cl)
  }

  message("[agent] ✅ Agent run completed successfully.")
}

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

  cli::cli_progress_step(sprintf("🔧 Running %s...", tool_name))

  repeat {
    # look up tool and call it
    if (!tool_name %in% names(registry)) {
      stop(sprintf("Tool '%s' not registered.", tool_name), call. = FALSE)
    }

    tool_fn <- registry[[tool_name]]
    result <- try(do.call(tool_fn@name, ctx$args %||% list()), silent = TRUE)

    # success
    if (!inherits(result, "try-error")) {
      ctx$results[[tool_name]] <- result
      ctx$attempts[[tool_name]] <- 0L
      return(list(ctx = ctx, ok = TRUE))
    }

    # failure bookkeeping
    attempt <- attempt + 1L
    ctx$attempts[[tool_name]] <- attempt
    ctx$last_error <- as.character(result)

    if (attempt > max_try) {
      stop(sprintf(
        "Tool '%s' failed after %d attempt(s):\n%s",
        tool_name, attempt, result
      ), call. = FALSE)
    }

    # Two retry strategies
    if (identical(retry_mode, "plain")) {
      cli::cli_alert_info(
        sprintf(
          "Tool '%s' failed (attempt %d/%d). Let's try again…",
          tool_name, attempt, max_try
        )
      )

      if (tool_name == "reason_inputs") {
        ctx$args$last_error <- paste0(ctx$args$last_error, ", ", ctx$last_error)
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
      as.character(result)
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
