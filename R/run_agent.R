
sanitize_args <- function(arg_list) {
  purrr::imap(arg_list, function(val, nm) {
    if (is.atomic(val)) val else sprintf("<object:%s>", nm)
  })
}

# execute specific tools with retry
execute_node <- function(node, ctx, chat) {
  # ──────────────────────────────────────────────────────────────────────────
  # Executes a workflow node (tool) with optional retry and LLM-based fix.
  # - If the tool runs successfully: store result and return.
  # - If it fails, retry up to `max_retry` times with LLM-suggested fixes.
  # - Tool must be registered via chat$register_tool(tool(...)).
  #
  # Args:
  #   node: a list from the workflow (contains fn, max_retry, etc.)
  #   ctx:  mutable state list (will be updated)
  #   chat: ellmer chat object (openai or local)
  #
  # Returns:
  #   list(ctx = ..., ok = TRUE)
  # ──────────────────────────────────────────────────────────────────────────
  
  tool_name  <- node$fn
  max_try    <- node$max_retry %||% 0L
  attempt    <- 0L
  registry   <- chat$get_tools()   # names of all registered tools
  
  cli::cli_progress_step(sprintf("🔧 Running %s...", tool_name))
  
  repeat {
    
    # Build and run the tool function (if registered)
    if (!tool_name %in% names(registry)) {
      stop(sprintf("Tool '%s' is not registered. Check workflow or LLM suggestion.", tool_name), call. = FALSE)
    }
    tool_fn <- registry[[tool_name]]
    result  <- try(do.call(tool_fn@name, ctx$args %||% list()), silent = TRUE)

    # ── Success path ─────────────────────────────────────────────
    if (!inherits(result, "try-error")) {
      ctx$results[[tool_name]] <- result
      ctx$attempts[[tool_name]] <- 0L
      return(list(ctx = ctx, ok = TRUE))
    }

    # ── Failure path ─────────────────────────────────────────────
    attempt <- attempt + 1L
    ctx$attempts[[tool_name]] <- attempt
    ctx$last_error <- as.character(result)

    if (attempt > max_try) {
      stop(sprintf("Tool '%s' failed after %d attempt(s):\n%s",
                   tool_name, attempt, result), call. = FALSE)
    }

    cli::cli_alert_info(
      sprintf("Tool '%s' failed. Asking LLM to suggest a fix (attempt %d/%d).",
              tool_name, attempt, max_try + 1L),
      "\nError message:\n",
      as.character(result)
    )
    
    # ── Ask the LLM for a new tool call ─────────────────────────
    prompt <- paste0(
      "The tool '", tool_name, "' failed with this error:\n",
      ctx$last_error, "\n\n",
      "Its last arguments were:\n",
      jsonlite::toJSON(sanitize_args(ctx$args %||% list()), auto_unbox = TRUE), "\n\n",
      "Suggest a valid tool call with ONLY json like this:\n",
      '```json\n{\"tool\": \"add_one\", \"arguments\": {"agent_info": ..., "arg2": ...}}\n```'
    )

    raw_response <- chat$chat(prompt, echo = FALSE)

    # ── Clean and parse JSON from model ─────────────────────────
    clean_json <- gsub("(?s)```.*?\\n|\\n```", "", raw_response, perl = TRUE)
    tool_call <- try(jsonlite::fromJSON(clean_json), silent = TRUE)
    
    if (inherits(tool_call, "try-error") || is.null(tool_call$tool)) {
      stop("LLM response could not be parsed or lacked a valid 'tool' field:\n", raw_response, call. = FALSE)
    }
    
    # ── Update tool and args based on LLM suggestion ────────────
    tool_name    <- tool_call$tool
    ctx$args     <- tool_call$arguments %||% list()
    
    cli::cli_alert_info(sprintf("Retrying with tool '%s' and args: %s",
                                tool_name,
                                paste(deparse(ctx$args), collapse = "")))
  }
}


# agent graph
run_graph <- function(chat, 
                      workflow,
                      init_ctx   = list(node = "start")) {
  
  ctx <- init_ctx
  
  # iterate through the workflow nodes
  repeat {
    # ── safety: unknown node? ───────────────────────────
    node <- workflow[[ctx$node]]
    if (is.null(node))
      stop("Unknown node: ", ctx$node, call. = FALSE)
    
    # ── termination ────────────────────────────────────
    if (ctx$node == "stop") break
    
    # ── LLM-decision node ──────────────────────────────
    if (identical(node$fn, "llm_decide")) {
      decision <- chat$chat(
        glue::glue(
          "Context: {jsonlite::toJSON(ctx, auto_unbox=TRUE)}\n",
          "You can choose one of: {paste(node$`next`, collapse=', ')}\n",
          "Reply ONLY with the chosen node name.")
      )
      ctx$node <- trimws(decision$content)
      next
    }
    
    # ── normal tool node ───────────────────────────────
    resolve_args <- function(arg_template, ctx) {
      
      if (is.null(arg_template)) return(list())
      
      # 
      # Build a data-mask so rlang/glue can safely evaluate curly expressions.
      # flatten_ctx() makes every nested element of `ctx` accessible by name.
      # 
      mask_env <- rlang::env(parent = emptyenv())
      
      flatten_ctx <- function(x, env) {
        if (is.list(x)) {
          purrr::imap(x, function(val, nm) {
            assign(nm, val, envir = env)
            flatten_ctx(val, env)            # recurse so results$foo -> foo
          })
        }
      }
      flatten_ctx(ctx, mask_env)
      mask <- rlang::new_data_mask(mask_env)   # << key line (no warning)
      
      # 
      walk_template <- function(x) {
        
        # recurse into sub-lists first
        if (is.list(x))  return(purrr::modify(x, walk_template))
        if (!is.character(x) || length(x) != 1) return(x)
        
        # ------ pure { expr } 
        if (stringr::str_detect(x, "^\\{[^{}]+\\}$")) {
          expr <- stringr::str_sub(x, 2, -2)          # remove outer braces
          out  <- tryCatch(
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
        
        # ------ mixed glue string 
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
    
    ctx$args <- resolve_args(node$args, ctx)
    res      <- execute_node(node, ctx, chat)
    ctx      <- res$ctx
    
    # ── choose next step ───────────────────────────────
    ctx$node <- if (!is.null(node$branch)) {
      node$branch(ctx)
    } else {
      node$`next`[1]
    }
    
    # ── checkpoint ─────────────────────────────────────
    ###
  }
  
  ctx      # return full run state
}

register_tools <- function(agent_info) {
  # register the tools for the agent
  register_eda_tools(agent_info)
  register_fcst_tools(agent_info)
}

run_agent <- function(agent_info, 
                      parallel_processing = NULL, 
                      inner_parallel = FALSE, 
                      num_cores = NULL) {
  
  # register tools
  register_tools(agent_info)
  
  # run exploratory data analysis
  eda_results <- eda_agent_workflow(agent_info = agent_info, 
                                    parallel_processing = parallel_processing, 
                                    num_cores = num_cores) 
  
  # run the forecast iteration workflow
  fcst_results <- fcst_agent_workflow(agent_info = agent_info, 
                                      parallel_processing = parallel_processing, 
                                      inner_parallel = inner_parallel,
                                      num_cores = num_cores)
  
  message("[agent] ✅ Agent run completed successfully.")
}
