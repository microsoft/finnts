
sanitize_args <- function(arg_list) {
  purrr::imap(arg_list, function(val, nm) {
    if (is.atomic(val)) val else sprintf("<object:%s>", nm)
  })
}

# execute specific tools with retry
execute_node <- function(node, ctx, chat) {
  
  tool_name   <- node$fn
  max_try     <- node$max_retry %||% 0L
  retry_mode  <- node$retry_mode  %||% "llm"   # "llm" (default) or "plain"
  attempt     <- 0L
  registry    <- chat$get_tools()
  
  cli::cli_progress_step(sprintf("🔧 Running %s...", tool_name))
  
  repeat {
    # look up tool and call it
    if (!tool_name %in% names(registry))
      stop(sprintf("Tool '%s' not registered.", tool_name), call. = FALSE)
    
    tool_fn <- registry[[tool_name]]
    result  <- try(do.call(tool_fn@name, ctx$args %||% list()), silent = TRUE)
    
    # success 
    if (!inherits(result, "try-error")) {
      ctx$results[[tool_name]]  <- result
      ctx$attempts[[tool_name]] <- 0L
      return(list(ctx = ctx, ok = TRUE))
    }
    
    # failure bookkeeping 
    attempt <- attempt + 1L
    ctx$attempts[[tool_name]] <- attempt
    ctx$last_error            <- as.character(result)
    
    if (attempt > max_try)
      stop(sprintf("Tool '%s' failed after %d attempt(s):\n%s",
                   tool_name, attempt, result), call. = FALSE)
    
    # Two retry strategies
    if (identical(retry_mode, "plain")) {
      cli::cli_alert_info(
        sprintf("Tool '%s' failed (attempt %d/%d). Let's try again…",
                tool_name, attempt, max_try)
      )
      
      if(tool_name == "reason_inputs") {
        ctx$args$last_error <- ctx$last_error
      }
      
      next                                    # loop again with same args
    }
    
    # LLM-guided retry 
    cli::cli_alert_info(
      sprintf("Tool '%s' failed. Asking LLM to suggest a fix (attempt %d/%d).",
              tool_name, attempt, max_try + 1L),
      "\nError message:\n",
      as.character(result)
    )
    
    prompt <- paste0(
      "The tool '", tool_name, "' failed with this error:\n",
      ctx$last_error, "\n\n",
      "Its last arguments were:\n",
      jsonlite::toJSON(sanitize_args(ctx$args %||% list()),
                       auto_unbox = TRUE), "\n\n",
      "Suggest a valid tool call with ONLY json like:\n",
      '{ "tool": "tool_name", "arguments": { "arg1": ..., "arg2": ... } }'
    )
    
    raw_response <- chat$chat(prompt, echo = FALSE)
    clean_json   <- gsub("(?s)```.*?\\n|\\n```", "", raw_response, perl = TRUE)
    tool_call    <- try(jsonlite::fromJSON(clean_json), silent = TRUE)
    
    if (inherits(tool_call, "try-error") || is.null(tool_call$tool))
      stop("LLM response lacked a valid 'tool' field:\n", raw_response,
           call. = FALSE)
    
    tool_name <- tool_call$tool
    ctx$args  <- tool_call$arguments %||% list()
    
    cli::cli_alert_info(
      sprintf("Retrying with tool '%s' and args: %s",
              tool_name, paste(deparse(ctx$args), collapse = " "))
    )
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
    if (!is.null(node$branch)) {
      br   <- node$branch(ctx)
      ctx  <- br$ctx            # ★ capture the updated context
      ctx$node <- br$`next`
    } else {
      ctx$node <- node$`next`[1]
    }
  }
  
  ctx      # return full run state
}

register_tools <- function(agent_info) {
  # register the tools for the agent
  register_eda_tools(agent_info)
  register_fcst_tools(agent_info)
}

run_agent <- function(agent_info, 
                      max_iter = 3,
                      weighted_mape_goal = 0.03, 
                      parallel_processing = NULL, 
                      inner_parallel = FALSE, 
                      num_cores = NULL) {
  
  # register tools
  register_tools(agent_info)
  
  # run exploratory data analysis
  eda_results <- eda_agent_workflow(agent_info = agent_info, 
                                    parallel_processing = parallel_processing, 
                                    num_cores = num_cores) 
  
  # optimize global models
  message("[agent] 🌎 Starting Global Model Iteration Workflow")
  
  fcst_results <- fcst_agent_workflow(agent_info = agent_info, 
                                      combo = NULL, 
                                      weighted_mape_goal = weighted_mape_goal,
                                      parallel_processing = parallel_processing, 
                                      inner_parallel = inner_parallel,
                                      num_cores = num_cores, 
                                      max_iter = max_iter)
  
  message("[agent] ✅ Agent run completed successfully.")
}
