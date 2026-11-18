# Require the public ellmer cloning API for ellmer-backed sessions only.
# Returns invisibly; missing/older optional ellmer installations are errors.
check_agent_ellmer_version <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE) ||
    utils::compareVersion(as.character(utils::packageVersion("ellmer")), "0.4.0") < 0) {
    stop(
      "Agent workflows require the 'ellmer' package version 0.4.0 or later. Install or upgrade it with install.packages(\"ellmer\").",
      call. = FALSE
    )
  }

  invisible(NULL)
}

# Create a clean workflow-local session from an immutable provider template.
# Copilot configuration is recreated in workers; ellmer uses public deep cloning.
new_llm_session <- function(llm) {
  if (inherits(llm, "finnts_copilot_chat")) {
    check_copilot_runtime(llm$command, llm$timeout)
    return(new_copilot_chat(llm$model, llm$command, llm$timeout))
  }
  check_agent_ellmer_version()

  session <- llm$clone(deep = TRUE)
  session$set_system_prompt(NULL)
  session$set_turns(list())
  session
}

has_completed_iteration_target <- function(best_run_tbl, max_iter) {
  required_columns <- c(
    "combo", "model_type", "weighted_mape", "run_complete", "max_iterations"
  )
  if (!is.data.frame(best_run_tbl) ||
    !all(required_columns %in% colnames(best_run_tbl))) {
    return(FALSE)
  }

  combo <- trimws(as.character(best_run_tbl$combo))
  model_type <- as.character(best_run_tbl$model_type)
  weighted_mape <- suppressWarnings(as.numeric(as.character(best_run_tbl$weighted_mape)))
  run_complete <- suppressWarnings(as.logical(as.character(best_run_tbl$run_complete)))
  max_iterations <- suppressWarnings(as.numeric(as.character(best_run_tbl$max_iterations)))

  if (any(is.na(combo) | !nzchar(combo)) ||
    any(is.na(model_type) | !model_type %in% c("global", "local")) ||
    any(!is.finite(weighted_mape) | weighted_mape < 0)) {
    return(FALSE)
  }

  any(
    run_complete %in% TRUE &
      is.finite(max_iterations) &
      max_iterations >= max_iter
  )
}

resolve_agent_global_forecast_approaches <- function(agent_info, eda_results) {
  required_columns <- c("Analysis_Type", "Metric", "Value")
  if (!is.data.frame(eda_results) ||
    !all(required_columns %in% names(eda_results))) {
    stop(
      "Error in iterate_forecast(). Expected exactly one hierarchy result in consolidated EDA data.",
      call. = FALSE
    )
  }

  hierarchy_results <- eda_results %>%
    dplyr::filter(
      .data$Analysis_Type == "Hierarchy",
      .data$Metric == "hierarchy_type"
    )

  if (nrow(hierarchy_results) != 1) {
    stop(
      "Error in iterate_forecast(). Expected exactly one hierarchy result in consolidated EDA data.",
      call. = FALSE
    )
  }

  hierarchy_type <- as.character(hierarchy_results$Value[[1]])
  if (is.na(hierarchy_type) ||
    !hierarchy_type %in% c("none", "standard", "grouped")) {
    stop(
      "Error in iterate_forecast(). Found unsupported hierarchy type in consolidated EDA data: ",
      hierarchy_type,
      ". Expected one of: none, standard, grouped.",
      call. = FALSE
    )
  }

  if (!identical(agent_info$forecast_approach, "bottoms_up")) {
    return("bottoms_up")
  }

  hierarchy_approach <- switch(hierarchy_type,
    none = NULL,
    standard = "standard_hierarchy",
    grouped = "grouped_hierarchy"
  )
  c("bottoms_up", hierarchy_approach)
}

#' Run the Finn Agent Forecast Iteration Process
#'
#' This function orchestrates the forecast iteration process for a Finn agent, including exploratory data analysis,
#'
#' @details Future quality is evaluated when [final_models()] selects the winner
#'   within each iteration, including usable least-issues fallbacks with warnings.
#'   Parallel fallback warnings are relayed to the caller without saved fields.
#'   Iteration ranking starts from the earliest minimum
#'   WMAPE and may prefer a later eligible iteration within 10 percent relative
#'   WMAPE when its average model WMAPE is strictly lower. Local mean, median, and
#'   standard deviation summarize individual-model backtests, excluding simple
#'   averages. This retains evidence that a setting improves other models even if
#'   the current best model does not improve. Global summary fields retain their
#'   original meaning: run WMAPE for mean and median, and zero standard deviation.
#'   Agent comparisons and stopping use WMAPE rounded to four decimal places.
#'   Search-context selection does not overwrite a better saved local forecast.
#'   Global promotion applies to one complete iteration, and all saved global
#'   winners must reference that same iteration. Individual models and averages
#'   may differ within it. Mixed global iteration metadata is rejected before
#'   publication or update; interrupted writes are not treated as successful.
#'   Normal accuracy-goal stopping requires a complete eligible result and finite WMAPE, not another
#'   soft-quality check. A winner with soft concerns may therefore beat an earlier
#'   winner on accuracy. Loaded backtests, run history, and best-run metrics are reused without
#'   re-evaluating past future paths. Rejected evaluations still consume iteration
#'   budget without repeating the same fit as an infrastructure retry. If no
#'   eligible result exists, the workflow fails or an enabled local phase handles
#'   unresolved global series. Rankings are kept in memory, not new files.
#'   Hierarchical accuracy uses reconciled backtests; later comparisons and
#'   reconciliation do not require retained source-node quality rankings. Final
#'   reconciliation publishes the selected mixture without future-quality scoring,
#'   whole-set fallback, or extra refitting.
#'
#'   Custom pools enrolled by [set_agent_info()] constrain every proposal to
#'   approved models and original-scale R1 settings, plus the raw C1 recipe. Missing proposal fields
#'   do not acquire built-in defaults. Local/mounted CSV data, RDS objects and
#'   sequential, local-machine or Spark execution are required; `inner_parallel`
#'   applies to custom models as it does to built-in models. Global-only
#'   pools require at least two series. Saved parent and
#'   child identities are verified before completed results can be reused.
#'   Ordinary EDA still runs; model source and approval payloads are not sent to
#'   forecast reasoning. The Agent can choose a subset but cannot rewrite source,
#'   calibrate fixed parameters or add models. Generic summaries preserve exact
#'   custom identity without executing source. Compatible new-data replay uses
#'   [update_forecast()] with explicit `allow_iterate_forecast = FALSE` and a new
#'   identically enrolled parent. Changed enrollment or series requires iteration.
#'
#' @param agent_info Agent info from `set_agent_info()`
#' @param max_iter Maximum number of iterations for forecast optimization. This
#'   is a total per search (the global search and each time series' local
#'   search) for the Agent version, not a number of extra iterations: calling
#'   again on the same Agent run with a higher value runs only the remaining
#'   iterations, and a lower value runs nothing new.
#' @param weighted_mape_goal Weighted MAPE goal the agent is trying to achieve
#'   for each time series. Series that already meet it are not searched again,
#'   and a search can stop early once it is reached. On the same Agent run, a
#'   stricter goal reopens time series whose best model is global and that
#'   miss the new goal.
#' @param parallel_processing Default of NULL runs no parallel processing and
#'   forecasts each individual time series one after another. 'local_machine'
#'   leverages all cores on current machine Finn is running on. 'spark'
#'   runs time series in parallel on a spark cluster in Azure Databricks or
#'   Azure Synapse. Ellmer-backed workflows require ellmer 0.4.0 or later on
#'   the main process and every worker. Copilot-backed workflows require
#'   the CLI, processx, and authentication on each worker; see [chat_copilot()].
#' @param inner_parallel Run components of forecast process inside a specific
#'   time series in parallel. Can only be used if parallel_processing is
#'   set to NULL or 'spark'.
#' @param num_cores Number of cores to run when parallel processing is set up.
#'   Used when running parallel computations on local machine or within Azure.
#'   Default of NULL uses every available core minus one, respecting container
#'   CPU limits. With `parallel_processing = "spark"` and `inner_parallel =
#'   TRUE`, NULL instead gives each time series its share of the executor
#'   node's cores (see `vignette("parallel-processing")`). Can't be greater
#'   than the available cores minus one.
#' @param seed Set seed for random number generator. Numeric value.
#'
#' @return NULL
#' @examples
#' \dontrun{
#' # load example data
#' hist_data <- timetk::m4_monthly %>%
#'   dplyr::filter(date >= "2013-01-01") %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id))
#'
#' # set up Finn project
#' project <- set_project_info(
#'   project_name = "Demo_Project",
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month"
#' )
#'
#' # set up LLM
#' llm <- ellmer::chat_azure_openai(model = "gpt-4o-mini")
#'
#' # set up agent info
#' agent_info <- set_agent_info(
#'   project_info = project,
#'   llm = llm,
#'   input_data = hist_data,
#'   forecast_horizon = 6
#' )
#'
#' # run the forecast iteration process
#' iterate_forecast(
#'   agent_info = agent_info,
#'   max_iter = 3,
#'   weighted_mape_goal = 0.03
#' )
#' }
#' @export
iterate_forecast <- function(agent_info,
                             max_iter = 3,
                             weighted_mape_goal = 0.03,
                             parallel_processing = NULL,
                             inner_parallel = FALSE,
                             num_cores = NULL,
                             seed = 123) {
  message("[agent] Starting Forecast Iteration Process")

  # formatting checks
  check_agent_info(agent_info = agent_info)
  check_input_type("max_iter", max_iter, "numeric")
  check_input_type("weighted_mape_goal", weighted_mape_goal, "numeric")
  check_input_type("parallel_processing", parallel_processing, c("character", "NULL"), c("NULL", "local_machine", "spark"))
  check_input_type("inner_parallel", inner_parallel, "logical")
  check_input_type("num_cores", num_cores, c("numeric", "NULL"))

  agent_info <- load_agent_custom_state(agent_info)
  agent_custom_preflight(agent_info, parallel_processing)
  if (!is.null(agent_info$custom_agent_contract)) {
    agent_info$custom_foundation_suffix <- agent_custom_foundations(agent_info$custom_agent_contract)
  }
  if (!is.null(parallel_processing)) {
    if (inherits(agent_info$llm, "finnts_copilot_chat")) {
      check_copilot_runtime(agent_info$llm$command, agent_info$llm$timeout)
    } else {
      check_agent_ellmer_version()
    }
  }

  # get project info
  project_info <- agent_info$project_info

  # get metadata
  run_global_models <- agent_info$run_global_models
  run_local_models <- agent_info$run_local_models
  if (!is.null(agent_info$custom_agent_contract)) {
    run_global_models <- run_global_models && length(agent_custom_candidates(agent_info$custom_agent_contract,
      foundation_suffix = agent_info$custom_foundation_suffix)$models) > 0L
    run_local_models <- run_local_models && length(agent_custom_candidates(agent_info$custom_agent_contract,
      "local", agent_info$custom_foundation_suffix)$models) > 0L
  }

  # agent info adjustments
  if (agent_info$forecast_approach != "bottoms_up") {
    agent_info$project_info$combo_variables <- "ID"
  }

  agent_info$max_iter <- max_iter

  # run exploratory data analysis
  # check if eda data already exists
  eda_results <- tryCatch(
    {
      get_eda_data(agent_info = agent_info)
    },
    error = function(e) {
      tibble::tibble()
    }
  )
  eda_exists <- is.data.frame(eda_results) && nrow(eda_results) > 0

  if (eda_exists) {
    message("[agent] EDA already ran. Skipping EDA process.")
  } else {
    eda_info <- agent_info
    if (!is.null(agent_info$custom_agent_contract)) {
      eda_info <- eda_info[setdiff(names(eda_info), c("custom_agent_contract", "custom_agent_contract_id", "custom_foundation_suffix"))]
    }
    eda_agent_workflow(
      agent_info = eda_info,
      parallel_processing = parallel_processing,
      num_cores = num_cores
    )
    eda_results <- get_eda_data(agent_info = agent_info)
  }

  # get total number of time series
  combo_list <- agent_input_series(agent_info, eda_results)

  agent_info$selection_combos <- combo_list
  if (!is.null(agent_info$custom_agent_contract) && !run_local_models &&
    (!run_global_models || length(combo_list) < 2L)) stop("No compatible custom Agent mode is available for these series.", call. = FALSE)
  agent_info$selection_cache <- new.env(parent = emptyenv())

  best_run_tbl <- load_best_agent_run(agent_info = agent_info)
  # Series with saved best-run records, from the one listing above. Global
  # promotion reads only these (plus records it writes) so it never probes a
  # record parallel local workers write later; see read_agent_best_run_records().
  agent_info$best_run_records <- new.env(parent = emptyenv())
  agent_info$best_run_records$series <- if (nrow(best_run_tbl)) {
    unique(as.character(best_run_tbl$combo))
  } else character()
  global_models_ran <- FALSE

  # optimize global models
  if (length(combo_list) > 1 & run_global_models) {
    if (has_completed_iteration_target(best_run_tbl, max_iter)) {
      cli::cli_alert_info("Global model iteration already finished. Skipping global model optimization.")
    } else {
      message("[agent] Starting Global Model Iteration Workflow")

      # adjust max iterations based on previous runs; the workflow lists its
      # run logs once here and adds each new iteration's log itself
      run_log_paths <- agent_run_log_paths(agent_info, combo = NULL)
      previous_runs <- load_run_results(
        agent_info = agent_info,
        combo = NULL,
        run_log_paths = run_log_paths
      )

      if (!tibble::is_tibble(previous_runs)) {
        max_iter_adj <- max_iter
      } else {
        prev_run_count <- previous_runs %>%
          dplyr::filter(agent_version == agent_info$agent_version) %>%
          nrow()

        max_iter_adj <- max_iter - prev_run_count
      }

      if (max_iter_adj > 0) {
        if (max_iter_adj < max_iter) {
          cli::cli_alert_info(
            "Adjusting max iterations down due to {prev_run_count} previously completed iteration{?s}."
          )
        }

        # run the global model optimization; a global iteration cut off after
        # training is finished first instead of being redone
        resume_run <- find_resumable_global_run(agent_info, run_log_paths, previous_runs, seed)
        fcst_results <- fcst_agent_workflow(
          agent_info = agent_info,
          combo = NULL, # NULL = global models
          weighted_mape_goal = weighted_mape_goal,
          parallel_processing = parallel_processing,
          inner_parallel = inner_parallel,
          num_cores = num_cores,
          max_iter = max_iter_adj,
          seed = seed,
          previous_run_results = previous_runs,
          fallback_available = run_local_models,
          eda_results = eda_results,
          run_log_paths = run_log_paths,
          resume_run = resume_run
        )
        global_models_ran <- TRUE
      } else {
        cli::cli_alert_info("Max iterations already met. Skipping global model optimization.")
      }
    }
  } else if (length(combo_list) > 1 & !run_global_models) {
    message("[agent] Global models disabled. Skipping global model optimization.")
  } else {
    message("[agent] Only one time series found. Skipping global model optimization.")
  }

  # get local combo list
  if (global_models_ran) {
    best_run_tbl <- load_best_agent_run(agent_info = agent_info)
  }

  local_combo_list <- agent_local_combos_to_run(best_run_tbl, combo_list, weighted_mape_goal, max_iter)

  # optimize local models
  if (length(local_combo_list) == 0) {
    message("[agent] All time series already finished running. Skipping local model optimization.")
  } else if (!run_local_models) {
    message("[agent] Local models disabled. Skipping local model optimization.")
  } else {
    message("[agent] Starting Local Model Iteration Workflow")

    # series optimized one after another in this process share one inner pool
    local_inner_pool(inner_parallel && is.null(parallel_processing))

    local_models <- run_agent_local_series(
      agent_info_lean = agent_custom_worker_info(agent_info, keep_llm = TRUE),
      local_combo_list = local_combo_list,
      max_iter = max_iter,
      weighted_mape_goal = weighted_mape_goal,
      parallel_processing = parallel_processing,
      inner_parallel = inner_parallel,
      num_cores = num_cores,
      seed = seed
    )
    for (result in local_models) {
      for (condition in result$warnings) warning(condition)
    }
  }

  # save the best run for the agent
  message("[agent] Saving Best Run Metadata")

  save_best_agent_run(
    agent_info = agent_info
  )

  # reconcile hierarchical forecast
  if (agent_info$forecast_approach != "bottoms_up") {
    message("[agent] Reconciling Hierarchical Forecast")

    reconcile_agent_forecast(
      agent_info = agent_info,
      project_info = project_info
    )

    message("[agent] Saving Hierarchy Summary")

    summarize_hierarchy(agent_info = agent_info)
  }

  # save the final forecast for the agent
  message("[agent] Saving Final Forecast")

  save_agent_forecast(
    agent_info = agent_info
  )

  # save model summaries
  message("[agent] Saving Model Summaries")

  summarize_models(
    agent_info = agent_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores
  )

  # finish up
  message("[agent] Forecast Iteration Process Complete")
}

#' Series names of an Agent run, from its input listing and EDA names
#'
#' [iterate_forecast()] needs every series' raw name when it starts. Reading
#' all input files for their `Combo` values costs one read per series (about
#' 5 to 10 minutes per start at 8,205 series on mounted Spark storage), but
#' each input file name already carries its series' hash, and the consolidated
#' EDA results loaded just before hold the raw names. A name from EDA is used
#' only when its hash equals a listed file's hash, so it is the file's exact
#' series; the other files (EDA that skipped a series, hierarchy-level input,
#' or unexpected names) are read in one call, as before. Names are text, as
#' each series' own input read keeps them.
#'
#' @param agent_info Agent info whose project holds the run's input files.
#' @param eda_results Consolidated EDA results; any table without a `Combo`
#'   column covers no series. The run-level `"All"` rows are ignored.
#'
#' @return Unique series names in input-listing order. Names read from files
#'   that match no listed file name keep their read order, after the others.
#'   No listed input files is an error, as before.
#' @noRd
agent_input_series <- function(agent_info, eda_results) {
  project_info <- agent_info$project_info
  prefix <- paste0(hash_data(project_info$project_name), "-", hash_data(agent_info$run_id), "-")
  extension <- paste0(".", project_info$data_output)
  files <- as.character(list_files(
    project_info$storage_object,
    paste0(project_info$path, "/input_data/*", prefix, "*", extension)
  ))
  files <- files[!is.na(files) & nzchar(files)]
  file_names <- fs::path_file(files)
  file_hashes <- ifelse(startsWith(file_names, prefix) & endsWith(file_names, extension),
    substr(file_names, nchar(prefix) + 1L, nchar(file_names) - nchar(extension)), NA_character_)

  eda_names <- if (is.data.frame(eda_results) && "Combo" %in% names(eda_results)) {
    unique(as.character(eda_results$Combo))
  } else {
    character()
  }
  eda_names <- eda_names[!is.na(eda_names) & eda_names != "All"]
  eda_hashes <- vapply(eda_names, hash_data, character(1), USE.NAMES = FALSE)

  series <- eda_names[match(file_hashes, eda_hashes)]
  covered <- !is.na(file_hashes) & !is.na(series)
  if (all(covered) && length(files)) {
    return(unique(series))
  }

  read_names <- read_file(
    run_info = project_info,
    file_list = files[!covered],
    return_type = "df",
    character_columns = "Combo"
  ) %>%
    dplyr::pull(Combo) %>%
    as.character() %>%
    unique()
  position <- match(vapply(read_names, hash_data, character(1), USE.NAMES = FALSE), file_hashes)
  placed <- !is.na(position) & !covered[position]
  series[position[placed]] <- read_names[placed]
  unique(c(series[!is.na(series)], read_names[!placed]))
}

#' Series that still need local Agent optimization
#'
#' The coordinator's completion rule, also used by each local series task
#' before it does any work. A series needs work when its saved best-run record
#' misses the accuracy goal (or is missing) and its search has not finished:
#' the record is not marked complete at `max_iter` or more iterations. A global
#' winner that misses the goal always needs local work, because its record
#' marks the global search, not the series' local search, complete.
#'
#' @param best_run_tbl Saved best-run records, one per series; may be empty.
#'   Records saved before `run_complete` and `max_iterations` existed count as
#'   unfinished.
#' @param combo_list Series names to consider. Series without a record need work.
#' @param weighted_mape_goal Accuracy goal of the run.
#' @param max_iter Iteration target of the run.
#'
#' @return Names of the series that need local optimization: all of
#'   `combo_list` when there are no records, otherwise the unfinished recorded
#'   series in record order followed by the series without records.
#' @noRd
agent_local_combos_to_run <- function(best_run_tbl, combo_list, weighted_mape_goal, max_iter) {
  if (nrow(best_run_tbl) == 0) {
    return(combo_list)
  }

  # check if max_iter OR run_complete columns exist (for backward compatibility)
  if (!"run_complete" %in% colnames(best_run_tbl) | !"max_iterations" %in% colnames(best_run_tbl)) {
    best_run_tbl <- best_run_tbl %>%
      dplyr::mutate(
        run_complete = FALSE,
        max_iterations = 0
      )
  }

  # set run_complete to FALSE for global models that did not meet mape goal,
  # which will allow local models to run if needed
  best_run_tbl <- best_run_tbl %>%
    dplyr::mutate(
      run_complete = ifelse(model_type == "global" & weighted_mape > weighted_mape_goal, FALSE, run_complete)
    )

  # add in missing combos that have not been run yet (no agent best run file)
  missing_combos <- setdiff(combo_list, best_run_tbl$combo)

  if (length(missing_combos) > 0) {
    missing_tbl <- tibble::tibble(
      combo = missing_combos,
      weighted_mape = Inf,
      run_complete = FALSE,
      max_iterations = 0
    )

    best_run_tbl <- best_run_tbl %>%
      dplyr::bind_rows(missing_tbl)
  }

  # filter combos that need local model optimization
  best_run_tbl %>%
    dplyr::filter(!is.finite(weighted_mape) | weighted_mape > weighted_mape_goal) %>%
    dplyr::filter(run_complete == FALSE | max_iterations < max_iter) %>%
    dplyr::pull(combo) %>%
    unique()
}

#' Whether a dispatched local series already finished its search
#'
#' A Spark task can run again after it finished, when Spark recomputes a lost
#' cached result or retries the task. The task then reads its own series'
#' best-run record by exact path and applies [agent_local_combos_to_run()], so
#' a finished series returns before listing its run logs or iterating. Only the
#' series' own record is read; no other path is probed.
#'
#' @param agent_info Lean Agent info of the local series task.
#' @param combo Raw series name.
#' @param weighted_mape_goal Accuracy goal of the run.
#' @param max_iter Iteration target of the run.
#'
#' @return `TRUE` when the saved record shows the series finished or met the
#'   goal; `FALSE` when the record is missing or the series needs work. An
#'   unreadable or empty record is an error.
#' @noRd
agent_local_series_done <- function(agent_info, combo, weighted_mape_goal, max_iter) {
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  record <- read_selection_file(project_info, "logs", "-agent_best_run", combo, optional = TRUE)
  length(agent_local_combos_to_run(record, combo, weighted_mape_goal, max_iter)) == 0L
}

#' List one Agent workflow's saved run logs once
#'
#' The global workflow and each local series list their run logs once when
#' they start; [fcst_agent_workflow()] then adds each new iteration's run log
#' itself, so later history reloads read exact paths instead of listing the
#' large logs folder again. The pattern and folder are the ones
#' [get_run_info()] lists for [load_run_results()].
#'
#' @param agent_info Agent info whose project holds the run logs.
#' @param combo Hashed series name of a local workflow, or `NULL` for the
#'   global workflow.
#'
#' @return Run-log paths in byte order (the order a listing returns them), or
#'   `NULL` for blob and OneDrive storage, which keep listing on every reload.
#' @noRd
agent_run_log_paths <- function(agent_info, combo = NULL) {
  project_info <- agent_info$project_info
  if (!is.null(project_info$storage_object)) {
    return(NULL)
  }

  project_name <- paste0(project_info$project_name, "_", if (is.null(combo)) hash_data("all") else combo)
  pattern <- paste0("/logs/*", hash_data(project_name), "-*.csv")
  files <- if (is.null(project_info$path)) {
    list_files(NULL, fs::path(tempdir(), "/logs", fs::path_file(pattern)))
  } else {
    list_files(NULL, fs::path(project_info$path, pattern))
  }
  files <- as.character(files)
  sort(files[!is.na(files) & nzchar(files)], method = "radix")
}

#' Add an iteration's run log to a workflow's known run logs
#'
#' @param run_log_paths Run-log paths from [agent_run_log_paths()].
#' @param run_info Run info of the iteration, from `submit_fcst_run()`.
#'
#' @return The paths with the iteration's run log added once, in byte order.
#' @noRd
add_agent_run_log_path <- function(run_log_paths, run_info) {
  run_log <- as.character(local_artifact_path(run_info, "logs", extension = "csv"))
  sort(unique(c(run_log_paths, run_log)), method = "radix")
}

#' Optimize local Agent series in parallel
#'
#' Dispatches one [agent_local_series_task()] per series through
#' [run_series_tasks()] from this small frame instead of [iterate_forecast()]'s,
#' because sparklyr's doSpark serializes the calling frame into every Spark
#' task: the global iteration's results, EDA and best-run tables there could
#' reach gigabytes. On Spark and `local_machine` each series runs in its own
#' process, so a series that crashes is named and dispatched again (see
#' [run_series_tasks()]).
#'
#' @param agent_info_lean Agent info without custom contracts or caches (see
#'   `agent_custom_worker_info()`), keeping the LLM template.
#' @param local_combo_list Raw names of the series to optimize.
#' @param max_iter Iteration target of the run.
#' @param weighted_mape_goal Accuracy goal of the run.
#' @param parallel_processing `NULL`, `"local_machine"` or `"spark"`.
#' @param inner_parallel Whether each series runs its stages on an inner pool.
#' @param num_cores Cores for the pools, or `NULL` for the defaults.
#' @param seed Random seed of the run.
#'
#' @return A list with one element per series, in series order: `Combo` and
#'   the fallback `warnings` to relay. Errors, and a series that crashes on
#'   every attempt (class `finnts_series_crash`), stop the run.
#' @noRd
run_agent_local_series <- function(agent_info_lean,
                                   local_combo_list,
                                   max_iter,
                                   weighted_mape_goal,
                                   parallel_processing,
                                   inner_parallel,
                                   num_cores,
                                   seed) {
  # evaluate every argument now: an unevaluated one would carry the caller's
  # frame into each Spark task
  force(agent_info_lean)
  force(local_combo_list)
  force(max_iter)
  force(weighted_mape_goal)
  force(parallel_processing)
  force(inner_parallel)
  force(num_cores)
  force(seed)

  context <- list(
    agent_info_lean = agent_info_lean,
    max_iter = max_iter,
    weighted_mape_goal = weighted_mape_goal,
    inner_parallel = inner_parallel,
    num_cores = num_cores,
    seed = seed,
    # each Spark task's share of its node for automatic inner pools (D13)
    task_cores = spark_inner_cores(num_cores, parallel_processing, inner_parallel)
  )

  run_series_tasks(
    items = local_combo_list,
    task = "agent_local_series_task",
    context = context,
    labels = local_combo_list,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    project_info = agent_info_lean$project_info,
    errorhandling = "stop",
    iterations = TRUE
  )
}

#' Optimize one local Agent series
#'
#' The task [run_agent_local_series()] runs for each series. It first skips a
#' series that already finished (a Spark recompute or retry, or a crashed
#' series dispatched again; see [agent_local_series_done()]), then lists its
#' run logs once and runs the remaining iterations, so a redispatched series
#' resumes after its completed iterations.
#'
#' @param item Raw series name.
#' @param context Shared inputs from [run_agent_local_series()]:
#'   `agent_info_lean`, `max_iter`, `weighted_mape_goal`, `inner_parallel`,
#'   `num_cores`, `seed` and `task_cores`.
#'
#' @return `list(Combo, warnings)`: the series name and the fallback warnings
#'   to relay.
#' @noRd
agent_local_series_task <- function(item, context) {
  agent_info_lean <- context$agent_info_lean
  max_iter <- context$max_iter
  weighted_mape_goal <- context$weighted_mape_goal
  inner_parallel <- context$inner_parallel

  message("[agent] Running local model optimization for combo: ", item)
  fallback_warnings <- list()

  # a re-run task stops here when the series' saved record already shows it
  # finished, before listing its run logs
  if (agent_local_series_done(agent_info_lean, item, weighted_mape_goal, max_iter)) {
    cli::cli_alert_info("Series already finished. Skipping local model optimization.")
    return(list(Combo = item, warnings = fallback_warnings))
  }

  # list this series' run logs once; each iteration adds its own run log
  run_log_paths <- agent_run_log_paths(agent_info_lean, hash_data(item))

  # adjust max iterations based on previous runs
  previous_runs <- load_run_results(
    agent_info = agent_info_lean,
    combo = hash_data(item),
    run_log_paths = run_log_paths
  )

  if (!tibble::is_tibble(previous_runs)) {
    max_iter_adj <- max_iter
  } else {
    prev_run_count <- previous_runs %>%
      dplyr::filter(agent_version == agent_info_lean$agent_version) %>%
      nrow()

    max_iter_adj <- max_iter - prev_run_count
  }

  if (max_iter_adj > 0) {
    if (max_iter_adj < max_iter) {
      # the count also tells a crash message which iteration was running
      cli::cli_alert_info(
        "Adjusting max iterations down due to {prev_run_count} previously completed iteration{?s}."
      )
    }

    # run the local model workflow; its iterations and stages share one
    # inner pool in this process
    fcst_results <- capture_forecast_warnings(with_inner_pool(fcst_agent_workflow(
      agent_info = agent_info_lean,
      combo = hash_data(item),
      weighted_mape_goal = weighted_mape_goal,
      parallel_processing = NULL,
      inner_parallel = inner_parallel,
      num_cores = context$num_cores,
      max_iter = max_iter_adj,
      seed = context$seed,
      previous_run_results = previous_runs,
      fallback_available = FALSE,
      run_log_paths = run_log_paths
    ), active = inner_parallel, cores = context$task_cores))
    fallback_warnings <- fcst_results$warnings
  } else {
    cli::cli_alert_info("Max iterations already met. Skipping local model optimization.")
  }

  list(Combo = item, warnings = fallback_warnings)
}

#' Get the final best forecast for an agent
#'
#' This function retrieves the final forecast for a Finn agent after the forecast iteration process is complete.
#'
#' @details For hierarchical Agent runs, returns only the final reconciled
#'   `Best-Model` forecast. Each series can run and select different models,
#'   recipes, or averages during [iterate_forecast()], so the selected forecasts
#'   are reconciled together rather than producing a comparison hierarchy for
#'   every model. `Best-Model` does not mean the same model family won for every
#'   series. This best-only reconciled output contract also applies after
#'   [update_forecast()]. It does not change non-hierarchical candidate output.
#'
#'   For non-agentic hierarchical runs, [get_forecast_data()] returns all
#'   successfully saved per-model reconciled forecasts plus `Best-Model`.
#'
#'   The final forecast artifact must exist. Missing local artifacts raise a
#'   required-artifact error; storage-provider and deserialization errors
#'   propagate rather than returning an empty forecast. Reads use the exact
#'   artifact path without directory discovery and do not modify saved outputs.
#'
#' @param agent_info Agent info from `set_agent_info()`
#'
#' @return A tibble containing the final forecast for the agent.
#' @examples
#' \dontrun{
#' # load example data
#' hist_data <- timetk::m4_monthly %>%
#'   dplyr::filter(date >= "2013-01-01") %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id))
#'
#' # set up Finn project
#' project <- set_project_info(
#'   project_name = "Demo_Project",
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month"
#' )
#'
#' # set up LLM
#' llm <- ellmer::chat_azure_openai(model = "gpt-4o-mini")
#'
#' # set up agent info
#' agent_info <- set_agent_info(
#'   project_info = project,
#'   llm = llm,
#'   input_data = hist_data,
#'   forecast_horizon = 6
#' )
#'
#' # run the forecast iteration process
#' iterate_forecast(
#'   agent_info = agent_info,
#'   max_iter = 3,
#'   weighted_mape_goal = 0.03
#' )
#'
#' # get the final forecast for the agent
#' final_forecast <- get_agent_forecast(agent_info = agent_info)
#' }
#' @export
get_agent_forecast <- function(agent_info) {
  # formatting checks
  check_agent_info(agent_info = agent_info)

  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  # load the final forecast for the agent
  path <- local_artifact_path(project_info, "final_output", "-forecast")
  final_fcst_tbl <- if (is.null(project_info$storage_object)) {
    read_local_artifacts(project_info, path)
  } else {
    read_exact_artifact(project_info, path)
  }

  return(final_fcst_tbl)
}

#' Get the best run for an agent
#'
#' This function retrieves the best run information for a Finn agent after the forecast iteration process is complete.
#'
#' @param agent_info Agent info from `set_agent_info()`
#'
#' @return A tibble containing the best run information for the agent.
#' @examples
#' \dontrun{
#' # load example data
#' hist_data <- timetk::m4_monthly %>%
#'   dplyr::filter(date >= "2013-01-01") %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id))
#'
#' # set up Finn project
#' project <- set_project_info(
#'   project_name = "Demo_Project",
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month"
#' )
#'
#' # set up LLM
#' llm <- ellmer::chat_azure_openai(model = "gpt-4o-mini")
#'
#' # set up agent info
#' agent_info <- set_agent_info(
#'   project_info = project,
#'   llm = llm,
#'   input_data = hist_data,
#'   forecast_horizon = 6
#' )
#'
#' # run the forecast iteration process
#' iterate_forecast(
#'   agent_info = agent_info,
#'   max_iter = 3,
#'   weighted_mape_goal = 0.03
#' )
#'
#' # get the best run information for the agent
#' best_run_info <- get_best_agent_run(agent_info = agent_info)
#' }
#' @export
get_best_agent_run <- function(agent_info) {
  # check inputs
  check_agent_info(agent_info = agent_info)

  final_run_tbl <- load_final_agent_run_metadata(agent_info = agent_info)

  return(final_run_tbl)
}

load_final_agent_run_metadata <- function(agent_info,
                                          allow_missing = FALSE) {
  load_final_agent_artifact(
    agent_info = agent_info,
    suffix = "run_metadata",
    allow_missing = allow_missing
  )
}

final_agent_artifact_exists <- function(project_info, artifact_path) {
  if (inherits(project_info$storage_object, c("blob_container", "ms_drive"))) {
    artifact_files <- list_files(
      storage_object = project_info$storage_object,
      path = fs::path(project_info$path, artifact_path),
      fail_on_error = TRUE
    )

    return(length(artifact_files) > 0)
  }

  artifact_file <- if (is.null(project_info$path)) {
    fs::path(tempdir(), artifact_path)
  } else {
    fs::path(project_info$path, artifact_path)
  }

  fs::file_exists(artifact_file)
}

#' Read one final Agent output without coercing saved run identities
#'
#' @param agent_info Agent identity and project storage settings.
#' @param suffix Final-output suffix, such as run_metadata or forecast.
#' @param allow_missing Whether absent outputs may return an empty tibble.
#' @return The saved data frame. Run-metadata CSV identifiers remain character,
#'   and its other columns are typed from every row, so a settings value such as
#'   `lag_periods = "1---2"` that a row sample would miss is kept as written.
#'   Other outputs and formats retain their existing types. Provider and
#'   unreadable-output failures propagate; no artifacts are written.
#' @noRd
load_final_agent_artifact <- function(agent_info,
                                      suffix,
                                      allow_missing = FALSE) {
  # metadata
  project_info <- agent_info$project_info
  artifact_path <- paste0(
    "/final_output/", hash_data(project_info$project_name), "-",
    hash_data(agent_info$run_id), "-", suffix, ".", project_info$data_output
  )
  run_metadata <- identical(suffix, "run_metadata")

  if (allow_missing &&
    !final_agent_artifact_exists(project_info, artifact_path)) {
    return(tibble::tibble())
  }

  withCallingHandlers(
    read_file(
      run_info = project_info,
      path = artifact_path,
      return_type = "df",
      allow_missing = allow_missing,
      character_columns = if (run_metadata) {
        c("combo", "agent_run_id", "best_run_name")
      } else {
        NULL
      },
      guess_max = if (run_metadata) Inf else 100
    ),
    warning = function(condition) {
      if (grepl(
        "Skipping empty or unreadable file:",
        conditionMessage(condition),
        fixed = TRUE
      )) {
        stop(conditionMessage(condition), call. = FALSE)
      }
    }
  )
}

#' load the best forecast for an agent before saving to disk
#'
#' This function retrieves the final forecast for a Finn agent after the forecast iteration process is complete and prepares it before it writes to disk.
#' It assembles the best-run records its caller passes, normally the saved
#' `run_metadata` snapshot, and never lists the logs folder. Final outputs
#' therefore always use the iteration `run_metadata` names, even if a late
#' worker rewrote a live best-run record after it was saved. The records are
#' checked for one global iteration and custom-winner identity before any
#' forecast is read. On mounted Spark storage the read is repeated with
#' [read_settled()] while it fails, because the final Spark stages' forecasts
#' can be briefly invisible or stale there; its errors are unchanged. Many local
#' series' forecasts are read on a series I/O pool (see [map_series_io()]).
#'
#' @param agent_info Agent info from `set_agent_info()`
#' @param final_output If TRUE, formats the output for final saving. If FALSE, returns raw forecast data.
#' @param best_run_tbl Best-run records to assemble: the saved run metadata
#'   from [load_final_agent_run_metadata()] or [get_best_agent_run()]. Required
#'   unless `final_output` is TRUE for a hierarchical run, which returns the
#'   saved reconciled forecast instead.
#'
#' @return A tibble containing the final forecast for the agent.
#' @noRd
load_agent_forecast <- function(agent_info,
                                final_output = FALSE,
                                best_run_tbl = NULL) {
  # formatting checks
  check_agent_info(agent_info = agent_info)

  reconciled <- agent_info$forecast_approach != "bottoms_up" & final_output
  pool <- NULL
  if (!reconciled) {
    if (!is.data.frame(best_run_tbl)) {
      stop("Error in load_agent_forecast(). The best-run records to assemble are required.", call. = FALSE)
    }
    validate_global_iteration(best_run_tbl)
    audit_agent_custom_best(agent_info, best_run_tbl)
    pool <- series_io_pool(agent_info$project_info, sum(best_run_tbl$model_type == "local", na.rm = TRUE))
    on.exit(series_io_close(pool), add = TRUE)
  }
  read_settled(function() load_agent_forecast_once(agent_info, reconciled, best_run_tbl, pool),
    function(forecasts) TRUE, agent_info$project_info$path, "final Agent forecasts")
}

#' Read and assemble the best Agent forecast once
#'
#' Body of [load_agent_forecast()]: reads the reconciled forecast, or each best
#' run's global and local forecasts, and errors when a series or its best model
#' is missing.
#'
#' @param agent_info Agent info from `set_agent_info()`
#' @param reconciled If TRUE, returns the saved reconciled forecast of a
#'   hierarchical Agent run instead of assembling the best runs.
#' @param best_run_tbl Best runs to assemble; unused when `reconciled` is TRUE.
#' @param pool Series I/O pool for the local forecast reads, or `NULL` to read
#'   them in this process. Rows and their order are the same either way.
#'
#' @return A tibble containing the final forecast for the agent.
#' @noRd
load_agent_forecast_once <- function(agent_info,
                                     reconciled,
                                     best_run_tbl,
                                     pool = NULL) {
  # check for reconciled forecast and return if found
  if (reconciled) {
    project_info <- agent_info$project_info
    project_info$run_name <- agent_info$run_id

    fcst_tbl <- if (inherits(project_info$storage_object, c("blob_container", "ms_drive"))) {
      read_exact_artifact(project_info,
        local_artifact_path(project_info, "forecasts", "-reconciled", hash_data("Best-Model")))
    } else read_file(
      run_info = project_info,
      path = paste0(
        "/forecasts/", hash_data(project_info$project_name), "-", hash_data(project_info$run_name),
        "-", hash_data("Best-Model"), "-reconciled.", project_info$data_output
      )
    )

    return(fcst_tbl)
  }

  model_type_list <- best_run_tbl %>%
    dplyr::pull(model_type) %>%
    unique()

  # load global model forecasts
  if ("global" %in% model_type_list) {
    global_runs <- best_run_tbl[best_run_tbl$model_type == "global", , drop = FALSE]
    global_fcst_tbl <- dplyr::bind_rows(lapply(split(global_runs, global_runs$best_run_name), function(selected_runs) {
      run_info <- agent_info$project_info
      run_info$project_name <- paste0(agent_info$project_info$project_name, "_", hash_data("all"))
      run_info$run_name <- selected_runs$best_run_name[1]
      read_selected_agent_forecasts(run_info, selected_runs$combo, agent_info$project_info$combo_variables)
    }))
  } else {
    global_fcst_tbl <- tibble::tibble()
  }

  # load local model forecasts
  if ("local" %in% model_type_list) {
    local_run_tbl <- best_run_tbl %>%
      dplyr::filter(model_type == "local")

    # read each series' forecasts, in order; on a pool for many series
    local_items <- lapply(seq_len(nrow(local_run_tbl)), function(index) {
      list(project_info = agent_info$project_info, run = local_run_tbl[index, , drop = FALSE])
    })
    local_fcst_tbl <- do.call(rbind, map_series_io(pool, local_items, read_local_agent_forecast))

    # load local forecast files
    if (nrow(local_fcst_tbl) == 0) {
      stop("Error in load_agent_forecast(). No local forecast files found for agent.", call. = FALSE)
    }

    # get train test split data
    local_run_example <- local_run_tbl %>%
      dplyr::slice(1)

    local_project_name <- paste0(
      agent_info$project_info$project_name,
      "_",
      hash_data(local_run_example$combo)
    )

    model_train_test_tbl <- read_exact_artifact(agent_info$project_info,
      file_list = paste0(
        agent_info$project_info$path, "/prep_models/", hash_data(local_project_name), "-", hash_data(local_run_example$best_run_name),
        "-train_test_split.", agent_info$project_info$data_output
      ),
      return_type = "df"
    ) %>%
      dplyr::select(Run_Type, Train_Test_ID) %>%
      dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID))

    # final formatting
    local_fcst_tbl <- local_fcst_tbl %>%
      dplyr::mutate(
        Train_Test_ID = as.numeric(Train_Test_ID),
        Date = as.Date(Date)
      ) %>%
      dplyr::left_join(model_train_test_tbl,
        by = "Train_Test_ID"
      ) %>%
      dplyr::relocate(Run_Type, .before = Train_Test_ID) %>%
      dplyr::select(-Combo_ID, -Hyperparameter_ID) %>%
      dplyr::relocate(Combo) %>%
      dplyr::arrange(Combo, dplyr::desc(Best_Model), Model_ID, Train_Test_ID, Date) %>%
      tidyr::separate(
        col = Combo,
        into = agent_info$project_info$combo_variables,
        remove = FALSE,
        sep = "--"
      ) %>%
      base::suppressWarnings()
  } else {
    local_fcst_tbl <- tibble::tibble()
  }

  # combine global and local forecasts
  final_fcst_tbl <- global_fcst_tbl %>%
    dplyr::bind_rows(local_fcst_tbl)

  # check that all combos are present
  if (length(unique(best_run_tbl$combo)) != length(unique(final_fcst_tbl$Combo))) {
    missing_combos <- setdiff(unique(best_run_tbl$combo), unique(final_fcst_tbl$Combo))
    stop(paste0("Error in load_agent_forecast(). Missing forecast data for combos: ", paste(missing_combos, collapse = ", ")), call. = FALSE)
  }

  # validate that every combo has a best model
  validate_best_model(final_fcst_tbl, context = "load_agent_forecast")

  # return final forecast table
  return(final_fcst_tbl)
}

#' Read one local series' selected Agent forecasts
#'
#' Per-series task of [load_agent_forecast_once()], run in this process or on a
#' series I/O pool. Reads the best run's single-model forecasts and, when that
#' run averaged several models or recipes, its optional average forecasts, by
#' exact path.
#'
#' @param item List with `project_info` (the Agent's project info) and `run`
#'   (one best-run row with `combo`, `best_run_name`, `models_to_run`,
#'   `recipes_to_run` and `average_models`).
#'
#' @return The series' forecast rows. Missing or unreadable single-model
#'   forecasts are errors; a missing optional average file adds no rows.
#' @noRd
read_local_agent_forecast <- function(item) {
  x <- item$run
  run_info <- item$project_info
  run_info$project_name <- paste0(item$project_info$project_name, "_", hash_data(x$combo))
  run_info$run_name <- x$best_run_name
  forecasts <- read_selection_file(run_info, "forecasts", "-single_models", x$combo)

  if ((grepl("---", x$models_to_run) | grepl("---", x$recipes_to_run)) & x$average_models) {
    forecasts <- dplyr::bind_rows(forecasts,
      read_selection_file(run_info, "forecasts", "-average_models", x$combo, optional = TRUE)
    )
  }

  forecasts
}

#' Save the final best forecast for an agent
#'
#' This function retrieves the final forecast for a Finn agent after the forecast iteration process is complete and saves to disk.
#' Bottoms-up runs assemble it from the saved `run_metadata`, which must exist
#' and be readable (it is written just before by `save_best_agent_run()`), so
#' the forecast names the same iteration as `run_metadata` and the model
#' summaries. Hierarchical runs save their reconciled forecast.
#'
#' @param agent_info Agent info from `set_agent_info()`
#'
#' @return Nothing
#' @noRd
save_agent_forecast <- function(agent_info) {
  # formatting checks
  check_agent_info(agent_info = agent_info)

  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  reconciled <- agent_info$forecast_approach != "bottoms_up"

  # load the final forecast for the agent
  final_fcst_tbl <- load_agent_forecast(
    agent_info = agent_info,
    final_output = TRUE,
    best_run_tbl = if (!reconciled) load_final_agent_run_metadata(agent_info)
  )

  if (nrow(final_fcst_tbl) == 0) {
    stop("Error in save_agent_forecast(). No final forecast found for agent.", call. = FALSE)
  }

  # save the final forecast for the agent
  write_data(
    x = final_fcst_tbl,
    combo = NULL,
    run_info = project_info,
    output_type = "data",
    folder = "final_output",
    suffix = "-forecast"
  )

  return(invisible(NULL))
}

#' Load the best run for an agent
#'
#' This function retrieves the best run information for a Finn agent after the forecast iteration process is complete
#' Intermediate CSV identities are parsed as text before they can be saved to
#' final metadata. Provider failures and inconsistent global winners remain errors.
#' Custom winner records additionally require matching parent/child enrollment
#' and fitted artifact identities before reuse; no scoring policy changes.
#'
#' @param agent_info Agent info from `set_agent_info()`
#'
#' @return Best-run records, or an empty tibble when no files exist. All
#'   per-series records are read in one call, and each non-identity column is
#'   typed from every record rather than vroom's 100-row sample. A settings
#'   value held by a few series, such as `lag_periods = "1---2"`, is therefore
#'   kept as written instead of being parsed as `NA` or `TRUE`; columns whose
#'   records agree keep the types a sample gives. No stored artifacts are modified.
#' @noRd
load_best_agent_run <- function(agent_info) {
  # metadata
  project_info <- agent_info$project_info

  # get the best run for the agent
  combo_best_run_list <- list_files(
    project_info$storage_object,
    paste0(
      project_info$path, "/logs/*", hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "*-agent_best_run.csv"
    ),
    fail_on_error = TRUE
  )

  if (length(combo_best_run_list) == 0) {
    best_run_tbl <- tibble::tibble()
  } else {
    if (inherits(project_info$storage_object, "ms_drive")) {
      combo_best_run_list <- fs::path(project_info$path, "logs", fs::path_file(combo_best_run_list))
    }
    best_run_tbl <- read_exact_artifact(
      run_info = project_info,
      file_list = combo_best_run_list,
      return_type = "df",
      character_columns = c("combo", "agent_run_id", "best_run_name"),
      guess_max = Inf
    )
  }

  validate_global_iteration(best_run_tbl)
  audit_agent_custom_best(agent_info, best_run_tbl)
  return(best_run_tbl)
}

#' Save the best run for an agent
#'
#' This function retrieves the best run information for a Finn agent after the forecast iteration process is complete and saves to disk.
#'
#' @param agent_info Agent info from `set_agent_info()`
#'
#' @return Nothing
#' @noRd
save_best_agent_run <- function(agent_info) {
  # formatting checks
  check_agent_info(agent_info = agent_info)

  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  # load the best run for the agent
  final_run_tbl <- load_best_agent_run(
    agent_info = agent_info
  )

  if (nrow(final_run_tbl) == 0) {
    stop("Error in save_best_agent_run(). No best run found for agent.", call. = FALSE)
  }

  # verify all expected combos have agent_best_run files
  expected_combos <- get_total_combos(agent_info)
  existing_combos <- final_run_tbl %>%
    dplyr::pull(combo) %>%
    unique()

  existing_hashes <- purrr::map_chr(existing_combos, hash_data)
  missing_hashes <- setdiff(expected_combos, existing_hashes)

  if (length(missing_hashes) > 0) {
    resolved_names <- resolve_combo_hashes(agent_info, missing_hashes)
    stop(
      "Error in save_best_agent_run(). Expected ", length(expected_combos),
      " time series but only found ", length(existing_combos),
      ". Missing combos: ", paste(resolved_names, collapse = ", "),
      call. = FALSE
    )
  }

  # remove unnecessary columns
  final_run_tbl <- final_run_tbl %>%
    dplyr::select(-dplyr::any_of(c("run_complete", "max_iterations")))

  # save the best run for the agent
  write_data(
    x = final_run_tbl,
    combo = NULL,
    run_info = project_info,
    output_type = "data",
    folder = "final_output",
    suffix = "-run_metadata"
  )

  return(invisible(NULL))
}

#' Forecast Agent Workflow
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#' @param weighted_mape_goal A numeric value representing the goal for the weighted MAPE.
#' @param parallel_processing Logical indicating if parallel processing should be used.
#' @param inner_parallel Logical indicating if inner parallel processing should be used.
#' @param num_cores Number of cores to use for parallel processing. If NULL, defaults to the number of available cores.
#' @param max_iter Maximum number of iterations for the workflow. Default is 3.
#' @param seed Random seed for reproducibility. Default is 123.
#' @param previous_run_results Existing completed run history loaded by the
#'   outer global or local workflow.
#' @param fallback_available Whether an exhausted global workflow may continue
#'   to enabled local models when no global best run exists.
#' @param eda_results Consolidated EDA results used to resolve the legal global
#'   forecast approaches before reasoning. Not used by local workflows.
#' @param run_log_paths The workflow's saved run logs from
#'   [agent_run_log_paths()], or `NULL` to list them on every history reload.
#'   When given, each iteration that is logged (accepted, rejected or partial)
#'   adds its own run log, so the reload after it reads exact paths and sees the
#'   same history a listing would. Reasoning retries reuse the loaded history
#'   and add no run log. The final paths are returned in `run_log_paths`.
#' @param resume_run Run-log row of a global iteration cut off after training,
#'   from [find_resumable_global_run()], or `NULL`. The global workflow first
#'   finishes that iteration under its own run name (see
#'   [resume_global_iteration()]); it counts toward `max_iter`. Ignored for
#'   local series and custom-model runs.
#' @details Custom contracts constrain mode/representation and preserve existing
#'   session isolation. Workers receive names and identity references, then load
#'   the authoritative version-owned enrollment before reasoning. They never
#'   fall through to an unselected local pool.
#'
#' @return A list containing the results of the workflow.
#' @noRd
fcst_agent_workflow <- function(agent_info,
                                combo,
                                weighted_mape_goal,
                                parallel_processing,
                                inner_parallel,
                                num_cores,
                                max_iter = 3,
                                seed = 123,
                                previous_run_results = NULL,
                                fallback_available = FALSE,
                                eda_results = NULL,
                                run_log_paths = NULL,
                                resume_run = NULL) {
  agent_info$custom_enrollment_cache <- new.env(parent = emptyenv())
  agent_info <- load_agent_custom_state(agent_info)
  agent_custom_preflight(agent_info, parallel_processing)
  agent_info$allow_quality_rejection <- TRUE
  if (is.null(agent_info$selection_cache)) agent_info$selection_cache <- new.env(parent = emptyenv())
  agent_info$global_forecast_approaches <- if (!is.null(agent_info$custom_agent_contract)) "bottoms_up" else if (is.null(combo)) {
    resolve_agent_global_forecast_approaches(agent_info, eda_results)
  } else {
    "bottoms_up"
  }

  # create one fresh session for this series and all of its iterations
  agent_info$llm <- new_llm_session(agent_info$llm)

  # add LLM system prompt for EDA info
  agent_info$llm <- agent_info$llm$set_system_prompt(iterate_forecast_system_prompt(
    agent_info = agent_info,
    combo = combo,
    weighted_mape_goal = weighted_mape_goal
  ))

  # create a timestamp for the run
  timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")

  reason_history <- if (is.null(previous_run_results)) {
    load_reason_history(agent_info = agent_info, combo = combo)
  } else {
    new_reason_history(previous_run_results, agent_info$agent_version)
  }

  # construct the workflow
  workflow <- list(
    start = list(
      fn = "reason_inputs",
      `next` = NULL,
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info,
        combo = combo,
        weighted_mape_goal = weighted_mape_goal,
        last_error = NULL,
        previous_run_results = "{ctx$reason_history$previous_run_results}",
        previous_version_results = "{ctx$reason_history$previous_version_results}",
        total_runs = "{ctx$reason_history$total_runs}"
      ),
      branch = function(ctx) {
        # extract the results from the current node
        results <- ctx$results

        # check if the LLM aborted the run
        if ("abort" %in% names(results$reason_inputs) && results$reason_inputs$abort == "TRUE") {
          ctx$completion_reason <- "reasoning_exhausted"
          ctx$abort_reason <- results$reason_inputs$reasoning %||% "Reasoning aborted."
          # always finalize to update max_iterations and run_complete in best_run file
          return(list(ctx = ctx, `next` = "finalize_run"))
        } else {
          return(list(ctx = ctx, `next` = "submit_fcst_run"))
        }
      }
    ),
    submit_fcst_run = list(
      fn = "submit_fcst_run",
      `next` = "get_fcst_output",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info          = agent_info,
        inputs              = "{results$reason_inputs}",
        combo               = combo,
        timestamp           = paste0(timestamp, "_", "{ctx$iter}"),
        parallel_processing = parallel_processing,
        inner_parallel      = inner_parallel,
        num_cores           = num_cores,
        seed                = seed
      ),
      branch = use_checked_forecast
    ),
    get_fcst_output = list(
      fn = "get_fcst_output",
      `next` = "calculate_fcst_metrics",
      retry_mode = "plain",
      max_retry = 3,
      args = list(run_info = "{results$submit_fcst_run}")
    ),
    calculate_fcst_metrics = list(
      fn = "calculate_fcst_metrics",
      `next` = "log_best_run",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        run_info  = "{results$submit_fcst_run}",
        fcst_tbl  = "{results$get_fcst_output}"
      )
    ),
    log_best_run = list(
      fn = "log_best_run",
      `next` = NULL,
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info,
        run_info = "{results$submit_fcst_run}",
        weighted_mape = "{results$calculate_fcst_metrics}",
        combo = combo,
        run_history = "{ctx$reason_history$previous_run_results}"
      ),
      branch = function(ctx) {
        # test if max run iterations have been reached
        ctx$iter <- ctx$iter + 1
        max_runs_reached <- ctx$iter >= ctx$max_iter

        # the iteration's run log is saved; later history reloads read it by path
        if (!is.null(ctx$run_log_paths)) {
          ctx$run_log_paths <- add_agent_run_log_path(ctx$run_log_paths, ctx$results$submit_fcst_run)
        }

        cli::cli_alert_info(
          "Forecast Iteration {ctx$iter}/{ctx$max_iter} Complete"
        )

        # check if the weighted MAPE is below the goal
        weighted_mape <- ctx$results$calculate_fcst_metrics

        selection_ok <- attr(weighted_mape, "selection_ok")
        if (is.null(selection_ok)) selection_ok <- TRUE
        logged <- ctx$results$log_best_run
        if (is.list(logged) && isTRUE(logged$status %in% c("partial", "rejected"))) {
          selection_ok <- FALSE
        }
        if (is.finite(weighted_mape) && weighted_mape < weighted_mape_goal && isTRUE(selection_ok)) {
          cli::cli_alert_success(
            "Weighted MAPE goal of {round(weighted_mape_goal * 100, 2)}% achieved! Latest weighted MAPE is {round(weighted_mape * 100, 2)}%. Stopping iterations."
          )
          wmape_goal_reached <- TRUE
        } else if (!isTRUE(selection_ok)) {
          cli::cli_alert_info(
            "Incomplete or rejected forecasts cannot meet the accuracy goal. {if (max_runs_reached) 'Iteration limit reached.' else 'Continuing to the next iteration.'}"
          )
          wmape_goal_reached <- FALSE
        } else if (max_runs_reached) {
          cli::cli_alert_info(
            "Weighted MAPE of {round(weighted_mape * 100, 2)}% is above the goal of {round(weighted_mape_goal * 100, 2)}%. Stopping iterations as max runs is reached."
          )
          wmape_goal_reached <- FALSE
        } else {
          cli::cli_alert_info(
            "Weighted MAPE of {round(weighted_mape * 100, 2)}% is above the goal of {round(weighted_mape_goal * 100, 2)}%. Continuing to next iteration."
          )
          wmape_goal_reached <- FALSE
        }

        # determine next node based on conditions
        if (wmape_goal_reached || max_runs_reached) {
          next_node <- "finalize_run"
          if (!isTRUE(selection_ok)) {
            ctx$completion_reason <- "quality_rejected"
            ctx$abort_reason <- "No complete eligible forecast was available before the iteration limit."
          }
        } else {
          next_node <- "refresh_reason_history"
        }

        return(list(ctx = ctx, `next` = next_node))
      }
    ),
    refresh_reason_history = list(
      fn = "load_reason_history",
      `next` = NULL,
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info,
        combo = combo,
        run_log_paths = "{ctx$run_log_paths}"
      ),
      branch = function(ctx) {
        ctx$reason_history <- ctx$results$load_reason_history
        return(list(ctx = ctx, `next` = "start"))
      }
    ),
    finalize_run = list(
      fn = "finalize_run",
      `next` = "stop",
      retry_mode = "plain",
      max_retry = 3,
      args = list(
        agent_info = agent_info,
        combo = combo,
        completion_reason = "{ctx$completion_reason}",
        abort_reason = "{ctx$abort_reason}",
        fallback_available = "{ctx$fallback_available}"
      )
    ),
    stop = list(fn = NULL)
  )

  init_ctx <- list(
    node      = "start",
    iter      = 0, # iteration counter
    max_iter  = max_iter, # loop limit
    results   = list(), # where each tool's output will be stored
    attempts  = list(), # retry bookkeeping for execute_node()
    reason_history = reason_history,
    completion_reason = "completed",
    abort_reason = NULL,
    fallback_available = fallback_available,
    run_log_paths = run_log_paths
  )

  if (!is.null(agent_info$custom_agent_contract)) {
    init_ctx$custom_agent_info <- agent_info
    for (node in names(workflow)) {
      if ("agent_info" %in% names(workflow[[node]]$args)) {
        workflow[[node]]$args$agent_info <- "{ctx$custom_agent_info}"
      }
    }
  }

  # a global iteration cut off after training runs first, under its own run name
  if (!is.null(resume_run) && is.null(combo) && is.null(agent_info$custom_agent_contract)) {
    init_ctx <- resume_global_iteration(agent_info, workflow, init_ctx, resume_run)
  }

  # run the graph
  run_graph(agent_info$llm, workflow, init_ctx)
}

#' Run-name prefix of an Agent run's global iterations
#'
#' @param agent_info Agent info of the run.
#'
#' @return `agent_<run_id>_<hash of "all">_`; the iteration's timestamp
#'   suffix follows it.
#' @noRd
global_run_prefix <- function(agent_info) {
  paste0("agent_", agent_info$run_id, "_", hash_data("all"), "_")
}

#' Find a global iteration that was cut off after training
#'
#' A global iteration is recorded (counted by [load_run_results()]) only by
#' the last write of [log_best_run()], after best-model selection and the
#' per-series best-run fan-out. An attempt stopped anywhere after training
#' (for example a Synapse timeout) leaves a run whose 1.5 to 2 hours of
#' training a restart would redo. This finds the run to resume: the latest
#' run of this Agent run that has no recorded result (`agent_version` is
#' missing), whose run log has every value [train_models()] writes when
#' training finishes, that was created after the latest recorded iteration of
#' this run (an older one was superseded), and that trained with `seed`, which
#' training compares with its run log.
#'
#' Only global run logs not already read for the history are read: on local
#' and mounted storage those in `run_log_paths` that are not recorded
#' iterations; on blob and OneDrive storage (no start listing) the run logs
#' are listed once. Custom-model runs are never resumed; a restart re-runs
#' their iteration as before.
#'
#' @param agent_info Agent info of the run.
#' @param run_log_paths The global workflow's start listing from
#'   [agent_run_log_paths()], or `NULL` on blob and OneDrive storage.
#' @param previous_runs History from [load_run_results()]; its `run_logs`
#'   attribute names the recorded iterations.
#' @param seed Random seed of the current call.
#'
#' @return The run's run-log row (a one-row data frame), or `NULL` when no run
#'   qualifies.
#' @noRd
find_resumable_global_run <- function(agent_info, run_log_paths, previous_runs, seed) {
  if (!is.null(agent_info$custom_agent_contract)) {
    return(NULL)
  }

  project_info <- agent_info$project_info
  child_project <- paste0(project_info$project_name, "_", hash_data("all"))
  prefix <- global_run_prefix(agent_info)
  as_time <- function(value) {
    if (inherits(value, "POSIXct")) value else lubridate::ymd_hms(value, tz = "UTC", quiet = TRUE)
  }

  recorded_all <- attr(previous_runs, "run_logs")
  if (!is.data.frame(recorded_all) || !all(c("run_name", "created") %in% names(recorded_all))) {
    recorded_all <- NULL
  }
  recorded_times <- if (is.null(recorded_all)) NULL else {
    as_time(recorded_all$created[startsWith(as.character(recorded_all$run_name), prefix)])
  }
  recorded_times <- recorded_times[!is.na(recorded_times)]
  latest_recorded <- if (length(recorded_times)) max(recorded_times) else NULL

  logs <- if (is.null(run_log_paths)) {
    get_run_info(
      project_name = child_project,
      run_name = NULL,
      storage_object = project_info$storage_object,
      path = project_info$path
    )
  } else {
    recorded_files <- if (is.null(recorded_all) || !nrow(recorded_all)) {
      character()
    } else {
      paste0(
        hash_data(child_project), "-",
        vapply(as.character(recorded_all$run_name), hash_data, character(1), USE.NAMES = FALSE), ".csv"
      )
    }
    unread <- run_log_paths[!fs::path_file(run_log_paths) %in% recorded_files]
    if (!length(unread)) {
      return(NULL)
    }
    read_file(
      list(storage_object = NULL, path = project_info$path),
      file_list = unread,
      return_type = "df",
      allow_missing = TRUE
    )
  }

  trained_columns <- c("run_global_models", "run_local_models", "global_model_recipes", "feature_selection", "seed")
  if (!is.data.frame(logs) || !nrow(logs) || !all(c("run_name", "created", trained_columns) %in% names(logs))) {
    return(NULL)
  }

  created <- as_time(logs$created)
  unrecorded <- if ("agent_version" %in% names(logs)) is.na(logs$agent_version) else rep(TRUE, nrow(logs))
  eligible <- startsWith(as.character(logs$run_name), prefix) & unrecorded &
    stats::complete.cases(as.data.frame(logs)[trained_columns]) &
    suppressWarnings(as.numeric(logs$seed)) %in% as.numeric(seed) &
    !is.na(created) & (if (is.null(latest_recorded)) TRUE else created > latest_recorded)
  if (!any(eligible)) {
    return(NULL)
  }

  candidates <- which(eligible)
  latest <- candidates[order(created[candidates], as.character(logs$run_name[candidates]), decreasing = TRUE)][[1]]
  logs[latest, , drop = FALSE]
}

#' Rebuild a cut-off global iteration's settings from its run log
#'
#' Each stage of the resumed run compares its settings with the run log and
#' stops with "Inputs have recently changed" on any difference, so the
#' proposal is rebuilt from the logged values themselves, in their logged
#' order (not canonicalized), with missing values as `"NULL"`. It is then
#' checked as a new proposal would be: [validate_agent_proposal()] against the
#' models available now and the allowed global approaches, and
#' [agent_inputs_history_check()] against the recorded iterations.
#'
#' @param agent_info Agent info with `global_forecast_approaches` resolved.
#' @param run_log The run-log row from [find_resumable_global_run()].
#' @param previous_run_results Formatted run history, or `"No Previous Runs"`.
#'
#' @return The validated inputs for [submit_fcst_run()], or `NULL` (with an
#'   info message naming the run and reason) when the settings are no longer
#'   valid or a recorded iteration already ran them.
#' @noRd
resume_global_run_inputs <- function(agent_info, run_log, previous_run_results) {
  run_name <- as.character(run_log$run_name[[1]])
  logged <- function(field) {
    value <- if (field %in% names(run_log)) run_log[[field]][[1]] else NA
    if (length(value) != 1L || is.na(value) || !nzchar(trimws(as.character(value)))) "NULL" else as.character(value)
  }
  flag <- function(field) {
    if (field %in% names(run_log)) as.logical(run_log[[field]][[1]]) else NA
  }

  proposal <- list(
    models_to_run = logged("models_to_run"),
    external_regressors = logged("external_regressors"),
    clean_missing_values = flag("clean_missing_values"),
    clean_outliers = flag("clean_outliers"),
    forecast_approach = logged("forecast_approach"),
    stationary = flag("stationary"),
    feature_selection = flag("feature_selection"),
    multistep_horizon = flag("multistep_horizon"),
    seasonal_period = logged("seasonal_period"),
    recipes_to_run = logged("recipes_to_run"),
    lag_periods = logged("lag_periods"),
    rolling_window_periods = logged("rolling_window_periods"),
    reasoning = "Resume the interrupted iteration with its saved settings."
  )

  inputs <- tryCatch(
    validate_agent_proposal(
      input_list = proposal,
      agent_info = agent_info,
      combo = NULL,
      available_models = get_available_agent_models(NULL, get_foundation_model_suffix())
    ),
    finnts_reason_proposal_invalid = function(error) error
  )
  if (inherits(inputs, "condition")) {
    reason <- conditionMessage(inputs)
    cli::cli_alert_info("Not resuming global iteration '{run_name}': {reason}")
    return(NULL)
  }
  inputs$negative_forecast <- if (!is.null(agent_info$negative_forecast)) agent_info$negative_forecast else FALSE

  if (is.data.frame(previous_run_results) &&
    agent_inputs_history_check(inputs, agent_info, NULL, previous_run_results)$duplicate) {
    cli::cli_alert_info("Not resuming global iteration '{run_name}': a recorded iteration already ran its settings.")
    return(NULL)
  }
  inputs
}

#' Finish a cut-off global iteration before reasoning about the next one
#'
#' Runs the workflow's own submit, output, metrics and logging steps, with
#' their usual retries, for the run from [find_resumable_global_run()] under
#' its original timestamp, so [submit_fcst_run()] reuses its run name and the
#' finished preparation and training. The workflow's logging branch then
#' counts the iteration and decides the next step, exactly as for a new
#' iteration. If the settings can't be rebuilt, or the steps still fail after
#' their retries, the workflow starts with reasoning, as a restart did before;
#' a failure is reported with the run name and error.
#'
#' Side effects: writes what recording the iteration writes (its run-log
#' values and per-series best-run records).
#'
#' @param agent_info Agent info of the global workflow.
#' @param workflow The global workflow graph from [fcst_agent_workflow()].
#' @param ctx The graph's initial context.
#' @param resume_run The run-log row of the iteration to finish.
#'
#' @return The context to start the graph from: after the resumed iteration
#'   (at the refresh or finalize step), or `ctx` unchanged.
#' @noRd
resume_global_iteration <- function(agent_info, workflow, ctx, resume_run) {
  inputs <- resume_global_run_inputs(agent_info, resume_run, ctx$reason_history$previous_run_results)
  if (is.null(inputs)) {
    return(ctx)
  }

  run_name <- as.character(resume_run$run_name[[1]])
  cli::cli_alert_info("Resuming global iteration '{run_name}': its training finished before the run was interrupted.")

  steps <- workflow[c("submit_fcst_run", "get_fcst_output", "calculate_fcst_metrics", "log_best_run")]
  steps$submit_fcst_run$args$timestamp <- substring(run_name, nchar(global_run_prefix(agent_info)) + 1L)
  steps$log_best_run$branch <- NULL
  steps$log_best_run$`next` <- "stop"
  steps$stop <- list(fn = NULL)
  resume_ctx <- ctx
  resume_ctx$node <- "submit_fcst_run"
  resume_ctx$results$reason_inputs <- inputs

  resumed <- tryCatch(run_graph(agent_info$llm, steps, resume_ctx), error = function(error) error)
  if (inherits(resumed, "error")) {
    reason <- conditionMessage(resumed)
    cli::cli_alert_warning("Could not resume global iteration '{run_name}': {reason} Starting a new iteration.")
    return(ctx)
  }

  decision <- workflow$log_best_run$branch(resumed)
  decision$ctx$node <- decision$`next`
  decision$ctx
}

#' Abort an invalid agent input proposal
#'
#' @param field Name of the invalid setting.
#' @param value Proposed value.
#' @param reason Explanation of the validation failure.
#'
#' @return This function does not return.
#' @noRd
abort_invalid_agent_proposal <- function(field, value, reason) {
  proposed_value <- paste(value, collapse = "---")
  rlang::abort(
    paste0(
      "Invalid proposed ", field, ": ", reason,
      " Please propose a valid value or ABORT."
    ),
    class = "finnts_reason_proposal_invalid",
    field = field,
    proposed_value = proposed_value,
    action = "correct_or_abort"
  )
}

#' Abort an exhausted agent setting search
#'
#' @param field Name of the exhausted setting.
#' @param reason Explanation of the exhausted search space.
#'
#' @return This function does not return.
#' @noRd
abort_exhausted_agent_search <- function(field, reason) {
  rlang::abort(
    paste0(reason, " Stop modifying ", field, " or ABORT."),
    class = "finnts_reason_search_exhausted",
    field = field,
    action = "abort_optimization"
  )
}

#' Parse a serialized agent setting
#'
#' @param value Proposed value.
#' @param field Setting name used in errors.
#' @param allow_null Whether the string `"NULL"` is allowed.
#'
#' @return A character vector or the string `"NULL"`.
#' @noRd
parse_agent_values <- function(value, field, allow_null = TRUE) {
  if (!is.character(value) || length(value) != 1 || is.na(value) || !nzchar(trimws(value))) {
    abort_invalid_agent_proposal(
      field,
      value,
      "use one non-empty '---'-separated character value."
    )
  }

  value <- trimws(value)
  if (identical(value, "NULL")) {
    if (allow_null) {
      return("NULL")
    }
    abort_invalid_agent_proposal(field, value, "'NULL' is not allowed for this setting.")
  }

  values <- trimws(strsplit(value, "---", fixed = TRUE)[[1]])
  if (length(values) == 0 || any(!nzchar(values))) {
    abort_invalid_agent_proposal(field, value, "values cannot be empty.")
  }
  values
}

#' Parse a logical agent setting
#'
#' @param value Proposed value.
#' @param field Setting name used in errors.
#'
#' @return A logical scalar.
#' @noRd
parse_agent_logical <- function(value, field) {
  if (is.logical(value) && length(value) == 1 && !is.na(value)) {
    return(value)
  }
  if (is.character(value) && length(value) == 1 && !is.na(value)) {
    normalized <- toupper(trimws(value))
    if (normalized %in% c("TRUE", "FALSE")) {
      return(normalized == "TRUE")
    }
  }
  abort_invalid_agent_proposal(field, value, "use exactly TRUE or FALSE.")
}

#' Parse a positive whole-number agent setting
#'
#' @param value Proposed value.
#' @param field Setting name used in errors.
#'
#' @return A numeric vector or the string `"NULL"`.
#' @noRd
parse_agent_periods <- function(value, field) {
  values <- parse_agent_values(value, field, allow_null = TRUE)
  if (identical(values, "NULL")) {
    return("NULL")
  }

  periods <- suppressWarnings(as.numeric(values))
  if (any(!is.finite(periods)) || any(periods <= 0) || any(periods != floor(periods))) {
    abort_invalid_agent_proposal(
      field,
      value,
      "use 'NULL' or positive finite whole-number periods."
    )
  }
  periods
}

#' Get models available to an agent proposal
#'
#' @param combo A combo hash for local models, or NULL for global models.
#' @param foundation_suffix Serialized available foundation-model suffix.
#'
#' @return A character vector of model names.
#' @noRd
get_available_agent_models <- function(combo, foundation_suffix) {
  foundation_models <- sub("^---", "", foundation_suffix)
  foundation_models <- if (nzchar(foundation_models)) {
    strsplit(foundation_models, "---", fixed = TRUE)[[1]]
  } else {
    character(0)
  }
  available <- unique(c(
    setdiff(list_models(), list_foundation_models()),
    foundation_models
  ))
  if (is.null(combo)) {
    available <- intersect(available, list_global_models())
  }
  available
}

#' Validate and format an agent proposal
#'
#' @param input_list Parsed LLM response.
#' @param agent_info Agent metadata.
#' @param combo A combo hash for local models, or NULL for global models.
#' @param available_models Models currently available to this workflow.
#'
#' @return A validated, formatted input list.
#' @noRd
validate_agent_proposal <- function(input_list,
                                    agent_info,
                                    combo,
                                    available_models) {
  models <- parse_agent_values(input_list$models_to_run, "models_to_run", allow_null = FALSE)
  unknown_models <- setdiff(models, available_models)
  if (length(unknown_models) > 0) {
    abort_invalid_agent_proposal(
      "models_to_run",
      unknown_models,
      paste0("models are unavailable. Choose from: ", paste(available_models, collapse = "---"), ".")
    )
  }

  external_regressors <- parse_agent_values(
    input_list$external_regressors,
    "external_regressors",
    allow_null = TRUE
  )
  if (!identical(external_regressors, "NULL")) {
    unknown_regressors <- setdiff(external_regressors, agent_info$external_regressors)
    if (length(unknown_regressors) > 0) {
      abort_invalid_agent_proposal(
        "external_regressors",
        unknown_regressors,
        "regressors are not available in the project metadata."
      )
    }
  }

  forecast_approach <- parse_agent_values(
    input_list$forecast_approach,
    "forecast_approach",
    allow_null = FALSE
  )
  allowed_approaches <- if (is.null(combo)) {
    agent_info$global_forecast_approaches %||% "bottoms_up"
  } else {
    "bottoms_up"
  }
  if (length(forecast_approach) != 1 || !forecast_approach %in% allowed_approaches) {
    abort_invalid_agent_proposal(
      "forecast_approach",
      forecast_approach,
      paste0("choose from: ", paste(allowed_approaches, collapse = "---"), ".")
    )
  }

  recipes <- parse_agent_values(input_list$recipes_to_run, "recipes_to_run", allow_null = TRUE)
  if (!identical(recipes, "NULL") && any(!recipes %in% c("R1", "R2", "all"))) {
    abort_invalid_agent_proposal(
      "recipes_to_run",
      recipes,
      "use 'NULL', R1, R2, all, or a supported combination."
    )
  }
  if (is.null(combo) &&
    !identical(recipes, "NULL") &&
    !any(recipes %in% c("R1", "all"))) {
    abort_invalid_agent_proposal(
      "recipes_to_run",
      recipes,
      "global model proposals must prepare R1."
    )
  }

  seasonal_period <- parse_agent_seasonal_period(input_list$seasonal_period)
  lag_periods <- parse_agent_periods(input_list$lag_periods, "lag_periods")
  rolling_periods <- parse_agent_periods(
    input_list$rolling_window_periods,
    "rolling_window_periods"
  )
  if (!identical(seasonal_period, "NULL") &&
    !any(models %in% c("stlm-arima", "stlm-ets", "tbats"))) {
    abort_invalid_agent_proposal(
      "seasonal_period",
      seasonal_period,
      "custom periods require stlm-arima, stlm-ets, or tbats in models_to_run."
    )
  }

  reasoning <- input_list$reasoning
  if (!is.character(reasoning) || length(reasoning) != 1 || is.na(reasoning) || !nzchar(trimws(reasoning))) {
    abort_invalid_agent_proposal("reasoning", reasoning, "provide one non-empty explanation.")
  }

  input_list$models_to_run <- models
  input_list$external_regressors <- external_regressors
  input_list$clean_missing_values <- parse_agent_logical(
    input_list$clean_missing_values,
    "clean_missing_values"
  )
  input_list$clean_outliers <- parse_agent_logical(input_list$clean_outliers, "clean_outliers")
  input_list$forecast_approach <- forecast_approach
  input_list$stationary <- parse_agent_logical(input_list$stationary, "stationary")
  input_list$feature_selection <- parse_agent_logical(
    input_list$feature_selection,
    "feature_selection"
  )
  input_list$multistep_horizon <- parse_agent_logical(
    input_list$multistep_horizon,
    "multistep_horizon"
  )
  input_list$seasonal_period <- seasonal_period
  input_list$recipes_to_run <- recipes
  input_list$lag_periods <- lag_periods
  input_list$rolling_window_periods <- rolling_periods
  input_list$reasoning <- trimws(reasoning)
  input_list
}

#' Validate a canonical agent setting change budget
#'
#' @param value Serialized setting value.
#' @param default_value Resolved default setting value.
#' @param field Setting name used in errors.
#'
#' @return A canonical setting signature.
#' @noRd
canonicalize_agent_period_setting <- function(value, default_value, field) {
  value <- as.character(value)
  if (length(value) == 0 || is.na(value[[1]]) || !nzchar(value[[1]]) || value[[1]] == "NULL") {
    values <- default_value
  } else {
    values <- suppressWarnings(as.numeric(strsplit(value[[1]], "---", fixed = TRUE)[[1]]))
  }
  if (any(!is.finite(values))) {
    abort_invalid_agent_proposal(field, value, "periods must be finite numeric values.")
  }
  paste(sort(unique(signif(values, digits = 12))), collapse = "---")
}

#' Count canonical non-default agent setting changes
#'
#' @param previous_values Serialized prior setting values.
#' @param default_value Resolved default setting value.
#' @param field Setting name used in errors.
#'
#' @return The number of distinct non-default settings.
#' @noRd
count_agent_setting_changes <- function(previous_values, default_value, field) {
  default_canonical <- canonicalize_agent_period_setting("NULL", default_value, field)
  previous_canonical <- unique(vapply(
    previous_values,
    canonicalize_agent_period_setting,
    character(1),
    default_value = default_value,
    field = field
  ))
  length(setdiff(previous_canonical, default_canonical))
}

#' Validate a canonical agent setting change budget
#'
#' @param previous_values Serialized prior setting values.
#' @param proposed_value Proposed setting value.
#' @param default_value Resolved default setting value.
#' @param field Setting name used in errors.
#' @param max_changes Maximum distinct non-default settings.
#'
#' @return NULL invisibly.
#' @noRd
validate_agent_setting_change_budget <- function(previous_values,
                                                 proposed_value,
                                                 default_value,
                                                 field,
                                                 max_changes = 3L) {
  default_canonical <- canonicalize_agent_period_setting("NULL", default_value, field)
  previous_canonical <- unique(vapply(
    previous_values,
    canonicalize_agent_period_setting,
    character(1),
    default_value = default_value,
    field = field
  ))
  proposed_canonical <- canonicalize_agent_period_setting(
    proposed_value,
    default_value,
    field
  )
  if (proposed_canonical %in% previous_canonical) {
    return(invisible(NULL))
  }

  prior_change_count <- count_agent_setting_changes(
    previous_values,
    default_value,
    field
  )
  if (prior_change_count >= max_changes && proposed_canonical != default_canonical) {
    abort_exhausted_agent_search(
      field,
      paste0(
        "Cannot propose more than ", max_changes, " unique ", field,
        " changes within the current agent version."
      )
    )
  }
  invisible(NULL)
}

#' Normalize previous-version inputs for LLM replay
#'
#' @param previous_version_results Earlier-version run history.
#' @param warn_invalid Whether to warn about invalid legacy seasonal periods.
#'
#' @return A replay-safe copy of the earlier-version run history.
#' @noRd
normalize_previous_version_replay_inputs <- function(previous_version_results,
                                                     warn_invalid = TRUE) {
  if (!is.data.frame(previous_version_results) || nrow(previous_version_results) == 0) {
    return(previous_version_results)
  }

  replay_results <- previous_version_results
  nullable_fields <- intersect(
    c(
      "external_regressors",
      "recipes_to_run",
      "lag_periods",
      "rolling_window_periods",
      "seasonal_period"
    ),
    names(replay_results)
  )

  for (field in nullable_fields) {
    field_values <- as.character(replay_results[[field]])
    field_values[is.na(field_values)] <- "NULL"
    replay_results[[field]] <- field_values
  }

  if (!"seasonal_period" %in% names(replay_results)) {
    return(replay_results)
  }

  seasonal_values <- replay_results$seasonal_period
  invalid_values <- character(0)

  for (index in seq_along(seasonal_values)) {
    value <- seasonal_values[[index]]
    if (identical(toupper(trimws(value)), "NULL")) {
      seasonal_values[[index]] <- "NULL"
      next
    }

    parsed_values <- suppressWarnings(
      as.numeric(strsplit(value, "---", fixed = TRUE)[[1]])
    )
    is_valid <- tryCatch(
      {
        validate_seasonal_period(parsed_values)
        TRUE
      },
      error = function(error) FALSE
    )

    if (!is_valid) {
      invalid_values <- c(invalid_values, value)
      seasonal_values[[index]] <- "NULL"
    }
  }

  replay_results$seasonal_period <- seasonal_values

  if (warn_invalid && length(invalid_values) > 0) {
    warning(
      "Previous agent version results contain invalid seasonal_period values: ",
      paste0("'", unique(invalid_values), "'", collapse = ", "),
      ". Using 'NULL' so cadence defaults are replayed.",
      call. = FALSE
    )
  }

  replay_results
}

#' Create an in-memory reasoning history snapshot
#'
#' @param previous_run_results Formatted previous run results.
#' @param agent_version Current agent version.
#'
#' @return A list containing current-version results, earlier-version replay
#'   context, and the current-version completed run count.
#' @noRd
new_reason_history <- function(previous_run_results, agent_version = NULL) {
  current_run_results <- previous_run_results
  previous_version_results <- if (is.data.frame(previous_run_results)) {
    previous_run_results[0, , drop = FALSE]
  } else {
    "No Previous Runs"
  }

  if (is.data.frame(previous_run_results) &&
      !is.null(agent_version) &&
      "agent_version" %in% names(previous_run_results)) {
    current_agent_version <- agent_version
    current_run_results <- previous_run_results %>%
      dplyr::filter(.data$agent_version == current_agent_version)
    previous_version_results <- previous_run_results %>%
      dplyr::filter(.data$agent_version < current_agent_version)
  }

  previous_version_results <- normalize_previous_version_replay_inputs(
    previous_version_results,
    warn_invalid = is.data.frame(current_run_results) && nrow(current_run_results) == 0
  )

  list(
    previous_run_results = current_run_results,
    current_run_results = current_run_results,
    previous_version_results = previous_version_results,
    total_runs = if (is.data.frame(current_run_results)) nrow(current_run_results) else 0L
  )
}

#' Load one reasoning history snapshot
#'
#' @param agent_info Agent metadata.
#' @param combo A combo hash for local models, or NULL for global models.
#' @param run_log_paths The workflow's known run logs to read by exact path, or
#'   `NULL` to list them (see [load_run_results()]).
#'
#' @return An in-memory reasoning history snapshot.
#' @noRd
load_reason_history <- function(agent_info, combo = NULL, run_log_paths = NULL) {
  new_reason_history(
    load_run_results(agent_info = agent_info, combo = combo, run_log_paths = run_log_paths),
    agent_info$agent_version
  )
}

#' Parse an agent-proposed seasonal period
#'
#' @param value A serialized seasonal-period proposal from the reasoning LLM.
#'
#' @return The string `"NULL"` or a validated numeric vector.
#' @noRd
parse_agent_seasonal_period <- function(value) {
  if (identical(value, "NULL")) {
    return("NULL")
  }

  if (is.null(value) || length(value) == 0) {
    abort_invalid_agent_proposal(
      "seasonal_period",
      value,
      "use 'NULL' or propose one to three unique, finite numeric values greater than 1."
    )
  }

  proposed_periods <- if (is.character(value)) {
    if (length(value) != 1) {
      value
    } else {
      suppressWarnings(as.numeric(strsplit(value, "---", fixed = TRUE)[[1]]))
    }
  } else {
    value
  }

  tryCatch(
    validate_seasonal_period(proposed_periods),
    error = function(error) {
      abort_invalid_agent_proposal(
        "seasonal_period",
        value,
        paste0(
          conditionMessage(error),
          " Use 'NULL' or propose one to three unique, finite numeric values greater than 1."
        )
      )
    }
  )
}

#' Generate Finn run inputs for the reasoning LLM
#'
#' @param agent_info A list containing agent information including LLMs, project info, and other parameters.
#' @param combo A character string representing the combo variables, or NULL for global model.
#' @param weighted_mape_goal A numeric value representing the weighted MAPE goal.
#' @param last_error A character string representing the last error message, or NULL if no errors.
#' @details Active custom contracts require complete proposals before legacy
#'   defaults or coercion. Retry history and setting-change budgets are unchanged.
#'
#' @return A list containing the LLM response and the parsed JSON object.
#' @noRd
reason_inputs <- function(agent_info,
                          combo = NULL,
                          weighted_mape_goal,
                          last_error = NULL,
                          previous_run_results = NULL,
                          previous_version_results = NULL,
                          total_runs = NULL) {
  # get metadata
  llm <- agent_info$llm

  project_info <- agent_info$project_info

  # load one snapshot when the caller did not supply one
  if (is.null(previous_run_results) ||
      is.null(previous_version_results) ||
      is.null(total_runs)) {
    all_run_results <- if (is.null(previous_run_results)) {
      load_run_results(agent_info = agent_info, combo = combo)
    } else {
      previous_run_results
    }
    reason_history <- new_reason_history(all_run_results, agent_info$agent_version)
    previous_run_results <- reason_history$previous_run_results
    if (is.null(previous_version_results)) {
      previous_version_results <- reason_history$previous_version_results
    }
    if (is.null(total_runs)) {
      total_runs <- reason_history$total_runs
    }
  }

  if (is.data.frame(previous_run_results) && nrow(previous_run_results) > 0) {
    best_mape <- previous_run_results %>%
      dplyr::filter(best_run == "yes") %>%
      dplyr::pull(weighted_mape) %>%
      unique(round(4))

    best_run <- previous_run_results %>%
      dplyr::filter(best_run == "yes") %>%
      dplyr::pull(run_number)
    if (!length(best_mape)) best_mape <- "NA"
    if (!length(best_run)) best_run <- "NA"

    lag_changes_allowed <- count_agent_setting_changes(
      previous_run_results$lag_periods,
      get_lag_periods(
        NULL,
        agent_info$project_info$date_type,
        agent_info$forecast_horizon,
        TRUE,
        TRUE
      ),
      "lag_periods"
    ) < 3

    rolling_changes_allowed <- count_agent_setting_changes(
      previous_run_results$rolling_window_periods,
      get_rolling_window_periods(NULL, agent_info$project_info$date_type),
      "rolling_window_periods"
    ) < 3

    seasonal_changes_allowed <- if (!is.null(combo) &&
      "seasonal_period" %in% names(previous_run_results)) {
      count_agent_setting_changes(
        previous_run_results$seasonal_period,
        get_seasonal_periods(agent_info$project_info$date_type),
        "seasonal_period"
      ) < 3
    } else {
      TRUE
    }
  } else {
    best_mape <- "NA"
    best_run <- "NA"
    lag_changes_allowed <- TRUE
    rolling_changes_allowed <- TRUE
    seasonal_changes_allowed <- TRUE
  }

  # create final prompt
  final_prompt <- glue::glue(
    "
      -----LATEST CONTEXT-----
      Leverage all previous and current info provided to recommend the best inputs to create the most accurate forecast as possible.

      -----LATEST METADATA-----
      - run count : <<run_count>>
      - best weighted MAPE from current-version runs : <<best_mape>>
      - best run number from current-version runs : <<best_run>>
      - lag_changes_allowed : <<lag_changes>>
      - rolling_changes_allowed : <<rolling_changes>>
      - seasonal_changes_allowed : <<seasonal_changes>>

      -----CURRENT AGENT VERSION RUN RESULTS-----
      <<current_run_results>>

      -----PREVIOUS AGENT VERSION RESULTS-----
      <<previous_version_run_results>>

      -----LAST ERROR-----
      <<last_error>>

      -----END OUTPUT-----",
    .open = "<<", .close = ">>",
    current_run_results = if (is.data.frame(previous_run_results) && nrow(previous_run_results) == 0) {
      "No Previous Runs"
    } else {
      make_pipe_table(previous_run_results)
    },
    previous_version_run_results = if (is.data.frame(previous_version_results) && nrow(previous_version_results) == 0) {
      "No Previous Runs"
    } else {
      make_pipe_table(previous_version_results)
    },
    run_count = total_runs,
    best_mape = best_mape,
    best_run = best_run,
    lag_changes = lag_changes_allowed,
    rolling_changes = rolling_changes_allowed,
    seasonal_changes = seasonal_changes_allowed,
    last_error = ifelse(is.null(last_error), "No errors in previous input recommendation.", last_error),
    agent_version = agent_info$agent_version
  )

  # send prompt to LLM
  response <- agent_chat_text(llm$chat(final_prompt, echo = FALSE))

  # extract out json from response and convert to list
  input_list <- tryCatch(
    extract_json_object(response),
    error = function(error) {
      abort_invalid_agent_proposal(
        "response",
        "<invalid response>",
        paste0("return one valid JSON object. ", conditionMessage(error))
      )
    }
  )

  # check if the response is an abort schema
  if ("abort" %in% names(input_list) && input_list$abort == "TRUE") {
    cli::cli_alert_info("LLM has aborted the run. Reason: {input_list$reasoning}")
    return(input_list)
  }

  # force specific inputs if single time series
  if (!is.null(agent_info$custom_agent_contract)) {
    input_list <- validate_agent_custom_proposal(input_list, agent_info$custom_agent_contract, combo,
      agent_info$custom_foundation_suffix %||% "")
  } else {
  if (!is.null(combo)) {
    input_list$forecast_approach <- "bottoms_up"
  }

  # check if all required fields are present
  required_fields <- c(
    "models_to_run", "external_regressors", "clean_missing_values",
    "clean_outliers", "forecast_approach",
    "stationary", "feature_selection", "multistep_horizon",
    "seasonal_period", "recipes_to_run", "lag_periods",
    "rolling_window_periods", "reasoning"
  )

  missing_fields <- setdiff(required_fields, names(input_list))

  # warn if any required fields are missing
  if (length(missing_fields) > 0 & !is.null(combo)) {
    cli::cli_alert_warning(
      "Missing fields in LLM response: {paste(missing_fields, collapse = ', ')}. Using default values."
    )
  }

  # Check foundation model availability
  fm_suffix <- get_foundation_model_suffix()
  fm_global_suffix <- if (grepl("chronos2", fm_suffix)) "---chronos2" else ""

  # fill in missing fields with default values
  if (is.null(combo)) {
    default_values <- list(
      models_to_run = paste0("xgboost", fm_global_suffix),
      external_regressors = "NULL",
      clean_missing_values = TRUE,
      clean_outliers = FALSE,
      forecast_approach = "bottoms_up",
      stationary = TRUE,
      feature_selection = TRUE,
      multistep_horizon = TRUE,
      seasonal_period = "NULL",
      recipes_to_run = "R1",
      lag_periods = "NULL",
      rolling_window_periods = "NULL",
      reasoning = "no reasoning provided"
    )
  } else {
    default_values <- list(
      models_to_run = paste0("arima---ets---tbats---stlm-arima---xgboost---glmnet", fm_suffix),
      external_regressors = "NULL",
      clean_missing_values = TRUE,
      clean_outliers = FALSE,
      forecast_approach = "bottoms_up",
      stationary = FALSE,
      feature_selection = TRUE,
      multistep_horizon = FALSE,
      seasonal_period = "NULL",
      recipes_to_run = "R1",
      lag_periods = "NULL",
      rolling_window_periods = "NULL",
      reasoning = "no reasoning provided"
    )
  }

  for (field in required_fields) {
    if (!(field %in% names(input_list))) {
      input_list[[field]] <- default_values[[field]]
    }
  }

  input_list <- validate_agent_proposal(
    input_list = input_list,
    agent_info = agent_info,
    combo = combo,
    available_models = get_available_agent_models(combo, fm_suffix)
  )
  }

  # inject negative_forecast from agent_info (user-controlled, not LLM-decided)
  input_list$negative_forecast <- if (!is.null(agent_info$negative_forecast)) {
    agent_info$negative_forecast
  } else {
    FALSE
  }

  # check if these inputs were used before in previous runs
  if (is.data.frame(previous_run_results)) {
    history_check <- agent_inputs_history_check(input_list, agent_info, combo, previous_run_results)
    check_inputs <- history_check$check_inputs
    previous_runs_tbl <- history_check$previous_runs_tbl

    if (history_check$duplicate) {
      cli::cli_alert_info("The proposed input parameters have already been used in a previous run.")

      abort_exhausted_agent_search(
        "parameter set",
        sprintf(
          "Duplicate parameter set detected. Inputs proposed:\n%s",
          jsonlite::toJSON(check_inputs, auto_unbox = TRUE)
        )
      )
    }

    validate_agent_setting_change_budget(
      previous_runs_tbl$lag_periods,
      check_inputs$lag_periods,
      get_lag_periods(
        NULL,
        agent_info$project_info$date_type,
        agent_info$forecast_horizon,
        input_list$multistep_horizon,
        TRUE
      ),
      "lag_periods"
    )
    validate_agent_setting_change_budget(
      previous_runs_tbl$rolling_window_periods,
      check_inputs$rolling_window_periods,
      get_rolling_window_periods(NULL, agent_info$project_info$date_type),
      "rolling_window_periods"
    )
    if (!is.null(combo)) {
      validate_agent_setting_change_budget(
        previous_runs_tbl$seasonal_period,
        check_inputs$seasonal_period,
        get_seasonal_periods(agent_info$project_info$date_type),
        "seasonal_period"
      )
    }
  }

  return(input_list)
}

#' Compare proposed Agent inputs with the current version's history
#'
#' The duplicate check of [reason_inputs()], shared with resuming a cut-off
#' global iteration ([resume_global_run_inputs()]). Proposed values are joined
#' the way run logs store them, both sides are canonicalized with
#' [normalize_agent_run_inputs()] (defaults resolved, multipart values sorted),
#' and only columns the history has are compared.
#'
#' @param input_list Validated proposal, including `negative_forecast`.
#' @param agent_info Agent metadata; only its own version's rows are compared.
#' @param combo A combo hash for local models, or NULL for global models.
#' @param previous_run_results Formatted run history (a data frame).
#'
#' @return A list: `duplicate` (whether a current-version run used the same
#'   effective settings), `check_inputs` (the proposal as compared) and
#'   `previous_runs_tbl` (current-version rows with text multipart values),
#'   which [reason_inputs()] also uses for its setting-change budgets.
#' @noRd
agent_inputs_history_check <- function(input_list, agent_info, combo, previous_run_results) {
  previous_runs_tbl <- previous_run_results %>%
    dplyr::filter(agent_version == agent_info$agent_version)

  check_inputs <- input_list
  check_inputs$agent_version <- agent_info$agent_version
  check_inputs$models_to_run <- collapse_or_na(check_inputs$models_to_run)
  check_inputs$external_regressors <- collapse_or_na(check_inputs$external_regressors)
  check_inputs$recipes_to_run <- collapse_or_na(check_inputs$recipes_to_run)
  check_inputs$lag_periods <- collapse_or_na(check_inputs$lag_periods)
  check_inputs$rolling_window_periods <- collapse_or_na(check_inputs$rolling_window_periods)
  check_inputs$seasonal_period <- collapse_or_na(check_inputs$seasonal_period)
  check_inputs <- check_inputs[names(check_inputs) %in% names(previous_runs_tbl)]

  cols_to_fix <- c(
    "models_to_run",
    "external_regressors",
    "recipes_to_run",
    "lag_periods",
    "rolling_window_periods",
    "seasonal_period"
  )

  previous_runs_tbl <- previous_runs_tbl %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(intersect(names(previous_runs_tbl), cols_to_fix)),
        base::as.character
      )
    )

  default_recipes <- if (is.null(combo)) "R1" else NULL
  normalized_check_inputs <- normalize_agent_run_inputs(
    check_inputs,
    date_type = agent_info$project_info$date_type,
    forecast_horizon = agent_info$forecast_horizon,
    default_recipes = default_recipes
  )
  normalized_previous_runs <- normalize_agent_run_inputs(
    previous_runs_tbl,
    date_type = agent_info$project_info$date_type,
    forecast_horizon = agent_info$forecast_horizon,
    default_recipes = default_recipes
  )

  list(
    duplicate = does_param_set_exist(normalized_check_inputs, normalized_previous_runs),
    check_inputs = check_inputs,
    previous_runs_tbl = previous_runs_tbl
  )
}

# Reject a saved default only when every individual path is structurally unusable.
# All-No flags alone do not prove rejection: require complete candidate coverage
# or final_models' existing unavailable-accuracy result and finalization inputs.
# The latter preserves genuine incomplete-model rejections without new metadata.
# A usable candidate with saved fits returns TRUE for selection-only recovery;
# otherwise damaged output returns FALSE for ordinary repair.
# Reads exact forecasts and preparation without writes or model fitting.
# Provider errors propagate; a confirmed rejection raises the typed condition.
reject_saved_default_forecasts <- function(run_info, run_log, combos, global) {
  cache <- new.env(parent = emptyenv())
  recoverable <- FALSE
  finalized_rejection <- all(c("average_models", "max_model_average",
    "weekly_to_daily", "weighted_mape") %in% names(run_log)) &&
    isTRUE(is.na(run_log[["weighted_mape"]]))
  for (combo in combos) {
    rows <- read_update_artifact(run_info, local_artifact_path(run_info, "forecasts",
      if (global) "-global_models" else "-single_models", hash_data(combo)))
    required <- c("Combo", "Model_ID", "Train_Test_ID", "Date", "Forecast", "Target",
      "Best_Model", "lo_80", "lo_95", "hi_80", "hi_95")
    if (!is.data.frame(rows) || !nrow(rows) || !all(required %in% names(rows)) ||
        anyNA(rows[, c("Combo", "Model_ID", "Best_Model")]) ||
        any(rows$Combo != combo) || any(!nzchar(rows$Model_ID)) ||
        any(rows$Best_Model != "No")) next
    rows <- native_forecast_rows(rows, run_log$date_type)
    coverage <- read_update_keys(run_info, combo, recipes = run_log[["recipes_to_run"]])
    if (is.null(coverage) || anyNA(rows$Train_Test_ID) ||
        any(!rows$Train_Test_ID %in% c(coverage$required$Train_Test_ID, coverage$non_delivery_ids))) next
    delivery <- rows[!rows$Train_Test_ID %in% coverage$non_delivery_ids, , drop = FALSE]
    complete <- vapply(split(delivery, delivery$Model_ID), function(candidate) {
      forecast_keys_complete(candidate, coverage$required)
    }, logical(1))
    if ((!length(complete) || !all(complete) ||
        !setequal(names(complete), unique(rows$Model_ID))) && !finalized_rejection) next
    selection <- select_series_forecasts(rows,
      read_series_history(run_info, combo, run_log, cache),
      read_selection_file(run_info, "prep_models", "-train_test_split", cache = cache),
      allow_plausibility_fallback = TRUE, require_clean_selection = TRUE)
    if (is.na(selection$selected_id)) {
      rlang::abort("The default replacement was already rejected and cannot be fitted again.",
        class = "finnts_forecast_selection_rejected", combo = combo)
    }
    models <- read_update_artifact(run_info, local_artifact_path(run_info, "models",
      "-single_models", hash_data(if (global) "All-Data" else combo), run_info$object_output))
    if (is.data.frame(models) && all(c("Model_ID", "Model_Fit") %in% names(models)) &&
        !anyNA(models$Model_ID) && !anyDuplicated(models$Model_ID)) {
      index <- match(selection$selected_id, models$Model_ID)
      recoverable <- recoverable || (!is.na(index) && !is.null(models$Model_Fit[[index]]))
    }
  }
  recoverable
}

#' Submit a Finn forecasting run
#'
#' @details Default results are reused only when their current models and
#'   forecasts remain readable and pass acceptance recomputed from saved output.
#'   No acceptance status is persisted. Otherwise a worker-local
#'   `rebuild_update_models` value bypasses model/final-output caches for this
#'   submission, without changing preparation, saved log fields, or retry limits.
#'   A structurally unusable rejected default still cannot be submitted again.
#'   Older plausibility-only rejections are reselected from saved predictions
#'   and fits without preparation or training. The default policy is attached
#'   only to finalization, never fitted objects or metadata. Existing
#'   output validation precedes returning a successful run. Input CSV series
#'   identities remain text, and known recipe settings are reused for coverage.
#'   Custom submissions verify parent identity and complete fixed-rule settings,
#'   then pass only the exact approved envelopes selected in `models_to_run` into
#'   ordinary standard preparation, so built-in-only children run without custom
#'   enrollment.
#'   Default replacements also retain the current update marker, so accepted
#'   results can be audited independently from predecessor replay choices.
#'   Restarted global custom children skip averages and ensembles in their
#'   custom audit, because parallel finalization writes those later; no
#'   listing is added.
#'   Two in-memory run info fields, never saved: a local run sets
#'   `single_series_project` (its project holds one series, so forecast reads
#'   skip the condensed-file listing), and a global run carries the forecast its
#'   output check read as `checked_forecast`, for `use_checked_forecast()`.
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param inputs A list of inputs for the forecasting run.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#' @param timestamp A timestamp for the run, used to create a unique run name.
#' @param parallel_processing Logical indicating if parallel processing should be used.
#' @param inner_parallel Logical indicating if inner parallel processing should be used.
#' @param num_cores Number of cores to use for parallel processing. If NULL, defaults to the number of available cores.
#' @param seed An integer seed for reproducibility. Default is 123.
#'
#' @return A list containing the run information including project name, run name, storage object, path, data output, and object output.
#' @noRd
submit_fcst_run <- function(agent_info,
                            inputs,
                            combo,
                            timestamp,
                            parallel_processing = NULL,
                            inner_parallel = FALSE,
                            num_cores = NULL,
                            seed = 123) {
  agent_info <- load_agent_custom_state(agent_info)
  custom_contract <- agent_info$custom_agent_contract
  if (!is.null(custom_contract)) {
    agent_custom_preflight(agent_info, parallel_processing)
    inputs <- agent_custom_submission(inputs, agent_info, combo)
  }
  cli::cli_alert_info(
    "Starting Finn forecasting run with inputs: {jsonlite::toJSON(inputs, auto_unbox = TRUE)}"
  )

  # get metadata
  project_info <- agent_info$project_info
  back_test_scenarios <- agent_info$back_test_scenarios
  back_test_spacing <- agent_info$back_test_spacing
  pca <- NULL

  # read all combos or just one
  if (is.null(combo)) {
    combo_value <- "*"
    global_models <- TRUE
    local_models <- FALSE
  } else {
    combo_value <- combo
    global_models <- FALSE
    local_models <- TRUE
  }

  # adjust parallel processing for combos
  if (!is.null(parallel_processing)) {
    if (!is.null(combo) & parallel_processing == "spark") {
      # turn off parallel processing when running single combo
      final_parallel <- NULL
      parallel_processing <- NULL
      prep_parallel <- NULL
    } else if (is.null(combo) & parallel_processing == "spark") {
      # local parallel process instead of spark on global models
      # use spark when doing model averaging on multiple global models
      final_parallel <- "spark"
      parallel_processing <- NULL
      prep_parallel <- "local_machine"
    } else {
      final_parallel <- parallel_processing
      prep_parallel <- parallel_processing
    }
  } else {
    final_parallel <- parallel_processing
    prep_parallel <- parallel_processing
  }

  # get input data
  input_data <- if (!is.null(combo) && is.null(project_info$storage_object) &&
    project_info$data_output %in% c("csv", "parquet", "rds")) {
    input_info <- project_info
    input_info$run_name <- agent_info$run_id
    read_local_artifacts(input_info,
      local_artifact_path(input_info, "input_data", combo = combo),
      character_columns = "Combo"
    )
  } else read_file(
    run_info = project_info,
    file_list = list_files(
      project_info$storage_object,
      paste0(
        project_info$path, "/input_data/*", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", combo_value, ".", project_info$data_output
      )
    ),
    return_type = "df", character_columns = "Combo"
  )

  # adjust inputs based on data
  if (nrow(input_data) >= 10000 && is.null(custom_contract)) {
    pca <- TRUE
  }

  # create unique run name and project name
  run_name <- paste0(
    "agent_",
    agent_info$run_id, "_",
    ifelse(is.null(combo), hash_data("all"), combo_value), "_",
    timestamp
  )

  project_name <- paste0(
    agent_info$project_info$project_name,
    "_",
    ifelse(is.null(combo), hash_data("all"), combo_value)
  )

  # kick off Finn run
  if (!is.null(custom_contract)) {
    child_info <- project_info
    child_info$project_name <- project_name
    child_info$run_name <- run_name
    prior <- read_selection_file(child_info, "logs", optional = TRUE)
    if (nrow(prior) && !identical(prior[["custom_agent_contract_id"]], agent_info$custom_agent_contract_id)) {
      stop("Custom Agent child parent identity changed; start a new run.", call. = FALSE)
    }
    if (nrow(prior) && all(c("run_local_models", "run_global_models", "negative_forecast", "feature_selection") %in% names(prior))) {
      # global children later finalize on parallel workers, so the restart
      # audit skips outputs that step writes rather than probing their names
      audit_agent_custom_child(agent_info, child_info, vapply(unique(input_data$Combo), hash_data, character(1)),
        complete = FALSE, skip = if (is.null(combo)) c("-average_models", "-ensemble_models") else character())
    }
    pca <- FALSE
  }
  run_info <- set_run_info(
    project_name = project_name,
    run_name = run_name,
    storage_object = project_info$storage_object,
    path = project_info$path,
    data_output = project_info$data_output,
    object_output = project_info$object_output,
    add_unique_id = FALSE
  )

  if (!is.null(combo)) {
    # adjust to prevent unnecessary list_files() calls in spark
    run_info$combo <- combo_value
    # in memory only: this project holds one series, so it has no condensed
    # forecasts and get_forecast_data() skips listing for them
    run_info$single_series_project <- TRUE
  }
  run_info$allow_quality_rejection <- isTRUE(agent_info$allow_quality_rejection) || isTRUE(agent_info$default_reforecast)
  if (isTRUE(agent_info$default_reforecast)) {
    default_log <- read_selection_file(run_info, "logs")
    if ("average_models" %in% names(default_log)) {
      combos <- unique(as.character(input_data$Combo))
      current_result <- read_update_result(run_info, combos,
        global_models, agent_info$forecast_horizon, default_log$forecast_approach,
        recipes = default_log[["recipes_to_run"]])
      if (!is.null(current_result) ||
          reject_saved_default_forecasts(run_info, default_log, combos, global_models)) {
        restored <- if (!is.null(current_result)) {
          assess_agent_run(run_info, default_log, combos, check_quality = TRUE)
        } else NULL
        if (is.null(restored) || !agent_selection_summary(restored, check_quality = TRUE)$acceptable) {
          final_info <- run_info
          final_info$default_reforecast <- TRUE
          restored <- final_models(final_info, average_models = default_log$average_models,
            max_model_average = default_log$max_model_average,
            weekly_to_daily = project_info$weekly_to_daily,
            parallel_processing = final_parallel, inner_parallel = inner_parallel, num_cores = num_cores)
        }
        if (!agent_selection_summary(restored, check_quality = TRUE,
          allow_plausibility_fallback = TRUE)$acceptable) {
          rlang::abort("The default replacement was already rejected and cannot be fitted again.",
            class = "finnts_forecast_selection_rejected", combo = combos)
        }
        run_info$forecast_selection <- restored[c("selections", "source_selections", "rejected_combos")]
        run_info$selection_combos <- names(restored$selections)
        checked <- validate_run_outputs(run_info, combo, keep_forecast = TRUE)
        if (is.data.frame(checked)) run_info$checked_forecast <- checked
        return(run_info)
      }
      run_info$rebuild_update_models <- TRUE
    }
  }

  # clean and prepare data for training
  prep_data(
    run_info = run_info,
    input_data = input_data,
    combo_variables = project_info$combo_variables,
    target_variable = "Target",
    date_type = project_info$date_type,
    forecast_horizon = agent_info$forecast_horizon,
    external_regressors = null_converter(inputs$external_regressors),
    hist_start_date = agent_info$hist_start_date,
    hist_end_date = agent_info$hist_end_date,
    combo_cleanup_date = agent_info$combo_cleanup_date,
    fiscal_year_start = project_info$fiscal_year_start,
    clean_missing_values = inputs$clean_missing_values,
    clean_outliers = inputs$clean_outliers,
    box_cox = FALSE,
    stationary = inputs$stationary,
    forecast_approach = inputs$forecast_approach,
    parallel_processing = prep_parallel,
    num_cores = num_cores,
    fourier_periods = NULL,
    lag_periods = null_converter(inputs$lag_periods),
    rolling_window_periods = null_converter(inputs$rolling_window_periods),
    recipes_to_run = null_converter(inputs$recipes_to_run),
    multistep_horizon = inputs$multistep_horizon
  )

  # prepare models for training
  if (!is.null(custom_contract)) {
    child_log <- read_exact_artifact(run_info, local_artifact_path(run_info, "logs", extension = "csv"))
    child_log$custom_agent_contract_id <- agent_info$custom_agent_contract_id
    if (isTRUE(agent_info$default_reforecast)) {
      record <- read_agent_custom_update(agent_info, TRUE)
      if (is.null(combo) || !combo %in% names(record$inputs) ||
        !identical(run_info$run_name, agent_custom_default_run(record, combo))) {
        stop("Custom update default child identity changed.", call. = FALSE)
      }
      child_log$custom_agent_update_id <- paste0("custom-update-", record$manifest_id)
    }
    write_data(child_log, NULL, run_info, "log", "logs")
  }
  prep_models(
    run_info = run_info,
    back_test_scenarios = back_test_scenarios,
    back_test_spacing = back_test_spacing,
    models_to_run = null_converter(inputs$models_to_run),
    models_not_to_run = NULL,
    run_ensemble_models = FALSE,
    pca = pca,
    num_hyperparameters = 10,
    seasonal_period = null_converter(inputs$seasonal_period),
    custom_models = if (!is.null(custom_contract)) {
      selected_custom <- intersect(names(custom_contract$envelopes), inputs$models_to_run)
      if (length(selected_custom)) custom_contract$envelopes[selected_custom] else NULL
    } else {
      NULL
    }
  )

  # train models
  train_models(
    run_info = run_info,
    run_global_models = global_models,
    run_local_models = local_models,
    global_model_recipes = c("R1"),
    feature_selection = inputs$feature_selection,
    negative_forecast = inputs$negative_forecast,
    parallel_processing = parallel_processing,
    inner_parallel = inner_parallel,
    num_cores = num_cores,
    seed = seed,
    debug = FALSE
  )

  # use spark for model averaging only when multiple global models are run
  if (identical(final_parallel, "spark") && global_models) {
    global_model_count <- length(intersect(
      null_converter(inputs$models_to_run),
      list_global_models()
    ))
    if (global_model_count <= 1) {
      final_parallel <- NULL
    }
  }

  # evaluate models
  final_info <- run_info
  final_info$default_reforecast <- isTRUE(agent_info$default_reforecast)
  selection_result <- tryCatch(final_models(
    run_info = final_info,
    average_models = TRUE,
    max_model_average = 3,
    weekly_to_daily = project_info$weekly_to_daily,
    parallel_processing = final_parallel,
    inner_parallel = inner_parallel,
    num_cores = num_cores
  ), finnts_forecast_selection_rejected = function(error) {
    if (!isTRUE(run_info$allow_quality_rejection)) stop(error)
    rejected_agent_selection(unique(as.character(input_data$Combo)), conditionMessage(error))
  })
  run_info$forecast_selection <- selection_result
  run_info$selection_combos <- names(selection_result$selections)
  if (!length(run_info$selection_combos)) {
    combos <- unique(as.character(input_data$Combo))
    log <- read_selection_file(run_info, "logs")
    evaluated <- assess_agent_run(run_info, log, combos,
      agent_info$selection_cache %||% new.env(parent = emptyenv()),
      check_quality = isTRUE(agent_info$default_reforecast),
      allow_plausibility_fallback = isTRUE(agent_info$default_reforecast))
    run_info$forecast_selection <- evaluated[c("selections", "source_selections", "rejected_combos")]
    run_info$selection_combos <- combos
  }
  if (isTRUE(agent_info$default_reforecast)) {
    default_log <- read_selection_file(run_info, "logs")
    quality_missing <- vapply(run_info$forecast_selection$selections, function(selection) {
      if (is.null(selection) || is.na(selection$selected_id)) return(FALSE)
      score <- selection$rankings$Violations[match(selection$selected_id, selection$rankings$Model_ID)]
      length(score) != 1 || is.na(score)
    }, logical(1))
    if (any(quality_missing) && !isTRUE(run_info$forecast_selection$quality_accepted)) {
      evaluated <- assess_agent_run(run_info, default_log, run_info$selection_combos, check_quality = TRUE,
        allow_plausibility_fallback = TRUE)
      run_info$forecast_selection <- evaluated[c("selections", "source_selections", "rejected_combos")]
      for (series in names(evaluated$selections)) {
        warn_forecast_fallback(evaluated$selections[[series]], series)
      }
    }
  }

  # validate that all outputs can be loaded before proceeding; a global run's
  # forecast read here is handed to the next step (use_checked_forecast())
  checked <- validate_run_outputs(
    run_info = run_info,
    combo = combo,
    keep_forecast = TRUE
  )
  if (is.data.frame(checked)) run_info$checked_forecast <- checked

  return(run_info)
}

#' Validate that all expected outputs from a forecast run can be loaded
#'
#' Checks that forecast data, trained model objects, and prep model objects
#' (train_test_split, model_hyperparameters, model_workflows) are readable
#' from disk. Stops with a descriptive error if any output cannot be loaded.
#'
#' @param run_info A list containing run information (project_name, run_name,
#'   storage_object, path, data_output, object_output).
#' @param combo A character string of the combo hash for local models, or NULL
#'   for global models.
#' @param saved_update Optional complete result already read and validated by
#'   read_update_result(). Reuses those persisted forecasts and fits without
#'   listing or downloading them again; preparation objects are still checked.
#' @param keep_forecast When TRUE and the forecast check is exactly the read
#'   `get_fcst_output()` makes next (a global run that is neither unpublished
#'   nor has rejected series), return that forecast so it isn't read again.
#'   Its warnings are not suppressed, as they are not in `get_fcst_output()`.
#'
#' @return TRUE invisibly if all outputs are valid, or the kept forecast table
#'   (invisibly) when `keep_forecast` applies.
#' @noRd
validate_run_outputs <- function(run_info, combo = NULL, saved_update = NULL, keep_forecast = FALSE) {
  # determine combo hash for file paths
  if (is.null(combo)) {
    forecast_combo <- "All-Data"
    model_combo_hash <- hash_data("All-Data")
  } else {
    forecast_combo <- combo
    model_combo_hash <- combo
  }

  # validate forecast data
  unpublished <- isTRUE(run_info$forecast_selection$unpublished)
  rejected <- !is.null(run_info$forecast_selection) && length(run_info$forecast_selection$rejected_combos)
  kept_forecast <- NULL
  fcst_tbl <- if (!is.null(saved_update)) saved_update$forecasts else if (isTRUE(keep_forecast) &&
    is.null(combo) && !unpublished && !rejected) {
    kept_forecast <- tryCatch(get_forecast_data(run_info), error = function(e) NULL)
    kept_forecast
  } else tryCatch(
    if (unpublished) {
      NULL
    } else if (rejected) {
      read_candidate_forecasts(run_info, run_info$selection_combos)
    } else {
      load_combo_forecast(combo = forecast_combo, run_info = run_info)
    },
    error = function(e) NULL
  ) %>%
    base::suppressWarnings()

  if (!unpublished && (is.null(fcst_tbl) || nrow(fcst_tbl) == 0)) {
    stop(
      "Failed to load forecast data for run '", run_info$run_name,
      "' (combo: ", forecast_combo, "). ",
      "Cannot log this as a best agent run.",
      call. = FALSE
    )
  }

  # validate trained models
  trained_models_tbl <- if (!is.null(saved_update)) saved_update$models else tryCatch(
    read_file(
      run_info = run_info,
      file_list = paste0(
        run_info$path, "/models/",
        hash_data(run_info$project_name), "-",
        hash_data(run_info$run_name), "-",
        model_combo_hash, "-single_models.",
        run_info$object_output
      ) %>% fs::path_tidy(),
      return_type = "df"
    ),
    error = function(e) NULL
  ) %>%
    base::suppressWarnings()

  if (is.null(trained_models_tbl) || nrow(trained_models_tbl) == 0) {
    stop(
      "Failed to load trained models for run '", run_info$run_name,
      "' (combo: ", forecast_combo, "). ",
      "Cannot log this as a best agent run.",
      call. = FALSE
    )
  }

  # validate prep model objects
  data_path <- paste0(
    run_info$path,
    "/prep_models/", hash_data(run_info$project_name), "-",
    hash_data(run_info$run_name)
  )

  prep_files <- list(
    train_test_split = paste0(data_path, "-train_test_split.", run_info$data_output),
    model_hyperparameters = paste0(data_path, "-model_hyperparameters.", run_info$object_output),
    model_workflows = paste0(data_path, "-model_workflows.", run_info$object_output)
  )

  for (file_name in names(prep_files)) {
    file_path <- fs::path_tidy(prep_files[[file_name]])

    result <- tryCatch(
      read_file(
        run_info = run_info,
        file_list = file_path,
        return_type = "df"
      ),
      error = function(e) NULL
    ) %>%
      base::suppressWarnings()

    if (is.null(result) || (is.data.frame(result) && nrow(result) == 0)) {
      stop(
        "Failed to load prep model object '", file_name,
        "' for run '", run_info$run_name,
        "' (combo: ", forecast_combo, "). ",
        "Cannot log this as a best agent run.",
        call. = FALSE
      )
    }
  }

  if (!is.null(kept_forecast)) {
    return(invisible(kept_forecast))
  }
  invisible(TRUE)
}

#' Hand a submitted run's checked forecast to the metrics step
#'
#' Branch of the `submit_fcst_run` workflow step. A global run's
#' `validate_run_outputs()` reads the same forecast `get_fcst_output()` would,
#' and `submit_fcst_run()` keeps it as `run_info$checked_forecast`. This moves
#' it to the `get_fcst_output` result and skips that step, so the forecast is
#' read once. Removing it from the run info keeps the table out of later steps,
#' such as best-run logging, which pass the run info on. Without a checked
#' forecast, `get_fcst_output()` runs as before.
#'
#' @param ctx Workflow context after `submit_fcst_run()`.
#'
#' @return A list with the updated `ctx` and the `next` step name.
#' @noRd
use_checked_forecast <- function(ctx) {
  checked <- ctx$results$submit_fcst_run$checked_forecast
  if (is.null(checked)) {
    return(list(ctx = ctx, `next` = "get_fcst_output"))
  }
  ctx$results$submit_fcst_run$checked_forecast <- NULL
  ctx$results$get_fcst_output <- checked
  list(ctx = ctx, `next` = "calculate_fcst_metrics")
}

#' Get forecast output from a Finn run
#'
#' @param run_info A list containing run information including project name, run name, storage object, path, data output, and object output.
#'
#' @return A tibble containing the forecast output.
#' @noRd
get_fcst_output <- function(run_info) {
  fcst_tbl <- if (isTRUE(run_info$forecast_selection$unpublished)) {
    tibble::tibble()
  } else if (!is.null(run_info$forecast_selection) && length(run_info$forecast_selection$rejected_combos)) {
    rows <- read_candidate_forecasts(run_info, run_info$selection_combos)
    splits <- read_selection_file(run_info, "prep_models", "-train_test_split")
    dplyr::left_join(rows, splits[, c("Train_Test_ID", "Run_Type")], by = "Train_Test_ID")
  } else {
    get_forecast_data(run_info)
  }

  return(fcst_tbl)
}

#' Calculate forecast metrics from a Finn run
#'
#' @param run_info A list containing run information including project name, run name, storage object, path, data output, and object output.
#' @param fcst_tbl A tibble containing the forecast output.
#' @param aggregate_wmape Optional native aggregate for update logging; per-series and model-pool metrics still use the forecast output.
#'
#' @return A numeric value representing the weighted MAPE of the forecast.
#' @noRd
calculate_fcst_metrics <- function(run_info,
                                   fcst_tbl,
                                   aggregate_wmape = NULL) {
  if (!is.null(run_info$forecast_selection) && length(run_info$forecast_selection$selections)) {
    # Reuse selection completeness, but score the delivered backtests. Native
    # weekly selection WMAPE can differ after daily allocation and rounding.
    summary <- agent_selection_summary(run_info$forecast_selection)
    accuracy <- agent_forecast_accuracy(fcst_tbl, names(run_info$forecast_selection$selections))
    if (!is.null(aggregate_wmape)) {
      if (!is.numeric(aggregate_wmape) || length(aggregate_wmape) != 1L ||
          !is.finite(aggregate_wmape) || aggregate_wmape < 0) {
        stop("The update aggregate WMAPE must be a single finite non-negative number.", call. = FALSE)
      }
      if (is.finite(accuracy$weighted_mape)) accuracy$weighted_mape <- round(aggregate_wmape, 4)
    }
    value <- if (isTRUE(summary$acceptable)) accuracy$weighted_mape else Inf
    # These attributes carry already-computed evidence to the logger; the scalar
    # remains compatible with callers that compare it to the accuracy goal.
    attr(value, "selection_ok") <- isTRUE(summary$acceptable) && is.finite(value)
    attr(value, "forecast_accuracy") <- accuracy
    attr(value, "model_accuracy") <- agent_model_accuracy(fcst_tbl)
    return(value)
  }
  # get weighted mape from run logging
  run_log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )

  weighted_mape <- run_log_df$weighted_mape

  if (is.null(weighted_mape)) {
    stop("No weighted MAPE found in run log. Ensure the run was completed successfully.", call. = FALSE)
  }

  return(weighted_mape)
}

#' Log the best run results
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param run_info A list containing run information including project name, run name, storage object, path, data output, and object output.
#' @param weighted_mape A numeric value representing the weighted MAPE of the forecast.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#' @param check_best_run Logical indicating if the best run check should be performed. Default is TRUE
#' @param run_history Existing in-memory iteration history, when available.
#'
#' @return NULL
#' @noRd
log_best_run <- function(agent_info,
                         run_info,
                         weighted_mape,
                         combo = NULL,
                         check_best_run = TRUE,
                         run_history = NULL) {
  if (!is.null(run_info$forecast_selection)) {
    return(log_selected_agent_run(agent_info, run_info, combo, check_best_run, weighted_mape, run_history))
  }
  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  # update the run log file with additional model accuracy information
  if (!is.null(combo)) {
    model_back_test_tbl <- load_combo_forecast(
      combo = combo,
      run_info = run_info
    ) %>%
      dplyr::filter(
        Run_Type == "Back_Test",
        Recipe_ID != "simple_average"
      )

    # calculate weighted mape by Model_ID
    model_wmape_tbl <- model_back_test_tbl %>%
      dplyr::mutate(
        Target = ifelse(Target == 0, 0.1, Target)
      ) %>%
      dplyr::group_by(Model_ID) %>%
      dplyr::mutate(
        MAPE = round(abs((Forecast - Target) / abs(Target)), digits = 4),
        Total = sum(abs(Target), na.rm = TRUE),
        Weight = (MAPE * abs(Target)) / Total
      ) %>%
      dplyr::summarise(weighted_mape = sum(Weight, na.rm = TRUE)) %>%
      dplyr::ungroup()

    avg_wmape <- model_wmape_tbl %>%
      dplyr::pull(weighted_mape) %>%
      mean()

    median_wmape <- model_wmape_tbl %>%
      dplyr::pull(weighted_mape) %>%
      median()

    std_wmape <- model_wmape_tbl %>%
      dplyr::pull(weighted_mape) %>%
      sd()

    # build log data frame (not saved to disk yet)
    log_df <- get_run_info(
      project_name = run_info$project_name,
      run_name = run_info$run_name,
      storage_object = run_info$storage_object,
      path = run_info$path
    ) %>%
      dplyr::mutate(
        model_avg_wmape = avg_wmape,
        model_median_wmape = median_wmape,
        model_std_wmape = std_wmape,
        agent_version = as.numeric(agent_info$agent_version),
        agent_forecast_approach = agent_info$forecast_approach
      )
  } else {
    # build log data frame (not saved to disk yet)
    log_df <- get_run_info(
      project_name = run_info$project_name,
      run_name = run_info$run_name,
      storage_object = run_info$storage_object,
      path = run_info$path
    ) %>%
      dplyr::mutate(
        model_avg_wmape = weighted_mape,
        model_median_wmape = weighted_mape,
        model_std_wmape = 0,
        agent_version = as.numeric(agent_info$agent_version),
        agent_forecast_approach = agent_info$forecast_approach
      )
  }

  # check if previous best run exists and is more accurate
  if (check_best_run) {
    previous_runs <- load_run_results(
      agent_info = agent_info,
      combo = combo,
      current_run_log = log_df
    )
  } else {
    # skip best run check when running update forecast
    previous_runs <- NULL
  }

  if (is.data.frame(previous_runs) && nrow(previous_runs) > 1) {
    # see if latest run was the best run
    last_run <- previous_runs %>%
      dplyr::filter(run_number == max(run_number))

    if (last_run$best_run == "yes") {
      log_results <- TRUE
    } else {
      log_results <- FALSE
    }
  } else {
    log_results <- TRUE
  }

  # log results if the new run is better or if no previous runs exist
  if (log_results) {
    if (is.null(combo)) {
      combo <- "all"
    }

    # get back test data; get_forecast_data() settles the all-series read after
    # listing its files once, so only the per-series read is settled here
    read_back_tests <- function() {
      load_combo_forecast(
        combo = ifelse(combo == "all", "All-Data", combo),
        run_info = run_info
      )
    }
    back_test_tbl <- if (combo == "all") {
      read_back_tests()
    } else read_settled(read_back_tests, best_models_complete, run_info$path, "selected forecasts")

    # validate that every combo has a best model
    validate_best_model(back_test_tbl, context = "log_best_run")

    back_test_tbl <- back_test_tbl %>%
      dplyr::filter(
        Best_Model == "Yes",
        Run_Type == "Back_Test"
      )

    combo_list <- unique(back_test_tbl$Combo)

    # for each combo, filter the back test data, calculate the weighted mape and save to logs
    for (combo_name in combo_list) {
      # filter the back test data for the current combo
      combo_data <- back_test_tbl %>%
        dplyr::filter(Combo == combo_name)

      # calculate mape and weight it by value of target colum
      wmape <- combo_data %>%
        dplyr::mutate(
          Target = ifelse(Target == 0, 0.1, Target)
        ) %>%
        dplyr::mutate(
          MAPE = round(abs((Forecast - Target) / abs(Target)), digits = 4),
          Total = sum(abs(Target), na.rm = TRUE),
          Weight = (MAPE * abs(Target)) / Total
        ) %>%
        dplyr::pull(Weight) %>%
        sum(na.rm = TRUE) %>%
        round(digits = 4)

      # check if previous log file exists, if so check if mape is better
      prev_log_df <- tryCatch(
        read_file(project_info,
          file_list = paste0(
            project_info$path,
            "/logs/", hash_data(project_info$project_name), "-", hash_data(agent_info$run_id),
            "-", hash_data(combo_name), "-agent_best_run.csv"
          ) %>% fs::path_tidy(),
          return_type = "df"
        ),
        error = function(e) {
          tibble::tibble()
        }
      ) %>%
        base::suppressWarnings()

      if (nrow(prev_log_df) > 0) {
        prev_wmape <- as.numeric(prev_log_df$weighted_mape)
        prev_model_type <- prev_log_df$model_type
      } else {
        prev_wmape <- Inf # if no previous log, assume previous was worse
        prev_model_type <- "global" # assume global model if no previous log
      }

      if (wmape < prev_wmape || (combo == "all" & prev_model_type == "global")) { # always log the wmape for global models with no local best run, but check for local models

        # log the best run
        best_run_log_df <- tibble::tibble(
          project_name = agent_info$project_info$project_name,
          agent_run_id = agent_info$run_id,
          best_run_name = run_info$run_name,
          model_type = ifelse(combo == "all", "global", "local"),
          combo = combo_name,
          weighted_mape = wmape,
          max_iterations = 0,
          run_complete = FALSE
        ) %>%
          dplyr::left_join(
            log_df %>%
              dplyr::select(-project_name, -path, -data_output, -object_output, -weighted_mape),
            by = dplyr::join_by(best_run_name == run_name)
          )

        write_data(
          x = best_run_log_df,
          combo = combo_name,
          run_info = project_info,
          output_type = "log",
          folder = "logs",
          suffix = "-agent_best_run"
        )
      } else {
        next # don't log if the previous run was better
      }
    }

    # verify that all expected agent_best_run files exist
    if (combo == "all" && is.null(project_info$storage_object)) {
      expected_files <- local_artifact_files(local_artifact_path(
        project_info, "logs", "-agent_best_run",
        combo = purrr::map_chr(combo_list, hash_data), extension = "csv"
      ), allow_missing = TRUE)
      existing_count <- sum(vapply(expected_files, function(path) {
        as.integer(nrow(read_file(project_info, file_list = path, strict = TRUE)) > 0L)
      }, integer(1)))
    } else if (combo == "all") {
      # global model: use list_files since multiple combos are involved
      existing_best_run_files <- list_files(
        project_info$storage_object,
        paste0(
          project_info$path, "/logs/*", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "*-agent_best_run.csv"
        ),
        fail_on_error = TRUE
      )
      existing_count <- length(existing_best_run_files)
    } else {
      # local model: single combo, read the file directly
      file_path <- paste0(
        project_info$path, "/logs/",
        hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-",
        combo, "-agent_best_run.csv"
      ) %>% fs::path_tidy()
      best_run_check <- read_file(
        run_info = project_info,
        file_list = file_path,
        return_type = "df"
      )
      existing_count <- if (nrow(best_run_check) > 0) 1L else 0L
    }

    expected_count <- length(combo_list)

    if (existing_count < expected_count) {
      stop(
        "Error in log_best_run(). Expected ", expected_count,
        " agent_best_run file(s) but only found ", existing_count,
        ".",
        call. = FALSE
      )
    }
  }

  # save the run log only after best run logging succeeds
  write_data(
    x = log_df,
    combo = NULL,
    run_info = run_info,
    output_type = "log",
    folder = "logs",
    suffix = NULL
  )

  return("Run logged successfully.")
}

# Read the saved per-series best-run records a global promotion compares
# against. `project_info` carries the Agent run ID as `run_name`; `series` are
# raw combo names. Returns a list named by `series` holding each record, or an
# empty tibble when none exists. `known` is the environment iterate_forecast()
# fills from its one best-run listing (series with saved records), extended as
# promotions write records on the coordinator. With it, only known series are
# read: an exact probe of a series without a record would let a FUSE mount (for
# example Synapse /synfs) cache "not found" and hide the record parallel local
# workers write later. No listing is added. Without it (direct callers), each
# series is read exactly as before. `pool` (from series_io_pool()) runs the
# exact reads on workers; NULL reads in this process. Read failures propagate.
read_agent_best_run_records <- function(project_info, series, known = NULL, pool = NULL) {
  records <- rep(list(tibble::tibble()), length(series))
  saved <- if (is.null(known)) rep(TRUE, length(series)) else series %in% known$series
  items <- lapply(series[saved], function(combo) list(project_info = project_info, combo = combo))
  records[saved] <- map_series_io(pool, items, read_agent_best_run_record)
  stats::setNames(records, series)
}

# Per-series task of read_agent_best_run_records(): `item` holds `project_info`
# and the raw `combo`. Returns the series' saved best-run record by exact path,
# or an empty tibble when it is missing; read errors propagate.
read_agent_best_run_record <- function(item) {
  read_selection_file(item$project_info, "logs", "-agent_best_run", item$combo, optional = TRUE)
}

# Per-series task of log_selected_agent_run(): writes `item$record` as the
# best-run record of raw series `item$series` under `item$project_info` (the
# Agent run). Returns the series name; write errors propagate.
write_agent_best_run_record <- function(item) {
  write_data(item$record, combo = item$series, run_info = item$project_info, output_type = "log",
    folder = "logs", suffix = "-agent_best_run")
  item$series
}

# Per-series task of log_selected_agent_run(): reads back the record just
# written for `item$series` by exact path and errors unless it is the one row
# naming `item$run_name`, that series and `item$agent_version`. Returns the
# series name.
check_agent_best_run_record <- function(item) {
  saved <- read_selection_file(item$project_info, "logs", "-agent_best_run", item$series)
  if (nrow(saved) != 1L || !identical(as.character(saved$best_run_name), item$run_name) ||
      !identical(as.character(saved$combo), item$series) ||
      !isTRUE(as.numeric(saved$agent_version) == as.numeric(item$agent_version))) {
    stop("Saved best-run record does not match the selected iteration for series: ", item$series, call. = FALSE)
  }
  item$series
}

# Persist a completed iteration using its in-memory selections and metric
# attributes; run_history supplies recorded search-context evidence, not forecasts.
# Global promotion is one run-level decision; per-series guards protect local
# winners. check_best_run = FALSE supports writing an update's accepted subset.
# Writes existing run/best-run logs and returns status plus retained/written combos.
# Exact post-write reads verify identity; storage errors propagate. These writes
# are not a transaction, so reload/update must also reject mixed global metadata.
# Every decision is made here first; the per-series reads, writes and read-backs
# then run on a series I/O pool when there are many series on mounted storage
# (see map_series_io()), with the same files and the run log still written last.
log_selected_agent_run <- function(agent_info, run_info, combo = NULL, check_best_run = TRUE,
                                   weighted_mape = NULL, run_history = NULL) {
  if (!is.null(agent_info$custom_agent_contract)) audit_agent_custom_child(agent_info, run_info,
    if (!is.null(combo)) combo else vapply(run_info$selection_combos, hash_data, character(1)))
  current_log <- read_selection_file(run_info, "logs")
  current_result <- list(selections = run_info$forecast_selection$selections,
    run_info = run_info, run_log = current_log)
  global <- isTRUE(as.logical(current_log$run_global_models))
  if (global) {
    expected <- if (check_best_run) {
      agent_info$selection_combos %||% run_info$selection_combos
    } else run_info$selection_combos
    missing <- setdiff(expected, names(current_result$selections))
    current_result$selections[missing] <- rep(list(NULL), length(missing))
  }
  summary <- agent_selection_summary(current_result)
  forecast_accuracy <- attr(weighted_mape, "forecast_accuracy", exact = TRUE)
  current_log <- record_agent_selection_attempt(current_log, current_result, agent_info,
    attr(weighted_mape, "model_accuracy", exact = TRUE), forecast_accuracy)
  promote_run <- TRUE
  if (check_best_run && is.data.frame(run_history)) {
    history <- attr(run_history, "run_logs", exact = TRUE) %||% run_history
    if ("run_name" %in% names(history)) {
      history <- history[is.na(history$run_name) | history$run_name != run_info$run_name, , drop = FALSE]
    }
    comparison <- dplyr::bind_rows(lapply(list(history, current_log), function(log) {
      columns <- intersect(c("weighted_mape", "model_avg_wmape", "agent_version", "selection_status"), names(log))
      rows <- log[, columns, drop = FALSE]
      for (column in intersect(c("weighted_mape", "model_avg_wmape", "agent_version"), columns)) {
        rows[[column]] <- as.numeric(rows[[column]])
      }
      if ("selection_status" %in% columns) rows$selection_status <- as.character(rows$selection_status)
      rows
    }))
    promote_run <- identical(best_agent_iteration(comparison, agent_info$agent_version), as.integer(nrow(comparison)))
  }
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  pool <- series_io_pool(project_info, length(current_result$selections))
  on.exit(series_io_close(pool), add = TRUE)
  previous_by_series <- NULL
  promote_global <- isTRUE(summary$acceptable) && is.finite(current_log$weighted_mape) && promote_run
  if (global && check_best_run) {
    previous_by_series <- read_agent_best_run_records(project_info, names(current_result$selections),
      agent_info$best_run_records, pool)
    previous_global <- dplyr::bind_rows(previous_by_series)
    if (nrow(previous_global)) {
      previous_global <- previous_global[
        !is.na(previous_global$model_type) & previous_global$model_type == "global" &
          !is.na(previous_global$agent_version) &
          as.numeric(previous_global$agent_version) == as.numeric(agent_info$agent_version), , drop = FALSE]
    }
    if (nrow(previous_global)) {
      validate_global_iteration(previous_global)
      previous_accuracy <- unique(as.numeric(previous_global$model_avg_wmape))
      if (length(previous_accuracy) != 1L || !is.finite(previous_accuracy)) {
        stop("Saved global iteration metadata is inconsistent. Restore one complete global best run before continuing.",
          call. = FALSE)
      }
      if (!is.data.frame(run_history)) {
        comparison <- data.frame(weighted_mape = c(previous_accuracy, current_log$weighted_mape),
          model_avg_wmape = c(previous_accuracy, current_log$weighted_mape))
        promote_global <- promote_global && identical(best_agent_iteration(comparison), 2L)
      }
    }
  }
  pending <- list()
  retained <- character()
  # Apply the global gate uniformly. A single global run may improve the overall
  # result while worsening one series; never mix its global rows with an old run.
  for (series in names(current_result$selections)) {
    selected <- current_result$selections[[series]]
    if (is.null(selected) || is.na(selected$selected_id)) next
    score <- selected$rankings[selected$rankings$Model_ID == selected$selected_id, , drop = FALSE]
    if (!usable_forecast_selection(selected)) next
    if (!is.null(forecast_accuracy)) {
      completed_wmape <- unname(forecast_accuracy$by_series[series])
      if (length(completed_wmape) != 1L || !is.finite(completed_wmape)) next
      score$WMAPE <- completed_wmape
    }
    score$WMAPE <- round(score$WMAPE, 4)
    if (global && !promote_global) {
      if (!is.null(previous_by_series) && nrow(previous_by_series[[series]]) > 0L) retained <- c(retained, series)
      next
    }
    if (check_best_run) {
      previous <- if (global) previous_by_series[[series]] else
        read_selection_file(project_info, "logs", "-agent_best_run", series, optional = TRUE)
      if (nrow(previous) > 1) stop("The saved best-run record is ambiguous for series: ", series, call. = FALSE)
      if (!promote_run) {
        if (nrow(previous)) retained <- c(retained, series)
        next
      }
      same_version <- nrow(previous) == 1 &&
        isTRUE(as.numeric(previous$agent_version) == as.numeric(agent_info$agent_version))
      # Advancing search settings is not permission to overwrite a superior local
      # forecast. Existing global rows instead follow the shared promotion gate.
      protect_individual <- !global || !identical(as.character(previous[["model_type"]]), "global")
      if (same_version && protect_individual && isTRUE(is.finite(as.numeric(previous$weighted_mape))) &&
          isTRUE(as.numeric(previous$weighted_mape) <= score$WMAPE)) {
        retained <- c(retained, series)
        next
      }
    }
    log <- current_log
    best_log <- log[, setdiff(names(log), c("project_name", "path", "data_output", "object_output", "weighted_mape")), drop = FALSE]
    best_log$best_run_name <- run_info$run_name
    best_log$project_name <- project_info$project_name
    best_log$agent_run_id <- agent_info$run_id
    best_log$model_type <- if (isTRUE(as.logical(log$run_global_models))) "global" else "local"
    best_log$combo <- series
    best_log$weighted_mape <- score$WMAPE
    best_log$max_iterations <- 0
    best_log$run_complete <- FALSE
    pending[[series]] <- best_log
  }
  written <- as.character(names(pending))
  map_series_io(pool, lapply(written, function(series) {
    list(project_info = project_info, series = series, record = pending[[series]])
  }), write_agent_best_run_record)
  # records written here (on the coordinator for global runs) are known to exist
  if (!is.null(agent_info$best_run_records)) {
    agent_info$best_run_records$series <- union(agent_info$best_run_records$series, written)
  }
  map_series_io(pool, lapply(written, function(series) {
    list(project_info = project_info, series = series, run_name = run_info$run_name,
      agent_version = agent_info$agent_version)
  }), check_agent_best_run_record)
  write_data(current_log, combo = NULL, run_info = run_info, output_type = "log", folder = "logs", suffix = NULL)
  list(status = current_log$selection_status, selected_combos = c(retained, written))
}

#' Finalize Agent Run Metadata
#'
#' This function updates the agent best run file for each combo by setting
#' the max_iterations and run_complete flags. For global models (combo = NULL or combo = "all"),
#' it updates all combo files. For local models, it updates a single combo file.
#' Identity columns are read as text before rewriting, preserving numeric-looking
#' run hashes and leading-zero series names while metrics retain their types.
#' Global records are rewritten one series per task with
#' [finalize_agent_best_run_record()], on a series I/O pool when there are many
#' on mounted storage (see [map_series_io()]); the rewritten files are the same.
#'
#' @param agent_info Agent info from `set_agent_info()`
#' @param combo A character string representing the hashed combo. If NULL or "all", updates all combos for global models.
#' @param completion_reason Why the workflow reached finalization.
#' @param abort_reason The exhausted reasoning message, when applicable.
#' @param fallback_available Whether a global workflow may continue to local models.
#'
#' @return Character string indicating success
#' @noRd
finalize_run <- function(agent_info,
                         combo = NULL,
                         completion_reason = "completed",
                         abort_reason = NULL,
                         fallback_available = FALSE) {
  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  max_iter <- agent_info$max_iter

  if (!is.null(combo)) {
    # local combo: read best_run file directly
    best_run_file <- paste0(
      project_info$path, "/logs/",
      hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "-",
      combo, "-agent_best_run.csv"
    ) %>% fs::path_tidy()

    best_run_tbl <- read_file(
      run_info = project_info,
      file_list = best_run_file,
      return_type = "df",
      character_columns = c("combo", "agent_run_id", "best_run_name")
    )

    if (nrow(best_run_tbl) == 0) {
      missing_message <- paste0(
        "Error in finalize_run(). No best run file found for combo hash: ",
        combo
      )
      if (identical(completion_reason, "reasoning_exhausted")) {
        stop(abort_reason, " ", missing_message, call. = FALSE)
      }
      stop(missing_message, call. = FALSE)
    }

    write_data(
      x = best_run_tbl %>% dplyr::mutate(max_iterations = max_iter, run_complete = TRUE),
      combo = best_run_tbl$combo[[1]],
      run_info = project_info,
      output_type = "log",
      folder = "logs",
      suffix = "-agent_best_run"
    )

    return("Run finalized successfully.")
  }

  # global models: load all best_run files and update each one
  best_run_tbl_all <- load_best_agent_run(agent_info = agent_info)

  if (nrow(best_run_tbl_all) == 0) {
    if (completion_reason %in% c("reasoning_exhausted", "quality_rejected") && isTRUE(fallback_available)) {
      return(list(
        status = "skipped",
        reason = abort_reason,
        continue_to_local = TRUE
      ))
    }
    if (identical(completion_reason, "reasoning_exhausted")) {
      stop(abort_reason, " Error in finalize_run(). No best run files found.", call. = FALSE)
    }
    stop("Error in finalize_run(). No best run files found.", call. = FALSE)
  }

  combo_list <- best_run_tbl_all %>%
    dplyr::filter(model_type == "global") %>%
    dplyr::pull(combo) %>%
    unique()

  if (length(combo_list) == 0) { # when global models have been run but local models are always better
    if (completion_reason %in% c("reasoning_exhausted", "quality_rejected") && isTRUE(fallback_available)) {
      return(list(
        status = "skipped",
        reason = abort_reason,
        continue_to_local = TRUE
      ))
    }
    if (identical(completion_reason, "reasoning_exhausted")) {
      stop(abort_reason, " Error in finalize_run(). No global best run files found.", call. = FALSE)
    }
    return("Run finalized successfully.")
  }

  pool <- series_io_pool(project_info, length(combo_list))
  on.exit(series_io_close(pool), add = TRUE)
  map_series_io(pool, lapply(combo_list, function(combo_name) {
    list(project_info = project_info, run_id = agent_info$run_id, combo = combo_name, max_iter = max_iter)
  }), finalize_agent_best_run_record)

  return("Run finalized successfully.")
}

#' Mark one global series' best-run record complete
#'
#' Per-series task of [finalize_run()], run in this process or on a series I/O
#' pool: reads the series' best-run record by exact path, with identity columns
#' as text, and rewrites it with `max_iterations` and `run_complete = TRUE`.
#'
#' @param item List with `project_info` (Agent run as `run_name`), `run_id`,
#'   the raw `combo` name and `max_iter`.
#'
#' @return The combo name. Errors when the record is missing or empty.
#' @noRd
finalize_agent_best_run_record <- function(item) {
  project_info <- item$project_info
  best_run_file <- paste0(
    project_info$path, "/logs/",
    hash_data(project_info$project_name), "-",
    hash_data(item$run_id), "-",
    hash_data(item$combo), "-agent_best_run.csv"
  ) %>% fs::path_tidy()

  best_run_tbl <- read_file(
    run_info = project_info,
    file_list = best_run_file,
    return_type = "df",
    character_columns = c("combo", "agent_run_id", "best_run_name")
  )

  if (nrow(best_run_tbl) == 0) {
    stop(
      paste0("Error in finalize_run(). No best run file found for combo: ", item$combo),
      call. = FALSE
    )
  }

  # update max_iterations and run_complete
  write_data(
    x = best_run_tbl %>% dplyr::mutate(max_iterations = item$max_iter, run_complete = TRUE),
    combo = item$combo,
    run_info = project_info,
    output_type = "log",
    folder = "logs",
    suffix = "-agent_best_run"
  )

  item$combo
}

#' Load previous run results for the agent
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#' @param current_run_log Optional data frame of the current run's log entry
#'   (not yet saved to disk). When supplied, it is appended to the on-disk runs
#'   so the best-run comparison can include the current run.
#' @param run_log_paths The workflow's run logs from [agent_run_log_paths()],
#'   read by exact path instead of listing the logs folder. `NULL` (the
#'   default) lists them with [get_run_info()]. Both read the same files the
#'   same way, so the result is the same.
#'
#' @return A tibble containing the previous run results or a message indicating no previous runs.
#'   Active custom histories require the same parent identity for current rows;
#'   incompatible earlier-version context is excluded. Global history retains
#'   explicit model subsets. Metrics and iteration-ranking rules are unchanged.
#' @noRd
load_run_results <- function(agent_info,
                             combo = NULL,
                             current_run_log = NULL,
                             run_log_paths = NULL) {
  # determine the combo value and columns to return
  if (is.null(combo)) {
    combo_value <- hash_data("all")

    column_list <- c(
      "agent_version", "run_number", "best_run", "weighted_mape",
      "external_regressors", "clean_missing_values", "clean_outliers",
      "stationary", "forecast_approach",
      "lag_periods", "rolling_window_periods",
      "multistep_horizon", "feature_selection",
      "negative_forecast"
    )
  } else {
    combo_value <- combo

    column_list <- c(
      "agent_version", "run_number", "best_run", "weighted_mape",
      "external_regressors", "clean_missing_values", "clean_outliers",
      "stationary", "box_cox", "forecast_approach",
      "lag_periods", "rolling_window_periods", "recipes_to_run",
      "multistep_horizon", "models_to_run", "pca",
      "seasonal_period", "num_hyperparameters", "feature_selection",
      "negative_forecast"
    )
  }

  # get previous runs
  project_name <- paste0(
    agent_info$project_info$project_name,
    "_",
    ifelse(is.null(combo), hash_data("all"), combo_value)
  )

  previous_runs <- if (is.null(run_log_paths)) {
    get_run_info(
      project_name = project_name,
      run_name = NULL,
      storage_object = agent_info$project_info$storage_object,
      path = agent_info$project_info$path
    )
  } else {
    read_file(
      list(storage_object = NULL, path = agent_info$project_info$path),
      file_list = run_log_paths,
      return_type = "df",
      allow_missing = TRUE
    )
  }
  if (!"selection_status" %in% names(previous_runs)) previous_runs$selection_status <- NA_character_

  # columns that must remain numeric for downstream arithmetic
  numeric_cols <- c(
    "weighted_mape", "model_avg_wmape", "model_median_wmape",
    "model_std_wmape", "agent_version"
  )

  # coerce numeric columns unconditionally to prevent type errors downstream
  for (col in intersect(numeric_cols, names(previous_runs))) {
    previous_runs[[col]] <- as.numeric(previous_runs[[col]])
  }

  # append the current run log if provided (not yet saved to disk)
  if (!is.null(current_run_log) && "run_name" %in% names(current_run_log)) {
    # remove the incomplete on-disk row for this run before appending
    current_run_name <- current_run_log$run_name[[1]]
    previous_runs <- previous_runs %>%
      dplyr::filter(run_name != current_run_name)

    # coerce shared columns to compatible types to avoid bind_rows errors
    common_cols <- intersect(names(previous_runs), names(current_run_log))
    for (col in common_cols) {
      prev_class <- class(previous_runs[[col]])[[1]]
      curr_class <- class(current_run_log[[col]])[[1]]
      if (prev_class != curr_class) {
        if (col %in% numeric_cols) {
          # keep numeric columns numeric
          previous_runs[[col]] <- as.numeric(previous_runs[[col]])
          current_run_log[[col]] <- as.numeric(current_run_log[[col]])
        } else {
          # coerce non-numeric columns to character when types disagree
          previous_runs[[col]] <- as.character(previous_runs[[col]])
          current_run_log[[col]] <- as.character(current_run_log[[col]])
        }
      }
    }

    # bind_rows fills missing columns with NA in either direction
    previous_runs <- dplyr::bind_rows(previous_runs, current_run_log)

    # enforce numeric type on key columns after merge
    for (col in intersect(numeric_cols, names(previous_runs))) {
      previous_runs[[col]] <- as.numeric(previous_runs[[col]])
    }
  }

  # filter previous runs based on the combo value and select relevant columns
  if ("run_name" %in% names(previous_runs) & "weighted_mape" %in% names(previous_runs) & "agent_version" %in% names(previous_runs)) {
    pattern <- sprintf("^agent_[^_]+_%s.*", combo_value)

    previous_runs_formatted <- previous_runs %>%
      dplyr::filter(stringr::str_detect(run_name, pattern)) %>%
      dplyr::mutate(created = lubridate::ymd_hms(created, tz = "UTC")) %>%
      dplyr::arrange(created, run_name) %>%
      dplyr::filter(!is.na(weighted_mape) | .data$selection_status %in% c("rejected", "partial")) %>%
      dplyr::filter(!is.na(agent_version)) %>%
      dplyr::filter(agent_forecast_approach == agent_info$forecast_approach) %>%
      dplyr::mutate(agent_run_id = stringr::str_extract(run_name, "agent_([^_]+)")) %>%
      dplyr::mutate(
        run_number = dplyr::row_number()
      ) %>%
      dplyr::ungroup() %>%
      dplyr::relocate(agent_version, run_number, weighted_mape)

    if (!is.null(agent_info$custom_agent_contract)) {
      marker <- previous_runs_formatted[["custom_agent_contract_id"]]
      if (is.null(marker)) marker <- rep(NA_character_, nrow(previous_runs_formatted))
      current <- as.numeric(previous_runs_formatted$agent_version) == as.numeric(agent_info$agent_version)
      matches <- !is.na(marker) & marker == agent_info$custom_agent_contract_id
      if (any(current & !matches)) stop("Current custom Agent history has inconsistent parent identity.", call. = FALSE)
      previous_runs_formatted <- previous_runs_formatted[matches, , drop = FALSE]
      column_list <- unique(c(column_list, "models_to_run", "recipes_to_run", "custom_agent_contract_id", "custom_pool_id", "custom_manifest_id"))
    }
    if (nrow(previous_runs_formatted) == 0) return("No Previous Runs")
    current_version <- agent_info$agent_version %||% max(previous_runs_formatted$agent_version)
    previous_runs_formatted$best_run <- "no"
    winner <- best_agent_iteration(previous_runs_formatted, current_version)
    if (!is.na(winner)) previous_runs_formatted$best_run[winner] <- "yes"
    run_output <- previous_runs_formatted %>%
      dplyr::select(tidyselect::any_of(c(column_list, "selection_status", "model_avg_wmape", "model_median_wmape", "model_std_wmape"))) %>%
      dplyr::relocate(agent_version, run_number, best_run, weighted_mape)
    attr(run_output, "run_logs") <- previous_runs_formatted
  } else {
    run_output <- "No Previous Runs"
  }

  return(run_output)
}

#' Foundation model availability registry
#'
#' Returns a combined "---model1---model2" suffix string for all foundation
#' models whose credentials are available. To add a new foundation model,
#' add one entry to the `registry` list: name + check function.
#'
#' @return A single character string (possibly empty) with "---" prefixed
#'   names of available foundation models.
#' @noRd
get_foundation_model_suffix <- function() {
  registry <- list(
    list(name = "timegpt", check = check_timegpt_available),
    list(name = "chronos2", check = check_chronos2_available),
    list(name = "chronos-bolt-base", check = check_chronos2_available),
    list(name = "chronos-bolt-tiny", check = check_chronos2_available),
    list(name = "timesfm", check = check_timesfm_available)
  )

  available <- vapply(registry, function(entry) {
    if (entry$check()) entry$name else NA_character_
  }, character(1))

  available <- available[!is.na(available)]

  if (length(available) == 0) {
    return("")
  }
  paste0("---", paste(available, collapse = "---"))
}

#' TimeGPT Availability
#'
#' Checks if TimeGPT API key is set (via environment variable or option) and
#' validates it by calling the Nixtla API.
#'
#' @return Logical indicating if TimeGPT is available.
#' @noRd
check_timegpt_available <- function() {
  tryCatch(
    {
      env_api_key <- Sys.getenv("NIXTLA_API_KEY")
      opt_api_key <- getOption("NIXTLA_API_KEY")

      has_key <- nzchar(env_api_key) || (!is.null(opt_api_key) && nzchar(opt_api_key))

      if (!has_key) {
        FALSE
      } else {
        nixtlar::nixtla_validate_api_key()
      }
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Chronos2 Availability
#'
#' Checks if Chronos2 API credentials are set (via environment variables).
#'
#' @return Logical indicating if Chronos2 is available.
#' @noRd
check_chronos2_available <- function() {
  tryCatch(
    {
      api_url <- Sys.getenv("CHRONOS_API_URL")
      api_token <- Sys.getenv("CHRONOS_API_TOKEN")

      nzchar(api_url) && nzchar(api_token)
    },
    error = function(e) {
      FALSE
    }
  )
}

#' TimesFM Availability
#'
#' Checks if TimesFM API credentials are set (via environment variables).
#'
#' @return Logical indicating if TimesFM is available.
#' @noRd
check_timesfm_available <- function() {
  tryCatch(
    {
      api_url <- Sys.getenv("TIMESFM_API_URL")
      api_token <- Sys.getenv("TIMESFM_API_TOKEN")

      nzchar(api_url) && nzchar(api_token)
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Collapse a vector into a string with "---" as separator, or return NA if the vector is "NULL"
#'
#' @param x A character vector to collapse.
#'
#' @return A character string with elements collapsed by "---", or NA if the input is "NULL".
#' @noRd
collapse_or_na <- function(x) {
  if (length(x) == 1 && identical(x, "NULL")) {
    NA_character_
  } else {
    paste(x, collapse = "---")
  }
}

#' Normalize agent inputs for semantic duplicate checks
#'
#' Resolves default-backed settings and sorts order-insensitive multipart
#' values without changing the raw values saved in run logs.
#'
#' @param inputs One proposed input row or a data frame of prior runs.
#' @param date_type Date granularity used to resolve defaults.
#' @param forecast_horizon Forecast horizon used to resolve lag defaults.
#' @param default_recipes Optional recipe default for contexts that do not
#'   retain `recipes_to_run`, such as global agent runs.
#'
#' @return A data frame with canonical values for duplicate comparison.
#' @noRd
normalize_agent_run_inputs <- function(inputs,
                                       date_type,
                                       forecast_horizon,
                                       default_recipes = NULL) {
  normalized <- as.data.frame(inputs, stringsAsFactors = FALSE)

  if (nrow(normalized) == 0) {
    return(normalized)
  }

  split_values <- function(value) {
    value <- as.character(value)

    if (length(value) == 0) {
      return(character())
    }

    value <- trimws(value[[1]])
    if (is.na(value) || !nzchar(value) || toupper(value) == "NULL") {
      return(character())
    }

    values <- unlist(
      strsplit(value, "---", fixed = TRUE),
      use.names = FALSE
    )
    values <- trimws(values)
    values[nzchar(values)]
  }

  collapse_values <- function(values, numeric_values = FALSE) {
    if (length(values) == 0) {
      return("NULL")
    }

    if (numeric_values) {
      values <- suppressWarnings(as.numeric(values))
      if (any(!is.finite(values))) {
        stop(
          "Agent numeric multipart settings must contain finite values.",
          call. = FALSE
        )
      }
      values <- sort(unique(signif(values, digits = 12)))
      values <- vapply(
        values,
        format,
        character(1),
        scientific = FALSE,
        trim = TRUE,
        digits = 12
      )
    } else {
      values <- sort(unique(as.character(values)))
    }

    paste(values, collapse = "---")
  }

  resolve_recipes <- function(value) {
    recipes <- split_values(value)

    if (length(recipes) == 0) {
      recipes <- if (is.null(default_recipes)) {
        get_recipes_to_run(NULL, date_type)
      } else {
        default_recipes
      }
    }

    if ("all" %in% tolower(recipes)) {
      recipes <- c("R1", "R2")
    }

    sort(unique(recipes))
  }

  row_recipes <- lapply(seq_len(nrow(normalized)), function(index) {
    value <- if ("recipes_to_run" %in% names(normalized)) {
      normalized$recipes_to_run[[index]]
    } else {
      NA_character_
    }
    resolve_recipes(value)
  })

  normalize_column <- function(column, default, numeric_values = FALSE) {
    if (!column %in% names(normalized)) {
      return(invisible(NULL))
    }

    normalized[[column]] <<- vapply(
      seq_len(nrow(normalized)),
      function(index) {
        values <- split_values(normalized[[column]][[index]])
        if (length(values) == 0) {
          values <- default
        }
        collapse_values(values, numeric_values)
      },
      character(1)
    )

    invisible(NULL)
  }

  normalize_column("models_to_run", list_models())
  normalize_column("external_regressors", character())
  normalize_column(
    "rolling_window_periods",
    get_rolling_window_periods(NULL, date_type),
    numeric_values = TRUE
  )
  normalize_column(
    "seasonal_period",
    get_seasonal_periods(date_type),
    numeric_values = TRUE
  )

  if ("recipes_to_run" %in% names(normalized)) {
    normalized$recipes_to_run <- vapply(
      row_recipes,
      collapse_values,
      character(1)
    )
  }

  if ("lag_periods" %in% names(normalized)) {
    normalized$lag_periods <- vapply(
      seq_len(nrow(normalized)),
      function(index) {
        raw_lags <- split_values(normalized$lag_periods[[index]])
        if (length(raw_lags) == 0) {
          raw_lags <- NULL
        } else {
          raw_lags <- suppressWarnings(as.numeric(raw_lags))
          if (any(!is.finite(raw_lags))) {
            stop("Agent lag periods must contain finite values.", call. = FALSE)
          }
        }

        multistep <- FALSE
        if ("multistep_horizon" %in% names(normalized)) {
          multistep_value <- normalized$multistep_horizon[[index]]
          if (!is.na(multistep_value)) {
            multistep <- isTRUE(as.logical(multistep_value))
          }
        }

        recipe_lags <- vapply(
          row_recipes[[index]],
          function(recipe) {
            feature_lags <- if (recipe == "R1") {
              get_lag_periods(
                raw_lags,
                date_type,
                forecast_horizon,
                multistep,
                TRUE
              )
            } else {
              get_lag_periods(raw_lags, date_type, forecast_horizon)
            }

            signature <- paste0(
              recipe,
              ":feature=",
              collapse_values(feature_lags, numeric_values = TRUE)
            )

            if (recipe == "R1" && multistep) {
              model_lags <- get_lag_periods(
                raw_lags,
                date_type,
                forecast_horizon,
                TRUE
              )
              signature <- paste0(
                signature,
                ";model=",
                collapse_values(model_lags, numeric_values = TRUE)
              )
            }

            signature
          },
          character(1)
        )

        paste(recipe_lags, collapse = "|")
      },
      character(1)
    )
  }

  normalized
}

#' Apply column types from a template dataframe to a target dataframe
#'
#' @param target_df The target dataframe to apply types to.
#' @param template_df The template dataframe containing the desired column types.
#' @param drop_extra Logical indicating whether to drop columns in the target that are not in the template.
#' @param reorder Logical indicating whether to reorder columns in the target to match the template.
#'
#' @return The target dataframe with applied column types.
#' @noRd
apply_column_types <- function(target_df,
                               template_df,
                               drop_extra = FALSE,
                               reorder = FALSE) {
  # get common columns between template and target
  common_cols <- intersect(names(template_df), names(target_df))

  # function to cast a vector to the type of a template column
  cast_col <- function(vec, template) {
    if (inherits(template, "factor")) {
      factor(vec, levels = levels(template))
    } else if (inherits(template, "Date")) {
      as.Date(vec)
    } else if (inherits(template, "POSIXct")) {
      as.POSIXct(vec, tz = attr(template, "tzone") %||% "UTC")
    } else if (inherits(template, "POSIXlt")) {
      as.POSIXlt(vec, tz = attr(template, "tzone") %||% "UTC")
    } else if (is.integer(template)) {
      as.integer(vec)
    } else if (is.numeric(template)) {
      as.numeric(vec)
    } else if (is.logical(template)) {
      as.logical(vec)
    } else if (is.character(template)) {
      as.character(vec)
    } else {
      vec # leave as is for other classes (lists, df columns, etc.)
    }
  }

  # coerce all common columns
  for (col in common_cols) {
    target_df[[col]] <- cast_col(target_df[[col]], template_df[[col]])
  }

  # drop or reorder columns if requested
  if (drop_extra) {
    target_df <- target_df[common_cols]
  } else if (reorder) {
    target_df <- dplyr::relocate(target_df, tidyselect::all_of(common_cols),
      .after = dplyr::last_col()
    ) %>%
      dplyr::relocate(tidyselect::all_of(common_cols), .before = 1)
  }

  return(target_df)
}


#' Check if a parameter set exists in a previous run results dataframe
#'
#' @param x A list or atomic vector containing the parameter set to check.
#' @param df A dataframe containing previous run results with named columns.
#'
#' @return TRUE if the parameter set exists in the dataframe, FALSE otherwise.
#' @noRd
does_param_set_exist <- function(x, df) {
  # ensure x is a named list or atomic vector
  stopifnot(
    is.list(x) || is.atomic(x),
    !is.null(names(x)),
    all(names(x) %in% names(df))
  )

  # transform x into a 1-row tibble with matching column types
  probe <- x %>%
    tibble::as_tibble() %>% # 1-row tibble with same names as df
    apply_column_types(template_df = df) # ensure types match

  # check if the probe matches any row in df
  final_df <- df %>%
    dplyr::select(tidyselect::all_of(names(probe))) # select only matching cols

  dplyr::semi_join(final_df, probe, by = names(probe)) %>% # keep rows that match
    nrow() > 0 # TRUE if >=1 match
}

#' Extract JSON object from raw text
#'
#' @param raw_text A character string containing the raw text with JSON object.
#'
#' @return A parsed JSON object as a list.
#' @noRd
extract_json_object <- function(raw_text) {
  # format check
  if (!is.character(raw_text) || length(raw_text) != 1) {
    stop("`raw_text` must be a single character string.", call. = FALSE)
  }

  # locate the first {...} block
  json_block <- raw_text %>%
    stringr::str_remove_all("(?s)```json\\s*|```") %>% # strip code-fences
    stringr::str_extract("(?s)\\{.*?\\}") # non-greedy first brace

  if (is.na(json_block) || json_block == "") {
    stop("No JSON object found in `raw_text`.", call. = FALSE)
  }

  # parse JSON with error trap
  result <- tryCatch(
    jsonlite::fromJSON(json_block),
    error = function(e) {
      stop(
        sprintf("Failed to parse JSON object: %s", conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  return(result)
}

#' Convert NULL string to NULL value
#'
#' @param x A character string that may be "NULL" or a vector.
#'
#' @return NULL if "NULL", otherwise returns the input vector.
#' @noRd
null_converter <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  if (length(x) > 1) {
    return(x)
  }
  if (is.na(x) || identical(x, "NULL")) {
    return(NULL)
  }
  x
}

#' Create the system prompt for the forecasting agent
#'
#' @param agent_info A list containing agent information including project info and run ID.
#' @param combo A character string representing the combo to use for the run. If NULL, all combos are used.
#' @param weighted_mape_goal A numeric value representing the target weighted MAPE goal for the agent.
#'
#' @return A character string containing the system prompt for the agent.
#'   Custom enrollment uses a separate constrained prompt with source-free
#'   candidate metadata; the legacy model/default prompt is unchanged.
#' @noRd
iterate_forecast_system_prompt <- function(agent_info,
                                           combo = NULL,
                                           weighted_mape_goal) {
  if (!is.null(agent_info$custom_agent_contract)) return(agent_custom_prompt(agent_info, combo, weighted_mape_goal))
  # get metadata
  project_info <- agent_info$project_info
  combo_str <- paste(project_info$combo_variables, collapse = "---")
  xregs_str <- paste(agent_info$external_regressors, collapse = "---")
  # get NULL defaults
  lag_default <- get_lag_periods(
    lag_periods = NULL,
    date_type = project_info$date_type,
    forecast_horizon = agent_info$forecast_horizon,
    multistep_horizon = TRUE,
    feature_engineering = TRUE
  ) %>%
    paste(collapse = "---")

  rolling_default <- get_rolling_window_periods(
    rolling_window_periods = NULL,
    date_type = project_info$date_type
  ) %>%
    paste(collapse = "---")

  recipe_default <- get_recipes_to_run(
    recipes_to_run = NULL,
    date_type = project_info$date_type
  ) %>%
    paste(collapse = "---")

  seasonal_period_default <- get_seasonal_periods(date_type = project_info$date_type) %>%
    paste(collapse = "---")

  global_forecast_approaches <-
    agent_info$global_forecast_approaches %||% "bottoms_up"
  global_forecast_approaches_str <-
    paste(global_forecast_approaches, collapse = "|")

  # load EDA results
  eda_results <- load_eda_results(agent_info = agent_info, combo = combo)

  # Check foundation model availability
  fm_suffix <- get_foundation_model_suffix()

  # Model lists with conditional foundation models
  models_rule_10a <- paste0("arima---meanf---naive---snaive---stlm-arima---tbats---xgboost", fm_suffix)
  models_rule_10b <- paste0("arima---ets---meanf---naive---nnetar---prophet---snaive---stlm-arima---tbats---theta---cubist---glmnet---xgboost", fm_suffix)
  models_rule_10c <- paste0("arima---croston---ets---meanf---naive---nnetar---prophet---snaive---stlm-arima---stlm-ets---tbats---theta---cubist---mars---glmnet---svm-poly---svm-rbf---xgboost", fm_suffix)

  # create final prompt
  if (is.null(combo)) {
    # global model prompt
    final_prompt <- glue::glue(
      '
      -----CONTEXT-----
      You are an autonomous time-series forecasting agent. Your goal is to choose
      Finn-API parameters that **lower weighted MAPE** versus all prior runs.

      -----INITIAL METADATA-----
      - combos : <<combo>>
      - target : <<target>>
      - date type : <<dtype>>
      - hist end date : <<hist_end>>
      - forecast horizon : <<horizon>>
      - potential external regressors : <<xregs>>
      - weighted MAPE goal : <<weighted_mape_goal>>
      - Allowed forecast_approach values: <<global_forecast_approaches>>

      -----Exploratory Data Analysis-----
      <<eda>>

      -----RULES (MUST / MUST NOT)-----
      1.  YOU MUST output exactly one JSON object matching the schema below.
      2.  YOU MUST include a "reasoning" field with <= 250 words.
      3.  RUN CHANGE RULES
          3-A.  IF changes made in the previous run reduced the weighted mape compared to the best run, keep them, otherwise revert back to previous best run.
          3-B.  AFTER the first run (run_count > 0), YOU MUST change at most ONE parameter per new run.
          3-C.  Reverting back to a previous best run AND changing ONE parameter from that best run counts as ONE parameter change.
          3-D.  You MUST NOT recommend the same set of parameters as a run in the current agent version. If you do you MUST ABORT.
      4.  IF data is not stationary then set stationary="TRUE".
      5.  IF EDA shows strong autocorrelation on periods less than the forecast horizon AND forecast horizon > 1 then set multistep_horizon="TRUE".
      6.  HIERARCHICAL RULES
          6-A.  IF run_count == 0 then set forecast_approach="bottoms_up"
          6-B.  IF run_count > 0 AND *Step A is complete*, choose only from: <<global_forecast_approaches>>.
          6-C.  IF only "bottoms_up" is allowed, ALWAYS set forecast_approach="bottoms_up". This includes one retained series, the Agent single-column policy, and already prepared hierarchy-level ID input.
          6-D.  IF an exact hierarchical value is allowed, you may test it after bottoms_up. Finn reconciles that candidate to bottom-level series before weighted MAPE comparison.
          6-E.  You MUST NOT propose a forecast_approach that is not listed in the allowed values.
          6-F.  A "none" hierarchy result does not remove a saved outer hierarchy. Prepared hierarchy-level ID input is reconciled by the outer workflow, not by an inner hierarchical proposal.
      7.  MISSING VALUES RULES
          7-A.  IF missing values are present AND run_count == 0 then set clean_missing_values="FALSE"
          7-B.  IF missing values are present AND run_count > 0 AND *Step B is complete* then set clean_missing_values="TRUE"
          7-C.  ALWAYS use "FALSE" if no missing values are detected
      8.  OUTLIER RULES
          8-A.  IF outliers are present AND run_count == 0 then set clean_outliers="FALSE"
          8-B.  IF outliers are present AND run_count > 0 AND *Step C is complete* then set clean_outliers="TRUE"
          8-C.  ALWAYS use "FALSE" if no outliers are detected
      9.  EXTERNAL REGRESSOR RULES
          9-A.  When choosing external_regressors, YOU MUST only select from the external regressors listed in the metadata. Separate multiple regressors with "---".
          9-B.  IF adding external regressors AND run_count == 0 then set external_regressors="NULL"
          9-C.  IF adding external regressors AND run_count > 0 AND *Step D is complete*, add ONLY ONE new external regressor variable per run.
          9-D.  ALWAYS use "NULL" if no external regressors are needed.
          9-E.  ALWAYS start with the most promising external regressors based on distance correlation results.
          9-F.  ALWAYS set feature_selection="TRUE" if any external regressors are used.
          9-G.  IF an external regressors is a previous run helped reduce forecast error, then keep it. Then try adding one new external regressor in addition to the previous external regressor.
          9-H.  ALWAYS try all promising external regressors (either individually or combination of multiple external regressors) highlighted from EDA before moving along in decision tree.
          9-I.  IF using multiple external_regressors, you MUST NOT change the ordering of them compared to previous runs.
      10. FEATURE LAG RULES
          10-A. IF run_count == 0 then set lag_periods = "NULL"
          10-B. IF run_count > 0 AND *Step E is complete* -> use ACF and PCF results from EDA to select lag_periods.
          10-C. IF selecting lag periods to use, ALWAYS combine them using "---" separator.
          10-D. IF selecting lag periods less than the forecast horizon, ALWAYS set multistep_horizon="TRUE".
          10-E. You may try at most 3 distinct non-default lag_periods configurations within the current agent version.
          10-F. IF lag_changes_allowed == FALSE, you MUST reuse a previously tested configuration, select "NULL", or ABORT.
          10-G. A value of "NULL" means that lags are defaulted to <<lag_default>>. Note that these default lags assume multistep_horizon="TRUE". If not the lags will be greater than or equal to forecast horizon.
          10-H. If using multiple lag_periods, you MUST NOT change the ordering of them compared to previous runs.
      11. ROLLING WINDOW LAGS
          11-A. IF run_count == 0 then set rolling_window_periods = "NULL"
          11-B. IF run_count > 0 AND *Step F is complete* then set rolling_window_periods = "NULL" or a list of periods separated by "---".
          11-C. You may try at most 3 distinct non-default rolling_window_periods configurations within the current agent version.
          11-D. IF rolling_changes_allowed == FALSE, you MUST reuse a previously tested configuration, select "NULL", or ABORT.
          11-E. A value of "NULL" means that rolling window lags are defaulted to <<rolling_default>>.
          11-F. If using multiple rolling_window_periods, you MUST NOT change the ordering of them compared to previous runs.
      12. PREVIOUS VERSION REPLAY RULES
          12-A. IF run_count == 0 AND one or more *earlier agent versions* exist before current agent version of <<agent_version>>,
          YOU MUST first try the **full input set** from the best run of the most-recent previous agent version.
          12-B. IF that set has already been tried in the current version AND run_count < 4, choose the next best run, and so on.
          12-C. AFTER following 12-A AND 12-B, you MUST follow the decision tree below to propose a new set of parameters.
          12-D. Using the full input set from a previous run counts as ONE parameter change.
      13. An example value of "NULL|var1---var2" means YOU MUST either
          include "NULL" or a list of variables separated by "---". NOT both.
      14. DECISION TREE RULES
          14-A. YOU MUST follow the order of operations (decision tree) below when deciding on parameters.
          14-B. YOU MUST stop at the first step where a rule applies that has not been tried.
          14-C. YOU MUST exhaust all ideas on a step before moving to the next step in the decision tree.
          14-D. YOU MUST learn from previous agent_version runs and EDA results to avoid repeating params that have not worked in the past.
      15. ABORT RULES
          15-A. AFTER completing the decision tree of options, ABORT IF you cannot propose a set that you believe will beat the weighted mape goal based on EDA results and weighted_mape from previous runs.
          15-B. IF you ABORT, output the *abort-schema* instead of the normal parameter schema.

      -----ORDER OF OPERATIONS DECISION TREE (Rule 14)-----
      Step A (Previous Version Replay - Rule 12)
      -> Step B (Hierarchy - Rule 6)
      -> Step C (Missing Values - Rule 7)
      -> Step D (Outliers - Rule 8)
      -> Step E (External Regressors - Rule 9)
      -> Step F (Feature Lags - Rule 10)
      -> Step G (Rolling Window Lags - Rule 11)

      -----OUTPUT FORMAT-----
      <scratchpad>
      ...your chain-of-thought, work through the decision tree, cite Rules #,
      compare input recommendation to previous runs to prevent duplicates or violate parameter change constraints...
      </scratchpad>
      ```json
      // ---- normal schema (when you propose a new run) ----
      {
        "external_regressors"   : "NULL|var1---var2",
        "clean_missing_values"  : "TRUE|FALSE",
        "clean_outliers"        : "TRUE|FALSE",
        "forecast_approach"     : "<<global_forecast_approaches>>",
        "stationary"            : "TRUE|FALSE",
        "feature_selection"     : "TRUE|FALSE",
        "multistep_horizon"     : "TRUE|FALSE",
        "lag_periods"           : "NULL|1---2---3",
        "rolling_window_periods" : "NULL|1---2---3",
        "reasoning"             : "...<=250 words..."
      }
      // ---- abort schema (Rule 15) ----
      {
        "abort"     : "TRUE",
        "reasoning" : "...<=250 words explaining why no further improvement is likely..."
      }
      ```
      -----END OUTPUT-----',
      .open = "<<", .close = ">>",
      combo = combo_str,
      target = project_info$target_variable,
      dtype = project_info$date_type,
      hist_end = agent_info$hist_end_date,
      horizon = agent_info$forecast_horizon,
      xregs = xregs_str,
      eda = eda_results,
      weighted_mape_goal = weighted_mape_goal,
      global_forecast_approaches = global_forecast_approaches_str,
      lag_default = lag_default,
      rolling_default = rolling_default,
      agent_version = agent_info$agent_version
    )
  } else {
    # local model prompt
    final_prompt <- glue::glue(
      '
      -----CONTEXT-----
      You are an autonomous time-series forecasting agent. Your goal is to choose
      Finn-API parameters that **lower weighted MAPE** versus all prior runs.

      -----METADATA-----
      - combos : <<combo>>
      - target : <<target>>
      - date type : <<dtype>>
      - hist end date : <<hist_end>>
      - forecast horizon : <<horizon>>
      - potential external regressors : <<xregs>>
      - weighted MAPE goal : <<weighted_mape_goal>>

      -----Exploratory Data Analysis-----
      <<eda>>

      -----RULES (MUST / MUST NOT)-----
      1.  YOU MUST output exactly one JSON object matching the schema below.
      2.  YOU MUST include a "reasoning" field with <= 250 words.
      3.  RUN CHANGE RULES
          3-A.  IF changes made in the previous run reduced the weighted mape compared to the best run, keep them, otherwise revert back to previous best run.
          3-B.  AFTER the first run (run_count > 0), YOU MUST change at most ONE parameter per new run.
          3-C.  Reverting back to a previous best run AND changing ONE parameter from that best run counts as ONE parameter change.
          3-D.  You MUST NOT recommend the same set of parameters as a run in the current agent version. If you do you MUST ABORT.
      4.  IF data is not stationary -> stationary="TRUE".
      5.  IF EDA shows strong autocorrelation on periods less than the forecast horizon AND forecast horizon > 1 -> multistep_horizon="TRUE".
      6.  MISSING VALUES RULES
          6-A.  IF missing values are present AND run_count == 0 AND *Step A is complete* -> clean_missing_values="FALSE"
          6-B.  IF missing values are present AND run_count > 0 -> clean_missing_values="TRUE"
          6-C.  ALWAYS use "FALSE" if no missing values are detected
      7.  OUTLIER RULES
          7-A.  IF outliers are present AND run_count == 0 -> clean_outliers="FALSE"
          7-B.  ALWAYS use "FALSE" if no outliers are detected
      8.  SEASONAL PERIOD RULES
          8-A.  IF run_count == 0 -> seasonal_period = "NULL"
          8-B.  IF run_count > 0 AND *Step C is complete* -> seasonal_period = "NULL" or a list of periods separated by "---". Use EDA results to select seasonal periods.
          8-C.  You may try at most 3 distinct non-default seasonal_period configurations within the current agent version.
          8-D.  Reusing a previously tested configuration or selecting "NULL" does not consume another change.
          8-E.  IF seasonal_changes_allowed == FALSE, you MUST reuse a previously tested configuration, select "NULL", or ABORT.
          8-F.  A value of "NULL" means that seasonal periods are defaulted to <<seasonal_period_default>>.
          8-G.  There can only be at most 3 seasonal periods, separated by "---". If you select more than 3, you MUST ABORT.
          8-H.  Seasonal period inputs ONLY apply to these models: "stlm-arima", "stlm-ets", "tbats". IF you are not using these models, you MUST NOT select any seasonal periods.
          8-I.  Every seasonal period MUST be a unique, finite numeric value greater than 1. You MUST NOT use 1 in any position.
      9.  FULL MODEL SWEEP RULES
          9-A.  IF run_count == 0 -> models_to_run = "<<models_rule_10a>>"
          9-B.  IF run_count > 0 AND *Step D is complete* -> models_to_run = "<<models_rule_10b>>"
          9-C.  AFTER applying rule 9-B, if wmape goal was not met -> models_to_run = "<<models_rule_10c>>"
      10. EXTERNAL REGRESSOR RULES
          10-A. When choosing external_regressors, YOU MUST only select from the external regressors listed in the metadata. Separate multiple regressors with "---".
          10-B. IF adding external regressors AND run_count == 0 -> external_regressors="NULL"
          10-C. IF adding external regressors AND run_count > 0 AND *Step E is complete*, add ONLY ONE new external regressor variable per run.
          10-D. ALWAYS use "NULL" if no external regressors are needed.
          10-E. ALWAYS start with the most promising external regressors based on distance correlation results.
          10-F. ALWAYS set feature_selection="TRUE" if any external regressors are used. Changing feature_selection AND external_regressors counts as ONE parameter change.
          10-G. IF an external regressors is a previous run helped reduce forecast error, then keep it. Then try adding one new external regressor in addition to the previous external regressor.
          10-H. ALWAYS try all promising external regressors (either individually or combination of multiple external regressors) highlighted from EDA before moving along in decision tree.
          10-I. IF using multiple external_regressors, you MUST NOT change the ordering of them compared to previous runs.
      11. FEATURE LAG RULES
          11-A. IF run_count == 0 -> lag_periods = "NULL"
          11-B. IF run_count > 0 AND *Step F is complete* -> use ACF and PCF results from EDA to select lag_periods.
          11-C. IF selecting lag periods to use, ALWAYS combine them using "---" separator.
          11-D. IF selecting lag periods less than the forecast horizon, ALWAYS set multistep_horizon="TRUE".
          11-E. You may try at most 3 distinct non-default lag_periods configurations within the current agent version.
          11-F. IF lag_changes_allowed == FALSE, you MUST reuse a previously tested configuration, select "NULL", or ABORT.
          11-G. A value of "NULL" means that lags are defaulted to <<lag_default>>. Note that these default lags assume multistep_horizon="TRUE". If not the lags will be greater than or equal to forecast horizon.
          11-H. If using multiple lag_periods, you MUST NOT change the ordering of them compared to previous runs.
      12. ROLLING WINDOW LAGS
          12-A. IF run_count == 0 -> rolling_window_periods = "NULL"
          12-B. IF run_count > 0 AND *Step G is complete* -> rolling_window_periods = "NULL" or a list of periods separated by "---".
          12-C. You may try at most 3 distinct non-default rolling_window_periods configurations within the current agent version.
          12-D. IF rolling_changes_allowed == FALSE, you MUST reuse a previously tested configuration, select "NULL", or ABORT.
          12-E. A value of "NULL" means that rolling window lags are defaulted to <<rolling_default>>.
          12-F. If using multiple rolling_window_periods, you MUST NOT change the ordering of them compared to previous runs.
      13. RECIPE RULES
          13-A. IF run_count == 0 -> recipes_to_run = "R1"
          13-B. IF run_count > 0 AND *Step H is complete* -> recipes_to_run = "NULL" or "R1"
          13-C. A value of "NULL" means that recipes are defaulted to <<recipe_default>>.
      14. PREVIOUS VERSION REPLAY RULES
          14-A. IF run_count == 0 AND one or more *earlier agent versions* exist before current agent version of <<agent_version>>,
          YOU MUST first try the **full input set** from the best run of the most-recent previous agent version.
          14-B. IF that set has already been tried in the current version AND run_count < 4, choose the next best run, and so on.
          14-C. AFTER following 14-A AND 14-B, you MUST follow the decision tree below to propose a new set of parameters.
          14-D. Using the full input set from a previous run counts as ONE parameter change.
      15. An example value of "NULL|var1---var2" means YOU MUST either
          include "NULL" or a list of variables separated by "---". NOT both.
      16. DECISION TREE RULES
          16-A. YOU MUST follow the order of operations (decision tree) below when deciding on parameters.
          16-B. YOU MUST stop at the first step where a rule applies that has not been tried.
          16-C. YOU MUST exhaust all ideas on a step before moving to the next step in the decision tree.
          16-D. YOU MUST learn from previous agent_version runs and EDA results to avoid repeating params that have not worked in the past.
      17. ABORT RULES
          17-A. AFTER completing the decision tree of options, ABORT IF you cannot propose a set that you believe will beat the weighted mape goal based on EDA results and weighted_mape from previous runs.
          17-B. IF you ABORT, output the *abort-schema* instead of the normal parameter schema.

      -----ORDER OF OPERATIONS DECISION TREE (Rule 16)-----
      Step A (Previous Version Replay - Rule 14)
      -> Step B (Missing Values - Rule 6)
      -> Step C (Outliers - Rule 7)
      -> Step D (Seasonal Period - Rule 8)
      -> Step E (Full Model Sweep - Rule 9)
      -> Step F (External Regressors - Rule 10)
      -> Step G (Feature Lags - Rule 11)
      -> Step H (Rolling Window Lags - Rule 12)
      -> Step I (Recipes - Rule 13)

      -----OUTPUT FORMAT-----
      <scratchpad>
      ...your chain-of-thought, work through the decision tree, cite Rules #,
      compare input recommendation to previous runs to prevent duplicates or violate parameter change constraints...
      </scratchpad>
      ```json
      // ---- normal schema (when you propose a new run) ----
      {
        "external_regressors"   : "NULL|var1---var2",
        "clean_missing_values"  : "TRUE|FALSE",
        "clean_outliers"        : "TRUE|FALSE",
        "seasonal_period"       : "NULL|2---6---12",
        "models_to_run"         : "arima---ets---tbats---snaive---stlm-arima---xgboost",
        "stationary"            : "TRUE|FALSE",
        "feature_selection"     : "TRUE|FALSE",
        "multistep_horizon"     : "TRUE|FALSE",
        "lag_periods"           : "NULL|1---2---3",
        "rolling_window_periods": "NULL|1---2---3",
        "recipes_to_run"        : "R1|NULL",
        "reasoning"             : "...<=250 words..."
      }
      // ---- abort schema (Rule 17) ----
      {
        "abort"     : "TRUE",
        "reasoning" : "...<=250 words explaining why no further improvement is likely..."
      }
      ```
      -----END OUTPUT-----',
      .open = "<<", .close = ">>",
      combo = combo_str,
      target = project_info$target_variable,
      dtype = project_info$date_type,
      hist_end = agent_info$hist_end_date,
      horizon = agent_info$forecast_horizon,
      xregs = xregs_str,
      eda = eda_results,
      weighted_mape_goal = weighted_mape_goal,
      lag_default = lag_default,
      rolling_default = rolling_default,
      recipe_default = recipe_default,
      seasonal_period_default = seasonal_period_default,
      agent_version = agent_info$agent_version,
      models_rule_10a = models_rule_10a,
      models_rule_10b = models_rule_10b,
      models_rule_10c = models_rule_10c
    )
  }

  return(final_prompt)
}
