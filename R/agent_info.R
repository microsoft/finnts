#' Set up Finn Agent Run Information
#'
#' This function sets up the necessary information for a Finn Agent run,
#'  including input data, forecast horizon, and other parameters.
#'  It checks for existing runs and allows for overwriting if specified.
#'
#' @param project_info A Finn project from `set_project_info()`
#' @param llm An ellmer Chat or [chat_copilot()] object used as the template
#'   for isolated agent sessions.
#' @param input_data A data frame or tibble containing the input data. Leading
#'   and trailing whitespace in character combo-variable values is removed
#'   before Finn creates internal series identifiers and writes input artifacts.
#'   Resuming a version uses previously uploaded inputs, not replacement values
#'   from this data frame. Create a new version to use revised data.
#' @param forecast_horizon The number of periods to forecast
#' @param external_regressors Optional character vector of candidate external
#'   regressors for built-in models. Custom models always use their declared
#'   drivers from eligible original input columns, independently of this list
#'   or the Agent's per-iteration choices.
#' @param hist_end_date Optional Date object indicating the end of the historical data
#' @param hist_start_date Optional Date object indicating the start of the historical data
#' @param back_test_scenarios Optional character vector of back test scenarios
#' @param back_test_spacing Optional numeric value for back test spacing
#' @param combo_cleanup_date Optional Date object for combo cleanup
#' @param allow_hierarchical_forecast Logical controlling the Agent optimization
#'   scope. `TRUE` expands input data to all detected hierarchy levels before
#'   optimization, runs inner iterations with `forecast_approach = "bottoms_up"`,
#'   and reconciles the final forecast. `FALSE` keeps the original bottom-level
#'   series; global iterations may still compare the exact hierarchy detected by
#'   EDA after reconciling that candidate back to the bottom level.
#' @param run_global_models If TRUE, run multivariate models on the entire data
#'   set (across all time series) as a global model. Default of NULL runs global models for all date types
#'   except week and day.
#' @param negative_forecast If TRUE, allow forecasts to dip below zero.
#' @param run_local_models If TRUE, run models by individual time series as
#'   local models. Default is TRUE.
#' @param overwrite Logical indicating whether to overwrite existing agent run info
#' @param custom_models Named approved custom-model envelopes from [create_custom_model()].
#'   All supplied models are enrolled automatically alongside built-in candidates.
#'   The returned object retains names only; approved objects are saved once in
#'   an Agent-version bundle in `prep_models`.
#' @details Custom Agent runs add the raw `C1` recipe and require CSV data, RDS objects,
#'   supported local/mounted, Blob or M365 storage and no hierarchy. `negative_forecast = FALSE` floors
#'   finished custom forecasts at zero, as for built-in models. The exact
#'   approved pool is pinned before the parent run log. Existing-run setup requires
#'   equivalent explicit enrollment; changed or omitted approval requires a new
#'   run. Source is trusted R, not sandboxed. For compatible new-data updates,
#'   create a new parent with `overwrite = TRUE`, the same explicit enrollment,
#'   horizon, regressors and modes, then call [update_forecast()] with
#'   explicit `allow_iterate_forecast = FALSE`. Changed enrollment needs iteration.
#'   New series are routed to defaults within the existing new-series limit.
#'   The setup API has no
#'   `models_to_run` argument: Finn includes every compatible enrolled custom
#'   model from the first iteration onward, outside LLM control. Built-in
#'   candidate choices and supported preprocessing remain independent of raw C1.
#'   Custom driver values must be supplied through the forecast horizon; Finn
#'   does not forecast missing future custom drivers. The same driver declarations
#'   apply during saved-winner updates and default recovery. Changed source data
#'   requires a new Agent version (`overwrite = TRUE`), not an in-place restart.
#'   Global-only custom
#'   runs need at least two series. Each selected custom model must support an
#'   enabled mode and the project's cadence, horizon and required predictors.
#'   The return keeps only custom-model names, not untrained models or approval
#'   payloads. The Agent-version CSV always contains one `custom_models` names
#'   field (`NA` without enrollment); the complete immutable bundle lives in
#'   `prep_models`, under the existing project/run/version identity.
#'   Saved approval is trusted-caller evidence, not
#'   authenticated identity; forecasting never calls the authoring LLM again.
#'
#'   With `overwrite = FALSE`, an existing version retains its saved
#'   forecast approach even if hierarchy detection changes after a package upgrade.
#'   Logged user settings must still match. New versions use current detection;
#'   one retained series or one combo column uses bottoms-up. Updating a new
#'   version across an outer forecast-approach change requires fresh iteration.
#'
#' @return A list containing the agent run information
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
#' }
#' @export
set_agent_info <- function(project_info,
                           llm,
                           input_data,
                           forecast_horizon,
                           external_regressors = NULL,
                           hist_end_date = NULL,
                           hist_start_date = NULL,
                           back_test_scenarios = NULL,
                           back_test_spacing = NULL,
                           combo_cleanup_date = NULL,
                           allow_hierarchical_forecast = FALSE,
                           negative_forecast = FALSE,
                           run_global_models = NULL,
                           run_local_models = TRUE,
                           overwrite = FALSE,
                           custom_models = NULL) {
  set_agent_info_impl(
    project_info, llm, input_data, forecast_horizon, external_regressors,
    hist_end_date, hist_start_date, back_test_scenarios, back_test_spacing,
    combo_cleanup_date, allow_hierarchical_forecast, negative_forecast,
    run_global_models, run_local_models, overwrite,
    custom_models = custom_models
  )
}

# Shared setup preserves the public defaults and saved-setting validation.
# custom_models carries the explicit approved enrollment unchanged. Custom
# horizons normalize plain integer storage to the numeric run-log representation.
# An internal resume_run_id selects exactly one saved run for request-ID retries;
# missing or duplicate matches fail before writes. NULL retains latest-run setup.
# Returns the Agent contract; new versions retain the existing artifact writes.
set_agent_info_impl <- function(project_info,
                                llm,
                                input_data,
                                forecast_horizon,
                                external_regressors = NULL,
                                hist_end_date = NULL,
                                hist_start_date = NULL,
                                back_test_scenarios = NULL,
                                back_test_spacing = NULL,
                                combo_cleanup_date = NULL,
                                allow_hierarchical_forecast = FALSE,
                                negative_forecast = FALSE,
                                run_global_models = NULL,
                                run_local_models = TRUE,
                                overwrite = FALSE,
                                custom_models = NULL,
                                resume_run_id = NULL) {
  # get metadata
  combo_variables <- project_info$combo_variables
  target_variable <- project_info$target_variable
  date_type <- project_info$date_type
  fiscal_year_start <- project_info$fiscal_year_start

  # check inputs
  check_input_type("project_info", project_info, "list")
  check_agent_llm(llm)
  check_input_type("input_data", input_data, c("tbl", "tbl_df", "data.frame"))
  if (length(custom_models) && is.integer(forecast_horizon) && is.null(attributes(forecast_horizon))) {
    forecast_horizon <- as.numeric(forecast_horizon)
  }
  check_input_type("forecast_horizon", forecast_horizon, "numeric")
  check_input_type("external_regressors", external_regressors, c("character", "NULL"))
  check_input_type("hist_end_date", hist_end_date, c("Date", "NULL"))
  check_input_type("hist_start_date", hist_start_date, c("Date", "NULL"))
  check_input_type("back_test_scenarios", back_test_scenarios, c("numeric", "NULL"))
  check_input_type("back_test_spacing", back_test_spacing, c("numeric", "NULL"))
  check_input_type("combo_cleanup_date", combo_cleanup_date, c("Date", "NULL"))
  check_input_type("allow_hierarchical_forecast", allow_hierarchical_forecast, "logical")
  check_input_type("negative_forecast", negative_forecast, "logical")
  check_input_type("run_global_models", run_global_models, c("NULL", "logical"))
  check_input_type("run_local_models", run_local_models, "logical")
  check_input_type("overwrite", overwrite, "logical")

  input_data <- normalize_combo_values(
    input_data = input_data,
    combo_variables = combo_variables
  )

  check_input_data(
    input_data,
    combo_variables,
    target_variable,
    external_regressors,
    date_type,
    fiscal_year_start,
    parallel_processing = NULL
  )

  # set default for run_global_models based on date_type
  if (is.null(run_global_models) & date_type %in% c("day", "week")) {
    run_global_models <- FALSE
  } else if (is.null(run_global_models)) {
    run_global_models <- TRUE
  }

  if (run_global_models == FALSE & run_local_models == FALSE) {
    stop("At least one of 'run_global_models' or 'run_local_models' must be TRUE.", call. = FALSE)
  }

  c1_columns <- if (length(custom_models)) custom_input_columns(input_data, target_variable) else character()
  c1_source_id <- if (length(custom_models)) custom_source_identity(input_data %>%
    tidyr::unite("Combo", tidyselect::all_of(combo_variables), sep = "--", remove = FALSE) %>%
    dplyr::rename("Target" = tidyselect::all_of(target_variable)) %>%
    dplyr::select(Combo, Date, Target, tidyselect::all_of(c1_columns))) else NULL
  custom_contract <- resolve_agent_custom_models(list(project_info = project_info,
    c1_predictors = c1_columns, c1_source_id = c1_source_id,
    forecast_horizon = forecast_horizon, external_regressors = external_regressors,
    run_global_models = run_global_models, run_local_models = run_local_models,
    negative_forecast = negative_forecast, allow_hierarchical_forecast = allow_hierarchical_forecast,
    forecast_approach = "bottoms_up"),
    if (length(custom_models)) unique(c(list_models(), names(custom_models))) else NULL,
    custom_models)
  if (!is.null(custom_contract) && !identical(project_info$data_output, "csv")) {
    stop("Custom Agent runs currently require CSV data artifacts.", call. = FALSE)
  }

  # input data formatting
  if (is.null(hist_end_date)) {
    hist_end_date <- input_data %>%
      dplyr::select(Date) %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::distinct() %>%
      dplyr::pull(Date) %>%
      max() %>%
      suppressWarnings()
  }

  if (is.null(hist_start_date)) {
    hist_start_date <- input_data %>%
      dplyr::select(Date) %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::distinct() %>%
      dplyr::filter(Date == min(Date)) %>%
      dplyr::pull(Date) %>%
      suppressWarnings()
  }

  final_input_data <- input_data %>%
    tidyr::unite("Combo",
      tidyselect::all_of(combo_variables),
      sep = "--",
      remove = F
    ) %>%
    dplyr::rename("Target" = tidyselect::all_of(target_variable)) %>%
    dplyr::select(c(
      "Combo",
      tidyselect::all_of(combo_variables),
      tidyselect::all_of(unique(c(external_regressors, c1_columns))),
      "Date", "Target"
    )) %>%
    combo_cleanup_fn(
      combo_cleanup_date,
      hist_end_date
    )

  if (!is.null(custom_contract) && !run_local_models &&
    length(unique(final_input_data$Combo)) < 2L) {
    stop("Global-only custom Agent runs require at least two series.", call. = FALSE)
  }

  raw_agent_runs_tbl <- load_agent_runs(project_info)
  agent_runs_tbl <- if (!is.null(resume_run_id)) {
    matched <- raw_agent_runs_tbl %>%
      dplyr::filter(.data$run_id == resume_run_id)
    if (overwrite || nrow(matched) != 1L) {
      stop("Cannot resume requested Agent run: expected exactly one saved run matching run_id. Restore the original run metadata.",
        call. = FALSE)
    }
    matched
  } else if (nrow(raw_agent_runs_tbl) > 0) {
    raw_agent_runs_tbl %>%
      dplyr::arrange(dplyr::desc(created)) %>%
      dplyr::slice(1)
  } else {
    tibble::tibble()
  }

  if ((nrow(agent_runs_tbl) == 0 || overwrite) && nrow(final_input_data) == 0) {
    stop("No retained time series remain for Agent setup. Check the cleanup date and historical target activity.",
      call. = FALSE)
  }

  # A resume keeps its saved interpretation; detector upgrades apply to new versions.
  if (nrow(agent_runs_tbl) > 0 && !overwrite) {
    forecast_approach <- agent_runs_tbl$forecast_approach
    if (length(forecast_approach) != 1L || is.na(forecast_approach) ||
      !forecast_approach %in% c("bottoms_up", "standard_hierarchy", "grouped_hierarchy")) {
      stop("Cannot resume Agent version: saved forecast_approach is missing or unsupported. Restore the original run metadata or create a new version with overwrite = TRUE.",
        call. = FALSE)
    }
  } else if (allow_hierarchical_forecast) {
    forecast_approach <- hierarchy_detect(
      agent_info = list(
        project_info = project_info,
        run_id = 1
      ),
      input_data = final_input_data,
      write_data = FALSE
    )

    if (forecast_approach != "bottoms_up") {
      cli::cli_alert_info(
        "Hierarchical data detected. Using '{forecast_approach}' forecast approach in a hierarchical forecast."
      )
    }
  } else {
    forecast_approach <- "bottoms_up"
  }

  if (nrow(agent_runs_tbl) > 0 & overwrite == FALSE) {
    previous_info <- list(project_info = project_info, run_id = as.character(agent_runs_tbl$run_id),
      agent_version = as.numeric(agent_runs_tbl$agent_version))
    previous_names <- agent_runs_tbl[["custom_models"]]
    marked <- length(previous_names) == 1L && !is.na(previous_names) && nzchar(previous_names)
    previous_contract <- read_agent_custom_record(previous_info, required = marked)
    if (!is.null(custom_contract) || !is.null(previous_contract) || marked) {
      if (is.null(custom_contract) || is.null(previous_contract) || !marked ||
        !identical(as.character(previous_names), paste(sort(names(previous_contract$envelopes)), collapse = "---")) ||
        !identical(agent_custom_marker(custom_contract), agent_custom_marker(previous_contract)) ||
        !identical(custom_contract$context$c1_source_id, previous_contract$context$c1_source_id)) {
        stop("Custom Agent enrollment changed or was omitted; start a new run with overwrite = TRUE.", call. = FALSE)
      }
    }
    # check if input values have changed
    current_log_df <- tibble::tibble(
      project_name = project_info$project_name,
      forecast_horizon = forecast_horizon,
      external_regressors = ifelse(is.null(external_regressors), NA_character_, paste(external_regressors, collapse = ", ")),
      hist_end_date = if (is.null(hist_end_date)) as.Date(NA) else hist_end_date,
      hist_start_date = if (is.null(hist_start_date)) as.Date(NA) else hist_start_date,
      back_test_scenarios = ifelse(is.null(back_test_scenarios), NA_real_, as.numeric(back_test_scenarios)),
      back_test_spacing = ifelse(is.null(back_test_spacing), NA_real_, as.numeric(back_test_spacing)),
      combo_cleanup_date = if (is.null(combo_cleanup_date)) {
        as.Date(NA)
      } else {
        combo_cleanup_date
      },
      allow_hierarchical_forecast = allow_hierarchical_forecast,
      negative_forecast = negative_forecast,
      forecast_approach = forecast_approach,
      run_global_models = run_global_models,
      run_local_models = run_local_models
    ) %>%
      data.frame()

    # build prev_log_df from saved log
    # keep forecast_approach as its own column and log/compare
    # allow_hierarchical_forecast explicitly
    prev_log_raw <- agent_runs_tbl %>%
      dplyr::select(tidyselect::any_of(
        colnames(current_log_df)
      ))

    # Hierarchical saved inputs prove expansion was enabled. Bottoms-up legacy
    # logs cannot distinguish a disabled flag from a single-column policy.
    if (!"allow_hierarchical_forecast" %in% colnames(prev_log_raw)) {
      prev_log_raw$allow_hierarchical_forecast <- if (forecast_approach != "bottoms_up") {
        TRUE
      } else {
        allow_hierarchical_forecast
      }
    }
    prev_log_df <- align_types(
      current_log_df,
      prev_log_raw
    ) %>%
      data.frame()

    # handle missing columns in previous log (backward compatibility)
    for (col in setdiff(colnames(current_log_df), colnames(prev_log_df))) {
      if (col == "run_global_models") {
        # default based on date_type
        if (date_type %in% c("day", "week")) {
          prev_log_df$run_global_models <- FALSE
        } else {
          prev_log_df$run_global_models <- TRUE
        }
      } else if (col == "run_local_models") {
        prev_log_df$run_local_models <- TRUE
      } else if (col == "negative_forecast") {
        prev_log_df$negative_forecast <- FALSE
      } else {
        prev_log_df[[col]] <- current_log_df[[col]]
      }
    }

    # ensure column order matches so hash comparison is reliable
    prev_log_df <- prev_log_df[, colnames(current_log_df), drop = FALSE]

    if (hash_data(normalize_log_df(current_log_df)) != hash_data(normalize_log_df(prev_log_df))) {
      nullable <- c(
        "external_regressors", "hist_end_date", "hist_start_date",
        "back_test_scenarios", "back_test_spacing", "combo_cleanup_date"
      )
      diff_details <- format_input_diff(prev_log_df, current_log_df, nullable)
      stop(
        "Inputs have recently changed in 'set_agent_info'.\n",
        "The following inputs differ from the previous run:\n",
        diff_details, "\n",
        "Please revert back to original inputs or start new agent run ",
        "with 'overwrite' argument set to TRUE.",
        call. = FALSE
      )
    }

    output_list <- list(
      agent_version = agent_runs_tbl$agent_version,
      run_id = agent_runs_tbl$run_id,
      project_info = project_info,
      llm = llm,
      forecast_horizon = prev_log_df$forecast_horizon,
      external_regressors = if (is.na(prev_log_df$external_regressors)) {
        NULL
      } else {
        strsplit(prev_log_df$external_regressors, ", ")[[1]]
      },
      hist_end_date = prev_log_df$hist_end_date,
      hist_start_date = prev_log_df$hist_start_date,
      back_test_scenarios = if (is.na(prev_log_df$back_test_scenarios)) {
        NULL
      } else {
        as.numeric(prev_log_df$back_test_scenarios)
      },
      back_test_spacing = if (is.na(prev_log_df$back_test_spacing)) {
        NULL
      } else {
        as.numeric(prev_log_df$back_test_spacing)
      },
      combo_cleanup_date = if (is.na(prev_log_df$combo_cleanup_date)) {
        NULL
      } else {
        as.Date(prev_log_df$combo_cleanup_date)
      },
      forecast_approach = prev_log_df$forecast_approach,
      negative_forecast = as.logical(prev_log_df$negative_forecast),
      run_global_models = prev_log_df$run_global_models,
      run_local_models = prev_log_df$run_local_models,
      overwrite = overwrite
    )

    cli::cli_bullets(c(
      "Using Existing Finn Agent Run with Previously Uploaded Input Data",
      "*" = paste0("Project Name: ", prev_log_df$project_name),
      "*" = paste0("Agent Version: ", agent_runs_tbl$agent_version),
      "*" = paste0("Agent Run ID: ", agent_runs_tbl$run_id),
      ""
    ))

    return(attach_agent_custom_contract(output_list, custom_contract))
  } else {
    # create unique id for agent run
    created_time <- get_timestamp()

    agent_run_id <- hash_data(created_time)

    # add to project info
    project_info$run_name <- agent_run_id

    # create agent version
    agent_version <- nrow(raw_agent_runs_tbl) + 1

    if (!is.null(custom_contract)) write_agent_custom_record(list(project_info = project_info,
      run_id = agent_run_id, agent_version = agent_version), custom_contract)

    # write input data to disc
    if (forecast_approach != "bottoms_up") {
      final_input_data <- final_input_data %>%
        prep_hierarchical_data(
          run_info = project_info,
          combo_variables = combo_variables,
          external_regressors = external_regressors,
          forecast_approach = forecast_approach,
          frequency_number = get_frequency_number(date_type)
        ) %>%
        dplyr::mutate(ID = Combo) %>%
        dplyr::relocate(ID, .before = Date)
    }

    for (combo_name in unique(final_input_data$Combo)) {
      write_data(
        x = final_input_data %>% dplyr::filter(Combo == combo_name),
        combo = combo_name,
        run_info = project_info,
        output_type = "data",
        folder = "input_data",
        suffix = NULL
      )
    }

    # create agent run metadata
    output_list <- list(
      agent_version = agent_version,
      run_id = agent_run_id,
      project_info = project_info,
      llm = llm,
      forecast_horizon = forecast_horizon,
      external_regressors = external_regressors,
      hist_end_date = hist_end_date,
      hist_start_date = hist_start_date,
      back_test_scenarios = back_test_scenarios,
      back_test_spacing = back_test_spacing,
      combo_cleanup_date = combo_cleanup_date,
      forecast_approach = forecast_approach,
      negative_forecast = negative_forecast,
      run_global_models = run_global_models,
      run_local_models = run_local_models,
      overwrite = overwrite
    )

    output_tbl <- tibble::tibble(
      agent_version = agent_version,
      run_id = agent_run_id,
      project_name = project_info$project_name,
      created = created_time,
      forecast_horizon = forecast_horizon,
      external_regressors = ifelse(is.null(external_regressors), NA_character_, paste(external_regressors, collapse = ", ")),
      hist_end_date = ifelse(is.null(hist_end_date), NA_character_, as.character(hist_end_date)),
      hist_start_date = ifelse(is.null(hist_start_date), NA_character_, as.character(hist_start_date)),
      back_test_scenarios = ifelse(is.null(back_test_scenarios), NA_real_, as.numeric(back_test_scenarios)),
      back_test_spacing = ifelse(is.null(back_test_spacing), NA_real_, as.numeric(back_test_spacing)),
      combo_cleanup_date = ifelse(is.null(combo_cleanup_date), NA_character_, as.character(combo_cleanup_date)),
      forecast_approach = forecast_approach,
      negative_forecast = negative_forecast,
      run_global_models = run_global_models,
      run_local_models = run_local_models
    )

    # write run info to disc
    output_tbl$custom_models <- if (is.null(custom_contract)) NA_character_ else
      paste(sort(names(custom_contract$envelopes)), collapse = "---")
    write_data(
      x = output_tbl,
      combo = NULL,
      run_info = project_info,
      output_type = "log",
      folder = "logs",
      suffix = "-agent_run"
    )

    cli::cli_bullets(c(
      "Created New Finn Agent Run",
      "*" = paste0("Project Name: ", project_info$project_name),
      "*" = paste0("Agent Version: ", agent_version),
      "*" = paste0("Agent Run ID: ", agent_run_id),
      ""
    ))

    return(attach_agent_custom_contract(output_list, custom_contract))
  }
}

#' Set Up Finn Agent Run Information with Custom Logic
#'
#' This function sets up the necessary information for a Finn Agent run,
#' including input data, forecast horizon, and other parameters.
#' It checks for existing runs based on a request ID and allows for overwriting if specified.
#' This allows more advanced control over agent runs when running in production where
#' you may need to rerun forecasts multiple times with the same or updated parameters.
#'
#' @param project_info A Finn project from `set_project_info()`
#' @param llm An ellmer Chat or [chat_copilot()] object used as the template
#'   for isolated agent sessions.
#' @param input_data A data frame or tibble containing the input data
#' @param forecast_horizon The number of periods to forecast
#' @param external_regressors Optional character vector of external regressors
#' @param hist_end_date Optional Date object indicating the end of the historical data
#' @param hist_start_date Optional Date object indicating the start of the historical data
#' @param back_test_scenarios Optional character vector of back test scenarios
#' @param back_test_spacing Optional numeric value for back test spacing
#' @param combo_cleanup_date Optional Date object for combo cleanup
#' @param allow_hierarchical_forecast Logical indicating whether to allow hierarchical forecasting
#' @param negative_forecast If TRUE, allow forecasts to dip below zero.
#' @param run_global_models If TRUE, run multivariate models on the entire data
#'   set (across all time series) as a global model. Default of NULL runs global models for all date types
#'   except week and day.
#' @param run_local_models If TRUE, run models by individual time series as
#'   local models. Default is TRUE.
#' @param overwrite Logical indicating whether to overwrite existing agent run info
#' @param request_id A unique identifier for the agent run request
#' @param agent_action A character string indicating the action: "iterate_forecast" or
#' "update_forecast"
#' @param custom_models Optional approved enrollment forwarded
#'   unchanged to [set_agent_info()]; request identity does not grant approval.
#' @details Matching request IDs resume their exact saved run, not the latest
#'   version. Saved settings are still validated and retry artifacts are not
#'   rewritten. Missing run identities or duplicate request matches fail.
#'
#' @return A list containing the agent run information
#' @noRd
set_agent_info_custom <- function(project_info,
                                  llm,
                                  input_data,
                                  forecast_horizon,
                                  external_regressors = NULL,
                                  hist_end_date = NULL,
                                  hist_start_date = NULL,
                                  back_test_scenarios = NULL,
                                  back_test_spacing = NULL,
                                  combo_cleanup_date = NULL,
                                  allow_hierarchical_forecast = FALSE,
                                  negative_forecast = FALSE,
                                  run_global_models = NULL,
                                  run_local_models = TRUE,
                                  overwrite = FALSE,
                                  request_id,
                                  agent_action,
                                  custom_models = NULL) {
  request_id_value <- request_id

  # check inputs
  check_input_type("request_id", request_id, "character")
  check_input_type("agent_action", agent_action, "character")
  if (!agent_action %in% c("iterate_forecast", "update_forecast")) {
    stop("agent_action must be either 'iterate_forecast' or 'update_forecast'", call. = FALSE)
  }

  # set agent info args
  agent_args <- list(
    project_info = project_info,
    llm = llm,
    input_data = input_data,
    forecast_horizon = forecast_horizon,
    external_regressors = external_regressors,
    hist_end_date = hist_end_date,
    hist_start_date = hist_start_date,
    back_test_scenarios = back_test_scenarios,
    back_test_spacing = back_test_spacing,
    combo_cleanup_date = combo_cleanup_date,
    allow_hierarchical_forecast = allow_hierarchical_forecast,
    negative_forecast = negative_forecast,
    run_global_models = run_global_models,
    run_local_models = run_local_models,
    overwrite = overwrite,
    custom_models = custom_models
  )

  # see if previous agent run exists with same request_id
  agent_runs_tbl <- load_agent_runs(project_info)

  if (nrow(agent_runs_tbl) > 0) {
    # ensure request_id column exists
    if (!"request_id" %in% colnames(agent_runs_tbl)) {
      agent_runs_tbl <- agent_runs_tbl %>%
        dplyr::mutate(request_id = NA_character_)
    } else {
      agent_runs_tbl$request_id <- as.character(agent_runs_tbl$request_id)
    }

    # filter on request id
    agent_run_request_id_tbl <- agent_runs_tbl %>%
      dplyr::filter(request_id == request_id_value)
  } else {
    agent_run_request_id_tbl <- tibble::tibble()
  }

  # use existing agent run info if request id matches
  if (nrow(agent_run_request_id_tbl) > 0) {
    if (nrow(agent_run_request_id_tbl) != 1L) {
      stop("Cannot resume request_id: multiple saved Agent runs match this request. Correct the run metadata.",
        call. = FALSE)
    }
    requested_run <- agent_run_request_id_tbl$run_id
    if (!is.character(requested_run) || length(requested_run) != 1L ||
      is.na(requested_run) || !nzchar(requested_run)) {
      stop("Cannot resume request_id: saved run_id is missing or invalid. Restore the original run metadata.",
        call. = FALSE)
    }
    agent_args$resume_run_id <- requested_run
    if (agent_action == "iterate_forecast") {
      # use existing agent run info with overwrite = FALSE
      agent_args$overwrite <- FALSE
      agent_info <- do.call("set_agent_info_impl", agent_args, quote = TRUE)
    } else if (agent_action == "update_forecast") {
      # use existing agent run info but set overwrite = TRUE manually
      agent_args$overwrite <- FALSE
      agent_info <- do.call("set_agent_info_impl", agent_args, quote = TRUE)
      agent_info$overwrite <- TRUE
    }

    return(agent_info)
  }

  if (nrow(agent_runs_tbl) > 0 && nrow(agent_run_request_id_tbl) == 0 & overwrite == TRUE) {
    # create new agent run info with overwrite = TRUE
    agent_args$overwrite <- TRUE
    agent_info <- do.call("set_agent_info", agent_args, quote = TRUE)
  } else {
    # create new agent run info with current params
    agent_info <- do.call("set_agent_info", agent_args, quote = TRUE)
  }

  # load latest agent runs again
  new_agent_runs_tbl <- load_agent_runs(project_info)

  # filter on latest run and add request id
  new_agent_runs_tbl <- new_agent_runs_tbl %>%
    dplyr::filter(
      agent_version == agent_info$agent_version,
      run_id == agent_info$run_id
    ) %>%
    dplyr::mutate(request_id = as.character(request_id_value))

  # write updated run info with request id to disc
  project_info$run_name <- agent_info$run_id

  write_data(
    x = new_agent_runs_tbl,
    combo = NULL,
    run_info = project_info,
    output_type = "log",
    folder = "logs",
    suffix = "-agent_run"
  )

  # return agent info
  return(agent_info)
}

#' Align Data Frame Column Types
#'
#' This function aligns the column types of `df2` to match those of `df1`
#' for all shared columns.
#'
#' @param df1 A data frame whose column types will be used as reference.
#' @param df2 A data frame whose column types will be aligned to match `df1`.
#'
#' @return A data frame `df2` with column types aligned to `df1`.
#' @noRd
align_types <- function(df1, df2) {
  shared_cols <- intersect(names(df1), names(df2))

  for (col in shared_cols) {
    target_class <- class(df1[[col]])[1]

    # select proper converter
    convert_fun <- switch(target_class,
      Date = function(x) as.Date(x),
      POSIXct = function(x) as.POSIXct(x, tz = if (is.null(attr(df1[[col]], "tzone"))) "UTC" else attr(df1[[col]], "tzone")),
      POSIXt = function(x) as.POSIXct(x, tz = if (is.null(attr(df1[[col]], "tzone"))) "UTC" else attr(df1[[col]], "tzone")),
      factor = function(x) as.factor(x),
      integer = function(x) as.integer(x),
      numeric = function(x) as.numeric(x),
      logical = function(x) as.logical(x),
      character = function(x) as.character(x),
      # fallback: return unchanged
      function(x) x
    )

    # convert df2
    df2[[col]] <- convert_fun(df2[[col]])
  }

  df2
}

#' Load Latest Agent Run Information
#'
#' This function loads the latest agent run information for a given Finn project.
#'
#' @param project_info A Finn project from `set_project_info()`
#'
#' @return A data frame containing the latest agent run information.
#'   Optional textual custom enrollment markers are retained without granting
#'   approval; setup verifies their exact parent record before existing-run reuse.
#' @noRd
load_agent_runs <- function(project_info) {
  # list agent runs
  agent_runs_list <- list_files(
    project_info$storage_object,
    paste0(
      project_info$path, "/logs/*", hash_data(project_info$project_name), "-",
      "*agent_run.", project_info$data_output
    )
  )

  if (length(agent_runs_list)) {
    # get the latest agent run info
    agent_runs_tbl <- read_file(
      run_info = project_info,
      file_list = agent_runs_list,
      return_type = "df"
    )
  } else {
    agent_runs_tbl <- tibble::tibble()
  }

  return(agent_runs_tbl)
}
