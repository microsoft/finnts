#' Set up Finn Agent Run Information
#'
#' This function sets up the necessary information for a Finn Agent run,
#'  including input data, forecast horizon, and other parameters.
#'  It checks for existing runs and allows for overwriting if specified.
#'
#' @param project_info A Finn project from `set_project_info()`
#' @param llm A Chat LLM object used as the template for isolated agent sessions
#' @param input_data A data frame or tibble containing the input data. Leading
#'   and trailing whitespace in character combo-variable values is removed
#'   before Finn creates internal series identifiers and writes input artifacts.
#' @param forecast_horizon The number of periods to forecast
#' @param external_regressors Optional character vector of external regressors
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
#' @param models_to_run Explicit custom or mixed candidate names. Must select at
#'   least one supplied custom model; NULL retains ordinary Agent behavior.
#' @details Custom Agent runs require original-scale R1, CSV data, RDS objects,
#'   local/mounted storage, no hierarchy and `negative_forecast = TRUE`. The exact
#'   approved pool is pinned before the parent run log. Existing-run setup requires
#'   equivalent explicit enrollment; changed or omitted approval requires a new
#'   run. Source is trusted R, not sandboxed. For compatible new-data updates,
#'   create a new parent with `overwrite = TRUE`, the same explicit enrollment,
#'   horizon, regressors, modes and series, then call [update_forecast()] with
#'   explicit `allow_iterate_forecast = FALSE`. Changed enrollment needs iteration.
#'   Supplying definitions alone does not enroll them. Explicit built-in-only
#'   narrowing is not supported by this Agent argument. Mixed pools may select
#'   built-in subsets, but retain the fixed representation. Global-only custom
#'   runs need at least two series. Each selected custom model must support an
#'   enabled mode and the project's cadence, horizon and required predictors.
#'   The return adds `custom_agent_contract` and `custom_agent_contract_id` only
#'   for active enrollment. Saved approval is trusted-caller evidence, not
#'   authenticated identity; forecasting never calls the authoring LLM again.
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
                           custom_models = NULL,
                           models_to_run = NULL) {
  # get metadata
  combo_variables <- project_info$combo_variables
  target_variable <- project_info$target_variable
  date_type <- project_info$date_type
  fiscal_year_start <- project_info$fiscal_year_start

  # check inputs
  check_input_type("project_info", project_info, "list")
  check_input_type("llm", llm, "Chat")
  check_input_type("input_data", input_data, c("tbl", "tbl_df", "data.frame"))
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

  custom_contract <- resolve_agent_custom_models(list(project_info = project_info,
    forecast_horizon = forecast_horizon, external_regressors = external_regressors,
    run_global_models = run_global_models, run_local_models = run_local_models,
    negative_forecast = negative_forecast, allow_hierarchical_forecast = allow_hierarchical_forecast,
    forecast_approach = "bottoms_up"), models_to_run, custom_models)
  if (!is.null(models_to_run) && is.null(custom_contract)) {
    stop("Agent models_to_run must select at least one approved custom model.", call. = FALSE)
  }
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
      tidyselect::all_of(external_regressors),
      "Date", "Target"
    )) %>%
    combo_cleanup_fn(
      combo_cleanup_date,
      hist_end_date
    )

  # check if a hierarchy exists in the data and should be applied
  if (!is.null(custom_contract) && !length(custom_contract$mode_models$local) &&
    length(unique(final_input_data$Combo)) < 2L) {
    stop("Global-only custom Agent runs require at least two series.", call. = FALSE)
  }
  if (allow_hierarchical_forecast) {
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

  # check if agent run already exists
  raw_agent_runs_tbl <- load_agent_runs(project_info)

  if (nrow(raw_agent_runs_tbl) > 0) {
    # filter on latest run
    agent_runs_tbl <- raw_agent_runs_tbl %>%
      dplyr::arrange(dplyr::desc(created)) %>%
      dplyr::slice(1)
  } else {
    agent_runs_tbl <- tibble::tibble()
  }

  if (nrow(agent_runs_tbl) > 0 & overwrite == FALSE) {
    previous_info <- list(project_info = project_info, run_id = as.character(agent_runs_tbl$run_id),
      agent_version = as.numeric(agent_runs_tbl$agent_version))
    previous_marker <- agent_runs_tbl[["custom_agent_contract_id"]]
    marked <- length(previous_marker) == 1L && !is.na(previous_marker) && nzchar(previous_marker)
    previous_contract <- read_agent_custom_record(previous_info, required = marked)
    if (!is.null(custom_contract) || !is.null(previous_contract) || marked) {
      if (is.null(custom_contract) || is.null(previous_contract) || !marked ||
        !identical(as.character(previous_marker), agent_custom_marker(previous_contract)) ||
        !identical(agent_custom_marker(custom_contract), agent_custom_marker(previous_contract))) {
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

    # for older logs that do not yet have allow_hierarchical_forecast recorded,
    # default to the current value for backward compatibility
    if (!"allow_hierarchical_forecast" %in% colnames(prev_log_raw)) {
      prev_log_raw$allow_hierarchical_forecast <- allow_hierarchical_forecast
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
    if (!is.null(custom_contract)) output_tbl$custom_agent_contract_id <- agent_custom_marker(custom_contract)
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
#' @param llm A Chat LLM object used as the template for isolated agent sessions
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
#' @param custom_models,models_to_run Optional approved enrollment forwarded
#'   unchanged to [set_agent_info()]; request identity does not grant approval.
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
                                  custom_models = NULL,
                                  models_to_run = NULL) {
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
    custom_models = custom_models,
    models_to_run = models_to_run
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
    if (agent_action == "iterate_forecast") {
      # use existing agent run info with overwrite = FALSE
      agent_args$overwrite <- FALSE
      agent_info <- do.call("set_agent_info", agent_args, quote = TRUE)
    } else if (agent_action == "update_forecast") {
      # use existing agent run info but set overwrite = TRUE manually
      agent_args$overwrite <- FALSE
      agent_info <- do.call("set_agent_info", agent_args, quote = TRUE)
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
