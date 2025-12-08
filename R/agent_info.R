#' Set up Finn Agent Run Information
#'
#' This function sets up the necessary information for a Finn Agent run,
#'  including input data, forecast horizon, and other parameters.
#'  It checks for existing runs and allows for overwriting if specified.
#'
#' @param project_info A Finn project from `set_project_info()`
#' @param driver_llm A Chat LLM object
#' @param input_data A data frame or tibble containing the input data
#' @param forecast_horizon The number of periods to forecast
#' @param external_regressors Optional character vector of external regressors
#' @param hist_end_date Optional Date object indicating the end of the historical data
#' @param hist_start_date Optional Date object indicating the start of the historical data
#' @param back_test_scenarios Optional character vector of back test scenarios
#' @param back_test_spacing Optional numeric value for back test spacing
#' @param combo_cleanup_date Optional Date object for combo cleanup
#' @param allow_hierarchical_forecast Logical indicating whether to allow hierarchical forecasting
#' @param run_global_models If TRUE, run multivariate models on the entire data
#'   set (across all time series) as a global model. Default of NULL runs global models for all date types
#'   except week and day.
#' @param run_local_models If TRUE, run models by individual time series as
#'   local models. Default is TRUE.
#' @param reason_llm Optional Chat LLM object for reasoning tasks
#' @param overwrite Logical indicating whether to overwrite existing agent run info
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
#' driver_llm <- ellmer::chat_azure_openai(model = "gpt-4o-mini")
#'
#' # set up agent info
#' agent_info <- set_agent_info(
#'   project_info = project,
#'   driver_llm = driver_llm,
#'   input_data = hist_data,
#'   forecast_horizon = 6
#' )
#' }
#' @export
set_agent_info <- function(project_info,
                           driver_llm,
                           input_data,
                           forecast_horizon,
                           external_regressors = NULL,
                           hist_end_date = NULL,
                           hist_start_date = NULL,
                           back_test_scenarios = NULL,
                           back_test_spacing = NULL,
                           combo_cleanup_date = NULL,
                           allow_hierarchical_forecast = FALSE,
                           run_global_models = NULL,
                           run_local_models = TRUE,
                           reason_llm = NULL,
                           overwrite = FALSE) {
  # get metadata
  combo_variables <- project_info$combo_variables
  target_variable <- project_info$target_variable
  date_type <- project_info$date_type
  fiscal_year_start <- project_info$fiscal_year_start

  # check inputs
  check_input_type("project_info", project_info, "list")
  check_input_type("driver_llm", driver_llm, "Chat")
  check_input_type("input_data", input_data, c("tbl", "tbl_df", "data.frame"))
  check_input_type("forecast_horizon", forecast_horizon, "numeric")
  check_input_type("external_regressors", external_regressors, c("character", "NULL"))
  check_input_type("hist_end_date", hist_end_date, c("Date", "NULL"))
  check_input_type("hist_start_date", hist_start_date, c("Date", "NULL"))
  check_input_type("back_test_scenarios", back_test_scenarios, c("numeric", "NULL"))
  check_input_type("back_test_spacing", back_test_spacing, c("numeric", "NULL"))
  check_input_type("combo_cleanup_date", combo_cleanup_date, c("Date", "NULL"))
  check_input_type("allow_hierarchical_forecast", allow_hierarchical_forecast, "logical")
  check_input_type("run_global_models", run_global_models, c("NULL", "logical"))
  check_input_type("run_local_models", run_local_models, "logical")
  check_input_type("reason_llm", reason_llm, c("Chat", "NULL"))
  check_input_type("overwrite", overwrite, "logical")

  check_input_data(
    input_data,
    combo_variables,
    target_variable,
    external_regressors,
    date_type,
    fiscal_year_start,
    parallel_processing = NULL
  )

  # Inform user about TimeGPT setup
  cli::cli_alert_info("To use TimeGPT models in the agent run, ensure you have set up your API keys.")
  cli::cli_bullets(c(
    "i" = "See setup instructions at: {.url https://microsoft.github.io/finnts/articles/forecasting-genai.html}",
    " " = "Set via environment variable: Sys.setenv(NIXTLA_API_KEY = 'your_key')",
    " " = "Or for Azure: Also set Sys.setenv(NIXTLA_BASE_URL = 'your_url')"
  ))
  # set default for run_global_models based on date_type
  if (is.null(run_global_models) & date_type %in% c("day", "week")) {
    run_global_models <- FALSE
  } else if (is.null(run_global_models)) {
    run_global_models <- TRUE
  }

  if (run_global_models == FALSE & run_local_models == FALSE) {
    stop("At least one of 'run_global_models' or 'run_local_models' must be TRUE.", call. = FALSE)
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
    # check if input values have changed
    current_log_df <- tibble::tibble(
      project_name = project_info$project_name,
      forecast_horizon = forecast_horizon,
      external_regressors = ifelse(is.null(external_regressors), NA, paste(external_regressors, collapse = ", ")),
      hist_end_date = hist_end_date,
      hist_start_date = hist_start_date,
      back_test_scenarios = ifelse(is.null(back_test_scenarios), NA, as.numeric(back_test_scenarios)),
      back_test_spacing = ifelse(is.null(back_test_spacing), NA, as.numeric(back_test_spacing)),
      combo_cleanup_date = if (is.null(combo_cleanup_date)) {
        NA
      } else {
        combo_cleanup_date
      },
      forecast_approach = forecast_approach,
      run_global_models = run_global_models,
      run_local_models = run_local_models
    ) %>%
      data.frame()

    prev_log_df <- align_types(
      current_log_df,
      agent_runs_tbl %>%
        dplyr::select(tidyselect::any_of(colnames(current_log_df)))
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
      } else {
        prev_log_df[[col]] <- current_log_df[[col]]
      }
    }

    if (hash_data(current_log_df) != hash_data(prev_log_df)) {
      stop("Inputs have recently changed in 'set_agent_info',
           please revert back to original inputs or start new agent run with 'overwrite' argument set to TRUE.",
        call. = FALSE
      )
    }

    output_list <- list(
      agent_version = agent_runs_tbl$agent_version,
      run_id = agent_runs_tbl$run_id,
      project_info = project_info,
      driver_llm = driver_llm,
      reason_llm = reason_llm,
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

    return(output_list)
  } else {
    # create unique id for agent run
    created_time <- get_timestamp()

    agent_run_id <- hash_data(created_time)

    # add to project info
    project_info$run_name <- agent_run_id

    # create agent version
    agent_version <- nrow(raw_agent_runs_tbl) + 1

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
      driver_llm = driver_llm,
      reason_llm = reason_llm,
      forecast_horizon = forecast_horizon,
      external_regressors = external_regressors,
      hist_end_date = hist_end_date,
      hist_start_date = hist_start_date,
      back_test_scenarios = back_test_scenarios,
      back_test_spacing = back_test_spacing,
      combo_cleanup_date = combo_cleanup_date,
      forecast_approach = forecast_approach,
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
      external_regressors = ifelse(is.null(external_regressors), NA, paste(external_regressors, collapse = ", ")),
      hist_end_date = ifelse(is.null(hist_end_date), NA, as.character(hist_end_date)),
      hist_start_date = ifelse(is.null(hist_start_date), NA, as.character(hist_start_date)),
      back_test_scenarios = ifelse(is.null(back_test_scenarios), NA, as.numeric(back_test_scenarios)),
      back_test_spacing = ifelse(is.null(back_test_spacing), NA, as.numeric(back_test_spacing)),
      combo_cleanup_date = ifelse(is.null(combo_cleanup_date), NA, as.character(combo_cleanup_date)),
      forecast_approach = forecast_approach,
      run_global_models = run_global_models,
      run_local_models = run_local_models
    )

    # write run info to disc
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

    return(output_list)
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
#' @param driver_llm A Chat LLM object
#' @param input_data A data frame or tibble containing the input data
#' @param forecast_horizon The number of periods to forecast
#' @param external_regressors Optional character vector of external regressors
#' @param hist_end_date Optional Date object indicating the end of the historical data
#' @param hist_start_date Optional Date object indicating the start of the historical data
#' @param back_test_scenarios Optional character vector of back test scenarios
#' @param back_test_spacing Optional numeric value for back test spacing
#' @param combo_cleanup_date Optional Date object for combo cleanup
#' @param allow_hierarchical_forecast Logical indicating whether to allow hierarchical forecasting
#' @param run_global_models If TRUE, run multivariate models on the entire data
#'   set (across all time series) as a global model. Default of NULL runs global models for all date types
#'   except week and day.
#' @param run_local_models If TRUE, run models by individual time series as
#'   local models. Default is TRUE.
#' @param reason_llm Optional Chat LLM object for reasoning tasks
#' @param overwrite Logical indicating whether to overwrite existing agent run info
#' @param request_id A unique identifier for the agent run request
#' @param agent_action A character string indicating the action: "iterate_forecast" or
#' "update_forecast"
#'
#' @return A list containing the agent run information
#' @noRd
set_agent_info_custom <- function(project_info,
                                  driver_llm,
                                  input_data,
                                  forecast_horizon,
                                  external_regressors = NULL,
                                  hist_end_date = NULL,
                                  hist_start_date = NULL,
                                  back_test_scenarios = NULL,
                                  back_test_spacing = NULL,
                                  combo_cleanup_date = NULL,
                                  allow_hierarchical_forecast = FALSE,
                                  run_global_models = NULL,
                                  run_local_models = TRUE,
                                  reason_llm = NULL,
                                  overwrite = FALSE,
                                  request_id,
                                  agent_action) {
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
    driver_llm = driver_llm,
    input_data = input_data,
    forecast_horizon = forecast_horizon,
    external_regressors = external_regressors,
    hist_end_date = hist_end_date,
    hist_start_date = hist_start_date,
    back_test_scenarios = back_test_scenarios,
    back_test_spacing = back_test_spacing,
    combo_cleanup_date = combo_cleanup_date,
    allow_hierarchical_forecast = allow_hierarchical_forecast,
    run_global_models = run_global_models,
    run_local_models = run_local_models,
    reason_llm = reason_llm,
    overwrite = overwrite
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
