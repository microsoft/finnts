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
#' @param reason_llm Optional Chat LLM object for reasoning tasks
#' @param overwrite Logical indicating whether to overwrite existing agent run info
#'
#' @return A list containing the agent run information
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

  # check if agent run already exists
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
    ) %>%
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
      combo_cleanup_date = if(is.null(combo_cleanup_date)) {NA} else {combo_cleanup_date}) %>%
      data.frame()

    prev_log_df <- agent_runs_tbl %>%
      dplyr::select(colnames(current_log_df)) %>%
      data.frame()

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
    agent_version <- length(agent_runs_list) + 1

    # write input data to disc
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
      combo_cleanup_date = ifelse(is.null(combo_cleanup_date), NA, as.character(combo_cleanup_date))
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
