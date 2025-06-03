
set_agent_info <- function(project_info, 
                           driver_llm, 
                           input_data, 
                           forecast_horizon, 
                           external_regressors = NULL, 
                           hist_end_date = NULL, 
                           combo_cleanup_date = NULL,
                           overwrite = FALSE) {
  
  # get metadata
  combo_variables <- project_info$combo_variables
  target_variable <- project_info$target_variable
  date_type       <- project_info$date_type
  fiscal_year_start <- project_info$fiscal_year_start
  
  # check inputs 
  check_input_type("project_info", project_info, "list")
  check_input_type("driver_llm", driver_llm, "Chat")
  check_input_type("input_data", input_data, c("tbl", "tbl_df", "data.frame"))
  check_input_type("forecast_horizon", forecast_horizon, "numeric")
  check_input_type("external_regressors", external_regressors, c("character", "NULL"))
  check_input_type("hist_end_date", hist_end_date, c("Date", "NULL"))
  
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
  print(hist_end_date)
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
  
  # check if agent run already exists and isn't finished
  agent_runs_list <- list_files(
    project_info$storage_object,
    paste0(
      project_info$path, "/logs/*", hash_data(project_info$project_name), "-",
      "*agent_run.", project_info$data_output
    )
  )

  if(length(agent_runs_list)) {
    
    agent_runs_tbl <- read_file(run_info = project_info,
                                file_list = agent_runs_list,
                                return_type = "df") %>%
      # get only the latest run
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
      combo_cleanup_date = ifelse(is.null(combo_cleanup_date), NA, as.character(combo_cleanup_date))
    ) %>%
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
      run_id = agent_runs_tbl$run_id,
      project_info = project_info,
      driver_llm = driver_llm,
      forecast_horizon = prev_log_df$forecast_horizon,
      external_regressors = prev_log_df$external_regressors,
      hist_end_date = prev_log_df$hist_end_date,
      combo_cleanup_date = prev_log_df$combo_cleanup_date
    )
    
    cli::cli_bullets(c(
      "Using Existing Finn Agent Run with Previously Uploaded Input Data",
      "*" = paste0("Project Name: ", prev_log_df$project_name),
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
    
    # write combo info to disc
    combo_tbl <- tibble::tibble(
      Combo = unique(final_input_data$Combo),
      State = "inactive", 
      Best_Run = "NA",
      WMAPE = "NA"
    )
    
    write_data(
      x = combo_tbl,
      combo = NULL,
      run_info = project_info,
      output_type = "log",
      folder = "logs",
      suffix = "-agent_leaderboard"
    )
    
    # create agent run metadata
    output_list <- list(
      run_id = agent_run_id,
      project_info = project_info,
      driver_llm = driver_llm,
      forecast_horizon = forecast_horizon,
      external_regressors = external_regressors,
      hist_end_date = hist_end_date,
      combo_cleanup_date = combo_cleanup_date
    )
    
    output_tbl <- tibble::tibble(
      run_id = agent_run_id,
      project_name = project_info$project_name,
      created = created_time, 
      forecast_horizon = forecast_horizon,
      external_regressors = ifelse(is.null(external_regressors), NA, paste(external_regressors, collapse = ", ")),
      hist_end_date = ifelse(is.null(hist_end_date), NA, as.character(hist_end_date)),
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
      "*" = paste0("Run ID: ", agent_run_id),
      ""
    ))
    
    return(output_list)
      
    }
}