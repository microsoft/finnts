
# profile data
data_profile <- function(agent_info) {
  
  # input checks 
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  
  combo_variables <- strsplit(project_info$combo_variables, split = "---")[[1]]
  hist_end_date <- agent_info$hist_end_date
  
  # check if profiling has already been done
  profile_list <- tryCatch(
    read_file(project_info,
              path = paste0("eda/*", hash_data(project_info$project_name), "-", 
                            hash_data(project_info$run_name), "-data_profile.", project_info$object_output),
              return_type = "object"
    ),
    error = function(e) {
      list()
    }
  ) %>%
    base::suppressWarnings()
  
  if(length(profile_list) > 0) {
    message("[agent] Data profile already exists for this agent run.")
    return("Data profile already exists for this agent run. Skipping profiling step.")
  }
  
  # load all time series data
  input_data_list <- list_files(
    project_info$storage_object,
    paste0(
      project_info$path, "/input_data/*", hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "*.", project_info$data_output
    )
  )
  
  if (length(input_data_list) == 0) {
    stop("No input data found for the agent run. Please check the project setup.", call. = FALSE)
  }
  
  input_data <- read_file(
    run_info = project_info,
    file_list = input_data_list,
    return_type = "df"
  )
  
  # ──slice data
  dfx <- input_data %>%
    dplyr::mutate(Date = as.Date(Date)) %>%
    dplyr::filter(Date <= hist_end_date)
  
  if (nrow(dfx) == 0) {
    return("FAIL: No data on or before hist_end_date.")
  }
  
  # core metrics 
  grp <- dfx %>% dplyr::group_by(dplyr::across(tidyselect::all_of(combo_variables)))
  
  n_series  <- dplyr::n_groups(grp)
  rows_per  <- grp %>% dplyr::tally() %>% dplyr::pull(n)
  min_len   <- min(rows_per)
  avg_len   <- mean(rows_per)
  max_len   <- max(rows_per)
  
  total_rows <- nrow(dfx)
  neg_cnt    <- dfx %>% dplyr::filter(Target < 0) %>% nrow()
  neg_pct    <- neg_cnt / total_rows * 100
  
  overall_start <- min(dfx$Date)
  overall_end   <- max(dfx$Date)
  
  # build summary text 
  summary_text <- glue::glue(
    "Data Profile (≤ {hist_end_date})\n",
    "Rows total           : {scales::comma(total_rows)}\n",
    "Distinct series      : {n_series}\n",
    "Rows per series      : min {min_len}, avg {round(avg_len,1)}, max {max_len}\n",
    "Negative Target      : {neg_cnt} rows ({round(neg_pct,2)} %)\n",
    "Overall date span    : {overall_start} → {overall_end}"
  )
  
  # log into metadata 
  entry <- list(
    timestamp      = get_timestamp(),
    type           = "DATA_PROFILE",
    hist_end_date  = as.character(hist_end_date),
    total_rows     = total_rows,
    n_series       = n_series,
    rows_min       = min_len,
    rows_avg       = avg_len,
    rows_max       = max_len,
    neg_count      = neg_cnt,
    neg_pct        = neg_pct,
    date_start     = as.character(overall_start),
    date_end       = as.character(overall_end)
  )
  
  write_data(
    x = entry,
    combo = NULL,
    run_info = project_info,
    output_type = "object",
    folder = "eda",
    suffix = "-data_profile"
  )

  return(summary_text)
}

# acf
acf <- function(agent_info, 
                parallel_processing, 
                num_cores) {
  
  # get metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  
  hist_end_date <- agent_info$hist_end_date
  date_type <- project_info$date_type
  
  # get time series to run
  combo_list <- list_files(
    project_info$storage_object,
    paste0(
      project_info$path, "/input_data/*", hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "*.", project_info$data_output
      )
    ) %>% 
    tibble::tibble(
      Path = .
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(File = ifelse(is.null(Path), "NA", fs::path_file(Path))) %>%
    dplyr::ungroup() %>%
    tidyr::separate(File, into = c("Project", "Agent_Run", "Combo"), sep = "-", remove = TRUE) %>%
    dplyr::pull(Combo) %>%
    stringr::str_remove_all("\\..*$") %>%
    unique() %>%
    suppressWarnings()
  
  # run acf across each time series combo
  par_info <- par_start(
    run_info = project_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    task_length = length(combo_list)
  )
  
  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator
  
  # submit tasks
  final_data <- foreach::foreach(
    x = combo_list,
    .combine = "rbind",
    .packages = packages,
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .noexport = NULL
  ) %op%
    {
      
      # read data
      input_data <- read_file(
        run_info = project_info,
        file_list = list_files(
          project_info$storage_object,
          paste0(
            project_info$path, "/input_data/*", hash_data(project_info$project_name), "-",
            hash_data(agent_info$run_id), "-", x, "*.", project_info$data_output
          )
        ),
        return_type = "df"
      ) %>%
        dplyr::filter(Date <= hist_end_date) %>%
        dplyr::select(Date, Target) %>%
        dplyr::mutate(Target = as.numeric(Target)) %>%
        timetk::pad_by_time(.pad_value = 0, 
                            .by = date_type)
      
      # calculate acf
      acf_result <- stats::acf(input_data$Target, plot = FALSE)
      
      # calculate critical value for significance
      n_obs   <- sum(!is.na(input_data$Target))      # length after any NA removal
      crit    <- 1.96 / sqrt(n_obs)                 # two-sided 95 % limit
      
      # convert to table and filter
      acf_tbl <- tibble::tibble(
        Combo = x,
        Lag = drop(acf_result$lag),
        Value = drop(acf_result$acf)
      ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Significant = abs(Value) > crit) %>%  # flag spikes beyond band
        dplyr::ungroup() %>%
        dplyr::filter(Significant, Lag > 0)     
      
      print(acf_tbl)
    } %>%
    base::suppressPackageStartupMessages()
}


# eda tool
eda_agent_workflow <- function(agent_info, 
                               parallel_processing, 
                               num_cores) {
  
  message("[agent] 🚗 Starting exploratory data analysis workflow")
  
  # construct workflow
  workflow <- list(
    start = list(
      fn = "data_profile", `next` = "acf", max_retry = 2, 
                 args = list("agent_info" = agent_info)
      ),
    acf = list(
      fn = "acf", `next` = "stop", max_retry = 2, 
      args = list("agent_info" = agent_info, 
                  "parallel_processing" = agent_info$parallel_processing, 
                  "num_cores" = agent_info$num_cores)
    ),
    stop  = list(fn = NULL)
  )
  
  # call the agent graph
  results <- run_graph(agent_info$driver_llm, workflow)
  
  return(results)
}

register_eda_tools <- function(agent_info) {
  
  # workflows
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "eda_agent_workflow",
    .description = "perform exploratory data analysis on dataframe",
    .fun = eda_agent_workflow
  )) 
  
  # individual tools
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "data_profile",
    .description = "profile the data to understand its structure and content",
    .fun = data_profile
  )) 
  
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "acf",
    .description = "calculate the autocorrelation function for time series data",
    .fun = acf
  ))
}