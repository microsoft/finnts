
# get time series to run
get_total_combos <- function(agent_info) {
  
  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  
  # get time series to run
  total_combo_list <- list_files(
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
  
  return(total_combo_list)
}

get_finished_combos <- function(agent_info, 
                                eda_wildcard) {
  
  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  
  prev_combo_list <- list_files(
    project_info$storage_object,
    paste0(
      project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), eda_wildcard, project_info$data_output
    )
  ) %>%
    tibble::tibble(
      Path = .
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(File = ifelse(is.null(Path), "NA", fs::path_file(Path))) %>%
    dplyr::ungroup() %>%
    tidyr::separate(File, into = c("Project", "Agent_Run", "Combo", "EDA"), sep = "-", remove = TRUE) %>%
    dplyr::pull(Combo) %>%
    unique() %>%
    suppressWarnings()
  
  return(prev_combo_list)
}

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
    cli::cli_alert_info("Data Profile Already Ran")
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
  
  # slice data
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
acf_scan <- function(agent_info, 
                     parallel_processing, 
                     num_cores) {
  
  # get metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  
  hist_end_date <- agent_info$hist_end_date
  date_type <- project_info$date_type
  
  # get time series to run
  total_combo_list <- get_total_combos(agent_info = agent_info)

  # check if a previous run already has necessary outputs
  prev_combo_list <- get_finished_combos(agent_info = agent_info, 
                                         eda_wildcard = "*-acf.")

  current_combo_list <- setdiff(
    total_combo_list,
    prev_combo_list
  )

  if (length(current_combo_list) == 0 & length(prev_combo_list) > 0) {
    cli::cli_alert_info("ACF Already Ran")
    return(cli::cli_progress_done())
  }
  
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
    x = current_combo_list,
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
        path = paste0(
          "/input_data/", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", x, ".", project_info$data_output
        ),
        return_type = "df"
      ) %>%
        dplyr::filter(Date <= hist_end_date) %>%
        dplyr::select(Combo, Date, Target) %>%
        dplyr::mutate(Target = as.numeric(Target)) %>%
        timetk::pad_by_time(.pad_value = 0, 
                            .by = date_type, 
                            .date_var = Date)

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
        dplyr::filter(Significant, Lag > 0) %>%
        dplyr::select(-Significant)
      
      # save results to disc
      write_data(
        x = acf_tbl,
        combo = unique(input_data$Combo),
        run_info = project_info,
        output_type = "data",
        folder = "eda",
        suffix = "-acf"
      )
      
    } %>%
    base::suppressPackageStartupMessages()
  
  # stop parallel processing
  par_end(cl)
  
  # check if all time series combos ran correctly
  successful_combos <- get_finished_combos(agent_info = agent_info, 
                                           eda_wildcard = "*-acf.")
  
  if (length(successful_combos) != length(total_combo_list)) {
    stop(
      paste0(
        "Not all time series were ran within 'acf', expected ",
        length(total_combo_list), " time series but only ", length(successful_combos),
        " time series were ran. ", "Please run 'acf' again."
      ),
      call. = FALSE
    )
  }
}

# pacf
pacf_scan <- function(agent_info, 
                      parallel_processing, 
                      num_cores) {
  
  # get metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  
  hist_end_date <- agent_info$hist_end_date
  date_type <- project_info$date_type
  
  # get time series to run
  total_combo_list <- get_total_combos(agent_info = agent_info)
  
  # check if a previous run already has necessary outputs
  prev_combo_list <- get_finished_combos(agent_info = agent_info, 
                                         eda_wildcard = "*-pacf.")
  
  current_combo_list <- setdiff(
    total_combo_list,
    prev_combo_list
  )
  
  if (length(current_combo_list) == 0 & length(prev_combo_list) > 0) {
    cli::cli_alert_info("PACF Already Ran")
    return(cli::cli_progress_done())
  }
  
  # run pacf across each time series combo
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
    x = current_combo_list,
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
        path = paste0(
          "/input_data/", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", x, ".", project_info$data_output
        ),
        return_type = "df"
      ) %>%
        dplyr::filter(Date <= hist_end_date) %>%
        dplyr::select(Combo, Date, Target) %>%
        dplyr::mutate(Target = as.numeric(Target)) %>%
        timetk::pad_by_time(.pad_value = 0, 
                            .by = date_type, 
                            .date_var = Date)
      
      # calculate pacf
      pacf_result <- stats::pacf(input_data$Target, plot = FALSE)
      
      # calculate critical value for significance
      n_obs   <- sum(!is.na(input_data$Target))      # length after any NA removal
      crit    <- 1.96 / sqrt(n_obs)                 # two-sided 95 % limit
      
      # convert to table and filter
      pacf_tbl <- tibble::tibble(
        Combo = x,
        Lag = drop(pacf_result$lag),
        Value = drop(pacf_result$acf)
      ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Significant = abs(Value) > crit) %>%  # flag spikes beyond band
        dplyr::ungroup() %>%
        dplyr::filter(Significant, Lag > 0) %>%
        dplyr::select(-Significant)
      
      # save results to disc
      write_data(
        x = pacf_tbl,
        combo = unique(input_data$Combo),
        run_info = project_info,
        output_type = "data",
        folder = "eda",
        suffix = "-pacf"
      )
      
    } %>%
    base::suppressPackageStartupMessages()
  
  # stop parallel processing
  par_end(cl)
  
  # check if all time series combos ran correctly
  successful_combos <- get_finished_combos(agent_info = agent_info, 
                                           eda_wildcard = "*-pacf.")
  
  if (length(successful_combos) != length(total_combo_list)) {
    stop(
      paste0(
        "Not all time series were ran within 'pacf', expected ",
        length(total_combo_list), " time series but only ", length(successful_combos),
        " time series were ran. ", "Please run 'pacf' again."
      ),
      call. = FALSE
    )
  }
}

# stationarity
stationarity_scan <- function(agent_info, 
                              parallel_processing, 
                              num_cores) {
  
  # get metadata 
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  
  hist_end_date <- agent_info$hist_end_date
  date_type     <- project_info$date_type
  
  # identify time-series combos 
  total_combo_list <- get_total_combos(agent_info = agent_info)
  
  # detect previously completed combos 
  prev_combo_list <- get_finished_combos(
    agent_info   = agent_info, 
    eda_wildcard = "*-stationarity."
  )
  
  current_combo_list <- setdiff(
    total_combo_list,
    prev_combo_list
  )
  
  if (length(current_combo_list) == 0 & length(prev_combo_list) > 0) {
    cli::cli_alert_info("Stationarity Already Ran")
    return(cli::cli_progress_done())
  }
  
  # parallel setup 
  par_info <- par_start(
    run_info            = project_info,
    parallel_processing = parallel_processing,
    num_cores           = num_cores,
    task_length         = length(current_combo_list)
  )
  
  cl        <- par_info$cl
  packages  <- c(par_info$packages, "tseries")   # add tseries for adf.test
  `%op%`    <- par_info$foreach_operator
  
  # submit tasks
  final_stat <- foreach::foreach(
    x               = current_combo_list,
    .combine        = "rbind",
    .packages       = packages,
    .errorhandling  = "stop",
    .verbose        = FALSE,
    .inorder        = FALSE,
    .multicombine   = TRUE,
    .noexport       = NULL
  ) %op%
    {
      # read data 
      input_data <- read_file(
        run_info = project_info,
        path = paste0(
          "/input_data/", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", x, ".", project_info$data_output
        ),
        return_type = "df"
      ) %>%
        dplyr::filter(Date <= hist_end_date) %>%
        dplyr::select(Combo, Date, Target) %>%
        dplyr::mutate(Target = as.numeric(Target))
      
      # calculate stationarity
      adf_res <- tseries::adf.test(x = input_data$Target, alternative = "stationary")
      kpss_res <- tseries::kpss.test(input_data$Target, null = "Level")
      
      # build results table 
      stat_tbl <- tibble::tibble(
        Combo          = x,
        p_value_adf    = adf_res$p.value,
        stationary_adf = adf_res$p.value < 0.05, 
        p_value_kpss  = kpss_res$p.value,
        stationary_kpss = kpss_res$p.value > 0.05
      )
      
      # save results to disc
      write_data(
        x          = stat_tbl,
        combo      = unique(input_data$Combo),
        run_info   = project_info,
        output_type = "data",
        folder      = "eda",
        suffix      = "-stationarity"
      )
    } %>%
    base::suppressWarnings() %>%
    base::suppressPackageStartupMessages()
  
  par_end(cl)
  
  # sanity check 
  successful_combos <- get_finished_combos(
    agent_info   = agent_info, 
    eda_wildcard = "*-stationarity."
  )
  
  if (length(successful_combos) != length(total_combo_list)) {
    stop(
      paste0(
        "Not all time series were ran within 'stationarity_scan', expected ",
        length(total_combo_list), " time series but only ", length(successful_combos),
        " time series were ran. ", "Please run 'stationarity_scan' again."
      ),
      call. = FALSE
    )
  }
}

# missing data 
missing_scan <- function(agent_info, 
                         parallel_processing, 
                         num_cores) {
  
  # get metadata 
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  
  hist_end_date <- agent_info$hist_end_date
  date_type     <- project_info$date_type
  
  # identify time-series combos 
  total_combo_list <- get_total_combos(agent_info = agent_info)
  
  # detect previously completed combos 
  prev_combo_list <- get_finished_combos(
    agent_info   = agent_info, 
    eda_wildcard = "*-missing."
  )
  
  current_combo_list <- setdiff(total_combo_list, prev_combo_list)
  
  if (length(current_combo_list) == 0 & length(prev_combo_list) > 0) {
    cli::cli_alert_info("Missing Data Scan Already Ran")
    return(cli::cli_progress_done())
  }
  
  # parallel setup
  par_info <- par_start(
    run_info            = project_info,
    parallel_processing = parallel_processing,
    num_cores           = num_cores,
    task_length         = length(current_combo_list)
  )
  
  cl        <- par_info$cl
  packages  <- par_info$packages          # no extra pkgs needed
  `%op%`    <- par_info$foreach_operator
  
  # submit tasks 
  foreach::foreach(
    x               = current_combo_list,
    .packages       = packages,
    .errorhandling  = "stop",
    .verbose        = FALSE,
    .inorder        = FALSE,
    .multicombine   = TRUE,
    .noexport       = NULL
  ) %op% {
    
    # read data 
    input_data <- read_file(
      run_info = project_info,
      path = paste0(
        "/input_data/", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", x, ".", project_info$data_output
      ),
      return_type = "df"
    ) %>%
      dplyr::filter(Date <= hist_end_date) %>%
      dplyr::select(Combo, Date, Target) %>%
      dplyr::mutate(Target = as.numeric(Target)) %>%
      timetk::pad_by_time(.pad_value = NA_real_, 
                          .by        = date_type, 
                          .date_var  = Date)
    
    # missing-data metrics
    total_rows     <- nrow(input_data)
    missing_count  <- sum(is.na(input_data$Target))
    missing_pct    <- missing_count / total_rows * 100
    
    # longest consecutive NA streak
    rle_na <- rle(is.na(input_data$Target))
    longest_gap <- if (any(rle_na$values)) max(rle_na$lengths[rle_na$values]) else 0L
    
    miss_tbl <- tibble::tibble(
      Combo           = x,
      total_rows      = total_rows,
      missing_count   = missing_count,
      missing_pct     = missing_pct,
      longest_gap     = longest_gap
    )
    
    # save results
    write_data(
      x          = miss_tbl,
      combo      = unique(input_data$Combo),
      run_info   = project_info,
      output_type= "data",
      folder     = "eda",
      suffix     = "-missing"
    )
  } %>%
    base::suppressPackageStartupMessages()
  
  par_end(cl)
  
  # sanity check
  successful_combos <- get_finished_combos(
    agent_info   = agent_info, 
    eda_wildcard = "*-missing."
  )
  
  if (length(successful_combos) != length(total_combo_list)) {
    stop(
      paste0(
        "Not all time series were ran within 'missing_scan', expected ",
        length(total_combo_list), " time series but only ", length(successful_combos),
        " time series were ran. ", "Please run 'missing_scan' again."
      ),
      call. = FALSE
    )
  }
}

# outlier data (STL) 
outlier_scan <- function(agent_info, 
                         parallel_processing, 
                         num_cores) {
  
  # get metadata 
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  
  hist_end_date <- agent_info$hist_end_date
  date_type     <- project_info$date_type
  
  # identify time-series combos 
  total_combo_list <- get_total_combos(agent_info = agent_info)
  
  # detect previously completed combos 
  prev_combo_list <- get_finished_combos(
    agent_info   = agent_info, 
    eda_wildcard = "*-outliers."
  )
  
  current_combo_list <- setdiff(total_combo_list, prev_combo_list)
  
  if (length(current_combo_list) == 0 & length(prev_combo_list) > 0) {
    cli::cli_alert_info("Outlier Scan Already Ran")
    return(cli::cli_progress_done())
  }
  
  # parallel setup 
  par_info <- par_start(
    run_info            = project_info,
    parallel_processing = parallel_processing,
    num_cores           = num_cores,
    task_length         = length(current_combo_list)
  )
  
  cl        <- par_info$cl
  packages  <- par_info$packages          # no extra pkgs needed
  `%op%`    <- par_info$foreach_operator
  
  # helper: map date_type → ts() frequency 
  freq_map <- c(
    "day"     = 7,     # weekly seasonality for daily data
    "week"    = 52,
    "month"   = 12,
    "quarter" = 4,
    "year"    = 1
  )
  freq_val <- freq_map[date_type] %||% 1
  
  # submit tasks 
  foreach::foreach(
    x               = current_combo_list,
    .packages       = packages,
    .errorhandling  = "stop",
    .verbose        = FALSE,
    .inorder        = FALSE,
    .multicombine   = TRUE,
    .noexport       = NULL
  ) %op% {
    
    # read data 
    input_data <- read_file(
      run_info = project_info,
      path = paste0(
        "/input_data/", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", x, ".", project_info$data_output
      ),
      return_type = "df"
    ) %>%
      dplyr::filter(Date <= hist_end_date) %>%
      dplyr::select(Combo, Date, Target) %>%
      dplyr::mutate(Target = as.numeric(Target)) %>%
      timetk::pad_by_time(.pad_value = NA_real_, 
                          .by        = date_type, 
                          .date_var  = Date) %>%
      dplyr::arrange(Date)
    
    # skip series with < 2 * freq observations 
    if (nrow(input_data) < 2 * freq_val) {
      out_tbl <- tibble::tibble(
        Combo            = x,
        total_rows       = nrow(input_data),
        outlier_count    = NA_integer_,
        outlier_pct      = NA_real_,
        first_outlier_dt = as.Date(NA),
        last_outlier_dt  = as.Date(NA)
      )
      
      write_data(
        x          = out_tbl,
        combo      = unique(input_data$Combo),
        run_info   = project_info,
        output_type= "data",
        folder     = "eda",
        suffix     = "-outliers"
      )
      next
    }
    
    # create ts object 
    ts_vec <- stats::ts(input_data$Target, frequency = freq_val)
    
    # STL decomposition (robust) 
    stl_res <- stats::stl(ts_vec, s.window = "periodic", robust = TRUE)
    remainder <- as.numeric(stl_res$time.series[, "remainder"])
    
    # detect outliers in remainder (MAD-based z > 3) 
    med  <- stats::median(remainder, na.rm = TRUE)
    madv <- stats::mad(remainder, constant = 1, na.rm = TRUE)  # raw MAD
    z_sc <- abs(remainder - med) / (1.4826 * madv)             # 1.4826 ≈ to SD
    outlier_flag <- z_sc > 3                                    # 3-sigma rule
    
    # attach flags back to data frame 
    input_data <- input_data %>%
      dplyr::mutate(outlier_flag = outlier_flag)
    
    # summarise 
    total_rows    <- nrow(input_data)
    outlier_count <- sum(input_data$outlier_flag, na.rm = TRUE)
    outlier_pct   <- outlier_count / total_rows * 100
    
    outlier_dates <- input_data$Date[input_data$outlier_flag]
    first_outlier <- if (length(outlier_dates)) min(outlier_dates) else as.Date(NA)
    last_outlier  <- if (length(outlier_dates)) max(outlier_dates) else as.Date(NA)
    
    out_tbl <- tibble::tibble(
      Combo            = x,
      total_rows       = total_rows,
      outlier_count    = outlier_count,
      outlier_pct      = outlier_pct,
      first_outlier_dt = first_outlier,
      last_outlier_dt  = last_outlier
    )
    
    # save results
    write_data(
      x          = out_tbl,
      combo      = unique(input_data$Combo),
      run_info   = project_info,
      output_type= "data",
      folder     = "eda",
      suffix     = "-outliers"
    )
  } %>%
    base::suppressPackageStartupMessages()
  
  par_end(cl)
  
  # sanity check
  successful_combos <- get_finished_combos(
    agent_info   = agent_info, 
    eda_wildcard = "*-outliers."
  )
  
  if (length(successful_combos) != length(total_combo_list)) {
    stop(
      paste0(
        "Not all time series were ran within 'outlier_scan', expected ",
        length(total_combo_list), " time series but only ", length(successful_combos),
        " time series were ran. ", "Please run 'outlier_scan' again."
      ),
      call. = FALSE
    )
  }
}

# multiple seasonality 
seasonality_scan <- function(agent_info, 
                             parallel_processing, 
                             num_cores) {
  
  # get metadata 
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id
  
  hist_end_date <- agent_info$hist_end_date
  date_type     <- project_info$date_type        # "day" | "week" | "month" | "quarter"
  
  # identify time-series combos 
  total_combo_list <- get_total_combos(agent_info = agent_info)
  
  # detect previously completed combos 
  prev_combo_list <- get_finished_combos(
    agent_info   = agent_info, 
    eda_wildcard = "*-add_season."
  )
  
  current_combo_list <- setdiff(total_combo_list, prev_combo_list)
  
  if (length(current_combo_list) == 0 & length(prev_combo_list) > 0) {
    cli::cli_alert_info("Additional Seasonality Scan Already Ran")
    return(cli::cli_progress_done())
  }
  
  # parallel setup 
  par_info <- par_start(
    run_info            = project_info,
    parallel_processing = parallel_processing,
    num_cores           = num_cores,
    task_length         = length(current_combo_list)
  )
  
  cl        <- par_info$cl
  packages  <- par_info$packages
  `%op%`    <- par_info$foreach_operator
  
  # helper: map date_type → ts() frequency 
  freq_map <- c(
    "day"     = 7,     # primary weekly seasonality for daily data
    "week"    = 52,
    "month"   = 12,
    "quarter" = 4,
    "year"    = 1
  )
  primary_freq <- freq_map[date_type] %||% 1
  
  # submit tasks 
  foreach::foreach(
    x               = current_combo_list,
    .packages       = packages,
    .errorhandling  = "stop",
    .verbose        = FALSE,
    .inorder        = FALSE,
    .multicombine   = TRUE,
    .noexport       = NULL
  ) %op% {
    
    # read data 
    input_data <- read_file(
      run_info = project_info,
      path = paste0(
        "/input_data/", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", x, ".", project_info$data_output
      ),
      return_type = "df"
    ) %>%
      dplyr::filter(Date <= hist_end_date) %>%
      dplyr::select(Combo, Date, Target) %>%
      dplyr::mutate(Target = as.numeric(Target)) %>%
      timetk::pad_by_time(.pad_value = 0, 
                          .by        = date_type, 
                          .date_var  = Date) %>%
      dplyr::arrange(Date)
    
    # skip short series 
    if (nrow(input_data) < 2 * primary_freq) {
      add_tbl <- tibble::tibble(
        Combo = x,
        Lag   = integer(),
        Value = numeric()
      )
      
      write_data(
        x          = add_tbl,
        combo      = unique(input_data$Combo),
        run_info   = project_info,
        output_type= "data",
        folder     = "eda",
        suffix     = "-add_season"
      )
      next
    }
    
    # create ts object 
    ts_vec <- stats::ts(input_data$Target, frequency = primary_freq)
    
    # remove primary season via STL 
    stl_res <- stats::stl(ts_vec, s.window = "periodic", robust = TRUE)
    resid   <- as.numeric(stl_res$time.series[, "remainder"])
    
    # residual ACF 
    acf_res <- stats::acf(resid, plot = FALSE)
    
    # significance threshold 
    n_obs <- sum(!is.na(resid))
    crit  <- 1.96 / sqrt(n_obs)
    
    add_tbl <- tibble::tibble(
      Combo = x,
      Lag   = drop(acf_res$lag),
      Value = drop(acf_res$acf)
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Significant = abs(Value) > crit) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Significant, Lag > 0) %>%
      dplyr::select(-Significant)
    
    # save results
    write_data(
      x          = add_tbl,
      combo      = unique(input_data$Combo),
      run_info   = project_info,
      output_type= "data",
      folder     = "eda",
      suffix     = "-add_season"
    )
  } %>%
    base::suppressPackageStartupMessages()
  
  par_end(cl)
  
  # sanity check 
  successful_combos <- get_finished_combos(
    agent_info   = agent_info, 
    eda_wildcard = "*-add_season."
  )
  
  if (length(successful_combos) != length(total_combo_list)) {
    stop(
      paste0(
        "Not all time series were ran within 'seasonality_scan', expected ",
        length(total_combo_list), " time series but only ", length(successful_combos),
        " time series were ran. ", "Please run 'seasonality_scan' again."
      ),
      call. = FALSE
    )
  }
}

# eda tool
eda_agent_workflow <- function(agent_info, 
                               parallel_processing, 
                               num_cores) {
  
  message("[agent] 🚗 Starting exploratory data analysis workflow")
  
  # construct workflow
  workflow <- list(
    start = list(
      fn = "data_profile", `next` = "acf_scan", max_retry = 2, 
                 args = list("agent_info" = agent_info)
      ),
    acf_scan = list(
      fn = "acf_scan", `next` = "pacf_scan", max_retry = 2, 
      args = list("agent_info" = agent_info, 
                  "parallel_processing" = agent_info$parallel_processing, 
                  "num_cores" = agent_info$num_cores)
    ),
    pacf_scan = list(
      fn = "pacf_scan", `next` = "stationarity_scan", max_retry = 2, 
      args = list("agent_info" = agent_info, 
                  "parallel_processing" = agent_info$parallel_processing, 
                  "num_cores" = agent_info$num_cores)
    ),
    stationarity_scan = list(
      fn = "stationarity_scan", `next` = "missing_scan", max_retry = 2, 
      args = list("agent_info" = agent_info, 
                  "parallel_processing" = agent_info$parallel_processing, 
                  "num_cores" = agent_info$num_cores)
    ),
    missing_scan = list(
      fn = "missing_scan", `next` = "outlier_scan", max_retry = 2, 
      args = list("agent_info" = agent_info, 
                  "parallel_processing" = agent_info$parallel_processing, 
                  "num_cores" = agent_info$num_cores)
    ),
    outlier_scan = list(
      fn = "outlier_scan", `next` = "seasonality_scan", max_retry = 2, 
      args = list("agent_info" = agent_info, 
                  "parallel_processing" = agent_info$parallel_processing, 
                  "num_cores" = agent_info$num_cores)
    ),
    seasonality_scan = list(
      fn = "seasonality_scan", `next` = "stop", max_retry = 2, 
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
    .name = "acf_scan",
    .description = "calculate the autocorrelation function for time series data",
    .fun = acf_scan
  ))
  
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "pacf_scan",
    .description = "calculate the partial autocorrelation function for time series data",
    .fun = pacf_scan
  ))
  
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "stationarity_scan",
    .description = "test for stationarity in time series data using ADF and KPSS tests",
    .fun = stationarity_scan
  ))
  
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "missing_scan",
    .description = "scan for missing data in time series and calculate metrics",
    .fun = missing_scan
  ))
  
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "outlier_scan",
    .description = "detect outliers in time series data using STL decomposition",
    .fun = outlier_scan
  ))
  
  agent_info$driver_llm$register_tool(ellmer::tool(
    .name = "seasonality_scan",
    .description = "identify additional seasonal patterns in time series data",
    .fun = seasonality_scan
  ))
}