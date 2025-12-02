#' Get EDA Data
#'
#' Load exploratory data analysis results from a Finn Agent run and return as a single data frame
#'
#' @param agent_info Agent info from `set_agent_info()`
#'
#' @return A data frame containing all EDA results with columns:
#'   - Combo: Time series identifier
#'   - Analysis_Type: Type of EDA analysis (e.g., "ACF", "PACF", "Stationarity", etc.)
#'   - Metric: Specific metric or measure within each analysis type
#'   - Value: Numeric or character value of the metric
#'
#' @examples
#' \dontrun{
#' # Get EDA results for all time series
#' eda_df <- get_eda_data(agent_info)
#'
#' # Filter for specific analysis types
#' acf_results <- eda_df %>%
#'   dplyr::filter(Analysis_Type == "ACF")
#'
#' # Filter for specific time series
#' ts_results <- eda_df %>%
#'   dplyr::filter(Combo == "Product_A--Region_1")
#' }
#' @export
get_eda_data <- function(agent_info) {
  # Check inputs
  check_agent_info(agent_info)

  # Get project info
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  # read eda data
  eda_results <- read_file(
    run_info = project_info,
    path = paste0(
      "/final_output/", hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "-eda.", project_info$data_output
    ),
    return_type = "df"
  )

  return(eda_results)
}

#' Save EDA Data
#'
#' Consolidate exploratory data analysis results from a Finn Agent run and write to disk
#'
#' @param agent_info Agent info from `set_agent_info()`
#'
#' @return Nothing, writes consolidated EDA results to disk
#' @noRd
save_eda_data <- function(agent_info) {
  # Check inputs
  check_agent_info(agent_info)

  # Get project info
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  # Initialize results data frame
  eda_results <- tibble::tibble()

  # Always get all combos
  combo_value <- "*"
  combo_list <- get_total_combos(agent_info)

  # 1. Data Profile
  tryCatch(
    {
      data_profile <- read_file(
        run_info = project_info,
        path = paste0(
          "/eda/", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-data_profile.", project_info$object_output
        ),
        return_type = "object"
      )

      profile_df <- tibble::tibble(
        Combo = "All",
        Analysis_Type = "Data_Profile",
        Metric = c(
          "Total_Rows", "Number_Series", "Min_Rows_Per_Series", "Max_Rows_Per_Series",
          "Avg_Rows_Per_Series", "Negative_Count", "Negative_Percent", "Start_Date", "End_Date"
        ),
        Value = as.character(c(
          data_profile$total_rows, data_profile$n_series, data_profile$rows_min,
          data_profile$rows_max, data_profile$rows_avg, data_profile$neg_count,
          data_profile$neg_pct, data_profile$date_start, data_profile$date_end
        ))
      )

      eda_results <- dplyr::bind_rows(eda_results, profile_df)
    },
    error = function(e) {
      cli::cli_alert_warning("Data profile not found")
    }
  )

  # 2. ACF Results
  tryCatch(
    {
      acf_files <- list_files(
        project_info$storage_object,
        paste0(
          project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", combo_value, "-acf.", project_info$data_output
        )
      )

      if (length(acf_files) > 0) {
        acf_data <- read_file(
          run_info = project_info,
          file_list = acf_files,
          return_type = "df"
        )

        acf_df <- acf_data %>%
          dplyr::mutate(
            Analysis_Type = "ACF",
            Metric = paste0("Lag_", Lag),
            Value = as.character(Value)
          ) %>%
          dplyr::select(Combo, Analysis_Type, Metric, Value)

        eda_results <- dplyr::bind_rows(eda_results, acf_df)
      }
    },
    error = function(e) {
      cli::cli_alert_warning("ACF results not found")
    }
  )

  # 3. PACF Results
  tryCatch(
    {
      pacf_files <- list_files(
        project_info$storage_object,
        paste0(
          project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", combo_value, "-pacf.", project_info$data_output
        )
      )

      if (length(pacf_files) > 0) {
        pacf_data <- read_file(
          run_info = project_info,
          file_list = pacf_files,
          return_type = "df"
        )

        pacf_df <- pacf_data %>%
          dplyr::mutate(
            Analysis_Type = "PACF",
            Metric = paste0("Lag_", Lag),
            Value = as.character(Value)
          ) %>%
          dplyr::select(Combo, Analysis_Type, Metric, Value)

        eda_results <- dplyr::bind_rows(eda_results, pacf_df)
      }
    },
    error = function(e) {
      cli::cli_alert_warning("PACF results not found")
    }
  )

  # 4. Stationarity Results
  tryCatch(
    {
      stat_files <- list_files(
        project_info$storage_object,
        paste0(
          project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", combo_value, "-stationarity.", project_info$data_output
        )
      )

      if (length(stat_files) > 0) {
        stat_data <- read_file(
          run_info = project_info,
          file_list = stat_files,
          return_type = "df"
        )

        stat_df <- stat_data %>%
          dplyr::mutate(
            # Data is stationary if both tests agree (ADF rejects null, KPSS fails to reject null)
            is_stationary = stationary_adf & stationary_kpss,
            Analysis_Type = "Stationarity",
            Metric = "is_stationary",
            Value = as.character(is_stationary)
          ) %>%
          dplyr::select(Combo, Analysis_Type, Metric, Value)

        eda_results <- dplyr::bind_rows(eda_results, stat_df)
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Stationarity results not found")
    }
  )

  # 5. Missing Data Results
  tryCatch(
    {
      miss_files <- list_files(
        project_info$storage_object,
        paste0(
          project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", combo_value, "-missing.", project_info$data_output
        )
      )

      if (length(miss_files) > 0) {
        miss_data <- read_file(
          run_info = project_info,
          file_list = miss_files,
          return_type = "df"
        )

        miss_df <- miss_data %>%
          tidyr::pivot_longer(
            cols = c(total_rows, missing_count, missing_pct, longest_gap),
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          dplyr::mutate(
            Analysis_Type = "Missing_Data",
            Value = as.character(Value)
          ) %>%
          dplyr::select(Combo, Analysis_Type, Metric, Value)

        eda_results <- dplyr::bind_rows(eda_results, miss_df)
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Missing data results not found")
    }
  )

  # 6. Outlier Results
  tryCatch(
    {
      outlier_files <- list_files(
        project_info$storage_object,
        paste0(
          project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", combo_value, "-outliers.", project_info$data_output
        )
      )

      if (length(outlier_files) > 0) {
        outlier_data <- read_file(
          run_info = project_info,
          file_list = outlier_files,
          return_type = "df"
        )

        # Convert dates to character before pivoting
        outlier_df <- outlier_data %>%
          dplyr::mutate(
            total_rows = as.character(total_rows),
            outlier_count = as.character(outlier_count),
            outlier_pct = as.character(round(outlier_pct, 2)),
            first_outlier_dt = as.character(first_outlier_dt),
            last_outlier_dt = as.character(last_outlier_dt)
          ) %>%
          tidyr::pivot_longer(
            cols = c(total_rows, outlier_count, outlier_pct, first_outlier_dt, last_outlier_dt),
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          dplyr::mutate(
            Analysis_Type = "Outliers",
            Value = as.character(Value)
          ) %>%
          dplyr::select(Combo, Analysis_Type, Metric, Value)

        eda_results <- dplyr::bind_rows(eda_results, outlier_df)
      }
    },
    error = function(e) {
      cli::cli_alert_warning(paste0("Outlier results not found. Error: ", e$message))
    }
  )

  # 7. Additional Seasonality Results
  tryCatch(
    {
      season_files <- list_files(
        project_info$storage_object,
        paste0(
          project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", combo_value, "-add_season.", project_info$data_output
        )
      )

      if (length(season_files) > 0) {
        season_data <- read_file(
          run_info = project_info,
          file_list = season_files,
          return_type = "df"
        )

        season_df <- season_data %>%
          dplyr::mutate(
            Analysis_Type = "Additional_Seasonality",
            Metric = paste0("Lag_", Lag),
            Value = as.character(Value)
          ) %>%
          dplyr::select(Combo, Analysis_Type, Metric, Value)

        eda_results <- dplyr::bind_rows(eda_results, season_df)
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Additional seasonality results not found")
    }
  )

  # 8. Hierarchy Detection
  tryCatch(
    {
      hier_data <- read_file(
        run_info = project_info,
        path = paste0(
          "/eda/", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-hierarchy.", project_info$object_output
        ),
        return_type = "object"
      )

      hier_df <- tibble::tibble(
        Combo = "All",
        Analysis_Type = "Hierarchy",
        Metric = "hierarchy_type",
        Value = hier_data$hierarchy
      )

      eda_results <- dplyr::bind_rows(eda_results, hier_df)
    },
    error = function(e) {
      cli::cli_alert_warning("Hierarchy detection results not found")
    }
  )

  # 9. External Regressor Results
  if (!is.null(agent_info$external_regressors)) {
    tryCatch(
      {
        xreg_files <- list_files(
          project_info$storage_object,
          paste0(
            project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
            hash_data(agent_info$run_id), "-", combo_value, "-xreg_scan.", project_info$data_output
          )
        )

        if (length(xreg_files) > 0) {
          xreg_data <- read_file(
            run_info = project_info,
            file_list = xreg_files,
            return_type = "df"
          )

          xreg_df <- xreg_data %>%
            dplyr::mutate(
              Analysis_Type = "External_Regressor_Distance_Correlation",
              Metric = paste0(Regressor, "_Lag_", Lag),
              Value = as.character(dCor)
            ) %>%
            dplyr::select(Combo, Analysis_Type, Metric, Value)

          eda_results <- dplyr::bind_rows(eda_results, xreg_df)
        }
      },
      error = function(e) {
        cli::cli_alert_warning(paste0("External regressor results not found. Error: ", e$message))
      }
    )
  }

  # Convert Value column to appropriate type where possible
  eda_results <- eda_results %>%
    dplyr::mutate(
      Value_Numeric = suppressWarnings(as.numeric(Value)),
      Value = dplyr::coalesce(as.character(Value_Numeric), Value)
    ) %>%
    dplyr::select(-Value_Numeric)

  # write the final eda results
  write_data(
    x = eda_results,
    combo = NULL,
    run_info = project_info,
    output_type = "data",
    folder = "final_output",
    suffix = "-eda"
  )

  # return nothing
  invisible(NULL)
}


#' Exploratory Data Analysis Agent Workflow
#'
#' @param agent_info agent information list
#' @param parallel_processing whether to use parallel processing
#' @param num_cores number of cores to use for parallel processing
#'
#' @return nothing
#' @noRd
eda_agent_workflow <- function(agent_info,
                               parallel_processing,
                               num_cores) {
  message("[agent] Starting Exploratory Data Analysis Workflow")

  # construct workflow
  workflow <- list(
    start = list(
      fn = "data_profile",
      `next` = "run_all_eda_per_combo",
      retry_mode = "plain",
      max_retry = 2,
      args = list("agent_info" = agent_info)
    ),
    run_all_eda_per_combo = list(
      fn = "run_all_eda_per_combo",
      `next` = "hierarchy_detect",
      retry_mode = "plain",
      max_retry = 2,
      args = list(
        "agent_info" = agent_info,
        "parallel_processing" = parallel_processing,
        "num_cores" = num_cores
      )
    ),
    hierarchy_detect = list(
      fn = "hierarchy_detect",
      `next` = "save_eda_data",
      retry_mode = "plain",
      max_retry = 2,
      args = list("agent_info" = agent_info)
    ),
    save_eda_data = list(
      fn = "save_eda_data",
      `next` = "stop",
      retry_mode = "plain",
      max_retry = 2,
      args = list("agent_info" = agent_info)
    ),
    stop = list(fn = NULL)
  )

  # call the agent graph
  run_graph(agent_info$driver_llm, workflow)
}

#' Load EDA results for a specific agent run for iterate forecast reasoning
#'
#' @param agent_info agent information list
#' @param combo specific combo to load results for (optional)
#'
#' @return list containing EDA results
#' @noRd
load_eda_results <- function(agent_info,
                             combo = NULL) {
  # get project info
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  # read all combos or just one
  if (is.null(combo)) {
    combo_value <- "*"
  } else {
    combo_value <- combo
  }

  # data profile
  data_profile <- read_file(
    run_info = project_info,
    path = paste0(
      "/eda/", hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "-data_profile.", project_info$object_output
    ),
    return_type = "object"
  )

  if (is.null(combo)) {
    data_profile_prompt <-
      glue::glue("Data Profile:
                - Total Rows: {data_profile$total_rows}
                - Number of Time Series: {data_profile$n_series}
                - Min Rows Per Series: {data_profile$rows_min}
                - Max Rows Per Series: {data_profile$rows_max}
                - Avg Rows Per Series: {data_profile$rows_avg}
                - Count of Negative Values: {data_profile$neg_count}
                - Percent of Negative Values: {data_profile$neg_pct}%
                - Start Date: {data_profile$date_start}
                - End Date: {data_profile$date_end}
                ")
  } else {
    data_profile_prompt <-
      glue::glue("Data Profile:
                - Total Rows: {round(data_profile$total_rows/data_profile$n_series, 0)}
                - Percent of Negative Values: {data_profile$neg_pct}%
                - Start Date: {data_profile$date_start}
                - End Date: {data_profile$date_end}
                ")
  }

  # acf scan
  acf_scan <- read_file(
    run_info = project_info,
    file_list = list_files(
      project_info$storage_object,
      paste0(
        project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", combo_value, "-acf.", project_info$data_output
      )
    ),
    return_type = "df"
  ) %>%
    dplyr::mutate(Value = as.numeric(Value))

  if (is.null(combo)) {
    # summarize across all combos
    acf_scan <- acf_scan %>%
      dplyr::group_by(Lag) %>%
      dplyr::summarise(
        Combo_Count = dplyr::n(),
        Combo_Percent = dplyr::n() / data_profile$n_series * 100,
        Avg_Value = round(mean(Value, na.rm = TRUE), 2),
        Mean_Abs_Value = round(mean(abs(Value), na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Lag)
  } else {
    acf_scan <- acf_scan %>%
      dplyr::select(-Combo)
  }

  acf_scan_prompt <-
    glue::glue("ACF Scan Results:
                {make_pipe_table(acf_scan)}
                ")

  # pacf scan
  pacf_scan <- read_file(
    run_info = project_info,
    file_list = list_files(
      project_info$storage_object,
      paste0(
        project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", combo_value, "-pacf.", project_info$data_output
      )
    ),
    return_type = "df"
  ) %>%
    dplyr::mutate(Value = as.numeric(Value))

  if (is.null(combo)) {
    # summarize across all combos
    pacf_scan <- pacf_scan %>%
      dplyr::group_by(Lag) %>%
      dplyr::summarise(
        Combo_Count = dplyr::n(),
        Combo_Percent = dplyr::n() / data_profile$n_series * 100,
        Avg_Value = round(mean(Value, na.rm = TRUE), 2),
        Mean_Abs_Value = round(mean(abs(Value), na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Lag)
  } else {
    pacf_scan <- pacf_scan %>%
      dplyr::select(-Combo)
  }

  pacf_scan_prompt <-
    glue::glue("PACF Scan Results:
                {make_pipe_table(pacf_scan)}
                ")

  # stationarity scan
  stationarity_scan <- read_file(
    run_info = project_info,
    file_list = list_files(
      project_info$storage_object,
      paste0(
        project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", combo_value, "-stationarity.", project_info$data_output
      )
    ),
    return_type = "df"
  )

  if (is.null(combo)) {
    stationarity_scan <- stationarity_scan %>%
      dplyr::mutate(Stationary = ifelse(stationary_adf & stationary_kpss, "Stationary", "Non-Stationary")) %>%
      dplyr::group_by(Stationary) %>%
      dplyr::summarise(
        Count = dplyr::n(),
        Percent = dplyr::n() / data_profile$n_series * 100,
        .groups = "drop"
      )
  } else {
    stationarity_scan <- stationarity_scan %>%
      dplyr::select(-Combo)
  }

  stationarity_scan_prompt <-
    glue::glue("Stationarity Scan Results:
                {make_pipe_table(stationarity_scan)}
                ")

  # missing data scan
  missing_scan <- read_file(
    run_info = project_info,
    file_list = list_files(
      project_info$storage_object,
      paste0(
        project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", combo_value, "-missing.", project_info$data_output
      )
    ),
    return_type = "df"
  )

  if (is.null(combo)) {
    missing_scan <- missing_scan %>%
      dplyr::group_by() %>%
      dplyr::summarise(
        missing_count = sum(missing_count, na.rm = TRUE),
        missing_pct = mean(missing_pct, na.rm = TRUE),
        longest_gap = max(longest_gap, na.rm = TRUE),
        .groups = "drop"
      )
  }

  missing_scan_prompt <-
    glue::glue("Missing Data Scan Results:
                - Missing Count: {missing_scan$missing_count}
                - Missing Percent: {round(missing_scan$missing_pct)}%
                - Longest Gap: {missing_scan$longest_gap}
                ")

  # outlier scan
  outlier_scan <- read_file(
    run_info = project_info,
    file_list = list_files(
      project_info$storage_object,
      paste0(
        project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", combo_value, "-outliers.", project_info$data_output
      )
    ),
    return_type = "df"
  )

  if (is.null(combo)) {
    outlier_scan <- outlier_scan %>%
      dplyr::group_by() %>%
      dplyr::summarise(
        total_rows = sum(total_rows, na.rm = TRUE),
        outlier_count = sum(outlier_count, na.rm = TRUE),
        outlier_pct = outlier_count / total_rows * 100, ,
        first_outlier_dt = min(first_outlier_dt, na.rm = TRUE),
        last_outlier_dt = max(last_outlier_dt, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    outlier_scan <- outlier_scan %>%
      dplyr::select(-Combo)
  }

  outlier_scan_prompt <-
    glue::glue("Outlier Scan Results:
                - Outlier Count: {outlier_scan$outlier_count}
                - Outlier Percent: {round(outlier_scan$outlier_pct)}%
                - First Outlier Date: {outlier_scan$first_outlier_dt}
                - Last Outlier Date: {outlier_scan$last_outlier_dt}
                ")

  # seasonality scan
  seasonality_scan <- read_file(
    run_info = project_info,
    file_list = list_files(
      project_info$storage_object,
      paste0(
        project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-", combo_value, "-add_season.", project_info$data_output
      )
    ),
    return_type = "df"
  ) %>%
    dplyr::mutate(Value = as.numeric(Value))

  if (is.null(combo)) {
    seasonality_scan <- seasonality_scan %>%
      dplyr::group_by(Lag) %>%
      dplyr::summarise(
        Combo_Count = dplyr::n(),
        Combo_Percent = dplyr::n() / data_profile$n_series * 100,
        Avg_ACF_Value = round(mean(Value, na.rm = TRUE), 2),
        Mean_Abs_ACF_Value = round(mean(abs(Value), na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Lag)
  } else {
    seasonality_scan <- seasonality_scan %>%
      dplyr::select(-Combo)
  }

  seasonality_scan_prompt <-
    glue::glue("Additional Seasonality Scan Results:
                {make_pipe_table(seasonality_scan)}
                ")

  # hierarchy detection
  hierarchy_detect <- read_file(
    run_info = project_info,
    path = paste0(
      "/eda/", hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "-hierarchy.", project_info$object_output
    ),
    return_type = "object"
  )

  if (is.null(combo)) {
    hierarchy_detect_prompt <-
      glue::glue("Hierarchy Detection Results:
                - Hierarchy Type: {hierarchy_detect$hierarchy}
                ")
  } else {
    hierarchy_detect_prompt <-
      glue::glue("Hierarchy Detection Results:
                - Hierarchy Type: bottoms_up (single time series)
                ")
  }

  # external regressor scan
  if (is.null(agent_info$external_regressors)) {
    xreg_scan_prompt <- "No external regressors set for this agent run."
  } else {
    xreg_scan <- read_file(
      run_info = project_info,
      file_list = list_files(
        project_info$storage_object,
        paste0(
          project_info$path, "/eda/*", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", combo_value, "-xreg_scan.", project_info$data_output
        )
      ),
      return_type = "df"
    )

    if (is.null(combo)) {
      xreg_scan <- xreg_scan %>%
        dplyr::group_by(Regressor, Lag) %>%
        dplyr::summarise(
          Avg_dCor = round(mean(dCor, na.rm = TRUE), 2),
          Median_dCor = round(median(dCor, na.rm = TRUE), 2),
          Max_dCor = round(max(dCor, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(Avg_dCor))
    } else {
      xreg_scan <- xreg_scan %>%
        dplyr::select(-Combo)
    }

    xreg_scan_prompt <-
      glue::glue("External Regressor Scan Results:
                  {make_pipe_table(xreg_scan)}
                  ")
  }

  # combine all prompts into one overall prompt using glue
  overall_prompt <- glue::glue(
    "{data_profile_prompt}

    {acf_scan_prompt}

    {pacf_scan_prompt}

    {stationarity_scan_prompt}

    {missing_scan_prompt}

    {outlier_scan_prompt}

    {seasonality_scan_prompt}

    {hierarchy_detect_prompt}

    {xreg_scan_prompt}"
  )

  return(overall_prompt)
}

#' Data profiling EDA tool
#'
#' @param agent_info agent information list
#'
#' @return summary text of data profile
#' @noRd
data_profile <- function(agent_info) {
  # get metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  combo_variables <- strsplit(project_info$combo_variables, split = "---")[[1]]
  hist_end_date <- agent_info$hist_end_date

  # check if profiling has already been done
  profile_list <- tryCatch(
    read_file(project_info,
      path = paste0(
        "eda/*", hash_data(project_info$project_name), "-",
        hash_data(agent_info$run_id), "-data_profile.", project_info$object_output
      ),
      return_type = "object"
    ),
    error = function(e) {
      list()
    }
  ) %>%
    base::suppressWarnings()

  if (length(profile_list) > 0) {
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
  grp <- dfx %>% dplyr::group_by(Combo)

  n_series <- dplyr::n_groups(grp)
  rows_per <- grp %>%
    dplyr::tally() %>%
    dplyr::pull(n)
  min_len <- min(rows_per)
  avg_len <- mean(rows_per)
  max_len <- max(rows_per)

  total_rows <- nrow(dfx)
  neg_cnt <- dfx %>%
    dplyr::filter(Target < 0) %>%
    nrow()
  neg_pct <- neg_cnt / total_rows * 100

  overall_start <- min(dfx$Date)
  overall_end <- max(dfx$Date)

  # build summary text
  summary_text <- glue::glue(
    "Data Profile (up until {hist_end_date})\n",
    "Rows total           : {scales::comma(total_rows)}\n",
    "Distinct series      : {n_series}\n",
    "Rows per series      : min {min_len}, avg {round(avg_len,1)}, max {max_len}\n",
    "Negative Target      : {neg_cnt} rows ({round(neg_pct,2)} %)\n",
    "Overall date span    : {overall_start} to {overall_end}"
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

#' Run ACF analysis on a single time series
#'
#' @param input_data data frame with Combo, Date, and Target columns
#' @param combo_name name of the combo
#' @param date_type date type (day, week, month, quarter, year)
#' @param project_info project information list
#'
#' @return nothing, writes results to disk
#' @noRd
run_acf_analysis <- function(input_data, combo_name, date_type, project_info) {
  tryCatch(
    {
      input_data_acf <- input_data %>%
        dplyr::select(Combo, Date, Target) %>%
        dplyr::mutate(Target = as.numeric(Target)) %>%
        dplyr::group_by(Combo) %>%
        timetk::pad_by_time(
          .pad_value = 0,
          .by = date_type,
          .date_var = Date
        ) %>%
        dplyr::ungroup()

      # calculate maximum lag
      date_type_max_lag <- switch(date_type,
        "day" = 364 * 2,
        "week" = 52 * 2,
        "month" = 12 * 2,
        "quarter" = 4 * 2,
        "year" = 10
      )

      max_lag <- min(nrow(input_data_acf) - 1, date_type_max_lag)

      # calculate acf
      acf_result <- stats::acf(input_data_acf$Target, plot = FALSE, lag.max = max_lag)
      n_obs <- sum(!is.na(input_data_acf$Target))
      crit <- 1.96 / sqrt(n_obs)

      # convert to table and filter
      acf_tbl <- tibble::tibble(
        Combo = combo_name,
        Lag = drop(acf_result$lag),
        Value = drop(acf_result$acf)
      ) %>%
        dplyr::mutate(Value = round(Value, 2)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Significant = abs(Value) > crit) %>%
        dplyr::ungroup() %>%
        dplyr::filter(Significant, Lag > 0) %>%
        dplyr::select(-Significant)

      # save results
      write_data(
        x = acf_tbl,
        combo = combo_name,
        run_info = project_info,
        output_type = "data",
        folder = "eda",
        suffix = "-acf"
      )
    },
    error = function(e) {
      cli::cli_alert_warning(paste0("ACF failed for ", combo_name, ": ", e$message))
    }
  )
}

#' Run PACF analysis on a single time series
#'
#' @param input_data data frame with Combo, Date, and Target columns
#' @param combo_name name of the combo
#' @param date_type date type (day, week, month, quarter, year)
#' @param project_info project information list
#'
#' @return nothing, writes results to disk
#' @noRd
run_pacf_analysis <- function(input_data, combo_name, date_type, project_info) {
  tryCatch(
    {
      input_data_pacf <- input_data %>%
        dplyr::select(Combo, Date, Target) %>%
        dplyr::mutate(Target = as.numeric(Target)) %>%
        dplyr::group_by(Combo) %>%
        timetk::pad_by_time(
          .pad_value = 0,
          .by = date_type,
          .date_var = Date
        ) %>%
        dplyr::ungroup()

      # calculate maximum lag
      date_type_max_lag <- switch(date_type,
        "day" = 364 * 2,
        "week" = 52 * 2,
        "month" = 12 * 2,
        "quarter" = 4 * 2,
        "year" = 10
      )

      max_lag <- min(nrow(input_data_pacf) - 1, date_type_max_lag)

      if (max_lag > 0) {
        # calculate pacf
        pacf_result <- stats::pacf(input_data_pacf$Target, plot = FALSE, lag.max = max_lag)
        n_obs <- sum(!is.na(input_data_pacf$Target))
        crit <- 1.96 / sqrt(n_obs)

        # convert to table and filter
        pacf_tbl <- tibble::tibble(
          Combo = combo_name,
          Lag = drop(pacf_result$lag),
          Value = drop(pacf_result$acf)
        ) %>%
          dplyr::mutate(Value = round(Value, 2)) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(Significant = abs(Value) > crit) %>%
          dplyr::ungroup() %>%
          dplyr::filter(Significant, Lag > 0) %>%
          dplyr::select(-Significant)
      } else {
        pacf_tbl <- tibble::tibble(
          Combo = combo_name,
          Lag = 1,
          Value = 0
        ) %>%
          dplyr::filter(Value > 0)
      }

      # save results
      write_data(
        x = pacf_tbl,
        combo = combo_name,
        run_info = project_info,
        output_type = "data",
        folder = "eda",
        suffix = "-pacf"
      )
    },
    error = function(e) {
      cli::cli_alert_warning(paste0("PACF failed for ", combo_name, ": ", e$message))
    }
  )
}

#' Run stationarity analysis on a single time series
#'
#' @param input_data data frame with Combo, Date, and Target columns
#' @param combo_name name of the combo
#' @param date_type date type (day, week, month, quarter, year)
#' @param hist_start_date historical start date
#' @param hist_end_date historical end date
#' @param project_info project information list
#'
#' @return nothing, writes results to disk
#' @noRd
run_stationarity_analysis <- function(input_data, combo_name, date_type, hist_start_date, hist_end_date, project_info) {
  tryCatch(
    {
      input_data_stat <- input_data %>%
        dplyr::select(Combo, Date, Target) %>%
        dplyr::mutate(Target = as.numeric(Target)) %>%
        dplyr::group_by(Combo) %>%
        timetk::pad_by_time(
          .pad_value = 0,
          .by = date_type,
          .date_var = Date,
          .start_date = hist_start_date,
          .end_date = hist_end_date
        ) %>%
        dplyr::ungroup()

      # calculate stationarity
      adf_res <- tseries::adf.test(x = input_data_stat$Target, alternative = "stationary")
      kpss_res <- tseries::kpss.test(input_data_stat$Target, null = "Level")

      # build results table
      stat_tbl <- tibble::tibble(
        Combo = combo_name,
        p_value_adf = round(adf_res$p.value, 2),
        stationary_adf = adf_res$p.value < 0.05,
        p_value_kpss = round(kpss_res$p.value, 2),
        stationary_kpss = kpss_res$p.value > 0.05
      )

      # save results
      write_data(
        x = stat_tbl,
        combo = combo_name,
        run_info = project_info,
        output_type = "data",
        folder = "eda",
        suffix = "-stationarity"
      )
    },
    error = function(e) {
      cli::cli_alert_warning(paste0("Stationarity failed for ", combo_name, ": ", e$message))
    }
  )
}

#' Run missing data analysis on a single time series
#'
#' @param input_data data frame with Combo, Date, and Target columns
#' @param combo_name name of the combo
#' @param date_type date type (day, week, month, quarter, year)
#' @param project_info project information list
#'
#' @return nothing, writes results to disk
#' @noRd
run_missing_analysis <- function(input_data, combo_name, date_type, project_info) {
  tryCatch(
    {
      input_data_miss <- input_data %>%
        dplyr::select(Combo, Date, Target) %>%
        dplyr::mutate(Target = as.numeric(Target)) %>%
        dplyr::group_by(Combo) %>%
        timetk::pad_by_time(
          .pad_value = NA_real_,
          .by = date_type,
          .date_var = Date
        ) %>%
        dplyr::ungroup()

      # missing-data metrics
      total_rows <- nrow(input_data_miss)
      missing_count <- sum(is.na(input_data_miss$Target))
      missing_pct <- missing_count / total_rows * 100

      # longest consecutive NA streak
      rle_na <- rle(is.na(input_data_miss$Target))
      longest_gap <- if (any(rle_na$values)) max(rle_na$lengths[rle_na$values]) else 0L

      miss_tbl <- tibble::tibble(
        Combo = combo_name,
        total_rows = total_rows,
        missing_count = missing_count,
        missing_pct = missing_pct,
        longest_gap = longest_gap
      )

      # save results
      write_data(
        x = miss_tbl,
        combo = combo_name,
        run_info = project_info,
        output_type = "data",
        folder = "eda",
        suffix = "-missing"
      )
    },
    error = function(e) {
      cli::cli_alert_warning(paste0("Missing data scan failed for ", combo_name, ": ", e$message))
    }
  )
}

#' Run outlier analysis on a single time series
#'
#' @param input_data data frame with Combo, Date, and Target columns
#' @param combo_name name of the combo
#' @param date_type date type (day, week, month, quarter, year)
#' @param freq_val frequency value for time series
#' @param project_info project information list
#'
#' @return nothing, writes results to disk
#' @noRd
run_outlier_analysis <- function(input_data, combo_name, date_type, freq_val, project_info) {
  tryCatch(
    {
      input_data_outlier <- input_data %>%
        dplyr::select(Combo, Date, Target) %>%
        dplyr::mutate(Target = as.numeric(Target)) %>%
        dplyr::group_by(Combo) %>%
        timetk::pad_by_time(
          .pad_value = 0,
          .by = date_type,
          .date_var = Date
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(Date)

      # skip series with not enough data
      if (nrow(input_data_outlier) <= 2 * freq_val) {
        out_tbl <- tibble::tibble(
          Combo = combo_name,
          total_rows = nrow(input_data_outlier),
          outlier_count = NA_integer_,
          outlier_pct = NA_real_,
          first_outlier_dt = as.Date(NA),
          last_outlier_dt = as.Date(NA)
        )
      } else {
        # create ts object
        ts_vec <- stats::ts(input_data_outlier$Target, frequency = freq_val)

        # STL decomposition (robust)
        stl_res <- stats::stl(ts_vec, s.window = "periodic", robust = TRUE)
        remainder <- as.numeric(stl_res$time.series[, "remainder"])

        # detect outliers in remainder (MAD-based z > 3)
        med <- stats::median(remainder, na.rm = TRUE)
        madv <- stats::mad(remainder, constant = 1, na.rm = TRUE)
        z_sc <- abs(remainder - med) / (1.4826 * madv)
        outlier_flag <- z_sc > 3

        # attach flags back to data frame
        input_data_outlier <- input_data_outlier %>%
          dplyr::mutate(outlier_flag = outlier_flag)

        # summarise
        total_rows <- nrow(input_data_outlier)
        outlier_count <- sum(input_data_outlier$outlier_flag, na.rm = TRUE)
        outlier_pct <- outlier_count / total_rows * 100

        outlier_dates <- input_data_outlier$Date[input_data_outlier$outlier_flag]
        first_outlier <- if (length(outlier_dates)) min(outlier_dates) else as.Date(NA)
        last_outlier <- if (length(outlier_dates)) max(outlier_dates) else as.Date(NA)

        out_tbl <- tibble::tibble(
          Combo = combo_name,
          total_rows = total_rows,
          outlier_count = outlier_count,
          outlier_pct = round(outlier_pct, 2),
          first_outlier_dt = first_outlier,
          last_outlier_dt = last_outlier
        )
      }

      # save results
      write_data(
        x = out_tbl,
        combo = combo_name,
        run_info = project_info,
        output_type = "data",
        folder = "eda",
        suffix = "-outliers"
      )
    },
    error = function(e) {
      cli::cli_alert_warning(paste0("Outlier scan failed for ", combo_name, ": ", e$message))
    }
  )
}

#' Run seasonality analysis on a single time series
#'
#' @param input_data data frame with Combo, Date, and Target columns
#' @param combo_name name of the combo
#' @param date_type date type (day, week, month, quarter, year)
#' @param freq_val frequency value for time series
#' @param project_info project information list
#'
#' @return nothing, writes results to disk
#' @noRd
run_seasonality_analysis <- function(input_data, combo_name, date_type, freq_val, project_info) {
  tryCatch(
    {
      input_data_season <- input_data %>%
        dplyr::select(Combo, Date, Target) %>%
        dplyr::mutate(Target = as.numeric(Target)) %>%
        dplyr::group_by(Combo) %>%
        timetk::pad_by_time(
          .pad_value = 0,
          .by = date_type,
          .date_var = Date
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(Date)

      # skip short series
      if (nrow(input_data_season) <= 2 * freq_val) {
        add_tbl <- tibble::tibble(
          Combo = combo_name,
          Lag = integer(),
          Value = numeric()
        )
      } else {
        # create ts object
        ts_vec <- stats::ts(input_data_season$Target, frequency = freq_val)

        # remove primary season via STL
        stl_res <- stats::stl(ts_vec, s.window = "periodic", robust = TRUE)
        resid <- as.numeric(stl_res$time.series[, "remainder"])

        # calc maximum lag
        date_type_max_lag <- switch(date_type,
          "day" = 364 * 2,
          "week" = 52 * 2,
          "month" = 12 * 2,
          "quarter" = 4 * 2,
          "year" = 10
        )

        max_lag <- min(length(resid) - 1, date_type_max_lag)

        # residual ACF
        acf_res <- stats::acf(resid, plot = FALSE, lag.max = max_lag)

        # significance threshold
        n_obs <- sum(!is.na(resid))
        crit <- 1.96 / sqrt(n_obs)

        add_tbl <- tibble::tibble(
          Combo = combo_name,
          Lag = drop(acf_res$lag),
          Value = drop(acf_res$acf)
        ) %>%
          dplyr::mutate(Value = round(Value, 2)) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(Significant = abs(Value) > crit) %>%
          dplyr::ungroup() %>%
          dplyr::filter(Significant, Lag > 0) %>%
          dplyr::select(-Significant)
      }

      # save results
      write_data(
        x = add_tbl,
        combo = combo_name,
        run_info = project_info,
        output_type = "data",
        folder = "eda",
        suffix = "-add_season"
      )
    },
    error = function(e) {
      cli::cli_alert_warning(paste0("Seasonality scan failed for ", combo_name, ": ", e$message))
    }
  )
}

#' Run external regressor analysis on a single time series
#'
#' @param input_data data frame with Combo, Date, Target, and regressor columns (already filtered to hist_end_date)
#' @param combo_name name of the combo
#' @param date_type date type (day, week, month, quarter, year)
#' @param regressors vector of regressor column names
#' @param hist_end_date historical end date
#' @param project_info project information list
#'
#' @return nothing, writes results to disk
#' @noRd
run_xreg_analysis <- function(input_data, combo_name, date_type, regressors, hist_end_date, project_info) {
  if (is.null(regressors) || length(regressors) == 0) {
    return(invisible(NULL))
  }

  tryCatch(
    {
      # Need to read unfiltered data to check for future regressor values
      input_data_full <- read_file(
        run_info = project_info,
        path = paste0(
          "/input_data/", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", combo_name, ".", project_info$data_output
        ),
        return_type = "df"
      )

      # determine future xregs using full unfiltered data
      future_xregs_list <- get_xregs_future_values_tbl(
        data_tbl = input_data_full,
        external_regressors = regressors,
        hist_end_date = hist_end_date
      ) %>%
        dplyr::select(-Combo, -Date) %>%
        colnames()

      # finalize input data (use filtered data for analysis)
      input_data_xreg <- input_data %>%
        dplyr::arrange(Date)

      # get lags by date type
      date_type_lags <- switch(date_type,
        "day" = c(0:7, seq(14, 364, by = 7)),
        "week" = 0:52,
        "month" = 0:12,
        "quarter" = 0:4,
        "year" = 0:5
      )

      # build lagged regressors
      lag_tbl <- tidyr::crossing(
        Regressor = regressors,
        Lag = date_type_lags
      ) %>%
        dplyr::mutate(
          dCor = purrr::map2_dbl(Regressor, Lag, \(var, l) {
            x <- dplyr::lag(input_data_xreg[[var]], l)
            y <- input_data_xreg$Target
            keep <- !(is.na(x) | is.na(y))
            if (sum(keep) < 5) {
              return(NA_real_)
            }
            energy::dcor(x[keep], y[keep])
          })
        ) %>%
        dplyr::mutate(dCor = round(dCor, 2)) %>%
        dplyr::mutate(Combo = combo_name, .before = 1)

      # filter out lag 0 values if a regressor does not have future values
      if (!is.null(future_xregs_list) && length(future_xregs_list) > 0) {
        lag_tbl <- lag_tbl %>%
          dplyr::mutate(
            Has_Future = Regressor %in% future_xregs_list,
            Drop = ifelse((Lag == 0 & Has_Future == FALSE), TRUE, FALSE)
          ) %>%
          dplyr::filter(!Drop) %>%
          dplyr::select(-Has_Future, -Drop)
      }

      # write per-combo result
      write_data(
        x = lag_tbl,
        combo = combo_name,
        run_info = project_info,
        output_type = "data",
        folder = "eda",
        suffix = "-xreg_scan"
      )
    },
    error = function(e) {
      cli::cli_alert_warning(paste0("External regressor scan failed for ", combo_name, ": ", e$message))
    }
  )
}

#' Run all EDA functions per time series combo
#'
#' @param agent_info agent information list
#' @param parallel_processing whether to use parallel processing
#' @param num_cores number of cores to use for parallel processing
#'
#' @return nothing
#' @noRd
run_all_eda_per_combo <- function(agent_info,
                                  parallel_processing,
                                  num_cores) {
  # get metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  hist_end_date <- agent_info$hist_end_date
  hist_start_date <- agent_info$hist_start_date
  date_type <- project_info$date_type
  regressors <- agent_info$external_regressors

  # get time series to run
  total_combo_list <- get_total_combos(agent_info = agent_info)

  # define which EDA types are required
  eda_types <- c("acf", "pacf", "stationarity", "missing", "outliers", "add_season")
  if (!is.null(regressors) && length(regressors) > 0) {
    eda_types <- c(eda_types, "xreg_scan")
  }

  # for each EDA type, check which combos are missing
  # then take the union of all missing combos
  combos_needing_work <- c()

  for (eda_type in eda_types) {
    finished_combos <- get_finished_eda_combos(
      agent_info = agent_info,
      eda_wildcard = paste0("*-", eda_type, ".")
    )

    # find combos that don't have this EDA type
    missing_combos <- setdiff(total_combo_list, finished_combos)

    # add to the list of combos needing work
    combos_needing_work <- union(combos_needing_work, missing_combos)
  }

  # sort for consistency
  current_combo_list <- sort(combos_needing_work)

  if (length(current_combo_list) == 0) {
    cli::cli_alert_info("All EDA functions already completed for all time series")
    return(cli::cli_progress_done())
  }

  # parallel setup
  par_info <- par_start(
    run_info = project_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    task_length = length(current_combo_list)
  )

  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  # helper: map date_type to ts() frequency
  freq_map <- c(
    "day" = 7,
    "week" = 52,
    "month" = 12,
    "quarter" = 4,
    "year" = 1
  )
  freq_val <- freq_map[date_type] %||% 1

  # submit tasks - one task per time series running all EDA functions
  foreach::foreach(
    x = current_combo_list,
    .packages = packages,
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .noexport = NULL
  ) %op%
    {
      # read data once for all analyses
      input_data <- read_file(
        run_info = project_info,
        path = paste0(
          "/input_data/", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", x, ".", project_info$data_output
        ),
        return_type = "df"
      ) %>%
        dplyr::filter(Date <= hist_end_date)

      combo_name <- unique(input_data$Combo)

      # Run all EDA analyses
      run_acf_analysis(input_data, combo_name, date_type, project_info)
      run_pacf_analysis(input_data, combo_name, date_type, project_info)
      run_stationarity_analysis(input_data, combo_name, date_type, hist_start_date, hist_end_date, project_info)
      run_missing_analysis(input_data, combo_name, date_type, project_info)
      run_outlier_analysis(input_data, combo_name, date_type, freq_val, project_info)
      run_seasonality_analysis(input_data, combo_name, date_type, freq_val, project_info)
      run_xreg_analysis(input_data, combo_name, date_type, regressors, hist_end_date, project_info)
    } %>%
    base::suppressWarnings() %>%
    base::suppressPackageStartupMessages()

  # stop parallel processing
  par_end(cl)

  # check if all time series combos ran correctly for each EDA type
  for (eda_type in eda_types) {
    successful_combos <- get_finished_eda_combos(
      agent_info = agent_info,
      eda_wildcard = paste0("*-", eda_type, ".")
    )

    if (length(successful_combos) != length(total_combo_list)) {
      stop(
        paste0(
          "Not all time series completed '", eda_type, "', expected ",
          length(total_combo_list), " time series but only ", length(successful_combos),
          " completed successfully."
        )
      )
    }
  }

  # all checks passed
  return(cli::cli_progress_done())
}

#' Hierarchy detection tool
#'
#' @param agent_info agent information list
#' @param input_data optional input data frame to use instead of reading from disk
#' @param write_data whether to write the hierarchy detection results to disk
#'
#' @return summary text of hierarchy detection
#' @noRd
hierarchy_detect <- function(agent_info,
                             input_data = NULL,
                             write_data = TRUE) {
  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  combo_vars <- project_info$combo_variables

  if (length(combo_vars) == 0) {
    return("FAIL: project_info$combo_variables not set.")
  }

  # single-column panel = always "none"
  if (length(combo_vars) == 1) {
    if (write_data) {
      entry <- list(
        timestamp = get_timestamp(),
        type = "HIERARCHY",
        hist_end_date = as.character(agent_info$hist_end_date),
        hierarchy = "none",
        pair_tests = list()
      )

      write_data(
        x           = entry,
        combo       = NULL,
        run_info    = project_info,
        output_type = "object",
        folder      = "eda",
        suffix      = "-hierarchy"
      )

      cli::cli_alert_info("Only one combo column supplied, hierarchy is 'none'.")
      return("Hierarchy detection: none (no hierarchy).")
    } else {
      return("bottoms_up")
    }
  }

  if (is.null(input_data)) {
    # check if hierarchy detection has already been done
    hier_list <- tryCatch(
      read_file(project_info,
        path = paste0(
          "eda/*", hash_data(project_info$project_name), "-",
          hash_data(project_info$run_name), "-hierarchy.", project_info$object_output
        ),
        return_type = "object"
      ),
      error = function(e) {
        list()
      }
    ) %>%
      base::suppressWarnings()

    if (length(hier_list) > 0) {
      cli::cli_alert_info("Hierarchy Detection Already Ran")
      return("Hierarchy info already exists for this agent run. Skipping hierarchy step.")
    }

    # load hist data
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

    df <- read_file(
      run_info = project_info,
      file_list = input_data_list,
      return_type = "df"
    )
  } else {
    df <- input_data
  }

  if (any(!combo_vars %in% names(df))) {
    return("FAIL: combo column(s) missing in df.")
  }

  if (length(combo_vars) == 1) {
    hierarchy_type <- "none"
  } else {
    # helper: classify one ordered pair
    pair_test <- function(a, b) {
      df %>%
        dplyr::distinct(!!rlang::sym(a), !!rlang::sym(b)) %>%
        dplyr::count(!!rlang::sym(b), name = "n_parent") %>%
        dplyr::pull(n_parent) %>%
        {
          \(x) if (any(x > 1)) "many-to-many" else "one-to-many"
        }()
    }

    # build pair table
    pair_df <- expand.grid(
      from = combo_vars,
      to = combo_vars,
      stringsAsFactors = FALSE
    ) %>%
      dplyr::filter(from != to) %>%
      dplyr::mutate(
        test = purrr::map2_chr(from, to, ~ pair_test(.x, .y))
      )

    pair_tests <- rlang::set_names(
      pair_df$test,
      paste0(pair_df$from, "->", pair_df$to)
    )

    # detect hierarchy type
    is_chain <- \(ord) {
      purrr::map_lgl(
        seq_len(length(ord) - 1L),
        \(i) pair_tests[paste0(ord[i + 1L], "->", ord[i])] ==
          "one-to-many"
      ) %>% all()
    }

    chain_found <- gtools::permutations(
      length(combo_vars),
      length(combo_vars),
      combo_vars
    ) %>%
      apply(1L, is_chain) %>%
      any()

    hierarchy_type <- if (chain_found) "standard" else "grouped"
  }

  if (write_data) {
    # human-readable summary
    header <- switch(hierarchy_type,
      none     = "Hierarchy detection: flat panel (no hierarchy).",
      standard = "Hierarchy detection: STANDARD tree structure.",
      grouped  = "Hierarchy detection: GROUPED / crossed hierarchy."
    )

    details <- pair_tests %>%
      purrr::imap_chr(\(v, k) sprintf("%s: %s", k, v)) %>%
      paste(collapse = ", ")

    summary_text <- paste(header, details, sep = " ")

    # log results
    entry <- list(
      timestamp  = get_timestamp(),
      type       = "HIERARCHY",
      hierarchy  = hierarchy_type,
      pair_tests = pair_tests
    )

    write_data(
      x           = entry,
      combo       = NULL,
      run_info    = project_info,
      output_type = "object",
      folder      = "eda",
      suffix      = "-hierarchy"
    )

    return(summary_text)
  } else {
    # return hierarchy type
    final_type <- switch(hierarchy_type,
      none     = "bottoms_up",
      standard = "standard_hierarchy",
      grouped  = "grouped_hierarchy"
    )

    return(final_type)
  }
}

#' External regressor scan tool
#'
#' @param agent_info agent information list
#' @param parallel_processing whether to use parallel processing
#' @param num_cores number of cores to use for parallel processing
#'
#' @return nothing
#' @noRd
xreg_scan <- function(agent_info,
                      parallel_processing,
                      num_cores) {
  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  combo_vars <- project_info$combo_variables
  regressors <- agent_info$external_regressors
  hist_end_date <- agent_info$hist_end_date
  date_type <- project_info$date_type

  if (length(regressors) == 0) {
    cli::cli_alert_info("No external regressors set for this agent run. Skipping 'reg_scan'.")
    return("SKIPPING: no xregs in data")
  }

  # identify time-series combos
  total_combo_list <- get_total_combos(agent_info = agent_info)

  # detect previously completed combos
  prev_combo_list <- get_finished_eda_combos(
    agent_info   = agent_info,
    eda_wildcard = "*-xreg_scan."
  )

  current_combo_list <- setdiff(total_combo_list, prev_combo_list)

  if (length(current_combo_list) == 0 & length(prev_combo_list) > 0) {
    cli::cli_alert_info("External Regressor Scan Already Ran")
    return("SKIPPING: xregs_scan already ran")
  }

  # parallel setup

  par_info <- par_start(
    run_info            = project_info,
    parallel_processing = parallel_processing,
    num_cores           = num_cores,
    task_length         = length(current_combo_list)
  )

  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  # foreach over each combo file
  foreach::foreach(
    x               = current_combo_list,
    .packages       = packages,
    .errorhandling  = "stop",
    .inorder        = FALSE,
    .multicombine   = TRUE
  ) %op%
    {
      # read one combo
      input_data <- read_file(
        run_info = project_info,
        path = paste0(
          "/input_data/", hash_data(project_info$project_name), "-",
          hash_data(agent_info$run_id), "-", x, ".", project_info$data_output
        ),
        return_type = "df"
      )

      combo_name <- unique(input_data$Combo)

      # determine future xregs
      if (!is.null(regressors)) {
        future_xregs_list <- get_xregs_future_values_tbl(
          data_tbl = input_data,
          external_regressors = regressors,
          hist_end_date = hist_end_date
        ) %>%
          dplyr::select(-Combo, -Date) %>%
          colnames()
      }

      # finalize input data
      input_data <- input_data %>%
        dplyr::filter(Date <= hist_end_date) %>%
        dplyr::arrange(Date)

      # get lags by date type
      date_type_lags <- switch(date_type,
        "day"     = c(0:7, seq(14, 364, by = 7)), # daily: 0-7, then weekly steps up to 364
        "week"    = 0:52, # weekly data lags
        "month"   = 0:12, # monthly data lags
        "quarter" = 0:4, # quarterly data lags
        "year"    = 0:5 # yearly data lags
      )

      # build lagged regressors
      lag_tbl <- tidyr::crossing(
        Regressor = regressors,
        Lag       = date_type_lags
      ) %>%
        dplyr::mutate(
          dCor = purrr::map2_dbl(Regressor, Lag, \(var, l) {
            x <- dplyr::lag(input_data[[var]], l)
            y <- input_data$Target
            keep <- !(is.na(x) | is.na(y))
            if (sum(keep) < 5) {
              return(NA_real_)
            }
            energy::dcor(x[keep], y[keep])
          })
        ) %>%
        dplyr::mutate(dCor = round(dCor, 2)) %>%
        dplyr::mutate(Combo = combo_name, .before = 1)

      # filter out lag 0 values if a regressor does not have future values
      if (!is.null(future_xregs_list)) {
        lag_tbl <- lag_tbl %>%
          # create flag column
          dplyr::mutate(
            Has_Future = Regressor %in% future_xregs_list,
            Drop = ifelse((Lag == 0 & Has_Future == FALSE), TRUE, FALSE)
          ) %>%
          dplyr::filter(!Drop) %>%
          dplyr::select(-Has_Future, -Drop)
      }

      # write per-combo result
      write_data(
        x = lag_tbl,
        combo = combo_name,
        run_info = project_info,
        output_type = "data",
        folder = "eda",
        suffix = "-xreg_scan"
      )
    } %>% base::suppressPackageStartupMessages()

  par_end(cl)

  # sanity check
  successful_combos <- get_finished_eda_combos(
    agent_info   = agent_info,
    eda_wildcard = "*-xreg_scan."
  )

  if (length(successful_combos) != length(total_combo_list)) {
    stop(
      paste0(
        "Not all time series were ran within 'xreg_scan', expected ",
        length(total_combo_list), " time series but only ", length(successful_combos),
        " time series were ran. ", "Please run 'xreg_scan' again."
      ),
      call. = FALSE
    )
  }
}

#' Helper function to create a pipe table from a data frame
#'
#' @param df data frame to convert to pipe table
#'
#' @return string representation of the data frame in pipe table format
#' @noRd
make_pipe_table <- function(df) {
  knitr::kable(df, format = "pipe") %>% paste(collapse = "\n")
}


#' Get total time series combos
#'
#' @param agent_info agent information list
#'
#' @return list of total time series combos
#' @noRd
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
    sort() %>%
    suppressWarnings()

  return(total_combo_list)
}

#' Get finished time series combos for EDA
#' @param agent_info agent information list
#' @param eda_wildcard wildcard for EDA files
#'
#' @return list of finished time series combos
#' @noRd
get_finished_eda_combos <- function(agent_info,
                                    eda_wildcard) {
  # metadata
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  # list of previous combos already ran
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
