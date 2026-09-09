artifact_test_run <- function(path, data_output = "csv", object_output = "rds") {
  list(
    project_name = paste0("artifact_io_", basename(tempfile())),
    run_name = "artifact_run",
    storage_object = NULL,
    path = path,
    data_output = data_output,
    object_output = object_output
  )
}

artifact_test_path <- function(run_info, folder, combo = NULL, suffix = "",
                               extension = run_info$data_output) {
  root <- if (is.null(run_info$path)) tempdir() else run_info$path
  combo_suffix <- if (is.null(combo)) "" else paste0("-", hash_data(combo))
  fs::path(root, folder, paste0(
    hash_data(run_info$project_name), "-", hash_data(run_info$run_name),
    combo_suffix, suffix, ".", extension
  ))
}

artifact_test_log <- function(run_info, recipes = "R1", date_type = "month", ...) {
  log <- tibble::tibble(
    recipes_to_run = if (is.null(recipes)) NA_character_ else paste(recipes, collapse = "---"),
    date_type = date_type,
    ...
  )
  writer_info <- run_info
  if (is.null(writer_info$path)) writer_info$path <- tempdir()
  write_data(log, NULL, writer_info, "log", "logs")
  log
}

local_artifact_spies <- function(max_listings = 0L, .env = parent.frame()) {
  tracker <- new.env(parent = emptyenv())
  tracker$listings <- 0L
  tracker$directory_listings <- 0L
  tracker$directory_calls <- list()
  tracker$metadata_paths <- character()
  tracker$access_paths <- character()
  tracker$payload_paths <- character()
  tracker$reads <- list()
  original_list <- list_files
  original_read <- read_file
  original_info <- fs::file_info
  original_access <- file.access
  original_dir_ls <- fs::dir_ls
  original_list_files <- list.files
  original_dir <- dir
  original_list_dirs <- list.dirs
  original_rds <- readRDS
  original_csv <- utils::read.csv
  original_vroom <- vroom::vroom
  directory_depth <- 0L
  artifact_paths <- function(paths) {
    paths <- gsub("\\", "/", as.character(paths), fixed = TRUE)
    paths[grepl("(^|/)(prep_data|prep_models|models|logs|forecasts|input_data|eda|final_output)(/|$)", paths)]
  }
  record_directory <- function(path, api, read) {
    paths <- artifact_paths(path)
    if (directory_depth == 0L && length(paths) > 0L) {
      tracker$directory_listings <- tracker$directory_listings + 1L
      tracker$directory_calls[[length(tracker$directory_calls) + 1L]] <- list(api = api, paths = paths)
      if (tracker$directory_listings > max_listings) {
        stop("Unexpected low-level directory enumeration in artifact read", call. = FALSE)
      }
    }
    directory_depth <<- directory_depth + 1L
    on.exit(directory_depth <<- directory_depth - 1L)
    force(read)
  }
  testthat::local_mocked_bindings(
    list_files = function(storage_object, path, ...) {
      if (grepl("*", path, fixed = TRUE)) {
        tracker$listings <- tracker$listings + 1L
        if (tracker$listings > max_listings) {
          stop("Unexpected directory enumeration in artifact read", call. = FALSE)
        }
      }
      original_list(storage_object, path, ...)
    },
    read_file = function(run_info, path = NULL, file_list = NULL, ...) {
      tracker$reads[[length(tracker$reads) + 1L]] <- list(
        path = path, file_list = file_list
      )
      original_read(run_info, path = path, file_list = file_list, ...)
    },
    download_file = function(...) {
      stop("Unexpected download in local artifact read", call. = FALSE)
    },
    .package = "finnts",
    .env = .env
  )
  testthat::local_mocked_bindings(
    file_info = function(path, ...) {
      tracker$metadata_paths <- c(tracker$metadata_paths, artifact_paths(path))
      original_info(path, ...)
    },
    dir_ls = function(path = ".", ...) record_directory(path, "fs::dir_ls", original_dir_ls(path, ...)),
    .package = "fs", .env = .env
  )
  testthat::local_mocked_bindings(
    file.access = function(names, mode = 0) {
      tracker$access_paths <- c(tracker$access_paths, artifact_paths(names))
      original_access(names, mode)
    },
    readRDS = function(file, ...) {
      if (is.character(file)) tracker$payload_paths <- c(tracker$payload_paths, artifact_paths(file))
      original_rds(file, ...)
    },
    list.files = function(path = ".", ...) record_directory(path, "base::list.files", original_list_files(path, ...)),
    dir = function(path = ".", ...) record_directory(path, "base::dir", original_dir(path, ...)),
    list.dirs = function(path = ".", ...) record_directory(path, "base::list.dirs", original_list_dirs(path, ...)),
    .package = "base", .env = .env
  )
  testthat::local_mocked_bindings(
    read.csv = function(file, ...) {
      if (is.character(file)) tracker$payload_paths <- c(tracker$payload_paths, artifact_paths(file))
      original_csv(file, ...)
    },
    .package = "utils", .env = .env
  )
  testthat::local_mocked_bindings(
    vroom = function(file, ...) {
      if (is.character(file)) tracker$payload_paths <- c(tracker$payload_paths, artifact_paths(file))
      original_vroom(file, ...)
    },
    .package = "vroom", .env = .env
  )
  if (requireNamespace("arrow", quietly = TRUE)) {
    original_parquet <- arrow::read_parquet
    testthat::local_mocked_bindings(
      read_parquet = function(file, ...) {
        if (is.character(file)) tracker$payload_paths <- c(tracker$payload_paths, artifact_paths(file))
        original_parquet(file, ...)
      },
      .package = "arrow", .env = .env
    )
  }
  tracker
}

artifact_test_recipe <- function(run_info, combo, recipe, value = 1) {
  data <- tibble::tibble(
    Combo = combo,
    Date = as.Date("2024-01-01") + 0:2,
    Target = value + 0:2
  )
  writer_info <- run_info
  if (is.null(writer_info$path)) writer_info$path <- tempdir()
  write_data(data, combo, writer_info, "data", "prep_data", paste0("-", recipe))
  data
}

artifact_test_agent <- function(path, data_output = "csv") {
  project_info <- artifact_test_run(path, data_output)
  project_info$combo_variables <- "ID"
  project_info$target_variable <- "Target"
  project_info$date_type <- "month"
  list(
    project_info = project_info,
    run_id = project_info$run_name,
    hist_start_date = as.Date("2020-01-01"),
    hist_end_date = as.Date("2024-01-01"),
    external_regressors = NULL,
    forecast_horizon = 2,
    back_test_scenarios = 1,
    back_test_spacing = 1
  )
}

artifact_test_eda <- function(agent_info, combos = c("A", "B"), omit = character()) {
  run_info <- agent_info$project_info
  row_counts <- 4 + 2 * seq_along(combos)
  write_data(list(
    total_rows = sum(row_counts), n_series = length(combos), rows_min = min(row_counts), rows_max = max(row_counts),
    rows_avg = mean(row_counts), neg_count = 0, neg_pct = 0,
    date_start = "2020-01-01", date_end = "2024-01-01"
  ), NULL, run_info, "object", "eda", "-data_profile")
  write_data(list(hierarchy = "none"), NULL, run_info, "object", "eda", "-hierarchy")
  for (combo in combos) {
    offset <- match(combo, combos) - 1L
    row_count <- row_counts[[offset + 1L]]
    values <- c(acf = 0.5 - 0.8 * offset, pacf = 0.4 + 0.4 * offset, add_season = 0.2 + 0.4 * offset)
    for (suffix in setdiff(c("acf", "pacf", "add_season"), omit)) {
      write_data(tibble::tibble(Combo = combo, Lag = 1, Value = unname(values[[suffix]])),
        combo, run_info, "data", "eda", paste0("-", suffix)
      )
    }
    scans <- list(
      stationarity = tibble::tibble(Combo = combo, stationary_adf = offset == 0L, stationary_kpss = TRUE),
      missing = tibble::tibble(Combo = combo, total_rows = row_count, missing_count = 2 * offset,
        missing_pct = 100 * 2 * offset / row_count, longest_gap = 2 * offset),
      outliers = tibble::tibble(Combo = combo, total_rows = row_count, outlier_count = 2 * offset,
        outlier_pct = 100 * 2 * offset / row_count,
        first_outlier_dt = if (offset == 0L) as.Date(NA) else as.Date("2020-03-01"),
        last_outlier_dt = if (offset == 0L) as.Date(NA) else as.Date("2020-06-01")),
      xreg_scan = tibble::tibble(Combo = combo, Regressor = "price", Lag = 1, dCor = 0.25 + 0.5 * offset)
    )
    for (suffix in setdiff(names(scans), omit)) {
      write_data(scans[[suffix]], combo, run_info, "data", "eda", paste0("-", suffix))
    }
    write_data(tibble::tibble(Combo = combo, Date = as.Date("2024-01-01"), Target = 1),
      combo, run_info, "data", "input_data"
    )
  }
  invisible(agent_info)
}

artifact_expected_eda <- function() {
  profile <- tibble::tibble(
    Combo = "All", Analysis_Type = "Data_Profile",
    Metric = c("Total_Rows", "Number_Series", "Min_Rows_Per_Series", "Max_Rows_Per_Series",
      "Avg_Rows_Per_Series", "Negative_Count", "Negative_Percent", "Start_Date", "End_Date"),
    Value = c("14", "2", "6", "8", "7", "0", "0", "2020-01-01", "2024-01-01")
  )
  series <- tibble::tibble(
    Combo = rep(c("A", "B"), each = 14),
    Analysis_Type = rep(c("ACF", "PACF", "Stationarity", rep("Missing_Data", 4),
      rep("Outliers", 5), "Additional_Seasonality", "External_Regressor_Distance_Correlation"), 2),
    Metric = rep(c("Lag_1", "Lag_1", "is_stationary", "total_rows", "missing_count", "missing_pct",
      "longest_gap", "total_rows", "outlier_count", "outlier_pct", "first_outlier_dt", "last_outlier_dt",
      "Lag_1", "price_Lag_1"), 2),
    Value = c("0.5", "0.4", "TRUE", "6", "0", "0", "0", "6", "0", "0", NA, NA, "0.2", "0.25",
      "-0.3", "0.8", "FALSE", "8", "2", "25", "2", "8", "2", "25", "2020-03-01", "2020-06-01", "0.6", "0.75")
  )
  hierarchy <- tibble::tibble(Combo = "All", Analysis_Type = "Hierarchy", Metric = "hierarchy_type", Value = "none")
  dplyr::bind_rows(profile, series, hierarchy) %>% dplyr::arrange(Combo, Analysis_Type, Metric)
}

artifact_test_forecast <- function(run_info, combo = "A", suffix = "single_models",
                                   model = "meanf", best = "Yes", write_output = TRUE) {
  data <- tibble::tibble(
    Combo = combo, Combo_ID = combo, Hyperparameter_ID = 1,
    Model_ID = paste0(model, "--local--R1"), Model_Name = model,
    Model_Type = "local", Recipe_ID = "R1", Train_Test_ID = 1,
    Date = as.Date("2024-02-01"), Forecast = 100, Target = NA_real_,
    Best_Model = best
  )
  if (write_output) {
    write_data(data, combo, run_info, "data", "forecasts", paste0("-", suffix))
  }
  data
}

artifact_test_splits <- function(run_info) {
  splits <- tibble::tibble(Run_Type = "Future_Forecast", Train_Test_ID = 1)
  write_data(splits, NULL, run_info, "data", "prep_models", "-train_test_split")
  splits
}

artifact_test_model_log <- function(run_info, forecast_approach = "bottoms_up") {
  artifact_test_log(run_info, combo_variables = "ID", forecast_approach = forecast_approach,
    num_hyperparameters = 1, negative_forecast = FALSE, run_global_models = FALSE,
    run_local_models = TRUE, run_ensemble_models = TRUE,
    models_to_run = NA_character_, models_not_to_run = NA_character_
  )
}

artifact_test_selected_models <- function(agent_info) {
  best_runs <- tibble::tibble(combo = c("A", "B"), best_run_name = c("selected_A", "selected_B"),
    model_type = "local"
  )
  for (index in seq_len(nrow(best_runs))) {
    selected <- agent_info$project_info
    combo <- best_runs$combo[[index]]
    selected$project_name <- paste0(selected$project_name, "_", hash_data(combo))
    selected$run_name <- best_runs$best_run_name[[index]]
    write_data(tibble::tibble(
      Model_ID = "meanf--local--R1", Model_Name = "meanf", Model_Type = "local",
      Model_Fit = list(list(value = 100))
    ), combo, selected, "object", "models", "-single_models")
    artifact_test_forecast(selected, combo)
  }
  best_runs
}

local_artifact_summary_mocks <- function(best_runs, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_agent_info = function(...) invisible(NULL),
    vip_available = function(...) TRUE,
    get_best_agent_run = function(...) best_runs,
    summarize_model_meanf = function(...) tibble::tibble(section = "model", name = "mean", value = "100"),
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    par_end = function(...) invisible(NULL),
    .package = "finnts", .env = .env
  )
}