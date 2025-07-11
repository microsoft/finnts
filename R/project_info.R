#' Set up new finnts forecast project
#'
#' Creates list object of information helpful in logging information
#'   about your entire forecast project.
#'
#' @param project_name Name used to group similar runs under a
#'   single project name.
#' @param storage_object Used to store outputs during the project to other
#'   storage services in Azure. Could be a storage container object from
#'   the 'AzureStor' package to connect to ADLS blob storage or a
#'   OneDrive/SharePoint object from the 'Microsoft365R' package to connect
#'   to a OneDrive folder or SharePoint site. Default of NULL will save outputs
#'   to the local file system.
#' @param path String showing what file path the outputs should be written to.
#'   Default of NULL will write the outputs to a temporary directory within R,
#'   which will delete itself after the R session closes.
#' @param data_output String value describing the file type for data outputs.
#'   Default will write data frame outputs as csv files. The other option
#'   of 'parquet' will instead write parquet files.
#' @param object_output String value describing the file type for object
#'   outputs. Default will write object outputs like trained models as
#'   rds files. The other option of 'qs' will instead serialize R objects
#'   as qs files by using the 'qs' package.
#' @param combo_variables Character vector of variables to combine into a
#'   combo variable.
#' @param target_variable Character string of the target variable to
#'   forecast.
#' @param date_type Character string of the type of date variable
#' @param fiscal_year_start Numeric value of the month that the fiscal year
#'   starts in.
#' @param overwrite Logical value of whether to overwrite existing project
#'
#' @return A list of project information
#' @examples
#' \donttest{
#' run_info <- set_project_info(
#'   project_name = "test_exp", 
#'   combo_variables = c("Store", "Product"),
#'   target_variable = "Sales",
#'   date_type = "month"
#' )
#' }
#' @export
set_project_info <- function(project_name = "finn_project",
                             path = NULL,
                             combo_variables,
                             target_variable,
                             date_type,
                             fiscal_year_start = 1,
                             storage_object = NULL,
                             data_output = "csv",
                             object_output = "rds",
                             overwrite = FALSE) {
  # initial input checks
  if (!inherits(project_name, c("NULL", "character"))) {
    stop("`project_name` must either be a NULL or a string")
  }

  check_input_type("combo_variables", combo_variables, "character")
  check_input_type("target_variable", target_variable, "character")
  check_input_type("date_type", date_type, "character", c("year", "quarter", "month", "week", "day"))
  check_input_type("fiscal_year_start", fiscal_year_start, "numeric")

  if (!inherits(storage_object, c("blob_container", "ms_drive", "NULL"))) {
    stop("`storage_object` must either be a NULL or a Azure Blob Storage,
          OneDrive, or SharePoint document library object")
  }

  if (!inherits(path, c("NULL", "character"))) {
    stop("`path` must either be a NULL or a string")
  }

  if (inherits(storage_object, c("blob_container", "ms_drive")) &
    is.null(path)) {
    path <- ""
  }

  # create dir paths
  eda_folder <- "eda"
  input_data_folder <- "input_data"
  logs_folder <- "logs"

  if (is.null(path)) {
    path <- fs::path(tempdir())

    fs::dir_create(tempdir(), eda_folder)
    fs::dir_create(tempdir(), input_data_folder)
    fs::dir_create(tempdir(), logs_folder)
  } else if (is.null(storage_object) & substr(path, 1, 6) == "/synfs") {
    temp_path <- stringr::str_replace(path, "/synfs/", "synfs:/")

    if (!dir.exists(fs::path(temp_path, eda_folder) %>% as.character())) {
      notebookutils::mssparkutils.fs.mkdirs(fs::path(temp_path, eda_folder) %>% as.character())
    }

    if (!dir.exists(fs::path(temp_path, input_data_folder) %>% as.character())) {
      notebookutils::mssparkutils.fs.mkdirs(fs::path(temp_path, input_data_folder) %>% as.character())
    }

    if (!dir.exists(fs::path(path, logs_folder) %>% as.character())) {
      notebookutils::mssparkutils.fs.mkdirs(fs::path(temp_path, logs_folder) %>% as.character())
    }
  } else if (is.null(storage_object)) {
    fs::dir_create(path, eda_folder)
    fs::dir_create(path, input_data_folder)
    fs::dir_create(path, logs_folder)
  } else if (inherits(storage_object, "blob_container")) {
    AzureStor::create_storage_dir(storage_object, fs::path(path, eda_folder))
    AzureStor::create_storage_dir(storage_object, fs::path(path, input_data_folder))
    AzureStor::create_storage_dir(storage_object, fs::path(path, logs_folder))
  } else if (inherits(storage_object, "ms_drive")) {
    try(storage_object$create_folder(fs::path(path, eda_folder)), silent = TRUE)
    try(storage_object$create_folder(fs::path(path, input_data_folder)), silent = TRUE)
    try(storage_object$create_folder(fs::path(path, logs_folder)), silent = TRUE)
  }

  temp_project_info <- list(
    project_name = project_name,
    storage_object = storage_object,
    path = path,
    data_output = data_output,
    object_output = object_output,
    combo_variables,
    target_variable,
    date_type,
    fiscal_year_start
  )

  log_df <- tryCatch(
    read_file(temp_project_info,
      path = paste0("logs/", hash_data(project_name), "-project", ".csv"),
      return_type = "df"
    ),
    error = function(e) {
      tibble::tibble()
    }
  ) %>%
    base::suppressWarnings()

  if (nrow(log_df) > 0 & overwrite == FALSE) {
    # check if input values have changed
    current_log_df <- tibble::tibble(
      project_name = project_name,
      path = gsub("synfs(/notebook)?/\\d+", "synfs", path), # remove synapse id to prevent issues
      data_output = data_output,
      object_output = object_output,
      combo_variables = paste(combo_variables, collapse = ", "),
      target_variable = target_variable,
      date_type = date_type,
      fiscal_year_start = fiscal_year_start
    ) %>%
      data.frame()

    prev_log_df <- log_df %>%
      dplyr::select(colnames(current_log_df)) %>%
      dplyr::mutate(path = gsub("synfs(/notebook)?/\\d+", "synfs", path)) %>% # remove synapse id to prevent issues
      data.frame()

    if (hash_data(current_log_df) != hash_data(prev_log_df)) {
      stop("Inputs have recently changed in 'set_project_info',
           please revert back to original inputs or overwrite existing
           project info with 'overwrite' argument set to TRUE.",
        call. = FALSE
      )
    }

    output_list <- list(
      project_name = project_name,
      created = log_df$created,
      storage_object = storage_object,
      path = path,
      data_output = data_output,
      object_output = object_output,
      combo_variables = combo_variables,
      target_variable = target_variable,
      date_type = date_type,
      fiscal_year_start = fiscal_year_start
    )

    cli::cli_bullets(c(
      "Using Existing Finn Project",
      "*" = paste0("Project Name: ", project_name),
      ""
    ))

    return(output_list)
  } else {
    created <- get_timestamp()

    output_list <- list(
      project_name = project_name,
      created = created,
      storage_object = storage_object,
      path = path,
      data_output = data_output,
      object_output = object_output,
      combo_variables = combo_variables,
      target_variable = target_variable,
      date_type = date_type,
      fiscal_year_start = fiscal_year_start
    )

    output_tbl <- tibble::tibble(
      project_name = project_name,
      created = created,
      path = path,
      data_output = data_output,
      object_output = object_output,
      combo_variables = paste(combo_variables, collapse = ", "),
      target_variable = target_variable,
      date_type = date_type,
      fiscal_year_start = fiscal_year_start
    )

    write_data(
      x = output_tbl,
      combo = NULL,
      run_info = output_list,
      output_type = "log",
      folder = "logs",
      suffix = "project"
    )

    cli::cli_bullets(c(
      "Created New Finn Project",
      "*" = paste0("Project Name: ", project_name),
      ""
    ))

    return(output_list)
  }
}

#' Get project info
#'
#' Lets you get all of the logging associated with a specific project or run.
#'
#' @param project_name Name used to group similar runs under a
#'   single project name.
#' @param storage_object Used to store outputs during a run to other
#'   storage services in Azure. Could be a storage container object from
#'   the 'AzureStor' package to connect to ADLS blob storage or a
#'   OneDrive/SharePoint object from the 'Microsoft365R' package to connect
#'   to a OneDrive folder or SharePoint site. Default of NULL will save outputs
#'   to the local file system.
#' @param path String showing what file path the outputs should be written to.
#'   Default of NULL will write the outputs to a temporary directory within R,
#'   which will delete itself after the R session closes.
#'
#' @return Data frame of run log information
#' @examples
#' \donttest{
#' project_info <- set_project_info(
#'   project_name = "finn_forecast",
#'   combo_variables = c("Store", "Product"),
#'   target_variable = "Sales",
#'   date_type = "month"
#' )
#'
#' project_info_tbl <- get_project_info(
#'   project_name = "finn_forecast"
#' )
#' }
#' @export
get_project_info <- function(project_name = NULL,
                             storage_object = NULL,
                             path = NULL) {
  # input checks
  if (!inherits(project_name, c("NULL", "character"))) {
    stop("`project_name` must either be a NULL or a string")
  }

  if (!inherits(storage_object, c("blob_container", "ms_drive", "NULL"))) {
    stop("`storage_object` must either be a NULL or a Azure Blob Storage, OneDrive, or SharePoint document library object")
  }

  if (!inherits(path, c("NULL", "character"))) {
    stop("`path` must either be a NULL or a string")
  }

  if (inherits(storage_object, c("blob_container", "ms_drive")) & is.null(path)) {
    path <- ""
  }

  # run info formatting
  if (is.null(project_name)) {
    project_name_final <- "*"
  } else {
    project_name_final <- hash_data(project_name)
  }

  run_name_final <- "*"

  info_list <- list(
    storage_object = storage_object,
    path = path
  )

  # read run metadata
  file_path <- paste0(
    "/logs/*", project_name_final, "-",
    run_name_final, ".csv"
  )

  project_tbl <- read_file(info_list,
    path = file_path,
    return_type = "df"
  )

  return(project_tbl)
}
