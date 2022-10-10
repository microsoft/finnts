#' Set up finnts submission
#'
#' Creates list object of information helpful in logging information
#'   about your run.
#'
#' @param experiment_name Name used to group similar runs under a
#'   single experiment name.
#' @param run_name Name to distinguish one run of Finn from another.
#'   The current time in UTC is appended to the run name to ensure
#'   a unique run name is created.
#' @param storage_object Used to store outputs during a run to other
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
#'
#' @return A list of run information
#' @examples
#' \donttest{
#' run_info <- set_run_info(
#'   experiment_name = "test_exp",
#'   run_name = "test_run_1"
#' )
#' }
#' @export
set_run_info <- function(experiment_name = "finn_fcst",
                         run_name = "finn_fcst",
                         storage_object = NULL,
                         path = NULL,
                         data_output = "csv",
                         object_output = "rds") {
  if (!inherits(run_name, c("NULL", "character"))) {
    stop("`run_name` must either be a NULL or a string")
  }

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

  run_name <- paste0(
    run_name, "-",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  )

  created <- as.POSIXct(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
    format = "%Y%m%dT%H%M%SZ", tz = "UTC"
  )
  
  if(is.null(path)) {
    path <- fs::path(tempdir())
    
    fs::dir_create(tempdir(), "prep_data")
    fs::dir_create(tempdir(), "prep_models")
    fs::dir_create(tempdir(), "models")
    fs::dir_create(tempdir(), "forecasts")
    fs::dir_create(tempdir(), "logs")
  }

  output_list <- list(
    experiment_name = experiment_name,
    run_name = run_name,
    created = created,
    storage_object = storage_object,
    path = path,
    data_output = data_output,
    object_output = object_output
  )

  output_tbl <- tibble::tibble(
    experiment_name = experiment_name,
    run_name = run_name,
    created = created,
    path = path,
    data_output = data_output,
    object_output = object_output
  )

  write_data(
    x = output_tbl,
    combo = NULL,
    run_info = output_list,
    output_type = "log",
    folder = "logs",
    suffix = NULL
  )

  return(output_list)
}

#' Get run info
#'
#' Lets you get all of the logging associated with a specific experiment or run.
#'
#' @param experiment_name Name used to group similar runs under a
#'   single experiment name.
#' @param run_name Name to distinguish one run of Finn from another.
#'   The current time in UTC is appended to the run name to ensure
#'   a unique run name is created.
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
#' run_info <- set_run_info(
#'   experiment_name = "finn_forecast",
#'   run_name = "test_run",
#'   path = NULL
#' )
#'
#' run_info_tbl <- get_run_info(
#'   experiment_name = "finn_forecast",
#'   run_name = "test_run"
#' )
#' }
#' @export
get_run_info <- function(experiment_name = "finn_fcst",
                         run_name = NULL,
                         storage_object = NULL,
                         path = NULL) {
  if (!inherits(run_name, c("NULL", "character"))) {
    stop("`run_name` must either be a NULL or a string")
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

  if (is.null(run_name)) {
    run_name <- "*"
  } else {
    run_name <- hash_data(run_name)
  }

  info_list <- list(
    storage_object = storage_object,
    path = path
  )

  file_path <- paste0(
    "/logs/*", hash_data(experiment_name), "-",
    run_name, ".*"
  )

  run_tbl <- read_file(info_list,
    path = file_path,
    return_type = "df"
  )
}
