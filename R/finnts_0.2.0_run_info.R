#' Set up finnts submission
#' 
#' @param experiment_name
#' @param run_name
#' @param storage_object
#' @param path
#' @param data_output
#' @param object_output
#'  
#' @return A list of run information
#' @keywords internal
#' @export
#' @examples
#' \donttest{
#' run_info <- set_run_info(experiment_name = "finn forecast", 
#'                          run_name = "test_run", 
#'                          path = "test")
#' }
set_run_info <- function(experiment_name = 'finn_fcst', 
                         run_name = 'finn_fcst', 
                         storage_object = NULL, 
                         path = NULL, 
                         data_output = 'csv', 
                         object_output = 'rds') {
  
  if(!inherits(run_name, c("NULL", "character"))) {
    stop("`run_name` must either be a NULL or a string")
  }
  
  if(!inherits(storage_object, c("blob_container", "ms_drive", "NULL"))) {
    stop("`storage_object` must either be a NULL or a Azure Blob Storage, OneDrive, or SharePoint document library object")
  }
  
  if(!inherits(path, c("NULL", "character"))) {
    stop("`path` must either be a NULL or a string")
  }
  
  if(inherits(storage_object, c("blob_container", "ms_drive")) & is.null(path)) {
    path <- ''
  }
  
  run_name <- paste0(run_name, '-', format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  
  created <- as.POSIXct(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), format="%Y%m%dT%H%M%SZ", tz="UTC")
  
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
  
  write_data(x = output_tbl, 
             combo = NULL, 
             run_info = output_list, 
             output_type = "log",
             folder = "logs", 
             suffix = NULL)
  
  return(output_list)
  
}