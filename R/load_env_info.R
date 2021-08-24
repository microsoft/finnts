#' Load Environment Info
#' 
#' Checks if reticulate environment is needed
#' 
#' @param reticulate_environment Input python environment
#'   ex. to connect to gluonts
load_env_info <- function(reticulate_environment)
{
  if(!is.null(reticulate_environment)) {
    Sys.setenv(GLUONTS_PYTHON = reticulate_environment) #connect to gluonts python environment via reticulate
  }
}