#' Check whether vip is available
#'
#' @return A logical value indicating whether vip can be loaded.
#' @noRd
vip_available <- function() {
  requireNamespace("vip", quietly = TRUE) &&
    utils::packageVersion("vip") >= "0.5.0"
}

#' Calculate variable importance when vip is available
#'
#' @param ... Arguments passed to [vip::vi()].
#'
#' @return The variable importance result, or NULL when vip is unavailable.
#' @noRd
vip_vi <- function(...) {
  if (!vip_available()) {
    return(NULL)
  }

  vip::vi(...)
}

#' Require vip for an operation
#'
#' @param context Description of the operation that requires vip.
#'
#' @return Invisibly returns TRUE when vip is available.
#' @noRd
require_vip <- function(context) {
  if (vip_available()) {
    return(invisible(TRUE))
  }

  stop(
    paste0(
      "Package 'vip' 0.5.0 or newer is required for ", context,
      " but is not available. ",
      "Install vip from its r-universe repository with ",
      "install.packages(\"vip\", repos = c(",
      "\"https://bgreenwell.r-universe.dev\", ",
      "\"https://cloud.r-project.org\")). ",
      "vip 0.5.0 requires R 4.1 or newer."
    ),
    call. = FALSE
  )
}