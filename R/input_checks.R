#' Check input values
#'
#' @param input_name input name
#' @param input_value input value
#' @param type type
#' @param expected_value expected value
#'
#' @return nothing
#' @noRd
check_input_type <- function(input_name,
                             input_value,
                             type,
                             expected_value = NULL) {
  if (!inherits(input_value, type)) {
    stop(
      paste0(
        "invalid type for input name '", input_name, "', needs to be of type ",
        glue::glue_collapse(type, " or ")
      ),
      call. = FALSE
    )
  }

  if (!is.null(expected_value) & !is.null(input_value)) {
    if (!sum(input_value %in% expected_value)) {
      stop(
        paste0(
          "invalid value for input name '", input_name, "', value needs to equal ",
          glue::glue_collapse(expected_value, " or ")
        ),
        call. = FALSE
      )
    }
  }
}

#' Check input data
#'
#' @param input_data input name
#' @param combo_variables combo variables
#' @param target_variable target variable
#' @param external_regressors external regressors
#' @param date_type date type
#' @param fiscal_year_start fiscal year start
#' @param parallel_processing parallel processing
#'
#' @return nothing
#' @noRd
check_input_data <- function(input_data,
                             combo_variables,
                             target_variable,
                             external_regressors,
                             date_type,
                             fiscal_year_start,
                             parallel_processing) {
  # data combo names match the input data
  if (sum(combo_variables %in% colnames(input_data)) != length(combo_variables)) {
    stop("combo variables do not match column headers in input data")
  }

  # target variable name matches the input data
  if (!(target_variable %in% colnames(input_data))) {
    stop("target variable does not match a column header in input data")
  }

  # target variable is numeric
  if (!input_data %>%
    dplyr::rename(Target = target_variable) %>%
    dplyr::pull(Target) %>%
    is.numeric()) {
    stop("Target variable in input data needs to be numeric")
  }

  # external regressors match the input data
  if (!is.null(external_regressors) & sum(external_regressors %in% colnames(input_data)) != length(external_regressors)) {
    stop("external regressors do not match column headers in input data")
  }

  # date column is labeled as "Date"
  if (!("Date" %in% colnames(input_data))) {
    stop("date column in input data needs to be named as 'Date'")
  }

  # date column is formatted as a date
  if (!input_data %>%
    dplyr::select(Date) %>%
    utils::head() %>%
    dplyr::collect() %>%
    dplyr::pull(Date) %>%
    lubridate::is.Date()) {
    stop("date column in input data needs to be formatted as a date value")
  }

  # ensure month, quarter, year data repeats on the same day of each period
  if ((date_type != "day" & date_type != "week") & length(unique(format(input_data$Date, format = "%d"))) != 1) {
    stop("historical date values are not evenly spaced")
  }

  # fiscal year start formatting
  if (!is.numeric(fiscal_year_start) | fiscal_year_start < 1 | fiscal_year_start > 12) {
    stop("fiscal year start should be a number from 1 to 12")
  }

  # input_data is correct type for parallel processing
  if (inherits(input_data, c("data.frame", "tbl")) & is.null(parallel_processing)) {
    # do nothing
  } else if (inherits(input_data, "tbl_spark") & is.null(parallel_processing)) {
    stop("spark data frames should run with spark parallel processing",
      call. = FALSE
    )
  } else if (inherits(input_data, "tbl_spark") & parallel_processing != "spark") {
    stop("spark data frames should run with spark parallel processing",
      call. = FALSE
    )
  }

  # duplicate rows
  dup_col_check <- c(combo_variables, "Date")

  duplicate_tbl <- input_data %>%
    tidyr::unite("Combo",
      combo_variables,
      sep = "--",
      remove = F
    ) %>%
    dplyr::group_by(Combo, Date) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(Date) %>%
    dplyr::collect()

  if (nrow(duplicate_tbl) > 1) {
    stop("duplicate rows have been detected in the input data",
      call. = FALSE
    )
  }
}

#' Check parallel processing set up
#'
#' @param run_info run info
#' @param parallel_processing parallel processing
#' @param inner_parallel inner parallel
#'
#' @return nothing
#' @noRd
check_parallel_processing <- function(run_info,
                                      parallel_processing,
                                      inner_parallel = FALSE) {
  # parallel processing formatting
  if (is.null(parallel_processing)) {
    return()
  }

  if (parallel_processing %in% c("local_machine", "spark") == FALSE) {
    stop("parallel processing input must be one of these values: NULL, 'local_machine', 'azure_batch', 'spark'")
  } else if (parallel_processing == "local_machine" & inner_parallel) {
    stop("cannot run parallel process (inner_parallel input) within another parallel process (parallel_processing input) on a local machine. Please set inner_parallel to FALSE or run in spark")
  } else if (parallel_processing == "spark") {
    if (!exists("sc")) {
      stop("Ensure that you are connected to a spark cluster using an object called 'sc'",
        call. = FALSE
      )
    } else if (is.null(run_info$path)) {
      stop("Path argument in set_run_info() needs to be a path to a mounted Azure Data Lake Storage blob container",
        call. = FALSE
      )
    } else if (substr(run_info$path, 1, 5) != "/dbfs" && substr(run_info$path, 1, 6) != "/synfs") {
      stop("Path argument in set_run_info() needs to be a path to a mounted Azure Data Lake Storage blob container",
        call. = FALSE
      )
    }
  }
}
