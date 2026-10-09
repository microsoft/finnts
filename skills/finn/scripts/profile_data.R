# Profile a data file and suggest Finn settings.
# Usage:
#   Rscript profile_data.R --file=<csv or xlsx> [--sheet=<sheet>]
#   Rscript profile_data.R --project=<name>      (uses the project's data file)
# Read-only: never changes the data file.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Share of non-empty values that parse as dates
#'
#' @param x Column vector.
#' @return Numeric share between 0 and 1.
profile_date_share <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    return(1)
  }
  if (is.numeric(x)) {
    return(if (all(is.na(x)) || any(x < 20000 | x > 80000, na.rm = TRUE)) 0 else 0.5)
  }
  present <- !is.na(x) & nzchar(trimws(as.character(x)))
  if (!any(present)) {
    return(0)
  }
  mean(!is.na(finn_parse_date(x[present])$value))
}

#' Infer the finnts date type from sorted unique dates
#'
#' @param dates Date vector.
#' @return One of year, quarter, month, week, day, or NA.
profile_date_type <- function(dates) {
  d <- sort(unique(dates[!is.na(dates)]))
  if (length(d) < 2) {
    return(NA_character_)
  }
  gap <- stats::median(as.numeric(diff(d)))
  if (gap >= 300) "year" else if (gap >= 80) "quarter" else if (gap >= 27) "month" else if (gap >= 6) "week" else "day"
}

finn_main(function(args) {
  file <- finn_arg(args, "file")
  sheet <- finn_arg(args, "sheet")
  if (is.null(file) && !is.null(finn_arg(args, "project"))) {
    paths <- finn_project_from_args(args)
    project <- finn_read_project(paths)
    file <- finn_resolve_data_file(paths, project$data_file)
    sheet <- sheet %||% project$sheet
  }
  if (is.null(file)) stop("Pass --file=<data file> or --project=<name>.", call. = FALSE)
  df <- finn_load_data(file, sheet)
  sheets <- if (tolower(tools::file_ext(file)) %in% c("xlsx", "xls")) readxl::excel_sheets(file) else NULL
  shape_issues <- finn_shape_issues(df)
  if (length(shape_issues)) {
    fix <- do.call(c, lapply(shape_issues, `[[`, "fix"))
    fix <- fix[!duplicated(names(fix))]
    return(finn_result(
      TRUE, "needs_reshape",
      sprintf("The layout of %s needs reshaping before Finn can use it.", basename(file)),
      list(
        file = finn_norm(file), sheets = sheets, rows = nrow(df), columns = names(df),
        shape_issues = shape_issues, suggested_prepare_args = fix
      ),
      c(
        "Explain each shape issue to the user in plain language and confirm the fix.",
        "Run prepare_data.R --file=<same file> with suggested_prepare_args (add --project=<name> to write into the project's input folder). It writes a cleaned copy; the original is never changed.",
        "Then run profile_data.R on the cleaned copy."
      )
    ))
  }

  cols <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    list(
      name = nm,
      type = class(x)[1],
      missing = sum(is.na(x) | (is.character(x) & !nzchar(trimws(x %||% "")))),
      unique = length(unique(x)),
      example = utils::head(as.character(x[!is.na(x)]), 3)
    )
  })
  date_shares <- vapply(df, profile_date_share, numeric(1))
  date_guess <- if ("Date" %in% names(df) && date_shares[["Date"]] > 0.9) "Date" else names(which.max(date_shares))
  if (length(date_guess) == 0 || max(date_shares) < 0.9) date_guess <- NULL

  others <- setdiff(names(df), date_guess)
  numeric_share <- vapply(others, function(nm) {
    x <- df[[nm]]
    present <- !is.na(x) & nzchar(trimws(as.character(x)))
    if (!any(present)) 0 else mean(!is.na(finn_parse_number(x[present])))
  }, numeric(1))
  numeric_cols <- others[numeric_share > 0.95]
  text_cols <- setdiff(others, numeric_cols)
  combo_guess <- text_cols[vapply(text_cols, function(nm) length(unique(df[[nm]])) < max(0.5 * nrow(df), 2), logical(1))]
  target_guess <- if (length(numeric_cols)) {
    preferred <- numeric_cols[grepl("value|amount|revenue|sales|target|actual|units|qty|quantity|spend|cost", numeric_cols, ignore.case = TRUE)]
    (if (length(preferred)) preferred else numeric_cols)[1]
  } else {
    NULL
  }
  regressor_candidates <- setdiff(numeric_cols, target_guess)

  facts <- list(rows = nrow(df), columns = ncol(df))
  date_type <- NA_character_
  if (!is.null(date_guess)) {
    parsed_dates <- finn_parse_date(df[[date_guess]])
    dates <- parsed_dates$value
    date_type <- profile_date_type(dates)
    facts$date_min <- as.character(min(dates, na.rm = TRUE))
    facts$date_max <- as.character(max(dates, na.rm = TRUE))
    facts$unique_dates <- length(unique(dates))
    facts$date_format <- parsed_dates$format
    facts$date_format_ambiguous <- isTRUE(parsed_dates$ambiguous)
    facts$date_format_alternative <- parsed_dates$alternative
  }
  facts$delimiter <- attr(df, "finn_delimiter")
  facts$encoding <- attr(df, "finn_encoding")
  decimal_mark <- if (!is.null(target_guess) && is.character(df[[target_guess]])) attr(finn_parse_number(df[[target_guess]]), "decimal_mark")
  facts$decimal_mark <- decimal_mark
  if (length(combo_guess)) {
    facts$series <- nrow(unique(df[combo_guess]))
  } else {
    facts$series <- 1L
  }
  horizon <- if (!is.na(date_type)) finn_horizon_guide()[[date_type]][[1]] else NULL

  data <- list(
    file = finn_norm(file), sheets = sheets, columns = cols, facts = facts,
    suggested = list(
      date_column = date_guess,
      date_type = date_type,
      date_format = if (isTRUE(facts$date_format_ambiguous)) facts$date_format,
      decimal_mark = if (identical(decimal_mark, ",")) ",",
      target_variable = target_guess,
      combo_variables = if (length(combo_guess)) as.list(combo_guess) else NULL,
      external_regressor_candidates = if (length(regressor_candidates)) as.list(regressor_candidates) else NULL,
      forecast_horizon = horizon
    )
  )
  next_actions <- c(
    "Show the user the suggested date, value, and series columns and the date type; confirm or correct them.",
    "Do not use other numeric columns as external regressors unless the user asks; they need future values for the forecast period.",
    if (length(combo_guess) == 0) "No series columns were found. If the data is a single series, add a constant column such as Series='Total' in a copy, or ask the user which column identifies each series.",
    if (!is.null(sheets) && length(sheets) > 1) "The workbook has several sheets; confirm which one holds the data.",
    if (isTRUE(facts$date_format_ambiguous)) {
      sprintf(
        "Dates could be month/day or day/month. Show the user a few raw dates and ask which it is; they were read as %s, the other choice is \"%s\". Save the answer as date_format in the config.",
        facts$date_format, facts$date_format_alternative
      )
    },
    if (identical(decimal_mark, ",")) "Values use a decimal comma (for example 1.234,56). Confirm with the user and keep decimal_mark \",\" in the config.",
    if (!is.na(date_type) && date_type %in% c("month", "quarter", "year")) "Ask which month the user's fiscal year starts and set fiscal_year_start (1 = January)."
  )
  finn_result(TRUE, "ok", sprintf("Profiled %d rows and %d columns.", nrow(df), ncol(df)), data, next_actions)
})
