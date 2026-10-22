# Profile a data file and suggest Finn settings.
# Usage:
#   Rscript profile_data.R --file=<csv or xlsx> [--sheet=<sheet>]
#   Rscript profile_data.R --project=<name>      (uses the project's data file)
#       [--reveal_labels=true]   (show text labels in diagnostics; needs consent)
# Read-only: never changes the data file. The diagnostics block reports layout
# problems with text labels masked; it never decides a fix.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  dir <- dirname(gsub("~\\+~", " ", file_arg))
  source(file.path(dir, "finn_common.R"))
  source(file.path(dir, "finn_prep.R"))
})

#' Layout diagnostics that never stop profiling
#'
#' @param file Source path.
#' @param sheet Sheet name or number, or NULL.
#' @param reveal TRUE to show text labels unmasked.
#' @return Diagnostics list from [finn_tx_diagnose()], or a list with `error`.
profile_diagnostics <- function(file, sheet, reveal = FALSE) {
  tryCatch(finn_tx_diagnose(file, sheet, reveal_labels = reveal), error = function(e) list(error = finn_redact(conditionMessage(e))))
}

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

#' Layout problems the automatic reshape arguments cannot fix
#'
#' [finn_shape_issues()] knows only title rows, dates across columns and a few
#' total labels. When the diagnostics show more, its `prepare_data.R`
#' arguments could reshape the file wrongly, so a transform spec is needed.
#'
#' @param diag Diagnostics from [finn_tx_diagnose()]; a list with `error`
#'   gives no reasons.
#' @param df Raw data frame as read by [finn_load_data()].
#' @param shape_issues Result of [finn_shape_issues()] for `df`.
#' @return Character vector of plain-language reasons; empty when nothing
#'   beyond the automatic reshape was found.
profile_transform_reasons <- function(diag, df, shape_issues) {
  if (is.null(diag) || !is.null(diag$error)) {
    return(character())
  }
  plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1) "" else "s")
  rows_text <- function(x) paste(utils::head(unlist(x), 5), collapse = ", ")
  reasons <- character()
  filters <- diag$pivot_filters
  if (length(filters)) {
    pairs <- vapply(filters, function(f) sprintf("%s = %s", f$field, if (identical(f$kind, "single")) sprintf("a single item (%s)", f$selection) else f$selection), character(1))
    reasons <- c(reasons, sprintf("%s above the table (%s), so the sheet holds only the slice they select.", plural(length(filters), "pivot filter"), paste(pairs, collapse = "; ")))
  }
  guess <- diag$header_guess
  if (!is.null(guess)) {
    key <- function(x) {
      x <- trimws(as.character(x))
      x <- x[!is.na(x) & nzchar(x) & !grepl("^\\.\\.\\.[0-9]+$|^V[0-9]+$|^X[0-9.]*$", x)]
      gsub("[^[:alnum:]]", "", tolower(x))
    }
    title <- Filter(function(i) identical(i$type, "title_rows"), shape_issues)
    legacy <- if (length(title)) unlist(df[title[[1]]$fix$header_row, ], use.names = FALSE) else names(df)
    found <- Filter(function(h) identical(as.integer(h$row), as.integer(guess)), diag$header_candidates)
    want <- key(if (length(found)) unlist(found[[1]]$cells) else character())
    if (length(want) && !identical(utils::head(key(legacy), length(want)), want)) {
      reasons <- c(reasons, sprintf("The column headers look like they are on sheet row %d, which the automatic reshape would not pick.", guess))
    }
  }
  if (length(diag$outline_columns)) {
    cols <- vapply(diag$outline_columns, `[[`, character(1), "column")
    reasons <- c(reasons, sprintf("Column %s shows each label only on the first row of its group (outline layout); the blanks below need filling down.", paste(cols, collapse = ", ")))
  }
  if (!is.null(diag$two_row_date_header)) {
    reasons <- c(reasons, sprintf("Sheet rows %s together form the date header (years on one row, periods on the next).", rows_text(diag$two_row_date_header$rows)))
  }
  if (length(diag$total_columns)) {
    reasons <- c(reasons, sprintf("Column %s holds totals across the other value columns.", paste(unlist(diag$total_columns), collapse = ", ")))
  }
  legacy_rx <- "^(grand[ _-]?)?(sub[ _-]?)?totals?$|^all$|^total[ _-]"
  vals <- trimws(unlist(lapply(df, function(x) if (is.numeric(x)) NULL else as.character(x)), use.names = FALSE))
  missed <- sum(grepl(paste0("(?i)", finn_tx_total_regex()), vals, perl = TRUE) & !grepl(legacy_rx, vals, ignore.case = TRUE))
  if (missed) {
    reasons <- c(reasons, sprintf("%s such as '<label> Total' that the automatic reshape would keep and double count.", plural(missed, "subtotal label")))
  }
  footnotes <- unlist(diag$footnote_rows)
  if (length(footnotes)) {
    reasons <- c(reasons, sprintf("%s inside the data (sheet rows %s).", plural(length(footnotes), "note or footnote row"), rows_text(footnotes)))
  }
  labels <- setdiff(unlist(diag$label_only_rows$rows), footnotes)
  if (length(labels)) {
    reasons <- c(reasons, sprintf("%s hold a label but no values (sheet rows %s).", plural(length(labels), "row"), rows_text(labels)))
  }
  repeated <- unlist(diag$repeated_header_rows)
  if (length(repeated)) {
    reasons <- c(reasons, sprintf("The header repeats inside the data (sheet rows %s).", rows_text(repeated)))
  }
  reasons
}

#' Latest period of a profiled file that has not ended yet
#'
#' @param df Raw data frame.
#' @param date_guess Date column name, or NULL.
#' @param target_guess Target column name, or NULL.
#' @param date_type Inferred date type.
#' @param today Date to compare with; tests pass a fixed date.
#' @return Result of [finn_incomplete_period()], or NULL.
profile_latest_period <- function(df, date_guess, target_guess, date_type, today = Sys.Date()) {
  if (is.null(date_guess) || is.null(target_guess) || is.na(date_type)) {
    return(NULL)
  }
  dates <- finn_parse_date(df[[date_guess]])$value
  has <- !is.na(dates) & !is.na(finn_parse_number(df[[target_guess]]))
  if (!any(has)) {
    return(NULL)
  }
  finn_incomplete_period(max(dates[has]), date_type, dates[has], today)
}

#' Next action for a sheet that is a filtered pivot
#'
#' @param diag Diagnostics from [finn_tx_diagnose()].
#' @return One instruction, or NULL when there are no pivot filters.
profile_filter_action <- function(diag) {
  if (!length(diag$pivot_filters)) {
    return(NULL)
  }
  paste(
    "Tell the user the sheet is a filtered pivot (see diagnostics pivot_filters): Finn forecasts only the slice those filters select.",
    "Confirm that slice is what they want. For recurring refreshes, suggest exporting the flat, unfiltered data from the upstream source instead of the pivot."
  )
}

#' Load profile data without losing CSV identifiers before inference
#'
#' CSV columns are read lexically first. Configured combos and low-cardinality
#' columns containing zero-padded integer labels stay as text; other columns
#' use the usual CSV type conversion. Excel cell types are left unchanged.
#'
#' @param file Source path.
#' @param sheet Optional Excel sheet.
#' @param combo_variables Configured series columns, or NULL.
#' @return List with `data` and `combo_variables` to exclude from numeric/date
#'   suggestions. Missing configured columns are ignored. Does not write files.
profile_load_data <- function(file, sheet = NULL, combo_variables = NULL) {
  df <- finn_load_data(file, sheet, all_text = TRUE)
  combos <- intersect(as.character(unlist(combo_variables)), names(df))
  if (tolower(tools::file_ext(file)) %in% c("csv", "txt")) {
    padded <- names(df)[vapply(df, function(x) {
      any(grepl("^0[0-9]+$", trimws(x))) &&
        length(unique(x)) < max(0.5 * nrow(df), 2)
    }, logical(1))]
    combos <- union(combos, padded)
    convert <- setdiff(names(df), combos)
    df[convert] <- lapply(df[convert], utils::type.convert, as.is = TRUE)
  }
  list(data = df, combo_variables = combos)
}

# Profile the selected source using saved combo settings when available.
# Return suggestions and diagnostics only; never change the source or config.
finn_main(function(args) {
  file <- finn_arg(args, "file")
  sheet <- finn_arg(args, "sheet")
  cfg <- list()
  if (!is.null(finn_arg(args, "project"))) {
    paths <- finn_project_from_args(args)
    project <- finn_read_project(paths)
    if (is.null(file)) {
      file <- finn_resolve_data_file(paths, project$data_file)
      sheet <- sheet %||% project$sheet
    }
    if (file.exists(file.path(paths$dir, "config.json"))) cfg <- finn_read_project_config(paths)
  }
  if (is.null(file)) stop("Pass --file=<data file> or --project=<name>.", call. = FALSE)
  loaded <- profile_load_data(file, sheet, cfg$combo_variables)
  df <- loaded$data
  sheets <- if (tolower(tools::file_ext(file)) %in% c("xlsx", "xls")) readxl::excel_sheets(file) else NULL
  shape_issues <- finn_shape_issues(df)
  reveal <- finn_bool(finn_arg(args, "reveal_labels"))
  diag <- profile_diagnostics(file, sheet, reveal)
  reasons <- profile_transform_reasons(diag, df, shape_issues)
  if (length(shape_issues) && length(reasons)) {
    return(finn_result(
      TRUE, "needs_reshape",
      sprintf("The layout of %s needs a transform spec; the automatic reshape arguments would not handle all of it.", basename(file)),
      list(
        file = finn_norm(file), sheets = sheets, rows = nrow(df), columns = names(df),
        shape_issues = shape_issues, suggested_prepare_args = NULL, needs_transform = as.list(reasons),
        diagnostics = diag
      ),
      c(
        "Explain each item in needs_transform to the user in plain language.",
        "Write a transform spec from diagnostics (references/data-preparation.md) and preview it with prepare_data.R --file=<same file> --transform=- --preview=true. Do not use the automatic reshape arguments for this file.",
        profile_filter_action(diag),
        "After the user approves and the prepared copy is written, run profile_data.R on that copy."
      )
    ))
  }
  if (length(shape_issues)) {
    fix <- do.call(c, lapply(shape_issues, `[[`, "fix"))
    fix <- fix[!duplicated(names(fix))]
    return(finn_result(
      TRUE, "needs_reshape",
      sprintf("The layout of %s needs reshaping before Finn can use it.", basename(file)),
      list(
        file = finn_norm(file), sheets = sheets, rows = nrow(df), columns = names(df),
        shape_issues = shape_issues, suggested_prepare_args = fix,
        diagnostics = diag
      ),
      c(
        "Explain each shape issue to the user in plain language and confirm the fix.",
        "Run prepare_data.R --file=<same file> with suggested_prepare_args (add --project=<name> to write into the project's input folder). It writes a cleaned copy; the original is never changed.",
        "If the suggested args do not cover everything in diagnostics (subtotals, blank outline labels, two-row date headers, footnotes, text numbers, several tables), write a transform spec instead and preview it with prepare_data.R --transform=- --preview=true.",
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
  date_shares[loaded$combo_variables] <- 0
  date_guess <- if ("Date" %in% names(df) && date_shares[["Date"]] > 0.9) "Date" else names(which.max(date_shares))
  if (length(date_guess) == 0 || max(date_shares) < 0.9) date_guess <- NULL

  others <- setdiff(names(df), date_guess)
  numeric_share <- vapply(others, function(nm) {
    x <- df[[nm]]
    present <- !is.na(x) & nzchar(trimws(as.character(x)))
    if (!any(present)) 0 else mean(!is.na(finn_parse_number(x[present])))
  }, numeric(1))
  numeric_cols <- setdiff(others[numeric_share > 0.95], loaded$combo_variables)
  text_cols <- setdiff(others, numeric_cols)
  combo_guess <- union(loaded$combo_variables,
    text_cols[vapply(text_cols, function(nm) length(unique(df[[nm]])) < max(0.5 * nrow(df), 2), logical(1))])
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
  if (!is.null(date_guess)) {
    key <- do.call(paste, c(df[c(combo_guess, date_guess)], sep = "\r"))
    facts$duplicate_keys <- sum(duplicated(key))
  }
  horizon <- if (!is.na(date_type)) finn_horizon_guide()[[date_type]][[1]] else NULL
  latest <- profile_latest_period(df, date_guess, target_guess, date_type)

  data <- list(
    file = finn_norm(file), sheets = sheets, columns = cols, facts = facts,
    diagnostics = diag,
    layout_problems = if (length(reasons)) as.list(reasons),
    latest_period = latest,
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
    if (length(reasons)) "diagnostics found layout problems (layout_problems) that profiling read past. Show them to the user; if they are real, write a transform spec and preview it with prepare_data.R --transform=- --preview=true before setting up the project.",
    profile_filter_action(diag),
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
    if (!is.na(date_type) && date_type %in% c("month", "quarter", "year")) "Ask which month the user's fiscal year starts and set fiscal_year_start (1 = January).",
    if (!is.null(latest)) {
      paste(
        "Tell the user the latest period has not ended yet (see latest_period), so its actuals are probably partial.",
        if (is.null(latest$suggested_hist_end_date)) "Ask whether it is complete." else sprintf("When proposing settings, offer hist_end_date `%s`; keep the rows unless the user asks to remove them.", latest$suggested_hist_end_date)
      )
    }
  )
  finn_result(TRUE, "ok", sprintf("Profiled %d rows and %d columns.", nrow(df), ncol(df)), data, next_actions)
})
