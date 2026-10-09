# Export a completed run's forecast in a finance-friendly layout.
# Usage:
#   Rscript export_results.R --project=<name> [--run_id=<id>]
#     [--layout=wide|long] [--format=csv|xlsx] [--dest=<folder>]
#     [--include_backtest=false] [--include_intervals=false]
#
# Defaults: latest completed run, wide layout (one row per series, one column
# per date), CSV, written to output/<run_id>/exports/. Existing files are
# never replaced; a timestamped name is used instead. Nothing is deleted.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Path that does not replace an existing file
#'
#' @param path Desired path.
#' @return `path`, or a timestamped sibling when `path` exists.
export_safe_path <- function(path) {
  if (!file.exists(path)) {
    return(path)
  }
  ext <- tools::file_ext(path)
  paste0(tools::file_path_sans_ext(path), "-", finn_stamp(), ".", ext)
}

#' Largest number of data rows one Excel sheet holds (plus a header row)
export_xlsx_max_rows <- 1048575L

#' Write a CSV that Excel opens with the right characters
#'
#' Writes UTF-8 with a byte-order mark so Excel shows accented series names
#' correctly.
#'
#' @param df Data frame to write.
#' @param path Target file path.
#' @return `path`, invisibly.
export_write_csv <- function(df, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  text <- utils::capture.output(utils::write.csv(df, row.names = FALSE, na = ""))
  writeBin(charToRaw(enc2utf8(paste0(paste(enc2utf8(text), collapse = "\r\n"), "\r\n"))), con)
  invisible(path)
}

#' Most recent completed run id
#'
#' @param paths Project paths.
#' @return Run id, or NULL.
export_latest_completed <- function(paths) {
  for (r in finn_list_runs(paths)) {
    if (identical(r$status, "completed")) {
      return(r$run_id)
    }
  }
  NULL
}

#' Reshape a long forecast table to one row per series and one column per date
#'
#' @param df Forecast rows with `Combo`, `Date`, and `Forecast`.
#' @param value Column to spread.
#' @param combo_vars Series columns (such as Region) to keep beside `Combo`.
#' @return Wide data frame.
export_wide <- function(df, value = "Forecast", combo_vars = character()) {
  df$Date <- as.character(as.Date(df$Date))
  keep <- intersect(c("Combo", combo_vars, "Model_ID", "Date", value), names(df))
  d <- df[, keep, drop = FALSE]
  id_cols <- intersect(c("Combo", combo_vars, "Model_ID"), names(d))
  wide <- stats::reshape(d, idvar = id_cols, timevar = "Date", direction = "wide", v.names = value)
  names(wide) <- sub(paste0("^", value, "\\."), "", names(wide))
  date_cols <- sort(setdiff(names(wide), id_cols))
  wide <- wide[order(wide$Combo), c(id_cols, date_cols), drop = FALSE]
  rownames(wide) <- NULL
  wide
}

#' Per-series back-test accuracy for the selected model
#'
#' @param back Back-test rows for the selected model.
#' @param combo_vars Series columns (such as Region) to keep beside `Combo`.
#' @return Data frame with `Combo`, any series columns, `Model_ID`, `WMAPE`,
#'   and `Accuracy`.
export_accuracy <- function(back, combo_vars = character()) {
  if (!nrow(back) || !all(c("Combo", "Forecast", "Target") %in% names(back))) {
    return(NULL)
  }
  out <- do.call(rbind, lapply(split(back, back$Combo), function(g) {
    w <- finn_wmape(g$Forecast, g$Target)
    cbind(
      data.frame(Combo = g$Combo[1], stringsAsFactors = FALSE),
      g[1, intersect(combo_vars, names(g)), drop = FALSE],
      data.frame(
        Model_ID = if ("Model_ID" %in% names(g)) g$Model_ID[1] else NA_character_,
        WMAPE = round(w, 4), Accuracy = round(1 - w, 4), stringsAsFactors = FALSE
      )
    )
  }))
  rownames(out) <- NULL
  out
}

finn_main(function(args) {
  paths <- finn_project_from_args(args)
  run_id <- finn_arg(args, "run_id") %||% export_latest_completed(paths)
  if (is.null(run_id)) {
    return(finn_result(FALSE, "no_completed_run", "This project has no completed run to export.",
      next_actions = "Check run_status.R; start or resume a run with run_forecast.R."))
  }
  run_paths <- finn_run_paths(paths, run_id)
  state <- finn_read_state(run_paths)
  if (is.null(state) || !identical(finn_effective_status(state), "completed")) {
    return(finn_result(FALSE, "not_completed", paste0("Run ", run_id, " has not completed, so there is nothing to export yet.")))
  }
  src <- state$results$all_forecasts %||% file.path(run_paths$output, "all_forecasts.csv")
  if (!file.exists(src)) {
    return(finn_result(FALSE, "missing_output", "The run's saved forecast file was not found.",
      list(expected = src), "Run diagnose_run.R, or re-read the forecast with finnts::get_forecast_data() in an analysis script."))
  }

  layout <- finn_arg(args, "layout", "wide")
  fmt <- tolower(finn_arg(args, "format", "csv"))
  dest <- finn_arg(args, "dest") %||% file.path(run_paths$output, "exports")
  with_back <- finn_bool(finn_arg(args, "include_backtest"), FALSE)
  with_int <- finn_bool(finn_arg(args, "include_intervals"), FALSE)
  if (!layout %in% c("wide", "long")) stop("--layout must be wide or long.", call. = FALSE)
  if (!fmt %in% c("csv", "xlsx")) stop("--format must be csv or xlsx.", call. = FALSE)
  if (identical(fmt, "xlsx") && !requireNamespace("writexl", quietly = TRUE)) {
    return(finn_result(FALSE, "needs_package", "Excel export needs the small 'writexl' package.",
      next_actions = c("Ask the user before installing: install.packages('writexl').", "Or export with --format=csv, which opens in Excel.")))
  }

  fcst <- utils::read.csv(src, stringsAsFactors = FALSE, check.names = FALSE)
  best <- if ("Best_Model" %in% names(fcst)) fcst[fcst$Best_Model %in% "Yes", , drop = FALSE] else fcst
  future <- best[best$Run_Type %in% "Future_Forecast", , drop = FALSE]
  back <- best[best$Run_Type %in% "Back_Test", , drop = FALSE]
  if (!with_int) future <- future[, !grepl("^(lo|hi)_", names(future)), drop = FALSE]
  combo_vars <- setdiff(as.character(unlist(state$config$combo_variables)), "Combo")
  long_cols <- intersect(c("Combo", combo_vars, "Model_ID", "Date", "Forecast", grep("^(lo|hi)_", names(future), value = TRUE)), names(future))

  sheets <- list(
    Forecast = if (identical(layout, "wide")) export_wide(future, combo_vars = combo_vars) else future[, long_cols, drop = FALSE]
  )
  acc <- export_accuracy(back, combo_vars)
  if (!is.null(acc)) sheets$Accuracy <- acc
  if (with_back && nrow(back)) {
    sheets$Backtest <- back[, intersect(c("Combo", combo_vars, "Model_ID", "Train_Test_ID", "Date", "Target", "Forecast"), names(back)), drop = FALSE]
  }

  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  stem <- paste0(paths$name, "_", run_id)
  written <- list()
  notes <- character()
  if (identical(fmt, "xlsx") && max(vapply(sheets, nrow, integer(1))) >= export_xlsx_max_rows) {
    fmt <- "csv"
    notes <- c(notes, "The results have more rows than one Excel sheet can hold (1,048,576), so they were saved as CSV files instead. Offer the wide layout or a filtered export if the user needs Excel.")
  }
  if (identical(fmt, "xlsx")) {
    target <- export_safe_path(file.path(dest, paste0(stem, ".xlsx")))
    writexl::write_xlsx(sheets, target)
    written$workbook <- target
  } else {
    for (nm in names(sheets)) {
      target <- export_safe_path(file.path(dest, paste0(stem, "_", tolower(nm), ".csv")))
      export_write_csv(sheets[[nm]], target)
      written[[tolower(nm)]] <- target
    }
  }
  overall <- if (nrow(back)) finn_wmape(back$Forecast, back$Target) else NA_real_
  finn_result(TRUE, "exported", paste0("Exported ", nrow(sheets$Forecast), " forecast rows."), list(
    run_id = run_id, layout = layout, format = fmt, files = written,
    series = length(unique(future$Combo)),
    back_test_accuracy = if (is.na(overall)) NULL else round(1 - overall, 4)
  ), c(notes, "Tell the user where the files are; offer a quick summary or chart via a free-form analysis script."))
})
