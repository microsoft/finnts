# Write a cleaned, Finn-ready copy of a data file. The original is never changed.
# Usage:
#   Rscript prepare_data.R --file=<csv or xlsx> [--sheet=<sheet>]
#       [--header_row=<n>] [--pivot_longer=true --id_columns=Region,Product --value_name=Value]
#       [--drop_totals=true] [--sample_series=<n> --combo_variables=Region,Product]
#       [--project=<name> | --out_dir=<folder>] [--out_name=<name.csv>]
# Run profile_data.R first; its shape_issues list the fix arguments to pass here.
# --sample_series keeps a reproducible random sample of n whole series, for a
# quick test run before forecasting a large dataset.
# The copy goes to the project's input/ folder when --project is given, else
# next to the source file. Existing files are never overwritten.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Choose a destination path that does not already exist
#'
#' @param dir Destination folder.
#' @param name Desired file name ending in .csv.
#' @return Path to `name` in `dir`, or a timestamped variant when taken.
prepare_out_path <- function(dir, name) {
  path <- file.path(dir, name)
  if (!file.exists(path)) {
    return(path)
  }
  stem <- tools::file_path_sans_ext(name)
  file.path(dir, sprintf("%s_%s.csv", stem, format(Sys.time(), "%Y%m%d_%H%M%S")))
}

#' Keep a reproducible random sample of whole time series
#'
#' @param data Data frame.
#' @param combo_variables Columns that identify each series.
#' @param n Number of series to keep.
#' @param seed Random seed so the same sample is drawn each time.
#' @return List with `data` (every row of the sampled series) and `note`.
prepare_sample_series <- function(data, combo_variables, n, seed = 123) {
  missing <- setdiff(combo_variables, names(data))
  if (length(missing)) {
    stop(sprintf("Columns not found for --combo_variables: %s.", paste(missing, collapse = ", ")), call. = FALSE)
  }
  n <- suppressWarnings(as.integer(n))
  if (is.na(n) || n < 1) stop("--sample_series must be a positive whole number.", call. = FALSE)
  key <- do.call(paste, c(data[combo_variables], sep = "--"))
  keys <- sort(unique(key))
  if (n >= length(keys)) {
    return(list(data = data, note = sprintf("The data has only %d series, so all were kept.", length(keys))))
  }
  keep <- prepare_with_seed(seed, sample(keys, n))
  list(
    data = data[key %in% keep, , drop = FALSE],
    note = sprintf("Kept a random sample of %d of %d series for a quick test run.", n, length(keys))
  )
}

#' Evaluate an expression with a temporary random seed
#'
#' Restores the caller's random number state afterwards.
#'
#' @param seed Seed to use.
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
prepare_with_seed <- function(seed, expr) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  expr
}

finn_main(function(args) {
  file <- finn_arg(args, "file")
  if (is.null(file)) stop("Pass --file=<data file>.", call. = FALSE)
  raw <- finn_load_data(file, finn_arg(args, "sheet"))
  header_row <- finn_arg(args, "header_row")
  pivot <- finn_bool(finn_arg(args, "pivot_longer"))
  drop_totals <- finn_bool(finn_arg(args, "drop_totals"))
  sample_n <- finn_arg(args, "sample_series")
  if (is.null(header_row) && !pivot && !drop_totals && is.null(sample_n)) {
    stop("Nothing to do. Pass --header_row, --pivot_longer=true, or --drop_totals=true as suggested by profile_data.R, or --sample_series=<n> with --combo_variables for a test subset.", call. = FALSE)
  }
  shaped <- if (is.null(header_row) && !pivot && !drop_totals) {
    list(data = raw, notes = character())
  } else {
    finn_reshape_data(
      raw,
      header_row = header_row, pivot_longer = pivot,
      id_columns = finn_arg(args, "id_columns"),
      value_name = finn_arg(args, "value_name") %||% "Value",
      drop_totals = drop_totals
    )
  }
  if (!is.null(sample_n)) {
    combos <- trimws(unlist(strsplit(finn_arg(args, "combo_variables") %||% "", ",")))
    combos <- combos[nzchar(combos)]
    if (!length(combos)) stop("--sample_series needs --combo_variables=<columns that identify each series>.", call. = FALSE)
    sampled <- prepare_sample_series(shaped$data, combos, sample_n)
    shaped$data <- sampled$data
    shaped$notes <- c(shaped$notes, sampled$note)
  }

  out_dir <- if (!is.null(finn_arg(args, "project"))) {
    paths <- finn_project_from_args(args, must_exist = FALSE)
    finn_ensure_project_dirs(paths)
    paths$input
  } else {
    finn_norm(finn_arg(args, "out_dir") %||% dirname(file))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  default_suffix <- if (!is.null(sample_n) && is.null(header_row) && !pivot && !drop_totals) sprintf("_sample%s.csv", sample_n) else "_finn_ready.csv"
  name <- finn_arg(args, "out_name") %||% paste0(tools::file_path_sans_ext(basename(file)), default_suffix)
  if (!grepl("\\.csv$", name, ignore.case = TRUE)) name <- paste0(name, ".csv")
  out <- prepare_out_path(out_dir, name)
  if (finn_norm(out) == finn_norm(file)) stop("The cleaned copy cannot replace the source file.", call. = FALSE)
  data <- shaped$data
  if (inherits(data$Date, "Date")) data$Date <- format(data$Date, "%Y-%m-%d")
  utils::write.csv(data, out, row.names = FALSE, na = "")

  finn_result(
    TRUE, "ok", sprintf("Wrote a cleaned copy with %d rows to %s.", nrow(data), basename(out)),
    list(source = finn_norm(file), output = finn_norm(out), rows = nrow(data), columns = names(data), changes = shaped$notes),
    c(
      "Tell the user what changed and that the original file is untouched.",
      "Run profile_data.R --file=<output> to confirm the layout, then use the output path as the project data file."
    )
  )
})
