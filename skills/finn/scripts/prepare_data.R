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
#
# Transform specs (any layout the fixed flags cannot handle):
#   <spec JSON on stdin> | Rscript prepare_data.R --file=<data> --transform=- --preview=true
#       [--project=<name>] [--reveal_labels=true]
#   <same spec on stdin> | Rscript prepare_data.R --file=<data> --transform=- --approve=<token>
#       [--project=<name> | --out_dir=<folder>] [--out_name=<name.csv>]
# The spec is read from stdin only and is never saved. Preview applies it,
# runs the verification gate, and returns a masked preview and, when the gate
# passes, an approval token. Approve re-applies the same spec, re-runs the gate,
# and writes the prepared CSV only when the token matches.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  dir <- dirname(gsub("~\\+~", " ", file_arg))
  source(file.path(dir, "finn_common.R"))
  source(file.path(dir, "finn_prep.R"))
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

#' Read a transform spec from standard input
#'
#' Specs are never read from or saved to a file, so nothing is left behind.
#'
#' @param value Value of `--transform`; must be "-".
#' @param con Connection to read; defaults to standard input.
#' @return Spec JSON text.
prepare_read_spec <- function(value, con = NULL) {
  if (!identical(value, "-")) {
    stop("--transform only accepts '-': pipe the spec JSON on standard input. Specs are never read from or saved to a file.", call. = FALSE)
  }
  if (is.null(con)) {
    con <- file("stdin")
    on.exit(close(con))
  }
  text <- paste(readLines(con, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  if (!nzchar(trimws(text))) stop("No transform spec arrived on standard input.", call. = FALSE)
  text
}

#' Folder that receives prepared copies and previews
#'
#' @param args Parsed arguments.
#' @param file Source path.
#' @return Existing folder path.
prepare_out_dir <- function(args, file) {
  out_dir <- if (!is.null(finn_arg(args, "project"))) {
    paths <- finn_project_from_args(args, must_exist = FALSE)
    finn_ensure_project_dirs(paths)
    paths$input
  } else {
    finn_norm(finn_arg(args, "out_dir") %||% dirname(file))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir
}

#' Project config for the gate, when the project has one
#'
#' @param args Parsed arguments.
#' @return Config list or NULL.
prepare_project_config <- function(args) {
  if (is.null(finn_arg(args, "project"))) {
    return(NULL)
  }
  paths <- finn_project_from_args(args, must_exist = FALSE)
  if (!file.exists(file.path(paths$dir, "config.json"))) {
    return(NULL)
  }
  cfg <- finn_read_project_config(paths)
  cfg[c("project_name", "data")] <- NULL
  cfg
}

#' Write the local real-value preview of a transform result
#'
#' The file stays on this machine for the user to open; its values are never
#' sent anywhere by the skill. It replaces only an earlier preview written by
#' this function (recognised by its first column name).
#'
#' @param out_dir Destination folder.
#' @param file Source path.
#' @param run Result of [finn_tx_run()].
#' @param n Number of rows to include.
#' @return Path written.
prepare_write_preview <- function(out_dir, file, run, n = 100L) {
  out <- finn_tx_output(run$ctx)
  rows <- utils::head(seq_len(nrow(out)), n)
  preview <- cbind(finn_preview_source_rows = run$ctx$d$.finn_row[rows], out[rows, , drop = FALSE])
  name <- paste0(tools::file_path_sans_ext(basename(file)), "_finn_preview.csv")
  path <- file.path(out_dir, name)
  if (file.exists(path)) {
    first <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) "")
    if (!startsWith(gsub("\"", "", first), "finn_preview_source_rows")) path <- prepare_out_path(out_dir, name)
  }
  writeLines(finn_tx_csv_lines(preview), path, useBytes = TRUE)
  finn_norm(path)
}

#' Value-free plan summary for the audit record
#'
#' One line per applied step with its number, operation, and affected count,
#' for example "Step 4: fill_values (1 value)". The spec description, the
#' reasons, and the labels or values a step used are never included.
#'
#' @param changes `ctx$changes` from [finn_tx_run()].
#' @return Character vector, one element per step.
prepare_audit_steps <- function(changes) {
  vapply(changes, function(ch) sprintf("Step %d: %s (%s)", ch$step, ch$op, ch$affects), character(1))
}

#' Value-free verification results for the audit record
#'
#' Keeps each rule's id and status. The message becomes a fixed phrase for
#' that status, because a validator message can name series labels and values.
#'
#' @param checks Gate checks with `Rule`, `Status`, and `Message` columns.
#' @return List with one `Rule`, `Status`, `Message` list per check.
prepare_audit_checks <- function(checks) {
  phrase <- c(pass = "Passed.", report = "Reported.", fail = "Failed.", skipped = "Skipped.")
  lapply(seq_len(nrow(checks)), function(i) {
    status <- checks$Status[i]
    list(Rule = checks$Rule[i], Status = status, Message = if (status %in% names(phrase)) unname(phrase[status]) else "Recorded.")
  })
}

#' Append a data preparation audit record to the project
#'
#' Keeps what was done and how it checked out: one line per step (operation
#' and affected count), each rule's id and status with a fixed message, and
#' the row counts. Never the spec, its description or reasons, series labels,
#' validator messages, or data values, and at most the 20 latest records.
#'
#' @param args Parsed arguments.
#' @param file Source path.
#' @param out Prepared file path.
#' @param run Result of [finn_tx_run()].
#' @return Invisible NULL.
prepare_record_audit <- function(args, file, out, run) {
  if (is.null(finn_arg(args, "project"))) {
    return(invisible(NULL))
  }
  paths <- finn_project_from_args(args, must_exist = FALSE)
  project <- finn_read_project(paths)
  if (is.null(project)) {
    return(invisible(NULL))
  }
  record <- list(
    prepared_at = finn_now(),
    source = finn_project_relpath(paths, file), output = finn_project_relpath(paths, out),
    source_md5 = run$ctx$md5, token = run$token, plan_summary = prepare_audit_steps(run$ctx$changes),
    checks = prepare_audit_checks(run$gate$checks),
    counts = run$gate$counts
  )
  project$data_preparations <- utils::tail(c(project$data_preparations, list(record)), 20L)
  finn_write_project(paths, project)
  invisible(NULL)
}

#' Preview or apply a transform spec
#'
#' @param args Parsed arguments.
#' @param file Source path.
#' @param spec_text Spec JSON text.
#' @param validator Validator function, or NULL to fail closed.
#' @param today Date used to spot a latest period that has not ended; tests
#'   pass a fixed date.
#' @return Finn result list. `data$checks` holds the validation messages
#'   masked like the preview tables unless `--reveal_labels=true`.
#'   `data$latest_period` flags a latest period that has not ended; it is
#'   advisory and never stored in the audit record.
prepare_transform <- function(args, file, spec_text, validator = finn_tx_default_validator(), today = Sys.Date()) {
  preview <- finn_bool(finn_arg(args, "preview"))
  token <- finn_arg(args, "approve")
  if (preview == !is.null(token)) stop("Pass exactly one of --preview=true or --approve=<token>.", call. = FALSE)
  if (is.null(validator)) {
    stop("The installed finnts has no validate_prepared_input(), so the prepared data cannot be verified. Update finnts with install_finnts.R, then try again.", call. = FALSE)
  }
  run <- finn_tx_run(file, spec_text, prepare_project_config(args), validator,
    sheet = finn_arg(args, "sheet"), reveal_labels = finn_bool(finn_arg(args, "reveal_labels")), today = today
  )
  gate <- run$gate
  checks <- lapply(seq_len(nrow(run$display_checks)), function(i) as.list(run$display_checks[i, ]))
  data <- list(
    source = finn_norm(file), passed = isTRUE(gate$passed), preview_markdown = run$preview_markdown,
    plan_summary = as.list(run$plan_summary), needs_confirmation = as.list(run$ctx$confirm %||% character()),
    checks = checks, counts = gate$counts, summary = gate$summary, latest_period = run$latest_period
  )
  partial_action <- if (!is.null(run$latest_period)) {
    paste(
      "Point out the Latest period line: that period has not ended, so its actuals are probably partial.",
      if (is.null(run$latest_period$suggested_hist_end_date)) {
        "Ask whether it is complete; keep the rows unless the user asks to remove them."
      } else {
        sprintf("Offer hist_end_date `%s` in the settings review instead of dropping rows; the user decides.", run$latest_period$suggested_hist_end_date)
      }
    )
  }
  if (preview) {
    data$token <- run$token
    if (!isTRUE(gate$passed)) {
      return(finn_result(FALSE, "gate_failed", "The prepared data failed verification. Nothing was written.", data, c(
        "Show the user preview_markdown and explain each FAIL row in plain language.",
        "Revise the spec and preview again; a revised spec needs the user's approval again."
      )))
    }
    data$preview_file <- prepare_write_preview(prepare_out_dir(args, file), file, run)
    return(finn_result(TRUE, "preview", "The plan passed verification. Nothing is prepared until the user approves.", data, c(
      "Show the user preview_markdown exactly as returned and tell them preview_file holds real values they can open locally.",
      partial_action,
      "Ask for one approval of the whole plan, including every item in needs_confirmation.",
      "After approval, pipe the same spec again with --approve=<token>. Any change to the spec needs a new preview and approval."
    )))
  }
  if (!isTRUE(gate$passed)) stop("The prepared data failed verification. Run --preview=true to see why. Nothing was written.", call. = FALSE)
  if (!identical(token, run$token)) {
    stop("The approval token does not match this spec, data file, and settings. Run --preview=true again and get the user's approval for the new preview.", call. = FALSE)
  }
  out_dir <- prepare_out_dir(args, file)
  name <- finn_arg(args, "out_name") %||% paste0(tools::file_path_sans_ext(basename(file)), "_finn_ready.csv")
  if (!grepl("\\.csv$", name, ignore.case = TRUE)) name <- paste0(name, ".csv")
  out <- prepare_out_path(out_dir, name)
  if (finn_norm(out) == finn_norm(file)) stop("The prepared copy cannot replace the source file.", call. = FALSE)
  writeLines(finn_tx_csv_lines(gate$output), out, useBytes = TRUE)
  prepare_record_audit(args, file, out, run)
  data$preview_markdown <- NULL
  data$output <- finn_norm(out)
  data$rows <- nrow(gate$output)
  data$columns <- names(gate$output)
  finn_result(TRUE, "ok", sprintf("Wrote the prepared copy with %d rows to %s.", nrow(gate$output), basename(out)), data, c(
    "Tell the user the original file is untouched and use output as the project data file.",
    partial_action,
    "If the same fix will be needed on every data refresh, recommend moving it into the upstream workflow or source query."
  ))
}

# Prepare the requested copy, preserving declared CSV ID columns before sampling.
# Return its path and changes; neither the source nor an existing copy is replaced.
finn_main(function(args) {
  file <- finn_arg(args, "file")
  if (is.null(file)) stop("Pass --file=<data file>.", call. = FALSE)
  if (!is.null(finn_arg(args, "transform"))) {
    return(prepare_transform(args, file, prepare_read_spec(finn_arg(args, "transform"))))
  }
  header_row <- finn_arg(args, "header_row")
  pivot <- finn_bool(finn_arg(args, "pivot_longer"))
  drop_totals <- finn_bool(finn_arg(args, "drop_totals"))
  sample_n <- finn_arg(args, "sample_series")
  combos <- trimws(unlist(strsplit(finn_arg(args, "combo_variables") %||% "", ",")))
  combos <- combos[nzchar(combos)]
  ids <- trimws(unlist(strsplit(finn_arg(args, "id_columns") %||% "", ",")))
  text_columns <- if (!is.null(sample_n)) c(combos, ids[nzchar(ids)])
  raw <- finn_load_data(file, finn_arg(args, "sheet"),
    text_columns = text_columns, all_text = !is.null(sample_n) && !is.null(header_row))
  if (is.null(header_row) && !pivot && !drop_totals && is.null(sample_n)) {
    stop("Nothing to do. Pass --header_row, --pivot_longer=true, or --drop_totals=true as suggested by profile_data.R, --sample_series=<n> with --combo_variables for a test subset, or pipe a transform spec with --transform=- for any other layout.", call. = FALSE)
  }
  shaped <- if (is.null(header_row) && !pivot && !drop_totals) {
    list(data = raw, notes = character())
  } else {
    finn_reshape_data(
      raw,
      header_row = header_row, pivot_longer = pivot,
      id_columns = finn_arg(args, "id_columns"),
      value_name = finn_arg(args, "value_name") %||% "Value",
      drop_totals = drop_totals, text_columns = text_columns
    )
  }
  if (!is.null(sample_n)) {
    if (!length(combos)) stop("--sample_series needs --combo_variables=<columns that identify each series>.", call. = FALSE)
    sampled <- prepare_sample_series(shaped$data, combos, sample_n)
    shaped$data <- sampled$data
    shaped$notes <- c(shaped$notes, sampled$note)
  }

  out_dir <- prepare_out_dir(args, file)
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
