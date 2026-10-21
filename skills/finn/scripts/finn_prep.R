# Declarative input preparation for the Finn skill.
#
# Copilot describes a fix as a JSON transform spec. This file applies the spec
# with a fixed set of operations (no R code, file paths, or network access),
# records where every value came from, and runs a verification gate before
# anything is written. The spec arrives on standard input and is never saved.
# Requires finn_common.R to be sourced first.

#' Operations a transform spec may use, with their parameters
#'
#' Every step may also carry `op` and `note`. Any other parameter is rejected,
#' so a spec cannot name files, URLs, or code.
#'
#' @return Named list of `list(required, optional)` parameter names.
finn_tx_ops <- function() {
  cond <- c("equals", "in", "matches", "blank")
  list(
    keep_rows = list(required = c("rows", "reason"), optional = character()),
    drop_rows = list(required = c("rows", "reason"), optional = character()),
    drop_rows_where = list(required = c("column", "reason"), optional = cond),
    drop_blank_rows = list(required = character(), optional = c("columns", "reason")),
    use_header_rows = list(required = "rows", optional = "sep"),
    drop_columns = list(required = c("columns", "reason"), optional = character()),
    drop_total_columns = list(required = "columns", optional = c("of", "reason", "reconcile")),
    rename_columns = list(required = "map", optional = character()),
    fill_down = list(required = "columns", optional = character()),
    split_compact_labels = list(required = c("column", "into"), optional = "parent_rows"),
    drop_total_rows = list(required = "columns", optional = c("value_columns", "reason", "reconcile")),
    pivot_longer = list(required = "id_columns", optional = c("names_to", "values_to")),
    split_column = list(required = c("column", "into"), optional = c("sep", "pattern", "keep")),
    combine_columns = list(required = c("columns", "into"), optional = c("sep", "keep")),
    parse_numbers = list(required = "columns", optional = c("decimal_mark", "missing", "suffixes")),
    parse_dates = list(required = "column", optional = c("format", "formats")),
    fiscal_to_date = list(required = c("column", "pattern", "fiscal_year_start"), optional = c("year_is", "period_type", "into")),
    scale = list(required = c("column", "factor", "reason"), optional = "where"),
    aggregate_duplicates = list(required = c("keys", "columns", "reason"), optional = character()),
    drop_duplicates = list(required = c("keys", "reason"), optional = "keep"),
    derive_column = list(required = c("name", "expression", "reason"), optional = character()),
    fill_values = list(required = c("column", "method", "reason"), optional = c("value", "by", "extend_into_future"))
  )
}

#' Patterns that mark a label as a total or subtotal
#'
#' @return Regular expression matched case-insensitively against trimmed text.
finn_tx_total_regex <- function() {
  "^(grand\\s+total|total|subtotal|sub-total|sum|all)$|^(grand\\s+)?total\\b|\\btotal$|^sub-?total\\b"
}

#' Flatten a JSON value to a character vector
#'
#' @param x Value parsed with `simplifyVector = FALSE`.
#' @return Character vector (empty for NULL).
finn_tx_chr <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  as.character(unlist(x, use.names = FALSE))
}

#' Read and check a transform spec
#'
#' Rejects oversized text, unknown keys, unknown operations, and unknown
#' parameters. Nothing in a valid spec can point at a file or address.
#'
#' @param text Spec JSON as one string.
#' @param max_bytes Largest accepted spec size.
#' @return Parsed spec list with defaults filled in.
finn_tx_read_spec <- function(text, max_bytes = 200000) {
  text <- paste(text, collapse = "\n")
  if (!nzchar(trimws(text))) stop("The transform spec on standard input is empty.", call. = FALSE)
  if (nchar(text, type = "bytes") > max_bytes) stop("The transform spec is larger than 200 KB. Simplify it.", call. = FALSE)
  spec <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) {
    stop("The transform spec is not valid JSON: ", conditionMessage(e), call. = FALSE)
  })
  if (!is.list(spec) || is.null(names(spec))) stop("The transform spec must be a JSON object.", call. = FALSE)
  top <- c("version", "description", "source", "settings", "steps", "tolerance", "not_totals")
  extra <- setdiff(names(spec), top)
  if (length(extra)) stop("Unknown spec keys: ", paste(extra, collapse = ", "), ". Allowed: ", paste(top, collapse = ", "), ".", call. = FALSE)
  if (!identical(as.integer(spec$version %||% 0L), 1L)) stop("Set \"version\": 1 in the transform spec.", call. = FALSE)
  if (!is.null(spec$source)) {
    bad <- setdiff(names(spec$source), c("sheet", "sheets", "sheet_column"))
    if (length(bad)) stop("Unknown source keys: ", paste(bad, collapse = ", "), ". Only sheet, sheets, and sheet_column are allowed.", call. = FALSE)
  }
  if (!is.null(spec$tolerance)) {
    bad <- setdiff(names(spec$tolerance), c("absolute", "relative"))
    if (length(bad)) stop("Unknown tolerance keys: ", paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  ops <- finn_tx_ops()
  steps <- spec$steps %||% list()
  if (!is.list(steps)) stop("\"steps\" must be a list of operations.", call. = FALSE)
  for (i in seq_along(steps)) {
    s <- steps[[i]]
    op <- s$op %||% ""
    if (!op %in% names(ops)) stop(sprintf("Step %d uses unknown op '%s'. Allowed: %s.", i, op, paste(names(ops), collapse = ", ")), call. = FALSE)
    allowed <- c("op", "note", ops[[op]]$required, ops[[op]]$optional)
    bad <- setdiff(names(s), allowed)
    if (length(bad)) stop(sprintf("Step %d (%s) has unknown parameters: %s.", i, op, paste(bad, collapse = ", ")), call. = FALSE)
    miss <- setdiff(ops[[op]]$required, names(s))
    if (length(miss)) stop(sprintf("Step %d (%s) is missing: %s.", i, op, paste(miss, collapse = ", ")), call. = FALSE)
    if (!is.null(s$where)) {
      bad <- setdiff(names(s$where), c("column", "equals", "in", "matches", "blank"))
      if (length(bad) || is.null(s$where$column)) stop(sprintf("Step %d (%s): where needs column plus equals, in, matches, or blank.", i, op), call. = FALSE)
    }
  }
  spec$steps <- steps
  spec$tolerance <- list(
    absolute = as.numeric(spec$tolerance$absolute %||% 1e-6),
    relative = as.numeric(spec$tolerance$relative %||% 1e-9)
  )
  spec
}

#' Canonical JSON text of a spec, used for the approval token
#'
#' @param spec Parsed spec.
#' @return One string.
finn_tx_canonical <- function(spec) {
  as.character(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null", digits = NA))
}

#' Spreadsheet-style column letters
#'
#' @param n Number of columns.
#' @return Character vector A, B, ..., Z, AA, AB, ...
finn_tx_letters <- function(n) {
  vapply(seq_len(n), function(i) {
    out <- ""
    while (i > 0) {
      r <- (i - 1) %% 26
      out <- paste0(LETTERS[r + 1], out)
      i <- (i - 1) %/% 26
    }
    out
  }, character(1))
}

#' TRUE where a cell is missing or only whitespace
#'
#' @param x Character vector.
#' @return Logical vector.
finn_tx_blank <- function(x) is.na(x) | !nzchar(trimws(x))

#' Read one sheet or delimited file as an all-text grid
#'
#' Keeps every row, including leading blank and title rows, and names columns
#' by spreadsheet letter.
#'
#' @param file Source path.
#' @param sheet Optional sheet name or number for Excel files.
#' @return Data frame of character columns named A, B, ...
finn_tx_read_grid <- function(file, sheet = NULL) {
  if (is.null(file) || !file.exists(file)) stop("Data file not found: ", file, call. = FALSE)
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("xlsx", "xls")) {
    finn_require("readxl")
    if (!is.null(sheet) && grepl("^[0-9]+$", sheet)) sheet <- as.integer(sheet)
    g <- suppressMessages(readxl::read_excel(
      file,
      sheet = sheet %||% 1L, col_names = FALSE, col_types = "text",
      range = readxl::cell_limits(c(1, 1), c(NA, NA)), .name_repair = "minimal"
    ))
    g <- as.data.frame(g, stringsAsFactors = FALSE)
  } else if (ext %in% c("csv", "txt")) {
    sep <- finn_sniff_delimiter(file)
    bytes <- readBin(file, "raw", file.info(file)$size)
    enc <- if (validUTF8(rawToChar(bytes[bytes != as.raw(0)]))) "UTF-8-BOM" else "latin1"
    width <- suppressWarnings(max(utils::count.fields(file, sep = sep, quote = "\"", blank.lines.skip = FALSE, comment.char = ""), na.rm = TRUE))
    if (!is.finite(width) || width < 1) stop("The file has no readable columns.", call. = FALSE)
    g <- utils::read.table(
      file,
      sep = sep, quote = "\"", header = FALSE, colClasses = "character", fill = TRUE,
      blank.lines.skip = FALSE, na.strings = character(), comment.char = "",
      col.names = paste0("V", seq_len(width)), fileEncoding = enc, check.names = FALSE
    )
  } else {
    stop("Unsupported file type '.", ext, "'. Use CSV or Excel.", call. = FALSE)
  }
  if (!ncol(g)) stop("The source has no columns.", call. = FALSE)
  g[] <- lapply(g, function(v) {
    v <- as.character(v)
    v[!is.na(v)] <- trimws(v[!is.na(v)])
    v
  })
  names(g) <- finn_tx_letters(ncol(g))
  g
}

#' Load the source named by a spec into a lineage-tracked context
#'
#' Fully empty rows and columns are removed and recorded as explained drops.
#' Several sheets are stacked when `source.sheets` is given; row labels then
#' take the form `Sheet!n`.
#'
#' @param file Source path.
#' @param spec Parsed spec.
#' @param sheet Default sheet from the command line.
#' @return Context environment.
finn_tx_load <- function(file, spec, sheet = NULL) {
  sheets <- finn_tx_chr(spec$source$sheets)
  one <- spec$source$sheet %||% sheet
  if (length(sheets) && !tolower(tools::file_ext(file)) %in% c("xlsx", "xls")) {
    stop("source.sheets works only with Excel files.", call. = FALSE)
  }
  grids <- if (length(sheets)) {
    lapply(sheets, function(s) {
      g <- finn_tx_read_grid(file, s)
      g$.finn_row <- paste0(s, "!", seq_len(nrow(g)))
      g$.finn_sheet <- s
      g
    })
  } else {
    g <- finn_tx_read_grid(file, one)
    g$.finn_row <- as.character(seq_len(nrow(g)))
    list(g)
  }
  letters_all <- finn_tx_letters(max(vapply(grids, function(g) sum(!startsWith(names(g), ".")), numeric(1))))
  grids <- lapply(grids, function(g) {
    for (l in setdiff(letters_all, names(g))) g[[l]] <- NA_character_
    g[c(letters_all, setdiff(names(g), letters_all))]
  })
  grid <- do.call(rbind, grids)
  rownames(grid) <- NULL
  ctx <- new.env(parent = emptyenv())
  ctx$file <- file
  ctx$md5 <- unname(tools::md5sum(file))
  ctx$grid <- grid
  ctx$drops <- list()
  ctx$coldrops <- list()
  ctx$totals <- list()
  ctx$changes <- list()
  ctx$confirm <- character()
  ctx$counts <- list(filled = 0, scaled = 0, derived = 0, aggregated = 0, unit_converted = 0, parsed_suffix = 0)
  ctx$parse <- list()
  ctx$date_src <- NULL
  ctx$step <- 0L
  d <- grid[c(letters_all, ".finn_row")]
  for (l in letters_all) d[[paste0(".lin.", l)]] <- paste0(grid$.finn_row, ":", l)
  ctx$colsrc <- stats::setNames(as.list(letters_all), letters_all)
  empty_cols <- letters_all[vapply(letters_all, function(l) all(finn_tx_blank(d[[l]])), logical(1))]
  ctx$d <- d
  if (length(empty_cols)) finn_tx_drop_cols(ctx, empty_cols, "empty column")
  sheet_col <- spec$source$sheet_column
  if (!is.null(sheet_col)) {
    if (!length(sheets)) stop("source.sheet_column needs source.sheets.", call. = FALSE)
    ctx$d[[sheet_col]] <- grid$.finn_sheet
    ctx$d[[paste0(".lin.", sheet_col)]] <- paste0(grid$.finn_row, ":sheet")
    ctx$colsrc[[sheet_col]] <- character()
  }
  value_cols <- finn_tx_cols(ctx$d)
  blank_row <- Reduce(`&`, lapply(value_cols, function(v) finn_tx_blank(ctx$d[[v]])), TRUE)
  if (any(blank_row)) finn_tx_keep(ctx, !blank_row, "empty row")
  ctx
}

#' User-visible columns of a working table
#'
#' @param d Working data frame.
#' @return Column names that do not start with a dot.
finn_tx_cols <- function(d) names(d)[!startsWith(names(d), ".")]

#' Keep rows of the working table and record the rest as explained drops
#'
#' @param ctx Context environment.
#' @param keep Logical vector over current rows.
#' @param reason Plain-language reason shown to the user.
#' @return Number of dropped rows, invisibly.
finn_tx_keep <- function(ctx, keep, reason) {
  keep[is.na(keep)] <- FALSE
  dropped <- ctx$d$.finn_row[!keep]
  if (length(dropped)) {
    ctx$drops[[length(ctx$drops) + 1]] <- list(step = ctx$step, reason = reason, rows = unlist(strsplit(dropped, ";", fixed = TRUE)))
  }
  ctx$d <- ctx$d[keep, , drop = FALSE]
  rownames(ctx$d) <- NULL
  invisible(length(dropped))
}

#' Drop columns and record the source columns they held
#'
#' @param ctx Context environment.
#' @param cols Current column names.
#' @param reason Plain-language reason.
#' @return NULL, invisibly.
finn_tx_drop_cols <- function(ctx, cols, reason) {
  finn_tx_need_cols(ctx, cols)
  ctx$coldrops[[length(ctx$coldrops) + 1]] <- list(step = ctx$step, reason = reason, columns = cols, letters = unique(unlist(ctx$colsrc[cols])))
  ctx$d <- ctx$d[setdiff(names(ctx$d), c(cols, paste0(".lin.", cols)))]
  ctx$colsrc[cols] <- NULL
  ctx$parse[cols] <- NULL
  invisible(NULL)
}

#' Stop when columns are not in the working table
#'
#' @param ctx Context environment.
#' @param cols Column names.
#' @return NULL, invisibly.
finn_tx_need_cols <- function(ctx, cols) {
  miss <- setdiff(cols, finn_tx_cols(ctx$d))
  if (length(miss)) {
    stop(sprintf(
      "Step %d: column(s) %s not found. Current columns: %s.", ctx$step,
      paste(miss, collapse = ", "), paste(utils::head(finn_tx_cols(ctx$d), 40), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' Expand row references such as 3, "5-20", or "Sheet!5-20"
#'
#' @param rows Row references from a spec.
#' @return Character vector of source row labels.
finn_tx_expand_rows <- function(rows) {
  out <- character()
  for (r in finn_tx_chr(rows)) {
    prefix <- if (grepl("!", r, fixed = TRUE)) sub("!.*$", "!", r) else ""
    body <- sub("^.*!", "", r)
    if (grepl("^[0-9]+-[0-9]+$", body)) {
      b <- as.integer(strsplit(body, "-", fixed = TRUE)[[1]])
      out <- c(out, paste0(prefix, seq(b[1], b[2])))
    } else if (grepl("^[0-9]+$", body)) {
      out <- c(out, paste0(prefix, as.integer(body)))
    } else {
      stop("Row reference '", r, "' is not a row number or range such as 5-20.", call. = FALSE)
    }
  }
  out
}

#' Rows of the working table that match a condition
#'
#' @param ctx Context environment.
#' @param cond List with `column` and one of `equals`, `in`, `matches`, or
#'   `blank`.
#' @return Logical vector over current rows.
finn_tx_match <- function(ctx, cond) {
  col <- finn_tx_chr(cond$column)
  finn_tx_need_cols(ctx, col)
  v <- as.character(ctx$d[[col]])
  if (!is.null(cond$equals)) {
    return(!is.na(v) & v == finn_tx_chr(cond$equals)[1])
  }
  if (!is.null(cond$`in`)) {
    return(!is.na(v) & v %in% finn_tx_chr(cond$`in`))
  }
  if (!is.null(cond$matches)) {
    return(!is.na(v) & grepl(finn_tx_chr(cond$matches)[1], v, perl = TRUE))
  }
  if (isTRUE(cond$blank)) {
    return(finn_tx_blank(v))
  }
  stop(sprintf("Step %d: give equals, in, matches, or blank: true.", ctx$step), call. = FALSE)
}

#' Record a change for the plan table
#'
#' @param ctx Context environment.
#' @param op Operation name.
#' @param what Plain-language description.
#' @param affects Short count such as "12 rows"; "1 rows" is written "1 row".
#' @param confirm TRUE when the change drops, combines, fills, rescales, or
#'   derives data and needs explicit confirmation.
#' @return NULL, invisibly.
finn_tx_change <- function(ctx, op, what, affects, confirm = FALSE) {
  affects <- gsub("(^|[^0-9.,])1 (row|column|value)s\\b", "\\11 \\2", affects, perl = TRUE)
  ctx$changes[[length(ctx$changes) + 1]] <- list(step = ctx$step, op = op, change = what, affects = affects, confirm = isTRUE(confirm))
  if (isTRUE(confirm)) ctx$confirm <- c(ctx$confirm, sprintf("Step %d: %s (%s)", ctx$step, what, affects))
  invisible(NULL)
}

#' Rename working-table columns together with their lineage
#'
#' @param ctx Context environment.
#' @param from Current names.
#' @param to New names.
#' @return NULL, invisibly.
finn_tx_rename <- function(ctx, from, to) {
  if (!length(from)) {
    return(invisible(NULL))
  }
  bad <- to[startsWith(to, ".") | !nzchar(to)]
  if (length(bad)) stop(sprintf("Step %d: column names cannot be empty or start with a dot.", ctx$step), call. = FALSE)
  keep <- setdiff(finn_tx_cols(ctx$d), from)
  dup <- c(to[duplicated(to)], intersect(to, keep))
  if (length(dup)) stop(sprintf("Step %d: renaming would create duplicate column(s): %s.", ctx$step, paste(unique(dup), collapse = ", ")), call. = FALSE)
  nm <- names(ctx$d)
  i <- match(from, nm)
  j <- match(paste0(".lin.", from), nm)
  nm[i] <- to
  nm[j] <- paste0(".lin.", to)
  names(ctx$d) <- nm
  src <- ctx$colsrc[from]
  ctx$colsrc[from] <- NULL
  ctx$colsrc[to] <- src
  hit <- from %in% names(ctx$parse)
  if (any(hit)) {
    p <- ctx$parse[from[hit]]
    ctx$parse[from[hit]] <- NULL
    ctx$parse[to[hit]] <- p
  }
  if (!is.null(ctx$pivot_names) && ctx$pivot_names %in% from) ctx$pivot_names <- to[match(ctx$pivot_names, from)]
  invisible(NULL)
}

#' Mark a block of source rows
#'
#' @param ctx Context environment.
#' @param rows Row references.
#' @return Logical vector over current rows.
finn_tx_rows_hit <- function(ctx, rows) {
  want <- finn_tx_expand_rows(rows)
  have <- ctx$d$.finn_row
  miss <- setdiff(want, have)
  if (length(miss) == length(want)) stop(sprintf("Step %d: none of the rows %s are in the table (already dropped or out of range).", ctx$step, paste(utils::head(miss, 10), collapse = ", ")), call. = FALSE)
  have %in% want
}

#' Apply `keep_rows`: keep a block such as one table among several
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_keep_rows <- function(ctx, s) {
  hit <- finn_tx_rows_hit(ctx, s$rows)
  n <- finn_tx_keep(ctx, hit, s$reason)
  finn_tx_change(ctx, "keep_rows", sprintf("Kept only rows %s: %s", paste(finn_tx_chr(s$rows), collapse = ", "), s$reason), sprintf("%d rows dropped", n))
}

#' Apply `drop_rows`: drop listed rows
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_drop_rows <- function(ctx, s) {
  hit <- finn_tx_rows_hit(ctx, s$rows)
  n <- finn_tx_keep(ctx, !hit, s$reason)
  finn_tx_change(ctx, "drop_rows", sprintf("Dropped rows %s: %s", paste(finn_tx_chr(s$rows), collapse = ", "), s$reason), sprintf("%d rows", n))
}

#' Apply `drop_rows_where`: drop rows whose column matches a condition
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_drop_rows_where <- function(ctx, s) {
  hit <- finn_tx_match(ctx, s)
  if (!any(hit)) stop(sprintf("Step %d: no rows match the condition on '%s'.", ctx$step, s$column), call. = FALSE)
  n <- finn_tx_keep(ctx, !hit, s$reason)
  finn_tx_change(ctx, "drop_rows_where", sprintf("Dropped rows by a rule on '%s': %s", s$column, s$reason), sprintf("%d rows", n))
}

#' Apply `drop_blank_rows`: drop rows blank in the given (or all) columns
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_drop_blank_rows <- function(ctx, s) {
  cols <- finn_tx_chr(s$columns)
  if (!length(cols)) cols <- finn_tx_cols(ctx$d)
  finn_tx_need_cols(ctx, cols)
  blank <- Reduce(`&`, lapply(cols, function(v) is.na(ctx$d[[v]]) | finn_tx_blank(as.character(ctx$d[[v]]))), TRUE)
  reason <- s$reason %||% sprintf("blank in %s", paste(cols, collapse = ", "))
  n <- finn_tx_keep(ctx, !blank, reason)
  finn_tx_change(ctx, "drop_blank_rows", sprintf("Dropped rows that are blank in %s", paste(cols, collapse = ", ")), sprintf("%d rows", n))
}

#' Apply `use_header_rows`: take column names from one or more rows
#'
#' Upper header rows are filled to the right across merged cells and joined
#' with lower rows, so a Year row above a Month row becomes "2024 Jan". Rows
#' above the header (titles or a filter area) are dropped.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_use_header_rows <- function(ctx, s) {
  want <- finn_tx_expand_rows(s$rows)
  idx <- match(want, ctx$d$.finn_row)
  if (anyNA(idx)) stop(sprintf("Step %d: header row(s) %s are not in the table.", ctx$step, paste(want[is.na(idx)], collapse = ", ")), call. = FALSE)
  cols <- finn_tx_cols(ctx$d)
  pieces <- lapply(seq_along(idx), function(k) {
    v <- as.character(unlist(ctx$d[idx[k], cols]))
    v[finn_tx_blank(v)] <- NA
    if (k < length(idx)) {
      for (j in seq_along(v)[-1]) if (is.na(v[j])) v[j] <- v[j - 1]
    }
    v
  })
  sep <- s$sep %||% " "
  hdr <- vapply(seq_along(cols), function(j) {
    p <- vapply(pieces, `[`, character(1), j)
    paste(p[!is.na(p)], collapse = sep)
  }, character(1))
  unnamed <- !nzchar(hdr)
  hdr[unnamed] <- cols[unnamed]
  hdr <- sub("^\\.+", "", hdr)
  hdr[!nzchar(hdr)] <- cols[!nzchar(hdr)]
  dups <- sum(duplicated(hdr))
  hdr <- make.unique(hdr, sep = "_")
  above <- seq_len(nrow(ctx$d)) < min(idx) & !seq_len(nrow(ctx$d)) %in% idx
  n_above <- finn_tx_keep(ctx, !above, "title or filter area above the header")
  finn_tx_keep(ctx, !ctx$d$.finn_row %in% want, "header row")
  finn_tx_rename(ctx, cols, hdr)
  note <- if (dups) sprintf("; %d repeated names were made unique with _1, _2", dups) else ""
  if (any(unnamed)) note <- paste0(note, sprintf("; %d columns had no header and kept their letter", sum(unnamed)))
  finn_tx_change(ctx, "use_header_rows", sprintf("Used row(s) %s as column names%s", paste(want, collapse = ", "), note), sprintf("%d rows above dropped", n_above))
}

#' Apply `drop_columns`
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_drop_columns <- function(ctx, s) {
  cols <- finn_tx_chr(s$columns)
  finn_tx_drop_cols(ctx, cols, s$reason)
  finn_tx_change(ctx, "drop_columns", sprintf("Dropped column(s) %s: %s", paste(cols, collapse = ", "), s$reason), sprintf("%d columns", length(cols)))
}

#' Apply `rename_columns`
#'
#' @param ctx Context environment.
#' @param s Step list with `map` of old name to new name.
#' @return NULL, invisibly.
finn_tx_op_rename_columns <- function(ctx, s) {
  from <- names(s$map)
  to <- finn_tx_chr(s$map)
  if (!length(from) || length(from) != length(to)) stop(sprintf("Step %d: map must be an object such as {\"Old\": \"New\"}.", ctx$step), call. = FALSE)
  finn_tx_need_cols(ctx, from)
  finn_tx_rename(ctx, from, to)
  finn_tx_change(ctx, "rename_columns", paste(sprintf("'%s' -> '%s'", from, to), collapse = ", "), sprintf("%d columns", length(from)))
}

#' Apply `fill_down`: repeat labels into the blank cells below them
#'
#' For pivot outline layouts where a label is shown only on its first row.
#' Copies the lineage of the cell each label came from.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_fill_down <- function(ctx, s) {
  cols <- finn_tx_chr(s$columns)
  finn_tx_need_cols(ctx, cols)
  total <- 0
  for (col in cols) {
    if (!is.character(ctx$d[[col]]) || col %in% names(ctx$parse)) stop(sprintf("Step %d: fill_down is for labels; use fill_values for numbers in '%s'.", ctx$step, col), call. = FALSE)
    v <- ctx$d[[col]]
    lin <- ctx$d[[paste0(".lin.", col)]]
    blank <- finn_tx_blank(v)
    src <- cummax(ifelse(blank, 0L, seq_along(v)))
    fill <- blank & src > 0
    v[fill] <- v[src[fill]]
    lin[fill] <- lin[src[fill]]
    ctx$d[[col]] <- v
    ctx$d[[paste0(".lin.", col)]] <- lin
    total <- total + sum(fill)
  }
  finn_tx_change(ctx, "fill_down", sprintf("Repeated labels down into blank cells of %s", paste(cols, collapse = ", ")), sprintf("%d cells", total))
}

#' Apply `split_compact_labels`: split a compact pivot label column
#'
#' In a compact pivot, group labels sit on their own rows (with no values)
#' above the items they contain. Group rows are found as rows where every
#' other column is blank, or are listed in `parent_rows`. A grand total row
#' keeps its own label as the group, so it is reconciled against every item.
#'
#' @param ctx Context environment.
#' @param s Step list with `column`, `into` (group name, item name).
#' @return NULL, invisibly.
finn_tx_op_split_compact_labels <- function(ctx, s) {
  col <- s$column
  into <- finn_tx_chr(s$into)
  finn_tx_need_cols(ctx, col)
  if (length(into) != 2) stop(sprintf("Step %d: into needs two names, for the group and the item.", ctx$step), call. = FALSE)
  others <- setdiff(finn_tx_cols(ctx$d), col)
  parent <- if (!is.null(s$parent_rows)) {
    finn_tx_rows_hit(ctx, s$parent_rows)
  } else {
    !finn_tx_blank(ctx$d[[col]]) & Reduce(`&`, lapply(others, function(v) finn_tx_blank(as.character(ctx$d[[v]]))), TRUE)
  }
  if (!any(parent)) stop(sprintf("Step %d: found no group rows in '%s'.", ctx$step, col), call. = FALSE)
  if (!parent[1]) stop(sprintf("Step %d: the first row of '%s' is not a group label; drop rows above it first.", ctx$step, col), call. = FALSE)
  src <- cummax(ifelse(parent, seq_len(nrow(ctx$d)), 0L))
  lin <- ctx$d[[paste0(".lin.", col)]]
  ctx$d[[into[1]]] <- ctx$d[[col]][src]
  ctx$d[[paste0(".lin.", into[1])]] <- lin[src]
  labels <- trimws(as.character(ctx$d[[col]]))
  grand <- !is.na(labels) & grepl(finn_tx_total_regex(), labels, ignore.case = TRUE, perl = TRUE) & !nzchar(finn_tx_total_stem(labels))
  ctx$d[[into[1]]][grand] <- ctx$d[[col]][grand]
  ctx$d[[paste0(".lin.", into[1])]][grand] <- lin[grand]
  ctx$colsrc[[into[1]]] <- ctx$colsrc[[col]]
  if (into[2] != col) finn_tx_rename(ctx, col, into[2])
  ord <- c(into[1], setdiff(names(ctx$d), into[1]))
  ctx$d <- ctx$d[ord]
  n <- finn_tx_keep(ctx, !parent, "compact pivot group label row")
  finn_tx_change(ctx, "split_compact_labels", sprintf("Split '%s' into group '%s' and item '%s'", col, into[1], into[2]), sprintf("%d group rows dropped", n))
}

#' Label stem of a total row, such as "East" for "East Total"
#'
#' @param x Character vector of labels.
#' @return Stems; "" for grand totals.
finn_tx_total_stem <- function(x) {
  x <- trimws(x)
  out <- sub("(?i)\\s*\\b(sub-?)?total$", "", x, perl = TRUE)
  out <- sub("(?i)^(grand\\s+)?(sub-?)?total\\b\\s*:?\\s*", "", out, perl = TRUE)
  out[grepl("(?i)^(grand\\s+total|total|subtotal|sub-total|sum|all)$", x, perl = TRUE) | grepl("(?i)^grand$", out, perl = TRUE)] <- ""
  trimws(out)
}

#' Numbers held in a working column, for reconciliation
#'
#' Uses the column's parsed values when it has been parsed, otherwise reads
#' text with the skill's number parser and K/M/B suffixes.
#'
#' @param ctx Context environment.
#' @param col Column name.
#' @return Numeric vector.
finn_tx_numbers <- function(ctx, col) {
  v <- ctx$d[[col]]
  if (is.numeric(v)) {
    return(v)
  }
  finn_tx_parse_text_numbers(as.character(v))$value
}

#' Apply `drop_total_rows`: drop total and subtotal rows after reconciling them
#'
#' A row is a total when one of `columns` holds a label such as "Total",
#' "Grand Total", or "East Total". Each total is compared with the sum of the
#' non-total rows it covers: rows that match it on earlier label columns and,
#' for "East Total", have "East" in that column. Mismatches fail the gate
#' unless `reconcile` is false.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @param tol Tolerance list from the spec.
#' @return NULL, invisibly.
finn_tx_op_drop_total_rows <- function(ctx, s, tol) {
  cols <- finn_tx_chr(s$columns)
  finn_tx_need_cols(ctx, cols)
  is_tot <- lapply(cols, function(v) {
    x <- as.character(ctx$d[[v]])
    !is.na(x) & grepl(finn_tx_total_regex(), trimws(x), ignore.case = TRUE, perl = TRUE)
  })
  any_tot <- Reduce(`|`, is_tot)
  if (!any(any_tot)) stop(sprintf("Step %d: no total rows found in %s.", ctx$step, paste(cols, collapse = ", ")), call. = FALSE)
  vals <- finn_tx_chr(s$value_columns)
  if (!length(vals)) vals <- setdiff(finn_tx_cols(ctx$d), c(cols, ctx$pivot_names))
  finn_tx_need_cols(ctx, vals)
  nums <- lapply(stats::setNames(vals, vals), function(v) finn_tx_numbers(ctx, v))
  extra <- intersect(ctx$pivot_names, finn_tx_cols(ctx$d))
  leaf <- which(!any_tot)
  for (i in which(any_tot)) {
    c_i <- min(which(vapply(is_tot, `[`, logical(1), i)))
    label <- as.character(ctx$d[[cols[c_i]]][i])
    stem <- finn_tx_total_stem(label)
    grp <- leaf
    for (k in seq_len(c_i - 1)) {
      tv <- as.character(ctx$d[[cols[k]]][i])
      if (!finn_tx_blank(tv)) grp <- grp[as.character(ctx$d[[cols[k]]][grp]) %in% tv]
    }
    prior <- vapply(seq_len(c_i - 1), function(k) as.character(ctx$d[[cols[k]]][i]), character(1))
    if (nzchar(stem) && !stem %in% prior) grp <- grp[as.character(ctx$d[[cols[c_i]]][grp]) %in% stem]
    for (e in extra) grp <- grp[as.character(ctx$d[[e]][grp]) %in% as.character(ctx$d[[e]][i])]
    cmp <- do.call(rbind, lapply(vals, function(v) {
      t <- nums[[v]][i]
      if (is.na(t)) {
        return(NULL)
      }
      data.frame(column = v, total = t, leaves = sum(nums[[v]][grp], na.rm = TRUE), stringsAsFactors = FALSE)
    }))
    ctx$totals[[length(ctx$totals) + 1]] <- list(
      step = ctx$step, kind = "row", label = label, row = ctx$d$.finn_row[i], n_leaves = length(grp),
      compare = cmp, reconcile = !identical(s$reconcile, FALSE)
    )
  }
  n <- finn_tx_keep(ctx, !any_tot, s$reason %||% "total or subtotal row (would double count)")
  finn_tx_change(ctx, "drop_total_rows", sprintf("Dropped total/subtotal rows found in %s after comparing them with the rows they sum", paste(cols, collapse = ", ")), sprintf("%d rows", n))
}

#' Apply `drop_total_columns`: drop total columns after reconciling them
#'
#' Each total column is compared, row by row, with the sum of the `of`
#' columns. Without `of`, the columns whose names read as dates are used.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_drop_total_columns <- function(ctx, s) {
  cols <- finn_tx_chr(s$columns)
  finn_tx_need_cols(ctx, cols)
  of <- finn_tx_chr(s$of)
  if (!length(of)) {
    cand <- setdiff(finn_tx_cols(ctx$d), cols)
    of <- cand[!is.na(finn_header_dates(cand))]
    if (!length(of)) stop(sprintf("Step %d: say which columns each total sums with \"of\".", ctx$step), call. = FALSE)
  }
  finn_tx_need_cols(ctx, of)
  parts <- Reduce(`+`, lapply(of, function(v) {
    x <- finn_tx_numbers(ctx, v)
    x[is.na(x)] <- 0
    x
  }))
  for (col in cols) {
    t <- finn_tx_numbers(ctx, col)
    ok <- !is.na(t)
    cmp <- data.frame(column = col, row = ctx$d$.finn_row[ok], total = t[ok], leaves = parts[ok], stringsAsFactors = FALSE)
    ctx$totals[[length(ctx$totals) + 1]] <- list(
      step = ctx$step, kind = "column", label = col, row = NA_character_, n_leaves = length(of),
      compare = cmp, reconcile = !identical(s$reconcile, FALSE)
    )
  }
  finn_tx_drop_cols(ctx, cols, s$reason %||% "total column (would double count)")
  finn_tx_change(ctx, "drop_total_columns", sprintf("Dropped total column(s) %s after comparing them with %d columns", paste(cols, collapse = ", "), length(of)), sprintf("%d columns", length(cols)))
}

#' Apply `pivot_longer`: turn one column per period into rows
#'
#' Every column not in `id_columns` is stacked. Column names become the
#' `names_to` column (default "Date") and cell values the `values_to` column
#' (default "Value"). Blank cells stay blank rather than becoming zero.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_pivot_longer <- function(ctx, s) {
  ids <- finn_tx_chr(s$id_columns)
  finn_tx_need_cols(ctx, ids)
  names_to <- s$names_to %||% "Date"
  values_to <- s$values_to %||% "Value"
  vals <- setdiff(finn_tx_cols(ctx$d), ids)
  if (!length(vals)) stop(sprintf("Step %d: no columns left to pivot.", ctx$step), call. = FALSE)
  if (any(vals %in% names(ctx$parse))) stop(sprintf("Step %d: run pivot_longer before parse_numbers.", ctx$step), call. = FALSE)
  if (any(c(names_to, values_to) %in% ids)) stop(sprintf("Step %d: names_to and values_to must be new column names.", ctx$step), call. = FALSE)
  n <- nrow(ctx$d)
  k <- length(vals)
  r <- rep(seq_len(n), each = k)
  out <- ctx$d[r, c(ids, paste0(".lin.", ids), ".finn_row"), drop = FALSE]
  vals_src <- vapply(vals, function(v) paste(ctx$colsrc[[v]], collapse = "+"), character(1))
  out[[names_to]] <- rep(vals, times = n)
  out[[paste0(".lin.", names_to)]] <- rep(paste0("h:", vals_src), times = n)
  m <- as.matrix(ctx$d[vals])
  lm <- as.matrix(ctx$d[paste0(".lin.", vals)])
  out[[values_to]] <- as.character(as.vector(t(m)))
  out[[paste0(".lin.", values_to)]] <- as.vector(t(lm))
  rownames(out) <- NULL
  letters_used <- unique(unlist(ctx$colsrc[vals]))
  ctx$colsrc[vals] <- NULL
  ctx$colsrc[[names_to]] <- letters_used
  ctx$colsrc[[values_to]] <- letters_used
  ctx$d <- out[c(ids, names_to, values_to, setdiff(names(out), c(ids, names_to, values_to)))]
  ctx$pivot_names <- names_to
  ctx$date_src <- if (identical(names_to, "Date")) "header" else ctx$date_src
  finn_tx_change(ctx, "pivot_longer", sprintf("Turned %d period columns into rows ('%s' and '%s')", k, names_to, values_to), sprintf("%d rows -> %d rows", n, nrow(out)))
}

#' Default text that means "no value" in a number column
#'
#' Dashes are not included because finance exports often use them for zero;
#' list them in `missing` when they mean "no value".
#'
#' @return Lower-case tokens.
finn_tx_missing_tokens <- function() c("", "n/a", "na", "#n/a", "null", "none")

#' Read number text, including K/M/B suffixes
#'
#' @param x Character vector.
#' @param decimal_mark Optional "." or ",".
#' @param missing Tokens that mean no value (compared in lower case).
#' @param suffixes TRUE to read 1.2K, 3M, 2B, 2bn, and 4MM.
#' @return List with `value`, `suffix` (multiplier per cell), `missing`,
#'   `bad` (text that is not a number), and `mark`.
finn_tx_parse_text_numbers <- function(x, decimal_mark = NULL, missing = finn_tx_missing_tokens(), suffixes = TRUE) {
  x <- as.character(x)
  t <- trimws(x)
  is_missing <- is.na(t) | tolower(t) %in% tolower(missing)
  body <- t
  mult <- rep(1, length(t))
  if (isTRUE(suffixes)) {
    m <- regmatches(t, regexec("^(.*?[0-9.,)])\\s*(?i)(k|mm|m|bn|b)\\)?$", t, perl = TRUE))
    hit <- lengths(m) == 3
    if (any(hit)) {
      suf <- tolower(vapply(m[hit], `[`, character(1), 3))
      body[hit] <- vapply(m[hit], `[`, character(1), 2)
      body[hit] <- ifelse(grepl("^\\(", body[hit]) & !grepl("\\)$", body[hit]), paste0(body[hit], ")"), body[hit])
      mult[hit] <- c(k = 1e3, m = 1e6, mm = 1e6, b = 1e9, bn = 1e9)[suf]
    }
  }
  body[is_missing] <- NA
  value <- finn_parse_number(body, decimal_mark)
  mark <- attr(value, "decimal_mark") %||% decimal_mark %||% "."
  attr(value, "decimal_mark") <- NULL
  value <- value * mult
  bad <- !is_missing & is.na(value)
  list(value = value, suffix = mult, missing = is_missing & !is.na(t) & nzchar(t), bad = bad, mark = mark)
}

#' Mask a value's characters, keeping its shape
#'
#' Digits become "#", letters "x"; punctuation is kept.
#'
#' @param x Character vector.
#' @return Character vector.
finn_tx_shape <- function(x) {
  x <- gsub("[0-9]", "#", as.character(x))
  gsub("[[:alpha:]]", "x", x)
}

#' Apply `parse_numbers`: turn number text into numbers
#'
#' Reads currency, thousands separators, parentheses negatives, and K/M/B
#' suffixes. Text listed in `missing` becomes blank; any other text that is
#' not a number stops the step.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_parse_numbers <- function(ctx, s) {
  cols <- finn_tx_chr(s$columns)
  finn_tx_need_cols(ctx, cols)
  missing <- unique(c(finn_tx_missing_tokens(), finn_tx_chr(s$missing)))
  suffixes <- !identical(s$suffixes, FALSE)
  n_missing <- 0
  n_suffix <- 0
  for (col in cols) {
    v <- ctx$d[[col]]
    if (is.numeric(v)) next
    p <- finn_tx_parse_text_numbers(v, s$decimal_mark, missing, suffixes)
    if (any(p$bad)) {
      rows <- ctx$d$.finn_row[p$bad]
      stop(sprintf(
        "Step %d: %d values in '%s' are not numbers (rows %s; shapes %s). List no-value text in missing, or drop those rows.",
        ctx$step, sum(p$bad), col, paste(utils::head(rows, 5), collapse = ", "),
        paste(unique(utils::head(finn_tx_shape(v[p$bad]), 5)), collapse = ", ")
      ), call. = FALSE)
    }
    ctx$d[[col]] <- p$value
    ctx$parse[[col]] <- list(decimal_mark = p$mark, missing = missing, suffixes = suffixes)
    n_missing <- n_missing + sum(p$missing)
    n_suffix <- n_suffix + sum(p$suffix != 1)
  }
  ctx$counts$parsed_suffix <- ctx$counts$parsed_suffix + n_suffix
  extra <- c(
    if (n_suffix) sprintf("%d K/M/B values expanded", n_suffix),
    if (n_missing) sprintf("%d no-value entries left blank", n_missing)
  )
  finn_tx_change(ctx, "parse_numbers", sprintf("Read %s as numbers%s", paste(cols, collapse = ", "), if (length(extra)) paste0(" (", paste(extra, collapse = "; "), ")") else ""), sprintf("%d columns", length(cols)))
}

#' Date formats tried when no format is given
#'
#' @return Character vector of formats and the special readers
#'   "excel_serial", "quarter", and "%Y".
finn_tx_date_formats <- function() {
  c(
    "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y", "%m-%d-%Y", "%d.%m.%Y",
    "%m/%d/%y", "%d/%m/%y", "%d.%m.%y", "%Y%m%d", "%d-%b-%Y", "%d-%b-%y", "%d %b %Y", "%b %d, %Y",
    "%b %Y", "%B %Y", "%Y %b", "%Y %B", "%b-%y", "%b-%Y", "%Y-%m", "%m/%Y", "%Y/%m",
    "excel_serial", "quarter", "%Y"
  )
}

#' Regular expression that a value must fully match for a date format
#'
#' Prevents partial reads such as "%y" taking the first two digits of a year.
#'
#' @param f A strptime format.
#' @return Anchored regular expression.
finn_tx_format_regex <- function(f) {
  tokens <- c("%Y" = "[0-9]{4}", "%y" = "[0-9]{2}", "%m" = "[0-9]{1,2}", "%d" = "[0-9]{1,2}", "%b" = "[A-Za-z]{3}\\.?", "%B" = "[A-Za-z]+", "%j" = "[0-9]{1,3}")
  parts <- regmatches(f, gregexpr("%[A-Za-z]|[^%]+", f))[[1]]
  rx <- vapply(parts, function(p) if (p %in% names(tokens)) tokens[[p]] else gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", p, perl = TRUE), character(1))
  paste0("^", paste(rx, collapse = ""), "$")
}

#' Read dates with one format or special reader
#'
#' Formats without a day read as the first of the month. Years outside
#' 1900-2200 are rejected.
#'
#' @param x Character vector.
#' @param f Format or "excel_serial", "quarter", or "%Y".
#' @return Date vector, NA where the value does not match.
finn_tx_date_try <- function(x, f) {
  x <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  if (identical(f, "excel_serial")) {
    ok <- grepl("^[0-9]{5}(\\.0+)?$", x)
    out[ok] <- as.Date(as.numeric(x[ok]), origin = "1899-12-30")
  } else if (identical(f, "quarter")) {
    m <- regmatches(x, regexec("^(?i)(?:([0-9]{4})\\s*-?\\s*q([1-4])|q([1-4])\\s*-?\\s*([0-9]{4}))$", x, perl = TRUE))
    ok <- lengths(m) == 5
    if (any(ok)) {
      y <- vapply(m[ok], function(g) if (nzchar(g[2])) g[2] else g[5], character(1))
      q <- vapply(m[ok], function(g) if (nzchar(g[3])) g[3] else g[4], character(1))
      out[ok] <- as.Date(sprintf("%s-%02d-01", y, (as.integer(q) - 1L) * 3L + 1L))
    }
  } else if (identical(f, "%Y")) {
    ok <- grepl("^[0-9]{4}$", x)
    if (any(ok)) out[ok] <- as.Date(paste0(x[ok], "-01-01"))
  } else {
    ok <- grepl(finn_tx_format_regex(f), x, perl = TRUE)
    if (any(ok)) {
      out[ok] <- if (grepl("%d|%j", f)) as.Date(x[ok], format = f) else as.Date(paste0(x[ok], "|01"), format = paste0(f, "|%d"))
    }
  }
  yr <- as.integer(format(out, "%Y"))
  out[!is.na(yr) & (yr < 1900 | yr > 2200)] <- NA
  out
}

#' Read a date vector value by value with several formats
#'
#' A value read differently by two formats is ambiguous and is reported.
#'
#' @param x Character vector.
#' @param formats Formats to try.
#' @return List with `value`, `bad` (unreadable), and `ambiguous` (logical).
finn_tx_parse_dates <- function(x, formats) {
  res <- lapply(formats, function(f) finn_tx_date_try(x, f))
  m <- do.call(cbind, lapply(res, as.numeric))
  if (is.null(dim(m))) m <- matrix(m, ncol = 1)
  first <- apply(m, 1, function(r) if (all(is.na(r))) NA_real_ else r[!is.na(r)][1])
  amb <- apply(m, 1, function(r) length(unique(r[!is.na(r)])) > 1)
  blank <- finn_tx_blank(x)
  list(value = as.Date(first, origin = "1970-01-01"), bad = is.na(first) & !blank, ambiguous = amb & !blank)
}

#' Apply `parse_dates`: read a date column
#'
#' Without `format`, tries common formats value by value, so mixed formats
#' are read; a value that two formats read differently (such as 01/02/2024)
#' stops the step until a format is given.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_parse_dates <- function(ctx, s) {
  col <- s$column
  finn_tx_need_cols(ctx, col)
  v <- ctx$d[[col]]
  if (inherits(v, "Date")) {
    return(invisible(NULL))
  }
  formats <- c(finn_tx_chr(s$format), finn_tx_chr(s$formats))
  if (!length(formats)) formats <- finn_tx_date_formats()
  p <- finn_tx_parse_dates(v, formats)
  if (any(p$ambiguous)) {
    stop(sprintf(
      "Step %d: %d dates in '%s' read two ways, for example shape %s (rows %s). Give format, such as \"%%d/%%m/%%Y\".",
      ctx$step, sum(p$ambiguous), col, finn_tx_shape(v[p$ambiguous][1]), paste(utils::head(ctx$d$.finn_row[p$ambiguous], 5), collapse = ", ")
    ), call. = FALSE)
  }
  if (any(p$bad)) {
    stop(sprintf(
      "Step %d: %d values in '%s' are not dates (rows %s; shapes %s).", ctx$step, sum(p$bad), col,
      paste(utils::head(ctx$d$.finn_row[p$bad], 5), collapse = ", "), paste(unique(utils::head(finn_tx_shape(v[p$bad]), 5)), collapse = ", ")
    ), call. = FALSE)
  }
  ctx$d[[col]] <- p$value
  ctx$parse[[col]] <- NULL
  if (identical(col, "Date") && !identical(ctx$date_src, "header")) ctx$date_src <- "cell"
  used <- formats[vapply(formats, function(f) any(!is.na(finn_tx_date_try(v, f))), logical(1))]
  finn_tx_change(ctx, "parse_dates", sprintf("Read '%s' as dates (%s)", col, paste(utils::head(used, 4), collapse = ", ")), sprintf("%d values", sum(!is.na(p$value))))
}

#' Apply `fiscal_to_date`: turn fiscal period labels into calendar dates
#'
#' `pattern` is a regular expression with named groups `year` and `period`,
#' for example "FY(?<year>[0-9]{2,4})\\s*P(?<period>[0-9]{1,2})". With
#' `year_is` "end" (default) FY24 is the fiscal year that ends in 2024.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_fiscal_to_date <- function(ctx, s) {
  col <- s$column
  finn_tx_need_cols(ctx, col)
  fys <- as.integer(s$fiscal_year_start)
  if (is.na(fys) || fys < 1 || fys > 12) stop(sprintf("Step %d: fiscal_year_start must be 1-12.", ctx$step), call. = FALSE)
  type <- s$period_type %||% "month"
  if (!type %in% c("month", "quarter")) stop(sprintf("Step %d: period_type must be month or quarter.", ctx$step), call. = FALSE)
  year_is <- s$year_is %||% "end"
  if (!year_is %in% c("end", "start")) stop(sprintf("Step %d: year_is must be end or start.", ctx$step), call. = FALSE)
  x <- trimws(as.character(ctx$d[[col]]))
  m <- regexpr(s$pattern, x, perl = TRUE)
  st <- attr(m, "capture.start")
  ln <- attr(m, "capture.length")
  if (is.null(st) || !all(c("year", "period") %in% colnames(st))) stop(sprintf("Step %d: pattern needs named groups (?<year>...) and (?<period>...).", ctx$step), call. = FALSE)
  grab <- function(g) ifelse(m > 0, substring(x, st[, g], st[, g] + ln[, g] - 1), NA)
  yr <- suppressWarnings(as.integer(grab("year")))
  pd <- suppressWarnings(as.integer(grab("period")))
  yr <- ifelse(!is.na(yr) & yr < 100, yr + 2000L, yr)
  maxp <- if (type == "month") 12L else 4L
  bad <- !finn_tx_blank(x) & (is.na(yr) | is.na(pd) | pd < 1 | pd > maxp)
  if (any(bad)) stop(sprintf("Step %d: %d values in '%s' do not match the fiscal pattern (rows %s).", ctx$step, sum(bad), col, paste(utils::head(ctx$d$.finn_row[bad], 5), collapse = ", ")), call. = FALSE)
  start_year <- if (year_is == "end" && fys > 1) yr - 1L else yr
  offset <- (fys - 1L) + (pd - 1L) * (if (type == "month") 1L else 3L)
  out <- as.Date(ifelse(is.na(yr), NA_character_, sprintf("%04d-%02d-01", start_year + offset %/% 12L, offset %% 12L + 1L)))
  into <- s$into %||% col
  ctx$d[[into]] <- out
  if (into != col) {
    ctx$d[[paste0(".lin.", into)]] <- ctx$d[[paste0(".lin.", col)]]
    ctx$colsrc[[into]] <- ctx$colsrc[[col]]
  }
  if (identical(into, "Date") && !identical(ctx$date_src, "header")) ctx$date_src <- "cell"
  finn_tx_change(ctx, "fiscal_to_date", sprintf("Converted fiscal %ss in '%s' to calendar dates (fiscal year starts in month %d, FY named by its %s year)", type, col, fys, year_is), sprintf("%d values", sum(!is.na(out))))
}

#' Apply `split_column`: split one column into several
#'
#' @param ctx Context environment.
#' @param s Step list with `sep` (fixed text) or `pattern` (regex with one
#'   group per new column).
#' @return NULL, invisibly.
finn_tx_op_split_column <- function(ctx, s) {
  col <- s$column
  finn_tx_need_cols(ctx, col)
  into <- finn_tx_chr(s$into)
  x <- as.character(ctx$d[[col]])
  parts <- if (!is.null(s$pattern)) {
    m <- regmatches(x, regexec(s$pattern, x, perl = TRUE))
    lapply(m, function(g) if (length(g) == length(into) + 1) g[-1] else rep(NA_character_, length(into)))
  } else {
    lapply(strsplit(ifelse(is.na(x), "", x), s$sep %||% " ", fixed = TRUE), function(p) {
      if (length(p) == length(into)) trimws(p) else rep(NA_character_, length(into))
    })
  }
  bad <- vapply(parts, function(p) anyNA(p), logical(1)) & !finn_tx_blank(x)
  if (any(bad)) stop(sprintf("Step %d: %d values in '%s' do not split into %d parts (rows %s).", ctx$step, sum(bad), col, length(into), paste(utils::head(ctx$d$.finn_row[bad], 5), collapse = ", ")), call. = FALSE)
  for (k in seq_along(into)) {
    if (into[k] %in% setdiff(finn_tx_cols(ctx$d), col)) stop(sprintf("Step %d: column '%s' already exists.", ctx$step, into[k]), call. = FALSE)
    ctx$d[[into[k]]] <- vapply(parts, `[`, character(1), k)
    ctx$d[[paste0(".lin.", into[k])]] <- ctx$d[[paste0(".lin.", col)]]
    ctx$colsrc[[into[k]]] <- ctx$colsrc[[col]]
  }
  if (!isTRUE(s$keep) && !col %in% into) {
    ctx$d <- ctx$d[setdiff(names(ctx$d), c(col, paste0(".lin.", col)))]
    ctx$colsrc[[col]] <- NULL
  }
  finn_tx_change(ctx, "split_column", sprintf("Split '%s' into %s", col, paste(into, collapse = ", ")), sprintf("%d rows", nrow(ctx$d)))
}

#' Apply `combine_columns`: join several columns into one label
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_combine_columns <- function(ctx, s) {
  cols <- finn_tx_chr(s$columns)
  finn_tx_need_cols(ctx, cols)
  into <- s$into
  if (into %in% setdiff(finn_tx_cols(ctx$d), cols)) stop(sprintf("Step %d: column '%s' already exists.", ctx$step, into), call. = FALSE)
  vals <- lapply(cols, function(v) ifelse(is.na(ctx$d[[v]]), "", as.character(ctx$d[[v]])))
  lins <- lapply(cols, function(v) ctx$d[[paste0(".lin.", v)]])
  src <- unique(unlist(ctx$colsrc[cols]))
  new <- do.call(paste, c(vals, sep = s$sep %||% " - "))
  new_lin <- do.call(paste, c(lins, sep = ";"))
  if (!isTRUE(s$keep)) {
    ctx$d <- ctx$d[setdiff(names(ctx$d), c(cols, paste0(".lin.", cols)))]
    ctx$colsrc[cols] <- NULL
  }
  ctx$d[[into]] <- new
  ctx$d[[paste0(".lin.", into)]] <- new_lin
  ctx$colsrc[[into]] <- src
  finn_tx_change(ctx, "combine_columns", sprintf("Combined %s into '%s'", paste(cols, collapse = ", "), into), sprintf("%d rows", nrow(ctx$d)))
}

#' Multiply the factor recorded in lineage strings
#'
#' @param lin Lineage strings such as "4:C" or "4:C*0.001;5:C".
#' @param factor Multiplier.
#' @return Updated lineage strings.
finn_tx_lin_scale <- function(lin, factor) {
  u <- unique(lin)
  new <- vapply(u, function(l) {
    if (is.na(l) || !grepl(":", l, fixed = TRUE)) {
      return(l)
    }
    pieces <- strsplit(l, ";", fixed = TRUE)[[1]]
    f <- ifelse(grepl("*", pieces, fixed = TRUE), suppressWarnings(as.numeric(sub("^.*\\*", "", pieces))), 1) * factor
    base <- sub("\\*.*$", "", pieces)
    paste(ifelse(f == 1, base, paste0(base, "*", format(f, digits = 15))), collapse = ";")
  }, character(1))
  unname(new[match(lin, u)])
}

#' Apply `scale`: multiply a number column, for example to convert units
#'
#' @param ctx Context environment.
#' @param s Step list with optional `where` to scale only some rows.
#' @return NULL, invisibly.
finn_tx_op_scale <- function(ctx, s) {
  col <- s$column
  finn_tx_need_cols(ctx, col)
  if (!is.numeric(ctx$d[[col]])) stop(sprintf("Step %d: run parse_numbers on '%s' before scale.", ctx$step, col), call. = FALSE)
  f <- as.numeric(s$factor)
  if (!length(f) || is.na(f) || f == 0) stop(sprintf("Step %d: factor must be a non-zero number.", ctx$step), call. = FALSE)
  hit <- if (is.null(s$where)) rep(TRUE, nrow(ctx$d)) else finn_tx_match(ctx, s$where)
  if (!any(hit)) stop(sprintf("Step %d: no rows match where.", ctx$step), call. = FALSE)
  ctx$d[[col]][hit] <- ctx$d[[col]][hit] * f
  lc <- paste0(".lin.", col)
  ctx$d[[lc]][hit] <- finn_tx_lin_scale(ctx$d[[lc]][hit], f)
  key <- if (is.null(s$where)) "scaled" else "unit_converted"
  ctx$counts[[key]] <- ctx$counts[[key]] + sum(hit)
  finn_tx_change(ctx, "scale", sprintf("Multiplied '%s' by %s: %s", col, format(f, digits = 15), s$reason), sprintf("%d values", sum(hit)), confirm = TRUE)
}

#' Group key for a set of columns
#'
#' @param d Data frame.
#' @param keys Column names.
#' @return Character key per row.
finn_tx_key <- function(d, keys) {
  do.call(paste, c(lapply(keys, function(k) {
    v <- d[[k]]
    if (inherits(v, "Date")) format(v, "%Y-%m-%d") else ifelse(is.na(v), "\001", trimws(as.character(v)))
  }), sep = "\037"))
}

#' Apply `aggregate_duplicates`: add up rows that share the same keys
#'
#' Other columns must agree within each group; conflicting labels stop the
#' step rather than being silently picked.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_aggregate_duplicates <- function(ctx, s) {
  keys <- finn_tx_chr(s$keys)
  sums <- finn_tx_chr(s$columns)
  finn_tx_need_cols(ctx, c(keys, sums))
  for (v in sums) if (!is.numeric(ctx$d[[v]])) stop(sprintf("Step %d: run parse_numbers on '%s' first.", ctx$step, v), call. = FALSE)
  d <- ctx$d
  key <- finn_tx_key(d, keys)
  g <- match(key, unique(key))
  if (!anyDuplicated(g)) stop(sprintf("Step %d: no duplicate rows for %s.", ctx$step, paste(keys, collapse = ", ")), call. = FALSE)
  others <- setdiff(finn_tx_cols(d), c(keys, sums))
  for (v in others) {
    k2 <- paste(g, finn_tx_key(d, v))
    if (length(unique(k2)) != max(g)) stop(sprintf("Step %d: rows with the same %s disagree on '%s'. Drop or combine that column first.", ctx$step, paste(keys, collapse = ", "), v), call. = FALSE)
  }
  first <- !duplicated(g)
  out <- d[first, , drop = FALSE]
  gi <- g[first]
  out$.finn_row <- as.vector(tapply(d$.finn_row, g, paste, collapse = ";"))[gi]
  for (v in sums) {
    out[[v]] <- as.vector(tapply(d[[v]], g, function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)))[gi]
    out[[paste0(".lin.", v)]] <- as.vector(tapply(d[[paste0(".lin.", v)]], g, paste, collapse = ";"))[gi]
  }
  rownames(out) <- NULL
  merged <- nrow(d) - nrow(out)
  ctx$d <- out
  ctx$counts$aggregated <- ctx$counts$aggregated + merged
  finn_tx_change(ctx, "aggregate_duplicates", sprintf("Added up rows with the same %s: %s", paste(keys, collapse = ", "), s$reason), sprintf("%d rows merged", merged), confirm = TRUE)
}

#' Apply `drop_duplicates`: remove repeated rows
#'
#' With `keep` "identical_only" (default) only exact copies are removed and
#' conflicting rows stop the step. "first" or "last" keeps one row per key
#' and needs extra confirmation.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_drop_duplicates <- function(ctx, s) {
  keys <- finn_tx_chr(s$keys)
  finn_tx_need_cols(ctx, keys)
  keep <- s$keep %||% "identical_only"
  if (!keep %in% c("identical_only", "first", "last")) stop(sprintf("Step %d: keep must be identical_only, first, or last.", ctx$step), call. = FALSE)
  key <- finn_tx_key(ctx$d, keys)
  full <- finn_tx_key(ctx$d, finn_tx_cols(ctx$d))
  if (keep == "identical_only") {
    conflict <- tapply(full, key, function(x) length(unique(x)) > 1)
    if (any(conflict)) stop(sprintf("Step %d: %d keys have rows with different values. Use aggregate_duplicates, or keep first/last.", ctx$step, sum(conflict)), call. = FALSE)
    drop <- duplicated(key)
  } else {
    drop <- if (keep == "first") duplicated(key) else duplicated(key, fromLast = TRUE)
  }
  if (!any(drop)) stop(sprintf("Step %d: no duplicate rows for %s.", ctx$step, paste(keys, collapse = ", ")), call. = FALSE)
  n <- finn_tx_keep(ctx, !drop, s$reason)
  finn_tx_change(ctx, "drop_duplicates", sprintf("Removed repeated rows by %s (keep %s): %s", paste(keys, collapse = ", "), keep, s$reason), sprintf("%d rows", n), confirm = keep != "identical_only")
}

#' Check that an expression uses only arithmetic on columns and numbers
#'
#' @param e Parsed expression.
#' @param cols Allowed column names.
#' @return Column names used.
finn_tx_expr_columns <- function(e, cols) {
  if (is.numeric(e)) {
    return(character())
  }
  if (is.name(e)) {
    nm <- as.character(e)
    if (!nm %in% cols) stop("Expression uses '", nm, "', which is not a column.", call. = FALSE)
    return(nm)
  }
  if (is.call(e)) {
    fn <- as.character(e[[1]])
    if (!fn %in% c("+", "-", "*", "/", "(")) stop("Expression may use only + - * / and parentheses, not '", fn, "'.", call. = FALSE)
    return(unique(unlist(lapply(as.list(e)[-1], finn_tx_expr_columns, cols = cols))))
  }
  stop("Expression may use only column names, numbers, + - * / and parentheses.", call. = FALSE)
}

#' Apply `derive_column`: compute a column from other number columns
#'
#' @param ctx Context environment.
#' @param s Step list with `expression`, for example "Units * Price".
#' @return NULL, invisibly.
finn_tx_op_derive_column <- function(ctx, s) {
  if (s$name %in% finn_tx_cols(ctx$d)) stop(sprintf("Step %d: column '%s' already exists.", ctx$step, s$name), call. = FALSE)
  e <- tryCatch(parse(text = s$expression, keep.source = FALSE), error = function(err) stop(sprintf("Step %d: expression is not valid: %s", ctx$step, conditionMessage(err)), call. = FALSE))
  if (length(e) != 1) stop(sprintf("Step %d: give one expression.", ctx$step), call. = FALSE)
  used <- finn_tx_expr_columns(e[[1]], finn_tx_cols(ctx$d))
  for (v in used) if (!is.numeric(ctx$d[[v]])) stop(sprintf("Step %d: run parse_numbers on '%s' first.", ctx$step, v), call. = FALSE)
  env <- list2env(as.list(ctx$d[used]), parent = baseenv())
  val <- eval(e[[1]], env)
  ctx$d[[s$name]] <- as.numeric(val) + numeric(nrow(ctx$d))
  ctx$d[[paste0(".lin.", s$name)]] <- "derived"
  ctx$colsrc[[s$name]] <- unique(unlist(ctx$colsrc[used]))
  ctx$counts$derived <- ctx$counts$derived + 1
  finn_tx_change(ctx, "derive_column", sprintf("Created '%s' = %s: %s", s$name, s$expression, s$reason), sprintf("%d rows", nrow(ctx$d)), confirm = TRUE)
}

#' Apply `fill_values`: fill blank values in a column
#'
#' Methods: "zero", "value", "previous", or "next", optionally within `by`
#' groups and in date order. Blanks after the column's last value are left
#' blank unless `extend_into_future` is true, so history is not extended by
#' accident.
#'
#' @param ctx Context environment.
#' @param s Step list.
#' @return NULL, invisibly.
finn_tx_op_fill_values <- function(ctx, s) {
  col <- s$column
  by <- finn_tx_chr(s$by)
  finn_tx_need_cols(ctx, c(col, by))
  method <- s$method
  if (!method %in% c("zero", "value", "previous", "next")) stop(sprintf("Step %d: method must be zero, value, previous, or next.", ctx$step), call. = FALSE)
  v <- ctx$d[[col]]
  na <- if (is.numeric(v)) is.na(v) else finn_tx_blank(v)
  has_date <- inherits(ctx$d$Date, "Date")
  if (!isTRUE(s$extend_into_future) && has_date && any(!na)) {
    last <- max(ctx$d$Date[!na], na.rm = TRUE)
    na <- na & !is.na(ctx$d$Date) & ctx$d$Date <= last
  }
  if (!any(na)) stop(sprintf("Step %d: '%s' has no blanks to fill.", ctx$step, col), call. = FALSE)
  new <- v
  if (method %in% c("zero", "value")) {
    fillv <- if (method == "zero") 0 else s$value
    if (is.null(fillv)) stop(sprintf("Step %d: method value needs value.", ctx$step), call. = FALSE)
    new[na] <- if (is.numeric(v)) as.numeric(fillv) else as.character(fillv)
  } else {
    present <- if (is.numeric(v)) !is.na(v) else !finn_tx_blank(v)
    grp <- if (length(by)) finn_tx_key(ctx$d, by) else rep("", nrow(ctx$d))
    ord <- if (has_date) order(grp, ctx$d$Date) else order(grp, seq_len(nrow(ctx$d)))
    for (g in unique(grp)) {
      idx <- ord[grp[ord] == g]
      pos <- seq_along(idx)
      known <- present[idx]
      take <- if (method == "previous") {
        cummax(ifelse(known, pos, 0L))
      } else {
        rev(cummin(rev(ifelse(known, pos, .Machine$integer.max))))
      }
      fill <- na[idx] & take > 0L & take < .Machine$integer.max
      new[idx[fill]] <- v[idx[take[fill]]]
    }
  }
  filled <- na & !(if (is.numeric(new)) is.na(new) else finn_tx_blank(new))
  ctx$d[[col]] <- new
  ctx$d[[paste0(".lin.", col)]][filled] <- paste0("fill:", method)
  ctx$counts$filled <- ctx$counts$filled + sum(filled)
  finn_tx_change(ctx, "fill_values", sprintf("Filled blank '%s' values with %s: %s", col, if (method == "value") paste("value", s$value) else method, s$reason), sprintf("%d values", sum(filled)), confirm = TRUE)
}

#' Run every step of a spec on a loaded context
#'
#' @param ctx Context from `finn_tx_load()`.
#' @param spec Parsed spec.
#' @return The context, invisibly.
finn_tx_apply <- function(ctx, spec) {
  for (i in seq_along(spec$steps)) {
    s <- spec$steps[[i]]
    ctx$step <- i
    fn <- get(paste0("finn_tx_op_", s$op), mode = "function")
    tryCatch(
      if (identical(s$op, "drop_total_rows")) fn(ctx, s, spec$tolerance) else fn(ctx, s),
      error = function(e) {
        msg <- conditionMessage(e)
        if (!grepl("^Step [0-9]+", msg)) msg <- sprintf("Step %d (%s): %s", i, s$op, msg)
        stop(msg, call. = FALSE)
      }
    )
  }
  invisible(ctx)
}

#' Settings the gate checks against
#'
#' Merges the project config with the spec's `settings`, so the gate uses the
#' same values the run will use.
#'
#' @param project_cfg Project config list or NULL.
#' @param spec_settings Settings from the spec or NULL.
#' @return Merged config.
finn_tx_settings <- function(project_cfg, spec_settings) {
  cfg <- utils::modifyList(project_cfg %||% list(), spec_settings %||% list(), keep.null = TRUE)
  cfg <- finn_merge_config(cfg)
  miss <- c(
    if (!length(unlist(cfg$combo_variables))) "combo_variables",
    if (is.null(cfg$target_variable)) "target_variable",
    if (is.null(cfg$date_type)) "date_type",
    if (is.null(cfg$forecast_horizon)) "forecast_horizon"
  )
  if (length(miss)) stop("The gate needs the run settings. Add to the spec's settings (or the project config): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  if (!identical(cfg$date_column %||% "Date", "Date")) stop("The prepared file names its date column 'Date'; remove date_column from the settings.", call. = FALSE)
  cfg
}

#' The finnts validator, or NULL when the installed finnts lacks it
#'
#' @return Function or NULL.
finn_tx_default_validator <- function() {
  if (requireNamespace("finnts", quietly = TRUE) && "validate_prepared_input" %in% getNamespaceExports("finnts")) {
    return(getExportedValue("finnts", "validate_prepared_input"))
  }
  NULL
}

#' Arguments for the finnts validator, matching how the run calls prep_data
#'
#' @param data Prepared data frame.
#' @param cfg Merged config.
#' @return Named list.
finn_tx_validator_args <- function(data, cfg) {
  as_date <- function(x) if (is.null(x)) NULL else as.Date(x)
  as_num <- function(x) if (is.null(x)) NULL else as.numeric(x)
  regs <- unlist(cfg$external_regressors)
  list(
    input_data = data,
    combo_variables = unlist(cfg$combo_variables),
    target_variable = cfg$target_variable,
    date_type = cfg$date_type,
    forecast_horizon = as.numeric(cfg$forecast_horizon),
    external_regressors = if (length(regs)) regs else NULL,
    hist_start_date = as_date(cfg$hist_start_date),
    hist_end_date = as_date(cfg$hist_end_date),
    combo_cleanup_date = as_date(cfg$combo_cleanup_date),
    fiscal_year_start = as.numeric(cfg$fiscal_year_start %||% 1),
    clean_missing_values = isTRUE(cfg$clean_missing_values),
    forecast_approach = cfg$forecast_approach %||% "bottoms_up",
    parallel_processing = NULL,
    back_test_scenarios = as_num(cfg$back_test_scenarios),
    back_test_spacing = as_num(cfg$back_test_spacing)
  )
}

#' The prepared table without lineage columns
#'
#' @param ctx Context environment.
#' @return Data frame of user-visible columns.
finn_tx_output <- function(ctx) {
  ctx$d[finn_tx_cols(ctx$d)]
}

#' Write the prepared table as CSV text
#'
#' @param out Prepared data frame.
#' @return Character vector of CSV lines.
finn_tx_csv_lines <- function(out) {
  for (v in names(out)) if (inherits(out[[v]], "Date")) out[[v]] <- format(out[[v]], "%Y-%m-%d")
  con <- textConnection("lines", "w", local = TRUE)
  utils::write.csv(out, con, row.names = FALSE, na = "")
  close(con)
  lines
}

#' One gate check row
#'
#' @param rule Rule id.
#' @param description What is checked.
#' @param status "pass", "fail", or "report".
#' @param message Result in plain language.
#' @return One-row data frame.
finn_tx_check <- function(rule, description, status, message) {
  data.frame(Rule = rule, Description = description, Status = status, Message = message, stringsAsFactors = FALSE)
}

#' Source values behind lineage pieces
#'
#' @param ctx Context environment.
#' @param pieces Lineage pieces such as "4:C" or "4:C*0.001".
#' @param opts Parse options recorded by parse_numbers.
#' @return List with `value` (parsed source value times factor) and `cell`
#'   (the "row:letter" reference).
finn_tx_piece_values <- function(ctx, pieces, opts) {
  cell <- sub("\\*.*$", "", pieces)
  f <- ifelse(grepl("*", pieces, fixed = TRUE), suppressWarnings(as.numeric(sub("^.*\\*", "", pieces))), 1)
  r <- sub(":[^:]*$", "", cell)
  l <- sub("^.*:", "", cell)
  ri <- match(r, ctx$grid$.finn_row)
  ci <- match(l, names(ctx$grid))
  raw <- rep(NA_character_, length(cell))
  ok <- !is.na(ri) & !is.na(ci)
  if (any(ok)) raw[ok] <- as.matrix(ctx$grid)[cbind(ri[ok], ci[ok])]
  p <- finn_tx_parse_text_numbers(raw, opts$decimal_mark, opts$missing %||% finn_tx_missing_tokens(), !identical(opts$suffixes, FALSE))
  list(value = p$value * f, cell = cell, found = ok)
}

#' Tolerance test used by every reconciliation
#'
#' @param a,b Numeric vectors.
#' @param tol Tolerance list with `absolute` and `relative`.
#' @return Logical vector; TRUE where values agree or are both missing.
finn_tx_close <- function(a, b, tol) {
  both_na <- is.na(a) & is.na(b)
  ok <- !is.na(a) & !is.na(b) & abs(a - b) <= tol$absolute + tol$relative * pmax(abs(a), abs(b))
  both_na | ok
}

#' Verification gate for a prepared table
#'
#' Reads the table back the way the run will, then runs finnts'
#' `validate_prepared_input()` with the real run settings (rules R1-R11 and
#' P1-P8) plus the checks that need the source: every dropped row and column
#' explained, every target value traced to the source cells it came from
#' (each used at most once), totals that were dropped reconcile with the rows
#' they sum, no filled target values in the future, and no total labels left
#' among the series. Fails closed when the validator is unavailable.
#'
#' @param ctx Context after `finn_tx_apply()`.
#' @param spec Parsed spec.
#' @param cfg Settings from `finn_tx_settings()`.
#' @param validator Function with the `validate_prepared_input()` signature,
#'   or NULL.
#' @return List with `passed`, `checks`, `summary`, `reconciliation`,
#'   `dropped`, `totals`, and `counts`.
finn_tx_gate <- function(ctx, spec, cfg, validator = finn_tx_default_validator()) {
  tol <- spec$tolerance
  out <- finn_tx_output(ctx)
  target <- cfg$target_variable
  combos <- unlist(cfg$combo_variables)
  checks <- list()
  add <- function(...) checks[[length(checks) + 1]] <<- finn_tx_check(...)

  # Read the table back as the run will.
  prepared <- NULL
  read_err <- NULL
  shape_ok <- "Date" %in% names(out) && inherits(out$Date, "Date") && target %in% names(out) && is.numeric(out[[target]])
  if (!shape_ok) {
    read_err <- "The result needs a 'Date' column read with parse_dates and a target column read with parse_numbers."
  } else {
    lines <- finn_tx_csv_lines(out)
    cls <- stats::setNames(rep("character", length(intersect(combos, names(out)))), intersect(combos, names(out)))
    back <- utils::read.csv(text = lines, check.names = FALSE, stringsAsFactors = FALSE, colClasses = cls, na.strings = "")
    rt_cfg <- utils::modifyList(cfg, list(date_column = "Date", date_format = "%Y-%m-%d", decimal_mark = "."), keep.null = TRUE)
    prepared <- tryCatch(finn_prepare_input(back, rt_cfg)$data, error = function(e) {
      read_err <<- conditionMessage(e)
      NULL
    })
  }
  add("S1", "The prepared file reads back with the run's settings", if (is.null(prepared)) "fail" else "pass", read_err %||% "Passed.")

  summary <- list()
  details <- list()
  if (is.null(validator)) {
    add("R/P", "finnts input checks (validate_prepared_input)", "fail", "The installed finnts does not have validate_prepared_input(). Update finnts, then rerun this preview.")
  } else if (!is.null(prepared)) {
    res <- tryCatch(do.call(validator, finn_tx_validator_args(prepared, cfg)), error = function(e) e)
    if (inherits(res, "error")) {
      add("R/P", "finnts input checks (validate_prepared_input)", "fail", conditionMessage(res))
    } else {
      vc <- as.data.frame(res$checks, stringsAsFactors = FALSE)
      checks[[length(checks) + 1]] <- vc[c("Rule", "Description", "Status", "Message")]
      if (!isTRUE(res$passed) && !any(vc$Status %in% c("fail", "skipped"))) {
        add("R/P", "finnts input checks (validate_prepared_input)", "fail", "validate_prepared_input() did not pass.")
      }
      summary <- res$summary %||% list()
      details <- res$details %||% list()
    }
  }

  # P9a: every source row and column is either used or dropped with a reason.
  used_rows <- unique(unlist(strsplit(ctx$d$.finn_row, ";", fixed = TRUE)))
  dropped_rows <- unique(unlist(lapply(ctx$drops, `[[`, "rows")))
  lost_rows <- setdiff(ctx$grid$.finn_row, c(used_rows, dropped_rows))
  src_letters <- setdiff(names(ctx$grid), c(".finn_row", ".finn_sheet"))
  used_letters <- unique(unlist(ctx$colsrc[names(out)]))
  dropped_letters <- unique(unlist(lapply(ctx$coldrops, `[[`, "letters")))
  lost_letters <- setdiff(src_letters, c(used_letters, dropped_letters))
  add(
    "P9a", "Every dropped row and column is explained",
    if (length(lost_rows) || length(lost_letters)) "fail" else "pass",
    if (length(lost_rows) || length(lost_letters)) {
      sprintf("Unexplained: rows %s; columns %s.", paste(utils::head(lost_rows, 10), collapse = ", "), paste(utils::head(lost_letters, 10), collapse = ", "))
    } else {
      sprintf("%d source rows used, %d dropped with a reason; %d source columns used, %d dropped with a reason.", length(setdiff(used_rows, dropped_rows)), length(dropped_rows), length(used_letters), length(setdiff(dropped_letters, used_letters)))
    }
  )

  # P9b: every target value traces to the source cells it came from.
  recon <- NULL
  if (shape_ok) {
    lin <- ctx$d[[paste0(".lin.", target)]]
    special <- is.na(lin) | grepl("^(fill:|derived)", lin)
    pieces <- strsplit(lin[!special], ";", fixed = TRUE)
    owner <- rep(which(!special), lengths(pieces))
    pv <- finn_tx_piece_values(ctx, unlist(pieces), ctx$parse[[target]] %||% list())
    expected <- rep(NA_real_, nrow(out))
    if (length(owner)) {
      s <- tapply(pv$value, owner, function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE))
      expected[as.integer(names(s))] <- s
    }
    got <- out[[target]]
    traced <- !special
    bad <- traced & !finn_tx_close(got, expected, tol)
    reused <- unique(pv$cell[duplicated(pv$cell)])
    missing_cells <- sum(!pv$found)
    status <- if (any(bad) || length(reused) || missing_cells) "fail" else "pass"
    msg <- if (status == "fail") {
      paste(c(
        if (any(bad)) sprintf("%d target values do not match their source cells (rows %s).", sum(bad), paste(utils::head(ctx$d$.finn_row[bad], 5), collapse = ", ")),
        if (length(reused)) sprintf("%d source cells are counted more than once (%s).", length(reused), paste(utils::head(reused, 5), collapse = ", ")),
        if (missing_cells) sprintf("%d target values point at cells outside the source.", missing_cells)
      ), collapse = " ")
    } else {
      sprintf(
        "Target total %s matches the source cells it came from (%s)%s.",
        format(sum(got[traced], na.rm = TRUE), big.mark = ",", digits = 15), format(sum(expected[traced], na.rm = TRUE), big.mark = ",", digits = 15),
        if (any(special)) sprintf("; %d filled or derived values are not from the source", sum(special)) else ""
      )
    }
    add("P9b", "Target values match the source, overall, per series and per period", status, msg)
    key <- if (length(intersect(combos, names(out))) == length(combos)) finn_tx_key(out, combos) else rep("all", nrow(out))
    by_combo <- tapply(ifelse(traced, got, 0) - ifelse(traced, expected, 0), key, function(x) sum(x, na.rm = TRUE))
    by_period <- tapply(ifelse(traced, got, 0) - ifelse(traced, expected, 0), format(out$Date, "%Y-%m-%d"), function(x) sum(x, na.rm = TRUE))
    recon <- list(
      output_total = sum(got, na.rm = TRUE), source_total = sum(expected[traced], na.rm = TRUE),
      filled_or_derived = sum(special), max_series_difference = max(abs(by_combo), 0, na.rm = TRUE),
      max_period_difference = max(abs(by_period), 0, na.rm = TRUE)
    )

    # Transform faults: filled target values in the future.
    hist_end <- if (!is.null(cfg$hist_end_date)) as.Date(cfg$hist_end_date) else suppressWarnings(max(out$Date[!is.na(got) & !special], na.rm = TRUE))
    future_fill <- grepl("^fill:", lin) & !is.na(out$Date) & is.finite(hist_end) & out$Date > hist_end
    add(
      "T1", "No target values are filled in after the history end", if (any(future_fill)) "fail" else "pass",
      if (any(future_fill)) sprintf("%d filled target values fall after %s.", sum(future_fill), format(hist_end)) else "Passed."
    )
  }

  # P9c: totals that were dropped agree with the rows or columns they sum.
  tot_rows <- list()
  for (t in ctx$totals) {
    cmp <- t$compare
    if (is.null(cmp) || !nrow(cmp)) {
      ok <- FALSE
      cmp <- data.frame(column = NA_character_, total = NA_real_, leaves = NA_real_)
    } else {
      ok <- finn_tx_close(cmp$total, cmp$leaves, tol)
    }
    tot_rows[[length(tot_rows) + 1]] <- data.frame(
      step = t$step, kind = t$kind, label = t$label, row = t$row %||% NA_character_, column = cmp$column,
      total = cmp$total, sum_of_parts = cmp$leaves, ok = ok, reconcile = t$reconcile, stringsAsFactors = FALSE
    )
  }
  totals <- if (length(tot_rows)) do.call(rbind, tot_rows) else NULL
  if (is.null(totals)) {
    add("P9c", "Dropped totals agree with the values they sum", "pass", "No total rows or columns were dropped.")
  } else {
    hard <- !totals$ok & totals$reconcile
    soft <- !totals$ok & !totals$reconcile
    add(
      "P9c", "Dropped totals agree with the values they sum",
      if (any(hard)) "fail" else if (any(soft)) "report" else "pass",
      if (any(hard)) {
        sprintf("%d total values differ from the sum of their parts (first: '%s'). Fix the steps, widen tolerance for rounded exports, or set reconcile: false with the user's agreement.", sum(hard), totals$label[hard][1])
      } else if (any(soft)) {
        sprintf("%d total values differ from their parts; the user agreed not to reconcile them.", sum(soft))
      } else {
        sprintf("%d total values match the sum of their parts.", nrow(totals))
      }
    )
  }

  # Total labels left among the series would double count.
  allow <- finn_tx_chr(spec$not_totals)
  left <- unique(unlist(lapply(intersect(combos, names(out)), function(v) {
    x <- unique(trimws(as.character(out[[v]])))
    x[!is.na(x) & grepl(finn_tx_total_regex(), x, ignore.case = TRUE, perl = TRUE) & !x %in% allow]
  })))
  add(
    "T5", "No total or subtotal labels remain in the series columns", if (length(left)) "fail" else "pass",
    if (length(left)) sprintf("Series labels that look like totals: %s. Drop them with drop_total_rows, or list real names in not_totals.", paste(utils::head(left, 5), collapse = ", ")) else "Passed."
  )

  # Dropped rows, with the values they held in the target's source columns.
  value_letters <- if (shape_ok) unique(sub("^.*:", "", sub("\\*.*$", "", unlist(strsplit(ctx$d[[paste0(".lin.", target)]][!grepl("^(fill:|derived)", ctx$d[[paste0(".lin.", target)]])], ";", fixed = TRUE))))) else character()
  value_letters <- intersect(value_letters, src_letters)
  dropped <- do.call(rbind, lapply(ctx$drops, function(dr) {
    held <- if (length(value_letters)) {
      cells <- as.matrix(ctx$grid[match(dr$rows, ctx$grid$.finn_row), value_letters, drop = FALSE])
      sum(finn_tx_parse_text_numbers(as.vector(cells))$value, na.rm = TRUE)
    } else {
      0
    }
    data.frame(step = dr$step, reason = dr$reason, rows = length(dr$rows), first_rows = paste(utils::head(dr$rows, 5), collapse = ", "), value_total = held, stringsAsFactors = FALSE)
  }))
  checks <- do.call(rbind, checks)
  rownames(checks) <- NULL
  list(
    passed = !any(checks$Status %in% c("fail", "skipped")),
    checks = checks,
    summary = summary,
    details = details,
    reconciliation = recon,
    dropped = dropped,
    totals = totals,
    counts = ctx$counts,
    output = out
  )
}

#' Mask values for a preview shown to Copilot
#'
#' Numbers keep only their shape ("#,###.##"); dates, period names, total
#' words, and unit words stay readable; other text becomes `text_1`,
#' `text_2`, ... consistently across one preview.
#'
#' @param x Vector.
#' @param labels Environment that maps text to its token, shared across
#'   tables.
#' @param reveal TRUE to show text labels as they are.
#' @return Character vector.
finn_tx_mask <- function(x, labels, reveal = FALSE) {
  if (inherits(x, "Date")) {
    return(ifelse(is.na(x), "", format(x, "%Y-%m-%d")))
  }
  x <- as.character(x)
  out <- ifelse(is.na(x), "", x)
  t <- trimws(out)
  keep_rx <- paste0(
    "(?i)^(", finn_tx_total_regex(), ")$|",
    "^(jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)[a-z]*\\.?([ -][0-9]{2,4})?$|",
    "^[0-9]{4}([ -/](jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*|[ -/]?q[1-4]|[-/][0-9]{1,2}([-/][0-9]{1,2})?)?$|",
    "^q[1-4][ -]?[0-9]{2,4}$|^fy ?[0-9]{2,4}( ?(p|m|q)[0-9]{1,2})?$|^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$|",
    "^(units?|usd|eur|gbp|k|m|mm|bn|thousands?|millions?|billions?|in thousands|in millions|%|n/a|na|-|\u2014|\u2013)$"
  )
  num <- !is.na(suppressWarnings(finn_tx_parse_text_numbers(t)$value)) & nzchar(t)
  keep <- !num & grepl(keep_rx, t, perl = TRUE)
  text <- nzchar(t) & !num & !keep
  out[num] <- finn_tx_shape(t[num])
  if (!reveal && any(text)) {
    token <- function(u) {
      if (is.null(labels[[u]])) labels[[u]] <- sprintf("text_%d", length(ls(labels, all.names = TRUE)) + 1L)
      labels[[u]]
    }
    out[text] <- vapply(t[text], function(u) {
      suffix <- regmatches(u, regexec("(?i)^(.+?)\\s+((?:sub-?)?total)$", u, perl = TRUE))[[1]]
      prefix <- regmatches(u, regexec("(?i)^((?:grand\\s+|sub-?)?total)\\s+(.+)$", u, perl = TRUE))[[1]]
      if (length(suffix) == 3) {
        paste(token(suffix[2]), suffix[3])
      } else if (length(prefix) == 3) {
        paste(prefix[2], token(prefix[3]))
      } else {
        token(u)
      }
    }, character(1))
  }
  out
}

#' Markdown table from a data frame
#'
#' @param df Data frame of character columns.
#' @return Character vector of table lines.
finn_tx_md_table <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return("_None._")
  }
  esc <- function(x) gsub("|", "\\|", gsub("\n", " ", ifelse(is.na(x), "", as.character(x))), fixed = TRUE)
  c(
    paste0("| ", paste(esc(names(df)), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(esc(r), collapse = " | "), " |"))
  )
}

#' Approval token for one spec, source, and settings
#'
#' Recomputed on approval; any change to the spec, the source file, the
#' settings, or the skill version gives a different token. Never stored.
#'
#' @param spec Parsed spec.
#' @param md5 Source file md5.
#' @param cfg Merged settings.
#' @return 16-character token.
finn_tx_token <- function(spec, md5, cfg) {
  payload <- paste(finn_tx_canonical(spec), md5, finn_to_json(cfg), finn_skill_version, sep = "\n")
  substr(digest::digest(payload, algo = "sha256", serialize = FALSE), 1, 16)
}

#' Fixed-format preview of a proposed preparation
#'
#' Sections, in order: summary (with a `Latest period` line when the last
#' period has not ended), Changes, Needs your extra confirmation, Dropped
#' rows, Before, After, Verification.
#'
#' @param run Result of `finn_tx_run()` without `preview_markdown`.
#' @param reveal TRUE to show text labels unmasked.
#' @param n_rows Rows shown in the Before and After tables.
#' @return One markdown string.
finn_tx_preview_markdown <- function(run, reveal = FALSE, n_rows = 8L) {
  ctx <- run$ctx
  gate <- run$gate
  cfg <- run$settings
  out <- gate$output
  labels <- new.env(parent = emptyenv())
  src_cols <- utils::head(setdiff(names(ctx$grid), c(".finn_row", ".finn_sheet")), 10)
  before <- ctx$grid[utils::head(seq_len(nrow(ctx$grid)), n_rows), src_cols, drop = FALSE]
  before[] <- lapply(before, finn_tx_mask, labels = labels, reveal = reveal)
  before <- cbind(Row = utils::head(ctx$grid$.finn_row, n_rows), before)
  after <- utils::head(out[utils::head(names(out), 10)], n_rows)
  after[] <- lapply(after, finn_tx_mask, labels = labels, reveal = reveal)
  changes <- do.call(rbind, lapply(ctx$changes, function(ch) data.frame(Step = ch$step, Change = ch$change, Affects = ch$affects, stringsAsFactors = FALSE)))
  dropped <- if (!is.null(gate$dropped)) {
    data.frame(Reason = gate$dropped$reason, Rows = gate$dropped$rows, `Value in target columns` = format(gate$dropped$value_total, big.mark = ",", digits = 15), check.names = FALSE)
  }
  ver <- gate$checks
  ver <- data.frame(Rule = ver$Rule, Check = ver$Description, Result = toupper(ver$Status), Detail = ver$Message, stringsAsFactors = FALSE)
  n_series <- if (all(unlist(cfg$combo_variables) %in% names(out))) length(unique(finn_tx_key(out, unlist(cfg$combo_variables)))) else NA
  dates <- if (inherits(out$Date, "Date") && any(!is.na(out$Date))) sprintf("dates %s to %s", format(min(out$Date, na.rm = TRUE)), format(max(out$Date, na.rm = TRUE))) else "dates not read yet"
  regs <- unlist(cfg$external_regressors)
  rec <- gate$reconciliation
  confirm <- if (length(ctx$confirm)) paste0("- ", ctx$confirm) else "- None."
  lines <- c(
    "## Proposed data preparation",
    "",
    if (!is.null(run$spec$description)) c(run$spec$description, ""),
    sprintf("**Source:** %s - %d rows x %d columns (original file is never changed)", basename(ctx$file), nrow(ctx$grid), length(setdiff(names(ctx$grid), c(".finn_row", ".finn_sheet")))),
    sprintf("**Result:** %d rows x %d columns; %s series; %s", nrow(out), ncol(out), n_series, dates),
    sprintf(
      "**Checked against settings:** series %s | target %s | date type %s | horizon %s | regressors %s",
      paste(unlist(cfg$combo_variables), collapse = ", "), cfg$target_variable, cfg$date_type, cfg$forecast_horizon,
      if (length(regs)) paste(regs, collapse = ", ") else "none"
    ),
    if (!is.null(rec)) sprintf("**Target total:** %s in the result vs %s in the source cells used", format(rec$output_total, big.mark = ",", digits = 15), format(rec$source_total, big.mark = ",", digits = 15)),
    if (!is.null(run$latest_period)) sprintf("**Latest period:** %s", run$latest_period$message),
    "",
    "### Changes",
    finn_tx_md_table(changes),
    "",
    "### Needs your extra confirmation",
    confirm,
    "",
    "### Dropped rows",
    finn_tx_md_table(dropped),
    "",
    sprintf("### Before (source, first %d rows%s)", n_rows, if (reveal) "" else ", values masked"),
    finn_tx_md_table(before),
    "",
    sprintf("### After (prepared, first %d rows%s)", n_rows, if (reveal) "" else ", values masked"),
    finn_tx_md_table(after),
    "",
    "### Verification",
    finn_tx_md_table(ver),
    "",
    if (isTRUE(gate$passed)) "**Overall: PASSED.** Nothing is written until the user approves this plan." else "**Overall: FAILED.** Nothing will be written. Fix the plan and preview again."
  )
  paste(lines, collapse = "\n")
}

#' Apply a transform spec and verify the result, without writing anything
#'
#' @param file Source path; never modified.
#' @param spec_text Spec JSON text from standard input.
#' @param project_cfg Project config list or NULL.
#' @param validator Validator function or NULL (fails closed).
#' @param sheet Default sheet name or number.
#' @param reveal_labels TRUE to show text labels unmasked in the preview.
#' @param today Date used to spot a latest period that has not ended; tests
#'   pass a fixed date. It never affects the gate or the token.
#' @return List with `spec`, `settings`, `ctx`, `gate`, `token` (NULL unless
#'   the gate passed), `plan_summary`, `latest_period` (from
#'   [finn_tx_latest_period()]), and `preview_markdown`.
finn_tx_run <- function(file, spec_text, project_cfg = NULL, validator = finn_tx_default_validator(), sheet = NULL, reveal_labels = FALSE, today = Sys.Date()) {
  spec <- finn_tx_read_spec(spec_text)
  cfg <- finn_tx_settings(project_cfg, spec$settings)
  ctx <- finn_tx_load(file, spec, sheet)
  finn_tx_apply(ctx, spec)
  gate <- finn_tx_gate(ctx, spec, cfg, validator)
  run <- list(
    spec = spec, settings = cfg, ctx = ctx, gate = gate,
    token = if (isTRUE(gate$passed)) finn_tx_token(spec, ctx$md5, cfg) else NULL,
    plan_summary = c(spec$description, vapply(ctx$changes, function(ch) sprintf("Step %d: %s (%s)", ch$step, ch$change, ch$affects), character(1))),
    latest_period = finn_tx_latest_period(gate$output, cfg, today)
  )
  run$preview_markdown <- finn_tx_preview_markdown(run, reveal = reveal_labels)
  run
}

#' Latest prepared period that has not ended and would still be history
#'
#' Advisory only: never a gate check, never part of the token or the audit
#' record, because it depends on today's date.
#'
#' @param out Prepared data frame with a `Date` column.
#' @param cfg Settings from `finn_tx_settings()`.
#' @param today Date to compare with.
#' @return Result of [finn_incomplete_period()], or NULL when the latest
#'   period has ended, is left out by `hist_end_date`, or cannot be read.
finn_tx_latest_period <- function(out, cfg, today = Sys.Date()) {
  target <- cfg$target_variable
  if (is.null(out) || !inherits(out$Date, "Date") || is.null(target) || !target %in% names(out)) {
    return(NULL)
  }
  has <- !is.na(out$Date) & !is.na(suppressWarnings(as.numeric(out[[target]])))
  if (!any(has)) {
    return(NULL)
  }
  partial <- finn_incomplete_period(max(out$Date[has]), cfg$date_type, out$Date[has], today)
  finn_partial_in_history(cfg, list(incomplete_period = partial))
}

#' Masked layout diagnostics for a raw data file
#'
#' Reads the file as an all-text grid and reports layout problems a transform
#' spec may need to fix. Reports only; it never decides a fix. Text labels are
#' masked with [finn_tx_mask()], so the result can be shown to Copilot.
#'
#' @param file Source path; never modified.
#' @param sheet Optional sheet name or number for Excel files.
#' @param max_rows Rows analysed for layout (the first rows of the grid).
#' @param n_sample Rows in the masked sample.
#' @param reveal_labels TRUE to show text labels unmasked (needs the user's
#'   consent for this file). Cells of the guessed header row, which become
#'   column names, are always shown.
#' @return List of diagnostics: grid size, blank rows and columns, header
#'   candidates and best guess, title rows, two-row date headers, pivot
#'   filters (field names shown; a single selected item is masked), total
#'   rows and columns, label-only and outline rows, footnotes, repeated
#'   headers, unit mentions, per-column value patterns, and a masked sample.
finn_tx_diagnose <- function(file, sheet = NULL, max_rows = 2000L, n_sample = 12L, reveal_labels = FALSE) {
  grid <- finn_tx_read_grid(file, sheet)
  letters_all <- setdiff(names(grid), c(".finn_row", ".finn_sheet"))
  n_total <- nrow(grid)
  g <- utils::head(grid[letters_all], max_rows)
  m <- as.matrix(g)
  m[is.na(m)] <- ""
  dims <- dim(m)
  filled <- matrix(nzchar(m), dims[1])
  num <- matrix(!is.na(finn_tx_parse_text_numbers(as.vector(m))$value), dims[1]) & filled
  dt <- matrix(!is.na(finn_tx_parse_dates(as.vector(m), setdiff(finn_tx_date_formats(), c("excel_serial", "%Y")))$value), dims[1]) & filled & !num
  total_rx <- paste0("(?i)", finn_tx_total_regex())
  is_total <- matrix(grepl(total_rx, as.vector(m), perl = TRUE), dims[1])
  n_filled <- rowSums(filled)
  n_num <- rowSums(num)
  labels <- new.env()
  mask <- function(x) finn_tx_mask(x, labels, reveal = reveal_labels)
  rows_of <- function(i, n = 20L) as.integer(utils::head(i, n))

  head_scan <- seq_len(min(30L, dims[1]))
  widest <- max(n_filled[head_scan], 1)
  cand <- head_scan[n_filled[head_scan] >= max(2, 0.5 * widest) & (n_filled[head_scan] - n_num[head_scan]) / pmax(n_filled[head_scan], 1) >= 0.6]
  header_guess <- if (length(cand)) {
    nxt <- vapply(cand, function(r) r < dims[1] && n_num[r + 1] > 0, logical(1))
    if (any(nxt)) cand[nxt][1] else cand[1]
  }
  header_candidates <- lapply(cand, function(r) list(
    row = r, filled = n_filled[r], date_cells = sum(dt[r, ]),
    cells = as.list(if (identical(r, header_guess)) utils::head(m[r, ], 15) else mask(utils::head(m[r, ], 15)))
  ))
  year_rx <- "^(fy ?)?[0-9]{2,4}$"
  month_rx <- "(?i)^(jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)[a-z]*\\.?$|^(p|m|q)[0-9]{1,2}$"
  two_row <- NULL
  for (r in head_scan[head_scan < dims[1]]) {
    yrs <- grepl(year_rx, m[r, ], ignore.case = TRUE)
    mons <- grepl(month_rx, m[r + 1, ], perl = TRUE)
    if (sum(yrs) >= 1 && sum(mons) >= 3) {
      two_row <- list(rows = c(r, r + 1L), years = sum(yrs), periods = sum(mons))
      break
    }
  }
  data_rows <- if (is.null(header_guess)) seq_len(dims[1]) else seq.int(header_guess + 1L, length.out = max(0L, dims[1] - header_guess))
  title_rows <- if (!is.null(header_guess) && header_guess > 1) {
    above <- seq_len(header_guess - 1L)
    above[n_filled[above] > 0 & n_filled[above] <= 2]
  } else {
    integer()
  }
  total_hits <- which(is_total, arr.ind = TRUE)
  total_hits <- total_hits[total_hits[, 1] %in% data_rows, , drop = FALSE]
  total_rows <- sort(unique(total_hits[, 1]))
  col_filled <- colSums(filled[data_rows, , drop = FALSE])
  value_cols <- which(col_filled > 0 & colSums(num[data_rows, , drop = FALSE]) >= 0.8 * col_filled)
  total_cols <- if (!is.null(header_guess)) which(is_total[header_guess, ]) else integer()
  # A lone value column headed "Total" has nothing to sum, so it is the value.
  if (!length(setdiff(value_cols, total_cols))) total_cols <- integer()
  total_columns <- letters_all[total_cols]
  label_only <- data_rows[n_filled[data_rows] > 0 & n_num[data_rows] == 0 & n_filled[data_rows] <= 2]
  # Outline labels: a label column left of the values whose label is blank on
  # the rows below it until the next label (total rows aside).
  outline <- list()
  body <- data_rows[n_num[data_rows] > 0 & rowSums(is_total[data_rows, , drop = FALSE]) == 0]
  first_value <- if (length(value_cols)) min(value_cols) else dims[2] + 1L
  for (j in seq_len(min(first_value - 1L, dims[2]))) {
    if (!length(body)) break
    vals <- m[body, j]
    lab <- nzchar(vals) & !num[body, j]
    blank <- !nzchar(vals)
    if (lab[1] && any(blank) && sum(lab) >= 0.8 * sum(!blank)) {
      outline[[length(outline) + 1]] <- list(column = letters_all[j], blank_label_rows = sum(blank), labelled_rows = sum(lab))
    }
  }
  # Pivot filters: "Field | selection" pairs above the header, either with an
  # Excel marker such as "(Multiple Items)" or set apart by a blank row.
  filters <- list()
  if (!is.null(header_guess) && header_guess > 1) {
    above <- seq_len(header_guess - 1L)
    last_gap <- suppressWarnings(max(above[n_filled[above] == 0]))
    for (r in above[n_filled[above] == 2]) {
      j <- which(filled[r, ])
      if (j[2] - j[1] != 1 || num[r, j[1]] || dt[r, j[1]]) next
      sel <- trimws(m[r, j[2]])
      kind <- if (grepl("^\\(all\\)$", sel, ignore.case = TRUE)) "all" else if (grepl("^\\(multiple items\\)$", sel, ignore.case = TRUE)) "multiple" else "single"
      if (kind == "single" && !(r < last_gap)) next
      filters[[length(filters) + 1]] <- list(row = r, field = m[r, j[1]], selection = if (kind == "single") mask(sel) else sel, kind = kind)
    }
  }
  note_rx <- "(?i)^(note|notes|source|sources|footnote|\\*|\\([0-9a-z]\\)|[0-9]\\))"
  footnotes <- data_rows[n_filled[data_rows] == 1 & n_num[data_rows] == 0 & apply(m[data_rows, , drop = FALSE], 1, function(r) {
    t <- r[nzchar(r)]
    length(t) == 1 && (grepl(note_rx, t, perl = TRUE) || nchar(t) > 40)
  })]
  repeated <- if (!is.null(header_guess)) {
    hdr <- m[header_guess, ]
    data_rows[vapply(data_rows, function(r) sum(nzchar(hdr) & m[r, ] == hdr) >= max(2, 0.5 * sum(nzchar(hdr))), logical(1))]
  } else {
    integer()
  }
  unit_rx <- "(?i)(in thousands|in millions|in billions|\\$ ?000|000s|\\(000\\)|\\bthousands\\b|\\bmillions\\b|\\bbillions\\b|\\busd\\b|\\beur\\b|\\bgbp\\b|\\bunits\\b|\\$k\\b|\\$m\\b)"
  unit_hits <- which(matrix(grepl(unit_rx, as.vector(m), perl = TRUE), dims[1]), arr.ind = TRUE)
  units <- lapply(utils::head(seq_len(nrow(unit_hits)), 20), function(k) {
    r <- unit_hits[k, 1]
    j <- unit_hits[k, 2]
    word <- regmatches(m[r, j], regexpr(unit_rx, m[r, j], perl = TRUE))
    list(row = as.integer(r), column = letters_all[j], unit = tolower(word))
  })
  columns <- lapply(seq_len(dims[2]), function(j) {
    v <- m[data_rows, j]
    f <- nzchar(v)
    nf <- max(sum(f), 1)
    share <- function(x) round(sum(x & f) / nf, 3)
    shapes <- utils::head(sort(table(mask(v[f])), decreasing = TRUE), 3)
    list(
      column = letters_all[j], header = if (!is.null(header_guess)) m[header_guess, j] else NULL,
      filled = sum(f), numeric_share = share(num[data_rows, j]), date_share = share(dt[data_rows, j]),
      suffix_share = share(grepl("(?i)[0-9.,)]\\s*(k|mm|m|bn|b)\\)?$", v, perl = TRUE)),
      paren_share = share(grepl("^-?\\(.*[0-9].*\\)$", v)),
      currency_share = share(grepl("[$\u20ac\u00a3\u00a5]", v)),
      percent_share = share(grepl("%\\s*$", v)),
      text_share = share(!num[data_rows, j] & !dt[data_rows, j]),
      total_labels = sum(is_total[data_rows, j]),
      top_shapes = as.list(stats::setNames(as.integer(shapes), names(shapes)))
    )
  })
  sample_rows <- seq_len(min(n_sample, dims[1]))
  sample <- lapply(sample_rows, function(r) c(list(row = r), as.list(stats::setNames(mask(utils::head(m[r, ], 15)), utils::head(letters_all, 15)))))
  list(
    rows = n_total, columns = length(letters_all), analysed_rows = dims[1],
    blank_rows = list(count = sum(n_filled == 0), rows = rows_of(which(n_filled == 0))),
    blank_columns = as.list(letters_all[colSums(filled) == 0]),
    header_guess = header_guess, header_candidates = header_candidates,
    title_rows = as.list(rows_of(title_rows)), two_row_date_header = two_row,
    pivot_filters = filters,
    total_rows = list(count = length(total_rows), rows = rows_of(total_rows), columns = as.list(unique(letters_all[total_hits[, 2]]))),
    total_columns = as.list(total_columns),
    label_only_rows = list(count = length(label_only), rows = rows_of(label_only)),
    outline_columns = outline,
    footnote_rows = as.list(rows_of(footnotes)),
    repeated_header_rows = as.list(rows_of(repeated)),
    unit_mentions = units,
    column_patterns = columns,
    sample_masked = sample
  )
}
