# Shared helpers for the Finn skill scripts.
#
# Every script sources this file, parses `--key=value` arguments, does its
# work with all console output diverted to stderr, and prints exactly one JSON
# object as the final stdout line:
#   {"ok": true|false, "status": "...", "message": "...", "data": {...},
#    "next_actions": [...]}
# Coding agents read only that final line. Nothing here deletes files.

finn_skill_version <- "0.2.0"
finn_schema_version <- 1L

#' Null-coalescing helper
#'
#' @param a Value.
#' @param b Fallback when `a` is NULL.
#' @return `a` unless NULL, else `b`.
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Script plumbing
# ---------------------------------------------------------------------------

#' Locate the directory of the running script
#'
#' @return Normalized directory path of the script passed with `--file=`, or
#'   the working directory when run interactively.
finn_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0) {
    return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  }
  path <- sub("^--file=", "", file_arg[[1]])
  path <- gsub("~\\+~", " ", path)
  normalizePath(dirname(path), winslash = "/", mustWork = FALSE)
}

#' Parse `--key=value` command-line arguments
#'
#' @param args Character vector of trailing arguments.
#' @return Named list of character values. A bare `--flag` maps to "true";
#'   hyphens in keys become underscores.
finn_parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list()
  for (arg in args) {
    if (!startsWith(arg, "--")) next
    body <- substring(arg, 3)
    if (grepl("=", body, fixed = TRUE)) {
      key <- sub("=.*$", "", body)
      value <- sub("^[^=]*=", "", body)
    } else {
      key <- body
      value <- "true"
    }
    out[[gsub("-", "_", key, fixed = TRUE)]] <- value
  }
  out
}

#' Read one parsed argument with a default
#'
#' @param args Named list from [finn_parse_args()].
#' @param key Argument name.
#' @param default Value returned when the argument is absent or empty.
#' @return The argument value or `default`.
finn_arg <- function(args, key, default = NULL) {
  value <- args[[key]]
  if (is.null(value) || identical(value, "")) default else value
}

#' Interpret a flag value as logical
#'
#' @param x Value such as "true", "yes", "1", or a logical.
#' @param default Value used when `x` is NULL or NA.
#' @return A single logical.
finn_bool <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) {
    return(default)
  }
  if (is.logical(x)) {
    return(isTRUE(x[[1]]))
  }
  tolower(as.character(x[[1]])) %in% c("true", "t", "yes", "y", "1")
}

#' Build a script result
#'
#' @param ok Whether the script achieved its goal.
#' @param status Machine-readable status such as "ok", "needs_confirmation",
#'   "needs_input", "blocked", or "error".
#' @param message One plain-language sentence for the user.
#' @param data Named list of structured details.
#' @param next_actions Character vector of suggested follow-ups.
#' @return A result list consumed by [finn_emit()].
finn_result <- function(ok = TRUE, status = "ok", message = "", data = list(),
                        next_actions = character()) {
  list(
    ok = isTRUE(ok), status = status, message = message,
    data = data, next_actions = as.list(next_actions)
  )
}

#' Write a timestamped progress line to stderr
#'
#' @param ... Message parts pasted together.
#' @return Invisible NULL.
finn_log <- function(...) {
  message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
  invisible(NULL)
}

#' Convert an R object to JSON text
#'
#' @param x Object to serialize.
#' @param pretty Whether to indent.
#' @return JSON string.
finn_to_json <- function(x, pretty = FALSE) {
  as.character(jsonlite::toJSON(
    x,
    auto_unbox = TRUE, null = "null", na = "null", digits = NA,
    pretty = pretty, POSIXt = "ISO8601", Date = "ISO8601", force = TRUE
  ))
}

#' Print the final JSON result line
#'
#' Removes any active output sinks so the result reaches real stdout.
#'
#' @param result Result list from [finn_result()].
#' @return Invisible result.
finn_emit <- function(result) {
  while (sink.number() > 0) sink()
  cat(finn_to_json(result), "\n", sep = "", file = stdout())
  invisible(result)
}

#' Run a script body with uniform error handling
#'
#' Diverts printed output to stderr, converts errors to an `error` result, and
#' always emits exactly one JSON line.
#'
#' @param fn Function taking the parsed argument list and returning a result.
#' @return Invisible result; quits with status 1 on error when non-interactive.
finn_main <- function(fn) {
  args <- finn_parse_args()
  sink(stderr(), type = "output")
  result <- tryCatch(
    fn(args),
    error = function(e) {
      finn_result(
        ok = FALSE, status = "error",
        message = finn_redact(conditionMessage(e)),
        data = list(error_class = class(e)),
        next_actions = c("Run diagnose_run.R for this project to find a fix.")
      )
    }
  )
  finn_emit(result)
  if (!interactive() && identical(result$status, "error")) {
    quit(save = "no", status = 1)
  }
  invisible(result)
}

#' Stop when required packages are missing
#'
#' @param pkgs Package names.
#' @return Invisible TRUE when all are installed.
finn_require <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      ". Run check_env.R, then install_finnts.R with the user's OK.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Hide secrets in text before showing or logging it
#'
#' @param x Character vector.
#' @return Character vector with keys, tokens, and SAS signatures masked.
finn_redact <- function(x) {
  x <- gsub("(?i)(api[_-]?key|token|secret|password|sig)=([^&\\s\"']+)", "\\1=***", x, perl = TRUE)
  x <- gsub("(?i)(bearer\\s+)[A-Za-z0-9._~+/=-]{12,}", "\\1***", x, perl = TRUE)
  x <- gsub("gh[pousr]_[A-Za-z0-9]{20,}", "***", x, perl = TRUE)
  gsub("sk-[A-Za-z0-9_-]{20,}", "***", x, perl = TRUE)
}

# ---------------------------------------------------------------------------
# JSON files and settings
# ---------------------------------------------------------------------------

#' Read a JSON file
#'
#' A file caught mid-write or mid-sync (for example by OneDrive) can be empty
#' or truncated for a moment, so one failed parse is retried after a short
#' pause before giving up with a plain-language error of class
#' `finn_json_error`.
#'
#' @param path File path.
#' @param default Value returned when the file does not exist.
#' @param retry_wait Seconds to wait before the single retry.
#' @return Parsed list, or `default`.
finn_read_json <- function(path, default = NULL, retry_wait = 1) {
  if (is.null(path) || !file.exists(path)) {
    return(default)
  }
  parse <- function() {
    jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  }
  first <- tryCatch(parse(), error = function(e) e)
  if (!inherits(first, "error")) {
    return(first)
  }
  Sys.sleep(retry_wait)
  second <- tryCatch(parse(), error = function(e) e)
  if (!inherits(second, "error")) {
    return(second)
  }
  stop(structure(class = c("finn_json_error", "error", "condition"), list(
    message = paste0(
      "Could not read ", path, ": the file is damaged or still syncing. ",
      "Wait a minute and try again; if it keeps failing, check the folder for OneDrive conflict copies. (",
      conditionMessage(second), ")"
    ),
    call = NULL
  )))
}

#' Acquire an exclusive cross-process lock
#'
#' Atomic directory creation admits one owner. A sidecar directory preserves
#' compatibility with legacy token files, which block acquisition until released
#' or stale. Released directories are archived, never deleted.
#'
#' @param path Lock file path.
#' @param timeout Seconds to keep trying.
#' @param stale Seconds after which a held lock counts as abandoned.
#' @return Owner token, or NULL when the lock could not be acquired in time.
finn_lock_acquire <- function(path, timeout = 15, stale = 120) {
  if (file.exists(path)) {
    cur <- readLines(path, n = 1, warn = FALSE)
    age <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "secs"))
    if (length(cur) && !identical(cur, "released") && nzchar(cur) && age <= stale) return(NULL)
  }
  finn_directory_lock_acquire(paste0(path, ".d"), timeout, stale)
}

#' Release a lock acquired with [finn_lock_acquire()]
#'
#' Only the owner releases it; the directory is archived.
#'
#' @param path Lock file path.
#' @param token Owner token returned by [finn_lock_acquire()].
#' @return Invisible logical, TRUE when released.
finn_lock_release <- function(path, token) {
  finn_directory_lock_release(paste0(path, ".d"), token)
}

#' Write a JSON file atomically
#'
#' Writes to a sibling temporary file and renames it over the target so a
#' crash or sync client never sees a half-written file.
#'
#' @param x Object to write.
#' @param path Destination path.
#' @return Invisible path.
finn_write_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  text <- finn_to_json(x, pretty = TRUE)
  tmp <- paste0(path, ".tmp")
  writeLines(text, tmp, useBytes = TRUE)
  if (!isTRUE(file.rename(tmp, path))) {
    writeLines(text, path, useBytes = TRUE)
  }
  invisible(path)
}

#' Current user's home directory
#'
#' Uses USERPROFILE on Windows because R maps `~` to Documents there.
#'
#' @return Home directory path.
finn_home <- function() {
  home <- if (.Platform$OS.type == "windows") Sys.getenv("USERPROFILE") else Sys.getenv("HOME")
  if (!nzchar(home)) home <- path.expand("~")
  normalizePath(home, winslash = "/", mustWork = FALSE)
}

#' Directory holding the user's Finn settings
#'
#' @return `FINN_SETTINGS_DIR` when set (used by tests), else `~/.finn`.
finn_settings_dir <- function() {
  override <- Sys.getenv("FINN_SETTINGS_DIR")
  if (nzchar(override)) {
    return(normalizePath(override, winslash = "/", mustWork = FALSE))
  }
  file.path(finn_home(), ".finn")
}

#' Load user settings with defaults
#'
#' An unreadable settings file falls back to defaults so the skill keeps
#' working; the user is then asked to confirm the storage root again.
#'
#' @return Settings list with `root`, `root_confirmed`,
#'   `large_onedrive_runs`, and `auto_update_check`.
finn_load_settings <- function() {
  defaults <- list(
    schema_version = finn_schema_version, root = NULL,
    root_confirmed = FALSE, large_onedrive_runs = 0L,
    move_suggestion_dismissed = FALSE, auto_update_check = TRUE
  )
  saved <- tryCatch(finn_read_json(file.path(finn_settings_dir(), "settings.json"), list()), error = function(e) {
    finn_log("Settings file could not be read; using defaults. ", conditionMessage(e))
    list()
  })
  utils::modifyList(defaults, saved)
}

#' Save user settings
#'
#' @param settings Settings list.
#' @return Invisible settings path.
finn_save_settings <- function(settings) {
  settings$updated_at <- finn_now()
  finn_write_json(settings, file.path(finn_settings_dir(), "settings.json"))
}

# ---------------------------------------------------------------------------
# Storage root and projects
# ---------------------------------------------------------------------------

#' Normalize a path for comparison and display
#'
#' @param path Path.
#' @return Forward-slash normalized path.
finn_norm <- function(path) {
  normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
}

#' Whether a path lives inside a OneDrive-synced folder
#'
#' @param path Path to test.
#' @return TRUE when the path is under a OneDrive root or named OneDrive.
finn_is_onedrive <- function(path) {
  if (is.null(path) || length(path) == 0 || !nzchar(path)) {
    return(FALSE)
  }
  p <- tolower(finn_norm(path))
  roots <- Sys.getenv(c("OneDriveCommercial", "OneDrive", "OneDriveConsumer"))
  roots <- roots[nzchar(roots)]
  roots <- if (length(roots)) tolower(finn_norm(roots)) else character()
  any(startsWith(p, roots)) || grepl("onedrive", p, fixed = TRUE)
}

#' Suggest the default Finn storage root
#'
#' Prefers the work OneDrive, then personal OneDrive, then macOS CloudStorage
#' OneDrive, then a local Documents folder. Never a temporary directory.
#'
#' @return List with `path`, `source`, and `is_onedrive`.
finn_detect_default_root <- function() {
  for (var in c("OneDriveCommercial", "OneDrive", "OneDriveConsumer")) {
    value <- Sys.getenv(var)
    if (nzchar(value) && dir.exists(value)) {
      return(list(path = file.path(finn_norm(value), "Finn"), source = var, is_onedrive = TRUE))
    }
  }
  cloud <- file.path(finn_home(), "Library", "CloudStorage")
  if (dir.exists(cloud)) {
    od <- sort(list.dirs(cloud, recursive = FALSE, full.names = TRUE))
    od <- od[grepl("^OneDrive", basename(od))]
    if (length(od) > 0) {
      return(list(path = file.path(finn_norm(od[[1]]), "Finn"), source = "macOS CloudStorage", is_onedrive = TRUE))
    }
  }
  list(path = file.path(finn_home(), "Documents", "Finn"), source = "Documents", is_onedrive = FALSE)
}

#' Turn a user-supplied project name into a safe folder name
#'
#' @param x Project name.
#' @return Name using letters, digits, underscores, and hyphens (max 40).
finn_sanitize_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", trimws(as.character(x)))
  x <- gsub("_+", "_", gsub("^[_-]+|[_-]+$", "", x))
  x <- substr(x, 1, 40)
  if (!nzchar(x)) stop("Project name must contain at least one letter or digit.", call. = FALSE)
  x
}

#' Paths that make up one Finn project
#'
#' @param root Finn storage root.
#' @param project Sanitized project folder name.
#' @return Named list of project paths.
finn_project_paths <- function(root, project) {
  dir <- file.path(finn_norm(root), project)
  list(
    root = finn_norm(root), name = project, dir = dir,
    project_json = file.path(dir, "project.json"),
    input = file.path(dir, "input"),
    output = file.path(dir, "output"),
    artifacts = file.path(dir, "finn_artifacts"),
    runs = file.path(dir, "runs"),
    analysis = file.path(dir, "analysis")
  )
}

#' Create a project's folders if missing
#'
#' @param paths Paths from [finn_project_paths()].
#' @return Invisible paths.
finn_ensure_project_dirs <- function(paths) {
  for (p in paths[c("dir", "input", "output", "artifacts", "runs", "analysis")]) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  readme <- file.path(paths$artifacts, "README.txt")
  if (!file.exists(readme)) {
    writeLines(c(
      "Managed by Finn. Do not edit, rename, or delete files in this folder.",
      "It holds the finnts run history used to resume, update, and analyze forecasts."
    ), readme)
  }
  invisible(paths)
}

#' Resolve the storage root from arguments or settings
#'
#' @param args Parsed arguments; `--root` overrides saved settings.
#' @return Root path, or NULL when none is configured.
finn_resolve_root <- function(args) {
  root <- finn_arg(args, "root")
  if (is.null(root)) root <- finn_load_settings()$root
  if (is.null(root) || !nzchar(root)) NULL else finn_norm(root)
}

#' Resolve and validate a project's paths from arguments
#'
#' @param args Parsed arguments containing `--project` and optionally `--root`.
#' @param must_exist Whether project.json must already exist.
#' @return Paths list from [finn_project_paths()].
finn_project_from_args <- function(args, must_exist = TRUE) {
  root <- finn_resolve_root(args)
  if (is.null(root)) {
    stop("No Finn folder is set. Run setup_project.R --action=status and confirm a location with the user.", call. = FALSE)
  }
  project <- finn_arg(args, "project")
  if (is.null(project)) stop("Pass --project=<name>.", call. = FALSE)
  paths <- finn_project_paths(root, finn_sanitize_name(project))
  if (must_exist && !file.exists(paths$project_json)) {
    stop("Project '", project, "' was not found under ", root, ". Create it with setup_project.R --action=create.", call. = FALSE)
  }
  paths
}

#' Read project metadata
#'
#' @param paths Project paths.
#' @return Project list, or NULL.
finn_read_project <- function(paths) finn_read_json(paths$project_json)

#' Write project metadata
#'
#' @param paths Project paths.
#' @param project Project list.
#' @return Invisible path.
finn_write_project <- function(paths, project) {
  project$updated_at <- finn_now()
  finn_write_json(project, paths$project_json)
}

#' Store a file path relative to the project folder when it lives inside it
#'
#' Relative paths keep projects working after the Finn folder is copied.
#'
#' @param paths Project paths.
#' @param file File path.
#' @return Relative path with forward slashes, or the absolute path.
finn_project_relpath <- function(paths, file) {
  if (is.null(file)) {
    return(NULL)
  }
  full <- finn_norm(file)
  base <- paste0(finn_norm(paths$dir), "/")
  if (startsWith(tolower(full), tolower(base))) substring(full, nchar(base) + 1L) else full
}

#' Resolve a stored data file path for a project
#'
#' Relative paths resolve against the project folder. An absolute path that
#' no longer exists falls back to the same file name in `input/`.
#'
#' @param paths Project paths.
#' @param file Stored path.
#' @return Absolute path, or NULL.
finn_resolve_data_file <- function(paths, file) {
  if (is.null(file) || !nzchar(file)) {
    return(NULL)
  }
  if (!grepl("^([A-Za-z]:|/|\\\\)", file)) {
    return(finn_norm(file.path(paths$dir, file)))
  }
  if (file.exists(file)) {
    return(finn_norm(file))
  }
  fallback <- file.path(paths$input, basename(file))
  if (file.exists(fallback)) finn_norm(fallback) else file
}

#' Read a project's run config with data and name filled in
#'
#' @param paths Project paths.
#' @param config_file Config path; defaults to `<project>/config.json`.
#' @return Config list whose `data$file` is an absolute path.
finn_read_project_config <- function(paths, config_file = NULL) {
  config_file <- config_file %||% file.path(paths$dir, "config.json")
  if (!file.exists(config_file)) {
    stop("No config found at ", config_file, ". Write one from templates/config.template.json, then run validate_config.R --save=true.", call. = FALSE)
  }
  cfg <- finn_read_json(config_file, list())
  project <- finn_read_project(paths) %||% list()
  cfg$project_name <- paths$name
  if (is.null(cfg$data$file)) {
    cfg$data$file <- project$data_file
    cfg$data$sheet <- cfg$data$sheet %||% project$sheet
  }
  cfg$data$file <- finn_resolve_data_file(paths, cfg$data$file)
  cfg
}

#' Save a run config with the data path stored portably
#'
#' @param paths Project paths.
#' @param cfg Config list.
#' @param target Destination path.
#' @return Invisible destination path.
finn_save_project_config <- function(paths, cfg, target = file.path(paths$dir, "config.json")) {
  cfg$data$file <- finn_project_relpath(paths, cfg$data$file)
  finn_write_json(cfg, target)
  invisible(target)
}

# ---------------------------------------------------------------------------
# Runs, state, and process liveness
# ---------------------------------------------------------------------------

#' Current time as an ISO 8601 string
#'
#' @return Character timestamp in local time with offset.
finn_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

#' Compact timestamp for identifiers
#'
#' @return Character such as "20260810-101500".
finn_stamp <- function() format(Sys.time(), "%Y%m%d-%H%M%S")

#' New skill run identifier
#'
#' Standard runs also use it as the finnts `run_name`.
#'
#' @param mode "standard", "iterate", or "update".
#' @return Run identifier string.
finn_new_run_id <- function(mode) {
  prefix <- switch(mode,
    standard = "std",
    iterate = "agent",
    update = "update",
    mode
  )
  paste0(prefix, "-", finn_stamp())
}

#' New Agent request identifier
#'
#' @return Character id unique per call.
finn_new_request_id <- function() {
  paste0("finn-", finn_stamp(), "-", Sys.getpid(), "-", sprintf("%04d", sample.int(9999, 1)))
}

#' Paths for one skill run
#'
#' @param paths Project paths.
#' @param run_id Skill run id.
#' @return List with `dir`, `state`, `log`, and `output`.
finn_run_paths <- function(paths, run_id) {
  dir <- file.path(paths$runs, run_id)
  list(
    id = run_id, dir = dir,
    state = file.path(dir, "state.json"),
    log = file.path(dir, "run.log"),
    output = file.path(paths$output, run_id)
  )
}

#' Read a run's state
#'
#' @param run_paths Paths from [finn_run_paths()].
#' @return State list, or NULL.
finn_read_state <- function(run_paths) finn_read_json(run_paths$state)

#' Merge fields into a run's state file
#'
#' The read-modify-write runs under an exclusive lock so the launcher, worker,
#' and cancel script cannot overwrite each other's fields. A lock timeout raises
#' an actionable error without writing state. A damaged state file is renamed
#' to `state.json.corrupt-<stamp>` (kept for diagnosis) and a fresh state is
#' written from the new fields.
#'
#' @param run_paths Paths from [finn_run_paths()].
#' @param ... Named fields to set.
#' @return Updated state list (invisibly).
finn_update_state <- function(run_paths, ...) {
  lock <- paste0(run_paths$state, ".lock")
  token <- finn_lock_acquire(lock, timeout = 10, stale = 60)
  if (is.null(token)) stop("Could not lock run state. Another process is updating it; retry shortly: ",
    run_paths$state, call. = FALSE)
  on.exit(finn_lock_release(lock, token), add = TRUE)
  state <- tryCatch(finn_read_json(run_paths$state, retry_wait = 0.5), finn_json_error = function(e) {
    file.rename(run_paths$state, paste0(run_paths$state, ".corrupt-", finn_stamp()))
    finn_log("State file for ", run_paths$id, " was damaged; kept a copy and wrote a new one.")
    list(recovered_from_corrupt = finn_now())
  })
  if (is.null(state)) state <- list()
  updates <- list(...)
  for (nm in names(updates)) state[nm] <- list(updates[[nm]])
  state$updated_at <- finn_now()
  finn_write_json(state, run_paths$state)
  invisible(state)
}

#' Parse a timestamp written by [finn_now()]
#'
#' @param x ISO timestamp string or NULL.
#' @return POSIXct, or NA when missing or unparseable.
finn_parse_time <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(as.POSIXct(NA))
  }
  as.POSIXct(x[1], format = "%Y-%m-%dT%H:%M:%S%z")
}

#' Whether a recorded process is still the same live process
#'
#' Compares the creation time to guard against reused process ids.
#'
#' @param pid Process id.
#' @param create_time Recorded creation time in epoch seconds.
#' @return TRUE when alive and matching, FALSE when gone, NA when unknown.
finn_pid_alive <- function(pid, create_time = NULL) {
  if (is.null(pid) || length(pid) == 0 || is.na(pid)) {
    return(FALSE)
  }
  if (!requireNamespace("ps", quietly = TRUE)) {
    return(NA)
  }
  tryCatch(
    {
      h <- ps::ps_handle(as.integer(pid))
      if (!ps::ps_is_running(h)) {
        return(FALSE)
      }
      if (is.null(create_time)) {
        return(TRUE)
      }
      abs(as.numeric(ps::ps_create_time(h)) - as.numeric(create_time)) < 3
    },
    error = function(e) if (inherits(e, "no_such_process")) FALSE else NA
  )
}

#' Status of a run after checking process liveness
#'
#' A queued or running run whose worker is gone is reported as interrupted.
#' A queued run without a worker id yet is still being launched, so it stays
#' queued for `grace_secs` after its last update instead of looking
#' interrupted to a second launch or status check.
#'
#' @param state Run state list.
#' @param grace_secs Seconds a just-queued run without a worker id counts as
#'   still launching.
#' @return Status string.
finn_effective_status <- function(state, grace_secs = 120) {
  status <- state$status %||% "unknown"
  if (status %in% c("queued", "running")) {
    host_ok <- is.null(state$host) || identical(state$host, Sys.info()[["nodename"]])
    if (identical(status, "queued") && host_ok && (is.null(state$pid) || length(state$pid) == 0)) {
      age <- as.numeric(difftime(Sys.time(), finn_parse_time(state$updated_at %||% state$created_at), units = "secs"))
      if (!is.na(age) && age < grace_secs) {
        return("queued")
      }
    }
    alive <- if (host_ok) finn_pid_alive(state$pid, state$pid_create_time) else NA
    if (isFALSE(alive)) {
      return("interrupted")
    }
  }
  status
}

#' List a project's runs, newest first
#'
#' A run whose state file cannot be read (damaged or mid-sync) is listed with
#' status "unreadable" instead of breaking the whole listing. It sorts by its
#' folder's modification time.
#'
#' @param paths Project paths.
#' @param exclude_states Exact state paths already read by the caller.
#' @return List of state lists, each with `effective_status`.
finn_list_runs <- function(paths, exclude_states = character()) {
  if (!dir.exists(paths$runs)) {
    return(list())
  }
  dirs <- list.dirs(paths$runs, recursive = FALSE, full.names = FALSE)
  states <- lapply(dirs, function(d) {
    rp <- finn_run_paths(paths, d)
    if (rp$state %in% exclude_states) return(NULL)
    s <- tryCatch(finn_read_json(rp$state, retry_wait = 0.2), error = function(e) e)
    if (inherits(s, "error")) {
      return(list(
        run_id = d, status = "unreadable", effective_status = "unreadable",
        error = conditionMessage(s),
        created_at = format(file.mtime(rp$dir), "%Y-%m-%dT%H:%M:%S%z")
      ))
    }
    if (is.null(s)) {
      return(NULL)
    }
    s$run_id <- s$run_id %||% d
    s$effective_status <- finn_effective_status(s)
    s
  })
  states <- Filter(Negate(is.null), states)
  if (length(states) == 0) {
    return(list())
  }
  created <- vapply(states, function(s) s$created_at %||% "", character(1))
  states[order(created, decreasing = TRUE)]
}

#' The project's currently active run, if any
#'
#' @param paths Project paths.
#' @return State list of the queued or running run, or NULL.
finn_active_run <- function(paths) {
  for (s in finn_list_runs(paths)) {
    if (s$effective_status %in% c("queued", "running")) {
      return(s)
    }
  }
  NULL
}

#' Whether a run was started on a different computer
#'
#' Runs recorded on another machine cannot be checked or cancelled here,
#' which matters when the Finn folder syncs through OneDrive.
#'
#' @param state Run state list.
#' @return Logical.
finn_other_host <- function(state) {
  !is.null(state$host) && !identical(state$host, Sys.info()[["nodename"]])
}

#' Installed finnts and R versions
#'
#' @return List with `finnts_version` and `r_version` (NULL when unknown).
finn_versions <- function() {
  finnts <- if (requireNamespace("finnts", quietly = TRUE)) as.character(utils::packageVersion("finnts"))
  list(finnts_version = finnts, r_version = paste(R.version$major, R.version$minor, sep = "."))
}

# ---------------------------------------------------------------------------
# Skill and finnts updates
# ---------------------------------------------------------------------------

#' GitHub repository that hosts the skill and finnts
#'
#' @return "owner/repo"; `FINN_SKILL_REPO` overrides it for forks and tests.
finn_skill_repo <- function() {
  repo <- Sys.getenv("FINN_SKILL_REPO")
  if (nzchar(repo)) repo else "microsoft/finnts"
}

#' Download a URL as text
#'
#' Sends a User-Agent header (required by the GitHub API) and honors the
#' standard proxy environment variables through `download.file()`.
#'
#' @param url URL to fetch.
#' @param timeout Seconds before giving up.
#' @return Character scalar with the response body; errors on failure.
finn_http_get <- function(url, timeout = 10) {
  old <- options(timeout = timeout)
  on.exit(options(old), add = TRUE)
  dest <- tempfile("finn-http-")
  status <- suppressWarnings(utils::download.file(
    url, dest,
    quiet = TRUE, mode = "wb",
    headers = c("User-Agent" = "finn-skill", Accept = "application/vnd.github+json")
  ))
  if (!identical(as.integer(status), 0L) || !file.exists(dest)) stop("Could not download ", url, call. = FALSE)
  paste(readLines(dest, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

#' Whether version `a` is newer than version `b`
#'
#' @param a,b Version strings; NULL or unparsable values count as unknown.
#' @return TRUE only when both parse and `a > b`.
finn_version_newer <- function(a, b) {
  pa <- tryCatch(package_version(a), error = function(e) NULL)
  pb <- tryCatch(package_version(b), error = function(e) NULL)
  if (is.null(pa) || is.null(pb) || length(pa) != 1 || length(pb) != 1) {
    return(FALSE)
  }
  pa > pb
}

#' Read `finn_skill_version` from finn_common.R source text
#'
#' @param text File contents.
#' @return Version string, or NULL when absent.
finn_parse_skill_version <- function(text) {
  m <- regmatches(text, regexpr("finn_skill_version\\s*<-\\s*\"[^\"]+\"", text))
  if (length(m) == 0) {
    return(NULL)
  }
  sub("^.*\"([^\"]+)\"$", "\\1", m)
}

#' Read the Version field from DESCRIPTION text
#'
#' @param text DESCRIPTION contents.
#' @return Version string, or NULL when absent.
finn_parse_desc_version <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  v <- grep("^Version:", lines, value = TRUE)
  if (length(v) == 0) {
    return(NULL)
  }
  trimws(sub("^Version:", "", v[[1]]))
}

#' Skill and finnts versions published on GitHub
#'
#' Resolves `ref` to a commit so the skill files and finnts package are
#' always taken from the same snapshot.
#'
#' @param ref Branch, tag, or commit.
#' @param fetch Function `(url, timeout)` returning text; injectable for tests.
#' @param timeout Seconds per request.
#' @param repo "owner/repo".
#' @return List with `ref`, `sha`, `skill_version`, and `finnts_version`.
finn_remote_versions <- function(ref = "main", fetch = finn_http_get, timeout = 10, repo = finn_skill_repo()) {
  commit <- jsonlite::fromJSON(fetch(paste0("https://api.github.com/repos/", repo, "/commits/", ref), timeout), simplifyVector = FALSE)
  sha <- commit$sha
  if (is.null(sha) || !nzchar(sha)) stop("GitHub did not return a commit for '", ref, "'.", call. = FALSE)
  raw <- function(path) fetch(paste0("https://raw.githubusercontent.com/", repo, "/", sha, "/", path), timeout)
  skill <- tryCatch(finn_parse_skill_version(raw("skills/finn/scripts/finn_common.R")), error = function(e) NULL)
  finnts <- tryCatch(finn_parse_desc_version(raw("DESCRIPTION")), error = function(e) NULL)
  list(ref = ref, sha = sha, skill_version = skill, finnts_version = finnts)
}

#' How finnts is installed on this machine
#'
#' @return List with `version`, `source` ("github", "cran", other, or NULL),
#'   `sha`, and `ref`; all NULL when finnts is not installed.
finn_installed_finnts <- function() {
  desc <- suppressWarnings(utils::packageDescription("finnts"))
  if (!is.list(desc)) {
    return(list(version = NULL, source = NULL, sha = NULL, ref = NULL))
  }
  type <- desc$RemoteType %||% if (identical(desc$Repository, "CRAN")) "cran" else NULL
  list(version = desc$Version, source = type, sha = desc$RemoteSha, ref = desc$RemoteRef)
}

#' Whether the automatic update check is turned on
#'
#' @param args Parsed arguments; `--check_updates=false` turns it off.
#' @return FALSE when disabled by argument, the `FINN_SKILL_NO_UPDATE_CHECK`
#'   environment variable, or the saved `auto_update_check` setting.
finn_update_check_enabled <- function(args = list()) {
  if (!finn_bool(finn_arg(args, "check_updates"), TRUE)) {
    return(FALSE)
  }
  if (finn_bool(Sys.getenv("FINN_SKILL_NO_UPDATE_CHECK"), FALSE)) {
    return(FALSE)
  }
  !isFALSE(finn_load_settings()$auto_update_check)
}

#' Check GitHub for a newer skill, at most once per interval
#'
#' Results are cached in the settings folder so routine preflight checks stay
#' fast and offline-friendly. Network errors are recorded, never raised, and a
#' failed check is retried after one day.
#'
#' @param force Ignore the cache.
#' @param max_age_days Cache lifetime.
#' @param ref Branch or tag to compare against.
#' @param fetch Function `(url, timeout)` returning text.
#' @param timeout Seconds per request.
#' @return List with local and remote versions, `update_available`,
#'   `finnts_update_available`, `checked_at`, `from_cache`, and `error`.
finn_update_check <- function(force = FALSE, max_age_days = 7, ref = "main", fetch = finn_http_get, timeout = 5) {
  cache_path <- file.path(finn_settings_dir(), "update_check.json")
  cache <- tryCatch(finn_read_json(cache_path), error = function(e) NULL)
  fresh <- !force && !is.null(cache) && identical(cache$ref, ref) &&
    identical(cache$local_skill_version, finn_skill_version) &&
    is.numeric(cache$checked_epoch) &&
    (as.numeric(Sys.time()) - cache$checked_epoch) < (if (is.null(cache$error)) max_age_days else min(1, max_age_days)) * 86400
  if (fresh) {
    remote <- cache[c("ref", "sha", "skill_version", "finnts_version")]
    checked_at <- cache$checked_at
    err <- cache$error
  } else {
    checked_at <- finn_now()
    remote <- tryCatch(finn_remote_versions(ref, fetch, timeout), error = function(e) e)
    err <- if (inherits(remote, "error")) finn_redact(conditionMessage(remote))
    if (!is.null(err)) remote <- list(ref = ref, sha = NULL, skill_version = NULL, finnts_version = NULL)
    tryCatch(finn_write_json(c(remote, list(
      local_skill_version = finn_skill_version, checked_at = checked_at,
      checked_epoch = as.numeric(Sys.time()), error = err
    )), cache_path), error = function(e) NULL)
  }
  local_finnts <- finn_installed_finnts()$version
  list(
    ref = ref, sha = remote$sha,
    local_skill_version = finn_skill_version, remote_skill_version = remote$skill_version,
    local_finnts_version = local_finnts, remote_finnts_version = remote$finnts_version,
    update_available = finn_version_newer(remote$skill_version, finn_skill_version),
    finnts_update_available = finn_version_newer(remote$finnts_version, local_finnts),
    checked_at = checked_at, from_cache = fresh, error = err
  )
}

#' Every queued or running run under a Finn storage root
#'
#' Used to block package or skill changes while a forecast is in flight.
#'
#' @param root Finn storage root, or NULL.
#' @return List of active run states, each with a `project` field.
finn_all_active_runs <- function(root) {
  if (is.null(root) || !dir.exists(root)) {
    return(list())
  }
  out <- list()
  for (d in list.dirs(root, recursive = FALSE, full.names = FALSE)) {
    paths <- finn_project_paths(root, d)
    if (!file.exists(paths$project_json)) next
    active <- tryCatch(finn_active_run(paths), error = function(e) NULL)
    if (!is.null(active)) {
      active$project <- d
      out[[length(out) + 1]] <- active
    }
  }
  out
}

#' Possible OneDrive conflict copies of Finn's own JSON files
#'
#' When two machines edit a synced file at once, OneDrive keeps both and
#' renames one, for example `state-LAPTOP.json` or `project (1).json`. Finn
#' only reads the original name, so a conflict copy can hide the newest
#' changes. Read-only: the user decides which copy to keep.
#'
#' @param paths Project paths.
#' @return Character vector of normalized file paths, possibly empty.
finn_conflict_copies <- function(paths) {
  scan <- function(dir, bases) {
    if (is.null(dir) || !dir.exists(dir)) {
      return(character())
    }
    pat <- paste0("^(", paste(bases, collapse = "|"), ")(-[^.]+| \\([0-9]+\\))\\.json$")
    list.files(dir, pattern = pat, full.names = TRUE)
  }
  found <- scan(paths$dir, c("project", "config"))
  if (dir.exists(paths$runs %||% "")) {
    for (d in list.dirs(paths$runs, recursive = FALSE, full.names = TRUE)) {
      found <- c(found, scan(d, "state"))
    }
  }
  found <- c(found, scan(finn_settings_dir(), "settings"))
  unique(vapply(found, finn_norm, character(1), USE.NAMES = FALSE))
}

#' Free disk space where a path lives
#'
#' @param path Existing folder or file path.
#' @return Free space in GB, or NA when it cannot be measured.
finn_free_disk_gb <- function(path) {
  if (is.null(path) || !requireNamespace("ps", quietly = TRUE)) {
    return(NA_real_)
  }
  while (!file.exists(path) && dirname(path) != path) path <- dirname(path)
  du <- tryCatch(ps::ps_disk_usage(path), error = function(e) NULL)
  if (is.null(du) || nrow(du) == 0) {
    return(NA_real_)
  }
  round(as.numeric(du$available[1]) / 1024^3, 1)
}

#' Plain-language warning when free disk space is low
#'
#' finnts writes many intermediate files, and a full disk fails runs part
#' way through.
#'
#' @param path Folder to check.
#' @param min_gb Threshold in GB.
#' @return Warning string, or NULL when space is fine or unknown.
finn_disk_warning <- function(path, min_gb = 5) {
  free <- finn_free_disk_gb(path)
  if (is.na(free) || free >= min_gb) {
    return(NULL)
  }
  sprintf("Only %.1f GB of disk space is free where Finn saves files. Runs can fail part way when the disk fills; free up space or move the Finn folder first.", free)
}

#' Append an entry to the skill and package update history
#'
#' Kept in the settings folder (not in any project) so rollbacks know which
#' finnts build to return to.
#'
#' @param entry Named list describing the change.
#' @return Invisible history path.
finn_append_history <- function(entry) {
  path <- file.path(finn_settings_dir(), "update_history.json")
  history <- tryCatch(finn_read_json(path, list()), error = function(e) list())
  history[[length(history) + 1]] <- c(list(at = finn_now()), entry)
  finn_write_json(history, path)
}

#' Rough runtime range for a run
#'
#' Coarse buckets from series count, mode, and parallel workers. Real time
#' varies with data size, models, and machine load, so callers must present
#' it as a rough guide.
#'
#' @param n_series Number of time series.
#' @param mode "standard", "iterate", or "update".
#' @param workers Parallel workers in use (NULL or 1 for sequential).
#' @return List with `low_min`, `high_min`, and `text` (a single value when
#'   both ends of the range round to the same label).
finn_estimate_runtime <- function(n_series, mode = "standard", workers = NULL) {
  n <- max(as.numeric(n_series %||% 1), 1)
  w <- max(as.numeric(workers %||% 1), 1)
  per_series <- c(low = 0.5, high = 2)
  mult <- switch(mode, iterate = 4, update = 1.5, 1)
  mins <- 3 + per_series * n * mult / w
  mins <- pmin(mins, 72 * 60)
  fmt <- function(m) if (m < 90) sprintf("%d minutes", as.integer(ceiling(m))) else sprintf("%.1f hours", m / 60)
  low <- fmt(mins[["low"]])
  high <- fmt(mins[["high"]])
  list(
    low_min = round(mins[["low"]]), high_min = round(mins[["high"]]),
    text = if (identical(low, high)) paste0("roughly ", low) else paste0("roughly ", low, " to ", high)
  )
}

#' Disk usage of the main folders in a project
#'
#' Read-only. Lists sizes so the user can decide what to archive by hand.
#'
#' @param paths Project paths.
#' @return List with `total_mb` and `folders`, a list of `name`, `path`, `mb`.
finn_disk_usage <- function(paths) {
  size_mb <- function(dir) {
    if (!dir.exists(dir)) {
      return(0)
    }
    files <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
    round(sum(file.size(files), na.rm = TRUE) / 1024^2, 1)
  }
  dirs <- list(input = paths$input, finn_artifacts = paths$artifacts, runs = paths$runs, output = paths$output, analysis = paths$analysis)
  folders <- lapply(names(dirs), function(n) list(name = n, path = dirs[[n]], mb = size_mb(dirs[[n]] %||% "")))
  list(total_mb = sum(vapply(folders, function(f) f$mb, numeric(1))), folders = folders)
}

# ---------------------------------------------------------------------------
# Machine resources and parallel strategy
# ---------------------------------------------------------------------------

#' Machine cores and memory
#'
#' @return List with `cores`, `free_gb`, and `total_gb` (NA when unknown).
finn_system_resources <- function() {
  cores <- parallel::detectCores(logical = TRUE)
  mem <- if (requireNamespace("ps", quietly = TRUE)) {
    tryCatch(ps::ps_system_memory(), error = function(e) NULL)
  }
  list(
    cores = as.integer(cores),
    free_gb = if (is.null(mem)) NA_real_ else round(mem$avail / 1024^3, 1),
    total_gb = if (is.null(mem)) NA_real_ else round(mem$total / 1024^3, 1)
  )
}

#' Machine-wide CPU and memory planning budgets
#'
#' Leaves one logical processor available and estimates 300 MB per worker
#' within 80 percent of total RAM. Unknown resources allow only sequential
#' work and are explicitly reported; available RAM is a warning signal.
#'
#' @param resources List from [finn_system_resources()].
#' @return List with `cpu_budget`, `ram_budget_gb`, and `warnings`.
finn_resource_budget <- function(resources) {
  valid <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
  warnings <- character()
  cpu <- if (valid(resources$cores)) max(1L, floor(resources$cores) - 1L) else {
    warnings <- c(warnings, "Could not detect the logical processor count; using sequential processing.")
    1L
  }
  ram <- if (valid(resources$total_gb)) resources$total_gb * 0.8 else {
    warnings <- c(warnings, "Could not detect total RAM; using sequential processing.")
    cpu <- min(cpu, 1L)
    0.3
  }
  list(cpu_budget = as.integer(cpu), ram_budget_gb = ram, warnings = warnings)
}

#' Whether global models run by default for a date type
#'
#' Mirrors the finnts default of skipping global models for weekly and daily
#' data, where they are slow and memory hungry.
#'
#' @param date_type finnts date type.
#' @return Logical.
finn_default_global_models <- function(date_type) {
  !(date_type %in% c("week", "day"))
}

#' Choose a parallel processing setup for a run
#'
#' `local_machine` spreads time series across worker processes and suits many
#' series. `inner_parallel` parallelizes model training inside each series and
#' suits few series or global models. finnts rejects both together.
#'
#' @param n_series Number of time series.
#' @param data_gb Approximate in-memory input size in GB.
#' @param global_models Whether global models will run.
#' @param resources List from [finn_system_resources()].
#' @param step_down Fallback level after memory failures: 0 normal, 1 halve
#'   workers, 2 inner parallel only, 3 sequential.
#' @return List with `parallel_processing`, `inner_parallel`, `num_cores`,
#'   `safe_workers`, `warnings`, and `reason`.
finn_choose_parallel <- function(n_series, data_gb, global_models,
                                 resources = finn_system_resources(), step_down = 0L) {
  budget <- finn_resource_budget(resources)
  cpu <- resources$cpu_budget %||% budget$cpu_budget
  ram <- resources$ram_budget_gb %||% budget$ram_budget_gb
  safe <- min(cpu, floor((ram + 1e-9) / finn_worker_gb(data_gb)))
  safe <- max(as.integer(safe), 0L)

  plan <- function(pp, inner, n, reason) {
    workers <- n %||% 1L
    warnings <- budget$warnings
    free <- resources$free_gb
    if (is.null(free) || length(free) != 1L || !is.finite(free)) {
      warnings <- c(warnings, "Available RAM could not be detected; actual worker memory may exceed the 300 MB estimate.")
    } else if (workers * 0.3 > free) {
      warnings <- c(warnings, "Available RAM is below the estimated worker memory. Windows or your operating system may reclaim or page memory, slowing Finn and other applications.")
    }
    list(parallel_processing = pp, inner_parallel = inner, num_cores = n, safe_workers = safe,
      detected_cores = resources$cores, total_gb = resources$total_gb,
      ram_budget_gb = ram, warnings = warnings,
      reason = paste0(reason, " Uses ", workers, " worker(s); 300 MB per worker is an estimate, not a memory limit."))
  }

  if (step_down >= 3L || safe < 2L) {
    return(plan(NULL, FALSE, NULL, "Sequential: limited CPU/RAM budget, or earlier memory failures."))
  }
  inner_plan <- plan(NULL, TRUE, safe, "Inner parallel: few series or global models, so models train in parallel within each series.")
  if (step_down >= 2L || n_series <= 1L) {
    return(inner_plan)
  }
  if (isTRUE(global_models) && n_series < 4L * safe) {
    return(inner_plan)
  }
  if (n_series >= 2L * safe) {
    workers <- min(n_series, if (step_down >= 1L) max(2L, safe %/% 2L) else safe)
    return(plan("local_machine", FALSE, workers, "Local machine: many series, so series train in parallel worker processes."))
  }
  inner_plan
}

#' Approximate memory one Finn worker process needs
#'
#' @param data_gb Input size, retained for compatibility; not used in sizing.
#' @return GB per worker, the same estimate [finn_choose_parallel()] uses.
finn_worker_gb <- function(data_gb) {
  0.3
}

#' Record the cores and memory a parallel plan may use
#'
#' Stored in run state so launches in other projects can see how much of the
#' machine this run has claimed.
#'
#' @param plan Plan from [finn_choose_parallel()] or a sequential plan.
#' @param data_gb Approximate in-memory input size in GB.
#' @return `plan` with `claimed_cores` and `claimed_gb` added.
finn_plan_claim <- function(plan, data_gb) {
  workers <- max(1L, as.integer(plan$num_cores %||% 1L))
  plan$claimed_cores <- workers
  plan$claimed_gb <- round(workers * finn_worker_gb(data_gb), 1)
  plan
}

#' Cores and memory claimed by active runs on this computer
#'
#' Reads registered state files across storage roots and discovers legacy runs
#' in known roots. Missing or unreadable registered states retain their claims.
#' Terminal runs retain claims until their recorded process has exited.
#' Older claims are preserved; runs without memory claims count at 2 GB/core.
#'
#' @param root Finn storage root.
#' @param exclude_project Project folder name to skip for a settings preview
#'   in the selected root. Launches count even a terminal run until it exits.
#' @return List with `cores`, `gb`, `runs` (compact claims), and `states`.
finn_claimed_resources <- function(root, exclude_project = NULL) {
  registry <- finn_read_json(file.path(finn_settings_dir(), "machine-runs.json"), list(roots = list(), runs = list()))
  runs <- registry$runs %||% list()
  known <- vapply(runs, function(s) s$state_file, character(1))
  roots <- unique(c(unlist(registry$roots), root))
  for (r in roots) {
    if (!dir.exists(r)) next
    for (d in list.dirs(r, recursive = FALSE, full.names = FALSE)) {
      paths <- finn_project_paths(r, d)
      if (!file.exists(paths$project_json)) next
      for (s in finn_list_runs(paths, exclude_states = known)) {
        if (identical(s$effective_status, "unreadable")) {
          stop("Cannot determine resource claims for ", d, ": ", s$error, call. = FALSE)
        }
        if (s$effective_status %in% c("queued", "running") && !finn_other_host(s)) {
          s$root <- finn_norm(r)
          s$project <- d
          s$state_file <- finn_run_paths(paths, s$run_id)$state
          if (!any(vapply(runs, function(x) identical(x$state_file, s$state_file), logical(1)))) {
            runs[[length(runs) + 1L]] <- s
          }
        }
      }
    }
  }
  runs <- Filter(Negate(is.null), lapply(runs, function(saved) {
    s <- tryCatch(finn_read_json(saved$state_file), finn_json_error = function(e) {
      finn_log("Retaining resource claim: ", conditionMessage(e))
      NULL
    })
    if (is.null(s)) {
      finn_log("Cannot read registered run state; retaining its resource claim: ", saved$state_file)
      s <- saved
      s$allocation_uncertain <- TRUE
    }
    s$state_file <- saved$state_file
    s$root <- saved$root
    s$project <- saved$project
    s$launcher_pid <- saved$launcher_pid
    s$launcher_create_time <- saved$launcher_create_time
    if (finn_other_host(s)) return(NULL)
    if (!isTRUE(s$allocation_uncertain)) {
      alive <- finn_pid_alive(s$pid, s$pid_create_time)
      uncertain <- is.na(alive)
      if (is.null(s$pid) && !s$status %in% c("queued", "completed", "failed", "cancelled", "interrupted")) {
        alive <- NA
        uncertain <- TRUE
      }
      if (isFALSE(alive)) {
        if (!identical(s$status, "queued") || !is.null(s$pid)) return(NULL)
        launching <- finn_pid_alive(s$launcher_pid, s$launcher_create_time)
        if (isFALSE(launching) && !identical(finn_effective_status(s), "queued")) return(NULL)
        uncertain <- is.na(launching)
      }
      s$allocation_uncertain <- uncertain
    }
    if (!is.null(exclude_project) && identical(s$project, exclude_project) &&
        identical(finn_norm(s$root), finn_norm(root))) return(NULL)
    s$effective_status <- finn_effective_status(s)
    s
  }))
  claims <- lapply(runs, function(s) {
    p <- s$parallel %||% list()
    cores <- as.integer(p$claimed_cores %||% max(1L, as.integer(p$num_cores %||% 1L)))
    gb <- as.numeric(p$claimed_gb %||% (cores * 2))
    if (length(cores) != 1L || !is.finite(cores) || cores < 1L ||
        length(gb) != 1L || !is.finite(gb) || gb <= 0) {
      stop("Invalid resource claim for ", s$project, "/", s$run_id, ". Check its run state before launching more forecasts.", call. = FALSE)
    }
    list(project = s$project, root = s$root, state_file = s$state_file,
      run_id = s$run_id, mode = s$mode, status = s$effective_status,
      stage = s$stage, claimed_cores = cores, claimed_gb = gb,
      allocation_uncertain = isTRUE(s$allocation_uncertain))
  })
  list(
    cores = sum(vapply(claims, function(x) x$claimed_cores, integer(1))),
    gb = sum(vapply(claims, function(x) x$claimed_gb, numeric(1))),
    runs = claims, states = runs
  )
}

#' Register a queued run in the local cross-root allocation registry
#'
#' Called while holding the machine launch lock, before starting the worker.
#' Reconciliation discards only confirmed exited allocations. Forecast files
#' are untouched; the registry contains bookkeeping and exact state paths.
#'
#' @param paths Project paths.
#' @param run_paths Run paths.
#' @param claimed Reconciled claims from [finn_claimed_resources()].
#' @return Invisible registry path.
finn_register_allocation <- function(paths, run_paths, claimed) {
  file <- file.path(finn_settings_dir(), "machine-runs.json")
  registry <- finn_read_json(file, list(roots = list()))
  state <- finn_read_state(run_paths)
  state$root <- finn_norm(paths$root)
  state$project <- paths$name
  state$state_file <- run_paths$state
  state$launcher_pid <- Sys.getpid()
  state$launcher_create_time <- if (requireNamespace("ps", quietly = TRUE)) as.numeric(ps::ps_create_time(ps::ps_handle()))
  states <- Filter(function(s) !identical(s$state_file, run_paths$state), claimed$states %||% list())
  states[[length(states) + 1L]] <- state
  fields <- c("root", "project", "state_file", "run_id", "mode", "status",
    "host", "parallel", "updated_at", "pid", "pid_create_time", "launcher_pid", "launcher_create_time")
  states <- lapply(states, function(s) s[intersect(fields, names(s))])
  finn_write_json(list(roots = as.list(unique(c(unlist(registry$roots), state$root))), runs = states), file)
}

#' Acquire the machine allocation lock using an atomic directory creation
#'
#' Unlike a read/write lock file, directory creation admits only one launcher.
#' Released directories are renamed and kept, never deleted. A dead owner can
#' be recovered; unknown liveness is never treated as an abandoned lock.
#'
#' @param timeout Seconds to wait for another launcher.
#' @return Owner token, or NULL when busy.
finn_machine_lock_acquire <- function(timeout = 30) {
  finn_directory_lock_acquire(file.path(finn_settings_dir(), "machine-launch-lock"), timeout)
}

#' Acquire an atomic directory lock, recovering only confirmed dead local owners
#'
#' @param path Directory to create exclusively.
#' @param timeout Seconds to wait.
#' @param stale Seconds before an ownerless directory requires manual recovery.
#' @return Owner token or NULL when busy; unreadable ownership raises an error.
finn_directory_lock_acquire <- function(path, timeout = 30, stale = 120) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  token <- paste0(Sys.getpid(), "-", format(Sys.time(), "%Y%m%d%H%M%OS6"), "-", sample.int(999999, 1))
  deadline <- Sys.time() + timeout
  repeat {
    if (dir.create(path, showWarnings = FALSE)) {
      created <- if (requireNamespace("ps", quietly = TRUE)) as.numeric(ps::ps_create_time(ps::ps_handle()))
      finn_write_json(list(token = token, pid = Sys.getpid(), create_time = created,
        host = Sys.info()[["nodename"]]), file.path(path, "owner.json"))
      return(token)
    }
    owner_file <- file.path(path, "owner.json")
    owner <- tryCatch(finn_read_json(owner_file), finn_json_error = function(e) {
      if (!file.exists(owner_file)) return(NULL)
      stop(e)
    })
    age <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "secs"))
    dead <- !is.null(owner) && (is.null(owner$host) || identical(owner$host, Sys.info()[["nodename"]])) &&
      isFALSE(finn_pid_alive(owner$pid, owner$create_time))
    if (dead) {
      # Only one contender may recover this owner; a late observer must never
      # rename a newly acquired live lock in place of the dead one.
      recovery <- paste0(path, ".recovery-", owner$token)
      if (dir.create(recovery, showWarnings = FALSE)) {
        current <- finn_read_json(file.path(path, "owner.json"))
        if (!is.null(current) && identical(current$token, owner$token) &&
            file.rename(path, paste0(path, ".abandoned-", owner$token))) next
      }
    }
    if (is.null(owner) && is.finite(age) && age > stale) {
      stop("Finn's lock has no recorded owner. Check that no operation is active before moving the lock directory aside: ",
        path, call. = FALSE)
    }
    if (Sys.time() >= deadline) return(NULL)
    Sys.sleep(0.2)
  }
}

#' Runs that must finish before changing the installed software
#'
#' Includes local allocations across roots and remote runs in the selected
#' synced root. Uncertain allocations block updates conservatively.
#'
#' @param root Selected storage root, or NULL.
#' @return List of run states, deduplicated by root, project and run id.
finn_software_active_runs <- function(root) {
  selected <- finn_all_active_runs(root)
  selected <- lapply(selected, function(s) {
    s$root <- finn_norm(root)
    s
  })
  runs <- c(selected, finn_claimed_resources(root)$states)
  keys <- vapply(runs, function(s) paste(s$root, s$project, s$run_id, sep = "\n"), character(1))
  runs[!duplicated(keys)]
}

#' Release the machine allocation lock without deleting files
#'
#' @param token Token from [finn_machine_lock_acquire()].
#' @return Invisible logical indicating release.
finn_machine_lock_release <- function(token) {
  finn_directory_lock_release(file.path(finn_settings_dir(), "machine-launch-lock"), token)
}

#' Exclude forecasts while changing software, allowing a verified installer handoff
#'
#' The updater passes its held token to its child installer, avoiding recursive
#' acquisition. Standalone operations acquire the launch lock themselves.
#'
#' @param args Parsed arguments with optional internal `software_lock` token.
#' @return List with token, owned flag, and an optional blocked result.
finn_software_guard <- function(args) {
  token <- finn_arg(args, "software_lock")
  owned <- is.null(token)
  if (owned) {
    token <- finn_machine_lock_acquire()
  } else {
    owner <- finn_read_json(file.path(finn_settings_dir(), "machine-launch-lock", "owner.json"))
    if (is.null(owner) || !identical(owner$token, token) ||
        !isTRUE(finn_pid_alive(owner$pid, owner$create_time))) {
      stop("The updater's software lock is no longer held. Retry the update.", call. = FALSE)
    }
  }
  if (is.null(token)) {
    return(list(result = finn_result(FALSE, "busy", "Another Finn launch or software change is in progress.",
      next_actions = "Wait for it to finish, then retry.")))
  }
  active <- tryCatch(finn_software_active_runs(finn_resolve_root(args)), error = function(e) {
    if (owned) finn_machine_lock_release(token)
    stop(e)
  })
  if (length(active)) {
    if (owned) finn_machine_lock_release(token)
    return(list(result = finn_result(FALSE, "blocked", "A forecast is running. Changing Finn now could break it.",
      list(active_runs = lapply(active, function(s) list(root = s$root, project = s$project, run_id = s$run_id))),
      "Wait for forecasts to finish, or cancel with the user's OK, then retry.")))
  }
  list(token = token, owned = owned)
}

#' Archive an owned directory lock without deleting files
#'
#' @param path Held directory.
#' @param token Owner token.
#' @return Invisible logical indicating release; failed renames raise an error.
finn_directory_lock_release <- function(path, token) {
  if (is.null(token)) return(invisible(FALSE))
  owner <- finn_read_json(file.path(path, "owner.json"))
  if (is.null(owner) || !identical(owner$token, token)) return(invisible(FALSE))
  released <- file.rename(path, paste0(path, ".released-", token))
  if (!released) stop("Could not release the Finn lock: ", path, call. = FALSE)
  invisible(TRUE)
}

#' Machine resources left after other active runs
#'
#' Subtracts claims from the machine-wide CPU and 80-percent-total-RAM
#' budgets. Available memory is kept unchanged for pressure warnings.
#'
#' @param resources List from [finn_system_resources()].
#' @param claimed List from [finn_claimed_resources()].
#' @return Resources list with remaining `cpu_budget` and `ram_budget_gb`.
finn_remaining_resources <- function(resources, claimed) {
  budget <- finn_resource_budget(resources)
  resources$cpu_budget <- max(0L, budget$cpu_budget - claimed$cores)
  resources$ram_budget_gb <- max(0, budget$ram_budget_gb - claimed$gb)
  resources
}

#' Plan a run's parallel setup around other runs on this computer
#'
#' One run per project is enforced elsewhere; this limits runs across
#' projects. With no other runs the plan is the normal one. Otherwise the plan
#' uses only the cores and memory left, and the launch is refused when not
#' even a sequential run fits.
#'
#' @param n_series Number of time series.
#' @param data_gb Approximate in-memory input size in GB.
#' @param global_models Whether global models will run.
#' @param claimed List from [finn_claimed_resources()].
#' @param resources List from [finn_system_resources()].
#' @param step_down Fallback level after memory failures.
#' @param sequential Whether the config forces sequential processing.
#' @param batch_size Number of concurrent submissions planned together.
#'   Each receives at most an equal share of the full machine budget.
#' @param num_cores Optional requested worker ceiling within the shared budget.
#' @return List with `fits` (logical), `plan` (with claims recorded), `shared`
#'   (whether other runs reduced the plan), and `remaining` resources.
finn_plan_with_others <- function(n_series, data_gb, global_models, claimed,
                                  resources = finn_system_resources(), step_down = 0L,
                                  sequential = FALSE, batch_size = 1L, num_cores = NULL) {
  if (!is.numeric(batch_size) || length(batch_size) != 1L || !is.finite(batch_size) ||
      batch_size < 1 || batch_size != floor(batch_size)) {
    stop("batch_size must be a positive whole number.", call. = FALSE)
  }
  budget <- finn_resource_budget(resources)
  remaining <- finn_remaining_resources(resources, claimed)
  remaining$cpu_budget <- min(remaining$cpu_budget, floor(budget$cpu_budget / batch_size))
  remaining$ram_budget_gb <- min(remaining$ram_budget_gb, budget$ram_budget_gb / batch_size)
  if (!is.null(num_cores)) {
    if (!is.numeric(num_cores) || length(num_cores) != 1L || !is.finite(num_cores) ||
        num_cores < 1 || num_cores != floor(num_cores)) {
      stop("num_cores must be a positive whole number.", call. = FALSE)
    }
    remaining$cpu_budget <- min(remaining$cpu_budget, num_cores)
  }
  fits <- remaining$cpu_budget >= 1L && remaining$ram_budget_gb + 1e-9 >= 0.3
  plan <- finn_choose_parallel(n_series, data_gb, global_models, remaining,
    step_down = if (sequential) 3L else step_down)
  if (sequential) plan$reason <- paste("Sequential (set by config).", plan$reason)
  if (batch_size > 1L) plan$reason <- paste(plan$reason, "Equal budget share for", batch_size, "planned concurrent runs.")
  if (!is.null(num_cores) && plan$safe_workers < num_cores) {
    plan$warnings <- c(plan$warnings, "Requested workers exceed the shared CPU/RAM or batch budget; the allocation was reduced to fit.")
  }
  if (any(vapply(claimed$runs, function(s) isTRUE(s$allocation_uncertain), logical(1)))) {
    plan$warnings <- c(plan$warnings, "Some existing runs have uncertain state or process liveness; their resource claims have been retained.")
  }
  if (length(claimed$runs) && !sequential) {
    plan$reason <- paste0(plan$reason, " Shares this computer with ", length(claimed$runs), " other active Finn run(s).")
  }
  list(fits = fits, plan = finn_plan_claim(plan, data_gb), shared = length(claimed$runs) > 0, remaining = remaining)
}

# ---------------------------------------------------------------------------
# Hashing, artifacts, and progress
# ---------------------------------------------------------------------------

#' Hash a value the way finnts names artifact files
#'
#' Mirrors the internal finnts `hash_data()`.
#'
#' @param x Value to hash.
#' @return xxhash64 hex digest.
finn_hash <- function(x) {
  if (is.character(x)) Encoding(x) <- "unknown"
  digest::digest(object = x, algo = "xxhash64")
}

#' Fingerprint of the settings that change forecast results
#'
#' Excludes fields that only affect speed or bookkeeping, so changing the
#' parallel setting does not block a resume. Agent LLM settings do participate:
#' changing them requires confirmation because resumes use the saved snapshot.
#' Run states store this value as `config_hash`, so its output must stay
#' stable for existing projects.
#'
#' @param cfg Config list.
#' @return Hash string.
finn_config_hash <- function(cfg) {
  keep <- cfg
  keep$parallel <- NULL
  keep$project_name <- NULL
  keep$data$file <- basename(keep$data$file %||% "")
  if (identical(keep$mode, "standard")) keep$agent$llm <- NULL
  finn_hash(finn_to_json(keep))
}

#' MD5 of the input data file
#'
#' @param file Data file path.
#' @return Hash string, or NA when the file cannot be read.
finn_data_md5 <- function(file) {
  tryCatch(unname(tools::md5sum(file)), error = function(e) NA_character_)
}

#' File names in one artifact folder for a project and run
#'
#' Lists the folder once and filters by the hashed project/run prefix.
#'
#' @param artifacts finnts `path` for the project.
#' @param folder Artifact sub-folder such as "forecasts" or "logs".
#' @param project_name finnts project name.
#' @param run_name finnts run name or Agent run id.
#' @return Character vector of matching file names.
finn_artifact_files <- function(artifacts, folder, project_name, run_name) {
  dir <- file.path(artifacts, folder)
  if (!dir.exists(dir) || is.null(run_name)) {
    return(character())
  }
  prefix <- paste0(finn_hash(project_name), "-", finn_hash(run_name))
  files <- list.files(dir)
  files[startsWith(files, prefix)]
}

#' Stage weights used for standard-run progress
#'
#' @return Named numeric vector summing to 100.
finn_stage_weights <- function() {
  c(prep_data = 15, prep_models = 5, train_models = 65, ensemble_models = 5, final_models = 10)
}

#' Estimate standard-run completion percent
#'
#' Completed stages count fully; training progress is the share of series
#' with saved forecasts.
#'
#' @param state Run state list (uses `stage`, `stages_done`, `n_series`,
#'   `project_name`, `run_name`, and `config`).
#' @param artifacts finnts `path` for the project.
#' @return List with `percent` and `detail`.
finn_standard_progress <- function(state, artifacts) {
  weights <- finn_stage_weights()
  done <- unlist(state$stages_done %||% list())
  if (identical(state$status, "completed")) {
    return(list(percent = 100, detail = "All stages complete."))
  }
  pct <- sum(weights[intersect(names(weights), done)])
  detail <- NULL
  stage <- state$stage %||% ""
  if (identical(stage, "train_models") && !("train_models" %in% done)) {
    files <- finn_artifact_files(artifacts, "forecasts", state$project_name, state$run_name)
    n <- max(as.integer(state$n_series %||% 1L), 1L)
    local_on <- finn_bool(state$config$run_local_models, TRUE)
    global_on <- finn_bool(state$config$run_global_models, finn_default_global_models(state$config$date_type))
    local_files <- files[grepl("-single_models\\.", files)]
    local_done <- length(unique(sub("^([^-]+-[^-]+-[^-]+).*$", "\\1", local_files)))
    global_done <- any(grepl("-global_models\\.", files))
    parts <- c(if (local_on) min(local_done / n, 1), if (global_on) as.numeric(global_done))
    frac <- if (length(parts)) mean(parts) else 0
    pct <- pct + weights[["train_models"]] * frac
    global_note <- if (!global_on) "" else if (global_done) "; global models done" else "; global models pending"
    detail <- sprintf("Training: %d of %d series have local model forecasts%s.", min(local_done, n), n, global_note)
  }
  list(percent = round(min(pct, 99), 1), detail = detail)
}

#' Estimate Agent-run completion percent from best-run logs
#'
#' @param state Run state list (uses `project_name`, `agent_run_id`,
#'   `status`, and `n_series`).
#' @param artifacts finnts `path` for the project.
#' @return List with `percent` and `detail`.
finn_agent_progress <- function(state, artifacts) {
  if (identical(state$status, "completed")) {
    return(list(percent = 100, detail = "Agent run complete."))
  }
  if (is.null(state$agent_run_id)) {
    return(list(percent = 2, detail = "Agent is starting."))
  }
  files <- finn_artifact_files(artifacts, "logs", state$project_name, state$agent_run_id)
  best <- files[grepl("-agent_best_run\\.csv$", files)]
  n <- max(as.integer(state$n_series %||% 1L), 1L)
  total <- n + 1L
  frac <- min(length(best) / total, 1)
  list(
    percent = round(5 + 90 * frac, 1),
    detail = sprintf("%d of about %d global and per-series searches have a best result saved.", length(best), total)
  )
}

# ---------------------------------------------------------------------------
# Input data
# ---------------------------------------------------------------------------

#' Load a CSV or Excel file
#'
#' @param file Path to a .csv, .txt, .xlsx, or .xls file.
#' @param sheet Optional Excel sheet name or index.
#' @param text_columns Optional column names to read as text in CSV and text
#'   files, so identifiers such as `001` keep their leading zeros and stay
#'   apart from `1`. Names the file does not contain are ignored. A blank cell
#'   in one of these columns is empty text rather than missing. Excel cells
#'   keep their own type, so this has no effect there.
#' @return A data frame.
finn_load_data <- function(file, sheet = NULL, text_columns = NULL) {
  if (is.null(file) || !file.exists(file)) stop("Data file not found: ", file, call. = FALSE)
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("xlsx", "xls")) {
    finn_require("readxl")
    if (!is.null(sheet) && grepl("^[0-9]+$", sheet)) sheet <- as.integer(sheet)
    return(as.data.frame(readxl::read_excel(file, sheet = sheet %||% 1L)))
  }
  if (ext %in% c("csv", "txt")) {
    sep <- finn_sniff_delimiter(file)
    # Reading Latin-1 bytes as UTF-8 silently truncates values, so check the
    # bytes first and pick the encoding up front.
    bytes <- readBin(file, "raw", file.info(file)$size)
    enc <- if (validUTF8(rawToChar(bytes[bytes != as.raw(0)]))) "UTF-8-BOM" else "latin1"
    classes <- NA
    if (length(text_columns)) {
      header <- tryCatch(
        names(utils::read.csv(file, sep = sep, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = enc, nrows = 1L)),
        error = function(e) character()
      )
      as_text <- intersect(as.character(unlist(text_columns)), header)
      if (length(as_text)) classes <- stats::setNames(rep("character", length(as_text)), as_text)
    }
    out <- utils::read.csv(file, sep = sep, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = enc, colClasses = classes)
    if (identical(enc, "latin1")) attr(out, "finn_encoding") <- "latin1"
    attr(out, "finn_delimiter") <- sep
    return(out)
  }
  stop("Unsupported file type '.", ext, "'. Use CSV or Excel.", call. = FALSE)
}

#' Guess the field separator of a delimited text file
#'
#' European exports often use semicolons because the comma is the decimal
#' mark. Counts candidate separators on the first non-empty lines, ignoring
#' text inside double quotes.
#'
#' @param file Path to a text file.
#' @return One of ",", ";", "\\t", or "|".
finn_sniff_delimiter <- function(file) {
  lines <- tryCatch(readLines(file, n = 5, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  # Latin-1 bytes are not valid UTF-8; replace them so text functions do not fail.
  lines <- iconv(lines, "UTF-8", "UTF-8", sub = "?")
  lines <- gsub('"[^"]*"', "", lines[nzchar(trimws(lines))])
  if (!length(lines)) {
    return(",")
  }
  cands <- c(",", ";", "\t", "|")
  counts <- vapply(cands, function(s) {
    n <- lengths(regmatches(lines, gregexpr(s, lines, fixed = TRUE)))
    if (all(n == n[1])) n[1] else min(n)
  }, numeric(1))
  if (max(counts) == 0) "," else cands[which.max(counts)]
}

#' Parse text dates with one format, rejecting implausible years
#'
#' `as.Date("1/5/24", "%m/%d/%Y")` returns year 24, so years outside
#' 1900-2200 are treated as unparsed.
#'
#' @param x Character vector.
#' @param f A strptime format.
#' @return Date vector.
finn_try_date_format <- function(x, f) {
  out <- if (f %in% c("%b %Y", "%B %Y", "%Y-%m", "%b-%y", "%b-%Y")) {
    as.Date(paste0("01 ", x), format = paste0("%d ", f))
  } else {
    as.Date(x, format = f)
  }
  yr <- as.integer(format(out, "%Y"))
  out[!is.na(yr) & (yr < 1900 | yr > 2200)] <- NA
  out
}

#' Parse a date column using common business formats
#'
#' Tries common formats and keeps the one that reads the most values. When
#' both month/day and day/month read every value (for example 01/02/2024),
#' the result is flagged as ambiguous. If exactly one reading puts every date
#' on day 1 (typical for monthly data), that reading is chosen.
#'
#' @param x Vector of dates as Date, POSIXct, numbers (Excel serials), or text.
#' @param format Optional strptime format that overrides detection, for
#'   example "%d/%m/%Y".
#' @return List with `value` (Date vector), `format` used, `ambiguous`
#'   (logical), and `alternative` (the other plausible format or NULL).
finn_parse_date <- function(x, format = NULL) {
  wrap <- function(value, f, ambiguous = FALSE, alternative = NULL) {
    list(value = value, format = f, ambiguous = ambiguous, alternative = alternative)
  }
  if (inherits(x, "Date")) {
    return(wrap(x, "Date"))
  }
  if (inherits(x, "POSIXt")) {
    return(wrap(as.Date(x), "datetime"))
  }
  if (is.numeric(x)) {
    return(wrap(as.Date(x, origin = "1899-12-30"), "excel_serial"))
  }
  x <- trimws(as.character(x))
  if (!is.null(format) && nzchar(format)) {
    return(wrap(finn_try_date_format(x, format), format))
  }
  present <- sum(!is.na(x) & nzchar(x))
  formats <- c(
    "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y", "%m-%d-%Y", "%d.%m.%Y",
    "%m/%d/%y", "%d/%m/%y", "%d.%m.%y", "%Y%m%d", "%b %Y", "%B %Y", "%Y-%m", "%b-%y", "%b-%Y"
  )
  results <- lapply(formats, function(f) finn_try_date_format(x, f))
  ok <- vapply(results, function(v) sum(!is.na(v)), numeric(1))
  best_i <- which.max(ok)
  best <- wrap(results[[best_i]], formats[best_i])
  pairs <- list(c("%m/%d/%Y", "%d/%m/%Y"), c("%m-%d-%Y", "%d-%m-%Y"), c("%m/%d/%y", "%d/%m/%y"))
  for (p in pairs) {
    if (!best$format %in% p) next
    i <- match(p, formats)
    if (!all(ok[i] == ok[best_i]) || ok[best_i] == 0) next
    a <- results[[i[1]]]
    b <- results[[i[2]]]
    if (isTRUE(all.equal(a, b))) next
    first_a <- all(format(a[!is.na(a)], "%d") == "01")
    first_b <- all(format(b[!is.na(b)], "%d") == "01")
    if (first_a != first_b) {
      pick <- if (first_a) 1 else 2
      return(wrap(results[[i[pick]]], p[pick]))
    }
    return(wrap(a, p[1], ambiguous = TRUE, alternative = p[2]))
  }
  best
}

#' Guess whether text numbers use a decimal comma
#'
#' Decides once for the whole column so "1.234" is read consistently.
#'
#' @param x Character vector of cleaned number text (no currency or spaces).
#' @return "," or ".".
finn_guess_decimal_mark <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) {
    return(".")
  }
  both <- grepl("\\.", x) & grepl(",", x)
  if (any(both)) {
    last_comma <- regexpr(",[^,]*$", x[both])
    last_dot <- regexpr("\\.[^.]*$", x[both])
    return(if (mean(last_comma > last_dot) > 0.5) "," else ".")
  }
  if (any(grepl("\\.", x))) {
    if (all(grepl("^-?[0-9]{1,3}(\\.[0-9]{3})+$", x[grepl("\\.", x)])) && !any(grepl(",", x)) &&
      any(grepl("\\.[0-9]{3}\\.", x))) {
      return(",")
    }
    return(".")
  }
  commas <- x[grepl(",", x)]
  if (length(commas) && any(!grepl("^-?[0-9]{1,3}(,[0-9]{3})+$", commas))) "," else "."
}

#' Convert a target column to numbers
#'
#' Strips currency symbols and codes, spaces (including non-breaking),
#' apostrophe thousands separators, and percent signs; treats parentheses and
#' trailing minus signs as negatives. Detects decimal commas such as
#' "1.234,56" unless `decimal_mark` is given.
#'
#' @param x Vector.
#' @param decimal_mark Optional "." or ","; NULL to detect.
#' @return Numeric vector with attribute `decimal_mark` when text was parsed.
finn_parse_number <- function(x, decimal_mark = NULL) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  x <- trimws(as.character(x))
  neg <- grepl("^\\(.*\\)$", x) | grepl("^[^-]*[0-9]-$", x)
  x <- gsub("(?i)(USD|EUR|GBP|JPY|INR|CHF|CAD|AUD|CNY|RMB)", "", x, perl = TRUE)
  x <- gsub("[$\u20ac\u00a3\u00a5\u20b9\u20a9%()'\u2019\\s\u00a0\u202f]", "", x, perl = TRUE)
  x <- sub("-$", "", x)
  mark <- decimal_mark %||% finn_guess_decimal_mark(x)
  if (identical(mark, ",")) {
    x <- gsub(".", "", x, fixed = TRUE)
    x <- sub(",", ".", x, fixed = TRUE)
  } else {
    x <- gsub(",", "", x, fixed = TRUE)
  }
  out <- suppressWarnings(as.numeric(x))
  out[neg & !is.na(out)] <- -abs(out[neg & !is.na(out)])
  attr(out, "decimal_mark") <- mark
  out
}

#' Shape raw data into the columns finnts expects
#'
#' Renames the date column to `Date`, parses dates and the target, and turns
#' series columns into text. Values are never imputed or otherwise altered.
#'
#' @param df Raw data frame.
#' @param cfg Normalized config.
#' @return List with `data` and `notes`.
finn_prepare_input <- function(df, cfg) {
  notes <- character()
  date_col <- cfg$date_column %||% "Date"
  if (!date_col %in% names(df)) stop("Date column '", date_col, "' is not in the data.", call. = FALSE)
  if (date_col != "Date") {
    if ("Date" %in% names(df)) stop("Data has both 'Date' and '", date_col, "'. Pick one date column.", call. = FALSE)
    names(df)[names(df) == date_col] <- "Date"
    notes <- c(notes, paste0("Used '", date_col, "' as the date column."))
  }
  parsed <- finn_parse_date(df$Date, cfg$date_format)
  raw_present <- !is.na(df$Date) & nzchar(trimws(as.character(df$Date)))
  bad_dates <- sum(is.na(parsed$value) & raw_present)
  if (bad_dates > 0) {
    stop(bad_dates, " date values could not be read", if (!is.null(cfg$date_format)) paste0(" with date_format '", cfg$date_format, "'"),
      ". Fix them in the source file or set date_format in the config (for example \"%d/%m/%Y\").",
      call. = FALSE
    )
  }
  if (isTRUE(parsed$ambiguous)) {
    notes <- c(notes, sprintf(
      "Dates could be month/day or day/month; read them as %s. If that is wrong, set date_format to \"%s\" in the config.",
      parsed$format, parsed$alternative
    ))
  }
  df$Date <- parsed$value
  target <- cfg$target_variable
  if (!target %in% names(df)) stop("Target column '", target, "' is not in the data.", call. = FALSE)
  raw <- df[[target]]
  num <- finn_parse_number(raw, cfg$decimal_mark)
  if (!is.numeric(raw) && identical(attr(num, "decimal_mark"), ",") && is.null(cfg$decimal_mark)) {
    notes <- c(notes, "Read target values with a decimal comma (for example 1.234,56). If that is wrong, set decimal_mark to \".\" in the config.")
  }
  mark <- cfg$decimal_mark %||% attr(num, "decimal_mark") %||% "."
  attr(num, "decimal_mark") <- NULL
  bad_num <- sum(is.na(num) & !is.na(raw) & nzchar(trimws(as.character(raw))))
  if (bad_num > 0) stop(bad_num, " values in '", target, "' are not numbers.", call. = FALSE)
  df[[target]] <- num
  for (r in intersect(unlist(cfg$external_regressors), names(df))) {
    if (is.character(df[[r]])) {
      rn <- finn_parse_number(df[[r]], mark)
      present <- !is.na(df[[r]]) & nzchar(trimws(df[[r]]))
      if (all(!is.na(rn[present]))) {
        attr(rn, "decimal_mark") <- NULL
        df[[r]] <- rn
      }
    }
  }
  for (v in intersect(cfg$combo_variables, names(df))) df[[v]] <- as.character(df[[v]])
  list(data = df, notes = notes)
}

#' Parse column headers that look like dates
#'
#' Handles text dates and Excel serial numbers stored as header text.
#'
#' @param x Character vector of column names.
#' @return Date vector, NA where a header is not a date.
finn_header_dates <- function(x) {
  x <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  serial <- grepl("^[0-9]{5}(\\.0+)?$", x)
  if (any(serial)) {
    n <- as.numeric(x[serial])
    ok <- n >= 20000 & n <= 80000
    out[serial][ok] <- as.Date(n[ok], origin = "1899-12-30")
  }
  rest <- !serial & nzchar(x) & !grepl("^\\.\\.\\.[0-9]+$", x)
  if (any(rest)) out[rest] <- finn_parse_date(x[rest])$value
  out
}

#' Detect spreadsheet layouts that finnts cannot read directly
#'
#' Looks for title rows above the real header, dates spread across columns
#' (wide layout), and total or subtotal rows mixed in with series rows.
#' Detection only; nothing is changed.
#'
#' @param df Raw data frame as read by [finn_load_data()].
#' @return List of issues; each has `type`, `message`, and `fix`, a named list
#'   of `prepare_data.R` arguments that would correct it.
finn_shape_issues <- function(df) {
  issues <- list()
  nm <- names(df)
  auto <- !nzchar(trimws(nm)) | grepl("^\\.\\.\\.[0-9]+$|^V[0-9]+$|^X[0-9]*$", nm)
  if (length(nm) > 1 && mean(auto) >= 0.5 && nrow(df) > 0) {
    scan <- seq_len(min(10, nrow(df)))
    filled <- vapply(scan, function(i) {
      v <- as.character(unlist(df[i, ], use.names = FALSE))
      mean(!is.na(v) & nzchar(trimws(v)))
    }, numeric(1))
    header_row <- scan[which(filled >= 0.8)[1]]
    if (!is.na(header_row)) {
      issues[[length(issues) + 1]] <- list(
        type = "title_rows",
        message = sprintf("The real column headers look like they are on data row %d; rows above it are titles or notes.", header_row),
        fix = list(header_row = header_row)
      )
    }
  }
  header_dates <- finn_header_dates(nm)
  n_date_cols <- sum(!is.na(header_dates))
  if (n_date_cols >= 3 && n_date_cols >= 0.5 * length(nm)) {
    ids <- nm[is.na(header_dates)]
    issues[[length(issues) + 1]] <- list(
      type = "wide_dates",
      message = sprintf("%d columns are dates, so each row holds many periods. finnts needs one row per series and date.", n_date_cols),
      fix = list(pivot_longer = TRUE, id_columns = paste(ids, collapse = ","))
    )
  }
  total_pattern <- "^(grand[ _-]?)?(sub[ _-]?)?totals?$|^all$|^total[ _-]"
  for (col in nm[!vapply(df, is.numeric, logical(1))]) {
    hits <- sum(grepl(total_pattern, trimws(as.character(df[[col]])), ignore.case = TRUE))
    if (hits > 0) {
      issues[[length(issues) + 1]] <- list(
        type = "total_rows",
        message = sprintf("Column '%s' has %d total-like rows (for example 'Total'). Summing them with the detail rows double counts.", col, hits),
        fix = list(drop_totals = TRUE)
      )
    }
  }
  issues
}

#' Reshape a raw table into one row per series and date
#'
#' Applies only the corrections asked for. Values are not changed beyond
#' moving them into the long layout.
#'
#' @param df Raw data frame.
#' @param header_row Optional data row (1-based) holding the real headers;
#'   that row becomes the names and rows up to it are dropped.
#' @param pivot_longer TRUE to turn date columns into `Date` and value rows.
#' @param id_columns Columns kept as identifiers when pivoting; defaults to
#'   every column whose header is not a date.
#' @param value_name Name for the value column created by pivoting.
#' @param drop_totals TRUE to drop rows where any text column reads like a
#'   total, subtotal, grand total, or all.
#' @return List with `data` and `notes` describing each change.
finn_reshape_data <- function(df, header_row = NULL, pivot_longer = FALSE, id_columns = NULL,
                              value_name = "Value", drop_totals = FALSE) {
  notes <- character()
  if (!is.null(header_row)) {
    header_row <- as.integer(header_row)
    if (is.na(header_row) || header_row < 1 || header_row >= nrow(df)) {
      stop("header_row must be between 1 and ", nrow(df) - 1, ".", call. = FALSE)
    }
    new_names <- trimws(as.character(unlist(df[header_row, ], use.names = FALSE)))
    new_names[is.na(new_names) | !nzchar(new_names)] <- paste0("column_", which(is.na(new_names) | !nzchar(new_names)))
    df <- df[-seq_len(header_row), , drop = FALSE]
    names(df) <- make.unique(new_names)
    df[] <- lapply(df, function(x) utils::type.convert(as.character(x), as.is = TRUE))
    rownames(df) <- NULL
    notes <- c(notes, sprintf("Used row %d as the header and removed %d rows above the data.", header_row, header_row))
  }
  if (isTRUE(drop_totals)) {
    total_pattern <- "^(grand[ _-]?)?(sub[ _-]?)?totals?$|^all$|^total[ _-]"
    text_cols <- names(df)[!vapply(df, is.numeric, logical(1))]
    is_total <- Reduce(`|`, lapply(text_cols, function(col) {
      grepl(total_pattern, trimws(as.character(df[[col]])), ignore.case = TRUE)
    }), rep(FALSE, nrow(df)))
    if (any(is_total)) {
      df <- df[!is_total, , drop = FALSE]
      notes <- c(notes, sprintf("Removed %d total rows.", sum(is_total)))
    }
  }
  if (isTRUE(pivot_longer)) {
    header_dates <- finn_header_dates(names(df))
    ids <- if (length(id_columns)) trimws(unlist(strsplit(paste(id_columns, collapse = ","), ","))) else names(df)[is.na(header_dates)]
    missing <- setdiff(ids, names(df))
    if (length(missing)) stop("id_columns not found: ", paste(missing, collapse = ", "), call. = FALSE)
    date_cols <- setdiff(names(df)[!is.na(header_dates)], ids)
    if (length(date_cols) == 0) stop("No date columns found to pivot.", call. = FALSE)
    if (value_name %in% ids || identical(value_name, "Date")) stop("value_name clashes with an existing column.", call. = FALSE)
    long <- do.call(rbind, lapply(date_cols, function(col) {
      piece <- df[ids]
      piece$Date <- rep(header_dates[match(col, names(df))], nrow(df))
      piece[[value_name]] <- df[[col]]
      piece
    }))
    long <- long[order(do.call(paste, c(long[ids], sep = "\r")), long$Date), , drop = FALSE]
    rownames(long) <- NULL
    df <- long
    notes <- c(notes, sprintf("Turned %d date columns into rows with Date and %s columns.", length(date_cols), value_name))
  }
  list(data = df, notes = notes)
}

#' Weighted MAPE
#'
#' @param forecast Forecast values.
#' @param actual Actual values.
#' @return sum(|F - A|) / sum(|A|), or NA when actuals sum to zero.
finn_wmape <- function(forecast, actual) {
  keep <- !is.na(forecast) & !is.na(actual)
  denom <- sum(abs(actual[keep]))
  if (denom == 0) {
    return(NA_real_)
  }
  sum(abs(forecast[keep] - actual[keep])) / denom
}

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

#' Default run configuration
#'
#' @return Config list matching templates/config.template.json.
finn_default_config <- function() {
  list(
    project_name = NULL,
    mode = "standard",
    data = list(file = NULL, sheet = NULL),
    date_column = "Date",
    date_format = NULL,
    decimal_mark = NULL,
    combo_variables = NULL,
    target_variable = NULL,
    date_type = NULL,
    forecast_horizon = NULL,
    external_regressors = NULL,
    hist_start_date = NULL,
    hist_end_date = NULL,
    fiscal_year_start = 1L,
    back_test_scenarios = NULL,
    back_test_spacing = NULL,
    models_to_run = NULL,
    models_not_to_run = NULL,
    run_global_models = NULL,
    run_local_models = TRUE,
    run_ensemble_models = NULL,
    clean_missing_values = TRUE,
    clean_outliers = FALSE,
    negative_forecast = FALSE,
    forecast_approach = "bottoms_up",
    average_models = TRUE,
    max_model_average = 3L,
    feature_selection = FALSE,
    recipes_to_run = NULL,
    pca = NULL,
    seed = 123L,
    parallel = "auto",
    agent = list(
      llm = list(provider = "copilot", model = "auto"),
      max_iter = 3L,
      weighted_mape_goal = 0.03,
      allow_hierarchical_forecast = FALSE
    )
  )
}

#' Typical and maximum sensible horizons per date type
#'
#' @return Named list of c(typical, max).
finn_horizon_guide <- function() {
  list(year = c(3, 5), quarter = c(8, 12), month = c(12, 24), week = c(13, 104), day = c(90, 365))
}

#' Periods in one year for a date type
#'
#' @param date_type finnts date type.
#' @return Number of periods per year, or NA for unknown types.
finn_periods_per_year <- function(date_type) {
  switch(date_type %||% "",
    day = 365,
    week = 52,
    month = 12,
    quarter = 4,
    year = 1,
    NA_real_
  )
}

#' Size tier for a number of time series
#'
#' @param n_series Number of time series.
#' @return "small" (up to 100), "large" (up to 1,000), or "very_large".
finn_size_tier <- function(n_series) {
  n <- as.numeric(n_series %||% 0)
  if (n <= 100) "small" else if (n <= 1000) "large" else "very_large"
}

#' Back-test spacing that still covers the last year with fewer origins
#'
#' finnts adds one origin to `back_test_scenarios`, so `scenarios` origins
#' spaced `spacing` periods apart span about one year of history. Spacing never
#' drops below the finnts default for the date type.
#'
#' @param date_type finnts date type.
#' @param tier Size tier from [finn_size_tier()].
#' @param n_periods Distinct dates with actuals.
#' @param horizon Forecast horizon in periods.
#' @return NULL when the defaults already suit the data, else a list with
#'   `ok`; when `ok` is TRUE also `scenarios` and `spacing`, otherwise `note`.
finn_recommend_back_test <- function(date_type, tier, n_periods, horizon) {
  ppy <- finn_periods_per_year(date_type)
  if (identical(tier, "small") || is.na(ppy) || ppy < 4) {
    return(NULL)
  }
  default_spacing <- switch(date_type,
    day = 7,
    week = 4,
    1
  )
  target <- if (identical(tier, "very_large")) 4 else 6
  spacing <- max(default_spacing, ceiling(ppy / target))
  scenarios <- floor(ppy / spacing)
  n_periods <- as.numeric(n_periods %||% NA)
  h <- as.numeric(horizon)
  if (is.na(n_periods) || n_periods - h - ppy < max(ppy, 2 * h)) {
    return(list(ok = FALSE, note = sprintf(
      "History is too short to back test over a full year and still train well (%s periods of actuals), so Finn's default back testing is kept.",
      if (is.na(n_periods)) "unknown" else n_periods
    )))
  }
  list(ok = TRUE, scenarios = scenarios, spacing = spacing)
}

#' Recommend run settings for the size of the data
#'
#' Advisory only: nothing is changed or saved, and no run is ever refused.
#' Back testing always aims to cover the last year of history; for many series
#' it spaces back-test origins further apart instead of shortening the window.
#' When the latest period with actuals has not ended, suggests ending history
#' at the last complete period.
#'
#' @param cfg Merged config.
#' @param facts Facts from [finn_check_config()], with `n_series`,
#'   `n_periods`, and optionally `incomplete_period`.
#' @param root Optional storage root, used to suggest a local folder.
#' @param workers Optional planned parallel workers for the runtime estimate.
#' @return List with `tier`, `recommendations` (each `key`, `value`,
#'   `reason`), `notes`, `suggested_mode`, `suggested_mode_reason`,
#'   `test_subset_first`, and `estimated_runtime`.
finn_recommend_settings <- function(cfg, facts, root = NULL, workers = NULL) {
  n <- as.numeric(facts$n_series %||% 0)
  tier <- finn_size_tier(n)
  recs <- list()
  notes <- character()
  add <- function(key, value, reason) recs[[length(recs) + 1]] <<- list(key = key, value = value, reason = reason)
  agent <- identical(cfg$mode, "agent")

  partial <- finn_partial_in_history(cfg, facts)
  if (!is.null(partial$suggested_hist_end_date)) {
    add("hist_end_date", partial$suggested_hist_end_date, sprintf(
      "The `%s` period has not ended yet, so its actuals are probably partial. Ending history at the last complete period keeps them out of training and back tests.",
      partial$date
    ))
  }

  if (is.null(cfg$back_test_scenarios) && is.null(cfg$back_test_spacing)) {
    bt <- finn_recommend_back_test(cfg$date_type, tier, facts$n_periods, cfg$forecast_horizon)
    if (isTRUE(bt$ok)) {
      reason <- sprintf("Keeps back testing over the last year while testing %d dates instead of every %s, which cuts run time for %s series.",
        bt$scenarios + 1, cfg$date_type, format(n, big.mark = ","))
      add("back_test_scenarios", bt$scenarios, reason)
      add("back_test_spacing", bt$spacing, reason)
    } else if (!is.null(bt$note)) {
      notes <- c(notes, bt$note)
    }
  } else if (!is.null(facts$n_periods)) {
    spacing <- as.numeric(cfg$back_test_spacing %||% switch(cfg$date_type, day = 7, week = 4, 1))
    scenarios <- as.numeric(cfg$back_test_scenarios %||% 0)
    left <- facts$n_periods - scenarios * spacing - as.numeric(cfg$forecast_horizon)
    if (scenarios > 0 && left < 2 * as.numeric(cfg$forecast_horizon)) {
      notes <- c(notes, sprintf(
        "The chosen back testing reaches far back and leaves about %d periods to train on in the earliest test. Consider fewer back-test scenarios or a smaller spacing.",
        as.integer(max(left, 0))
      ))
    }
  }

  week_or_day <- isTRUE(cfg$date_type %in% c("week", "day"))
  if (tier == "very_large") {
    if (isTRUE(cfg$run_local_models)) {
      add("run_local_models", FALSE, "Training separate models for every series is the slowest part of large runs; one shared (global) model learns across all series.")
    }
    if (!isTRUE(cfg$run_global_models)) {
      add("run_global_models", TRUE, if (week_or_day) {
        "Global models train one model across all series. On weekly or daily data they need more memory, so watch the first run."
      } else {
        "Global models train one model across all series, which scales far better than one model per series."
      })
    }
    if (length(cfg$models_to_run) == 0) {
      add("models_to_run", list("xgboost"), "A single fast global model keeps the first large run manageable; add more models after seeing results.")
    }
    if (isTRUE(cfg$feature_selection)) {
      add("feature_selection", FALSE, "Feature selection repeats work per series and adds hours at this size.")
    }
    if (!is.null(root) && finn_is_onedrive(root)) {
      notes <- c(notes, "Thousands of series write many files. Consider moving the Finn folder to a local folder such as Documents (setup_project.R --action=set_root).")
    }
  } else if (tier == "large" && week_or_day && is.null(cfg$run_global_models)) {
    add("run_global_models", TRUE, "With hundreds of weekly or daily series, a global model learns across series and is usually faster than per-series models alone; it uses more memory.")
  }

  suggested_mode <- cfg$mode
  mode_reason <- NULL
  if (agent && tier == "very_large") {
    suggested_mode <- "standard"
    mode_reason <- "The Agent searches per series over several iterations, which can take days at this size. A standard run is recommended for data this size."
  } else if (agent && tier == "large") {
    mode_reason <- "The Agent takes several times longer than a standard run at this size and uses more Copilot requests. Choose standard if speed matters more than the extra accuracy search."
  } else if (!agent && tier == "small") {
    mode_reason <- "At this size the Agent is practical: with Copilot ready, it searches for better models on every series. Standard is faster."
  }

  run_mode <- if (agent) "iterate" else "standard"
  list(
    tier = tier,
    recommendations = recs,
    notes = notes,
    suggested_mode = suggested_mode,
    suggested_mode_reason = mode_reason,
    test_subset_first = tier != "small",
    estimated_runtime = finn_estimate_runtime(n, run_mode, workers)$text
  )
}

#' User-facing name of a run type
#'
#' @param run_type "standard", "iterate", or "update".
#' @return Label such as "Agent update".
finn_run_type_label <- function(run_type) {
  switch(run_type, iterate = "Agent iterate", update = "Agent update", "Standard")
}

#' Plain-language explanation of what a run type does
#'
#' @param run_type "standard", "iterate", or "update".
#' @param cfg Merged config, used for the Agent iteration limit.
#' @return One or two sentences.
finn_run_type_hint <- function(run_type, cfg = list()) {
  switch(run_type,
    iterate = sprintf(paste(
      "An AI model looks at your data, tries different settings over up to %s iterations,",
      "and keeps what improves accuracy per series. Slower, and it uses Copilot requests."
    ), cfg$agent$max_iter %||% 3),
    update = paste(
      "Reuses what the Agent learned in your last Agent forecast and refits on the new actuals;",
      "much faster than a new Agent search."
    ),
    "Finn trains the selected models once, scores them on back tests, and picks the best per series. Fast and predictable."
  )
}

#' Forecast settings that differ between two configs
#'
#' Compares the settings that change what an Agent update or a continued
#' Agent search produces, including the enrolled custom models.
#' `hist_end_date` is left out because a refresh is
#' expected to move it, and speed-only settings are ignored.
#'
#' @param prior Config of the earlier run, or NULL.
#' @param cfg Current config.
#' @return Character vector such as `"forecast_horizon 12 -> 6"`; empty when
#'   nothing changed or `prior` is NULL.
finn_settings_drift <- function(prior, cfg) {
  if (is.null(prior)) {
    return(character())
  }
  prior <- finn_merge_config(prior)
  cfg <- finn_merge_config(cfg)
  fmt <- function(v) {
    if (is.null(v) || !length(v)) "null" else as.character(jsonlite::toJSON(unlist(v), auto_unbox = TRUE, digits = NA))
  }
  top <- c("target_variable", "combo_variables", "date_type", "forecast_horizon", "external_regressors",
    "hist_start_date", "combo_cleanup_date", "back_test_scenarios", "back_test_spacing", "negative_forecast",
    "run_global_models", "run_local_models", "fiscal_year_start")
  agent <- c("allow_hierarchical_forecast", "max_iter", "weighted_mape_goal", "allow_iterate_forecast")
  pairs <- c(
    lapply(top, function(k) list(k, prior[[k]], cfg[[k]])),
    lapply(agent, function(k) list(k, prior$agent[[k]], cfg$agent[[k]])),
    list(list("llm", prior$agent$llm[c("provider", "model")], cfg$agent$llm[c("provider", "model")])),
    list(list("custom_models", finn_custom_labels(prior), finn_custom_labels(cfg)))
  )
  out <- character()
  for (p in pairs) {
    old <- fmt(p[[2]])
    new <- fmt(p[[3]])
    if (!identical(old, new)) out <- c(out, sprintf("%s %s -> %s", p[[1]], old, new))
  }
  out
}

#' Classify an iterate launch against an earlier iterate run's settings
#'
#' `iterate_forecast()` treats `max_iter` as the total number of iterations
#' per search (the global search and each series' local search) for one Agent
#' version, and uses `weighted_mape_goal` to pick which series reopen and when
#' a search stops. On the same Agent version, then, a higher `max_iter` runs up
#' to the new total; a stricter goal with the same `max_iter` reopens only
#' series whose best model is global and that miss the new goal; and a lower
#' `max_iter` or a looser goal alone runs nothing new. Every other setting
#' fingerprinted by [finn_config_hash()] must match to stay on that version.
#'
#' @param prior Config saved with the earlier iterate run.
#' @param cfg Current config.
#' @return List with `kind` ("continue", "unchanged", "looser_goal_only",
#'   "lower_max_iter", or "other_changes"), `partial` (TRUE when only a
#'   stricter goal continues with the same `max_iter`), `goal_changed`,
#'   `max_iter` and `weighted_mape_goal` (each with `previous` and `new`), and
#'   `changed` (differing settings other than those two, from
#'   [finn_settings_drift()]; may be empty when the difference is in a setting
#'   that function does not list).
finn_iterate_continuation <- function(prior, cfg) {
  prior <- finn_merge_config(prior)
  cfg <- finn_merge_config(cfg)
  num <- function(v) if (is.null(v) || !length(v)) NA_real_ else suppressWarnings(as.numeric(v[[1]]))
  old_max <- num(prior$agent$max_iter)
  new_max <- num(cfg$agent$max_iter)
  old_goal <- num(prior$agent$weighted_mape_goal)
  new_goal <- num(cfg$agent$weighted_mape_goal)
  aligned <- prior
  aligned$agent$max_iter <- cfg$agent$max_iter
  aligned$agent$weighted_mape_goal <- cfg$agent$weighted_mape_goal
  # Goals are decimals saved through JSON, so compare with a small tolerance.
  tol <- 1e-9
  stricter <- isTRUE(new_goal < old_goal - tol)
  looser <- isTRUE(new_goal > old_goal + tol)
  kind <- if (!identical(finn_config_hash(aligned), finn_config_hash(cfg)) || is.na(old_max) || is.na(new_max)) {
    "other_changes"
  } else if (new_max > old_max) {
    "continue"
  } else if (new_max < old_max) {
    "lower_max_iter"
  } else if (stricter) {
    "continue"
  } else if (looser) {
    "looser_goal_only"
  } else {
    "unchanged"
  }
  drift <- finn_settings_drift(prior, cfg)
  list(kind = kind, partial = identical(kind, "continue") && identical(new_max, old_max),
    goal_changed = stricter || looser,
    max_iter = list(previous = old_max, new = new_max),
    weighted_mape_goal = list(previous = old_goal, new = new_goal),
    changed = drift[!grepl("^(max_iter|weighted_mape_goal) ", drift)])
}

#' Plain-language explanation of an iterate continuation for the review
#'
#' @param cont Result of [finn_iterate_continuation()].
#' @param version Agent version label of the earlier iterate run.
#' @return One string.
finn_continuation_why <- function(cont, version) {
  old_max <- format(cont$max_iter$previous)
  new_max <- format(cont$max_iter$new)
  goal <- sprintf("`%s` -> `%s`", format(cont$weighted_mape_goal$previous), format(cont$weighted_mape_goal$new))
  switch(cont$kind,
    continue = if (cont$partial) {
      sprintf(paste(
        "Launching iterate continues version %s with a stricter weighted MAPE goal (%s). With `max_iter` still `%s`,",
        "only series whose best model is global and that miss the new goal reopen; set `max_iter` above `%s` to search longer."
      ), version, goal, old_max, old_max)
    } else {
      paste0(sprintf(paste(
        "Launching iterate continues version %s: `max_iter` `%s` -> `%s` is a total per search,",
        "so each search runs only the iterations it has left up to `%s`."
      ), version, old_max, new_max, new_max), if (cont$goal_changed) sprintf(" The weighted MAPE goal changes %s.", goal) else "")
    },
    looser_goal_only = sprintf(paste(
      "A looser weighted MAPE goal (%s) alone runs nothing new on version %s.",
      "To search longer, set `max_iter` above `%s`: the same Agent version continues and only the extra iterations run."
    ), goal, version, old_max),
    lower_max_iter = paste0(sprintf(paste(
      "Version %s already ran up to `%s` iterations per search, and a lower `max_iter` (`%s`) can't undo iterations,",
      "so nothing new would run. Set `max_iter` above `%s` to continue that Agent version."
    ), version, old_max, new_max, old_max),
    if (cont$goal_changed) sprintf(" Keep `max_iter` at `%s` or higher for the goal change to apply.", old_max) else ""),
    other_changes = sprintf(paste(
      "Other settings differ from it (%s), so launching iterate needs a new Agent version, which needs your confirmation;",
      "earlier versions are kept. Only `max_iter` and `weighted_mape_goal` can change on the same version."
    ), if (length(cont$changed)) paste(sprintf("`%s`", cont$changed), collapse = ", ") else "forecast settings"),
    sprintf(paste(
      "To search longer, set `max_iter` above `%s`: the same Agent version continues and only the extra iterations run.",
      "A stricter `weighted_mape_goal` also continues it, but reopens only series whose best model is global and that miss the new goal."
    ), old_max)
  )
}

#' Budget or goal revision for a stopped iterate run
#'
#' A stopped iterate run can take a revised `max_iter` or `weighted_mape_goal`
#' on the same Agent version: the launcher starts a new skill run that reuses
#' the stopped run's request id, so finished iterations are kept. Allowed only
#' when the input file and every other setting are unchanged and `max_iter`
#' is not lower; anything else keeps the usual resume-or-start-fresh
#' confirmation.
#'
#' @param latest State of the latest run of the mode, with `effective_status`.
#' @param cfg Current config.
#' @return Result of [finn_iterate_continuation()] when the revision applies,
#'   otherwise NULL.
finn_stopped_iterate_revision <- function(latest, cfg) {
  saved <- latest[["config", exact = TRUE]]
  if (!identical(latest$mode, "iterate") || !isTRUE(latest$effective_status %in% c("interrupted", "failed", "cancelled")) ||
    !is.list(saved) || is.null(latest$request_id) || !identical(latest$data_md5, finn_data_md5(cfg$data$file))) {
    return(NULL)
  }
  cont <- finn_iterate_continuation(saved, cfg)
  if (cont$kind %in% c("continue", "looser_goal_only")) cont
}

#' Recommend the next run type from a project's run history
#'
#' Read-only. Resumes an unfinished latest run; otherwise refreshes on the
#' project's track (Agent projects with Agent update, standard projects with
#' a standard run) when the file has actuals past the last forecast. With no
#' new actuals, an Agent project can continue its search on the same Agent
#' version by raising `max_iter` or tightening `weighted_mape_goal`
#' ([finn_iterate_continuation()]). A stopped iterate run whose only changes
#' are those two settings continues with them ([finn_stopped_iterate_revision()]).
#' When the enrolled custom models differ from the last Agent forecast's, a
#' refresh needs a new Agent version, because Agent update must reuse the
#' same custom models.
#' A project with no runs follows the config
#' mode, so the size-based advice in [finn_recommend_settings()] still applies.
#' When `hist_end_date` already leaves out a latest period that has not ended,
#' the actuals count only through the last complete period.
#'
#' @param paths Project paths.
#' @param cfg Merged, validated config.
#' @param facts Data facts from [finn_check_config()].
#' @return List with `run_type`, `situation` ("new_project", "resume",
#'   "agent_refresh", "agent_current", "standard_refresh", or
#'   "standard_current"), `why` (or NULL), `launch` (run_forecast.R arguments),
#'   `alternative` (list with `run_type`, `label`, `reason`, `launch`, or
#'   NULL), `data_end`, `last_end`, `prior_max_iter`,
#'   `prior_weighted_mape_goal`, `last_agent_version`, `continuation` (from
#'   [finn_iterate_continuation()] when iterate would continue or reopen an
#'   Agent version, else NULL), `settings_changes` (from
#'   [finn_settings_drift()] against the last Agent forecast),
#'   `custom_models_changed` (list with `previous` and `new` custom model
#'   labels when they differ from the last Agent forecast, else NULL),
#'   `new_version` (TRUE when the recommended launch starts a new Agent
#'   version; absent otherwise), and
#'   `recommendations` (each `key`, `value`, `reason`).
finn_next_run <- function(paths, cfg, facts = list()) {
  runs <- Filter(function(s) !identical(s$effective_status, "unreadable"), finn_list_runs(paths))
  last_agent <- (finn_read_project(paths) %||% list())$last_agent
  agent_done <- identical(last_agent$status, "completed")
  data_end <- facts$last_actual_date %||% facts$date_max
  data_end <- if (is.null(data_end)) NULL else as.character(data_end)
  partial <- facts$incomplete_period
  left_out <- NULL
  if (!is.null(partial$suggested_hist_end_date) && identical(partial$date, data_end) && is.null(finn_partial_in_history(cfg, facts))) {
    left_out <- data_end
    data_end <- partial$suggested_hist_end_date
  }
  completed <- Filter(function(s) identical(s$effective_status, "completed"), runs)
  last_done <- if (length(completed)) completed[[1]]
  out <- list(run_type = if (identical(cfg$mode, "agent")) "iterate" else "standard", situation = "new_project",
    why = NULL, alternative = NULL, data_end = data_end, last_end = NULL, prior_max_iter = NULL,
    prior_weighted_mape_goal = NULL, continuation = NULL, custom_models_changed = NULL,
    last_agent_version = if (agent_done) last_agent$agent_version, settings_changes = character(),
    recommendations = list())
  later <- function(a, b) !is.null(a) && !is.null(b) && a > as.character(b)
  hist_note <- function() {
    if (!later(data_end, cfg$hist_end_date)) {
      return(NULL)
    }
    out$recommendations <<- list(list(key = "hist_end_date", value = data_end, reason = sprintf(
      "`hist_end_date` is set to `%s`, which leaves out the new actuals through `%s`. Use `%s` so the forecast starts after them.",
      cfg$hist_end_date, data_end, data_end
    )))
    sprintf(" Set `hist_end_date` to `%s` to include them.", data_end)
  }
  agent_switch <- list(run_type = "iterate", label = "Agent iterate",
    reason = "Switches the whole project to the Agent, which searches settings per series for better accuracy. Slower, uses Copilot requests, and later refreshes use Agent update.",
    launch = "--mode=iterate")
  new_version <- list(run_type = "iterate", label = "Agent iterate (new Agent version)",
    reason = "Searches again from scratch, for example after the series or drivers changed a lot. Slower, uses more Copilot requests, and needs your confirmation; earlier versions are kept.",
    launch = "--mode=iterate --new=true --confirm=true")

  latest <- if (length(runs)) runs[[1]]
  if (!is.null(latest) && latest$effective_status %in% c("interrupted", "failed", "cancelled") &&
    isTRUE(latest$mode %in% c("standard", "iterate", "update"))) {
    out$run_type <- latest$mode
    out$situation <- "resume"
    out$why <- sprintf("Run `%s` (%s) stopped before finishing (%s). Running it again resumes where it stopped with its saved settings.",
      latest$run_id, finn_run_type_label(latest$mode), latest$effective_status)
    revised <- finn_stopped_iterate_revision(latest, cfg)
    if (!is.null(revised)) {
      out$continuation <- revised
      out$why <- sprintf(paste(
        "Run `%s` (%s) stopped before finishing (%s). Running it again continues the same Agent version and its finished",
        "iterations with your revised settings: `max_iter` `%s` -> `%s`, weighted MAPE goal `%s` -> `%s`."
      ), latest$run_id, finn_run_type_label(latest$mode), latest$effective_status,
      format(revised$max_iter$previous), format(revised$max_iter$new),
      format(revised$weighted_mape_goal$previous), format(revised$weighted_mape_goal$new))
    }
  } else if (agent_done && (is.null(last_done) || !identical(last_done$mode, "standard"))) {
    out$last_end <- last_agent$hist_end_date
    version <- last_agent$agent_version %||% "?"
    prior <- if (!is.null(last_agent$run_id)) finn_read_state(finn_run_paths(paths, last_agent$run_id))
    out$settings_changes <- finn_settings_drift(prior$config, cfg)
    custom_old <- finn_custom_labels(prior$config)
    custom_new <- finn_custom_labels(cfg)
    custom_changed <- is.list(prior$config) && !identical(custom_old, custom_new)
    if (custom_changed) out$custom_models_changed <- list(previous = custom_old, new = custom_new)
    if (later(data_end, last_agent$hist_end_date) && custom_changed) {
      out$run_type <- "iterate"
      out$situation <- "agent_refresh"
      out$why <- paste0(sprintf(paste(
        "Your last Agent forecast (version %s) used actuals through `%s`; the file now has actuals through `%s`.",
        "Agent update must reuse the custom models that version used (%s), but this config enrolls %s,",
        "so refreshing with them needs a new Agent version, which needs your confirmation; earlier versions are kept."
      ), version, last_agent$hist_end_date, data_end, finn_custom_label_text(custom_old), finn_custom_label_text(custom_new)),
      hist_note() %||% "")
      out$alternative <- list(run_type = "update", label = "Agent update (previous custom models)",
        reason = "Refreshes the current Agent version instead. Restore its custom model enrollment first with custom_model.R --action=enroll or --action=unenroll.",
        launch = "--mode=update")
      out$new_version <- TRUE
    } else if (later(data_end, last_agent$hist_end_date)) {
      out$run_type <- "update"
      out$situation <- "agent_refresh"
      out$why <- paste0(sprintf("Your last Agent forecast (version %s) used actuals through `%s`; the file now has actuals through `%s`.",
        version, last_agent$hist_end_date, data_end), hist_note() %||% "")
      out$alternative <- new_version
    } else {
      out$prior_max_iter <- prior$config$agent$max_iter
      out$prior_weighted_mape_goal <- prior$config$agent$weighted_mape_goal
      out$run_type <- "iterate"
      out$situation <- "agent_current"
      current <- sprintf("Your last Agent forecast (version %s) already uses actuals through `%s`.",
        version, last_agent$hist_end_date %||% data_end)
      out$why <- if (identical(last_agent$mode, "iterate") && is.list(prior$config)) {
        out$continuation <- finn_iterate_continuation(prior$config, cfg)
        paste(current, finn_continuation_why(out$continuation, version))
      } else {
        paste(current, "Add newer actuals to refresh it with Agent update.")
      }
      out$alternative <- new_version
    }
  } else if (!is.null(last_done)) {
    out$last_end <- last_done$data_end
    out$run_type <- "standard"
    if (later(data_end, last_done$data_end)) {
      out$situation <- "standard_refresh"
      out$why <- paste0(sprintf("Your last standard forecast used actuals through `%s`; the file now has actuals through `%s`. Standard projects refresh with another standard run.",
        last_done$data_end, data_end), hist_note() %||% "")
    } else {
      out$situation <- "standard_current"
      out$why <- sprintf("Your last standard forecast already uses actuals through `%s`. A new standard run is only needed if you change settings.",
        last_done$data_end %||% data_end)
    }
    out$alternative <- agent_switch
  }
  if (!is.null(left_out) && !identical(out$situation, "resume") && !is.null(out$why)) {
    out$why <- paste(out$why, sprintf("The `%s` period has not ended yet, so `hist_end_date` leaves it out.", left_out))
  }
  out$launch <- if (identical(out$situation, "resume")) paste0("--mode=", out$run_type) else if (isTRUE(out$new_version)) {
    "--mode=iterate --new=true --confirm=true"
  } else switch(out$run_type,
    iterate = "--mode=iterate", update = "--mode=update", "--mode=standard")
  out
}

#' Merge a user config over defaults
#'
#' @param cfg User config list.
#' @return Config with every default key present, `custom_models` normalized
#'   by [finn_custom_normalize()] (and removed when empty), and integer values
#'   stored as doubles, because finnts type checks require `numeric` class.
finn_merge_config <- function(cfg) {
  defaults <- finn_default_config()
  cfg <- cfg %||% list()
  out <- utils::modifyList(defaults, cfg, keep.null = TRUE)
  out$agent <- utils::modifyList(defaults$agent, cfg$agent %||% list(), keep.null = TRUE)
  out$agent$llm <- utils::modifyList(defaults$agent$llm, cfg$agent$llm %||% list(), keep.null = TRUE)
  # Assigning NULL drops an empty entry, so configs without custom models keep their fingerprint.
  out$custom_models <- finn_custom_normalize(out$custom_models)
  finn_ints_to_double(out)
}

#' Convert integer values in a nested list to doubles
#'
#' JSON parsing returns whole numbers as integers, while finnts argument
#' checks require class `numeric`.
#'
#' @param x A list or atomic value.
#' @return `x` with every integer vector converted to double; NULL elements kept.
finn_ints_to_double <- function(x) {
  if (is.list(x)) {
    for (nm in seq_along(x)) {
      if (!is.null(x[[nm]])) x[[nm]] <- finn_ints_to_double(x[[nm]])
    }
    return(x)
  }
  if (is.integer(x) && !is.factor(x)) storage.mode(x) <- "double"
  x
}

#' Validate a config against optional data
#'
#' @param cfg Config list (merged or raw).
#' @param data Optional prepared data frame with a `Date` column.
#' @param root Optional storage root, used for OneDrive warnings and to read
#'   enrolled custom models.
#' @param today Date used to spot a latest period that has not ended; tests
#'   pass a fixed date.
#' @return List with `config`, `errors`, `warnings`, and `facts`. `facts`
#'   includes `incomplete_period` from [finn_incomplete_period()] when the
#'   latest period with actuals has not ended, and `custom_models` (from
#'   [finn_check_custom_models()]) when data is given and custom models are
#'   enrolled.
finn_check_config <- function(cfg, data = NULL, root = NULL, today = Sys.Date()) {
  fiscal_unset <- is.null(cfg$fiscal_year_start)
  cfg <- finn_merge_config(cfg)
  errors <- character()
  warnings <- character()
  facts <- list()
  need <- function(cond, msg) if (!isTRUE(cond)) errors <<- c(errors, msg)

  need(!is.null(cfg$project_name) && nzchar(cfg$project_name), "project_name is required.")
  need(isTRUE(cfg$mode %in% c("standard", "agent")), "mode must be 'standard' or 'agent'.")
  need(length(cfg$combo_variables) > 0, "combo_variables needs at least one column that identifies each series.")
  need(!("Date" %in% cfg$combo_variables), "combo_variables cannot include the date column.")
  need(!is.null(cfg$target_variable), "target_variable is required.")
  need(isTRUE(cfg$date_type %in% c("year", "quarter", "month", "week", "day")), "date_type must be year, quarter, month, week, or day.")
  h <- suppressWarnings(as.integer(cfg$forecast_horizon))
  need(length(h) == 1 && !is.na(h) && h > 0, "forecast_horizon must be a positive whole number.")
  if (identical(cfg$parallel, "spark")) {
    errors <- c(errors, "Spark is not supported by the local Finn skill.")
  } else {
    need(isTRUE((cfg$parallel %||% "auto") %in% c("auto", "none")), "parallel must be 'auto' or 'none'.")
  }
  if (!is.null(cfg$decimal_mark)) need(isTRUE(cfg$decimal_mark %in% c(".", ",")), "decimal_mark must be \".\" or \",\".")
  if (!is.null(cfg$date_format)) {
    need(is.character(cfg$date_format) && length(cfg$date_format) == 1 && grepl("%", cfg$date_format, fixed = TRUE),
      "date_format must be one R date format such as \"%d/%m/%Y\" or \"%Y-%m-%d\".")
  }
  if (fiscal_unset && isTRUE(cfg$date_type %in% c("month", "quarter", "year"))) {
    warnings <- c(warnings, "fiscal_year_start is not set, so a January fiscal year is assumed. Ask the user which month their fiscal year starts and set it (for example 7 for July).")
  } else if (!fiscal_unset) {
    fy <- suppressWarnings(as.numeric(cfg$fiscal_year_start))
    need(length(fy) == 1 && !is.na(fy) && fy %in% 1:12, "fiscal_year_start must be a month number from 1 to 12.")
  }

  if (length(errors) == 0) {
    guide <- finn_horizon_guide()[[cfg$date_type]]
    if (h > guide[[2]]) {
      warnings <- c(warnings, sprintf("A %d-%s horizon is long; accuracy usually drops past %d.", h, cfg$date_type, guide[[2]]))
    }
    if (identical(cfg$mode, "agent") && is.null(cfg$agent$llm$provider)) {
      errors <- c(errors, "agent.llm.provider is required for agent mode.")
    }
  }

  if (!is.null(data) && length(errors) == 0) {
    cols <- c(cfg$combo_variables, cfg$target_variable, cfg$external_regressors)
    missing <- setdiff(cols, names(data))
    if (length(missing) > 0) errors <- c(errors, paste0("Columns not found in data: ", paste(missing, collapse = ", "), "."))
  }

  if (!is.null(data) && length(errors) == 0) {
    key <- do.call(paste, c(data[cfg$combo_variables], sep = "--"))
    facts$n_series <- length(unique(key))
    facts$n_rows <- nrow(data)
    facts$date_min <- as.character(min(data$Date, na.rm = TRUE))
    facts$date_max <- as.character(max(data$Date, na.rm = TRUE))
    has_target <- !is.na(data[[cfg$target_variable]])
    facts$last_actual_date <- if (any(has_target)) as.character(max(data$Date[has_target], na.rm = TRUE)) else NULL
    facts$incomplete_period <- finn_incomplete_period(facts$last_actual_date, cfg$date_type, data$Date[has_target], today)
    facts$n_periods <- length(unique(data$Date[has_target & !is.na(data$Date)]))
    dup <- sum(duplicated(data[c("Date", cfg$combo_variables)]))
    if (dup > 0) errors <- c(errors, sprintf("%d rows repeat the same series and date. Aggregate or remove duplicates first.", dup))
    per <- table(key[!is.na(data[[cfg$target_variable]])])
    facts$min_points_per_series <- if (length(per)) as.integer(min(per)) else 0L
    if (facts$min_points_per_series < 2 * h) {
      warnings <- c(warnings, sprintf(
        "Some series have only %d data points; at least %d (twice the horizon) gives more reliable back tests.",
        facts$min_points_per_series, 2 * h
      ))
    }
    if (any(data[[cfg$target_variable]] < 0, na.rm = TRUE) && !isTRUE(cfg$negative_forecast)) {
      warnings <- c(warnings, "Target has negative values but negative_forecast is false; forecasts below zero will be set to zero.")
    }
    warnings <- c(warnings, finn_quality_flags(data, cfg, key, facts))
    if (facts$n_series >= 100 && finn_is_onedrive(root %||% "")) {
      warnings <- c(warnings, sprintf(
        "%d series in a OneDrive folder will write many files and can slow syncing. Consider pausing sync during the run.",
        facts$n_series
      ))
    }
  }
  if (!is.null(data) && length(errors) == 0 && length(cfg$custom_models)) {
    custom <- finn_check_custom_models(cfg, root)
    errors <- c(errors, custom$errors)
    facts$custom_models <- custom$models
  }
  list(config = cfg, errors = unique(errors), warnings = unique(warnings), facts = facts)
}

#' Latest history period that has not ended yet
#'
#' Month, quarter, and year data only. A date on the last day of a month
#' stands for the period ending on it; any other date stands for the period
#' starting on it. When `today` falls inside that period its actuals are
#' probably partial, so it usually should not be the history end.
#'
#' @param last_date Last history date, as a Date or "YYYY-MM-DD" string.
#' @param date_type finnts date type.
#' @param dates Optional dates with actuals; the latest one before
#'   `last_date` becomes the suggested history end.
#' @param today Date to compare with; tests pass a fixed date.
#' @return NULL when the period has ended, has not started, or is not
#'   checked; otherwise a list with `date`, `period_start`, `period_end`,
#'   `today`, `suggested_hist_end_date` (NULL without an earlier date), and
#'   `message`.
finn_incomplete_period <- function(last_date, date_type, dates = NULL, today = Sys.Date()) {
  months <- if (length(date_type) == 1) switch(date_type, month = 1L, quarter = 3L, year = 12L, NULL)
  d <- tryCatch(as.Date(last_date), error = function(e) NULL)
  if (is.null(months) || length(d) != 1 || is.na(d)) {
    return(NULL)
  }
  shift <- function(x, n) if (n == 0) x else seq(x, by = paste(n, "months"), length.out = 2)[2]
  first <- as.Date(format(d, "%Y-%m-01"))
  if (d != first && d == shift(first, 1L) - 1) {
    start <- shift(first, -(months - 1L))
    end <- d
  } else {
    start <- d
    end <- shift(d, months) - 1
  }
  today <- as.Date(today)
  if (today < start || today > end) {
    return(NULL)
  }
  dates <- if (length(dates)) tryCatch(as.Date(dates), error = function(e) as.Date(character())) else as.Date(character())
  prior <- dates[!is.na(dates) & dates < d]
  prior <- if (length(prior)) format(max(prior))
  list(
    date = format(d), period_start = format(start), period_end = format(end), today = format(today),
    suggested_hist_end_date = prior,
    message = paste0(
      sprintf(
        "The latest period with actuals (`%s`, covering %s to %s) has not ended yet (day %d of %d), so its actuals are probably incomplete.",
        format(d), format(start), format(end), as.integer(today - start) + 1L, as.integer(end - start) + 1L
      ),
      if (is.null(prior)) {
        " Confirm it is complete, or remove it from the file."
      } else {
        sprintf(" Set `hist_end_date` to `%s` so history ends at the last complete period.", prior)
      }
    )
  )
}

#' Incomplete latest period the run would still use
#'
#' @param cfg Merged config.
#' @param facts Data facts with `incomplete_period` from [finn_check_config()].
#' @return The `incomplete_period` entry, or NULL when there is none or
#'   `hist_end_date` already ends before it.
finn_partial_in_history <- function(cfg, facts) {
  p <- facts$incomplete_period
  if (is.null(p) || (!is.null(cfg$hist_end_date) && as.Date(cfg$hist_end_date) < as.Date(p$date))) {
    return(NULL)
  }
  p
}

#' Data-quality warnings that never block a run
#'
#' Flags series that are all zero, history and future boundaries that look
#' wrong, a latest history period that has not ended yet, and drivers without
#' future values. Every flag is a warning so the user decides whether to fix
#' the source file.
#'
#' @param data Prepared data frame with a `Date` column.
#' @param cfg Merged config.
#' @param key Series key per row.
#' @param facts Facts already computed, including `last_actual_date` and
#'   `incomplete_period`.
#' @return Character vector of warnings.
finn_quality_flags <- function(data, cfg, key, facts) {
  out <- character()
  target <- data[[cfg$target_variable]]
  has_target <- !is.na(target)
  if (any(has_target)) {
    nonzero <- tapply(target[has_target] != 0, key[has_target], any)
    zero_series <- names(nonzero)[!nonzero]
    if (length(zero_series) > 0) {
      out <- c(out, sprintf(
        "%d series have only zeros (for example %s). They will forecast zero; remove them if they are retired.",
        length(zero_series), paste(utils::head(zero_series, 3), collapse = ", ")
      ))
    }
  }
  hist_end <- if (!is.null(cfg$hist_end_date)) as.Date(cfg$hist_end_date) else NULL
  last_actual <- if (!is.null(facts$last_actual_date)) as.Date(facts$last_actual_date) else NULL
  if (!is.null(hist_end)) {
    late <- sum(has_target & data$Date > hist_end)
    if (late > 0) {
      out <- c(out, sprintf(
        "%d target values fall after hist_end_date (%s). finnts treats them as future rows and ignores them. Check that this is intended.",
        late, format(hist_end)
      ))
    }
  } else if (!is.null(last_actual) && any(data$Date > last_actual)) {
    out <- c(out, sprintf(
      "Rows after %s have no target value. If they hold future driver values, set hist_end_date to %s.",
      format(last_actual), format(last_actual)
    ))
  }
  partial <- finn_partial_in_history(cfg, facts)
  if (!is.null(partial)) out <- c(out, partial$message)
  regs <- unlist(cfg$external_regressors)
  boundary <- hist_end %||% last_actual
  if (length(regs) > 0 && !is.null(boundary)) {
    future <- data$Date > boundary
    if (!any(future)) {
      out <- c(out, sprintf(
        "Drivers (%s) have no values after %s, so finnts can only use their past values.",
        paste(regs, collapse = ", "), format(boundary)
      ))
    } else {
      gaps <- regs[vapply(regs, function(r) anyNA(data[[r]][future]), logical(1))]
      if (length(gaps) > 0) {
        out <- c(out, sprintf("Drivers %s have blank future values after %s.", paste(gaps, collapse = ", "), format(boundary)))
      }
    }
  }
  out
}

#' Load, prepare, and validate the data a config points at
#'
#' Series columns are read as text, as in runs, so identifiers such as `001`
#' and `1` stay two series.
#'
#' @param cfg Config list.
#' @param root Optional storage root for OneDrive warnings.
#' @return List from [finn_check_config()] plus `data` and `notes`.
finn_load_checked <- function(cfg, root = NULL) {
  first <- finn_check_config(cfg, root = root)
  if (length(first$errors) > 0) {
    return(c(first, list(data = NULL, notes = character())))
  }
  raw <- finn_load_data(first$config$data$file, first$config$data$sheet, text_columns = first$config$combo_variables)
  prepared <- finn_prepare_input(raw, first$config)
  checked <- finn_check_config(cfg, prepared$data, root = root)
  notes <- prepared$notes
  delim <- attr(raw, "finn_delimiter")
  if (!is.null(delim) && !identical(delim, ",")) {
    notes <- c(notes, sprintf("The file uses %s as the column separator.", c(";" = "semicolons", "\t" = "tabs", "|" = "pipes")[[delim]] %||% delim))
  }
  if (identical(attr(raw, "finn_encoding"), "latin1")) {
    notes <- c(notes, "The file is not UTF-8, so it was read as Latin-1. Check that accented names look right.")
  }
  c(checked, list(data = prepared$data, notes = notes))
}

# ---------------------------------------------------------------------------
# finnts setup helpers
# ---------------------------------------------------------------------------

#' Build the LLM chat object for Agent runs
#'
#' Keys are read from environment variables only and never written to disk.
#'
#' @param llm_cfg List with `provider` ("copilot", "azure_openai", "openai"),
#'   `model`, and for Copilot optional `command` (CLI path) and `timeout`.
#' @return An ellmer chat object.
finn_build_llm <- function(llm_cfg) {
  provider <- llm_cfg$provider %||% "copilot"
  model <- llm_cfg$model
  switch(provider,
    copilot = finnts::chat_copilot(
      model = model %||% "auto",
      command = finn_resolve_copilot_command(llm_cfg$command %||% "copilot"),
      timeout = as.numeric(llm_cfg$timeout %||% 120)
    ),
    azure_openai = {
      finn_require("ellmer")
      endpoint <- Sys.getenv(llm_cfg$endpoint_env %||% "AZURE_OPENAI_ENDPOINT")
      if (!nzchar(endpoint)) stop("Set the AZURE_OPENAI_ENDPOINT environment variable.", call. = FALSE)
      key <- Sys.getenv(llm_cfg$api_key_env %||% "AZURE_OPENAI_API_KEY")
      if (nzchar(key)) {
        ellmer::chat_azure_openai(endpoint = endpoint, model = model, api_key = key)
      } else {
        ellmer::chat_azure_openai(endpoint = endpoint, model = model)
      }
    },
    openai = {
      finn_require("ellmer")
      key <- Sys.getenv(llm_cfg$api_key_env %||% "OPENAI_API_KEY")
      if (!nzchar(key)) stop("Set the OPENAI_API_KEY environment variable.", call. = FALSE)
      ellmer::chat_openai(model = model, api_key = key)
    },
    stop("Unknown LLM provider '", provider, "'. Use copilot, azure_openai, or openai.", call. = FALSE)
  )
}

#' Detect which GitHub credential source chat_copilot() would use
#'
#' Mirrors the order used by `finnts::chat_copilot()`: environment tokens
#' first, then an existing GitHub CLI login. Token values are never read
#' into the result or printed.
#'
#' @param env Named character vector of environment values to inspect;
#'   defaults to the current process environment.
#' @param gh_logged_in Function returning TRUE when `gh auth status` succeeds.
#' @return One of "COPILOT_GITHUB_TOKEN", "GH_TOKEN", "GITHUB_TOKEN", "gh", or
#'   "none".
finn_copilot_auth_source <- function(env = Sys.getenv(c("COPILOT_GITHUB_TOKEN", "GH_TOKEN", "GITHUB_TOKEN")),
                                     gh_logged_in = finn_gh_logged_in) {
  for (v in c("COPILOT_GITHUB_TOKEN", "GH_TOKEN", "GITHUB_TOKEN")) {
    if (nzchar(trimws(env[[v]] %||% ""))) return(v)
  }
  if (isTRUE(gh_logged_in())) "gh" else "none"
}

#' Check whether the GitHub CLI has a signed-in account
#'
#' Runs `gh auth status` and discards its output, so no token is captured.
#'
#' @return TRUE when `gh` is on PATH and reports a signed-in account.
finn_gh_logged_in <- function() {
  gh <- unname(Sys.which("gh"))
  if (!nzchar(gh)) return(FALSE)
  status <- tryCatch(
    suppressWarnings(system2(gh, c("auth", "status"), stdout = FALSE, stderr = FALSE, timeout = 30)),
    error = function(e) 1L
  )
  identical(as.integer(status), 0L)
}

#' Find the Copilot CLI when it is installed but not on PATH
#'
#' Explicit paths and commands already on PATH are returned unchanged. For the
#' default `copilot` command, also looks in the usual standalone install
#' locations (WinGet links and package folders on Windows; ~/.local/bin and
#' Homebrew elsewhere). The VS Code Copilot Chat extension is not the CLI.
#'
#' @param command Configured command or path.
#' @param candidates Optional character vector of paths to try, for tests.
#' @return The command to pass to `chat_copilot()`; unchanged when nothing better is found.
finn_resolve_copilot_command <- function(command = "copilot", candidates = NULL) {
  if (!identical(command, "copilot") || nzchar(Sys.which(command))) return(command)
  if (is.null(candidates)) {
    home <- path.expand("~")
    if (.Platform$OS.type == "windows") {
      lad <- Sys.getenv("LOCALAPPDATA")
      pkgs <- if (nzchar(lad)) Sys.glob(file.path(lad, "Microsoft", "WinGet", "Packages", "GitHub.Copilot_*", "copilot.exe")) else character()
      candidates <- c(
        if (nzchar(lad)) file.path(lad, "Microsoft", "WinGet", "Links", "copilot.exe"),
        pkgs,
        if (nzchar(lad)) file.path(lad, "Programs", "GitHub Copilot", "copilot.exe")
      )
    } else {
      candidates <- c(file.path(home, ".local", "bin", "copilot"), "/opt/homebrew/bin/copilot", "/usr/local/bin/copilot")
    }
  }
  hit <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (length(hit)) normalizePath(hit[[1]], winslash = "/") else command
}

#' Check that Agent runs can reach GitHub Copilot through finnts::chat_copilot()
#'
#' Reuses the installed finnts runtime check (CLI 1.0.93+, standalone
#' executable, processx 3.9.0+, required CLI flags) when available, then checks
#' that a GitHub credential source exists. Makes no model request and never
#' prints tokens.
#'
#' @param command Copilot CLI command or path, as passed to `chat_copilot()`.
#' @param auth_source Function returning the credential source name.
#' @return List with `ready`, `command` (resolved CLI command or path),
#'   `cli_path`, `auth_source`, `chat_copilot_available`, and `problems`
#'   (plain-language fixes).
finn_copilot_status <- function(command = "copilot", auth_source = finn_copilot_auth_source) {
  problems <- character()
  command <- finn_resolve_copilot_command(command)
  has_fn <- function(name) {
    requireNamespace("finnts", quietly = TRUE) &&
      exists(name, envir = asNamespace("finnts"), inherits = FALSE)
  }
  chat_ok <- has_fn("chat_copilot")
  cli_path <- unname(Sys.which(command))
  if (!nzchar(cli_path) && file.exists(command) && !dir.exists(command)) cli_path <- normalizePath(command)

  if (!chat_ok) {
    problems <- c(problems, "This finnts version has no chat_copilot(). Install the latest GitHub version with install_finnts.R --confirm=true.")
  } else if (has_fn("check_copilot_runtime")) {
    err <- tryCatch({
      utils::getFromNamespace("check_copilot_runtime", "finnts")(command, 30)
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(err)) problems <- c(problems, finn_copilot_fix(err))
  } else if (!nzchar(cli_path)) {
    problems <- c(problems, finn_copilot_fix("not found"))
  }

  auth <- auth_source()
  if (identical(auth, "none")) {
    problems <- c(problems, paste(
      "No GitHub sign-in found. Walk the user through:",
      "1) install the GitHub CLI (https://cli.github.com/, e.g. `winget install GitHub.cli`);",
      "2) run `gh auth login` in their own terminal;",
      "3) sign in with an account that has Copilot access;",
      "4) retry the Finn request.",
      "Setting GH_TOKEN themselves also works. Signing in only inside the Copilot CLI is not enough.",
      "Guide: https://docs.github.com/en/copilot/how-tos/copilot-cli/set-up-copilot-cli/authenticate-copilot-cli"
    ))
  }
  list(
    ready = length(problems) == 0,
    command = command,
    cli_path = if (nzchar(cli_path)) cli_path else NULL,
    auth_source = auth,
    chat_copilot_available = chat_ok,
    problems = as.list(problems)
  )
}

#' Translate a chat_copilot() runtime error into a user-facing fix
#'
#' @param message Error message from the finnts Copilot runtime check.
#' @return One plain-language sentence telling the user what to do.
finn_copilot_fix <- function(message) {
  if (grepl("processx", message, ignore.case = TRUE)) {
    return("Upgrade the processx package to 3.9.0 or newer (install_finnts.R --confirm=true does this).")
  }
  if (grepl("not found", message, ignore.case = TRUE)) {
    return("Install GitHub Copilot CLI 1.0.93 or newer (for example `winget install GitHub.Copilot`; the VS Code Copilot Chat extension alone is not enough) and put `copilot` on PATH, or set agent.llm.command to its full path.")
  }
  if (grepl("shim", message, ignore.case = TRUE)) {
    return("On Windows, use the standalone copilot.exe (for example `winget install GitHub.Copilot`), not the npm copilot.cmd; set agent.llm.command to its path if needed.")
  }
  if (grepl("1\\.0\\.93|lacks required options|inspection failed", message)) {
    return("Update GitHub Copilot CLI to 1.0.93 or newer: run `copilot update` (or `winget upgrade GitHub.Copilot` on Windows).")
  }
  paste0("Copilot CLI check failed: ", message)
}

#' Register (or reopen) the finnts project for a Finn project
#'
#' @param paths Project paths.
#' @param cfg Normalized config.
#' @return finnts project_info list. `fiscal_year_start` is passed as a
#'   double because finnts rejects integers and JSON whole numbers parse as
#'   integers.
finn_project_info <- function(paths, cfg) {
  finnts::set_project_info(
    project_name = paths$name,
    path = paths$artifacts,
    combo_variables = unlist(cfg$combo_variables),
    target_variable = cfg$target_variable,
    date_type = cfg$date_type,
    fiscal_year_start = as.numeric(cfg$fiscal_year_start %||% 1)
  )
}

#' Path to the Rscript executable of the running R
#'
#' @return Rscript path.
finn_rscript <- function() {
  exe <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  file.path(R.home("bin"), exe)
}

#' Approximate in-memory size of a data frame in GB
#'
#' @param df Data frame.
#' @return Numeric GB.
finn_data_gb <- function(df) as.numeric(utils::object.size(df)) / 1024^3

#' Count how many series a prepared data frame holds
#'
#' @param df Prepared data.
#' @param combo_variables Series columns.
#' @return Integer count.
finn_count_series <- function(df, combo_variables) {
  nrow(unique(df[combo_variables]))
}

#' Read the last lines of a text file
#'
#' @param path File path.
#' @param n Number of lines.
#' @return Character vector, redacted.
finn_tail <- function(path, n = 40L) {
  if (!file.exists(path)) {
    return(character())
  }
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  finn_redact(utils::tail(lines, n))
}

# ---------------------------------------------------------------------------
# Custom models
# ---------------------------------------------------------------------------

#' Folder holding a project's saved custom models
#'
#' Each approved custom model version is saved once as
#' `custom_models/<name>/<version>.rds` with readable metadata in
#' `<version>.json`; worked examples live in `custom_models/<name>-examples.csv`.
#'
#' @param paths Project paths from [finn_project_paths()].
#' @return Folder path; it may not exist yet.
finn_custom_models_dir <- function(paths) {
  file.path(paths$dir, "custom_models")
}

#' Short form of a custom model version id
#'
#' @param version_id Full 64-character version id.
#' @return Its first 12 characters, used in file names and reviews.
finn_custom_version_short <- function(version_id) {
  substr(as.character(version_id), 1L, 12L)
}

#' Files that store one saved custom model version
#'
#' @param paths Project paths.
#' @param name Custom model name.
#' @param version_id Full version id.
#' @return List with `dir`, `rds` (the approved model), and `json` (metadata).
finn_custom_model_files <- function(paths, name, version_id) {
  dir <- file.path(finn_custom_models_dir(paths), name)
  short <- finn_custom_version_short(version_id)
  list(dir = dir, rds = file.path(dir, paste0(short, ".rds")), json = file.path(dir, paste0(short, ".json")))
}

#' Worked-examples file for a custom model
#'
#' @param paths Project paths.
#' @param name Custom model name.
#' @return Path of `custom_models/<name>-examples.csv`.
finn_custom_examples_file <- function(paths, name) {
  file.path(finn_custom_models_dir(paths), paste0(name, "-examples.csv"))
}

#' Normalize the `custom_models` config entry
#'
#' The entry lists the saved custom models every run of the project uses, as
#' `[{"name": ..., "version_id": ...}]`; one object is accepted as one entry.
#' List entries keep only `name` and `version_id` (NA when missing) and are
#' sorted by name, so the config fingerprint does not depend on their order.
#' Other values are kept for [finn_check_custom_models()] to report.
#'
#' @param x Raw `custom_models` value.
#' @return NULL when empty, otherwise the normalized entries.
finn_custom_normalize <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NULL)
  }
  if (!is.list(x)) {
    return(x)
  }
  if (!is.null(names(x)) && "name" %in% names(x)) x <- list(x)
  scalar <- function(v) {
    v <- unlist(v)
    if (length(v) != 1L || is.na(v)) NA_character_ else as.character(v)
  }
  out <- lapply(unname(x), function(e) if (is.list(e)) list(name = scalar(e$name), version_id = scalar(e$version_id)) else e)
  key <- vapply(out, function(e) if (is.list(e) && !is.na(e$name)) e$name else "", character(1))
  out[order(key, method = "radix")]
}

#' Names of the custom models a config enrolls
#'
#' @param cfg Config list (merged or raw), or NULL.
#' @return Character vector of model names; empty when none are enrolled.
finn_custom_names <- function(cfg) {
  entries <- finn_custom_normalize(cfg$custom_models)
  if (!is.list(entries)) {
    return(character())
  }
  as.character(unlist(lapply(entries, function(e) if (is.list(e) && !is.na(e$name)) e$name)))
}

#' Labels of the custom models a config enrolls
#'
#' @param cfg Config list (merged or raw), or NULL.
#' @return Sorted `name@version` labels with short version ids; empty when
#'   none are enrolled.
finn_custom_labels <- function(cfg) {
  entries <- finn_custom_normalize(cfg$custom_models)
  if (!length(entries)) {
    return(character())
  }
  if (!is.list(entries)) {
    return("?")
  }
  labels <- vapply(entries, function(e) {
    if (is.list(e)) paste0(e$name, "@", finn_custom_version_short(e$version_id)) else "?"
  }, character(1))
  sort(labels, method = "radix")
}

#' Format custom model labels for a sentence
#'
#' @param labels Labels from [finn_custom_labels()].
#' @return Labels in backticks separated by commas, or "none".
finn_custom_label_text <- function(labels) {
  if (length(labels)) paste(sprintf("`%s`", labels), collapse = ", ") else "none"
}

#' Plain-language rule for custom model names
#'
#' @return One sentence fragment describing valid names.
finn_custom_name_rule <- function() {
  paste("Custom model names must start with a lowercase letter, use only lowercase letters, digits,",
    "`_`, or `-` (no `--`), and differ from Finn's own model names")
}

#' Custom model names that finnts would reject
#'
#' @param names Candidate names.
#' @param builtin Finn's built-in model names, from `finnts::list_models()`.
#' @return The invalid names, in input order.
finn_custom_bad_names <- function(names, builtin = character()) {
  names <- as.character(names)
  reserved <- c(builtin, "all", "all-data", "best-model", "local", "global", "ensemble", "average")
  names[is.na(names) | !grepl("^[a-z][a-z0-9_-]*$", names) | grepl("--", names, fixed = TRUE) | names %in% reserved]
}

#' Whether the installed finnts can create and run custom models
#'
#' @param mode Config mode; `"agent"` also needs custom model support in the
#'   internal Agent setup.
#' @return Logical.
finn_custom_supported <- function(mode = "standard") {
  if (!requireNamespace("finnts", quietly = TRUE)) {
    return(FALSE)
  }
  ns <- asNamespace("finnts")
  has <- function(f) exists(f, envir = ns, inherits = FALSE)
  ok <- all(vapply(c("create_custom_model", "custom_model_feedback", "add_custom_recipe"), has, logical(1))) &&
    "custom_models" %in% names(formals(get("prep_models", envir = ns)))
  if (ok && identical(mode, "agent")) {
    ok <- has("set_agent_info_custom") && "custom_models" %in% names(formals(get("set_agent_info_custom", envir = ns)))
  }
  ok
}

#' Whether the installed finnts can create custom models with GitHub Copilot
#'
#' Copilot creation needs the structured replies that newer finnts versions
#' add to `chat_copilot()`.
#'
#' @return Logical.
finn_custom_copilot_supported <- function() {
  requireNamespace("finnts", quietly = TRUE) &&
    exists("copilot_request_turn", envir = asNamespace("finnts"), inherits = FALSE)
}

#' Check that a saved custom model is intact and approved
#'
#' Uses `finnts::custom_model_feedback()`, which checks the model's integrity
#' records without running its code.
#'
#' @param model Object read from a saved custom model file.
#' @return Invisible feedback list; errors when the model is not approved.
finn_custom_verify <- function(model) {
  feedback <- finnts::custom_model_feedback(model)
  if (!identical(feedback$status, "approved")) {
    stop("it is not an approved model (status ", feedback$status %||% "unknown", ").", call. = FALSE)
  }
  invisible(feedback)
}

#' Read one enrolled custom model from the project
#'
#' Reads the exact saved file for the entry; it never lists folders.
#'
#' @param paths Project paths.
#' @param entry Normalized entry with `name` and `version_id`.
#' @param verify Whether to run [finn_custom_verify()].
#' @return The custom model object. Errors with a plain message when the file
#'   is missing, unreadable, not a custom model, or does not match the entry.
finn_custom_read <- function(paths, entry, verify = TRUE) {
  files <- finn_custom_model_files(paths, entry$name, entry$version_id)
  label <- sprintf("Custom model `%s` (version %s)", entry$name, finn_custom_version_short(entry$version_id))
  if (!file.exists(files$rds)) {
    stop(label, " is not saved in this project (expected ", finn_norm(files$rds),
      "). Create it with custom_model.R --action=create, or unenroll it.", call. = FALSE)
  }
  model <- tryCatch(readRDS(files$rds), error = function(e) {
    stop(label, " could not be read: ", conditionMessage(e), call. = FALSE)
  })
  def <- if (is.list(model)) model$definition
  if (!inherits(model, "finnts_custom_model") || !is.list(def)) {
    stop(label, " is not a finnts custom model file.", call. = FALSE)
  }
  if (!identical(def$name, entry$name) || !identical(def$version_id, entry$version_id)) {
    stop(label, " does not match its saved file, which holds `", paste(def$name, collapse = ""), "` version ",
      finn_custom_version_short(paste(def$version_id, collapse = "")), ".", call. = FALSE)
  }
  if (isTRUE(verify)) {
    tryCatch(finn_custom_verify(model), error = function(e) {
      stop(label, " failed its integrity check: ", conditionMessage(e),
        " Create it again with custom_model.R --action=create.", call. = FALSE)
    })
  }
  model
}

#' Read every custom model a config enrolls
#'
#' @param paths Project paths.
#' @param cfg Config list.
#' @param verify Whether to verify each model; the worker skips it because
#'   finnts verifies models again when a run uses them.
#' @return Named list of custom model objects (names are model names), or
#'   NULL when none are enrolled.
finn_load_custom_models <- function(paths, cfg, verify = TRUE) {
  entries <- finn_custom_normalize(cfg$custom_models)
  if (!length(entries)) {
    return(NULL)
  }
  models <- lapply(entries, function(e) finn_custom_read(paths, e, verify))
  names(models) <- vapply(entries, function(e) e$name, character(1))
  models
}

#' Recipes a standard run prepares
#'
#' With custom models enrolled, adds the original-scale `C1` recipe they read
#' the same way `forecast_time_series()` does, keeping the requested standard
#' recipes (date-type defaults for NULL, R1 plus R2 for "all").
#'
#' @param cfg Config list.
#' @return Recipe ids, or NULL for finnts defaults.
finn_custom_recipes <- function(cfg) {
  recipes <- if (length(cfg$recipes_to_run)) unlist(cfg$recipes_to_run)
  if (!length(finn_custom_names(cfg))) {
    return(recipes)
  }
  finnts:::add_custom_recipe(recipes, cfg$date_type)
}

#' Models a standard run trains
#'
#' finnts needs every supplied custom model listed when `models_to_run` is
#' set, so enrolled custom models are added to an explicit list. NULL stays
#' NULL, which already includes them.
#'
#' @param cfg Config list.
#' @return Model names, or NULL for finnts defaults.
finn_custom_models_to_run <- function(cfg) {
  models <- if (length(cfg$models_to_run)) unlist(cfg$models_to_run)
  if (is.null(models)) {
    return(NULL)
  }
  unique(c(models, finn_custom_names(cfg)))
}

#' Parallel plan for an Agent run with custom models
#'
#' finnts runs Agent custom models only without inner parallelism, so an
#' inner-parallel plan becomes parallel series workers within the same worker
#' budget (halved after one memory failure, as [finn_choose_parallel()] does),
#' or sequential with one series, one worker, or after repeated memory
#' failures. Notes about other runs or batch shares are kept. Other plans are
#' returned unchanged.
#'
#' @param plan Plan from [finn_plan_with_others()].
#' @param cfg Config list.
#' @param data_gb Approximate input size in GB.
#' @param n_series Number of series.
#' @param step_down Fallback level after memory failures; 2 or more stays
#'   sequential.
#' @return Adjusted plan with its claim recomputed by [finn_plan_claim()].
finn_custom_agent_plan <- function(plan, cfg, data_gb, n_series, step_down = 0L) {
  if (!length(finn_custom_names(cfg)) || !isTRUE(plan$inner_parallel)) {
    return(plan)
  }
  workers <- as.integer(plan$num_cores %||% 1L)
  n <- as.integer(n_series %||% 1L)
  step_down <- as.integer(step_down %||% 0L)
  reason <- plan$reason %||% ""
  marker <- "not a memory limit."
  suffix <- if (grepl(marker, reason, fixed = TRUE)) sub("^.*?not a memory limit\\.", "", reason, perl = TRUE) else ""
  plan$inner_parallel <- FALSE
  if (n >= 2L && workers >= 2L && step_down < 2L) {
    if (step_down >= 1L) workers <- max(2L, workers %/% 2L)
    plan$parallel_processing <- "local_machine"
    plan$num_cores <- min(n, workers)
    reason <- paste("Local machine: Agent runs with custom models can't train models in parallel within a series,",
      "so series train in parallel worker processes instead.")
  } else {
    plan["parallel_processing"] <- list(NULL)
    plan["num_cores"] <- list(NULL)
    reason <- paste("Sequential: Agent runs with custom models can't train models in parallel within a series,",
      "and this run has one series, one worker, or earlier memory failures.")
  }
  plan$reason <- paste0(reason, " Uses ", plan$num_cores %||% 1L,
    " worker(s); 300 MB per worker is an estimate, not a memory limit.", suffix)
  finn_plan_claim(plan, data_gb)
}

#' Short description of a saved custom model
#'
#' @param model Custom model object.
#' @param combo_variables Series columns, left out of the driver list.
#' @return List with `name`, `version_id`, `version` (short id), `model_type`,
#'   `drivers`, `date_types`, `hierarchy` (approach or NULL), `approval`, and
#'   `interpretation`.
finn_custom_summary <- function(model, combo_variables = NULL) {
  def <- model$definition
  req <- def$requirements %||% list()
  list(
    name = def$name, version_id = def$version_id, version = finn_custom_version_short(def$version_id),
    model_type = as.list(unlist(def$model_type)),
    drivers = as.list(setdiff(unlist(req$predictors), c("Date", "Combo", unlist(combo_variables)))),
    date_types = as.list(unlist(req$date_types)),
    hierarchy = req$hierarchy$forecast_approach,
    approval = model$approval$mode,
    interpretation = paste(def$interpretation %||% "", collapse = " ")
  )
}

#' Readable metadata saved next to a custom model
#'
#' Holds no data values, model code, or secrets.
#'
#' @param model Custom model object.
#' @param llm_cfg LLM settings used to create it (`provider`, `model`).
#' @param n_examples Number of worked examples supplied.
#' @param combo_variables Series columns.
#' @return List written to `<version>.json`.
finn_custom_metadata <- function(model, llm_cfg = list(), n_examples = 0L, combo_variables = NULL) {
  s <- finn_custom_summary(model, combo_variables)
  list(
    name = s$name, version_id = s$version_id, model_type = s$model_type, drivers = s$drivers,
    date_types = s$date_types, hierarchy = s$hierarchy, approval = s$approval,
    checks = as.list(names(model$validation$checks)),
    provider = llm_cfg$provider %||% "copilot", llm_model = llm_cfg$model %||% "auto",
    created_at = as.character(model$definition$created_at %||% finn_now()),
    examples = as.integer(n_examples)
  )
}

#' Save an approved custom model in the project without replacing files
#'
#' @param paths Project paths.
#' @param model Approved custom model object.
#' @param meta Metadata from [finn_custom_metadata()].
#' @return List with `rds`, `json`, and `status` ("saved", or "exists" when
#'   this exact version was already saved and nothing was written).
finn_custom_save <- function(paths, model, meta) {
  files <- finn_custom_model_files(paths, model$definition$name, model$definition$version_id)
  if (file.exists(files$rds)) {
    return(list(rds = finn_norm(files$rds), json = finn_norm(files$json), status = "exists"))
  }
  dir.create(files$dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(model, files$rds)
  if (!file.exists(files$json)) finn_write_json(meta, files$json)
  list(rds = finn_norm(files$rds), json = finn_norm(files$json), status = "saved")
}

#' Check the custom models a config enrolls
#'
#' Early, plain-language versions of the checks finnts applies when a run
#' uses custom models: each enrolled model must be saved in the project,
#' intact, and compatible with the run settings. finnts enforces its own
#' checks again during the run.
#'
#' @param cfg Merged config.
#' @param root Storage root holding the project folder, or NULL.
#' @return List with `errors` and `models` (one [finn_custom_summary()] per
#'   enrolled model that passed).
finn_check_custom_models <- function(cfg, root) {
  entries <- finn_custom_normalize(cfg$custom_models)
  errors <- character()
  models <- list()
  done <- function() list(errors = unique(errors), models = models)
  if (!length(entries)) {
    return(done())
  }
  agent <- identical(cfg$mode, "agent")
  if (!finn_custom_supported(cfg$mode)) {
    errors <- paste("The installed finnts cannot run custom models", if (agent) "in Agent runs" else "",
      "- offer install_finnts.R --confirm=true to install the latest finnts from GitHub, or unenroll the custom models.")
    return(done())
  }
  if (is.null(root)) {
    errors <- "Custom models are read from the Finn project folder; validate the project with validate_config.R --project=<name>."
    return(done())
  }
  well_formed <- is.list(entries) && all(vapply(entries, function(e) {
    is.list(e) && !is.na(e$name) && !is.na(e$version_id)
  }, logical(1)))
  if (!well_formed) {
    errors <- "custom_models must be a list of {\"name\": ..., \"version_id\": ...} entries. Change it with custom_model.R --action=enroll or --action=unenroll."
    return(done())
  }
  names <- vapply(entries, function(e) e$name, character(1))
  ids <- vapply(entries, function(e) e$version_id, character(1))
  quote <- function(x) paste(sprintf("`%s`", x), collapse = ", ")
  builtin <- tryCatch(finnts::list_models(), error = function(e) character())
  bad <- finn_custom_bad_names(names, builtin)
  if (length(bad)) {
    errors <- c(errors, paste0(finn_custom_name_rule(), ": ", quote(bad), "."))
  }
  bad <- names[!grepl("^[a-f0-9]{64}$", ids)]
  if (length(bad)) {
    errors <- c(errors, paste0("custom_models needs each model's full 64-character version_id (see custom_model.R --action=list): ", quote(bad), "."))
  }
  bad <- unique(names[duplicated(names)])
  if (length(bad)) errors <- c(errors, paste0("Enroll each custom model only once: ", quote(bad), "."))
  bad <- intersect(names, unlist(cfg$models_not_to_run))
  if (length(bad)) {
    errors <- c(errors, paste0("Enrolled custom models can't also be in models_not_to_run: ", quote(bad), ". Unenroll them instead."))
  }

  approach <- cfg$forecast_approach %||% "bottoms_up"
  hierarchy <- !agent && !identical(approach, "bottoms_up")
  local_on <- finn_bool(cfg$run_local_models, TRUE)
  global_on <- isTRUE(cfg$run_global_models %||% finn_default_global_models(cfg$date_type))
  if (agent) {
    if (isTRUE(cfg$agent$allow_hierarchical_forecast)) {
      errors <- c(errors, "Agent runs with custom models can't try hierarchical forecasts. Set agent.allow_hierarchical_forecast to false, or unenroll the custom models.")
    }
    if (isTRUE(cfg$agent$allow_iterate_forecast)) {
      errors <- c(errors, "Agent updates with custom models can't start a new search. Set agent.allow_iterate_forecast to false, or unenroll the custom models.")
    }
  }
  if (hierarchy) {
    if (length(cfg$external_regressors)) {
      errors <- c(errors, sprintf("Custom models in a %s forecast can't use drivers. Remove external_regressors or use forecast_approach `bottoms_up`.", approach))
    }
    if (!local_on) errors <- c(errors, "Custom models in a hierarchical forecast need run_local_models set to true.")
    if (global_on) errors <- c(errors, "Custom models in a hierarchical forecast need run_global_models set to false.")
    if (identical(cfg$date_type, "week")) {
      errors <- c(errors, "Custom models can't run in a hierarchical forecast of weekly data. Use forecast_approach `bottoms_up`.")
    }
    pool <- if (length(cfg$models_to_run)) setdiff(unlist(cfg$models_to_run), names) else builtin
    pool <- setdiff(pool, unlist(cfg$models_not_to_run))
    foundation <- intersect(pool, tryCatch(finnts:::list_foundation_models(), error = function(e) character()))
    if (!length(pool)) {
      errors <- c(errors, "Custom models in a hierarchical forecast need at least one of Finn's models alongside them in models_to_run.")
    } else if (length(foundation)) {
      errors <- c(errors, paste0("Custom models in a hierarchical forecast can't run with foundation models (", quote(foundation),
        "). Add them to models_not_to_run, or list the models to run in models_to_run."))
    }
  }
  if (length(errors)) {
    return(done())
  }

  paths <- finn_project_paths(root, cfg$project_name)
  enabled <- c(if (local_on) "local", if (global_on) "global")
  for (e in entries) {
    model <- tryCatch(finn_custom_read(paths, e, verify = TRUE), error = function(err) conditionMessage(err))
    if (is.character(model)) {
      errors <- c(errors, model)
      next
    }
    def <- model$definition
    req <- def$requirements %||% list()
    label <- sprintf("Custom model `%s`", e$name)
    date_types <- unlist(req$date_types)
    if (length(date_types) && !isTRUE(cfg$date_type %in% date_types)) {
      errors <- c(errors, sprintf("%s works with %s data, not %s data.", label, paste(date_types, collapse = "/"), cfg$date_type))
    }
    types <- unlist(def$model_type)
    if (!length(intersect(types, enabled))) {
      off <- setdiff(types, enabled)
      errors <- c(errors, sprintf("%s is a %s model, but this config turns %s models off. Set %s to true, or unenroll it.",
        label, paste(types, collapse = "/"), paste(off, collapse = " and "), paste0("run_", off, "_models", collapse = " or ")))
    }
    drivers <- setdiff(unlist(req$predictors), c("Date", "Combo", unlist(cfg$combo_variables)))
    missing <- setdiff(drivers, unlist(cfg$external_regressors))
    if (length(missing)) {
      errors <- c(errors, sprintf("%s uses drivers that are not in external_regressors: %s. Add them, or unenroll the model.", label, quote(missing)))
    }
    packages <- unique(unlist(def$packages))
    absent <- packages[!nzchar(vapply(packages, function(p) system.file(package = p), character(1)))]
    if (length(absent)) {
      errors <- c(errors, sprintf("%s needs R packages that are not installed: %s. Ask the user before installing them.", label, quote(absent)))
    }
    hier <- req$hierarchy$forecast_approach
    if (!is.null(req$hierarchy) && agent) {
      errors <- c(errors, sprintf("%s was approved only for %s forecasts, which Agent runs don't use. Create a bottoms_up version for Agent runs.", label, hier %||% "hierarchical"))
    } else if (!is.null(req$hierarchy) && !hierarchy) {
      errors <- c(errors, sprintf("%s was approved only for %s forecasts. Set forecast_approach to `%s`, or create a bottoms_up version.", label, hier %||% "hierarchical", hier %||% "standard_hierarchy"))
    } else if (hierarchy && !identical(hier, approach)) {
      errors <- c(errors, sprintf("%s isn't approved for %s forecasts. Create a version for it while forecast_approach is `%s`, or use `bottoms_up`.", label, approach, approach))
    }
    models[[length(models) + 1L]] <- finn_custom_summary(model, cfg$combo_variables)
  }
  done()
}
