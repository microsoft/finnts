# Explain why a Finn run failed or stalled and suggest a fix.
# Usage:
#   Rscript diagnose_run.R --project=<name> [--run_id=<id>] [--log_lines=60]
#     [--report=true] [--include_log=true]
#
# Defaults to the most recent run. Reads the run state and log, matches known
# failure patterns, and returns a plain-language cause plus fix steps. It
# never changes existing files; fixes that change data or settings need the
# user's OK. With --report=true it also writes one support report,
# <project>/support/finn_support_<time>.md, for attaching to a GitHub issue.
# The report holds versions, the diagnosis, the settings, and (unless
# --include_log=false) the end of the run log. It never holds data values;
# home folders, the user name, OneDrive organization names, e-mail
# addresses, and secrets are masked.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Known failure patterns
#'
#' Each entry has a regex `pattern`, a `category`, a user-facing `cause`, and
#' agent-facing `fix` steps. Order matters: the first match wins.
#'
#' @return List of pattern entries.
diagnose_patterns <- function() {
  p <- function(pattern, category, cause, fix) list(pattern = pattern, category = category, cause = cause, fix = fix)
  list(
    p("No space left on device|disk (is )?full|not enough space on the disk|There is not enough space",
      "disk_space",
      "The computer's drive ran out of free space while saving Finn files.",
      c("Ask the user to free space (empty the recycle bin, remove old downloads) or move the Finn folder to a drive with more room.",
        "Then resume with run_forecast.R using the same --mode; saved progress is kept.")),
    p("cannot allocate|out of memory|memory exhausted|std::bad_alloc|worker.*(died|terminated)|unserialize|error reading from connection|connection.*closed",
      "memory",
      "The computer ran out of memory while training models in parallel.",
      c("Resuming automatically uses fewer parallel workers (step_down is already raised).",
        "Ask the user to close large apps, then resume with run_forecast.R using the same --mode.",
        "If it fails again, set \"parallel\": \"none\" in the config (with the user's OK) and resume.")),
    p("there is no package called|could not find function|namespace .* (is|was) .* required|package .* is not available",
      "missing_package",
      "An R package that Finn needs is missing or out of date.",
      c("Run check_env.R to list what is missing.",
        "Ask the user before installing, then run install_finnts.R --confirm=true and resume.")),
    p("Inputs have recently changed",
      "agent_inputs_changed",
      "The Agent saw different settings or data than its saved version and will not mix them.",
      c("Ask the user whether to keep the original settings and data (restore them, then resume) or start a new Agent version (run_forecast.R --mode=iterate --new=true --confirm=true).")),
    p("multiple saved Agent runs match|saved run_id is missing or invalid",
      "agent_history",
      "The Agent's saved history for this run could not be matched cleanly.",
      c("Do not delete anything. Report the run id and request id to the user.",
        "Offer a new Agent version (--new=true --confirm=true) as the safe way forward.")),
    p("Cannot resume Agent version|forecast approach changed|saved forecast_approach",
      "agent_approach_changed",
      "The forecast approach (for example hierarchical vs. standard) differs from the saved Agent version.",
      c("Restore the original approach setting to resume, or start a new Agent version with the user's confirmation.")),
    p("No previous agent runs found|No completed previous agent run",
      "update_needs_agent",
      "Updating needs a finished Agent forecast, and none was found.",
      c("Run --mode=iterate first, or resume the unfinished Agent run.")),
    p("new time series .*exceeds|exceeds the limit",
      "update_too_many_new_series",
      "Too many new time series appeared since the last Agent forecast for a quick update.",
      c("Suggest a full Agent search: run_forecast.R --mode=iterate --new=true --confirm=true after the user agrees.")),
    p("Previous agent run included (local|global) models",
      "update_model_mix",
      "The update settings turn off a model type that the last Agent forecast used.",
      c("Restore run_local_models / run_global_models to match the last Agent run, or start a new Agent version.")),
    p("external regressors? .*missing|missing .*external regressor",
      "missing_regressors",
      "A driver (external regressor) column is missing or has no future values.",
      c("Run profile_data.R to check the driver columns.",
        "Ask the user to add future values for each driver for the full forecast horizon, or remove the driver from the config.")),
    p("No retained time series remain|no time series|zero rows|0 rows",
      "no_series",
      "No time series were left to forecast after filtering and cleanup.",
      c("Run profile_data.R. Check combo_cleanup_date, hist_start_date, and that the target has recent non-zero values.")),
    p("copilot.*(disabled|not enabled|policy)|(organization|enterprise|admin).*(policy|disabled|not enabled)|not (authorized|entitled|licensed) .*copilot|no (active )?copilot (subscription|license|seat)",
      "copilot_policy",
      "GitHub Copilot is not available to this account; an organization policy or missing license is blocking it.",
      c("Ask the user to confirm they have an active Copilot license and that their organization allows Copilot CLI and the chosen model.",
        "If they have several GitHub accounts, run `gh auth status` and switch with `gh auth switch` to the licensed one, then resume.",
        "Otherwise use another model provider (references/setup.md) and resume.")),
    p("premium request|monthly (limit|quota|allowance)|usage limit|exceeded .*(premium|copilot)",
      "copilot_premium_limit",
      "The Copilot account has used up its premium requests for this period.",
      c("Tell the user the Agent search stopped because of the Copilot usage limit; progress is kept.",
        "Options: wait for the allowance to reset, ask an admin for more premium requests, switch to a model that is included, or switch provider. Then resume.")),
    p("GH_HOST|ghe\\.com|enterprise server|wrong (host|account)|could not resolve host.*github|account .* does not have access",
      "copilot_account_host",
      "Copilot is signed in to a different GitHub account or host than expected.",
      c("Run `gh auth status` and check GH_HOST / GH_TOKEN / GITHUB_TOKEN environment variables; a stale token overrides the signed-in account.",
        "Sign in to the right account (`gh auth login`, or `gh auth switch`), then resume.")),
    p("copilot.*(error|fail|not found|sign|authentication|shim|1\\.0\\.93)|processx.*3\\.9|github.*(auth|login|token)|(http|status|code)[ :]*(401|403|429)|unauthorized|api[_ ]?key|invalid.*key|rate limit|quota exceeded",
      "llm_access",
      "Finn could not reach the AI model used by the Agent (sign-in, key, or rate limit).",
      c("For Copilot: run check_env.R and follow its Copilot fixes (CLI 1.0.93+, standalone copilot.exe on Windows, processx 3.9.0+, and `gh auth login` or GH_TOKEN), then resume.",
        "For Azure OpenAI or OpenAI: check the API key environment variable named in references/setup.md.",
        "For rate limits: wait a few minutes and resume; progress is kept.")),
    p("Permission denied|cannot open (file|the connection)|being used by another process|resource busy|EBUSY|EACCES|cannot rename",
      "file_locked",
      "A file was locked, often by OneDrive sync or an open Excel window.",
      c("Ask the user to close the input file in Excel and pause OneDrive sync, then resume.",
        "If this repeats, suggest moving the Finn folder to a local folder (setup_project.R --action=move_root).")),
    p("not in a standard unambiguous format|(invalid|unable|cannot|failed to)[^\\n]{0,40}(date|parse)|date_type",
      "date_format",
      "Dates in the data could not be read or did not match the date type.",
      c("Run profile_data.R and confirm date_type and the date column with the user.")),
    p("invalid type for input name|invalid value for input name|needs to be of type",
      "config_value",
      "A setting in the project config has the wrong type or an unsupported value.",
      c("Run validate_config.R to see which setting is wrong and the allowed values.",
        "Fix config.json with the user's OK, then resume; resumes reread the config."))
  )
}

#' Match an error message and log text against known patterns
#'
#' @param text Combined error and log text.
#' @return Matching pattern entry, or NULL.
diagnose_match <- function(text) {
  if (!nzchar(text)) {
    return(NULL)
  }
  for (p in diagnose_patterns()) {
    if (grepl(p$pattern, text, ignore.case = TRUE, perl = TRUE)) {
      return(p)
    }
  }
  NULL
}

#' Error-looking lines from a log tail
#'
#' @param lines Log lines.
#' @return Character vector of lines mentioning errors or warnings.
diagnose_error_lines <- function(lines) {
  lines[grepl("error|failed|warning|cannot|killed|stopped", lines, ignore.case = TRUE)]
}

#' Mask personal details in text bound for a support report
#'
#' Replaces the home folder with `~`, the user and computer names with
#' placeholders, OneDrive organization names with `<org>`, and e-mail
#' addresses with `<email>`, then masks secrets with [finn_redact()].
#'
#' @param x Character vector.
#' @param homes Home folder paths to replace.
#' @param names User and computer names to replace (shorter than 3 characters
#'   are ignored so ordinary words are not masked).
#' @return Masked character vector.
diagnose_mask <- function(x, homes = c(Sys.getenv("USERPROFILE"), Sys.getenv("HOME"), path.expand("~")),
                          names = c(Sys.getenv("USERNAME"), Sys.getenv("USER"), Sys.info()[["user"]],
                            Sys.info()[["nodename"]], Sys.getenv("COMPUTERNAME"))) {
  if (length(x) == 0) {
    return(character())
  }
  esc <- function(s) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", s)
  homes <- unique(homes[nzchar(homes)])
  homes <- unique(c(homes, gsub("\\\\", "/", homes), gsub("/", "\\\\", homes)))
  homes <- homes[order(-nchar(homes))]
  for (h in homes) x <- gsub(esc(h), "~", x, ignore.case = TRUE)
  x <- gsub("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", "<email>", x, perl = TRUE)
  x <- gsub("(OneDrive|SharePoint) - [^/\\\\\"]+", "\\1 - <org>", x, perl = TRUE)
  names <- unique(names[!is.na(names) & nchar(names) >= 3])
  for (n in names[order(-nchar(names))]) {
    x <- gsub(paste0("(?i)(?<![A-Za-z0-9])", esc(n), "(?![A-Za-z0-9])"), "<user>", x, perl = TRUE)
  }
  finn_redact(x)
}

#' Build the text of a support report
#'
#' Holds versions, the diagnosis, the project settings, and optionally the end
#' of the run log. It never reads the input data, so no data values appear.
#'
#' @param result Diagnosis result from [diagnose_run()].
#' @param paths Project paths.
#' @param include_log Whether to include the end of the run log.
#' @param log_lines Number of log lines to include.
#' @return Character vector of Markdown lines (already masked).
diagnose_report_lines <- function(result, paths, include_log = TRUE, log_lines = 60L) {
  d <- result$data %||% list()
  v <- tryCatch(finn_versions(), error = function(e) list())
  fence <- function(x) c("```", if (length(x)) x else "(none)", "```")
  kv <- function(k, val) if (!is.null(val) && length(val) && !all(is.na(val))) paste0("- ", k, ": ", paste(val, collapse = "; "))
  cfg <- tryCatch(finn_read_json(file.path(paths$dir, "config.json")), error = function(e) NULL)
  cfg_text <- if (is.null(cfg)) "(no config.json found)" else strsplit(finn_to_json(cfg, pretty = TRUE), "\n", fixed = TRUE)[[1]]
  log_text <- if (!include_log) {
    "(left out at the user's request)"
  } else if (!is.null(d$run_id)) {
    finn_tail(finn_run_paths(paths, d$run_id)$log, log_lines)
  }
  lines <- c(
    "# Finn skill support report",
    "",
    "> Review before sharing. This report has no data values, and home folders, user",
    "> and computer names, e-mail addresses, and keys are masked. Series names and",
    "> column names from your settings or log can still appear; edit them out if needed.",
    "",
    "## Versions",
    kv("Skill", finn_skill_version),
    kv("finnts", v$finnts_version %||% "not installed"),
    kv("R", v$r_version),
    kv("Operating system", paste(Sys.info()[["sysname"]], Sys.info()[["release"]])),
    kv("Platform", R.version$platform),
    kv("Created", finn_now()),
    "",
    "## Diagnosis",
    kv("Result", result$status),
    kv("Summary", result$message),
    kv("Run", d$run_id),
    kv("Mode", d$mode),
    kv("Status", d$status),
    kv("Stage", d$stage),
    kv("Category", d$category),
    kv("Error", d$error),
    kv("Parallel note", d$parallel),
    kv("Agent version", d$agent_version),
    kv("Resume count", d$resume_count),
    kv("Disk warning", d$disk_warning),
    "",
    "Suggested fixes:",
    paste0("- ", unlist(result$next_actions)),
    "",
    "## Settings (config.json)",
    fence(cfg_text),
    "",
    "## Run log (last lines)",
    fence(log_text)
  )
  diagnose_mask(lines)
}

#' Write a support report into the project's support folder
#'
#' @param result Diagnosis result from [diagnose_run()].
#' @param paths Project paths.
#' @param include_log Whether to include the end of the run log.
#' @param log_lines Number of log lines to include.
#' @return Path of the new report.
diagnose_write_report <- function(result, paths, include_log = TRUE, log_lines = 60L) {
  dir <- file.path(paths$dir, "support")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(dir, paste0("finn_support_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".md"))
  i <- 1L
  while (file.exists(file)) {
    file <- file.path(dir, paste0("finn_support_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", i, ".md"))
    i <- i + 1L
  }
  writeLines(diagnose_report_lines(result, paths, include_log, log_lines), file, useBytes = TRUE)
  finn_norm(file)
}

#' Diagnose one run of a project
#'
#' @param args Parsed arguments (`--run_id`, `--log_lines`).
#' @param paths Project paths.
#' @return Result list from [finn_result()].
diagnose_run <- function(args, paths) {
  run_id <- finn_arg(args, "run_id")
  n_lines <- as.integer(finn_arg(args, "log_lines", "60"))
  if (is.null(run_id)) {
    runs <- finn_list_runs(paths)
    if (length(runs) == 0) {
      return(finn_result(TRUE, "no_runs", "This project has no runs to diagnose."))
    }
    run_id <- runs[[1]]$run_id
  }
  run_paths <- finn_run_paths(paths, run_id)
  state <- tryCatch(finn_read_state(run_paths), error = function(e) e)
  if (inherits(state, "error")) {
    return(finn_result(TRUE, "diagnosed", "The run's state file is damaged or still syncing, so its status cannot be read.",
      list(run_id = run_id, category = "unreadable_state", error = conditionMessage(state), log_file = run_paths$log,
        log_tail = finn_redact(finn_tail(run_paths$log, n_lines))),
      c("Wait a minute for OneDrive to finish syncing, then run run_status.R again.",
        "If it stays unreadable, resume with run_forecast.R using the same --mode; Finn keeps finnts progress, and a damaged state file is set aside as state.json.corrupt-<time>.")))
  }
  if (is.null(state)) {
    return(finn_result(FALSE, "not_found", paste0("No run named ", run_id, " in this project.")))
  }
  status <- finn_effective_status(state)
  log_tail <- finn_redact(finn_tail(run_paths$log, n_lines))
  error_lines <- diagnose_error_lines(log_tail)
  resume <- sprintf("run_forecast.R --project=\"%s\" --mode=%s", paths$name, state$mode)
  conflicts <- tryCatch(finn_conflict_copies(paths), error = function(e) character())
  disk_note <- finn_disk_warning(paths$dir)
  base <- list(
    run_id = run_id, mode = state$mode, status = status, stage = state$stage,
    error = state$error, step_down = state$step_down, parallel = state$parallel$reason,
    request_id = state$request_id, agent_version = state$agent_version,
    resume_count = state$resume_count, log_file = run_paths$log, error_lines = utils::tail(error_lines, 15),
    conflict_copies = if (length(conflicts)) conflicts, disk_warning = disk_note
  )

  if (status %in% c("queued", "running")) {
    stale <- FALSE
    if (!is.null(state$updated_at)) {
      upd <- as.POSIXct(state$updated_at, format = "%Y-%m-%dT%H:%M:%S%z")
      log_age <- if (file.exists(run_paths$log)) as.numeric(difftime(Sys.time(), file.mtime(run_paths$log), units = "mins")) else NA
      stale <- !is.na(log_age) && log_age > 60 && !is.na(upd)
    }
    msg <- if (stale) {
      "The run is still running, but its log has not changed for over an hour. Large training stages can be quiet; it may also be stuck."
    } else {
      "The run is still in progress and looks healthy."
    }
    return(finn_result(TRUE, if (stale) "possibly_stuck" else "running", msg, base,
      c(if (stale) "Ask the user whether to keep waiting or cancel (run_cancel.R --confirm=true) and resume later.", "Check again with run_status.R.")))
  }
  if (identical(status, "completed")) {
    return(finn_result(TRUE, "completed", "The run finished successfully; nothing to fix.", base,
      "Summarize the results for the user."))
  }
  if (identical(status, "cancelled")) {
    return(finn_result(TRUE, "cancelled", "The run was cancelled on request. Saved progress was kept.", base,
      paste("Resume when the user is ready:", resume)))
  }

  text <- paste(c(state$error %||% "", log_tail), collapse = "\n")
  hit <- diagnose_match(paste(c(state$error %||% "", error_lines), collapse = "\n"))
  if (is.null(hit)) hit <- diagnose_match(text)

  if (identical(status, "interrupted") && is.null(state$error)) {
    hit <- hit %||% list(
      category = "interrupted",
      cause = "The run stopped without an error, usually because the computer slept, restarted, or the terminal closed.",
      fix = c("Ask the user to keep the computer awake and plugged in during long runs.")
    )
  }
  if (is.null(hit) && !is.null(disk_note)) {
    hit <- list(category = "disk_space", cause = disk_note,
      fix = c("Ask the user to free disk space (empty the recycle bin, remove old downloads) or move the Finn folder to a drive with more room."))
  }
  if (is.null(hit)) {
    hit <- list(
      category = "unknown",
      cause = "The failure did not match a known pattern.",
      fix = c("Read error_lines and log_file, explain the likely cause to the user in plain language, and propose a fix.",
        "Write a small free-form R check against the input data if the error mentions specific series or columns.")
    )
  }
  if (length(conflicts)) {
    hit$fix <- c(hit$fix, "OneDrive conflict copies of Finn files exist (see conflict_copies). Ask the user which computer is the main one; Finn only reads the original file names.")
  }
  base$category <- hit$category
  base$cause <- hit$cause
  finn_result(TRUE, "diagnosed", hit$cause, base,
    c(hit$fix, paste("After fixing, resume with", resume, "(keeps saved progress and history).")))
}

finn_main(function(args) {
  paths <- finn_project_from_args(args)
  result <- diagnose_run(args, paths)
  if (finn_bool(finn_arg(args, "report"))) {
    n_lines <- as.integer(finn_arg(args, "log_lines", "60"))
    report <- diagnose_write_report(result, paths, finn_bool(finn_arg(args, "include_log"), default = TRUE), n_lines)
    result$data$report_file <- report
    result$next_actions <- c(result$next_actions, list(
      paste("Show the user report_file and ask them to read it before sharing.",
        "To report a bug, open https://github.com/microsoft/finnts/issues/new?template=finn-skill.yml",
        "(the 'Finn skill problem' form) and attach or paste the report.")
    ))
  }
  result
})
