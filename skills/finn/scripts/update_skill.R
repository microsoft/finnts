# Check for, install, or roll back Finn skill updates.
# Usage:
#   Rscript update_skill.R --action=check [--force=true] [--ref=main]
#   Rscript update_skill.R --action=update --confirm=true [--ref=main]
#   Rscript update_skill.R --action=rollback --confirm=true [--to=<backup id>] [--package_only=true]
#   Rscript update_skill.R --action=history
#   Rscript update_skill.R --action=settings --auto_check=true|false
# The skill and finnts ship together from GitHub. An update replaces the skill
# files and, when GitHub has a newer finnts version, reinstalls finnts from the
# same commit. The current skill is backed up first so rollback can restore it.
# Never deletes files. Refuses to change anything while a forecast is running.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Folder of the installed skill (the one that contains SKILL.md)
#'
#' @param args Parsed arguments; `--skill_dir` overrides the default.
#' @return Normalized path.
finn_skill_dir <- function(args = list()) {
  dir <- finn_arg(args, "skill_dir") %||% file.path(finn_script_dir(), "..")
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

#' Whether a folder sits inside a git checkout
#'
#' A skill used straight from a clone of the repository should be updated with
#' git, not by copying files over the working tree.
#'
#' @param dir Folder to test.
#' @return Logical.
finn_in_git_checkout <- function(dir) {
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(dir, ".git"))) {
      return(TRUE)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      return(FALSE)
    }
    dir <- parent
  }
}

#' Files that make up the skill at a GitHub commit
#'
#' @param sha Commit SHA.
#' @param fetch Function `(url, timeout)` returning text.
#' @param repo "owner/repo".
#' @return Character vector of paths relative to `skills/finn/`.
finn_remote_skill_files <- function(sha, fetch = finn_http_get, repo = finn_skill_repo()) {
  tree <- jsonlite::fromJSON(
    fetch(paste0("https://api.github.com/repos/", repo, "/git/trees/", sha, "?recursive=1"), 30),
    simplifyVector = FALSE
  )
  if (isTRUE(tree$truncated)) stop("GitHub returned a partial file list. Try again later.", call. = FALSE)
  paths <- vapply(Filter(function(x) identical(x$type, "blob"), tree$tree), function(x) x$path, character(1))
  prefix <- "skills/finn/"
  files <- substring(paths[startsWith(paths, prefix)], nchar(prefix) + 1)
  if (length(files) == 0) stop("The skill was not found at commit ", sha, ".", call. = FALSE)
  files
}

#' Download a file byte-for-byte
#'
#' @param url Source URL.
#' @param dest Destination path.
#' @return Invisible `dest`; errors on failure.
finn_download_file <- function(url, dest) {
  old <- options(timeout = 60)
  on.exit(options(old), add = TRUE)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  status <- suppressWarnings(utils::download.file(url, dest, quiet = TRUE, mode = "wb", headers = c("User-Agent" = "finn-skill")))
  if (!identical(as.integer(status), 0L) || !file.exists(dest)) stop("Could not download ", url, call. = FALSE)
  invisible(dest)
}

#' Download the skill at a commit into a staging folder
#'
#' @param sha Commit SHA.
#' @param files Relative paths from [finn_remote_skill_files()].
#' @param download Function `(url, dest)`; injectable for tests.
#' @param repo "owner/repo".
#' @return Staging folder path.
finn_stage_skill <- function(sha, files, download = finn_download_file, repo = finn_skill_repo()) {
  stage <- file.path(tempfile("finn-skill-stage-"), "skill")
  for (f in files) {
    download(paste0("https://raw.githubusercontent.com/", repo, "/", sha, "/skills/finn/", f), file.path(stage, f))
  }
  stage
}

#' Check that a staged skill is complete and its scripts parse
#'
#' @param dir Staged skill folder.
#' @return Invisible TRUE; errors with the first problem found.
finn_validate_skill_dir <- function(dir) {
  for (f in c("SKILL.md", "scripts/finn_common.R", "scripts/update_skill.R")) {
    if (!file.exists(file.path(dir, f))) stop("The downloaded skill is missing ", f, ".", call. = FALSE)
  }
  scripts <- list.files(dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  for (s in scripts) {
    tryCatch(parse(s, keep.source = FALSE), error = function(e) {
      stop("The downloaded file ", basename(s), " is damaged: ", conditionMessage(e), call. = FALSE)
    })
  }
  if (is.null(finn_parse_skill_version(paste(readLines(file.path(dir, "scripts/finn_common.R"), warn = FALSE), collapse = "\n")))) {
    stop("The downloaded skill has no version number.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Copy every file in one folder over another
#'
#' Overwrites matching files and creates folders as needed. Never deletes.
#'
#' @param from Source folder.
#' @param to Destination folder.
#' @return Character vector of copied relative paths.
finn_copy_tree <- function(from, to) {
  files <- list.files(from, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  for (f in files) {
    dir.create(dirname(file.path(to, f)), recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(file.copy(file.path(from, f), file.path(to, f), overwrite = TRUE, copy.date = TRUE))) {
      stop("Could not write ", f, ". Close any program using the skill folder and try again.", call. = FALSE)
    }
  }
  files
}

#' Folder that holds skill backups
#'
#' @return Path inside the settings folder.
finn_backups_dir <- function() file.path(finn_settings_dir(), "skill_backups")

#' Back up the installed skill before changing it
#'
#' @param skill_dir Installed skill folder.
#' @param reason Short reason recorded with the backup.
#' @return Backup metadata list, including `id` and `path`.
finn_backup_skill <- function(skill_dir, reason) {
  version <- finn_parse_skill_version(paste(readLines(file.path(skill_dir, "scripts", "finn_common.R"), warn = FALSE), collapse = "\n")) %||% "unknown"
  id <- paste0(finn_stamp(), "_skill-", version)
  path <- file.path(finn_backups_dir(), id)
  while (dir.exists(path)) {
    id <- paste0(id, "x")
    path <- file.path(finn_backups_dir(), id)
  }
  finn_copy_tree(skill_dir, file.path(path, "skill"))
  meta <- list(id = id, skill_version = version, finnts = finn_installed_finnts(), created_at = finn_now(), reason = reason)
  finn_write_json(meta, file.path(path, "backup.json"))
  c(meta, list(path = path))
}

#' Saved skill backups, newest first
#'
#' @return List of backup metadata lists.
finn_list_backups <- function() {
  dirs <- list.dirs(finn_backups_dir(), recursive = FALSE)
  metas <- lapply(dirs, function(d) {
    m <- tryCatch(finn_read_json(file.path(d, "backup.json")), error = function(e) NULL)
    if (!is.null(m) && dir.exists(file.path(d, "skill"))) c(m, list(path = d))
  })
  metas <- Filter(Negate(is.null), metas)
  metas[order(vapply(metas, function(m) m$id, character(1)), decreasing = TRUE)]
}

#' Run another skill script in a fresh R process and read its JSON result
#'
#' @param script Full path to the script.
#' @param script_args Character vector of `--key=value` arguments.
#' @return Parsed result list; a synthetic error result when none is printed.
finn_run_script <- function(script, script_args) {
  out <- suppressWarnings(system2(finn_rscript(), shQuote(c(script, script_args)), stdout = TRUE, stderr = ""))
  json <- rev(grep("^\\{", out, value = TRUE))
  if (length(json) == 0) {
    return(list(ok = FALSE, status = "error", message = paste("No result from", basename(script))))
  }
  jsonlite::fromJSON(json[[1]], simplifyVector = FALSE)
}

#' finnts version seen by a brand-new R process
#'
#' @return Version string, or NULL when finnts does not load.
finn_fresh_finnts_version <- function() {
  out <- suppressWarnings(system2(finn_rscript(), c("-e", shQuote("suppressPackageStartupMessages(library(finnts)); cat(as.character(packageVersion('finnts')))")),
    stdout = TRUE, stderr = FALSE
  ))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    return(NULL)
  }
  v <- trimws(utils::tail(out, 1))
  if (length(v) == 0 || !nzchar(v)) NULL else v
}

#' Installer arguments that reproduce a recorded finnts install
#'
#' @param finnts List from [finn_installed_finnts()].
#' @return Character vector of arguments, or NULL when it cannot be reproduced.
finn_reinstall_args <- function(finnts) {
  if (is.null(finnts$version)) {
    return(NULL)
  }
  if (identical(finnts$source, "github") && !is.null(finnts$sha)) {
    return(c("--source=github", paste0("--ref=", finnts$sha)))
  }
  if (identical(finnts$source, "cran")) {
    return(c("--source=cran", paste0("--version=", finnts$version)))
  }
  NULL
}

#' Whether two recorded finnts installs are the same build
#'
#' @param a,b Lists from [finn_installed_finnts()].
#' @return Logical.
finn_same_finnts <- function(a, b) {
  identical(a$version, b$version) && (is.null(a$sha) || is.null(b$sha) || identical(a$sha, b$sha))
}

#' Default finnts installer used by update and rollback
#'
#' @param skill_dir Skill folder whose `install_finnts.R` is used.
#' @param install_args Arguments such as `--source=github --ref=<sha>`.
#' @return Parsed installer result.
finn_default_installer <- function(skill_dir, install_args) {
  finn_run_script(file.path(skill_dir, "scripts", "install_finnts.R"), c(install_args, "--confirm=true"))
}

#' Install the newest skill (and finnts when its version is newer)
#'
#' @param args Parsed arguments.
#' @param fetch,download Network functions; injectable for tests.
#' @param installer Function `(skill_dir, install_args)` returning a result.
#' @param verify Function returning the finnts version a new process loads.
#' @return Result list from [finn_result()].
finn_do_update <- function(args, fetch = finn_http_get, download = finn_download_file,
                           installer = finn_default_installer, verify = finn_fresh_finnts_version) {
  skill_dir <- finn_skill_dir(args)
  force <- finn_bool(finn_arg(args, "force"))
  ref <- finn_arg(args, "ref", "main")
  remote <- finn_remote_versions(ref, fetch)
  local_finnts <- finn_installed_finnts()
  data <- list(
    skill_dir = skill_dir, ref = ref, sha = remote$sha,
    local_skill_version = finn_skill_version, remote_skill_version = remote$skill_version,
    local_finnts_version = local_finnts$version, remote_finnts_version = remote$finnts_version
  )
  skill_newer <- finn_version_newer(remote$skill_version, finn_skill_version)
  finnts_newer <- is.null(local_finnts$version) || finn_version_newer(remote$finnts_version, local_finnts$version)
  if (!skill_newer && !force) {
    return(finn_result(TRUE, "up_to_date", paste0("The Finn skill ", finn_skill_version, " is already the latest."), data,
      if (finnts_newer && !is.null(remote$finnts_version)) "A newer finnts exists without a skill change. Offer install_finnts.R --confirm=true." else character()))
  }
  if (!finn_bool(finn_arg(args, "confirm"))) {
    return(finn_result(FALSE, "needs_confirmation",
      paste0("Update the Finn skill ", finn_skill_version, " -> ", remote$skill_version %||% "?",
        if (finnts_newer) paste0(" and finnts ", local_finnts$version %||% "(none)", " -> ", remote$finnts_version %||% "?") else "", "?"),
      data, "Ask the user to approve, then rerun with --confirm=true."))
  }

  files <- finn_remote_skill_files(remote$sha, fetch)
  stage <- finn_stage_skill(remote$sha, files, download)
  finn_validate_skill_dir(stage)
  backup <- finn_backup_skill(skill_dir, paste("before update to", remote$skill_version %||% remote$sha))
  data$backup_id <- backup$id
  existing <- list.files(skill_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  data$stale_files <- as.list(setdiff(existing, files))

  restore <- function(why) {
    finn_log("Update failed (", why, "). Restoring the previous skill from ", backup$id)
    finn_copy_tree(file.path(backup$path, "skill"), skill_dir)
    now <- finn_installed_finnts()
    if (!finn_same_finnts(now, local_finnts) && !is.null(finn_reinstall_args(local_finnts))) {
      installer(skill_dir, finn_reinstall_args(local_finnts))
    }
    finn_append_history(list(type = "skill_update_failed", from = finn_skill_version, to = remote$skill_version, sha = remote$sha, backup_id = backup$id, error = why))
    data$restored <- TRUE
    finn_result(FALSE, "error", paste("The update failed and the previous version was restored:", why), data,
      "Tell the user nothing changed. Share the error; they can retry later or report it on GitHub.")
  }

  copied <- tryCatch(finn_copy_tree(stage, skill_dir), error = function(e) e)
  if (inherits(copied, "error")) {
    return(restore(conditionMessage(copied)))
  }
  data$finnts_updated <- FALSE
  if (finnts_newer) {
    res <- tryCatch(installer(skill_dir, c("--source=github", paste0("--ref=", remote$sha))), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    if (!isTRUE(res$ok)) {
      return(restore(paste("finnts install failed:", res$message %||% "unknown error")))
    }
    loaded <- verify()
    if (is.null(loaded) || !identical(loaded, remote$finnts_version)) {
      return(restore(paste0("a new R session loads finnts ", loaded %||% "(none)", ", expected ", remote$finnts_version)))
    }
    data$finnts_updated <- TRUE
    data$finnts_version <- loaded
  }
  finn_append_history(list(
    type = "skill_update", from = finn_skill_version, to = remote$skill_version, sha = remote$sha,
    backup_id = backup$id, previous_finnts = local_finnts, finnts = finn_installed_finnts()
  ))
  acts <- c(
    "Reload the skill: start a new chat so the agent reads the updated SKILL.md.",
    "Run check_env.R to confirm everything is ready.",
    paste0("If anything breaks, run update_skill.R --action=rollback --confirm=true (backup ", backup$id, ").")
  )
  if (length(data$stale_files) > 0) {
    acts <- c(acts, "Some old skill files are no longer used (stale_files). They are harmless; the user may remove them.")
  }
  finn_result(TRUE, "updated", paste0("The Finn skill is now ", remote$skill_version,
    if (isTRUE(data$finnts_updated)) paste0(" with finnts ", data$finnts_version) else "", "."), data, acts)
}

#' Restore a previous skill and finnts version
#'
#' @param args Parsed arguments (`--to`, `--package_only`, `--confirm`).
#' @param installer Function `(skill_dir, install_args)` returning a result.
#' @param verify Function returning the finnts version a new process loads.
#' @return Result list from [finn_result()].
finn_do_rollback <- function(args, installer = finn_default_installer, verify = finn_fresh_finnts_version) {
  skill_dir <- finn_skill_dir(args)
  package_only <- finn_bool(finn_arg(args, "package_only"))
  current <- finn_installed_finnts()
  backups <- finn_list_backups()
  to <- finn_arg(args, "to")
  data <- list(skill_dir = skill_dir, current_skill_version = finn_skill_version, current_finnts = current)

  if (package_only) {
    history <- rev(tryCatch(finn_read_json(file.path(finn_settings_dir(), "update_history.json"), list()), error = function(e) list()))
    target <- NULL
    for (h in history) {
      prev <- h$previous %||% h$previous_finnts
      if (!is.null(prev$version) && !finn_same_finnts(prev, current)) {
        target <- prev
        break
      }
    }
    if (!is.null(to)) {
      b <- Filter(function(m) identical(m$id, to), backups)
      if (length(b) == 0) stop("No backup named ", to, ". Use --action=history to list backups.", call. = FALSE)
      target <- b[[1]]$finnts
    }
  } else {
    if (length(backups) == 0) {
      return(finn_result(FALSE, "nothing_to_restore", "There is no saved skill backup yet.", data))
    }
    chosen <- if (is.null(to)) backups[1] else Filter(function(m) identical(m$id, to), backups)
    if (length(chosen) == 0) stop("No backup named ", to, ". Use --action=history to list backups.", call. = FALSE)
    chosen <- chosen[[1]]
    target <- chosen$finnts
    data$backup_id <- chosen$id
    data$restore_skill_version <- chosen$skill_version
  }
  data$restore_finnts <- target
  reinstall <- !is.null(target$version) && !finn_same_finnts(target, current)
  if (package_only && !reinstall) {
    return(finn_result(FALSE, "nothing_to_restore", "No earlier finnts install is recorded.", data))
  }
  if (!finn_bool(finn_arg(args, "confirm"))) {
    msg <- paste0(
      if (!package_only) paste0("Restore the Finn skill ", data$restore_skill_version, " (backup ", data$backup_id, ")") else "Restore",
      if (reinstall) paste0(if (package_only) " finnts " else " and finnts ", target$version) else "", "?"
    )
    return(finn_result(FALSE, "needs_confirmation", msg, data, "Ask the user to approve, then rerun with --confirm=true."))
  }

  if (!package_only) {
    safety <- finn_backup_skill(skill_dir, paste("before rollback to", data$restore_skill_version))
    data$safety_backup_id <- safety$id
    finn_copy_tree(file.path(chosen$path, "skill"), skill_dir)
  }
  acts <- c("Start a new chat so the agent reloads the skill.", "Run check_env.R to confirm everything is ready.")
  if (reinstall) {
    install_args <- finn_reinstall_args(target)
    if (is.null(install_args)) {
      acts <- c(acts, paste0("finnts ", target$version, " was not installed from GitHub or CRAN, so it cannot be restored automatically. Ask the user how it was installed."))
    } else {
      res <- tryCatch(installer(skill_dir, install_args), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      loaded <- if (isTRUE(res$ok)) verify()
      data$finnts_restored <- identical(loaded, target$version)
      if (!data$finnts_restored) {
        acts <- c(acts, paste0("finnts ", target$version, " could not be reinstalled (", res$message %||% paste("loaded", loaded %||% "none"), "). Check the network, then rerun the rollback."))
      }
    }
  }
  finn_append_history(list(type = "rollback", package_only = package_only, backup_id = data$backup_id, previous_finnts = current, finnts = finn_installed_finnts()))
  ok <- !reinstall || isTRUE(data$finnts_restored)
  finn_result(ok, if (ok) "rolled_back" else "partial",
    paste0("Rolled back", if (!package_only) paste0(" the skill to ", data$restore_skill_version) else "",
      if (isTRUE(data$finnts_restored)) paste0(if (package_only) " finnts to " else " and finnts to ", target$version) else "", "."),
    data, acts)
}

#' Relaunch this script from a temporary copy
#'
#' Updating or rolling back overwrites update_skill.R and finn_common.R, which
#' is unsafe while R is still reading them. The copy runs instead and its
#' output and exit status pass straight through.
#'
#' @param args Parsed arguments.
#' @return Never returns; quits with the child's exit status.
finn_relaunch_from_temp <- function(args) {
  tmp <- tempfile("finn-update-")
  dir.create(tmp)
  here <- finn_script_dir()
  for (f in c("update_skill.R", "finn_common.R")) file.copy(file.path(here, f), file.path(tmp, f))
  skill_dir <- finn_skill_dir(args)
  args <- args[setdiff(names(args), c("skill_dir", "from_temp"))]
  passthrough <- if (length(args) > 0) paste0("--", names(args), "=", unlist(args)) else character()
  status <- system2(finn_rscript(), shQuote(c(
    file.path(tmp, "update_skill.R"), passthrough,
    "--from_temp=true", paste0("--skill_dir=", skill_dir)
  )))
  quit(save = "no", status = status)
}

if (sys.nframe() == 0L) {
  local({
    args <- finn_parse_args()
    action <- args$action %||% "check"
    if (action %in% c("update", "rollback") && !finn_bool(args$from_temp)) finn_relaunch_from_temp(args)
  })

  finn_main(function(args) {
    action <- finn_arg(args, "action", "check")
    if (action == "check") {
      res <- finn_update_check(force = finn_bool(finn_arg(args, "force"), TRUE), ref = finn_arg(args, "ref", "main"))
      if (!is.null(res$error)) {
        return(finn_result(FALSE, "error", paste("Could not reach GitHub:", res$error), res,
          "Check the internet connection or proxy. Updates are optional; Finn keeps working."))
      }
      finnts_only <- !res$update_available && isTRUE(res$finnts_update_available)
      acts <- if (res$update_available) {
        "Ask the user whether to update, then run update_skill.R --action=update --confirm=true."
      } else if (finnts_only) {
        "Ask the user whether to install it, then run install_finnts.R --confirm=true. It can be undone with update_skill.R --action=rollback --package_only=true --confirm=true."
      } else {
        character()
      }
      msg <- if (res$update_available) {
        paste0("Finn skill ", res$remote_skill_version, " is available (installed: ", res$local_skill_version, ").",
          if (res$finnts_update_available) paste0(" It includes finnts ", res$remote_finnts_version, ".") else "")
      } else if (finnts_only) {
        paste0("The Finn skill ", res$local_skill_version, " is up to date, but a newer finnts ", res$remote_finnts_version,
          " is on GitHub (installed: ", res$local_finnts_version, ").")
      } else {
        paste0("The Finn skill ", res$local_skill_version, " is up to date.")
      }
      status <- if (res$update_available) "update_available" else if (finnts_only) "finnts_update_available" else "up_to_date"
      return(finn_result(TRUE, status, msg, res, acts))
    }
    if (action == "history") {
      backups <- lapply(finn_list_backups(), function(m) m[c("id", "skill_version", "finnts", "created_at", "reason")])
      history <- tryCatch(finn_read_json(file.path(finn_settings_dir(), "update_history.json"), list()), error = function(e) list())
      return(finn_result(TRUE, "ok", paste(length(backups), "skill backup(s) saved."),
        list(skill_version = finn_skill_version, finnts = finn_installed_finnts(), backups = backups, history = history)))
    }
    if (action == "settings") {
      value <- finn_arg(args, "auto_check")
      if (is.null(value)) stop("Pass --auto_check=true or --auto_check=false.", call. = FALSE)
      settings <- finn_load_settings()
      settings$auto_update_check <- finn_bool(value)
      finn_save_settings(settings)
      return(finn_result(TRUE, "ok", paste("Automatic update checks are", if (settings$auto_update_check) "on." else "off."),
        list(auto_update_check = settings$auto_update_check)))
    }
    if (!action %in% c("update", "rollback")) stop("Unknown --action. Use check, update, rollback, history, or settings.", call. = FALSE)

    skill_dir <- finn_skill_dir(args)
    if (finn_in_git_checkout(skill_dir) && !finn_bool(finn_arg(args, "force"))) {
      return(finn_result(FALSE, "blocked", "This skill runs from a git clone of the repository.", list(skill_dir = skill_dir),
        "Update with git pull (or git checkout for a rollback) in that clone, then run install_finnts.R --confirm=true."))
    }
    active <- finn_all_active_runs(finn_resolve_root(args))
    if (length(active) > 0) {
      return(finn_result(FALSE, "blocked", "A forecast is running. Changing Finn now could break it.",
        list(active_runs = lapply(active, function(s) list(project = s$project, run_id = s$run_id))),
        "Wait for the run to finish (run_status.R) or cancel it with the user's OK (run_cancel.R), then try again."))
    }
    if (action == "update") finn_do_update(args) else finn_do_rollback(args)
  })
}
