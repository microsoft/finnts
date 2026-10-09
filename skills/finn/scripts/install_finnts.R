# Install finnts and the helper packages the Finn skill needs.
# Usage: Rscript install_finnts.R --confirm=true [--ref=main] [--force=true]
#        [--source=github|cran] [--version=<cran version>] [--include_llm=true]
# finnts installs from GitHub by default (the skill and package ship together).
# The plan compares the installed finnts with the GitHub build, so users can get
# the latest finnts even when the skill itself is up to date. --force=true
# reinstalls an identical build (repair). Use --source=cran only when GitHub is
# blocked. Without --confirm=true it only reports what it would install.
# Refuses to run while a forecast is active.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Packages whose installed version is below what finnts declares
#'
#' `remotes::install_github(upgrade = "never")` installs missing packages but
#' leaves older installed ones alone, even when finnts needs a newer version.
#'
#' @param pkg Package whose Depends and Imports are checked.
#' @return Character vector of package names to install or upgrade.
finn_unmet_dependencies <- function(pkg = "finnts") {
  desc <- suppressWarnings(utils::packageDescription(pkg))
  if (!is.list(desc)) {
    return(character())
  }
  fields <- paste(desc$Depends %||% "", desc$Imports %||% "", sep = ",")
  entries <- trimws(strsplit(gsub("\\s+", " ", fields), ",", fixed = TRUE)[[1]])
  entries <- entries[nzchar(entries)]
  base_pkgs <- rownames(utils::installed.packages(priority = "base"))
  unmet <- character()
  for (e in entries) {
    name <- trimws(sub("\\(.*$", "", e))
    if (name %in% c("R", base_pkgs) || !nzchar(name)) next
    if (!requireNamespace(name, quietly = TRUE)) {
      unmet <- c(unmet, name)
      next
    }
    if (grepl(">=", e, fixed = TRUE)) {
      need <- trimws(sub("^.*>=\\s*([^)]+)\\).*$", "\\1", e))
      if (finn_version_newer(need, as.character(utils::packageVersion(name)))) unmet <- c(unmet, name)
    }
  }
  unique(unmet)
}

#' Look up the finnts build a GitHub ref points to
#'
#' Never fails: network problems are reported in `error` so installs still
#' work offline or behind strict proxies.
#'
#' @param ref Branch, tag, or commit.
#' @param fetch Function `(url, timeout)` returning text; injectable for tests.
#' @return List from [finn_remote_versions()] plus `error`.
finn_github_target <- function(ref, fetch = finn_http_get) {
  tryCatch(c(finn_remote_versions(ref, fetch), list(error = NULL)), error = function(e) {
    list(ref = ref, sha = NULL, skill_version = NULL, finnts_version = NULL, error = finn_redact(conditionMessage(e)))
  })
}

#' Compare the installed finnts with the GitHub build about to be installed
#'
#' @param installed List from [finn_installed_finnts()].
#' @param target List from [finn_github_target()].
#' @return List with `status` (`not_installed`, `same_commit`, `newer_available`,
#'   `new_commit`, `older_than_installed`, or `unknown`) and a plain `message`.
finn_compare_finnts <- function(installed, target) {
  have <- installed$version
  want <- target$finnts_version
  if (is.null(have)) {
    return(list(status = "not_installed", message = paste0("finnts is not installed; GitHub has ", want %||% "the latest build", ".")))
  }
  if (is.null(target$sha) || is.null(want)) {
    return(list(status = "unknown", message = paste0("finnts ", have, " is installed. Could not read the GitHub version, so it will be installed as-is.")))
  }
  if (identical(installed$source, "github") && identical(installed$sha, target$sha)) {
    return(list(status = "same_commit", message = paste0("finnts ", have, " from this exact GitHub commit is already installed.")))
  }
  if (finn_version_newer(want, have)) {
    return(list(status = "newer_available", message = paste0("GitHub has finnts ", want, " (installed: ", have, ").")))
  }
  if (finn_version_newer(have, want)) {
    return(list(status = "older_than_installed", message = paste0("GitHub has finnts ", want, ", which is OLDER than the installed ", have, ". Installing it is a downgrade.")))
  }
  list(status = "new_commit", message = paste0("GitHub has a newer build of finnts ", want, " (same version number, different code than installed)."))
}

finn_main(function(args) {
  source_type <- finn_arg(args, "source", "github")
  if (!source_type %in% c("github", "cran")) stop("--source must be github or cran.", call. = FALSE)
  ref <- finn_arg(args, "ref", "main")
  cran_version <- finn_arg(args, "version")
  helpers <- c("jsonlite", "ps", "processx", "digest", "readxl")
  if (finn_bool(finn_arg(args, "include_llm"), TRUE)) helpers <- c(helpers, "ellmer")
  missing_helpers <- helpers[!vapply(helpers, requireNamespace, logical(1), quietly = TRUE)]
  if (!"processx" %in% missing_helpers && utils::packageVersion("processx") < "3.9.0") {
    missing_helpers <- c(missing_helpers, "processx")
  }
  previous <- finn_installed_finnts()
  force <- finn_bool(finn_arg(args, "force"))
  target <- if (source_type == "github") finn_github_target(ref)
  comparison <- if (!is.null(target)) finn_compare_finnts(previous, target)
  # Install the exact commit that was compared, so a push in between cannot slip in.
  install_ref <- target$sha %||% ref
  plan <- list(
    source = source_type,
    finnts = if (source_type == "github") {
      paste0(finn_skill_repo(), "@", ref)
    } else {
      paste0("finnts ", cran_version %||% "(latest)", " from CRAN")
    },
    helper_packages = as.list(missing_helpers),
    library = .libPaths()[[1]],
    previous = previous,
    force = force
  )
  if (!is.null(target)) {
    plan$github <- list(ref = ref, sha = target$sha, finnts_version = target$finnts_version,
      skill_version = target$skill_version, error = target$error)
    plan$comparison <- comparison
  }
  skill_acts <- character()
  if (finn_version_newer(target$skill_version, finn_skill_version)) {
    skill_acts <- paste0("A newer Finn skill (", target$skill_version, ") ships with this finnts. Prefer update_skill.R --action=update --confirm=true, which updates both together.")
  }

  if (identical(comparison$status, "same_commit") && !force && length(missing_helpers) == 0) {
    return(finn_result(TRUE, "up_to_date", comparison$message, plan,
      c("Nothing to install. To reinstall the same build anyway (for example to repair a broken install), ask the user, then rerun with --force=true --confirm=true.", skill_acts)))
  }

  active <- finn_all_active_runs(finn_resolve_root(args))
  if (length(active) > 0) {
    plan$active_runs <- lapply(active, function(s) list(project = s$project, run_id = s$run_id))
    return(finn_result(
      FALSE, "blocked",
      "A forecast is running. Changing finnts now could break it.",
      plan,
      c("Wait for the run to finish (run_status.R) or cancel it with the user's OK (run_cancel.R), then install again.")
    ))
  }

  if (!finn_bool(finn_arg(args, "confirm"))) {
    msg <- "Installing finnts downloads packages from the internet and can take 10-30 minutes."
    if (!is.null(comparison)) msg <- paste(comparison$message, msg)
    acts <- "Ask the user to approve, then rerun with --confirm=true."
    if (identical(comparison$status, "older_than_installed")) {
      acts <- "This is a downgrade. Confirm the user really wants the older finnts before rerunning with --confirm=true."
    }
    return(finn_result(FALSE, "needs_confirmation", msg, plan, c(acts, skill_acts)))
  }

  repos <- getOption("repos")
  if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) repos <- c(CRAN = "https://cloud.r-project.org")
  lib <- .libPaths()[[1]]
  if (file.access(lib, 2) != 0) {
    lib <- Sys.getenv("R_LIBS_USER")
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(lib, .libPaths()))
    plan$library <- lib
  }

  if (length(missing_helpers) > 0) {
    finn_log("Installing helper packages: ", paste(missing_helpers, collapse = ", "))
    utils::install.packages(missing_helpers, lib = lib, repos = repos)
  }
  if (!requireNamespace("remotes", quietly = TRUE)) utils::install.packages("remotes", lib = lib, repos = repos)
  if (source_type == "github") {
    finn_log("Installing finnts from GitHub (", finn_skill_repo(), "@", install_ref, ")")
    remotes::install_github(paste0(finn_skill_repo(), "@", install_ref), lib = lib, upgrade = "never", dependencies = TRUE, force = force)
  } else if (!is.null(cran_version)) {
    finn_log("Installing finnts ", cran_version, " from CRAN")
    remotes::install_version("finnts", version = cran_version, lib = lib, repos = repos, upgrade = "never", dependencies = TRUE)
  } else {
    finn_log("Installing finnts from CRAN")
    utils::install.packages("finnts", lib = lib, repos = repos, dependencies = TRUE)
  }

  if (!requireNamespace("finnts", quietly = TRUE)) {
    return(finn_result(FALSE, "error", "finnts did not install. Check the log above for the first error.", plan,
      c("Share the first install error with the user. If GitHub is blocked, try install_finnts.R --source=cran --confirm=true.")))
  }
  unmet <- finn_unmet_dependencies()
  if (length(unmet) > 0) {
    finn_log("Upgrading packages below the versions finnts needs: ", paste(unmet, collapse = ", "))
    utils::install.packages(unmet, lib = lib, repos = repos)
    unmet <- finn_unmet_dependencies()
  }
  installed <- finn_installed_finnts()
  plan$finnts_version <- installed$version
  plan$installed <- installed
  plan$unmet_dependencies <- as.list(unmet)
  agent_fns <- c("chat_copilot", "set_agent_info_custom")
  plan$agent_mode_supported <- all(vapply(agent_fns, exists, logical(1), envir = asNamespace("finnts"), inherits = FALSE))
  tryCatch(finn_append_history(list(
    type = "finnts_install", source = source_type, ref = if (source_type == "github") ref else cran_version,
    sha = if (source_type == "github") installed$sha %||% target$sha, force = force,
    previous = previous, installed = installed
  )), error = function(e) finn_log("Could not record install history: ", conditionMessage(e)))

  acts <- c(
    "Run check_env.R in a new command to confirm.",
    "Ask the user to restart any open R sessions so they load the new finnts."
  )
  if (length(unmet) > 0) {
    acts <- c(acts, paste0("These packages are still older than finnts needs: ", paste(unmet, collapse = ", "), ". Close other R sessions and run install_finnts.R again."))
  }
  if (!plan$agent_mode_supported) {
    acts <- c(acts, "This finnts version lacks Agent support (chat_copilot, resumable Agent runs). Standard forecasts still work.")
  }
  if (!is.null(previous$version)) {
    acts <- c(acts, "If this version causes problems, run update_skill.R --action=rollback --package_only=true to return to the previous finnts.")
  }
  finn_result(TRUE, "ok", paste0("finnts ", installed$version, " is installed", if (!is.null(previous$version)) paste0(" (was ", previous$version, ")") else "", "."), plan, c(acts, skill_acts))
})
