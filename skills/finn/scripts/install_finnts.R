# Install finnts and the helper packages the Finn skill needs.
# Usage: Rscript install_finnts.R --confirm=true [--ref=main] [--force=true]
#        [--source=github|cran] [--version=<cran version>] [--include_llm=true]
# finnts always installs from GitHub (the skill and package ship together);
# dependencies install from CRAN. The plan compares the installed finnts with the
# GitHub build, so users can get the latest finnts even when the skill itself is
# up to date. --force=true reinstalls an identical build (repair). The cran source
# exists only so rollback can restore a recorded CRAN build; do not offer it as an
# install option. Without --confirm=true it only reports what it would install.
# Refuses to run while a forecast is active.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Installed packages R would load, one row each
#'
#' Reads DESCRIPTION metadata only, so no package is loaded: on Windows R
#' cannot replace a package whose DLL the installing session has loaded. The
#' cache is bypassed because packages change during installation.
#'
#' @return Character matrix with `Package`, `Version`, `Imports`, `Depends`, and
#'   `Priority` columns; the first library in `.libPaths()` wins for duplicates.
finn_installed_db <- function() {
  db <- utils::installed.packages(noCache = TRUE)
  db[!duplicated(db[, "Package"]), c("Package", "Version", "Imports", "Depends", "Priority"), drop = FALSE]
}

#' Split a Depends or Imports field into requirements
#'
#' @param field DESCRIPTION field text, e.g. `"dials (>= 1.4.3),\n hardhat"`.
#' @return List of `name`, `op` (NULL when unversioned), and `version`.
finn_parse_requirements <- function(field) {
  entries <- trimws(strsplit(gsub("\\s+", " ", paste(field, collapse = ",")), ",", fixed = TRUE)[[1]])
  lapply(entries[nzchar(entries)], function(e) {
    req <- regmatches(e, regexec("\\(\\s*([<>=]+)\\s*([^) ]+)\\s*\\)", e))[[1]]
    list(name = trimws(sub("\\(.*$", "", e)), op = if (length(req) == 3) req[[2]], version = if (length(req) == 3) req[[3]])
  })
}

#' Whether an installed version meets a minimum
#'
#' Only minimums (`>=`, `>`) are checked, since an upgrade can only fix those;
#' unparsable versions count as met so the installer never loops on them.
#'
#' @param have Installed version.
#' @param op Requirement operator, or NULL.
#' @param need Required version.
#' @return Logical.
finn_requirement_met <- function(have, op, need) {
  if (is.null(op) || !op %in% c(">=", ">")) {
    return(TRUE)
  }
  h <- tryCatch(package_version(have), error = function(e) NULL)
  n <- tryCatch(package_version(need), error = function(e) NULL)
  if (is.null(h) || is.null(n)) {
    return(TRUE)
  }
  if (identical(op, ">=")) h >= n else h > n
}

#' Packages in finnts' dependency tree that are missing or too old
#'
#' `remotes::install_github(upgrade = "never")` installs missing packages but
#' leaves older installed ones alone, and a newly installed package can need a
#' newer version of something already installed (tune 2.1.0 needs dials 1.4.3
#' while finnts asks only for dials 1.4.0). So every installed package
#' reachable from `pkg` through Depends and Imports is checked against the
#' minimums its dependents declare. Base packages and R itself are skipped.
#'
#' @param pkg Root package.
#' @param db Table from [finn_installed_db()].
#' @return Character vector of package names to install or upgrade, in the
#'   order found; empty when `pkg` is not installed.
finn_unmet_dependencies <- function(pkg = "finnts", db = finn_installed_db()) {
  base_pkgs <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
    "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")
  db <- db[!duplicated(db[, "Package"]), , drop = FALSE]
  rownames(db) <- db[, "Package"]
  base_pkgs <- union(base_pkgs, db[which(db[, "Priority"] == "base"), "Package"])
  if (!pkg %in% rownames(db)) {
    return(character())
  }
  queue <- pkg
  seen <- pkg
  unmet <- character()
  while (length(queue) > 0) {
    fields <- db[queue[[1]], c("Depends", "Imports")]
    queue <- queue[-1]
    for (req in finn_parse_requirements(fields[!is.na(fields)])) {
      if (!nzchar(req$name) || req$name %in% c("R", base_pkgs)) next
      if (!req$name %in% rownames(db)) {
        unmet <- c(unmet, req$name)
        next
      }
      if (!finn_requirement_met(db[req$name, "Version"], req$op, req$version)) unmet <- c(unmet, req$name)
      if (!req$name %in% seen) {
        seen <- c(seen, req$name)
        queue <- c(queue, req$name)
      }
    }
  }
  unique(unmet)
}

#' Helper packages the skill needs that are missing or too old
#'
#' Checks installed metadata without loading anything, so the helpers (and
#' the finnts dependencies they share) can still be upgraded on Windows.
#'
#' @param helpers Package names.
#' @return Character vector of packages to install.
finn_missing_helpers <- function(helpers) {
  missing <- helpers[!vapply(helpers, function(h) nzchar(system.file(package = h)), logical(1))]
  if ("processx" %in% helpers && !"processx" %in% missing && utils::packageVersion("processx") < "3.9.0") {
    missing <- c(missing, "processx")
  }
  missing
}

#' Validate finnts installation postconditions
#'
#' @param source Requested package source.
#' @param expected_version Expected package version, or NULL when unavailable.
#' @param expected_sha Expected GitHub commit, or NULL when unavailable.
#' @param installed Installed package metadata.
#' @param unmet Declared dependencies still below their required versions.
#' @param missing_capabilities Required forecast APIs that are unavailable.
#' @return NULL on success, otherwise a user-facing error message.
finn_install_validation_error <- function(source, expected_version, expected_sha, installed,
                                          unmet, missing_capabilities) {
  if (is.null(installed$version) || !nzchar(installed$version)) {
    return("finnts is not available after installation.")
  }
  if (!is.null(expected_version) && !identical(installed$version, expected_version)) {
    return(paste0("Installed finnts ", installed$version, " does not match expected version ", expected_version, "."))
  }
  if (identical(source, "github")) {
    if (!identical(installed$source, "github") || is.null(installed$sha) || !nzchar(installed$sha)) {
      return("The installed GitHub finnts build has no verifiable commit identity.")
    }
    if (!is.null(expected_sha) && !identical(installed$sha, expected_sha)) {
      return("The installed finnts commit does not match the commit selected for installation.")
    }
  }
  if (length(unmet) > 0) {
    return(paste0("finnts still has unmet dependencies: ", paste(unmet, collapse = ", "), "."))
  }
  if (length(missing_capabilities) > 0) {
    return(paste0("Installed finnts is missing required forecast capabilities: ",
      paste(missing_capabilities, collapse = ", "), "."))
  }
  NULL
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
  software <- finn_software_guard(args)
  if (!is.null(software$result)) return(software$result)
  if (software$owned) on.exit(finn_machine_lock_release(software$token), add = TRUE)
  source_type <- finn_arg(args, "source", "github")
  if (!source_type %in% c("github", "cran")) stop("--source must be github or cran.", call. = FALSE)
  ref <- finn_arg(args, "ref", "main")
  cran_version <- finn_arg(args, "version")
  helpers <- c("jsonlite", "ps", "processx", "digest", "readxl")
  if (finn_bool(finn_arg(args, "include_llm"), TRUE)) helpers <- c(helpers, "ellmer")
  missing_helpers <- finn_missing_helpers(helpers)
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

  # The result returned when finnts is absent or won't load after installing.
  install_failed <- function() {
    finn_result(FALSE, "error", "finnts did not install. Check the log above for the first error.", plan,
      c("Share the first install error with the user. If GitHub is blocked, ask the user or their IT team to allow github.com and api.github.com (see references/setup.md), then retry."))
  }
  # Check and upgrade before loading finnts: R on Windows won't replace a
  # package this session has loaded.
  if (!nzchar(system.file(package = "finnts"))) {
    return(install_failed())
  }
  unmet <- finn_unmet_dependencies()
  for (round in seq_len(3)) {
    if (length(unmet) == 0) break
    finn_log("Upgrading packages below the versions finnts or its dependencies need: ", paste(unmet, collapse = ", "))
    utils::install.packages(unmet, lib = lib, repos = repos)
    previous_unmet <- unmet
    unmet <- finn_unmet_dependencies()
    # CRAN has nothing newer for these, so another round can't help.
    if (setequal(unmet, previous_unmet)) break
  }
  if (!requireNamespace("finnts", quietly = TRUE)) {
    return(install_failed())
  }
  installed <- finn_installed_finnts()
  plan$finnts_version <- installed$version
  plan$installed <- installed
  plan$unmet_dependencies <- as.list(unmet)
  capabilities <- finn_check_finnts_capabilities(asNamespace("finnts"))
  plan$missing_core_capabilities <- as.list(capabilities$missing_core)
  plan$missing_agent_capabilities <- as.list(capabilities$missing_agent)
  plan$agent_mode_supported <- length(capabilities$missing_agent) == 0L
  postcondition_error <- finn_install_validation_error(
    source = source_type,
    expected_version = if (source_type == "github") target$finnts_version else cran_version,
    expected_sha = if (source_type == "github") target$sha else NULL,
    installed = installed,
    unmet = unmet,
    missing_capabilities = capabilities$missing_core
  )
  if (!is.null(postcondition_error)) {
    return(finn_result(FALSE, "error", postcondition_error, plan,
      c("Check the first installation error, then run check_env.R before forecasting.")))
  }
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
