# Check that R, finnts, and helper packages are ready on this machine.
# Usage: Rscript check_env.R [--copilot_command=<path to copilot>] [--check_updates=false]
# Read-only for packages. Occasionally checks GitHub for a newer skill (cached).

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

finn_main(function(args) {
  required <- c("finnts", "jsonlite", "ps", "processx", "digest", "readxl")
  optional <- c("ellmer")
  pkg_version <- function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
  }
  versions <- vapply(c(required, optional), pkg_version, character(1))
  missing <- required[is.na(versions[required])]

  has_custom <- !is.na(versions[["finnts"]]) &&
    exists("set_agent_info_custom", envir = asNamespace("finnts"), inherits = FALSE)
  copilot <- if (is.na(versions[["finnts"]])) NULL else finn_copilot_status(finn_arg(args, "copilot_command", "copilot"))

  r_ok <- getRversion() >= "4.1.0"
  data <- list(
    r_version = as.character(getRversion()),
    r_ok = r_ok,
    rscript = finn_rscript(),
    packages = as.list(versions),
    missing_packages = as.list(missing),
    agent_resume_supported = has_custom,
    copilot = copilot,
    resources = finn_system_resources(),
    os = Sys.info()[["sysname"]],
    skill_version = finn_skill_version
  )
  if (finn_update_check_enabled(args)) {
    data$update <- tryCatch(finn_update_check(), error = function(e) NULL)
  }
  update_action <- if (isTRUE(data$update$update_available)) {
    paste0(
      "A newer Finn skill is available (", data$update$local_skill_version, " -> ", data$update$remote_skill_version,
      if (isTRUE(data$update$finnts_update_available)) paste0(", with finnts ", data$update$remote_finnts_version) else "",
      "). Ask the user, then run update_skill.R --action=update --confirm=true. It can be undone with update_skill.R --action=rollback --confirm=true."
    )
  } else if (isTRUE(data$update$finnts_update_available)) {
    paste0(
      "The Finn skill is current, but a newer finnts is on GitHub (", data$update$local_finnts_version, " -> ",
      data$update$remote_finnts_version, "). Ask the user, then run install_finnts.R --confirm=true. It can be undone with update_skill.R --action=rollback --package_only=true --confirm=true."
    )
  }

  if (!r_ok) {
    return(finn_result(FALSE, "blocked", "R 4.1 or newer is required. Install a newer R, then run this check again.", data))
  }
  if (length(missing) > 0) {
    return(finn_result(
      FALSE, "needs_install",
      paste0("Missing packages: ", paste(missing, collapse = ", "), "."),
      data,
      c("Ask the user before installing, then run install_finnts.R --confirm=true.", update_action)
    ))
  }
  next_actions <- character()
  disk_note <- finn_disk_warning(finn_load_settings()$root %||% tempdir())
  if (!is.null(disk_note)) next_actions <- c(next_actions, disk_note)
  if (!has_custom) {
    next_actions <- c(next_actions, "This finnts version cannot resume Agent runs by request id. Offer install_finnts.R --confirm=true (installs the latest finnts from GitHub) for Agent mode.")
  }
  if (!isTRUE(copilot$ready)) {
    next_actions <- c(next_actions, paste(
      "Agent mode with the default Copilot provider is not ready (standard forecasts are fine):",
      paste(unlist(copilot$problems), collapse = " ")
    ))
  }
  next_actions <- c(next_actions, update_action)
  finn_result(TRUE, "ok", paste0("R ", data$r_version, " and finnts ", versions[["finnts"]], " are ready."), data, next_actions)
})
