# Manage the Finn storage folder and projects.
# Usage:
#   Rscript setup_project.R --action=status
#   Rscript setup_project.R --action=set_root --root=<folder> --confirm=true
#   Rscript setup_project.R --action=create --project=<name> --data=<file>
#       [--sheet=<sheet>] [--root=<folder>] [--replace_data=true]
#   Rscript setup_project.R --action=list
#   Rscript setup_project.R --action=disk_usage --project=<name>
#   Rscript setup_project.R --action=move_root --to=<folder> --confirm=true
#   Rscript setup_project.R --action=dismiss_move_suggestion
# Nothing here deletes files. move_root copies; the old folder stays until
# the user removes it.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' List project folders under a root
#'
#' @param root Storage root.
#' @return List of project summaries.
setup_list_projects <- function(root) {
  if (is.null(root) || !dir.exists(root)) {
    return(list())
  }
  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  dirs <- dirs[file.exists(file.path(dirs, "project.json"))]
  lapply(dirs, function(d) {
    p <- finn_read_json(file.path(d, "project.json"), list())
    paths <- finn_project_paths(root, basename(d))
    runs <- finn_list_runs(paths)
    list(
      name = basename(d),
      created_at = p$created_at,
      data_file = p$data_file,
      runs = length(runs),
      latest_run = if (length(runs)) runs[[1]]$run_id else NULL,
      latest_status = if (length(runs)) runs[[1]]$effective_status else NULL
    )
  })
}

#' Report storage settings and suggestions
#'
#' @param args Parsed arguments.
#' @return Finn result.
setup_status <- function(args) {
  settings <- finn_load_settings()
  suggested <- finn_detect_default_root()
  root <- finn_resolve_root(args)
  data <- list(
    root = root,
    root_confirmed = isTRUE(settings$root_confirmed),
    root_is_onedrive = finn_is_onedrive(root %||% ""),
    suggested_root = suggested$path,
    suggested_source = suggested$source,
    settings_file = file.path(finn_settings_dir(), "settings.json"),
    large_onedrive_runs = settings$large_onedrive_runs,
    projects = setup_list_projects(root)
  )
  if (is.null(root) || !isTRUE(settings$root_confirmed)) {
    return(finn_result(TRUE, "needs_root",
      paste0("No Finn folder is set yet. Suggested: ", suggested$path, "."), data,
      c(paste0(
        "Ask the user to accept the suggested folder or name another, then run setup_project.R --action=set_root --root=\"<folder>\" --confirm=true."
      ))
    ))
  }
  next_actions <- character()
  if (data$root_is_onedrive && settings$large_onedrive_runs >= 3 && !isTRUE(settings$move_suggestion_dismissed)) {
    next_actions <- c(next_actions, "The user has run several large forecasts on OneDrive. Offer to copy the Finn folder to a local folder (move_root), or dismiss with --action=dismiss_move_suggestion.")
  }
  finn_result(TRUE, "ok", paste0("Finn folder: ", root), data, next_actions)
}

#' Save the confirmed storage root
#'
#' @param args Parsed arguments.
#' @return Finn result.
setup_set_root <- function(args) {
  root <- finn_arg(args, "root")
  if (is.null(root)) stop("Pass --root=<folder>.", call. = FALSE)
  root <- finn_norm(root)
  if (grepl(tolower(finn_norm(tempdir())), tolower(root), fixed = TRUE)) {
    stop("Finn projects cannot live in a temporary folder. Pick a lasting folder such as OneDrive or Documents.", call. = FALSE)
  }
  data <- list(root = root, is_onedrive = finn_is_onedrive(root), exists = dir.exists(root))
  if (!finn_bool(finn_arg(args, "confirm"))) {
    return(finn_result(FALSE, "needs_confirmation", paste0("Confirm saving forecasts under ", root, "."), data,
      c("Confirm the folder with the user, then rerun with --confirm=true.")))
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(root)) stop("Could not create ", root, ". Check the path and permissions.", call. = FALSE)
  settings <- finn_load_settings()
  settings$root <- root
  settings$root_confirmed <- TRUE
  finn_save_settings(settings)
  notes <- if (data$is_onedrive) {
    "OneDrive keeps projects backed up. Runs with hundreds of series write many files and can slow syncing."
  } else {
    character()
  }
  finn_result(TRUE, "ok", paste0("Finn folder saved: ", root), data, notes)
}

#' Create a project and copy its input data
#'
#' @param args Parsed arguments.
#' @return Finn result.
setup_create <- function(args) {
  root <- finn_resolve_root(args)
  if (is.null(root)) stop("No Finn folder is set. Run --action=status first.", call. = FALSE)
  name <- finn_arg(args, "project")
  if (is.null(name)) stop("Pass --project=<name>.", call. = FALSE)
  paths <- finn_project_paths(root, finn_sanitize_name(name))
  existing <- finn_read_project(paths)
  data_file <- finn_arg(args, "data")
  finn_ensure_project_dirs(paths)
  copied <- NULL
  if (!is.null(data_file)) {
    if (!file.exists(data_file)) stop("Data file not found: ", data_file, call. = FALSE)
    src <- finn_norm(data_file)
    dest <- file.path(paths$input, basename(src))
    if (!identical(tolower(src), tolower(finn_norm(dest)))) {
      if (file.exists(dest)) {
        same <- tryCatch(identical(unname(tools::md5sum(src)), unname(tools::md5sum(dest))), error = function(e) FALSE)
        if (!same) {
          if (!finn_bool(finn_arg(args, "replace_data"))) {
            return(finn_result(FALSE, "needs_confirmation",
              paste0("Project '", paths$name, "' already has a different file named ", basename(src), "."),
              list(project = paths$name, existing = finn_project_relpath(paths, dest), new = src),
              c("Ask the user whether this is new data for the same forecast. If yes, rerun with --replace_data=true; the old copy is kept and the project points to the new one.",
                "If it is a different forecast, create a new project with another --project name instead.")))
          }
          dest <- file.path(paths$input, paste0(tools::file_path_sans_ext(basename(src)), "_", finn_stamp(), ".", tools::file_ext(src)))
        }
      }
      if (!file.exists(dest)) file.copy(src, dest, copy.date = TRUE)
    }
    copied <- finn_project_relpath(paths, dest)
  }
  project <- existing %||% list(schema_version = finn_schema_version, name = paths$name, display_name = name, created_at = finn_now())
  if (!is.null(copied)) project$data_file <- copied
  if (!is.null(finn_arg(args, "sheet"))) project$sheet <- finn_arg(args, "sheet")
  project$skill_version <- finn_skill_version
  versions <- finn_versions()
  project$finnts_version <- versions$finnts_version
  project$r_version <- versions$r_version
  finn_write_project(paths, project)

  long_path <- nchar(file.path(paths$artifacts, "forecasts", strrep("x", 60))) > 220
  data <- list(project = paths$name, folder = paths$dir, input = paths$input, output = paths$output,
    artifacts = paths$artifacts, data_file = project$data_file, already_existed = !is.null(existing))
  notes <- if (long_path) "The project folder path is long; Windows may fail on very long file paths. Consider a shorter root folder." else character()
  finn_result(TRUE, "ok",
    if (is.null(existing)) paste0("Project '", paths$name, "' created.") else paste0("Project '", paths$name, "' already existed; updated its data file."),
    data, c("Run profile_data.R, then build a config and run validate_config.R.", notes)
  )
}

#' Copy the whole Finn folder to a new root
#'
#' @param args Parsed arguments.
#' @return Finn result.
setup_move_root <- function(args) {
  from <- finn_resolve_root(args)
  to <- finn_arg(args, "to")
  if (is.null(from) || is.null(to)) stop("Pass --to=<new folder> and make sure a Finn folder is set.", call. = FALSE)
  to <- finn_norm(to)
  data <- list(from = from, to = to)
  for (p in setup_list_projects(from)) {
    paths <- finn_project_paths(from, p$name)
    if (!is.null(finn_active_run(paths))) {
      stop("Project '", p$name, "' has a run in progress. Wait for it or cancel it before moving.", call. = FALSE)
    }
  }
  if (!finn_bool(finn_arg(args, "confirm"))) {
    return(finn_result(FALSE, "needs_confirmation",
      paste0("This copies every Finn project from ", from, " to ", to, ". The old folder is kept until you remove it."),
      data, c("Confirm with the user, then rerun with --confirm=true.")))
  }
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  items <- list.files(from, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  ok <- file.copy(items, to, recursive = TRUE, copy.date = TRUE, overwrite = FALSE)
  if (!all(ok)) stop("Some items did not copy: ", paste(basename(items[!ok]), collapse = ", "), ". The old folder is unchanged.", call. = FALSE)
  settings <- finn_load_settings()
  settings$root <- to
  settings$root_confirmed <- TRUE
  settings$large_onedrive_runs <- 0L
  finn_save_settings(settings)
  finn_result(TRUE, "ok", paste0("Copied Finn projects to ", to, " and switched to it."), data,
    c(paste0("Tell the user the old folder ", from, " is still there; they can remove it after checking the new one.")))
}

#' Report how much disk space a project uses
#'
#' Read-only. Suggests archiving old runs by hand; never removes anything.
#'
#' @param args Parsed arguments.
#' @return Finn result.
setup_disk_usage <- function(args) {
  paths <- finn_project_from_args(args)
  usage <- finn_disk_usage(paths)
  notes <- if (usage$total_mb >= 1024) {
    c(paste0("The project uses about ", round(usage$total_mb / 1024, 1), " GB."),
      "If space is tight, the user can move older folders under runs/ and output/ to an archive location by hand. Keep finn_artifacts/ while they may update or resume forecasts.")
  } else {
    character()
  }
  finn_result(TRUE, "ok", paste0("Project '", paths$name, "' uses about ", round(usage$total_mb), " MB."), usage, notes)
}

finn_main(function(args) {
  action <- finn_arg(args, "action", "status")
  switch(action,
    status = setup_status(args),
    list = finn_result(TRUE, "ok", "Projects listed.", list(root = finn_resolve_root(args), projects = setup_list_projects(finn_resolve_root(args)))),
    set_root = setup_set_root(args),
    create = setup_create(args),
    move_root = setup_move_root(args),
    disk_usage = setup_disk_usage(args),
    dismiss_move_suggestion = {
      s <- finn_load_settings()
      s$move_suggestion_dismissed <- TRUE
      finn_save_settings(s)
      finn_result(TRUE, "ok", "Will not suggest moving the Finn folder again.")
    },
    stop("Unknown --action '", action, "'. Use status, list, set_root, create, move_root, disk_usage, or dismiss_move_suggestion.", call. = FALSE)
  )
})
