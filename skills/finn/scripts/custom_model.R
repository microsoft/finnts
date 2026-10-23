# Create, list, show, and enroll custom models for a Finn project.
# Usage:
#   Rscript custom_model.R --project=<name> --action=list
#   Rscript custom_model.R --project=<name> --action=template --name=<model> [--horizon=<n>] [--drivers=a,b]
#   Rscript custom_model.R --project=<name> --action=create --name=<model>
#       (--instructions="<rule in plain words>" | --instructions_file=<path>)
#       (--examples=<csv> | --skip_examples=true) [--drivers=a,b] [--model_type=local|global|local,global]
#       [--horizon=<n>] [--hist_start_date=YYYY-MM-DD] [--forecast_approach=<approach>] [--timeout=600]
#       [--enroll=true] --confirm=true
#   Rscript custom_model.R --project=<name> --action=show --name=<model> [--version=<id>]
#   Rscript custom_model.R --project=<name> --action=enroll --name=<model> [--version=<id>] --confirm=true
#   Rscript custom_model.R --project=<name> --action=unenroll --name=<model> --confirm=true
#
# create sends the instructions, column names, and worked examples (not the
# project's series values) to the config's LLM (GitHub Copilot by default),
# then runs the R code it writes on this computer to check it. That code is not
# sandboxed, so create needs --confirm=true after the user agrees. Approved
# models are saved once under <project>/custom_models and never replaced or
# deleted. Enrolled models are reused by every standard, iterate, and update run.

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  source(file.path(dirname(gsub("~\\+~", " ", file_arg)), "finn_common.R"))
})

#' Split a comma-separated argument
#'
#' @param x Argument value, or NULL.
#' @return Trimmed, non-empty, unique values.
custom_model_split <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  parts <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  unique(parts[nzchar(parts)])
}

#' Read and check the `--name` argument
#'
#' @param args Parsed arguments.
#' @return The custom model name. Errors when it is missing or invalid.
custom_model_name <- function(args) {
  name <- finn_arg(args, "name")
  if (is.null(name)) stop("Pass --name=<model name>, for example --name=last_year_growth.", call. = FALSE)
  builtin <- tryCatch(finnts::list_models(), error = function(e) character())
  if (length(finn_custom_bad_names(name, builtin))) {
    stop(finn_custom_name_rule(), ". `", name, "` doesn't qualify.", call. = FALSE)
  }
  name
}

#' Forecast horizon used by worked examples
#'
#' @param args Parsed arguments; `--horizon` overrides the config.
#' @param cfg Merged config.
#' @return Whole number from 1 through 24 (the config horizon, capped at 24).
custom_model_horizon <- function(args, cfg) {
  value <- finn_arg(args, "horizon")
  horizon <- if (is.null(value)) min(as.numeric(cfg$forecast_horizon %||% 3), 24) else suppressWarnings(as.numeric(value))
  if (length(horizon) != 1L || is.na(horizon) || horizon < 1 || horizon > 24 || horizon != floor(horizon)) {
    stop("--horizon must be a whole number from 1 through 24.", call. = FALSE)
  }
  as.integer(horizon)
}

#' Drivers a new custom model may use
#'
#' @param args Parsed arguments; `--drivers` lists columns.
#' @param cfg Merged config.
#' @return Driver columns. Errors when one is not in `external_regressors`.
custom_model_drivers <- function(args, cfg) {
  drivers <- custom_model_split(finn_arg(args, "drivers"))
  missing <- setdiff(drivers, unlist(cfg$external_regressors))
  if (length(missing)) {
    stop("Drivers must be listed in the config's external_regressors first: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  drivers
}

#' Plain-language instructions for a new custom model
#'
#' @param args Parsed arguments with `--instructions` or `--instructions_file`.
#' @return Instruction text. Errors when neither is given or the text is empty.
custom_model_instructions <- function(args) {
  file <- finn_arg(args, "instructions_file")
  text <- if (!is.null(file)) {
    if (!file.exists(file)) stop("No instructions file at ", file, ".", call. = FALSE)
    paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    finn_arg(args, "instructions")
  }
  if (is.null(text) || !nzchar(trimws(text))) {
    stop("Describe the model in plain words with --instructions=\"...\" or --instructions_file=<path>.", call. = FALSE)
  }
  text
}

#' Read a worked-examples CSV
#'
#' Dates stay as text so finnts can check they are ISO dates (YYYY-MM-DD).
#'
#' @param path CSV path.
#' @return Data frame of examples. Errors when the file is missing or empty.
custom_model_read_examples <- function(path) {
  if (!file.exists(path)) {
    stop("No examples file at ", finn_norm(path), ". Write one with --action=template, or pass --skip_examples=true.", call. = FALSE)
  }
  examples <- utils::read.csv(path,
    stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"),
    fileEncoding = "UTF-8-BOM", colClasses = c(Date = "character")
  )
  if (!nrow(examples)) stop("The examples file ", finn_norm(path), " has no rows.", call. = FALSE)
  examples
}

#' Choose the series and history window used to check a new custom model
#'
#' Model creation needs every series it checks to have finite actuals (and
#' drivers) on one shared, gap-free history window. Picks the window start that
#' keeps the most series-periods (series kept times periods), preferring longer
#' history on ties, then keeps only the series complete on that window. The
#' window is shortened to stay within the finnts creation budget of 10000
#' history rows. Only these checks use the subset; runs fit the model on every
#' series.
#'
#' The history grid follows `date_type` from the first date through the last
#' history date, so a period missing from every series is a gap in all of them
#' and the chosen window starts after it. Dates that do not follow `date_type`
#' (for example month-end dates) use the dates the data contains.
#'
#' @param data Prepared input with a `Date` column.
#' @param combo_variables Series columns.
#' @param target Target column.
#' @param drivers Driver columns the model may use.
#' @param date_type finnts date type.
#' @param horizon Forecast horizon of the examples.
#' @param hist_end_date Last history date, or NULL for the latest date with
#'   actuals.
#' @param hist_start_date Window start to use, or NULL to choose one.
#' @param global Whether the model may be global, so finnts checks up to 100
#'   series instead of 3.
#' @return List with `data`, `hist_start_date`, `hist_end_date`,
#'   `series_used`, `series_total`, and `periods`. Errors in plain language
#'   when no series qualifies.
custom_model_panel <- function(data, combo_variables, target, drivers = character(), date_type, horizon,
                               hist_end_date = NULL, hist_start_date = NULL, global = FALSE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)[, c("Date", combo_variables, target, drivers), drop = FALSE]
  rownames(data) <- NULL
  valid <- function(v) if (is.numeric(v)) is.finite(v) else !is.na(v)
  drivers_ok <- rep(TRUE, nrow(data))
  for (d in drivers) drivers_ok <- drivers_ok & valid(data[[d]])
  actual <- valid(data[[target]])
  if (!any(actual)) stop("The data has no actuals, so a custom model can't be checked.", call. = FALSE)
  end <- if (is.null(hist_end_date)) max(data$Date[actual]) else as.Date(hist_end_date)
  observed <- sort(unique(data$Date[data$Date <= end]))
  expected <- if (length(observed)) tryCatch(seq(observed[1L], end, by = date_type), error = function(e) NULL)
  grid <- if (!is.null(expected) && all(observed %in% expected)) expected else observed
  n <- length(grid)
  future <- if (length(drivers)) seq(end, by = date_type, length.out = horizon + 1L)[-1L] else as.Date(character())
  key <- do.call(paste, c(lapply(data[combo_variables], as.character), sep = "\r"))
  rows <- split(seq_len(nrow(data)), key)
  starts <- vapply(rows, function(i) {
    complete <- i[actual[i] & drivers_ok[i]]
    present <- grid %in% data$Date[complete]
    if (!n || !present[n]) {
      return(NA_integer_)
    }
    if (length(drivers) && !all(future %in% data$Date[i[drivers_ok[i]]])) {
      return(NA_integer_)
    }
    gaps <- which(!present)
    if (length(gaps)) as.integer(max(gaps) + 1L) else 1L
  }, integer(1))
  eligible <- starts[!is.na(starts)]
  if (!length(eligible)) {
    stop(sprintf(
      "No series has actuals%s for its latest periods through %s, so the new model can't be checked. Set hist_end_date to the last complete period.",
      if (length(drivers)) " and complete drivers (including future driver values)" else "", format(end)
    ), call. = FALSE)
  }
  if (!is.null(hist_start_date)) {
    start <- match(as.Date(hist_start_date), grid)
    if (is.na(start)) stop("--hist_start_date must be one of the data's dates on or before ", format(end), ".", call. = FALSE)
  } else {
    candidates <- sort(unique(eligible))
    score <- vapply(candidates, function(s) sum(eligible <= s) * (n - s + 1), numeric(1))
    start <- candidates[which.max(score)]
  }
  checked <- min(sum(eligible <= start), if (isTRUE(global)) 100L else 3L)
  max_periods <- floor(10000 / max(checked, 1L))
  if (n - start + 1L > max_periods) start <- as.integer(n - max_periods + 1L)
  keep <- names(eligible)[eligible <= start]
  if (!length(keep)) {
    stop("No series has complete actuals from ", format(grid[start]), " through ", format(end),
      ". Choose a later --hist_start_date.", call. = FALSE)
  }
  in_window <- data$Date >= grid[start] & (data$Date <= end | data$Date %in% future)
  out <- data[key %in% keep & in_window, , drop = FALSE]
  rownames(out) <- NULL
  list(
    data = out, hist_start_date = grid[start], hist_end_date = end,
    series_used = length(keep), series_total = length(rows), periods = as.integer(n - start + 1L)
  )
}

#' Saved versions of one custom model
#'
#' Lists the model's folder, which is genuine discovery of unknown version
#' files. Metadata comes from each `<version>.json`, or from the model file
#' when the metadata file is missing.
#'
#' @param paths Project paths.
#' @param name Custom model name.
#' @return List of metadata lists, newest first, each with `version`,
#'   `version_id`, and `rds` added.
custom_model_versions <- function(paths, name) {
  dir <- file.path(finn_custom_models_dir(paths), name)
  if (!dir.exists(dir)) {
    return(list())
  }
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  versions <- lapply(files, function(rds) {
    json <- sub("\\.rds$", ".json", rds)
    meta <- tryCatch(finn_read_json(json), error = function(e) NULL)
    if (!is.list(meta) || !is.character(meta$version_id)) {
      model <- tryCatch(readRDS(rds), error = function(e) NULL)
      if (!inherits(model, "finnts_custom_model")) {
        return(NULL)
      }
      meta <- finn_custom_metadata(model)
    }
    meta$version <- finn_custom_version_short(meta$version_id)
    meta$rds <- finn_norm(rds)
    meta
  })
  versions <- Filter(Negate(is.null), versions)
  created <- vapply(versions, function(v) as.character(v$created_at %||% ""), character(1))
  versions[order(created, decreasing = TRUE, method = "radix")]
}

#' Find one saved version of a custom model
#'
#' @param versions Saved versions from [custom_model_versions()].
#' @param name Custom model name.
#' @param version Leading characters of a version id, or NULL.
#' @return Metadata list of the matching version, or NULL when `version` is
#'   NULL and more than one version is saved. Errors when none matches.
custom_model_pick <- function(versions, name, version = NULL) {
  if (!length(versions)) {
    stop("No custom model named `", name, "` is saved in this project. Create it with --action=create.", call. = FALSE)
  }
  if (!is.null(version)) {
    hits <- Filter(function(v) startsWith(v$version_id, tolower(version)), versions)
    if (length(hits) != 1L) {
      stop(if (length(hits)) "More than one" else "No", " saved version of `", name, "` starts with `", version,
        "`. See --action=list.", call. = FALSE)
    }
    return(hits[[1]])
  }
  if (length(versions) > 1L) {
    return(NULL)
  }
  versions[[1]]
}

#' Short listing of saved versions for a user choice
#'
#' @param versions Metadata lists from [custom_model_versions()].
#' @return List of `version`, `created_at`, `model_type`, and `drivers`.
custom_model_version_choices <- function(versions) {
  lapply(versions, function(v) v[intersect(c("version", "created_at", "model_type", "drivers"), names(v))])
}

#' Enroll or unenroll a custom model in the saved project config
#'
#' Rewrites only the `custom_models` entry of `config.json`, removing the key
#' when nothing stays enrolled. A model has at most one enrolled version.
#'
#' @param paths Project paths.
#' @param name Custom model name.
#' @param version_id Full version id to enroll, or NULL to unenroll.
#' @return List with `previous` and `current` labels from
#'   [finn_custom_labels()].
custom_model_set_enrollment <- function(paths, name, version_id = NULL) {
  config_file <- file.path(paths$dir, "config.json")
  cfg <- finn_read_json(config_file)
  if (!is.list(cfg)) {
    stop("This project has no saved config yet. Save one with validate_config.R --save=true first.", call. = FALSE)
  }
  entries <- finn_custom_normalize(cfg$custom_models) %||% list()
  if (!is.list(entries)) {
    stop("custom_models in config.json is not a list of {\"name\", \"version_id\"} entries. Fix it before enrolling.", call. = FALSE)
  }
  previous <- finn_custom_labels(cfg)
  entries <- Filter(function(e) !(is.list(e) && identical(e$name, name)), entries)
  if (!is.null(version_id)) entries <- c(entries, list(list(name = name, version_id = version_id)))
  cfg$custom_models <- finn_custom_normalize(entries)
  finn_write_json(cfg, config_file)
  list(previous = previous, current = finn_custom_labels(cfg))
}

#' Next steps after the enrolled custom models change
#'
#' @param paths Project paths.
#' @return Suggested follow-ups.
custom_model_enrollment_actions <- function(paths) {
  c(
    sprintf("Rerun validate_config.R --project=%s and show the settings review, which lists the enrolled custom models, before any run.", paths$name),
    "If this project already has an Agent forecast, the next Agent iterate run starts a new Agent version (it asks for confirmation), and Agent update needs the same custom models as the last Agent version."
  )
}

#' Write a worked-examples template for a custom model
#'
#' Never replaces an existing examples file.
#'
#' @param args Parsed arguments.
#' @param paths Project paths.
#' @return Script result.
custom_model_template <- function(args, paths) {
  name <- custom_model_name(args)
  if (!finn_custom_supported("standard")) {
    return(custom_model_unsupported())
  }
  cfg <- finn_merge_config(finn_read_project_config(paths))
  if (!isTRUE(cfg$date_type %in% c("year", "quarter", "month", "week", "day"))) {
    stop("Set date_type in the project config first.", call. = FALSE)
  }
  horizon <- custom_model_horizon(args, cfg)
  drivers <- custom_model_drivers(args, cfg)
  path <- finn_custom_examples_file(paths, name)
  data <- list(name = name, examples_file = finn_norm(path), horizon = horizon, date_type = cfg$date_type)
  guide <- paste(
    "Help the user replace the illustrative rows with 2 to 8 worked examples calculated from their own rule,",
    "including at least one edge case (example names starting with edge_). History rows fill the target and leave",
    "expected blank; future rows leave the target blank and fill expected. Examples are sent to the LLM, so use made-up values."
  )
  if (file.exists(path)) {
    return(finn_result(TRUE, "exists", sprintf("An examples file for `%s` already exists; it was not replaced.", name), data,
      c(guide, sprintf("Pass --examples=%s to --action=create.", finn_norm(path)))))
  }
  template <- finnts::custom_model_examples_template(horizon, cfg$date_type)
  for (d in drivers) template[[d]] <- NA
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(template, path, row.names = FALSE, na = "")
  finn_result(TRUE, "ok", sprintf("Wrote a worked-examples template for `%s`.", name), data, c(
    guide,
    if (length(drivers)) "Fill every driver column on every row, including future rows.",
    sprintf("Then pass --examples=%s to --action=create.", finn_norm(path))
  ))
}

#' Ask finnts to write, check, and approve a custom model
#'
#' A thin wrapper so tests can replace the LLM step.
#'
#' @param ... Arguments for `finnts::create_custom_model()`.
#' @return The approved custom model object.
custom_model_author <- function(...) {
  finnts::create_custom_model(...)
}

#' Result for a finnts that cannot create custom models
#'
#' @return Script result with status "blocked".
custom_model_unsupported <- function() {
  finn_result(FALSE, "blocked", "The installed finnts can't create custom models.", list(),
    "Offer install_finnts.R --confirm=true, which installs the latest finnts from GitHub.")
}

#' Create, check, and save a custom model from plain-language instructions
#'
#' Checks every input first, then asks for confirmation, because creation
#' sends the instructions and examples to the LLM and runs the generated R
#' code unsandboxed on this computer.
#'
#' @param args Parsed arguments.
#' @param paths Project paths.
#' @return Script result.
custom_model_create <- function(args, paths) {
  name <- custom_model_name(args)
  if (!finn_custom_supported("standard")) {
    return(custom_model_unsupported())
  }
  base <- finn_read_project_config(paths)
  base$custom_models <- NULL
  checked <- finn_load_checked(base, root = paths$root)
  if (length(checked$errors)) {
    return(finn_result(FALSE, "invalid", "The project config has problems to fix before creating a custom model.",
      list(errors = checked$errors), "Fix the config with the user and run validate_config.R, then try again."))
  }
  cfg <- checked$config
  llm_cfg <- cfg$agent$llm %||% list(provider = "copilot", model = "auto")
  provider <- llm_cfg$provider %||% "copilot"
  if (identical(provider, "copilot")) {
    if (!finn_custom_copilot_supported()) {
      return(finn_result(FALSE, "blocked", "The installed finnts can't create custom models with GitHub Copilot.", list(),
        "Offer install_finnts.R --confirm=true (the latest finnts from GitHub), or set agent.llm to an azure_openai or openai provider."))
    }
    timeout <- suppressWarnings(as.numeric(finn_arg(args, "timeout") %||% max(600, as.numeric(llm_cfg$timeout %||% 0))))
    if (length(timeout) != 1L || is.na(timeout) || timeout <= 0) stop("--timeout must be a positive number of seconds.", call. = FALSE)
    llm_cfg$timeout <- timeout
  }
  instructions <- custom_model_instructions(args)
  drivers <- custom_model_drivers(args, cfg)
  model_type <- custom_model_split(finn_arg(args, "model_type", "local"))
  if (!length(model_type) || !all(model_type %in% c("local", "global"))) {
    stop("--model_type must be local, global, or local,global.", call. = FALSE)
  }
  horizon <- custom_model_horizon(args, cfg)
  approach <- finn_arg(args, "forecast_approach") %||%
    (if (identical(cfg$mode, "standard")) cfg$forecast_approach else NULL) %||% "bottoms_up"
  if (!approach %in% c("bottoms_up", "standard_hierarchy", "grouped_hierarchy")) {
    stop("--forecast_approach must be bottoms_up, standard_hierarchy, or grouped_hierarchy.", call. = FALSE)
  }
  if (!identical(approach, "bottoms_up") && (length(drivers) || !identical(model_type, "local"))) {
    stop("Custom models for ", approach, " forecasts must be local models without drivers.", call. = FALSE)
  }
  skip <- finn_bool(finn_arg(args, "skip_examples"))
  examples_file <- finn_arg(args, "examples")
  if (is.null(examples_file) && !skip) {
    stop("Pass --examples=<csv> with worked examples (write one with --action=template), or --skip_examples=true.", call. = FALSE)
  }
  examples <- if (!skip) custom_model_read_examples(examples_file)
  data <- checked$data
  if (!is.null(cfg$hist_start_date)) data <- data[data$Date >= as.Date(cfg$hist_start_date), , drop = FALSE]
  panel <- custom_model_panel(data, unlist(cfg$combo_variables), cfg$target_variable, drivers, cfg$date_type, horizon,
    hist_end_date = cfg$hist_end_date, hist_start_date = finn_arg(args, "hist_start_date"),
    global = "global" %in% model_type
  )
  check_window <- list(
    series_used = panel$series_used, series_total = panel$series_total, periods = panel$periods,
    hist_start_date = format(panel$hist_start_date), hist_end_date = format(panel$hist_end_date)
  )
  request <- list(
    name = name, provider = provider, llm_model = llm_cfg$model %||% "auto", model_type = model_type,
    drivers = drivers, forecast_approach = approach, horizon = horizon,
    examples = if (skip) 0L else length(unique(examples$example)), check_window = check_window
  )
  if (!finn_bool(finn_arg(args, "confirm"))) {
    return(finn_result(FALSE, "needs_confirmation", sprintf(paste(
      "Creating `%s` sends your instructions, column names, and worked examples (not your series values) to %s,",
      "then runs the R code it writes on this computer to check it. That code is not sandboxed."
    ), name, if (identical(provider, "copilot")) "GitHub Copilot" else provider), request, c(
      "Explain this to the user in plain words and get a clear yes before rerunning with --confirm=true.",
      if (skip) "Without worked examples, finnts can't confirm the forecasts match the user's rule. Recommend --action=template first."
    )))
  }
  control <- if (!identical(approach, "bottoms_up")) finnts::custom_model_control(forecast_approach = approach)
  llm <- finn_build_llm(llm_cfg)
  finn_log("Creating custom model ", name, " with ", provider, "; this can take several minutes.")
  model <- tryCatch(
    custom_model_author(
      instructions = instructions, llm = llm, input_data = panel$data,
      combo_variables = unlist(cfg$combo_variables), target_variable = cfg$target_variable,
      date_type = cfg$date_type, forecast_horizon = horizon,
      external_regressors = if (length(drivers)) drivers,
      hist_start_date = panel$hist_start_date, hist_end_date = panel$hist_end_date,
      name = name, model_type = model_type, validation_examples = examples,
      approval = "automatic", control = control
    ),
    error = function(e) e
  )
  if (inherits(model, "error")) {
    return(custom_model_failure(model, request))
  }
  feedback <- finn_custom_verify(model)
  meta <- finn_custom_metadata(model, llm_cfg, request$examples, unlist(cfg$combo_variables))
  saved <- finn_custom_save(paths, model, meta)
  summary <- finn_custom_summary(model, unlist(cfg$combo_variables))
  out <- c(request, list(model = summary, saved = saved, checks = as.list(names(model$validation$checks))))
  enroll <- finn_bool(finn_arg(args, "enroll"))
  if (enroll) out$enrollment <- custom_model_set_enrollment(paths, name, model$definition$version_id)
  message <- sprintf("Created custom model `%s` (version %s). It passed its checks on %d of %d series from %s to %s.",
    name, summary$version, panel$series_used, panel$series_total, check_window$hist_start_date, check_window$hist_end_date)
  if (identical(saved$status, "exists")) message <- paste(message, "This exact version was already saved, so nothing was replaced.")
  finn_result(TRUE, "ok", message, out, c(
    paste("Tell the user in plain words what data.model.interpretation says the model does, and that passing its checks",
      "does not guarantee forecast accuracy:", feedback$message %||% ""),
    if (enroll) {
      custom_model_enrollment_actions(paths)
    } else {
      sprintf("Ask whether this project's runs should use it. If yes, run custom_model.R --project=%s --action=enroll --name=%s --version=%s --confirm=true.",
        paths$name, name, summary$version)
    }
  ))
}

#' Result for a custom model that was not created
#'
#' @param error Condition raised by `finnts::create_custom_model()`.
#' @param request Request summary from [custom_model_create()].
#' @return Script result with the finnts feedback status ("needs_input" when
#'   the LLM asked business questions, otherwise "not_created").
custom_model_failure <- function(error, request) {
  feedback <- tryCatch(finnts::custom_model_feedback(error), error = function(e) list())
  questions <- unname(unlist(feedback$questions))
  status <- if (identical(feedback$status, "needs_input")) "needs_input" else "not_created"
  data <- c(request, list(feedback = feedback[intersect(c("status", "stage", "code", "next_action", "details"), names(feedback))],
    questions = as.list(questions), error = finn_redact(conditionMessage(error))))
  finn_result(FALSE, status, paste("The custom model was not created.", feedback$message %||% ""), data, c(
    if (length(questions)) {
      "Ask the user these questions in plain words, add their answers to the instructions, and run create again."
    } else {
      "Explain the problem in plain words. Adjust the instructions or worked examples with the user, then run create again; nothing was saved."
    }
  ))
}

#' List saved custom models and which are enrolled
#'
#' @param args Parsed arguments.
#' @param paths Project paths.
#' @return Script result.
custom_model_list <- function(args, paths) {
  dir <- finn_custom_models_dir(paths)
  cfg <- finn_read_json(file.path(paths$dir, "config.json"), list())
  enrolled <- finn_custom_labels(cfg)
  model_names <- if (dir.exists(dir)) sort(basename(list.dirs(dir, recursive = FALSE, full.names = TRUE)), method = "radix") else character()
  models <- lapply(model_names, function(n) {
    versions <- lapply(custom_model_versions(paths, n), function(v) {
      v$enrolled <- paste0(n, "@", v$version) %in% enrolled
      v[intersect(c("version", "version_id", "created_at", "model_type", "drivers", "date_types", "hierarchy", "provider", "llm_model", "examples", "enrolled"), names(v))]
    })
    examples <- finn_custom_examples_file(paths, n)
    list(name = n, versions = versions, examples_file = if (file.exists(examples)) finn_norm(examples))
  })
  models <- Filter(function(m) length(m$versions), models)
  finn_result(TRUE, "ok", sprintf("%d saved custom model(s); enrolled: %s.", length(models), finn_custom_label_text(enrolled)),
    list(models = models, enrolled = as.list(enrolled)),
    if (length(models)) "Use --action=show --name=<model> to explain what a model does." else "Create one with --action=create.")
}

#' Show what one saved custom model does
#'
#' Uses the enrolled version when no `--version` is given, otherwise the
#' newest saved version.
#'
#' @param args Parsed arguments.
#' @param paths Project paths.
#' @return Script result.
custom_model_show <- function(args, paths) {
  name <- custom_model_name(args)
  cfg <- finn_read_json(file.path(paths$dir, "config.json"), list())
  version <- finn_arg(args, "version")
  if (is.null(version)) {
    entry <- Filter(function(e) is.list(e) && identical(e$name, name), finn_custom_normalize(cfg$custom_models) %||% list())
    if (length(entry)) version <- entry[[1]]$version_id
  }
  versions <- custom_model_versions(paths, name)
  meta <- custom_model_pick(versions, name, version) %||% versions[[1]]
  model <- finn_custom_read(paths, list(name = name, version_id = meta$version_id), verify = TRUE)
  summary <- finn_custom_summary(model, unlist(cfg$combo_variables))
  summary$enrolled <- paste0(name, "@", summary$version) %in% finn_custom_labels(cfg)
  finn_result(TRUE, "ok", sprintf("Custom model `%s` (version %s) is approved and intact.", name, summary$version),
    list(model = summary, other_versions = custom_model_version_choices(Filter(function(v) !identical(v$version_id, meta$version_id), versions))),
    "Explain data.model.interpretation to the user in plain words.")
}

#' Enroll a saved custom model so every run of the project uses it
#'
#' @param args Parsed arguments.
#' @param paths Project paths.
#' @return Script result.
custom_model_enroll <- function(args, paths) {
  name <- custom_model_name(args)
  versions <- custom_model_versions(paths, name)
  meta <- custom_model_pick(versions, name, finn_arg(args, "version"))
  if (is.null(meta)) {
    return(finn_result(FALSE, "needs_input", sprintf("`%s` has more than one saved version.", name),
      list(versions = custom_model_version_choices(versions)),
      "Ask the user which version to use, then rerun with --version=<version>."))
  }
  finn_custom_read(paths, list(name = name, version_id = meta$version_id), verify = TRUE)
  label <- sprintf("`%s@%s`", name, meta$version)
  if (!finn_bool(finn_arg(args, "confirm"))) {
    return(finn_result(FALSE, "needs_confirmation",
      sprintf("Enrolling %s makes every standard, iterate, and update run of this project train it alongside Finn's models.", label),
      list(name = name, version = meta$version, version_id = meta$version_id),
      "Get a clear yes from the user, then rerun with --confirm=true."))
  }
  change <- custom_model_set_enrollment(paths, name, meta$version_id)
  finn_result(TRUE, "ok", sprintf("Enrolled %s. Enrolled custom models: %s.", label, finn_custom_label_text(change$current)),
    list(enrollment = change), custom_model_enrollment_actions(paths))
}

#' Stop runs of the project from using a custom model
#'
#' Keeps the saved model files.
#'
#' @param args Parsed arguments.
#' @param paths Project paths.
#' @return Script result.
custom_model_unenroll <- function(args, paths) {
  name <- custom_model_name(args)
  cfg <- finn_read_json(file.path(paths$dir, "config.json"), list())
  if (!name %in% finn_custom_names(cfg)) {
    return(finn_result(TRUE, "not_enrolled", sprintf("`%s` is not enrolled, so nothing changed.", name),
      list(enrolled = as.list(finn_custom_labels(cfg)))))
  }
  if (!finn_bool(finn_arg(args, "confirm"))) {
    return(finn_result(FALSE, "needs_confirmation",
      sprintf("Unenrolling `%s` stops this project's runs from using it. Its saved files stay, so it can be enrolled again.", name),
      list(name = name), "Get a clear yes from the user, then rerun with --confirm=true."))
  }
  change <- custom_model_set_enrollment(paths, name, NULL)
  finn_result(TRUE, "ok", sprintf("Unenrolled `%s`. Enrolled custom models: %s.", name, finn_custom_label_text(change$current)),
    list(enrollment = change), custom_model_enrollment_actions(paths))
}

finn_main(function(args) {
  paths <- finn_project_from_args(args)
  action <- finn_arg(args, "action", "list")
  handler <- switch(action,
    list = custom_model_list,
    show = custom_model_show,
    template = custom_model_template,
    create = custom_model_create,
    enroll = custom_model_enroll,
    unenroll = custom_model_unenroll,
    stop("--action must be list, show, template, create, enroll, or unenroll.", call. = FALSE)
  )
  handler(args, paths)
})
