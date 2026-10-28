# Offline tests for the Finn skill scripts.
# Usage: Rscript skills/finn/tests/test_skill.R
#
# Needs jsonlite, digest, and a POSIX shell (sh). Does not train models, call an
# LLM, or touch the user's saved Finn settings: settings and projects live in
# a temporary folder.

skill_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  here <- if (length(file_arg)) dirname(gsub("~\\+~", " ", sub("^--file=", "", file_arg[[1]]))) else "skills/finn/tests"
  normalizePath(file.path(here, ".."), winslash = "/", mustWork = TRUE)
})
scripts_dir <- file.path(skill_dir, "scripts")

tmp <- file.path(tempdir(), paste0("finn-skill-test-", Sys.getpid()))
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(FINN_SETTINGS_DIR = file.path(tmp, "settings"), FINN_SKILL_SCRIPTS = scripts_dir)

failures <- character()
passes <- 0L

#' Record one test expectation
#'
#' @param label Test description.
#' @param cond Expression that should evaluate to TRUE.
#' @return Invisible logical result.
check <- function(label, cond) {
  ok <- tryCatch(isTRUE(cond), error = function(e) {
    message("  error: ", conditionMessage(e))
    FALSE
  })
  if (ok) {
    passes <<- passes + 1L
  } else {
    failures <<- c(failures, label)
    message("FAIL: ", label)
  }
  invisible(ok)
}

#' Whether an expression raises an error matching a pattern
#'
#' @param expr Expression to evaluate.
#' @param pattern Regex the error message must match.
#' @return Logical.
errors_with <- function(expr, pattern) {
  msg <- tryCatch({
    force(expr)
    NULL
  }, error = function(e) conditionMessage(e))
  !is.null(msg) && grepl(pattern, msg)
}

#' Load only the function definitions from a script
#'
#' Scripts source finn_common.R and call finn_main() at top level; both are
#' skipped so the functions can be tested without running the script.
#'
#' @param file Script file name in scripts/.
#' @param parent Environment holding the common helpers.
#' @return Environment containing the script's functions.
load_functions <- function(file, parent) {
  env <- new.env(parent = parent)
  for (e in parse(file.path(scripts_dir, file), keep.source = FALSE)) {
    is_fn <- is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
    if (is_fn) eval(e, env)
  }
  env
}

common <- new.env(parent = globalenv())
sys.source(file.path(scripts_dir, "finn_common.R"), envir = common)

# ---- Every script parses ----------------------------------------------------
for (f in list.files(scripts_dir, pattern = "\\.R$")) {
  check(paste("parses:", f), !inherits(try(parse(file.path(scripts_dir, f)), silent = TRUE), "try-error"))
}

with(common, {
  # ---- Argument parsing -----------------------------------------------------
  a <- finn_parse_args(c("--project=My Proj", "--log-lines=5", "--wait", "positional", "--eq=a=b"))
  check("parse_args keeps values with spaces", identical(a$project, "My Proj"))
  check("parse_args turns hyphens into underscores", identical(a$log_lines, "5"))
  check("parse_args maps bare flags to true", identical(a$wait, "true"))
  check("parse_args keeps '=' inside values", identical(a$eq, "a=b"))
  check("parse_args ignores positional arguments", length(a) == 4)
  check("finn_arg default on empty", identical(finn_arg(list(x = ""), "x", "d"), "d"))
  check("finn_bool yes", finn_bool("Yes") && finn_bool(TRUE) && finn_bool("1"))
  check("finn_bool no", !finn_bool("false") && !finn_bool(NULL) && finn_bool(NA, default = TRUE))

  # ---- Names and paths --------------------------------------------------------
  check("sanitize replaces unsafe characters", identical(finn_sanitize_name("  Q3 Revenue / EMEA! "), "Q3_Revenue_EMEA"))
  check("sanitize caps length", nchar(finn_sanitize_name(strrep("a", 80))) == 40)
  check("sanitize rejects empty names", errors_with(finn_sanitize_name("!!!"), "at least one letter"))
  p <- finn_project_paths(file.path(tmp, "root"), "Demo")
  check("project paths use finn_artifacts", identical(basename(p$artifacts), "finn_artifacts"))
  check("project paths share one folder", all(dirname(unlist(p[c("input", "output", "artifacts", "runs", "analysis")])) == p$dir))
  check("relpath inside project", identical(finn_project_relpath(p, file.path(p$input, "a.csv")), "input/a.csv"))
  check("relpath outside project stays absolute", identical(finn_project_relpath(p, file.path(tmp, "x.csv")), finn_norm(file.path(tmp, "x.csv"))))

  # ---- Number and date parsing ------------------------------------------------
  check("parse_number handles currency and commas", identical(as.vector(finn_parse_number(c("$1,234.50", " 7 "))), c(1234.5, 7)))
  check("parse_number treats parentheses as negative", identical(as.vector(finn_parse_number("(1,000)")), -1000))
  check("parse_number leaves text as NA", is.na(finn_parse_number("n/a")))
  d <- finn_parse_date(c("01/31/2024", "02/29/2024"))
  check("parse_date month/day/year", identical(d$value, as.Date(c("2024-01-31", "2024-02-29"))) && identical(d$format, "%m/%d/%Y"))
  check("parse_date month name", identical(finn_parse_date(c("Jan 2024", "Feb 2024"))$value, as.Date(c("2024-01-01", "2024-02-01"))))
  check("parse_date year-month", identical(finn_parse_date("2024-03")$value, as.Date("2024-03-01")))
  check("parse_date Excel serial", identical(finn_parse_date(45292)$value, as.Date("2024-01-01")))

  # ---- WMAPE ------------------------------------------------------------------
  check("wmape basic", isTRUE(all.equal(finn_wmape(c(90, 110), c(100, 100)), 0.1)))
  check("wmape ignores NA pairs", isTRUE(all.equal(finn_wmape(c(90, NA), c(100, 5)), 0.1)))
  check("wmape NA when actuals are zero", is.na(finn_wmape(c(1, 2), c(0, 0))))

  # ---- Config -----------------------------------------------------------------
  merged <- finn_merge_config(list(forecast_horizon = 6L, agent = list(max_iter = 2L, llm = list(model = "x"))))
  check("merge keeps defaults", identical(merged$forecast_approach, "bottoms_up"))
  check("merge converts integers to doubles", is.double(merged$forecast_horizon) && is.double(merged$agent$max_iter) && is.double(merged$seed))
  check("merge keeps nested llm defaults", identical(merged$agent$llm$provider, "copilot") && identical(merged$agent$llm$model, "x"))

  template <- jsonlite::fromJSON(file.path(skill_dir, "templates", "config.template.json"), simplifyVector = TRUE, simplifyDataFrame = FALSE)
  template$project_name <- "Demo"
  check("template config is valid", length(finn_check_config(template)$errors) == 0)
  check("default keys cover template keys", all(setdiff(names(template), "project_name") %in% names(finn_default_config())))

  base <- list(project_name = "Demo", combo_variables = c("Region", "Product"), target_variable = "Revenue", date_type = "month", forecast_horizon = 6)
  check("valid base config", length(finn_check_config(base)$errors) == 0)
  check("spark is rejected", any(grepl("Spark is not supported", finn_check_config(modifyList(base, list(parallel = "spark")))$errors)))
  check("unknown parallel is rejected", any(grepl("parallel must be 'auto' or 'none'", finn_check_config(modifyList(base, list(parallel = "fast")))$errors)))
  check("bad horizon is rejected", any(grepl("forecast_horizon", finn_check_config(modifyList(base, list(forecast_horizon = 0)))$errors)))
  check("bad date_type is rejected", any(grepl("date_type", finn_check_config(modifyList(base, list(date_type = "hour")))$errors)))
  check("long horizon warns", any(grepl("horizon is long", finn_check_config(modifyList(base, list(forecast_horizon = 36)))$warnings)))

  # ---- Input preparation on the bundled example --------------------------------
  raw <- utils::read.csv(file.path(skill_dir, "examples", "sample_monthly.csv"), stringsAsFactors = FALSE)
  prep <- finn_prepare_input(raw, finn_merge_config(base))
  check("example prepares", inherits(prep$data$Date, "Date") && is.numeric(prep$data$Revenue))
  chk <- finn_check_config(base, prep$data)
  check("example passes validation", length(chk$errors) == 0)
  check("example facts", chk$facts$n_series == 4 && chk$facts$n_rows == 192)
  check("example facts count history periods", identical(chk$facts$n_periods, 48L))

  renamed <- raw
  names(renamed)[names(renamed) == "Date"] <- "Month"
  prep2 <- finn_prepare_input(renamed, finn_merge_config(modifyList(base, list(date_column = "Month"))))
  check("date_column is renamed to Date", "Date" %in% names(prep2$data) && length(prep2$notes) == 1)
  bad_dates <- raw
  bad_dates$Date[3] <- "not a date"
  check("unreadable dates stop", errors_with(finn_prepare_input(bad_dates, finn_merge_config(base)), "date values could not be read"))
  bad_num <- raw
  bad_num$Revenue <- as.character(bad_num$Revenue)
  bad_num$Revenue[5] <- "abc"
  check("non-numeric target stops", errors_with(finn_prepare_input(bad_num, finn_merge_config(base)), "are not numbers"))
  dup <- rbind(prep$data, prep$data[1, ])
  check("duplicate rows are errors", any(grepl("repeat the same series", finn_check_config(base, dup)$errors)))
  check("missing columns are errors", any(grepl("Columns not found", finn_check_config(modifyList(base, list(target_variable = "Sales")), prep$data)$errors)))

  # ---- Parallel strategy --------------------------------------------------------
  res <- function(cores, free_gb, total_gb = free_gb) list(cores = cores, free_gb = free_gb, total_gb = total_gb)
  big <- res(16, 34)
  check("global models default by date type", finn_default_global_models("month") && !finn_default_global_models("day"))
  s1 <- finn_choose_parallel(200, 0, FALSE, big)
  check("many series use local_machine", identical(s1$parallel_processing, "local_machine") && !s1$inner_parallel && s1$num_cores == 15)
  s2 <- finn_choose_parallel(5, 0, FALSE, big)
  check("few series use inner_parallel", is.null(s2$parallel_processing) && s2$inner_parallel)
  s3 <- finn_choose_parallel(40, 0, TRUE, big)
  check("global models with moderate series use inner_parallel", s3$inner_parallel && is.null(s3$parallel_processing))
  s4 <- finn_choose_parallel(200, 0, FALSE, big, step_down = 1)
  check("step_down 1 halves workers", identical(s4$parallel_processing, "local_machine") && s4$num_cores == 7)
  check("step_down 2 forces inner_parallel", finn_choose_parallel(200, 0, FALSE, big, step_down = 2)$inner_parallel)
  s5 <- finn_choose_parallel(200, 0, FALSE, big, step_down = 3)
  check("step_down 3 is sequential", is.null(s5$parallel_processing) && !s5$inner_parallel)
  check("few cores are sequential", !finn_choose_parallel(200, 0, FALSE, res(2, 64))$inner_parallel)
  check("80 percent total memory determines workers", finn_choose_parallel(200, 0, FALSE, res(16, 3))$safe_workers == 8)
  check("unknown total memory is reported and sequential", finn_choose_parallel(200, 0, FALSE, res(16, NA))$safe_workers == 1 &&
    length(finn_choose_parallel(200, 0, FALSE, res(16, NA))$warnings) > 0)
  check("flat worker estimate ignores input size", finn_choose_parallel(200, 100, FALSE, big)$safe_workers == 15)
  check("low available memory warns without lowering budget", finn_choose_parallel(200, 1, FALSE, res(22, 2.85, 31.65))$num_cores == 21 &&
    length(finn_choose_parallel(200, 1, FALSE, res(22, 2.85, 31.65))$warnings) > 0)
  check("failed CPU detection is reported and sequential", finn_choose_parallel(200, 0, FALSE, res(NA, 32))$safe_workers == 1 &&
    !finn_choose_parallel(200, 0, FALSE, res(NA, 32))$inner_parallel)
  check("one processor is sequential", is.null(finn_choose_parallel(200, 0, FALSE, res(1, 32))$num_cores))
  check("batch shares full machine budget", finn_plan_with_others(200, 0, FALSE, list(cores = 0L, gb = 0, runs = list()),
    res(22, 2.85, 31.65), batch_size = 3)$plan$num_cores == 7)
  check("invalid batch size rejected", errors_with(finn_plan_with_others(200, 0, FALSE, list(cores = 0L, gb = 0, runs = list()),
    big, batch_size = 0), "batch_size must be a positive whole"))
  check("requested worker ceiling honored", finn_plan_with_others(200, 0, FALSE, list(cores = 0L, gb = 0, runs = list()),
    big, num_cores = 4)$plan$num_cores == 4)
  check("invalid requested workers rejected", errors_with(finn_plan_with_others(200, 0, FALSE,
    list(cores = 0L, gb = 0, runs = list()), big, num_cores = 1.5), "num_cores must be a positive whole"))
  check("oversized requested workers capped with warning", finn_plan_with_others(200, 0, FALSE,
    list(cores = 0L, gb = 0, runs = list()), big, num_cores = 99)$plan$num_cores == 15 &&
    any(grepl("Requested workers exceed", finn_plan_with_others(200, 0, FALSE,
      list(cores = 0L, gb = 0, runs = list()), big, num_cores = 99)$plan$warnings)))
  check("never both parallel modes", all(vapply(list(s1, s2, s3, s4, s5), function(s) !(identical(s$parallel_processing, "local_machine") && s$inner_parallel), logical(1))))

  # ---- Sharing the computer across projects -----------------------------------
  check("worker memory estimate", finn_worker_gb(0) == 0.3 && finn_worker_gb(1) == 0.3 && finn_worker_gb(NULL) == 0.3)
  cl <- finn_plan_claim(s1, 1)
  check("claim records workers and memory", cl$claimed_cores == 15 && cl$claimed_gb == 4.5)
  check("sequential plan claims one core", finn_plan_claim(s5, 0)$claimed_cores == 1)
  none <- list(cores = 0L, gb = 0, runs = list())
  two <- list(cores = 8L, gb = 20, runs = list(list(), list()))
  rem <- finn_remaining_resources(list(cores = 16, free_gb = 30, total_gb = 32), two)
  check("remaining subtracts shared budgets once", rem$cpu_budget == 7 && abs(rem$ram_budget_gb - 5.6) < 1e-9 && rem$free_gb == 30)
  check("remaining with unknown memory stays conservative", finn_remaining_resources(list(cores = 16, free_gb = NA, total_gb = NA),
    list(cores = 2L, gb = 3, runs = list(1)))$cpu_budget == 0)
  alone <- finn_plan_with_others(200, 0, FALSE, none, big)
  check("no other runs: plan unchanged", alone$fits && !alone$shared && alone$plan$num_cores == s1$num_cores && identical(alone$plan$parallel_processing, "local_machine"))
  check("lone run never refused, even with little memory", finn_plan_with_others(200, 0, FALSE, none, res(2, 1))$fits)
  shared <- finn_plan_with_others(200, 0, FALSE, list(cores = 8L, gb = 16, runs = list(list())), list(cores = 16, free_gb = 34, total_gb = 34))
  check("other runs reduce workers", shared$fits && shared$shared && shared$plan$num_cores < s1$num_cores && grepl("Shares this computer", shared$plan$reason))
  full <- finn_plan_with_others(200, 0, FALSE, list(cores = 16L, gb = 10, runs = list(list())), list(cores = 16, free_gb = 30, total_gb = 32))
  check("no cores left refuses", !full$fits)
  check("no memory left refuses", !finn_plan_with_others(200, 2, FALSE, list(cores = 2L, gb = 28, runs = list(list())), list(cores = 16, free_gb = 30, total_gb = 32))$fits)
  check("last core permits only sequential", is.null(finn_plan_with_others(200, 0, FALSE, list(cores = 14L, gb = 4.2, runs = list(list())), big)$plan$num_cores))
  check("config-forced sequential still claims one core", finn_plan_with_others(200, 0, FALSE, none, big, sequential = TRUE)$plan$claimed_cores == 1)

  # ---- Data shape detection and reshaping ----------------------------------------
  check("header dates: text and Excel serials", identical(
    finn_header_dates(c("Region", "2024-01-01", "45323", "...4")),
    as.Date(c(NA, "2024-01-01", "2024-02-01", NA))
  ))
  titled <- data.frame(a = c("Region", "East", "Total"), b = c("Product", "A", "All"), c = c("2024-01-01", "1", "1"), stringsAsFactors = FALSE)
  names(titled) <- c("Sales Report", "", "")
  types_of <- function(df) vapply(finn_shape_issues(df), `[[`, character(1), "type")
  check("shape: title rows found", "title_rows" %in% types_of(titled) &&
    identical(finn_shape_issues(titled)[[1]]$fix$header_row, 1L))
  wide <- data.frame(Region = c("East", "West"), `2024-01-01` = 1:2, `2024-02-01` = 3:4, `2024-03-01` = 5:6, check.names = FALSE)
  wi <- finn_shape_issues(wide)
  check("shape: wide dates found with id columns", identical(types_of(wide), "wide_dates") && identical(wi[[1]]$fix$id_columns, "Region"))
  long_ok <- data.frame(Region = "East", Date = "2024-01-01", Value = 1)
  check("shape: long data has no issues", length(finn_shape_issues(long_ok)) == 0)
  check("shape: total rows found", "total_rows" %in% types_of(data.frame(Region = c("East", "Grand Total"), Value = 1:2)))
  check("shape: 'Totally Fresh' is not a total", length(finn_shape_issues(data.frame(Region = c("East", "Totally Fresh"), Value = 1:2))) == 0)
  titled2 <- data.frame(a = c("Region", "East", "West", "Total"), b = c("Product", "A", "B", "All"), c = c("2024-01-01", "1", "2", "3"), stringsAsFactors = FALSE)
  names(titled2) <- c("Sales Report", "", "")
  rs <- finn_reshape_data(titled2, header_row = 1, drop_totals = TRUE)
  check("reshape: header promoted and totals dropped", identical(names(rs$data)[1:2], c("Region", "Product")) &&
    !any(grepl("Total", rs$data$Region)) && nrow(rs$data) == 2)
  rw <- finn_reshape_data(wide, pivot_longer = TRUE)
  check("reshape: pivot gives one row per series and date", identical(names(rw$data), c("Region", "Date", "Value")) &&
    nrow(rw$data) == 6 && identical(rw$data$Value[rw$data$Region == "West"], c(2L, 4L, 6L)) && inherits(rw$data$Date, "Date"))
  check("reshape: pivot keeps values unchanged", sum(rw$data$Value) == sum(wide[, -1]))
  check("reshape: notes describe changes", length(rw$notes) >= 1)

  # ---- Data-quality warnings never block ------------------------------------------
  qd <- data.frame(
    Region = rep(c("East", "Dead"), each = 4), Date = rep(as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01")), 2),
    Revenue = c(1, 2, 3, NA, 0, 0, 0, NA), Price = c(1, 1, 1, NA, 1, 1, 1, 1)
  )
  qcfg <- finn_merge_config(list(combo_variables = "Region", target_variable = "Revenue", date_type = "month", external_regressors = "Price"))
  qf <- finn_quality_flags(qd, qcfg, qd$Region, list(last_actual_date = "2024-03-01"))
  check("quality: all-zero series flagged", any(grepl("only zeros", qf)))
  check("quality: future rows suggest hist_end_date", any(grepl("set hist_end_date", qf)))
  check("quality: blank future driver flagged", any(grepl("blank future values", qf)))
  qf2 <- finn_quality_flags(qd, modifyList(qcfg, list(hist_end_date = "2024-02-01")), qd$Region, list(last_actual_date = "2024-03-01"))
  check("quality: actuals after hist_end_date flagged", any(grepl("after hist_end_date", qf2)))
  qbase <- list(project_name = "Q", combo_variables = "Region", target_variable = "Revenue", date_type = "month", forecast_horizon = 1, external_regressors = "Price")
  qchk <- finn_check_config(qbase, qd)
  check("quality: flags are warnings, not errors", !any(grepl("only zeros|blank future", qchk$errors)) && any(grepl("only zeros", qchk$warnings)))

  # ---- Latest period that has not ended yet ----------------------------------------
  oct20 <- as.Date("2026-10-20")
  ip <- finn_incomplete_period("2026-10-01", "month", as.Date(c("2026-08-01", "2026-09-01", "2026-10-01")), today = oct20)
  check("incomplete: a month in progress is flagged with the previous month as the history end",
    identical(ip$period_end, "2026-10-31") && identical(ip$suggested_hist_end_date, "2026-09-01") &&
      grepl("day 20 of 31", ip$message) && grepl("Set `hist_end_date` to `2026-09-01`", ip$message, fixed = TRUE))
  ipe <- finn_incomplete_period("2026-10-31", "month", today = oct20)
  check("incomplete: a month-end date covers the month it ends, and without earlier dates asks to confirm",
    identical(ipe$period_start, "2026-10-01") && is.null(ipe$suggested_hist_end_date) && grepl("Confirm it is complete", ipe$message))
  check("incomplete: quarter and year periods",
    identical(finn_incomplete_period("2026-10-01", "quarter", today = as.Date("2026-12-31"))$period_end, "2026-12-31") &&
      identical(finn_incomplete_period("2026-12-31", "quarter", today = as.Date("2026-11-15"))$period_start, "2026-10-01") &&
      identical(finn_incomplete_period("2026-01-01", "year", today = oct20)$period_end, "2026-12-31"))
  check("incomplete: ended or future periods, week and day data, and bad dates are not flagged",
    is.null(finn_incomplete_period("2026-09-01", "month", today = oct20)) && is.null(finn_incomplete_period("2026-11-01", "month", today = oct20)) &&
      is.null(finn_incomplete_period("2026-10-19", "week", today = oct20)) && is.null(finn_incomplete_period("2026-10-20", "day", today = oct20)) &&
      is.null(finn_incomplete_period(NA, "month", today = oct20)) && is.null(finn_incomplete_period("bad", "month", today = oct20)) &&
      is.null(finn_incomplete_period(NULL, "month", today = oct20)))
  idat <- data.frame(Region = "East", Date = as.Date(c("2026-08-01", "2026-09-01", "2026-10-01")), Revenue = c(1, 2, 3))
  ibase <- list(project_name = "I", combo_variables = "Region", target_variable = "Revenue", date_type = "month", forecast_horizon = 1, fiscal_year_start = 1)
  ichk <- finn_check_config(ibase, idat, today = oct20)
  check("incomplete: the config check warns but never blocks",
    any(grepl("has not ended yet", ichk$warnings)) && !any(grepl("has not ended", ichk$errors)) &&
      identical(ichk$facts$incomplete_period$suggested_hist_end_date, "2026-09-01"))
  check("incomplete: no warning once the period has ended",
    !any(grepl("has not ended yet", finn_check_config(ibase, idat, today = as.Date("2026-11-02"))$warnings)))
  ileft <- modifyList(ibase, list(hist_end_date = "2026-09-01"))
  ileft_chk <- finn_check_config(ileft, idat, today = oct20)
  check("incomplete: a hist_end_date before the period silences the warning",
    !any(grepl("has not ended yet", ileft_chk$warnings)) && !is.null(ileft_chk$facts$incomplete_period))
  irec <- finn_recommend_settings(finn_merge_config(ibase), ichk$facts)$recommendations
  check("incomplete: the settings review suggests ending history at the last complete period",
    identical(irec[[1]]$key, "hist_end_date") && identical(irec[[1]]$value, "2026-09-01") && grepl("has not ended yet", irec[[1]]$reason))
  check("incomplete: no suggestion once hist_end_date leaves the period out",
    !any(vapply(finn_recommend_settings(finn_merge_config(ileft), ileft_chk$facts)$recommendations, `[[`, character(1), "key") == "hist_end_date"))

  # ---- Versions, runtime estimate, host, disk usage --------------------------------
  v <- finn_versions()
  check("versions: R version recorded", grepl("^[0-9]+\\.[0-9]+", v$r_version))
  check("other host: same host is local", !finn_other_host(list(host = Sys.info()[["nodename"]])) && !finn_other_host(list()))
  check("other host: different host detected", finn_other_host(list(host = "some-other-pc")))
  e1 <- finn_estimate_runtime(100, "standard", 1)
  check("estimate: more workers is faster", finn_estimate_runtime(100, "standard", 10)$high_min < e1$high_min)
  check("estimate: iterate takes longer", finn_estimate_runtime(100, "iterate", 1)$high_min > e1$high_min)
  check("estimate: capped and readable", finn_estimate_runtime(1e7)$high_min <= 72 * 60 && grepl("hours", finn_estimate_runtime(1e7)$text))
  check("estimate: NULL inputs are safe", grepl("minutes", finn_estimate_runtime(NULL)$text))
  check("estimate: equal range ends show one value", identical(finn_estimate_runtime(1, "standard", 100)$text, "roughly 4 minutes"))
  du_paths <- finn_project_paths(file.path(tmp, "root"), "Disk")
  finn_ensure_project_dirs(du_paths)
  writeBin(raw(2 * 1024^2), file.path(du_paths$input, "big.bin"))
  du <- finn_disk_usage(du_paths)
  check("disk usage counts files", du$total_mb >= 2 && length(du$folders) == 5)
})

# ---- Project setup ----------------------------------------------------------------
setup <- load_functions("setup_project.R", common)
with(setup, {
  root <- file.path(tmp, "setup_root")
  src_dir <- file.path(tmp, "setup_src")
  dir.create(src_dir, showWarnings = FALSE)
  f <- file.path(src_dir, "sales.csv")
  writeLines(c("Date,Value", "2024-01-01,1"), f)
  r1 <- setup_create(list(root = root, project = "Setup", data = f))
  check("setup: create copies data and stamps versions", isTRUE(r1$ok) &&
    !is.null(finn_read_project(finn_project_paths(root, "Setup"))$r_version))
  writeLines(c("Date,Value", "2024-01-01,1", "2024-02-01,2"), f)
  r2 <- setup_create(list(root = root, project = "Setup", data = f))
  check("setup: different file with same name needs confirmation", identical(r2$status, "needs_confirmation"))
  r3 <- setup_create(list(root = root, project = "Setup", data = f, replace_data = "true"))
  paths <- finn_project_paths(root, "Setup")
  check("setup: replace_data keeps the old copy", isTRUE(r3$ok) && length(list.files(paths$input)) == 2 &&
    !identical(finn_read_project(paths)$data_file, "input/sales.csv"))
  du <- setup_disk_usage(list(root = root, project = "Setup"))
  check("setup: disk_usage is read-only and reports size", isTRUE(du$ok) && length(list.files(paths$input)) == 2)
  copy_args <- list(root = root, project = "CopyFail", data = f)
  check("setup: failed copy never writes project metadata",
    errors_with(setup_create(copy_args, copy = function(...) FALSE), "Could not copy") &&
      is.null(finn_read_project(finn_project_paths(root, "CopyFail"))))
  before <- finn_read_project(paths)
  writeLines("different input", f)
  check("setup: failed replacement preserves existing project metadata",
    errors_with(setup_create(list(root = root, project = "Setup", data = f, replace_data = "true"),
      copy = function(...) FALSE), "Could not copy") &&
      identical(finn_read_project(paths), before))
})

# ---- Data preparation script ---------------------------------------------------------
tx <- new.env(parent = common)
sys.source(file.path(scripts_dir, "finn_prep.R"), envir = tx)
prep_env <- load_functions("prepare_data.R", tx)
with(prep_env, {
  d <- file.path(tmp, "prep_out")
  dir.create(d, showWarnings = FALSE)
  first <- prepare_out_path(d, "x.csv")
  writeLines("a", first)
  check("prepare: never reuses an existing file name", !identical(prepare_out_path(d, "x.csv"), first))

  sd <- data.frame(R = rep(c("a", "b", "c", "d", "e"), each = 3), P = "x", Value = 1:15)
  s1 <- prepare_sample_series(sd, c("R", "P"), 2)
  s2 <- prepare_sample_series(sd, c("R", "P"), 2)
  check("prepare: sample keeps n whole series", length(unique(s1$data$R)) == 2 && nrow(s1$data) == 6)
  check("prepare: sample is reproducible", identical(s1$data, s2$data))
  set.seed(1)
  before <- runif(1)
  set.seed(1)
  prepare_sample_series(sd, c("R", "P"), 2)
  check("prepare: sample leaves the caller's random state alone", identical(runif(1), before))
  check("prepare: asking for more series than exist keeps all", nrow(prepare_sample_series(sd, "R", 50)$data) == 15)
  check("prepare: sample needs real columns", errors_with(prepare_sample_series(sd, "Nope", 2), "Columns not found"))
  check("prepare: sample size must be positive", errors_with(prepare_sample_series(sd, "R", "0"), "positive whole number"))
})

# ---- Transform specs and the verification gate --------------------------------------
tx_dir <- file.path(tmp, "tx")
dir.create(tx_dir, showWarnings = FALSE)

#' Write a fixture file for transform tests
#'
#' @param name File name.
#' @param lines Lines to write.
#' @return Path.
tx_put <- function(name, lines) {
  p <- file.path(tx_dir, name)
  writeLines(lines, p)
  p
}

#' Fake finnts validator that passes, standing in when finnts is not installed
#'
#' @param input_data Prepared data.
#' @param ... Other validator arguments.
#' @return Validator result list.
tx_pass_validator <- function(input_data, ...) {
  list(passed = TRUE, checks = data.frame(Rule = "R1", Description = "fake", Status = "pass", Message = "ok", stringsAsFactors = FALSE), summary = list(), details = list())
}

#' Fake validator that fails one rule
#'
#' @param rule Rule id.
#' @return Validator function.
tx_fail_validator <- function(rule) {
  force(rule)
  function(input_data, ...) {
    list(passed = FALSE, checks = data.frame(Rule = rule, Description = "fake", Status = "fail", Message = paste(rule, "failed"), stringsAsFactors = FALSE), summary = list(), details = list())
  }
}

#' Build a transform spec
#'
#' @param steps Step JSON strings.
#' @param settings Settings JSON object text.
#' @param extra Extra top-level JSON members, starting with a comma.
#' @param description Plan description.
#' @return Spec JSON text.
tx_spec <- function(steps, settings = '{"combo_variables":["Region","Product"],"target_variable":"Value","date_type":"month","forecast_horizon":3}', extra = "", description = "test plan") {
  sprintf('{"version":1,"description":"%s","settings":%s,"steps":[%s]%s}', description, settings, paste(steps, collapse = ","), extra)
}

#' Gate statuses by rule
#'
#' @param r Result of finn_tx_run().
#' @return Named character vector.
tx_status <- function(r) stats::setNames(r$gate$checks$Status, r$gate$checks$Rule)

tx_region <- '{"combo_variables":["Region"],"target_variable":"Value","date_type":"month","forecast_horizon":3}'
tx_tidy <- tx_put("tidy.csv", c(
  "Region,Date,Value", "East,2024-01-01,10", "East,2024-02-01,11", "East,2024-03-01,12",
  "West,2024-01-01,20", "West,2024-02-01,21", "West,2024-03-01,22"
))
tx_tidy_steps <- c('{"op":"use_header_rows","rows":[1]}', '{"op":"parse_numbers","columns":["Value"]}', '{"op":"parse_dates","column":"Date"}')
tx_outline <- tx_put("outline.csv", c(
  "Sales Report,,,,,", "Filter: All,,,,,", ",,,,,",
  "Region,Product,Jan 2024,Feb 2024,Mar 2024,Total",
  "East,A,\"1,000\",1.2K,(50),2150", ",B,200,300,400,900", "East Total,,1200,1500,350,3050",
  "West,A,10,20,30,60", "West Total,,10,20,30,60", "Grand Total,,1210,1520,380,3110",
  "Note: values in USD,,,,,"
))
tx_outline_steps <- c(
  '{"op":"use_header_rows","rows":[4]}', '{"op":"drop_rows","rows":[11],"reason":"footnote"}',
  '{"op":"fill_down","columns":["Region"]}', '{"op":"drop_total_rows","columns":["Region","Product"]}',
  '{"op":"drop_total_columns","columns":["Total"]}', '{"op":"pivot_longer","id_columns":["Region","Product"]}',
  '{"op":"parse_numbers","columns":["Value"]}', '{"op":"parse_dates","column":"Date"}'
)
# Filtered pivot with a blank first column, outline labels, and a lone Total
# value column, like an exported Excel pivot.
tx_filtered <- tx_put("filtered_pivot.csv", c(
  ",Adjustment Type,(Multiple Items),", ",Anomaly Flag,No,", ",,,", ",Revenue by area,,",
  ",Area,Fiscal Month,Total",
  ",North,2026-07-01,100", ",,2026-08-01,110", ",,2026-09-01,120", ",,2026-10-01,40",
  ",Grand Total,,370"
))
tx_recent <- tx_put("recent.csv", c("Region,Date,Value", "East,2026-08-01,10", "East,2026-09-01,11", "East,2026-10-01,3"))
tx_oct20 <- as.Date("2026-10-20")

with(tx, {
  run <- function(file, spec, validator = tx_pass_validator, ...) finn_tx_run(file, spec, validator = validator, ...)

  # Positive fixtures.
  md5_before <- unname(tools::md5sum(tx_outline))
  r <- run(tx_outline, tx_spec(tx_outline_steps))
  out <- r$gate$output
  check("transform: outline pivot passes the gate with a token", isTRUE(r$gate$passed) && nchar(r$token) == 16)
  check("transform: outline pivot becomes one row per series and month", nrow(out) == 9 &&
    identical(names(out), c("Region", "Product", "Date", "Value")) && !any(out$Region %in% c("", NA)))
  check("transform: K suffix, thousands separators, and parentheses are read",
    identical(out$Value[out$Region == "East" & out$Product == "A"], c(1000, 1200, -50)))
  check("transform: leaf values add up to the grand total", sum(out$Value) == 3110)
  check("transform: subtotals, totals, and the total column reconcile", tx_status(r)[["P9c"]] == "pass" && length(r$gate$totals) >= 3)
  check("transform: every dropped row and column is explained", tx_status(r)[["P9a"]] == "pass" && tx_status(r)[["P9b"]] == "pass")
  check("transform: the source file is never changed", identical(unname(tools::md5sum(tx_outline)), md5_before))
  sections <- c(
    "## Proposed data preparation", "**Source:**", "**Result:**", "**Checked against settings:**", "**Target total:**",
    "### Changes", "### Needs your extra confirmation", "### Dropped rows", "### Before", "### After", "### Verification", "**Overall: PASSED.**"
  )
  pos <- vapply(sections, function(s) regexpr(s, r$preview_markdown, fixed = TRUE)[1], integer(1))
  check("transform: preview has the fixed sections in order", all(pos > 0) && !is.unsorted(pos))
  check("transform: preview masks labels and values by default",
    !grepl("East|West|Sales Report|1,000", r$preview_markdown) && grepl("text_[0-9]+ Total", r$preview_markdown) && grepl("#,###", r$preview_markdown))
  check("transform: labels are shown only when revealed", grepl("East", run(tx_outline, tx_spec(tx_outline_steps), reveal_labels = TRUE)$preview_markdown))
  set.seed(42)
  draw <- runif(1)
  set.seed(42)
  r2 <- run(tx_outline, tx_spec(tx_outline_steps))
  check("transform: runs are deterministic and leave the random state alone",
    identical(r$token, r2$token) && identical(finn_tx_csv_lines(out), finn_tx_csv_lines(r2$gate$output)) && identical(runif(1), draw))
  check("transform: token changes with the spec or settings",
    !identical(r$token, run(tx_outline, tx_spec(tx_outline_steps, description = "other"))$token) &&
      !identical(r$token, run(tx_outline, tx_spec(tx_outline_steps, settings = '{"combo_variables":["Region","Product"],"target_variable":"Value","date_type":"month","forecast_horizon":6}'))$token))

  seen <- new.env()
  spy <- function(...) {
    seen$args <- list(...)
    tx_pass_validator(...)
  }
  run(tx_outline, tx_spec(tx_outline_steps, settings = '{"combo_variables":["Region","Product"],"target_variable":"Value","date_type":"month","forecast_horizon":3,"fiscal_year_start":7,"clean_missing_values":true}'), spy)
  check("transform: the gate passes the real run settings to the finnts validator",
    identical(seen$args$combo_variables, c("Region", "Product")) && identical(seen$args$target_variable, "Value") &&
      identical(seen$args$date_type, "month") && seen$args$forecast_horizon == 3 && seen$args$fiscal_year_start == 7 &&
      isTRUE(seen$args$clean_missing_values) && inherits(seen$args$input_data$Date, "Date") && is.character(seen$args$input_data$Region))

  compact <- tx_put("compact.csv", c(
    "Row Labels,Jan 2024,Feb 2024,Mar 2024", "East,,,", "A,1,2,3", "B,4,5,6", "East Total,5,7,9",
    "West,,,", "A,7,8,9", "West Total,7,8,9", "Grand Total,12,15,18"
  ))
  rc <- run(compact, tx_spec(c(
    '{"op":"use_header_rows","rows":[1]}', '{"op":"split_compact_labels","column":"Row Labels","into":["Region","Product"]}',
    '{"op":"drop_total_rows","columns":["Region","Product"]}', '{"op":"pivot_longer","id_columns":["Region","Product"]}',
    '{"op":"parse_numbers","columns":["Value"]}', '{"op":"parse_dates","column":"Date"}'
  )))
  check("transform: compact pivot splits group labels and reconciles totals",
    isTRUE(rc$gate$passed) && nrow(rc$gate$output) == 9 && sum(rc$gate$output$Value) == 45 && tx_status(rc)[["P9c"]] == "pass")
  check("transform: change counts of one use the singular", grepl("| 1 column |", rc$preview_markdown, fixed = TRUE) &&
    !grepl("\\b1 (rows|columns|values)\\b", rc$preview_markdown))

  tabular <- tx_put("tabular.csv", c(
    "Region,Product,Jan 2024,Feb 2024,Mar 2024", "East,A,1,2,3", "East,B,4,5,6", "East Total,,5,7,9",
    "West,A,7,8,9", "West Total,,7,8,9", "Grand Total,,12,15,18"
  ))
  rt <- run(tabular, tx_spec(c(
    '{"op":"use_header_rows","rows":[1]}', '{"op":"drop_total_rows","columns":["Region","Product"]}',
    '{"op":"pivot_longer","id_columns":["Region","Product"]}', '{"op":"parse_numbers","columns":["Value"]}', '{"op":"parse_dates","column":"Date"}'
  )))
  check("transform: tabular pivot passes", isTRUE(rt$gate$passed) && nrow(rt$gate$output) == 9)

  long_totals <- tx_put("long_totals.csv", c(
    "Region,Date,Value",
    "East,2024-01-01,10", "West,2024-01-01,20", "Total,2024-01-01,30",
    "East,2024-02-01,10", "West,2024-02-01,20", "Total,2024-02-01,30",
    "East,2024-03-01,10", "West,2024-03-01,20", "Total,2024-03-01,30",
    "Grand Total,,90"
  ))
  long_steps <- c(
    '{"op":"use_header_rows","rows":[1]}', '{"op":"drop_total_rows","columns":["Region"]}',
    '{"op":"parse_numbers","columns":["Value"]}', '{"op":"parse_dates","column":"Date"}'
  )
  rlt <- run(long_totals, tx_spec(long_steps, settings = tx_region))
  long_cmp <- rlt$gate$totals
  check("transform: long-table totals are compared with parts from the same period",
    isTRUE(rlt$gate$passed) && tx_status(rlt)[["P9c"]] == "pass" && nrow(rlt$gate$output) == 6 &&
      identical(unique(long_cmp$column), "Value") &&
      isTRUE(all.equal(long_cmp$sum_of_parts[long_cmp$label == "Total"], c(30, 30, 30))))
  check("transform: a long-table total with no period is compared with every period",
    isTRUE(all.equal(long_cmp$sum_of_parts[long_cmp$label == "Grand Total"], 90)))
  long_wrong <- tx_put("long_totals_wrong.csv", sub("^Total,2024-02-01,30$", "Total,2024-02-01,31", readLines(long_totals)))
  check("transform: a wrong long-table total in one period still fails reconciliation",
    tx_status(run(long_wrong, tx_spec(long_steps, settings = tx_region)))[["P9c"]] == "fail")

  two_row <- tx_put("two_row.csv", c(",,2023,,2024,", "Region,Product,Nov,Dec,Jan,Feb", "East,A,1,2,3,4", "West,A,5,6,7,8"))
  r2r <- run(two_row, tx_spec(c(
    '{"op":"use_header_rows","rows":[1,2],"sep":" "}', '{"op":"pivot_longer","id_columns":["Region","Product"]}',
    '{"op":"parse_numbers","columns":["Value"]}', '{"op":"parse_dates","column":"Date"}'
  )))
  check("transform: merged two-row year/month headers become dates",
    isTRUE(r2r$gate$passed) && identical(sort(unique(format(r2r$gate$output$Date))), c("2023-11-01", "2023-12-01", "2024-01-01", "2024-02-01")))

  tables <- tx_put("tables.csv", c(
    "Region,Date,Value", "East,2024-01-01,10", "East,2024-02-01,11", "East,2024-03-01,12", ",,",
    "Region,Date,Value", "West,2024-01-01,20", "West,2024-02-01,21", "West,2024-03-01,22", "* Preliminary figures,,"
  ))
  rtb <- run(tables, tx_spec(c(
    '{"op":"use_header_rows","rows":[1]}', '{"op":"drop_rows","rows":[6],"reason":"header of the second table"}',
    '{"op":"drop_rows","rows":[10],"reason":"footnote"}', '{"op":"parse_numbers","columns":["Value"]}', '{"op":"parse_dates","column":"Date"}'
  ), settings = tx_region))
  check("transform: stacked tables with a footnote pass", isTRUE(rtb$gate$passed) && nrow(rtb$gate$output) == 6 && sum(rtb$gate$output$Value) == 96)

  money <- tx_put("money.csv", c(
    "Region,Date,Value", "East,2024-01-01,\"$1,234.50\"", "East,2024-02-01,(100)", "East,2024-03-01,1.5M",
    "West,2024-01-01,n/a", "West,2024-02-01,\u2014", "West,2024-03-01,\"\u20ac2,000\""
  ))
  rm1 <- run(money, tx_spec(c(tx_tidy_steps[1], '{"op":"parse_numbers","columns":["Value"],"missing":["\u2014"]}', tx_tidy_steps[3]), settings = tx_region))
  check("transform: currency, parentheses, suffixes, and no-value text are read",
    identical(rm1$gate$output$Value, c(1234.5, -100, 1.5e6, NA, NA, 2000)) && isTRUE(rm1$gate$passed))
  check("transform: unlisted text in a number column stops the step",
    errors_with(run(money, tx_spec(tx_tidy_steps, settings = tx_region)), "not numbers"))

  units <- tx_put("units.csv", c(
    "Region,Date,Value,Unit", "East,2024-01-01,1,USD k", "East,2024-02-01,2,USD k", "East,2024-03-01,3000,USD",
    "West,2024-01-01,5,USD k", "West,2024-02-01,6,USD k", "West,2024-03-01,7,USD k"
  ))
  ru <- run(units, tx_spec(c(
    tx_tidy_steps, '{"op":"scale","column":"Value","factor":1000,"reason":"thousands to dollars","where":{"column":"Unit","equals":"USD k"}}',
    '{"op":"drop_columns","columns":["Unit"],"reason":"unit is now USD everywhere"}'
  ), settings = tx_region))
  check("transform: a unit change is converted and needs extra confirmation",
    isTRUE(ru$gate$passed) && identical(ru$gate$output$Value, c(1000, 2000, 3000, 5000, 6000, 7000)) &&
      length(ru$ctx$confirm) == 1 && ru$gate$counts$unit_converted == 5 && !grepl("- None.", ru$preview_markdown, fixed = TRUE))

  mixed <- tx_put("mixed.csv", c("Region,Date,Value", "East,Jan 2024,1", "East,2024-02,2", "East,03/2024,3"))
  rmx <- run(mixed, tx_spec(tx_tidy_steps, settings = tx_region))
  check("transform: mixed month formats read value by value",
    isTRUE(rmx$gate$passed) && identical(format(rmx$gate$output$Date), c("2024-01-01", "2024-02-01", "2024-03-01")))
  ambiguous <- tx_put("ambiguous.csv", c("Region,Date,Value", "East,01/02/2024,1", "East,01/03/2024,2"))
  check("transform: ambiguous day/month dates stop until a format is given",
    errors_with(run(ambiguous, tx_spec(tx_tidy_steps, settings = tx_region)), "(?i)ambiguous|format") &&
      isTRUE(run(ambiguous, tx_spec(c(tx_tidy_steps[1:2], '{"op":"parse_dates","column":"Date","format":"%d/%m/%Y"}'), settings = '{"combo_variables":["Region"],"target_variable":"Value","date_type":"month","forecast_horizon":3}'))$gate$passed))

  fiscal <- tx_put("fiscal.csv", c("Region,Period,Value", "East,FY24 P01,1", "East,FY24 P02,2", "East,FY24 P03,3"))
  rf <- run(fiscal, tx_spec(c(
    tx_tidy_steps[1:2], '{"op":"fiscal_to_date","column":"Period","pattern":"FY(?<year>[0-9]{2})\\\\s*P(?<period>[0-9]{2})","fiscal_year_start":7,"into":"Date"}',
    '{"op":"drop_columns","columns":["Period"],"reason":"replaced by Date"}'
  ), settings = tx_region))
  check("transform: fiscal periods become calendar months", isTRUE(rf$gate$passed) && identical(format(rf$gate$output$Date)[1], "2023-07-01"))

  dupes <- tx_put("dupes.csv", c("Region,Date,Value", "East,2024-01-01,1", "East,2024-01-01,2", "East,2024-02-01,3", "East,2024-03-01,4"))
  rd <- run(dupes, tx_spec(c(tx_tidy_steps, '{"op":"aggregate_duplicates","keys":["Region","Date"],"columns":["Value"],"reason":"two invoices in one month"}'), settings = tx_region))
  check("transform: duplicate keys are summed with extra confirmation and conserve the total",
    isTRUE(rd$gate$passed) && identical(rd$gate$output$Value, c(3, 3, 4)) && length(rd$ctx$confirm) == 1 && tx_status(rd)[["P9b"]] == "pass")

  # Negative fixtures: spec rules.
  check("transform: unknown ops are rejected", errors_with(finn_tx_read_spec('{"version":1,"steps":[{"op":"read_file"}]}'), "unknown op"))
  check("transform: file or address parameters are rejected",
    errors_with(finn_tx_read_spec('{"version":1,"steps":[{"op":"drop_rows","rows":[1],"reason":"x","path":"/tmp/x"}]}'), "unknown parameters") &&
      errors_with(finn_tx_read_spec('{"version":1,"steps":[{"op":"parse_dates","column":"Date","url":"http://x"}]}'), "unknown parameters"))
  check("transform: unknown top-level keys and missing version are rejected",
    errors_with(finn_tx_read_spec('{"version":1,"code":"x","steps":[]}'), "Unknown spec keys") &&
      errors_with(finn_tx_read_spec('{"steps":[]}'), "version"))
  check("transform: a spec without required settings is rejected",
    errors_with(run(tx_tidy, tx_spec(tx_tidy_steps, settings = '{"target_variable":"Value"}')), "needs the run settings"))

  # Negative fixtures: gate rules.
  rules <- c(paste0("R", 1:11), paste0("P", 1:8))
  check("transform: every finnts rule failure (R1-R11, P1-P8) fails the gate", all(vapply(rules, function(rule) {
    g <- run(tx_tidy, tx_spec(tx_tidy_steps, settings = tx_region), tx_fail_validator(rule))
    !isTRUE(g$gate$passed) && is.null(g$token) && tx_status(g)[[rule]] == "fail" && grepl("**Overall: FAILED.**", g$preview_markdown, fixed = TRUE)
  }, logical(1))))
  nv <- run(tx_tidy, tx_spec(tx_tidy_steps, settings = tx_region), NULL)
  check("transform: the gate fails closed without the finnts validator", !isTRUE(nv$gate$passed) && is.null(nv$token))
  ev <- run(tx_tidy, tx_spec(tx_tidy_steps, settings = tx_region), function(...) stop("boom"))
  check("transform: a validator error fails the gate", !isTRUE(ev$gate$passed) && tx_status(ev)[["R/P"]] == "fail")
  lie <- run(tx_tidy, tx_spec(tx_tidy_steps, settings = tx_region), function(...) modifyList(tx_pass_validator(), list(passed = FALSE)))
  check("transform: a validator that does not pass fails the gate even without a failed row", !isTRUE(lie$gate$passed))
  check("transform: a result without parsed dates fails S1",
    tx_status(run(tx_tidy, tx_spec(tx_tidy_steps[1:2], settings = tx_region)))[["S1"]] == "fail")

  gate_after <- function(edit) {
    spec <- finn_tx_read_spec(tx_spec(tx_tidy_steps, settings = tx_region))
    cfg <- finn_tx_settings(NULL, spec$settings)
    ctx <- finn_tx_load(tx_tidy, spec)
    finn_tx_apply(ctx, spec)
    edit(ctx)
    finn_tx_gate(ctx, spec, cfg, tx_pass_validator)
  }
  g_drop <- gate_after(function(ctx) ctx$d <- ctx$d[-1, ])
  check("transform: an unexplained dropped row fails P9a", !g_drop$passed && g_drop$checks$Status[g_drop$checks$Rule == "P9a"] == "fail")
  g_change <- gate_after(function(ctx) ctx$d$Value[1] <- ctx$d$Value[1] + 1)
  check("transform: a changed target value fails P9b", !g_change$passed && g_change$checks$Status[g_change$checks$Rule == "P9b"] == "fail")

  bad_total <- tx_put("bad_total.csv", sub("^East Total,,1200", "East Total,,1250", readLines(tx_outline)))
  rbt <- run(bad_total, tx_spec(tx_outline_steps))
  check("transform: a subtotal that does not match its rows fails P9c", !isTRUE(rbt$gate$passed) && tx_status(rbt)[["P9c"]] == "fail")
  check("transform: the failed subtotal check shows masked labels in the preview",
    !grepl("East", rbt$preview_markdown, fixed = TRUE) && grepl("first: 'text_[0-9]+ Total'", rbt$preview_markdown))
  check("transform: revealing labels shows the failed subtotal check as written",
    grepl("first: 'East Total'", run(bad_total, tx_spec(tx_outline_steps), reveal_labels = TRUE)$preview_markdown, fixed = TRUE))
  check("transform: the display checks mask the messages while the gate keeps them as written",
    any(grepl("East Total", rbt$gate$checks$Message, fixed = TRUE)) && !any(grepl("East", rbt$display_checks$Message, fixed = TRUE)) &&
      identical(rbt$display_checks[c("Rule", "Description", "Status")], rbt$gate$checks[c("Rule", "Description", "Status")]))
  rbt_shown <- run(bad_total, tx_spec(tx_outline_steps), reveal_labels = TRUE)
  check("transform: revealing labels returns the display checks as written", identical(rbt_shown$display_checks, rbt_shown$gate$checks))
  check("transform: message labels are the prepared text values and dropped totals, not column names",
    all(c("East", "West", "East Total", "Grand Total") %in% finn_tx_message_labels(rbt)) &&
      !any(c("Region", "Product", "Value", "Date") %in% finn_tx_message_labels(rbt)))

  # Messages are masked by finding the known labels as whole words in the
  # original text.
  mask_msg <- function(text, known, labels = new.env(parent = emptyenv())) finn_tx_mask_text(text, known, labels)
  check("mask text: a label is replaced as a whole word only",
    identical(mask_msg("Series East--Eastern has 3 periods", "East"), "Series text_1--Eastern has 3 periods"))
  check("mask text: the longer label wins and shares the token of its base name",
    identical(mask_msg("first: 'East Total' and East", c("East", "East Total")), "first: 'text_1 Total' and text_1"))
  check("mask text: numbers, dates, and total words stay as written",
    identical(mask_msg("Series 001--East has 12 periods from Jan 2024; Total", c("001", "East", "12", "Jan 2024", "Total")),
      "Series 001--text_1 has 12 periods from Jan 2024; Total"))
  check("mask text: punctuation, underscores, and non-ASCII letters belong to a label",
    identical(mask_msg("R&D, O'Neil, North_America, Caf\u00e9, Caf\u00e9s and Caf", c("R&D", "O'Neil", "North_America", "Caf\u00e9", "Caf")),
      "text_1, text_2, text_3, text_4, Caf\u00e9s and text_5"))
  check("mask text: a label spelled like a token is not mixed up with a real token",
    identical(mask_msg("Series East--text_1", c("East", "text_1")), "Series text_1--text_2"))
  check("mask text: missing, empty, and unrelated messages pass through",
    identical(mask_msg(c(NA, "", "Passed.", "East is low"), "East"), c(NA, "", "Passed.", "text_1 is low")) &&
      identical(mask_msg("Series East", character()), "Series East"))
  table_tokens <- new.env(parent = emptyenv())
  finn_tx_mask(c("Quokka", "Wombat"), table_tokens)
  check("mask text: labels the tables already tokenized keep that token",
    identical(mask_msg("Series Wombat--Quokka--Kiwi", c("Quokka", "Wombat", "Kiwi"), table_tokens), "Series text_2--text_1--text_3"))
  many_labels <- sprintf("Account_%03d", 1:300)
  many_masked <- mask_msg(paste("Levels:", paste(many_labels, collapse = ", ")), many_labels)
  check("mask text: a message naming many labels masks each with its own token",
    !grepl("Account_", many_masked, fixed = TRUE) &&
      length(unique(strsplit(sub("^Levels: ", "", many_masked), ", ", fixed = TRUE)[[1]])) == 300L)
  no_rec <- sub('"columns":["Region","Product"]}', '"columns":["Region","Product"],"reconcile":false}', tx_outline_steps, fixed = TRUE)
  rnr <- run(bad_total, tx_spec(no_rec))
  check("transform: reconcile false reports a total mismatch instead of failing", isTRUE(rnr$gate$passed) && tx_status(rnr)[["P9c"]] == "report")

  gap <- tx_put("gap.csv", c("Region,Date,Value", "East,2024-01-01,1", "East,2024-02-01,2", "East,2024-03-01,3", "East,2024-04-01,"))
  fill_future <- tx_spec(c(tx_tidy_steps, '{"op":"fill_values","column":"Value","method":"previous","reason":"carry forward","extend_into_future":true}'),
    settings = '{"combo_variables":["Region"],"target_variable":"Value","date_type":"month","forecast_horizon":3,"hist_end_date":"2024-03-01"}')
  rff <- run(gap, fill_future)
  check("transform: target values filled after the history end fail T1", !isTRUE(rff$gate$passed) && tx_status(rff)[["T1"]] == "fail")

  totals_left <- tx_put("totals_left.csv", c("Region,Date,Value", "East,2024-01-01,1", "Total,2024-01-01,1"))
  rtl <- run(totals_left, tx_spec(tx_tidy_steps, settings = tx_region))
  check("transform: total labels left in the series fail T5", !isTRUE(rtl$gate$passed) && tx_status(rtl)[["T5"]] == "fail")
  check("transform: not_totals allows a real series named like a total",
    tx_status(run(totals_left, tx_spec(tx_tidy_steps, settings = tx_region, extra = ',"not_totals":["Total"]')))[["T5"]] == "pass")

  # Diagnostics.
  dg <- finn_tx_diagnose(tx_outline)
  check("diagnose: finds the header, title rows, totals, footnote, and outline labels",
    identical(dg$header_guess, 4L) && identical(unlist(dg$title_rows), 1:2) && identical(dg$total_rows$rows, c(7L, 9L, 10L)) &&
      identical(unlist(dg$total_columns), "F") && 11L %in% unlist(dg$footnote_rows) &&
      "A" %in% vapply(dg$outline_columns, `[[`, character(1), "column") && length(dg$unit_mentions) == 1)
  check("diagnose: data labels are masked but column names are shown",
    !grepl("East|West|Sales Report", finn_to_json(dg$sample_masked)) && identical(dg$header_candidates[[1]]$cells$A, "Region"))
  check("diagnose: two-row date headers are detected", identical(finn_tx_diagnose(two_row)$two_row_date_header$rows, 1:2))
  check("diagnose: a tidy file has no pivot filters or outline columns",
    length(finn_tx_diagnose(tx_tidy)$pivot_filters) == 0 && length(finn_tx_diagnose(tx_tidy)$outline_columns) == 0)
  fp <- finn_tx_diagnose(tx_filtered)
  check("diagnose: pivot filters show the field and an Excel marker, and mask a single selected item",
    length(fp$pivot_filters) == 2 && identical(fp$pivot_filters[[1]]$field, "Adjustment Type") &&
      identical(fp$pivot_filters[[1]]$selection, "(Multiple Items)") && identical(fp$pivot_filters[[2]]$kind, "single") &&
      !identical(fp$pivot_filters[[2]]$selection, "No"))
  check("diagnose: the header below the filters, outline labels next to dates, and the Grand Total row are found",
    identical(fp$header_guess, 5L) && identical(vapply(fp$outline_columns, `[[`, character(1), "column"), "B") &&
      identical(fp$total_rows$rows, 10L))
  check("diagnose: a lone value column headed Total is the value, not a total column", length(fp$total_columns) == 0)

  # Latest period that has not ended.
  rr <- run(tx_recent, tx_spec(tx_tidy_steps, settings = tx_region), today = tx_oct20)
  check("transform: the preview flags a latest period that has not ended",
    identical(rr$latest_period$suggested_hist_end_date, "2026-09-01") &&
      grepl("**Latest period:** The latest period with actuals (`2026-10-01`", rr$preview_markdown, fixed = TRUE))
  rr_later <- run(tx_recent, tx_spec(tx_tidy_steps, settings = tx_region), today = as.Date("2026-11-05"))
  check("transform: the latest-period note never changes the gate or the token",
    is.null(rr_later$latest_period) && !grepl("Latest period", rr_later$preview_markdown, fixed = TRUE) &&
      identical(rr$token, rr_later$token) && identical(rr$gate$checks, rr_later$gate$checks))
  check("transform: no latest-period note when hist_end_date already leaves it out",
    is.null(finn_tx_latest_period(rr$gate$output, list(target_variable = "Value", date_type = "month", hist_end_date = "2026-09-01"), tx_oct20)) &&
      is.null(finn_tx_latest_period(NULL, list(target_variable = "Value", date_type = "month"), tx_oct20)))
})

local({
  workbook <- tx_put("approval-sheets.xlsx", "Offline reader fixture; no workbook is parsed.")
  sheet_env <- load_functions("finn_prep.R", common)
  grid <- tx$finn_tx_read_grid(tx_tidy)
  # Identical sheets isolate selection identity from source bytes and output values.
  sheet_env$finn_tx_read_grid <- function(file, sheet = NULL) grid
  spec <- tx_spec(tx_tidy_steps, settings = tx_region)
  selected <- sheet_env$finn_tx_run(workbook, spec, validator = tx_pass_validator, sheet = "Data")
  other <- sheet_env$finn_tx_run(workbook, spec, validator = tx_pass_validator, sheet = "Other")
  default <- sheet_env$finn_tx_run(workbook, spec, validator = tx_pass_validator)
  check("transform: approval binds the CLI worksheet even when its values match another sheet",
    isTRUE(selected$gate$passed) && isTRUE(other$gate$passed) && isTRUE(default$gate$passed) &&
      !identical(selected$token, other$token) && !identical(selected$token, default$token))
  check("transform: the same worksheet selection keeps the approval token",
    identical(selected$token, sheet_env$finn_tx_run(workbook, spec, validator = tx_pass_validator, sheet = "Data")$token))
  check("transform: omitted and explicit first-sheet selections have the same identity",
    identical(default$token, sheet_env$finn_tx_run(workbook, spec, validator = tx_pass_validator, sheet = "1")$token))
  explicit <- tx_spec(tx_tidy_steps, settings = tx_region, extra = ',"source":{"sheet":"Data"}')
  check("transform: a spec worksheet overrides an unused CLI worksheet",
    identical(sheet_env$finn_tx_run(workbook, explicit, validator = tx_pass_validator)$token,
      sheet_env$finn_tx_run(workbook, explicit, validator = tx_pass_validator, sheet = "Other")$token))

  entry <- load_functions("prepare_data.R", sheet_env)
  args <- list(file = workbook, out_dir = file.path(tmp, "sheet-approval"))
  preview <- entry$prepare_transform(c(args, preview = "true", sheet = "Data"), workbook, spec, tx_pass_validator)
  before <- list.files(args$out_dir)
  check("prepare transform: changing or omitting the previewed sheet cannot write a prepared file",
    errors_with(entry$prepare_transform(c(args, approve = preview$data$token, sheet = "Other"), workbook, spec, tx_pass_validator), "does not match") &&
      errors_with(entry$prepare_transform(c(args, approve = preview$data$token), workbook, spec, tx_pass_validator), "does not match") &&
      identical(list.files(args$out_dir), before))
  approved <- entry$prepare_transform(c(args, approve = preview$data$token, sheet = "Data"), workbook, spec, tx_pass_validator)
  check("prepare transform: replaying the approved sheet writes the verified output",
    isTRUE(approved$ok) && file.exists(approved$data$output))
})

local({
  blank <- tx_put("gate-blank-ids.csv", c(
    "Region,Date,Value", "001,2024-01-01,10", ",2024-02-01,11", "1,2024-03-01,"
  ))
  gate_data <- NULL
  # Mirror the package's R6 blank predicate and capture the exact validator input.
  validator <- function(input_data, ...) {
    gate_data <<- input_data
    failed <- any(!is.na(input_data$Region) & trimws(input_data$Region) == "")
    list(passed = !failed, checks = data.frame(Rule = "R6", Description = "Blank identifiers",
      Status = if (failed) "fail" else "pass", Message = if (failed) "Blank identifier." else "Passed."),
      summary = list(), details = list())
  }
  result <- tx$finn_tx_run(blank, tx_spec(tx_tidy_steps, settings = tx_region), validator = validator)
  roundtrip <- tx_put("gate-blank-roundtrip.csv", tx$finn_tx_csv_lines(result$gate$output))
  worker_data <- common$finn_prepare_input(common$finn_load_data(roundtrip, text_columns = "Region"), result$settings)$data
  check("transform: verification and the worker preserve the same empty and zero-padded identifiers",
    identical(gate_data$Region, c("001", "", "1")) && identical(gate_data$Region, worker_data$Region) &&
      identical(gate_data$Date, worker_data$Date) && identical(gate_data$Value, worker_data$Value) &&
      is.na(gate_data$Value[3]))
  check("transform: a blank identifier fails verification without an approval token",
    !isTRUE(result$gate$passed) && is.null(result$token) && identical(tx_status(result)[["R6"]], "fail"))
})

profile_env <- load_functions("profile_data.R", tx)
with(profile_env, {
  reasons_for <- function(file) {
    df <- finn_load_data(file)
    profile_transform_reasons(finn_tx_diagnose(file), df, finn_shape_issues(df))
  }
  fdf <- finn_load_data(tx_filtered)
  check("profile guard: the automatic reshape alone would misread the filtered pivot",
    length(finn_shape_issues(fdf)) > 0 && !"title_rows" %in% vapply(finn_shape_issues(fdf), `[[`, character(1), "type"))
  fr <- reasons_for(tx_filtered)
  check("profile guard: filters, the real header row, and outline labels need a transform spec",
    any(grepl("2 pivot filters", fr)) && any(grepl("(Multiple Items)", fr, fixed = TRUE)) && any(grepl("a single item", fr)) &&
      !any(grepl("Anomaly Flag = No", fr, fixed = TRUE)) && any(grepl("sheet row 5", fr)) && any(grepl("Column B", fr)))
  orr <- reasons_for(tx_outline)
  check("profile guard: subtotals the automatic reshape misses, footnotes, and total columns are reported",
    any(grepl("2 subtotal labels", orr)) && any(grepl("footnote", orr)) && any(grepl("Column F holds totals", orr)) &&
      any(grepl("Column A", orr)) && !any(grepl("column headers look", orr)))
  check("profile guard: a tidy file needs nothing", length(reasons_for(tx_tidy)) == 0)
  check("profile guard: diagnostics errors give no reasons", length(profile_transform_reasons(list(error = "x"), fdf, list())) == 0)
  check("profile guard: pivot filters add a filtered-slice next action",
    grepl("filtered pivot", profile_filter_action(finn_tx_diagnose(tx_filtered))) && is.null(profile_filter_action(list())))
  lp <- profile_latest_period(utils::read.csv(tx_recent, stringsAsFactors = FALSE), "Date", "Value", "month", tx_oct20)
  check("profile: a latest month that has not ended is reported with the previous month",
    identical(lp$suggested_hist_end_date, "2026-09-01") &&
      is.null(profile_latest_period(utils::read.csv(tx_recent, stringsAsFactors = FALSE), "Date", "Value", "month", as.Date("2026-11-05"))) &&
      is.null(profile_latest_period(data.frame(), NULL, "Value", "month", tx_oct20)))
})

with(prep_env, {
  d <- file.path(tmp, "tx_out")
  spec <- tx_spec(tx_outline_steps, description = "SPEC-MARKER-7731")
  check("prepare transform: the spec is read from standard input only",
    errors_with(prepare_read_spec(file.path(tx_dir, "spec.json")), "only accepts '-'") &&
      identical(prepare_read_spec("-", textConnection(spec)), spec))
  base <- list(file = tx_outline, out_dir = d)
  check("prepare transform: preview and approve are exclusive",
    errors_with(prepare_transform(base, tx_outline, spec, tx_pass_validator), "exactly one"))
  check("prepare transform: a missing finnts validator stops before anything is written",
    errors_with(prepare_transform(c(base, preview = "true"), tx_outline, spec, NULL), "validate_prepared_input") && !dir.exists(d))
  pv <- prepare_transform(c(base, preview = "true"), tx_outline, spec, tx_pass_validator)
  check("prepare transform: preview returns a token and writes only the local preview file",
    identical(pv$status, "preview") && nchar(pv$data$token) == 16 && identical(list.files(d), "outline_finn_preview.csv") &&
      startsWith(readLines(file.path(d, "outline_finn_preview.csv"), n = 1), "\"finn_preview_source_rows\""))
  prepare_transform(c(base, preview = "true"), tx_outline, spec, tx_pass_validator)
  check("prepare transform: a new preview replaces only its own preview file", identical(list.files(d), "outline_finn_preview.csv"))
  check("prepare transform: a wrong token writes nothing",
    errors_with(prepare_transform(c(base, approve = "0000000000000000"), tx_outline, spec, tx_pass_validator), "does not match") &&
      identical(list.files(d), "outline_finn_preview.csv"))
  check("prepare transform: a changed spec needs a new preview",
    errors_with(prepare_transform(c(base, approve = pv$data$token), tx_outline, tx_spec(tx_outline_steps, description = "edited"), tx_pass_validator), "does not match"))
  failing <- prepare_transform(c(base, preview = "true"), tx_outline, tx_spec(tx_outline_steps), tx_fail_validator("R7"))
  check("prepare transform: a failed gate returns no token", identical(failing$status, "gate_failed") && is.null(failing$data$token))
  ap <- prepare_transform(c(base, approve = pv$data$token), tx_outline, spec, tx_pass_validator)
  written <- utils::read.csv(ap$data$output, stringsAsFactors = FALSE)
  check("prepare transform: approval writes the verified table", identical(ap$status, "ok") && nrow(written) == 9 && sum(written$Value) == 3110)
  check("prepare transform: the spec is never saved",
    !any(vapply(list.files(c(d, tx_dir), full.names = TRUE), function(f) any(grepl("SPEC-MARKER-7731|\"op\"", readLines(f, warn = FALSE))), logical(1))))

  root <- file.path(tmp, "tx_root")
  paths <- finn_project_paths(root, "Prep")
  finn_ensure_project_dirs(paths)
  finn_write_project(paths, list(name = "Prep"))
  pargs <- list(file = tx_outline, root = root, project = "Prep")
  tok <- prepare_transform(c(pargs, preview = "true"), tx_outline, spec, tx_pass_validator)$data$token
  pa <- prepare_transform(c(pargs, approve = tok), tx_outline, spec, tx_pass_validator)
  rec <- finn_read_project(paths)$data_preparations
  check("prepare transform: approval records an audit entry without the spec or data",
    length(rec) == 1 && setequal(names(rec[[1]]), c("prepared_at", "source", "output", "source_md5", "token", "plan_summary", "checks", "counts")) &&
      startsWith(rec[[1]]$output, "input/") && file.exists(pa$data$output))

  src_paths <- finn_project_paths(root, "PrepSourceSettings")
  finn_ensure_project_dirs(src_paths)
  finn_write_project(src_paths, list(name = "PrepSourceSettings"))
  finn_save_project_config(src_paths, finn_merge_config(list(mode = "standard", combo_variables = c("Region", "Product"),
    target_variable = "Value", date_type = "month", forecast_horizon = 3,
    data = list(file = tx_outline), date_column = "When", date_format = "%d/%m/%Y", decimal_mark = ",")))
  src_args <- list(file = tx_outline, root = root, project = "PrepSourceSettings")
  src_cfg <- prepare_project_config(src_args)
  src_pv <- tryCatch(prepare_transform(c(src_args, preview = "true"), tx_outline, spec, tx_pass_validator),
    error = function(e) conditionMessage(e))
  check("prepare transform: a project's source date and number settings do not block the preview",
    is.list(src_pv) && identical(src_pv$status, "preview") && nchar(src_pv$data$token) == 16 &&
      !any(c("date_column", "date_format", "decimal_mark") %in% names(src_cfg)) &&
      identical(src_cfg$target_variable, "Value"))

  handoff <- paste(pa$next_actions, collapse = "\n")
  check("prepare transform: handoff updates a draft config before approved saving",
    grepl("data.file", handoff, fixed = TRUE) && grepl("data.sheet", handoff, fixed = TRUE) &&
      grepl("date_column", handoff, fixed = TRUE) && grepl("%Y-%m-%d", handoff, fixed = TRUE) &&
      grepl("decimal_mark", handoff, fixed = TRUE) && grepl("--config=<draft>", handoff, fixed = TRUE) &&
      grepl("approval", handoff, fixed = TRUE) && grepl("--save=true", handoff, fixed = TRUE))
  original <- tx_put("handoff-original.csv", c("When,Region,Product,Value", "01/01/2024,Old,A,999"))
  previous <- finn_merge_config(list(mode = "standard", combo_variables = c("Region", "Product"),
    target_variable = "Value", date_type = "month", forecast_horizon = 3,
    data = list(file = original, sheet = "Source"), date_column = "When", date_format = "%d/%m/%Y", decimal_mark = ","))
  finn_save_project_config(paths, previous)
  before_config <- unname(tools::md5sum(file.path(paths$dir, "config.json")))
  draft <- finn_read_project_config(paths)
  draft$data$file <- pa$data$output
  draft$data$sheet <- NULL
  draft$date_column <- "Date"
  draft$date_format <- "%Y-%m-%d"
  draft$decimal_mark <- "."
  draft_file <- file.path(tmp, "handoff-draft.json")
  finn_write_json(draft, draft_file)
  proposed <- finn_read_project_config(paths, draft_file)
  proposed_data <- finn_prepare_input(finn_load_data(proposed$data$file, proposed$data$sheet,
    text_columns = proposed$combo_variables), proposed)$data
  check("prepared handoff: a draft reads the prepared CSV without replacing the saved config",
    nrow(proposed_data) == 9 && sum(proposed_data$Value) == 3110 &&
      inherits(proposed_data$Date, "Date") &&
      identical(unname(tools::md5sum(file.path(paths$dir, "config.json"))), before_config))
  finn_save_project_config(paths, proposed)
  saved <- finn_read_project_config(paths)
  saved_data <- finn_prepare_input(finn_load_data(saved$data$file, saved$data$sheet,
    text_columns = saved$combo_variables), saved)$data
  check("prepared handoff: saving the approved draft replaces the old path and parsing settings",
    identical(saved$data$file, pa$data$output) && is.null(saved$data$sheet) &&
      identical(saved$date_column, "Date") && identical(saved$date_format, "%Y-%m-%d") &&
      identical(saved$decimal_mark, ".") && identical(saved_data, proposed_data))

  # The audit keeps what was done and the verification outcome, never the labels,
  # reasons, descriptions, or values a plan or validator message carried.
  audit_csv <- tx_put("audit_fill.csv", c("Region,Date,Value",
    "Quokka,2024-01-01,10", "Quokka,2024-02-01,", "Quokka,2024-03-01,12",
    "Wombat,2024-01-01,20", "Wombat,2024-02-01,21", "Wombat,2024-03-01,22"))
  audit_spec <- tx_spec(c(tx_tidy_steps, '{"op":"fill_values","column":"Value","method":"value","value":7777.25,"reason":"FILL-REASON-9941 for Quokka"}'),
    settings = tx_region, description = "AUDIT-DESC-MARKER Quokka")
  label_validator <- function(input_data, ...) {
    list(passed = TRUE, checks = data.frame(Rule = "R1", Description = "fake", Status = "pass",
      Message = "Series Quokka--A has 3 periods totalling 7777.25", stringsAsFactors = FALSE), summary = list(), details = list())
  }
  apaths <- finn_project_paths(root, "PrepAudit")
  finn_ensure_project_dirs(apaths)
  finn_write_project(apaths, list(name = "PrepAudit"))
  aargs <- list(file = audit_csv, root = root, project = "PrepAudit")
  atok <- prepare_transform(c(aargs, preview = "true"), audit_csv, audit_spec, label_validator)$data$token
  prepare_transform(c(aargs, approve = atok), audit_csv, audit_spec, label_validator)
  arec <- finn_read_project(apaths)$data_preparations
  check("prepare transform: the audit record holds no labels, reasons, descriptions, or fill values",
    length(arec) == 1 && !grepl("AUDIT-DESC-MARKER|FILL-REASON-9941|7777\\.25|Quokka|Wombat", finn_to_json(arec)))
  check("prepare transform: the audit record keeps its keys, one line per step, and the rule outcomes", {
    arec_checks <- arec[[1]]$checks
    setequal(names(arec[[1]]), names(rec[[1]])) && setequal(names(arec_checks[[1]]), c("Rule", "Status", "Message")) &&
      identical(unlist(arec[[1]]$plan_summary)[4], "Step 4: fill_values (1 value)") &&
      all(c("S1", "R1", "P9a", "P9b") %in% vapply(arec_checks, function(ch) ch$Rule, character(1))) &&
      all(vapply(arec_checks, function(ch) ch$Status, character(1)) %in% c("pass", "report"))
  })

  # Verification messages name series like the preview tables do, so they are
  # masked by default and shown as written only when the user consents.
  mdir <- file.path(tmp, "tx_out_masked")
  leaky <- function(input_data, ...) {
    list(passed = TRUE, checks = data.frame(Rule = "R1", Description = "fake", Status = "pass",
      Message = "Series East--A has 3 periods", stringsAsFactors = FALSE), summary = list(), details = list())
  }
  masked <- prepare_transform(list(file = tx_outline, out_dir = mdir, preview = "true"), tx_outline, spec, leaky)
  check("prepare transform: validation messages show masked labels by default", {
    masked_text <- vapply(masked$data$checks, function(ch) ch$Message, character(1))
    parts <- Filter(function(p) length(p) == 3L,
      regmatches(masked_text, regexec("^Series (text_[0-9]+)--(text_[0-9]+) has 3 periods$", masked_text)))
    identical(masked$status, "preview") && length(parts) == 1L && parts[[1]][[2]] != parts[[1]][[3]] &&
      !any(grepl("East", c(masked_text, masked$data$preview_markdown), fixed = TRUE)) &&
      grepl(sprintf("Series %s--%s has 3 periods", parts[[1]][[2]], parts[[1]][[3]]), masked$data$preview_markdown, fixed = TRUE)
  })
  shown <- prepare_transform(list(file = tx_outline, out_dir = mdir, preview = "true", reveal_labels = "true"), tx_outline, spec, leaky)
  check("prepare transform: revealing labels shows validation messages as written",
    "Series East--A has 3 periods" %in% vapply(shown$data$checks, function(ch) ch$Message, character(1)) &&
      grepl("Series East--A has 3 periods", shown$data$preview_markdown, fixed = TRUE))

  rpaths <- finn_project_paths(root, "Recent")
  finn_ensure_project_dirs(rpaths)
  finn_write_project(rpaths, list(name = "Recent"))
  rargs <- list(file = tx_recent, root = root, project = "Recent")
  rspec <- tx_spec(tx_tidy_steps, settings = tx_region)
  rpv <- prepare_transform(c(rargs, preview = "true"), tx_recent, rspec, tx_pass_validator, today = tx_oct20)
  check("prepare transform: an open latest period is pointed out with hist_end_date offered, not rows dropped",
    identical(rpv$data$latest_period$suggested_hist_end_date, "2026-09-01") &&
      any(grepl("Offer hist_end_date `2026-09-01`", rpv$next_actions, fixed = TRUE)))
  prepare_transform(c(rargs, approve = rpv$data$token), tx_recent, rspec, tx_pass_validator, today = tx_oct20)
  rrec <- finn_read_project(rpaths)$data_preparations
  check("prepare transform: the latest-period note is never saved in the audit record",
    length(rrec) == 1 && setequal(names(rrec[[1]]), names(rec[[1]])) && !grepl("not ended|Latest period", finn_to_json(rrec)))
  check("prepare transform: no latest-period action when the period has ended",
    !any(grepl("Latest period", prepare_transform(c(rargs, preview = "true"), tx_recent, rspec, tx_pass_validator, today = as.Date("2026-11-05"))$next_actions)))
})

# ---- Size-based recommendations -------------------------------------------------
with(common, {
  check("size tiers", identical(c(finn_size_tier(100), finn_size_tier(101), finn_size_tier(1000), finn_size_tier(1001)),
    c("small", "large", "large", "very_large")))
  bt <- function(dt, tier) {
    r <- finn_recommend_back_test(dt, tier, 1000, 4)
    c(r$scenarios, r$spacing)
  }
  check("back test: month large", identical(bt("month", "large"), c(6, 2)))
  check("back test: month very large", identical(bt("month", "very_large"), c(4, 3)))
  check("back test: week large", identical(bt("week", "large"), c(5, 9)))
  check("back test: week very large", identical(bt("week", "very_large"), c(4, 13)))
  check("back test: day very large", identical(bt("day", "very_large"), c(3, 92)))
  check("back test: quarter keeps spacing 1", identical(bt("quarter", "very_large"), c(4, 1)))
  check("back test: small tier and yearly data get no recommendation",
    is.null(finn_recommend_back_test("month", "small", 1000, 12)) && is.null(finn_recommend_back_test("year", "large", 1000, 2)))
  short <- finn_recommend_back_test("month", "large", 30, 12)
  check("back test: short history gives a note instead", identical(short$ok, FALSE) && grepl("too short", short$note))

  cfg <- modifyList(finn_default_config(), list(date_type = "month", forecast_horizon = 12, mode = "agent"))
  big <- finn_recommend_settings(cfg, list(n_series = 5000, n_periods = 60), workers = 4)
  keys <- vapply(big$recommendations, `[[`, character(1), "key")
  check("recommend: very large suggests faster settings",
    all(c("back_test_scenarios", "back_test_spacing", "run_local_models", "run_global_models", "models_to_run") %in% keys))
  check("recommend: very large Agent run suggests standard for the whole data", identical(big$suggested_mode, "standard") && nzchar(big$suggested_mode_reason))
  check("recommend: large data tests a subset first", isTRUE(big$test_subset_first) && nzchar(big$estimated_runtime))
  small <- finn_recommend_settings(modifyList(cfg, list(mode = "standard")), list(n_series = 4, n_periods = 48))
  check("recommend: small data has no suggestions or subset test", length(small$recommendations) == 0 && !isTRUE(small$test_subset_first))
  user_bt <- finn_recommend_settings(modifyList(cfg, list(back_test_scenarios = 20, back_test_spacing = 1)), list(n_series = 500, n_periods = 40))
  check("recommend: user back test that leaves little history is flagged",
    !any(grepl("back_test", vapply(user_bt$recommendations, `[[`, character(1), "key"))) && any(grepl("periods to train on", user_bt$notes)))
  vc <- load_functions("validate_config.R", common)
  local({
    has <- function(text, pattern) grepl(pattern, text, fixed = TRUE)
    config <- finn_merge_config(list(mode = "standard", project_name = "demo", target_variable = "Sales",
      combo_variables = c("Region", "Product"), date_type = "month", forecast_horizon = 12,
      run_ensemble_models = FALSE, external_regressors = "Promo|Flag"))
    facts <- list(n_rows = 120, n_columns = 5, n_series = 4, date_min = "2021-01-01", date_max = "2023-06-01",
      last_actual_date = "2022-12-01")
    review <- vc$validate_settings_review(config, facts, list(reason = "offline plan"), big, "Low RAM.", "Read with a comma.")
    order <- vapply(c("Here are the proposed **Standard** forecast settings for project `demo`.", "- **Run type:** Standard.",
      "### Data check", "### Forecast configuration", "### Warnings", "### Suggestions", "### On this computer",
      "Reply `run` to start the forecast"), function(p) regexpr(p, review, fixed = TRUE)[[1]], numeric(1))
    check("settings review: section order with run footer", length(review) == 1 && all(order > 0) && !is.unsorted(order))
    check("settings review: configuration table header",
      has(review, "| Input | Recommended value | What it means for your forecast |\n|---|---|---|"))
    check("settings review: JSON values for arrays, strings, booleans, and null",
      has(review, "| `combo_variables` | `[\"Region\",\"Product\"]` |") && has(review, "| `target_variable` | `\"Sales\"` |") &&
        has(review, "| `negative_forecast` | `false` |") && has(review, "| `hist_start_date` | `null` |"))
    check("settings review: pipes in values are escaped", has(review, "`[\"Promo\\|Flag\"]`"))
    check("settings review: data check shows last actuals and notes",
      has(review, "**Last period with actuals:** `2022-12-01`") && has(review, "- Read with a comma.") &&
        has(review, "passed with 1 warning`"))
    check("settings review: a standard null hist_end_date names the latest date in the file and offers a cutoff",
      has(review, "Auto uses `2023-06-01`, the latest date in the file") && has(review, "set it to `2022-12-01`") &&
        !has(review, "Auto uses `2022-12-01`") && has(review, "or `null` for the latest date in the file."))
    flat <- vc$validate_settings_review(config, modifyList(facts, list(date_max = "2022-12-01")), list())
    check("settings review: a standard null hist_end_date needs no cutoff advice when the file ends with actuals",
      has(flat, "Auto uses `2022-12-01`, the latest date in the file.") && !has(flat, "To end history at the last actuals instead"))
    check("settings review: suggestions show size and JSON value; run type is not a suggestion",
      has(review, "very large") && has(review, "- `models_to_run`: `[\"xgboost\"]`.") && !has(review, "Run type advice") &&
        has(review, "Fast and predictable."))
    one <- vc$validate_settings_review(modifyList(config, list(combo_variables = "Region")), facts, list())
    check("settings review: single combo column stays an array", has(one, "| `combo_variables` | `[\"Region\"]` |"))
    check("settings review: ensembles off are kept separate from averaging",
      has(review, "| `run_ensemble_models` | `false` |") && has(review, "Kept `false` for standard runs"))
    check("settings review: explicitly requested ensembles on",
      has(vc$validate_settings_review(modifyList(config, list(run_ensemble_models = TRUE)), facts, list()), "| `run_ensemble_models` | `true` | Controls whether ensemble models are trained; separate from simple averaging. Ensembles are on"))
    auto <- config
    auto$run_ensemble_models <- NULL
    check("settings review: automatic ensembles are flagged",
      has(vc$validate_settings_review(auto, facts, list()), "| `run_ensemble_models` | `null` |") &&
        has(vc$validate_settings_review(auto, facts, list()), "Auto may enable ensembles"))
    agent <- modifyList(config, list(mode = "agent", agent = list(max_iter = 3, weighted_mape_goal = 0.03)))
    agent_review <- vc$validate_settings_review(agent, facts, list())
    check("settings review: Agent rows replace standard-only rows",
      !has(agent_review, "`run_ensemble_models`") && !has(agent_review, "`forecast_approach`") &&
        has(agent_review, "| `max_iter` | `3` |") && has(agent_review, "| `weighted_mape_goal` | `0.03` |") &&
        has(agent_review, "| `llm` |") && has(agent_review, "**Agent iterate** forecast settings") &&
        has(agent_review, "over up to 3 iterations"))
    check("settings review: hidden internals are not shown",
      !has(review, "`seed`") && !has(review, "`parallel`"))
    check("settings review: an Agent null hist_end_date keeps naming its last period with actuals",
      has(agent_review, "Auto uses `2022-12-01`.") && has(agent_review, "or `null` for the last period with actuals.") &&
        !has(agent_review, "the latest date in the file"))

    update_run <- list(run_type = "update", label = "Agent update", hint = finn_run_type_hint("update"),
      situation = "agent_refresh", why = "Your last Agent forecast used actuals through `2022-06-01`.",
      alternative = list(run_type = "iterate", label = "Agent iterate (new Agent version)", reason = "Searches again."))
    update_review <- vc$validate_settings_review(agent, facts, list(), run = update_run)
    check("settings review: existing project names the next run type with hint, why, and other option",
      has(update_review, "Here are the proposed settings for the next **Agent update** forecast for existing project `demo`.") &&
        has(update_review, "- **Run type:** Agent update. Reuses what the Agent learned") &&
        has(update_review, "- **Why:** Your last Agent forecast used actuals through `2022-06-01`.") &&
        has(update_review, "- **Other option:** Agent iterate (new Agent version). Searches again. Tell me if you prefer it."))
    cont <- vc$validate_settings_review(agent, facts, list(), run = list(run_type = "iterate", situation = "agent_current", prior_max_iter = 3))
    check("settings review: max_iter explains continuing the last Agent search",
      has(cont, "Your last Agent search ran up to `3`; a higher value continues that Agent version"))
    check("settings review: max_iter has no continue note for a new project",
      !has(agent_review, "Your last Agent search"))
    lower_review <- vc$validate_settings_review(modifyList(agent, list(agent = list(max_iter = 2))), facts, list(),
      run = list(run_type = "iterate", situation = "agent_current", prior_max_iter = 3, prior_weighted_mape_goal = 0.05))
    check("settings review: lower max_iter and the prior goal are explained",
      has(lower_review, "a lower value can't undo iterations, so nothing new would run. Set it above `3`") &&
        has(lower_review, "Your last Agent search used `0.05`. A stricter (lower) goal continues that Agent version"))

    nr_new <- list(run_type = "standard", situation = "new_project", launch = "--mode=standard")
    ri <- vc$validate_run_info(nr_new, "standard", config, small)
    check("run info: new small standard project offers Agent iterate from size advice",
      identical(ri$alternative$run_type, "iterate") && grepl("practical", ri$alternative$reason) && identical(ri$launch, "--mode=standard"))
    check("run info: very large standard project does not recommend the Agent",
      grepl("Not recommended", vc$validate_run_info(nr_new, "standard", config, big)$alternative$reason))
    ri_switch <- vc$validate_run_info(nr_new, "iterate", agent, small)
    check("run info: user-chosen Agent on a new project offers Standard and launches iterate",
      identical(ri_switch$run_type, "iterate") && identical(ri_switch$alternative$run_type, "standard") &&
        identical(ri_switch$launch, "--mode=iterate") && is.null(ri_switch$why))
    nr_refresh <- list(run_type = "update", situation = "agent_refresh", why = "New actuals.", launch = "--mode=update", prior_max_iter = 3,
      alternative = list(run_type = "iterate", label = "Agent iterate (new Agent version)", reason = "Again.", launch = "--mode=iterate --new=true --confirm=true"))
    ri_over <- vc$validate_run_info(nr_refresh, "iterate", agent)
    check("run info: overriding the recommendation keeps it as the other option",
      identical(ri_over$alternative$run_type, "update") && grepl("(recommended)", ri_over$alternative$label, fixed = TRUE) &&
        identical(ri_over$alternative$reason, "New actuals.") && identical(ri_over$launch, "--mode=iterate --new=true --confirm=true") &&
        is.null(ri_over$prior_max_iter))

    changed <- modifyList(config, list(forecast_horizon = 6, models_to_run = c("arima", "ets")))
    changes <- vc$validate_settings_changes(config, changed, facts, "Low RAM.")
    check("settings changes: only changed inputs with previous and new values",
      has(changes, "| Input | Previous | New value | What it means for your forecast |") &&
        has(changes, "| `forecast_horizon` | `12` | `6` |") && has(changes, "| `models_to_run` | `null` | `[\"arima\",\"ets\"]` |") &&
        !has(changes, "`target_variable`") && has(changes, "### Warnings") && has(changes, "`show full configuration`"))
    check("settings changes: no change says so",
      has(vc$validate_settings_changes(config, config, facts), "No settings changed."))
    check("settings changes: mode switch falls back to the full review", is.null(vc$validate_settings_changes(config, agent, facts)))

    root <- tempfile("finn-prev-")
    paths <- finn_project_paths(root, "demo")
    dir.create(paths$dir, recursive = TRUE)
    saved <- file.path(paths$dir, "config.json")
    check("previous config: none before a config is saved", is.null(vc$validate_previous_config(paths, saved)))
    finn_write_json(list(mode = "standard", forecast_horizon = 12), saved)
    check("previous config: saved config is not compared with itself", is.null(vc$validate_previous_config(paths, saved)))
    prev <- vc$validate_previous_config(paths, file.path(root, "draft.json"))
    check("previous config: draft is compared with the saved config", identical(prev$forecast_horizon, 12))
  })
})

# ---- Run status ---------------------------------------------------------------------
st <- load_functions("run_status.R", common)
with(st, {
  away <- status_next(list(run_id = "r1", status = "running", other_host = TRUE, host = "OTHER-PC"), "P")
  check("status: other-host run says check from that computer", any(grepl("OTHER-PC", away)) && any(grepl("takeover", away)))
  here <- status_next(list(run_id = "r1", status = "running", other_host = FALSE), "P")
  check("status: local running run has no other-host note", !any(grepl("another computer", here)))
})

# ---- Failure diagnosis --------------------------------------------------------
dx <- load_functions("diagnose_run.R", common)
with(dx, {
  cat_of <- function(text) diagnose_match(text)$category %||% "none"
  cases <- c(
    "Error: cannot allocate vector of size 2.1 Gb" = "memory",
    "there is no package called 'glmnet'" = "missing_package",
    "Inputs have recently changed for this agent" = "agent_inputs_changed",
    "No previous agent runs found for this project" = "update_needs_agent",
    "Previous agent run included global models" = "update_model_mix",
    "HTTP 401 Unauthorized" = "llm_access",
    "No Copilot authentication is available. Set COPILOT_GITHUB_TOKEN" = "llm_access",
    "Copilot Agent workflows require GitHub Copilot CLI 1.0.93 or later." = "llm_access",
    "Copilot is disabled by your organization policy" = "copilot_policy",
    "You have exceeded your monthly premium request allowance" = "copilot_premium_limit",
    "Check GH_HOST: could not resolve host api.example.ghe.com" = "copilot_account_host",
    "The process cannot access the file because it is being used by another process" = "file_locked",
    "character string is not in a standard unambiguous format" = "date_format",
    "invalid type for input name 'forecast_horizon'" = "config_value"
  )
  for (txt in names(cases)) check(paste("diagnose:", cases[[txt]]), identical(cat_of(txt), cases[[txt]]))
  config_fix <- paste(diagnose_match("invalid type for input name 'forecast_horizon'")$fix, collapse = " ")
  check("diagnose: config fixes require a confirmed fresh run",
    grepl("--new=true --confirm=true", config_fix, fixed = TRUE) &&
      grepl("--mode=<same mode>", config_fix, fixed = TRUE) &&
      grepl("original saved settings", config_fix, fixed = TRUE) &&
      !grepl("resumes reread", config_fix, fixed = TRUE))
  check("diagnose: temporary file lock still recommends resume",
    any(grepl("then resume", diagnose_match("Permission denied")$fix, fixed = TRUE)))
  check("diagnose: no match returns NULL", is.null(diagnose_match("all good")) && is.null(diagnose_match("")))
  check("diagnose: every pattern is a valid regex", all(vapply(diagnose_patterns(), function(p) {
    !inherits(try(grepl(p$pattern, "x", perl = TRUE), silent = TRUE), "try-error") && length(p$fix) > 0
  }, logical(1))))
})

# ---- Copilot readiness ----------------------------------------------------------
with(common, {
  no_gh <- function() FALSE
  yes_gh <- function() TRUE
  env_of <- function(...) {
    e <- c(COPILOT_GITHUB_TOKEN = "", GH_TOKEN = "", GITHUB_TOKEN = "")
    v <- c(...)
    e[names(v)] <- v
    e
  }
  check("auth: COPILOT_GITHUB_TOKEN wins", identical(finn_copilot_auth_source(env_of(GH_TOKEN = "a", COPILOT_GITHUB_TOKEN = "b"), no_gh), "COPILOT_GITHUB_TOKEN"))
  check("auth: GH_TOKEN before GITHUB_TOKEN", identical(finn_copilot_auth_source(env_of(GITHUB_TOKEN = "a", GH_TOKEN = "b"), no_gh), "GH_TOKEN"))
  check("auth: blank tokens are ignored", identical(finn_copilot_auth_source(env_of(GH_TOKEN = "  "), yes_gh), "gh"))
  check("auth: none without token or gh login", identical(finn_copilot_auth_source(env_of(), no_gh), "none"))
  check("fix: processx", grepl("processx", finn_copilot_fix("require 'processx' version 3.9.0 or later")))
  check("fix: shim", grepl("copilot.exe", finn_copilot_fix("Use the standalone copilot.exe, not a Windows .cmd or .bat shim.")))
  check("fix: missing CLI", grepl("agent.llm.command", finn_copilot_fix("GitHub Copilot CLI was not found.")))
  check("fix: old CLI", grepl("Update", finn_copilot_fix("Copilot Agent workflows require GitHub Copilot CLI 1.0.93 or later.")))
  fake_cli <- file.path(tmp, "fake-copilot.exe")
  writeLines("", fake_cli)
  check("resolve: explicit path kept", identical(finn_resolve_copilot_command("C:/x/copilot.exe", candidates = fake_cli), "C:/x/copilot.exe"))
  check("resolve: finds installed CLI off PATH", Sys.which("copilot") != "" ||
    identical(finn_resolve_copilot_command("copilot", candidates = c(file.path(tmp, "nope.exe"), fake_cli)), normalizePath(fake_cli, winslash = "/")))
  check("resolve: unchanged when nothing found", Sys.which("copilot") != "" ||
    identical(finn_resolve_copilot_command("copilot", candidates = file.path(tmp, "nope.exe")), "copilot"))
  st <- finn_copilot_status(file.path(tmp, "no-such-copilot.exe"), auth_source = function() "none")
  check("status: missing CLI and auth is not ready", !st$ready && length(st$problems) >= 2 && is.null(st$cli_path))
  check("status: never includes token values", !any(grepl("ghp_|gho_|github_pat_", unlist(st))))
})

# ---- Finn package readiness --------------------------------------------------------
local({
  partial <- new.env(parent = emptyenv())
  partial$prep_data <- function() NULL
  partial$prep_models <- function() NULL
  partial$train_models <- function() NULL
  partial$ensemble_models <- function() NULL
  partial$final_models <- function() NULL
  partial$get_forecast_data <- function() NULL
  partial$validate_prepared_input <- function() NULL
  partial$set_project_info <- function() NULL
  partial$set_run_info <- function() NULL
  partial$set_agent_info_custom <- function() NULL
  partial$iterate_forecast <- function() NULL
  partial$update_forecast <- function() NULL
  complete <- tryCatch(common$finn_check_finnts_capabilities(partial), error = identity)
  check("readiness reports missing required forecast capabilities",
    is.list(complete) && identical(complete$missing_core, character()))
  rm(list = "final_models", envir = partial)
  missing <- tryCatch(common$finn_check_finnts_capabilities(partial), error = identity)
  check("readiness does not accept a missing final-model capability",
    is.list(missing) && identical(missing$missing_core, "final_models"))
  partial$final_models <- function() NULL
  missing_agent <- tryCatch(common$finn_check_finnts_capabilities(partial), error = identity)
  check("readiness reports missing agent capabilities separately",
    is.list(missing_agent) && "get_agent_forecast" %in% missing_agent$missing_agent)
  rm(list = "set_project_info", envir = partial)
  missing_project_info <- common$finn_check_finnts_capabilities(partial)
  check("readiness rejects missing set_project_info",
    identical(missing_project_info$missing_core, "set_project_info"))
  partial$set_project_info <- function() NULL
  rm(list = "set_run_info", envir = partial)
  missing_run_info <- common$finn_check_finnts_capabilities(partial)
  check("readiness rejects missing set_run_info",
    identical(missing_run_info$missing_core, "set_run_info"))

  agent_without_chat <- new.env(parent = emptyenv())
  for (name in c("prep_data", "prep_models", "train_models", "ensemble_models",
    "final_models", "get_forecast_data", "validate_prepared_input",
    "set_project_info", "set_run_info",
    "set_agent_info_custom", "iterate_forecast", "update_forecast", "get_agent_forecast")) {
    agent_without_chat[[name]] <- function() NULL
  }
  missing_chat <- common$finn_check_finnts_capabilities(agent_without_chat)
  check("readiness reports missing chat_copilot as an agent capability",
    identical(missing_chat$missing_agent, "chat_copilot"))
  agent_without_chat$chat_copilot <- function() NULL
  with_chat <- common$finn_check_finnts_capabilities(agent_without_chat)
  check("readiness accepts chat_copilot when all agent capabilities are present",
    identical(with_chat$missing_agent, character()))
})

if (requireNamespace("finnts", quietly = TRUE)) {
  readiness_env <- load_functions("check_env.R", common)
  fake_namespace <- new.env(parent = emptyenv())
  for (name in c("prep_data", "prep_models", "train_models", "ensemble_models",
    "get_forecast_data", "validate_prepared_input", "set_project_info", "set_run_info",
    "set_agent_info_custom",
    "iterate_forecast", "update_forecast", "get_agent_forecast")) {
    fake_namespace[[name]] <- function() NULL
  }
  readiness_env$requireNamespace <- function(...) TRUE
  readiness_env$`::` <- function(package, name) {
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    if (identical(package, "utils") && identical(name, "packageVersion")) {
      return(function(package) package_version("1.0.0"))
    }
    getExportedValue(package, name)
  }
  readiness_env$asNamespace <- function(package) fake_namespace
  readiness_env$finn_custom_supported <- function(...) FALSE
  readiness_env$finn_custom_copilot_supported <- function(...) FALSE
  readiness_env$finn_custom_historical_supported <- function(...) FALSE
  readiness_env$finn_copilot_status <- function(...) list(ready = TRUE, problems = character())
  readiness_env$finn_system_resources <- function(...) list()
  readiness_env$finn_update_check_enabled <- function(...) FALSE
  readiness_env$finn_main <- function(fn) fn(list())
  readiness_expr <- tail(as.list(parse(file.path(scripts_dir, "check_env.R"))), 1)[[1]]
  readiness_result <- eval(readiness_expr, readiness_env)
  check("environment readiness blocks missing core finnts capabilities",
    !isTRUE(readiness_result$ok) && identical(readiness_result$status, "needs_update") &&
      identical(unlist(readiness_result$data$missing_core_capabilities), "final_models"))
}

# ---- finnts argument types ------------------------------------------------------
if (requireNamespace("finnts", quietly = TRUE)) {
  with(common, {
    json_cfg <- jsonlite::fromJSON('{"combo_variables":["Region"],"target_variable":"Revenue","date_type":"month","fiscal_year_start":7}', simplifyVector = FALSE)
    proj_paths <- list(name = "TypeCheck", artifacts = file.path(tmp, "typecheck_artifacts"))
    dir.create(proj_paths$artifacts, showWarnings = FALSE)
    pi <- tryCatch(finn_project_info(proj_paths, json_cfg), error = function(e) conditionMessage(e))
    check("project info: JSON whole-number fiscal_year_start is accepted", is.list(pi) && identical(pi$fiscal_year_start, 7))
  })
}

# ---- Launch and resume decisions ------------------------------------------------
launch <- load_functions("run_forecast.R", common)
with(launch, {
  base_cfg <- finn_merge_config(list(project_name = "Demo", combo_variables = "Region", target_variable = "Revenue", date_type = "month", forecast_horizon = 6))
  h <- finn_config_hash(base_cfg)
  check("hash ignores parallel", identical(h, finn_config_hash(modifyList(base_cfg, list(parallel = "none")))))
  check("hash ignores LLM model", identical(h, finn_config_hash(modifyList(base_cfg, list(agent = list(llm = list(model = "other")))))))
  check("hash ignores data folder", identical(
    finn_config_hash(modifyList(base_cfg, list(data = list(file = "C:/a/data.csv")))),
    finn_config_hash(modifyList(base_cfg, list(data = list(file = "D:/b/data.csv"))))
  ))
  check("hash tracks horizon", !identical(h, finn_config_hash(modifyList(base_cfg, list(forecast_horizon = 12)))))
  agent_cfg <- modifyList(base_cfg, list(mode = "agent"))
  for (change in list(list(model = "other"), list(provider = "openai"),
      list(command = "other-cli"), list(timeout = 99), list(api_key_env = "OTHER_KEY"))) {
    check(paste("Agent hash tracks LLM", names(change)),
      !identical(finn_config_hash(agent_cfg), finn_config_hash(modifyList(agent_cfg, list(agent = list(llm = change))))))
  }
  check("config mode mapping", identical(launch_config_mode("standard"), "standard") && identical(launch_config_mode("update"), "agent"))

  paths <- finn_project_paths(file.path(tmp, "root"), "Launch")
  finn_ensure_project_dirs(paths)
  check("artifacts README is written", file.exists(file.path(paths$artifacts, "README.txt")))
  data_file <- file.path(paths$input, "sample_monthly.csv")
  file.copy(file.path(skill_dir, "examples", "sample_monthly.csv"), data_file)
  cfg <- modifyList(base_cfg, list(data = list(file = data_file)))

  check("standard with no runs starts new", identical(launch_decide(paths, "standard", cfg, list(), list())$action, "new"))
  check("iterate with no history starts new", identical(launch_decide(paths, "iterate", cfg, list(), list())$action, "new"))

  rp <- finn_run_paths(paths, "std-20240101-000000")
  finn_update_state(rp,
    run_id = rp$id, mode = "standard", status = "failed", created_at = finn_now(),
    config_hash = finn_config_hash(cfg), data_md5 = finn_data_md5(data_file)
  )
  check("failed run is listed", length(finn_list_runs(paths)) == 1 && identical(finn_list_runs(paths)[[1]]$effective_status, "failed"))
  d1 <- launch_decide(paths, "standard", cfg, list(), list())
  check("failed run resumes by default", identical(d1$action, "resume") && identical(d1$prior$run_id, rp$id))
  d2 <- launch_decide(paths, "standard", modifyList(cfg, list(forecast_horizon = 12)), list(), list())
  check("changed settings need confirmation", identical(d2$action, "stop") && identical(d2$result$status, "needs_confirmation"))
  check("confirmed resume keeps old run", identical(launch_decide(paths, "standard", modifyList(cfg, list(forecast_horizon = 12)), list(), list(confirm = "true"))$action, "resume"))
  check("--new starts fresh", identical(launch_decide(paths, "standard", cfg, list(), list(new = "true"))$action, "new"))
  check("parallel change still resumes", identical(launch_decide(paths, "standard", modifyList(cfg, list(parallel = "none")), list(), list())$action, "resume"))
  local({
    agent_paths <- finn_project_paths(file.path(tmp, "root"), "LlmDrift")
    finn_ensure_project_dirs(agent_paths)
    saved <- modifyList(cfg, list(mode = "agent", project_name = "LlmDrift"))
    agent_run <- finn_run_paths(agent_paths, "agent-old")
    finn_update_state(agent_run, run_id = agent_run$id, mode = "iterate", status = "failed",
      created_at = finn_now(), config = saved, config_hash = "old-fingerprint-excluding-llm",
      data_md5 = finn_data_md5(data_file))
    check("legacy Agent snapshot resumes unchanged despite older hash",
      identical(launch_decide(agent_paths, "iterate", saved, list(), list())$action, "resume"))
    changed <- modifyList(saved, list(agent = list(llm = list(provider = "openai"))))
    decision <- launch_decide(agent_paths, "iterate", changed, list(), list())
    check("Agent provider drift requires confirmation",
      identical(decision$result$status, "needs_confirmation") && "settings" %in% decision$result$data$changed)
    confirmed <- launch_decide(agent_paths, "iterate", changed, list(), list(confirm = "true"))
    check("confirmed Agent resume retains original LLM snapshot",
      identical(confirmed$action, "resume") && identical(confirmed$prior$config$agent$llm, saved$agent$llm))
    check("fresh Agent run accepts changed LLM after confirmation",
      identical(launch_decide(agent_paths, "iterate", changed, list(), list(new = "true", confirm = "true"))$action, "new"))
  })

  check("versions: no recorded versions is not a change", length(launch_version_changes(list())) == 0)
  check("versions: finnts change reported", identical(
    launch_version_changes(list(finnts_version = "0.5.0", r_version = "4.4.1"), list(finnts_version = "0.6.0", r_version = "4.4.1")),
    "finnts 0.5.0 -> 0.6.0"
  ))
  finn_update_state(rp, finnts_version = "0.0.1")
  dv <- launch_decide(paths, "standard", cfg, list(), list())
  check("version change on resume needs confirmation", identical(dv$action, "stop") && length(dv$result$data$version_changes) == 1)
  check("confirmed version change resumes", identical(launch_decide(paths, "standard", cfg, list(), list(confirm = "true"))$action, "resume"))
  finn_update_state(rp, finnts_version = finn_versions()$finnts_version %||% "0.0.1", n_series = 4L)

  la <- list(run_id = rp$id, data_md5 = "old")
  check("drift: none when unchanged", length(launch_update_drift(paths, modifyList(la, list(data_md5 = "same")), list(n_series = 4), "same", FALSE)) == 0)
  check("drift: series count change", any(grepl("number of time series", launch_update_drift(paths, la, list(n_series = 5), "old", TRUE))))
  check("drift: revised history without newer dates", any(grepl("revised", launch_update_drift(paths, la, list(n_series = 4), "new", FALSE))))
  check("drift: newer dates are expected changes", length(launch_update_drift(paths, la, list(n_series = 4), "new", TRUE)) == 0)

  local({
    md5 <- finn_data_md5(data_file)
    acfg <- modifyList(cfg, list(mode = "agent", agent = list(max_iter = 3)))
    project_with <- function(name, runs = list(), last_agent = NULL) {
      p <- finn_project_paths(file.path(tmp, "root"), name)
      finn_ensure_project_dirs(p)
      for (r in runs) do.call(finn_update_state, c(list(finn_run_paths(p, r$run_id)), r))
      finn_write_project(p, list(name = name, last_agent = last_agent))
      p
    }
    agent_last <- function(run_id, mode = "iterate", hist_end = "2023-12-01") list(status = "completed", run_id = run_id, mode = mode,
      request_id = "req-1", agent_run_id = "agent-run-1", agent_version = 1L, hist_end_date = hist_end,
      config_hash = finn_config_hash(acfg), data_md5 = md5)
    agent_state <- function(run_id, mode = "iterate", config = acfg) list(run_id = run_id, mode = mode, status = "completed",
      created_at = "2024-01-01T00:00:00+0000", data_end = "2023-12-01", config = config, data_md5 = md5)

    np <- project_with("NextNew")
    check("next run: new project follows the config mode", identical(finn_next_run(np, cfg, list())$situation, "new_project") &&
      identical(finn_next_run(np, acfg, list())$run_type, "iterate"))

    sp <- project_with("NextStd", list(list(run_id = "std-1", mode = "standard", status = "completed",
      created_at = "2024-01-01T00:00:00+0000", data_end = "2023-12-01")))
    sr <- finn_next_run(sp, cfg, list(last_actual_date = "2024-06-01"))
    check("next run: standard project with new actuals refreshes with standard",
      identical(sr$situation, "standard_refresh") && identical(sr$run_type, "standard") && identical(sr$launch, "--mode=standard") &&
        identical(sr$alternative$run_type, "iterate") && grepl("2024-06-01", sr$why))
    check("next run: standard project without new actuals", identical(finn_next_run(sp, cfg, list(last_actual_date = "2023-12-01"))$situation, "standard_current"))
    pinned <- finn_next_run(sp, modifyList(cfg, list(hist_end_date = "2023-12-01")), list(last_actual_date = "2024-06-01"))
    check("next run: pinned hist_end_date is a suggestion, not a silent change",
      length(pinned$recommendations) == 1 && identical(pinned$recommendations[[1]]$key, "hist_end_date") &&
        identical(pinned$recommendations[[1]]$value, "2024-06-01") && grepl("Set `hist_end_date`", pinned$why))

    ap <- project_with("NextAgent", list(agent_state("iter-1")), agent_last("iter-1"))
    ar <- finn_next_run(ap, acfg, list(last_actual_date = "2024-06-01"))
    check("next run: Agent project with new actuals recommends Agent update",
      identical(ar$situation, "agent_refresh") && identical(ar$run_type, "update") && identical(ar$launch, "--mode=update") &&
        grepl("--new=true", ar$alternative$launch, fixed = TRUE) && length(ar$settings_changes) == 0)
    ac <- finn_next_run(ap, acfg, list(last_actual_date = "2023-12-01"))
    check("next run: current Agent project explains continuing with a higher max_iter",
      identical(ac$situation, "agent_current") && identical(ac$run_type, "iterate") && identical(as.numeric(ac$prior_max_iter), 3) &&
        grepl("above `3`", ac$why, fixed = TRUE))
    check("next run: settings changes since the last Agent forecast are listed",
      identical(finn_next_run(ap, modifyList(acfg, list(forecast_horizon = 12)), list(last_actual_date = "2024-06-01"))$settings_changes,
        "forecast_horizon 6 -> 12"))

    rpj <- project_with("NextResume", list(agent_state("iter-1"), list(run_id = "upd-2", mode = "update", status = "failed",
      created_at = "2024-02-01T00:00:00+0000")), agent_last("iter-1"))
    rr <- finn_next_run(rpj, acfg, list(last_actual_date = "2024-06-01"))
    check("next run: unfinished latest run is resumed first", identical(rr$situation, "resume") && identical(rr$run_type, "update"))

    check("settings drift: none without a prior config", length(finn_settings_drift(NULL, acfg)) == 0)
    check("settings drift: hist_end_date and speed settings are ignored",
      length(finn_settings_drift(acfg, modifyList(acfg, list(hist_end_date = "2024-06-01", parallel = "none")))) == 0)
    check("settings drift: Agent settings are compared",
      identical(finn_settings_drift(acfg, modifyList(acfg, list(agent = list(max_iter = 5)))), "max_iter 3 -> 5"))

    more <- modifyList(acfg, list(agent = list(max_iter = 5)))
    cd <- launch_decide(ap, "iterate", more, list(), list())
    check("continue: higher max_iter reopens the same Agent version",
      identical(cd$action, "new") && identical(cd$request_id, "req-1") && identical(cd$overwrite, FALSE) &&
        any(grepl("only the iterations up to 5", cd$notes)))
    other <- launch_decide(ap, "iterate", modifyList(more, list(forecast_horizon = 12)), list(), list())
    check("continue: other setting changes still need a new Agent version", identical(other$result$status, "needs_new_version"))
    same <- launch_decide(ap, "iterate", acfg, list(), list())
    check("continue: same settings explain raising max_iter",
      identical(same$result$status, "already_complete") && any(grepl("higher agent.max_iter", same$result$next_actions)))
    lower <- launch_decide(ap, "iterate", modifyList(acfg, list(agent = list(max_iter = 2))), list(), list())
    check("continue: lower max_iter is already complete, not a new version",
      identical(lower$result$status, "already_complete") && identical(lower$result$data$reason, "lower_max_iter") &&
        identical(as.numeric(lower$result$data$max_iter$previous), 3) && grepl("up to 3", lower$result$message, fixed = TRUE) &&
        any(grepl("above 3", lower$result$next_actions, fixed = TRUE)))
    lower_other <- launch_decide(ap, "iterate", modifyList(acfg, list(agent = list(max_iter = 2), forecast_horizon = 12)), list(), list())
    check("continue: lower max_iter with other changes still needs a new Agent version",
      identical(lower_other$result$status, "needs_new_version"))
    finn_write_project(ap, list(name = "NextAgent", last_agent = modifyList(agent_last("iter-1"), list(data_md5 = "changed"))))
    check("continue: lower max_iter with changed data still needs a new Agent version",
      identical(launch_decide(ap, "iterate", modifyList(acfg, list(agent = list(max_iter = 2))), list(), list())$result$status, "needs_new_version"))
    finn_write_project(ap, list(name = "NextAgent", last_agent = agent_last("iter-1")))

    pinned_cfg <- list(mode = "agent", project_name = "P", parallel = "auto", data = list(file = "C:/a/data.csv"), forecast_horizon = 6,
      agent = list(max_iter = 3, weighted_mape_goal = 0.03, llm = list(provider = "copilot")))
    check("hash: fingerprints saved by existing projects are unchanged",
      identical(finn_config_hash(pinned_cfg), "5418c3f4068e2dd8") &&
        identical(finn_config_hash(modifyList(pinned_cfg, list(mode = "standard"))), "683b53a5cce12f53"))
    with_goal <- function(config, goal) modifyList(config, list(agent = list(weighted_mape_goal = goal)))
    base_goal <- as.numeric(acfg$agent$weighted_mape_goal)
    both <- launch_decide(ap, "iterate", with_goal(more, base_goal / 2), list(), list())
    check("continue: higher max_iter with a stricter goal continues the same request",
      identical(both$action, "new") && identical(both$request_id, "req-1") && identical(both$overwrite, FALSE) &&
        any(grepl("only the iterations up to 5", both$notes)) && any(grepl("goal changes", both$notes)) &&
        identical(both$continuation$from_run_id, "iter-1") && identical(both$continuation$from_status, "completed") &&
        identical(as.numeric(both$continuation$max_iter$new), 5) && isFALSE(both$continuation$partial) &&
        identical(as.numeric(both$continuation$weighted_mape_goal$previous), base_goal))
    strict <- launch_decide(ap, "iterate", with_goal(acfg, base_goal / 2), list(), list())
    check("continue: a stricter goal alone continues and explains its partial effect",
      identical(strict$action, "new") && identical(strict$request_id, "req-1") && isTRUE(strict$continuation$partial) &&
        any(grepl("best model is global", strict$notes)))
    loose <- launch_decide(ap, "iterate", with_goal(acfg, base_goal * 2), list(), list())
    check("continue: a looser goal alone is already complete",
      identical(loose$result$status, "already_complete") && identical(loose$result$data$reason, "looser_goal_only"))
    lower_goal <- launch_decide(ap, "iterate", with_goal(modifyList(acfg, list(agent = list(max_iter = 2))), base_goal / 2), list(), list())
    check("continue: lower max_iter with a goal change says to keep max_iter",
      identical(lower_goal$result$data$reason, "lower_max_iter") &&
        grepl("Keep agent.max_iter at 3 or higher", lower_goal$result$message, fixed = TRUE))
    llm_change <- launch_decide(ap, "iterate", modifyList(more, list(agent = list(llm = list(provider = "openai")))), list(), list())
    check("continue: an LLM change needs a new Agent version",
      identical(llm_change$result$status, "needs_new_version") && identical(llm_change$result$data$reason, "settings_changed"))
    check("continue: a seed change needs a new Agent version",
      identical(launch_decide(ap, "iterate", modifyList(more, list(seed = 7L)), list(), list())$result$status, "needs_new_version"))
    check("continue: other changes name the reason",
      identical(other$result$data$reason, "settings_changed") && "forecast_horizon 6 -> 12" %in% other$result$data$changed)

    assign("finn_versions", function() list(finnts_version = "9.9.9", r_version = "9.9.9"), envir = launch)
    finn_write_project(ap, list(name = "NextAgent", last_agent = modifyList(agent_last("iter-1"), list(finnts_version = "0.0.1"))))
    sw <- launch_decide(ap, "iterate", more, list(), list())
    sw_ok <- launch_decide(ap, "iterate", more, list(), list(confirm = "true"))
    rm("finn_versions", envir = launch)
    finn_write_project(ap, list(name = "NextAgent", last_agent = agent_last("iter-1")))
    check("continue: different software needs confirmation, then continues the same version",
      identical(sw$result$status, "needs_confirmation") && identical(sw$result$data$reason, "software_changed") &&
        identical(sw_ok$action, "new") && identical(sw_ok$request_id, "req-1") && any(grepl("different software", sw_ok$notes)))

    uu <- project_with("UnfinishedUpdate", list(agent_state("iter-1"), list(run_id = "upd-2", mode = "update", status = "failed",
      created_at = "2024-02-01T00:00:00+0000")), agent_last("iter-1"))
    ub <- launch_decide(uu, "iterate", more, list(), list())
    check("continue: a newer unfinished update blocks continuation",
      identical(ub$result$status, "needs_new_version") && identical(ub$result$data$reason, "unfinished_update") &&
        any(grepl("--mode=update", ub$result$next_actions, fixed = TRUE)))

    stopped <- project_with("StoppedIterate", list(modifyList(agent_state("iter-1"),
      list(status = "failed", request_id = "req-9", step_down = 2L, overwrite = TRUE))))
    sd <- launch_decide(stopped, "iterate", more, list(), list())
    check("stopped iterate: a higher max_iter continues the same request with the new budget",
      identical(sd$action, "new") && identical(sd$request_id, "req-9") && identical(as.integer(sd$step_down), 2L) &&
        isTRUE(sd$overwrite) && identical(sd$continuation$from_run_id, "iter-1") && identical(sd$continuation$from_status, "failed"))
    check("stopped iterate: a design change still asks before resuming",
      identical(launch_decide(stopped, "iterate", modifyList(more, list(forecast_horizon = 12)), list(), list())$result$status,
        "needs_confirmation"))
    check("stopped iterate: unchanged settings resume the stopped run",
      identical(launch_decide(stopped, "iterate", acfg, list(), list())$action, "resume"))
    as_failed <- function(state, ...) modifyList(state, list(status = "failed", ...))
    second <- project_with("StoppedSecond", list(agent_state("iter-1"),
      as_failed(agent_state("iter-2"), request_id = "req-2", agent_version = 2L, created_at = "2024-02-01T00:00:00+0000")),
      agent_last("iter-1"))
    sd2 <- tryCatch(launch_decide(second, "iterate", more, list(), list()), error = function(e) list(action = conditionMessage(e)))
    check("stopped iterate: a stopped newer version continues that version, not the last completed one",
      identical(sd2$action, "new") && identical(sd2$continuation$from_run_id, "iter-2") && identical(sd2$request_id, "req-2") &&
        identical(as.integer(sd2$continuation$agent_version), 2L))
    first_failed <- project_with("StoppedFirst", list(as_failed(agent_state("iter-1"), request_id = "req-1", agent_version = 1L)))
    sd3 <- tryCatch(launch_decide(first_failed, "iterate", more, list(), list()), error = function(e) list(action = conditionMessage(e)))
    check("stopped iterate: a first run that failed reports its own version",
      identical(sd3$action, "new") && identical(sd3$continuation$from_run_id, "iter-1") &&
        identical(as.integer(sd3$continuation$agent_version), 1L))

    nr_lower <- finn_next_run(ap, modifyList(acfg, list(agent = list(max_iter = 2))), list(last_actual_date = "2023-12-01"))
    check("next run: lower max_iter explains nothing new would run",
      identical(nr_lower$continuation$kind, "lower_max_iter") && grepl("can't undo iterations", nr_lower$why, fixed = TRUE))
    nr_more <- finn_next_run(ap, more, list(last_actual_date = "2023-12-01"))
    check("next run: higher max_iter explains continuing the same version",
      identical(nr_more$continuation$kind, "continue") && grepl("continues version 1", nr_more$why, fixed = TRUE) &&
        identical(as.numeric(nr_more$prior_weighted_mape_goal), base_goal))
    nr_stop <- finn_next_run(stopped, more, list(last_actual_date = "2023-12-01"))
    check("next run: a stopped iterate run with a higher budget continues with revised settings",
      identical(nr_stop$situation, "resume") && identical(nr_stop$continuation$kind, "continue") &&
        grepl("revised settings", nr_stop$why, fixed = TRUE))
    check("continuation: other settings are listed separately from budget and goal",
      identical(finn_iterate_continuation(acfg, modifyList(more, list(forecast_horizon = 12)))$changed, "forecast_horizon 6 -> 12"))
    up <- project_with("UpdateOnly", list(agent_state("upd-1", mode = "update")), agent_last("upd-1", mode = "update"))
    check("continue: only after an iterate run", identical(launch_decide(up, "iterate", modifyList(more, list(project_name = "UpdateOnly")), list(), list())$action, "stop"))

    check("mismatch: new run of another mode needs a new review",
      identical(launch_mode_mismatch(list(action = "new"), "standard", "iterate")$status, "needs_review"))
    check("mismatch: matching mode and resumes are not blocked",
      is.null(launch_mode_mismatch(list(action = "new"), "agent", "update")) && is.null(launch_mode_mismatch(list(action = "resume"), "standard", "iterate")))

    std_update <- launch_decide(sp, "update", acfg, list(last_actual_date = "2024-06-01"), list())
    check("update: standard projects are told to refresh with standard",
      identical(std_update$result$status, "needs_agent_run") && any(grepl("--mode=standard", std_update$result$next_actions, fixed = TRUE)))
    stuck <- launch_decide(ap, "update", modifyList(acfg, list(hist_end_date = "2023-12-01")), list(last_actual_date = "2024-06-01"), list())
    check("update: pinned hist_end_date explains why nothing is new",
      identical(stuck$result$status, "no_new_data") && isTRUE(stuck$result$data$hist_end_pinned) && grepl("hist_end_date is set", stuck$result$message))
    open_jan <- list(last_actual_date = "2024-01-01", incomplete_period = list(date = "2024-01-01", suggested_hist_end_date = "2023-12-01"))
    left_out <- finn_next_run(ap, modifyList(acfg, list(hist_end_date = "2023-12-01")), open_jan)
    check("next run: a period that has not ended and is left out by hist_end_date is not new actuals",
      identical(left_out$situation, "agent_current") && identical(left_out$data_end, "2023-12-01") &&
        grepl("`2024-01-01` period has not ended yet", left_out$why, fixed = TRUE) && length(left_out$recommendations) == 0)
    check("next run: without hist_end_date the open period still counts, so the review can warn about it",
      identical(finn_next_run(ap, acfg, open_jan)$situation, "agent_refresh"))
    open_only <- launch_decide(ap, "update", modifyList(acfg, list(hist_end_date = "2023-12-01")), open_jan, list())
    check("update: only a period that has not ended is newer, so the user is told to wait",
      identical(open_only$result$status, "no_new_data") && identical(open_only$result$data$incomplete_period, "2024-01-01") &&
        grepl("has not ended yet", open_only$result$message) && any(grepl("wait until it ends", open_only$result$next_actions)))
    open_jun <- list(last_actual_date = "2024-06-01", incomplete_period = list(date = "2024-06-01", suggested_hist_end_date = "2024-05-01"))
    open_pinned <- launch_decide(ap, "update", modifyList(acfg, list(hist_end_date = "2023-12-01")), open_jun, list())
    check("update: a pinned hist_end_date points to the last complete period, not the open one",
      grepl("last complete period is 2024-05-01", open_pinned$result$message) && any(grepl("hist_end_date to 2024-05-01", open_pinned$result$next_actions)))
    drifted <- launch_decide(ap, "update", modifyList(acfg, list(forecast_horizon = 12)), list(last_actual_date = "2024-06-01", n_series = 4), list())
    check("update: changed settings need confirmation",
      identical(drifted$result$status, "needs_confirmation") && any(grepl("forecast_horizon 6 -> 12", drifted$result$data$changes, fixed = TRUE)))
  })

  plan_local <- list(parallel_processing = "local_machine", num_cores = 8, inner_parallel = FALSE)
  plan_seq <- list(parallel_processing = NULL, inner_parallel = FALSE)
  check("estimate: parallel plan is faster", launch_estimate("standard", plan_local, 200)$high_min < launch_estimate("standard", plan_seq, 200)$high_min)
  notes_std <- launch_run_notes("standard", plan_seq, 10)
  notes_it <- launch_run_notes("iterate", plan_seq, 10)
  check("notes: estimate and keep-awake always given", any(grepl("may take", notes_std)) && any(grepl("awake", notes_std)))
  check("notes: premium note only for Agent modes", !any(grepl("premium", notes_std)) && any(grepl("premium", notes_it)))

  rp2 <- finn_run_paths(paths, "std-20240102-000000")
  finn_update_state(rp2, run_id = rp2$id, mode = "standard", status = "running", created_at = finn_now(), pid = 999999L, host = Sys.info()[["nodename"]])
  if (requireNamespace("ps", quietly = TRUE)) {
    check("dead worker is reported as interrupted", identical(finn_effective_status(finn_read_state(rp2)), "interrupted"))
  }
  finn_update_state(rp2, status = "interrupted")

  rp3 <- finn_run_paths(paths, "std-20240103-000000")
  finn_update_state(rp3, run_id = rp3$id, mode = "standard", status = "running", created_at = finn_now(), host = "OTHER-PC-XYZ")
  busy <- launch_active_guard(paths, list())
  check("guard: other-host run blocks launch", identical(busy$status, "busy") && isTRUE(busy$data$other_host))
  check("guard: other-host message names the computer", grepl("OTHER-PC-XYZ", busy$message) && any(grepl("takeover", busy$next_actions)))
  check("guard: --confirm alone does not take over", identical(launch_active_guard(paths, list(confirm = "true"))$status, "busy"))
  check("guard: --takeover clears the block", is.null(launch_active_guard(paths, list(takeover = "true"))))
  check("guard: takeover marks run interrupted and keeps it", identical(finn_read_state(rp3)$status, "interrupted") && identical(finn_read_state(rp3)$host, "OTHER-PC-XYZ"))

  lroot <- file.path(tmp, "ledger")
  fake_run <- function(project, run_id, host = Sys.info()[["nodename"]], parallel = NULL) {
    lp <- finn_project_paths(lroot, project)
    finn_ensure_project_dirs(lp)
    finn_write_project(lp, list(name = project))
    lrp <- finn_run_paths(lp, run_id)
    finn_update_state(lrp, run_id = run_id, mode = "standard", status = "queued", stage = "starting",
      created_at = finn_now(), host = host, parallel = parallel)
    lp
  }
  fake_run("ProjA", "std-20240201-000000", parallel = list(num_cores = 4L, claimed_cores = 4L, claimed_gb = 8))
  fake_run("ProjB", "std-20240202-000000", parallel = list(num_cores = 6L))
  fake_run("ProjC", "std-20240203-000000", host = "OTHER-PC-XYZ", parallel = list(claimed_cores = 10L, claimed_gb = 40))
  fake_run("ProjD", "std-20240204-000000", parallel = list(claimed_cores = 3L, claimed_gb = 5))
  led <- finn_claimed_resources(lroot, exclude_project = "ProjD")
  check("ledger: counts same-computer runs in other projects", length(led$runs) == 2 && setequal(vapply(led$runs, function(r) r$project, ""), c("ProjA", "ProjB")))
  check("ledger: uses claims, falls back for older runs", led$cores == 10 && led$gb == 20)
  check("ledger: empty root claims nothing", length(finn_claimed_resources(file.path(tmp, "no-such-root"))$runs) == 0)
  mb <- launch_machine_busy(led, list(cores = 0L, free_gb = 1), 0)
  check("machine_busy: refuses and lists running projects", !mb$ok && identical(mb$status, "machine_busy") && grepl("ProjA", mb$message) && length(mb$data$active_runs) == 2)
  check("machine_busy: never cancels others", any(grepl("Never cancel", mb$next_actions)))
  check("shared note: none when alone", length(launch_shared_note(list(runs = list()), "iterate")) == 0)
  check("shared note: Agent runs mention premium usage", any(grepl("premium", launch_shared_note(led, "iterate"))) && !any(grepl("premium", launch_shared_note(led, "standard"))))

  local({
    old_settings <- Sys.getenv("FINN_SETTINGS_DIR")
    Sys.setenv(FINN_SETTINGS_DIR = file.path(tmp, "machine-registry-tests"))
    on.exit(Sys.setenv(FINN_SETTINGS_DIR = old_settings))
    root_a <- file.path(tmp, "registry-a")
    root_b <- file.path(tmp, "registry-b")
    a <- finn_project_paths(root_a, "SameName")
    b <- finn_project_paths(root_b, "SameName")
    for (p in list(a, b)) {
      finn_ensure_project_dirs(p)
      finn_write_project(p, list(name = p$name))
    }
    ar <- finn_run_paths(a, "run-a")
    br <- finn_run_paths(b, "run-b")
    finn_update_state(ar, run_id = ar$id, status = "queued", host = Sys.info()[["nodename"]],
      parallel = list(num_cores = 7L, claimed_cores = 7L, claimed_gb = 2.1))
    finn_register_allocation(a, ar, finn_claimed_resources(root_a, "SameName"))
    first <- finn_claimed_resources(root_b, "SameName")
    check("registry sees other roots with identical project names", first$cores == 7 && length(first$runs) == 1)
    finn_update_state(br, run_id = br$id, status = "queued", host = Sys.info()[["nodename"]],
      parallel = list(num_cores = 7L, claimed_cores = 7L, claimed_gb = 2.1))
    finn_register_allocation(b, br, first)
    saved_registry <- finn_read_json(file.path(finn_settings_dir(), "machine-runs.json"))
    check("registry contains bookkeeping only", all(vapply(saved_registry$runs, function(s) {
      !any(c("config", "results", "data_md5", "error") %in% names(s))
    }, logical(1))))
    both <- finn_claimed_resources(root_b)
    check("registry and discovery do not double-count", both$cores == 14 && length(both$runs) == 2)
    check("software changes guarded across roots without duplicates", length(finn_software_active_runs(root_b)) == 2)
    status_functions <- load_functions("run_status.R", common)
    across <- status_functions$status_all(list(root = root_b))
    check("all-status reports both storage roots", length(across$data$active) == 2 &&
      setequal(vapply(across$data$active, function(s) s$root, ""), c(finn_norm(root_a), finn_norm(root_b))))
    third <- finn_plan_with_others(200, 0, FALSE, both, res(22, 1, 32), batch_size = 3)
    check("third batch project gets seven workers", third$fits && third$plan$num_cores == 7)
    finn_update_state(ar, status = "running", pid = NULL)
    check("running allocation with missing process identity stays reserved", finn_claimed_resources(root_b)$cores == 14 &&
      any(vapply(finn_claimed_resources(root_b)$runs, function(s) s$allocation_uncertain, logical(1))))
    finn_update_state(ar, status = "queued")
    full <- finn_plan_with_others(200, 0, FALSE, list(cores = 21L, gb = 6.3, runs = both$runs), res(22, 1, 32))
    check("exhausted shared CPU budget refuses even sequential", !full$fits)
    finn_update_state(ar, status = "completed", pid = NULL)
    check("completed allocation releases capacity", finn_claimed_resources(root_b)$cores == 7)
    if (requireNamespace("ps", quietly = TRUE)) {
      finn_update_state(br, status = "cancelled", pid = Sys.getpid(),
        pid_create_time = as.numeric(ps::ps_create_time(ps::ps_handle())))
      check("cancelled live worker retains capacity", finn_claimed_resources(root_b)$cores == 7)
      finn_update_state(br, pid = 2147483647L)
      check("confirmed exited worker releases capacity", finn_claimed_resources(root_b)$cores == 0)
    }
    file.rename(br$state, paste0(br$state, ".saved"))
    missing <- finn_claimed_resources(root_b)
    check("missing registered state conservatively retains claim", missing$cores == 7 && missing$runs[[1]]$allocation_uncertain)
    writeLines("{broken", br$state)
    damaged <- finn_claimed_resources(root_b)
    check("unreadable registered state retains claim", damaged$cores == 7 && damaged$runs[[1]]$allocation_uncertain)
    file.rename(paste0(br$state, ".saved"), br$state)
    legacy_bad <- finn_run_paths(b, "legacy-unreadable")
    dir.create(legacy_bad$dir)
    writeLines("{broken", legacy_bad$state)
    check("unreadable legacy state is an explicit error", errors_with(finn_claimed_resources(root_b), "Cannot determine resource claims"))
    finn_write_json(list(run_id = legacy_bad$id, status = "completed"), legacy_bad$state)
    token <- finn_machine_lock_acquire(timeout = 0)
    check("atomic machine lock admits one owner", !is.null(token) && is.null(finn_machine_lock_acquire(timeout = 0)))
    check("wrong owner cannot release machine lock", !finn_machine_lock_release("wrong-token"))
    check("owner releases machine lock", finn_machine_lock_release(token))
    next_token <- finn_machine_lock_acquire(timeout = 0)
    check("released machine lock can be acquired again", !is.null(next_token))
    finn_machine_lock_release(next_token)
    if (requireNamespace("ps", quietly = TRUE)) {
      lock_dir <- file.path(finn_settings_dir(), "machine-launch-lock")
      dir.create(lock_dir)
      finn_write_json(list(token = "exited-owner", pid = 2147483647L),
        file.path(lock_dir, "owner.json"))
      recovered <- finn_machine_lock_acquire(timeout = 0)
      check("dead machine lock owner can be recovered", !is.null(recovered))
      finn_machine_lock_release(recovered)
      local({
        original_alive <- common$finn_pid_alive
        on.exit(common$finn_pid_alive <- original_alive)
        common$finn_pid_alive <- function(pid, create_time = NULL) if (is.null(pid)) FALSE else NA
        finn_update_state(ar, status = "running", pid = 2147483647L)
        uncertain <- finn_claimed_resources(root_b)
        check("unknown liveness never releases resource claims", uncertain$cores >= 7 &&
          any(vapply(uncertain$runs, function(s) s$allocation_uncertain, logical(1))))
        held <- finn_machine_lock_acquire(timeout = 0)
        check("unknown lock owner never stolen", is.null(finn_machine_lock_acquire(timeout = 0)))
        finn_machine_lock_release(held)
        finn_update_state(ar, status = "completed", pid = NULL)
      })
    }
    init_lock <- file.path(tmp, "owner-init-lock")
    original_write_json <- common$finn_write_json
    on.exit(common$finn_write_json <- original_write_json, add = TRUE)
    common$finn_write_json <- function(x, path) {
      if (identical(basename(path), "owner.json")) stop("owner metadata write failed")
      original_write_json(x, path)
    }
    init_error <- tryCatch(finn_directory_lock_acquire(init_lock, timeout = 0), error = identity)
    check("machine lock metadata failure never leaves an active ownerless lock",
      inherits(init_error, "error") && !dir.exists(init_lock) &&
        length(Sys.glob(paste0(init_lock, ".abandoned-init-*"))) == 1L)
    common$finn_write_json <- original_write_json
    if (requireNamespace("processx", quietly = TRUE) && requireNamespace("ps", quietly = TRUE)) {
      race_script <- file.path(tmp, "allocation-race.R")
      writeLines(c(
        "args <- commandArgs(TRUE)",
        "source(args[1])",
        "local({",
        "  token <- finn_machine_lock_acquire(timeout = 10)",
        "  if (is.null(token)) stop('lock timeout')",
        "  on.exit(finn_machine_lock_release(token))",
        "  paths <- finn_project_paths(args[2], args[3])",
        "  finn_ensure_project_dirs(paths)",
        "  finn_write_project(paths, list(name = paths$name))",
        "  claims <- finn_claimed_resources(paths$root, paths$name)",
        "  sizing <- finn_plan_with_others(200, 0, FALSE, claims, list(cores = 8L, total_gb = 32, free_gb = 1))",
        "  if (sizing$fits) {",
        "    rp <- finn_run_paths(paths, 'race-run')",
        "    finn_update_state(rp, run_id = rp$id, status = 'queued', host = Sys.info()[['nodename']], parallel = sizing$plan)",
        "    finn_register_allocation(paths, rp, claims)",
        "  }",
        "  cat(finn_to_json(list(fits = sizing$fits, workers = sizing$plan$claimed_cores)))",
        "})"
      ), race_script)
      children <- lapply(c("a", "b"), function(id) processx::process$new(finn_rscript(),
        c(race_script, file.path(scripts_dir, "finn_common.R"), file.path(tmp, paste0("race-root-", id)), id),
        stdout = "|", stderr = "|"))
      on.exit(for (child in children) if (child$is_alive()) child$kill(), add = TRUE)
      for (child in children) child$wait(timeout = 30000)
      for (child in children) if (!identical(child$get_exit_status(), 0L)) cat(child$read_all_error(), "\n")
      check("concurrent launch processes exit successfully", all(vapply(children,
        function(child) identical(child$get_exit_status(), 0L), logical(1))))
      outcomes <- lapply(children, function(child) jsonlite::fromJSON(child$read_all_output()))
      check("concurrent cross-root launches never double allocate", sum(vapply(outcomes, function(o) o$fits, logical(1))) == 1 &&
        sum(vapply(outcomes, function(o) if (o$fits) o$workers else 0, numeric(1))) == 7)
    }
  })

  local({
    old_settings <- Sys.getenv("FINN_SETTINGS_DIR")
    Sys.setenv(FINN_SETTINGS_DIR = file.path(tmp, "entrypoint-tests"))
    on.exit(Sys.setenv(FINN_SETTINGS_DIR = old_settings))
    entry_root <- file.path(tmp, "entry-root")
    entry <- new.env(parent = launch)
    for (name in c("First", "Second")) {
      paths <- finn_project_paths(entry_root, name)
      finn_ensure_project_dirs(paths)
      finn_write_project(paths, list(name = name))
      config <- finn_default_config()
      config$project_name <- name
      config$data$file <- data_file
      finn_save_project_config(paths, config)
    }
    checked <- function(config, root = NULL) list(config = config, errors = character(), warnings = character(),
      facts = list(n_series = 200, n_rows = 2400, date_min = "2024-01-01", date_max = "2024-12-01"),
      data = data.frame(value = 1))
    entry$finn_load_checked <- checked
    entry$finn_plan_with_others <- function(...) common$finn_plan_with_others(...,
      resources = list(cores = 22L, total_gb = 31.65, free_gb = 2.85))
    entry$finn_versions <- function() list(finnts_version = "offline", r_version = "offline")
    starts <- 0L
    entry$launch_worker <- function(paths, run_paths, wait = FALSE) {
      starts <<- starts + 1L
      list(pid = Sys.getpid(), pid_create_time = if (requireNamespace("ps", quietly = TRUE))
        as.numeric(ps::ps_create_time(ps::ps_handle())))
    }
    main_expr <- tail(as.list(parse(file.path(scripts_dir, "run_forecast.R"))), 1)[[1]]
    entry$finn_main <- function(fn) fn(list(root = entry_root, project = "First", mode = "standard"))
    first <- eval(main_expr, entry)
    check("launcher wires detected budget and pressure warning", first$ok && first$data$parallel$num_cores == 21 &&
      any(grepl("Available RAM is below", first$data$warnings)))
    entry$finn_main <- function(fn) fn(list(root = entry_root, project = "Second", mode = "standard"))
    blocked <- eval(main_expr, entry)
    check("launcher refuses excess capacity without starting a worker", !blocked$ok &&
      identical(blocked$status, "machine_busy") && starts == 1L)
    first_paths <- finn_project_paths(entry_root, "First")
    finn_update_state(finn_run_paths(first_paths, first$data$run_id), status = "completed")
    entry$finn_main <- function(fn) fn(list(root = entry_root, project = "First", mode = "standard"))
    still_exiting <- eval(main_expr, entry)
    check("same-project terminal worker retains capacity until exit", !still_exiting$ok &&
      identical(still_exiting$status, "machine_busy") && starts == 1L)
    preview <- load_functions("validate_config.R", common)
    preview$finn_load_checked <- checked
    preview$finn_plan_with_others <- entry$finn_plan_with_others
    preview$finn_main <- function(fn) fn(list(root = entry_root, project = "First", batch_size = "3"))
    preview_expr <- tail(as.list(parse(file.path(scripts_dir, "validate_config.R"))), 1)[[1]]
    card <- eval(preview_expr, preview)
    check("preview wires batch share without false pressure warning", card$ok && card$data$parallel$num_cores == 7 &&
      !any(grepl("Available RAM is below", card$data$warnings)))
    check("preview returns one settings review without the old card", length(card$data$settings_review) == 1 &&
      grepl("### Forecast configuration", card$data$settings_review, fixed = TRUE) && is.null(card$data$settings_card))
    check("preview names the next run type for an existing standard project",
      identical(card$data$run$run_type, "standard") && grepl("^standard_", card$data$run$situation) &&
        grepl("next **Standard** forecast for existing project `First`", card$data$settings_review, fixed = TRUE))
    preview$finn_main <- function(fn) fn(list(root = entry_root, project = "First", batch_size = "3", run_type = "update"))
    preview$finn_check_config <- function(cfg, data, root) list(errors = character())
    upd_card <- eval(preview_expr, preview)
    check("preview warns that update needs a completed Agent forecast",
      identical(upd_card$data$run$run_type, "update") && any(grepl("Agent", upd_card$data$warnings)))
  })

  local({
    old_settings <- Sys.getenv("FINN_SETTINGS_DIR")
    Sys.setenv(FINN_SETTINGS_DIR = file.path(tmp, "continue-settings"))
    on.exit(Sys.setenv(FINN_SETTINGS_DIR = old_settings))
    cont_root <- file.path(tmp, "continue-root")
    cpaths <- finn_project_paths(cont_root, "Continue")
    finn_ensure_project_dirs(cpaths)
    config <- finn_default_config()
    config$project_name <- "Continue"
    config$mode <- "agent"
    config$data$file <- data_file
    config$agent$max_iter <- 3L
    finn_save_project_config(cpaths, config)
    saved <- finn_read_project_config(cpaths)
    finn_update_state(finn_run_paths(cpaths, "iter-1"), run_id = "iter-1", mode = "iterate", status = "completed",
      created_at = "2024-01-01T00:00:00+0000", config = saved, request_id = "req-1", data_md5 = finn_data_md5(data_file))
    finn_write_project(cpaths, list(name = "Continue", last_agent = list(status = "completed", run_id = "iter-1", mode = "iterate",
      request_id = "req-1", agent_run_id = "agent-run-1", agent_version = 1L, hist_end_date = "2024-12-01",
      config_hash = finn_config_hash(saved), data_md5 = finn_data_md5(data_file))))
    checked <- function(config, root = NULL) list(config = config, errors = character(), warnings = character(),
      facts = list(n_series = 4, n_rows = 48, date_min = "2024-01-01", date_max = "2024-12-01"), data = data.frame(value = 1))
    plan_stub <- function(...) common$finn_plan_with_others(..., resources = list(cores = 8L, total_gb = 32, free_gb = 16))

    preview <- load_functions("validate_config.R", common)
    preview$finn_load_checked <- checked
    preview$finn_plan_with_others <- plan_stub
    preview$finn_main <- function(fn) fn(list(root = cont_root, project = "Continue", run_type = "iterate"))
    preview_expr <- tail(as.list(parse(file.path(scripts_dir, "validate_config.R"))), 1)[[1]]
    review_with <- function(max_iter) {
      config$agent$max_iter <- max_iter
      finn_save_project_config(cpaths, config)
      eval(preview_expr, preview)
    }
    budget <- review_with(5L)
    check("review: a higher max_iter continues the version without a settings-change warning",
      isTRUE(budget$ok) && identical(budget$data$run$situation, "agent_current") &&
        !any(grepl("These settings differ", budget$data$warnings)) && grepl("continues version 1", budget$data$run$why, fixed = TRUE))
    lower <- review_with(2L)
    check("review: a lower max_iter warns that nothing new would run",
      any(grepl("lower than the `3`", lower$data$warnings, fixed = TRUE)))
    review_with(5L)
    budget_steps <- unlist(budget$next_actions)
    check("review: a new Agent version of the same run type is not offered as a rerun of the same review",
      identical(budget$data$run$alternative$run_type, "iterate") && identical(budget$data$run$launch, "--mode=iterate") &&
        !any(grepl("rerun this script with --run_type=iterate", budget_steps, fixed = TRUE)))
    check("review: a new Agent version of the same run type names its own flags and needs explicit confirmation",
      any(grepl("--mode=iterate --new=true --confirm=true", budget_steps, fixed = TRUE) &
        grepl("explicit confirmation", budget_steps, fixed = TRUE)))
    check("review: the normal launch instruction keeps the recommended flags",
      any(grepl("start the run with run_forecast.R --project=<name> --mode=iterate.", budget_steps, fixed = TRUE)))
    refresh <- load_functions("validate_config.R", common)
    refresh$finn_load_checked <- function(config, root = NULL) modifyList(checked(config, root), list(facts = list(date_max = "2025-06-01")))
    refresh$finn_plan_with_others <- plan_stub
    refresh$finn_main <- function(fn) fn(list(root = cont_root, project = "Continue", run_type = "update"))
    newer <- tryCatch(eval(preview_expr, refresh), error = function(e) list(data = list(), next_actions = list(), message = conditionMessage(e)))
    newer_steps <- unlist(newer$next_actions)
    check("review: a new Agent version of another run type is still offered as a rerun with that run type",
      identical(newer$data$run$situation, "agent_refresh") && identical(newer$data$run$alternative$run_type, "iterate") &&
        any(grepl("rerun this script with --run_type=iterate", newer_steps, fixed = TRUE)) &&
        any(grepl("start the run with run_forecast.R --project=<name> --mode=update.", newer_steps, fixed = TRUE)))

    entry <- new.env(parent = launch)
    entry$finn_load_checked <- checked
    entry$finn_plan_with_others <- plan_stub
    entry$finn_versions <- function() list(finnts_version = "offline", r_version = "offline")
    entry$finn_copilot_status <- function(...) list(ready = TRUE)
    entry$launch_worker <- function(paths, run_paths, wait = FALSE) list(pid = Sys.getpid(), pid_create_time = NULL)
    entry$finn_main <- function(fn) fn(list(root = cont_root, project = "Continue", mode = "iterate"))
    main_expr <- tail(as.list(parse(file.path(scripts_dir, "run_forecast.R"))), 1)[[1]]
    started <- eval(main_expr, entry)
    new_state <- if (isTRUE(started$ok)) finn_read_state(finn_run_paths(cpaths, started$data$run_id))
    old_state <- finn_read_state(finn_run_paths(cpaths, "iter-1"))
    check("launcher: continuing a completed Agent version reuses its request without overwriting",
      isTRUE(started$ok) && identical(started$data$request_id, "req-1") && identical(started$data$continuation$from_run_id, "iter-1") &&
        identical(as.numeric(started$data$continuation$max_iter$previous), 3) &&
        identical(as.numeric(started$data$continuation$max_iter$new), 5) && !identical(started$data$run_id, "iter-1") &&
        identical(new_state$request_id, "req-1") && isFALSE(new_state$overwrite) &&
        identical(old_state$status, "completed") && identical(as.numeric(old_state$config$agent$max_iter), 3) &&
        identical(finn_read_project(cpaths)$last_agent$run_id, "iter-1"))
  })

  if (requireNamespace("processx", quietly = TRUE) && requireNamespace("ps", quietly = TRUE)) {
    kpaths <- finn_project_paths(file.path(tmp, "root"), "KillResume")
    finn_ensure_project_dirs(kpaths)
    kcfg <- modifyList(cfg, list(project_name = "KillResume"))
    finn_write_project(kpaths, list(name = "KillResume", last_agent = list(
      status = "completed", agent_run_id = "agent-v1", agent_version = 1L,
      config_hash = finn_config_hash(kcfg), data_md5 = finn_data_md5(data_file)
    )))
    worker <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("-e", "Sys.sleep(120)"))
    krp <- finn_run_paths(kpaths, "iter-20240104-000000")
    finn_update_state(krp,
      run_id = krp$id, mode = "iterate", status = "running", stage = "iterate_forecast",
      created_at = finn_now(), host = Sys.info()[["nodename"]],
      pid = worker$get_pid(), pid_create_time = as.numeric(ps::ps_create_time(ps::ps_handle(worker$get_pid()))),
      config_hash = finn_config_hash(kcfg), data_md5 = finn_data_md5(data_file),
      request_id = "req-kill-test", agent_run_id = "agent-v2", agent_version = 2L,
      finnts_version = finn_versions()$finnts_version, r_version = finn_versions()$r_version
    )
    check("kill: live worker shows running", identical(finn_effective_status(finn_read_state(krp)), "running"))
    check("kill: live local run blocks a second launch", identical(launch_active_guard(kpaths, list())$status, "busy"))
    worker$kill()
    for (i in 1:20) if (worker$is_alive()) Sys.sleep(0.25)
    check("kill: killed worker shows interrupted", identical(finn_effective_status(finn_read_state(krp)), "interrupted"))
    check("kill: nothing blocks the relaunch", is.null(launch_active_guard(kpaths, list())))
    kd <- launch_decide(kpaths, "iterate", kcfg, list(), list())
    check("kill: iterate relaunch resumes the same run", identical(kd$action, "resume") && identical(kd$prior$run_id, krp$id))
    check("kill: resume keeps the request id and Agent version", identical(kd$prior$request_id, "req-kill-test") && identical(as.integer(kd$prior$agent_version), 2L))
    check("kill: resume does not overwrite history", !isTRUE(kd$overwrite))
  }

  if (.Platform$OS.type == "windows" && requireNamespace("processx", quietly = TRUE)) {
    odd_dir <- file.path(tmp, "it's a dir with spaces\\")
    echo_script <- file.path(tmp, "echo args.R")
    out_file <- file.path(tmp, "spawn out.txt")
    writeLines(c(
      "a <- commandArgs(trailingOnly = TRUE)",
      "writeLines(a[-length(a)], sub('^--log=', '', a[length(a)]))"
    ), echo_script)
    wargs <- c(echo_script, paste0("--root=", odd_dir), "--quote=a\"b", paste0("--log=", out_file))
    started <- Sys.time()
    pid <- launch_worker_windows(wargs)
    check("windows launcher returns a pid", is.integer(pid) && !is.na(pid))
    check("windows launcher returns quickly", as.numeric(difftime(Sys.time(), started, units = "secs")) < 30)
    for (i in 1:60) if (!file.exists(out_file)) Sys.sleep(0.5)
    got <- if (file.exists(out_file)) readLines(out_file) else character()
    check("windows launcher passes arguments unchanged", identical(got, wargs[2:3]))
  }
})

# ---- Run cancellation -------------------------------------------------------------
local({
  cancel_env <- load_functions("run_cancel.R", common)
  real_cancel_kill_tree <- cancel_env$cancel_kill_tree
  paths <- list(name = "CancelFailure")
  run_paths <- list(id = "run-1")
  state <- list(run_id = "run-1", status = "running", host = Sys.info()[["nodename"]], mode = "standard")
  cancel_env$finn_require <- function(...) invisible(TRUE)
  cancel_env$finn_project_from_args <- function(...) paths
  cancel_env$finn_run_paths <- function(...) run_paths
  cancel_env$finn_read_state <- function(...) state
  cancel_env$finn_effective_status <- function(x) x$status
  cancel_env$cancel_kill_tree <- function(...) list(stopped = 0L, confirmed = FALSE, note = "The worker is still running.")
  cancel_env$finn_update_state <- function(...) {
    state <<- modifyList(state, list(...))
    state
  }
  cancel_env$finn_log <- function(...) invisible(NULL)
  cancel_env$finn_main <- function(fn) fn(list(run_id = "run-1", confirm = "true"))
  cancel_expr <- tail(as.list(parse(file.path(scripts_dir, "run_cancel.R"))), 1)[[1]]
  result <- eval(cancel_expr, cancel_env)
  check("cancel reports termination failure without persisting cancelled",
    !isTRUE(result$ok) && !identical(result$status, "cancelled") &&
      identical(state$status, "running"))

  cancel_env$cancel_kill_tree <- real_cancel_kill_tree
  fake_process_api <- list(
    ps_handle = function(pid) list(pid = pid),
    ps_children = function(handle, recursive) list(list(pid = 201L), list(pid = 202L)),
    ps_pid = function(handle) handle$pid,
    ps_create_time = function(handle) 1,
    ps_kill = function(handle) invisible(TRUE)
  )
  worker_checks <- 0L
  child_alive <- function(pid, create_time = NULL) {
    if (identical(as.integer(pid), 101L)) {
      worker_checks <<- worker_checks + 1L
      return(worker_checks == 1L)
    }
    identical(as.integer(pid), 202L)
  }
  survivor <- tryCatch(cancel_env$cancel_kill_tree(
    101L, 1, process_api = fake_process_api, is_alive = child_alive
  ), error = identity)
  check("cancel requires every captured child to be gone",
    is.list(survivor) && identical(survivor$confirmed, FALSE))

  worker_checks <- 0L
  all_gone <- tryCatch(cancel_env$cancel_kill_tree(
    101L, 1, process_api = fake_process_api,
    is_alive = function(pid, create_time = NULL) {
      if (identical(as.integer(pid), 101L)) {
        worker_checks <<- worker_checks + 1L
        return(worker_checks == 1L)
      }
      FALSE
    }
  ), error = identity)
  if (!is.list(all_gone) || !identical(all_gone$confirmed, TRUE)) {
    message("  cancellation fixture result: ", paste(capture.output(str(all_gone)), collapse = " "))
  }
  check("cancel confirms success only after worker and captured children exit",
    is.list(all_gone) && identical(all_gone$confirmed, TRUE))
})

# ---- Analysis helpers ---------------------------------------------------------------
an <- new.env(parent = globalenv())
sys.source(file.path(scripts_dir, "finn_analysis_helpers.R"), envir = an)
with(an, {
  paths <- finn_project_paths(file.path(tmp, "root"), "Analysis")
  finn_ensure_project_dirs(paths)
  file.copy(file.path(skill_dir, "examples", "sample_monthly.csv"), file.path(paths$input, "sample_monthly.csv"))
  finn_write_project(paths, list(name = "Analysis", data_file = "input/sample_monthly.csv"))
  finn_save_project_config(paths, list(
    project_name = "Analysis", data = list(file = "input/sample_monthly.csv"),
    combo_variables = c("Region", "Product"), target_variable = "Revenue", date_type = "month", forecast_horizon = 6
  ))
  ctx <- finn_open("Analysis", root = file.path(tmp, "root"))
  hist <- finn_history(ctx)
  check("history has forecast-style columns", all(c("Combo", "Date", "Target", "Region", "Product") %in% names(hist)))
  check("history combos join with --", "East--Cloud" %in% hist$Combo && length(unique(hist$Combo)) == 4)
  check("no completed run is a clear error", errors_with(finn_pick_run(ctx), "no completed run"))

  bt <- data.frame(
    Combo = c("A", "A", "B", "B", "B"), Run_Type = c("Back_Test", "Back_Test", "Back_Test", "Back_Test", "Future_Forecast"),
    Forecast = c(90, 110, 50, 50, 999), Target = c(100, 100, 100, 100, NA)
  )
  acc <- finn_accuracy_by_series(bt)
  check("accuracy by series is worst first", identical(acc$Combo, c("B", "A")) && isTRUE(all.equal(acc$WMAPE, c(0.5, 0.1))))
  check("accuracy with no back tests is empty", nrow(finn_accuracy_by_series(bt[bt$Run_Type == "Future_Forecast", ])) == 0)

  bt2 <- data.frame(
    Combo = c("A", "A", "C", "C"), Run_Type = c("Back_Test", "Future_Forecast", "Back_Test", "Future_Forecast"),
    Forecast = c(100, 50, 80, 20), Target = c(100, NA, 100, NA)
  )
  cmp <- finn_compare_runs(ctx, "a", "b", forecasts = list(a = bt, b = bt2))
  check("compare: union of series", setequal(cmp$Combo, c("A", "B", "C")))
  check("compare: forecast change and accuracy change", {
    b <- cmp[cmp$Combo == "B", ]
    a <- cmp[cmp$Combo == "A", ]
    b$Forecast_A == 999 && is.na(b$Forecast_B) && is.na(a$Forecast_A) && a$Forecast_B == 50 &&
      isTRUE(all.equal(a$Accuracy_Change, 0.1))
  })
  check("compare: missing run is a clear error", errors_with(finn_compare_runs(ctx, "nope", "nope2"), "No run named"))

  saved <- finn_save_analysis(data.frame(x = 1), "My Table", ctx)
  saved2 <- finn_save_analysis(data.frame(x = 2), "My Table", ctx)
  check("save_analysis never replaces files", file.exists(saved) && file.exists(saved2) && !identical(saved, saved2))
})

# ---- Skill updates and rollback (offline, mocked network and installer) ------------
with(common, {
  check("version_newer compares numerically", finn_version_newer("0.10.0", "0.9.1") && !finn_version_newer("0.1.0", "0.1.0"))
  check("version_newer treats unknown as not newer", !finn_version_newer(NULL, "0.1.0") && !finn_version_newer("0.2.0", NULL) && !finn_version_newer("x", "0.1"))
  check("parse skill version", identical(finn_parse_skill_version("a\nfinn_skill_version <- \"1.2.3\"\n"), "1.2.3"))
  check("parse skill version absent", is.null(finn_parse_skill_version("nothing here")))
  check("parse DESCRIPTION version", identical(finn_parse_desc_version("Package: finnts\nVersion: 0.7.0.9001\nTitle: x"), "0.7.0.9001"))
  Sys.setenv(FINN_SKILL_REPO = "someone/fork")
  check("repo override", identical(finn_skill_repo(), "someone/fork"))
  Sys.unsetenv("FINN_SKILL_REPO")
  check("repo default", identical(finn_skill_repo(), "microsoft/finnts"))

  calls <- 0L
  remote_fetch <- function(url, timeout) {
    calls <<- calls + 1L
    if (grepl("/commits/", url)) {
      return("{\"sha\":\"abc123\"}")
    }
    if (grepl("finn_common.R$", url)) {
      return("finn_skill_version <- \"9.0.0\"")
    }
    if (grepl("DESCRIPTION$", url)) {
      return("Package: finnts\nVersion: 99.0.0")
    }
    stop("unexpected url ", url)
  }
  rv <- finn_remote_versions("main", remote_fetch)
  check("remote versions resolve one commit", identical(rv$sha, "abc123") && identical(rv$skill_version, "9.0.0") && identical(rv$finnts_version, "99.0.0"))

  calls <- 0L
  u1 <- finn_update_check(force = TRUE, fetch = remote_fetch)
  check("update check finds newer skill", isTRUE(u1$update_available) && !isTRUE(u1$from_cache) && is.null(u1$error))
  check("update check finds newer finnts", isTRUE(u1$finnts_update_available))
  n <- calls
  u2 <- finn_update_check(fetch = remote_fetch)
  check("update check uses the cache", isTRUE(u2$from_cache) && calls == n && isTRUE(u2$update_available) && identical(u2$sha, "abc123"))
  u3 <- finn_update_check(fetch = remote_fetch, ref = "dev")
  check("update check ignores cache for another ref", !isTRUE(u3$from_cache))

  offline <- function(url, timeout) stop("offline")
  e1 <- finn_update_check(force = TRUE, fetch = offline)
  check("update check records network errors without failing", !is.null(e1$error) && !isTRUE(e1$update_available))
  e2 <- finn_update_check(fetch = remote_fetch)
  check("failed check is cached for a day", isTRUE(e2$from_cache) && !is.null(e2$error))
  cache_path <- file.path(finn_settings_dir(), "update_check.json")
  cache <- finn_read_json(cache_path)
  cache$checked_epoch <- as.numeric(Sys.time()) - 2 * 86400
  finn_write_json(cache, cache_path)
  e3 <- finn_update_check(fetch = remote_fetch)
  check("failed check retries after a day", !isTRUE(e3$from_cache) && is.null(e3$error) && isTRUE(e3$update_available))

  check("auto check on by default", finn_update_check_enabled(list()))
  check("auto check off by argument", !finn_update_check_enabled(list(check_updates = "false")))
  Sys.setenv(FINN_SKILL_NO_UPDATE_CHECK = "true")
  check("auto check off by environment", !finn_update_check_enabled(list()))
  Sys.unsetenv("FINN_SKILL_NO_UPDATE_CHECK")
  s <- finn_load_settings()
  s$auto_update_check <- FALSE
  finn_save_settings(s)
  check("auto check off by setting", !finn_update_check_enabled(list()))
  s$auto_update_check <- TRUE
  finn_save_settings(s)

  finn_append_history(list(type = "a"))
  finn_append_history(list(type = "b", previous = list(version = "1.0", sha = NULL)))
  h <- finn_read_json(file.path(finn_settings_dir(), "update_history.json"))
  check("history appends in order with timestamps", length(h) >= 2 && identical(h[[length(h)]]$type, "b") && !is.null(h[[length(h)]]$at))
  check("history keeps nested NULL as NULL", is.null(h[[length(h)]]$previous$sha) && identical(h[[length(h)]]$previous$version, "1.0"))
})

inst <- load_functions("install_finnts.R", parent = common)
with(inst, {
  check("installer rejects unverified GitHub build identity",
    !is.null(finn_install_validation_error(
      source = "github", expected_version = "1.2.0", expected_sha = "abc123",
      installed = list(version = "1.2.0", source = "github", sha = "different"),
      unmet = character(), missing_capabilities = character()
    )))
  check("installer rejects unmet declared dependencies",
    !is.null(finn_install_validation_error(
      source = "cran", expected_version = NULL, expected_sha = NULL,
      installed = list(version = "1.2.0", source = "cran"),
      unmet = "dplyr", missing_capabilities = character()
    )))
  check("installer accepts verified build with all dependencies and capabilities",
    is.null(finn_install_validation_error(
      source = "github", expected_version = "1.2.0", expected_sha = "abc123",
      installed = list(version = "1.2.0", source = "github", sha = "abc123"),
      unmet = character(), missing_capabilities = character()
    )))
  gh_fetch <- function(url, timeout) {
    if (grepl("/commits/", url)) return("{\"sha\":\"abc123\"}")
    if (grepl("finn_common.R$", url)) return("finn_skill_version <- \"9.0.0\"")
    if (grepl("DESCRIPTION$", url)) return("Package: finnts\nVersion: 1.2.0")
    stop("unexpected url ", url)
  }
  t1 <- finn_github_target("main", gh_fetch)
  check("github target resolves sha and versions", identical(t1$sha, "abc123") && identical(t1$finnts_version, "1.2.0") && is.null(t1$error))
  t2 <- finn_github_target("main", function(url, timeout) stop("offline"))
  check("github target reports offline without failing", is.null(t2$sha) && !is.null(t2$error) && identical(t2$ref, "main"))
  cmp <- function(installed, target = t1) finn_compare_finnts(installed, target)$status
  check("compare: not installed", identical(cmp(list(installed = FALSE)), "not_installed"))
  check("compare: unknown when offline", identical(cmp(list(version = "1.0.0"), t2), "unknown"))
  check("compare: same commit", identical(cmp(list(version = "1.2.0", source = "github", sha = "abc123")), "same_commit"))
  check("compare: same sha from cran is not same commit", !identical(cmp(list(version = "1.2.0", source = "cran", sha = "abc123")), "same_commit"))
  check("compare: newer available", identical(cmp(list(version = "1.1.0", source = "cran")), "newer_available"))
  check("compare: downgrade", identical(cmp(list(version = "1.3.0", source = "github", sha = "zzz")), "older_than_installed"))
  check("compare: new commit same version", identical(cmp(list(version = "1.2.0", source = "github", sha = "old")), "new_commit"))
  check("compare: messages are plain text", all(nzchar(vapply(list(
    list(version = "1.1.0"), list(version = "1.3.0"), list(version = "1.2.0", source = "github", sha = "abc123")
  ), function(i) finn_compare_finnts(i, t1)$message, character(1)))))
})

up <- load_functions("update_skill.R", parent = common)
with(up, {
  verifier_env <- new.env(parent = up)
  verifier_env$finn_rscript <- function() "Rscript"
  verifier_args <- NULL
  verifier_env$system2 <- function(command, args, stdout, stderr) {
    verifier_args <<- args
    structure(
      "{\"version\":\"1.2.0\",\"source\":\"github\",\"sha\":\"abc123\"}",
      status = 0L
    )
  }
  fresh_verifier <- up$finn_fresh_finnts_build
  environment(fresh_verifier) <- verifier_env
  fresh_build <- fresh_verifier()
  check("fresh finnts verification loads namespace before reading package identity",
    is.list(fresh_build) && identical(fresh_build$sha, "abc123") &&
      any(grepl("loadNamespace('finnts')", verifier_args, fixed = TRUE)))
  verifier_env$system2 <- function(...) structure("load failed", status = 1L)
  check("fresh finnts verification rejects a process that cannot load finnts",
    is.null(fresh_verifier()))

  check("same-version GitHub builds with unknown identity are not equivalent",
    !finn_same_finnts(list(version = "1.2.0", source = "github", sha = NULL),
      list(version = "1.2.0", source = "github", sha = "abc123")))
  check("identical local or unrecorded-source finnts installs are the same build",
    finn_same_finnts(list(version = "1.2.0", source = "local"), list(version = "1.2.0", source = "local")) &&
      finn_same_finnts(list(version = "1.2.0"), list(version = "1.2.0")) &&
      finn_verified_finnts_build(list(version = "1.2.0", source = "local"), list(version = "1.2.0", source = "local")))
  check("non-GitHub installs still differ by version, source, or recorded commit",
    !finn_same_finnts(list(version = "1.2.0", source = "local"), list(version = "1.3.0", source = "local")) &&
      !finn_same_finnts(list(version = "1.2.0", source = "local"), list(version = "1.2.0")) &&
      !finn_same_finnts(list(version = "1.2.0", source = "local", sha = "a"), list(version = "1.2.0", source = "local", sha = "b")))
  check("fresh rollback verification requires exact GitHub identity",
    finn_verified_finnts_build(list(version = "1.2.0", source = "github", sha = "abc123"),
      list(version = "1.2.0", source = "github", sha = "abc123")) &&
      !finn_verified_finnts_build(list(version = "1.2.0", source = "github", sha = "wrong"),
        list(version = "1.2.0", source = "github", sha = "abc123")))
  #' Build an offline version.json fixture with newest-first versions.
  #' @param versions Version strings; first is the top-level version.
  #' @return JSON text with the same schema as the installed history.
  history_json <- function(versions) {
    jsonlite::toJSON(list(version = versions[[1]], released = "2026-10-12",
      changelog = lapply(versions, function(version) list(
        version = version, date = "2026-10-12", changes = list("Simpler setup.")))),
      auto_unbox = TRUE)
  }
  #' Check the per-version NEWS-style authoring budget without restricting runtime reads.
  #' @param changes List of nonempty change strings for one version.
  #' @return TRUE for 1-5 bullets, each at most 200 characters and at most 800 total.
  concise_changes <- function(changes) {
    sizes <- nchar(unlist(changes, use.names = FALSE), type = "chars")
    length(sizes) >= 1L && length(sizes) <= 5L &&
      all(sizes >= 1L & sizes <= 200L) && sum(sizes) <= 800L
  }
  check("history budget accepts exact bullet and total limits", concise_changes(
    as.list(c(strrep("x", 200), rep(strrep("x", 150), 4)))))
  check("history budget rejects six short bullets", !concise_changes(as.list(rep("Short.", 6))))
  check("history budget rejects a 201-character bullet", !concise_changes(list(strrep("x", 201))))
  check("history budget rejects 801 total characters", !concise_changes(
    as.list(c(rep(strrep("x", 200), 4), "x"))))
  check("history budget rejects missing or empty bullets", !concise_changes(list()) && !concise_changes(list("")))
  check("history budget counts Unicode characters, not bytes", concise_changes(
    list(strrep("\u00e9", 200))))
  notes <- finn_skill_changes(list(skill_dir = skill_dir),
    fetch = function(...) stop("Local history must not contact the network"))
  check("installed history works offline", notes$available && length(notes$entries) >= 1)
  check("latest history matches runtime skill version", identical(notes$entries[[1]]$version, finn_skill_version))
  check("history retains one initial 0.1.0 entry", sum(
    vapply(notes$entries, function(e) identical(e$version, "0.1.0"), logical(1))) == 1L)
  for (entry in notes$entries) {
    check(paste("version history stays concise:", entry$version), concise_changes(entry$changes))
  }
  check("history matches internal JSON schema", identical(names(jsonlite::fromJSON(
    file.path(skill_dir, "version.json"), simplifyVector = FALSE)), c("version", "released", "changelog")) &&
    identical(names(notes$entries[[1]]), c("version", "date", "changes")) &&
    identical(notes$released, notes$entries[[1]]$date) &&
    is.list(notes$entries[[1]]$changes))
  check("current history has no newer installed entries", length(notes$changes_since_installed) == 0)
  missing_notes <- finn_skill_changes(list(skill_dir = file.path(tmp, "no-history")))
  check("older skill without history reports unavailable", !missing_notes$available && grepl("no version history", missing_notes$error))
  remote_notes <- finn_skill_changes(sha = "pinned123", fetch = function(url, timeout) {
    check("release notes pinned to compared commit", grepl("/pinned123/skills/finn/version.json$", url))
    history_json(c("99.0.0", "0.1.0"))
  })
  check("remote history filters changes since installed version", remote_notes$available &&
    length(remote_notes$changes_since_installed) == 1 &&
    identical(remote_notes$changes_since_installed[[1]]$version, "99.0.0"))
  check("remote history keeps older versions", length(remote_notes$entries) == 2)
  unavailable <- finn_skill_changes(sha = "old", fetch = function(...) stop("offline"))
  check("missing remote notes do not masquerade as success", !unavailable$available && grepl("offline", unavailable$error))
  check("invalid history reported", !finn_skill_changes(sha = "x", fetch = function(...) "no version headers")$available)
  check("duplicate history versions reported", !finn_skill_changes(sha = "x", fetch = function(...) history_json(c("0.1.0", "0.1.0")))$available)
  check("empty history reported", !finn_skill_changes(sha = "x", fetch = function(...) "")$available)
  check("version with no notes reported", !finn_skill_changes(sha = "x", fetch = function(...)
    '{"version":"0.1.0","released":"2026-10-12","changelog":[{"version":"0.1.0","date":"2026-10-12","changes":[]}]}')$available)
  check("out-of-order history rejected", !finn_skill_changes(sha = "x", fetch = function(...) history_json(c("0.1.0", "0.2.0")))$available)
  check("mismatched top-level version rejected", !finn_skill_changes(sha = "x", fetch = function(...)
    sub('"version":"0.2.0"', '"version":"0.3.0"', history_json(c("0.2.0", "0.1.0")), fixed = TRUE))$available)
  check("mismatched released date rejected", !finn_skill_changes(sha = "x", fetch = function(...)
    sub('"released":"2026-10-12"', '"released":"2026-10-11"', history_json("0.1.0"), fixed = TRUE))$available)
  check("invalid date rejected", !finn_skill_changes(sha = "x", fetch = function(...)
    gsub("2026-10-12", "2026-02-30", history_json("0.1.0"), fixed = TRUE))$available)
  check("scalar changes instead of array rejected", !finn_skill_changes(sha = "x", fetch = function(...)
    '{"version":"0.1.0","released":"2026-10-12","changelog":[{"version":"0.1.0","date":"2026-10-12","changes":"Wrong shape"}]}')$available)

  # Exercise read-only dispatch with fake network responses and no settings writes.
  entry <- new.env(parent = up)
  entry_args <- list(action = "changes", skill_dir = skill_dir)
  entry$finn_main <- function(fn) fn(entry_args)
  entry$finn_remote_versions <- function(...) stop("Local history must stay offline")
  entry$finn_update_check <- function(...) list(
    update_available = TRUE, finnts_update_available = FALSE, sha = "preview123",
    local_skill_version = finn_skill_version, remote_skill_version = "99.0.0")
  entry$finn_http_get <- function(url, timeout) {
    check("check dispatcher fetches notes at compared SHA", grepl("/preview123/skills/finn/version.json$", url))
    history_json("99.0.0")
  }
  # Rebind the history function so its default fetch uses the isolated seam.
  entry$finn_skill_changes <- finn_skill_changes
  environment(entry$finn_skill_changes) <- entry
  main_expr <- tail(as.list(parse(file.path(scripts_dir, "update_skill.R"))), 1)[[1]]
  handler <- tail(as.list(main_expr[[3]]), 1)[[1]]
  local_result <- eval(handler, entry)
  check("changes action reads offline installed history", identical(local_result$status, "ok") &&
    local_result$data$available && identical(local_result$data$skill_version, finn_skill_version))
  entry_args <- list(action = "check")
  check_result <- eval(handler, entry)
  check("check action includes release notes without installing", identical(check_result$status, "update_available") &&
    identical(check_result$data$release_notes$changes_since_installed[[1]]$version, "99.0.0"))
  entry$finn_update_check <- function(...) list(
    update_available = FALSE, finnts_update_available = TRUE,
    local_skill_version = finn_skill_version, remote_finnts_version = "99.0.0")
  entry$finn_http_get <- function(...) stop("Package-only check does not fetch skill notes")
  package_result <- eval(handler, entry)
  check("package-only check preserves its install route", identical(package_result$status, "finnts_update_available") &&
    is.null(package_result$data$release_notes))
  entry_args <- list(action = "changes", remote = "true", ref = "release")
  entry$finn_remote_versions <- function(ref) {
    check("remote history honors selected ref", identical(ref, "release"))
    list(sha = "preview123")
  }
  entry$finn_http_get <- function(...) history_json("99.0.0")
  online_result <- eval(handler, entry)
  check("remote changes action returns pinned history", identical(online_result$status, "ok") &&
    grepl("/preview123/", online_result$data$source) && length(online_result$data$changes_since_installed) == 1)

  # A fake installed skill and a fake newer skill on "GitHub".
  Sys.setenv(FINN_SETTINGS_DIR = file.path(tmp, "settings-update"))
  installed <- file.path(tmp, "installed-skill")
  dir.create(file.path(installed, "scripts"), recursive = TRUE, showWarnings = FALSE)
  writeLines("# Finn 0.1.0", file.path(installed, "SKILL.md"))
  writeLines("finn_skill_version <- \"0.1.0\"", file.path(installed, "scripts", "finn_common.R"))
  writeLines("x <- 1", file.path(installed, "scripts", "update_skill.R"))
  writeLines("user note", file.path(installed, "old_only.txt"))

  remote_files <- list(
    "version.json" = history_json(c("9.0.0", "0.1.0")),
    "SKILL.md" = "# Finn 9.0.0",
    "scripts/finn_common.R" = "finn_skill_version <- \"9.0.0\"",
    "scripts/update_skill.R" = "x <- 2",
    "scripts/new_script.R" = "y <- 3"
  )
  fetch <- function(url, timeout) {
    if (grepl("version.json$", url)) return(remote_files[["version.json"]])
    if (grepl("/commits/", url)) {
      return("{\"sha\":\"abc123\"}")
    }
    if (grepl("/git/trees/", url)) {
      tree <- lapply(c(paste0("skills/finn/", names(remote_files)), "R/other.R"), function(p) list(path = p, type = "blob"))
      return(jsonlite::toJSON(list(truncated = FALSE, tree = tree), auto_unbox = TRUE))
    }
    if (grepl("finn_common.R$", url)) {
      return(remote_files[["scripts/finn_common.R"]])
    }
    if (grepl("DESCRIPTION$", url)) {
      return("Version: 99.0.0")
    }
    stop("unexpected url ", url)
  }
  download <- function(url, dest) {
    rel <- sub("^.*/skills/finn/", "", url)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    writeLines(remote_files[[rel]], dest)
  }
  fake_finnts <- list(version = "1.0.0", source = "github", sha = "old111", ref = "main")
  finn_installed_finnts <- function() fake_finnts
  installs <- list()
  good_installer <- function(skill_dir, install_args) {
    installs[[length(installs) + 1]] <<- install_args
    ref <- sub("^--ref=", "", grep("^--ref=", install_args, value = TRUE))
    fake_finnts <<- list(version = if (identical(ref, "abc123")) "99.0.0" else "1.0.0", source = "github", sha = ref, ref = ref)
    list(ok = TRUE)
  }
  verify <- function() fake_finnts
  args <- list(skill_dir = installed)

  check("remote skill files keep only skills/finn", setequal(finn_remote_skill_files("abc123", fetch), names(remote_files)))
  check("truncated tree is refused", errors_with(finn_remote_skill_files("x", function(u, t) "{\"truncated\":true,\"tree\":[]}"), "partial"))
  check("git checkout detected", {
    g <- file.path(tmp, "gitrepo", "skills", "finn")
    dir.create(g, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(tmp, "gitrepo", ".git"), showWarnings = FALSE)
    finn_in_git_checkout(g) && !finn_in_git_checkout(installed)
  })
  check("validate rejects a skill with broken R", {
    bad <- file.path(tmp, "bad-skill")
    dir.create(file.path(bad, "scripts"), recursive = TRUE, showWarnings = FALSE)
    writeLines("x", file.path(bad, "SKILL.md"))
    writeLines("finn_skill_version <- \"1\"", file.path(bad, "scripts", "finn_common.R"))
    writeLines("x <- (", file.path(bad, "scripts", "update_skill.R"))
    errors_with(finn_validate_skill_dir(bad), "damaged")
  })
  check("copy_tree never deletes", {
    a <- file.path(tmp, "copy-a")
    b <- file.path(tmp, "copy-b")
    dir.create(a, showWarnings = FALSE)
    dir.create(b, showWarnings = FALSE)
    writeLines("new", file.path(a, "f.txt"))
    writeLines("keep", file.path(b, "only_b.txt"))
    finn_copy_tree(a, b)
    file.exists(file.path(b, "only_b.txt")) && identical(readLines(file.path(b, "f.txt")), "new")
  })
  check("reinstall args for github and cran", identical(finn_reinstall_args(list(version = "1", source = "github", sha = "s")), c("--source=github", "--ref=s")) &&
    identical(finn_reinstall_args(list(version = "1", source = "cran")), c("--source=cran", "--version=1")) &&
    is.null(finn_reinstall_args(list(version = "1", source = "local"))))

  r0 <- finn_do_update(args, fetch = fetch, download = download, installer = good_installer, verify = verify)
  check("update asks for confirmation first", identical(r0$status, "needs_confirmation") && length(installs) == 0 &&
    identical(readLines(file.path(installed, "SKILL.md")), "# Finn 0.1.0"))
  check("update preview shows newer version changes", r0$data$release_notes$available &&
    identical(r0$data$release_notes$changes_since_installed[[1]]$version, "9.0.0"))
  old_fetch <- function(url, timeout) {
    if (grepl("version.json$", url)) stop("history not present at old commit")
    fetch(url, timeout)
  }
  without_notes <- finn_do_update(args, fetch = old_fetch, installer = good_installer, verify = verify)
  check("missing release notes do not block update approval", identical(without_notes$status, "needs_confirmation") &&
    !without_notes$data$release_notes$available && grepl("old commit", without_notes$data$release_notes$error) && length(installs) == 0)

  r1 <- finn_do_update(c(args, confirm = "true"), fetch = fetch, download = download, installer = good_installer, verify = verify)
  check("update succeeds", identical(r1$status, "updated") && isTRUE(r1$data$finnts_updated))
  check("update writes the new skill", identical(readLines(file.path(installed, "SKILL.md")), "# Finn 9.0.0") && file.exists(file.path(installed, "scripts", "new_script.R")))
  check("update includes version history file", identical(readLines(file.path(installed, "version.json")),
    strsplit(as.character(remote_files[["version.json"]]), "\n", fixed = TRUE)[[1]]))
  check("update keeps unknown files and reports them", file.exists(file.path(installed, "old_only.txt")) && "old_only.txt" %in% unlist(r1$data$stale_files))
  check("update installs finnts from the same commit", identical(installs[[1]], c("--source=github", "--ref=abc123")))
  check("update backs up the old skill", {
    b <- finn_list_backups()
    length(b) == 1 && identical(b[[1]]$skill_version, "0.1.0") && identical(b[[1]]$finnts$sha, "old111") &&
      identical(readLines(file.path(b[[1]]$path, "skill", "SKILL.md")), "# Finn 0.1.0")
  })

  # Restore the installed skill to 0.1.0 for the failure test.
  writeLines("# Finn 0.1.0", file.path(installed, "SKILL.md"))
  fake_finnts <- list(version = "1.0.0", source = "github", sha = "old111", ref = "main")
  installs <- list()
  bad_installer <- function(skill_dir, install_args) {
    if (identical(install_args[2], "--ref=abc123")) {
      installs[[length(installs) + 1]] <<- install_args
      fake_finnts <<- list(version = "98.0.0", source = "github", sha = "half", ref = "x")
      return(list(ok = FALSE, message = "compilation failed"))
    }
    good_installer(skill_dir, install_args)
  }
  r2 <- finn_do_update(c(args, confirm = "true"), fetch = fetch, download = download, installer = bad_installer, verify = verify)
  check("failed finnts install restores the skill", identical(r2$status, "error") && isTRUE(r2$data$restored) &&
    identical(readLines(file.path(installed, "SKILL.md")), "# Finn 0.1.0"))
  check("failed finnts install restores the old finnts", length(installs) == 2 && identical(installs[[2]], c("--source=github", "--ref=old111")) &&
    identical(fake_finnts$sha, "old111"))
  check("failed update is recorded", {
    h <- finn_read_json(file.path(finn_settings_dir(), "update_history.json"))
    identical(h[[length(h)]]$type, "skill_update_failed")
  })

  wrong_verify <- function() "1.0.0"
  r3 <- finn_do_update(c(args, confirm = "true"), fetch = fetch, download = download, installer = good_installer, verify = wrong_verify)
  check("update restores when a new session loads the wrong finnts", identical(r3$status, "error") &&
    identical(readLines(file.path(installed, "SKILL.md")), "# Finn 0.1.0"))

  for (previous in list(list(installed = FALSE), list(version = "1.0.0", source = "local"))) {
    fake_finnts <- previous
    incomplete <- finn_do_update(c(args, confirm = "true"), fetch = fetch, download = download,
      installer = bad_installer, verify = verify)
    check(paste("unreproducible prior install reports incomplete recovery", previous$source %||% "absent"),
      identical(incomplete$status, "error") && identical(incomplete$data$restored, FALSE) &&
        length(incomplete$data$restoration_errors) > 0 && !any(grepl("nothing changed", incomplete$next_actions)))
  }
  fake_finnts <- list(version = "1.0.0", source = "github", sha = "old111", ref = "main")
  broken_recovery <- finn_do_update(c(args, confirm = "true"), fetch = fetch, download = download,
    installer = function(skill_dir, install_args) {
      fake_finnts <<- list(version = "98.0.0", source = "github", sha = "half")
      list(ok = FALSE, message = "installer unavailable")
    }, verify = verify)
  check("failed recovery installer is never reported as restored",
    identical(broken_recovery$data$restored, FALSE) && any(grepl("restoration failed", unlist(broken_recovery$data$restoration_errors))))
  history <- finn_read_json(file.path(finn_settings_dir(), "update_history.json"))
  check("failed history records incomplete restoration accurately",
    identical(history[[length(history)]]$restored, FALSE) && length(history[[length(history)]]$restoration_errors) > 0)
  fake_finnts <- list(version = "1.0.0", source = "github", sha = "old111", ref = "main")
  verification_error <- finn_do_update(c(args, confirm = "true"), fetch = fetch, download = download,
    installer = good_installer, verify = function() stop("verification unavailable"))
  check("verification errors trigger recovery and explicit failure",
    identical(verification_error$data$restored, FALSE) &&
      any(grepl("Recovery verification failed", unlist(verification_error$data$restoration_errors))))
  fake_finnts <- list(version = "1.0.0", source = "github", sha = "old111", ref = "main")

  old_version <- finn_skill_version
  finn_skill_version <- "9.0.0"
  r4 <- finn_do_update(args, fetch = fetch, download = download, installer = good_installer, verify = verify)
  check("no update when already current", identical(r4$status, "up_to_date"))
  finn_skill_version <- old_version

  # Full rollback: install the update, then roll back to the backup taken before it.
  r5 <- finn_do_update(c(args, confirm = "true"), fetch = fetch, download = download, installer = good_installer, verify = verify)
  check("second update succeeds", identical(r5$status, "updated") && identical(fake_finnts$version, "99.0.0"))
  rb0 <- finn_do_rollback(args, installer = good_installer, verify = verify)
  check("rollback asks for confirmation", identical(rb0$status, "needs_confirmation") && identical(rb0$data$backup_id, r5$data$backup_id))
  rb1 <- finn_do_rollback(c(args, confirm = "true"), installer = good_installer, verify = verify)
  check("rollback restores skill and finnts", identical(rb1$status, "rolled_back") &&
    identical(readLines(file.path(installed, "SKILL.md")), "# Finn 0.1.0") && identical(fake_finnts$sha, "old111"))
  check("rollback takes a safety backup", !is.null(rb1$data$safety_backup_id) &&
    any(vapply(finn_list_backups(), function(m) identical(m$id, rb1$data$safety_backup_id), logical(1))))
  check("rollback to an unknown backup is a clear error", errors_with(finn_do_rollback(c(args, to = "nope"), installer = good_installer), "No backup named"))

  # Package-only rollback returns to the finnts recorded before the latest change.
  fake_finnts <- list(version = "99.0.0", source = "github", sha = "abc123", ref = "abc123")
  rp <- finn_do_rollback(c(args, package_only = "true", confirm = "true"), installer = good_installer, verify = verify)
  check("package-only rollback reinstalls the previous finnts", identical(rp$status, "rolled_back") && identical(fake_finnts$sha, "old111") &&
    identical(readLines(file.path(installed, "SKILL.md")), "# Finn 0.1.0"))
  check("cran rollback when the old finnts came from CRAN", {
    fake_finnts <- list(version = "2.0.0", source = "github", sha = "z", ref = "z")
    finn_append_history(list(type = "finnts_install", previous = list(version = "0.6.0", source = "cran"), installed = fake_finnts))
    cran_args <- NULL
    rc <- finn_do_rollback(c(args, package_only = "true"), installer = function(d, a) {
      cran_args <<- a
      list(ok = TRUE)
    })
    identical(rc$status, "needs_confirmation") && identical(rc$data$restore_finnts$version, "0.6.0") && is.null(cran_args)
  })
  fake_finnts <- list(version = "1.0.0", source = "local")
  local_backup <- finn_backup_skill(installed, "local finnts fixture")
  local_installs <- 0L
  rl <- finn_do_rollback(c(args, to = local_backup$id, confirm = "true"), installer = function(d, a) {
    local_installs <<- local_installs + 1L
    list(ok = FALSE, message = "must not reinstall an unchanged finnts")
  }, verify = verify)
  check("full rollback with an unchanged local finnts does not report partial restoration",
    identical(rl$status, "rolled_back") && local_installs == 0L &&
      !any(grepl("cannot be restored automatically", unlist(rl$next_actions))))
  Sys.setenv(FINN_SETTINGS_DIR = file.path(tmp, "settings"))
})

# ---- Robustness: locks, damaged files, sync conflicts, disk -------------------------
with(common, {
  lp <- file.path(tmp, "locks", "a.lock")
  t1 <- finn_lock_acquire(lp, timeout = 1)
  check("lock: first caller gets the lock", !is.null(t1))
  check("lock: second caller times out", is.null(finn_lock_acquire(lp, timeout = 0.3)))
  check("lock: only the owner can release", !finn_lock_release(lp, "someone-else") && finn_lock_release(lp, t1))
  check("lock: free again after release", !is.null(t2 <- finn_lock_acquire(lp, timeout = 1)))
  Sys.setFileTime(paste0(lp, ".d"), Sys.time() - 600)
  check("lock: old live owner is never stolen", is.null(finn_lock_acquire(lp, timeout = 0, stale = 120)))
  finn_lock_release(lp, t2)
  check("lock: release archives ownership without deleting", length(list.files(dirname(lp), pattern = "released", all.files = TRUE)) == 2)
  local({
    old_settings <- Sys.getenv("FINN_SETTINGS_DIR")
    Sys.setenv(FINN_SETTINGS_DIR = file.path(tmp, "software-exclusion"))
    on.exit(Sys.setenv(FINN_SETTINGS_DIR = old_settings))
    guard <- finn_software_guard(list(root = file.path(tmp, "no-software-runs")))
    check("software transaction excludes competing launches", is.null(finn_machine_lock_acquire(timeout = 0)))
    nested <- finn_software_guard(list(root = file.path(tmp, "no-software-runs"), software_lock = guard$token))
    check("nested installer reuses verified software lock", identical(nested$owned, FALSE) && identical(nested$token, guard$token))
    check("invalid installer handoff is rejected",
      errors_with(finn_software_guard(list(software_lock = "wrong")), "no longer held"))
    if (requireNamespace("processx", quietly = TRUE)) {
      handoff_script <- file.path(tmp, "software-handoff.R")
      writeLines(c(
        "args <- commandArgs(TRUE)", "source(args[1])",
        "guard <- finn_software_guard(list(software_lock = args[2], root = args[3]))",
        "stopifnot(identical(guard$owned, FALSE), identical(guard$token, args[2]))",
        "stopifnot(is.null(finn_machine_lock_acquire(timeout = 0)))"
      ), handoff_script)
      child <- processx::process$new(finn_rscript(), c(handoff_script,
        file.path(scripts_dir, "finn_common.R"), guard$token, file.path(tmp, "no-software-runs")),
        stdout = "|", stderr = "|")
      child$wait(timeout = 10000)
      check("child installer shares updater lock without deadlock or releasing exclusion",
        identical(child$get_exit_status(), 0L) && is.null(finn_machine_lock_acquire(timeout = 0)))
      if (child$is_alive()) child$kill()
    }
    finn_machine_lock_release(guard$token)
    check("software transaction releases exclusion on completion", !is.null(token <- finn_machine_lock_acquire(timeout = 0)))
    finn_machine_lock_release(token)
    software_root <- file.path(tmp, "software-root")
    software_paths <- finn_project_paths(software_root, "Running")
    finn_ensure_project_dirs(software_paths)
    finn_write_project(software_paths, list(name = "Running"))
    finn_update_state(finn_run_paths(software_paths, "active"), run_id = "active", status = "queued",
      host = Sys.info()[["nodename"]], created_at = finn_now())
    stopped <- finn_software_guard(list(root = software_root))
    check("software transaction refuses active forecasts", identical(stopped$result$status, "blocked"))
    check("update and rollback entry functions block before any software changes",
      identical(up$finn_do_update(list(root = software_root),
        fetch = function(...) stop("must not fetch"))$status, "blocked") &&
        identical(up$finn_do_rollback(list(root = software_root))$status, "blocked"))
    entry <- new.env(parent = inst)
    entry$finn_main <- function(fn) fn(list(root = software_root, confirm = "true"))
    installer_expr <- tail(as.list(parse(file.path(scripts_dir, "install_finnts.R"))), 1)[[1]]
    check("standalone installer blocks before network or package changes",
      identical(eval(installer_expr, entry)$status, "blocked"))
    check("blocked software operation releases its lock", !is.null(token <- finn_machine_lock_acquire(timeout = 0)))
    finn_machine_lock_release(token)
  })
  local({
    rp <- finn_run_paths(finn_project_paths(tmp, "LockedState"), "run")
    finn_update_state(rp, status = "failed", marker = "unchanged")
    sandbox <- new.env(parent = common)
    sandbox$finn_lock_acquire <- function(...) NULL
    locked_update <- finn_update_state
    environment(locked_update) <- sandbox
    before <- readLines(rp$state)
    check("state: lock timeout cannot bypass exclusive ownership",
      errors_with(locked_update(rp, marker = "lost"), "Could not lock run state") &&
        identical(readLines(rp$state), before))
  })
  if (requireNamespace("processx", quietly = TRUE)) {
    race_script <- file.path(tmp, "state-lock-race.R")
    writeLines(c(
      "args <- commandArgs(TRUE)", "source(args[1])",
      "for (i in seq_len(12)) local({",
      "  token <- finn_lock_acquire(args[2], timeout = 20)",
      "  if (is.null(token)) stop('lock timeout')",
      "  on.exit(finn_lock_release(args[2], token))",
      "  n <- as.integer(readLines(args[3]))", "  Sys.sleep(0.02)",
      "  writeLines(as.character(n + 1L), args[3])", "})"
    ), race_script)
    count_file <- file.path(tmp, "state-count.txt")
    writeLines("0", count_file)
    if (requireNamespace("ps", quietly = TRUE)) {
      dir.create(paste0(lp, ".d"))
      finn_write_json(list(token = "dead-owner", pid = 2147483647L,
        host = Sys.info()[["nodename"]]), file.path(paste0(lp, ".d"), "owner.json"))
    }
    children <- lapply(seq_len(3), function(i) processx::process$new(finn_rscript(),
      c(race_script, file.path(scripts_dir, "finn_common.R"), lp, count_file), stdout = "|", stderr = "|"))
    for (child in children) child$wait(timeout = 30000)
    for (child in children) if (!identical(child$get_exit_status(), 0L)) cat(child$read_all_error(), "\n")
    check("lock: concurrent processes preserve every read-modify-write",
      all(vapply(children, function(child) identical(child$get_exit_status(), 0L), logical(1))) &&
        identical(readLines(count_file), "36"))
    for (child in children) if (child$is_alive()) child$kill()
  }

  bad <- file.path(tmp, "bad.json")
  writeLines("{\"status\": \"runn", bad)
  check("json: damaged file raises finn_json_error", inherits(tryCatch(finn_read_json(bad, retry_wait = 0), error = function(e) e), "finn_json_error"))
  check("json: missing file returns default", identical(finn_read_json(file.path(tmp, "none.json"), default = 1), 1))

  rp <- finn_project_paths(file.path(tmp, "robust_root"), "Robust")
  dir.create(file.path(rp$runs, "good"), recursive = TRUE)
  dir.create(file.path(rp$runs, "broken"), recursive = TRUE)
  finn_write_json(list(run_id = "good", status = "completed", created_at = "2024-01-01T00:00:00+0000"), file.path(rp$runs, "good", "state.json"))
  writeLines("{oops", file.path(rp$runs, "broken", "state.json"))
  runs <- finn_list_runs(rp)
  check("runs: one damaged state does not break the listing", length(runs) == 2 &&
    "unreadable" %in% vapply(runs, function(s) s$effective_status, character(1)))

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  old <- format(Sys.time() - 3600, "%Y-%m-%dT%H:%M:%S%z")
  check("status: just-queued run without a worker stays queued", identical(finn_effective_status(list(status = "queued", created_at = now, updated_at = now)), "queued"))
  check("status: long-queued run without a worker is interrupted", identical(finn_effective_status(list(status = "queued", created_at = old, updated_at = old)), "interrupted"))

  dir.create(rp$dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("{}", file.path(rp$dir, "project-LAPTOP.json"))
  writeLines("{}", file.path(rp$runs, "good", "state (1).json"))
  writeLines("{}", file.path(rp$dir, "project_notes.json"))
  cc <- basename(finn_conflict_copies(rp))
  check("conflicts: finds OneDrive conflict copies", all(c("project-LAPTOP.json", "state (1).json") %in% cc))
  check("conflicts: ignores unrelated files", !"project_notes.json" %in% cc && !"state.json" %in% cc)

  if (requireNamespace("ps", quietly = TRUE)) {
    check("disk: warns below the threshold", grepl("GB of disk space", finn_disk_warning(tmp, min_gb = 1e9) %||% ""))
    check("disk: quiet when space is fine", is.null(finn_disk_warning(tmp, min_gb = 0)))
  }
})

# ---- International data: dates, decimal commas, delimiters ----------------------
with(common, {
  amb <- finn_parse_date(c("01/02/2024", "03/04/2024"))
  check("dates: day-month ambiguity is flagged", isTRUE(amb$ambiguous) && identical(amb$alternative, "%d/%m/%Y"))
  first <- finn_parse_date(c("01/01/2024", "01/02/2024", "01/03/2024"))
  check("dates: all-first-of-month picks day/month", !isTRUE(first$ambiguous) && identical(first$format, "%d/%m/%Y") &&
    identical(first$value[2], as.Date("2024-02-01")))
  check("dates: format override wins", identical(finn_parse_date("01/02/2024", format = "%d/%m/%Y")$value, as.Date("2024-02-01")))
  check("dates: unambiguous day > 12 is not flagged", !isTRUE(finn_parse_date(c("31/01/2024", "29/02/2024"))$ambiguous))
  two <- finn_parse_date(c("1/31/24", "2/29/24"))
  check("dates: two-digit years land in this century", identical(two$value, as.Date(c("2024-01-31", "2024-02-29"))))

  eu <- finn_parse_number(c("1.234,50", "2,5", "-3.000,00"))
  check("numbers: decimal comma detected", identical(as.vector(eu), c(1234.5, 2.5, -3000)) && identical(attr(eu, "decimal_mark"), ","))
  check("numbers: explicit decimal_mark is respected", identical(as.vector(finn_parse_number("1.234", decimal_mark = ",")), 1234))
  check("numbers: thousands-only commas stay thousands", identical(as.vector(finn_parse_number(c("1,000", "12,500"))), c(1000, 12500)))

  semi <- file.path(tmp, "eu.csv")
  writeLines(c("Datum;Region;Umsatz", "01.01.2024;Nord;\"1.234,50\"", "01.02.2024;Nord;\"2.000,00\""), semi)
  sd <- finn_load_data(semi)
  check("csv: semicolon files are split into columns", ncol(sd) == 3 && identical(attr(sd, "finn_delimiter"), ";"))
  lat <- file.path(tmp, "latin.csv")
  con <- file(lat, "wb")
  writeBin(c(charToRaw("Region,Value\n"), as.raw(c(0x4d, 0xfc, 0x6e)), charToRaw(",1\n")), con)
  close(con)
  ld <- finn_load_data(lat)
  check("csv: Latin-1 files still load", nrow(ld) == 1 && identical(attr(ld, "finn_encoding"), "latin1"))
})

# ---- Combo identifiers keep their text ---------------------------------------------
id_lines <- c(
  "Region,Date,Revenue",
  "001,2024-01-01,10", "001,2024-02-01,11", "001,2024-03-01,12",
  "1,2024-01-01,20", "1,2024-02-01,21", "1,2024-03-01,22"
)
local({
  ids <- file.path(tmp, "ids.csv")
  writeLines(id_lines, ids)
  kept <- tryCatch(finn_load_data(ids, text_columns = "Region"), error = function(e) conditionMessage(e))
  check("ids: a text column keeps leading zeros and stays apart from 1",
    is.data.frame(kept) && identical(sort(unique(kept$Region)), c("001", "1")) && is.numeric(kept$Revenue))
  absent <- tryCatch(withCallingHandlers(finn_load_data(ids, text_columns = c("Region", "Not a column")),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)), error = function(e) conditionMessage(e))
  check("ids: a text column missing from the file is ignored quietly", is.data.frame(absent) && ncol(absent) == 3)
  blank <- file.path(tmp, "ids-blank.csv")
  writeLines(c("Region,Date,Revenue", "001,2024-01-01,10", ",2024-02-01,11"), blank)
  bl <- tryCatch(finn_load_data(blank, text_columns = "Region"), error = function(e) conditionMessage(e))
  check("ids: a blank cell in a text column stays empty text", is.data.frame(bl) && identical(bl$Region, c("001", "")))

  cfg_ids <- list(project_name = "Ids", combo_variables = "Region", target_variable = "Revenue", date_type = "month",
    forecast_horizon = 3, data = list(file = ids))
  loaded <- tryCatch(finn_load_checked(cfg_ids), error = function(e) conditionMessage(e))
  check("ids: validation keeps 001 and 1 as two series",
    is.list(loaded) && setequal(unique(loaded$data$Region), c("001", "1")) && isTRUE(loaded$facts$n_series == 2) &&
      !any(grepl("repeat the same series", loaded$errors)))

  worker <- load_functions("run_worker.R", common)
  worker$library <- function(...) invisible(NULL)
  seen <- new.env()
  worker$worker_standard <- function(paths, run_paths, state, data) {
    seen$data <- data
    data.frame(x = 1)
  }
  wroot <- file.path(tmp, "worker-ids-root")
  wpaths <- finn_project_paths(wroot, "WorkerIds")
  finn_ensure_project_dirs(wpaths)
  finn_write_project(wpaths, list(name = "WorkerIds"))
  winput <- file.path(wpaths$input, "ids.csv")
  writeLines(id_lines, winput)
  finn_update_state(finn_run_paths(wpaths, "std-1"), run_id = "std-1", mode = "standard", status = "queued",
    created_at = finn_now(), config = finn_merge_config(list(project_name = "WorkerIds", combo_variables = "Region",
      target_variable = "Revenue", date_type = "month", forecast_horizon = 3, data = list(file = winput))))
  worker$finn_main <- function(fn) fn(list(root = wroot, project = "WorkerIds", run_id = "std-1"))
  worker_expr <- tail(as.list(parse(file.path(scripts_dir, "run_worker.R"))), 1)[[1]]
  wres <- tryCatch(eval(worker_expr, worker), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
  check("ids: the worker passes 001 and 1 on as two series",
    isTRUE(wres$ok) && is.data.frame(seen$data) && setequal(unique(seen$data$Region), c("001", "1")))
  failed_state <- list(status = "running", error = NULL)
  worker$finn_update_state <- function(paths, ...) {
    failed_state <<- modifyList(failed_state, list(...))
    failed_state
  }
  worker$finn_result <- common$finn_result
  worker$finn_redact <- common$finn_redact
  worker$finn_now <- common$finn_now
  worker$worker_save_outputs <- function(...) stop("output finalization failed")
  final_result <- tryCatch(worker$worker_finalize_run(list(), list(output = "unused"), failed_state,
    list(forecast = data.frame(x = 1))), error = identity)
  check("worker finalization failure preserves failed state and original error",
    is.list(final_result) && !isTRUE(final_result$ok) && identical(final_result$status, "failed") &&
      identical(failed_state$status, "failed") &&
      identical(failed_state$error, "output finalization failed"))
  completed_state <- list(status = "running", error = NULL)
  worker$finn_update_state <- function(paths, ...) {
    completed_state <<- modifyList(completed_state, list(...))
    completed_state
  }
  worker$worker_save_outputs <- function(...) list(files = list("forecast.csv"))
  worker$finn_log <- function(...) stop("log write failed")
  logged_result <- tryCatch(worker$worker_finalize_run(list(), list(output = "unused"), completed_state,
    list(forecast = data.frame(x = 1))), error = identity)
  check("worker completion-log failure keeps the saved completed state",
    is.list(logged_result) && isTRUE(logged_result$ok) && identical(logged_result$status, "completed") &&
      identical(completed_state$status, "completed") && is.null(completed_state$error))
}, envir = new.env(parent = common))
local({
  # Run the real input-script entry point with isolated arguments and helpers;
  # return its result without sourcing the CLI bootstrap or printing JSON.
  run_input_script <- function(file, args) {
    entry <- load_functions(file, tx)
    entry$finn_main <- function(fn) fn(args)
    eval(tail(as.list(parse(file.path(scripts_dir, file))), 1)[[1]], entry)
  }

  ids <- file.path(tmp, "profile-sample-ids.csv")
  writeLines(c("Region,Date,Revenue,Driver", paste0(id_lines[-1], ",", seq(0.1, 0.6, by = 0.1))), ids)
  source_hash <- unname(tools::md5sum(ids))
  check("ids: all-text CSV loading is opt-in and preserves numeric-looking values", {
    lexical <- finn_load_data(ids, all_text = TRUE)
    inferred <- finn_load_data(ids)
    all(vapply(lexical, is.character, logical(1))) &&
      setequal(lexical$Region, c("001", "1")) && is.numeric(inferred$Revenue) &&
      identical(unique(inferred$Region), 1L)
  })
  excel <- file.path(tmp, "excel-dispatch.xlsx")
  writeLines("Reader stub fixture; no workbook is parsed.", excel)
  excel_env <- new.env(parent = common)
  excel_data <- data.frame(Region = c("001", "1"), Revenue = c(10, 20))
  excel_calls <- list()
  # Capture readxl delegation without adding an Excel package requirement to
  # this offline harness; all other namespace lookups use their real exports.
  excel_env$`::` <- function(package, name) {
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    if (identical(package, "readxl") && identical(name, "read_excel")) {
      return(function(path, sheet) {
        excel_calls[[length(excel_calls) + 1L]] <<- list(path = path, sheet = sheet)
        excel_data
      })
    }
    getExportedValue(package, name)
  }
  # The fake reader needs no installed package; reject unexpected dependencies.
  excel_env$finn_require <- function(pkgs) stopifnot(identical(pkgs, "readxl"))
  excel_loader <- finn_load_data
  environment(excel_loader) <- excel_env
  check("ids: CSV text options leave Excel delegation and cell types unchanged",
    identical(excel_loader(excel, sheet = "2"), excel_data) &&
      identical(excel_loader(excel, sheet = "2", all_text = TRUE, text_columns = "Revenue"), excel_data) &&
      identical(excel_calls, rep(list(list(path = excel, sheet = 2L)), 2)))

  profiled <- run_input_script("profile_data.R", list(file = ids))$data
  check("ids: profile suggests padded combo IDs and counts distinct series",
    identical(profiled$suggested$combo_variables, list("Region")) &&
      profiled$facts$series == 2 && profiled$facts$duplicate_keys == 0)
  check("ids: profile preserves numeric target, decimal regressor, and date suggestions",
    identical(profiled$suggested$target_variable, "Revenue") &&
      identical(profiled$suggested$external_regressor_candidates, list("Driver")) &&
      identical(profiled$suggested$date_column, "Date") && identical(profiled$suggested$date_type, "month"))
  check("ids: profile reports text IDs without changing numeric display types",
    identical(vapply(profiled$columns, `[[`, character(1), "type"), c("character", "character", "integer", "numeric")) &&
      profiled$columns[[1]]$unique == 2 && identical(profiled$columns[[1]]$example[[1]], "001"))

  plain <- file.path(tmp, "profile-plain-ids.csv")
  writeLines(sub("^001,", "2,", readLines(ids)), plain)
  ordinary <- run_input_script("profile_data.R", list(file = plain))$data
  check("ids: unconfigured non-padded numeric columns retain existing inference",
    is.null(ordinary$suggested$combo_variables) && identical(ordinary$suggested$target_variable, "Revenue") &&
      identical(ordinary$suggested$external_regressor_candidates, list("Region", "Driver")) &&
      identical(ordinary$columns[[1]]$type, "integer"))
  blanks <- file.path(tmp, "profile-blank-ids.csv")
  writeLines(c(readLines(ids), ",2024-04-01,30,0.7"), blanks)
  blank_profile <- run_input_script("profile_data.R", list(file = blanks))$data
  check("ids: blank combo cells do not hide padded IDs",
    identical(blank_profile$suggested$combo_variables, list("Region")) && blank_profile$columns[[1]]$missing == 1)

  root <- file.path(tmp, "profile-id-projects")
  paths <- finn_project_paths(root, "ProfileIds")
  finn_ensure_project_dirs(paths)
  finn_write_project(paths, list(name = "ProfileIds", data_file = plain))
  no_config <- run_input_script("profile_data.R", list(root = root, project = "ProfileIds"))$data
  check("ids: a project can still be profiled before its first config",
    identical(no_config$suggested, ordinary$suggested))
  finn_save_project_config(paths, list(combo_variables = "Region", data = list(file = plain)))
  configured <- run_input_script("profile_data.R", list(root = root, project = "ProfileIds"))$data
  check("ids: profile honors configured numeric combo columns",
    identical(configured$suggested$combo_variables, list("Region")) &&
      configured$facts$series == 2 && configured$facts$duplicate_keys == 0 &&
      identical(configured$suggested$external_regressor_candidates, list("Driver")))
  explicit <- run_input_script("profile_data.R", list(root = root, project = "ProfileIds", file = ids))$data
  check("ids: an explicit profile file still uses project combo settings",
    identical(explicit$suggested$combo_variables, list("Region")) && explicit$facts$series == 2)
  finn_save_project_config(paths, list(combo_variables = "Driver", data = list(file = plain)))
  high_cardinality <- run_input_script("profile_data.R", list(root = root, project = "ProfileIds"))$data
  check("ids: configured combo columns do not depend on the inference cardinality limit",
    identical(high_cardinality$suggested$combo_variables, list("Driver")) && high_cardinality$facts$series == 6)
  writeLines("{broken", file.path(paths$dir, "config.json"))
  check("ids: profile surfaces damaged project settings instead of silently guessing",
    errors_with(run_input_script("profile_data.R", list(root = root, project = "ProfileIds")), "Could not read"))

  # A project profile reads the file and sheet a run will read: saved config
  # first, then project metadata. Record what reaches the loader.
  source_root <- file.path(tmp, "profile-source-projects")
  source_paths <- finn_project_paths(source_root, "ProfileSource")
  finn_ensure_project_dirs(source_paths)
  source_old <- file.path(tmp, "profile-source-old.csv")
  writeLines(c("Region,Date,Revenue", "East,2024-01-01,10", "East,2024-02-01,11"), source_old)
  source_prepared <- file.path(tmp, "profile-source-prepared.csv")
  writeLines(c("Region,Date,Revenue", "East,2024-01-01,10", "East,2024-02-01,11", "East,2024-03-01,12"), source_prepared)
  source_manual <- file.path(tmp, "profile-source-manual.csv")
  writeLines(c("Region,Date,Revenue", sprintf("West,2024-%02d-01,%d", 1:4, 5:8)), source_manual)
  finn_write_project(source_paths, list(name = "ProfileSource", data_file = source_old, sheet = "Raw"))
  source_config <- file.path(source_paths$dir, "config.json")
  # Profile the fixture project with optional CLI arguments, returning the
  # selected file, sheet and row count. Local callbacks capture the source
  # without replacing the real reader and invoke the entrypoint with those args.
  profile_source <- function(...) {
    entry <- load_functions("profile_data.R", tx)
    seen <- NULL
    real_load <- entry$profile_load_data
    entry$profile_load_data <- function(file, sheet = NULL, combo_variables = NULL) {
      seen <<- list(file = finn_norm(file), sheet = sheet)
      real_load(file, sheet, combo_variables)
    }
    entry$finn_main <- function(fn) fn(list(root = source_root, project = "ProfileSource", ...))
    out <- eval(tail(as.list(parse(file.path(scripts_dir, "profile_data.R"))), 1)[[1]], entry)
    c(seen, rows = out$data$facts$rows)
  }
  source_inputs <- c(source_old, source_prepared, source_manual)
  source_md5 <- unname(tools::md5sum(source_inputs))
  legacy <- profile_source()
  check("profile: a project without config profiles its metadata file and sheet",
    identical(legacy$file, finn_norm(source_old)) && identical(legacy$sheet, "Raw") && identical(legacy$rows, 2L))
  finn_write_json(list(combo_variables = "Region"), source_config)
  partial <- profile_source()
  check("profile: config without a data file falls back to project metadata",
    identical(partial$file, finn_norm(source_old)) && identical(partial$sheet, "Raw") && identical(partial$rows, 2L))
  finn_save_project_config(source_paths, list(combo_variables = "Region", data = list(file = source_prepared, sheet = "Prepared")))
  config_md5 <- unname(tools::md5sum(c(source_config, source_paths$project_json)))
  configured_source <- profile_source()
  check("profile: a project profiles the saved config file and sheet, not stale metadata",
    identical(configured_source$file, finn_norm(source_prepared)) && identical(configured_source$sheet, "Prepared") &&
      identical(configured_source$rows, 3L))
  explicit_source <- profile_source(file = source_manual, sheet = "Manual")
  check("profile: explicit file and sheet arguments override the saved config",
    identical(explicit_source$file, finn_norm(source_manual)) && identical(explicit_source$sheet, "Manual") &&
      identical(explicit_source$rows, 4L))
  file_only <- profile_source(file = source_manual)
  check("profile: an explicit file does not inherit another file's saved sheet",
    identical(file_only$file, finn_norm(source_manual)) && is.null(file_only$sheet) &&
      identical(file_only$rows, 4L))
  sheet_only <- profile_source(sheet = "Manual")
  check("profile: an explicit sheet keeps the saved config file",
    identical(sheet_only$file, finn_norm(source_prepared)) && identical(sheet_only$sheet, "Manual"))
  check("profile: project profiling leaves data, config, and project metadata unchanged",
    identical(unname(tools::md5sum(source_inputs)), source_md5) &&
      identical(unname(tools::md5sum(c(source_config, source_paths$project_json))), config_md5))
  finn_save_project_config(source_paths, list(combo_variables = "Region", data = list(file = source_prepared)))
  no_saved_sheet <- profile_source()
  check("profile: a configured file without a sheet does not inherit the metadata sheet",
    identical(no_saved_sheet$file, finn_norm(source_prepared)) && is.null(no_saved_sheet$sheet) &&
      identical(no_saved_sheet$rows, 3L))
  distinct <- file.path(tmp, "profile-distinct-numbers.csv")
  writeLines(c("Number,Date,Revenue", sprintf("%03d,2024-01-%02d,%d", 1:6, 1:6, 11:16)), distinct)
  distinct_data <- profile_env$profile_load_data(distinct)
  check("ids: high-cardinality padded numbers are not automatically made combos",
    !length(distinct_data$combo_variables) && identical(distinct_data$data$Number, 1:6))

  out_dir <- file.path(tmp, "sample-id-output")
  all_args <- list(file = ids, sample_series = "2", combo_variables = " Region ", out_dir = out_dir)
  all_sample <- run_input_script("prepare_data.R", all_args)
  kept <- finn_load_data(all_sample$data$output, text_columns = "Region")
  check("ids: sampled CSV preserves 001 and 1 as separate whole series",
    identical(kept$Region, rep(c("001", "1"), each = 3)) && nrow(kept) == 6 &&
      identical(kept$Revenue, c(10:12, 20:22)))
  one_args <- modifyList(all_args, list(sample_series = "1", out_name = "one.csv"))
  one <- run_input_script("prepare_data.R", one_args)
  again <- run_input_script("prepare_data.R", modifyList(one_args, list(out_name = "again.csv")))
  one_data <- finn_load_data(one$data$output, text_columns = "Region")
  again_data <- finn_load_data(again$data$output, text_columns = "Region")
  check("ids: sampling one padded-ID series keeps exactly its complete rows reproducibly",
    nrow(one_data) == 3 && length(unique(one_data$Region)) == 1 &&
      identical(one_data, again_data) &&
      identical(one_data$Date, c("2024-01-01", "2024-02-01", "2024-03-01")))
  check("ids: sampling still requires combo variables",
    errors_with(run_input_script("prepare_data.R", modifyList(all_args, list(combo_variables = ""))), "needs --combo_variables"))

  wide <- file.path(tmp, "wide-sample-ids.csv")
  writeLines(c("Region,Product,2024-01-01,2024-02-01,2024-03-01",
    "001,010,10,11,12", "1,010,20,21,22"), wide)
  pivot <- run_input_script("prepare_data.R", list(file = wide, pivot_longer = "true",
    id_columns = "Region, Product", sample_series = "2", combo_variables = "Region",
    value_name = "Revenue", out_dir = out_dir))
  pivot_data <- finn_load_data(pivot$data$output, text_columns = c("Region", "Product"))
  check("ids: pivoted samples preserve source ID columns as well as sampled combo columns",
    setequal(pivot_data$Region, c("001", "1")) && identical(unique(pivot_data$Product), "010") &&
      nrow(pivot_data) == 6 && identical(sort(pivot_data$Revenue), c(10:12, 20:22)))
  header <- file.path(tmp, "header-sample-ids.csv")
  writeLines(c("V1,V2,V3,V4,V5", readLines(wide)), header)
  header_result <- run_input_script("prepare_data.R", list(file = header, header_row = "1",
    pivot_longer = "true", id_columns = "Region,Product", sample_series = "2",
    combo_variables = "Region", value_name = "Revenue", out_dir = out_dir))
  header_data <- finn_load_data(header_result$data$output, text_columns = c("Region", "Product"))
  check("ids: header promotion cannot coerce IDs before a pivoted sample",
    identical(header_data, pivot_data))
  pivot_all <- run_input_script("prepare_data.R", list(file = wide, pivot_longer = "true",
    id_columns = "Region, Product", value_name = "Revenue", out_dir = out_dir, out_name = "pivot-all.csv"))
  pivot_all_data <- finn_load_data(pivot_all$data$output, text_columns = c("Region", "Product"))
  check("ids: pivoting without sampling keeps leading zeros in declared ID columns",
    setequal(pivot_all_data$Region, c("001", "1")) && identical(unique(pivot_all_data$Product), "010") &&
      nrow(pivot_all_data) == 6 && is.numeric(pivot_all_data$Revenue) &&
      identical(sort(pivot_all_data$Revenue), c(10:12, 20:22)))
  header_all <- run_input_script("prepare_data.R", list(file = header, header_row = "1",
    pivot_longer = "true", id_columns = "Region,Product", value_name = "Revenue",
    out_dir = out_dir, out_name = "header-all.csv"))
  check("ids: header promotion without sampling keeps leading zeros and converts values",
    identical(finn_load_data(header_all$data$output, text_columns = c("Region", "Product")), pivot_all_data))
  check("ids: profiling and sampling never change their source CSV", identical(unname(tools::md5sum(ids)), source_hash))
}, envir = new.env(parent = common))
with(an, {
  ipaths <- finn_project_paths(file.path(tmp, "root"), "AnalysisIds")
  finn_ensure_project_dirs(ipaths)
  writeLines(id_lines, file.path(ipaths$input, "ids.csv"))
  finn_write_project(ipaths, list(name = "AnalysisIds", data_file = "input/ids.csv"))
  finn_save_project_config(ipaths, list(
    project_name = "AnalysisIds", data = list(file = "input/ids.csv"),
    combo_variables = "Region", target_variable = "Revenue", date_type = "month", forecast_horizon = 3
  ))
  ictx <- finn_open("AnalysisIds", root = file.path(tmp, "root"))
  ihist <- tryCatch(finn_history(ictx), error = function(e) conditionMessage(e))
  check("ids: analysis history keeps 001 and 1 as two series",
    is.data.frame(ihist) && nrow(ihist) == 6 && setequal(ihist$Region, c("001", "1")) && setequal(ihist$Combo, c("001", "1")))
})

# ---- Actuals end with future driver rows ------------------------------------------
with(common, {
  d <- data.frame(
    Region = "A", Date = seq(as.Date("2024-01-01"), by = "month", length.out = 8),
    Revenue = c(1:6, NA, NA), Driver = 1:8
  )
  cfg <- list(project_name = "Demo", combo_variables = "Region", target_variable = "Revenue", date_type = "month",
    forecast_horizon = 2, external_regressors = "Driver")
  facts <- finn_check_config(cfg, d)$facts
  check("facts: last actual ignores future driver rows", identical(facts$last_actual_date, "2024-06-01"))
  check("facts: fiscal year start missing is a warning", any(grepl("fiscal_year_start is not set", finn_check_config(cfg)$warnings)))
  check("facts: fiscal year start out of range is an error", any(grepl("fiscal_year_start", finn_check_config(modifyList(cfg, list(fiscal_year_start = 13)))$errors)))
})
with(launch, {
  check("launch: data end uses last actual, not last driver row", identical(launch_data_end(list(), list(last_actual_date = "2024-06-01", date_max = "2024-08-01")), "2024-06-01"))
  check("launch: hist_end_date wins", identical(launch_data_end(list(hist_end_date = "2024-05-01"), list(last_actual_date = "2024-06-01")), "2024-05-01"))
  check("launch: falls back to the last date", identical(launch_data_end(list(), list(date_max = "2024-08-01")), "2024-08-01"))
})

# ---- Stall hints ------------------------------------------------------------------
with(st, {
  sp <- finn_project_paths(file.path(tmp, "stall_root"), "Stall")
  rpp <- finn_run_paths(sp, "s1")
  dir.create(rpp$dir, recursive = TRUE)
  writeLines("x", rpp$log)
  state <- list(run_id = "s1", mode = "standard")
  check("stall: fresh log is not stalled", is.null(status_stall_minutes(state, sp, "running")))
  Sys.setFileTime(rpp$log, Sys.time() - 90 * 60)
  check("stall: quiet standard run is flagged", !is.null(status_stall_minutes(state, sp, "running")))
  check("stall: Agent runs get longer", is.null(status_stall_minutes(modifyList(state, list(mode = "agent")), sp, "running")))
  check("stall: only running runs are checked", is.null(status_stall_minutes(state, sp, "completed")))
})

with(dx, {
  check("diagnose: disk full", identical(diagnose_match("Error: No space left on device")$category, "disk_space"))
  check("diagnose: Windows disk full", identical(diagnose_match("There is not enough space on the disk.")$category, "disk_space"))
})

# ---- Support report ----------------------------------------------------------------
with(dx, {
  masked <- diagnose_mask(
    c("C:\\Users\\jdoe\\OneDrive - Contoso Ltd\\Finn\\x.csv", "C:/Users/jdoe/data", "user jdoe on PC-JDOE01",
      "mail jane.doe@contoso.com", "api_key=abc123secret", "a jdoeish word"),
    homes = "C:\\Users\\jdoe", names = c("jdoe", "PC-JDOE01", "ab")
  )
  check("report: home folder is masked", identical(masked[[1]], "~\\OneDrive - <org>\\Finn\\x.csv") && identical(masked[[2]], "~/data"))
  check("report: user and computer names are masked", identical(masked[[3]], "user <user> on <user>"))
  check("report: e-mail is masked", identical(masked[[4]], "mail <email>"))
  check("report: secret is masked", !grepl("abc123secret", masked[[5]]))
  check("report: names only match whole words", identical(masked[[6]], "a jdoeish word"))

  rp_root <- file.path(tmp, "report_root")
  lp <- finn_project_paths(rp_root, "Report")
  finn_ensure_project_dirs(lp)
  finn_write_project(lp, list(name = "Report"))
  writeLines("Date,Region,Revenue\n2024-01-01,East,987654.321", file.path(lp$dir, "data.csv"))
  finn_write_json(list(mode = "standard", data = list(file = "data.csv"), forecast_horizon = 3), file.path(lp$dir, "config.json"))
  rpp <- finn_run_paths(lp, "r1")
  finn_update_state(rpp, run_id = "r1", mode = "standard", status = "failed",
    error = "Error: cannot allocate vector of size 2.1 Gb", created_at = finn_now())
  writeLines(c("starting run", "token=ghp_abcdefghijklmnopqrstuvwxyz0123456789", "LOGLINE-MARKER"), rpp$log)

  res <- diagnose_run(list(run_id = "r1"), lp)
  file <- diagnose_write_report(res, lp)
  txt <- readLines(file, warn = FALSE)
  check("report: written under support/", file.exists(file) && grepl("/support/finn_support_.*\\.md$", file))
  check("report: has versions and diagnosis", any(grepl("^- Skill:", txt)) && any(grepl("^- Category: memory", txt)))
  check("report: includes settings", any(grepl("forecast_horizon", txt)))
  check("report: includes the log", any(grepl("LOGLINE-MARKER", txt)))
  check("report: token in log is masked", !any(grepl("ghp_abcdefghijklmnop", txt)))
  check("report: does not copy input table values", !any(grepl("987654", txt)))
  check("report: warns that business values may appear",
    any(grepl("does not include input tables", txt, fixed = TRUE)) &&
      any(grepl("may contain business values", txt, fixed = TRUE)) &&
      any(grepl("masked where recognized", txt, fixed = TRUE)) &&
      !any(grepl("has no data values", txt, fixed = TRUE)))
  file2 <- diagnose_write_report(res, lp, include_log = FALSE)
  txt2 <- readLines(file2, warn = FALSE)
  check("report: include_log=false leaves the log out", file2 != file && !any(grepl("LOGLINE-MARKER", txt2)))

  finn_update_state(rpp, error = "invalid value for input name: Revenue 12345.67; ghp_abcdefghijklmnopqrstuvwx")
  writeLines("Error: invalid value for input name: Revenue 76543.21; ghp_abcdefghijklmnopqrstuvwx", rpp$log)
  value_res <- diagnose_run(list(run_id = "r1"), lp)
  with_values <- readLines(diagnose_write_report(value_res, lp), warn = FALSE)
  without_log <- readLines(diagnose_write_report(value_res, lp, include_log = FALSE), warn = FALSE)
  check("report: errors and logs may retain business values but mask recognized secrets",
    any(grepl("12345.67", with_values, fixed = TRUE)) &&
      any(grepl("76543.21", with_values, fixed = TRUE)) &&
      !any(grepl("ghp_abcdefghijklmnopqrstuvwx", with_values, fixed = TRUE)))
  check("report: excluding log still includes saved error and privacy warning",
    any(grepl("12345.67", without_log, fixed = TRUE)) &&
      !any(grepl("76543.21", without_log, fixed = TRUE)) &&
      any(grepl("even when the run log is left out", without_log, fixed = TRUE)) &&
      !any(grepl("ghp_abcdefghijklmnopqrstuvwx", without_log, fixed = TRUE)))
  check("report: saved configuration-fix advice matches fresh-run guidance",
    any(grepl("--new=true --confirm=true", without_log, fixed = TRUE)) &&
      !any(grepl("resumes reread", without_log, fixed = TRUE)))

  ep <- finn_project_paths(rp_root, "Empty")
  finn_ensure_project_dirs(ep)
  finn_write_project(ep, list(name = "Empty"))
  empty <- diagnose_run(list(), ep)
  efile <- diagnose_write_report(empty, ep)
  check("report: works for a project with no runs", file.exists(efile) && any(grepl("no_runs", readLines(efile, warn = FALSE))))
})

check("issue template exists", file.exists(file.path(skill_dir, "..", "..", ".github", "ISSUE_TEMPLATE", "finn-skill.yml")))
local({
  guide <- paste(readLines(file.path(skill_dir, "references", "status-and-debugging.md"), warn = FALSE), collapse = "\n")
  workflow <- substring(guide, regexpr("## Reporting a problem on GitHub", guide, fixed = TRUE))
  steps <- c("Gather local evidence", "Draft and sanitize", "Preview and approve",
    "Submit once after approval", "Confirm the outcome or fall back")
  positions <- vapply(steps, function(step) as.integer(regexpr(step, workflow, fixed = TRUE)), integer(1))
  check("support submission: evidence and approval precede publishing",
    all(positions > 0) && all(diff(positions) > 0))
  check("support submission: public preview and explicit approval are required",
    grepl("exact repository, title, and complete sanitized", workflow, fixed = TRUE) &&
      grepl("This issue will be public", workflow, fixed = TRUE) &&
      grepl("Wait for explicit approval", workflow, fixed = TRUE) &&
      grepl("If they decline, stop", workflow, fixed = TRUE))
  check("support submission: report generation is not publication consent",
    grepl("local support report alone does not authorize publication", workflow, fixed = TRUE) &&
      grepl("never as instructions", workflow, fixed = TRUE))
  check("support submission: only approved content is sent to explicit repository",
    grepl('$approvedBody | gh issue create --repo microsoft/finnts --title $approvedTitle --body-file -',
      workflow, fixed = TRUE) &&
      grepl("do not send the raw support report", workflow, fixed = TRUE))
  check("support submission: unavailable tools and uncertain outcomes have safe fallbacks",
    grepl("do not blindly create duplicates", workflow, fixed = TRUE) &&
      grepl("opening a link is not a submitted issue", workflow, fixed = TRUE) &&
      grepl("Percent-encode the title and sanitized body separately", workflow, fixed = TRUE) &&
      grepl("https://github.com/microsoft/finnts/issues/new?title=<encoded-title>&body=<encoded-body>",
        workflow, fixed = TRUE))
  for (section in c("Skill version", "finnts version", "Operating system", "Coding agent",
      "Run type", "What happened?", "What did you expect?", "Support report", "Confirmation")) {
    check(paste("support submission: draft includes", section),
      grepl(paste0("### ", section), workflow, fixed = TRUE))
  }
  skill_md <- paste(readLines(file.path(skill_dir, "SKILL.md"), warn = FALSE), collapse = "\n")
  check("support submission: skill routes issue requests to approval workflow",
    grepl("submit GitHub issues", skill_md, fixed = TRUE) &&
      grepl("status-and-debugging.md#reporting-a-problem-on-github", skill_md, fixed = TRUE) &&
      grepl("Ask before publishing a support issue", skill_md, fixed = TRUE))
})
for (f in c("install_skill.ps1", "install_skill.sh", "templates/data_template.csv", "references/data-requirements.md",
  "references/glossary.md", "references/starter-prompts.md")) {
  check(paste("onboarding file exists:", f), file.exists(file.path(skill_dir, f)))
}
local({
  ps1 <- paste(readLines(file.path(skill_dir, "install_skill.ps1"), warn = FALSE), collapse = "\n")
  sh <- paste(readLines(file.path(skill_dir, "install_skill.sh"), warn = FALSE), collapse = "\n")
  check("PowerShell installer fetches files from the resolved commit",
    grepl("/commits/", ps1, fixed = TRUE) &&
      grepl("$sha/$prefix$f", ps1, fixed = TRUE) &&
      !grepl("$ref/$prefix$f", ps1, fixed = TRUE))
  # PowerShell treats `?` as part of a bare variable name, so "$x?y" reads $x?y, not $x.
  check("PowerShell installer never places ? directly after a bare variable",
    !grepl("\\$[A-Za-z_][A-Za-z0-9_]*\\?", ps1, perl = TRUE))
  ps_exe <- Sys.which(c("pwsh", "powershell"))
  ps_exe <- ps_exe[nzchar(ps_exe)]
  tree_uri <- regmatches(ps1, regexpr("\"https://api\\.github\\.com/repos/[^\"]*/git/trees/[^\"]*\"", ps1))
  if (length(ps_exe) > 0 && length(tree_uri) == 1L) {
    probe <- file.path(tmp, "ps-tree-uri.ps1")
    writeLines(c("$repo = 'microsoft/finnts'", "$treeSha = '89abcdef0123456789abcdef0123456789abcdef'",
      paste0("Write-Output ", tree_uri)), probe)
    uri_out <- suppressWarnings(system2(ps_exe[[1]], c("-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass",
      "-File", shQuote(probe)), stdout = TRUE, stderr = TRUE))
    check("PowerShell installer requests the tree by its parsed tree SHA", identical(trimws(utils::tail(uri_out, 1)),
      "https://api.github.com/repos/microsoft/finnts/git/trees/89abcdef0123456789abcdef0123456789abcdef?recursive=1"))
  }
  check("shell installer fetches files from the resolved commit",
    grepl("/commits/", sh, fixed = TRUE) &&
      grepl("$sha/skills/finn/$f", sh, fixed = TRUE) &&
      !grepl("$ref/skills/finn/$f", sh, fixed = TRUE))

  sh_exe <- Sys.which("sh")
  if (!nzchar(sh_exe)) {
    check("shell installer parses formatted GitHub commit JSON", FALSE)
  } else {
    to_shell_path <- function(path) {
      if (.Platform$OS.type != "windows") return(path)
      normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
      if (grepl("^[A-Za-z]:/", normalized)) {
        return(paste0("/", tolower(substr(normalized, 1, 1)), substring(normalized, 3)))
      }
      normalized
    }
    fake_root <- file.path(tmp, "shell-installer-json")
    fake_bin <- file.path(fake_root, "bin")
    dir.create(fake_bin, recursive = TRUE, showWarnings = FALSE)
    commit_sha <- "0123456789abcdef0123456789abcdef01234567"
    tree_sha <- "89abcdef0123456789abcdef0123456789abcdef"
    tree_json <- paste0(
      "{\"truncated\":false,\"tree\":[",
      "{\"path\":\"skills/finn/SKILL.md\",\"type\":\"blob\"},",
      "{\"path\":\"skills/finn/version.json\",\"type\":\"blob\"},",
      "{\"path\":\"skills/finn/scripts/finn_common.R\",\"type\":\"blob\"},",
      "{\"path\":\"skills/finn/scripts/update_skill.R\",\"type\":\"blob\"},",
      "{\"path\":\"skills/finn/scripts/install_finnts.R\",\"type\":\"blob\"},",
      "{\"path\":\"skills/finn/scripts/run_forecast.R\",\"type\":\"blob\"}]}"
    )
    tree_file <- file.path(fake_root, "tree.json")
    writeLines(tree_json, tree_file, useBytes = TRUE)
    fake_curl <- file.path(fake_bin, "curl")
    writeLines(c(
      "#!/bin/sh",
      "url=$4",
      "out=$6",
      "printf '%s\\n' \"$url\" >> \"$FAKE_CURL_LOG\"",
      "case \"$url\" in",
      "  */commits/*) cp \"$FAKE_COMMIT_JSON\" \"$out\" ;;",
      "  */git/trees/*) cp \"$FAKE_TREE_JSON\" \"$out\" ;;",
      "  https://raw.githubusercontent.com/*) printf 'fixture' > \"$out\" ;;",
      "  *) exit 2 ;;",
      "esac"
    ), fake_curl)
    Sys.chmod(fake_curl, mode = "0755")

    commit_cases <- list(
      compact = paste0(
        "{\"sha\":\"", commit_sha, "\",\"commit\":{\"tree\":{\"sha\":\"",
        tree_sha, "\"}}}"
      ),
      formatted_reordered = paste0(
        "{\n  \"commit\": { \"tree\": { \"url\": \"fixture\", ",
        "\"sha\": \"", tree_sha, "\" } },\n  \"sha\": \"",
        commit_sha, "\",\n  \"html_url\": \"fixture\"\n}"
      ),
      slash_ref = paste0(
        "{\"sha\":\"", commit_sha, "\",\"commit\":{\"tree\":{\"sha\":\"",
        tree_sha, "\"}}}"
      )
    )
    case_refs <- c(compact = "main", formatted_reordered = "main", slash_ref = "users/me/fix-1")
    installer_ok <- logical(length(commit_cases))
    names(installer_ok) <- names(commit_cases)
    installer_logs <- vector("list", length(commit_cases))
    names(installer_logs) <- names(commit_cases)
    for (case in names(commit_cases)) {
      fixture <- file.path(fake_root, paste0(case, "-commit.json"))
      writeLines(commit_cases[[case]], fixture, useBytes = TRUE)
      log_file <- file.path(fake_root, paste0(case, "-curl.log"))
      dest <- file.path(fake_root, paste0(case, "-install"))
      shell_path <- paste(
        to_shell_path(fake_bin),
        to_shell_path(dirname(sh_exe)),
        sep = if (.Platform$OS.type == "windows") ":" else .Platform$path.sep
      )
      env <- c(
        paste0("export PATH=", shQuote(shell_path, type = "sh")),
        paste0("export FINN_SKILL_DEST=", shQuote(to_shell_path(dest), type = "sh")),
        "export FINN_SKILL_REPO=microsoft/finnts",
        paste0("export FINN_SKILL_REF=", shQuote(case_refs[[case]], type = "sh")),
        paste0("export FAKE_COMMIT_JSON=", shQuote(to_shell_path(fixture), type = "sh")),
        paste0("export FAKE_TREE_JSON=", shQuote(to_shell_path(tree_file), type = "sh")),
        paste0("export FAKE_CURL_LOG=", shQuote(to_shell_path(log_file), type = "sh")),
        paste0("export TMPDIR=", shQuote(to_shell_path(fake_root), type = "sh"))
      )
      wrapper <- file.path(fake_root, paste0(case, "-run-installer.sh"))
      writeLines(c("#!/bin/sh", env,
        paste0("exec ", shQuote(to_shell_path(file.path(skill_dir, "install_skill.sh")), type = "sh"))
      ), wrapper)
      output <- suppressWarnings(system2(
        sh_exe, shQuote(to_shell_path(wrapper), type = "sh"),
        stdout = TRUE, stderr = TRUE
      ))
      status <- attr(output, "status")
      installer_ok[[case]] <- is.null(status) || identical(as.integer(status), 0L)
      installer_logs[[case]] <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character()
      if (!installer_ok[[case]]) {
        message("  shell installer fixture [", case, "] exit ",
          if (is.null(status)) 0L else status, ": ", paste(output, collapse = " | "))
      }
    }
    check("shell installer resolves compact and formatted commit JSON",
      all(installer_ok))
    check("shell installer requests the tree by its parsed tree SHA",
      all(vapply(installer_logs, function(log) any(grepl(
        paste0("/git/trees/", tree_sha, "?recursive=1"), log, fixed = TRUE
      )), logical(1))))
    check("shell installer downloads files from the parsed commit SHA",
      all(vapply(installer_logs, function(log) any(grepl(
        paste0("raw.githubusercontent.com/microsoft/finnts/", commit_sha, "/skills/finn/SKILL.md"),
        log, fixed = TRUE
      )), logical(1))))
    check("shell installer percent-encodes a slash-containing ref for the commit API",
      any(grepl("/commits/users%2Fme%2Ffix-1", installer_logs[["slash_ref"]], fixed = TRUE)) &&
        !any(grepl("/commits/users/me", installer_logs[["slash_ref"]], fixed = TRUE)))
  }
})
local({
  skill_md <- paste(readLines(file.path(skill_dir, "SKILL.md"), warn = FALSE), collapse = "\n")
  for (f in c("data-requirements.md", "glossary.md", "starter-prompts.md", "--report=true")) {
    check(paste("SKILL.md links", f), grepl(f, skill_md, fixed = TRUE))
  }
  tpl <- utils::read.csv(file.path(skill_dir, "templates", "data_template.csv"), stringsAsFactors = FALSE)
  check("data template: has Date, series, target, driver", identical(names(tpl), c("Date", "Region", "Product", "Revenue", "Price")))
  check("data template: future rows have only drivers", any(is.na(tpl$Revenue)) && !any(is.na(tpl$Price)))
  check("data template: no duplicate series/date rows", !any(duplicated(tpl[c("Date", "Region", "Product")])))
})

# ---- Custom models -------------------------------------------------------------------
cm <- load_functions("custom_model.R", common)
vcm <- load_functions("validate_config.R", common)
custom_saved <- mget(c("finn_custom_supported", "finn_custom_verify", "finn_custom_copilot_supported"), envir = common)
assign("finn_custom_supported", function(mode = "standard") TRUE, envir = common)
assign("finn_custom_verify", function(model) invisible(list(status = "approved", message = "Checks passed.")), envir = common)
assign("finn_custom_copilot_supported", function() TRUE, envir = common)
local({
  has_custom_api <- requireNamespace("finnts", quietly = TRUE) &&
    exists("add_custom_recipe", envir = asNamespace("finnts"), inherits = FALSE)
  safely <- function(expr) tryCatch(expr, error = function(e) {
    message("  error: ", conditionMessage(e))
    NULL
  })
  full_a <- strrep("a", 64)
  full_b <- strrep("b", 64)
  fake_model <- function(name, version_id = full_a, model_type = "local", predictors = c("Date", "Combo"),
                         date_types = "month", hierarchy = NULL, packages = character(),
                         created_at = "2024-01-01T00:00:00+0000", interpretation = "Adds last year's growth.") {
    req <- list(predictors = predictors, date_types = date_types)
    if (!is.null(hierarchy)) req$hierarchy <- list(forecast_approach = hierarchy)
    structure(list(
      definition = list(name = name, version_id = version_id, model_type = model_type, requirements = req,
        interpretation = interpretation, created_at = created_at, packages = packages),
      validation = list(checks = list(schema = TRUE, examples = TRUE)), approval = list(mode = "automatic")
    ), class = "finnts_custom_model")
  }
  enroll <- function(name, version_id = full_a) list(list(name = name, version_id = version_id))

  # Config helpers
  check("custom models: empty entries normalize to NULL",
    is.null(finn_custom_normalize(NULL)) && is.null(finn_custom_normalize(list())))
  check("custom models: one object is one entry",
    identical(finn_custom_normalize(list(name = "g", version_id = "x")), list(list(name = "g", version_id = "x"))))
  check("custom models: entries are sorted, trimmed to name/version_id, NA when missing",
    identical(finn_custom_normalize(list(list(name = "b", version_id = "1", extra = TRUE), list(name = "a"))),
      list(list(name = "a", version_id = NA_character_), list(name = "b", version_id = "1"))))
  check("custom models: malformed values are kept for the checks to report",
    identical(finn_custom_normalize("growth"), "growth") && identical(finn_custom_normalize(list("growth")), list("growth")))
  check("custom models: names", identical(finn_custom_names(list()), character()) &&
    identical(finn_custom_names(list(custom_models = "growth")), character()) &&
    identical(finn_custom_names(list(custom_models = list(list(name = "b", version_id = "1"), list(name = "a", version_id = "2")))), c("a", "b")))
  check("custom models: labels are sorted short ids, ? for malformed entries",
    identical(finn_custom_labels(list()), character()) &&
      identical(finn_custom_labels(list(custom_models = "growth")), "?") &&
      identical(finn_custom_labels(list(custom_models = list("x", list(name = "b", version_id = strrep("c", 64))))),
        c("?", "b@cccccccccccc")))
  check("custom models: label text", identical(finn_custom_label_text(character()), "none") &&
    identical(finn_custom_label_text(c("a@1", "b@2")), "`a@1`, `b@2`"))
  check("custom models: invalid names", identical(
    finn_custom_bad_names(c("growth_rule", "g2-x", "Growth", "1abc", "a--b", "all", "arima", NA), builtin = "arima"),
    c("Growth", "1abc", "a--b", "all", "arima", NA)
  ))

  # Merge and fingerprint
  check("custom models: merge drops an empty enrollment",
    !("custom_models" %in% names(finn_merge_config(list(custom_models = list())))))
  pinned_cfg <- list(mode = "agent", project_name = "P", parallel = "auto", data = list(file = "C:/a/data.csv"), forecast_horizon = 6,
    agent = list(max_iter = 3, weighted_mape_goal = 0.03, llm = list(provider = "copilot")))
  plain_hash <- finn_config_hash(finn_merge_config(pinned_cfg))
  e1 <- list(name = "a", version_id = strrep("1", 64))
  e2 <- list(name = "b", version_id = strrep("2", 64))
  check("custom models: an empty enrollment keeps the fingerprint",
    identical(plain_hash, finn_config_hash(finn_merge_config(c(pinned_cfg, list(custom_models = list()))))))
  check("custom models: entry order does not change the fingerprint", identical(
    finn_config_hash(finn_merge_config(c(pinned_cfg, list(custom_models = list(e1, e2))))),
    finn_config_hash(finn_merge_config(c(pinned_cfg, list(custom_models = list(e2, e1)))))
  ))
  check("custom models: enrolling changes the fingerprint",
    !identical(plain_hash, finn_config_hash(finn_merge_config(c(pinned_cfg, list(custom_models = list(e1)))))))

  # Models and recipes for standard runs
  check("custom models: NULL models_to_run stays NULL", is.null(finn_custom_models_to_run(list(custom_models = enroll("growth")))))
  check("custom models: explicit models_to_run gains enrolled models once",
    identical(finn_custom_models_to_run(list(models_to_run = list("arima", "ets"), custom_models = enroll("growth"))), c("arima", "ets", "growth")) &&
      identical(finn_custom_models_to_run(list(models_to_run = list("arima", "growth"), custom_models = enroll("growth"))), c("arima", "growth")))
  check("custom models: recipes unchanged without custom models",
    is.null(finn_custom_recipes(list())) && identical(finn_custom_recipes(list(recipes_to_run = list("R1"))), "R1"))
  if (has_custom_api) {
    check("custom models: recipes add C1",
      "C1" %in% finn_custom_recipes(list(recipes_to_run = list("R1"), date_type = "month", custom_models = enroll("growth"))))
  }

  # Agent parallel plan
  plan <- list(parallel_processing = NULL, inner_parallel = TRUE, num_cores = 4L,
    reason = "Inner parallel: models train in parallel. Uses 4 worker(s); 300 MB per worker is an estimate, not a memory limit. Another run is using 2 cores.")
  acust <- list(mode = "agent", custom_models = enroll("growth"))
  check("custom models: Agent plan unchanged without custom models or inner parallel",
    identical(finn_custom_agent_plan(plan, list(mode = "agent"), 0.1, 4), plan) &&
      identical(finn_custom_agent_plan(modifyList(plan, list(inner_parallel = FALSE)), acust, 0.1, 4), modifyList(plan, list(inner_parallel = FALSE))))
  a4 <- finn_custom_agent_plan(plan, acust, 0.1, 4)
  check("custom models: Agent plan moves inner parallel to series workers",
    identical(a4$parallel_processing, "local_machine") && isFALSE(a4$inner_parallel) && identical(as.integer(a4$num_cores), 4L) &&
      identical(as.integer(a4$claimed_cores), 4L) && grepl("Uses 4 worker(s)", a4$reason, fixed = TRUE) &&
      endsWith(a4$reason, " Another run is using 2 cores."))
  check("custom models: Agent plan caps workers at the series count",
    identical(as.integer(finn_custom_agent_plan(plan, acust, 0.1, 3)$num_cores), 3L))
  check("custom models: Agent plan halves workers after a memory failure",
    identical(as.integer(finn_custom_agent_plan(plan, acust, 0.1, 4, step_down = 1L)$num_cores), 2L))
  for (case in list(list(n = 4, step = 2L), list(n = 1, step = 0L))) {
    s <- finn_custom_agent_plan(plan, acust, 0.1, case$n, step_down = case$step)
    check(sprintf("custom models: Agent plan is sequential (%d series, step down %d)", case$n, case$step),
      is.null(s$parallel_processing) && "num_cores" %in% names(s) && is.null(s$num_cores) &&
        identical(as.integer(s$claimed_cores), 1L) && grepl("^Sequential:", s$reason))
  }

  # Saving, reading, and checking enrolled models
  root <- file.path(tmp, "custom-root")
  p <- finn_project_paths(root, "CustomChecks")
  finn_ensure_project_dirs(p)
  finn_write_project(p, list(name = "CustomChecks"))
  drv <- fake_model("drv", predictors = c("Date", "Combo", "Region", "Price"))
  meta <- finn_custom_metadata(drv, list(provider = "openai", model = "gpt"), 2L, "Region")
  check("custom models: metadata fields", identical(names(meta), c("name", "version_id", "model_type", "drivers",
    "historical_only_drivers", "future_required_drivers", "date_types",
    "hierarchy", "approval", "checks", "provider", "llm_model", "created_at", "examples")) &&
    identical(meta$drivers, list("Price")) && identical(meta$provider, "openai") && identical(meta$examples, 2L))
  saved <- finn_custom_save(p, drv, meta)
  drv_files <- finn_custom_model_files(p, "drv", full_a)
  json_before <- readLines(drv_files$json, warn = FALSE)
  again <- finn_custom_save(p, drv, modifyList(meta, list(provider = "copilot")))
  check("custom models: save never replaces a saved version",
    identical(saved$status, "saved") && identical(again$status, "exists") &&
      file.exists(drv_files$rds) && identical(readLines(drv_files$json, warn = FALSE), json_before))
  for (m in list(fake_model("growthok"), fake_model("weekly", date_types = "week"), fake_model("glob", model_type = "global"),
    fake_model("pkg", packages = "notarealpkg.finn"), fake_model("hier", hierarchy = "standard_hierarchy"))) {
    finn_custom_save(p, m, finn_custom_metadata(m))
  }
  loaded <- finn_load_custom_models(p, list(custom_models = enroll("growthok")))
  check("custom models: load enrolled models by name",
    identical(names(loaded), "growthok") && inherits(loaded$growthok, "finnts_custom_model") &&
      is.null(finn_load_custom_models(p, list())))

  junk <- finn_custom_model_files(p, "junk", full_a)
  dir.create(junk$dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("junk", junk$rds)
  plain <- finn_custom_model_files(p, "plain", full_a)
  dir.create(plain$dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(a = 1), plain$rds)
  other <- finn_custom_model_files(p, "mismatch", full_a)
  dir.create(other$dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(fake_model("other"), other$rds)
  entry <- function(name) list(name = name, version_id = full_a)
  check("custom models: read reports a missing file", errors_with(finn_custom_read(p, entry("missing")), "is not saved in this project"))
  check("custom models: read reports an unreadable file", errors_with(finn_custom_read(p, entry("junk")), "could not be read"))
  check("custom models: read rejects other objects", errors_with(finn_custom_read(p, entry("plain")), "is not a finnts custom model file"))
  check("custom models: read rejects a different model", errors_with(finn_custom_read(p, entry("mismatch")), "does not match its saved file"))
  assign("finn_custom_verify", function(model) stop("tampered.", call. = FALSE), envir = common)
  check("custom models: read reports a failed integrity check", errors_with(finn_custom_read(p, entry("growthok")), "failed its integrity check"))
  assign("finn_custom_verify", function(model) invisible(list(status = "approved", message = "Checks passed.")), envir = common)

  kbase <- finn_merge_config(list(project_name = "CustomChecks", combo_variables = "Region", target_variable = "Revenue",
    date_type = "month", forecast_horizon = 3))
  kcfg <- function(name, ...) {
    cfg <- kbase
    cfg$custom_models <- enroll(name)
    over <- list(...)
    for (k in names(over)) cfg[k] <- list(over[[k]])
    cfg
  }
  hcfg <- function(name = "growthok") {
    kcfg(name, forecast_approach = "standard_hierarchy", run_global_models = FALSE, models_to_run = list("arima"))
  }
  errs <- function(cfg, use_root = root) finn_check_custom_models(cfg, use_root)$errors
  has_error <- function(cfg, pattern, use_root = root) any(grepl(pattern, errs(cfg, use_root), fixed = TRUE))

  assign("finn_custom_supported", function(mode = "standard") FALSE, envir = common)
  check("custom models: check needs a finnts that runs them", has_error(kcfg("growthok"), "cannot run custom models"))
  assign("finn_custom_supported", function(mode = "standard") TRUE, envir = common)
  check("custom models: check needs the project folder", has_error(kcfg("growthok"), "read from the Finn project folder", NULL))
  bad <- kbase
  bad$custom_models <- list(list(name = "growthok"))
  check("custom models: check needs name and version_id", has_error(bad, "must be a list of"))
  check("custom models: check rejects invalid names", has_error(kcfg("Growth"), "must start with a lowercase letter"))
  bad$custom_models <- list(list(name = "growthok", version_id = "abc"))
  check("custom models: check needs the full version id", has_error(bad, "full 64-character version_id"))
  bad$custom_models <- c(enroll("growthok"), enroll("growthok"))
  check("custom models: check rejects duplicate enrollment", has_error(bad, "Enroll each custom model only once"))
  check("custom models: check rejects models_not_to_run overlap",
    has_error(kcfg("growthok", models_not_to_run = list("growthok")), "can't also be in models_not_to_run"))
  agent_k <- kcfg("growthok", mode = "agent")
  agent_k$agent$allow_hierarchical_forecast <- TRUE
  check("custom models: Agent runs can't try hierarchies", has_error(agent_k, "can't try hierarchical forecasts"))
  agent_k <- kcfg("growthok", mode = "agent")
  agent_k$agent$allow_iterate_forecast <- TRUE
  check("custom models: Agent updates can't start a new search", has_error(agent_k, "can't start a new search"))

  h <- hcfg()
  h$external_regressors <- list("Price")
  check("custom models: hierarchy can't use drivers", has_error(h, "can't use drivers"))
  h <- hcfg()
  h$run_local_models <- FALSE
  check("custom models: hierarchy needs local models", has_error(h, "need run_local_models set to true"))
  h <- hcfg()
  h$run_global_models <- TRUE
  check("custom models: hierarchy needs global models off", has_error(h, "need run_global_models set to false"))
  h <- hcfg()
  h$date_type <- "week"
  check("custom models: no weekly hierarchy", has_error(h, "hierarchical forecast of weekly data"))
  h <- hcfg()
  h$models_to_run <- list("growthok")
  check("custom models: hierarchy needs a Finn model too", has_error(h, "at least one of Finn's models"))
  foundation <- if (requireNamespace("finnts", quietly = TRUE)) {
    tryCatch(finnts:::list_foundation_models(), error = function(e) character())
  } else {
    character()
  }
  if (length(foundation)) {
    h <- hcfg()
    h$models_to_run <- list("arima", foundation[[1]])
    check("custom models: hierarchy can't run with foundation models", has_error(h, "can't run with foundation models"))
  }

  check("custom models: date type must match", has_error(kcfg("weekly"), "works with week data, not month data"))
  check("custom models: model type must be turned on",
    has_error(kcfg("glob", run_global_models = FALSE), "is a global model, but this config turns global models off"))
  check("custom models: drivers must be external regressors", has_error(kcfg("drv"), "uses drivers that are not in external_regressors"))
  check("custom models: packages must be installed", has_error(kcfg("pkg"), "needs R packages that are not installed"))
  check("custom models: hierarchy model is not for Agent runs", has_error(kcfg("hier", mode = "agent"), "which Agent runs don't use"))
  check("custom models: hierarchy model needs its approach", has_error(kcfg("hier"), "Set forecast_approach to"))
  check("custom models: bottoms_up model is not for a hierarchy", has_error(hcfg("growthok"), "isn't approved for standard_hierarchy forecasts"))
  ok <- finn_check_custom_models(kcfg("growthok"), root)
  check("custom models: a compatible model passes with its summary",
    !length(ok$errors) && identical(ok$models[[1]]$name, "growthok") && identical(ok$models[[1]]$version, "aaaaaaaaaaaa"))
  ok_h <- finn_check_custom_models(hcfg("hier"), root)
  check("custom models: a hierarchy model passes in its approach",
    !length(ok_h$errors) && identical(ok_h$models[[1]]$hierarchy, "standard_hierarchy"))

  cm_data <- data.frame(
    Date = rep(seq(as.Date("2022-01-01"), by = "month", length.out = 24), 2),
    Region = rep(c("A", "B"), each = 24), Revenue = c(1:24, 101:124) * 1.0, stringsAsFactors = FALSE
  )
  checked <- safely(finn_check_config(kcfg("growthok"), cm_data, root = root, today = as.Date("2025-01-01")))
  check("custom models: config check adds enrolled models to facts",
    !length(checked$errors) && identical(checked$facts$custom_models[[1]]$name, "growthok"))
  no_root <- safely(finn_check_config(kcfg("growthok"), cm_data, root = NULL, today = as.Date("2025-01-01")))
  check("custom models: config check without a project folder fails",
    any(grepl("read from the Finn project folder", no_root$errors, fixed = TRUE)))

  # Agent version rules
  adata <- file.path(tmp, "custom-agent-data.csv")
  file.copy(file.path(skill_dir, "examples", "sample_monthly.csv"), adata)
  amd5 <- finn_data_md5(adata)
  acfg <- modifyList(finn_merge_config(list(project_name = "CustomAgent", combo_variables = "Region", target_variable = "Revenue",
    date_type = "month", forecast_horizon = 6, data = list(file = adata))), list(mode = "agent", agent = list(max_iter = 3)))
  with_growth <- acfg
  with_growth$custom_models <- enroll("growth")
  drift <- finn_settings_drift(acfg, with_growth)
  check("custom models: enrollment is settings drift",
    length(drift) == 1 && grepl("^custom_models .*growth@aaaaaaaaaaaa", drift))
  cont <- finn_iterate_continuation(acfg, with_growth)
  check("custom models: enrollment change can't continue an Agent version",
    identical(cont$kind, "other_changes") && any(grepl("^custom_models ", cont$changed)))
  agent_project <- function(name, prior_cfg) {
    ap <- finn_project_paths(root, name)
    finn_ensure_project_dirs(ap)
    finn_update_state(finn_run_paths(ap, "iter-1"), run_id = "iter-1", mode = "iterate", status = "completed",
      created_at = "2024-01-01T00:00:00+0000", data_end = "2023-12-01", config = prior_cfg, data_md5 = amd5)
    finn_write_project(ap, list(name = name, last_agent = list(status = "completed", run_id = "iter-1", mode = "iterate",
      request_id = "req-1", agent_run_id = "agent-run-1", agent_version = 1L, hist_end_date = "2023-12-01",
      config_hash = finn_config_hash(prior_cfg), data_md5 = amd5)))
    ap
  }
  ap <- agent_project("CustomAgent", acfg)
  ap_same <- agent_project("CustomAgentSame", with_growth)
  newer <- list(last_actual_date = "2024-06-01")
  upd <- launch$launch_decide(ap, "update", with_growth, newer, list())
  check("custom models: update with different custom models needs a new Agent version",
    identical(upd$result$status, "needs_new_version") && identical(upd$result$data$reason, "custom_models_changed") &&
      identical(unlist(upd$result$data$new), "growth@aaaaaaaaaaaa"))
  check("custom models: update with the same custom models proceeds",
    identical(launch$launch_decide(ap_same, "update", with_growth, newer, list())$action, "new"))
  nr <- finn_next_run(ap, with_growth, newer)
  check("custom models: next run recommends a new Agent version when enrollment changed",
    identical(nr$situation, "agent_refresh") && identical(nr$run_type, "iterate") && isTRUE(nr$new_version) &&
      identical(nr$launch, "--mode=iterate --new=true --confirm=true") && identical(nr$alternative$run_type, "update") &&
      identical(nr$custom_models_changed$new, "growth@aaaaaaaaaaaa"))
  check("custom models: next run keeps update when enrollment matches",
    identical(finn_next_run(ap_same, with_growth, newer)$run_type, "update"))
  it <- launch$launch_decide(ap, "iterate", with_growth, list(last_actual_date = "2023-12-01"), list())
  check("custom models: iterate with changed enrollment needs a new Agent version",
    identical(it$result$status, "needs_new_version") && identical(it$result$data$reason, "settings_changed") &&
      any(grepl("^custom_models ", unlist(it$result$data$changed))))

  # Settings review
  sec <- vcm$validate_custom_models_section(list(finn_custom_summary(fake_model("growth"), "Region")), "standard")
  check("custom models: review section lists each model",
    grepl("### Custom models", sec, fixed = TRUE) &&
      grepl("| `growth` | `aaaaaaaaaaaa` | local | none | Adds last year's growth. |", sec, fixed = TRUE) &&
      grepl("compete with Finn's models", sec, fixed = TRUE))
  check("custom models: Agent review explains later runs",
    grepl("later iterate and update runs", vcm$validate_custom_models_section(list(finn_custom_summary(fake_model("growth"))), "iterate"), fixed = TRUE))
  check("custom models: no review section without models", is.null(vcm$validate_custom_models_section(list(), "standard")))
  long <- vcm$validate_custom_models_section(list(finn_custom_summary(fake_model("growth", interpretation = strrep("x", 250)))))
  check("custom models: long interpretations are shortened",
    grepl(paste0(strrep("x", 197), "..."), long, fixed = TRUE) && !grepl(strrep("x", 198), long, fixed = TRUE))
  prev_cfg <- finn_merge_config(list(project_name = "P", combo_variables = "Region", target_variable = "Revenue", date_type = "month", forecast_horizon = 3))
  next_cfg <- prev_cfg
  next_cfg$custom_models <- enroll("growth")
  changes <- vcm$validate_settings_changes(prev_cfg, next_cfg)
  check("custom models: revision view lists enrollment changes",
    grepl("| `custom_models` | none | `growth@aaaaaaaaaaaa` |", changes, fixed = TRUE))

  # custom_model.R helpers
  check("custom_model: split", identical(cm$custom_model_split(" a, b ,a,"), c("a", "b")) && identical(cm$custom_model_split(NULL), character()))
  check("custom_model: name is required", errors_with(cm$custom_model_name(list()), "Pass --name"))
  check("custom_model: name must qualify", errors_with(cm$custom_model_name(list(name = "Bad")), "doesn't qualify"))
  check("custom_model: horizon defaults to the config, capped at 24",
    identical(cm$custom_model_horizon(list(), list(forecast_horizon = 6)), 6L) &&
      identical(cm$custom_model_horizon(list(), list(forecast_horizon = 36)), 24L))
  for (v in c("0", "25", "1.5")) {
    check(paste("custom_model: horizon rejects", v),
      errors_with(cm$custom_model_horizon(list(horizon = v), list(forecast_horizon = 6)), "whole number from 1 through 24"))
  }
  check("custom_model: drivers must be external regressors",
    errors_with(cm$custom_model_drivers(list(drivers = "Price"), list()), "external_regressors first") &&
      identical(cm$custom_model_drivers(list(drivers = "Price"), list(external_regressors = list("Price"))), "Price"))
  instr <- file.path(tmp, "custom-instructions.txt")
  writeLines(c("Use growth.", "Keep it positive."), instr)
  check("custom_model: instructions are required", errors_with(cm$custom_model_instructions(list(instructions = "  ")), "Describe the model"))
  check("custom_model: instructions file must exist",
    errors_with(cm$custom_model_instructions(list(instructions_file = file.path(tmp, "no-such.txt"))), "No instructions file"))
  check("custom_model: instructions file is read",
    identical(cm$custom_model_instructions(list(instructions_file = instr)), "Use growth.\nKeep it positive."))
  header_only <- file.path(tmp, "custom-header.csv")
  writeLines("example,Date,Revenue,expected", header_only)
  check("custom_model: examples file must exist", errors_with(cm$custom_model_read_examples(file.path(tmp, "no-such.csv")), "No examples file"))
  check("custom_model: examples file needs rows", errors_with(cm$custom_model_read_examples(header_only), "has no rows"))

  panel <- function(data, ...) cm$custom_model_panel(data, "Region", "Revenue", date_type = "month", horizon = 3, ...)
  full <- panel(cm_data)
  check("custom_model: panel keeps complete series", identical(full$series_used, 2L) && identical(full$periods, 24L) && nrow(full$data) == 48)
  gap <- cm_data[!(cm_data$Region == "B" & cm_data$Date == as.Date("2022-05-01")), ]
  gp <- panel(gap)
  check("custom_model: panel starts after a gap when that keeps more data",
    identical(gp$hist_start_date, as.Date("2022-06-01")) && identical(gp$series_used, 2L) && identical(gp$periods, 19L))
  gs <- panel(gap, hist_start_date = "2022-01-01")
  check("custom_model: panel honors a chosen start", identical(gs$series_used, 1L) && identical(gs$periods, 24L))
  check("custom_model: panel start must be a data date", errors_with(panel(gap, hist_start_date = "2022-01-15"), "--hist_start_date must be one of"))
  # A period absent from every series leaves no row to reveal it, so the window grid comes from the date type.
  hole <- cm_data[cm_data$Date != as.Date("2022-05-01"), ]
  hp <- tryCatch(panel(hole), error = function(e) conditionMessage(e))
  check("custom_model: panel treats a date missing from every series as a gap",
    is.list(hp) && identical(hp$hist_start_date, as.Date("2022-06-01")) && identical(hp$series_used, 2L) &&
      identical(hp$periods, 19L) && nrow(hp$data) == 38 && !any(hp$data$Date == as.Date("2022-05-01")))
  check("custom_model: panel rejects a start at a date missing from every series",
    errors_with(panel(hole, hist_start_date = "2022-05-01"), "No series has complete actuals from 2022-05-01"))
  month_end <- cm_data
  month_end$Date <- rep(seq(as.Date("2022-02-01"), by = "month", length.out = 24) - 1, 2)
  me <- tryCatch(panel(month_end), error = function(e) conditionMessage(e))
  check("custom_model: panel keeps irregular dates it cannot rebuild from the date type",
    is.list(me) && identical(me$hist_start_date, as.Date("2022-01-31")) && identical(me$series_used, 2L) &&
      identical(me$periods, 24L) && nrow(me$data) == 48)
  check("custom_model: panel needs actuals", errors_with(panel(transform(cm_data, Revenue = NA_real_)), "has no actuals"))
  last_na <- cm_data
  last_na$Revenue[last_na$Date == as.Date("2023-12-01")] <- NA
  check("custom_model: panel needs actuals in the latest period",
    errors_with(panel(last_na, hist_end_date = "2023-12-01"), "No series has actuals"))
  future <- data.frame(Date = rep(seq(as.Date("2024-01-01"), by = "month", length.out = 3), 2), Region = rep(c("A", "B"), each = 3),
    Revenue = NA_real_, stringsAsFactors = FALSE)
  with_price <- rbind(cbind(cm_data, Price = 1), cbind(future, Price = 2))
  dp <- cm$custom_model_panel(with_price, "Region", "Revenue", "Price", "month", 3)
  check("custom_model: panel keeps future driver rows", nrow(dp$data) == 54 && identical(dp$series_used, 2L))
  daily <- data.frame(Date = rep(seq(as.Date("2015-01-01"), by = "day", length.out = 3500), 3),
    Region = rep(c("A", "B", "C"), each = 3500), Revenue = 1, stringsAsFactors = FALSE)
  dd <- cm$custom_model_panel(daily, "Region", "Revenue", date_type = "day", horizon = 3)
  check("custom_model: panel stays within the creation budget", identical(dd$periods, 3333L) && identical(dd$series_used, 3L))

  # custom_model.R actions
  cp <- finn_project_paths(root, "Custom")
  finn_ensure_project_dirs(cp)
  finn_write_project(cp, list(name = "Custom"))
  cdata <- file.path(cp$input, "sample_monthly.csv")
  file.copy(file.path(skill_dir, "examples", "sample_monthly.csv"), cdata)
  finn_write_json(list(project_name = "Custom", combo_variables = "Region", target_variable = "Revenue", date_type = "month",
    forecast_horizon = 3, data = list(file = cdata)), file.path(cp$dir, "config.json"))
  ex_file <- finn_custom_examples_file(cp, "growth")
  dir.create(dirname(ex_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("example,Date,Revenue,expected", "base,2024-01-01,10,"), ex_file)
  check("custom_model: examples keep dates as text", is.character(cm$custom_model_read_examples(ex_file)$Date))

  captured <- NULL
  built <- NULL
  cm$finn_load_checked <- function(cfg, root = NULL) {
    list(config = finn_merge_config(cfg), errors = character(), warnings = character(), data = cm_data)
  }
  cm$finn_build_llm <- function(llm_cfg) {
    built <<- llm_cfg
    "fake-llm"
  }
  author_ok <- function(...) {
    captured <<- list(...)
    fake_model(captured$name)
  }
  cm$custom_model_author <- author_ok
  cargs <- list(root = root, project = "Custom", name = "growth", instructions = "Use last year's growth.", examples = ex_file)
  growth_dir <- file.path(finn_custom_models_dir(cp), "growth")
  r0 <- safely(suppressMessages(cm$custom_model_create(cargs, cp)))
  check("custom_model: create asks for consent before contacting the LLM",
    identical(r0$status, "needs_confirmation") && is.null(captured) && !dir.exists(growth_dir) &&
      identical(r0$data$check_window$series_used, 2L) && identical(r0$data$examples, 1L))
  r1 <- safely(suppressMessages(cm$custom_model_create(c(cargs, list(confirm = "true", enroll = "true")), cp)))
  growth_files <- finn_custom_model_files(cp, "growth", full_a)
  check("custom_model: confirmed create saves and enrolls the approved model",
    isTRUE(r1$ok) && identical(captured$approval, "automatic") && identical(captured$llm, "fake-llm") &&
      nrow(captured$input_data) == 48 && is.null(captured$control) && file.exists(growth_files$rds) && file.exists(growth_files$json) &&
      identical(finn_custom_labels(finn_read_json(file.path(cp$dir, "config.json"))), "growth@aaaaaaaaaaaa"))
  check("custom_model: Copilot creation gets at least 10 minutes", identical(as.numeric(built$timeout), 600))
  r2 <- safely(suppressMessages(cm$custom_model_create(c(cargs, list(confirm = "true")), cp)))
  check("custom_model: create never replaces a saved version",
    isTRUE(r2$ok) && grepl("already saved", r2$message, fixed = TRUE) && identical(r2$data$saved$status, "exists"))
  cm$custom_model_author <- function(...) stop("LLM unavailable.", call. = FALSE)
  r3 <- safely(suppressMessages(cm$custom_model_create(modifyList(cargs, list(name = "broken", confirm = "true")), cp)))
  check("custom_model: a failed creation saves nothing",
    identical(r3$status, "not_created") && !dir.exists(file.path(finn_custom_models_dir(cp), "broken")))
  cm$custom_model_author <- author_ok
  check("custom_model: timeout must be positive",
    errors_with(cm$custom_model_create(c(cargs, list(timeout = "0")), cp), "--timeout must be"))
  assign("finn_custom_copilot_supported", function() FALSE, envir = common)
  check("custom_model: Copilot creation needs a finnts that supports it",
    identical(safely(cm$custom_model_create(cargs, cp))$status, "blocked"))
  assign("finn_custom_copilot_supported", function() TRUE, envir = common)
  assign("finn_custom_supported", function(mode = "standard") FALSE, envir = common)
  check("custom_model: create needs a finnts with custom models", identical(safely(cm$custom_model_create(cargs, cp))$status, "blocked"))
  assign("finn_custom_supported", function(mode = "standard") TRUE, envir = common)
  check("custom_model: hierarchy models must be local without drivers",
    errors_with(cm$custom_model_create(c(cargs, list(forecast_approach = "standard_hierarchy", model_type = "global")), cp),
      "must be local models without drivers"))

  check("custom_model: template never replaces an examples file",
    identical(safely(cm$custom_model_template(list(name = "growth"), cp))$status, "exists"))
  if (has_custom_api && exists("custom_model_examples_template", envir = asNamespace("finnts"), inherits = FALSE)) {
    tp <- safely(cm$custom_model_template(list(name = "fresh"), cp))
    check("custom_model: template writes a new examples file",
      isTRUE(tp$ok) && file.exists(finn_custom_examples_file(cp, "fresh")))
  }

  run_entry <- function(args) {
    entry <- new.env(parent = cm)
    entry$finn_main <- function(fn) fn(args)
    eval(tail(as.list(parse(file.path(scripts_dir, "custom_model.R"), keep.source = FALSE)), 1)[[1]], entry)
  }
  base_args <- list(root = root, project = "Custom")
  act <- function(...) safely(run_entry(c(base_args, list(...))))
  lst <- act(action = "list")
  check("custom_model: list shows saved models and enrollment",
    isTRUE(lst$ok) && length(lst$data$models) == 1 && identical(lst$data$models[[1]]$name, "growth") &&
      isTRUE(lst$data$models[[1]]$versions[[1]]$enrolled))
  shown <- act(action = "show", name = "growth")
  check("custom_model: show explains the enrolled model", isTRUE(shown$ok) && isTRUE(shown$data$model$enrolled))

  mb <- fake_model("growth", version_id = full_b, created_at = "2024-02-01T00:00:00+0000")
  finn_custom_save(cp, mb, finn_custom_metadata(mb))
  versions <- cm$custom_model_versions(cp, "growth")
  check("custom_model: versions are newest first", identical(versions[[1]]$version, "bbbbbbbbbbbb") && length(versions) == 2)
  nojson <- finn_custom_model_files(p, "nojson", strrep("d", 64))
  dir.create(nojson$dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(fake_model("nojson", version_id = strrep("d", 64)), nojson$rds)
  check("custom_model: versions fall back to the model file without metadata",
    identical(cm$custom_model_versions(p, "nojson")[[1]]$version_id, strrep("d", 64)))
  check("custom_model: pick needs a saved model", errors_with(cm$custom_model_pick(list(), "x"), "No custom model named"))
  check("custom_model: pick asks when several versions exist", is.null(cm$custom_model_pick(versions, "growth")))
  check("custom_model: pick reports an unknown version", errors_with(cm$custom_model_pick(versions, "growth", "c"), "No saved version"))
  check("custom_model: pick reports an ambiguous version",
    errors_with(cm$custom_model_pick(list(list(version_id = "abc1"), list(version_id = "abc2")), "m", "abc"), "More than one"))
  nocfg <- finn_project_paths(root, "NoConfig")
  finn_ensure_project_dirs(nocfg)
  check("custom_model: enrollment needs a saved config",
    errors_with(cm$custom_model_set_enrollment(nocfg, "growth", full_a), "no saved config yet"))

  pick <- act(action = "enroll", name = "growth")
  check("custom_model: enroll asks which version", identical(pick$status, "needs_input") && length(pick$data$versions) == 2)
  check("custom_model: enroll asks for consent",
    identical(act(action = "enroll", name = "growth", version = "bbbb")$status, "needs_confirmation"))
  en <- act(action = "enroll", name = "growth", version = "bbbb", confirm = "true")
  check("custom_model: enroll replaces the enrolled version",
    isTRUE(en$ok) && identical(en$data$enrollment$current, "growth@bbbbbbbbbbbb") &&
      identical(en$data$enrollment$previous, "growth@aaaaaaaaaaaa"))
  check("custom_model: show defaults to the enrolled version",
    identical(act(action = "show", name = "growth")$data$model$version, "bbbbbbbbbbbb"))
  check("custom_model: unenroll a model that isn't enrolled", identical(act(action = "unenroll", name = "other")$status, "not_enrolled"))
  check("custom_model: unenroll asks for consent", identical(act(action = "unenroll", name = "growth")$status, "needs_confirmation"))
  un <- act(action = "unenroll", name = "growth", confirm = "true")
  saved_cfg <- finn_read_json(file.path(cp$dir, "config.json"))
  check("custom_model: unenroll removes the entry and keeps the files",
    isTRUE(un$ok) && !("custom_models" %in% names(saved_cfg)) && identical(saved_cfg$target_variable, "Revenue") &&
      file.exists(growth_files$rds) && file.exists(finn_custom_model_files(cp, "growth", full_b)$rds))
  check("custom_model: unknown action", errors_with(run_entry(c(base_args, list(action = "bogus"))),
    "--action must be list, show, template, create, enroll, or unenroll"))

  skill_md <- paste(readLines(file.path(skill_dir, "SKILL.md"), warn = FALSE), collapse = "\n")
  check("custom models: SKILL.md routes to custom_model.R and its reference",
    grepl("custom_model.R", skill_md, fixed = TRUE) && grepl("custom-models.md", skill_md, fixed = TRUE) &&
      file.exists(file.path(skill_dir, "references", "custom-models.md")))
}, envir = new.env(parent = cm))
list2env(custom_saved, envir = common)

# ---- Export ------------------------------------------------------------------------
ex <- load_functions("export_results.R", common)
with(ex, {
  out <- file.path(tmp, "bom.csv")
  export_write_csv(data.frame(Region = "M\u00fcnchen", Value = 1.5), out)
  raw_bytes <- readBin(out, "raw", 200)
  check("export: CSV starts with a UTF-8 BOM for Excel", identical(raw_bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf))))
  check("export: CSV uses Windows line endings", grepl("\r\n", rawToChar(raw_bytes[-(1:3)]), fixed = TRUE))
  back <- utils::read.csv(out, fileEncoding = "UTF-8-BOM", check.names = FALSE)
  check("export: CSV round-trips non-ASCII text", identical(names(back), c("Region", "Value")) && identical(back$Region, "M\u00fcnchen"))
  local({
    paths <- finn_project_paths(file.path(tmp, "export-root"), "Export")
    finn_ensure_project_dirs(paths)
    finn_write_project(paths, list(name = "Export"))
    rp <- finn_run_paths(paths, "finished")
    df <- data.frame(Combo = "A", Date = c("2025-01-01", "2025-02-01"), Forecast = c(10, 20),
      lo_95 = c(8, 18), hi_95 = c(12, 22), Run_Type = "Future_Forecast", Best_Model = "Yes")
    src <- file.path(rp$output, "all_forecasts.csv")
    dir.create(rp$output, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(df, src, row.names = FALSE)
    finn_update_state(rp, run_id = rp$id, status = "completed", results = list(all_forecasts = src),
      config = list(combo_variables = character()))
    entry <- new.env(parent = ex)
    args <- list(root = paths$root, project = paths$name, run_id = rp$id, include_intervals = "true")
    entry$finn_main <- function(fn) fn(args)
    expr <- tail(as.list(parse(file.path(scripts_dir, "export_results.R"))), 1)[[1]]
    refused <- eval(expr, entry)
    check("export: wide intervals rejected before any output is written",
      identical(refused$status, "unsupported_layout") &&
        !dir.exists(file.path(rp$output, "exports")) && any(grepl("--layout=long", refused$next_actions, fixed = TRUE)))
    args$layout <- "long"
    long <- eval(expr, entry)
    exported <- utils::read.csv(long$data$files$forecast,
      fileEncoding = "UTF-8-BOM")
    check("export: long layout preserves requested intervals",
      long$ok && isTRUE(all.equal(exported$lo_95, df$lo_95)) && isTRUE(all.equal(exported$hi_95, df$hi_95)))
    args$layout <- "wide"
    args$include_intervals <- "false"
    wide <- eval(expr, entry)
    exported <- utils::read.csv(wide$data$files$forecast,
      fileEncoding = "UTF-8-BOM", check.names = FALSE)
    check("export: wide forecasts retain date columns without intervals",
      wide$ok && all(df$Date %in% names(exported)) && !any(grepl("^(lo|hi)_", names(exported))))
  })
})

cat(sprintf("\n%d passed, %d failed\n", passes, length(failures)))
if (length(failures)) {
  cat("Failed:\n", paste0("  - ", failures, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}
