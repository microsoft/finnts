# Offline tests for the Finn skill scripts.
# Usage: Rscript skills/finn/tests/test_skill.R
#
# Needs jsonlite and digest (both installed with finnts). Does not train
# models, call an LLM, or touch the user's saved Finn settings: settings and
# projects live in a temporary folder.

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
prep_env <- load_functions("prepare_data.R", common)
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
  lines <- vc$validate_recommendation_lines(big)
  check("validate: card shows size, suggestions, and run type advice",
    any(grepl("very large", lines)) && any(grepl("models_to_run = xgboost", lines)) && any(grepl("Run type advice", lines)))
  local({
    config <- finn_merge_config(list(mode = "standard", run_ensemble_models = FALSE))
    card <- function(config) vc$validate_settings_card(config, list(), list(reason = "offline"))
    check("approval card shows ensembles off separately from averaging",
      any(grepl("Ensemble training: off; simple averaging:", card(config), fixed = TRUE)))
    check("approval card shows explicitly requested ensembles on",
      any(grepl("Ensemble training: on", card(modifyList(config, list(run_ensemble_models = TRUE))), fixed = TRUE)))
    config$run_ensemble_models <- NULL
    check("approval card warns about automatic ensembles",
      any(grepl("auto (may enable ensembles", card(config), fixed = TRUE)))
    config$mode <- "agent"
    check("approval card does not imply standard ensemble setting affects Agent",
      !any(grepl("Ensemble training", card(config), fixed = TRUE)))
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
  h <- launch_config_hash(base_cfg)
  check("hash ignores parallel", identical(h, launch_config_hash(modifyList(base_cfg, list(parallel = "none")))))
  check("hash ignores LLM model", identical(h, launch_config_hash(modifyList(base_cfg, list(agent = list(llm = list(model = "other")))))))
  check("hash ignores data folder", identical(
    launch_config_hash(modifyList(base_cfg, list(data = list(file = "C:/a/data.csv")))),
    launch_config_hash(modifyList(base_cfg, list(data = list(file = "D:/b/data.csv"))))
  ))
  check("hash tracks horizon", !identical(h, launch_config_hash(modifyList(base_cfg, list(forecast_horizon = 12)))))
  agent_cfg <- modifyList(base_cfg, list(mode = "agent"))
  for (change in list(list(model = "other"), list(provider = "openai"),
      list(command = "other-cli"), list(timeout = 99), list(api_key_env = "OTHER_KEY"))) {
    check(paste("Agent hash tracks LLM", names(change)),
      !identical(launch_config_hash(agent_cfg), launch_config_hash(modifyList(agent_cfg, list(agent = list(llm = change))))))
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
    config_hash = launch_config_hash(cfg), data_md5 = launch_data_md5(data_file)
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
      data_md5 = launch_data_md5(data_file))
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
  })

  if (requireNamespace("processx", quietly = TRUE) && requireNamespace("ps", quietly = TRUE)) {
    kpaths <- finn_project_paths(file.path(tmp, "root"), "KillResume")
    finn_ensure_project_dirs(kpaths)
    kcfg <- modifyList(cfg, list(project_name = "KillResume"))
    finn_write_project(kpaths, list(name = "KillResume", last_agent = list(
      status = "completed", agent_run_id = "agent-v1", agent_version = 1L,
      config_hash = launch_config_hash(kcfg), data_md5 = launch_data_md5(data_file)
    )))
    worker <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("-e", "Sys.sleep(120)"))
    krp <- finn_run_paths(kpaths, "iter-20240104-000000")
    finn_update_state(krp,
      run_id = krp$id, mode = "iterate", status = "running", stage = "iterate_forecast",
      created_at = finn_now(), host = Sys.info()[["nodename"]],
      pid = worker$get_pid(), pid_create_time = as.numeric(ps::ps_create_time(ps::ps_handle(worker$get_pid()))),
      config_hash = launch_config_hash(kcfg), data_md5 = launch_data_md5(data_file),
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
    history_json(c("0.2.0", "0.1.0"))
  })
  check("remote history filters changes since installed version", remote_notes$available &&
    length(remote_notes$changes_since_installed) == 1 &&
    identical(remote_notes$changes_since_installed[[1]]$version, "0.2.0"))
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
    local_skill_version = finn_skill_version, remote_skill_version = "0.2.0")
  entry$finn_http_get <- function(url, timeout) {
    check("check dispatcher fetches notes at compared SHA", grepl("/preview123/skills/finn/version.json$", url))
    history_json("0.2.0")
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
    identical(check_result$data$release_notes$changes_since_installed[[1]]$version, "0.2.0"))
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
  entry$finn_http_get <- function(...) history_json("0.2.0")
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
  verify <- function() fake_finnts$version
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
for (f in c("install_skill.ps1", "install_skill.sh", "templates/data_template.csv", "references/data-requirements.md",
  "references/glossary.md", "references/starter-prompts.md")) {
  check(paste("onboarding file exists:", f), file.exists(file.path(skill_dir, f)))
}
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
