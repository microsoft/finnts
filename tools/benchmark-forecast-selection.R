Sys.setenv(NOT_CRAN = "false")
pkgload::load_all(quiet = TRUE, export_all = FALSE, helpers = FALSE)

checkout <- normalizePath(".", winslash = "/", mustWork = TRUE)
fixtures <- new.env(parent = asNamespace("finnts"))
sys.source(file.path(checkout, "tests/testthat/helper-forecast-selection.R"), envir = fixtures)

arguments <- commandArgs(trailingOnly = TRUE)
options <- list(mode = "selection", trials = "3", `max-average` = "3", `catalogue-limit` = "0",
  sizes = "4,16,64,256,1024,4096,10000,20000", seed = "20260905",
  `case-timeout` = "120", `total-timeout` = "1200")
for (argument in arguments) {
  parts <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1]]
  if (length(parts) != 2L || !parts[1] %in% names(options)) stop("Unsupported benchmark argument: ", argument)
  options[[parts[1]]] <- parts[2]
}

measure_catalogue_selection <- function(trial, catalogue, mixed = FALSE) {
  setup <- system.time(fixture <- fixtures$make_catalogue_selection_fixture(trial, catalogue, mixed))[["elapsed"]]
  timing <- new.env(parent = emptyenv())
  timing$starts <- 0L
  timing$ends <- 0L
  timing$average_seconds <- 0
  timing$evaluation_seconds <- 0
  timing$eligible <- 0L
  timing$evaluations <- 0L
  original_start <- getFromNamespace("par_start", "finnts")
  original_end <- getFromNamespace("par_end", "finnts")
  original_select <- getFromNamespace("select_series_forecasts", "finnts")
  testthat::local_mocked_bindings(
    par_start = function(...) {
      timing$starts <- timing$starts + 1L
      result <- original_start(...)
      if (timing$starts == 2L) timing$average_started <- proc.time()[["elapsed"]]
      result
    },
    par_end = function(...) {
      timing$ends <- timing$ends + 1L
      if (timing$ends == 1L && !is.null(timing$average_started)) {
        timing$average_seconds <- proc.time()[["elapsed"]] - timing$average_started
      }
      original_end(...)
    },
    select_series_forecasts = function(...) {
      started <- proc.time()[["elapsed"]]
      result <- original_select(...)
      timing$evaluation_seconds <- timing$evaluation_seconds + proc.time()[["elapsed"]] - started
      timing$evaluations <- timing$evaluations + 1L
      if (timing$evaluations == 1L) timing$eligible <- sum(result$rankings$Eligible)
      result
    },
    .package = "finnts"
  )
  gc(reset = TRUE)
  elapsed <- system.time(result <- finnts::final_models(fixture$run_info,
    average_models = TRUE, max_model_average = 3, weekly_to_daily = FALSE,
    parallel_processing = NULL, inner_parallel = FALSE, num_cores = 1))
  memory <- gc()
  selection <- result$selections$Synthetic
  averages <- choose(timing$eligible, 2) + choose(timing$eligible, 3)
  stopifnot(timing$evaluations == 2L, timing$starts == if (timing$eligible >= 2L) 2L else 1L,
    nrow(selection$rankings) == timing$eligible + averages,
    !is.na(selection$selected_id), identical(length(result$rejected_combos), 0L))
  if (!mixed) stopifnot(timing$eligible == nrow(catalogue), all(selection$rankings$Eligible))
  if (averages > 0) {
    saved <- finnts:::read_selection_file(fixture$run_info, "forecasts", "-average_models", "Synthetic")
    stopifnot(length(unique(saved$Model_ID)) == 1L, all(is.finite(saved$Forecast)))
  }
  data.frame(Trial = trial, Mixed = mixed, Base_Candidates = nrow(catalogue),
    Eligible = timing$eligible, Averages = averages, Ranked_Candidates = nrow(selection$rankings),
    Setup_Seconds = setup, Average_Loop_Seconds = timing$average_seconds,
    Evaluation_Seconds = timing$evaluation_seconds, Total_Seconds = elapsed[["elapsed"]],
    User_Seconds = elapsed[["user.self"]], System_Seconds = elapsed[["sys.self"]],
    Input_MiB = round(as.numeric(object.size(fixture)) / 1024^2, 1),
    Result_MiB = round(as.numeric(object.size(result)) / 1024^2, 1),
    Peak_Vector_MiB = round(memory["Vcells", 5] * 8 / 1024^2, 1))
}

run_hts_stress <- function() {
  sizes <- as.integer(strsplit(options$sizes, ",", fixed = TRUE)[[1]])
  total_limit <- as.numeric(options[["total-timeout"]])
  case_limit <- as.numeric(options[["case-timeout"]])
  stopifnot(all(is.finite(sizes)), all(sizes >= 4), total_limit > 0, case_limit > 0)
  core <- expand.grid(approach = c("standard_hierarchy", "grouped_hierarchy"),
    placement = c("root", "aggregate", "leaf", "siblings", "all"),
    pathology = c("magnitude", "trend", "level", "phase", "amplitude", "flatten"),
    stringsAsFactors = FALSE)
  cases <- lapply(seq_len(nrow(core)), function(index) as.list(core[index, ]))
  corner_cases <- list(
    list(pathology = "clean", shape = "flat"), list(pathology = "clean", shape = "noisy"),
    list(pathology = "clean", shape = "zero"), list(pathology = "level", shape = "intermittent"),
    list(pathology = "phase", history_cycles = 1L), list(pathology = "phase", history_cycles = 2L),
    list(pathology = "trend", date_type = "day"), list(pathology = "level", date_type = "week"),
    list(pathology = "amplitude", date_type = "quarter"), list(pathology = "trend", date_type = "year"),
    list(pathology = "phase", horizon = 3L), list(pathology = "trend", horizon = 24L),
    list(pathology = "level", shape = "signed"), list(pathology = "magnitude", recipe = "R2")
  )
  cases <- c(cases, corner_cases)
  started <- proc.time()[["elapsed"]]
  results <- list()
  execute_case <- function(specification, label) {
    remaining <- total_limit - (proc.time()[["elapsed"]] - started)
    if (remaining <= 0) return(list(Status = "total budget reached", Case = label))
    series_count <- specification$series_count
    if (!is.null(series_count)) {
      memory <- ps::ps_system_memory()
      available <- if ("available" %in% names(memory)) memory[["available"]] else memory[["free"]]
      estimated <- (series_count + 4 * ceiling(sqrt(series_count)) + 1) * 48 * 3 * 24 * 8
      if (available - estimated < 2 * 1024^3) return(list(Status = "memory preflight stopped", Case = label))
    }
    tryCatch({
      result <- callr::r(function(checkout, specification, seed) {
        Sys.setenv(NOT_CRAN = "false")
        set.seed(seed)
        pkgload::load_all(checkout, quiet = TRUE, helpers = FALSE)
        helpers <- new.env(parent = asNamespace("finnts"))
        sys.source(file.path(checkout, "tests/testthat/helper-forecast-selection.R"), envir = helpers)
        fixture <- do.call(helpers$make_pre_reconciliation_case, specification)
        helpers$measure_pre_selection_case(fixture)
      }, args = list(checkout = checkout, specification = specification, seed = as.integer(options$seed)),
      timeout = min(case_limit, remaining), show = FALSE)
      c(list(Status = "completed", Case = label), as.list(result))
    }, error = function(error) list(Status = if (inherits(error, "callr_timeout_error")) "time safety stop" else "execution limit",
      Case = label, Message = conditionMessage(error)))
  }
  for (case_index in seq_along(cases)) {
    result <- execute_case(cases[[case_index]], paste0("paired-", case_index))
    results[[length(results) + 1L]] <- result
    cat(jsonlite::toJSON(result, auto_unbox = TRUE, na = "null"), "\n")
    if (result$Status == "total budget reached") break
  }
  for (approach in c("standard_hierarchy", "grouped_hierarchy")) {
    for (series_count in sizes) {
      result <- execute_case(list(approach = approach, series_count = series_count,
        pathology = "level", placement = "leaf"), paste(approach, series_count, sep = "/"))
      results[[length(results) + 1L]] <- result
      cat(jsonlite::toJSON(result, auto_unbox = TRUE, na = "null"), "\n")
      if (result$Status != "completed") break
    }
  }
  cat("Stress observations are not a guarantee of improvement or a production runtime limit.\n")
  invisible(results)
}

if (options$mode == "selection") {
  stopifnot(as.integer(options[["max-average"]]) == 3L, as.integer(options$trials) >= 1L)
  catalogue <- fixtures$selection_benchmark_catalogue()
  limit <- as.integer(options[["catalogue-limit"]])
  if (limit > 0L) catalogue <- utils::head(catalogue, limit)
  stopifnot(nrow(catalogue) >= 3L)
  cat("Catalogue candidates:", nrow(catalogue), "; pair/triple averages:",
    choose(nrow(catalogue), 2) + choose(nrow(catalogue), 3), "\n")
  results <- list()
  for (trial in c("warmup", paste0("measured-", seq_len(as.integer(options$trials))), "mixed-quality")) {
    cat("Starting", trial, "with fresh artifacts.\n")
    measurement <- measure_catalogue_selection(trial, catalogue, mixed = trial == "mixed-quality")
    print(measurement, row.names = FALSE)
    results[[length(results) + 1L]] <- measurement
  }
  measured <- do.call(rbind, results)
  measured <- measured[grepl("^measured-", measured$Trial), ]
  cat("Measured full-call seconds: median", stats::median(measured$Total_Seconds),
    "; maximum", max(measured$Total_Seconds), ". No elapsed-time cap was applied.\n")
} else if (options$mode == "hts-stress") {
  run_hts_stress()
} else {
  stop("Unsupported benchmark mode.")
}