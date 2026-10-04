# Build a small real-artifact global run without training or provider calls.
# Members define current bottom identities; hierarchy preparation and the solver
# are real. The source choice is either heterogeneous, uniform, or deliberately
# absent to model a legacy update. Optional levels supply named target levels;
# local runs contain one member. Returns the run, fits, and expected selections.
make_update_chain_case <- function(path, run_name = "previous",
                                   approach = "standard_hierarchy",
                                   members = data.frame(Region = c("North", "North", "South", "South"),
                                     Product = c("A", "B", "A", "B")),
                                   models = c("xgboost", "chronos2"), recipes = rep("R1", length(models)),
                                   legacy = FALSE, uniform = legacy, data_output = "rds",
                                   date_type = "month", omit_sources = character(), model_type = "global",
                                   levels = NULL) {
  info <- list(project_name = paste0("chain_", hash_data("all")), run_name = run_name,
    path = path, storage_object = NULL, data_output = data_output, object_output = "rds")
  calendar <- seq(as.Date("2020-01-01"), by = date_type, length.out = 42)
  input <- dplyr::bind_rows(lapply(seq_len(nrow(members)), function(index) {
    data.frame(members[index, , drop = FALSE], Combo = paste(members[index, ], collapse = "--"),
      Date = calendar[1:36], Target = 100, row.names = NULL)
  }))
  if (!is.null(levels)) input$Target <- unname(levels[input$Combo])
  global <- identical(model_type, "global")
  model_combo <- if (global) "All-Data" else unique(input$Combo)
  if (!global) info$project_name <- paste0("chain_", hash_data(model_combo))
  if (approach == "bottoms_up") {
    prepared <- input
    hierarchy <- list(original_combos = unique(input$Combo), hts_combos = unique(input$Combo))
  } else {
    prepared <- prep_hierarchical_data(input, info, c("Region", "Product"), NULL,
      approach, if (date_type == "week") 52 else 12)
    hierarchy <- read_selection_hierarchy(info)
  }
  ids <- paste(models, model_type, recipes, sep = "--")
  average_id <- paste(sort(ids), collapse = "_")
  winners <- stats::setNames(vapply(seq_along(hierarchy$hts_combos), function(index) {
    if (length(ids) == 1L) ids else if (uniform || index %% 3L == 1L) average_id else ids[(index %% length(ids)) + 1L]
  }, character(1)), hierarchy$hts_combos)
  sources <- list()
  contexts <- list()
  for (combo in hierarchy$hts_combos) {
    value <- prepared$Target[match(combo, prepared$Combo)]
    fixture <- make_selection_case(actuals = rep(value, 36),
      futures = list(template = rep(value, 6)), errors = c(template = 0), date_type = date_type)
    context <- fixture$context
    context$history <- fixture$history
    contexts[[combo]] <- context
    template <- dplyr::bind_rows(fixture$backtests, fixture$forecasts)
    source <- dplyr::bind_rows(lapply(seq_along(ids), function(index) {
      rows <- template
      rows$Combo <- combo
      rows$Combo_ID <- model_combo
      rows$Model_Name <- models[index]
      rows$Model_Type <- model_type
      rows$Recipe_ID <- recipes[index]
      rows$Model_ID <- ids[index]
      rows$Hyperparameter_ID <- 1L
      rows$Run_Type <- ifelse(rows$Train_Test_ID == 1L, "Future_Forecast", "Back_Test")
      rows$Forecast <- value * (1 + (index - mean(seq_along(ids))) * 0.02)
      if (winners[[combo]] %in% ids) {
        rows$Forecast <- value * if (ids[index] == winners[[combo]]) 1 else 1.08
      }
      rows$Best_Model <- ifelse(rows$Model_ID == winners[[combo]], "Yes", "No")
      rows
    }))
    source <- source %>% dplyr::group_by(Model_ID, Train_Test_ID) %>%
      dplyr::mutate(Horizon = dplyr::row_number()) %>% dplyr::ungroup()
    average <- source[0, ]
    if (length(ids) > 1L && identical(winners[[combo]], average_id)) {
      average <- source[source$Model_ID == ids[1], ]
      average$Forecast <- value
      average$Model_ID <- average_id
      average$Model_Name <- NA_character_
      average$Model_Type <- "local"
      average$Recipe_ID <- "simple_average"
      average$Best_Model <- "Yes"
    }
    rows <- dplyr::bind_rows(source, average)
    sources[[combo]] <- rows
    for (recipe in unique(recipes)) {
      data <- data.frame(Combo = combo, Date = context$calendar,
        Target = context$history$Target[match(context$calendar, context$history$Date)])
      if (recipe == "R2") data$Horizon <- 1L
      write_data(data, combo, info, "data", "prep_data", paste0("-", recipe))
    }
    if (!legacy && !combo %in% omit_sources) {
      write_data(create_prediction_intervals(source[, setdiff(names(source), "Run_Type")], context$train_test_split),
        combo, info, "data", "forecasts", if (global) "-global_models" else "-single_models")
      if (nrow(average) || !global) {
        write_data(create_prediction_intervals(average[, setdiff(names(average), "Run_Type")], context$train_test_split),
          combo, info, "data", "forecasts", "-average_models")
      }
    }
  }
  splits <- contexts[[1]]$train_test_split
  source <- dplyr::bind_rows(sources)
  fit <- stats::lm(stats::as.formula("mpg ~ wt", env = baseenv()), data = datasets::mtcars)
  fitted <- dplyr::bind_rows(lapply(seq_along(ids), function(index) {
    rows <- source[source$Model_ID == ids[index], ]
    tibble::tibble(Combo_ID = model_combo, Model_Name = models[index], Model_Type = model_type,
      Recipe_ID = recipes[index],
      Forecast_Tbl = list(rows[, c("Combo", "Train_Test_ID", "Date", "Target",
        "Forecast", "Run_Type", "Hyperparameter_ID")]), Model_Fit = list(fit))
  }))
  saved_models <- fitted[, setdiff(names(fitted), "Forecast_Tbl")]
  saved_models$Model_ID <- ids
  log <- data.frame(project_name = info$project_name, run_name = run_name,
    path = path, data_output = data_output, object_output = "rds",
    combo_variables = paste(names(members), collapse = "---"),
    models_to_run = paste(unique(models), collapse = "---"), recipes_to_run = paste(unique(recipes), collapse = "---"),
    external_regressors = NA_character_, lag_periods = NA_character_, rolling_window_periods = NA_character_,
    seasonal_period = 12, forecast_approach = approach, date_type = date_type, negative_forecast = FALSE,
    box_cox = FALSE, stationary = FALSE, feature_selection = FALSE, global_model_recipes = paste(unique(recipes), collapse = "---"),
    average_models = length(ids) > 1L, max_model_average = length(ids), weekly_to_daily = FALSE,
    pca = FALSE, num_hyperparameters = 1, forecast_horizon = 6, multistep_horizon = FALSE,
    run_global_models = global, run_local_models = !global, run_ensemble_models = FALSE,
    clean_missing_values = TRUE, clean_outliers = FALSE, hist_start_date = calendar[1],
    hist_end_date = calendar[36], weighted_mape = 0.1, agent_version = 1,
    agent_forecast_approach = approach, seed = 123, inner_parallel = FALSE)
  write_data(log, NULL, info, "log", "logs")
  write_data(saved_models, model_combo, info, "object", "models", "-single_models")
  write_data(splits, NULL, info, "data", "prep_models", "-train_test_split")
  for (suffix in c("-model_hyperparameters", "-model_workflows")) {
    write_data(data.frame(Hyperparameter_ID = 1), NULL, info, "object", "prep_models", suffix)
  }
  selected <- source[source$Best_Model == "Yes", ]
  if (approach != "bottoms_up") {
    selected <- reconcile(selected, info, approach, FALSE)
    write_data(create_prediction_intervals(selected, splits), "Best-Model", info, "data", "forecasts", "-reconciled")
  }
  list(info = info, log = log, hierarchy = hierarchy, input = input, source = source,
    fitted = fitted, models = saved_models, ids = ids, winners = winners, splits = splits,
    contexts = contexts, selected = selected, members = members)
}

# Execute the real updater against prepared current artifacts and predecessor
# files. Only input/preparation and expensive fitting are substituted; selection,
# assessment, reconciliation, publication, and completion records remain real.
# Optional dropped writes simulate interruption. Returns observed fits/reads,
# status, and the parent identity for inspecting persisted completion metadata.
# Explicit agent/combos let parent-chain tests use real routed completion groups.
run_update_chain_step <- function(previous, current, drop_source = NULL, agent = NULL, combos = NULL,
                                  drop_suffix = "-global_models") {
  parent <- current$info
  parent$project_name <- "chain"
  parent$date_type <- current$log$date_type
  parent$combo_variables <- names(current$members)
  parent$fiscal_year_start <- 1
  if (is.null(agent)) agent <- list(project_info = parent, run_id = paste0("parent-", current$info$run_name),
    agent_version = 2, forecast_approach = current$log$forecast_approach,
    forecast_horizon = 6, external_regressors = NULL, hist_start_date = NULL,
    hist_end_date = current$log$hist_end_date, combo_cleanup_date = NULL,
    back_test_scenarios = 1, back_test_spacing = 1)
  if (is.null(combos)) combos <- intersect(previous$hierarchy$original_combos, current$hierarchy$original_combos)
  global <- isTRUE(current$log$run_global_models)
  best <- data.frame(combo = combos, best_run_name = previous$info$run_name,
    model_type = if (global) "global" else "local", weighted_mape = 0.1)
  state <- new.env(parent = emptyenv())
  state$fits <- list()
  state$model_reads <- 0L
  reader <- read_file
  listing <- list_files
  artifact_reader <- read_update_artifact
  writer <- write_data
  testthat::local_mocked_bindings(
    list_files = function(storage_object, path, ...) {
      if (grepl("/input_data/", path, fixed = TRUE)) return("chain-input")
      if (current$log$forecast_approach == "bottoms_up") return(listing(storage_object, path, ...))
      stop("Update chain unexpectedly listed artifacts: ", path)
    },
    read_file = function(run_info, path = NULL, file_list = NULL, ...) {
      if (identical(file_list, "chain-input") ||
        any(grepl("/input_data/", file_list, fixed = TRUE))) return(current$input)
      reader(run_info, path = path, file_list = file_list, ...)
    },
    read_update_artifact = function(info, path, ...) {
      if (identical(path, local_artifact_path(previous$info, "models", "-single_models",
        hash_data(if (global) "All-Data" else combos[[1]]), "rds"))) state$model_reads <- state$model_reads + 1L
      artifact_reader(info, path, ...)
    },
    set_run_info = function(...) current$info,
    prep_data = function(...) NULL,
    prep_models = function(...) NULL,
    get_prepped_models = function(...) tibble::tibble(Type = c("Train_Test_Splits", "Model_Hyperparameters"),
      Data = list(current$splits, data.frame(Hyperparameter_ID = 1L))),
    fit_models = function(trained_models_tbl, ...) {
      state$fits[[length(state$fits) + 1L]] <- trained_models_tbl$Model_ID
      current$fitted[match(trained_models_tbl$Model_ID, current$ids), ]
    },
    write_data = function(x, combo, run_info, output_type, folder = NULL, suffix = NULL) {
      if (identical(run_info$run_name, current$info$run_name) &&
        identical(combo, drop_source) && identical(suffix, drop_suffix)) return(invisible(NULL))
      writer(x, combo, run_info, output_type, folder, suffix)
    },
    .package = "finnts"
  )
  result <- update_forecast_combo(agent, best, NULL, 1, FALSE, 123)
  list(result = result, fits = state$fits, model_reads = state$model_reads, agent = agent)
}

# Finalize synthetic trained predictions through the iteration's real selection,
# average writer, and reconciliation. Sequential foreach replaces worker setup;
# no candidate policy, persistence, or solver is mocked. Returns final_models'
# selection result after discarding only the fixture's preassigned winner flags.
finalize_update_chain_iteration <- function(fixture) {
  for (combo in fixture$hierarchy$hts_combos) {
    rows <- fixture$source[fixture$source$Combo == combo & fixture$source$Recipe_ID != "simple_average", ]
    rows$Best_Model <- NULL
    rows$Run_Type <- NULL
    write_data(rows, combo, fixture$info, "data", "forecasts",
      if (fixture$log$run_global_models) "-global_models" else "-single_models")
    write_data(fixture$source[0, setdiff(names(fixture$source), "Run_Type")],
      combo, fixture$info, "data", "forecasts", "-average_models")
  }
  log <- fixture$log
  log$weighted_mape <- NA_real_
  write_data(log, NULL, fixture$info, "log", "logs")
  testthat::local_mocked_bindings(
    par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
    train_models = function(...) stop("iteration fixture must not train models"),
    .package = "finnts")
  final_models(fixture$info, average_models = length(fixture$ids) > 1L,
    max_model_average = as.numeric(length(fixture$ids)), weekly_to_daily = FALSE)
}

# Persist a parent Agent version with real pre-expanded input and hierarchy
# metadata. Its children forecast ID nodes bottoms-up; the parent reconciles the
# chosen local/global mixture exactly once. Returns settings and current levels.
make_update_node_parent <- function(path, version, approach, members) {
  topology <- make_update_chain_case(path, paste0("topology-", version), approach,
    members = members, models = "xgboost", recipes = "R1", legacy = TRUE)
  info <- topology$info
  info$project_name <- "chain"
  info$run_name <- paste0("version-", version)
  info$combo_variables <- "ID"
  info$date_type <- "month"
  info$weekly_to_daily <- FALSE
  info$fiscal_year_start <- 1
  agent <- list(project_info = info, run_id = info$run_name, agent_version = version,
    llm = NULL, overwrite = TRUE, forecast_approach = approach, forecast_horizon = 6,
    external_regressors = NULL, hist_start_date = NULL, hist_end_date = topology$log$hist_end_date,
    back_test_scenarios = 1, back_test_spacing = 1, combo_cleanup_date = NULL,
    run_global_models = TRUE, run_local_models = TRUE)
  write_data(topology$hierarchy, NULL, info, "object", "prep_data", "-hts_info")
  write_data(read_selection_file(topology$info, "prep_data", "-hts_data"),
    NULL, info, "data", "prep_data", "-hts_data")
  levels <- vapply(topology$contexts, function(context) context$history$Target[1], numeric(1))
  for (combo in names(levels)) {
    input <- topology$contexts[[combo]]$history
    input$Combo <- combo
    input$ID <- combo
    write_data(input, combo, info, "data", "input_data")
  }
  log <- data.frame(run_id = agent$run_id, agent_version = version, forecast_approach = approach,
    forecast_horizon = 6, external_regressors = NA_character_, hist_end_date = agent$hist_end_date,
    back_test_scenarios = 1, back_test_spacing = 1, combo_cleanup_date = as.Date(NA),
    run_global_models = TRUE, run_local_models = TRUE)
  write_data(log, NULL, info, "log", "logs", "-agent_run")
  list(agent = agent, topology = topology, levels = levels)
}

# Make a global node pool or a single local node from the current parent's
# actual aggregate levels. Global pools use two components with an exact-mean
# winner; local/default children use one fit. No provider or engine is invoked.
make_update_node_child <- function(parent, combos, type, name, unfinished = FALSE) {
  make_update_chain_case(parent$agent$project_info$path, name, "bottoms_up",
    members = data.frame(ID = combos), models = if (type == "global") c("xgboost", "chronos2") else "arima",
    recipes = if (type == "global") c("R1", "R1") else "R1", legacy = unfinished,
    uniform = TRUE, model_type = type, levels = parent$levels)
}

# Publish an already fitted iteration fixture using real quality assessment,
# metric calculation and per-series best-run records. Returns its enriched run
# info so the default-submission boundary can reuse the same persisted outputs.
publish_update_node_child <- function(parent, child, publish = TRUE) {
  info <- child$info
  info$forecast_selection <- assess_agent_run(info, read_selection_file(info, "logs"),
    child$hierarchy$original_combos, check_quality = TRUE)
  info$selection_combos <- child$hierarchy$original_combos
  if (publish) {
    log_best_run(parent$agent, info, calculate_fcst_metrics(info, get_fcst_output(info)),
      combo = if (child$log$run_global_models) NULL else hash_data(child$hierarchy$original_combos),
      check_best_run = FALSE)
  }
  info
}

# Finish the real parent persistence/reconciliation boundary and verify exact
# current source/bottom membership, coherent pool identity, and arithmetic.
# Minimal synthetic summary/EDA tables provide the ordinary completion envelope.
finish_update_node_parent <- function(parent) {
  agent <- parent$agent
  info <- agent$project_info
  save_best_agent_run(agent)
  metadata <- get_best_agent_run(agent)
  testthat::expect_setequal(metadata$combo, names(parent$levels))
  validate_global_iteration(metadata)
  sources <- load_agent_forecast(agent)
  testthat::expect_setequal(sources$Combo, names(parent$levels))
  reconcile_agent_forecast(agent, info)
  forecasts <- load_agent_forecast(agent, final_output = TRUE)
  testthat::expect_setequal(forecasts$Combo, parent$topology$hierarchy$original_combos)
  testthat::expect_true(all(is.finite(forecasts$Forecast)))
  testthat::expect_equal(forecasts$Forecast, rep(100, nrow(forecasts)), tolerance = 1e-6)
  write_data(forecasts, NULL, info, "data", "final_output", "-forecast")
  write_data(unique(sources[, c("Combo", "Model_ID", "Best_Model")]),
    NULL, info, "data", "final_output", "-model_summary")
  write_data(data.frame(Metric = "Number_Series", Value = length(parent$levels)),
    NULL, info, "data", "final_output", "-eda")
  summarize_hierarchy(agent)
  testthat::expect_identical(initial_checks(agent), "no updates required")
  metadata
}

# Exercise persisted node-level iterate -> update -> update -> reintroduction,
# including local-only, global-only and mixed winning runs. Real initial_checks,
# update workers, default dispatch, best-run I/O and outer reconciliation are
# used. Only costly default submission is replaced with a finalized local fit.
# Assertions cover new/removed aggregates as well as bottoms and next-run routes.
check_update_node_chain <- function(approach, mode) {
  path <- withr::local_tempdir()
  base <- data.frame(Region = c("North", "North", "South", "South"), Product = c("A", "B", "A", "B"))
  changed <- rbind(base[1:3, ], data.frame(Region = "East", Product = "C"))
  previous <- make_update_node_parent(path, 1, approach, base)
  nodes <- names(previous$levels)
  global <- switch(mode, global = nodes, local = character(), mixed = nodes[seq_along(nodes) %% 2L == 0L])
  children <- list()
  if (length(global)) {
    child <- make_update_node_child(previous, global, "global", "initial-global")
    publish_update_node_child(previous, child)
    children[[child$info$run_name]] <- child
  }
  for (combo in setdiff(nodes, global)) {
    child <- make_update_node_child(previous, combo, "local", paste0("initial-", hash_data(combo)))
    publish_update_node_child(previous, child)
    children[[child$info$run_name]] <- child
  }
  previous_metadata <- finish_update_node_parent(previous)
  for (version in 2:4) {
    current <- make_update_node_parent(path, version, approach, if (version == 4) base else changed)
    routing <- initial_checks(current$agent)
    reassigned <- if (approach == "standard_hierarchy" && version %in% c(2, 4)) c("A", "B") else character()
    expected_existing <- setdiff(intersect(names(previous$levels), names(current$levels)), reassigned)
    expected_new <- union(setdiff(names(current$levels), names(previous$levels)), reassigned)
    testthat::expect_setequal(routing$prev_best_runs_tbl$combo, expected_existing)
    testthat::expect_setequal(routing$new_combos, vapply(expected_new, hash_data, character(1)))
    old_global <- previous_metadata$combo[previous_metadata$model_type == "global"]
    current_global <- intersect(old_global, expected_existing)
    groups <- split(routing$prev_best_runs_tbl, routing$prev_best_runs_tbl$best_run_name)
    for (group in groups) {
      type <- unique(group$model_type)
      testthat::expect_length(type, 1)
      child <- make_update_node_child(current,
        if (type == "global") names(current$levels) else group$combo, type,
        paste0("updated-", version, "-", hash_data(group$best_run_name[1])), unfinished = TRUE)
      step <- run_update_chain_step(children[[group$best_run_name[1]]], child,
        agent = current$agent, combos = group$combo)
      testthat::expect_identical(step$result$status, "done")
      testthat::expect_length(step$result$quality_rejected_combos, 0)
      children[[child$info$run_name]] <- child
    }
    submitted <- character()
    testthat::with_mocked_bindings(
      forecast_new_combos(current$agent, routing$new_combos, character(), NULL, FALSE, 1, 123),
      par_start = function(...) list(cl = NULL, packages = character(), foreach_operator = foreach::`%do%`),
      get_foundation_model_suffix = function() "",
      submit_fcst_run = function(agent_info, inputs, combo, timestamp, ...) {
        testthat::expect_true(agent_info$default_reforecast)
        testthat::expect_identical(inputs$forecast_approach, "bottoms_up")
        node <- names(current$levels)[match(combo, vapply(names(current$levels), hash_data, character(1)))]
        submitted <<- c(submitted, node)
        child <- make_update_node_child(current, node, "local", paste0("default-", version, "-", combo))
        finalize_update_chain_iteration(child)
        children[[child$info$run_name]] <<- child
        publish_update_node_child(current, child, publish = FALSE)
      }, .package = "finnts")
    testthat::expect_setequal(submitted, expected_new)
    metadata <- finish_update_node_parent(current)
    testthat::expect_setequal(metadata$combo[metadata$model_type == "global"], current_global)
    testthat::expect_true(all(metadata$model_type[metadata$combo %in% expected_new] == "local"))
    previous <- current
    previous_metadata <- metadata
  }
}
