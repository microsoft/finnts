locate_single_models_file <- function(run_info) {
  forecasts_dir <- file.path(run_info$path, "forecasts")
  run_hash <- hash_data(run_info$run_name)
  pattern <- paste0(
    "^",
    hash_data(run_info$project_name), "-",
    run_hash, "-.*-single_models\\.(csv|parquet)$"
  )
  files <- list.files(forecasts_dir, pattern = pattern, full.names = TRUE)
  if (length(files) != 1) {
    stop(
      "expected exactly 1 single_models file for run ", run_info$run_name,
      " but found ", length(files)
    )
  }
  files
}

read_fcst_file <- function(path) {
  ext <- tools::file_ext(path)
  if (ext == "parquet") {
    arrow::read_parquet(path)
  } else {
    suppressMessages(vroom::vroom(path, delim = ",", show_col_types = FALSE, altrep = FALSE))
  }
}

write_fcst_file <- function(data, path) {
  ext <- tools::file_ext(path)
  if (ext == "parquet") {
    arrow::write_parquet(data, path)
  } else {
    vroom::vroom_write(data, path, delim = ",")
  }
}

make_best_models_fixture <- function() {
  run_path <- withr::local_tempdir(
    pattern = "finnts-best-models-",
    .local_envir = parent.frame()
  )
  run_info <- set_run_info(
    project_name = "best_models_test",
    run_name = "partial_fold_coverage",
    path = run_path,
    data_output = "csv",
    add_unique_id = FALSE
  )
  dates <- seq(as.Date("2021-01-01"), by = "month", length.out = 39)
  hist_end_date <- dates[36]
  log_path <- paste0(
    "logs/", hash_data(run_info$project_name), "-",
    hash_data(run_info$run_name), ".csv"
  )
  log_data <- finnts:::read_file(run_info, path = log_path, return_type = "df") %>%
    dplyr::mutate(
      date_type = "month",
      combo_variables = "Series",
      forecast_approach = "bottoms_up",
      forecast_horizon = 3,
      hist_start_date = dates[1],
      hist_end_date = hist_end_date,
      recipes_to_run = "R1",
      clean_outliers = FALSE,
      clean_missing_values = TRUE,
      stationary = FALSE,
      box_cox = FALSE,
      negative_forecast = FALSE,
      run_global_models = FALSE,
      run_local_models = TRUE,
      run_ensemble_models = FALSE
    )
  finnts:::write_data(
    log_data,
    combo = NULL,
    run_info = run_info,
    output_type = "log",
    folder = "logs",
    suffix = NULL
  )

  splits <- tibble::tibble(
    Run_Type = c("Future_Forecast", rep("Back_Test", 3)),
    Train_Test_ID = 1:4,
    Train_End = dates[c(36, 33, 30, 27)],
    Test_End = dates[c(39, 36, 33, 30)]
  )
  finnts:::write_data(
    splits,
    combo = NULL,
    run_info = run_info,
    output_type = "data",
    folder = "prep_models",
    suffix = "-train_test_split"
  )
  history <- tibble::tibble(
    Combo = "Synthetic",
    Date = dates,
    Target = ifelse(dates <= hist_end_date, 100 + as.integer(format(dates, "%m")), NA_real_)
  )
  finnts:::write_data(
    history,
    combo = "Synthetic",
    run_info = run_info,
    output_type = "data",
    folder = "prep_data",
    suffix = "-R1"
  )
  forecasts <- tidyr::expand_grid(
    Model_Name = c("meanf", "snaive"),
    Train_Test_ID = 1:4,
    Horizon = 1:3
  ) %>%
    dplyr::mutate(
      Combo_ID = "Synthetic",
      Combo = "Synthetic",
      Model_Type = "local",
      Recipe_ID = "R1",
      Model_ID = paste(Model_Name, Model_Type, Recipe_ID, sep = "--"),
      Hyperparameter_ID = 1,
      Date = dates[c(36, 33, 30, 27)[Train_Test_ID] + Horizon],
      Target = ifelse(Train_Test_ID == 1, NA_real_, 100 + as.integer(format(Date, "%m"))),
      Forecast = dplyr::case_when(
        Train_Test_ID == 1 ~ 100 + as.integer(format(Date, "%m")),
        Model_Name == "meanf" ~ Target * 1.10,
        Train_Test_ID == 2 ~ Target * 1.01,
        Train_Test_ID == 3 ~ Target * 1.20,
        TRUE ~ Target * 1.30
      )
    ) %>%
    dplyr::select(
      Combo_ID, Model_ID, Model_Name, Model_Type, Recipe_ID,
      Train_Test_ID, Hyperparameter_ID, Combo, Horizon, Date, Target, Forecast
    )
  finnts:::write_data(
    forecasts,
    combo = "Synthetic",
    run_info = run_info,
    output_type = "data",
    folder = "forecasts",
    suffix = "-single_models"
  )
  run_info
}

selection_benchmark_catalogue <- function() {
  dplyr::bind_rows(
    data.frame(Model_Name = list_models(), Model_Type = "local", Recipe_ID = "R1"),
    data.frame(Model_Name = list_r2_models(), Model_Type = "local", Recipe_ID = "R2"),
    data.frame(Model_Name = list_global_models(), Model_Type = "global", Recipe_ID = "R1"),
    data.frame(Model_Name = intersect(list_global_models(), list_r2_models()), Model_Type = "global", Recipe_ID = "R2"),
    data.frame(Model_Name = list_ensemble_models(), Model_Type = "local", Recipe_ID = "ensemble")
  ) %>% dplyr::mutate(Model_ID = paste(Model_Name, Model_Type, Recipe_ID, sep = "--"))
}

make_catalogue_selection_fixture <- function(trial, catalogue = selection_benchmark_catalogue(), mixed = FALSE,
                                             distinct_forecasts = FALSE) {
  path <- withr::local_tempdir(pattern = "finnts-selection-catalogue-", .local_envir = parent.frame())
  info <- set_run_info(project_name = "selection-catalogue", run_name = paste0("trial-", trial),
    path = path, data_output = "csv", add_unique_id = FALSE)
  info$combo <- hash_data("Synthetic")
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 60)
  actuals <- 100 + 10 * cos(2 * pi * (seq_along(dates) - 0.5) / 12)
  splits <- data.frame(Train_Test_ID = 1:4, Run_Type = c("Future_Forecast", rep("Back_Test", 3)),
    Train_End = dates[c(48, 36, 30, 24)], Test_End = dates[c(60, 48, 42, 36)])
  log <- read_selection_file(info, "logs")
  settings <- list(date_type = "month", combo_variables = "Series", forecast_approach = "bottoms_up",
    forecast_horizon = 12L, hist_start_date = dates[1], hist_end_date = dates[48], recipes_to_run = "R1---R2",
    clean_outliers = FALSE, clean_missing_values = TRUE, stationary = FALSE, box_cox = FALSE,
    negative_forecast = FALSE, run_global_models = any(catalogue$Model_Type == "global"),
    run_local_models = any(catalogue$Model_Type == "local" & catalogue$Recipe_ID != "ensemble"),
    run_ensemble_models = any(catalogue$Recipe_ID == "ensemble"))
  for (setting in names(settings)) log[[setting]] <- settings[[setting]]
  write_data(log, combo = NULL, run_info = info, output_type = "log", folder = "logs", suffix = NULL)
  write_data(splits, combo = NULL, run_info = info, output_type = "data", folder = "prep_models", suffix = "-train_test_split")
  history <- data.frame(Combo = "Synthetic", Date = dates, Target = c(actuals[1:48], rep(NA_real_, 12)))
  write_data(history, combo = "Synthetic", run_info = info, output_type = "data", folder = "prep_data", suffix = "-R1")
  r2 <- history[rep(seq_len(nrow(history)), 12), ]
  r2$Horizon <- rep(1:12, each = nrow(history))
  r2$Origin <- rep(seq_len(nrow(history)), 12) - r2$Horizon
  write_data(r2, combo = "Synthetic", run_info = info, output_type = "data", folder = "prep_data", suffix = "-R2")
  rows <- dplyr::bind_rows(lapply(seq_len(nrow(catalogue)), function(model_index) {
    model <- catalogue[model_index, ]
    positions <- unlist(lapply(c(48L, 36L, 30L, 24L), function(origin) origin + seq_len(12)))
    future <- rep(c(TRUE, FALSE, FALSE, FALSE), each = 12)
    forecast <- actuals[positions] * (1 + 0.06 + model_index * 0.0002)
    forecast[future] <- actuals[positions[future]]
    if (distinct_forecasts) forecast <- forecast + model_index * rep(seq_len(12), 4) / 1000
    if (mixed && model_index == 1L) forecast[future] <- forecast[future] * 100000
    if (mixed && model_index == 2L) forecast[future] <- Inf
    predictions <- data.frame(Combo = "Synthetic", Combo_ID = "Synthetic",
      Model_ID = model$Model_ID, Model_Name = model$Model_Name, Model_Type = model$Model_Type,
      Recipe_ID = model$Recipe_ID, Hyperparameter_ID = 1L, Train_Test_ID = rep(1:4, each = 12),
      Horizon = rep(1:12, 4), Date = dates[positions],
      Target = ifelse(future, NA_real_, actuals[positions]), Forecast = forecast)
    if (mixed && model_index == 3L) predictions <- predictions[predictions$Train_Test_ID != 4L, ]
    predictions
  }))
  suffixes <- ifelse(rows$Recipe_ID == "ensemble", "-ensemble_models",
    ifelse(rows$Model_Type == "global", "-global_models", "-single_models"))
  for (suffix in unique(suffixes)) {
    write_data(rows[suffixes == suffix, ], combo = "Synthetic", run_info = info,
      output_type = "data", folder = "forecasts", suffix = suffix)
  }
  list(run_info = info, forecasts = rows, catalogue = catalogue, splits = splits)
}

make_hierarchical_selection_artifacts <- function(approach = "standard_hierarchy", date_type = "month",
                                                 shape = "flat", horizon = NULL, backtest_scenarios = 3L) {
  fixture <- make_pre_reconciliation_case("magnitude", "all", approach,
    date_type = date_type, shape = shape, horizon = horizon)
  fixture$splits <- utils::head(fixture$splits, backtest_scenarios + 1L)
  fixture$forecasts <- fixture$forecasts[fixture$forecasts$Train_Test_ID %in% fixture$splits$Train_Test_ID, ]
  path <- withr::local_tempdir(pattern = "finnts-selection-hierarchy-", .local_envir = parent.frame())
  info <- set_run_info(project_name = "selection-hierarchy", run_name = approach,
    path = path, data_output = "csv", add_unique_id = FALSE)
  log <- read_selection_file(info, "logs")
  context <- fixture$contexts[[1]]
  settings <- list(date_type = date_type, combo_variables = "Series", forecast_approach = approach,
    forecast_horizon = fixture$horizon, hist_start_date = min(context$history$Date),
    hist_end_date = context$hist_end_date, recipes_to_run = "R1", clean_outliers = FALSE,
    clean_missing_values = TRUE, stationary = FALSE, box_cox = FALSE, negative_forecast = FALSE,
    run_global_models = FALSE, run_local_models = TRUE, run_ensemble_models = FALSE)
  for (setting in names(settings)) log[[setting]] <- settings[[setting]]
  write_data(log, combo = NULL, run_info = info, output_type = "log", folder = "logs", suffix = NULL)
  write_data(fixture$splits, combo = NULL, run_info = info, output_type = "data", folder = "prep_models", suffix = "-train_test_split")
  write_data(fixture$metadata, combo = NULL, run_info = info, output_type = "object", folder = "prep_data", suffix = "-hts_info")
  write_data(fixture$history, combo = NULL, run_info = info, output_type = "data", folder = "prep_data", suffix = "-hts_data")
  for (combo in fixture$metadata$hts_combos) {
    context <- fixture$contexts[[combo]]
    history <- data.frame(Combo = combo, Date = context$calendar,
      Target = c(context$history$Target, rep(NA_real_, fixture$horizon)))
    write_data(history, combo = combo, run_info = info, output_type = "data", folder = "prep_data", suffix = "-R1")
    predictions <- fixture$forecasts[fixture$forecasts$Combo == combo, ]
    predictions$Combo_ID <- combo
    predictions$Hyperparameter_ID <- 1L
    predictions <- predictions %>% dplyr::group_by(Model_ID, Train_Test_ID) %>%
      dplyr::mutate(Horizon = dplyr::row_number()) %>% dplyr::ungroup()
    write_data(predictions, combo = combo, run_info = info, output_type = "data", folder = "forecasts", suffix = "-single_models")
  }
  fixture$run_info <- info
  fixture
}

make_selection_history_recipe <- function(actuals, cleaned = actuals,
                                          difference_order = 0, box_cox = FALSE,
                                          recipe = "R1", horizon = 3) {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = length(actuals) + horizon)
  data <- tibble::tibble(
    Combo = "Synthetic", Date = dates,
    Target = c(cleaned, rep(NA_real_, horizon)),
    Target_Original = c(actuals, rep(NA_real_, horizon))
  )
  lambda <- NA_real_
  if (box_cox) {
    transformed <- apply_box_cox(data)
    data <- transformed$data
    lambda <- transformed$diff_info$Box_Cox_Lambda
  }
  testthat::local_mocked_bindings(
    unitroot_ndiffs = function(...) difference_order,
    .package = "feasts"
  )
  transformed <- make_stationary(data)
  data <- transformed$data
  metadata <- transformed$diff_info
  metadata$Box_Cox_Lambda <- lambda
  if (identical(recipe, "R2")) {
    data <- dplyr::bind_rows(lapply(seq_len(horizon), function(period) {
      dplyr::mutate(data, Horizon = period, Origin = seq_len(nrow(data)) - period)
    }))
  }
  list(
    data = data, combo_info = metadata,
    hist_end_date = dates[length(actuals)],
    expected = data.frame(Date = dates[seq_along(actuals)], Target = actuals)
  )
}

make_selection_case <- function(actuals = rep(100, 36),
                                futures = list(accurate = rep(200, 6), safe = rep(100, 6)),
                                errors = c(accurate = 0.08, safe = 0.083),
                                date_type = "month", period = NULL) {
  forecast_horizon <- length(futures[[1]])
  cadence <- switch(date_type, day = "day", week = "week", month = "month", quarter = "3 months", year = "year")
  dates <- seq(as.Date("2020-01-01"), by = cadence, length.out = length(actuals) + forecast_horizon)
  history <- data.frame(Date = dates[seq_along(actuals)], Target = actuals)
  backtest_size <- min(forecast_horizon, length(actuals) - 1L)
  backtest_dates <- utils::tail(history$Date, backtest_size)
  future_dates <- utils::tail(dates, forecast_horizon)
  backtests <- dplyr::bind_rows(lapply(names(futures), function(model_id) {
    target <- utils::tail(actuals, backtest_size)
    data.frame(Model_ID = model_id, Train_Test_ID = 2L, Date = backtest_dates,
               Target = target, Forecast = target * (1 + errors[[model_id]]))
  }))
  forecasts <- dplyr::bind_rows(lapply(names(futures), function(model_id) {
    data.frame(Model_ID = model_id, Train_Test_ID = 1L, Date = future_dates,
               Target = NA_real_, Forecast = futures[[model_id]])
  }))
  context <- list(
    calendar = dates, date_type = date_type, seasonal_period = period,
    hist_end_date = max(history$Date),
    train_test_split = data.frame(
      Train_Test_ID = c(1L, 2L), Run_Type = c("Future_Forecast", "Back_Test"),
      Train_End = c(max(history$Date), dates[length(actuals) - backtest_size]),
      Test_End = c(max(dates), max(history$Date))
    )
  )
  list(history = history, backtests = backtests, forecasts = forecasts, context = context)
}

make_fitted_selection_models <- function(forecasts, model_names = c("xgboost", "chronos2")) {
  dplyr::bind_rows(lapply(model_names, function(model) {
    predictions <- forecasts[, c("Combo", "Train_Test_ID", "Date", "Target", "Forecast", "Run_Type")]
    predictions$Hyperparameter_ID <- 1L
    tibble::tibble(Combo_ID = "All-Data", Model_Name = model, Model_Type = "global",
      Recipe_ID = "R1", Forecast_Tbl = list(predictions), Model_Fit = list(NULL))
  }))
}

make_pre_reconciliation_case <- function(pathology = "magnitude", placement = "all",
                                         approach = "standard_hierarchy", series_count = 4L,
                                         date_type = "month", shape = "seasonal",
                                         history_cycles = 4L, horizon = NULL, recipe = "R1") {
  period <- forecast_seasonal_period(list(date_type = date_type))
  history_size <- max(12L, history_cycles * period)
  if (is.null(horizon)) horizon <- max(4L, period)
  cadence <- switch(date_type, day = "day", week = "week", month = "month", quarter = "3 months", year = "year")
  dates <- seq(as.Date("2020-01-01"), by = cadence, length.out = history_size + horizon)
  positions <- seq_along(dates)
  levels <- 100 * (1 + (seq_len(series_count) %% 17L) / 20)
  profile <- switch(shape,
    flat = rep(1, length(dates)),
    trend = 1 + 0.001 * positions,
    noisy = 1 + 0.02 * sin(positions * 1.31) + 0.01 * cos(positions * 2.1),
    intermittent = ifelse(positions %% 5L == 0L, 1, 0),
    zero = rep(0, length(dates)),
    1 + 0.1 * cos(2 * pi * (positions - 0.5) / period))
  if (shape == "signed") levels <- levels * rep(c(-1, 1), length.out = series_count)
  original_combos <- sprintf("series-%06d", seq_len(series_count))
  truth <- outer(profile, levels)
  colnames(truth) <- original_combos
  group_width <- max(2L, ceiling(sqrt(series_count)))
  first_group <- (seq_len(series_count) - 1L) %/% group_width + 1L
  second_group <- (seq_len(series_count) - 1L) %% group_width + 1L
  hierarchy <- suppressMessages(if (approach == "standard_hierarchy") {
    hts::hts(stats::ts(truth), nodes = list(max(first_group), as.integer(tabulate(first_group))))
  } else {
    hts::gts(stats::ts(truth), groups = rbind(first_group, second_group))
  })
  all_truth <- hts::allts(hierarchy)
  source_combos <- sprintf("node-%06d", seq_len(ncol(all_truth)))
  aggregate_count <- ncol(all_truth) - series_count
  affected <- switch(placement, root = 1L, aggregate = 2L, leaf = aggregate_count + 1L,
    siblings = aggregate_count + seq_len(min(2L, series_count)), all = seq_len(ncol(all_truth)))
  backtest_size <- min(horizon, floor(history_size / 2))
  origins <- history_size - backtest_size - (0:2) * max(1L, floor(backtest_size / 2))
  origins <- origins[origins >= 1L]
  split_origins <- c(history_size, origins)
  split_lengths <- c(horizon, rep(backtest_size, length(origins)))
  keys <- dplyr::bind_rows(lapply(seq_along(split_origins), function(split_index) {
    data.frame(Train_Test_ID = split_index, Position = split_origins[split_index] + seq_len(split_lengths[split_index]))
  }))
  splits <- data.frame(Train_Test_ID = seq_along(split_origins),
    Run_Type = c("Future_Forecast", rep("Back_Test", length(origins))),
    Train_End = dates[split_origins], Test_End = dates[split_origins + split_lengths])
  contexts <- stats::setNames(lapply(seq_along(source_combos), function(source_index) {
    list(history = data.frame(Date = dates[seq_len(history_size)], Target = all_truth[seq_len(history_size), source_index]),
      calendar = dates, hist_end_date = dates[history_size], date_type = date_type, seasonal_period = period)
  }), source_combos)
  models <- if (recipe == "R2") c("glmnet", "svm-rbf", "xgboost") else c("arima", "ets", "snaive")
  forecasts <- dplyr::bind_rows(lapply(seq_along(source_combos), function(source_index) {
    actuals <- all_truth[seq_len(history_size), source_index]
    future <- utils::tail(all_truth[, source_index], horizon)
    level <- mean(utils::tail(actuals, min(period, length(actuals))))
    magnitude <- max(abs(level), 1)
    damaged <- switch(pathology,
      magnitude = future * 100000,
      trend = future + magnitude * 0.15 * seq_len(horizon),
      level = future + 2 * magnitude,
      phase = 2 * level - future,
      amplitude = level + 6 * (future - level),
      flatten = rep(level, horizon), future)
    bad_index <- 1L + source_index %% 2L
    dplyr::bind_rows(lapply(seq_along(models), function(model_index) {
      error <- if (model_index == bad_index) 0.08 else if (model_index == 3L) 0.085 else 0.083
      target <- all_truth[keys$Position, source_index]
      forecast <- target * (1 + error)
      forecast[keys$Train_Test_ID == 1L] <- if (source_index %in% affected && model_index == bad_index) damaged else future
      data.frame(Combo = source_combos[source_index], Model_ID = paste(models[model_index], "local", recipe, sep = "--"),
        Model_Name = models[model_index], Model_Type = "local", Recipe_ID = recipe,
        Train_Test_ID = keys$Train_Test_ID, Date = dates[keys$Position],
        Target = ifelse(keys$Train_Test_ID == 1L, NA_real_, target), Forecast = forecast)
    }))
  }))
  history <- data.frame(Combo = rep(original_combos, each = history_size),
    Date = rep(dates[seq_len(history_size)], series_count),
    Target = as.vector(truth[seq_len(history_size), , drop = FALSE]))
  held_out <- data.frame(Combo = rep(original_combos, each = horizon),
    Date = rep(utils::tail(dates, horizon), series_count), Truth = as.vector(utils::tail(truth, horizon)))
  affected_bottoms <- switch(placement, root = original_combos, all = original_combos,
    aggregate = original_combos[first_group == 1L], leaf = original_combos[1],
    siblings = utils::head(original_combos, 2))
  list(forecasts = forecasts, contexts = contexts, history = history, held_out = held_out, splits = splits,
    metadata = list(original_combos = original_combos, hts_combos = source_combos,
      nodes = if (approach == "standard_hierarchy") hts::get_nodes(hierarchy) else hts::get_groups(hierarchy)),
    run_info = list(project_name = "paired-selection", run_name = "synthetic", data_output = "csv", object_output = "rds"),
    approach = approach, negative_forecast = shape == "signed", pathology = pathology, placement = placement,
    horizon = horizon, history_size = history_size, affected_bottoms = affected_bottoms)
}

select_pre_reconciliation_inputs <- function(fixture, quality = TRUE, max_average = 3L) {
  results <- lapply(names(fixture$contexts), function(combo) {
    rows <- fixture$forecasts[fixture$forecasts$Combo == combo, , drop = FALSE]
    context <- fixture$contexts[[combo]]
    selection <- NULL
    if (quality) {
      selection <- select_series_forecasts(rows, context, fixture$splits)
    } else {
      model_ids <- unique(as.character(rows$Model_ID))
      if (!length(model_ids) || anyNA(model_ids) || any(!nzchar(model_ids))) {
        stop("Candidate identities must be nonmissing.", call. = FALSE)
      }
      history <- context$history
      history$Date <- as.Date(history$Date)
      history <- history[history$Date <= as.Date(context$hist_end_date), , drop = FALSE]
      if (anyNA(history$Date) || anyDuplicated(history$Date) || !any(is.finite(history$Target))) {
        stop("Forecast selection requires unique dated historical actuals.", call. = FALSE)
      }
    }
    eligible <- if (quality) selection$rankings$Model_ID[selection$rankings$Eligible] else unique(rows$Model_ID)
    averages <- list()
    if (length(eligible) >= 2L && max_average >= 2L) {
      for (size in seq.int(2L, min(max_average, length(eligible)))) {
        combinations <- utils::combn(sort(eligible), size, simplify = FALSE)
        averages <- c(averages, lapply(combinations, function(components) {
          rows[rows$Model_ID %in% components, ] %>%
            dplyr::group_by(Combo, Train_Test_ID, Date) %>%
            dplyr::summarise(Target = mean(Target, na.rm = TRUE), Forecast = mean(Forecast), .groups = "drop") %>%
            dplyr::mutate(Model_ID = paste(components, collapse = "_"), Model_Name = NA_character_,
              Model_Type = "local", Recipe_ID = "simple_average")
        }))
      }
    }
    candidates <- dplyr::bind_rows(c(list(rows), averages))
    if (quality) {
      selection <- select_series_forecasts(candidates, context, fixture$splits)
      chosen <- selection$selected_id
    } else {
      ids <- sort(unique(candidates$Model_ID))
      scores <- vapply(ids, function(model_id) {
        backtest <- candidates[candidates$Model_ID == model_id & candidates$Train_Test_ID != 1L, ]
        forecast_backtest_accuracy(context$history, backtest)$WMAPE
      }, numeric(1))
      chosen <- ids[order(scores, ids, na.last = TRUE)][1]
    }
    if (is.na(chosen)) abort_forecast_selection(combo, selection)
    selected <- candidates[candidates$Model_ID == chosen, , drop = FALSE]
    selected$Best_Model <- "Yes"
    selected$Run_Type <- fixture$splits$Run_Type[match(selected$Train_Test_ID, fixture$splits$Train_Test_ID)]
    list(forecasts = selected, selection = selection)
  })
  list(forecasts = dplyr::bind_rows(lapply(results, `[[`, "forecasts")),
    selections = stats::setNames(lapply(results, `[[`, "selection"), names(fixture$contexts)))
}

reconcile_pre_selection_case <- function(fixture, selected) {
  testthat::local_mocked_bindings(read_file = function(run_info, path, return_type = "df", ...) {
    if (return_type == "object") fixture$metadata else fixture$history
  }, .package = "finnts")
  reconcile(selected$forecasts, fixture$run_info, fixture$approach, fixture$negative_forecast)
}

score_pre_selection_case <- function(fixture, forecast) {
  future <- forecast[forecast$Train_Test_ID == 1L, ]
  measured <- dplyr::inner_join(future[, c("Combo", "Date", "Forecast")], fixture$held_out, by = c("Combo", "Date"))
  stopifnot(nrow(measured) == nrow(fixture$held_out), all(is.finite(measured$Forecast)))
  errors <- abs(measured$Forecast - measured$Truth)
  scales <- pmax(abs(measured$Truth), 1)
  shapes <- lapply(split(measured, measured$Combo), function(series) {
    series <- series[order(series$Date), ]
    scale <- max(abs(series$Truth), 1)
    amplitude <- diff(range(series$Truth))
    c(endpoint = abs(utils::tail(series$Forecast - series$Truth, 1)) / scale,
      slope = abs(stats::median(diff(series$Forecast)) - stats::median(diff(series$Truth))) / scale,
      amplitude = if (amplitude > 1e-10) abs(diff(range(series$Forecast)) / amplitude - 1) else NA_real_,
      phase = if (stats::sd(series$Truth) > 1e-10 && stats::sd(series$Forecast) > 1e-10) {
        stats::cor(series$Truth, series$Forecast)
      } else NA_real_)
  })
  shape_matrix <- do.call(rbind, shapes)
  finite_max <- function(values) if (any(is.finite(values))) max(values[is.finite(values)]) else NA_real_
  untouched <- !measured$Combo %in% fixture$affected_bottoms
  c(relative_error = sum(errors) / max(sum(abs(measured$Truth)), 1),
    maximum_point_error = max(errors / scales),
    total_error = sum(abs(tapply(measured$Forecast - measured$Truth, measured$Date, sum))) /
      max(sum(abs(measured$Truth)), 1), endpoint_error = finite_max(shape_matrix[, "endpoint"]),
    slope_error = finite_max(shape_matrix[, "slope"]), amplitude_error = finite_max(shape_matrix[, "amplitude"]),
    phase_correlation = -finite_max(-shape_matrix[, "phase"]),
    sibling_error = if (any(untouched)) sum(errors[untouched]) / max(sum(abs(measured$Truth[untouched])), 1) else 0)
}

measure_pre_selection_case <- function(fixture) {
  baseline_seconds <- system.time(baseline <- select_pre_reconciliation_inputs(fixture, quality = FALSE))[["elapsed"]]
  selected_seconds <- system.time(selected <- select_pre_reconciliation_inputs(fixture))[["elapsed"]]
  baseline_hts_seconds <- system.time(before_rows <- reconcile_pre_selection_case(fixture, baseline))[["elapsed"]]
  selected_hts_seconds <- system.time(after_rows <- reconcile_pre_selection_case(fixture, selected))[["elapsed"]]
  before <- score_pre_selection_case(fixture, before_rows)
  after <- score_pre_selection_case(fixture, after_rows)
  accuracy_ok <- all(vapply(selected$selections, function(selection) {
    eligible <- selection$rankings[selection$rankings$Eligible, ]
    best <- min(eligible$WMAPE)
    chosen <- eligible[eligible$Model_ID == selection$selected_id, ]
    nrow(chosen) == 1L && chosen$WMAPE <= best + max(0.005, 0.05 * best) + 1e-12
  }, logical(1)))
  improved <- after[["relative_error"]] < before[["relative_error"]] - 1e-8
  meets_expectation <- if (fixture$placement == "all" && fixture$pathology != "clean") {
    before[["relative_error"]] > 0.01 && after[["relative_error"]] < before[["relative_error"]] / 2 - 1e-10
  } else NA
  data.frame(Approach = fixture$approach, Placement = fixture$placement, Pathology = fixture$pathology,
    Series = length(fixture$metadata$original_combos), Nodes = length(fixture$metadata$hts_combos),
    Before_Error = before[["relative_error"]], After_Error = after[["relative_error"]],
    Before_Maximum = before[["maximum_point_error"]], After_Maximum = after[["maximum_point_error"]],
    Before_Endpoint = before[["endpoint_error"]], After_Endpoint = after[["endpoint_error"]],
    Before_Slope = before[["slope_error"]], After_Slope = after[["slope_error"]],
    Before_Amplitude = before[["amplitude_error"]], After_Amplitude = after[["amplitude_error"]],
    Before_Phase = before[["phase_correlation"]], After_Phase = after[["phase_correlation"]],
    Before_Sibling = before[["sibling_error"]], After_Sibling = after[["sibling_error"]],
    Preselection_Accuracy_OK = accuracy_ok, Meets_Expectation = meets_expectation,
    Outcome = if (after[["relative_error"]] < 1e-6 && before[["relative_error"]] > 0.01) "fixed" else
      if (improved) "improved" else if (before[["relative_error"]] < 0.01) "attenuated" else "remaining distortion",
    Baseline_Selection_Seconds = baseline_seconds, Quality_Selection_Seconds = selected_seconds,
    Baseline_Hts_Seconds = baseline_hts_seconds, Selected_Hts_Seconds = selected_hts_seconds)
}

make_reconciled_selection_fixture <- function(approach = "standard_hierarchy", date_type = "month") {
  values <- c(100, 120, 80, 110)
  original_combos <- paste0("series-", seq_along(values))
  bottom <- matrix(rep(values, each = 36), nrow = 36, dimnames = list(NULL, original_combos))
  hierarchy <- if (approach == "standard_hierarchy") {
    hts::hts(stats::ts(bottom), nodes = list(2L, c(2L, 2L)))
  } else {
    hts::gts(stats::ts(bottom), groups = rbind(c(1L, 1L, 2L, 2L), c(1L, 2L, 1L, 2L)))
  }
  all_values <- hts::allts(hierarchy)
  stored_combos <- paste0("stored-", seq_len(ncol(all_values)))
  metadata <- list(original_combos = original_combos, hts_combos = stored_combos,
    nodes = if (approach == "standard_hierarchy") hts::get_nodes(hierarchy) else hts::get_groups(hierarchy))
  contexts <- lapply(values, function(value) {
    fixture <- make_selection_case(actuals = rep(value, 36),
      futures = list(accurate = rep(3 * value, 6), safe = rep(value, 6)), date_type = date_type)
    fixture$context$history <- fixture$history
    fixture$context
  })
  predictions <- dplyr::bind_rows(lapply(seq_along(stored_combos), function(series_index) {
    value <- all_values[36, series_index]
    fixture <- make_selection_case(actuals = rep(value, 36),
      futures = list(accurate = rep(3 * value, 6), safe = rep(value, 6)), date_type = date_type)
    dplyr::bind_rows(fixture$backtests, fixture$forecasts) %>%
      dplyr::mutate(Combo = stored_combos[series_index],
        Best_Model = ifelse(Model_ID == "accurate", "Yes", "No"),
        Run_Type = ifelse(Train_Test_ID == 1, "Future_Forecast", "Back_Test"))
  }))
  history <- dplyr::bind_rows(lapply(seq_along(contexts), function(series_index) {
    dplyr::mutate(contexts[[series_index]]$history, Combo = original_combos[series_index])
  }))
  list(metadata = metadata, contexts = contexts, history = history, forecasts = predictions,
    splits = contexts[[1]]$train_test_split, values = stats::setNames(values, original_combos),
    run_inputs = data.frame(combo = utils::tail(stored_combos, length(values)),
      model_type = "local", best_run_name = "previous"),
    agent_info = list(forecast_approach = approach),
    project_info = list(project_name = "reconciliation-test", run_name = "outer",
      date_type = date_type, data_output = "csv", object_output = "rds"))
}