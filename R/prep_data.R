#' Prep Data
#'
#' Preps data with various feature engineering recipes to create features before training models
#'
#' @param run_info Run info using [set_run_info()]
#' @param input_data A standard data frame, tibble, or spark data frame using sparklyr of historical time series data.
#'   Can also include external regressors for both historical and future data.
#'   Leading and trailing whitespace in character combo-variable values is
#'   removed before Finn creates internal series identifiers.
#' @param combo_variables List of column headers within input data to be used to separate individual time series.
#' @param target_variable The column header formatted as a character value within input data you want to forecast.
#' @param date_type The date granularity of the input data. Finn accepts the following as a character string:
#'   day, week, month, quarter, year.
#' @param forecast_horizon Number of periods to forecast into the future.
#' @param external_regressors List of column headers within input data to be used as features in multivariate models.
#'   Controls built-in R1/R2 recipes only. C1 retains eligible original numeric,
#'   character, factor, logical and Date columns independently of this list in
#'   bottoms_up runs; hierarchical C1 nodes keep only these aggregated regressors.
#'   Each custom workflow uses only its declared predictors plus Date and Combo.
#' @param hist_start_date Date value of when your input_data starts. Default of NULL uses earliest date value in
#'   input_data.
#' @param hist_end_date Date value of when your input_data ends. Default of NULL uses the latest date value in
#'   input_data.
#' @param combo_cleanup_date Date value to remove individual time series that don't contain non-zero values after
#'   that specified date. Default of NULL is to not remove any time series and attempt to forecast all time series.
#' @param fiscal_year_start Month number of start of fiscal year of input data, aids in building out date features.
#'   Formatted as a numeric value. Default of 1 assumes fiscal year starts in January.
#' @param clean_missing_values If TRUE, cleans missing values. Only impute values for missing data within an
#'   existing series, and does not add new values onto the beginning or end, but does provide a value of 0 for said
#'   values.
#' @param clean_outliers If TRUE, outliers are cleaned and inputted with values more in line with historical data.
#' @param box_cox Apply box-cox transformation to normalize variance in data
#' @param stationary Apply differencing to make data stationary
#' @param forecast_approach How the forecast is created. The default of 'bottoms_up' trains models for each individual
#'   time series. Value of 'grouped_hierarchy' creates a grouped time series to forecast at while 'standard_hierarchy' creates
#'   a more traditional hierarchical time series to forecast, both based on the hts package.
#' @param parallel_processing Default of NULL runs no parallel processing and forecasts each individual time series
#'   one after another. Value of 'local_machine' leverages all cores on current machine Finn is running on.
#'   Value of 'spark' runs time series in parallel on a spark cluster in Azure Databricks/Synapse.
#' @param num_cores Number of cores to run when parallel processing is set up. Used when running parallel computations
#'   on local machine or within Azure. Default of NULL uses total amount of cores on machine minus one. Can't be greater
#'   than number of cores on machine minus 1.
#' @param fourier_periods List of values to use in creating fourier series as features. Default of NULL automatically chooses
#'   these values based on the date_type.
#' @param lag_periods List of values to use in creating lag features. Default of NULL automatically chooses these values
#'   based on date_type.
#' @param rolling_window_periods List of values to use in creating rolling window features. Default of NULL automatically
#'   chooses these values based on date_type.
#' @param recipes_to_run List of recipes to run on multivariate models that can run different recipes. A value of NULL runs
#'   all recipes, but only runs the R1 recipe for weekly and daily date types. A value of "all" runs all recipes, regardless
#'   of date type. A list like c("R1") or c("R2") would only run models with the R1 or R2 recipe. Add "C1" to also write
#'   the custom-model recipe used by custom models: the padded history and future frame on the original scale, with
#'   missing values imputed only when `clean_missing_values` is TRUE and no outlier cleaning, Box-Cox, differencing or
#'   engineered features. C1 is never added by NULL or "all"; c("C1") alone prepares only the custom-model recipe.
#'   Reserved outcome/assessment columns and unsupported column types are excluded.
#'   Supply every required custom driver through the forecast horizon; missing
#'   future values error rather than being forecast or replaced with zero.
#'   C1 source identity is pinned before preparation, including interrupted runs.
#'   Changing eligible source data requires a new [set_run_info()] run.
#' @param multistep_horizon Use a multistep horizon approach when training multivariate models with R1 recipe.
#'
#' @return No return object. Feature engineered data is written to disk based on the output locations provided in
#'   [set_run_info()].
#' @examples
#' \donttest{
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(
#'     Date >= "2013-01-01",
#'     Date <= "2015-06-01"
#'   )
#'
#' run_info <- set_run_info()
#'
#' prep_data(run_info,
#'   input_data = data_tbl,
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month",
#'   forecast_horizon = 3,
#'   recipes_to_run = "R1"
#' )
#' }
#' @export
prep_data <- function(run_info,
                      input_data,
                      combo_variables,
                      target_variable,
                      date_type,
                      forecast_horizon,
                      external_regressors = NULL,
                      hist_start_date = NULL,
                      hist_end_date = NULL,
                      combo_cleanup_date = NULL,
                      fiscal_year_start = 1,
                      clean_missing_values = TRUE,
                      clean_outliers = FALSE,
                      box_cox = FALSE,
                      stationary = TRUE,
                      forecast_approach = "bottoms_up",
                      parallel_processing = NULL,
                      num_cores = NULL,
                      fourier_periods = NULL,
                      lag_periods = NULL,
                      rolling_window_periods = NULL,
                      recipes_to_run = NULL,
                      multistep_horizon = FALSE) {
  cli::cli_progress_step("Prepping Data")

  # Check if external regressors contain "original" in their names
  if (!is.null(external_regressors)) {
    original_vars <- external_regressors[grepl("original", external_regressors, ignore.case = TRUE)]
    if (length(original_vars) > 0) {
      cli::cli_warn(c(
        "!" = "External regressor name(s) contain 'original': {.val {original_vars}}",
        "i" = "This may cause conflicts with internal processing that creates '_original' columns.",
        "i" = "Consider renaming these variables to avoid potential issues."
      ))
    }
  }

  # check input values
  check_input_type("run_info", run_info, "list")
  check_input_type("input_data", input_data, c("tbl", "tbl_df", "data.frame", "tbl_spark"))
  check_input_type("combo_variables", combo_variables, "character")
  check_input_type("target_variable", target_variable, "character")
  check_input_type("date_type", date_type, "character", c("year", "quarter", "month", "week", "day"))
  check_input_type("forecast_horizon", forecast_horizon, "numeric")
  check_input_type("external_regressors", external_regressors, c("character", "NULL"))
  check_input_type("hist_start_date", hist_start_date, c("Date", "NULL"))
  check_input_type("hist_end_date", hist_end_date, c("Date", "NULL"))
  check_input_type("combo_cleanup_date", combo_cleanup_date, c("Date", "NULL"))
  check_input_type("fiscal_year_start", fiscal_year_start, "numeric")
  check_input_type("clean_missing_values", clean_missing_values, "logical")
  check_input_type("clean_outliers", clean_outliers, "logical")
  check_input_type("box_cox", box_cox, "logical")
  check_input_type("stationary", stationary, "logical")
  check_input_type("forecast_approach", forecast_approach, "character", c("bottoms_up", "grouped_hierarchy", "standard_hierarchy"))
  check_input_type("parallel_processing", parallel_processing, c("character", "NULL"), c("NULL", "local_machine", "spark"))
  check_input_type("num_cores", num_cores, c("numeric", "NULL"))
  check_input_type("fourier_periods", fourier_periods, c("list", "numeric", "NULL"))
  check_input_type("lag_periods", lag_periods, c("list", "numeric", "NULL"))
  check_input_type("rolling_window_periods", rolling_window_periods, c("list", "numeric", "NULL"))
  check_recipes_to_run(recipes_to_run)
  check_input_type("multistep_horizon", multistep_horizon, "logical")
  recipe_ids <- expand_recipes_to_run(recipes_to_run, date_type)
  c1_columns <- if ("C1" %in% recipe_ids) custom_input_columns(input_data, target_variable) else character()
  # Hierarchy nodes only carry aggregated external regressors, and hierarchy
  # custom models cannot declare predictors.
  if (forecast_approach != "bottoms_up") c1_columns <- intersect(c1_columns, external_regressors)

  input_data <- normalize_combo_values(
    input_data = input_data,
    combo_variables = combo_variables
  )

  check_input_data(
    input_data,
    combo_variables,
    target_variable,
    external_regressors,
    date_type,
    fiscal_year_start,
    parallel_processing
  )
  check_parallel_processing(
    run_info,
    parallel_processing
  )

  # get hist data start and end date
  hist_bounds <- resolve_hist_date_bounds(input_data, hist_start_date, hist_end_date)
  hist_start_date <- hist_bounds$hist_start_date
  hist_end_date <- hist_bounds$hist_end_date

  # prep initial data before feature engineering
  initial_prep_tbl <- input_data %>%
    tidyr::unite("Combo",
      tidyselect::all_of(combo_variables),
      sep = "--",
      remove = F
    ) %>%
    dplyr::rename("Target" = tidyselect::all_of(target_variable)) %>%
    dplyr::select(c(
      "Combo",
      tidyselect::all_of(combo_variables),
      tidyselect::all_of(unique(c(external_regressors, c1_columns))),
      "Date", "Target"
    )) %>%
    combo_cleanup_fn(
      combo_cleanup_date,
      hist_end_date
    ) %>%
    prep_hierarchical_data(run_info,
      combo_variables,
      external_regressors,
      forecast_approach,
      frequency_number = get_frequency_number(date_type)
    )

  # define columns to check for input changes
  cols_check_list <- c(
    "combo_variables", "target_variable", "date_type",
    "forecast_horizon", "external_regressors", "hist_start_date",
    "hist_end_date", "combo_cleanup_date", "fiscal_year_start",
    "clean_missing_values", "clean_outliers", "forecast_approach",
    "parallel_processing", "num_cores", "fourier_periods",
    "lag_periods", "rolling_window_periods", "recipes_to_run"
  )

  # read previous log
  previous_source <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  )
  prev_log_df <- previous_source %>%
    dplyr::select(tidyselect::any_of(cols_check_list))

  # create current log
  current_log_df <- tibble::tibble(
    combo_variables = paste(combo_variables, collapse = "---"),
    target_variable = target_variable,
    date_type = date_type,
    forecast_horizon = forecast_horizon,
    external_regressors = ifelse(is.null(external_regressors), NA, paste(external_regressors, collapse = "---")),
    hist_start_date = hist_start_date,
    hist_end_date = hist_end_date,
    combo_cleanup_date = ifelse(is.null(combo_cleanup_date), NA, combo_cleanup_date),
    fiscal_year_start = fiscal_year_start,
    clean_missing_values = clean_missing_values,
    clean_outliers = clean_outliers,
    forecast_approach = forecast_approach,
    fourier_periods = ifelse(is.null(fourier_periods), NA, paste(fourier_periods, collapse = "---")),
    lag_periods = ifelse(is.null(lag_periods), NA, paste(lag_periods, collapse = "---")),
    rolling_window_periods = ifelse(is.null(rolling_window_periods), NA, paste(rolling_window_periods, collapse = "---")),
    recipes_to_run = ifelse(is.null(recipes_to_run), NA, paste(recipes_to_run, collapse = "---"))
  ) %>%
    data.frame()
  if ("C1" %in% recipe_ids) {
    current_log_df$c1_source_id <- custom_source_identity(initial_prep_tbl)
    if (!is.null(previous_source) && nrow(previous_source) &&
      ("c1_source_id" %in% names(previous_source) ||
        ("recipes_to_run" %in% names(previous_source) &&
          any(grepl("C1", previous_source$recipes_to_run, fixed = TRUE)))) &&
      (!"c1_source_id" %in% names(previous_source) ||
        !identical(as.character(previous_source$c1_source_id), current_log_df$c1_source_id))) {
      stop("C1 custom source data changed; start a new run with set_run_info().", call. = FALSE)
    }
    if ("c1_source_id" %in% names(previous_source)) prev_log_df$c1_source_id <- previous_source[["c1_source_id"]]
    cols_check_list <- c(cols_check_list, "c1_source_id")
  }

  # check if run has already been completed
  if (length(cols_check_list) == length(colnames(prev_log_df))) {
    prev_log_df_aligned <- prev_log_df %>%
      dplyr::select(colnames(current_log_df)) %>%
      data.frame()

    if (hash_data(normalize_log_df(current_log_df)) != hash_data(normalize_log_df(prev_log_df_aligned))) {
      diff_details <- format_input_diff(prev_log_df_aligned, current_log_df)
      stop(
        "Inputs have recently changed in 'prep_data'.\n",
        "The following inputs differ from the previous run:\n",
        diff_details, "\n",
        "Please revert back to original inputs or start a ",
        "new run with 'set_run_info'.",
        call. = FALSE
      )
    } else {
      cli::cli_alert_info("Data Already Prepped")
      return(cli::cli_progress_done())
    }
  }

  # check if combos have already been processed
  current_combo_list <- initial_prep_tbl %>%
    dplyr::select(Combo) %>%
    dplyr::distinct(Combo) %>%
    dplyr::collect() %>%
    dplyr::distinct(Combo) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Combo_Hash = hash_data(Combo)) %>%
    dplyr::ungroup() %>%
    suppressWarnings()

  if ("combo" %in% names(run_info)) {
    prev_combo_list <- NULL
    combo_diff <- current_combo_list %>%
      dplyr::pull(Combo_Hash)
  } else {
    prev_combo_list <- prepared_recipe_combos(run_info, recipe_ids)
    if ("C1" %in% recipe_ids && length(prev_combo_list) &&
      !"c1_source_id" %in% names(previous_source)) {
      stop("C1 custom source identity is missing; start a new run with set_run_info().", call. = FALSE)
    }

    combo_diff <- setdiff(
      current_combo_list %>%
        dplyr::pull(Combo_Hash) %>%
        unique(),
      prev_combo_list
    )
  }

  # check if previous run is complete
  if (length(combo_diff) == 0 & length(prev_combo_list) > 0) {
    cli::cli_alert_info("Data Already Prepped")
    return(cli::cli_progress_done())
  }

  # Pin source identity before the first recipe write, including partial runs.
  if ("C1" %in% recipe_ids && !"c1_source_id" %in% names(previous_source)) {
    previous_source$c1_source_id <- current_log_df$c1_source_id
    write_data(previous_source, NULL, run_info, "log", "logs")
  }

  # filter to only combos that need to be processed
  current_combo_list_final <- current_combo_list %>%
    dplyr::filter(Combo_Hash %in% combo_diff) %>%
    dplyr::pull(Combo)

  filtered_initial_prep_tbl <- initial_prep_tbl %>% # filter input data on combos that haven't completed running
    dplyr::filter(Combo %in% current_combo_list_final)

  # compute global xregs_future_values_list across all combos so every combo
  # keeps the same raw xreg columns in recipe output (prevents NA when combined)
  global_xregs_future_list <- global_future_regressors(
    initial_prep_tbl,
    external_regressors,
    hist_end_date
  )

  # parallel run info
  if (is.null(parallel_processing) || parallel_processing == "local_machine") {
    par_info <- par_start(
      run_info = run_info,
      parallel_processing = parallel_processing,
      num_cores = num_cores,
      task_length = length(unique(filtered_initial_prep_tbl$Combo))
    )

    cl <- par_info$cl
    packages <- par_info$packages
    `%op%` <- par_info$foreach_operator

    # submit tasks
    final_data <- foreach::foreach(
      x = filtered_initial_prep_tbl %>%
        dplyr::select(Combo) %>%
        dplyr::distinct() %>%
        dplyr::group_split(dplyr::row_number(), .keep = FALSE),
      .combine = "rbind",
      .packages = packages,
      .errorhandling = "stop",
      .verbose = FALSE,
      .inorder = FALSE,
      .multicombine = TRUE,
      .noexport = NULL
    ) %op%
      {
        # get specific time series
        combo <- x %>%
          dplyr::pull(Combo)

        return_tbl <- tibble::tibble(
          Combo = combo,
          Combo_Hash = hash_data(combo)
        )

        initial_prep_combo_tbl <- filtered_initial_prep_tbl %>%
          dplyr::filter(Combo == combo) %>%
          dplyr::collect()

        # build join table for xregs that have future values globally
        xregs_future_tbl <- initial_prep_combo_tbl %>%
          dplyr::select(Combo, Date, tidyselect::any_of(global_xregs_future_list))

        # initial data prep
        initial_tbl <- initial_prep_combo_tbl %>%
          dplyr::filter(Combo == combo) %>%
          dplyr::select(
            Combo,
            Date,
            Target,
            tidyselect::all_of(setdiff(external_regressors, global_xregs_future_list))
          ) %>%
          dplyr::group_by(Combo) %>%
          timetk::pad_by_time(Date,
            .by = date_type,
            .pad_value = ifelse(clean_missing_values, NA, 0),
            .end_date = hist_end_date
          ) %>% # fill in missing values in between existing data points
          timetk::pad_by_time(Date,
            .by = date_type,
            .pad_value = 0,
            .start_date = hist_start_date,
            .end_date = hist_end_date
          ) %>% # fill in missing values at beginning of time series with zero
          timetk::future_frame(Date,
            .length_out = forecast_horizon,
            .bind_data = TRUE
          ) %>% # add future data
          dplyr::ungroup() %>%
          dplyr::left_join(xregs_future_tbl,
            by = c("Combo", "Date")
          ) # join xregs that contain future values given by user

        if ("C1" %in% recipe_ids) {
          write_data(
            x = custom_view_build(
              custom_source_join(initial_tbl, initial_prep_combo_tbl, c1_columns),
              c1_columns,
              character(),
              clean_missing_values,
              date_type,
              hist_end_date
            ),
            combo = combo,
            run_info = run_info,
            output_type = "data",
            folder = "prep_data",
            suffix = "-C1"
          )
        }

        initial_tbl <- initial_tbl %>%
          clean_outliers_missing_values(
            clean_outliers,
            clean_missing_values,
            get_frequency_number(date_type),
            external_regressors
          ) %>% # clean outliers and missing values
          dplyr::mutate_if(is.numeric, list(~ replace(., is.infinite(.), NA))) %>% # replace infinite values
          dplyr::mutate_if(is.numeric, list(~ replace(., is.nan(.), NA))) %>% # replace NaN values
          dplyr::mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>% # replace NA values
          dplyr::mutate(Target = ifelse(Date > hist_end_date,
            NA,
            Target
          ))

        # adjust original target col
        if ("Target_Original" %in% colnames(initial_tbl)) {
          initial_tbl <- initial_tbl %>%
            dplyr::mutate(Target_Original = ifelse(Date > hist_end_date, NA, Target_Original))
        }

        # preserve raw external regressors for foundation models that use raw xregs as features
        xreg_raw_df <- NULL
        if (!is.null(external_regressors) && length(external_regressors) > 0) {
          xreg_raw_df <- initial_tbl %>%
            dplyr::select(Date, tidyselect::all_of(external_regressors))
        }

        # box-cox transformation
        if (box_cox) {
          box_cox_tbl <- initial_tbl %>%
            apply_box_cox()

          initial_tbl <- box_cox_tbl$data

          return_tbl <- return_tbl %>%
            dplyr::left_join(box_cox_tbl$diff_info, by = "Combo")
        }

        # make stationary
        if (stationary) {
          stationary_tbl <- initial_tbl %>%
            make_stationary()

          initial_tbl <- stationary_tbl$data

          return_tbl <- return_tbl %>%
            dplyr::left_join(stationary_tbl$diff_info, by = "Combo")
        }

        # date features
        date_features <- initial_tbl %>%
          dplyr::select(Date) %>%
          dplyr::mutate(
            Date_Adj = Date %m+% months(fiscal_year_start - 1),
            Date_day_month_end = ifelse(lubridate::day(Date_Adj) == lubridate::days_in_month(Date_Adj), 1, 0)
          ) %>%
          timetk::tk_augment_timeseries_signature(Date_Adj) %>%
          dplyr::select(!tidyselect::matches(get_date_regex(date_type)), -Date_Adj, -Date)

        names(date_features) <- stringr::str_c("Date_", names(date_features))

        initial_tbl <- initial_tbl %>%
          cbind(date_features)

        # Run Recipes
        if ("R1" %in% recipe_ids) {
          R1 <- initial_tbl %>%
            multivariate_prep_recipe_1(external_regressors,
              xregs_future_values_list = global_xregs_future_list,
              get_fourier_periods(fourier_periods, date_type),
              get_lag_periods(lag_periods, date_type, forecast_horizon, multistep_horizon, TRUE),
              get_rolling_window_periods(rolling_window_periods, date_type),
              hist_end_date,
              date_type,
              xreg_raw_df = xreg_raw_df
            ) %>%
            dplyr::mutate(dplyr::across(
              tidyselect::any_of(c("Target", "Target_Original")),
              ~ base::ifelse(Date > hist_end_date, NA, .x)
            ))

          write_data(
            x = R1,
            combo = combo,
            run_info = run_info,
            output_type = "data",
            folder = "prep_data",
            suffix = "-R1"
          )
        }

        if ("R2" %in% recipe_ids) {
          R2 <- initial_tbl %>%
            multivariate_prep_recipe_2(external_regressors,
              xregs_future_values_list = global_xregs_future_list,
              get_fourier_periods(fourier_periods, date_type),
              get_lag_periods(lag_periods, date_type, forecast_horizon),
              get_rolling_window_periods(rolling_window_periods, date_type),
              date_type,
              forecast_horizon,
              hist_end_date
            ) %>%
            dplyr::mutate(dplyr::across(
              tidyselect::any_of(c("Target", "Target_Original")),
              ~ base::ifelse(Date > hist_end_date, NA, .x)
            ))

          write_data(
            x = R2,
            combo = combo,
            run_info = run_info,
            output_type = "data",
            folder = "prep_data",
            suffix = "-R2"
          )
        }
        return(return_tbl)
      } %>%
      base::suppressPackageStartupMessages()
    # clean up any parallel run process
    par_end(cl)
  } else if (parallel_processing == "spark") {
    final_data <- filtered_initial_prep_tbl %>%
      adjust_df(return_type = "sdf") %>%
      sparklyr::spark_apply(
        function(df, context) {
          # update objects
          fn_env <- .GlobalEnv

          for (name in names(context)) {
            assign(name, context[[name]], envir = fn_env)
          }

          # get specific time series
          combo <- unique(df$Combo)

          return_tbl <- tibble::tibble(
            Combo = combo,
            Combo_Hash = hash_data(combo)
          )

          # build join table for xregs that have future values globally
          xregs_future_tbl <- df %>%
            dplyr::select(Combo, Date, tidyselect::any_of(global_xregs_future_list))

          # initial data prep
          initial_tbl <- df %>%
            dplyr::filter(Combo == combo) %>%
            dplyr::select(
              Combo,
              Date,
              Target,
              tidyselect::all_of(setdiff(external_regressors, global_xregs_future_list))
            ) %>%
            dplyr::group_by(Combo) %>%
            timetk::pad_by_time(Date,
              .by = date_type,
              .pad_value = ifelse(clean_missing_values, NA, 0),
              .end_date = hist_end_date
            ) %>% # fill in missing values in between existing data points
            timetk::pad_by_time(Date,
              .by = date_type,
              .pad_value = 0,
              .start_date = hist_start_date,
              .end_date = hist_end_date
            ) %>% # fill in missing values at beginning of time series with zero
            timetk::future_frame(Date,
              .length_out = forecast_horizon,
              .bind_data = TRUE
            ) %>% # add future data
            dplyr::ungroup() %>%
            dplyr::left_join(xregs_future_tbl,
              by = c("Combo", "Date")
            ) # join xregs that contain values given by user

          if ("C1" %in% recipe_ids) {
            write_data(
              x = custom_view_build(
                custom_source_join(initial_tbl, df, c1_columns),
                c1_columns,
                character(),
                clean_missing_values,
                date_type,
                hist_end_date
              ),
              combo = combo,
              run_info = run_info,
              output_type = "data",
              folder = "prep_data",
              suffix = "-C1"
            )
          }

          initial_tbl <- initial_tbl %>%
            clean_outliers_missing_values(
              clean_outliers,
              clean_missing_values,
              get_frequency_number(date_type),
              external_regressors
            ) %>% # clean outliers and missing values
            dplyr::mutate_if(is.numeric, list(~ replace(., is.infinite(.), NA))) %>% # replace infinite values
            dplyr::mutate_if(is.numeric, list(~ replace(., is.nan(.), NA))) %>% # replace NaN values
            dplyr::mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>% # replace NA values
            dplyr::mutate(Target = ifelse(Date > hist_end_date,
              NA,
              Target
            ))

          # adjust original target col
          if ("Target_Original" %in% colnames(initial_tbl)) {
            initial_tbl <- initial_tbl %>%
              dplyr::mutate(Target_Original = ifelse(Date > hist_end_date, NA, Target_Original))
          }

          # preserve raw external regressors for TimeGPT
          xreg_raw_df <- NULL
          if (!is.null(external_regressors) && length(external_regressors) > 0) {
            xreg_raw_df <- initial_tbl %>%
              dplyr::select(Date, tidyselect::all_of(external_regressors))
          }

          # box-cox transformation
          if (box_cox) {
            box_cox_tbl <- initial_tbl %>%
              apply_box_cox()

            initial_tbl <- box_cox_tbl$data

            return_tbl <- return_tbl %>%
              dplyr::left_join(box_cox_tbl$diff_info, by = "Combo")
          }

          # make stationary
          if (stationary) {
            stationary_tbl <- initial_tbl %>%
              make_stationary()

            initial_tbl <- stationary_tbl$data

            return_tbl <- return_tbl %>%
              dplyr::left_join(stationary_tbl$diff_info, by = "Combo")
          }

          # create date features
          date_features <- initial_tbl %>%
            dplyr::select(Date) %>%
            dplyr::mutate(
              Date_Adj = Date %m+% months(fiscal_year_start - 1),
              Date_day_month_end = ifelse(lubridate::day(Date_Adj) == lubridate::days_in_month(Date_Adj), 1, 0)
            ) %>%
            timetk::tk_augment_timeseries_signature(Date_Adj) %>%
            dplyr::select(!tidyselect::matches(get_date_regex(date_type)), -Date_Adj, -Date)

          names(date_features) <- stringr::str_c("Date_", names(date_features))

          initial_tbl <- initial_tbl %>%
            cbind(date_features)

          # Run Recipes
          if ("R1" %in% recipe_ids) {
            R1 <- initial_tbl %>%
              multivariate_prep_recipe_1(external_regressors,
                xregs_future_values_list = global_xregs_future_list,
                get_fourier_periods(fourier_periods, date_type),
                get_lag_periods(lag_periods, date_type, forecast_horizon, multistep_horizon, TRUE),
                get_rolling_window_periods(rolling_window_periods, date_type),
                hist_end_date,
                date_type,
                xreg_raw_df = xreg_raw_df
              ) %>%
              dplyr::mutate(dplyr::across(
                tidyselect::any_of(c("Target", "Target_Original")),
                ~ base::ifelse(Date > hist_end_date, NA, .x)
              ))

            write_data(
              x = R1,
              combo = combo,
              run_info = run_info,
              output_type = "data",
              folder = "prep_data",
              suffix = "-R1"
            )
          }

          if ("R2" %in% recipe_ids) {
            R2 <- initial_tbl %>%
              multivariate_prep_recipe_2(external_regressors,
                xregs_future_values_list = global_xregs_future_list,
                get_fourier_periods(fourier_periods, date_type),
                get_lag_periods(lag_periods, date_type, forecast_horizon),
                get_rolling_window_periods(rolling_window_periods, date_type),
                date_type,
                forecast_horizon
              ) %>%
              dplyr::mutate(dplyr::across(
                tidyselect::any_of(c("Target", "Target_Original")),
                ~ base::ifelse(Date > hist_end_date, NA, .x)
              ))

            write_data(
              x = R2,
              combo = combo,
              run_info = run_info,
              output_type = "data",
              folder = "prep_data",
              suffix = "-R2"
            )
          }

          return(data.frame(return_tbl))
        },
        group_by = "Combo",
        context = list(
          external_regressors = external_regressors,
          clean_missing_values = clean_missing_values,
          clean_outliers_missing_values = clean_outliers_missing_values,
          hash_data = hash_data,
          hist_end_date = hist_end_date,
          hist_start_date = hist_start_date,
          forecast_approach = forecast_approach,
          forecast_horizon = forecast_horizon,
          clean_outliers = clean_outliers,
          get_frequency_number = get_frequency_number,
          date_type = date_type,
          fiscal_year_start = fiscal_year_start,
          get_date_regex = get_date_regex,
          recipes_to_run = recipes_to_run,
          recipe_ids = recipe_ids,
          custom_view_build = custom_view_build,
          custom_source_join = custom_source_join,
          c1_columns = c1_columns,
          multivariate_prep_recipe_1 = multivariate_prep_recipe_1,
          multivariate_prep_recipe_2 = multivariate_prep_recipe_2,
          run_info = run_info,
          get_fourier_periods = get_fourier_periods,
          fourier_periods = fourier_periods,
          get_lag_periods = get_lag_periods,
          lag_periods = lag_periods,
          get_rolling_window_periods = get_rolling_window_periods,
          rolling_window_periods = rolling_window_periods,
          write_data = write_data,
          write_data_folder = write_data_folder,
          write_data_type = write_data_type,
          box_cox = box_cox,
          stationary = stationary,
          make_stationary = make_stationary,
          apply_box_cox = apply_box_cox,
          global_xregs_future_list = global_xregs_future_list
        )
      )
  }

  # check if all time series combos ran correctly
  if (!"combo" %in% names(run_info)) {
    successful_combos <- prepared_recipe_combos(run_info, recipe_ids) %>%
      length()

    total_combos <- current_combo_list %>%
      dplyr::pull(Combo_Hash) %>%
      unique() %>%
      length()

    if (successful_combos != total_combos) {
      stop(
        paste0(
          "Not all time series were prepped within 'prep_data', expected ",
          total_combos, " time series but only ", successful_combos,
          " time series are prepped. ", "Please run 'prep_data' again."
        ),
        call. = FALSE
      )
    }
  }

  # update logging file
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$project_name), "-", hash_data(run_info$run_name), ".csv"),
    return_type = "df"
  ) %>%
    dplyr::mutate(
      combo_variables = paste(combo_variables, collapse = "---"),
      target_variable = target_variable,
      date_type = date_type,
      forecast_horizon = forecast_horizon,
      external_regressors = ifelse(is.null(external_regressors), NA, paste(external_regressors, collapse = "---")),
      hist_start_date = hist_start_date,
      hist_end_date = hist_end_date,
      combo_cleanup_date = ifelse(is.null(combo_cleanup_date), NA, combo_cleanup_date),
      fiscal_year_start = fiscal_year_start,
      clean_missing_values = clean_missing_values,
      clean_outliers = clean_outliers,
      stationary = stationary,
      box_cox = box_cox,
      forecast_approach = forecast_approach,
      parallel_processing = ifelse(is.null(parallel_processing), NA, parallel_processing),
      num_cores = ifelse(is.null(num_cores), NA, num_cores),
      fourier_periods = ifelse(is.null(fourier_periods), NA, paste(fourier_periods, collapse = "---")),
      lag_periods = ifelse(is.null(lag_periods), NA, paste(lag_periods, collapse = "---")),
      rolling_window_periods = ifelse(is.null(rolling_window_periods), NA, paste(rolling_window_periods, collapse = "---")),
      recipes_to_run = ifelse(is.null(recipes_to_run), NA, paste(recipes_to_run, collapse = "---")),
      multistep_horizon = multistep_horizon
    )
  if ("C1" %in% recipe_ids) log_df$c1_source_id <- current_log_df$c1_source_id

  write_data(
    x = log_df,
    combo = NULL,
    run_info = run_info,
    output_type = "log",
    folder = "logs",
    suffix = NULL
  )

  # write any transformation data
  if (box_cox || stationary) {
    write_data(
      x = final_data,
      combo = NULL,
      run_info = run_info,
      output_type = "data",
      folder = "prep_data",
      suffix = "-orig_combo_info"
    )
  }
}

#' Resolve the historical start and end dates
#'
#' Fills NULL bounds with the earliest and latest `Date` in the input so every
#' caller derives the same history window as [prep_data()].
#'
#' @param input_data input data with a `Date` column
#' @param hist_start_date Date value or NULL for the earliest date
#' @param hist_end_date Date value or NULL for the latest date
#'
#' @return list with `hist_start_date` and `hist_end_date`
#' @noRd
resolve_hist_date_bounds <- function(input_data,
                                     hist_start_date,
                                     hist_end_date) {
  if (is.null(hist_end_date)) {
    hist_end_date <- input_data %>%
      dplyr::select(Date) %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::distinct() %>%
      dplyr::filter(Date == max(Date)) %>%
      dplyr::pull(Date) %>%
      suppressWarnings()
  }

  if (is.null(hist_start_date)) {
    hist_start_date <- input_data %>%
      dplyr::select(Date) %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::distinct() %>%
      dplyr::filter(Date == min(Date)) %>%
      dplyr::pull(Date) %>%
      suppressWarnings()
  }

  list(hist_start_date = hist_start_date, hist_end_date = hist_end_date)
}

#' Find external regressors with future values
#'
#' An external regressor becomes a future regressor when any row after the
#' historical end date has a non-missing value, across all combos.
#'
#' @param data_tbl input data with `Date` and regressor columns
#' @param external_regressors external regressor names or NULL
#' @param hist_end_date last historical date
#'
#' @return character vector of future regressor names, or NULL when none
#' @noRd
global_future_regressors <- function(data_tbl,
                                     external_regressors,
                                     hist_end_date) {
  if (is.null(external_regressors) || length(external_regressors) == 0) {
    return(NULL)
  }

  global_future <- c()
  for (xr in external_regressors) {
    has_future <- data_tbl %>%
      dplyr::filter(Date > hist_end_date, !is.na(.data[[xr]])) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::collect() %>%
      dplyr::pull(n)
    if (isTRUE(has_future > 0)) {
      global_future <- c(global_future, xr)
    }
  }

  if (length(global_future) > 0) global_future else NULL
}

#' Function to remove time series that don't contain recent values
#'
#' @param df data frame
#' @param combo_cleanup_date date value to test for non-zero values after
#' @param hist_end_date last period in historical data
#'
#' @return tbl with or without specific time series removed
#' @noRd
combo_cleanup_fn <- function(df,
                             combo_cleanup_date,
                             hist_end_date) {
  if (!is.null(combo_cleanup_date)) {
    combo_df <- df %>%
      dplyr::filter(
        Date >= combo_cleanup_date,
        Date <= hist_end_date
      ) %>%
      dplyr::group_by(Combo) %>%
      dplyr::summarise(Sum = sum(Target, na.rm = TRUE)) %>%
      dplyr::filter(Sum != 0) %>%
      dplyr::select(Combo) %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::distinct() %>%
      dplyr::pull(Combo)

    df %>% dplyr::filter(Combo %in% combo_df)
  } else {
    df
  }
}

#' Function to get external regressor features that contain future values after hist_end_date
#'
#' @param data_tbl data frame
#' @param external_regressors list of external regressors
#' @param hist_end_date date of when your historical data ends
#'
#' @return tbl with external regressors with future values
#' @noRd
get_xregs_future_values_tbl <- function(data_tbl,
                                        external_regressors,
                                        hist_end_date) {
  xregs_future_values_list <- c()

  for (variable in external_regressors) {
    temp <- data_tbl %>%
      dplyr::filter(Date > hist_end_date) %>%
      dplyr::select(tidyselect::all_of(variable)) %>%
      tidyr::drop_na()

    if (nrow(temp) > 0) {
      xregs_future_values_list <- append(
        xregs_future_values_list,
        variable
      )
    }
  }

  data_tbl %>%
    dplyr::select(
      Combo,
      Date,
      tidyselect::all_of(xregs_future_values_list)
    )
}

# Select eligible original input columns for C1 without changing built-in xregs.
# Zero-row collection obtains Spark schema without materializing its input.
# Reserved outcomes/assessment metadata and unsupported vector types are excluded.
custom_input_columns <- function(data, target_variable) {
  schema <- dplyr::collect(utils::head(data, 0))
  supported <- vapply(schema, function(value) is.numeric(value) || is.character(value) ||
    is.factor(value) || is.logical(value) || inherits(value, "Date"), logical(1))
  setdiff(names(schema)[supported], c(target_variable, "Target", "Date", "Combo",
    "Target_Original", "Train_Test_ID", "Run_Type", "Horizon", "Origin", ".finnts_row", ".row", ".pred"))
}

# Hash original C1 input deterministically before any completed-run shortcut.
# Both backends hash the same per-series map; Spark collects only that small map.
# Sorted rows/columns make input ordering irrelevant; no artifacts are written.
custom_source_identity <- function(data) {
  hash_panel <- function(data) {
    data <- as.data.frame(data)[, sort(names(data)), drop = FALSE]
    keys <- intersect(c("Combo", "Date"), names(data))
    data <- data[do.call(order, data[keys]), , drop = FALSE]
    rownames(data) <- NULL
    digest::digest(data, algo = "sha256")
  }
  environment(hash_panel) <- baseenv()
  if (inherits(data, "tbl_spark")) {
    hash_group <- function(df, context) {
      data.frame(Combo = unique(df$Combo), source_id = context$hash_panel(df))
    }
    environment(hash_group) <- baseenv()
    hashes <- sparklyr::spark_apply(data, hash_group,
      group_by = "Combo", context = list(hash_panel = hash_panel)) %>% dplyr::collect()
  } else {
    hashes <- dplyr::bind_rows(lapply(split(as.data.frame(data), data$Combo), function(panel) {
      data.frame(Combo = unique(panel$Combo), source_id = hash_panel(panel))
    }))
  }
  hash_panel(hashes)
}

# Project each padded original-scale timeline onto raw C1 source columns.
# Original source values, including incomplete future cells, are never replaced
# by the built-in recipe's forecasted or selected regressors. No I/O.
custom_source_join <- function(timeline, source, columns) {
  dplyr::left_join(dplyr::select(timeline, Combo, Date, Target),
    dplyr::select(source, Combo, Date, tidyselect::all_of(columns)), by = c("Combo", "Date"))
}

#' Build the custom-model C1 view of one prepared series
#'
#' C1 is the custom-model-only recipe. It keeps the padded history and future
#' frame on the original (scoring) scale: no outlier cleaning, Box-Cox,
#' differencing or engineered features. Non-finite values are treated as
#' missing. Missing values are imputed only when `clean_missing_values` is TRUE,
#' using the same imputation as built-in recipes. Remaining missing history
#' values become zero. Future rows keep
#' NA for regressors without supplied future values, so custom models that
#' declare them fail predictor validation instead of forecasting from zeros.
#' Future source cells outside `future_regressors` are restored after cleaning,
#' preventing history imputation from inventing required future inputs.
#'
#' @param df Padded series table with Combo, Date, Target and joined regressors,
#'   taken before outlier cleaning or transformations.
#' @param external_regressors Character vector of declared regressors, or NULL.
#' @param future_regressors Regressors that carry user-supplied future values.
#' @param clean_missing_values Logical; impute missing history values.
#' @param date_type Date type used for the imputation period.
#' @param hist_end_date Last historical date; later Target values become NA.
#'
#' @return Tibble with Combo, Date, Target and the declared regressors. No I/O.
#' @noRd
custom_view_build <- function(df,
                              external_regressors,
                              future_regressors,
                              clean_missing_values,
                              date_type,
                              hist_end_date) {
  external_regressors <- as.character(unlist(external_regressors))
  future_regressors <- as.character(unlist(future_regressors))
  view <- df %>%
    dplyr::select(Combo, Date, Target, tidyselect::all_of(external_regressors)) %>%
    dplyr::mutate(dplyr::across(
      (where(is.numeric) & tidyselect::all_of(c("Target", external_regressors))),
      function(values) replace(values, !is.finite(values), NA)
    )) %>%
    clean_outliers_missing_values(
      FALSE,
      clean_missing_values,
      get_frequency_number(date_type),
      external_regressors
    )
  history <- view$Date <= hist_end_date
  for (column in setdiff(external_regressors, future_regressors)) {
    view[[column]][!history] <- df[[column]][!history]
  }
  for (column in c("Target", external_regressors)) {
    values <- view[[column]]
    if (!is.numeric(values)) {
      next
    }
    values[!is.finite(values)] <- NA
    fill <- history | column %in% future_regressors
    values[fill & is.na(values)] <- 0
    view[[column]] <- values
  }
  view$Target[!history] <- NA
  view
}

#' Expand requested recipe identifiers into concrete recipe files
#'
#' `NULL` keeps the date-type defaults and `"all"` means R1 plus R2. C1 is
#' included only when explicitly requested. Used for restart, completion and
#' artifact discovery so every stage agrees on the expected files.
#'
#' @param recipes_to_run Requested recipes: NULL or values from R1, R2, C1, all.
#' @param date_type Date type used for the NULL defaults.
#'
#' @return Unique character vector of recipe ids, R recipes first.
#' @noRd
expand_recipes_to_run <- function(recipes_to_run, date_type) {
  recipes <- as.character(unlist(recipes_to_run))
  if (!length(recipes) || all(is.na(recipes))) {
    return(get_recipes_to_run(NULL, date_type))
  }
  standard <- setdiff(recipes, c("all", "C1"))
  if ("all" %in% recipes) {
    standard <- c("R1", "R2")
  } else if (!length(standard) && !"C1" %in% recipes) {
    standard <- get_recipes_to_run(NULL, date_type)
  }
  unique(c(intersect(c("R1", "R2"), standard), if ("C1" %in% recipes) "C1"))
}

#' Add the custom-model C1 recipe to a recipe request
#'
#' Used by [forecast_time_series()] and the Agent when custom models are
#' selected. Keeps the requested R recipes (date-type defaults for NULL, R1
#' plus R2 for "all") and appends C1 when missing.
#'
#' @param recipes_to_run Requested recipes: NULL or values from R1, R2, C1, all.
#' @param date_type Date type used for the NULL defaults.
#'
#' @return Character vector of concrete recipe ids including C1.
#' @noRd
add_custom_recipe <- function(recipes_to_run, date_type) {
  recipes <- as.character(unlist(recipes_to_run))
  standard <- if (!length(recipes) || all(recipes == "C1")) {
    if (length(recipes)) character() else get_recipes_to_run(NULL, date_type)
  } else {
    setdiff(expand_recipes_to_run(recipes, date_type), "C1")
  }
  c(standard, "C1")
}

#' Expand the recipes recorded in a run log
#'
#' Splits the logged `recipes_to_run` ("---" separated, or NA) and expands it
#' with `expand_recipes_to_run()` for the logged date type.
#'
#' @param log_df Run log data frame with `recipes_to_run` and `date_type`.
#'
#' @return Character vector of prepared recipe ids.
#' @noRd
logged_recipe_ids <- function(log_df) {
  configured <- log_df$recipes_to_run
  configured <- if (length(configured) == 0L || is.na(configured[[1]])) {
    NULL
  } else {
    strsplit(as.character(configured[[1]]), "---", fixed = TRUE)[[1]]
  }
  expand_recipes_to_run(configured, log_df$date_type[[1]])
}

#' Require an R recipe when built-in models are selected
#'
#' Built-in models train only on R1/R2. C1-only runs may therefore select only
#' custom models. Called before any custom manifest write and again when
#' workflows are built, so a rejected request leaves no pinned artifacts.
#'
#' @param builtin_models Character vector of selected built-in model names.
#' @param recipes Character vector of prepared recipe ids.
#'
#' @return Invisible NULL; errors when built-ins have no R recipe.
#' @noRd
check_builtin_recipes <- function(builtin_models, recipes) {
  if (length(builtin_models) && !any(recipes %in% c("R1", "R2"))) {
    stop(
      "Built-in models need recipe R1 (or R2), but this run prepared only C1. ",
      "Rerun prep_data with R1 in recipes_to_run in a new run, or select only custom models.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Validate the requested recipe identifiers
#'
#' Accepts NULL or a vector/list of distinct values from R1, R2, C1 and all.
#' Rejects unknown, missing or duplicated values with an actionable error.
#'
#' @param recipes_to_run Requested recipes.
#'
#' @return Invisibly NULL; errors on invalid input.
#' @noRd
check_recipes_to_run <- function(recipes_to_run) {
  if (is.null(recipes_to_run)) {
    return(invisible(NULL))
  }
  recipes <- unlist(recipes_to_run)
  allowed <- c("R1", "R2", "C1", "all")
  if (!is.character(recipes) || !length(recipes) || anyNA(recipes) || any(!recipes %in% allowed) || anyDuplicated(recipes)) {
    stop(
      "invalid value for input name 'recipes_to_run', values need to be distinct and from ",
      paste(allowed, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Find combos with every requested recipe file prepared
#'
#' Lists the run's prep_data recipe files once and keeps combos that have a
#' file for each recipe in `recipe_ids`. Used for restart and completion checks
#' so a combo with only some recipes (for example R1 without C1) is redone.
#'
#' @param run_info Run info list.
#' @param recipe_ids Expected recipe ids from `expand_recipes_to_run()`.
#'
#' @return Character vector of combo hashes with all recipes prepared.
#' @noRd
prepared_recipe_combos <- function(run_info, recipe_ids) {
  paths <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/prep_data/*", hash_data(run_info$project_name), "-",
      hash_data(run_info$run_name), "-*.", run_info$data_output
    )
  )
  paths <- as.character(unlist(paths))
  if (!length(paths)) {
    return(character())
  }
  parts <- strsplit(fs::path_ext_remove(fs::path_file(paths)), "-", fixed = TRUE)
  files <- tibble::tibble(
    Combo = vapply(parts, function(x) if (length(x) == 4) x[[3]] else NA_character_, character(1)),
    Recipe = vapply(parts, function(x) if (length(x) == 4) x[[4]] else NA_character_, character(1))
  ) %>%
    dplyr::filter(!is.na(Combo), Recipe %in% recipe_ids) %>%
    dplyr::distinct(Combo, Recipe) %>%
    dplyr::count(Combo) %>%
    dplyr::filter(n == length(recipe_ids))
  unique(files$Combo)
}

#' Function to replace outliers and fill in missing values
#'
#' @param df data frame
#' @param clean_outliers logical; clean outliers or not
#' @param clean_missing_values logical; clean missing values or not
#' @param frequency_number number of time‑series frequency
#' @param external_regressors character vector of external regressor names
#'
#' @return tbl with or without missing / outlier values replaced
#' @noRd
clean_outliers_missing_values <- function(df,
                                          clean_outliers,
                                          clean_missing_values,
                                          frequency_number,
                                          external_regressors) {
  # Create Target_Original copy only when outlier cleaning is on
  if (clean_outliers && "Target" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        Target_Original = if (clean_missing_values) {
          timetk::ts_impute_vec(Target, period = frequency_number)
        } else {
          Target
        }
      )
  }

  correct_clean_func <- function(col) {
    if (sum(!is.na(col)) < 2) {
      col
    } else if (clean_outliers) {
      timetk::ts_clean_vec(col, period = frequency_number)
    } else if (clean_missing_values) {
      timetk::ts_impute_vec(col, period = frequency_number)
    } else {
      col
    }
  }

  df_clean <- df %>%
    dplyr::mutate(
      dplyr::across(
        (where(is.numeric) & tidyselect::all_of(c("Target", external_regressors))),
        correct_clean_func
      )
    )

  # Ensure earliest value of Target matches Target_Original
  if ("Target_Original" %in% names(df_clean)) {
    df_clean$Target[1] <- df_clean$Target_Original[1]
  }

  tibble::tibble(df_clean)
}

#' Function to get frequency number of time series
#'
#' @param date_type date type
#'
#' @return frequency number of time series
#' @noRd
get_frequency_number <- function(date_type) {
  frequency_number <- switch(date_type,
    "year" = 1,
    "quarter" = 4,
    "month" = 12,
    "week" = 365.25 / 7,
    "day" = 365.25
  )

  return(frequency_number)
}

#' Function to get how many fourier periods to use
#'
#' @param fourier_periods list of fourier periods
#' @param date_type date type
#'
#' @return list of fourier periods
#' @noRd
get_fourier_periods <- function(fourier_periods,
                                date_type) {
  if (!is.null(fourier_periods)) {
    return(fourier_periods)
  }

  fourier_periods <- switch(date_type,
    "year" = c(1, 2, 3, 4, 5),
    "quarter" = c(1, 2, 3, 4),
    "month" = c(3, 6, 9, 12),
    "week" = c(2, 4, 8, 12, 24, 48, 52),
    "day" = c(
      7, 14, 21, 28, 28 * 2,
      28 * 3, 28 * 6, 28 * 9, 28 * 12, 365
    )
  )

  return(fourier_periods)
}

#' Function to get lag periods to use
#'
#' @param lag_periods list of lag periods
#' @param date_type date type
#' @param forecast_horizon forecast horizon
#' @param multistep_horizon multistep horizon
#' @param feature_engineering adjust lags used for feature engineering
#'
#' @return list of lag periods
#' @noRd
get_lag_periods <- function(lag_periods,
                            date_type,
                            forecast_horizon,
                            multistep_horizon = FALSE,
                            feature_engineering = FALSE) {
  if (!is.null(lag_periods)) {
    lag_periods <- as.numeric(unlist(lag_periods, use.names = FALSE))
    if (multistep_horizon && max(lag_periods) < forecast_horizon) {
      return(unique(c(lag_periods, forecast_horizon)))
    }
    return(lag_periods)
  }
  oplist <- switch(date_type,
    "year" = c(1, 2, 3),
    "quarter" = c(1, 2, 3, 4),
    "month" = c(1, 2, 3, 6, 9, 12),
    "week" = c(1, 2, 3, 4, 8, 12, 24, 48, 52),
    "day" = c(7, 14, 21, 28, 60, 90, 180, 365)
  )

  # change multistep horizons to run based on date type
  if (multistep_horizon) {
    if (date_type == "day") {
      if (feature_engineering) {
        oplist <- c(28, 60, 90, 180, 365)
      } else {
        oplist <- c(28, 90, 180)
      }
    } else if (date_type == "week") {
      if (feature_engineering) {
        oplist <- c(4, 8, 12, 24, 48, 52)
      } else {
        oplist <- c(4, 12, 24)
      }
    } else if (date_type == "month") {
      if (feature_engineering) {
        oplist <- c(1, 2, 3, 6, 9, 12)
      } else {
        oplist <- c(1, 2, 3, 6, 12)
      }
    }

    if (max(oplist) < forecast_horizon) {
      lag_periods <- c(oplist, forecast_horizon)
    } else {
      lag_periods <- oplist
    }
  } else {
    oplist <- c(oplist, forecast_horizon)
    lag_periods <- oplist[oplist >= forecast_horizon]
    lag_periods <- unique(lag_periods)
  }

  return(lag_periods)
}

#' Function to get rolling window periods
#'
#' @param rolling_window_periods list of rolling window periods
#' @param date_type date type
#'
#' @return list of rolling window periods
#' @noRd
get_rolling_window_periods <- function(rolling_window_periods,
                                       date_type) {
  if (!is.null(rolling_window_periods)) {
    return(rolling_window_periods)
  }

  rolling_window_periods <- switch(date_type,
    "year" = c(2, 3, 4, 5),
    "quarter" = c(2, 3, 4),
    "month" = c(3, 6, 9, 12),
    "week" = c(2, 4, 8, 12, 24, 48, 52),
    "day" = c(
      7, 14, 21, 28, 28 * 2,
      28 * 3, 28 * 6, 28 * 9, 28 * 12, 365
    )
  )

  return(rolling_window_periods)
}

#' Function to get recipes to use in feature engineering
#'
#' @param recipes_to_run list of recipes to run
#' @param date_type date type
#'
#' @return list of recipes to run
#' @noRd
get_recipes_to_run <- function(recipes_to_run,
                               date_type) {
  if (is.null(recipes_to_run)) {
    switch(date_type,
      "year" = c("R1", "R2"),
      "quarter" = c("R1", "R2"),
      "month" = c("R1", "R2"),
      "week" = c("R1"),
      "day" = c("R1")
    )
  } else {
    recipes_to_run
  }
}

#' Gets the date regex for removing unneeded date features
#'
#' @param date_type year, quarter, month, week, day
#'
#' @return Returns date_regex
#' @noRd
get_date_regex <- function(date_type) {
  date_regex <- switch(date_type,
    "year" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(half)|(quarter)|(month)|(week)|(day)",
    "quarter" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(month)|(week)|(day)",
    "month" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(day)",
    "week" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(day)",
    "day" = "(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)"
  )

  return(date_regex)
}

#' Apply Box‑Cox transformation
#'
#' @param df input data
#'
#' @return Returns a list with `$data` (tibble of transformed data)
#'         and `$diff_info` (tibble with lambda used for Target)
#' @noRd
apply_box_cox <- function(df) {
  final_tbl <- df %>% dplyr::select(Date)

  diff_info <- tibble::tibble(
    Combo           = unique(df$Combo),
    Box_Cox_Lambda  = NA
  )

  # Pre‑compute lambda for Target so Target_Original can reuse it
  target_lambda <- NA
  if ("Target" %in% names(df) &&
    is.numeric(df$Target) &&
    length(unique(df$Target)) > 2) {
    target_lambda <- timetk::auto_lambda(df$Target)
  }

  for (column_name in names(df)) {
    # Only check numeric columns with more than 2 unique values
    if (is.numeric(df[[column_name]]) & length(unique(df[[column_name]])) > 2) {
      temp_tbl <- df %>%
        dplyr::select(Date, tidyselect::all_of(column_name)) %>%
        dplyr::rename(Column = tidyselect::all_of(column_name))

      # Choose lambda
      lambda_value <- timetk::auto_lambda(temp_tbl$Column)

      # Force Target and Target_Original to share the same lambda
      if (column_name %in% c("Target", "Target_Original") && !is.na(target_lambda)) {
        lambda_value <- target_lambda
      }

      # Record lambda once for Target
      if (column_name == "Target") {
        diff_info <- diff_info %>%
          dplyr::mutate(Box_Cox_Lambda = lambda_value)
      }

      # Apply Box‑Cox
      temp_tbl <- temp_tbl %>%
        dplyr::mutate(Column = timetk::box_cox_vec(
          Column,
          lambda = lambda_value,
          silent = TRUE
        ))

      # Clean up names and add to final df
      colnames(temp_tbl)[colnames(temp_tbl) == "Column"] <- column_name

      final_tbl <- cbind(
        final_tbl,
        temp_tbl %>% dplyr::select(tidyselect::all_of(column_name))
      )
    } else {
      if (column_name != "Date") {
        final_tbl <- cbind(
          final_tbl,
          df %>% dplyr::select(tidyselect::all_of(column_name))
        )
      }
    }
  }

  return(list(data = tibble::tibble(final_tbl), diff_info = diff_info))
}


#' Make data stationary
#'
#' @param data input data
#'
#' @return Returns df of differenced data
#' @noRd
make_stationary <- function(df) {
  final_tbl <- df %>% dplyr::select(Date)

  diff_info <- tibble::tibble(
    Combo       = unique(df$Combo),
    Diff_Value1 = NA,
    Diff_Value2 = NA
  )
  if ("Target_Original" %in% names(df)) {
    diff_info$Target_Original_Diff_Value1 <- NA_real_
    diff_info$Target_Original_Diff_Value2 <- NA_real_
  }

  # store the number of differences used for Target so we can reuse for Target_Original
  target_ndiffs <- NA

  for (column_name in names(df)) {
    # Only check numeric columns with more than 2 unique values
    if (is.numeric(df[[column_name]]) & length(unique(df[[column_name]])) > 2) {
      temp_tbl <- df %>%
        dplyr::select(Date, tidyselect::all_of(column_name)) %>%
        dplyr::rename(Column = tidyselect::all_of(column_name))

      ndiffs <- temp_tbl %>%
        dplyr::pull(Column) %>%
        feasts::unitroot_ndiffs() %>%
        as.numeric()

      # Handle Target and remember its differencing order
      if (column_name == "Target") {
        target_ndiffs <- ndiffs # remember it

        if (ndiffs > 0) {
          diff_info <- diff_info %>%
            dplyr::mutate(Diff_Value1 = temp_tbl %>% dplyr::slice(1) %>% dplyr::pull(Column))

          if (ndiffs > 1) {
            diff_info <- diff_info %>%
              dplyr::mutate(Diff_Value2 = temp_tbl %>% dplyr::slice(2) %>% dplyr::pull(Column))
          }
        }
      }

      # If Target_Original exists, force it to use target_ndiffs
      if (column_name == "Target_Original" && !is.na(target_ndiffs)) {
        ndiffs <- target_ndiffs
        if (ndiffs > 0) {
          diff_info$Target_Original_Diff_Value1 <- temp_tbl$Column[1]
          if (ndiffs > 1) {
            diff_info$Target_Original_Diff_Value2 <- temp_tbl$Column[2]
          }
        }
      }

      if (ndiffs > 0) {
        temp_tbl <- temp_tbl %>%
          dplyr::mutate(Column = timetk::diff_vec(Column,
            difference = ndiffs,
            silent = TRUE
          ))
      }

      colnames(temp_tbl)[colnames(temp_tbl) == "Column"] <- column_name

      final_tbl <- cbind(
        final_tbl,
        temp_tbl %>% dplyr::select(tidyselect::all_of(column_name))
      )
    } else {
      if (column_name != "Date") {
        final_tbl <- cbind(final_tbl, df %>% dplyr::select(tidyselect::all_of(column_name)))
      }
    }
  }

  return(list(data = tibble::tibble(final_tbl), diff_info = diff_info))
}

#' Function to perform feature engineering according to R1 recipe
#'
#' @param data data frame
#' @param external_regressors list of external regressors
#' @param xregs_future_values_list list of external regressors that contain future values
#' @param fourier_periods list of fourier periods
#' @param lag_periods list of lag periods
#' @param rolling_window_periods list of rolling window periods
#' @param hist_end_date hist end date
#' @param date_type date_type
#' @param xreg_raw_df data frame of raw external regressor values for use in foundation models
#'
#' @return tbl with R1 feature engineering applied
#' @noRd
multivariate_prep_recipe_1 <- function(data,
                                       external_regressors,
                                       xregs_future_values_list,
                                       fourier_periods,
                                       lag_periods,
                                       rolling_window_periods,
                                       hist_end_date,
                                       date_type,
                                       xreg_raw_df = NULL) {
  # apply polynomial transformations
  numeric_xregs <- c()

  df_poly <- data

  for (column in c("Target", external_regressors)) {
    if (is.numeric(dplyr::select(data, tidyselect::all_of(column))[[1]])) {
      column_names_final <- c(column)

      if ((column %in% external_regressors) & !(column %in% xregs_future_values_list)) {
        numeric_xregs <- c(numeric_xregs, stringr::str_c(column, c("", "_squared", "_cubed", "_log")))
        column_names_final <- stringr::str_c(column, c("", "_squared", "_cubed", "_log"))
      }

      if (column %in% external_regressors) {
        df_poly_column <- data %>%
          dplyr::select(tidyselect::all_of(column)) %>%
          dplyr::rename(Col = tidyselect::all_of(column))

        temp_squared <- df_poly_column^2
        temp_cubed <- df_poly_column^3
        temp_log <- log1p(df_poly_column %>% dplyr::mutate(Col = ifelse(Col < 0, 0, Col)))

        temp_final <- cbind(temp_squared, temp_cubed, temp_log)
        colnames(temp_final) <- c(paste0(column, "_squared"), paste0(column, "_cubed"), paste0(column, "_log"))

        df_poly <- cbind(df_poly, temp_final)
      }
    }
  }

  # add lags, rolling window calcs, and fourier periods
  if (!is.null(xregs_future_values_list)) {
    # create lags for xregs with future values
    df_lag_initial <- df_poly %>%
      timetk::tk_augment_lags(tidyselect::contains(xregs_future_values_list), .lags = get_lag_periods(NULL, date_type, 1))
  } else {
    df_lag_initial <- df_poly
  }

  data_lag_window <- df_lag_initial %>%
    timetk::tk_augment_lags(tidyselect::contains(c("Target", setdiff(external_regressors, xregs_future_values_list))), .lags = lag_periods) %>% # create standard lags
    dplyr::select(-tidyselect::any_of(stringr::str_c("Target_Original_lag", lag_periods))) %>% # drop original target cols if found
    tidyr::fill(tidyselect::contains(c("Target", external_regressors)), .direction = "up") %>%
    timetk::tk_augment_slidify( # create rolling windows
      tidyselect::any_of(stringr::str_c("Target_lag", lag_periods)),
      .f = ~ mean(.x, na.rm = TRUE),
      .period = rolling_window_periods,
      .partial = TRUE,
      .align = "right",
      .names = lapply(rolling_window_periods,
        FUN = function(x) {
          stringr::str_c(stringr::str_c("Target_lag", lag_periods, "_roll"), x, "_Avg")
        }
      ) %>%
        unlist()
    ) %>%
    timetk::tk_augment_slidify(
      tidyselect::any_of(stringr::str_c("Target_lag", lag_periods)),
      .f = ~ sum(.x, na.rm = TRUE),
      .period = rolling_window_periods,
      .partial = TRUE,
      .align = "right",
      .names = lapply(rolling_window_periods,
        FUN = function(x) {
          stringr::str_c(stringr::str_c("Target_lag", lag_periods, "_roll"), x, "_Sum")
        }
      ) %>%
        unlist()
    ) %>%
    timetk::tk_augment_slidify(
      tidyselect::any_of(stringr::str_c("Target_lag", lag_periods)),
      .f = ~ sd(.x, na.rm = TRUE),
      .period = rolling_window_periods,
      .partial = TRUE,
      .align = "right",
      .names = lapply(rolling_window_periods,
        FUN = function(x) {
          stringr::str_c(stringr::str_c("Target_lag", lag_periods, "_roll"), x, "_StdDev")
        }
      ) %>%
        unlist()
    ) %>%
    timetk::tk_augment_fourier(Date, .periods = fourier_periods, .K = 2) %>% # add fourier series
    tidyr::fill(tidyselect::contains("_roll"), .direction = "down") %>%
    dplyr::select(-tidyselect::all_of(numeric_xregs))


  is.na(data_lag_window) <- sapply(
    data_lag_window,
    is.infinite
  )

  is.na(data_lag_window) <- sapply(data_lag_window, is.nan)
  data_lag_window[is.na(data_lag_window)] <- 0.00

  if (!is.null(xreg_raw_df) && !is.null(external_regressors) && length(external_regressors) > 0) {
    for (xr in external_regressors) {
      original_col <- paste0(xr, "_original")
      if (xr %in% names(xreg_raw_df) && !(original_col %in% names(data_lag_window))) {
        temp_join <- xreg_raw_df %>%
          dplyr::select(Date, tidyselect::all_of(xr)) %>%
          dplyr::rename(!!original_col := !!xr)

        # Only mask future values as NA if this regressor is NOT in the future values list
        if (!(xr %in% xregs_future_values_list)) {
          temp_join <- temp_join %>%
            dplyr::mutate(!!original_col := ifelse(Date > hist_end_date, NA, !!rlang::sym(original_col)))
        }

        data_lag_window <- data_lag_window %>%
          dplyr::left_join(temp_join, by = "Date")
      }
    }
  }

  return(data_lag_window)
}

#' Function to perform feature engineering according to R2 recipe
#'
#' @param data data frame
#' @param external_regressors list of external regressors
#' @param xregs_future_values_list list of external regressors that contain future values
#' @param fourier_periods list of fourier periods
#' @param lag_periods list of lag periods
#' @param rolling_window_periods list of rolling window periods
#' @param date_type date type
#' @param forecast_horizon forecast horizon
#' @param hist_end_date hist end date
#'
#' @return tbl with R2 feature engineering applied
#' @noRd
multivariate_prep_recipe_2 <- function(data,
                                       external_regressors,
                                       xregs_future_values_list,
                                       fourier_periods,
                                       lag_periods,
                                       rolling_window_periods,
                                       date_type,
                                       forecast_horizon,
                                       hist_end_date) {
  data_trans <- tibble::tibble()

  # apply polynomial transformations
  numeric_xregs <- c()

  df_poly <- data

  for (column in c("Target", external_regressors)) {
    if (is.numeric(dplyr::select(data, tidyselect::all_of(column))[[1]])) {
      column_names_final <- c(column)

      if ((column %in% external_regressors) & !(column %in% xregs_future_values_list)) {
        numeric_xregs <- c(numeric_xregs, stringr::str_c(column, c("", "_squared", "_cubed", "_log")))
        column_names_final <- stringr::str_c(column, c("", "_squared", "_cubed", "_log"))
      }

      if (column %in% external_regressors) {
        df_poly_column <- data %>%
          dplyr::select(tidyselect::all_of(column)) %>%
          dplyr::rename(Col = tidyselect::all_of(column))

        temp_squared <- df_poly_column^2
        temp_cubed <- df_poly_column^3
        temp_log <- log1p(df_poly_column %>% dplyr::mutate(Col = ifelse(Col < 0, 0, Col)))

        temp_final <- cbind(temp_squared, temp_cubed, temp_log)
        colnames(temp_final) <- c(paste0(column, "_squared"), paste0(column, "_cubed"), paste0(column, "_log"))

        df_poly <- cbind(df_poly, temp_final)
      }
    }
  }

  # add horizon specific features
  if (date_type == "day") {
    lag_periods_r2 <- unique(c(7, 14, 21, 30, 90, 180, 365, forecast_horizon))
  } else {
    lag_periods_r2 <- 1:forecast_horizon
  }

  for (period in 1:forecast_horizon) {
    # add horizon and origin components
    data_lag_window <- df_poly %>%
      dplyr::mutate(
        Horizon = period,
        Origin = dplyr::row_number() - period
      ) %>%
      timetk::tk_augment_lags(tidyselect::contains(c("Target", external_regressors)),
        .lags = unique(c(lag_periods_r2, lag_periods)) + (period - 1),
        .names = expand.grid(
          col = df_poly %>% dplyr::select(tidyselect::contains(c("Target", external_regressors))) %>% colnames(),
          lag_val = unique(c(lag_periods_r2, lag_periods)),
          stringsAsFactors = FALSE
        ) %>%
          dplyr::mutate(Final_Col = paste0(col, "_lag", lag_val)) %>%
          dplyr::pull(Final_Col)
      ) %>%
      dplyr::select(-tidyselect::any_of(stringr::str_c("Target_Original_lag", unique(c(lag_periods_r2, lag_periods))))) %>% # drop original target cols if found
      tidyr::fill(tidyselect::contains(c("Target", external_regressors)), .direction = "up") %>%
      timetk::tk_augment_slidify(
        tidyselect::any_of(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)))),
        .f = ~ mean(.x, na.rm = TRUE),
        .period = rolling_window_periods,
        .partial = TRUE,
        .align = "right",
        .names = lapply(rolling_window_periods,
          FUN = function(x) {
            stringr::str_c(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)), "_roll"), x, "_Avg")
          }
        ) %>%
          unlist()
      ) %>%
      timetk::tk_augment_slidify(
        tidyselect::any_of(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)))),
        .f = ~ sum(.x, na.rm = TRUE),
        .period = rolling_window_periods,
        .partial = TRUE,
        .align = "right",
        .names = lapply(rolling_window_periods,
          FUN = function(x) {
            stringr::str_c(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)), "_roll"), x, "_Sum")
          }
        ) %>%
          unlist()
      ) %>%
      timetk::tk_augment_slidify(
        tidyselect::any_of(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)))),
        .f = ~ sd(.x, na.rm = TRUE),
        .period = rolling_window_periods,
        .partial = TRUE,
        .align = "right",
        .names = lapply(rolling_window_periods,
          FUN = function(x) {
            stringr::str_c(stringr::str_c("Target_lag", unique(c(lag_periods_r2, lag_periods)), "_roll"), x, "_StdDev")
          }
        ) %>%
          unlist()
      ) %>%
      tidyr::fill(tidyselect::contains("_roll"), .direction = "down") %>%
      timetk::tk_augment_fourier(Date, .periods = fourier_periods, .K = 2) %>% # add fourier series
      dplyr::select(-tidyselect::all_of(numeric_xregs)) # drop xregs that do not contain future values

    is.na(data_lag_window) <- sapply(
      data_lag_window,
      is.infinite
    )

    is.na(data_lag_window) <- sapply(data_lag_window, is.nan)
    data_lag_window[is.na(data_lag_window)] <- 0.00

    # combine transformed data
    data_trans <- rbind(data_trans, data_lag_window)
  }

  return(data_trans)
}
