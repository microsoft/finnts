#' Prep Data
#'
#' Preps data with various feature engineering recipes to create features before training models
#'
#' @param run_info Run info using [set_run_info()]
#' @param input_data A standard data frame, tibble, or spark data frame using sparklyr of historical time series data.
#'   Can also include external regressors for both historical and future data.
#' @param combo_variables List of column headers within input data to be used to separate individual time series.
#' @param target_variable The column header formatted as a character value within input data you want to forecast.
#' @param date_type The date granularity of the input data. Finn accepts the following as a character string:
#'   day, week, month, quarter, year.
#' @param forecast_horizon Number of periods to forecast into the future.
#' @param external_regressors List of column headers within input data to be used as features in multivariate models.
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
#' @param forecast_approach How the forecast is created. The default of 'bottoms_up' trains models for each individual
#'   time series. Value of 'grouped_hierarchy' creates a grouped time series to forecast at while 'standard_hierarchy' creates
#'   a more traditional hierarchical time series to forecast, both based on the hts package.
#' @param parallel_processing Default of NULL runs no parallel processing and forecasts each individual time series
#'   one after another. Value of 'local_machine' leverages all cores on current machine Finn is running on.
#'   Value of 'azure_batch' runs time series in parallel on a remote compute cluster in Azure Batch. Value of 'spark'
#'   runs time series in parallel on a spark cluster in Azure Databricks/Synapse.
#' @param num_cores Number of cores to run when parallel processing is set up. Used when running parallel computations
#'   on local machine or within Azure. Default of NULL uses total amount of cores on machine minus one. Can't be greater
#'   than number of cores on machine minus 1.
#' @param target_log_transformation If TRUE, log transform target variable before training models.
#' @param fourier_periods List of values to use in creating fourier series as features. Default of NULL automatically chooses
#'   these values based on the date_type.
#' @param lag_periods List of values to use in creating lag features. Default of NULL automatically chooses these values
#'   based on date_type.
#' @param rolling_window_periods List of values to use in creating rolling window features. Default of NULL automatically
#'   chooses these values based on date_type.
#' @param recipes_to_run List of recipes to run on multivariate models that can run different recipes. A value of NULL runs
#'   all recipes, but only runs the R1 recipe for weekly and daily date types. A value of "all" runs all recipes, regardless
#'   of date type. A list like c("R1") or c("R2") would only run models with the R1 or R2 recipe.
#'
#' @return No return object. Feature engineered data is written to disk based on the output locations provided in
#'   [set_run_info()].
#' @examples
#' \donttest{
#' data_tbl <- timetk::m4_monthly %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id)) %>%
#'   dplyr::filter(
#'     Date >= "2012-01-01",
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
#'   forecast_horizon = 3
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
                      forecast_approach = "bottoms_up",
                      parallel_processing = NULL,
                      num_cores = NULL,
                      target_log_transformation = FALSE,
                      fourier_periods = NULL,
                      lag_periods = NULL,
                      rolling_window_periods = NULL,
                      recipes_to_run = NULL) {
  cli::cli_progress_step("Prepping Data")

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
  check_input_type("forecast_approach", forecast_approach, "character", c("bottoms_up", "grouped_hierarchy", "standard_hierarchy"))
  check_input_type("parallel_processing", parallel_processing, c("character", "NULL"), c("NULL", "local_machine", "spark"))
  check_input_type("num_cores", num_cores, c("numeric", "NULL"))
  check_input_type("target_log_transformation", target_log_transformation, "logical")
  check_input_type("fourier_periods", fourier_periods, c("list", "numeric", "NULL"))
  check_input_type("lag_periods", lag_periods, c("list", "numeric", "NULL"))
  check_input_type("rolling_window_periods", rolling_window_periods, c("list", "numeric", "NULL"))
  check_input_type("recipes_to_run", recipes_to_run, c("list", "character", "NULL"), c("R1", "R2"))
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

  # prep initial data before feature engineering
  initial_prep_tbl <- input_data %>%
    tidyr::unite("Combo",
      combo_variables,
      sep = "--",
      remove = F
    ) %>%
    dplyr::rename("Target" = target_variable) %>%
    dplyr::select(c(
      "Combo",
      tidyselect::all_of(combo_variables),
      tidyselect::all_of(external_regressors),
      "Date", "Target"
    )) %>%
    combo_cleanup_fn(combo_cleanup_date) %>%
    prep_hierarchical_data(run_info,
      combo_variables,
      forecast_approach,
      frequency_number = get_frequency_number(date_type)
    )

  # check if a previous run already has necessary outputs
  prev_combo_list <- list_files(
    run_info$storage_object,
    paste0(
      run_info$path, "/prep_data/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*R*.", run_info$data_output
    )
  ) %>%
    tibble::tibble(
      Path = .
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(File = ifelse(is.null(Path), "NA", fs::path_file(Path))) %>%
    dplyr::ungroup() %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Recipe"), sep = "-", remove = TRUE) %>%
    dplyr::pull(Combo) %>%
    unique() %>%
    suppressWarnings()

  current_combo_list <- initial_prep_tbl %>%
    dplyr::select(Combo) %>%
    dplyr::distinct(Combo) %>%
    dplyr::collect() %>%
    dplyr::distinct(Combo) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Combo_Hash = hash_data(Combo)) %>%
    dplyr::ungroup() %>%
    suppressWarnings()

  combo_diff <- setdiff(
    current_combo_list %>%
      dplyr::pull(Combo_Hash) %>%
      unique(),
    prev_combo_list
  )

  current_combo_list_final <- current_combo_list %>%
    dplyr::filter(Combo_Hash %in% combo_diff) %>%
    dplyr::pull(Combo)

  filtered_initial_prep_tbl <- initial_prep_tbl %>% # filter input data on combos that haven't completed running
    dplyr::filter(Combo %in% current_combo_list_final)

  if (length(combo_diff) == 0 & length(prev_combo_list) > 0) {

    # check if input values have changed
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
      parallel_processing = ifelse(is.null(parallel_processing), NA, parallel_processing),
      num_cores = ifelse(is.null(num_cores), NA, num_cores),
      target_log_transformation = target_log_transformation,
      fourier_periods = ifelse(is.null(fourier_periods), NA, paste(fourier_periods, collapse = "---")),
      lag_periods = ifelse(is.null(lag_periods), NA, paste(lag_periods, collapse = "---")),
      rolling_window_periods = ifelse(is.null(rolling_window_periods), NA, paste(rolling_window_periods, collapse = "---")),
      recipes_to_run = ifelse(is.null(recipes_to_run), NA, paste(recipes_to_run, collapse = "---"))
    ) %>%
      data.frame()

    prev_log_df <- read_file(run_info,
      path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
      return_type = "df"
    ) %>%
      dplyr::select(colnames(current_log_df)) %>%
      data.frame()

    if (hash_data(current_log_df) == hash_data(prev_log_df)) {
      cli::cli_alert_info("Data Already Prepped")
      return(cli::cli_progress_done())
    } else {
      stop("Inputs have recently changed in 'prep_data', please revert back to original inputs or start a new run with 'set_run_info'",
        call. = FALSE
      )
    }
  }

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
        combo <- x %>%
          dplyr::pull(Combo)

        initial_prep_combo_tbl <- filtered_initial_prep_tbl %>%
          dplyr::filter(Combo == combo) %>%
          dplyr::collect()

        xregs_future_tbl <- get_xregs_future_values_tbl(
          initial_prep_combo_tbl,
          external_regressors,
          hist_end_date,
          forecast_approach
        )

        if (length(colnames(xregs_future_tbl)) > 2) {
          xregs_future_list <- xregs_future_tbl %>%
            dplyr::select(-Date, -Combo) %>%
            colnames()
        } else {
          xregs_future_list <- NULL
        }

        initial_tbl <- initial_prep_combo_tbl %>%
          dplyr::filter(Combo == combo) %>%
          dplyr::select(
            Combo,
            Date,
            Target,
            tidyselect::all_of(setdiff(external_regressors, xregs_future_list))
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
          ) %>% # join xregs that contain future values given by user
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
        if (is.null(recipes_to_run)) {
          run_all_recipes_override <- FALSE
        } else if ("all" %in% recipes_to_run) {
          run_all_recipes_override <- TRUE
        } else {
          run_all_recipes_override <- FALSE
        }

        if (is.null(recipes_to_run) | "R1" %in% recipes_to_run | run_all_recipes_override) {
          R1 <- initial_tbl %>%
            multivariate_prep_recipe_1(external_regressors,
              xregs_future_values_list = xregs_future_list,
              get_fourier_periods(fourier_periods, date_type),
              get_lag_periods(lag_periods, date_type, forecast_horizon),
              get_rolling_window_periods(rolling_window_periods, date_type),
              hist_end_date,
              date_type
            ) %>%
            dplyr::mutate(Target = base::ifelse(Date > hist_end_date, NA, Target))

          write_data(
            x = R1,
            combo = combo,
            run_info = run_info,
            output_type = "data",
            folder = "prep_data",
            suffix = "-R1"
          )
        }

        if ((is.null(recipes_to_run) & date_type %in% c("month", "quarter", "year")) | "R2" %in% recipes_to_run | run_all_recipes_override) {
          R2 <- initial_tbl %>%
            multivariate_prep_recipe_2(external_regressors,
              xregs_future_values_list = xregs_future_list,
              get_fourier_periods(fourier_periods, date_type),
              get_lag_periods(lag_periods, date_type, forecast_horizon),
              get_rolling_window_periods(rolling_window_periods, date_type),
              date_type,
              forecast_horizon,
              hist_end_date
            ) %>%
            dplyr::mutate(Target = base::ifelse(Date > hist_end_date, NA, Target))

          write_data(
            x = R2,
            combo = combo,
            run_info = run_info,
            output_type = "data",
            folder = "prep_data",
            suffix = "-R2"
          )
        }
        return()
      } %>%
      base::suppressPackageStartupMessages()

    # clean up any parallel run process
    par_end(cl)
  } else if (parallel_processing == "spark") {
    # print(filtered_initial_prep_tbl) # prevents spark tbl errors
    final_data <- filtered_initial_prep_tbl %>%
      adjust_df(return_type = "sdf") %>%
      sparklyr::spark_apply(function(df, context) {
        for (name in names(context)) {
          assign(name, context[[name]], envir = .GlobalEnv)
        }

        combo <- unique(df$Combo)

        xregs_future_tbl <- get_xregs_future_values_tbl(
          df,
          external_regressors,
          hist_end_date,
          forecast_approach
        )

        if (length(colnames(xregs_future_tbl)) > 2) {
          xregs_future_list <- xregs_future_tbl %>%
            dplyr::select(-Date, -Combo) %>%
            colnames()
        } else {
          xregs_future_list <- NULL
        }

        initial_tbl <- df %>%
          dplyr::filter(Combo == combo) %>%
          dplyr::select(
            Combo,
            Date,
            Target,
            tidyselect::all_of(external_regressors)
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
          ) %>% # join xregs that contain values given by user
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
        if (is.null(recipes_to_run)) {
          run_all_recipes_override <- FALSE
        } else if (recipes_to_run == "all") {
          run_all_recipes_override <- TRUE
        } else {
          run_all_recipes_override <- FALSE
        }

        if (is.null(recipes_to_run) | "R1" %in% recipes_to_run | run_all_recipes_override) {
          R1 <- initial_tbl %>%
            multivariate_prep_recipe_1(external_regressors,
              xregs_future_values_list = xregs_future_list,
              get_fourier_periods(fourier_periods, date_type),
              get_lag_periods(lag_periods, date_type, forecast_horizon),
              get_rolling_window_periods(rolling_window_periods, date_type)
            ) %>%
            dplyr::mutate(Target = base::ifelse(Date > hist_end_date, NA, Target))

          write_data(
            x = R1,
            combo = combo,
            run_info = run_info,
            output_type = "data",
            folder = "prep_data",
            suffix = "-R1"
          )
        }

        if ((is.null(recipes_to_run) & date_type %in% c("month", "quarter", "year")) | "R2" %in% recipes_to_run | run_all_recipes_override) {
          R2 <- initial_tbl %>%
            multivariate_prep_recipe_2(external_regressors,
              xregs_future_values_list = xregs_future_list,
              get_fourier_periods(fourier_periods, date_type),
              get_lag_periods(lag_periods, date_type, forecast_horizon),
              get_rolling_window_periods(rolling_window_periods, date_type),
              date_type,
              forecast_horizon
            ) %>%
            dplyr::mutate(Target = base::ifelse(Date > hist_end_date, NA, Target))

          write_data(
            x = R2,
            combo = combo,
            run_info = run_info,
            output_type = "data",
            folder = "prep_data",
            suffix = "-R2"
          )
        }

        return(data.frame(Combo = combo))
      },
      group_by = "Combo",
      context = list(
        get_xregs_future_values_tbl = get_xregs_future_values_tbl,
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
        write_data_type = write_data_type
      )
      )
  }

  # update logging file
  log_df <- read_file(run_info,
    path = paste0("logs/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), ".csv"),
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
      forecast_approach = forecast_approach,
      parallel_processing = ifelse(is.null(parallel_processing), NA, parallel_processing),
      num_cores = ifelse(is.null(num_cores), NA, num_cores),
      target_log_transformation = target_log_transformation,
      fourier_periods = ifelse(is.null(fourier_periods), NA, paste(fourier_periods, collapse = "---")),
      lag_periods = ifelse(is.null(lag_periods), NA, paste(lag_periods, collapse = "---")),
      rolling_window_periods = ifelse(is.null(rolling_window_periods), NA, paste(rolling_window_periods, collapse = "---")),
      recipes_to_run = ifelse(is.null(recipes_to_run), NA, paste(recipes_to_run, collapse = "---"))
    )

  write_data(
    x = log_df,
    combo = NULL,
    run_info = run_info,
    output_type = "log",
    folder = "logs",
    suffix = NULL
  )
}

#' Function to perform log transformation
#'
#' @param df data frame
#' @param target_log_transformation variable to indicate log transformation
#'
#' @return tbl with or without log transformation
#' @noRd
get_log_transformation <- function(df,
                                   target_log_transformation) {
  if (target_log_transformation) {
    df %>%
      dplyr::mutate(
        Target = log1p(Target),
        Target = ifelse(is.nan(Target), 0, Target)
      )
  } else {
    df
  }
}

#' Function to remove time series that don't contain recent values
#'
#' @param df data frame
#' @param combo_cleanup_date date value to test for non-zero values after
#'
#' @return tbl with or without specific time series removed
#' @noRd
combo_cleanup_fn <- function(df,
                             combo_cleanup_date) {
  if (!is.null(combo_cleanup_date)) {
    combo_df <- df %>%
      dplyr::filter(Date >= combo_cleanup_date) %>%
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
#' @param forecast_approach indicates what type of hierarchical time series method
#'
#' @return tbl with external regressors with future values
#' @noRd
get_xregs_future_values_tbl <- function(data_tbl,
                                        external_regressors,
                                        hist_end_date,
                                        forecast_approach) {
  if (forecast_approach != "bottoms_up") {
    data_tbl %>%
      tibble::tibble() %>%
      dplyr::select(Combo, Date)
  } else {
    xregs_future_values_list <- c()

    for (variable in external_regressors) {
      temp <- data_tbl %>%
        dplyr::filter(Date > hist_end_date) %>%
        dplyr::select(variable) %>%
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
}

#' Function to replace outliers and fill in missing values
#'
#' @param df data frame
#' @param clean_outliers clean outliers or not
#' @param clean_missing_values clean missing values or not
#' @param frequency_number number of time series frequency
#' @param external_regressors list of external regressors
#'
#' @return tbl with or without missing/outlier values replaced
#' @noRd
clean_outliers_missing_values <- function(df,
                                          clean_outliers,
                                          clean_missing_values,
                                          frequency_number,
                                          external_regressors) {
  correct_clean_func <- function(col) {
    if (clean_missing_values & sum(!is.na(col)) < 2) {
      col
    } else if (clean_outliers) {
      timetk::ts_clean_vec(col, period = frequency_number)
    } else if (clean_missing_values) {
      timetk::ts_impute_vec(col, period = frequency_number)
    } else {
      col
    }
  }

  df %>%
    dplyr::mutate(
      dplyr::across(
        (where(is.numeric) & c("Target", external_regressors)),
        correct_clean_func
      )
    ) %>%
    tibble::tibble()
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
#'
#' @return list of lag periods
#' @noRd
get_lag_periods <- function(lag_periods,
                            date_type,
                            forecast_horizon) {
  if (!is.null(lag_periods)) {
    return(lag_periods)
  }
  oplist <- switch(date_type,
    "year" = c(1, 2, 3),
    "quarter" = c(1, 2, 3, 4),
    "month" = c(1, 2, 3, 6, 9, 12),
    "week" = c(1, 2, 3, 4, 8, 12, 24, 48, 52),
    "day" = c(7, 14, 21, 28, 60, 90, 180, 365)
  )

  oplist <- c(oplist, forecast_horizon)
  lag_periods <- oplist[oplist >= forecast_horizon]
  lag_periods <- unique(lag_periods)

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
                                       date_type) {

  # apply polynomial transformations
  numeric_xregs <- c()

  df_poly <- data

  for (column in c("Target", external_regressors)) {
    if (is.numeric(dplyr::select(data, column)[[1]])) {
      column_names_final <- c(column)

      if ((column %in% external_regressors) & !(column %in% xregs_future_values_list)) {
        numeric_xregs <- c(numeric_xregs, stringr::str_c(column, c("", "_squared", "_cubed", "_log")))
        column_names_final <- stringr::str_c(column, c("", "_squared", "_cubed", "_log"))
      }

      if (column %in% external_regressors) {
        df_poly_column <- data %>%
          dplyr::select(column) %>%
          dplyr::rename(Col = column)

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
    dplyr::select(-numeric_xregs)

  is.na(data_lag_window) <- sapply(
    data_lag_window,
    is.infinite
  )

  is.na(data_lag_window) <- sapply(data_lag_window, is.nan)
  data_lag_window[is.na(data_lag_window)] <- 0.00

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
    if (is.numeric(dplyr::select(data, column)[[1]])) {
      column_names_final <- c(column)

      if ((column %in% external_regressors) & !(column %in% xregs_future_values_list)) {
        numeric_xregs <- c(numeric_xregs, stringr::str_c(column, c("", "_squared", "_cubed", "_log")))
        column_names_final <- stringr::str_c(column, c("", "_squared", "_cubed", "_log"))
      }

      if (column %in% external_regressors) {
        df_poly_column <- data %>%
          dplyr::select(column) %>%
          dplyr::rename(Col = column)

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
      dplyr::select(-numeric_xregs) # drop xregs that do not contain future values

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
