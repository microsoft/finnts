#' Prep hierarchical data
#'
#' @param input_data initial historical data
#' @param run_info run info
#' @param combo_variables combo variables
#' @param external_regressors external regressors
#' @param forecast_approach whether it's a bottoms up or hierarchical forecast
#' @param frequency_number frequency of time series
#'
#' @return data aggregated to the correct data hierarchies
#' @noRd
prep_hierarchical_data <- function(input_data,
                                   run_info,
                                   combo_variables,
                                   external_regressors,
                                   forecast_approach,
                                   frequency_number) {
  if (forecast_approach == "bottoms_up") {
    return(input_data)
  }

  df_return_type <- ifelse(inherits(input_data, "tbl_spark"), "sdf", "df")

  # initial data prep
  input_data_adj <- input_data %>%
    adjust_df()

  combo_tbl <- input_data_adj %>%
    dplyr::select(tidyselect::all_of(combo_variables)) %>%
    dplyr::distinct()

  hts_nodes <- combo_tbl %>%
    pick_right_hierarchy(
      combo_variables,
      forecast_approach
    )

  bottom_level_tbl <- input_data_adj %>%
    dplyr::select(Combo, Date, Target) %>%
    tidyr::pivot_wider(
      names_from = Combo,
      values_from = Target
    ) %>%
    dplyr::mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>%
    base::suppressWarnings()

  # create aggregations for target variable
  hierarchical_tbl <- sum_hts_data(
    bottom_level_tbl,
    hts_nodes,
    "Target",
    forecast_approach,
    frequency_number
  )

  # create aggregations for external regressors
  if (!is.null(external_regressors)) {
    regressor_mapping <- external_regressor_mapping(
      input_data_adj,
      combo_variables,
      external_regressors
    )

    regressor_agg <- foreach::foreach(
      regressor_tbl = regressor_mapping %>%
        dplyr::group_split(dplyr::row_number(), .keep = FALSE),
      .combine = "rbind",
      .errorhandling = "stop",
      .verbose = FALSE,
      .inorder = FALSE,
      .multicombine = TRUE,
      .noexport = NULL
    ) %do% {
      regressor_var <- regressor_tbl$Regressor
      value_level <- regressor_tbl$Var

      if (value_level == "Global") {
        temp_tbl <- input_data_adj %>%
          dplyr::select(Date, tidyselect::all_of(regressor_var)) %>%
          dplyr::distinct()

        hierarchical_tbl <- hierarchical_tbl %>%
          dplyr::left_join(temp_tbl, by = c("Date"))
      } else if (value_level != "All") {
        # agg by lowest level
        bottom_tbl <- input_data_adj %>%
          tidyr::unite("Combo",
            tidyselect::all_of(combo_variables),
            sep = "_",
            remove = F
          ) %>%
          dplyr::select(Date, Combo, tidyselect::all_of(regressor_var)) %>%
          dplyr::mutate(Combo = snakecase::to_any_case(Combo, case = "none"))

        bottom_combos <- unique(bottom_tbl$Combo)

        hier_temp_tbl_1 <- hierarchical_tbl %>%
          dplyr::select(Combo, Date) %>%
          dplyr::filter(Combo %in% bottom_combos) %>%
          dplyr::left_join(bottom_tbl, by = c("Combo", "Date"))

        # agg by specific combo variable level
        value_level <- strsplit(value_level, split = "---")[[1]]

        hier_temp_tbl_2 <- foreach::foreach(
          value_level_iter = value_level,
          .combine = "rbind",
          .errorhandling = "stop",
          .verbose = FALSE,
          .inorder = FALSE,
          .multicombine = TRUE,
          .noexport = NULL
        ) %do% {
          temp_tbl <- input_data_adj %>%
            tidyr::drop_na(tidyselect::all_of(regressor_var)) %>%
            dplyr::select(Date, tidyselect::all_of(value_level_iter), tidyselect::all_of(regressor_var)) %>%
            dplyr::distinct()

          if (length(value_level) > 1) {
            temp_tbl <- temp_tbl %>%
              dplyr::group_by(dplyr::across(tidyselect::all_of(c("Date", value_level_iter)))) %>%
              dplyr::summarise(Value = sum(.data[[regressor_var]], na.rm = TRUE)) %>%
              dplyr::ungroup()

            names(temp_tbl)[names(temp_tbl) == "Value"] <- regressor_var
          }

          colnames(temp_tbl) <- c("Date", "Combo", regressor_var)

          temp_tbl$Combo <- paste0(value_level_iter, "_", temp_tbl$Combo)

          temp_tbl <- temp_tbl %>%
            dplyr::mutate(Combo = snakecase::to_any_case(Combo, case = "none"))

          temp_combos <- unique(temp_tbl$Combo)

          hier_temp_tbl <- hierarchical_tbl %>%
            dplyr::select(Combo, Date) %>%
            dplyr::filter(Combo %in% temp_combos) %>%
            dplyr::distinct() %>%
            dplyr::left_join(temp_tbl, by = c("Combo", "Date"))

          return(hier_temp_tbl)
        }

        # agg by total
        total_tbl <- input_data_adj %>%
          tidyr::drop_na(tidyselect::all_of(regressor_var)) %>%
          dplyr::select(Date, value_level[[1]], tidyselect::all_of(regressor_var)) %>%
          dplyr::distinct() %>%
          dplyr::group_by(Date) %>%
          dplyr::rename("Agg" = tidyselect::all_of(regressor_var)) %>%
          dplyr::summarise(Agg = sum(Agg, na.rm = TRUE))

        colnames(total_tbl)[colnames(total_tbl) == "Agg"] <- regressor_var

        hier_temp_tbl_3 <- hierarchical_tbl %>%
          dplyr::select(Combo, Date) %>%
          dplyr::filter(Combo %in% setdiff(unique(hierarchical_tbl$Combo), c(unique(hier_temp_tbl_2$Combo), bottom_combos))) %>%
          dplyr::left_join(total_tbl, by = c("Date"))

        # combine together
        hierarchical_tbl <- hierarchical_tbl %>%
          dplyr::left_join(
            rbind(hier_temp_tbl_1, hier_temp_tbl_2, hier_temp_tbl_3),
            by = c("Combo", "Date")
          )
      } else if (value_level == "All") {
        bottom_level_temp_tbl <- input_data_adj %>%
          dplyr::select(Combo, Date, tidyselect::all_of(regressor_var)) %>%
          tidyr::pivot_wider(
            names_from = Combo,
            values_from = tidyselect::all_of(regressor_var)
          ) %>%
          dplyr::mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>%
          base::suppressWarnings()

        temp_tbl <- sum_hts_data(
          bottom_level_temp_tbl,
          hts_nodes,
          regressor_var,
          forecast_approach,
          frequency_number
        )

        hierarchical_tbl <- hierarchical_tbl %>%
          dplyr::left_join(temp_tbl, by = c("Date", "Combo"))
      }
      return(regressor_tbl)
    }
  }

  # write hierarchy structure to disk
  hts_list <- list(
    original_combos = colnames(bottom_level_tbl %>% dplyr::select(-Date)),
    hts_combos = hierarchical_tbl %>% dplyr::pull(Combo) %>% unique(),
    nodes = sum_hts_data(bottom_level_tbl,
      hts_nodes,
      "Target",
      forecast_approach,
      frequency_number,
      return_type = "nodes"
    )
  )

  write_data(
    x = hts_list,
    combo = NULL,
    run_info = run_info,
    output_type = "object",
    folder = "prep_data",
    suffix = "-hts_info"
  )

  # write bottom level data to disk
  write_data(
    x = input_data_adj,
    combo = NULL,
    run_info = run_info,
    output_type = "data",
    folder = "prep_data",
    suffix = "-hts_data"
  )

  return_data <- hierarchical_tbl %>%
    adjust_df(return_type = df_return_type) %>%
    dplyr::select(Combo, Date, Target, tidyselect::any_of(external_regressors))

  return(return_data)
}

#' Return correct data frame format
#'
#' @param input_data initial historical data
#' @param return_type whether it should be returned as standard df or spark df
#'
#' @return data frame
#' @noRd
adjust_df <- function(input_data,
                      return_type = "df") {
  if (return_type == "sdf" & inherits(input_data, c("tbl", "tbl_df", "data.frame"))) {
    input_data <- sparklyr::sdf_copy_to(sc, input_data, overwrite = TRUE) %>%
      sparklyr::sdf_register()
    return(input_data)
  } else if (return_type == "sdf" & inherits(input_data, "tbl_spark")) {
    # sparklyr::tbl_cache(sc, 'input_data', force = TRUE)
    input_data
  } else {
    input_data %>% dplyr::collect()
  }
}

#' Get the righ hierarchical data structure
#'
#' @param input_data initial historical data
#' @param nodes hts info
#' @param forecast_approach forecast approach
#'
#' @return transformed data frame with proper hierarchies
#' @noRd
get_hts <- function(input_data,
                    nodes,
                    forecast_approach) {
  if (forecast_approach == "grouped_hierarchy") {
    input_data %>%
      hts::gts(groups = nodes) %>%
      base::suppressMessages()
  } else {
    input_data %>%
      hts::hts(nodes = nodes) %>%
      base::suppressMessages()
  }
}


#' Get the correct hierarchical structure
#'
#' @param input_data initial historical data
#' @param combo_variables combo variables
#' @param forecast_approach whether it's a bottoms up or hierarchical forecast
#'
#' @return hierarchical structure
#' @noRd
pick_right_hierarchy <- function(input_data,
                                 combo_variables,
                                 forecast_approach) {
  if (forecast_approach == "grouped_hierarchy") {
    input_data %>% get_grouped_nodes(combo_variables)
  } else {
    input_data %>% get_standard_nodes(combo_variables)
  }
}

#' Grouped hierarchy structure
#'
#' @param input_data initial historical data
#' @param combo_variables combo variables
#'
#' @return hierarchy structure
#' @noRd
get_grouped_nodes <- function(input_data,
                              combo_variables) {
  group_list <- vector()

  for (variable in combo_variables) {
    var <- input_data[[variable]]

    group_list <- rbind(group_list, var)
  }
  rownames(group_list) <- combo_variables

  return(group_list)
}

#' Standard hierarchy structure
#'
#' @param input_data initial historical data
#' @param combo_variables combo variables
#'
#' @return hierarchy structure
#' @noRd
get_standard_nodes <- function(input_data,
                               combo_variables) {
  hierarchy_length_tbl <- tibble::tibble()

  node_list <- list()

  num <- 1

  for (variable in combo_variables) {
    hierarchy_length_tbl <- rbind(
      hierarchy_length_tbl,
      tibble::tibble(
        Variable = variable,
        Count = length(unique(input_data[[variable]]))
      )
    )
  }

  hierarchy_combo_variables <- hierarchy_length_tbl %>%
    dplyr::arrange(Count) %>%
    dplyr::select(Variable) %>%
    unlist(use.names = FALSE)

  for (variable in hierarchy_combo_variables) {
    if (num == 1) {
      node_list <- append(node_list, length(unique(input_data[[variable]])))

      num <- num + 1
    } else {
      grouping_current <- variable

      grouping_minus_1 <- hierarchy_combo_variables[num - 1]

      grouping_values <- input_data %>%
        dplyr::mutate(Sum = 1) %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(c(grouping_minus_1, grouping_current)))) %>%
        dplyr::summarise(Sum = sum(Sum, na.rm = TRUE)) %>%
        dplyr::mutate(Sum = 1) %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(grouping_minus_1))) %>%
        dplyr::summarise(Count = sum(Sum)) %>%
        dplyr::select(Count) %>%
        unlist(use.names = FALSE)

      node_list <- append(node_list, list(grouping_values))
      num <- num + 1
    }
  }

  return(node_list)
}

#' Get the correct hts nodes
#'
#' @param hts_object hts_object
#' @param forecast_approach forecast approach
#'
#' @return hierarchy structure
#' @noRd
get_hts_nodes <- function(hts_object,
                          forecast_approach) {
  if (forecast_approach == "grouped_hierarchy") {
    suppressMessages(hts_object %>% hts::get_groups())
  } else {
    suppressMessages(hts_object %>% hts::get_nodes())
  }
}

#' Reconcile hierarchical forecasts down to lowest bottoms up level
#'
#' @param run_info run info
#' @param parallel_processing parallel processing
#' @param forecast_approach forecast approach
#' @param negative_forecast negative forecast
#' @param num_cores number of cores for parallel processing
#'
#' @return hierarchy structure
#' @noRd
reconcile_hierarchical_data <- function(run_info,
                                        parallel_processing,
                                        forecast_approach,
                                        negative_forecast = FALSE,
                                        num_cores) {
  # get run splits
  model_train_test_tbl <- read_file(run_info,
    path = paste0(
      "/prep_models/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name),
      "-train_test_split.", run_info$data_output
    ),
    return_type = "df"
  ) %>%
    dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID))

  # get hierarchical info
  hts_list <- read_file(run_info,
    path = paste0("/prep_data/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), "-hts_info.", run_info$object_output),
    return_type = "object"
  )

  hts_nodes <- hts_list$nodes
  original_combo_list <- hts_list$original_combos
  hts_combo_list <- hts_list$hts_combos

  # check if data has been condensed
  cond_path <- paste0(
    run_info$path, "/forecasts/*", hash_data(run_info$experiment_name), "-",
    hash_data(run_info$run_name), "*condensed", ".", run_info$data_output
  )

  condensed_files <- list_files(run_info$storage_object, fs::path(cond_path))

  if (length(condensed_files) > 0) {
    condensed <- TRUE
  } else {
    condensed <- FALSE
  }

  # get unreconciled forecast data
  if (is.null(parallel_processing)) {
    return_type <- "df"
  } else if (parallel_processing == "spark") {
    return_type <- "sdf"
  } else {
    return_type <- "df"
  }

  if (condensed) {
    fcst_path <- paste0(
      "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*condensed", ".", run_info$data_output
    )
  } else {
    fcst_path <- paste0(
      "/forecasts/*", hash_data(run_info$experiment_name), "-",
      hash_data(run_info$run_name), "*models", ".", run_info$data_output
    )
  }

  unreconciled_tbl <- read_file(run_info,
    path = fcst_path,
    return_type = return_type
  ) %>%
    dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID))

  # get models to reconcile down to lowest level
  model_list <- unreconciled_tbl %>%
    dplyr::filter(!is.na(Model_Name) & Model_Name != "NA") %>%
    dplyr::select(Model_ID) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::distinct() %>%
    dplyr::pull(Model_ID) %>%
    c("Best-Model") %>%
    suppressWarnings()

  if (is.null(parallel_processing) || parallel_processing == "local_machine") {
    hist_tbl <- read_file(run_info,
      path = paste0("/prep_data/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), "-hts_data.", run_info$data_output)
    ) %>%
      dplyr::select(Combo, Date, Target)

    # parallel run info
    par_info <- par_start(
      run_info = run_info,
      parallel_processing = parallel_processing,
      num_cores = num_cores,
      task_length = length(model_list)
    )

    cl <- par_info$cl
    packages <- par_info$packages
    `%op%` <- par_info$foreach_operator

    # submit tasks
    reconciled_tbl <- foreach::foreach(
      x = model_list,
      .combine = "rbind",
      .packages = packages,
      .errorhandling = "stop",
      .verbose = FALSE,
      .inorder = FALSE,
      .multicombine = TRUE,
      .noexport = NULL
    ) %op%
      {
        model <- x

        ts_combined <- NULL

        tryCatch(
          {
            if (model == "Best-Model") {
              model_tbl <- unreconciled_tbl %>%
                dplyr::filter(Best_Model == "Yes") %>%
                dplyr::left_join(model_train_test_tbl %>% dplyr::select(Run_Type, Train_Test_ID),
                  by = "Train_Test_ID"
                ) %>%
                dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test"))
            } else {
              model_tbl <- unreconciled_tbl %>%
                dplyr::filter(Model_ID == model) %>%
                dplyr::left_join(model_train_test_tbl %>% dplyr::select(Run_Type, Train_Test_ID),
                  by = "Train_Test_ID"
                ) %>%
                dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test"))
            }

            if (length(unique(model_tbl$Combo)) != length(hts_combo_list)) {
              # add snaive fcst to missing combos to get a full hierarchy of forecasts to reconcile
              snaive_combo_list <- setdiff(hts_combo_list, unique(model_tbl$Combo))

              snaive_tbl <- unreconciled_tbl %>%
                dplyr::filter(Model_Name == "snaive") %>%
                dplyr::left_join(model_train_test_tbl %>% dplyr::select(Run_Type, Train_Test_ID),
                  by = "Train_Test_ID"
                ) %>%
                dplyr::filter(
                  Run_Type %in% c("Future_Forecast", "Back_Test"),
                  Combo %in% snaive_combo_list
                )

              model_tbl <- model_tbl %>%
                rbind(snaive_tbl)
            }

            forecast_tbl <- model_tbl %>%
              dplyr::select(Date, Train_Test_ID, Combo, Forecast) %>%
              dplyr::rowwise() %>%
              dplyr::mutate(Forecast = ifelse(Forecast > 100000000000000, 100000000000000, Forecast)) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(names_from = Combo, values_from = Forecast)

            forecast_tbl[is.na(forecast_tbl)] <- 0

            date_tbl <- forecast_tbl %>%
              dplyr::select(Date, Train_Test_ID)

            ts <- forecast_tbl %>%
              dplyr::select(-Date, -Train_Test_ID) %>%
              dplyr::select(tidyselect::all_of(hts_combo_list)) %>%
              stats::ts()

            residual_multiplier <- 10 # shrink extra large residuals to prevent recon issues

            residuals_tbl <- model_tbl %>%
              dplyr::filter(Run_Type == "Back_Test") %>%
              dplyr::mutate(
                Forecast_Adj = ifelse((abs(Target) + 1) * residual_multiplier < abs(Forecast), (Target + 1) * residual_multiplier, Forecast), # prevent hts recon issues
                Residual = Target - Forecast_Adj
              ) %>%
              dplyr::rowwise() %>%
              dplyr::mutate(Residual = ifelse(Residual == 0, 0.0001, Residual)) %>%
              dplyr::ungroup() %>%
              dplyr::select(Combo, Date, Train_Test_ID, Residual) %>%
              tidyr::pivot_wider(names_from = Combo, values_from = Residual) %>%
              dplyr::select(-Date, -Train_Test_ID) %>%
              dplyr::select(tidyselect::all_of(hts_combo_list)) %>%
              as.matrix()

            if (forecast_approach == "standard_hierarchy") {
              ts_combined <- data.frame(hts::combinef(ts,
                nodes = hts_nodes, weights = (1 / colMeans(residuals_tbl^2, na.rm = TRUE)),
                keep = "bottom", nonnegative = !negative_forecast
              ))
              colnames(ts_combined) <- original_combo_list
            } else if (forecast_approach == "grouped_hierarchy") {
              ts_combined <- data.frame(hts::combinef(ts,
                groups = hts_nodes, weights = (1 / colMeans(residuals_tbl^2, na.rm = TRUE)),
                keep = "bottom", nonnegative = !negative_forecast
              ))
              colnames(ts_combined) <- original_combo_list
            }
          },
          error = function(e) {
            if (model != "Best-Model") {
              warning(paste0("The model '", model, "' was not able to be reconciled, skipping..."),
                call. = FALSE
              )
            } else {
              stop("The 'Best-Model' was not able to be properly reconciled.",
                call. = FALSE
              )
            }
          }
        )

        # return if there was an error in the recon process for non best-model
        if (is.null(ts_combined)) {
          return(tibble::tibble())
        }

        # final transformations before writing to disk
        reconciled_tbl <- ts_combined %>%
          tibble::add_column(
            Train_Test_ID = date_tbl$Train_Test_ID,
            .before = 1
          ) %>%
          tibble::add_column(
            Date = date_tbl$Date,
            .before = 1
          ) %>%
          tidyr::pivot_longer(!c(Date, Train_Test_ID),
            names_to = "Combo",
            values_to = "Forecast"
          ) %>%
          dplyr::left_join(hist_tbl, by = c("Date", "Combo")) %>%
          dplyr::rename(Combo_ID = Combo) %>%
          dplyr::mutate(
            Model_ID = model,
            Best_Model = ifelse(model == "Best-Model", "Yes", "No"),
            Combo = Combo_ID
          ) %>%
          dplyr::group_by(Combo_ID, Model_ID, Train_Test_ID) %>%
          dplyr::mutate(Horizon = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Hyperparameter_ID = NA) %>%
          dplyr::select(Combo_ID, Model_ID, Train_Test_ID, Hyperparameter_ID, Best_Model, Combo, Horizon, Date, Target, Forecast) %>%
          create_prediction_intervals(model_train_test_tbl) %>%
          tidyr::separate(
            col = Model_ID,
            into = c("Model_Name", "Model_Type", "Recipe_ID"),
            remove = FALSE,
            sep = "--"
          ) %>%
          suppressWarnings()

        # write outputs to disk
        write_data(
          x = reconciled_tbl,
          combo = model,
          run_info = run_info,
          output_type = "data",
          folder = "forecasts",
          suffix = "-reconciled"
        )

        return(tibble::tibble())
      } %>%
      base::suppressPackageStartupMessages()

    # clean up any parallel run process
    par_end(cl)
  } else {
    rm(unreconciled_tbl)

    # parallel run info
    par_info <- par_start(
      run_info = run_info,
      parallel_processing = parallel_processing,
      num_cores = num_cores,
      task_length = length(model_list)
    )

    cl <- par_info$cl
    packages <- par_info$packages
    `%op%` <- par_info$foreach_operator

    # submit tasks
    reconciled_tbl <- foreach::foreach(
      x = model_list,
      .combine = "rbind",
      .packages = packages,
      .errorhandling = "stop",
      .verbose = FALSE,
      .inorder = FALSE,
      .multicombine = TRUE,
      .noexport = NULL
    ) %op%
      {
        model <- x

        ts_combined <- NULL

        tryCatch(
          {
            hist_tbl <- read_file(run_info,
              path = paste0("/prep_data/", hash_data(run_info$experiment_name), "-", hash_data(run_info$run_name), "-hts_data.", run_info$data_output)
            ) %>%
              dplyr::select(Combo, Date, Target)

            schema <- arrow::schema(
              arrow::field("Combo_ID", arrow::string()),
              arrow::field("Model_ID", arrow::string()),
              arrow::field("Model_Name", arrow::string()),
              arrow::field("Model_Type", arrow::string()),
              arrow::field("Recipe_ID", arrow::string()),
              arrow::field("Train_Test_ID", arrow::float64()),
              arrow::field("Hyperparameter_ID", arrow::float64()),
              arrow::field("Best_Model", arrow::string()),
              arrow::field("Combo", arrow::string()),
              arrow::field("Horizon", arrow::float64()),
              arrow::field("Date", arrow::date32()),
              arrow::field("Target", arrow::float64()),
              arrow::field("Forecast", arrow::float64()),
              arrow::field("lo_95", arrow::float64()),
              arrow::field("lo_80", arrow::float64()),
              arrow::field("hi_80", arrow::float64()),
              arrow::field("hi_95", arrow::float64())
            )

            unreconciled_tbl <- read_file(run_info,
              path = fcst_path,
              return_type = "arrow",
              schema = schema
            )

            if (model == "Best-Model") {
              model_tbl <- unreconciled_tbl %>%
                dplyr::filter(Best_Model == "Yes") %>%
                dplyr::collect() %>%
                dplyr::left_join(model_train_test_tbl %>% dplyr::select(Run_Type, Train_Test_ID),
                  by = "Train_Test_ID"
                ) %>%
                dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test"))
            } else {
              model_tbl <- unreconciled_tbl %>%
                dplyr::filter(Model_ID == model) %>%
                dplyr::collect() %>%
                dplyr::left_join(model_train_test_tbl %>% dplyr::select(Run_Type, Train_Test_ID),
                  by = "Train_Test_ID"
                ) %>%
                dplyr::filter(Run_Type %in% c("Future_Forecast", "Back_Test"))
            }

            if (length(unique(model_tbl$Combo)) != length(hts_combo_list)) {
              # add snaive fcst to missing combos to get a full hierarchy of forecasts to reconcile
              snaive_combo_list <- setdiff(hts_combo_list, unique(model_tbl$Combo))

              snaive_tbl <- unreconciled_tbl %>%
                dplyr::filter(Model_Name == "snaive") %>%
                dplyr::collect() %>%
                dplyr::left_join(model_train_test_tbl %>% dplyr::select(Run_Type, Train_Test_ID),
                  by = "Train_Test_ID"
                ) %>%
                dplyr::filter(
                  Run_Type %in% c("Future_Forecast", "Back_Test"),
                  Combo %in% snaive_combo_list
                )

              model_tbl <- model_tbl %>%
                rbind(snaive_tbl)
            }

            forecast_tbl <- model_tbl %>%
              dplyr::select(Date, Train_Test_ID, Combo, Forecast) %>%
              tidyr::pivot_wider(names_from = Combo, values_from = Forecast)

            forecast_tbl[is.na(forecast_tbl)] <- 0

            date_tbl <- forecast_tbl %>%
              dplyr::select(Date, Train_Test_ID)

            ts <- forecast_tbl %>%
              tibble::as_tibble() %>%
              dplyr::select(tidyselect::all_of(hts_combo_list)) %>%
              stats::ts()

            residual_multiplier <- 10 # shrink extra large residuals to prevent recon issues

            residuals_tbl <- model_tbl %>%
              dplyr::filter(Run_Type == "Back_Test") %>%
              dplyr::mutate(
                Forecast_Adj = ifelse((abs(Target) + 1) * residual_multiplier < abs(Forecast), (Target + 1) * residual_multiplier, Forecast), # prevent hts recon issues
                Residual = Target - Forecast_Adj
              ) %>%
              dplyr::rowwise() %>%
              dplyr::mutate(Residual = ifelse(Residual == 0, 0.0001, Residual)) %>%
              dplyr::ungroup() %>%
              dplyr::select(Combo, Date, Train_Test_ID, Residual) %>%
              tidyr::pivot_wider(names_from = Combo, values_from = Residual) %>%
              tibble::as_tibble() %>%
              dplyr::select(tidyselect::all_of(hts_combo_list)) %>%
              as.matrix()

            if (forecast_approach == "standard_hierarchy") {
              ts_combined <- data.frame(hts::combinef(ts,
                nodes = hts_nodes, weights = (1 / colMeans(residuals_tbl^2, na.rm = TRUE)),
                keep = "bottom", nonnegative = !negative_forecast
              ))
              colnames(ts_combined) <- original_combo_list
            } else if (forecast_approach == "grouped_hierarchy") {
              ts_combined <- data.frame(hts::combinef(ts,
                groups = hts_nodes, weights = (1 / colMeans(residuals_tbl^2, na.rm = TRUE)),
                keep = "bottom", nonnegative = !negative_forecast
              ))
              colnames(ts_combined) <- original_combo_list
            }
          },
          error = function(e) {
            if (model != "Best-Model") {
              warning(paste0("The model '", model, "' was not able to be reconciled, skipping..."),
                call. = FALSE
              )
            } else {
              stop("The 'Best-Model' was not able to be properly reconciled.",
                call. = FALSE
              )
            }
          }
        )

        # return if there was an error in reconciling a non best-model
        if (is.null(ts_combined)) {
          return(tibble::tibble())
        }

        # final transformations before writing to disk
        reconciled_tbl <- ts_combined %>%
          tibble::add_column(
            Train_Test_ID = date_tbl$Train_Test_ID,
            .before = 1
          ) %>%
          tibble::add_column(
            Date = date_tbl$Date,
            .before = 1
          ) %>%
          tidyr::pivot_longer(!c(Date, Train_Test_ID),
            names_to = "Combo",
            values_to = "Forecast"
          ) %>%
          dplyr::left_join(hist_tbl, by = c("Date", "Combo")) %>%
          dplyr::rename(Combo_ID = Combo) %>%
          dplyr::mutate(
            Model_ID = model,
            Best_Model = ifelse(model == "Best-Model", "Yes", "No"),
            Combo = Combo_ID
          ) %>%
          dplyr::group_by(Combo_ID, Model_ID, Train_Test_ID) %>%
          dplyr::mutate(Horizon = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Hyperparameter_ID = NA) %>%
          dplyr::select(Combo_ID, Model_ID, Train_Test_ID, Hyperparameter_ID, Best_Model, Combo, Horizon, Date, Target, Forecast) %>%
          create_prediction_intervals(model_train_test_tbl) %>%
          tidyr::separate(
            col = Model_ID,
            into = c("Model_Name", "Model_Type", "Recipe_ID"),
            remove = FALSE,
            sep = "--"
          ) %>%
          suppressWarnings()

        # write outputs to disk
        write_data(
          x = reconciled_tbl,
          combo = model,
          run_info = run_info,
          output_type = "data",
          folder = "forecasts",
          suffix = "-reconciled"
        )

        return(tibble::tibble())
      } %>%
      base::suppressPackageStartupMessages()

    # clean up any parallel run process
    par_end(cl)
  }
}

#' Determine how external regressors should be aggregated
#'
#' @param data data
#' @param combo_variables combo variables
#' @param external_regressors external regressors
#'
#' @return data frame of regressor mappings
#' @noRd
external_regressor_mapping <- function(data,
                                       combo_variables,
                                       external_regressors) {
  # create var combinations list
  var_combinations <- tibble::tibble()

  for (number in 2:min(length(combo_variables), 10)) {
    temp <- data.frame(gtools::combinations(v = combo_variables, n = length(combo_variables), r = number))

    temp <- temp %>%
      tidyr::unite(Var_Combo, tidyselect::all_of(colnames(temp)), sep = "---") %>%
      dplyr::select(Var_Combo) %>%
      tibble::tibble()

    var_combinations <- rbind(var_combinations, temp)
  }

  iter_list <- var_combinations %>%
    dplyr::pull(Var_Combo) %>%
    c(combo_variables)

  # get final mapping of regressor to combo var level
  regressor_mapping_tbl <- foreach::foreach(
    regressor = external_regressors,
    .combine = "rbind",
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .noexport = NULL
  ) %do% {
    # get unique values of regressor per combo variable iteration
    var_unique_tbl <- foreach::foreach(
      var = iter_list,
      .combine = "rbind",
      .errorhandling = "stop",
      .verbose = FALSE,
      .inorder = FALSE,
      .multicombine = TRUE,
      .noexport = NULL
    ) %do% {
      var_list <- strsplit(var, split = "---")[[1]]

      if (length(var_list) == length(combo_variables)) {
        var <- "All"
      }

      temp_unique <- data %>%
        tidyr::unite(Unique, tidyselect::all_of(c(var_list, "Date", regressor)), sep = "_") %>%
        dplyr::pull(Unique) %>%
        unique() %>%
        length()

      return(data.frame(Var = var, Unique = temp_unique))
    }

    # determine regressor mappings
    if (length(unique(var_unique_tbl$Unique)) > 1) {
      all_unique <- var_unique_tbl %>%
        dplyr::filter(Var == "All") %>%
        dplyr::pull(Unique)

      regressor_test <- var_unique_tbl %>%
        dplyr::filter(Unique < all_unique) %>%
        dplyr::pull(Var)

      if (length(unique(data$Date)) == data %>%
        dplyr::select(Date, tidyselect::all_of(regressor)) %>%
        dplyr::distinct() %>%
        nrow()) {
        regressor_test <- "Global"
      } else if (length(regressor_test) > 1) {
        combo_unique <- var_unique_tbl %>%
          dplyr::filter(Var %in% combo_variables)

        min_val <- min(unique(combo_unique$Unique))

        regressor_test <- combo_unique %>%
          dplyr::filter(Unique == min_val) %>%
          dplyr::pull(Var)
      }

      if (length(regressor_test) > 1) {
        regressor_test <- paste0(regressor_test, collapse = "---")
      }

      return(data.frame(Regressor = regressor, Var = regressor_test))
    } else {
      return(data.frame(Regressor = regressor, Var = "All"))
    }
  }

  return(regressor_mapping_tbl)
}

#' Create hierarchical aggregations
#'
#' @param bottom_level_tbl bottom level table
#' @param hts_nodes hts nodes
#' @param sum_var column to get aggregated
#' @param forecast_approach forecast approach
#' @param frequency_number frequency number
#' @param return_type return type
#'
#' @return data frame of hierarchical aggregations
#' @noRd
sum_hts_data <- function(bottom_level_tbl,
                         hts_nodes,
                         sum_var,
                         forecast_approach,
                         frequency_number,
                         return_type = "data") {
  # create aggregations for target variable
  Date <- bottom_level_tbl$Date

  hierarchical_object <- bottom_level_tbl %>%
    dplyr::select(-Date) %>%
    stats::ts(frequency = frequency_number) %>%
    get_hts(
      hts_nodes,
      forecast_approach
    )

  hts_nodes_final <- get_hts_nodes(
    hierarchical_object,
    forecast_approach
  )

  if (return_type == "nodes") {
    return(hts_nodes_final)
  }

  hierarchical_tbl <- hierarchical_object %>%
    hts::allts() %>%
    data.frame() %>%
    tibble::add_column(
      Date = Date,
      .before = 1
    ) %>%
    tidyr::pivot_longer(!Date,
      names_to = "Combo",
      values_to = sum_var
    ) %>%
    dplyr::mutate(Combo = snakecase::to_any_case(Combo, case = "none"))

  return(hierarchical_tbl)
}
