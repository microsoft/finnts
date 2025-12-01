#' Get the trained model summaries info for an agent
#'
#' This function retrieves the final summarized model info (hyperparameters, recipe steps, feature importance, etc.) after agent completes its run.
#'
#' @param agent_info Agent info from `set_agent_info()`
#'
#' @return A tibble containing the summarized models for the agent.
#' @examples
#' \dontrun{
#' # load example data
#' hist_data <- timetk::m4_monthly %>%
#'   dplyr::filter(date >= "2013-01-01") %>%
#'   dplyr::rename(Date = date) %>%
#'   dplyr::mutate(id = as.character(id))
#'
#' # set up Finn project
#' project <- set_project_info(
#'   project_name = "Demo_Project",
#'   combo_variables = c("id"),
#'   target_variable = "value",
#'   date_type = "month"
#' )
#'
#' # set up LLM
#' driver_llm <- ellmer::chat_azure_openai(model = "gpt-4o-mini")
#'
#' # set up agent info
#' agent_info <- set_agent_info(
#'   project_info = project,
#'   driver_llm = driver_llm,
#'   input_data = hist_data,
#'   forecast_horizon = 6
#' )
#'
#' # run the forecast iteration process
#' iterate_forecast(
#'   agent_info = agent_info,
#'   max_iter = 3,
#'   weighted_mape_goal = 0.03
#' )
#'
#' # get the final model summaries for an agent
#' model_summary <- get_summarized_models(agent_info = agent_info)
#' }
#' @export
get_summarized_models <- function(agent_info) {
  # formatting checks
  check_agent_info(agent_info = agent_info)

  # metadata
  project_info <- agent_info$project_info

  # load the final forecast for the agent
  model_summary_tbl <- read_file(
    run_info = project_info,
    path = paste0(
      "/final_output/", hash_data(project_info$project_name), "-",
      hash_data(agent_info$run_id), "-model_summary.", project_info$data_output
    )
  )

  return(model_summary_tbl)
}

#' Summarize Models
#'
#' Summarizes trained models for each time series in the best agent run by extracting
#' model details, hyperparameters, and diagnostics into a structured format.
#'
#' @param agent_info Agent info from [set_agent_info()]
#' @param parallel_processing Default of NULL runs no parallel processing and
#'   processes each time series one after another. 'local_machine' leverages
#'   all cores on current machine. 'spark' runs in parallel on a spark cluster
#'   in Azure Databricks or Azure Synapse.
#' @param num_cores Number of cores to use for parallel processing. If NULL,
#'   defaults to the number of available cores.
#'
#' @return A tibble containing summarized model information for all time series
#'   in the best agent run, saved to the logs folder.
#'
#' @noRd
summarize_models <- function(agent_info,
                             parallel_processing = NULL,
                             num_cores = NULL) {
  # Check inputs
  check_agent_info(agent_info = agent_info)
  check_input_type("parallel_processing", parallel_processing, c("character", "NULL"), c("NULL", "local_machine", "spark"))
  check_input_type("num_cores", num_cores, c("numeric", "NULL"))

  # Get project info
  project_info <- agent_info$project_info
  project_info$run_name <- agent_info$run_id

  # Step 1: Get best agent run info
  best_run_tbl <- get_best_agent_run(agent_info = agent_info) %>%
    dplyr::select(combo, best_run_name, model_type) %>%
    dplyr::distinct()

  # Map model names to summarize functions
  model_summarize_map <- list(
    "arima" = summarize_model_arima,
    "arimax" = summarize_model_arimax,
    "arima-boost" = summarize_model_arima_boost,
    "croston" = summarize_model_croston,
    "cubist" = summarize_model_cubist,
    "ets" = summarize_model_ets,
    "glmnet" = summarize_model_glmnet,
    "mars" = summarize_model_mars,
    "meanf" = summarize_model_meanf,
    "nnetar" = summarize_model_nnetar,
    "prophet" = summarize_model_prophet,
    "prophet-boost" = summarize_model_prophet_boost,
    "snaive" = summarize_model_snaive,
    "stlm-arima" = summarize_model_stlm_arima,
    "stlm-ets" = summarize_model_stlm_ets,
    "svm-poly" = summarize_model_svm_poly,
    "svm-rbf" = summarize_model_svm_rbf,
    "tbats" = summarize_model_tbats,
    "theta" = summarize_model_theta,
    "xgboost" = summarize_model_xgboost,
    "nnetar-xregs" = summarize_model_nnetar,
    "prophet-xregs" = summarize_model_prophet
  )

  # Setup parallel processing
  par_info <- par_start(
    run_info = project_info,
    parallel_processing = parallel_processing,
    num_cores = num_cores,
    task_length = nrow(best_run_tbl)
  )

  cl <- par_info$cl
  packages <- par_info$packages
  `%op%` <- par_info$foreach_operator

  # Step 2 & 3: Process each time series combo
  summary_tbl <- foreach::foreach(
    x = best_run_tbl %>%
      dplyr::group_split(dplyr::row_number(), .keep = FALSE),
    .combine = "rbind",
    .packages = packages,
    .errorhandling = "stop",
    .verbose = FALSE,
    .inorder = FALSE,
    .multicombine = TRUE,
    .export = NULL,
    .noexport = NULL
  ) %op%
    {
      # set metadata
      combo <- x$combo[1]
      model_type <- x$model_type[1]
      run_name <- x$best_run_name[1]

      if (model_type == "global") {
        hash_combo <- hash_data("all")
        forecast_hash_combo <- hash_data(combo)
        model_hash_combo <- hash_data("All-Data")
      } else {
        hash_combo <- hash_data(combo)
        forecast_hash_combo <- hash_data(combo)
        model_hash_combo <- hash_combo
      }

      # Create run info metadata
      project_name <- paste0(
        agent_info$project_info$project_name,
        "_",
        hash_combo
      )

      # Get trained models for this time series
      trained_models_tbl <- tryCatch(
        {
          read_file(project_info,
            path = paste0(
              "/models/", hash_data(project_name), "-", hash_data(run_name), "-",
              model_hash_combo, "-single_models.", project_info$object_output
            )
          )
        },
        error = function(e) {
          return(tibble::tibble())
        }
      )

      if (nrow(trained_models_tbl) == 0) {
        stop(paste0("No trained models found for combo: ", combo, "."), call. = FALSE)
      }

      # Get forecast data to identify best models by reading specific files directly
      if (model_type == "global") {
        # Try to read global models forecast file
        forecast_tbl <- tryCatch(
          {
            read_file(project_info,
              path = paste0(
                "/forecasts/", hash_data(project_name), "-", hash_data(run_name), "-",
                forecast_hash_combo, "-global_models.", project_info$data_output
              )
            ) %>%
              adjust_combo_column()
          },
          error = function(e) {
            return(tibble::tibble())
          }
        ) %>%
          suppressWarnings()

        # if no data, try reading a reconciled forecast file
        if (nrow(forecast_tbl) == 0) {
          forecast_tbl <- tryCatch(
            {
              read_file(project_info,
                path = paste0(
                  "/forecasts/", hash_data(project_name), "-", hash_data(run_name), "-",
                  hash_data("Best-Model"), "-reconciled.", project_info$data_output
                )
              ) %>%
                adjust_combo_column()
            },
            error = function(e) {
              return(tibble::tibble())
            }
          ) %>%
            suppressWarnings()
        }
      } else {
        # Try to read local models forecast file
        local_forecast_tbl <- tryCatch(
          {
            read_file(project_info,
              path = paste0(
                "/forecasts/", hash_data(project_name), "-", hash_data(run_name), "-",
                forecast_hash_combo, "-single_models.", project_info$data_output
              )
            ) %>%
              adjust_combo_column()
          },
          error = function(e) {
            return(tibble::tibble())
          }
        ) %>%
          suppressWarnings()

        # Try to read average models forecast file
        average_forecast_tbl <- tryCatch(
          {
            read_file(project_info,
              path = paste0(
                "/forecasts/", hash_data(project_name), "-", hash_data(run_name), "-",
                forecast_hash_combo, "-average_models.", project_info$data_output
              )
            ) %>%
              adjust_combo_column()
          },
          error = function(e) {
            return(tibble::tibble())
          }
        ) %>%
          suppressWarnings()

        # Combine all forecast data
        forecast_tbl <- dplyr::bind_rows(
          local_forecast_tbl,
          average_forecast_tbl
        )
      }

      if (nrow(forecast_tbl) == 0) {
        stop(paste0("No forecast data found to determine best models for combo: ", combo), call. = FALSE)
      }

      # Determine best models for this combo
      best_models <- tibble::tibble(Model_ID = character(), Best_Model = logical())

      if (nrow(forecast_tbl) > 0) {
        # Get best models, handling simple model averages
        best_models <- forecast_tbl %>%
          dplyr::filter(Combo == combo, Best_Model == "Yes") %>%
          dplyr::select(Model_ID) %>%
          dplyr::distinct() %>%
          # Split simple model averages into individual model IDs (separated by "_")
          tidyr::separate_rows(Model_ID, sep = "_") %>%
          dplyr::distinct() %>%
          dplyr::mutate(Best_Model = TRUE)
      }

      # If no best models found, set all to FALSE
      if (nrow(best_models) == 0) {
        best_models <- trained_models_tbl %>%
          dplyr::select(Model_ID) %>%
          dplyr::distinct() %>%
          dplyr::mutate(Best_Model = FALSE)
      }

      # Summarize each model
      model_summaries <- list()

      for (i in seq_len(nrow(trained_models_tbl))) {
        model_row <- trained_models_tbl[i, ]
        model_name <- model_row$Model_Name
        model_id <- model_row$Model_ID
        model_type <- model_row$Model_Type
        workflow <- model_row$Model_Fit[[1]]

        # Check if this model is a best model
        if (best_models$Model_ID[[1]] == "Best-Model" && model_type == "global" && model_name == "xgboost") {
          # Special case for global xgboost best model that runs hts reconciliation
          is_best <- "Yes"
        } else {
          is_best <- model_id %in% best_models$Model_ID[best_models$Best_Model]
          is_best <- ifelse(is_best, "Yes", "No") # Convert TRUE/FALSE to Yes/No
        }

        # Get the appropriate summarize function
        summarize_fn <- model_summarize_map[[model_name]]

        if (is.null(summarize_fn)) {
          # Model type not supported for summarization
          summary_df <- tibble::tibble(
            section = "error",
            name = "unsupported_model",
            value = paste0("No summarize function for model: ", model_name)
          )
        } else {
          # Summarize the workflow
          summary_df <- tryCatch(
            {
              summarize_fn(workflow)
            },
            error = function(e) {
              tibble::tibble(
                section = "error",
                name = "summarization_error",
                value = as.character(e$message)
              )
            }
          )
        }

        # Add metadata
        summary_df <- summary_df %>%
          dplyr::mutate(
            Combo = combo,
            Model_ID = model_id,
            Model_Name = model_name,
            Model_Type = model_type,
            Best_Model = is_best,
            .before = 1
          )

        model_summaries[[i]] <- summary_df
      }

      # Combine all model summaries for this combo
      combo_summary <- dplyr::bind_rows(model_summaries)

      # Write to disk in models folder
      write_data(
        x = combo_summary,
        combo = combo,
        run_info = project_info,
        output_type = "data",
        folder = "models",
        suffix = "-model_summary"
      )

      return(data.frame(Combo = combo))
    } %>%
    base::suppressPackageStartupMessages()

  # Clean up parallel processing
  par_end(cl)

  # Step 4: Read all individual model summary files and consolidate
  model_summary_path <- paste0(
    "/models/*", hash_data(project_info$project_name), "-",
    hash_data(project_info$run_name), "*-model_summary.", project_info$data_output
  )

  summary_results <- read_file(
    run_info = project_info,
    path = model_summary_path,
    return_type = "df"
  )

  # Error handling: Check if any model summary files were found/read
  if (is.null(summary_results) || (is.data.frame(summary_results) && nrow(summary_results) == 0)) {
    stop("No model summary files found. Cannot consolidate model summaries.")
  }

  # Error handling: Check if the number of unique combos matches the expected count
  if (length(unique(summary_results$Combo)) != nrow(best_run_tbl)) {
    stop(
      "Mismatch in number of combos summarized. Expected: ", nrow(best_run_tbl),
      ", Found: ", length(unique(summary_results$Combo))
    )
  }

  # Error handling: Check that all best models were summarized
  best_model_combos <- summary_results %>%
    dplyr::filter(Best_Model == "Yes") %>%
    dplyr::select(Combo, Best_Model) %>%
    dplyr::distinct()

  if (nrow(best_model_combos) != nrow(best_run_tbl)) {
    stop(
      "Not all best models were summarized. Expected combos with best models: ", nrow(best_run_tbl),
      ", Found: ", nrow(best_model_combos)
    )
  }

  # Step 5: Write final consolidated model summaries to final_output folder
  write_data(
    x = summary_results,
    combo = NULL,
    run_info = project_info,
    output_type = "data",
    folder = "final_output",
    suffix = "-model_summary"
  )
}

#' Summarize an ARIMA Workflow
#'
#' Extracts and summarizes key information from a fitted ARIMA workflow,
#' including model arguments, engine parameters, coefficients, and diagnostics.
#'
#' @param wf A fitted tidymodels workflow containing a modeltime::arima_reg()
#'   model with engine 'auto_arima' or 'arima'.
#'
#' @return A tibble with columns: section, name, value containing model details.
#'
#' @noRd
summarize_model_arima <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  add_diagnostics <- TRUE

  if (!inherits(wf, "workflow")) stop("summarize_model_arima() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!engine %in% c("auto_arima", "arima")) {
    stop("summarize_model_arima() only supports modeltime::arima_reg() with engines 'auto_arima' or 'arima'.")
  }

  # Vectorized: TRUE for bare symbol-like tokens
  is_symbolish_vec <- function(x) {
    x <- as.character(x)
    x_trim <- trimws(x)
    is_blank <- is.na(x_trim) | !nzchar(x_trim)
    is_quoted <- grepl('^".*"$|^\'.*\'$', x_trim)
    is_logical <- x_trim %in% c("TRUE", "FALSE", "True", "False")
    is_numeric <- grepl("^[+-]?[0-9.]+([eE][+-]?[0-9]+)?(\\s*,\\s*[0-9.]+([eE][+-]?[0-9]+)?)*$", x_trim)
    is_symbol <- grepl("^[A-Za-z][A-Za-z0-9._]*$", x_trim)
    (!is_blank) & is_symbol & !is_quoted & !is_logical & !is_numeric
  }

  # Find the underlying forecast::Arima object
  find_arima <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "Arima") || (!is.null(x$arma) && !is.null(x$coef))
    }, depth)
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # parsnip ARIMA args
  arg_names <- c(
    "non_seasonal_ar", "non_seasonal_differences", "non_seasonal_ma",
    "seasonal_ar", "seasonal_differences", "seasonal_ma", "seasonal_period"
  )
  arg_vals <- vapply(arg_names, function(nm) .chr1(spec$args[[nm]]), FUN.VALUE = character(1))
  args_tbl <- tibble::tibble(section = "model_arg", name = arg_names, value = arg_vals)

  # drill into Arima -> overwrite with ACTUAL (p,d,q)(P,D,Q)[m]
  arima_obj <- find_arima(fit$fit, scan_depth)
  p <- q <- P <- Q <- d <- D <- m <- NA_integer_

  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  if (!is.null(arima_obj)) {
    arma <- try(arima_obj$arma, silent = TRUE)
    if (!inherits(arma, "try-error") && length(arma) >= 7) {
      p <- arma[1]
      q <- arma[2]
      P <- arma[3]
      Q <- arma[4]
      m <- arma[5]
      d <- arma[6]
      D <- arma[7]
      actuals <- c(
        non_seasonal_ar          = p,
        non_seasonal_differences = d,
        non_seasonal_ma          = q,
        seasonal_ar              = P,
        seasonal_differences     = D,
        seasonal_ma              = Q,
        seasonal_period          = m
      )
      for (nm in names(actuals)) {
        if (nm %in% args_tbl$name) {
          args_tbl$value[args_tbl$name == nm] <- as.character(actuals[[nm]])
        } else {
          args_tbl <- dplyr::bind_rows(args_tbl, .kv("model_arg", nm, as.character(actuals[[nm]])))
        }
      }
    }

    # Info criteria & summary fields
    for (nm in c("aic", "aicc", "bic", "sigma2", "loglik", "method")) {
      val <- try(arima_obj[[nm]], silent = TRUE)
      if (!inherits(val, "try-error") && !is.null(val)) {
        v <- if (is.numeric(val)) as.character(signif(val, digits)) else .chr1(val)
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", nm, v))
      }
    }

    # Order strings
    if (is.finite(p) && is.finite(d) && is.finite(q)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "order_str", sprintf("(%d,%d,%d)", p, d, q)))
    }
    if (is.finite(P) && is.finite(D) && is.finite(Q) && is.finite(m)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "seasonal_order_str", sprintf("(%d,%d,%d)[%d]", P, D, Q, m)))
    }

    # Stored call args (ic, lambda, biasadj, stepwise, approximation, include.*)
    cl <- try(arima_obj$call, silent = TRUE)
    if (!inherits(cl, "try-error") && is.language(cl)) {
      args <- as.list(cl)[-1]
      drop <- c("x", "y", "data", "xreg", "fitted", "residuals", "formula", "object", "ts", "series", "...")
      args <- args[setdiff(names(args), drop)]
      args <- args[names(args) != ""]
      for (nm in names(args)) {
        val <- args[[nm]]
        if (is.list(val)) {
          subnm <- names(val)
          for (j in seq_along(val)) {
            key <- if (!is.null(subnm) && nzchar(subnm[j])) paste0(nm, ".", subnm[j]) else paste0(nm, ".[", j, "]")
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", key, .chr1(val[[j]])))
          }
        } else {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", nm, .chr1(val)))
        }
      }

      # Normalize IC used (default for auto_arima is AICc if not set)
      if (!any(eng_tbl$name == "ic") && identical(engine, "auto_arima")) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "ic_used", "aicc"))
      } else if (any(eng_tbl$name == "ic")) {
        ic_val <- eng_tbl$value[eng_tbl$name == "ic"][1]
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "ic_used", ic_val))
      }

      # Transform flags: lambda/biasadj; report "none" if absent
      if (!any(eng_tbl$name == "lambda")) eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lambda", "none"))
      if (!any(eng_tbl$name == "biasadj")) eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "biasadj", "FALSE"))
    } else {
      # If no call seen, still provide defaults for readability
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "ic_used", if (identical(engine, "auto_arima")) "aicc" else "aic"))
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lambda", "none"), .kv("engine_param", "biasadj", "FALSE"))
    }

    # nobs
    nobs <- try(arima_obj$nobs, silent = TRUE)
    if (!inherits(nobs, "try-error") && !is.null(nobs)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "nobs", as.character(nobs)))
    }

    # Coefficients
    coefs <- try(arima_obj$coef, silent = TRUE)
    if (!inherits(coefs, "try-error") && !is.null(coefs) && length(coefs)) {
      cn <- names(coefs)
      if (is.null(cn)) cn <- paste0("coef[", seq_along(coefs), "]")

      if (!any(eng_tbl$name == "include.mean")) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "include.mean",
          if (any(grepl("intercept|mean", cn, ignore.case = TRUE))) "TRUE" else "FALSE"
        ))
      }
      if (!any(eng_tbl$name == "include.drift")) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "include.drift",
          if (any(grepl("drift", cn, ignore.case = TRUE))) "TRUE" else "FALSE"
        ))
      }
      coef_tbl <- tibble::tibble(
        section = "coefficient",
        name = cn,
        value = as.character(signif(as.numeric(coefs), digits))
      )
      eng_tbl <- dplyr::bind_rows(eng_tbl, coef_tbl)
    }

    # Optional diagnostics: Ljung–Box
    if (isTRUE(add_diagnostics)) {
      resids <- try(arima_obj$residuals, silent = TRUE)
      if (!inherits(resids, "try-error") && !is.null(resids)) {
        # Check for external regressors first
        xreg_names <- try(names(arima_obj$xreg), silent = TRUE)
        if (inherits(xreg_names, "try-error") || is.null(xreg_names)) {
          xreg_names <- character(0)
        }

        lb_lag <- 24L

        if (is.finite(m) && !is.na(m)) lb_lag <- min(24L, max(8L, 2L * as.integer(m)))
        # Conservative df (lags consumed by AR + seasonal AR + xreg)
        # Make sure fitdf doesn't exceed lag
        fitdf <- sum(is.finite(c(p, P))) + length(xreg_names)
        fitdf <- min(fitdf, lb_lag - 1) # Ensure at least 1 df remains

        lb <- try(stats::Box.test(resids, lag = lb_lag, type = "Ljung-Box", fitdf = fitdf), silent = TRUE)
        if (!inherits(lb, "try-error") && !is.null(lb$p.value)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "ljung_box_lag", as.character(lb_lag)),
            .kv("engine_param", "ljung_box_p", sprintf("%.4g", lb$p.value))
          )

          # Only add statistic if it's valid
          if (!is.null(lb$statistic) && is.finite(lb$statistic)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "ljung_box_statistic", sprintf("%.4g", lb$statistic)))
          }
        }

        # Basic residual statistics
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "residuals.mean", as.character(signif(mean(resids, na.rm = TRUE), digits))),
          .kv("engine_param", "residuals.sd", as.character(signif(sd(resids, na.rm = TRUE), digits)))
        )

        # Add external regressors info if any
        if (length(xreg_names) > 0) {
          # Add the names of external regressors
          for (xreg_name in xreg_names) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv("engine_param", paste0("xreg_", xreg_name), "included")
            )
          }
        }
      }
    }
  }

  # Drop symbol-like placeholders from engine_param (e.g., "max.P", "max.q")
  if (nrow(eng_tbl)) {
    keep <- (eng_tbl$section != "engine_param") | (!is_symbolish_vec(eng_tbl$value))
    eng_tbl <- eng_tbl[keep, , drop = FALSE]
  }

  .assemble_output(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize an ARIMAX Workflow
#'
#' Extracts and summarizes key information from a fitted ARIMAX workflow,
#' including model arguments, engine parameters, coefficients (both ARIMA and
#' external regressors), and diagnostics.
#'
#' @param wf A fitted tidymodels workflow containing a modeltime::arima_reg()
#'   model with engines 'auto_arima', 'auto_arima_xregs', 'arima', or 'arima_xregs'.
#'
#' @return A tibble with columns: model_class, engine, section, name, value
#'   containing model details including external regressor information.
#'
#' @noRd
summarize_model_arimax <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  add_diagnostics <- TRUE

  if (!inherits(wf, "workflow")) stop("summarize_model_arimax() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!engine %in% c("auto_arima", "auto_arima_xregs", "arima", "arima_xregs")) {
    stop("summarize_model_arimax() only supports modeltime::arima_reg() with engines 'auto_arima', 'auto_arima_xregs', 'arima', or 'arima_xregs'.")
  }

  # Vectorized: TRUE for bare symbol-like tokens
  is_symbolish_vec <- function(x) {
    x <- as.character(x)
    x_trim <- trimws(x)
    is_blank <- is.na(x_trim) | !nzchar(x_trim)
    is_quoted <- grepl('^".*"$|^\'.*\'$', x_trim)
    is_logical <- x_trim %in% c("TRUE", "FALSE", "True", "False")
    is_numeric <- grepl("^[+-]?[0-9.]+([eE][+-]?[0-9]+)?(\\s*,\\s*[0-9.]+([eE][+-]?[0-9]+)?)*$", x_trim)
    is_symbol <- grepl("^[A-Za-z][A-Za-z0-9._]*$", x_trim)
    (!is_blank) & is_symbol & !is_quoted & !is_logical & !is_numeric
  }

  # Find the underlying forecast::Arima object
  find_arima <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "Arima") || (!is.null(x$arma) && !is.null(x$coef))
    }, depth)
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Separate external regressors from date predictors
  xreg_names <- character()
  if (!inherits(mold, "try-error") && !is.null(mold$predictors) && ncol(mold$predictors) > 0) {
    is_date <- vapply(mold$predictors, function(col) inherits(col, c("Date", "POSIXct", "POSIXt")), logical(1))
    xreg_names <- names(mold$predictors)[!is_date]

    # Update predictor types to indicate which are external regressors
    if (length(xreg_names) > 0 && nrow(preds_tbl) > 0) {
      preds_tbl$value[preds_tbl$name %in% xreg_names] <-
        paste0(preds_tbl$value[preds_tbl$name %in% xreg_names], " [xreg]")
    }
  }

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # parsnip ARIMA args
  arg_names <- c(
    "non_seasonal_ar", "non_seasonal_differences", "non_seasonal_ma",
    "seasonal_ar", "seasonal_differences", "seasonal_ma", "seasonal_period"
  )
  arg_vals <- vapply(arg_names, function(nm) .chr1(spec$args[[nm]]), FUN.VALUE = character(1))
  args_tbl <- tibble::tibble(section = "model_arg", name = arg_names, value = arg_vals)

  # drill into Arima -> overwrite with ACTUAL (p,d,q)(P,D,Q)[m]
  arima_obj <- find_arima(fit$fit, scan_depth)
  p <- q <- P <- Q <- d <- D <- m <- NA_integer_

  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Add external regressor information
  if (length(xreg_names) > 0) {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "num_xregs", as.character(length(xreg_names))))
    for (i in seq_along(xreg_names)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", paste0("xreg_", i), xreg_names[i]))
    }
  }

  if (!is.null(arima_obj)) {
    arma <- try(arima_obj$arma, silent = TRUE)
    if (!inherits(arma, "try-error") && length(arma) >= 7) {
      p <- arma[1]
      q <- arma[2]
      P <- arma[3]
      Q <- arma[4]
      m <- arma[5]
      d <- arma[6]
      D <- arma[7]
      actuals <- c(
        non_seasonal_ar          = p,
        non_seasonal_differences = d,
        non_seasonal_ma          = q,
        seasonal_ar              = P,
        seasonal_differences     = D,
        seasonal_ma              = Q,
        seasonal_period          = m
      )
      for (nm in names(actuals)) {
        if (nm %in% args_tbl$name) {
          args_tbl$value[args_tbl$name == nm] <- as.character(actuals[[nm]])
        } else {
          args_tbl <- dplyr::bind_rows(args_tbl, .kv("model_arg", nm, as.character(actuals[[nm]])))
        }
      }
    }

    # Info criteria & summary fields
    for (nm in c("aic", "aicc", "bic", "sigma2", "loglik", "method")) {
      val <- try(arima_obj[[nm]], silent = TRUE)
      if (!inherits(val, "try-error") && !is.null(val)) {
        v <- if (is.numeric(val)) as.character(signif(val, digits)) else .chr1(val)
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", nm, v))
      }
    }

    # Order strings
    if (is.finite(p) && is.finite(d) && is.finite(q)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "order_str", sprintf("(%d,%d,%d)", p, d, q)))
    }
    if (is.finite(P) && is.finite(D) && is.finite(Q) && is.finite(m)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "seasonal_order_str", sprintf("(%d,%d,%d)[%d]", P, D, Q, m)))
    }

    # Extract xreg matrix information
    xreg_matrix <- try(arima_obj$xreg, silent = TRUE)
    if (!inherits(xreg_matrix, "try-error") && !is.null(xreg_matrix)) {
      if (is.matrix(xreg_matrix) || is.data.frame(xreg_matrix)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "xreg_dim", paste0(nrow(xreg_matrix), "x", ncol(xreg_matrix))))

        # Get column names of xreg matrix
        xreg_cols <- colnames(xreg_matrix)
        if (!is.null(xreg_cols)) {
          for (i in seq_along(xreg_cols)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", paste0("xreg_col_", i), xreg_cols[i]))
          }
        }
      }
    }

    # Stored call args (ic, lambda, biasadj, stepwise, approximation, include.*)
    cl <- try(arima_obj$call, silent = TRUE)
    if (!inherits(cl, "try-error") && is.language(cl)) {
      args <- as.list(cl)[-1]
      drop <- c("x", "y", "data", "fitted", "residuals", "formula", "object", "ts", "series", "...")
      args <- args[setdiff(names(args), drop)]
      args <- args[names(args) != ""]

      # Don't duplicate xreg info
      if ("xreg" %in% names(args)) {
        args$xreg <- NULL # We've already extracted this info above
      }

      for (nm in names(args)) {
        val <- args[[nm]]
        if (is.list(val)) {
          subnm <- names(val)
          for (j in seq_along(val)) {
            key <- if (!is.null(subnm) && nzchar(subnm[j])) paste0(nm, ".", subnm[j]) else paste0(nm, ".[", j, "]")
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", key, .chr1(val[[j]])))
          }
        } else {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", nm, .chr1(val)))
        }
      }

      # Normalize IC used (default for auto_arima is AICc if not set)
      if (!any(eng_tbl$name == "ic") && grepl("auto_arima", engine)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "ic_used", "aicc"))
      } else if (any(eng_tbl$name == "ic")) {
        ic_val <- eng_tbl$value[eng_tbl$name == "ic"][1]
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "ic_used", ic_val))
      }

      # Transform flags: lambda/biasadj; report "none" if absent
      if (!any(eng_tbl$name == "lambda")) eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lambda", "none"))
      if (!any(eng_tbl$name == "biasadj")) eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "biasadj", "FALSE"))
    } else {
      # If no call seen, still provide defaults for readability
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "ic_used", if (grepl("auto_arima", engine)) "aicc" else "aic"))
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lambda", "none"), .kv("engine_param", "biasadj", "FALSE"))
    }

    # nobs
    nobs <- try(arima_obj$nobs, silent = TRUE)
    if (!inherits(nobs, "try-error") && !is.null(nobs)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "nobs", as.character(nobs)))
    }

    # Coefficients - separate ARIMA and xreg coefficients
    coefs <- try(arima_obj$coef, silent = TRUE)
    if (!inherits(coefs, "try-error") && !is.null(coefs) && length(coefs)) {
      cn <- names(coefs)
      if (is.null(cn)) cn <- paste0("coef[", seq_along(coefs), "]")

      # Separate xreg coefficients from ARIMA coefficients
      is_xreg_coef <- grepl("^xreg", cn, ignore.case = FALSE) | cn %in% xreg_names

      # ARIMA coefficients
      arima_coefs <- coefs[!is_xreg_coef]
      if (length(arima_coefs) > 0) {
        arima_cn <- cn[!is_xreg_coef]

        if (!any(eng_tbl$name == "include.mean")) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
            "engine_param", "include.mean",
            if (any(grepl("intercept|mean", arima_cn, ignore.case = TRUE))) "TRUE" else "FALSE"
          ))
        }
        if (!any(eng_tbl$name == "include.drift")) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
            "engine_param", "include.drift",
            if (any(grepl("drift", arima_cn, ignore.case = TRUE))) "TRUE" else "FALSE"
          ))
        }

        for (i in seq_along(arima_coefs)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
            "coefficient", arima_cn[i],
            as.character(signif(as.numeric(arima_coefs[i]), digits))
          ))
        }
      }

      # External regressor coefficients
      xreg_coefs <- coefs[is_xreg_coef]
      if (length(xreg_coefs) > 0) {
        xreg_cn <- cn[is_xreg_coef]

        # Try to get standard errors for xreg coefficients
        var_coef <- try(arima_obj$var.coef, silent = TRUE)
        if (!inherits(var_coef, "try-error") && !is.null(var_coef)) {
          se <- sqrt(diag(var_coef))
          se_xreg <- se[is_xreg_coef]

          for (i in seq_along(xreg_coefs)) {
            coef_val <- signif(as.numeric(xreg_coefs[i]), digits)
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("xreg_coefficient", xreg_cn[i], as.character(coef_val)))

            if (i <= length(se_xreg) && is.finite(se_xreg[i])) {
              se_val <- signif(se_xreg[i], digits)
              t_val <- coef_val / se_val
              eng_tbl <- dplyr::bind_rows(
                eng_tbl,
                .kv("xreg_coefficient", paste0(xreg_cn[i], ".se"), as.character(se_val)),
                .kv("xreg_coefficient", paste0(xreg_cn[i], ".t_value"), as.character(signif(t_val, digits)))
              )
            }
          }
        } else {
          for (i in seq_along(xreg_coefs)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
              "xreg_coefficient", xreg_cn[i],
              as.character(signif(as.numeric(xreg_coefs[i]), digits))
            ))
          }
        }
      }
    }

    # Optional diagnostics: Ljung–Box
    if (isTRUE(add_diagnostics)) {
      resids <- try(arima_obj$residuals, silent = TRUE)
      if (!inherits(resids, "try-error") && !is.null(resids)) {
        # Check for external regressors first
        if (inherits(xreg_names, "try-error") || is.null(xreg_names)) {
          xreg_names <- character(0)
        }

        lb_lag <- 24L

        if (is.finite(m) && !is.na(m)) lb_lag <- min(24L, max(8L, 2L * as.integer(m)))
        # Calculate degrees of freedom for Ljung-Box test
        # df = AR terms + seasonal AR terms + external regressors
        fitdf <- sum(is.finite(c(p, P))) + length(xreg_names)
        fitdf <- min(fitdf, lb_lag - 1) # Ensure at least 1 df remains

        lb <- try(stats::Box.test(resids, lag = lb_lag, type = "Ljung-Box", fitdf = fitdf), silent = TRUE)
        if (!inherits(lb, "try-error") && !is.null(lb$p.value)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "ljung_box_lag", as.character(lb_lag)),
            .kv("engine_param", "ljung_box_p", sprintf("%.4g", lb$p.value))
          )

          # Only add statistic if it's valid
          if (!is.null(lb$statistic) && is.finite(lb$statistic)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "ljung_box_statistic", sprintf("%.4g", lb$statistic)))
          }
        }

        # Basic residual statistics
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "residuals.mean", as.character(signif(mean(resids, na.rm = TRUE), digits))),
          .kv("engine_param", "residuals.sd", as.character(signif(sd(resids, na.rm = TRUE), digits)))
        )

        # Add external regressors info if any
        if (length(xreg_names) > 0) {
          # Add the names of external regressors
          for (xreg_name in xreg_names) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv("engine_param", paste0("xreg_", xreg_name), "included")
            )
          }
        }
      }
    }
  }

  # Drop symbol-like placeholders from engine_param (e.g., "max.P", "max.q")
  if (nrow(eng_tbl)) {
    keep <- (eng_tbl$section != "engine_param") | (!is_symbolish_vec(eng_tbl$value))
    eng_tbl <- eng_tbl[keep, , drop = FALSE]
  }

  # Custom assembly for ARIMAX to handle xreg_coefficient section
  out <- dplyr::bind_rows(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl) |>
    dplyr::mutate(
      model_class = class(fit$fit)[1],
      engine = engine,
      .before = 1
    ) |>
    dplyr::distinct(model_class, engine, section, name, value, .keep_all = TRUE)

  # Determine sections present for proper ordering
  sections_present <- unique(out$section)
  section_levels <- c("predictor", "outcome", "recipe_step", "model_arg", "engine_param", "coefficient", "xreg_coefficient")
  section_levels <- intersect(section_levels, sections_present)

  out <- dplyr::arrange(
    out,
    model_class,
    engine,
    factor(section, levels = section_levels),
    name
  )

  out
}

#' Summarize an ARIMA Boost Workflow
#'
#' Extracts and summarizes key information from a fitted ARIMA Boost workflow.
#' ARIMA Boost combines an ARIMA model for trend with an XGBoost model for residuals,
#' providing both time series structure and machine learning capabilities.
#'
#' @param wf A fitted tidymodels workflow containing a modeltime::arima_boost()
#'   model with engine 'auto_arima_xgboost'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including ARIMA parameters, XGBoost hyperparameters, and variable importance.
#'
#' @noRd
summarize_model_arima_boost <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  add_diagnostics <- TRUE
  importance_threshold <- 1e-6

  if (!inherits(wf, "workflow")) stop("summarize_model_arima_boost() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "auto_arima_xgboost")) {
    stop("summarize_model_arima_boost() only supports modeltime::arima_boost() with engine 'auto_arima_xgboost'.")
  }

  # Helper function to format numeric values
  .format_numeric <- function(x) {
    if (is.character(x) && (x == "auto" || x == "")) {
      return(x)
    }
    x_num <- suppressWarnings(as.numeric(x))
    if (!is.na(x_num) && is.finite(x_num)) {
      rounded <- round(x_num, digits)
      if (rounded == floor(rounded)) {
        return(as.character(as.integer(rounded)))
      } else {
        return(format(rounded, scientific = FALSE, trim = TRUE))
      }
    }
    return(as.character(x))
  }

  # Find the underlying forecast::Arima object for ARIMA component
  find_arima <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "Arima") || (!is.null(x$arma) && !is.null(x$coef))
    }, depth)
  }

  # Find the xgboost object for XGBoost component
  find_xgboost <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "xgb.Booster") ||
        (is.list(x) && !is.null(x$handle) && inherits(x$handle, "xgb.Booster.handle"))
    }, depth)
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Model arguments - combine ARIMA and XGBoost parameters
  # ARIMA parameters
  arima_arg_names <- c(
    "non_seasonal_ar", "non_seasonal_differences", "non_seasonal_ma",
    "seasonal_ar", "seasonal_differences", "seasonal_ma", "seasonal_period"
  )
  # XGBoost parameters
  xgb_arg_names <- c(
    "tree_depth", "trees", "learn_rate", "mtry", "min_n",
    "loss_reduction", "sample_size", "stop_iter"
  )

  all_arg_names <- c(arima_arg_names, xgb_arg_names)
  arg_vals <- vapply(all_arg_names, function(nm) {
    val <- .chr1(spec$args[[nm]])
    if (nm %in% xgb_arg_names) {
      .format_numeric(val)
    } else {
      val
    }
  }, FUN.VALUE = character(1))
  args_tbl <- tibble::tibble(section = "model_arg", name = all_arg_names, value = arg_vals)

  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Add model type indicator
  eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "ARIMA-Boost Hybrid"))

  # ARIMA Component
  arima_obj <- find_arima(fit$fit, scan_depth)
  p <- q <- P <- Q <- d <- D <- m <- NA_integer_

  if (!is.null(arima_obj)) {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "arima_component", "present"))

    arma <- try(arima_obj$arma, silent = TRUE)
    if (!inherits(arma, "try-error") && length(arma) >= 7) {
      p <- arma[1]
      q <- arma[2]
      P <- arma[3]
      Q <- arma[4]
      m <- arma[5]
      d <- arma[6]
      D <- arma[7]

      # Update ARIMA args with actual values
      actuals <- c(
        non_seasonal_ar          = p,
        non_seasonal_differences = d,
        non_seasonal_ma          = q,
        seasonal_ar              = P,
        seasonal_differences     = D,
        seasonal_ma              = Q,
        seasonal_period          = m
      )
      for (nm in names(actuals)) {
        if (nm %in% args_tbl$name) {
          args_tbl$value[args_tbl$name == nm] <- as.character(actuals[[nm]])
        }
      }
    }

    # ARIMA order strings
    if (is.finite(p) && is.finite(d) && is.finite(q)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "arima_order", sprintf("(%d,%d,%d)", p, d, q)))
    }
    if (is.finite(P) && is.finite(D) && is.finite(Q) && is.finite(m)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "arima_seasonal_order", sprintf("(%d,%d,%d)[%d]", P, D, Q, m)))
    }

    # ARIMA info criteria
    for (nm in c("aic", "aicc", "bic", "sigma2", "loglik")) {
      val <- try(arima_obj[[nm]], silent = TRUE)
      if (!inherits(val, "try-error") && !is.null(val) && is.finite(val)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", paste0("arima_", nm), as.character(signif(val, digits))))
      }
    }

    # ARIMA coefficients
    coefs <- try(arima_obj$coef, silent = TRUE)
    if (!inherits(coefs, "try-error") && !is.null(coefs) && length(coefs)) {
      cn <- names(coefs)
      if (is.null(cn)) {
        cn <- paste0("arima_coef[", seq_along(coefs), "]")
      } else {
        cn <- paste0("arima_", cn)
      }

      coef_tbl <- tibble::tibble(
        section = "coefficient",
        name = cn,
        value = as.character(signif(as.numeric(coefs), digits))
      )
      eng_tbl <- dplyr::bind_rows(eng_tbl, coef_tbl)
    }

    # ARIMA diagnostics
    if (isTRUE(add_diagnostics)) {
      resids <- try(arima_obj$residuals, silent = TRUE)
      if (!inherits(resids, "try-error") && !is.null(resids) && length(resids) > 0) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "arima_residuals_mean", as.character(signif(mean(resids, na.rm = TRUE), digits))),
          .kv("engine_param", "arima_residuals_sd", as.character(signif(sd(resids, na.rm = TRUE), digits)))
        )
      }
    }
  } else {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "arima_component", "not_found"))
  }

  # XGBoost Component
  xgb_obj <- find_xgboost(fit$fit, scan_depth)

  if (!is.null(xgb_obj)) {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "xgboost_component", "present"))

    # Extract xgb.Booster if wrapped
    if (!inherits(xgb_obj, "xgb.Booster")) {
      inner_xgb <- try(xgb_obj$fit, silent = TRUE)
      if (!inherits(inner_xgb, "try-error") && inherits(inner_xgb, "xgb.Booster")) {
        xgb_obj <- inner_xgb
      }
    }

    if (inherits(xgb_obj, "xgb.Booster")) {
      # Number of boosting rounds
      niter <- try(xgb_obj$niter, silent = TRUE)
      if (!inherits(niter, "try-error") && !is.null(niter) && is.finite(niter)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "xgb_nrounds", as.character(niter)))

        # Update args_tbl if trees was "auto"
        if (args_tbl$value[args_tbl$name == "trees"] == "auto") {
          args_tbl$value[args_tbl$name == "trees"] <- as.character(niter)
        }
      }

      # Get training parameters
      params <- try(xgb_obj$params, silent = TRUE)
      if (!inherits(params, "try-error") && !is.null(params) && is.list(params)) {
        param_names <- c("objective", "eval_metric", "tree_method")

        for (pname in param_names) {
          pval <- params[[pname]]
          if (!is.null(pval)) {
            if (is.numeric(pval)) {
              eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", paste0("xgb_", pname), .format_numeric(pval)))
            } else {
              eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", paste0("xgb_", pname), as.character(pval)))
            }
          }
        }

        # Update args_tbl with actual parameter values if they were "auto"
        param_map <- list(
          tree_depth = "max_depth",
          learn_rate = "eta",
          min_n = "min_child_weight",
          loss_reduction = "gamma",
          sample_size = "subsample"
        )

        for (arg_name in names(param_map)) {
          param_name <- param_map[[arg_name]]
          if (args_tbl$value[args_tbl$name == arg_name] == "auto" && !is.null(params[[param_name]])) {
            args_tbl$value[args_tbl$name == arg_name] <- .format_numeric(params[[param_name]])
          }
        }

        # Handle mtry specially
        if (args_tbl$value[args_tbl$name == "mtry"] == "auto") {
          if (!is.null(params$colsample_bynode)) {
            args_tbl$value[args_tbl$name == "mtry"] <- .format_numeric(params$colsample_bynode)
          } else if (!is.null(params$colsample_bytree)) {
            args_tbl$value[args_tbl$name == "mtry"] <- .format_numeric(params$colsample_bytree)
          }
        }
      }

      # Variable importance
      importance <- try(vip::vi(xgb_obj, scale = TRUE) %>% suppressWarnings(), silent = TRUE)
      importance_list <- list()

      if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
        # Filter out features with negligible importance
        importance <- importance[importance$Importance > importance_threshold, ]

        if (nrow(importance) > 0) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "xgb_n_important_features", as.character(nrow(importance))))

          for (i in seq_len(nrow(importance))) {
            feat_name <- importance$Variable[i]
            importance_val <- importance$Importance[i]

            importance_list[[length(importance_list) + 1]] <- .kv(
              "importance",
              feat_name,
              .format_numeric(importance_val)
            )
          }
        } else {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "xgb_n_important_features", "0"))
        }
      }

      if (length(importance_list) > 0) {
        importance_tbl <- dplyr::bind_rows(importance_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
      }
    }
  } else {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "xgboost_component", "not_found"))
  }

  .assemble_output(
    preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize a Croston Workflow
#'
#' Extracts and summarizes key information from a fitted Croston model workflow,
#' designed specifically for intermittent demand forecasting. Includes detailed
#' analysis of zero/non-zero patterns, demand intervals, and forecast performance.
#'
#' @param wf A fitted tidymodels workflow containing a modeltime::exp_smoothing()
#'   model with engine 'croston'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including intermittent demand statistics, smoothing parameters, and diagnostics.
#'
#' @noRd
summarize_model_croston <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6

  if (!inherits(wf, "workflow")) stop("summarize_model_croston() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "croston")) {
    stop("summarize_model_croston() only supports modeltime::exp_smoothing() with set_engine('croston').")
  }

  # Specific predicates for Croston
  is_croston <- function(o) {
    inherits(o, "forecast") || inherits(o, "croston") ||
      (is.list(o) && !is.null(o$method) && grepl("croston", o$method, ignore.case = TRUE))
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Model args - Croston specific
  args_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Check for smooth_level argument
  if (!is.null(spec$args$smooth_level)) {
    args_tbl <- dplyr::bind_rows(
      args_tbl,
      .kv("model_arg", "smooth_level", .chr1(spec$args$smooth_level))
    )
  }

  # Engine params
  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Find the Croston model object - try multiple approaches
  engine_fit <- fit$fit
  croston_obj <- .find_obj(engine_fit, is_croston, scan_depth)

  # Also check if the fit itself is a Croston object
  if (is.null(croston_obj) && is_croston(engine_fit)) {
    croston_obj <- engine_fit
  }

  # Look for the models$model_1 structure (based on explore output)
  if (is.null(croston_obj)) {
    if (!is.null(engine_fit$models) && !is.null(engine_fit$models$model_1)) {
      if (!is.null(engine_fit$models$model_1$method) &&
        grepl("croston", tolower(engine_fit$models$model_1$method))) {
        croston_obj <- engine_fit$models$model_1
      }
    }
  }

  # Extract alpha from extras if available
  alpha_val <- NULL
  if (!is.null(engine_fit$extras) && !is.null(engine_fit$extras$alpha)) {
    alpha_val <- engine_fit$extras$alpha
    if (is.numeric(alpha_val) && length(alpha_val) == 1 && is.finite(alpha_val)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "alpha_fitted", as.character(signif(alpha_val, digits))))
    }
  }

  if (!is.null(croston_obj)) {
    # Extract method first - this should work based on the JSON output
    if (!is.null(croston_obj$method)) {
      method_val <- croston_obj$method
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "method", as.character(method_val)))

      # Check for specific variants
      if (grepl("SBA|sba", method_val, ignore.case = TRUE)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "method_variant", "Syntetos-Boylan Approximation"))
      } else if (grepl("SBJ|sbj", method_val, ignore.case = TRUE)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "method_variant", "Shale-Boylan-Johnston"))
      } else {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "method_variant", "Classic Croston"))
      }
    }

    # Extract the smoothed demand value
    if (!is.null(croston_obj$model) && !is.null(croston_obj$model$demand)) {
      demand_val <- croston_obj$model$demand
      if (is.numeric(demand_val)) {
        # Check if it's a vector or single value
        if (length(demand_val) > 1) {
          final_demand <- utils::tail(demand_val, 1)
        } else {
          final_demand <- demand_val
        }

        if (is.finite(final_demand)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv(
              "engine_param", "final_demand_size",
              as.character(signif(final_demand, digits))
            )
          )
        }
      }
    }

    # Extract the period/interval value
    if (!is.null(croston_obj$model) && !is.null(croston_obj$model$period)) {
      period_val <- croston_obj$model$period
      if (is.numeric(period_val)) {
        # Check if it's a vector or single value
        if (length(period_val) > 1) {
          final_period <- utils::tail(period_val, 1)
        } else {
          final_period <- period_val
        }

        if (is.finite(final_period)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv(
              "engine_param", "final_interval",
              as.character(signif(final_period, digits))
            )
          )
        }
      }
    }

    # Extract mean (forecast value)
    if (!is.null(croston_obj$mean)) {
      mean_val <- croston_obj$mean
      if (is.numeric(mean_val) && length(mean_val) > 0) {
        # Take the first forecast value
        forecast_val <- mean_val[1]
        if (is.finite(forecast_val)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv(
              "engine_param", "forecast_value",
              as.character(signif(forecast_val, digits))
            )
          )
        }
      }
    }

    # Extract fitted values statistics
    if (!is.null(croston_obj$fitted)) {
      fitted_vals <- croston_obj$fitted
      if (is.numeric(fitted_vals) && length(fitted_vals) > 0) {
        # Calculate statistics on fitted values
        fitted_mean <- mean(fitted_vals, na.rm = TRUE)
        fitted_min <- min(fitted_vals, na.rm = TRUE)
        fitted_max <- max(fitted_vals, na.rm = TRUE)

        if (is.finite(fitted_mean) && is.finite(fitted_min) && is.finite(fitted_max)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv(
              "engine_param", "fitted_mean",
              as.character(signif(fitted_mean, digits))
            ),
            .kv(
              "engine_param", "fitted_min",
              as.character(signif(fitted_min, digits))
            ),
            .kv(
              "engine_param", "fitted_max",
              as.character(signif(fitted_max, digits))
            )
          )
        }
      }
    }

    # Extract and calculate statistics from residuals
    if (!is.null(croston_obj$residuals)) {
      resids <- croston_obj$residuals
      if (is.numeric(resids) && length(resids) > 0) {
        resid_mean <- mean(resids, na.rm = TRUE)
        resid_min <- min(resids, na.rm = TRUE)
        resid_max <- max(resids, na.rm = TRUE)
        resid_sd <- stats::sd(resids, na.rm = TRUE)

        if (is.finite(resid_mean) && is.finite(resid_min) && is.finite(resid_max) && is.finite(resid_sd)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv(
              "engine_param", "residuals.mean",
              as.character(signif(resid_mean, digits))
            ),
            .kv(
              "engine_param", "residuals.min",
              as.character(signif(resid_min, digits))
            ),
            .kv(
              "engine_param", "residuals.max",
              as.character(signif(resid_max, digits))
            ),
            .kv(
              "engine_param", "residuals.sd",
              as.character(signif(resid_sd, digits))
            )
          )
        }

        # Calculate RMSE
        rmse <- sqrt(mean(resids^2, na.rm = TRUE))
        if (is.finite(rmse)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv(
              "engine_param", "rmse",
              as.character(signif(rmse, digits))
            )
          )
        }

        # Calculate MAE
        mae <- mean(abs(resids), na.rm = TRUE)
        if (is.finite(mae)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv(
              "engine_param", "mae",
              as.character(signif(mae, digits))
            )
          )
        }
      }
    }

    # Extract series data for intermittent demand analysis
    # Croston's method is specifically designed for intermittent/sparse demand patterns
    # Key metrics:
    #   - Zero proportion: frequency of zero demand periods
    #   - Non-zero demand stats: average size and variability of actual demands
    #   - Interval stats: time between demands (CV² > 0.49 indicates high variability)
    if (!is.null(croston_obj$x)) {
      x_vals <- croston_obj$x
      if (is.numeric(x_vals) && length(x_vals) > 0) {
        # Count zeros and non-zeros
        n_zeros <- sum(x_vals == 0, na.rm = TRUE)
        n_nonzeros <- sum(x_vals != 0, na.rm = TRUE)
        total_obs <- length(x_vals)

        # Calculate zero proportion
        zero_prop <- n_zeros / total_obs

        # Calculate statistics on non-zero demands
        if (n_nonzeros > 0) {
          nonzero_vals <- x_vals[x_vals != 0]
          avg_nonzero <- mean(nonzero_vals, na.rm = TRUE)
          sd_nonzero <- stats::sd(nonzero_vals, na.rm = TRUE)
          cv_nonzero <- if (avg_nonzero != 0) sd_nonzero / avg_nonzero else NA_real_

          # Add all these statistics
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv(
              "engine_param", "n_zeros",
              as.character(n_zeros)
            ),
            .kv(
              "engine_param", "n_nonzeros",
              as.character(n_nonzeros)
            ),
            .kv(
              "engine_param", "nobs",
              as.character(total_obs)
            ),
            .kv(
              "engine_param", "zero_proportion",
              as.character(signif(zero_prop, digits))
            ),
            .kv(
              "engine_param", "avg_nonzero_demand",
              as.character(signif(avg_nonzero, digits))
            ),
            .kv(
              "engine_param", "sd_nonzero_demand",
              as.character(signif(sd_nonzero, digits))
            ),
            .kv(
              "engine_param", "cv_nonzero_demand",
              as.character(signif(cv_nonzero, digits))
            )
          )
        }

        # Calculate demand interval statistics
        if (n_zeros > 0) {
          # Create binary series (1 for demand, 0 for no demand)
          binary_series <- as.numeric(x_vals > 0)
          # Find runs of zeros
          zero_runs <- rle(binary_series)
          # Extract lengths of zero runs
          zero_lengths <- zero_runs$lengths[zero_runs$values == 0]

          if (length(zero_lengths) > 0) {
            # Calculate average interval between demands
            # +1 to include the demand point itself in the interval
            avg_interval <- mean(zero_lengths, na.rm = TRUE) + 1
            sd_interval <- stats::sd(zero_lengths, na.rm = TRUE)
            max_interval <- max(zero_lengths, na.rm = TRUE) + 1
            cv2_interval <- if (avg_interval != 0) (sd_interval / avg_interval)^2 else NA_real_

            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "engine_param", "avg_demand_interval",
                as.character(signif(avg_interval, digits))
              ),
              .kv(
                "engine_param", "sd_demand_interval",
                as.character(signif(sd_interval, digits))
              ),
              .kv(
                "engine_param", "max_demand_interval",
                as.character(signif(max_interval, digits))
              ),
              .kv(
                "engine_param", "cv2_interval",
                as.character(signif(cv2_interval, digits))
              )
            )
          }
        } else {
          # If no zeros, interval is always 1
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "avg_demand_interval", "1"),
            .kv("engine_param", "sd_demand_interval", "0"),
            .kv("engine_param", "max_demand_interval", "1"),
            .kv("engine_param", "cv2_interval", "0")
          )
        }
      }
    }

    # Extract alpha from the model (if not already set from extras)
    if (is.null(alpha_val)) {
      # Alpha might be in model$demand or elsewhere
      if (!is.null(croston_obj$model) && !is.null(croston_obj$model$alpha)) {
        alpha_val <- croston_obj$model$alpha
      } else if (!is.null(croston_obj$alpha)) {
        alpha_val <- croston_obj$alpha
      }

      if (!is.null(alpha_val) && is.numeric(alpha_val) && length(alpha_val) == 1 && is.finite(alpha_val)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "alpha",
          as.character(signif(alpha_val, digits))
        ))
      }
    }
  }

  # Return the combined table
  .assemble_output(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize a Cubist Workflow
#'
#' Extracts and summarizes key information from a fitted Cubist workflow,
#' including model arguments, engine parameters, and variable importance.
#' Supports both standard cubist models and multistep horizon models.
#'
#' @param wf A fitted tidymodels workflow containing a parsnip::cubist_rules()
#'   model with engine 'Cubist' or cubist_multistep with engine 'cubist_multistep_horizon'.
#'
#' @return A tibble with columns: section, name, value containing model details.
#'
#' @noRd
summarize_model_cubist <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  importance_threshold <- 1e-6 # Filter out negligible importance values

  if (!inherits(wf, "workflow")) stop("summarize_model_cubist() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine

  # Check if it's standard cubist or multistep
  is_multistep <- identical(engine, "cubist_multistep_horizon")

  if (!engine %in% c("Cubist", "cubist_multistep_horizon")) {
    stop("summarize_model_cubist() only supports parsnip::cubist_rules() with engine 'Cubist' or cubist_multistep with engine 'cubist_multistep_horizon'.")
  }

  # Find the cubist object(s)
  find_cubist <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "cubist") ||
        inherits(x, "cubist_multistep_fit_impl") ||
        (is.list(x) && !is.null(x$model) && inherits(x$model, "C5.0"))
    }, depth)
  }

  # Helper function to format numeric values
  .format_numeric <- function(x) {
    if (is.character(x) && (x == "auto" || x == "")) {
      return(x)
    }
    x_num <- suppressWarnings(as.numeric(x))
    if (!is.na(x_num) && is.finite(x_num)) {
      rounded <- round(x_num, digits)
      if (rounded == floor(rounded)) {
        return(as.character(as.integer(rounded)))
      } else {
        return(format(rounded, scientific = FALSE, trim = TRUE))
      }
    }
    return(as.character(x))
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # parsnip cubist args
  arg_names <- c("committees", "neighbors", "max_rules")
  arg_vals <- vapply(arg_names, function(nm) {
    val <- .chr1(spec$args[[nm]])
    .format_numeric(val)
  }, FUN.VALUE = character(1))
  args_tbl <- tibble::tibble(section = "model_arg", name = arg_names, value = arg_vals)

  # Find the cubist model object
  cubist_obj <- find_cubist(fit$fit, scan_depth)

  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  if (!is.null(cubist_obj)) {
    # Handle multistep vs standard cubist differently
    if (is_multistep && inherits(cubist_obj, "cubist_multistep_fit_impl")) {
      # MULTISTEP CUBIST

      # Extract individual models from $models list
      model_list <- cubist_obj$models
      n_models <- length(model_list)

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Multistep Horizon"))
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_models", as.character(n_models)))

      # Extract lag info from model names (e.g., "model_lag_1", "model_lag_3")
      lag_names <- names(model_list)
      if (!is.null(lag_names) && length(lag_names) > 0) {
        lags <- stringr::str_extract(lag_names, "[0-9]+")
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lag_horizons", paste(lags, collapse = ", ")))
      }

      # Aggregate metrics across all models
      all_nrules <- c()
      all_nfeatures <- c()

      # Collect importance from all models using vip
      all_importance <- list()

      for (model_name in names(model_list)) {
        cubist_model <- model_list[[model_name]]

        # The cubist model object is the result of rules::cubist_fit()
        inner_cubist <- NULL

        if (inherits(cubist_model, "cubist")) {
          inner_cubist <- cubist_model
        }

        if (!is.null(inner_cubist) && inherits(inner_cubist, "cubist")) {
          # Number of rules
          if (!is.null(inner_cubist$output)) {
            rules_output <- inner_cubist$output
            n_rules <- stringr::str_count(rules_output, "\\nRule ")
            if (is.finite(n_rules) && n_rules > 0) {
              all_nrules <- c(all_nrules, n_rules)
            }
          }

          # Number of features (variables used)
          if (!is.null(inner_cubist$vars)) {
            vars_used <- inner_cubist$vars$used
            if (!is.null(vars_used) && length(vars_used) > 0) {
              all_nfeatures <- c(all_nfeatures, length(vars_used))
            }
          }

          # Variable importance using vip::vi()
          importance <- try(vip::vi(inner_cubist, scale = TRUE) %>% suppressWarnings(), silent = TRUE)

          if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
            # Filter out features with negligible importance
            importance <- importance[importance$Importance > importance_threshold, ]

            if (nrow(importance) > 0) {
              for (i in seq_len(nrow(importance))) {
                feat_name <- importance$Variable[i]
                importance_val <- importance$Importance[i]

                # Accumulate importance across models
                if (is.null(all_importance[[feat_name]])) {
                  all_importance[[feat_name]] <- c()
                }
                all_importance[[feat_name]] <- c(all_importance[[feat_name]], importance_val)
              }
            }
          }
        }
      }

      # Add aggregated metrics
      if (length(all_nrules) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_n_rules", .format_numeric(mean(all_nrules))))
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "total_rules", as.character(sum(all_nrules))))
      }

      # Aggregate importance - average across all models (negligible values already excluded)
      importance_list <- list()

      if (length(all_importance) > 0) {
        # Calculate average importance for each feature
        avg_importance <- sapply(all_importance, mean)

        # Sort by importance (descending)
        sorted_idx <- order(avg_importance, decreasing = TRUE)
        sorted_names <- names(avg_importance)[sorted_idx]
        sorted_vals <- avg_importance[sorted_idx]

        # Add all averaged importance values with consistent formatting
        for (i in seq_along(sorted_names)) {
          importance_list[[length(importance_list) + 1]] <- .kv(
            "importance",
            sorted_names[i],
            .format_numeric(sorted_vals[i])
          )
        }
      }

      # Combine engine params and importance
      if (length(importance_list) > 0) {
        importance_tbl <- dplyr::bind_rows(importance_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
      }
    } else {
      # STANDARD CUBIST

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Standard"))

      # Extract cubist object if wrapped
      if (!inherits(cubist_obj, "cubist")) {
        inner_cubist <- try(cubist_obj$fit, silent = TRUE)
        if (!inherits(inner_cubist, "try-error") && inherits(inner_cubist, "cubist")) {
          cubist_obj <- inner_cubist
        }
      }

      if (inherits(cubist_obj, "cubist")) {
        # Number of rules
        if (!is.null(cubist_obj$output)) {
          rules_output <- cubist_obj$output
          n_rules <- stringr::str_count(rules_output, "\\nRule ")
          if (is.finite(n_rules) && n_rules > 0) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_rules", as.character(n_rules)))
          }
        }

        # Number of committees (from model structure)
        if (!is.null(cubist_obj$coefficients)) {
          n_committees <- length(cubist_obj$coefficients)
          if (is.finite(n_committees) && n_committees > 0) {
            # Update args_tbl if committees was tuned
            if (args_tbl$value[args_tbl$name == "committees"] == "") {
              args_tbl$value[args_tbl$name == "committees"] <- as.character(n_committees)
            }
          }
        }

        # Variable importance using vip::vi()
        importance <- try(vip::vi(cubist_obj, scale = TRUE) %>% suppressWarnings(), silent = TRUE)
        importance_list <- list()

        if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
          # Filter out features with negligible importance
          importance <- importance[importance$Importance > importance_threshold, ]

          if (nrow(importance) > 0) {
            # vip::vi already returns sorted by Importance (descending)
            for (i in seq_len(nrow(importance))) {
              feat_name <- importance$Variable[i]
              importance_val <- importance$Importance[i]

              importance_list[[length(importance_list) + 1]] <- .kv(
                "importance",
                feat_name,
                .format_numeric(importance_val)
              )
            }
          }
        }

        if (length(importance_list) > 0) {
          importance_tbl <- dplyr::bind_rows(importance_list)
          eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
        }

        # Sample coefficient information (from first committee)
        if (!is.null(cubist_obj$coefficients) && length(cubist_obj$coefficients) > 0) {
          first_coef <- cubist_obj$coefficients[[1]]
          if (!is.null(first_coef) && is.matrix(first_coef)) {
            n_predictors <- ncol(first_coef) - 1 # Exclude intercept
            if (n_predictors > 0) {
              eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_linear_predictors", as.character(n_predictors)))
            }
          }
        }

        # Model size information
        if (!is.null(cubist_obj$size)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_size_bytes", as.character(cubist_obj$size)))
        }
      }
    }
  }

  .assemble_output(
    preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize an ETS Workflow
#'
#' Extracts and summarizes key information from a fitted ETS (Error, Trend, Seasonal)
#' exponential smoothing workflow. Includes model components, smoothing parameters,
#' initial and final states, and model fit statistics.
#'
#' @param wf A fitted tidymodels workflow containing a modeltime::exp_smoothing()
#'   model with engine 'ets'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including ETS components, smoothing parameters (alpha, beta, gamma, phi),
#'   states, and information criteria.
#'
#' @noRd
summarize_model_ets <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6

  if (!inherits(wf, "workflow")) stop("summarize_model_ets() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "ets")) {
    stop("summarize_model_ets() only supports modeltime::exp_smoothing() with set_engine('ets').")
  }

  # Specific predicates for ETS
  is_ets <- function(o) {
    inherits(o, "ets") ||
      (is.list(o) && !is.null(o$method) && grepl("ETS", o$method)) ||
      (is.list(o) && !is.null(o$call) && grepl("ets", as.character(o$call[[1]])))
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Model args - ETS specific
  args_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Check for ETS-specific arguments
  ets_args <- c("error", "trend", "season", "damped")
  for (arg_name in ets_args) {
    if (!is.null(spec$args[[arg_name]])) {
      args_tbl <- dplyr::bind_rows(
        args_tbl,
        .kv("model_arg", arg_name, .chr1(spec$args[[arg_name]]))
      )
    }
  }

  # Engine params
  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Find the ETS model object - try multiple approaches
  engine_fit <- fit$fit
  ets_obj <- .find_obj(engine_fit, is_ets, scan_depth)

  # Also check if the fit itself is an ETS object
  if (is.null(ets_obj) && is_ets(engine_fit)) {
    ets_obj <- engine_fit
  }

  # Look for the models$model_1 structure (common in modeltime objects)
  if (is.null(ets_obj)) {
    if (!is.null(engine_fit$models) && !is.null(engine_fit$models$model_1)) {
      if (is_ets(engine_fit$models$model_1)) {
        ets_obj <- engine_fit$models$model_1
      }
    }
  }

  if (!is.null(ets_obj)) {
    # Store parsed components for later reuse
    parsed_components <- NULL

    # 1. Model Type and Components
    if (!is.null(ets_obj$method)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "method", as.character(ets_obj$method)))

      # Try to parse the method string (typically in format "ETS(A,A,A)")
      method_match <- regexpr("\\(([^)]+)\\)", ets_obj$method)
      if (method_match > 0) {
        components_str <- substr(
          ets_obj$method, method_match + 1,
          method_match + attr(method_match, "match.length") - 2
        )
        components <- strsplit(components_str, ",")[[1]]
        parsed_components <- components # Store for later use

        # Add components individually (handle variable length)
        if (length(components) >= 1) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "error_component", trimws(components[1])))
        }
        if (length(components) >= 2) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "trend_component", trimws(components[2])))
        }
        if (length(components) >= 3) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "seasonal_component", trimws(components[3])))
        }

        # Check for damping in trend component (indicated by "Ad", "Md", etc.)
        if (length(components) >= 2) {
          trend_comp <- trimws(components[2])
          if (grepl("[AMN]d$", trend_comp, ignore.case = TRUE)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "damped", "TRUE"))
          } else if (trend_comp != "N") {
            # Has trend but no damping
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "damped", "FALSE"))
          }
        }
      }
    }

    # 2. Smoothing parameters (alpha, beta, gamma, phi)
    param_names <- c("alpha", "beta", "gamma", "phi")
    for (param in param_names) {
      if (!is.null(ets_obj[[param]])) {
        param_val <- ets_obj[[param]]
        if (is.numeric(param_val) && length(param_val) == 1 && is.finite(param_val)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", param, as.character(signif(param_val, digits)))
          )
        }
      }
    }

    # Also try extracting from $par if parameters not found above
    if (!is.null(ets_obj$par) && is.numeric(ets_obj$par)) {
      par_vec <- ets_obj$par
      par_names <- names(par_vec)

      # If no names, try to infer based on parsed method components
      if (is.null(par_names) && !is.null(parsed_components) && length(parsed_components) >= 3) {
        # Create expected parameter names based on ETS components
        expected_names <- character()
        error_comp <- trimws(parsed_components[1])
        trend_comp <- trimws(parsed_components[2])
        season_comp <- trimws(parsed_components[3])

        # Alpha always present for error component
        if (error_comp != "N") expected_names <- c(expected_names, "alpha")
        # Beta present if trend component exists
        if (trend_comp != "N") expected_names <- c(expected_names, "beta")
        # Gamma present if seasonal component exists
        if (season_comp != "N") expected_names <- c(expected_names, "gamma")
        # Phi present if trend is damped
        if (grepl("d$", trend_comp, ignore.case = TRUE)) expected_names <- c(expected_names, "phi")

        if (length(expected_names) == length(par_vec)) {
          par_names <- expected_names
        }
      }

      # Add parameters that weren't already added
      if (!is.null(par_names)) {
        for (i in seq_along(par_vec)) {
          param_name <- par_names[i]
          if (!any(eng_tbl$name == param_name) && is.finite(par_vec[i])) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv("engine_param", param_name, as.character(signif(par_vec[i], digits)))
            )
          }
        }
      } else {
        # Fallback: add as par[1], par[2], etc.
        for (i in seq_along(par_vec)) {
          if (is.finite(par_vec[i])) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv("engine_param", paste0("par[", i, "]"), as.character(signif(par_vec[i], digits)))
            )
          }
        }
      }
    }

    # 3. Initial states (level, trend, seasonal)
    state_names <- c("l", "b", "s") # level, trend, seasonal
    seen_states <- character(0)

    # Handle direct state access (single values)
    for (state_name in state_names) {
      if (!is.null(ets_obj[[state_name]])) {
        state_val <- ets_obj[[state_name]]
        if (is.numeric(state_val) && length(state_val) == 1 && is.finite(state_val)) {
          if (state_name == "l") {
            display_name <- "initial_level"
          } else if (state_name == "b") {
            display_name <- "initial_trend"
          } else if (state_name == "s") {
            display_name <- "initial_seasonal1"
          }

          seen_states <- c(seen_states, state_name)
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", display_name, as.character(signif(state_val, digits)))
          )
        }
      }
    }

    # Handle indexed seasonal states (try both 0-indexed and 1-indexed patterns)
    # ETS models can use s0, s1, s2, ... or s1, s2, s3, ... depending on implementation
    seasonal_states_found <- FALSE

    for (start_idx in 0:1) {
      if (seasonal_states_found) break

      i <- start_idx
      temp_states <- character(0)

      # Try to find sequential seasonal states
      while (i < 100) { # Safety limit to prevent infinite loop
        state_name <- paste0("s", i)

        if (is.null(ets_obj[[state_name]])) {
          # If we found at least one state, we're done with this pattern
          if (length(temp_states) > 0) {
            seasonal_states_found <- TRUE
            break
          }
          # If this is the first attempt and we found nothing, try next pattern
          if (i == start_idx) {
            break
          }
        } else {
          state_val <- ets_obj[[state_name]]
          if (is.numeric(state_val) && length(state_val) == 1 && is.finite(state_val) &&
            !(state_name %in% seen_states)) {
            # Display as 1-indexed for user-friendly output
            display_idx <- if (start_idx == 0) i + 1 else i
            display_name <- paste0("initial_seasonal", display_idx)

            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv("engine_param", display_name, as.character(signif(state_val, digits)))
            )

            temp_states <- c(temp_states, state_name)
            seen_states <- c(seen_states, state_name)
          }
        }
        i <- i + 1
      }
    }

    # 4. Error variance (sigma2) - fundamental to ETS models
    if (!is.null(ets_obj$sigma2)) {
      sigma_val <- ets_obj$sigma2
      if (is.numeric(sigma_val) && length(sigma_val) == 1 && is.finite(sigma_val)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "sigma2", as.character(signif(sigma_val, digits)))
        )
      }
    }

    # 5. Residual statistics and accuracy metrics
    if (!is.null(ets_obj$residuals)) {
      resids <- ets_obj$residuals
      if (is.numeric(resids) && length(resids) > 0) {
        # Calculate key metrics
        mae <- mean(abs(resids), na.rm = TRUE)
        rmse <- sqrt(mean(resids^2, na.rm = TRUE))
        resid_mean <- mean(resids, na.rm = TRUE)
        resid_sd <- sd(resids, na.rm = TRUE)

        # Add all metrics if they're valid
        if (is.finite(mae)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "mae", as.character(signif(mae, digits)))
          )
        }
        if (is.finite(rmse)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "rmse", as.character(signif(rmse, digits)))
          )
        }
        if (is.finite(resid_mean)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "residuals.mean", as.character(signif(resid_mean, digits)))
          )
        }
        if (is.finite(resid_sd)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "residuals.sd", as.character(signif(resid_sd, digits)))
          )
        }
      }
    }

    # 6. Information criteria and model fit statistics
    for (criterion in c("aic", "aicc", "bic", "loglik")) {
      if (!is.null(ets_obj[[criterion]])) {
        criterion_val <- ets_obj[[criterion]]
        if (is.numeric(criterion_val) && length(criterion_val) == 1 && is.finite(criterion_val)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", criterion, as.character(signif(criterion_val, digits)))
          )
        }
      }
    }

    # 7. Seasonal frequency
    if (!is.null(ets_obj$m) || !is.null(ets_obj$frequency)) {
      freq_val <- if (!is.null(ets_obj$m)) ets_obj$m else ets_obj$frequency
      if (is.numeric(freq_val) && is.finite(freq_val)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "seasonal_frequency", as.character(freq_val))
        )
      }
    }

    # 8. Forecast values - look in multiple places
    forecast_val <- NULL

    # Try 'mean' field first (common in forecast objects)
    if (is.null(forecast_val) && !is.null(ets_obj$mean)) {
      if (is.numeric(ets_obj$mean) && length(ets_obj$mean) > 0) {
        forecast_val <- ets_obj$mean[1]
      }
    }

    # Try 'forecast' field
    if (is.null(forecast_val) && !is.null(ets_obj$forecast)) {
      if (is.numeric(ets_obj$forecast) && length(ets_obj$forecast) > 0) {
        forecast_val <- ets_obj$forecast[1]
      } else if (is.list(ets_obj$forecast) && !is.null(ets_obj$forecast$mean)) {
        if (is.numeric(ets_obj$forecast$mean) && length(ets_obj$forecast$mean) > 0) {
          forecast_val <- ets_obj$forecast$mean[1]
        }
      }
    }

    # Add forecast value if found and valid
    if (!is.null(forecast_val) && is.numeric(forecast_val) && is.finite(forecast_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "forecast_value", as.character(signif(forecast_val, digits)))
      )
    }

    # 9. Number of observations
    if (!is.null(ets_obj$nobs) || !is.null(ets_obj$n)) {
      n_obs <- if (!is.null(ets_obj$nobs)) ets_obj$nobs else ets_obj$n
      if (is.numeric(n_obs) && is.finite(n_obs)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "nobs", as.character(n_obs))
        )
      }
    }

    # 10. Final states - from states matrix
    if (!is.null(ets_obj$states)) {
      states <- ets_obj$states
      if (is.matrix(states) && nrow(states) > 0) {
        final_states <- states[nrow(states), ]
        state_names_matrix <- colnames(states)

        for (i in seq_along(final_states)) {
          state_val <- final_states[i]

          # Determine state name based on column name or position
          if (!is.null(state_names_matrix) && length(state_names_matrix) >= i) {
            state_name <- state_names_matrix[i]
          } else if (i == 1) {
            state_name <- "l" # level
          } else if (i == 2) {
            state_name <- "b" # trend
          } else {
            state_name <- paste0("s", i - 2) # seasonal components
          }

          # Format the state name for display
          if (state_name == "l") {
            display_name <- "final_level"
          } else if (state_name == "b") {
            display_name <- "final_trend"
          } else if (grepl("^s", state_name)) {
            # Extract the number from seasonal component (s0, s1, s2, etc.)
            s_num <- gsub("^s", "", state_name)
            if (grepl("^[0-9]+$", s_num)) {
              s_num <- as.numeric(s_num)
              # Display as 1-indexed for user-friendly output
              display_name <- paste0("final_seasonal", s_num + 1)
            } else {
              # If it's not a number (e.g., "s"), just use generic naming
              display_name <- paste0("final_", state_name)
            }
          } else {
            display_name <- paste0("final_", state_name)
          }

          if (is.numeric(state_val) && is.finite(state_val)) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv("engine_param", display_name, as.character(signif(state_val, digits)))
            )
          }
        }
      }
    }
  }

  # Return the combined table
  .assemble_output(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize a GLMNET Workflow
#'
#' Extracts and summarizes key information from a fitted glmnet workflow,
#' including model arguments, engine parameters, and coefficients. Supports
#' both standard glmnet (parsnip::linear_reg) and multistep glmnet models.
#' For multistep models, coefficients are averaged across all lag horizons.
#'
#' @param wf A fitted tidymodels workflow containing a parsnip::linear_reg()
#'   model with engine 'glmnet' or a glmnet_multistep model with engine
#'   'glmnet_multistep_horizon'.
#'
#' @return A tibble with columns: model_class, engine, section, name, value
#'   containing model details including penalty type (Ridge/Lasso/Elastic Net),
#'   lambda values, degrees of freedom, deviance ratio, and all non-zero
#'   coefficients sorted by absolute value.
#'
#' @noRd
summarize_model_glmnet <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  importance_threshold <- 1e-6 # Filter out negligible importance values

  if (!inherits(wf, "workflow")) stop("summarize_model_glmnet() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine

  # Check if it's standard glmnet or multistep
  is_multistep <- identical(engine, "glmnet_multistep_horizon")

  if (!engine %in% c("glmnet", "glmnet_multistep_horizon")) {
    stop("summarize_model_glmnet() only supports parsnip::linear_reg() with engine 'glmnet' or glmnet_multistep with engine 'glmnet_multistep_horizon'.")
  }

  # Find the glmnet object(s)
  find_glmnet <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "glmnet") || inherits(x, "elnet") ||
        inherits(x, "glmnet_multistep_fit_impl") ||
        (is.list(x) && !is.null(x$lambda) && !is.null(x$beta))
    }, depth)
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # parsnip glmnet args - only include penalty and mixture for both standard and multistep
  arg_names <- c("penalty", "mixture")
  arg_vals <- vapply(arg_names, function(nm) .chr1(spec$args[[nm]]), FUN.VALUE = character(1))
  args_tbl <- tibble::tibble(section = "model_arg", name = arg_names, value = arg_vals)

  # Find the glmnet model object
  glmnet_obj <- find_glmnet(fit$fit, scan_depth)

  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  if (!is.null(glmnet_obj)) {
    # Handle multistep vs standard glmnet differently
    if (is_multistep && inherits(glmnet_obj, "glmnet_multistep_fit_impl")) {
      # MULTISTEP GLMNET

      # Extract individual models from $models list
      model_list <- glmnet_obj$models
      n_models <- length(model_list)

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Multistep Horizon"))
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_models", as.character(n_models)))

      # Extract lag info from model names (e.g., "model_lag_1", "model_lag_3")
      lag_names <- names(model_list)
      if (!is.null(lag_names) && length(lag_names) > 0) {
        lags <- stringr::str_extract(lag_names, "[0-9]+")
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lag_horizons", paste(lags, collapse = ", ")))
      }

      # Get mixture value to determine penalty type
      mixture_val <- suppressWarnings(as.numeric(args_tbl$value[args_tbl$name == "mixture"]))

      # Try to get alpha from first model if mixture was "auto"
      if (is.na(mixture_val) || args_tbl$value[args_tbl$name == "mixture"] == "auto") {
        first_model <- model_list[[1]]
        first_glmnet <- try(first_model$fit, silent = TRUE)
        if (!inherits(first_glmnet, "try-error") && !is.null(first_glmnet$alpha)) {
          mixture_val <- first_glmnet$alpha
          args_tbl$value[args_tbl$name == "mixture"] <- as.character(signif(mixture_val, digits))
        }
      }

      # Add penalty type interpretation
      if (!is.na(mixture_val) && is.finite(mixture_val)) {
        if (mixture_val == 0) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "penalty_type", "Ridge (L2)"))
        } else if (mixture_val == 1) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "penalty_type", "Lasso (L1)"))
        } else {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "penalty_type", "Elastic Net"))
        }
      }

      # Aggregate metrics across all models
      all_dev_ratios <- c()
      all_dfs <- c()
      all_nobs <- c()
      all_lambdas <- c()

      # Collect coefficients from all models
      all_coefficients <- list()
      all_intercepts <- c()

      # Collect importance from all models using vip
      all_importance <- list()

      for (model_name in names(model_list)) {
        model_fit <- model_list[[model_name]]

        # Extract the underlying glmnet object - only ONE level of $fit
        inner_glmnet <- try(model_fit$fit, silent = TRUE)

        if (!inherits(inner_glmnet, "try-error") && !is.null(inner_glmnet)) {
          # Dev ratio
          dev_ratio <- try(inner_glmnet$dev.ratio, silent = TRUE)
          if (!inherits(dev_ratio, "try-error") && !is.null(dev_ratio)) {
            if (length(dev_ratio) > 1) {
              all_dev_ratios <- c(all_dev_ratios, dev_ratio[length(dev_ratio)])
            } else if (length(dev_ratio) == 1) {
              all_dev_ratios <- c(all_dev_ratios, dev_ratio)
            }
          }

          # Degrees of freedom
          df <- try(inner_glmnet$df, silent = TRUE)
          if (!inherits(df, "try-error") && !is.null(df)) {
            if (length(df) > 1) {
              all_dfs <- c(all_dfs, df[length(df)])
            } else if (length(df) == 1) {
              all_dfs <- c(all_dfs, df)
            }
          }

          # Nobs
          nobs <- try(inner_glmnet$nobs, silent = TRUE)
          if (!inherits(nobs, "try-error") && !is.null(nobs) && is.finite(nobs)) {
            all_nobs <- c(all_nobs, nobs)
          }

          # Lambda
          lambda_val <- try(inner_glmnet$lambda, silent = TRUE)
          if (!inherits(lambda_val, "try-error") && !is.null(lambda_val)) {
            if (length(lambda_val) > 1) {
              all_lambdas <- c(all_lambdas, lambda_val[length(lambda_val)])
            } else if (length(lambda_val) == 1) {
              all_lambdas <- c(all_lambdas, lambda_val)
            }
          }

          # Intercept
          a0 <- try(inner_glmnet$a0, silent = TRUE)
          if (!inherits(a0, "try-error") && !is.null(a0)) {
            if (length(a0) > 1) {
              all_intercepts <- c(all_intercepts, a0[length(a0)])
            } else if (length(a0) == 1) {
              all_intercepts <- c(all_intercepts, a0)
            }
          }

          # Coefficients
          beta <- try(inner_glmnet$beta, silent = TRUE)
          if (!inherits(beta, "try-error") && !is.null(beta)) {
            if (inherits(beta, "dgCMatrix") || is.matrix(beta)) {
              beta_mat <- as.matrix(beta)

              # Get last column if multiple lambdas
              if (ncol(beta_mat) > 1) {
                beta_vec <- beta_mat[, ncol(beta_mat)]
              } else {
                beta_vec <- beta_mat[, 1]
              }

              coef_names <- rownames(beta_mat)
              if (is.null(coef_names)) {
                coef_names <- paste0("V", seq_along(beta_vec))
              }

              # Store non-zero coefficients
              for (i in seq_along(beta_vec)) {
                if (abs(beta_vec[i]) > 1e-10) {
                  if (is.null(all_coefficients[[coef_names[i]]])) {
                    all_coefficients[[coef_names[i]]] <- c()
                  }
                  all_coefficients[[coef_names[i]]] <- c(all_coefficients[[coef_names[i]]], beta_vec[i])
                }
              }
            }
          }

          # Variable importance using vip::vi()
          importance <- try(vip::vi(inner_glmnet, scale = TRUE) %>% suppressWarnings(), silent = TRUE)

          if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
            # Filter out features with negligible importance
            importance <- importance[importance$Importance > importance_threshold, ]

            if (nrow(importance) > 0) {
              for (i in seq_len(nrow(importance))) {
                feat_name <- importance$Variable[i]
                importance_val <- importance$Importance[i]

                # Accumulate importance across models
                if (is.null(all_importance[[feat_name]])) {
                  all_importance[[feat_name]] <- c()
                }
                all_importance[[feat_name]] <- c(all_importance[[feat_name]], importance_val)
              }
            }
          }
        }
      }

      # Add aggregated metrics
      if (length(all_dev_ratios) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_dev_ratio", as.character(signif(mean(all_dev_ratios), digits))))
      }

      if (length(all_dfs) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_df", as.character(round(mean(all_dfs)))))
      }

      if (length(all_nobs) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "nobs", as.character(all_nobs[1]))) # Should be same for all
      }

      if (length(all_lambdas) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_lambda", as.character(signif(mean(all_lambdas), digits))))
      }

      # Aggregate coefficients - average across all models
      coef_list <- list()

      if (length(all_intercepts) > 0) {
        avg_intercept <- mean(all_intercepts)
        coef_list[[length(coef_list) + 1]] <- .kv("coefficient", "(Intercept)", as.character(signif(avg_intercept, digits)))
      }

      if (length(all_coefficients) > 0) {
        # Calculate average coefficient for each feature
        avg_coefs <- sapply(all_coefficients, mean)

        # Sort by absolute value (descending)
        sorted_idx <- order(abs(avg_coefs), decreasing = TRUE)
        sorted_names <- names(avg_coefs)[sorted_idx]
        sorted_vals <- avg_coefs[sorted_idx]

        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_features_used", as.character(length(sorted_names))))

        # Add all averaged coefficients
        for (i in seq_along(sorted_names)) {
          coef_list[[length(coef_list) + 1]] <- .kv(
            "coefficient",
            sorted_names[i],
            as.character(signif(sorted_vals[i], digits))
          )
        }
      }

      # Combine engine params and coefficients
      if (length(coef_list) > 0) {
        coef_tbl <- dplyr::bind_rows(coef_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, coef_tbl)
      }

      # Aggregate importance - average across all models (negligible values already excluded)
      importance_list <- list()

      if (length(all_importance) > 0) {
        # Calculate average importance for each feature
        avg_importance <- sapply(all_importance, mean)

        # Sort by importance (descending)
        sorted_idx <- order(avg_importance, decreasing = TRUE)
        sorted_names <- names(avg_importance)[sorted_idx]
        sorted_vals <- avg_importance[sorted_idx]

        # Add all averaged importance values with consistent formatting
        for (i in seq_along(sorted_names)) {
          importance_list[[length(importance_list) + 1]] <- .kv(
            "importance",
            sorted_names[i],
            as.character(signif(sorted_vals[i], digits))
          )
        }
      }

      # Add importance to eng_tbl
      if (length(importance_list) > 0) {
        importance_tbl <- dplyr::bind_rows(importance_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
      }
    } else {
      # STANDARD GLMNET

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Standard"))

      # Get mixture value to determine penalty type
      mixture_val <- suppressWarnings(as.numeric(args_tbl$value[args_tbl$name == "mixture"]))

      # Try to get alpha from glmnet object if mixture was "auto"
      if (is.na(mixture_val) || args_tbl$value[args_tbl$name == "mixture"] == "auto") {
        alpha_val <- try(glmnet_obj$alpha, silent = TRUE)
        if (!inherits(alpha_val, "try-error") && !is.null(alpha_val) && length(alpha_val) == 1 && is.finite(alpha_val)) {
          mixture_val <- alpha_val
          args_tbl$value[args_tbl$name == "mixture"] <- as.character(signif(alpha_val, digits))
        }
      }

      # Add penalty type interpretation
      if (!is.na(mixture_val) && is.finite(mixture_val)) {
        if (mixture_val == 0) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "penalty_type", "Ridge (L2)"))
        } else if (mixture_val == 1) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "penalty_type", "Lasso (L1)"))
        } else {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "penalty_type", "Elastic Net"))
        }
      }

      # Lambda (penalty) information
      lambda_val <- try(glmnet_obj$lambda, silent = TRUE)
      if (!inherits(lambda_val, "try-error") && !is.null(lambda_val)) {
        if (length(lambda_val) == 1) {
          if (args_tbl$value[args_tbl$name == "penalty"] == "auto") {
            args_tbl$value[args_tbl$name == "penalty"] <- as.character(signif(lambda_val, digits))
          }
        } else if (length(lambda_val) > 1) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_lambda", as.character(length(lambda_val))))
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lambda_min", as.character(signif(min(lambda_val), digits))))
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lambda_max", as.character(signif(max(lambda_val), digits))))

          if (args_tbl$value[args_tbl$name == "penalty"] == "auto") {
            selected_lambda <- lambda_val[length(lambda_val)]
            args_tbl$value[args_tbl$name == "penalty"] <- as.character(signif(selected_lambda, digits))
          }
        }
      }

      # Number of observations
      nobs <- try(glmnet_obj$nobs, silent = TRUE)
      if (!inherits(nobs, "try-error") && !is.null(nobs) && is.finite(nobs)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "nobs", as.character(nobs)))
      }

      # Number of passes
      npasses <- try(glmnet_obj$npasses, silent = TRUE)
      if (!inherits(npasses, "try-error") && !is.null(npasses) && is.finite(npasses)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "npasses", as.character(npasses)))
      }

      # Degrees of freedom
      df <- try(glmnet_obj$df, silent = TRUE)
      if (!inherits(df, "try-error") && !is.null(df)) {
        if (length(df) == 1 && is.finite(df)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "df", as.character(df)))
        } else if (length(df) > 1) {
          df_final <- df[length(df)]
          if (is.finite(df_final)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "df", as.character(df_final)))
          }
        }
      }

      # Deviance explained
      dev_ratio <- try(glmnet_obj$dev.ratio, silent = TRUE)
      if (!inherits(dev_ratio, "try-error") && !is.null(dev_ratio)) {
        if (length(dev_ratio) == 1 && is.finite(dev_ratio)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "dev_ratio", as.character(signif(dev_ratio, digits))))
        } else if (length(dev_ratio) > 1) {
          dev_final <- dev_ratio[length(dev_ratio)]
          if (is.finite(dev_final)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "dev_ratio", as.character(signif(dev_final, digits))))
          }
        }
      }

      # Null deviance
      nulldev <- try(glmnet_obj$nulldev, silent = TRUE)
      if (!inherits(nulldev, "try-error") && !is.null(nulldev) && is.finite(nulldev)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "nulldev", as.character(signif(nulldev, digits))))
      }

      # Offset
      offset <- try(glmnet_obj$offset, silent = TRUE)
      if (!inherits(offset, "try-error") && !is.null(offset) && isTRUE(offset)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "offset", "TRUE"))
      }

      # Intercept
      a0 <- try(glmnet_obj$a0, silent = TRUE)
      intercept_val <- NULL
      if (!inherits(a0, "try-error") && !is.null(a0)) {
        if (length(a0) == 1 && is.finite(a0)) {
          intercept_val <- a0
        } else if (length(a0) > 1) {
          a0_final <- a0[length(a0)]
          if (is.finite(a0_final)) {
            intercept_val <- a0_final
          }
        }
      }

      # Coefficients (beta)
      beta <- try(glmnet_obj$beta, silent = TRUE)
      coef_list <- list()

      if (!inherits(beta, "try-error") && !is.null(beta)) {
        if (inherits(beta, "dgCMatrix") || is.matrix(beta)) {
          beta_mat <- as.matrix(beta)

          if (ncol(beta_mat) > 1) {
            beta_vec <- beta_mat[, ncol(beta_mat)]
          } else {
            beta_vec <- beta_mat[, 1]
          }

          coef_names <- rownames(beta_mat)
          if (is.null(coef_names)) {
            coef_names <- paste0("V", seq_along(beta_vec))
          }

          nonzero_idx <- which(abs(beta_vec) > 1e-10)

          if (length(nonzero_idx) > 0) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_nonzero_coefs", as.character(length(nonzero_idx))))

            beta_sorted <- sort(abs(beta_vec[nonzero_idx]), decreasing = TRUE, index.return = TRUE)
            sorted_idx <- nonzero_idx[beta_sorted$ix]

            if (!is.null(intercept_val)) {
              coef_list[[length(coef_list) + 1]] <- .kv("coefficient", "(Intercept)", as.character(signif(intercept_val, digits)))
            }

            for (i in seq_along(sorted_idx)) {
              idx <- sorted_idx[i]
              coef_list[[length(coef_list) + 1]] <- .kv("coefficient", coef_names[idx], as.character(signif(beta_vec[idx], digits)))
            }
          } else {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_nonzero_coefs", "0"))
            if (!is.null(intercept_val)) {
              coef_list[[length(coef_list) + 1]] <- .kv("coefficient", "(Intercept)", as.character(signif(intercept_val, digits)))
            }
          }
        }
      } else {
        if (!is.null(intercept_val)) {
          coef_list[[length(coef_list) + 1]] <- .kv("coefficient", "(Intercept)", as.character(signif(intercept_val, digits)))
        }
      }

      if (length(coef_list) > 0) {
        coef_tbl <- dplyr::bind_rows(coef_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, coef_tbl)
      }

      # Variable importance using vip::vi()
      importance <- try(vip::vi(glmnet_obj, scale = TRUE) %>% suppressWarnings(), silent = TRUE)
      importance_list <- list()

      if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
        # Filter out features with negligible importance
        importance <- importance[importance$Importance > importance_threshold, ]

        if (nrow(importance) > 0) {
          # vip::vi already returns sorted by Importance (descending)
          for (i in seq_len(nrow(importance))) {
            feat_name <- importance$Variable[i]
            importance_val <- importance$Importance[i]

            importance_list[[length(importance_list) + 1]] <- .kv(
              "importance",
              feat_name,
              as.character(signif(importance_val, digits))
            )
          }
        }
      }

      if (length(importance_list) > 0) {
        importance_tbl <- dplyr::bind_rows(importance_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
      }
    }
  }

  .assemble_output(
    preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize a MARS Workflow
#'
#' Extracts and summarizes key information from a fitted MARS (Multivariate
#' Adaptive Regression Splines) workflow. MARS builds flexible regression
#' models using piecewise linear basis functions (hinge functions) and
#' automatic feature selection. Supports both standard earth models and
#' multistep horizon models.
#'
#' @param wf A fitted tidymodels workflow containing a parsnip::mars()
#'   model with engine 'earth' or mars_multistep with engine 'mars_multistep_horizon'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including number of terms, interaction degree, pruning method, basis
#'   functions, and variable importance.
#'
#' @noRd
summarize_model_mars <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  importance_threshold <- 1e-6 # Filter out negligible importance values

  if (!inherits(wf, "workflow")) stop("summarize_model_mars() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine

  # Check if it's standard mars or multistep
  is_multistep <- identical(engine, "mars_multistep_horizon")

  if (!engine %in% c("earth", "mars_multistep_horizon")) {
    stop("summarize_model_mars() only supports parsnip::mars() with engine 'earth' or mars_multistep with engine 'mars_multistep_horizon'.")
  }

  # Find the mars/earth object(s)
  find_mars <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "earth") ||
        inherits(x, "mars_multistep_fit_impl") ||
        (is.list(x) && !is.null(x$coefficients) && !is.null(x$selected.terms))
    }, depth)
  }

  # Helper function to format numeric values
  .format_numeric <- function(x) {
    if (is.character(x) && (x == "auto" || x == "")) {
      return(x)
    }
    x_num <- suppressWarnings(as.numeric(x))
    if (!is.na(x_num) && is.finite(x_num)) {
      rounded <- round(x_num, digits)
      if (rounded == floor(rounded)) {
        return(as.character(as.integer(rounded)))
      } else {
        return(format(rounded, scientific = FALSE, trim = TRUE))
      }
    }
    return(as.character(x))
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # parsnip mars args
  arg_names <- c("num_terms", "prod_degree", "prune_method")
  arg_vals <- vapply(arg_names, function(nm) {
    val <- .chr1(spec$args[[nm]])
    .format_numeric(val)
  }, FUN.VALUE = character(1))
  args_tbl <- tibble::tibble(section = "model_arg", name = arg_names, value = arg_vals)

  # Find the mars model object
  mars_obj <- find_mars(fit$fit, scan_depth)

  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  if (!is.null(mars_obj)) {
    # Handle multistep vs standard mars differently
    if (is_multistep && inherits(mars_obj, "mars_multistep_fit_impl")) {
      # MULTISTEP MARS

      # Extract individual models from $models list
      model_list <- mars_obj$models
      n_models <- length(model_list)

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Multistep Horizon"))
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_models", as.character(n_models)))

      # Extract lag info from model names (e.g., "model_lag_1", "model_lag_3")
      lag_names <- names(model_list)
      if (!is.null(lag_names) && length(lag_names) > 0) {
        lags <- stringr::str_extract(lag_names, "[0-9]+")
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lag_horizons", paste(lags, collapse = ", ")))
      }

      # Aggregate metrics across all models
      all_nterms <- c()
      all_nsubsets <- c()
      all_rsq <- c()
      all_gcv <- c()

      # Collect importance from all models
      all_importance <- list()

      for (model_name in names(model_list)) {
        mars_model <- model_list[[model_name]]

        # The mars model is wrapped in a parsnip fit object
        inner_mars <- NULL

        if (inherits(mars_model, "model_fit")) {
          inner_mars <- try(mars_model$fit, silent = TRUE)
          if (inherits(inner_mars, "try-error")) inner_mars <- NULL
        } else if (inherits(mars_model, "earth")) {
          inner_mars <- mars_model
        }

        if (!is.null(inner_mars) && inherits(inner_mars, "earth")) {
          # Number of terms (including intercept)
          if (!is.null(inner_mars$selected.terms)) {
            n_terms <- length(inner_mars$selected.terms)
            if (is.finite(n_terms) && n_terms > 0) {
              all_nterms <- c(all_nterms, n_terms)
            }
          }

          # Number of subsets evaluated during pruning
          if (!is.null(inner_mars$nsubsets)) {
            all_nsubsets <- c(all_nsubsets, inner_mars$nsubsets)
          }

          # R-squared
          if (!is.null(inner_mars$rsq)) {
            all_rsq <- c(all_rsq, inner_mars$rsq)
          }

          # GCV (Generalized Cross-Validation)
          if (!is.null(inner_mars$gcv)) {
            all_gcv <- c(all_gcv, inner_mars$gcv)
          }

          # Variable importance using vip::vi()
          importance <- try(vip::vi(inner_mars, scale = TRUE) %>% suppressWarnings(), silent = TRUE)

          if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
            # Filter out features with negligible importance
            importance <- importance[importance$Importance > importance_threshold, ]

            if (nrow(importance) > 0) {
              for (i in seq_len(nrow(importance))) {
                feat_name <- importance$Variable[i]
                importance_val <- importance$Importance[i]

                # Accumulate importance across models
                if (is.null(all_importance[[feat_name]])) {
                  all_importance[[feat_name]] <- c()
                }
                all_importance[[feat_name]] <- c(all_importance[[feat_name]], importance_val)
              }
            }
          }
        }
      }

      # Add aggregated metrics
      if (length(all_nterms) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_n_terms", .format_numeric(mean(all_nterms))))
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "total_terms", as.character(sum(all_nterms))))
      }

      if (length(all_nsubsets) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_nsubsets", .format_numeric(mean(all_nsubsets))))
      }

      if (length(all_rsq) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_rsq", .format_numeric(mean(all_rsq))))
      }

      if (length(all_gcv) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_gcv", .format_numeric(mean(all_gcv))))
      }

      # Aggregate importance - average across all models (negligible values already excluded)
      importance_list <- list()

      if (length(all_importance) > 0) {
        # Calculate average importance for each feature
        avg_importance <- sapply(all_importance, mean)

        # Sort by importance (descending)
        sorted_idx <- order(avg_importance, decreasing = TRUE)
        sorted_names <- names(avg_importance)[sorted_idx]
        sorted_vals <- avg_importance[sorted_idx]

        # Add all averaged importance values with consistent formatting
        for (i in seq_along(sorted_names)) {
          importance_list[[length(importance_list) + 1]] <- .kv(
            "importance",
            sorted_names[i],
            .format_numeric(sorted_vals[i])
          )
        }
      }

      # Combine engine params and importance
      if (length(importance_list) > 0) {
        importance_tbl <- dplyr::bind_rows(importance_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
      }
    } else {
      # STANDARD MARS

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Standard"))

      # Extract earth object if wrapped
      if (!inherits(mars_obj, "earth")) {
        inner_mars <- try(mars_obj$fit, silent = TRUE)
        if (!inherits(inner_mars, "try-error") && inherits(inner_mars, "earth")) {
          mars_obj <- inner_mars
        }
      }

      if (inherits(mars_obj, "earth")) {
        # Update args_tbl if num_terms was tuned
        if (!is.null(mars_obj$selected.terms)) {
          n_terms <- length(mars_obj$selected.terms)
          if (is.finite(n_terms) && n_terms > 0) {
            if (args_tbl$value[args_tbl$name == "num_terms"] == "") {
              args_tbl$value[args_tbl$name == "num_terms"] <- as.character(n_terms)
            }
          }
        }

        # Number of basis functions in full model (before pruning)
        if (!is.null(mars_obj$dirs)) {
          n_basis <- nrow(mars_obj$dirs)
          if (is.finite(n_basis) && n_basis > 0) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_basis_functions", as.character(n_basis)))
          }
        }

        # Maximum degree of interaction actually used
        if (!is.null(mars_obj$degree.used)) {
          degree_used <- mars_obj$degree.used
          if (is.finite(degree_used)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "degree_used", as.character(degree_used)))
          }
        }

        # Penalty parameter
        if (!is.null(mars_obj$penalty)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "penalty", .format_numeric(mars_obj$penalty)))
        }

        # Number of subsets evaluated during pruning
        if (!is.null(mars_obj$nsubsets)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_subsets_evaluated", as.character(mars_obj$nsubsets)))
        }

        # R-squared
        if (!is.null(mars_obj$rsq)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "rsq", .format_numeric(mars_obj$rsq)))
        }

        # GCV (Generalized Cross-Validation)
        if (!is.null(mars_obj$gcv)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "gcv", .format_numeric(mars_obj$gcv)))
        }

        # Number of observations
        if (!is.null(mars_obj$nobs)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_obs", as.character(mars_obj$nobs)))
        }

        # Pruning method used
        if (!is.null(mars_obj$pmethod)) {
          pmethod <- mars_obj$pmethod
          if (args_tbl$value[args_tbl$name == "prune_method"] == "") {
            args_tbl$value[args_tbl$name == "prune_method"] <- as.character(pmethod)
          }
        }

        # Variable importance using vip::vi()
        importance <- try(vip::vi(mars_obj, scale = TRUE) %>% suppressWarnings(), silent = TRUE)
        importance_list <- list()

        if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
          # Filter out features with negligible importance
          importance <- importance[importance$Importance > importance_threshold, ]

          if (nrow(importance) > 0) {
            # vip::vi already returns sorted by Importance (descending)
            for (i in seq_len(nrow(importance))) {
              feat_name <- importance$Variable[i]
              importance_val <- importance$Importance[i]

              importance_list[[length(importance_list) + 1]] <- .kv(
                "importance",
                feat_name,
                .format_numeric(importance_val)
              )
            }
          }
        }

        if (length(importance_list) > 0) {
          importance_tbl <- dplyr::bind_rows(importance_list)
          eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
        }

        # Basis function information (sample of terms)
        if (!is.null(mars_obj$cuts) && length(mars_obj$cuts) > 0) {
          # Number of unique knots/cuts used
          n_cuts <- sum(sapply(mars_obj$cuts, length))
          if (is.finite(n_cuts) && n_cuts > 0) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_knots", as.character(n_cuts)))
          }
        }
      }
    }
  }

  .assemble_output(
    preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize a Mean Forecast (MEANF) Workflow
#'
#' Extracts and summarizes key information from a fitted mean forecast workflow.
#' This is a simple benchmark model that uses the mean of a window of historical
#' values for forecasting.
#'
#' @param wf A fitted tidymodels workflow containing a modeltime::window_reg()
#'   model with engine 'window_function'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including window size, window values, and number of observations.
#'
#' @noRd
summarize_model_meanf <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 5

  if (!inherits(wf, "workflow")) stop("summarize_model_meanf() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "window_function")) {
    stop("summarize_model_meanf() expects modeltime::window_reg() with set_engine('window_function').")
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)
  nobs <- if (!inherits(mold, "try-error") && !is.null(mold$outcomes)) nrow(mold$outcomes) else NA_integer_

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Model args
  args_tbl <- tibble::tibble(
    section = "model_arg",
    name    = "window_size",
    value   = .chr1(spec$args[["window_size"]])
  )

  # Engine args
  eng_args <- try(spec$eng_args, silent = TRUE)
  eng_tbl <- if (!inherits(eng_args, "try-error") && length(eng_args)) {
    tibble::tibble(
      section = "engine_param",
      name    = names(eng_args),
      value   = vapply(eng_args, .chr1, FUN.VALUE = character(1))
    )
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  engine_fit <- fit$fit

  # Find window values in the fitted object
  find_first_tbl_with_cols <- function(o, cols, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "data.frame") && all(cols %in% names(x))
    }, depth)
  }

  value_tbl <- find_first_tbl_with_cols(engine_fit, c("value"))
  if (!is.null(value_tbl)) {
    if ("id" %in% names(value_tbl)) {
      # Multiple window values with IDs
      numeric_vals <- as.numeric(value_tbl$value)
      valid_rows <- is.finite(numeric_vals)

      if (any(valid_rows)) {
        vt <- tibble::tibble(
          section = "engine_param",
          name    = paste0("window_value[", as.character(value_tbl$id[valid_rows]), "]"),
          value   = as.character(signif(numeric_vals[valid_rows], digits))
        )
        eng_tbl <- dplyr::bind_rows(eng_tbl, vt)
      }
    } else if (nrow(value_tbl) >= 1) {
      # Single window value
      val <- as.numeric(value_tbl$value[[1]])
      if (is.finite(val)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "window_value", as.character(signif(val, digits)))
        )
      }
    }
  }

  # Recursively search for window size parameter in the fit object
  # Looks for common window-related keys like "window_size", "window.length", etc.
  find_window_numeric <- function(o, depth = scan_depth, path = character()) {
    # Stop if we've exceeded depth limit or hit NULL
    if (depth < 0 || is.null(o)) {
      return(NULL)
    }

    # If we found a single numeric value, return it with its path
    if (is.numeric(o) && length(o) == 1) {
      return(list(name = paste(path, collapse = "."), value = o))
    }

    # Recursively search through list structures
    if (is.list(o) && length(o)) {
      nm <- names(o)
      if (is.null(nm)) nm <- as.character(seq_along(o))

      for (i in seq_along(o)) {
        res <- find_window_numeric(o[[i]], depth - 1, c(path, nm[i]))
        if (!is.null(res)) {
          key <- tolower(res$name)
          # Check if the path suggests this is a window size parameter
          if (grepl("window_size|window.length|window_length|^size$|frequency|period", key)) {
            return(res)
          }
        }
      }
    }
    NULL
  }

  # Try to find window size, fall back to inferring from dates
  ws <- find_window_numeric(engine_fit, scan_depth)
  ws_val <- if (!is.null(ws)) as.character(ws$value) else .infer_period_from_dates(mold)

  # Update window_size in args_tbl if we found a valid value
  if (!is.null(ws_val) && !is.na(ws_val) && nzchar(ws_val)) {
    args_tbl$value[args_tbl$name == "window_size"] <- ws_val
  }

  # Add number of observations
  if (is.finite(nobs)) {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "nobs", as.character(nobs)))
  }

  # Use unquote_values = TRUE for window_function engine to display raw window size
  .assemble_output(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = TRUE, digits = digits
  )
}

#' Summarize an NNETAR Workflow
#'
#' Extracts and summarizes key information from a fitted NNETAR (Neural Network
#' AutoRegression) workflow. NNETAR is a feed-forward neural network with lagged
#' inputs for time series forecasting.
#'
#' @param wf A fitted tidymodels workflow containing a modeltime::nnetar_reg()
#'   model with engine 'nnetar'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including network architecture (p, P, hidden units), seasonal period,
#'   number of networks, lags used, and accuracy metrics.
#'
#' @noRd
summarize_model_nnetar <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6

  if (!inherits(wf, "workflow")) stop("summarize_model_nnetar() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "nnetar")) {
    stop("summarize_model_nnetar() only supports modeltime::nnetar_reg() with set_engine('nnetar').")
  }

  # Specific predicates for NNETAR
  is_nnetar <- function(o) {
    inherits(o, "nnetar") ||
      (is.list(o) && !is.null(o$method) && grepl("NNAR", o$method)) ||
      (is.list(o) && !is.null(o$call) && grepl("nnetar", as.character(o$call[[1]])))
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Model args - NNETAR specific
  args_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Check for NNETAR-specific arguments
  nnetar_args <- c("non_seasonal_ar", "seasonal_ar", "hidden_units", "epochs", "penalty")
  for (arg_name in nnetar_args) {
    if (!is.null(spec$args[[arg_name]])) {
      args_tbl <- dplyr::bind_rows(
        args_tbl,
        .kv("model_arg", arg_name, .chr1(spec$args[[arg_name]]))
      )
    }
  }

  # Engine params
  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Find the NNETAR model object - try multiple approaches
  engine_fit <- fit$fit
  nnetar_obj <- .find_obj(engine_fit, is_nnetar, scan_depth)

  # Also check if the fit itself is an NNETAR object
  if (is.null(nnetar_obj) && is_nnetar(engine_fit)) {
    nnetar_obj <- engine_fit
  }

  # Look for the models$model_1 structure (common in modeltime objects)
  if (is.null(nnetar_obj)) {
    if (!is.null(engine_fit$models) && !is.null(engine_fit$models$model_1)) {
      if (is_nnetar(engine_fit$models$model_1)) {
        nnetar_obj <- engine_fit$models$model_1
      }
    }
  }

  if (!is.null(nnetar_obj)) {
    # 1. Model Architecture/Type from method field
    # Format is typically "NNAR(p,P,k)" or "NNAR(p,k)" where:
    #   p = non-seasonal AR lags, P = seasonal AR lags, k = hidden units
    if (!is.null(nnetar_obj$method)) {
      method_str <- as.character(nnetar_obj$method)

      # Add seasonal period in brackets if not already present and seasonal period exists
      if (!grepl("\\[", method_str) && !is.null(nnetar_obj$m) && is.finite(nnetar_obj$m)) {
        method_str <- paste0(method_str, "[", nnetar_obj$m, "]")
      }

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "method", method_str))
    }

    # 2. Extract p, P, and size (hidden units) directly from the object
    if (!is.null(nnetar_obj$p) && is.finite(nnetar_obj$p)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "non_seasonal_ar", as.character(nnetar_obj$p))
      )
    }

    if (!is.null(nnetar_obj$P) && is.finite(nnetar_obj$P)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "seasonal_ar", as.character(nnetar_obj$P))
      )
    }

    if (!is.null(nnetar_obj$size) && is.finite(nnetar_obj$size)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "hidden_units", as.character(nnetar_obj$size))
      )
    }

    # 3. Seasonal period from m field
    if (!is.null(nnetar_obj$m) && is.finite(nnetar_obj$m)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "seasonal_period", as.character(nnetar_obj$m)),
        .kv("engine_param", "seasonal_frequency", as.character(nnetar_obj$m))
      )
    }

    # 4. Number of networks/models (NNETAR typically uses ensemble of networks)
    if (!is.null(nnetar_obj$model) && is.list(nnetar_obj$model)) {
      num_networks <- length(nnetar_obj$model)
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "num_networks", as.character(num_networks))
      )

      # Extract details from first network for architecture information
      if (num_networks > 0 && !is.null(nnetar_obj$model[[1]])) {
        first_nn <- nnetar_obj$model[[1]]

        # Number of weights in the network
        if (!is.null(first_nn$wts) && is.numeric(first_nn$wts)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "num_weights", as.character(length(first_nn$wts)))
          )
        }

        # Weight decay parameter (regularization)
        if (!is.null(first_nn$decay) && is.numeric(first_nn$decay) && is.finite(first_nn$decay)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "decay", as.character(signif(first_nn$decay, digits)))
          )
        }

        # Network convergence value
        if (!is.null(first_nn$value) && is.numeric(first_nn$value) && is.finite(first_nn$value)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "convergence_value", as.character(signif(first_nn$value, digits)))
          )
        }
      }
    }

    # 5. Lags used in the model
    if (!is.null(nnetar_obj$lags) && length(nnetar_obj$lags) > 0) {
      lags_str <- paste(nnetar_obj$lags, collapse = ", ")
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "lags_used", lags_str)
      )
    }

    # 6. External regressors - count only
    if (!is.null(nnetar_obj$xreg)) {
      n_xregs <- ncol(nnetar_obj$xreg)
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "num_xregs", as.character(n_xregs))
      )
    }

    # 7. Fitted values and residuals - key accuracy metrics
    if (!is.null(nnetar_obj$residuals)) {
      resids <- nnetar_obj$residuals
      if (is.numeric(resids) && length(resids) > 0) {
        # MAE - most interpretable, same units as target
        mae <- mean(abs(resids), na.rm = TRUE)
        if (is.finite(mae)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "mae", as.character(signif(mae, digits)))
          )
        }

        # RMSE - penalizes larger errors
        rmse <- sqrt(mean(resids^2, na.rm = TRUE))
        if (is.finite(rmse)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "rmse", as.character(signif(rmse, digits)))
          )
        }

        # Residual SD - shows error variability
        resid_sd <- sd(resids, na.rm = TRUE)
        if (is.finite(resid_sd)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "residuals.sd", as.character(signif(resid_sd, digits)))
          )
        }
      }
    }

    # 8. Number of observations - from x field or series field
    n_obs <- NULL
    if (!is.null(nnetar_obj$x)) {
      n_obs <- length(nnetar_obj$x)
    } else if (!is.null(nnetar_obj$series)) {
      n_obs <- length(nnetar_obj$series)
    } else if (!is.null(nnetar_obj$fitted)) {
      n_obs <- length(nnetar_obj$fitted)
    }

    if (!is.null(n_obs) && is.numeric(n_obs) && is.finite(n_obs)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "nobs", as.character(n_obs))
      )
    }

    # 9. Scale parameter (if the series was scaled during training)
    if (!is.null(nnetar_obj$scale)) {
      scale_vals <- nnetar_obj$scale
      if (is.list(scale_vals)) {
        # Extract center and scale if available
        if (!is.null(scale_vals$center) && is.finite(scale_vals$center)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "scale_center", as.character(signif(scale_vals$center, digits)))
          )
        }
        if (!is.null(scale_vals$scale) && is.finite(scale_vals$scale)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "scale_value", as.character(signif(scale_vals$scale, digits)))
          )
        }
      } else if (is.numeric(scale_vals) && length(scale_vals) > 0 && is.finite(scale_vals[1])) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "scale", as.character(signif(scale_vals[1], digits)))
        )
      }
    }
  }

  # Return the combined table
  .assemble_output(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize a Prophet Workflow
#'
#' Extracts and summarizes key information from a fitted Prophet workflow.
#' Prophet is Facebook's time series forecasting tool that uses an additive
#' model with trend, seasonality, holidays, and optional regressors.
#'
#' @param wf A fitted tidymodels workflow containing a modeltime::prophet_reg()
#'   model with engine 'prophet'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including growth type, changepoints, seasonalities, holidays, regressors,
#'   and fitted parameters.
#'
#' @noRd
summarize_model_prophet <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6

  # Numeric formatting constants
  NUM_FORMAT_SMALL_THRESHOLD <- 0.01
  NUM_FORMAT_LARGE_THRESHOLD <- 100
  NUM_FORMAT_PRECISION <- 4

  if (!inherits(wf, "workflow")) stop("summarize_model_prophet() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "prophet")) {
    stop("summarize_model_prophet() only supports modeltime::prophet_reg() with set_engine('prophet').")
  }

  # Specific predicates for Prophet
  is_prophet <- function(o) {
    inherits(o, "prophet") ||
      (is.list(o) && !is.null(o$growth)) ||
      (is.list(o) && !is.null(o$changepoints)) ||
      (is.list(o) && !is.null(o$seasonalities))
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Check for regressors (non-date predictors)
  xreg_names <- character()
  if (!inherits(mold, "try-error") && !is.null(mold$predictors) && ncol(mold$predictors) > 0) {
    is_date <- vapply(mold$predictors, function(col) inherits(col, c("Date", "POSIXct", "POSIXt")), logical(1))
    xreg_names <- names(mold$predictors)[!is_date]

    # Update predictor types to indicate which are external regressors
    if (length(xreg_names) > 0 && nrow(preds_tbl) > 0) {
      preds_tbl$value[preds_tbl$name %in% xreg_names] <-
        paste0(preds_tbl$value[preds_tbl$name %in% xreg_names], " [regressor]")
    }
  }

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Model args - Prophet specific from parsnip spec
  args_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Check for Prophet-specific arguments in the spec
  prophet_args <- c(
    "growth", "changepoint_num", "changepoint_range", "seasonality_yearly",
    "seasonality_weekly", "seasonality_daily", "season", "prior_scale_changepoints",
    "prior_scale_seasonality", "prior_scale_holidays", "logistic_cap", "logistic_floor"
  )

  for (arg_name in prophet_args) {
    if (!is.null(spec$args[[arg_name]])) {
      arg_val <- .chr1(spec$args[[arg_name]])

      # Format numeric values to reasonable precision
      num_val <- suppressWarnings(as.numeric(arg_val))
      if (!is.na(num_val) && is.finite(num_val)) {
        # Use different precision based on the scale of the value
        if (abs(num_val) < NUM_FORMAT_SMALL_THRESHOLD || abs(num_val) > NUM_FORMAT_LARGE_THRESHOLD) {
          # For very small or large values, use scientific notation with fewer digits
          arg_val <- as.character(signif(num_val, NUM_FORMAT_PRECISION))
        } else {
          # For regular values, round to reasonable decimal places
          arg_val <- as.character(round(num_val, NUM_FORMAT_PRECISION))
        }
      }

      args_tbl <- dplyr::bind_rows(
        args_tbl,
        .kv("model_arg", arg_name, arg_val)
      )
    }
  }

  # Engine params
  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Find the Prophet model object
  engine_fit <- fit$fit
  prophet_obj <- .find_obj(engine_fit, is_prophet, scan_depth)

  # Also check if the fit itself is a Prophet object
  if (is.null(prophet_obj) && is_prophet(engine_fit)) {
    prophet_obj <- engine_fit
  }

  # Look for the models$model_1 structure (common in modeltime objects)
  if (is.null(prophet_obj)) {
    if (!is.null(engine_fit$models) && !is.null(engine_fit$models$model_1)) {
      if (is_prophet(engine_fit$models$model_1)) {
        prophet_obj <- engine_fit$models$model_1
      }
    }
  }

  if (!is.null(prophet_obj)) {
    # Override "auto" values in args_tbl with actual fitted values

    # prior_scale_holidays - get actual value used
    if (any(args_tbl$name == "prior_scale_holidays" & args_tbl$value == "auto")) {
      actual_holiday_prior <- prophet_obj$holidays.prior.scale
      if (!is.null(actual_holiday_prior) && is.numeric(actual_holiday_prior) && is.finite(actual_holiday_prior)) {
        args_tbl$value[args_tbl$name == "prior_scale_holidays"] <- as.character(signif(actual_holiday_prior, NUM_FORMAT_PRECISION))
      }
    }

    # season - get actual season mode used
    if (any(args_tbl$name == "season" & args_tbl$value == "auto")) {
      # Prophet uses "additive" or "multiplicative" for seasonality mode
      actual_season_mode <- prophet_obj$seasonality.mode
      if (!is.null(actual_season_mode)) {
        args_tbl$value[args_tbl$name == "season"] <- as.character(actual_season_mode)
      }
    }

    # logistic_cap and logistic_floor - only relevant for logistic growth
    if (!is.null(prophet_obj$growth) && prophet_obj$growth == "logistic") {
      # Check for cap and floor in the history data
      if (!is.null(prophet_obj$history)) {
        if ("cap" %in% names(prophet_obj$history)) {
          cap_vals <- unique(prophet_obj$history$cap)
          if (length(cap_vals) == 1 && is.finite(cap_vals)) {
            args_tbl$value[args_tbl$name == "logistic_cap"] <- as.character(signif(cap_vals, NUM_FORMAT_PRECISION))
          }
        }
        if ("floor" %in% names(prophet_obj$history)) {
          floor_vals <- unique(prophet_obj$history$floor)
          if (length(floor_vals) == 1 && is.finite(floor_vals)) {
            args_tbl$value[args_tbl$name == "logistic_floor"] <- as.character(signif(floor_vals, NUM_FORMAT_PRECISION))
          }
        }
      }
    } else {
      # For non-logistic growth, these aren't used
      if (any(args_tbl$name == "logistic_cap")) {
        args_tbl$value[args_tbl$name == "logistic_cap"] <- "not_used"
      }
      if (any(args_tbl$name == "logistic_floor")) {
        args_tbl$value[args_tbl$name == "logistic_floor"] <- "not_used"
      }
    }

    # 1. Growth type
    if (!is.null(prophet_obj$growth)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "growth", as.character(prophet_obj$growth)))
    }

    # 2. Changepoints
    if (!is.null(prophet_obj$changepoints) && length(prophet_obj$changepoints) > 0) {
      n_changepoints <- length(prophet_obj$changepoints)
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_changepoints", as.character(n_changepoints)))

      # Show last few changepoints (most recent are most relevant for forecasting)
      cp_to_show <- min(3, n_changepoints)
      cp_start_idx <- n_changepoints - cp_to_show + 1

      for (i in seq_len(cp_to_show)) {
        actual_idx <- cp_start_idx + i - 1
        label <- if (n_changepoints <= 3) {
          paste0("changepoint_", i)
        } else {
          paste0("changepoint_last_", i) # Makes it clear these are the most recent
        }
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", label, as.character(prophet_obj$changepoints[actual_idx]))
        )
      }
    }

    # 3. Changepoint range
    if (!is.null(prophet_obj$changepoint.range)) {
      cp_range_val <- prophet_obj$changepoint.range
      if (is.numeric(cp_range_val) && is.finite(cp_range_val)) {
        cp_range_val <- as.character(round(cp_range_val, NUM_FORMAT_PRECISION))
      } else {
        cp_range_val <- as.character(cp_range_val)
      }
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "changepoint_range", cp_range_val))
    }

    # 4. Changepoint prior scale
    if (!is.null(prophet_obj$changepoint.prior.scale) &&
      is.numeric(prophet_obj$changepoint.prior.scale) &&
      is.finite(prophet_obj$changepoint.prior.scale)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
        "engine_param", "changepoint_prior_scale",
        as.character(signif(prophet_obj$changepoint.prior.scale, digits))
      ))
    }

    # 5. Seasonalities - detailed information
    if (!is.null(prophet_obj$seasonalities)) {
      seasons <- prophet_obj$seasonalities
      if (is.data.frame(seasons) && nrow(seasons) > 0) {
        for (i in 1:nrow(seasons)) {
          season_name <- seasons$name[i]
          season_period <- seasons$period[i]
          season_fourier <- seasons$fourier.order[i]
          season_prior <- seasons$prior.scale[i]

          if (is.finite(season_period)) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "engine_param", paste0("seasonality_", season_name, "_period"),
                as.character(signif(season_period, digits))
              )
            )
          }

          if (is.finite(season_fourier)) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "engine_param", paste0("seasonality_", season_name, "_fourier_order"),
                as.character(season_fourier)
              )
            )
          }

          if (is.finite(season_prior)) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "engine_param", paste0("seasonality_", season_name, "_prior_scale"),
                as.character(signif(season_prior, digits))
              )
            )
          }
        }
      }
    }

    # 6. Seasonality prior scale (global)
    if (!is.null(prophet_obj$seasonality.prior.scale) &&
      is.numeric(prophet_obj$seasonality.prior.scale) &&
      is.finite(prophet_obj$seasonality.prior.scale)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
        "engine_param", "seasonality_prior_scale",
        as.character(signif(prophet_obj$seasonality.prior.scale, digits))
      ))
    }

    # 7. Holidays
    if (!is.null(prophet_obj$holidays)) {
      holidays_df <- prophet_obj$holidays
      if (is.data.frame(holidays_df) && nrow(holidays_df) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "n_holidays",
          as.character(nrow(holidays_df))
        ))

        # Show unique holiday names
        unique_holidays <- unique(holidays_df$holiday)
        if (length(unique_holidays) > 0) {
          holiday_list <- paste(head(unique_holidays, 5), collapse = ", ")
          if (length(unique_holidays) > 5) {
            holiday_list <- paste0(holiday_list, ", ...")
          }
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "holidays", holiday_list))
        }
      }
    }

    # 8. Holidays prior scale
    if (!is.null(prophet_obj$holidays.prior.scale) &&
      is.numeric(prophet_obj$holidays.prior.scale) &&
      is.finite(prophet_obj$holidays.prior.scale)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
        "engine_param", "holidays_prior_scale",
        as.character(signif(prophet_obj$holidays.prior.scale, digits))
      ))
    }

    # 9. Extra regressors - simplified summary
    if (!is.null(prophet_obj$extra_regressors)) {
      extra_regs <- prophet_obj$extra_regressors
      if (is.list(extra_regs) && length(extra_regs) > 0) {
        n_regressors <- length(extra_regs)

        # Show individual details only if there are 5 or fewer regressors
        if (n_regressors <= 5) {
          for (reg_name in names(extra_regs)) {
            reg_info <- extra_regs[[reg_name]]

            if (!is.null(reg_info$prior.scale) && is.finite(reg_info$prior.scale)) {
              eng_tbl <- dplyr::bind_rows(
                eng_tbl,
                .kv(
                  "engine_param", paste0("regressor_", reg_name, "_prior_scale"),
                  as.character(signif(reg_info$prior.scale, digits))
                )
              )
            }
            if (!is.null(reg_info$standardize)) {
              eng_tbl <- dplyr::bind_rows(
                eng_tbl,
                .kv(
                  "engine_param", paste0("regressor_", reg_name, "_standardize"),
                  as.character(reg_info$standardize)
                )
              )
            }
            if (!is.null(reg_info$mode)) {
              eng_tbl <- dplyr::bind_rows(
                eng_tbl,
                .kv(
                  "engine_param", paste0("regressor_", reg_name, "_mode"),
                  as.character(reg_info$mode)
                )
              )
            }
          }
        } else {
          # For many regressors, show summary statistics only
          prior_scales <- sapply(extra_regs, function(x) x$prior.scale)
          modes <- sapply(extra_regs, function(x) x$mode)

          # Count unique settings
          unique_prior_scales <- unique(prior_scales[!is.na(prior_scales) & is.finite(prior_scales)])
          unique_modes <- unique(modes[!is.na(modes)])

          if (length(unique_prior_scales) == 1) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "engine_param", "regressors_prior_scale_all",
                as.character(signif(unique_prior_scales, digits))
              )
            )
          } else if (length(prior_scales[is.finite(prior_scales)]) > 0) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "engine_param", "regressors_prior_scale_range",
                paste0(
                  signif(min(prior_scales, na.rm = TRUE), 3), " to ",
                  signif(max(prior_scales, na.rm = TRUE), 3)
                )
              )
            )
          }

          if (length(unique_modes) == 1) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "engine_param", "regressors_mode_all",
                as.character(unique_modes)
              )
            )
          } else if (length(unique_modes) > 0) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "engine_param", "regressors_modes",
                paste(unique_modes, collapse = ", ")
              )
            )
          }
        }
      }
    }

    # 10. MCMC samples (for uncertainty intervals)
    if (!is.null(prophet_obj$mcmc.samples) && is.numeric(prophet_obj$mcmc.samples)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
        "engine_param", "mcmc_samples",
        as.character(prophet_obj$mcmc.samples)
      ))
    }

    # 11. Uncertainty samples
    if (!is.null(prophet_obj$uncertainty.samples) && is.numeric(prophet_obj$uncertainty.samples)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
        "engine_param", "uncertainty_samples",
        as.character(prophet_obj$uncertainty.samples)
      ))
    }

    # 12. Logistic floor enabled flag
    if (!is.null(prophet_obj$logistic.floor) && isTRUE(prophet_obj$logistic.floor)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "logistic_floor_enabled", "TRUE"))
    }

    # 13. History size
    if (!is.null(prophet_obj$history)) {
      history_df <- prophet_obj$history
      if (is.data.frame(history_df) && nrow(history_df) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "n_historical_points",
          as.character(nrow(history_df))
        ))
      }
    }

    # 14. Training parameters and coefficients
    if (!is.null(prophet_obj$params)) {
      params <- prophet_obj$params

      # Extract beta coefficients for regressors
      # Note: Prophet may expand regressors (e.g., one-hot encoding categoricals),
      # so we use generic beta_1, beta_2, etc. naming
      if (!is.null(params$beta) && is.numeric(params$beta) && length(params$beta) > 0) {
        beta_vals <- params$beta

        for (i in seq_along(beta_vals)) {
          if (is.finite(beta_vals[i])) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "coefficient", paste0("beta_", i),
                as.character(signif(beta_vals[i], digits))
              )
            )
          }
        }
      }

      # Extract trend parameters
      if (!is.null(params$k) && is.numeric(params$k) && is.finite(params$k)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "trend_k",
          as.character(signif(params$k, digits))
        ))
      }

      if (!is.null(params$m) && is.numeric(params$m) && is.finite(params$m)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "trend_m",
          as.character(signif(params$m, digits))
        ))
      }

      # Count non-zero trend changes
      if (!is.null(params$delta) && is.numeric(params$delta) && length(params$delta) > 0) {
        delta_vals <- params$delta
        non_zero_deltas <- sum(abs(delta_vals) > 1e-10, na.rm = TRUE)
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "n_trend_changes",
          as.character(non_zero_deltas)
        ))
      }

      # Extract sigma_obs (observation noise)
      if (!is.null(params$sigma_obs) && is.numeric(params$sigma_obs) && is.finite(params$sigma_obs)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "sigma_obs",
          as.character(signif(params$sigma_obs, digits))
        ))
      }
    }

    # 15. Training time (if stored)
    if (!is.null(prophet_obj$train.time) && is.numeric(prophet_obj$train.time) && is.finite(prophet_obj$train.time)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
        "engine_param", "train_time_seconds",
        as.character(signif(prophet_obj$train.time, digits))
      ))
    }
  }

  # Return the combined table
  .assemble_output(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize a Prophet Boost Workflow
#'
#' Extracts and summarizes key information from a fitted Prophet Boost workflow.
#' Prophet Boost combines Prophet's trend and seasonality modeling with XGBoost
#' for residuals, providing both time series structure and machine learning capabilities.
#'
#' @param wf A fitted tidymodels workflow containing a modeltime::prophet_boost()
#'   model with engine 'prophet_xgboost'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including Prophet parameters (growth, changepoints, seasonalities),
#'   XGBoost hyperparameters, and variable importance.
#'
#' @noRd
summarize_model_prophet_boost <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  importance_threshold <- 1e-6

  # Numeric formatting constants
  NUM_FORMAT_SMALL_THRESHOLD <- 0.01
  NUM_FORMAT_LARGE_THRESHOLD <- 100
  NUM_FORMAT_PRECISION <- 4

  if (!inherits(wf, "workflow")) stop("summarize_model_prophet_boost() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "prophet_xgboost")) {
    stop("summarize_model_prophet_boost() only supports modeltime::prophet_boost() with engine 'prophet_xgboost'.")
  }

  # Helper function to format numeric values
  .format_numeric <- function(x) {
    if (is.character(x) && (x == "auto" || x == "")) {
      return(x)
    }
    x_num <- suppressWarnings(as.numeric(x))
    if (!is.na(x_num) && is.finite(x_num)) {
      rounded <- round(x_num, digits)
      if (rounded == floor(rounded)) {
        return(as.character(as.integer(rounded)))
      } else {
        return(format(rounded, scientific = FALSE, trim = TRUE))
      }
    }
    return(as.character(x))
  }

  # Specific predicates for Prophet and XGBoost
  is_prophet <- function(o) {
    inherits(o, "prophet") ||
      (is.list(o) && !is.null(o$growth)) ||
      (is.list(o) && !is.null(o$changepoints)) ||
      (is.list(o) && !is.null(o$seasonalities))
  }

  find_xgboost <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "xgb.Booster") ||
        (is.list(x) && "handle" %in% names(x) && inherits(x$handle, "xgb.Booster.handle"))
    }, depth)
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Check for regressors (non-date predictors)
  xreg_names <- character()
  if (!inherits(mold, "try-error") && !is.null(mold$predictors) && ncol(mold$predictors) > 0) {
    is_date <- vapply(mold$predictors, function(col) inherits(col, c("Date", "POSIXct", "POSIXt")), logical(1))
    xreg_names <- names(mold$predictors)[!is_date]

    # Update predictor types to indicate which are external regressors
    if (length(xreg_names) > 0 && nrow(preds_tbl) > 0) {
      preds_tbl$value[preds_tbl$name %in% xreg_names] <-
        paste0(preds_tbl$value[preds_tbl$name %in% xreg_names], " [regressor]")
    }
  }

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Model arguments - combine Prophet and XGBoost parameters
  # Prophet parameters
  prophet_arg_names <- c(
    "growth", "changepoint_num", "changepoint_range", "seasonality_yearly",
    "seasonality_weekly", "seasonality_daily", "season", "prior_scale_changepoints",
    "prior_scale_seasonality", "prior_scale_holidays", "logistic_cap", "logistic_floor"
  )
  # XGBoost parameters
  xgb_arg_names <- c(
    "tree_depth", "trees", "learn_rate", "mtry", "min_n",
    "loss_reduction", "sample_size", "stop_iter"
  )

  all_arg_names <- c(prophet_arg_names, xgb_arg_names)
  arg_vals <- vapply(all_arg_names, function(nm) {
    val <- .chr1(spec$args[[nm]])
    if (nm %in% xgb_arg_names) {
      .format_numeric(val)
    } else if (nm %in% prophet_arg_names) {
      # Format numeric Prophet values to reasonable precision
      num_val <- suppressWarnings(as.numeric(val))
      if (!is.na(num_val) && is.finite(num_val)) {
        if (abs(num_val) < NUM_FORMAT_SMALL_THRESHOLD || abs(num_val) > NUM_FORMAT_LARGE_THRESHOLD) {
          val <- as.character(signif(num_val, NUM_FORMAT_PRECISION))
        } else {
          val <- as.character(round(num_val, NUM_FORMAT_PRECISION))
        }
      }
      val
    } else {
      val
    }
  }, FUN.VALUE = character(1))
  args_tbl <- tibble::tibble(section = "model_arg", name = all_arg_names, value = arg_vals)

  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Add model type indicator
  eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Prophet-Boost Hybrid"))

  # Prophet Component
  engine_fit <- fit$fit
  prophet_obj <- .find_obj(engine_fit, is_prophet, scan_depth)

  # Also check if the fit itself is a Prophet object or in models structure
  if (is.null(prophet_obj) && is_prophet(engine_fit)) {
    prophet_obj <- engine_fit
  }
  if (is.null(prophet_obj)) {
    if (!is.null(engine_fit$models) && !is.null(engine_fit$models$model_1)) {
      if (is_prophet(engine_fit$models$model_1)) {
        prophet_obj <- engine_fit$models$model_1
      }
    }
  }

  if (!is.null(prophet_obj)) {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "prophet_component", "present"))

    # Override "auto" values in args_tbl with actual fitted values

    # Growth
    if (any(args_tbl$name == "growth" & args_tbl$value == "auto")) {
      if (!is.null(prophet_obj$growth)) {
        args_tbl$value[args_tbl$name == "growth"] <- as.character(prophet_obj$growth)
      }
    }

    # Changepoint num
    if (any(args_tbl$name == "changepoint_num" & args_tbl$value == "auto")) {
      if (!is.null(prophet_obj$changepoints) && length(prophet_obj$changepoints) > 0) {
        args_tbl$value[args_tbl$name == "changepoint_num"] <- as.character(length(prophet_obj$changepoints))
      }
    }

    # Changepoint range
    if (any(args_tbl$name == "changepoint_range" & args_tbl$value == "auto")) {
      if (!is.null(prophet_obj$changepoint.range) && is.numeric(prophet_obj$changepoint.range) && is.finite(prophet_obj$changepoint.range)) {
        args_tbl$value[args_tbl$name == "changepoint_range"] <- as.character(round(prophet_obj$changepoint.range, NUM_FORMAT_PRECISION))
      }
    }

    # Prior scale changepoints
    if (any(args_tbl$name == "prior_scale_changepoints" & args_tbl$value == "auto")) {
      if (!is.null(prophet_obj$changepoint.prior.scale) && is.numeric(prophet_obj$changepoint.prior.scale) && is.finite(prophet_obj$changepoint.prior.scale)) {
        args_tbl$value[args_tbl$name == "prior_scale_changepoints"] <- as.character(signif(prophet_obj$changepoint.prior.scale, NUM_FORMAT_PRECISION))
      }
    }

    # Prior scale seasonality
    if (any(args_tbl$name == "prior_scale_seasonality" & args_tbl$value == "auto")) {
      if (!is.null(prophet_obj$seasonality.prior.scale) && is.numeric(prophet_obj$seasonality.prior.scale) && is.finite(prophet_obj$seasonality.prior.scale)) {
        args_tbl$value[args_tbl$name == "prior_scale_seasonality"] <- as.character(signif(prophet_obj$seasonality.prior.scale, NUM_FORMAT_PRECISION))
      }
    }

    # Prior scale holidays
    if (any(args_tbl$name == "prior_scale_holidays" & args_tbl$value == "auto")) {
      actual_holiday_prior <- prophet_obj$holidays.prior.scale
      if (!is.null(actual_holiday_prior) && is.numeric(actual_holiday_prior) && is.finite(actual_holiday_prior)) {
        args_tbl$value[args_tbl$name == "prior_scale_holidays"] <- as.character(signif(actual_holiday_prior, NUM_FORMAT_PRECISION))
      }
    }

    # Season mode
    if (any(args_tbl$name == "season" & args_tbl$value == "auto")) {
      actual_season_mode <- prophet_obj$seasonality.mode
      if (!is.null(actual_season_mode)) {
        args_tbl$value[args_tbl$name == "season"] <- as.character(actual_season_mode)
      }
    }

    # Seasonalities - check which are actually present
    seasonalities_found <- FALSE
    if (!is.null(prophet_obj$seasonalities)) {
      if (is.data.frame(prophet_obj$seasonalities) && nrow(prophet_obj$seasonalities) > 0) {
        seasonalities_found <- TRUE
        season_names <- tolower(as.character(prophet_obj$seasonalities$name))

        # Update yearly
        if (any(args_tbl$name == "seasonality_yearly" & args_tbl$value == "auto")) {
          args_tbl$value[args_tbl$name == "seasonality_yearly"] <- ifelse("yearly" %in% season_names, "TRUE", "FALSE")
        }
        # Update weekly
        if (any(args_tbl$name == "seasonality_weekly" & args_tbl$value == "auto")) {
          args_tbl$value[args_tbl$name == "seasonality_weekly"] <- ifelse("weekly" %in% season_names, "TRUE", "FALSE")
        }
        # Update daily
        if (any(args_tbl$name == "seasonality_daily" & args_tbl$value == "auto")) {
          args_tbl$value[args_tbl$name == "seasonality_daily"] <- ifelse("daily" %in% season_names, "TRUE", "FALSE")
        }
      } else if (is.list(prophet_obj$seasonalities) && length(prophet_obj$seasonalities) > 0) {
        # Sometimes seasonalities is a list instead of dataframe
        seasonalities_found <- TRUE
        season_names <- tolower(names(prophet_obj$seasonalities))

        # Update yearly
        if (any(args_tbl$name == "seasonality_yearly" & args_tbl$value == "auto")) {
          args_tbl$value[args_tbl$name == "seasonality_yearly"] <- ifelse("yearly" %in% season_names, "TRUE", "FALSE")
        }
        # Update weekly
        if (any(args_tbl$name == "seasonality_weekly" & args_tbl$value == "auto")) {
          args_tbl$value[args_tbl$name == "seasonality_weekly"] <- ifelse("weekly" %in% season_names, "TRUE", "FALSE")
        }
        # Update daily
        if (any(args_tbl$name == "seasonality_daily" & args_tbl$value == "auto")) {
          args_tbl$value[args_tbl$name == "seasonality_daily"] <- ifelse("daily" %in% season_names, "TRUE", "FALSE")
        }
      }
    }

    # If no seasonalities found at all, set to FALSE
    if (!seasonalities_found) {
      if (any(args_tbl$name == "seasonality_yearly" & args_tbl$value == "auto")) {
        args_tbl$value[args_tbl$name == "seasonality_yearly"] <- "FALSE"
      }
      if (any(args_tbl$name == "seasonality_weekly" & args_tbl$value == "auto")) {
        args_tbl$value[args_tbl$name == "seasonality_weekly"] <- "FALSE"
      }
      if (any(args_tbl$name == "seasonality_daily" & args_tbl$value == "auto")) {
        args_tbl$value[args_tbl$name == "seasonality_daily"] <- "FALSE"
      }
    }

    # Logistic cap and floor - only relevant for logistic growth
    if (!is.null(prophet_obj$growth) && prophet_obj$growth == "logistic") {
      # Check for cap and floor in the history data
      if (!is.null(prophet_obj$history)) {
        if ("cap" %in% names(prophet_obj$history) && any(args_tbl$name == "logistic_cap" & args_tbl$value == "auto")) {
          cap_vals <- unique(prophet_obj$history$cap)
          if (length(cap_vals) == 1 && is.finite(cap_vals)) {
            args_tbl$value[args_tbl$name == "logistic_cap"] <- as.character(signif(cap_vals, NUM_FORMAT_PRECISION))
          }
        }
        if ("floor" %in% names(prophet_obj$history) && any(args_tbl$name == "logistic_floor" & args_tbl$value == "auto")) {
          floor_vals <- unique(prophet_obj$history$floor)
          if (length(floor_vals) == 1 && is.finite(floor_vals)) {
            args_tbl$value[args_tbl$name == "logistic_floor"] <- as.character(signif(floor_vals, NUM_FORMAT_PRECISION))
          }
        }
      }
    } else {
      # For non-logistic growth, these aren't used
      if (any(args_tbl$name == "logistic_cap" & args_tbl$value == "auto")) {
        args_tbl$value[args_tbl$name == "logistic_cap"] <- "not_used"
      }
      if (any(args_tbl$name == "logistic_floor" & args_tbl$value == "auto")) {
        args_tbl$value[args_tbl$name == "logistic_floor"] <- "not_used"
      }
    }

    # Changepoints - show last few only (don't duplicate the count)
    if (!is.null(prophet_obj$changepoints) && length(prophet_obj$changepoints) > 0) {
      n_changepoints <- length(prophet_obj$changepoints)

      # Show last few changepoints
      cp_to_show <- min(3, n_changepoints)
      cp_start_idx <- n_changepoints - cp_to_show + 1

      for (i in seq_len(cp_to_show)) {
        actual_idx <- cp_start_idx + i - 1
        label <- if (n_changepoints <= 3) {
          paste0("prophet_changepoint_", i)
        } else {
          paste0("prophet_changepoint_last_", i)
        }
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", label, as.character(prophet_obj$changepoints[actual_idx]))
        )
      }
    }

    # Seasonalities - detailed information
    if (!is.null(prophet_obj$seasonalities)) {
      seasons <- prophet_obj$seasonalities
      if (is.data.frame(seasons) && nrow(seasons) > 0) {
        for (i in 1:nrow(seasons)) {
          season_name <- seasons$name[i]
          season_period <- seasons$period[i]
          season_fourier <- seasons$fourier.order[i]

          if (is.finite(season_period)) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "engine_param", paste0("prophet_seasonality_", season_name, "_period"),
                as.character(signif(season_period, digits))
              )
            )
          }

          if (is.finite(season_fourier)) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv(
                "engine_param", paste0("prophet_seasonality_", season_name, "_fourier_order"),
                as.character(season_fourier)
              )
            )
          }
        }
      }
    }

    # Holidays
    if (!is.null(prophet_obj$holidays)) {
      holidays_df <- prophet_obj$holidays
      if (is.data.frame(holidays_df) && nrow(holidays_df) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "prophet_n_holidays",
          as.character(nrow(holidays_df))
        ))

        unique_holidays <- unique(holidays_df$holiday)
        if (length(unique_holidays) > 0) {
          holiday_list <- paste(head(unique_holidays, 5), collapse = ", ")
          if (length(unique_holidays) > 5) {
            holiday_list <- paste0(holiday_list, ", ...")
          }
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "prophet_holidays", holiday_list))
        }
      }
    }

    # Extra regressors summary
    if (!is.null(prophet_obj$extra_regressors)) {
      extra_regs <- prophet_obj$extra_regressors
      if (is.list(extra_regs) && length(extra_regs) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
          "engine_param", "prophet_n_extra_regressors",
          as.character(length(extra_regs))
        ))

        if (length(extra_regs) <= 5) {
          for (reg_name in names(extra_regs)) {
            reg_info <- extra_regs[[reg_name]]
            if (!is.null(reg_info$prior.scale) && is.finite(reg_info$prior.scale)) {
              eng_tbl <- dplyr::bind_rows(
                eng_tbl,
                .kv(
                  "engine_param", paste0("prophet_regressor_", reg_name, "_prior_scale"),
                  as.character(signif(reg_info$prior.scale, digits))
                )
              )
            }
          }
        }
      }
    }
  } else {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "prophet_component", "not_found"))
  }

  # XGBoost Component
  xgb_obj <- find_xgboost(engine_fit, scan_depth)

  if (!is.null(xgb_obj)) {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "xgboost_component", "present"))

    # Extract xgb.Booster if wrapped
    if (!inherits(xgb_obj, "xgb.Booster")) {
      inner_xgb <- try(xgb_obj$fit, silent = TRUE)
      if (!inherits(inner_xgb, "try-error") && inherits(inner_xgb, "xgb.Booster")) {
        xgb_obj <- inner_xgb
      }
    }

    if (inherits(xgb_obj, "xgb.Booster")) {
      # Number of boosting rounds (already in model_arg as trees, so don't duplicate)
      niter <- try(xgb_obj$niter, silent = TRUE)
      if (!inherits(niter, "try-error") && !is.null(niter) && is.finite(niter)) {
        # Update args_tbl if trees was "auto"
        if (args_tbl$value[args_tbl$name == "trees"] == "auto") {
          args_tbl$value[args_tbl$name == "trees"] <- as.character(niter)
        }
      }

      # Get training parameters
      params <- try(xgb_obj$params, silent = TRUE)
      if (!inherits(params, "try-error") && !is.null(params) && is.list(params)) {
        param_names <- c("objective", "eval_metric", "tree_method")

        for (pname in param_names) {
          pval <- params[[pname]]
          if (!is.null(pval)) {
            if (is.numeric(pval)) {
              eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", paste0("xgb_", pname), .format_numeric(pval)))
            } else {
              eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", paste0("xgb_", pname), as.character(pval)))
            }
          }
        }

        # Update args_tbl with actual parameter values if they were "auto"
        param_map <- list(
          tree_depth = "max_depth",
          learn_rate = "eta",
          min_n = "min_child_weight",
          loss_reduction = "gamma",
          sample_size = "subsample",
          stop_iter = "early_stopping_rounds"
        )

        for (arg_name in names(param_map)) {
          param_name <- param_map[[arg_name]]
          if (args_tbl$value[args_tbl$name == arg_name] == "auto" && !is.null(params[[param_name]])) {
            args_tbl$value[args_tbl$name == arg_name] <- .format_numeric(params[[param_name]])
          }
        }

        # Handle mtry specially
        if (args_tbl$value[args_tbl$name == "mtry"] == "auto") {
          if (!is.null(params$colsample_bynode)) {
            args_tbl$value[args_tbl$name == "mtry"] <- .format_numeric(params$colsample_bynode)
          } else if (!is.null(params$colsample_bytree)) {
            args_tbl$value[args_tbl$name == "mtry"] <- .format_numeric(params$colsample_bytree)
          }
        }
      }

      # Variable importance (don't add count since the importance section shows all features)
      importance <- try(vip::vi(xgb_obj, scale = TRUE) %>% suppressWarnings(), silent = TRUE)
      importance_list <- list()

      if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
        # Filter out features with negligible importance
        importance <- importance[importance$Importance > importance_threshold, ]

        if (nrow(importance) > 0) {
          for (i in seq_len(nrow(importance))) {
            feat_name <- importance$Variable[i]
            importance_val <- importance$Importance[i]

            importance_list[[length(importance_list) + 1]] <- .kv(
              "importance",
              feat_name,
              .format_numeric(importance_val)
            )
          }
        }
      }

      if (length(importance_list) > 0) {
        importance_tbl <- dplyr::bind_rows(importance_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
      }
    }
  } else {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "xgboost_component", "not_found"))
  }

  .assemble_output(
    preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize a Seasonal Naive (SNAIVE) Workflow
#'
#' Extracts and summarizes key information from a fitted seasonal naive workflow.
#' SNAIVE is a simple benchmark forecasting method that uses the last observed
#' value from the same season (e.g., same month last year) as the forecast.
#'
#' @param wf A fitted tidymodels workflow containing a modeltime::naive_reg()
#'   model with engine 'snaive'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including seasonal period and number of observations.
#'
#' @noRd
summarize_model_snaive <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6

  if (!inherits(wf, "workflow")) stop("summarize_model_snaive() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "snaive")) {
    stop("summarize_model_snaive() only supports modeltime::naive_reg() with set_engine('snaive').")
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Model args
  args_tbl <- tibble::tibble(
    section = "model_arg",
    name    = "seasonal_period",
    value   = .chr1(spec$args[["seasonal_period"]])
  )

  # Recursively search for seasonal period parameter in the fit object
  engine_fit <- fit$fit
  find_period_numeric <- function(o, depth = scan_depth, path = character()) {
    # Stop if we've exceeded depth limit or hit NULL
    if (depth < 0 || is.null(o)) {
      return(NULL)
    }

    # If we found a single numeric value, return it with its path
    if (is.numeric(o) && length(o) == 1) {
      return(list(name = paste(path, collapse = "."), value = o))
    }

    # Recursively search through list structures
    if (is.list(o) && length(o)) {
      nm <- names(o)
      if (is.null(nm)) nm <- as.character(seq_along(o))

      for (i in seq_along(o)) {
        res <- find_period_numeric(o[[i]], depth - 1, c(path, nm[i]))
        if (!is.null(res)) {
          key <- tolower(res$name)
          # Check if the path suggests this is a seasonal period parameter
          if (grepl("seasonal_period|season.period|frequency|period|freq|m\\b", key)) {
            return(res)
          }
        }
      }
    }
    NULL
  }

  # Try to find seasonal period, fall back to inferring from dates
  sp <- find_period_numeric(engine_fit, scan_depth)
  sp_val <- if (!is.null(sp)) as.character(sp$value) else .infer_period_from_dates(mold)

  # Update seasonal_period in args_tbl if we found a valid value
  if (!is.null(sp_val) && !is.na(sp_val) && nzchar(sp_val)) {
    args_tbl$value[args_tbl$name == "seasonal_period"] <- sp_val
  }

  # Engine params (minimal for snaive - it's a simple benchmark method)
  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Add number of observations if available
  nobs <- if (!inherits(mold, "try-error") && !is.null(mold$outcomes)) {
    nrow(mold$outcomes)
  } else {
    NA_integer_
  }

  if (is.finite(nobs)) {
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "nobs", as.character(nobs)))
  }

  # Use unquote_values = TRUE for snaive to display raw seasonal period
  .assemble_output(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = TRUE, digits = digits
  )
}



#' Summarize an STLM-ARIMA Workflow
#'
#' Extracts and summarizes key information from a fitted STLM-ARIMA workflow.
#' STLM (Seasonal and Trend decomposition using Loess with ARIMA modeling)
#' first decomposes the time series using STL, then fits an ARIMA model to
#' the seasonally adjusted (remainder) series.
#'
#' @param wf A fitted tidymodels workflow containing a seasonal_reg()
#'   model with engine 'stlm_arima'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including STL decomposition parameters, ARIMA remainder model specification,
#'   coefficients, and diagnostics.
#'
#' @noRd
summarize_model_stlm_arima <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  include_coefficients <- TRUE

  # Cadence thresholds in days
  CADENCE_YEARLY <- 360
  CADENCE_QUARTERLY <- 85
  CADENCE_MONTHLY <- 25
  CADENCE_WEEKLY <- 6

  if (!inherits(wf, "workflow")) stop("summarize_model_stlm_arima() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "stlm_arima")) {
    stop("summarize_model_stlm_arima() only supports seasonal_reg() with set_engine('stlm_arima').")
  }

  # Specific predicates for this function
  is_stlm <- function(o) {
    inherits(o, "stlm") ||
      (is.list(o) && !is.null(o$call) && identical(as.character(o$call[[1]]), "stlm"))
  }

  is_arima <- function(o) {
    inherits(o, "Arima") || (is.list(o) && !is.null(o$arma) && !is.null(o$coef))
  }

  is_series <- function(o) {
    inherits(o, "msts") || stats::is.ts(o)
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Infer cadence from date predictors for fallback period estimation
  cadence_days <- NA_real_
  freq_label <- ""

  if (!inherits(mold, "try-error") && !is.null(mold$predictors) && ncol(mold$predictors) > 0) {
    is_date <- vapply(mold$predictors, function(col) {
      inherits(col, c("Date", "POSIXct", "POSIXt"))
    }, logical(1))

    if (any(is_date)) {
      d <- mold$predictors[[which(is_date)[1]]]
      diffs <- suppressWarnings(as.numeric(diff(sort(unique(as.Date(d)))), units = "days"))
      mdiff <- suppressWarnings(stats::median(diffs, na.rm = TRUE))
      cadence_days <- if (is.finite(mdiff)) mdiff else NA_real_

      freq_label <- if (is.finite(mdiff)) {
        if (mdiff >= CADENCE_YEARLY) {
          "yearly"
        } else if (mdiff >= CADENCE_QUARTERLY) {
          "quarterly"
        } else if (mdiff >= CADENCE_MONTHLY) {
          "monthly"
        } else if (mdiff >= CADENCE_WEEKLY) {
          "weekly"
        } else {
          "daily"
        }
      } else {
        ""
      }
    }
  }

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Find STLM and series objects
  engine_fit <- fit$fit
  stlm_obj <- .find_obj(engine_fit, is_stlm, scan_depth)

  # Find series object for extracting seasonal periods
  series_candidates <- list(
    try(if (!is.null(stlm_obj$x)) stlm_obj$x, silent = TRUE),
    try(if (!is.null(stlm_obj$origx)) stlm_obj$origx, silent = TRUE),
    try(if (!is.null(stlm_obj$series)) stlm_obj$series, silent = TRUE),
    try(if (!is.null(stlm_obj$y)) stlm_obj$y, silent = TRUE)
  )

  series_candidates <- series_candidates[vapply(series_candidates, function(z) {
    !inherits(z, "try-error") && !is.null(z)
  }, logical(1))]

  series_obj <- NULL
  if (length(series_candidates)) {
    for (s in series_candidates) {
      if (is_series(s)) {
        series_obj <- s
        break
      }
    }
    if (is.null(series_obj)) series_obj <- series_candidates[[1]]
  }
  if (is.null(series_obj)) series_obj <- .find_obj(engine_fit, is_series, scan_depth)

  # Extract seasonal periods from series object
  get_periods <- function(s) {
    out <- numeric(0)
    if (is.null(s)) {
      return(out)
    }

    # Check for msts attribute
    m1 <- suppressWarnings(attr(s, "msts", exact = TRUE))
    if (!is.null(m1)) {
      if (is.list(m1) && !is.null(m1$seasonal.periods)) {
        out <- c(out, as.numeric(m1$seasonal.periods))
      } else if (is.atomic(m1)) {
        out <- c(out, as.numeric(m1))
      }
    }

    # Check for seasonal.periods attribute
    m2 <- suppressWarnings(attr(s, "seasonal.periods", exact = TRUE))
    if (!is.null(m2)) out <- c(out, as.numeric(m2))

    # Check ts frequency
    if (stats::is.ts(s)) {
      fr <- suppressWarnings(stats::frequency(s))
      if (is.finite(fr) && fr > 1) out <- c(out, as.numeric(fr))
    }

    unique(out[is.finite(out) & out > 0])
  }

  periods <- get_periods(series_obj)

  # Fallback period inference from cadence
  if (!length(periods) && is.finite(cadence_days)) {
    periods <- switch(freq_label,
      "yearly"    = 1,
      "quarterly" = 4,
      "monthly"   = 12,
      "weekly"    = 52,
      "daily"     = 7,
      numeric(0)
    )
  }

  # Model args from periods (show up to 3 periods)
  args_tbl <- if (length(periods)) {
    k <- min(3L, length(periods))
    tibble::tibble(
      section = "model_arg",
      name    = paste0("seasonal_period_", seq_len(k)),
      value   = as.character(periods[seq_len(k)])
    )
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Engine params
  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Extract STLM parameters
  if (!is.null(stlm_obj)) {
    # Extract call arguments
    if (!is.null(stlm_obj$call)) {
      stlm_args <- as.list(stlm_obj$call)[-1]
      drop <- c("x", "y", "data", "xreg", "series", "ts", "...")
      stlm_args <- stlm_args[setdiff(names(stlm_args), drop)]
      stlm_args <- stlm_args[names(stlm_args) != ""]

      for (nm in names(stlm_args)) {
        val <- stlm_args[[nm]]
        if (!is.list(val)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", paste0("stlm.", nm), .chr1(val))
          )
        }
      }
    }

    # Extract STL decomposition parameters
    if (!is.null(stlm_obj$stl)) {
      stl_obj <- stlm_obj$stl

      # STL window parameters (s.window, t.window, l.window)
      for (param in c("s.window", "t.window", "l.window", "s.degree", "t.degree", "l.degree")) {
        val <- try(stl_obj[[param]], silent = TRUE)
        if (!inherits(val, "try-error") && !is.null(val)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", paste0("stl.", param), .chr1(val))
          )
        }
      }

      # Check if robust STL was used
      robust_val <- try(stl_obj$robust, silent = TRUE)
      if (!inherits(robust_val, "try-error") && !is.null(robust_val)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "stl.robust", as.character(robust_val))
        )
      }
    }

    # Extract lambda (Box-Cox transformation parameter)
    lambda_val <- try(stlm_obj$lambda, silent = TRUE)
    if (!inherits(lambda_val, "try-error") && !is.null(lambda_val) &&
      is.numeric(lambda_val) && is.finite(lambda_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "lambda", as.character(signif(lambda_val, digits)))
      )
    }

    # Extract biasadj
    biasadj_val <- try(stlm_obj$biasadj, silent = TRUE)
    if (!inherits(biasadj_val, "try-error") && !is.null(biasadj_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "biasadj", as.character(biasadj_val))
      )
    }

    # Extract method used
    method_val <- try(stlm_obj$method, silent = TRUE)
    if (!inherits(method_val, "try-error") && !is.null(method_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "stlm.method", .chr1(method_val))
      )
    } else if (!any(eng_tbl$name == "stlm.method")) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "stlm.method", "arima")
      )
    }

    # Number of observations
    nobs <- try(length(stlm_obj$x), silent = TRUE)
    if (!inherits(nobs, "try-error") && is.numeric(nobs) && is.finite(nobs) && nobs > 0) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "nobs", as.character(nobs))
      )
    }
  } else {
    # If no STLM object found, set default method
    eng_tbl <- dplyr::bind_rows(
      eng_tbl,
      .kv("engine_param", "stlm.method", "arima")
    )
  }

  # Extract ARIMA remainder model parameters
  arima_obj <- .find_obj(engine_fit, is_arima, scan_depth)

  if (!is.null(arima_obj)) {
    # Extract ARMA order
    arma <- try(arima_obj$arma, silent = TRUE)
    if (!inherits(arma, "try-error") && length(arma) >= 7) {
      p <- arma[1]
      q <- arma[2]
      P <- arma[3]
      Q <- arma[4]
      m <- arma[5]
      d <- arma[6]
      D <- arma[7]

      # Non-seasonal order
      if (is.finite(p) && is.finite(d) && is.finite(q)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "remainder.order", sprintf("(%d,%d,%d)", p, d, q))
        )
      }

      # Seasonal order
      if (is.finite(P) && is.finite(D) && is.finite(Q) && is.finite(m) && m > 1) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "remainder.seasonal_order", sprintf("(%d,%d,%d)[%d]", P, D, Q, m))
        )
      }

      # Check for mean/drift in coefficients
      coef_names <- names(arima_obj$coef)
      include_mean <- !is.null(coef_names) &&
        any(grepl("intercept|mean", coef_names, ignore.case = TRUE))
      include_drift <- !is.null(coef_names) &&
        any(grepl("drift", coef_names, ignore.case = TRUE))

      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "remainder.include.mean", as.character(include_mean)),
        .kv("engine_param", "remainder.include.drift", as.character(include_drift))
      )
    }

    # Information criteria and fit statistics
    for (nm in c("aic", "aicc", "bic", "sigma2", "loglik")) {
      val <- try(arima_obj[[nm]], silent = TRUE)
      if (!inherits(val, "try-error") && !is.null(val) && is.numeric(val) && is.finite(val)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", paste0("remainder.", nm), as.character(signif(val, digits)))
        )
      }
    }

    # Extract ARIMA method
    arima_method <- try(arima_obj$method, silent = TRUE)
    if (!inherits(arima_method, "try-error") && !is.null(arima_method)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "remainder.method", .chr1(arima_method))
      )
    }

    # Extract coefficients with standard errors if available
    if (isTRUE(include_coefficients)) {
      coefs <- try(arima_obj$coef, silent = TRUE)
      if (!inherits(coefs, "try-error") && !is.null(coefs) && length(coefs)) {
        cn <- names(coefs)
        if (is.null(cn)) cn <- paste0("coef[", seq_along(coefs), "]")

        # Try to get standard errors
        var_coef <- try(arima_obj$var.coef, silent = TRUE)
        if (!inherits(var_coef, "try-error") && !is.null(var_coef) && is.matrix(var_coef)) {
          se <- sqrt(diag(var_coef))

          for (i in seq_along(coefs)) {
            coef_val <- as.numeric(coefs[i])
            if (is.finite(coef_val)) {
              eng_tbl <- dplyr::bind_rows(
                eng_tbl,
                .kv("coefficient", cn[i], as.character(signif(coef_val, digits)))
              )

              # Add standard error if available and finite
              if (i <= length(se) && is.finite(se[i])) {
                eng_tbl <- dplyr::bind_rows(
                  eng_tbl,
                  .kv("coefficient", paste0(cn[i], ".se"), as.character(signif(se[i], digits)))
                )
              }
            }
          }
        } else {
          # No standard errors available, just add coefficients
          for (i in seq_along(coefs)) {
            coef_val <- as.numeric(coefs[i])
            if (is.finite(coef_val)) {
              eng_tbl <- dplyr::bind_rows(
                eng_tbl,
                .kv("coefficient", cn[i], as.character(signif(coef_val, digits)))
              )
            }
          }
        }
      }
    }

    # Residual diagnostics
    resids <- try(arima_obj$residuals, silent = TRUE)
    if (!inherits(resids, "try-error") && !is.null(resids) &&
      is.numeric(resids) && length(resids) > 0) {
      # Ljung-Box test for residual autocorrelation
      lb_lag <- min(24L, max(8L, 2L * if (exists("m") && is.finite(m)) as.integer(m) else 12L))
      fitdf <- if (exists("p") && exists("q") && exists("P") && exists("Q")) {
        sum(is.finite(c(p, q, P, Q)))
      } else {
        0
      }
      fitdf <- min(fitdf, lb_lag - 1) # Ensure at least 1 df remains

      lb <- try(stats::Box.test(resids, lag = lb_lag, type = "Ljung-Box", fitdf = fitdf), silent = TRUE)
      if (!inherits(lb, "try-error") && !is.null(lb$p.value) && is.finite(lb$p.value)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "ljung_box.lag", as.character(lb_lag)),
          .kv("engine_param", "ljung_box_p_value", sprintf("%.4g", lb$p.value))
        )

        if (!is.null(lb$statistic) && is.finite(lb$statistic)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "ljung_box.statistic", sprintf("%.4g", lb$statistic))
          )
        }
      }

      # Basic residual statistics
      resid_mean <- mean(resids, na.rm = TRUE)
      resid_sd <- sd(resids, na.rm = TRUE)

      if (is.finite(resid_mean)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "residuals.mean", as.character(signif(resid_mean, digits)))
        )
      }

      if (is.finite(resid_sd)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "residuals.sd", as.character(signif(resid_sd, digits)))
        )
      }
    }
  }

  # Assemble and return output
  .assemble_output(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = TRUE, digits = digits
  )
}

#' Summarize an STLM-ETS Workflow
#'
#' Extracts and summarizes key information from a fitted STLM-ETS workflow.
#' STLM (Seasonal and Trend decomposition using Loess with ETS modeling)
#' first decomposes the time series using STL, then fits an ETS model to
#' the seasonally adjusted (remainder) series.
#'
#' @param wf A fitted tidymodels workflow containing a seasonal_reg()
#'   model with engine 'stlm_ets'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including STL decomposition parameters, ETS remainder model specification,
#'   smoothing parameters, and diagnostics.
#'
#' @noRd
summarize_model_stlm_ets <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  include_coefficients <- TRUE

  # Cadence thresholds in days
  CADENCE_YEARLY <- 360
  CADENCE_QUARTERLY <- 85
  CADENCE_MONTHLY <- 25
  CADENCE_WEEKLY <- 6

  if (!inherits(wf, "workflow")) stop("summarize_model_stlm_ets() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "stlm_ets")) {
    stop("summarize_model_stlm_ets() only supports seasonal_reg() with set_engine('stlm_ets').")
  }

  # Specific predicates for this function
  is_stlm <- function(o) {
    inherits(o, "stlm") ||
      (is.list(o) && !is.null(o$call) && identical(as.character(o$call[[1]]), "stlm"))
  }

  is_ets <- function(o) {
    inherits(o, "ets") || (is.list(o) && !is.null(o$method) && grepl("ETS", o$method))
  }

  is_series <- function(o) {
    inherits(o, "msts") || stats::is.ts(o)
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Infer cadence from date predictors for fallback period estimation
  cadence_days <- NA_real_
  freq_label <- ""

  if (!inherits(mold, "try-error") && !is.null(mold$predictors) && ncol(mold$predictors) > 0) {
    is_date <- vapply(mold$predictors, function(col) {
      inherits(col, c("Date", "POSIXct", "POSIXt"))
    }, logical(1))

    if (any(is_date)) {
      d <- mold$predictors[[which(is_date)[1]]]
      diffs <- suppressWarnings(as.numeric(diff(sort(unique(as.Date(d)))), units = "days"))
      mdiff <- suppressWarnings(stats::median(diffs, na.rm = TRUE))
      cadence_days <- if (is.finite(mdiff)) mdiff else NA_real_

      freq_label <- if (is.finite(mdiff)) {
        if (mdiff >= CADENCE_YEARLY) {
          "yearly"
        } else if (mdiff >= CADENCE_QUARTERLY) {
          "quarterly"
        } else if (mdiff >= CADENCE_MONTHLY) {
          "monthly"
        } else if (mdiff >= CADENCE_WEEKLY) {
          "weekly"
        } else {
          "daily"
        }
      } else {
        ""
      }
    }
  }

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Find STLM and series objects
  engine_fit <- fit$fit
  stlm_obj <- .find_obj(engine_fit, is_stlm, scan_depth)

  # Find series object for extracting seasonal periods
  series_candidates <- list()
  if (!is.null(stlm_obj)) {
    series_candidates <- list(
      try(if (!is.null(stlm_obj$x)) stlm_obj$x else NULL, silent = TRUE),
      try(if (!is.null(stlm_obj$origx)) stlm_obj$origx else NULL, silent = TRUE),
      try(if (!is.null(stlm_obj$series)) stlm_obj$series else NULL, silent = TRUE),
      try(if (!is.null(stlm_obj$y)) stlm_obj$y else NULL, silent = TRUE)
    )
  }

  series_candidates <- series_candidates[vapply(series_candidates, function(z) {
    !inherits(z, "try-error") && !is.null(z)
  }, logical(1))]

  series_obj <- NULL
  if (length(series_candidates)) {
    for (s in series_candidates) {
      if (is_series(s)) {
        series_obj <- s
        break
      }
    }
    if (is.null(series_obj) && length(series_candidates) > 0) {
      series_obj <- series_candidates[[1]]
    }
  }
  if (is.null(series_obj)) series_obj <- .find_obj(engine_fit, is_series, scan_depth)

  # Extract seasonal periods from series object
  get_periods <- function(s) {
    out <- numeric(0)
    if (is.null(s)) {
      return(out)
    }

    # Check for msts attribute
    m1 <- suppressWarnings(attr(s, "msts", exact = TRUE))
    if (!is.null(m1)) {
      if (is.list(m1) && !is.null(m1$seasonal.periods)) {
        out <- c(out, as.numeric(m1$seasonal.periods))
      } else if (is.atomic(m1)) {
        out <- c(out, as.numeric(m1))
      }
    }

    # Check for seasonal.periods attribute
    m2 <- suppressWarnings(attr(s, "seasonal.periods", exact = TRUE))
    if (!is.null(m2)) out <- c(out, as.numeric(m2))

    # Check ts frequency
    if (stats::is.ts(s)) {
      fr <- suppressWarnings(stats::frequency(s))
      if (is.finite(fr) && fr > 1) out <- c(out, as.numeric(fr))
    }

    unique(out[is.finite(out) & out > 0])
  }

  periods <- get_periods(series_obj)

  # Fallback period inference from cadence
  if (!length(periods) && is.finite(cadence_days) && nzchar(freq_label)) {
    periods <- switch(freq_label,
      "yearly"    = 1,
      "quarterly" = 4,
      "monthly"   = 12,
      "weekly"    = 52,
      "daily"     = 7,
      numeric(0)
    )
  }

  # Model args from periods (show up to 3 periods)
  args_tbl <- if (length(periods)) {
    k <- min(3L, length(periods))
    tibble::tibble(
      section = "model_arg",
      name    = paste0("seasonal_period_", seq_len(k)),
      value   = as.character(periods[seq_len(k)])
    )
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Engine params
  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Extract STLM parameters
  if (!is.null(stlm_obj)) {
    # Extract method used first
    method_val <- try(stlm_obj$method, silent = TRUE)
    if (!inherits(method_val, "try-error") && !is.null(method_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "stlm.method", .chr1(method_val))
      )
    } else {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "stlm.method", "ets")
      )
    }

    # Number of observations
    nobs <- try(length(stlm_obj$x), silent = TRUE)
    if (!inherits(nobs, "try-error") && is.numeric(nobs) && is.finite(nobs) && nobs > 0) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "nobs", as.character(nobs))
      )
    }

    # Extract lambda (Box-Cox transformation parameter)
    lambda_val <- try(stlm_obj$lambda, silent = TRUE)
    if (!inherits(lambda_val, "try-error") && !is.null(lambda_val) &&
      is.numeric(lambda_val) && is.finite(lambda_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "lambda", as.character(signif(lambda_val, digits)))
      )
    }

    # Extract biasadj
    biasadj_val <- try(stlm_obj$biasadj, silent = TRUE)
    if (!inherits(biasadj_val, "try-error") && !is.null(biasadj_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "biasadj", as.character(biasadj_val))
      )
    }

    # Extract STL decomposition parameters
    if (!is.null(stlm_obj$stl)) {
      stl_obj <- stlm_obj$stl

      # STL window parameters
      for (param in c("s.window", "t.window", "l.window", "s.degree", "t.degree", "l.degree")) {
        val <- try(stl_obj[[param]], silent = TRUE)
        if (!inherits(val, "try-error") && !is.null(val)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", paste0("stl.", param), .chr1(val))
          )
        }
      }

      # Check if robust STL was used
      robust_val <- try(stl_obj$robust, silent = TRUE)
      if (!inherits(robust_val, "try-error") && !is.null(robust_val)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "stl.robust", as.character(robust_val))
        )
      }
    }

    # Extract call arguments
    if (!is.null(stlm_obj$call)) {
      stlm_args <- as.list(stlm_obj$call)[-1]
      drop <- c("x", "y", "data", "xreg", "series", "ts", "...")
      stlm_args <- stlm_args[setdiff(names(stlm_args), drop)]
      stlm_args <- stlm_args[names(stlm_args) != ""]

      for (nm in names(stlm_args)) {
        val <- stlm_args[[nm]]
        if (!is.list(val) && !any(eng_tbl$name == paste0("stlm.", nm))) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", paste0("stlm.", nm), .chr1(val))
          )
        }
      }
    }
  } else {
    # If no STLM object found, set default method
    eng_tbl <- dplyr::bind_rows(
      eng_tbl,
      .kv("engine_param", "stlm.method", "ets")
    )
  }

  # Extract ETS remainder model parameters
  ets_obj <- .find_obj(engine_fit, is_ets, scan_depth)

  # Also check within stlm_obj for the model component
  if (is.null(ets_obj) && !is.null(stlm_obj) && !is.null(stlm_obj$model)) {
    if (is_ets(stlm_obj$model)) {
      ets_obj <- stlm_obj$model
    }
  }

  if (!is.null(ets_obj)) {
    # 1. ETS Model Type and Components
    if (!is.null(ets_obj$method)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "remainder.method", as.character(ets_obj$method))
      )

      # Parse the method string (format: "ETS(A,A,A)")
      method_match <- regexpr("\\(([^)]+)\\)", ets_obj$method)
      if (method_match > 0) {
        components_str <- substr(
          ets_obj$method, method_match + 1,
          method_match + attr(method_match, "match.length") - 2
        )
        components <- strsplit(components_str, ",")[[1]]

        if (length(components) >= 1) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "remainder.error", trimws(components[1]))
          )
        }
        if (length(components) >= 2) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "remainder.trend", trimws(components[2]))
          )
        }
        if (length(components) >= 3) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "remainder.seasonal", trimws(components[3]))
          )
        }

        # Check for damping in trend component
        if (length(components) >= 2 && grepl("d$", trimws(components[2]), ignore.case = TRUE)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "remainder.damped", "TRUE")
          )
        }
      }
    }

    # 2. Smoothing parameters (alpha, beta, gamma, phi)
    param_names <- c("alpha", "beta", "gamma", "phi")
    for (param in param_names) {
      if (!is.null(ets_obj[[param]])) {
        param_val <- ets_obj[[param]]
        if (is.numeric(param_val) && length(param_val) == 1 && is.finite(param_val)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", paste0("remainder.", param), as.character(signif(param_val, digits)))
          )
        }
      }
    }

    # 3. Initial states (level, trend, seasonal)
    state_names <- c("l", "b", "s")
    for (state_name in state_names) {
      if (!is.null(ets_obj[[state_name]])) {
        state_val <- ets_obj[[state_name]]
        if (is.numeric(state_val) && length(state_val) == 1 && is.finite(state_val)) {
          display_name <- switch(state_name,
            "l" = "remainder.initial_level",
            "b" = "remainder.initial_trend",
            "remainder.initial_seasonal"
          )
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", display_name, as.character(signif(state_val, digits)))
          )
        }
      }
    }

    # 4. Information criteria and fit statistics
    for (nm in c("aic", "aicc", "bic", "sigma2", "loglik")) {
      val <- try(ets_obj[[nm]], silent = TRUE)
      if (!inherits(val, "try-error") && !is.null(val) && is.numeric(val) && is.finite(val)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", paste0("remainder.", nm), as.character(signif(val, digits)))
        )
      }
    }

    # 5. Final states (coefficients) from states matrix
    if (isTRUE(include_coefficients) && !is.null(ets_obj$states)) {
      states <- ets_obj$states
      if (is.matrix(states) && nrow(states) > 0) {
        final_states <- states[nrow(states), ]
        state_names_matrix <- colnames(states)

        for (i in seq_along(final_states)) {
          state_val <- final_states[i]

          # Determine state name
          if (!is.null(state_names_matrix) && length(state_names_matrix) >= i) {
            state_name <- state_names_matrix[i]
          } else {
            state_name <- if (i == 1) "l" else if (i == 2) "b" else paste0("s", i - 2)
          }

          if (is.numeric(state_val) && is.finite(state_val)) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv("coefficient", paste0("remainder.final_", state_name), as.character(signif(state_val, digits)))
            )
          }
        }
      }
    }

    # 6. Residual diagnostics
    resids <- try(ets_obj$residuals, silent = TRUE)
    if (!inherits(resids, "try-error") && !is.null(resids) &&
      is.numeric(resids) && length(resids) > 0) {
      # Basic residual statistics
      resid_mean <- mean(resids, na.rm = TRUE)
      resid_sd <- sd(resids, na.rm = TRUE)

      if (is.finite(resid_mean)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "residuals.mean", as.character(signif(resid_mean, digits)))
        )
      }

      if (is.finite(resid_sd)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "residuals.sd", as.character(signif(resid_sd, digits)))
        )
      }

      # Error metrics
      rmse <- sqrt(mean(resids^2, na.rm = TRUE))
      mae <- mean(abs(resids), na.rm = TRUE)

      if (is.finite(rmse)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "rmse", as.character(signif(rmse, digits)))
        )
      }

      if (is.finite(mae)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "mae", as.character(signif(mae, digits)))
        )
      }

      # Ljung-Box test for residual autocorrelation
      m <- if (length(periods) > 0) max(periods) else 12
      lb_lag <- min(24L, max(8L, 2L * as.integer(m)))

      lb <- try(stats::Box.test(resids, lag = lb_lag, type = "Ljung-Box"), silent = TRUE)
      if (!inherits(lb, "try-error") && !is.null(lb$p.value) && is.finite(lb$p.value)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "ljung_box.lag", as.character(lb_lag)),
          .kv("engine_param", "ljung_box_p_value", sprintf("%.4g", lb$p.value))
        )

        if (!is.null(lb$statistic) && is.finite(lb$statistic)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "ljung_box.statistic", sprintf("%.4g", lb$statistic))
          )
        }
      }
    }
  }

  # Assemble and return output
  .assemble_output(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = TRUE, digits = digits
  )
}

#' Summarize an SVM-Poly Workflow
#'
#' Extracts and summarizes key information from a fitted SVM-Poly workflow.
#' SVM (Support Vector Machine) with polynomial kernel is a powerful method
#' for capturing polynomial relationships in data. Supports both standard
#' kernlab models and multistep horizon models.
#'
#' @param wf A fitted tidymodels workflow containing a parsnip::svm_poly()
#'   model with engine 'kernlab' or svm_poly_multistep with engine 'svm_poly_multistep_horizon'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including kernel parameters, support vectors, training error, and variable importance.
#'
#' @noRd
summarize_model_svm_poly <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  importance_threshold <- 1e-6 # Filter out negligible importance values

  if (!inherits(wf, "workflow")) stop("summarize_model_svm_poly() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine

  # Check if it's standard svm_poly or multistep
  is_multistep <- identical(engine, "svm_poly_multistep_horizon")

  if (!engine %in% c("kernlab", "svm_poly_multistep_horizon")) {
    stop("summarize_model_svm_poly() only supports parsnip::svm_poly() with engine 'kernlab' or svm_poly_multistep with engine 'svm_poly_multistep_horizon'.")
  }

  # Find the ksvm object(s)
  find_ksvm <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "ksvm") ||
        inherits(x, "svm_poly_multistep_fit_impl")
    }, depth)
  }

  # Helper function to format numeric values
  .format_numeric <- function(x) {
    if (is.character(x) && (x == "auto" || x == "")) {
      return(x)
    }
    x_num <- suppressWarnings(as.numeric(x))
    if (!is.na(x_num) && is.finite(x_num)) {
      rounded <- round(x_num, digits)
      if (rounded == floor(rounded)) {
        return(as.character(as.integer(rounded)))
      } else {
        return(format(rounded, scientific = FALSE, trim = TRUE))
      }
    }
    return(as.character(x))
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # parsnip svm_poly args
  arg_names <- c("cost", "degree", "scale_factor", "margin")
  arg_vals <- vapply(arg_names, function(nm) {
    val <- .chr1(spec$args[[nm]])
    .format_numeric(val)
  }, FUN.VALUE = character(1))
  args_tbl <- tibble::tibble(section = "model_arg", name = arg_names, value = arg_vals)

  # Find the ksvm model object
  svm_obj <- find_ksvm(fit$fit, scan_depth)

  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  if (!is.null(svm_obj)) {
    # Handle multistep vs standard svm_poly differently
    if (is_multistep && inherits(svm_obj, "svm_poly_multistep_fit_impl")) {
      # MULTISTEP SVM-POLY

      # Extract individual models from $models list
      model_list <- svm_obj$models
      n_models <- length(model_list)

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Multistep Horizon"))
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_models", as.character(n_models)))

      # Extract lag info from model names (e.g., "model_lag_1", "model_lag_3")
      lag_names <- names(model_list)
      if (!is.null(lag_names) && length(lag_names) > 0) {
        lags <- stringr::str_extract(lag_names, "[0-9]+")
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lag_horizons", paste(lags, collapse = ", ")))
      }

      # Aggregate metrics across all models
      all_n_sv <- c()
      all_training_error <- c()
      all_obj_function <- c()

      # Collect importance from all models using vip::vi_permute
      all_importance <- list()

      for (model_name in names(model_list)) {
        svm_model <- model_list[[model_name]]

        # Extract the ksvm object from the parsnip model
        inner_ksvm <- NULL

        # For parsnip svm_poly, the ksvm object is typically at $fit
        if (inherits(svm_model, "model_fit")) {
          inner_ksvm <- try(svm_model$fit, silent = TRUE)
          if (inherits(inner_ksvm, "try-error") || !inherits(inner_ksvm, "ksvm")) {
            inner_ksvm <- NULL
          }
        } else if (inherits(svm_model, "ksvm")) {
          inner_ksvm <- svm_model
        }

        if (!is.null(inner_ksvm) && inherits(inner_ksvm, "ksvm")) {
          # Number of support vectors
          n_sv <- try(kernlab::nSV(inner_ksvm), silent = TRUE)
          if (!inherits(n_sv, "try-error") && !is.null(n_sv) && is.finite(n_sv)) {
            all_n_sv <- c(all_n_sv, n_sv)
          }

          # Training error
          train_error <- try(kernlab::error(inner_ksvm), silent = TRUE)
          if (!inherits(train_error, "try-error") && !is.null(train_error) && is.finite(train_error)) {
            all_training_error <- c(all_training_error, train_error)
          }

          # Objective function value
          obj_val <- try(kernlab::obj(inner_ksvm), silent = TRUE)
          if (!inherits(obj_val, "try-error") && !is.null(obj_val) && is.finite(obj_val)) {
            all_obj_function <- c(all_obj_function, obj_val)
          }

          # Variable importance using vip::vi_permute (requires training data from mold)
          if (!inherits(mold, "try-error") && !is.null(mold$predictors) && !is.null(mold$outcomes)) {
            # Prepare training data
            train_x <- as.data.frame(mold$predictors)
            train_y <- as.numeric(mold$outcomes[[1]]) # Extract the outcome vector

            # Create predict function for vi_permute that returns a simple numeric vector
            pred_wrapper <- function(object, newdata) {
              pred <- try(kernlab::predict(object, as.data.frame(newdata)), silent = TRUE)
              if (inherits(pred, "try-error")) {
                return(rep(NA_real_, nrow(newdata)))
              }
              # Ensure we return a simple numeric vector
              result <- c(pred) # c() strips attributes and ensures vector
              return(as.vector(result))
            }

            importance <- try(
              vip::vi(
                object = inner_ksvm,
                method = "permute",
                train = train_x,
                target = train_y,
                metric = "rmse",
                pred_wrapper = pred_wrapper,
                nsim = 10,
                scale = TRUE
              ),
              silent = TRUE
            )

            if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
              # Filter out features with negligible importance
              importance <- importance[importance$Importance > importance_threshold, ]

              if (nrow(importance) > 0) {
                for (i in seq_len(nrow(importance))) {
                  feat_name <- importance$Variable[i]
                  importance_val <- importance$Importance[i]

                  # Accumulate importance across models
                  if (is.null(all_importance[[feat_name]])) {
                    all_importance[[feat_name]] <- c()
                  }
                  all_importance[[feat_name]] <- c(all_importance[[feat_name]], importance_val)
                }
              }
            }
          }
        }
      }

      # Add aggregated metrics
      if (length(all_n_sv) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_n_support_vectors", .format_numeric(mean(all_n_sv))))
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "total_support_vectors", as.character(sum(all_n_sv))))
      }

      if (length(all_training_error) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_training_error", .format_numeric(mean(all_training_error))))
      }

      if (length(all_obj_function) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_objective_value", .format_numeric(mean(all_obj_function))))
      }

      # Aggregate importance - average across all models (negligible values already excluded)
      importance_list <- list()

      if (length(all_importance) > 0) {
        # Calculate average importance for each feature
        avg_importance <- sapply(all_importance, mean)

        # Sort by importance (descending)
        sorted_idx <- order(avg_importance, decreasing = TRUE)
        sorted_names <- names(avg_importance)[sorted_idx]
        sorted_vals <- avg_importance[sorted_idx]

        # Add all averaged importance values with consistent formatting
        for (i in seq_along(sorted_names)) {
          importance_list[[length(importance_list) + 1]] <- .kv(
            "importance",
            sorted_names[i],
            .format_numeric(sorted_vals[i])
          )
        }
      }

      # Combine engine params and importance
      if (length(importance_list) > 0) {
        importance_tbl <- dplyr::bind_rows(importance_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
      }
    } else {
      # STANDARD SVM-POLY

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Standard"))

      # Extract ksvm object if wrapped
      if (!inherits(svm_obj, "ksvm")) {
        inner_ksvm <- try(svm_obj$fit, silent = TRUE)
        if (!inherits(inner_ksvm, "try-error") && inherits(inner_ksvm, "ksvm")) {
          svm_obj <- inner_ksvm
        }
      }

      if (inherits(svm_obj, "ksvm")) {
        # Kernel type
        kernel_type <- try(kernlab::kernelf(svm_obj), silent = TRUE)
        if (!inherits(kernel_type, "try-error") && !is.null(kernel_type)) {
          kernel_name <- try(class(kernel_type)[1], silent = TRUE)
          if (!inherits(kernel_name, "try-error")) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "kernel", kernel_name))
          }
        }

        # Number of support vectors
        n_sv <- try(kernlab::nSV(svm_obj), silent = TRUE)
        if (!inherits(n_sv, "try-error") && !is.null(n_sv) && is.finite(n_sv)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_support_vectors", as.character(n_sv)))
        }

        # Number of features
        n_features <- try(svm_obj@nvar, silent = TRUE)
        if (!inherits(n_features, "try-error") && !is.null(n_features) && is.finite(n_features)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_features", as.character(n_features)))
        }

        # Training error
        train_error <- try(kernlab::error(svm_obj), silent = TRUE)
        if (!inherits(train_error, "try-error") && !is.null(train_error) && is.finite(train_error)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "training_error", .format_numeric(train_error)))
        }

        # Objective function value (primal or dual)
        obj_val <- try(kernlab::obj(svm_obj), silent = TRUE)
        if (!inherits(obj_val, "try-error") && !is.null(obj_val) && is.finite(obj_val)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "objective_value", .format_numeric(obj_val)))
        }

        # Cross validation error if available
        cross_val <- try(kernlab::cross(svm_obj), silent = TRUE)
        if (!inherits(cross_val, "try-error") && !is.null(cross_val) && is.finite(cross_val)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "cross_validation_error", .format_numeric(cross_val)))
        }

        # Get actual parameter values from the model
        # Cost (C)
        cost_val <- try(svm_obj@param$C, silent = TRUE)
        if (!inherits(cost_val, "try-error") && !is.null(cost_val) && is.finite(cost_val)) {
          if (args_tbl$value[args_tbl$name == "cost"] == "") {
            args_tbl$value[args_tbl$name == "cost"] <- .format_numeric(cost_val)
          }
        }

        # Polynomial kernel parameters (degree, scale, offset)
        kpar_vals <- try(kernlab::kpar(svm_obj), silent = TRUE)
        if (!inherits(kpar_vals, "try-error") && !is.null(kpar_vals) && is.list(kpar_vals)) {
          # Degree
          if (!is.null(kpar_vals$degree) && is.finite(kpar_vals$degree)) {
            if (args_tbl$value[args_tbl$name == "degree"] == "") {
              args_tbl$value[args_tbl$name == "degree"] <- .format_numeric(kpar_vals$degree)
            }
          }

          # Scale factor
          if (!is.null(kpar_vals$scale) && is.finite(kpar_vals$scale)) {
            if (args_tbl$value[args_tbl$name == "scale_factor"] == "") {
              args_tbl$value[args_tbl$name == "scale_factor"] <- .format_numeric(kpar_vals$scale)
            }
          }

          # Offset (additional kernel parameter)
          if (!is.null(kpar_vals$offset) && is.finite(kpar_vals$offset)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "offset", .format_numeric(kpar_vals$offset)))
          }
        }

        # Epsilon/Margin
        epsilon_val <- try(svm_obj@param$epsilon, silent = TRUE)
        if (!inherits(epsilon_val, "try-error") && !is.null(epsilon_val) && is.finite(epsilon_val)) {
          if (args_tbl$value[args_tbl$name == "margin"] == "") {
            args_tbl$value[args_tbl$name == "margin"] <- .format_numeric(epsilon_val)
          }
        }

        # Number of training instances (alternative method using xmatrix)
        n_train <- try(nrow(svm_obj@xmatrix), silent = TRUE)
        if (!inherits(n_train, "try-error") && !is.null(n_train) && is.finite(n_train) && n_train > 0) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_training_obs", as.character(n_train)))
        }

        # SVM type
        svm_type <- try(svm_obj@type, silent = TRUE)
        if (!inherits(svm_type, "try-error") && !is.null(svm_type)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "svm_type", as.character(svm_type)))
        }

        # Scaling information
        scaling_info <- try(svm_obj@scaling, silent = TRUE)
        if (!inherits(scaling_info, "try-error") && !is.null(scaling_info)) {
          if (is.list(scaling_info) && !is.null(scaling_info$scaled)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "scaled", as.character(scaling_info$scaled)))
          }
        }

        # Variable importance using vip::vi_permute (requires training data from mold)
        if (!inherits(mold, "try-error") && !is.null(mold$predictors) && !is.null(mold$outcomes)) {
          # Prepare training data
          train_x <- as.data.frame(mold$predictors)
          train_y <- as.numeric(mold$outcomes[[1]]) # Extract the outcome vector

          # Create predict function for vi_permute that returns a simple numeric vector
          pred_wrapper <- function(object, newdata) {
            pred <- try(kernlab::predict(object, as.data.frame(newdata)), silent = TRUE)
            if (inherits(pred, "try-error")) {
              return(rep(NA_real_, nrow(newdata)))
            }
            # Ensure we return a simple numeric vector
            result <- c(pred) # c() strips attributes and ensures vector
            return(as.vector(result))
          }

          importance <- try(
            vip::vi(
              object = svm_obj,
              method = "permute",
              train = train_x,
              target = train_y,
              metric = "rmse",
              pred_wrapper = pred_wrapper,
              nsim = 10,
              scale = TRUE
            ),
            silent = TRUE
          )

          importance_list <- list()

          if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
            # Filter out features with negligible importance
            importance <- importance[importance$Importance > importance_threshold, ]

            if (nrow(importance) > 0) {
              # vi_permute returns sorted by Importance (descending)
              for (i in seq_len(nrow(importance))) {
                feat_name <- importance$Variable[i]
                importance_val <- importance$Importance[i]

                importance_list[[length(importance_list) + 1]] <- .kv(
                  "importance",
                  feat_name,
                  .format_numeric(importance_val)
                )
              }
            }
          }

          if (length(importance_list) > 0) {
            importance_tbl <- dplyr::bind_rows(importance_list)
            eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
          }
        }
      }
    }
  }

  .assemble_output(
    preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize an SVM-RBF Workflow
#'
#' Extracts and summarizes key information from a fitted SVM-RBF workflow.
#' SVM (Support Vector Machine) with RBF (Radial Basis Function) kernel is a
#' powerful non-linear regression method. Supports both standard kernlab models
#' and multistep horizon models.
#'
#' @param wf A fitted tidymodels workflow containing a parsnip::svm_rbf()
#'   model with engine 'kernlab' or svm_rbf_multistep with engine 'svm_rbf_multistep_horizon'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including kernel parameters, support vectors, training error, and variable importance.
#'
#' @noRd
summarize_model_svm_rbf <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  importance_threshold <- 1e-6 # Filter out negligible importance values

  if (!inherits(wf, "workflow")) stop("summarize_model_svm_rbf() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine

  # Check if it's standard svm_rbf or multistep
  is_multistep <- identical(engine, "svm_rbf_multistep_horizon")

  if (!engine %in% c("kernlab", "svm_rbf_multistep_horizon")) {
    stop("summarize_model_svm_rbf() only supports parsnip::svm_rbf() with engine 'kernlab' or svm_rbf_multistep with engine 'svm_rbf_multistep_horizon'.")
  }

  # Find the ksvm object(s)
  find_ksvm <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "ksvm") ||
        inherits(x, "svm_rbf_multistep_fit_impl")
    }, depth)
  }

  # Helper function to format numeric values
  .format_numeric <- function(x) {
    if (is.character(x) && (x == "auto" || x == "")) {
      return(x)
    }
    x_num <- suppressWarnings(as.numeric(x))
    if (!is.na(x_num) && is.finite(x_num)) {
      rounded <- round(x_num, digits)
      if (rounded == floor(rounded)) {
        return(as.character(as.integer(rounded)))
      } else {
        return(format(rounded, scientific = FALSE, trim = TRUE))
      }
    }
    return(as.character(x))
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # parsnip svm_rbf args
  arg_names <- c("cost", "rbf_sigma", "margin")
  arg_vals <- vapply(arg_names, function(nm) {
    val <- .chr1(spec$args[[nm]])
    .format_numeric(val)
  }, FUN.VALUE = character(1))
  args_tbl <- tibble::tibble(section = "model_arg", name = arg_names, value = arg_vals)

  # Find the ksvm model object
  svm_obj <- find_ksvm(fit$fit, scan_depth)

  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  if (!is.null(svm_obj)) {
    # Handle multistep vs standard svm_rbf differently
    if (is_multistep && inherits(svm_obj, "svm_rbf_multistep_fit_impl")) {
      # MULTISTEP SVM-RBF

      # Extract individual models from $models list
      model_list <- svm_obj$models
      n_models <- length(model_list)

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Multistep Horizon"))
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_models", as.character(n_models)))

      # Extract lag info from model names (e.g., "model_lag_1", "model_lag_3")
      lag_names <- names(model_list)
      if (!is.null(lag_names) && length(lag_names) > 0) {
        lags <- stringr::str_extract(lag_names, "[0-9]+")
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lag_horizons", paste(lags, collapse = ", ")))
      }

      # Aggregate metrics across all models
      all_n_sv <- c()
      all_training_error <- c()
      all_obj_function <- c()

      # Collect importance from all models using vip::vi_permute
      all_importance <- list()

      for (model_name in names(model_list)) {
        svm_model <- model_list[[model_name]]

        # Extract the ksvm object from the parsnip model
        inner_ksvm <- NULL

        # For parsnip svm_rbf, the ksvm object is typically at $fit
        if (inherits(svm_model, "model_fit")) {
          inner_ksvm <- try(svm_model$fit, silent = TRUE)
          if (inherits(inner_ksvm, "try-error") || !inherits(inner_ksvm, "ksvm")) {
            inner_ksvm <- NULL
          }
        } else if (inherits(svm_model, "ksvm")) {
          inner_ksvm <- svm_model
        }

        if (!is.null(inner_ksvm) && inherits(inner_ksvm, "ksvm")) {
          # Number of support vectors
          n_sv <- try(kernlab::nSV(inner_ksvm), silent = TRUE)
          if (!inherits(n_sv, "try-error") && !is.null(n_sv) && is.finite(n_sv)) {
            all_n_sv <- c(all_n_sv, n_sv)
          }

          # Training error
          train_error <- try(kernlab::error(inner_ksvm), silent = TRUE)
          if (!inherits(train_error, "try-error") && !is.null(train_error) && is.finite(train_error)) {
            all_training_error <- c(all_training_error, train_error)
          }

          # Objective function value
          obj_val <- try(kernlab::obj(inner_ksvm), silent = TRUE)
          if (!inherits(obj_val, "try-error") && !is.null(obj_val) && is.finite(obj_val)) {
            all_obj_function <- c(all_obj_function, obj_val)
          }

          # Variable importance using vip::vi_permute (requires training data from mold)
          if (!inherits(mold, "try-error") && !is.null(mold$predictors) && !is.null(mold$outcomes)) {
            # Prepare training data
            train_x <- as.data.frame(mold$predictors)
            train_y <- as.numeric(mold$outcomes[[1]]) # Extract the outcome vector

            # Create predict function for vi_permute that returns a simple numeric vector
            pred_wrapper <- function(object, newdata) {
              pred <- try(kernlab::predict(object, as.data.frame(newdata)), silent = TRUE)
              if (inherits(pred, "try-error")) {
                return(rep(NA_real_, nrow(newdata)))
              }
              # Ensure we return a simple numeric vector
              result <- c(pred) # c() strips attributes and ensures vector
              return(as.vector(result))
            }

            importance <- try(
              vip::vi(
                object = inner_ksvm,
                method = "permute",
                train = train_x,
                target = train_y,
                metric = "rmse",
                pred_wrapper = pred_wrapper,
                nsim = 10,
                scale = TRUE
              ),
              silent = TRUE
            )

            if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
              # Filter out features with negligible importance
              importance <- importance[importance$Importance > importance_threshold, ]

              if (nrow(importance) > 0) {
                for (i in seq_len(nrow(importance))) {
                  feat_name <- importance$Variable[i]
                  importance_val <- importance$Importance[i]

                  # Accumulate importance across models
                  if (is.null(all_importance[[feat_name]])) {
                    all_importance[[feat_name]] <- c()
                  }
                  all_importance[[feat_name]] <- c(all_importance[[feat_name]], importance_val)
                }
              }
            }
          }
        }
      }

      # Add aggregated metrics
      if (length(all_n_sv) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_n_support_vectors", .format_numeric(mean(all_n_sv))))
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "total_support_vectors", as.character(sum(all_n_sv))))
      }

      if (length(all_training_error) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_training_error", .format_numeric(mean(all_training_error))))
      }

      if (length(all_obj_function) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_objective_value", .format_numeric(mean(all_obj_function))))
      }

      # Aggregate importance - average across all models (negligible values already excluded)
      importance_list <- list()

      if (length(all_importance) > 0) {
        # Calculate average importance for each feature
        avg_importance <- sapply(all_importance, mean)

        # Sort by importance (descending)
        sorted_idx <- order(avg_importance, decreasing = TRUE)
        sorted_names <- names(avg_importance)[sorted_idx]
        sorted_vals <- avg_importance[sorted_idx]

        # Add all averaged importance values with consistent formatting
        for (i in seq_along(sorted_names)) {
          importance_list[[length(importance_list) + 1]] <- .kv(
            "importance",
            sorted_names[i],
            .format_numeric(sorted_vals[i])
          )
        }
      }

      # Combine engine params and importance
      if (length(importance_list) > 0) {
        importance_tbl <- dplyr::bind_rows(importance_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
      }
    } else {
      # STANDARD SVM-RBF

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Standard"))

      # Extract ksvm object if wrapped
      if (!inherits(svm_obj, "ksvm")) {
        inner_ksvm <- try(svm_obj$fit, silent = TRUE)
        if (!inherits(inner_ksvm, "try-error") && inherits(inner_ksvm, "ksvm")) {
          svm_obj <- inner_ksvm
        }
      }

      if (inherits(svm_obj, "ksvm")) {
        # Kernel type
        kernel_type <- try(kernlab::kernelf(svm_obj), silent = TRUE)
        if (!inherits(kernel_type, "try-error") && !is.null(kernel_type)) {
          kernel_name <- try(class(kernel_type)[1], silent = TRUE)
          if (!inherits(kernel_name, "try-error")) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "kernel", kernel_name))
          }
        }

        # Number of support vectors
        n_sv <- try(kernlab::nSV(svm_obj), silent = TRUE)
        if (!inherits(n_sv, "try-error") && !is.null(n_sv) && is.finite(n_sv)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_support_vectors", as.character(n_sv)))
        }

        # Number of features
        n_features <- try(svm_obj@nvar, silent = TRUE)
        if (!inherits(n_features, "try-error") && !is.null(n_features) && is.finite(n_features)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_features", as.character(n_features)))
        }

        # Training error
        train_error <- try(kernlab::error(svm_obj), silent = TRUE)
        if (!inherits(train_error, "try-error") && !is.null(train_error) && is.finite(train_error)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "training_error", .format_numeric(train_error)))
        }

        # Objective function value (primal or dual)
        obj_val <- try(kernlab::obj(svm_obj), silent = TRUE)
        if (!inherits(obj_val, "try-error") && !is.null(obj_val) && is.finite(obj_val)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "objective_value", .format_numeric(obj_val)))
        }

        # Cross validation error if available
        cross_val <- try(kernlab::cross(svm_obj), silent = TRUE)
        if (!inherits(cross_val, "try-error") && !is.null(cross_val) && is.finite(cross_val)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "cross_validation_error", .format_numeric(cross_val)))
        }

        # Get actual parameter values from the model
        # Cost (C)
        cost_val <- try(svm_obj@param$C, silent = TRUE)
        if (!inherits(cost_val, "try-error") && !is.null(cost_val) && is.finite(cost_val)) {
          if (args_tbl$value[args_tbl$name == "cost"] == "") {
            args_tbl$value[args_tbl$name == "cost"] <- .format_numeric(cost_val)
          }
        }

        # Sigma (for RBF kernel)
        sigma_val <- try(kernlab::kpar(svm_obj)$sigma, silent = TRUE)
        if (!inherits(sigma_val, "try-error") && !is.null(sigma_val) && is.finite(sigma_val)) {
          if (args_tbl$value[args_tbl$name == "rbf_sigma"] == "") {
            args_tbl$value[args_tbl$name == "rbf_sigma"] <- .format_numeric(sigma_val)
          }
        }

        # Epsilon/Margin
        epsilon_val <- try(svm_obj@param$epsilon, silent = TRUE)
        if (!inherits(epsilon_val, "try-error") && !is.null(epsilon_val) && is.finite(epsilon_val)) {
          if (args_tbl$value[args_tbl$name == "margin"] == "") {
            args_tbl$value[args_tbl$name == "margin"] <- .format_numeric(epsilon_val)
          }
        }

        # Number of training instances (alternative method using xmatrix)
        n_train <- try(nrow(svm_obj@xmatrix), silent = TRUE)
        if (!inherits(n_train, "try-error") && !is.null(n_train) && is.finite(n_train) && n_train > 0) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_training_obs", as.character(n_train)))
        }

        # SVM type
        svm_type <- try(svm_obj@type, silent = TRUE)
        if (!inherits(svm_type, "try-error") && !is.null(svm_type)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "svm_type", as.character(svm_type)))
        }

        # Scaling information
        scaling_info <- try(svm_obj@scaling, silent = TRUE)
        if (!inherits(scaling_info, "try-error") && !is.null(scaling_info)) {
          if (is.list(scaling_info) && !is.null(scaling_info$scaled)) {
            eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "scaled", as.character(scaling_info$scaled)))
          }
        }

        # Variable importance using vip::vi_permute (requires training data from mold)
        if (!inherits(mold, "try-error") && !is.null(mold$predictors) && !is.null(mold$outcomes)) {
          # Prepare training data
          train_x <- as.data.frame(mold$predictors)
          train_y <- as.numeric(mold$outcomes[[1]]) # Extract the outcome vector

          # Create predict function for vi_permute that returns a simple numeric vector
          pred_wrapper <- function(object, newdata) {
            pred <- try(kernlab::predict(object, as.data.frame(newdata)), silent = TRUE)
            if (inherits(pred, "try-error")) {
              return(rep(NA_real_, nrow(newdata)))
            }
            # Ensure we return a simple numeric vector
            result <- c(pred) # c() strips attributes and ensures vector
            return(as.vector(result))
          }

          importance <- try(
            vip::vi(
              object = svm_obj,
              method = "permute",
              train = train_x,
              target = train_y,
              metric = "rmse",
              pred_wrapper = pred_wrapper,
              nsim = 10,
              scale = TRUE
            ),
            silent = TRUE
          )

          importance_list <- list()

          if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
            # Filter out features with negligible importance
            importance <- importance[importance$Importance > importance_threshold, ]

            if (nrow(importance) > 0) {
              # vi_permute returns sorted by Importance (descending)
              for (i in seq_len(nrow(importance))) {
                feat_name <- importance$Variable[i]
                importance_val <- importance$Importance[i]

                importance_list[[length(importance_list) + 1]] <- .kv(
                  "importance",
                  feat_name,
                  .format_numeric(importance_val)
                )
              }
            }
          }

          if (length(importance_list) > 0) {
            importance_tbl <- dplyr::bind_rows(importance_list)
            eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
          }
        }
      }
    }
  }

  .assemble_output(
    preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Summarize a TBATS Workflow
#'
#' Extracts and summarizes key information from a fitted TBATS workflow.
#' TBATS (Trigonometric seasonality, Box-Cox transformation, ARMA errors,
#' Trend, and Seasonal components) handles complex seasonal patterns including
#' multiple seasonalities and non-integer seasonal periods.
#'
#' @param wf A fitted tidymodels workflow containing a seasonal_reg()
#'   model with engine 'tbats'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including TBATS components, smoothing parameters, ARMA errors, seasonal
#'   harmonics, and diagnostics.
#'
#' @noRd
summarize_model_tbats <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  include_coefficients <- TRUE

  # Cadence thresholds in days
  CADENCE_YEARLY <- 360
  CADENCE_QUARTERLY <- 85
  CADENCE_MONTHLY <- 25
  CADENCE_WEEKLY <- 6

  if (!inherits(wf, "workflow")) stop("summarize_model_tbats() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "tbats")) {
    stop("summarize_model_tbats() only supports seasonal_reg() with set_engine('tbats').")
  }

  # Specific predicates for TBATS
  is_tbats <- function(o) {
    inherits(o, "tbats") || inherits(o, "bats") ||
      (is.list(o) && !is.null(o$call) && grepl("tbats|bats", as.character(o$call[[1]])))
  }

  is_series <- function(o) {
    inherits(o, "msts") || stats::is.ts(o)
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Infer cadence from date predictors for fallback period estimation
  cadence_days <- NA_real_
  freq_label <- ""

  if (!inherits(mold, "try-error") && !is.null(mold$predictors) && ncol(mold$predictors) > 0) {
    is_date <- vapply(mold$predictors, function(col) {
      inherits(col, c("Date", "POSIXct", "POSIXt"))
    }, logical(1))

    if (any(is_date)) {
      d <- mold$predictors[[which(is_date)[1]]]
      diffs <- suppressWarnings(as.numeric(diff(sort(unique(as.Date(d)))), units = "days"))
      mdiff <- suppressWarnings(stats::median(diffs, na.rm = TRUE))
      cadence_days <- if (is.finite(mdiff)) mdiff else NA_real_

      freq_label <- if (is.finite(mdiff)) {
        if (mdiff >= CADENCE_YEARLY) {
          "yearly"
        } else if (mdiff >= CADENCE_QUARTERLY) {
          "quarterly"
        } else if (mdiff >= CADENCE_MONTHLY) {
          "monthly"
        } else if (mdiff >= CADENCE_WEEKLY) {
          "weekly"
        } else {
          "daily"
        }
      } else {
        ""
      }
    }
  }

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Find TBATS object
  engine_fit <- fit$fit
  tbats_obj <- .find_obj(engine_fit, is_tbats, scan_depth)

  # Find series object for extracting seasonal periods
  series_candidates <- list()
  if (!is.null(tbats_obj)) {
    series_candidates <- list(
      try(if (!is.null(tbats_obj$y)) tbats_obj$y, silent = TRUE),
      try(if (!is.null(tbats_obj$x)) tbats_obj$x, silent = TRUE),
      try(if (!is.null(tbats_obj$series)) tbats_obj$series, silent = TRUE)
    )
  }

  series_candidates <- series_candidates[vapply(series_candidates, function(z) {
    !inherits(z, "try-error") && !is.null(z)
  }, logical(1))]

  series_obj <- NULL
  if (length(series_candidates)) {
    for (s in series_candidates) {
      if (is_series(s)) {
        series_obj <- s
        break
      }
    }
    if (is.null(series_obj)) series_obj <- series_candidates[[1]]
  }
  if (is.null(series_obj)) series_obj <- .find_obj(engine_fit, is_series, scan_depth)

  # Extract seasonal periods from series object
  get_periods <- function(s) {
    out <- numeric(0)
    if (is.null(s)) {
      return(out)
    }

    # Check for msts attribute
    m1 <- suppressWarnings(attr(s, "msts", exact = TRUE))
    if (!is.null(m1)) {
      if (is.list(m1) && !is.null(m1$seasonal.periods)) {
        out <- c(out, as.numeric(m1$seasonal.periods))
      } else if (is.atomic(m1)) {
        out <- c(out, as.numeric(m1))
      }
    }

    # Check for seasonal.periods attribute
    m2 <- suppressWarnings(attr(s, "seasonal.periods", exact = TRUE))
    if (!is.null(m2)) out <- c(out, as.numeric(m2))

    # Check ts frequency
    if (stats::is.ts(s)) {
      fr <- suppressWarnings(stats::frequency(s))
      if (is.finite(fr) && fr > 1) out <- c(out, as.numeric(fr))
    }

    unique(out[is.finite(out) & out > 0])
  }

  periods <- numeric(0)

  # Try to get periods from TBATS object first (most reliable)
  if (!is.null(tbats_obj)) {
    # TBATS stores seasonal periods directly
    if (!is.null(tbats_obj$seasonal.periods)) {
      periods <- as.numeric(tbats_obj$seasonal.periods)
    } else if (!is.null(tbats_obj$seasonal)) {
      # Sometimes stored in seasonal component
      if (is.list(tbats_obj$seasonal) && !is.null(tbats_obj$seasonal$period)) {
        periods <- as.numeric(tbats_obj$seasonal$period)
      }
    }
  }

  # Fallback to series object
  if (!length(periods)) {
    periods <- get_periods(series_obj)
  }

  # Final fallback to date inference
  if (!length(periods) && is.finite(cadence_days) && nzchar(freq_label)) {
    periods <- switch(freq_label,
      "yearly"    = 1,
      "quarterly" = 4,
      "monthly"   = 12,
      "weekly"    = 52,
      "daily"     = 7,
      numeric(0)
    )
  }

  # Model args from periods (show up to 3 periods)
  args_tbl <- if (length(periods)) {
    k <- min(3L, length(periods))
    tibble::tibble(
      section = "model_arg",
      name    = paste0("seasonal_period_", seq_len(k)),
      value   = as.character(periods[seq_len(k)])
    )
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Engine params
  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  # Extract TBATS-specific parameters
  if (!is.null(tbats_obj)) {
    # Model type (BATS vs TBATS)
    model_type <- if (inherits(tbats_obj, "bats")) "BATS" else "TBATS"
    eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", model_type))

    # Lambda (Box-Cox parameter)
    lambda_val <- try(tbats_obj$lambda, silent = TRUE)
    if (!inherits(lambda_val, "try-error") && !is.null(lambda_val) &&
      is.numeric(lambda_val) && is.finite(lambda_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "lambda", as.character(signif(lambda_val, digits)))
      )
    }

    # Alpha (smoothing parameter for level)
    alpha_val <- try(tbats_obj$alpha, silent = TRUE)
    if (!inherits(alpha_val, "try-error") && !is.null(alpha_val) &&
      is.numeric(alpha_val) && is.finite(alpha_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "alpha", as.character(signif(alpha_val, digits)))
      )
    }

    # Beta (smoothing parameter for trend)
    beta_val <- try(tbats_obj$beta, silent = TRUE)
    if (!inherits(beta_val, "try-error") && !is.null(beta_val) &&
      is.numeric(beta_val) && is.finite(beta_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "beta", as.character(signif(beta_val, digits)))
      )
    }

    # Damping parameter (phi)
    damping_val <- try(tbats_obj$damping.parameter, silent = TRUE)
    if (!inherits(damping_val, "try-error") && !is.null(damping_val) &&
      is.numeric(damping_val) && is.finite(damping_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "damping_parameter", as.character(signif(damping_val, digits)))
      )
    } else {
      phi_val <- try(tbats_obj$phi, silent = TRUE)
      if (!inherits(phi_val, "try-error") && !is.null(phi_val) &&
        is.numeric(phi_val) && is.finite(phi_val)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "phi", as.character(signif(phi_val, digits)))
        )
      }
    }

    # ARMA errors parameters
    if (!is.null(tbats_obj$ar.coefficients) && length(tbats_obj$ar.coefficients) > 0) {
      ar_coefs <- tbats_obj$ar.coefficients
      for (i in seq_along(ar_coefs)) {
        if (is.finite(ar_coefs[i])) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", paste0("ar", i), as.character(signif(ar_coefs[i], digits)))
          )
        }
      }
    }

    if (!is.null(tbats_obj$ma.coefficients) && length(tbats_obj$ma.coefficients) > 0) {
      ma_coefs <- tbats_obj$ma.coefficients
      for (i in seq_along(ma_coefs)) {
        if (is.finite(ma_coefs[i])) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", paste0("ma", i), as.character(signif(ma_coefs[i], digits)))
          )
        }
      }
    }

    # P and Q (ARMA order)
    p_val <- try(tbats_obj$p, silent = TRUE)
    if (!inherits(p_val, "try-error") && !is.null(p_val) && is.numeric(p_val) && is.finite(p_val)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "arma_p", as.character(p_val)))
    }

    q_val <- try(tbats_obj$q, silent = TRUE)
    if (!inherits(q_val, "try-error") && !is.null(q_val) && is.numeric(q_val) && is.finite(q_val)) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "arma_q", as.character(q_val)))
    }

    # Seasonal parameters (gamma values for each seasonal period)
    if (!is.null(tbats_obj$gamma.one.values) && length(tbats_obj$gamma.one.values) > 0) {
      gamma1 <- tbats_obj$gamma.one.values
      for (i in seq_along(gamma1)) {
        if (is.finite(gamma1[i])) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", paste0("gamma1_", i), as.character(signif(gamma1[i], digits)))
          )
        }
      }
    }

    if (!is.null(tbats_obj$gamma.two.values) && length(tbats_obj$gamma.two.values) > 0) {
      gamma2 <- tbats_obj$gamma.two.values
      for (i in seq_along(gamma2)) {
        if (is.finite(gamma2[i])) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", paste0("gamma2_", i), as.character(signif(gamma2[i], digits)))
          )
        }
      }
    }

    # K values (number of harmonics for each seasonal period)
    if (!is.null(tbats_obj$k.vector) && length(tbats_obj$k.vector) > 0) {
      k_vec <- tbats_obj$k.vector
      for (i in seq_along(k_vec)) {
        if (is.finite(k_vec[i])) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", paste0("k_harmonics_", i), as.character(k_vec[i]))
          )
        }
      }
    }

    # Seed states dimension
    if (!is.null(tbats_obj$seed.states) && length(tbats_obj$seed.states) > 0) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "seed_states_length", as.character(length(tbats_obj$seed.states)))
      )
    }

    # Information criteria
    for (nm in c("AIC", "BIC", "AICc", "likelihood", "loglik")) {
      val <- try(tbats_obj[[nm]], silent = TRUE)
      if (!inherits(val, "try-error") && !is.null(val) && is.numeric(val) && is.finite(val)) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", tolower(nm), as.character(signif(val, digits)))
        )
      }
    }

    # Variance/sigma
    variance_val <- try(tbats_obj$variance, silent = TRUE)
    if (!inherits(variance_val, "try-error") && !is.null(variance_val) &&
      is.numeric(variance_val) && is.finite(variance_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "variance", as.character(signif(variance_val, digits)))
      )
    }

    # Number of observations
    nobs <- try(length(tbats_obj$y), silent = TRUE)
    if (!inherits(nobs, "try-error") && is.numeric(nobs) && is.finite(nobs) && nobs > 0) {
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "nobs", as.character(nobs)))
    }

    # Extract method/call information
    if (!is.null(tbats_obj$call)) {
      tbats_args <- as.list(tbats_obj$call)[-1]
      drop <- c("y", "x", "data", "ts", "series", "use.parallel", "...")
      tbats_args <- tbats_args[setdiff(names(tbats_args), drop)]
      tbats_args <- tbats_args[names(tbats_args) != ""]

      for (nm in names(tbats_args)) {
        val <- tbats_args[[nm]]
        # Don't duplicate values we've already extracted
        if (!is.list(val) && !any(eng_tbl$name == nm)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", nm, .chr1(val)))
        }
      }
    }

    # Biasadj
    biasadj_val <- try(tbats_obj$biasadj, silent = TRUE)
    if (!inherits(biasadj_val, "try-error") && !is.null(biasadj_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "biasadj", as.character(biasadj_val))
      )
    }

    # Fitted values and residuals statistics
    if (isTRUE(include_coefficients)) {
      # Extract x (fitted states) if available
      x_states <- try(tbats_obj$x, silent = TRUE)
      if (!inherits(x_states, "try-error") && !is.null(x_states) && is.matrix(x_states)) {
        # Report state space dimensions
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "state_space_dim", paste0(nrow(x_states), "x", ncol(x_states)))
        )
      }

      # Errors/residuals
      errors <- try(tbats_obj$errors, silent = TRUE)
      if (!inherits(errors, "try-error") && !is.null(errors) &&
        is.numeric(errors) && length(errors) > 0) {
        # Basic residual statistics
        resid_mean <- mean(errors, na.rm = TRUE)
        resid_sd <- sd(errors, na.rm = TRUE)

        if (is.finite(resid_mean)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "residuals.mean", as.character(signif(resid_mean, digits)))
          )
        }

        if (is.finite(resid_sd)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "residuals.sd", as.character(signif(resid_sd, digits)))
          )
        }

        # Ljung-Box test on residuals
        lb_lag <- if (length(periods) > 0) {
          min(24L, max(8L, 2L * max(periods)))
        } else {
          24L
        }

        lb <- try(stats::Box.test(errors, lag = lb_lag, type = "Ljung-Box"), silent = TRUE)
        if (!inherits(lb, "try-error") && !is.null(lb$p.value) && is.finite(lb$p.value)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "ljung_box.lag", as.character(lb_lag)),
            .kv("engine_param", "ljung_box_p_value", sprintf("%.4g", lb$p.value))
          )

          if (!is.null(lb$statistic) && is.finite(lb$statistic)) {
            eng_tbl <- dplyr::bind_rows(
              eng_tbl,
              .kv("engine_param", "ljung_box.statistic", sprintf("%.4g", lb$statistic))
            )
          }
        }
      }
    }
  }

  # Assemble and return output
  .assemble_output(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = TRUE, digits = digits
  )
}

#' Summarize a Theta Workflow
#'
#' Extracts and summarizes key information from a fitted Theta workflow.
#' The Theta method combines exponential smoothing with a linear regression
#' component, with variants including Standard (STM), Optimized (OTM),
#' and Dynamic methods.
#'
#' @param wf A fitted tidymodels workflow containing an exp_smoothing()
#'   model with engine 'theta'.
#'
#' @return A tibble with columns: section, name, value containing model details
#'   including Theta variant, theta coefficient, smoothing parameters,
#'   trend/seasonal components, and diagnostics.
#'
#' @noRd
summarize_model_theta <- function(wf) {
  # Set fixed defaults
  DIGITS <- 6
  SCAN_DEPTH <- 6

  # Cadence thresholds (in days)
  CADENCE_YEARLY <- 360
  CADENCE_QUARTERLY <- 85
  CADENCE_MONTHLY <- 25
  CADENCE_WEEKLY <- 6

  # Alpha interpretation thresholds
  ALPHA_DRIFT_THRESHOLD <- 0.99
  ALPHA_RECENT_THRESHOLD <- 0.5

  if (!inherits(wf, "workflow")) {
    stop("summarize_model_theta() expects a tidymodels workflow.")
  }

  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) {
    stop("Workflow appears untrained. Fit it first.")
  }

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "theta")) {
    stop("summarize_model_theta() only supports modeltime::exp_smoothing() with set_engine('theta').")
  }

  # Specific predicates for Theta
  is_theta <- function(o) {
    inherits(o, "forecast") ||
      inherits(o, "Theta") ||
      (is.list(o) && !is.null(o$method) && grepl("[Tt]heta", o$method)) ||
      (is.list(o) && !is.null(o$call) && grepl("thetaf", as.character(o$call[[1]])))
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # Find the Theta model object
  engine_fit <- fit$fit
  theta_obj <- .find_obj(engine_fit, is_theta, SCAN_DEPTH)

  # Also check if the fit itself is a Theta object
  if (is.null(theta_obj) && is_theta(engine_fit)) {
    theta_obj <- engine_fit
  }

  # Look for the models$model_1 structure (common in modeltime objects)
  if (is.null(theta_obj)) {
    if (!is.null(engine_fit$models) && !is.null(engine_fit$models$model_1)) {
      if (is_theta(engine_fit$models$model_1)) {
        theta_obj <- engine_fit$models$model_1
      }
    }
  }

  # Extract alpha and theta coefficient for trend detection
  alpha_val <- NULL
  theta_coef <- NULL

  # Try to get alpha value
  if (!is.null(theta_obj)) {
    if (!is.null(theta_obj$alpha)) {
      alpha_val <- theta_obj$alpha
    } else if (!is.null(theta_obj$model) && !is.null(theta_obj$model$alpha)) {
      alpha_val <- theta_obj$model$alpha
    } else if (!is.null(theta_obj$par) && !is.null(theta_obj$par$alpha)) {
      alpha_val <- theta_obj$par$alpha
    }

    # Try to get theta coefficient
    if (!is.null(theta_obj$theta)) {
      theta_coef <- theta_obj$theta
    }
  }

  # Model args - maintain order
  args_tbl <- tibble::tibble(section = character(), name = character(), value = character())
  theta_args <- c("seasonal_period", "error", "trend", "season", "damped")

  for (arg_name in theta_args) {
    if (!is.null(spec$args[[arg_name]])) {
      arg_val <- .chr1(spec$args[[arg_name]])

      # If the value is "auto", try to get the actual value from the fitted model
      if (arg_val == "auto" && !is.null(theta_obj)) {
        actual_val <- NULL

        if (arg_name == "error") {
          # Theta models typically use additive errors
          actual_val <- "additive"
        } else if (arg_name == "trend") {
          # Check if trend was fitted
          if (!is.null(theta_obj$drift) || !is.null(theta_obj$b)) {
            actual_val <- "additive"
          } else if (!is.null(theta_coef) && is.numeric(theta_coef) && is.finite(theta_coef) && theta_coef != 0) {
            actual_val <- "additive"
          } else if (!is.null(alpha_val) && is.numeric(alpha_val) && is.finite(alpha_val) && alpha_val > 0.9) {
            actual_val <- "additive"
          } else {
            actual_val <- "none"
          }
        } else if (arg_name == "season") {
          # Check if seasonal component exists
          if (!is.null(theta_obj$season)) {
            actual_val <- as.character(theta_obj$season)
          } else if (!is.null(theta_obj$m) && is.numeric(theta_obj$m) && is.finite(theta_obj$m) && theta_obj$m > 1) {
            actual_val <- "additive"
          } else {
            actual_val <- "none"
          }
        } else if (arg_name == "seasonal_period") {
          # Try to get actual frequency from the model
          if (!is.null(theta_obj$m) && is.numeric(theta_obj$m) && is.finite(theta_obj$m)) {
            actual_val <- as.character(theta_obj$m)
          } else if (!is.null(theta_obj$frequency) && is.numeric(theta_obj$frequency) && is.finite(theta_obj$frequency)) {
            actual_val <- as.character(theta_obj$frequency)
          } else if (!is.null(theta_obj$x) && stats::is.ts(theta_obj$x)) {
            freq <- stats::frequency(theta_obj$x)
            if (is.numeric(freq) && is.finite(freq)) {
              actual_val <- as.character(freq)
            }
          }
        } else if (arg_name == "damped") {
          # Theta models typically don't use damping
          actual_val <- "FALSE"
        }

        if (!is.null(actual_val)) {
          arg_val <- actual_val
        }
      }

      args_tbl <- dplyr::bind_rows(
        args_tbl,
        .kv("model_arg", arg_name, arg_val)
      )
    }
  }

  # Check for actual seasonal period value if "frequency" was used
  if (nrow(args_tbl) > 0 && any(args_tbl$name == "seasonal_period" & args_tbl$value == "frequency")) {
    # Try to get actual frequency from the model object
    actual_freq <- NULL

    if (!is.null(theta_obj)) {
      if (!is.null(theta_obj$m) && is.numeric(theta_obj$m) && is.finite(theta_obj$m)) {
        actual_freq <- theta_obj$m
      } else if (!is.null(theta_obj$frequency) && is.numeric(theta_obj$frequency) && is.finite(theta_obj$frequency)) {
        actual_freq <- theta_obj$frequency
      } else if (!is.null(theta_obj$x) && stats::is.ts(theta_obj$x)) {
        freq <- stats::frequency(theta_obj$x)
        if (is.numeric(freq) && is.finite(freq)) {
          actual_freq <- freq
        }
      }
    }

    if (!is.null(actual_freq)) {
      args_tbl$value[args_tbl$name == "seasonal_period"] <- as.character(actual_freq)
    }
  }

  # Engine params
  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  if (!is.null(theta_obj)) {
    # 1. Model Variant
    if (!is.null(theta_obj$method)) {
      method_str <- as.character(theta_obj$method)

      # Parse theta variant from method string
      if (grepl("STM", method_str, ignore.case = TRUE)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "variant", "Standard Theta Method"))
        # Standard Theta typically uses theta=2
        if (is.null(theta_obj$theta)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "theta_coefficient", "2"))
        }
      } else if (grepl("OTM", method_str, ignore.case = TRUE)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "variant", "Optimized Theta Method"))
      } else if (grepl("DSTM", method_str, ignore.case = TRUE)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "variant", "Dynamic Standard Theta Method"))
      } else if (grepl("DOTM", method_str, ignore.case = TRUE)) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "variant", "Dynamic Optimized Theta Method"))
      } else {
        # Default/Classic Theta method
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "variant", "Classic Theta"))
        # Classic Theta uses theta=2 by default
        if (is.null(theta_obj$theta)) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "theta_coefficient", "2"))
        }
      }
    }

    # 2. Theta parameter value
    if (!is.null(theta_obj$theta) && is.numeric(theta_obj$theta) && is.finite(theta_obj$theta)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "theta_coefficient", as.character(signif(theta_obj$theta, DIGITS)))
      )
    }

    # 3. Alpha (smoothing parameter for SES component)
    if (!is.null(alpha_val) && is.numeric(alpha_val) && is.finite(alpha_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "alpha", as.character(signif(alpha_val, DIGITS)))
      )

      # Interpret alpha value
      if (alpha_val > ALPHA_DRIFT_THRESHOLD) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "alpha_interpretation", "Drift extrapolation"))
      } else if (alpha_val > ALPHA_RECENT_THRESHOLD) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "alpha_interpretation", "Recent observations weighted"))
      } else {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "alpha_interpretation", "Historical average weighted"))
      }
    }

    # 4. Drift/Linear trend component
    if (!is.null(theta_obj$drift) && is.numeric(theta_obj$drift) && is.finite(theta_obj$drift)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "drift", as.character(signif(theta_obj$drift, DIGITS)))
      )
    } else if (!is.null(theta_obj$b) && is.numeric(theta_obj$b) && is.finite(theta_obj$b)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "drift", as.character(signif(theta_obj$b, DIGITS)))
      )
    }

    # 5. Decomposition method if used
    if (!is.null(theta_obj$decomp)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "decomposition", as.character(theta_obj$decomp))
      )
    }

    # 6. Seasonal component info
    if (!is.null(theta_obj$season)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "seasonal", as.character(theta_obj$season))
      )
    }

    # 7. Seasonal period/frequency (actual value used)
    freq_val <- NULL
    if (!is.null(theta_obj$m)) {
      freq_val <- theta_obj$m
    } else if (!is.null(theta_obj$frequency)) {
      freq_val <- theta_obj$frequency
    }

    if (!is.null(freq_val) && is.numeric(freq_val) && is.finite(freq_val) && freq_val > 1) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "seasonal_period_actual", as.character(freq_val))
      )
    }

    # 8. Initial level (a0 or l0)
    if (!is.null(theta_obj$a0) && is.numeric(theta_obj$a0) && is.finite(theta_obj$a0)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "initial_level", as.character(signif(theta_obj$a0, DIGITS)))
      )
    } else if (!is.null(theta_obj$l0) && is.numeric(theta_obj$l0) && is.finite(theta_obj$l0)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "initial_level", as.character(signif(theta_obj$l0, DIGITS)))
      )
    }

    # 9. Initial trend (b0)
    if (!is.null(theta_obj$b0) && is.numeric(theta_obj$b0) && is.finite(theta_obj$b0)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "initial_trend", as.character(signif(theta_obj$b0, DIGITS)))
      )
    }

    # 10. Residuals statistics - key metrics only
    if (!is.null(theta_obj$residuals)) {
      resids <- theta_obj$residuals
      if (is.numeric(resids) && length(resids) > 0) {
        # Calculate RMSE
        rmse <- sqrt(mean(resids^2, na.rm = TRUE))
        if (is.finite(rmse)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "rmse", as.character(signif(rmse, DIGITS)))
          )
        }

        # Calculate MAE
        mae <- mean(abs(resids), na.rm = TRUE)
        if (is.finite(mae)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", "mae", as.character(signif(mae, DIGITS)))
          )
        }

        # Calculate MAPE if original series is available
        if (!is.null(theta_obj$x)) {
          x_vals <- theta_obj$x
          if (is.numeric(x_vals) && length(x_vals) == length(resids)) {
            # Only calculate MAPE where actual values are not zero
            non_zero_idx <- abs(x_vals) > 1e-10
            if (sum(non_zero_idx) > 0) {
              mape <- mean(abs(resids[non_zero_idx] / x_vals[non_zero_idx]), na.rm = TRUE) * 100
              if (is.finite(mape)) {
                eng_tbl <- dplyr::bind_rows(
                  eng_tbl,
                  .kv("engine_param", "mape", as.character(signif(mape, DIGITS)))
                )
              }
            }
          }
        }
      }
    }

    # 11. Number of observations
    if (!is.null(theta_obj$x)) {
      x_vals <- theta_obj$x
      if (is.numeric(x_vals) && length(x_vals) > 0) {
        eng_tbl <- dplyr::bind_rows(
          eng_tbl,
          .kv("engine_param", "nobs", as.character(length(x_vals)))
        )
      }
    }

    # 12. Model selection criteria
    info_criteria <- c("aic", "bic", "aicc", "loglik")
    for (criterion in info_criteria) {
      if (!is.null(theta_obj[[criterion]])) {
        criterion_val <- theta_obj[[criterion]]
        if (is.numeric(criterion_val) && length(criterion_val) == 1 && is.finite(criterion_val)) {
          eng_tbl <- dplyr::bind_rows(
            eng_tbl,
            .kv("engine_param", toupper(criterion), as.character(signif(criterion_val, DIGITS)))
          )
        }
      }
    }

    # 13. Sigma (standard error of residuals)
    sigma_val <- NULL
    if (!is.null(theta_obj$sigma)) {
      sigma_val <- theta_obj$sigma
    } else if (!is.null(theta_obj$sigma2) && is.numeric(theta_obj$sigma2) && is.finite(theta_obj$sigma2)) {
      sigma_val <- sqrt(theta_obj$sigma2)
    }

    if (!is.null(sigma_val) && is.numeric(sigma_val) && is.finite(sigma_val)) {
      eng_tbl <- dplyr::bind_rows(
        eng_tbl,
        .kv("engine_param", "sigma", as.character(signif(sigma_val, DIGITS)))
      )
    }
  }

  # Reorder model args to maintain consistent order
  if (nrow(args_tbl) > 0) {
    preferred_order <- c("seasonal_period", "error", "trend", "season", "damped")
    args_tbl <- args_tbl[order(match(args_tbl$name, preferred_order, nomatch = 1000)), ]
  }

  # Sort engine parameters for consistent output
  if (nrow(eng_tbl) > 0) {
    param_order <- c(
      "variant", "theta_coefficient", "alpha", "alpha_interpretation",
      "drift", "initial_level", "initial_trend", "decomposition", "seasonal",
      "seasonal_period_actual",
      "rmse", "mae", "mape",
      "nobs",
      "AIC", "BIC", "AICC", "LOGLIK", "sigma"
    )
    eng_tbl <- eng_tbl[order(match(eng_tbl$name, param_order, nomatch = 1000)), ]
  }

  # Assemble output
  .assemble_output(
    preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = DIGITS
  )
}

#' Summarize an XGBoost Workflow
#'
#' Extracts and summarizes key information from a fitted XGBoost workflow,
#' including model arguments, engine parameters, and variable importance.
#' Supports both standard xgboost models and multistep horizon models.
#'
#' @param wf A fitted tidymodels workflow containing a parsnip::boost_tree()
#'   model with engine 'xgboost' or xgboost_multistep with engine 'xgboost_multistep_horizon'.
#'
#' @return A tibble with columns: section, name, value containing model details.
#'
#' @noRd
summarize_model_xgboost <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  importance_threshold <- 1e-6 # Filter out negligible importance values

  if (!inherits(wf, "workflow")) stop("summarize_model_xgboost() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine

  # Check if it's standard xgboost or multistep
  is_multistep <- identical(engine, "xgboost_multistep_horizon")

  if (!engine %in% c("xgboost", "xgboost_multistep_horizon")) {
    stop("summarize_model_xgboost() only supports parsnip::boost_tree() with engine 'xgboost' or xgboost_multistep with engine 'xgboost_multistep_horizon'.")
  }

  # Find the xgboost object(s)
  find_xgboost <- function(o, depth = scan_depth) {
    .find_obj(o, function(x) {
      inherits(x, "xgb.Booster") ||
        inherits(x, "xgboost_multistep_fit_impl") ||
        (is.list(x) && !is.null(x$handle) && inherits(x$handle, "xgb.Booster.handle"))
    }, depth)
  }

  # Helper function to format numeric values: round to max 6 decimals, but keep integers as integers
  .format_numeric <- function(x) {
    if (is.character(x) && (x == "auto" || x == "")) {
      return(x)
    }
    x_num <- suppressWarnings(as.numeric(x))
    if (!is.na(x_num) && is.finite(x_num)) {
      # Round to 6 decimal places
      rounded <- round(x_num, digits)
      # If it's an integer after rounding, return without decimals
      if (rounded == floor(rounded)) {
        return(as.character(as.integer(rounded)))
      } else {
        # Return with up to 6 significant decimals, trimming trailing zeros
        return(format(rounded, scientific = FALSE, trim = TRUE))
      }
    }
    return(as.character(x))
  }

  # Extract predictors & outcomes
  mold <- try(workflows::extract_mold(wf), silent = TRUE)
  preds_tbl <- .extract_predictors(mold)
  outs_tbl <- .extract_outcomes(mold)

  # Extract recipe steps
  preproc <- try(workflows::extract_preprocessor(wf), silent = TRUE)
  steps_tbl <- if (!inherits(preproc, "try-error")) {
    .extract_recipe_steps(preproc)
  } else {
    tibble::tibble(section = character(), name = character(), value = character())
  }

  # parsnip xgboost args
  arg_names <- c(
    "tree_depth", "trees", "learn_rate", "mtry", "min_n",
    "loss_reduction", "sample_size", "stop_iter"
  )
  arg_vals <- vapply(arg_names, function(nm) {
    val <- .chr1(spec$args[[nm]])
    # Format numeric values consistently (but keep "auto" as is)
    .format_numeric(val)
  }, FUN.VALUE = character(1))
  args_tbl <- tibble::tibble(section = "model_arg", name = arg_names, value = arg_vals)

  # Find the xgboost model object
  xgb_obj <- find_xgboost(fit$fit, scan_depth)

  eng_tbl <- tibble::tibble(section = character(), name = character(), value = character())

  if (!is.null(xgb_obj)) {
    # Handle multistep vs standard xgboost differently
    if (is_multistep && inherits(xgb_obj, "xgboost_multistep_fit_impl")) {
      # MULTISTEP XGBOOST

      # Extract individual models from $models list
      model_list <- xgb_obj$models
      n_models <- length(model_list)

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Multistep Horizon"))
      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_models", as.character(n_models)))

      # Extract lag info from model names (e.g., "model_lag_1", "model_lag_3")
      lag_names <- names(model_list)
      if (!is.null(lag_names) && length(lag_names) > 0) {
        lags <- stringr::str_extract(lag_names, "[0-9]+")
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "lag_horizons", paste(lags, collapse = ", ")))
      }

      # Aggregate metrics across all models
      all_nrounds <- c()
      all_nfeatures <- c()

      # Collect importance from all models using vip
      all_importance <- list()

      for (model_name in names(model_list)) {
        xgb_model <- model_list[[model_name]]

        # For multistep xgboost, the xgb.Booster object is directly in the list
        # It's the result of modeltime::xgboost_impl() which returns an xgb.Booster
        inner_xgb <- NULL

        if (inherits(xgb_model, "xgb.Booster")) {
          # Direct xgb.Booster object
          inner_xgb <- xgb_model
        } else {
          # It might be wrapped, try common locations
          inner_xgb <- try(xgb_model, silent = TRUE)
          if (inherits(inner_xgb, "try-error") || !inherits(inner_xgb, "xgb.Booster")) {
            inner_xgb <- NULL
          }
        }

        if (!is.null(inner_xgb) && inherits(inner_xgb, "xgb.Booster")) {
          # Number of rounds
          niter <- try(inner_xgb$niter, silent = TRUE)
          if (!inherits(niter, "try-error") && !is.null(niter) && is.finite(niter)) {
            all_nrounds <- c(all_nrounds, niter)
          }

          # Number of features
          feature_names <- try(inner_xgb$feature_names, silent = TRUE)
          if (!inherits(feature_names, "try-error") && !is.null(feature_names)) {
            all_nfeatures <- c(all_nfeatures, length(feature_names))
          }

          # Variable importance using vip::vi()
          importance <- try(vip::vi(inner_xgb, scale = TRUE) %>% suppressWarnings(), silent = TRUE)

          if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
            # Filter out features with negligible importance
            importance <- importance[importance$Importance > importance_threshold, ]

            if (nrow(importance) > 0) {
              for (i in seq_len(nrow(importance))) {
                feat_name <- importance$Variable[i]
                importance_val <- importance$Importance[i]

                # Accumulate importance across models
                if (is.null(all_importance[[feat_name]])) {
                  all_importance[[feat_name]] <- c()
                }
                all_importance[[feat_name]] <- c(all_importance[[feat_name]], importance_val)
              }
            }
          }
        }
      }

      # Add aggregated metrics
      if (length(all_nrounds) > 0) {
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "avg_nrounds", as.character(round(mean(all_nrounds)))))
      }

      if (length(all_nfeatures) > 0) {
        # Features might vary across models, so report range
        if (length(unique(all_nfeatures)) == 1) {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_features", as.character(all_nfeatures[1])))
        } else {
          eng_tbl <- dplyr::bind_rows(eng_tbl, .kv(
            "engine_param", "n_features_range",
            paste0(min(all_nfeatures), "-", max(all_nfeatures))
          ))
        }
      }

      # Aggregate importance - average across all models (negligible values already excluded)
      importance_list <- list()

      if (length(all_importance) > 0) {
        # Calculate average importance for each feature (only non-negligible values)
        avg_importance <- sapply(all_importance, mean)

        # Sort by importance (descending)
        sorted_idx <- order(avg_importance, decreasing = TRUE)
        sorted_names <- names(avg_importance)[sorted_idx]
        sorted_vals <- avg_importance[sorted_idx]

        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_important_features", as.character(length(sorted_names))))

        # Add all averaged importance values with consistent formatting
        for (i in seq_along(sorted_names)) {
          importance_list[[length(importance_list) + 1]] <- .kv(
            "importance",
            sorted_names[i],
            .format_numeric(sorted_vals[i])
          )
        }
      } else {
        # No importance extracted - models may have no splits (only leaf nodes)
        eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "n_important_features", "0"))
      }

      # Combine engine params and importance
      if (length(importance_list) > 0) {
        importance_tbl <- dplyr::bind_rows(importance_list)
        eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
      }
    } else {
      # STANDARD XGBOOST

      eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", "model_type", "Standard"))

      # Extract xgb.Booster if wrapped
      if (!inherits(xgb_obj, "xgb.Booster")) {
        inner_xgb <- try(xgb_obj$fit, silent = TRUE)
        if (!inherits(inner_xgb, "try-error") && inherits(inner_xgb, "xgb.Booster")) {
          xgb_obj <- inner_xgb
        }
      }

      if (inherits(xgb_obj, "xgb.Booster")) {
        # Number of boosting rounds
        niter <- try(xgb_obj$niter, silent = TRUE)
        if (!inherits(niter, "try-error") && !is.null(niter) && is.finite(niter)) {
          # Update args_tbl if trees was "auto"
          if (args_tbl$value[args_tbl$name == "trees"] == "auto") {
            args_tbl$value[args_tbl$name == "trees"] <- as.character(niter)
          }
        }

        # Get training parameters from params list
        params <- try(xgb_obj$params, silent = TRUE)
        if (!inherits(params, "try-error") && !is.null(params) && is.list(params)) {
          # Extract only non-duplicate parameters (not already in model_arg)
          param_names <- c("objective", "eval_metric", "tree_method", "colsample_bytree")

          for (pname in param_names) {
            pval <- params[[pname]]
            if (!is.null(pval)) {
              if (is.numeric(pval)) {
                eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", pname, .format_numeric(pval)))
              } else {
                eng_tbl <- dplyr::bind_rows(eng_tbl, .kv("engine_param", pname, as.character(pval)))
              }
            }
          }

          # Update args_tbl with actual values if they were "auto" (from full params)
          if (args_tbl$value[args_tbl$name == "tree_depth"] == "auto" && !is.null(params$max_depth)) {
            args_tbl$value[args_tbl$name == "tree_depth"] <- .format_numeric(params$max_depth)
          }
          if (args_tbl$value[args_tbl$name == "learn_rate"] == "auto" && !is.null(params$eta)) {
            args_tbl$value[args_tbl$name == "learn_rate"] <- .format_numeric(params$eta)
          }
          if (args_tbl$value[args_tbl$name == "min_n"] == "auto" && !is.null(params$min_child_weight)) {
            args_tbl$value[args_tbl$name == "min_n"] <- .format_numeric(params$min_child_weight)
          }
          if (args_tbl$value[args_tbl$name == "loss_reduction"] == "auto" && !is.null(params$gamma)) {
            args_tbl$value[args_tbl$name == "loss_reduction"] <- .format_numeric(params$gamma)
          }
          if (args_tbl$value[args_tbl$name == "sample_size"] == "auto" && !is.null(params$subsample)) {
            args_tbl$value[args_tbl$name == "sample_size"] <- .format_numeric(params$subsample)
          }
          if (args_tbl$value[args_tbl$name == "stop_iter"] == "auto" && !is.null(params$early_stopping_rounds)) {
            args_tbl$value[args_tbl$name == "stop_iter"] <- .format_numeric(params$early_stopping_rounds)
          }
          if (args_tbl$value[args_tbl$name == "mtry"] == "auto") {
            if (!is.null(params$colsample_bynode)) {
              args_tbl$value[args_tbl$name == "mtry"] <- .format_numeric(params$colsample_bynode)
            } else if (!is.null(params$colsample_bytree)) {
              args_tbl$value[args_tbl$name == "mtry"] <- .format_numeric(params$colsample_bytree)
            }
          }
        }

        # Variable importance using vip::vi()
        importance <- try(vip::vi(xgb_obj, scale = TRUE) %>% suppressWarnings(), silent = TRUE)
        importance_list <- list()

        if (!inherits(importance, "try-error") && !is.null(importance) && nrow(importance) > 0) {
          # Filter out features with negligible importance
          importance <- importance[importance$Importance > importance_threshold, ]

          if (nrow(importance) > 0) {
            # vip::vi already returns sorted by Importance (descending)
            for (i in seq_len(nrow(importance))) {
              feat_name <- importance$Variable[i]
              importance_val <- importance$Importance[i]

              importance_list[[length(importance_list) + 1]] <- .kv(
                "importance",
                feat_name,
                .format_numeric(importance_val)
              )
            }
          }
        }

        if (length(importance_list) > 0) {
          importance_tbl <- dplyr::bind_rows(importance_list)
          eng_tbl <- dplyr::bind_rows(eng_tbl, importance_tbl)
        }
      }
    }
  }

  .assemble_output(
    preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
    class(fit$fit)[1], engine,
    unquote_values = FALSE, digits = digits
  )
}

#' Convert various R objects to a single character string
#' @noRd
.chr1 <- function(x) {
  if (is.null(x)) {
    return("auto")
  }
  if (inherits(x, "quosure")) {
    if (rlang::quo_is_missing(x)) {
      return("auto")
    }
    x <- rlang::quo_get_expr(x)
  }
  if (rlang::is_missing(x)) {
    return("auto")
  }
  if (is.language(x)) {
    z <- rlang::expr_deparse(x)
    return(if (length(z)) paste(z, collapse = " ") else "auto")
  }
  if (length(x) == 0) {
    return("auto")
  }
  if (is.atomic(x) && length(x) > 1) {
    return(paste0(as.character(x), collapse = ","))
  }
  as.character(x)[1]
}

#' Create a key-value tibble row
#' @noRd
.kv <- function(section, name, value) {
  tibble::tibble(
    section = as.character(section),
    name    = as.character(name),
    value   = as.character(value)
  )
}

#' Remove quotes from string values
#' @noRd
.unquote <- function(s) {
  s <- as.character(s)
  s <- sub('^"(.*)"$', "\\1", s)
  s <- sub("^'(.*)'$", "\\1", s)
}

#' Convert to character with significant digits
#' @noRd
.signif_chr <- function(x, digits = 6) {
  xn <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(xn), as.character(signif(xn, digits)), as.character(x))
}

#' Extract predictors tibble from mold
#' @noRd
.extract_predictors <- function(mold) {
  if (inherits(mold, "try-error") || is.null(mold$predictors) || ncol(mold$predictors) == 0) {
    return(tibble::tibble(section = character(), name = character(), value = character()))
  }

  pred_cls <- vapply(mold$predictors, function(col) paste(class(col), collapse = "&"), character(1))
  tibble::tibble(section = "predictor", name = names(pred_cls), value = unname(pred_cls))
}

#' Extract outcomes tibble from mold
#' @noRd
.extract_outcomes <- function(mold) {
  if (inherits(mold, "try-error") || is.null(mold$outcomes) || ncol(mold$outcomes) == 0) {
    return(tibble::tibble(section = character(), name = character(), value = character()))
  }

  out_cls <- vapply(mold$outcomes, function(col) paste(class(col), collapse = "&"), character(1))
  tibble::tibble(section = "outcome", name = names(out_cls), value = unname(out_cls))
}

#' Extract recipe steps tibble with informative descriptions
#' @noRd
.extract_recipe_steps <- function(preproc) {
  if (!inherits(preproc, "recipe")) {
    return(tibble::tibble(section = character(), name = character(), value = character()))
  }

  # Helper function to get informative step description
  get_step_description <- function(step) {
    cls <- class(step)[1]
    id <- if (!is.null(step$id) && nzchar(step$id)) step$id else ""

    # Map step classes to informative descriptions
    desc <- switch(cls,
      "step_zv" = "Remove zero variance predictors",
      "step_nzv" = "Remove near-zero variance predictors",
      "step_dummy" = {
        one_hot <- if (!is.null(step$one_hot)) step$one_hot else FALSE
        if (one_hot) {
          "One-hot encode categorical variables"
        } else {
          "Create dummy variables (reference coding)"
        }
      },
      "step_lincomb" = "Remove linear combinations",
      "step_corr" = {
        threshold <- if (!is.null(step$threshold)) step$threshold else 0.9
        paste0("Remove highly correlated predictors (threshold: ", threshold, ")")
      },
      "step_rm" = {
        vars <- if (!is.null(step$terms)) {
          vars_selected <- tryCatch(
            {
              # Try to extract variable names from the terms
              if (inherits(step$terms, "quosures")) {
                vapply(step$terms, function(x) as.character(rlang::quo_get_expr(x)), character(1))
              } else {
                "variables"
              }
            },
            error = function(e) "variables"
          )
          paste0("Remove: ", paste(vars_selected, collapse = ", "))
        } else {
          "Remove specified variables"
        }
      },
      "step_mutate" = {
        if (grepl("adj_half", id)) {
          "Create date half/quarter factors"
        } else if (grepl("char_conv", id)) {
          "Convert character to factor"
        } else {
          "Transform variables"
        }
      },
      "step_mutate_at" = {
        if (grepl("char_conv", id)) {
          "Convert all character columns to factors"
        } else {
          "Apply transformation to selected columns"
        }
      },
      "step_normalize" = {
        if (grepl("date", id, ignore.case = TRUE)) {
          "Normalize date index and year"
        } else {
          "Normalize numeric predictors"
        }
      },
      "step_center" = "Center numeric predictors (mean = 0)",
      "step_scale" = "Scale numeric predictors (sd = 1)",
      "step_pca" = {
        threshold <- if (!is.null(step$threshold)) step$threshold else 0.95
        if (!is.null(step$terms)) {
          terms_str <- tryCatch(
            {
              if (any(grepl("lag", as.character(step$terms)))) {
                paste0("PCA on lag features (", threshold * 100, "% variance)")
              } else {
                paste0("PCA (", threshold * 100, "% variance)")
              }
            },
            error = function(e) paste0("PCA (", threshold * 100, "% variance)")
          )
        } else {
          paste0("PCA (", threshold * 100, "% variance)")
        }
      },
      # Add more step types as needed
      cls # Default to class name
    )

    # Add ID if it provides additional context
    if (nzchar(id) && !grepl(id, desc, ignore.case = TRUE)) {
      desc <- paste0(desc, " [", id, "]")
    }

    desc
  }

  tibble::tibble(
    section = "recipe_step",
    name    = as.character(seq_along(preproc$steps)),
    value   = vapply(preproc$steps, get_step_description, character(1))
  )
}

#' Infer seasonal period from date cadence
#' @noRd
.infer_period_from_dates <- function(mold) {
  if (inherits(mold, "try-error") || is.null(mold$predictors) || ncol(mold$predictors) == 0) {
    return(NA_character_)
  }

  is_date <- vapply(mold$predictors, function(col) inherits(col, c("Date", "POSIXct", "POSIXt")), logical(1))
  if (!any(is_date)) {
    return(NA_character_)
  }

  d <- mold$predictors[[which(is_date)[1]]]
  diffs <- as.numeric(diff(sort(unique(as.Date(d)))), units = "days")
  mdiff <- suppressWarnings(stats::median(diffs, na.rm = TRUE))

  if (!is.finite(mdiff)) {
    return(NA_character_)
  }

  as.character(
    if (mdiff >= 360) {
      1L
    } else if (mdiff >= 85) {
      4L
    } else if (mdiff >= 25) {
      12L
    } else if (mdiff >= 6) {
      52L
    } else {
      7L
    }
  )
}

#' Recursively find object matching predicate
#' @noRd
.find_obj <- function(o, pred, depth = 6) {
  if (depth < 0 || is.null(o)) {
    return(NULL)
  }

  pred_safe <- function(x) {
    y <- FALSE
    try(y <- isTRUE(pred(x)), silent = TRUE)
    y
  }

  if (pred_safe(o)) {
    return(o)
  }

  if (is.list(o) && length(o)) {
    nm <- names(o)
    if (is.null(nm)) nm <- as.character(seq_along(o))
    for (i in seq_along(o)) {
      res <- .find_obj(o[[i]], pred, depth - 1)
      if (!is.null(res)) {
        return(res)
      }
    }
  } else if (is.environment(o)) {
    for (nm in ls(o, all.names = TRUE)) {
      res <- .find_obj(get(nm, envir = o, inherits = FALSE), pred, depth - 1)
      if (!is.null(res)) {
        return(res)
      }
    }
  }
  NULL
}

#' Assemble and format output tibble
#' @noRd
.assemble_output <- function(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl,
                             model_class, engine, unquote_values = FALSE, digits = 6) {
  out <- dplyr::bind_rows(preds_tbl, outs_tbl, steps_tbl, args_tbl, eng_tbl) |>
    dplyr::mutate(
      model_class = model_class,
      engine = engine,
      .before = 1
    ) |>
    dplyr::distinct(model_class, engine, section, name, value, .keep_all = TRUE)

  if (unquote_values) {
    out$value <- vapply(out$value, .unquote, FUN.VALUE = character(1))
  }

  # Determine sections present for proper ordering
  sections_present <- unique(out$section)
  section_levels <- c("predictor", "outcome", "recipe_step", "model_arg", "engine_param", "coefficient")
  section_levels <- intersect(section_levels, sections_present)

  dplyr::arrange(
    out,
    model_class,
    engine,
    factor(section, levels = section_levels),
    name
  )
}
