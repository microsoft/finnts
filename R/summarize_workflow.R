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
summarize_workflow_arima <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  add_diagnostics <- TRUE

  if (!inherits(wf, "workflow")) stop("summarize_workflow_arima() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!engine %in% c("auto_arima", "arima")) {
    stop("summarize_workflow_arima() only supports modeltime::arima_reg() with engines 'auto_arima' or 'arima'.")
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
summarize_workflow_arimax <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  add_diagnostics <- TRUE

  if (!inherits(wf, "workflow")) stop("summarize_workflow_arimax() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!engine %in% c("auto_arima", "auto_arima_xregs", "arima", "arima_xregs")) {
    stop("summarize_workflow_arimax() only supports modeltime::arima_reg() with engines 'auto_arima', 'auto_arima_xregs', 'arima', or 'arima_xregs'.")
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
summarize_workflow_croston <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6

  if (!inherits(wf, "workflow")) stop("summarize_workflow_croston() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "croston")) {
    stop("summarize_workflow_croston() only supports modeltime::exp_smoothing() with set_engine('croston').")
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
          final_demand <- tail(demand_val, 1)
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
          final_period <- tail(period_val, 1)
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
summarize_workflow_ets <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6

  if (!inherits(wf, "workflow")) stop("summarize_workflow_ets() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "ets")) {
    stop("summarize_workflow_ets() only supports modeltime::exp_smoothing() with set_engine('ets').")
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
summarize_workflow_meanf <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 5

  if (!inherits(wf, "workflow")) stop("summarize_workflow_meanf() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "window_function")) {
    stop("summarize_workflow_meanf() expects modeltime::window_reg() with set_engine('window_function').")
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
summarize_workflow_nnetar <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6

  if (!inherits(wf, "workflow")) stop("summarize_workflow_nnetar() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "nnetar")) {
    stop("summarize_workflow_nnetar() only supports modeltime::nnetar_reg() with set_engine('nnetar').")
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
summarize_workflow_prophet <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6

  # Numeric formatting constants
  NUM_FORMAT_SMALL_THRESHOLD <- 0.01
  NUM_FORMAT_LARGE_THRESHOLD <- 100
  NUM_FORMAT_PRECISION <- 4

  if (!inherits(wf, "workflow")) stop("summarize_workflow_prophet() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "prophet")) {
    stop("summarize_workflow_prophet() only supports modeltime::prophet_reg() with set_engine('prophet').")
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
summarize_workflow_snaive <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6

  if (!inherits(wf, "workflow")) stop("summarize_workflow_snaive() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "snaive")) {
    stop("summarize_workflow_snaive() only supports modeltime::naive_reg() with set_engine('snaive').")
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
summarize_workflow_stlm_arima <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  include_coefficients <- TRUE

  # Cadence thresholds in days
  CADENCE_YEARLY <- 360
  CADENCE_QUARTERLY <- 85
  CADENCE_MONTHLY <- 25
  CADENCE_WEEKLY <- 6

  if (!inherits(wf, "workflow")) stop("summarize_workflow_stlm_arima() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "stlm_arima")) {
    stop("summarize_workflow_stlm_arima() only supports seasonal_reg() with set_engine('stlm_arima').")
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
summarize_workflow_stlm_ets <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  include_coefficients <- TRUE

  # Cadence thresholds in days
  CADENCE_YEARLY <- 360
  CADENCE_QUARTERLY <- 85
  CADENCE_MONTHLY <- 25
  CADENCE_WEEKLY <- 6

  if (!inherits(wf, "workflow")) stop("summarize_workflow_stlm_ets() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "stlm_ets")) {
    stop("summarize_workflow_stlm_ets() only supports seasonal_reg() with set_engine('stlm_ets').")
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
summarize_workflow_tbats <- function(wf) {
  # Set fixed defaults
  digits <- 6
  scan_depth <- 6
  include_coefficients <- TRUE

  # Cadence thresholds in days
  CADENCE_YEARLY <- 360
  CADENCE_QUARTERLY <- 85
  CADENCE_MONTHLY <- 25
  CADENCE_WEEKLY <- 6

  if (!inherits(wf, "workflow")) stop("summarize_workflow_tbats() expects a tidymodels workflow.")
  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) stop("Workflow appears untrained. Fit it first.")

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "tbats")) {
    stop("summarize_workflow_tbats() only supports seasonal_reg() with set_engine('tbats').")
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
summarize_workflow_theta <- function(wf) {
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
    stop("summarize_workflow_theta() expects a tidymodels workflow.")
  }

  fit <- try(workflows::extract_fit_parsnip(wf), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$fit)) {
    stop("Workflow appears untrained. Fit it first.")
  }

  spec <- fit$spec
  engine <- if (is.null(spec$engine)) "" else spec$engine
  if (!identical(engine, "theta")) {
    stop("summarize_workflow_theta() only supports modeltime::exp_smoothing() with set_engine('theta').")
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
