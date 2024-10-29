# define global variables to prevent notes in R CMD Check
utils::globalVariables(c(
  ".", ".estimate", ".id", ".key", ".pred", "Back_Test_Scenario", "Best_Model", "Col", "Combo",
  "Combo_Hash", "Combo_ID", "Combo_Total", "Count", "Data", "Date", "Date_Adj", "Date_Day",
  "Date_half", "Date_index.num", "Date_quarter", "Date_year", "File", "Forecast", "Horizon",
  "Hyperparameter_Combo", "Hyperparameter_ID", "Hyperparameters", "MAPE", "Model",
  "Model_Combo", "Model_Fit", "Model_ID", "Model_Key", "Model_Name", "Model_Recipe",
  "Model_Type", "Number", "Number_Char", "Origin", "Path", "Prediction", "RMSE", "Recipe",
  "Recipe_ID", "Residual", "Residual_Std_Dev", "Rolling_MAPE", "Run_Type", "SE", "Slice_ID",
  "Sum", "Target", "Test_End", "Train_End", "Train_Test_ID", "Type", "Variable", "as2",
  "combo_list", "data", "get_export_packages", "hi_80", "hi_95", "i", "lo_80", "lo_95",
  "model_spec_1", "name", "path_ext", "predict", "read.csv", "sc", "weighted_MAPE", "where",
  "x", "num_cores", "run_info", "negative_forecast", "Forecast_Adj", "Final_Col", "lag_val", "libs",
  ".config", "Forecast_Tbl", "Model_Workflow", "id", "model_run",
  "Auto_Accept", "Feature", "Imp", "Importance", "LOFO_Var", "Var_RMSE", "Vote", "Votes", "desc",
  "term", "Column", "Box_Cox_Lambda", "get_recipie_configurable", "Agg", "Unique", "Var",
  "Var_Combo", "regressor", "regressor_tbl", "value_level_iter", ".actual", ".fitted",
  "forecast_horizon", "lag", "new_data", "object", "fit", "Row_Num", "Run_Number", "weight",
  "Total", "Weight", "batch", "variable", "type"
))

#' @importFrom magrittr %>%

#' @importFrom methods formalArgs

#' @importFrom stats sd setNames

#' @importFrom foreach %do% %dopar%

#' @importFrom lubridate %m+%

#' @importFrom kernlab ksvm

#' @import modeltime

#' @importFrom Cubist cubist cubistControl

#' @importFrom earth earth

#' @importFrom glmnet glmnet

#' @importFrom rules cubist_fit committees max_rules

# * cbind.fill custom function ----
# create function to cbind dataframes that contain different amounts of rows
# https://github.com/cvarrichio/rowr/blob/master/R/rowr.R

vert <- function(object) {
  # result<-as.data.frame(cbind(as.matrix(object)))
  if (is.list(object)) {
    object <- cbind(object)
  }
  object <- data.frame(object)

  return(object)
}

len <- function(data) {
  result <- ifelse(is.null(nrow(data)), length(data), nrow(data))
  return(result)
}

buffer <- function(x, length.out = len(x), fill = NULL, preserveClass = TRUE) {
  xclass <- class(x)
  input <- lapply(vert(x), unlist)
  results <- as.data.frame(lapply(input, rep, length.out = length.out))
  if (length.out > len(x) && !is.null(fill)) {
    results <- t(results)
    results[(length(unlist(x)) + 1):length(unlist(results))] <- fill
    results <- t(results)
  }
  if (preserveClass) {
    results <- as2(results, xclass)
  }
  return(results)
}

cbind.fill <- function(..., fill = NA) {
  inputs <- list(...)
  inputs <- lapply(inputs, vert)
  maxlength <- max(unlist(lapply(inputs, len)))
  bufferedInputs <- lapply(inputs, buffer, length.out = maxlength, fill, preserveClass = FALSE)
  return(Reduce(cbind.data.frame, bufferedInputs))
}


# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # CRAN OMP THREAD LIMIT
  Sys.setenv("OMP_THREAD_LIMIT" = 1)

  # This defines the model database
  make_cubist_multistep()
  make_glmnet_multistep()
  make_mars_multistep()
  make_svm_poly_multistep()
  make_svm_rbf_multistep()
  make_xgboost_multistep()
}
