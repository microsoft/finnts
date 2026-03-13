# define global variables to prevent notes in R CMD Check
utils::globalVariables(c(
  ".", ".estimate", ".id", ".key", ".pred", "Back_Test_Scenario", "Best_Model", "Col", "Combo",
  "Combo_Hash", "Combo_ID", "Combo_Total", "Count", "Data", "Date", "Date_Adj", "Date_Day",
  "Date_half", "Date_index.num", "Date_quarter", "date_seq", "Date_year", "File", "Forecast", "Horizon",
  "Hyperparameter_Combo", "Hyperparameter_ID", "Hyperparameters", "latest_date", "n_rows", "needs_padding", "MAPE", "Model",
  "Model_Combo", "Model_Fit", "Model_ID", "Model_Key", "Model_Name", "Model_Recipe",
  "Model_Type", "Number", "Number_Char", "Origin", "Path", "Prediction", "RMSE", "Recipe",
  "Recipe_ID", "Residual", "Residual_Std_Dev", "Rolling_MAPE", "Run_Type", "SE", "Slice_ID",
  "Sum", "Target", "Test_End", "Train_End", "Train_Test_ID", "Type", "Variable",
  "combo_list", "data", "get_export_packages", "hi_80", "hi_95", "i", "lo_80", "lo_95",
  "model_spec_1", "name", "path_ext", "predict", "read.csv", "sc", "weighted_MAPE", "where",
  "x", "num_cores", "run_info", "negative_forecast", "Forecast_Adj", "Final_Col", "lag_val", "libs",
  ".config", "Forecast_Tbl", "Model_Workflow", "id", "model_run",
  "Auto_Accept", "Feature", "Imp", "Importance", "LOFO_Var", "Var_RMSE", "Vote", "Votes", "desc",
  "term", "Column", "Box_Cox_Lambda", "get_recipie_configurable", "Agg", "Unique", "Var",
  "Var_Combo", "regressor", "regressor_tbl", "value_level_iter", ".actual", ".fitted",
  "forecast_horizon", "lag", "new_data", "object", "fit", "Row_Num", "Run_Number", "weight",
  "Total", "Weight", "batch", "variable", "type", "Avg_dCor", "Drop", "Has_Future",
  "ID", "Lag", "Regressor", "Significant", "start_date", "Stationary", "Target_Original", "Value", "y",
  "agent_forecast_approach", "agent_version", "best_run", "best_run_name", "combo",
  "combo_hash", "created", "dCor", "data_output", "first_outlier_dt", "from",
  "getFromNamespace", "head", "improvement_pct", "lag_periods", "last_outlier_dt",
  "latest_weighted_mape", "longest_gap", "missing_count", "missing_pct", "model_avg_wmape",
  "model_median_wmape", "model_std_wmape", "model_type", "n", "n_parent", "object_output",
  "outlier_count", "path", "performance_flag", "previous_weighted_mape", "project_name",
  "rolling_window_periods", "run_name", "run_number", "stationary_adf", "stationary_kpss",
  "to", "total_rows", "weighted_mape", "Analysis_Type", "Metric", "Value_Numeric",
  "is_stationary", "outlier_pct", "model_class", "section", "value", "Hierarchy_Level",
  "Sort_Order", "run_id", "date_type", "file_path", "models_to_run", "underscore_count",
  "max_iterations", "run_complete"
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

#' @importFrom stats median

get_timestamp <- function() {
  as.POSIXct(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
    format = "%Y%m%dT%H%M%SZ", tz = "UTC"
  )
}

# Version-safe xgboost accessors (compatible with both 1.x and 3.x)
xgb_get_niter <- function(model) {
  tryCatch(
    xgboost::xgb.get.num.boosted.rounds(model),
    error = function(e) model$niter
  )
}

xgb_get_feature_names <- function(model) {
  tryCatch(
    xgboost::getinfo(model, "feature_name"),
    error = function(e) model$feature_names
  )
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
  make_timegpt_model()
  make_chronos2_model()
}
