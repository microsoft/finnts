# finnts 0.5.9001 (development version)

## Improvements

-   Shortened global model list to just xgboost
-   Faster xgboost model training for larger datasets 
-   Faster feature selection for global model training

## Bug Fixes

-   Error in formatting of training data for global models

# finnts 0.5.0

## Improvements

-   Added support for hierarchical forecasting with external regressors
-   Allow global models for hierarchical forecasts
-   Multistep horizon forecasts for R1 recipe, listed as `multistep_horizon` within `prep_data()` 
-   Always save the most accurate model average, regardless if selected as best model. This allows for improved scaling with large data sets.
-   Automatically condense large forecasts (+3k time series) into smaller amount of files to make it easier to read forecast outputs
-   Improved weighted MAPE calculation across all time series
-   Changed default for box_cox argument in `prep_data()` to FALSE
-   Support for spark version 3.4 in Azure Synapse/Fabric

## Bug Fixes

-   Error in run_type column join in final forecast output
-   Error in running feature selection

## Breaking Changes

-   Minimum R version now set to R 4.0 to comply with package dependency minimum version for tune

# finnts 0.4.0

## Improvements

-   Tidymodels speed up
-   Added external regressor support for ARIMA by introducing a new model option of `arimax`, which uses engineered features in addition to any external regressors supplied.
-   Automated feature selection, refer to feature selection vignette for more details
-   Error handling in hierarchical forecast reconciliation
-   Box-cox and differencing transformations
-   Added new function, `list_models()`, that lists available models in the package 

## Bug Fixes

-   Best model selection
-   Hierarchical forecast reconciliation

# finnts 0.3.0

## Improvements

-   Spark data frame support. Initial input data can now be a spark data frame, enabling millions of time series to be ran across a spark compute cluster.
-   Updated train/validation/test process for multivariate ML models.
-   In addition to existing `forecast_time_series()`, added new sub components of the finnts forecast process that can be called separately or in a production pipeline. Allows for more control of the forecast process
    -   `prep_data()`
    -   `prep_models()`
    -   `train_models()`
    -   `ensemble_models()`
    -   `final_models()`
-   Automated read and write capabilities. Intermediate and final Finn outputs are now automatically written to disk (see options below). This creates better MLOps capabilities, easier scale on spark, and better fault tolerance by not needing to start the whole forecast process over from scratch if an error occurred.
    -   Temporary location on local machine, which will then get deleted after R session is closed.
    -   Path on local machine or a mounted Azure Data Lake Storage path in spark to save the intermediate and final Finn run results.
    -   Azure Blob Storage to store non-spark runs on a data lake. SharePoint/OneDrive storage to store non-spark runs within M365.
-   New MLOps features that allow you to retrieve the final trained models through `get_trained_models()`, get specific run information thorough `get_run_info()`, and even retrieve the initial feature engineered data through `get_prepped_data()`.

## Deprecated

-   `run_model_parallel` has been replaced with `inner_parallel` within `forecast_time_series()`
-   Data being returned as a list when running `forecast_time_series()`. Instead please use `get_forecast_data()` to retrieve Finn forecast outputs.

## Breaking Changes

-   No longer support for Azure Batch parallel processing, please use spark instead
-   Parallel processing through spark now needs a mounted Azure Data Lake Storage path supplied through `set_run_info()`. Please refer to the vignettes for more details.

# finnts 0.2.4

## Dependency Fixes

-   Fixed dependency issue with timetk. 

# finnts 0.2.3

## Dependency Fixes

-   Removed package dependency modeltime.gluonts and its deep learning models because the package is no longer on CRAN.

# finnts 0.2.2

## Bug Fixes

-   Fixed hierarchical forecast reconciliation issues for certain forecasts that have high residuals. 
-   Compliant with latest dplyr v1.1.0

# finnts 0.2.1

## Bug Fixes

-   Fixed feature engineering issue around NaN/Inf values when computing log values of negative external regressor values.
-   Fixed issue of ensuring random seed is set correctly in parallel processing.

# finnts 0.2.0

## Improvements

-   Added spark support to run Finn in parallel on Azure Databricks or Azure Synapse.
-   Added error handling when creating simple model averages. Should allow forecast to keep running even if there are memory issues when averaging individual forecast models, which helps on large data sets.
-   Expand Azure Batch task timeout from one day to one week. Prevents errors when running large forecasts that take over a day to run in Azure Batch.

## Deprecated

-   Deprecated azure_batch parallel compute option within forecast_time_series function since the Azure Batch R packages are deprecated. Please use the new integration with spark on Azure.

# finnts 0.1.1

## Default Function Behavior

-   Change default behavior to only run R1 feature engineering recipe when the argument run_global_models is set to TRUE or NULL and recipes_to_run is set to NULL in the forecast_time_series function. Running R2 recipe with global models on large data sets often results in RAM issues when running in Azure Batch.

## Bug Fixes

-   Fixed error when converting infinite values to NA values after model forecasts are created.
-   Changed the cubist model to reference the new cubist model definition in parsnip package.
-   Fixed bug in hierarchical forecasting. Missing values in the hierarchy are converted from NA to zero, which fixes how data is aggregated at various levels of hierarchy.

# finnts 0.1.0

-   Initial CRAN Release
