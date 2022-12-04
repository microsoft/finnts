# finnts DEVELOPMENT VERSION

## Bug Fixes

-   Fixed hierarchical forecast reconciliation issues. 

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
