
# finnts 0.1.1.9000 (Development Version)

* Expand Azure Batch task timeout from one day to one week. Prevents errors when running large forecasts that take over a day to run in Azure. 

# finnts 0.1.1

## Default Function Behavior
* Change default behavior to only run R1 feature engineering recipe when the argument run_global_models is set to TRUE or NULL and recipes_to_run is set to NULL in the forecast_time_series function. Running R2 recipe with global models on large datasets often results in RAM issues when running in Azure Batch.

## Bug Fixes

* Fixed error when converting infinite values to NA values after model forecasts are created. 
* Changed the cubist model to reference the new cubist model definition in parsnip package.
* Fixed bug in hierarchical forecasting. Missing values in the hierarchy are converted from NA to zero, which fixes how data is aggregated at various levels of hierarchy. 

# finnts 0.1.0

* Initial CRAN Release