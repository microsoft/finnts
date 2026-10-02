# finnts 0.7.0.9009 (DEVELOPMENT VERSION)

## Improvements

- Improved model selection balances accuracy and plausibility, preserves supported growth and seasonality, and rejects invalid predictions.
- Improved averaging, forecast updates, and run recovery.

## Bug Fixes

-   Forecast updates verify saved models and forecasts, refit damaged results, and recover interrupted logging without rewriting valid outputs.
  -   Shared global updates preserve valid local winners; preparation, artifact formats, and worker payloads are unchanged.
  -   Restart checks do not prevent concurrent attempts from overwriting files.
-   Fixed future actuals becoming `0` with `clean_outliers = TRUE`; they remain `NA`, while backtests retain original actuals.
-   Nonnegative hierarchical reconciliation caps inverse-error weight ratios at `1e15`, reducing slowdowns from near-perfect fits without adding timeouts.
  -   Extreme-weight forecasts may change; valid weights and negative-allowed reconciliation remain unchanged. Invalid residual variances produce actionable errors.
-   Local and ADLS-mounted workflows read known artifacts by exact path and reuse necessary listings, reducing repeated storage discovery.
  -   Condensed forecast getters preserve batch precedence; storage failures propagate, while optional missing files and valid empty CSVs remain supported.
  -   Legacy reads, remote downloads, and Spark data-frame routing remain unchanged.
-   `update_forecast()` excludes removed series before update routing, avoiding missing-artifact errors; new series retain default local forecasts.
-   Global Agent iterations compare only hierarchy approaches supported by their input scope and consolidated EDA.
  -   Pre-expanded hierarchy `ID` inputs use `bottoms_up` internally, followed by one outer reconciliation.
  -   Bottom-level inputs may compare `bottoms_up` against their detected standard or grouped hierarchy after bottom-level reconciliation.
-   `iterate_forecast()` skips completed global optimization and resumes unfinished local series; incomplete metadata or higher iteration targets retain retries.
-   Forecast updates choose the newest completed predecessor with nonempty required final outputs, skipping canceled or incomplete versions.
  -   Required outputs include run metadata, forecasts, model summaries, and EDA; hierarchical runs also require hierarchy summaries.
  -   Predecessor metadata validates reuse fields only; other final outputs are not checked for schema, combo, or model identity.
  -   Storage and read failures remain hard errors during predecessor selection.
-   Agent duplicate detection treats `NULL` defaults, equivalent explicit values, and reordered multipart settings as identical, avoiding redundant iterations.
-   Trimmed character combo boundaries before validation and hashing, aligning artifacts across workflows; blank values and normalization collisions fail early.
-   Agent EDA summaries report absent outlier dates and omit non-finite regressor-lag correlations, keeping infinite values out of LLM context.
-   Missing intermediate forecasts now explain incomplete or inconsistent runs and recommend restarting or regenerating the missing step.
-   Validated custom `seasonal_period` values reach `stlm-arima`, `stlm-ets`, and `tbats`; default `NULL` remains logged as `NA`.
-   Agent seasonal-period proposals require values above one; exhausted correction retries preserve and finalize the best forecast.
  -   Yearly defaults use two- and three-year periods; earlier-version replay presents default-backed settings as literal `NULL`.
  -   Invalid legacy seasonal periods warn once with their original value, then use cadence defaults during replay and forecast updates.
-   Agent reasoning validates all proposed settings, using typed correction retries and graceful exhaustion.
  -   Retries reuse history; completed iterations refresh it once. Current-version runs control iteration limits, accuracy signals, duplicates, and change budgets.
  -   Earlier versions remain replay context; external-regressor exploration has no separate cap beyond the overall iteration limit.
  -   Canonical lag, rolling-window, and seasonal-period settings prevent reordered, default, or previously tested configurations consuming additional changes.
  -   Finalization and verification avoid repeated listings; best-run listing failures propagate as storage errors.

# finnts 0.7.0

## Improvements

- New AI Agent Capabilities
  - `iterate_forecast()` can use LLM's to find the optimal combination of data and inputs to create the most accurate forecast. 
  - `update_forecast()` can take previously trained models from `iterate_forecast()` to create forecasts on new data fast.  
  - `ask_agent()` can be used to ask questions about the forecast, data, or models to get insights.
  - Helper functions `set_project_info()` and `set_agent_info` to assist in iterating and updating forecasts. 
  - New functions to retrieve information from agent runs:
    - Added `get_agent_forecast()` to retrieve the final forecast output from an agent run.
    - Added `get_best_agent_run()` to retrieve the run metadata information from an agent run. 
    - Added `get_summarized_models()` to retrieve model summary information from an agent run.
    - Added `get_eda_data()` to retrieve the exploratory data analysis results from an agent run.

- New Chronos2 Model Integration
  - Added Chronos2, in addition to the existing model, to support zero-shot forecasting
  - It supports both historical and future external regressors
  - It can be used as a global model
  - Added a Chronos controller, which will support other Chronos variant API POST requests as well
  - Added two new package dependencies: `jsonlite` and `httr`
  - Integrated Chronos2 into the finn agent workflow
  - Added `chronos-bolt-base`: uses the Chronos2 API without external regressors, with `model_type = "chronos-bolt-base"`.
  - Added lightweight `chronos-bolt-tiny`: shares the Chronos API without external regressors, with `model_type = "chronos-bolt-tiny"`.

- New TimesFM Integration
  - Added TimesFM as a new foundation model for zero-shot time series forecasting
  - TimesFM is a local-only model (not global) that does not support external regressors
  - Uses its own API endpoint, configured via `TIMESFM_API_URL` and `TIMESFM_API_TOKEN` environment variables
  - Supports daily, weekly, monthly, quarterly, and yearly frequency data

- New TimeGPT Integration
  - Added TimeGPT in addition to existing model to support zero shot forecasting
  - Added support for both historical and future external regressors
  - Added timegpt-long-horizon model
  - Added finetuning for depth and layers
  - Enabled TimeGPT as a global model
  - Added support for padding time series that don't meet minimum data requirements
  - Integrated TimeGPT into the finn agent workflow
  - Optional `nixtlar` enables TimeGPT on R 4.1+; core FinnTS remains available on R 4.0.

- Updated Train Model function
  - Added debug arg to trace error while training over various models
  - Fixed differencing restoration for combo data in global models.
    
- `prep_data()` removes outliers from training data while retaining them in time-series cross-validation testing splits.

- Adaptive daily ARIMA to reduce runtime
  - Users continue to select `"arima"`; daily workflows now use the bounded `arima_fast` engine while non-daily workflows retain classic `auto_arima` behavior.
  - Daily ARIMA validates nonseasonal, weekly-difference, 364/365-day-difference, and Fourier-with-ARIMA-errors strategies on an internal holdout, then refits the simplest competitive strategy.
  - Outer backtests retain original targets when outlier cleaning is enabled.
  - Daily candidate searches use nonseasonal frequency-one ARIMA fits and never construct the expensive period-365 seasonal state-space model.
  - Failed daily ARIMA candidates use another validated strategy or deterministic drift, without timeouts or process termination.
  - Agent summaries report ARIMA engine, strategy, transformed/effective orders, Fourier/seasonal settings, validation WMAPE, candidate scores, and fallback status.
  - Declared `forecast` in `Imports`, reusing the mature ARIMA implementation already required transitively by `modeltime`.
  - This adds no runtime service, credentials, or network surface and preserves the existing open-source dependency chain.

- Updated optional variable importance to `vip` 0.5.0 from its maintainer's r-universe; `vip` remains in `Suggests`.
  - Declared `ranger` in `Suggests`; Boruta feature selection explicitly uses its ranger adapter after Boruta 10.0 changed defaults.
  - FinnTS works without feature-selection packages; requesting unavailable feature selection fails with installation guidance.
  - Without `vip`, model summaries retain everything except variable importance.

## Bug Fixes

-   Avoided MARS tuning failures by excluding CV pruning from automatic grids; explicit multistep CV pruning uses bounded folds.
-   Fixed non-ASCII combo hashing across readers, preventing mismatched input, EDA, forecast paths and `subscript out of bounds` errors.
-   Excluded models with incomplete backtest folds from best-model ranking while retaining complete candidates.
-   Fixed `null_converter()` crash in agent workflow when input is `NA`.
-   Added exponential-backoff retries, up to three, for transient Chronos and TimesFM failures: HTTP 429, 5xx, and connection errors.
-   Fixed hierarchical forecast reconciliation failure caused by floating-point Target discrepancies across models.
-   Improved error messages during hierarchical reconciliation to include the underlying error for easier debugging.
-   Fixed aggregation error when running hierarchical forecasts with standard hierarchy approach.
-   Fixed hierarchical issues when a combo variable contains a single unique value.
-   Fixed issue when reconciling standard hierarchical forecasts.
-   Fixed weighted mape calculation when target variable has negative values.
-   Support for latest xgboost 3x version.
-   Fixed model summary for global models by considering average models too.
-   Fixed global-model failures when future external-regressor values exist for only some series.
-   Fixed issue around NA handling with external regressors. 
-   Fixed issue when reconciling hierarchical forecasts that are very close to zero.
-   Fixed issue when checking if best models have been selected before. 
-   Fixed multistep Cubist, GLMnet, MARS, polynomial SVM, and radial SVM failures caused by non-unique fiscal date-index joins expanding assessment rows.
-   Multistep prediction preserves one row per assessment and rejects missing, duplicate, padded, truncated, recycled, or non-finite outputs.
-   Removed XGBoost multistep prediction padding and truncation that previously masked row-alignment defects.
-   Custom multistep `lag_periods` propagate through feature engineering, selection, training, and updates; uncovered horizons are appended as final lag boundaries.

## Breaking Changes

- `experiment_name` within `set_run_info()` has been changed to `project_name` to comply with new AI agent capabilities. 
- Migrated from `qs` to maintained, CRAN-ready [`qs2`](https://CRAN.R-project.org/package=qs2) for fast serialization and improved compression.
  - Existing `.qs` artifacts are incompatible with `qs2` and must be regenerated.

# finnts 0.6.0

## Improvements

-   Shortened global model list to just xgboost
-   Faster xgboost model training for larger datasets 
-   Faster feature selection for global model training
-   Added `seasonal_period` within `prep_models()` for more control over multiple seasonal periods in models like tbats

## Bug Fixes

-   Error in formatting of training data for global models
-   Error when using multiple external regressors with future values
-   Remove `target_log_transformation` within `prep_data()`, since `box_cox` has now replaced it for automated power transformations
-   Error when running hierarchical forecasts with weekly data

# finnts 0.5.0

## Improvements

-   Added support for hierarchical forecasting with external regressors
-   Allow global models for hierarchical forecasts
-   Multistep horizon forecasts for R1 recipe, listed as `multistep_horizon` within `prep_data()` 
-   Always save the most accurate model average, even when unselected, improving scalability for larger datasets.
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
-   Added `arimax` for ARIMA forecasting with engineered features and supplied external regressors.
-   Automated feature selection, refer to feature selection vignette for more details
-   Error handling in hierarchical forecast reconciliation
-   Box-cox and differencing transformations
-   Added new function, `list_models()`, that lists available models in the package 

## Bug Fixes

-   Best model selection
-   Hierarchical forecast reconciliation

# finnts 0.3.0

## Improvements

-   Added Spark data-frame input support for forecasting millions of time series across a cluster.
-   Updated train/validation/test process for multivariate ML models.
-   Added independently callable forecasting components for finer workflow control and production pipelines.
    -   `prep_data()`
    -   `prep_models()`
    -   `train_models()`
    -   `ensemble_models()`
    -   `final_models()`
-   Automated intermediate and final artifact reads and writes improve MLOps, scalability, and restart recovery.
    -   Temporary location on local machine, which will then get deleted after R session is closed.
  -   Local or mounted Azure Data Lake Storage paths persist intermediate and final results in Spark.
  -   Azure Blob Storage supports non-Spark data-lake runs; SharePoint/OneDrive stores results within M365.
-   Added `get_trained_models()`, `get_run_info()`, and `get_prepped_data()` to retrieve fitted models, run metadata, and feature-engineered data.

## Deprecated

-   `run_model_parallel` has been replaced with `inner_parallel` within `forecast_time_series()`
-   Data being returned as a list when running `forecast_time_series()`. Instead please use `get_forecast_data()` to retrieve Finn forecast outputs.

## Breaking Changes

-   No longer support for Azure Batch parallel processing, please use spark instead
-   Parallel Spark processing now requires an Azure Data Lake Storage mount supplied through `set_run_info()`; see the vignettes.

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
-   Added error handling for model averages so forecasting can continue despite memory failures with large model sets.
-   Extended Azure Batch task timeout from one day to one week for long-running forecasts.

## Deprecated

-   Deprecated `azure_batch` in `forecast_time_series()` following Azure Batch R package deprecation; use Spark instead.

# finnts 0.1.1

## Default Function Behavior

-   `forecast_time_series()` defaults to R1 when `run_global_models` is TRUE or NULL and `recipes_to_run` is NULL.
  -   This avoids R2 memory issues with large global-model datasets in Azure Batch.

## Bug Fixes

-   Fixed error when converting infinite values to NA values after model forecasts are created.
-   Changed the cubist model to reference the new cubist model definition in parsnip package.
-   Fixed hierarchical aggregation by replacing missing hierarchy values with zero.

# finnts 0.1.0

-   Initial CRAN Release
