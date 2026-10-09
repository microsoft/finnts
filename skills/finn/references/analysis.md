# Analyzing inputs and results

Forecast runs always go through the fixed scripts. Analysis is different: write a short,
custom R script for the user's exact question, using finnts functions and the read-only
helpers below. Do not use a canned report, and **never call `finnts::ask_agent()`**; you are
the analyst, and your script should compute the answer directly from saved data.

## Rules

- Read only. Never call `forecast_time_series()`, `iterate_forecast()`, `update_forecast()`,
  `set_run_info()`, or anything that trains, writes run history, or uses an LLM.
- Save the script in `<project>/analysis/scripts/<short-name>.R` so the user can re-run it.
  Do not replace an existing file; pick a new name.
- Save results with `finn_save_analysis()`, which writes to `<project>/analysis/` and never
  overwrites. Report the path and the key numbers in plain language.
- Accuracy is `1 - WMAPE` on back-test rows. Say "about 92% accurate on past periods"
  rather than quoting raw metrics.
- A `Model_ID` that joins several model names is a simple average of those models.
  Series names join their columns with `--`.

## Running a script

Set `FINN_SKILL_SCRIPTS` to this skill's `scripts` folder and run with the Rscript from
`find_rscript`:

```powershell
$env:FINN_SKILL_SCRIPTS = "<skill>\scripts"
& "<Rscript>" "<project>\analysis\scripts\accuracy.R"
```

## Helpers (`scripts/finn_analysis_helpers.R`)

| Function | Returns |
|---|---|
| `finn_open(project, root = NULL)` | context: paths, config, runs |
| `finn_pick_run(ctx, run_id = NULL, mode = NULL)` | newest completed run, or the one named |
| `finn_forecast(ctx, run_id = NULL, best_only = TRUE)` | saved forecast table (Combo, Model_ID, Run_Type, Date, Forecast, Target, Best_Model, intervals) |
| `finn_input(ctx)` | input data cleaned exactly as runs see it (original column names) |
| `finn_history(ctx)` | history as Combo, Date, Target (same shape as the forecast table) |
| `finn_accuracy_by_series(df)` | WMAPE and Accuracy per series, worst first |
| `finn_compare_runs(ctx, run_a, run_b)` | per-series future-forecast totals and back-test accuracy for two runs, with changes, largest change first ("what changed since last month?") |
| `finn_project_handle(ctx)` | finnts `project_info` |
| `finn_run_handle(ctx, run_id)` | finnts `run_info` for standard runs, for `get_forecast_data()`, `get_prepped_data()`, `get_prepped_models()`, `get_trained_models()` |
| `finn_agent_handle(ctx, run_id)` | read-only Agent info for `get_agent_forecast()`, `get_best_agent_run()`, `get_eda_data()`, `get_summarized_models()`; never pass it to iterate or update |
| `finn_save_analysis(x, name, ctx)` | writes CSV (data frame), PNG (ggplot), or TXT |

## Examples

Accuracy by series:

```r
source(file.path(Sys.getenv("FINN_SKILL_SCRIPTS"), "finn_analysis_helpers.R"))
ctx <- finn_open("Revenue")
acc <- finn_accuracy_by_series(finn_forecast(ctx))
print(head(acc, 10))
finn_save_analysis(acc, "accuracy_by_series", ctx)
```

Which models won, and how often:

```r
source(file.path(Sys.getenv("FINN_SKILL_SCRIPTS"), "finn_analysis_helpers.R"))
ctx <- finn_open("Revenue")
fc <- finn_forecast(ctx)
wins <- unique(fc[, c("Combo", "Model_ID")])
print(sort(table(wins$Model_ID), decreasing = TRUE))
```

Forecast next to history for one series (needs ggplot2):

```r
source(file.path(Sys.getenv("FINN_SKILL_SCRIPTS"), "finn_analysis_helpers.R"))
ctx <- finn_open("Revenue")
hist <- finn_history(ctx)
fc <- finn_forecast(ctx)
series <- fc$Combo[1]
fut <- fc[fc$Combo == series & fc$Run_Type == "Future_Forecast", ]
past <- hist[hist$Combo == series, ]
p <- ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(Date, Target), past) +
  ggplot2::geom_line(ggplot2::aes(Date, Forecast), fut, colour = "steelblue") +
  ggplot2::labs(title = series, x = NULL, y = NULL)
finn_save_analysis(p, paste0("forecast_", series), ctx)
```

Forecast versus the same periods last year (monthly, quarterly, or yearly data):

```r
source(file.path(Sys.getenv("FINN_SKILL_SCRIPTS"), "finn_analysis_helpers.R"))
ctx <- finn_open("Revenue")
fut <- finn_forecast(ctx)
fut <- fut[fut$Run_Type == "Future_Forecast", c("Combo", "Date", "Forecast")]
ly <- as.POSIXlt(fut$Date)
ly$year <- ly$year - 1
fut$Date <- as.Date(ly)
both <- merge(fut, finn_history(ctx)[, c("Combo", "Date", "Target")])
growth <- merge(aggregate(Forecast ~ Combo, both, sum), aggregate(Target ~ Combo, both, sum))
growth$Growth <- growth$Forecast / growth$Target - 1
finn_save_analysis(growth, "forecast_vs_last_year", ctx)
```

Check column names with `names()` before relying on them. `finn_input()` keeps the
user's original column names; use `finn_history()` when joining to forecasts.
