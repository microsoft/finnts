# Choosing a run type and settings by size

Recommend; never refuse. The user can run any size and any run type. Explain the
trade-off, offer the suggested settings, and keep the user's choice if they insist.

## Which run type

| Run type | Command | Use when | Avoid when |
|---|---|---|---|
| Standard | `--mode=standard` | Any large data set, speed matters, or no Copilot/LLM available | The data set is small to medium, accuracy matters most, and the user has time for the Agent |
| Agent iterate | `--mode=iterate` | Small to medium data (up to a few hundred series) where accuracy matters, Copilot is ready, and the user agrees their data may be sent to the LLM | Thousands of series (can take days), no data-sharing consent |
| Agent update | `--mode=update` | New actuals arrived for a project that already has a finished Agent iterate run | No earlier iterate run (run iterate first); data structure changed a lot (new series, new columns): start a new iterate run instead |

Plain-language explanation for the user:

- **Standard** trains Finn's model library once, scores every model on back tests, and
  picks the best per series. Fast and predictable.
- **Agent** uses an AI model to look at the data, try different settings over several
  iterations, and keep what improves accuracy per series. Slower, uses Copilot
  requests, often more accurate on hard series.
- **Update** reuses what the Agent learned and refits on the new data, much faster than a
  fresh iterate run.

Pick one run type for the whole data set and stay with it each period: standard runs
refresh with another standard run; Agent projects refresh with Agent update. Do not
suggest splitting series across run types, for example running standard on everything
and then sending poorly forecast or key series to the Agent in a separate project. Mixed
forecasts are harder to explain, compare, and maintain. If the user asks for that split
themselves, explain these drawbacks and follow their choice.

## Size tiers

`validate_config.R` returns `data.recommendations` and adds lines to the settings card.
Tiers by series count:

| Tier | Series | What the skill suggests |
|---|---|---|
| small | up to 100 | Finn defaults; standard or Agent both work well |
| large | 101-1,000 | Test on a 50-series sample first; spaced back tests; global models on weekly/daily data; mention the Agent's longer run time and Copilot use |
| very_large | over 1,000 | Test on a sample first; spaced back tests; global xgboost only, no local models or feature selection; standard instead of Agent; move off OneDrive |

Show `estimated_runtime` as a rough range, not a promise.

## Back testing policy

Keep back testing over the most recent year whenever history allows. For large data,
test fewer dates spread across that year (larger `back_test_spacing`) instead of fewer
dates bunched at the end. Typical suggestions (`back_test_scenarios` / `back_test_spacing`):

| Date type | large | very_large |
|---|---|---|
| month | 6 / 2 | 4 / 3 |
| week | 5 / 9 | 4 / 13 |
| day | 5 / 61 | 3 / 92 |
| quarter | 4 / 1 | 4 / 1 |
| year | Finn default | Finn default |

Finn tests one more date than `back_test_scenarios`. When history is too short for a
full year of back tests plus a forecast horizon, the skill makes no back-test suggestion
and adds a note; explain that accuracy estimates will rest on fewer tests. If the user
set their own back testing and it leaves little history to train on, relay the note.

## Test a sample first

For large and very large data, before the full run:

1. `prepare_data.R --file=<file> --sample_series=50 --combo_variables=<cols> --out_dir=<folder>`
   writes `<name>_sample50.csv` (whole series, reproducible pick; never overwrites).
2. Create a separate test project from that file, apply the suggested settings, and run
   standard.
3. Check run time and accuracy with the user, then run the full project with the same
   settings. Scale the run time by roughly the series ratio.

## Applying suggestions

Show each suggestion with its reason. Change `config.json` only after the user agrees,
then `validate_config.R --project=<name> --save=true`. If the user declines, run with
their settings. Memory and worker choices are handled separately (parallelism.md).
