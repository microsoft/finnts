# Parallel processing

finnts offers two ways to use several cores, and rejects using both at once:

- `parallel_processing = "local_machine"`: different time series train in separate worker
  processes. Best with many series. Each worker loads its own copy of the data and models,
  so memory use grows with the number of workers.
- `inner_parallel = TRUE`: models for one series train in parallel inside that series.
  Best with few series or when global models (one model across all series) run.

`config.json` has `"parallel": "auto"` (default) or `"none"` (always sequential).
With `auto`, `run_forecast.R` picks the setup from the machine and the data
(`finn_choose_parallel()` in `scripts/finn_common.R`):

1. `per_worker_gb = 1 + 3 x input size in GB`;
   `safe_workers = min(cores - 1, floor((free memory GB - 2) / per_worker_gb))`
   (free memory assumed 8 GB if unknown).
2. **Sequential** if fewer than 4 cores, fewer than 2 safe workers, or after three memory failures.
3. **Inner parallel** if there is one series, after two memory failures, or global models
   run with fewer than 4 x safe_workers series.
4. **Local machine** if series >= 2 x safe_workers (after one memory failure, workers are halved, minimum 2).
5. Otherwise **inner parallel**.

Global models run by default for monthly, quarterly, and yearly data, not weekly or daily.

## Memory failures

When a run fails with a memory error, the next resume steps down one level automatically
(halve workers, then inner parallel, then sequential). Tell the user it will be slower but
safer. If they want to force sequential, set `"parallel": "none"` with their OK; resume
still keeps saved progress (parallel settings are not part of the resume check).

`validate_config.R` and `run_forecast.R` return the chosen plan with a plain `reason`;
share it with the user when they ask why a run is slow.

## Several forecasts at once

Users may ask for forecasts on several projects at the same time. Rules:

- **One run per project.** A second launch in the same project returns `busy`. To try
  different settings side by side, use separate projects.
- **Launch each project once, in the background**, then track all of them with
  `run_status.R --all=true`. Do not start runs in a loop or retry while one is being started.
- **Runs share the computer.** Each run records the cores and memory it claimed
  (`parallel.claimed_cores` / `claimed_gb` in its `state.json`). A new launch plans with only
  what other active runs on this computer have not claimed, so worker counts shrink and runs
  may switch to inner parallel or sequential. The plan `reason` says when a run is sharing.
  A run alone on the computer is never refused.
- **`machine_busy` means nothing was started.** Not even a sequential run fits next to the
  runs already going. Tell the user which forecasts are running and offer to relaunch after
  one finishes. Finn never queues runs. Never cancel another run to make room unless the
  user explicitly asks.
- **Agent runs** (`iterate`, `update`) each call GitHub Copilot. Several at once use more
  premium requests and may hit rate limits; tell the user before launching several.
- `validate_config.R` previews the shared plan and warns when a launch would be refused.
- Only runs under the same storage root on this computer are counted. Runs in another root,
  or finnts used outside the skill, are not seen.
