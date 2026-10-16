# Parallel processing

finnts offers two ways to use several cores, and rejects using both at once:

- `parallel_processing = "local_machine"`: different time series train in separate worker
  processes. Best with many series. Each worker loads its own copy of the data and models,
  so memory use grows with the number of workers.
- `inner_parallel = TRUE`: tuning and backtesting work within one series runs in
  parallel. Model types are not necessarily trained simultaneously. Best with few
  series or when global models (one model across all series) run.

`config.json` has `"parallel": "auto"` (default) or `"none"` (always sequential).
With `auto`, `run_forecast.R` picks the setup from the machine and the data
(`finn_choose_parallel()` in `scripts/finn_common.R`):

1. Detect the machine's logical processors and total RAM before assigning workers.
   `per_worker_gb = 0.3`;
   `safe_workers = min(max(1, logical processors - 1), floor(total RAM GB x 0.8 / 0.3))`.
   The input size does not change this flat estimate. Unknown CPU count or total
   RAM is reported and falls back to sequential processing, rather than guessing.
2. **Sequential** if fewer than 2 budgeted workers, or after three memory failures.
3. **Inner parallel** if there is one series, after two memory failures, or global models
   run with fewer than 4 x safe_workers series.
4. **Local machine** if series >= 2 x safe_workers (after one memory failure, workers are halved, minimum 2).
5. Otherwise **inner parallel**.

Global models run by default for monthly, quarterly, and yearly data, not weekly or daily.

The 80 percent allowance is a **planning budget**, not a RAM reservation or enforced
limit. Actual workers may exceed 300 MB, particularly with large data or model tuning.
Low currently available RAM triggers a warning, not a lower allocation. The operating
system can reclaim memory or page, but this may slow Finn and other applications.
Missing available-RAM information also produces a warning.

For example, 22 logical processors and 31.65 GB total RAM allow up to 21 workers:
the RAM budget is 25.32 GB, enough for 84 estimated workers, so CPU is the limit.
Outer parallelism is also capped at useful series/tasks where known; inner work
may not keep every allocated worker busy.

To request fewer workers, pass `--num_cores=4` to both `validate_config.R` and
`run_forecast.R`. This is a launch-specific ceiling, not a saved project setting.
A request above the shared budget is reduced with a warning; it never overcommits
capacity silently. `"parallel": "none"` still forces sequential processing.

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
- **Runs share the computer, first-come, first-served.** Each run records the cores and memory it claimed
  (`parallel.claimed_cores` / `claimed_gb` in its `state.json`). A new launch plans with only
  what other active runs on this computer have not claimed, so worker counts shrink and runs
  may switch to inner parallel or sequential. The plan `reason` says when a run is sharing.
  Sequential runs count as one worker at 300 MB. Existing runs are never resized.
- **For a planned batch of N concurrent projects**, pass `--batch_size=N` on every
  preview and launch. Each gets at most one Nth of the full CPU/RAM budget, still
  capped by remaining capacity. On the example machine, three submissions can
  get seven workers each. Launch each once; this does not reserve the whole batch
  atomically, queue blocked projects, or take capacity from already-running runs.
- **`machine_busy` means nothing was started.** Not even a sequential run fits next to the
  runs already going. Tell the user which forecasts are running and offer to relaunch after
  one finishes. Finn never queues runs. Never cancel another run to make room unless the
  user explicitly asks.
- **Agent runs** (`iterate`, `update`) each call GitHub Copilot. Several at once use more
  premium requests and may hit rate limits; tell the user before launching several.
- `validate_config.R` previews the shared plan and warns when a launch would be refused.
- A local `~/.finn/machine-runs.json` registry tracks state paths and allocations
  across storage roots used by this skill, for the current OS user. It contains no
  input rows or forecast values. An atomic machine launch lock prevents two new
  launchers from claiming the same capacity. Released lock directories are kept,
  not deleted.
- Completed, failed, or cancelled runs release their claims after process exit
  is confirmed. Missing states and unknown process liveness retain claims and
  report uncertainty; unreadable legacy state fails explicitly rather than
  silently ignoring possible active runs. A crashed launch can be recovered.
- Existing runs in known roots are discovered, and their older recorded claims
  are preserved. Legacy runs in roots never used by this version, another OS
  user's runs, or finnts used outside the skill cannot be automatically accounted
  for. Tell the user before combining such runs.
- `run_status.R --all=true` shows local allocations across registered roots,
  including their root paths. Use `--root=<root>` for per-project commands when
  projects live in different roots.
