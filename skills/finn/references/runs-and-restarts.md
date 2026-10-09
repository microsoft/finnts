# Runs and restarts

All runs go through `scripts/run_forecast.R --project=<name>`. Never call finnts forecasting
functions directly for a run; the script records state so runs can be checked, cancelled,
and resumed.

## Modes

| `--mode` | finnts calls | use when |
|---|---|---|
| `standard` (default unless config `mode` is `agent`) | `set_run_info`, `prep_data`, `prep_models`, `train_models`, `ensemble_models`, `final_models` | a normal forecast from `config.json` settings |
| `iterate` | `set_agent_info_custom` + `iterate_forecast` | Agent searches settings per series to reduce error (needs an LLM) |
| `update` | `set_agent_info_custom` + `update_forecast` | refresh a finished Agent forecast with newer actuals, reusing what the Agent learned |

Add `--wait=true` for short runs (small data) so the command returns the final result.
Otherwise the run starts in the background and returns `started` with a `run_id`.

## Resume by default

Each launch first looks at the latest run of the same mode. If it was `interrupted`,
`failed`, or `cancelled`, the script **resumes it**: same `run_id`, same finnts run name or
Agent `request_id`, same saved `runs/<run_id>/config.json`. finnts skips work already saved
in `finn_artifacts/`, so resuming picks up where it stopped. Nothing is wiped and no new
Agent version is created. After memory failures, a resume uses fewer workers automatically.

If the settings or input data changed since that run started, the script returns
`needs_confirmation` with `changed`. Ask the user:

- resume with the original settings and data: rerun with `--confirm=true`, or
- start fresh with the new ones: rerun with `--new=true` (Agent modes also `--confirm=true`).

The same question comes up when the run started with a different finnts or R version
(`version_changes`): finishing it with the installed software can mix results from two
versions. Run state and `project.json` record `finnts_version` and `r_version` for this
check; runs from older skill releases without them are not flagged.

## Agent versions

`request_id` is stored in each run's state and passed to `set_agent_info_custom()`, so
the same Agent run is reopened on resume instead of creating a new version. A new
version (`overwrite = TRUE`) is only created when:

- the user confirms a fresh iterate (`--mode=iterate --new=true --confirm=true`), or
- an update runs (it builds on the latest finished Agent forecast).

Earlier versions and their history stay in `finn_artifacts/`.

## Updates

`--mode=update` needs a finished Agent forecast and data that ends later than it did
(`--force=true` if only past values were revised). If the series count changed, or the
file changed without newer dates, it returns `needs_confirmation` with `changes`; confirm
with the user and rerun with `--confirm=true`, or suggest a fresh iterate if the series
changed a lot. A different finnts or R version than the last Agent forecast only adds a
note that results may shift slightly.

## Result statuses to handle

| status | meaning | what to do |
|---|---|---|
| `started` / `completed` | run launched / finished (with `--wait`) | report `estimated_runtime` and `notes`; offer export and analysis |
| `busy` | another run in this project is active, or another launch is starting on this computer | check `run_status.R`; cancel only if the user asks |
| `machine_busy` | other projects' runs use the cores or memory this run needs; nothing started | tell the user what is running; relaunch after one finishes (parallelism.md) |
| `needs_setup` | Agent run but Copilot is not ready | walk through each listed fix (setup.md) |
| `invalid` | config errors | run `validate_config.R`; fix with the user |
| `needs_confirmation` | resume with changed inputs, or a new Agent version | ask the user, then rerun with the flags given |
| `already_complete` | iterate already ran on the same settings and data | show results; a new search needs a confirmed new version |
| `needs_new_version` | settings/data changed, or the earlier Agent run cannot be resumed | suggest `update` if data is newer; otherwise confirm a new version |
| `needs_agent_run` | update has no finished Agent forecast | run `--mode=iterate` first or resume the unfinished one |
| `no_new_data` | update data ends on the same date as last time | add newer actuals, or `--force=true` if past values were revised |

## Corner cases

- **Machine restarted or terminal closed:** status shows `interrupted`; rerun the same command.
- **Run on another machine:** state records the host. A run started on another computer
  keeps showing as running here (so launches return `busy` with `other_host`), and cancel
  returns `other_machine`; check or cancel it on that computer. If the user confirms that
  computer is off or the run is dead, rerun with `--takeover=true`: it marks that run
  `interrupted` here so it can be resumed. Avoid sharing one project between machines at
  the same time.
- **User edited `config.json` mid-run:** the running job uses its saved copy; the change applies next run.
- **User replaced the input file:** detected by checksum; resume asks for confirmation.
  To point the project at a newer file, rerun `setup_project.R --action=create` with the
  new `--data`; if a different file with the same name exists it asks first, then
  `--replace_data=true` keeps the old copy and saves the new one with a timestamp.
- **Older finnts without `set_agent_info_custom`:** Agent runs cannot resume by request id.
  `check_env.R` reports this; offer the GitHub install (see setup.md).
- **Standard run after a finished one:** always a new `run_id`; old outputs stay in `output/`.
