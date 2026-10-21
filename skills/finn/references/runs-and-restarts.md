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

Agent LLM changes (provider, model, command, timeout, or credential-variable names)
also count as settings drift. Confirmed resumes keep the original LLM snapshot;
choose a fresh run to apply those changes. Environment-variable values are not
saved or fingerprinted, so repairing credentials for the same provider can still
resume. Older runs with saved configuration are compared using that configuration,
not just their older fingerprint. Parallel settings remain adjustable on resume.

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

After a finished iterate run, launching iterate again with only a higher
`agent.max_iter` (same data and other settings) continues the same Agent version: the
Agent runs only the extra iterations. Any other change still needs a new version.

Earlier versions and their history stay in `finn_artifacts/`.

## Updates

`--mode=update` needs a finished Agent forecast and data that ends later than it did
(`--force=true` if only past values were revised). If `hist_end_date` is set to an
earlier date than the newest actuals, it returns `no_new_data` and says so; move
`hist_end_date` with the user's approval. If the only newer period is a month, quarter,
or year that has not ended yet, `no_new_data` says so and suggests waiting until it ends
rather than moving `hist_end_date` into partial actuals. If the series count changed,
the file changed without newer dates, or settings differ from the last Agent forecast,
it returns `needs_confirmation` with `changes`; confirm with the user and rerun with
`--confirm=true`, or suggest a fresh iterate if the series or settings changed a lot.
Projects whose last forecast was standard are refreshed with another standard run, not
an update. A different finnts or R version than the last Agent forecast only adds a
note that results may shift slightly.

## Result statuses to handle

| status | meaning | what to do |
|---|---|---|
| `started` / `completed` | run launched / finished (with `--wait`) | report `estimated_runtime` and `notes`; offer export and analysis |
| `busy` | another run in this project is active, or another launch is starting on this computer | check `run_status.R`; cancel only if the user asks |
| `machine_busy` | other projects' runs use the cores or memory this run needs; nothing started | tell the user what is running; relaunch after one finishes (parallelism.md) |
| `needs_setup` | Agent run but Copilot is not ready | walk through each listed fix (setup.md) |
| `invalid` | config errors | run `validate_config.R`; fix with the user |
| `needs_review` | the saved settings were reviewed for another run type | rerun `validate_config.R --run_type=<mode>`, show the review, save, then launch |
| `needs_confirmation` | resume with changed inputs, update with changed settings, or a new Agent version | ask the user, then rerun with the flags given |
| `already_complete` | iterate already ran on the same settings and data | show results; raise `agent.max_iter` to continue, or confirm a new version |
| `needs_new_version` | settings/data changed, or the earlier Agent run cannot be resumed | suggest `update` if data is newer; otherwise confirm a new version |
| `needs_agent_run` | update has no finished Agent forecast | run `--mode=iterate` first or resume the unfinished one; standard projects refresh with `--mode=standard` |
| `no_new_data` | update data ends on the same date as last time, `hist_end_date` holds it back, or the only newer period has not ended | add newer actuals, move `hist_end_date`, wait for the open period to end, or `--force=true` if past values were revised |

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
