# Status, cancel, and debugging

## Run status

`run_status.R --project=<name>` shows the active run (or the latest) and up to 10 recent
runs. Add `--run_id=<id>` for one run with its last log lines (`--log_lines=40` for more).
`run_status.R --all=true` lists every queued or running run in every project under the
storage root; use it when the user asks "is anything still running?".

Report to the user in one line: status, percent, stage label, elapsed minutes. Example:
"Your forecast is 62% done, training models (14 of 20 series finished), 18 minutes so far."

Statuses: `queued`, `running`, `completed`, `failed`, `cancelled`, `interrupted` (the worker
process is gone without finishing, for example after a restart or closed terminal).

### Stages and percent

Standard runs:

| stage | label | weight |
|---|---|---|
| prep_data | Preparing data | 15% |
| prep_models | Setting up back tests and models | 5% |
| train_models | Training models | 65%, by share of series with saved forecasts |
| ensemble_models | Training ensemble models | 5% |
| final_models | Selecting the best models | 10% |
| collect_results | Saving results | final step |

Agent runs: `agent_setup`, then `iterate_forecast` or `update_forecast`. Percent is
estimated from how many global and per-series searches have saved a best result, so it
moves in steps. Agent runs can take hours on many series.

Percent is an estimate; never promise a finish time. Failed, cancelled, or interrupted
runs cap at 99%.

### Health hints

- `stalled_minutes`: nothing has been written for 60+ minutes (120+ for Agent runs). Long
  model steps can be quiet; run `diagnose_run.R` and check the log before suggesting a
  cancel.
- `liveness_unknown`: the `ps` package is missing, so the worker process cannot be
  checked. Offer `install_finnts.R`.
- A run that just launched can show `queued` for a short moment; that is normal.
- OneDrive conflict copies of Finn files (for example `state-PCNAME.json`) are reported in
  `next_actions`; ask which computer is the main one. Finn reads only the original names.
- Sleep or hibernation stops a background run; it shows `interrupted` afterwards. For long
  runs, ask the user to keep the computer plugged in and awake, then resume.
- Two launches at the same moment: one gets `busy`. Check status and do not retry in a
  loop.
- `machine_busy` on launch: runs in other projects already use this computer's capacity.
  `data.active_runs` lists them with their claimed cores and memory. Relaunch after one
  finishes; never cancel others unless the user asks (parallelism.md).
- A run slower than usual while other projects run: check its `parallel.reason` in
  `state.json`; a shared plan uses fewer workers.

## Cancel

Only after the user asks or clearly agrees:

```
Rscript scripts/run_cancel.R --project=<name> --run_id=<id> --confirm=true
```

Without `--confirm=true` it returns `needs_confirmation`. Cancel stops the worker and its
child processes and marks the run `cancelled`. It deletes nothing; saved progress is kept
and the run can be resumed later with the same `run_forecast.R` command. A run started on
another computer returns `other_machine` and must be cancelled there. If that computer is
off or the run died there, the user can confirm and launch with
`run_forecast.R ... --takeover=true` to mark it `interrupted` and resume it here.

## Debugging a run

1. `run_status.R --run_id=<id>` to see status, stage, and the error.
2. `diagnose_run.R --project=<name> [--run_id=<id>]` (defaults to the latest run). It returns
   `category`, a plain-language `cause`, `error_lines` from the log, and `next_actions`.
   Secrets are redacted from the log text it returns.
3. Explain the cause to the user in finance terms, propose the fix, and get approval for
   anything that changes settings, data, installs, or creates a new Agent version.
4. Apply the fix, then resume with the same `run_forecast.R --mode=...` command.

Other `diagnose_run` statuses: `running` (healthy), `possibly_stuck` (log quiet for over
an hour; large training can be quiet, so check CPU before cancelling), `completed`,
`cancelled`, `not_found`, `no_runs`.

For `unknown`, read `log_file` directly and, if useful, write a small free-form R check on
the input data (see analysis.md). Category fixes are in troubleshooting.md.
