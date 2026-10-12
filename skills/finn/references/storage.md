# Storage: where Finn keeps projects

## Default

All projects live under one Finn folder. The default is a `Finn` folder in the user's
OneDrive (so work is backed up); if OneDrive is not found, `Documents/Finn`.
`setup_project.R --action=status` shows the suggestion; always ask the user to accept it or
choose another folder, then save with `--action=set_root --root=<folder> --confirm=true`.
The choice is stored in `~/.finn/settings.json` (override the folder with `FINN_SETTINGS_DIR`).

Temporary folders are refused. Forecasts and run history must outlive the session.

## One folder per project

```
<root>/<project>/input/  output/  finn_artifacts/  runs/  analysis/  config.json  project.json
```

Everything about a project (inputs, outputs, finnts working files, and run logs) stays
together, so the user can zip, share, or back up one folder. `finn_artifacts/` is the
finnts `path=`; never edit or delete files there.

## OneDrive and many files

finnts writes many small files per series (prepped data, models, forecasts, logs). On a
synced folder this can cause:

- slow syncing and high CPU from the sync client during and after a run;
- brief file locks while a file uploads, which can make a write fail;
- "files on demand" placeholders that must download before a resume can read them;
- sync conflicts (`name-PCNAME.ext` copies) if two machines touch the same project;
- storage quota use from large model files.

Guidance to give users:

- Runs with **100 or more series** on OneDrive: `run_forecast.R` returns a note. Tell the
  user syncing may slow down and they can pause OneDrive sync during the run.
- After **three or more large OneDrive runs**, the skill suggests moving. Offer to copy the
  whole Finn folder to a local folder the user picks (for example `C:\Users\<me>\Documents\Finn`):
  `setup_project.R --action=move_root --to=<folder> --confirm=true`.
  This **copies** projects and switches the saved root. It never deletes the old folder;
  tell the user they can remove it themselves once they have checked the copy.
  If they decline, `--action=dismiss_move_suggestion` stops the reminder.
- Never run the same project from two machines at once.
- If a run fails with a file-locked error, `diagnose_run.R` says so; resume after sync settles.
- With **Files On-Demand**, ask the user to right-click the Finn folder and choose
  "Always keep on this device" so resumes and analysis do not wait on downloads.
- Work machines may have both a personal and a business OneDrive (`OneDrive` and
  `OneDriveCommercial`). Confirm which one the suggested root uses before saving it.
- Keep the root path short; very long Windows paths can make finnts writes fail
  (`create` warns about this).
- `setup_project.R --action=disk_usage --project=<name>` reports project size. It never
  deletes anything; users archive old `runs/` and `output/` folders themselves.
- `run_status.R` and `diagnose_run.R` report OneDrive conflict copies of Finn's own files
  (for example `state-LAPTOP.json`, `project (1).json`). Finn reads only the original
  names; ask the user which computer is the main one and let them tidy the copies.
- `check_env.R` and launches warn when less than 5 GB is free on the drive. `diagnose_run.R`
  reports a full drive as `disk_space`; free space or `move_root` elsewhere, then resume.
- Renaming, archiving, and uninstalling are manual; see "Common requests" in
  data-and-settings.md. Never rename a project folder.
