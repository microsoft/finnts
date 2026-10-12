# Updates and rollback

The skill and finnts ship together from the GitHub repository. A skill update also
reinstalls finnts from the same commit when that commit's `DESCRIPTION` version is
newer than the installed one. Skill and package versions are independent; an
unchanged or already newer package is left alone.

## For finance users

Ask "Check for Finn updates and tell me what changed." Show the installed and
available versions of both the skill and finnts, then summarize the new skill
entries in [version.json](../version.json). Ask before installing anything.
For a newer skill, use the combined update; for finnts alone, use the installer
preview and confirmation below. After updating, start a new chat and check the
environment again. "Undo my last update" uses the existing backup/rollback flow.

`version.json` is the skill's release-change history, not this machine's installation
log or the R package's [release notes](../../../NEWS.md). It contains all recorded
skill versions, newest first. The current branch may contain changes not yet
published. Older installed skill builds may not include this file.
The format has top-level `version`, `released`, and `changelog` fields. Each
changelog entry contains `version`, `date`, and a `changes` array. The top-level
values match the first (newest) entry. The initial branch version is 0.1.0 with
one combined entry; the recorded date does not imply GitHub publication.

For maintainers, keep entries NEWS-style: summarize user-visible changes and
leave procedures and implementation details in the reference guides. Offline
tests enforce at most **5 bullets per version**, **200 characters per bullet**,
and **800 characters total** across that version's bullets (including spaces
and punctuation). Limits apply to each version, not the entire growing history.
These are authoring checks, not runtime restrictions on reading older histories.

`--action=changes` reads installed history without network access or settings writes.
`--action=changes --remote=true` reads GitHub history at a resolved commit;
`changes_since_installed` contains only entries newer than the running skill.
`--action=check` and the update approval preview include those entries from the
exact commit used in that comparison. Missing notes are explicitly reported in
`release_notes.error` but do not block updates or forecasts. Do not claim that
missing notes mean there were no changes. Version history is distinct from
`--action=history`, which shows local install/rollback events and backups.

## Automatic check

`check_env.R` checks GitHub at most once a week (the result is cached in
`update_check.json` in the settings folder). When `data$update.update_available` is true,
it adds a next action: run the explicit check to show changes, then ask whether to update. A failed
check (offline, proxy) never blocks Finn.

Turn the check off with any of:

- `update_skill.R --action=settings --auto_check=false` (saved in settings)
- the `FINN_SKILL_NO_UPDATE_CHECK=true` environment variable
- `check_env.R --check_updates=false` for one call

`FINN_SKILL_REPO=<owner>/<repo>` points checks, updates, and finnts installs to a fork.

## Commands

| Goal | Command |
|---|---|
| Check now | `update_skill.R --action=check` |
| Show installed skill version history (offline) | `update_skill.R --action=changes` |
| Show the latest GitHub skill history | `update_skill.R --action=changes --remote=true` |
| Update (after the user agrees) | `update_skill.R --action=update --confirm=true` |
| Undo the last update | `update_skill.R --action=rollback --confirm=true` |
| Restore an older backup | `update_skill.R --action=history`, then `--action=rollback --to=<backup id> --confirm=true` |
| Roll back only finnts | `update_skill.R --action=rollback --package_only=true --confirm=true` |
| Install the latest finnts from GitHub (even if the skill is current) | `install_finnts.R`, then `install_finnts.R --confirm=true` |
| Reinstall the same finnts build | `install_finnts.R --force=true --confirm=true` |

Without `--confirm=true`, update, rollback, and install return `needs_confirmation` with
what would change. Show it to the user before confirming.

## Latest finnts without a skill update

When the skill is current but GitHub has a newer finnts, `update_skill.R --action=check`
returns status `finnts_update_available` and `check_env.R` adds a finnts-only notice. Users
can also ask for the latest finnts at any time.

`install_finnts.R` compares the installed build with the GitHub commit it would install
(`data$comparison$status`):

| Status | Meaning |
|---|---|
| `not_installed` | finnts is not installed yet |
| `newer_version` | GitHub has a higher version number |
| `new_commit` | same version number, different code |
| `older_than_installed` | GitHub is older; the message warns this is a downgrade |
| `same_commit` | already installed; returns `up_to_date` without changes unless `--force=true` |
| `unknown` | GitHub could not be reached; installs the requested ref as before |

The exact compared commit is installed, so the build the user approved is the build they
get. If `data$github$skill_version` is newer than the skill, also offer a skill update.
The install is recorded in `update_history.json`, so
`update_skill.R --action=rollback --package_only=true --confirm=true` restores the
previous build.

## What an update does

1. Refuses while any forecast is running (`blocked`), or when the skill runs from a git
   clone (use `git pull` there, then `install_finnts.R --confirm=true`).
2. Backs up the current skill and records the installed finnts build in
   `skill_backups/<stamp>_skill-<version>/` in the settings folder.
3. Downloads the new skill files, checks every script parses, and copies them in.
   Files no longer in the new version are kept and listed in `data$stale_files`.
4. If finnts is newer, installs it from the same commit and verifies a fresh R session
   loads the expected version.
5. If any step fails, restores the backed-up skill and the previous finnts build and
   returns `error` with `data$restored = true`. Tell the user nothing changed.

Every update, failure, and rollback is logged in `update_history.json`. Nothing is ever
deleted; old backups can be removed by the user by hand.

## Rollback

A rollback first backs up the current skill (so it can be undone too), restores the
chosen backup, and reinstalls the finnts build recorded with it. For a finnts CRAN build,
the matching CRAN version is installed. `--package_only=true` reinstalls the previous
finnts build from the update history and leaves the skill unchanged.

After an update or rollback, ask the user to start a new chat so the agent rereads
`SKILL.md`. Unfinished runs resume as usual; if finnts changed, `run_forecast.R` asks
before resuming a run that started on another finnts version.
