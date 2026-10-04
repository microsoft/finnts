---
name: finnts-validation
description: "Use when validating a FinnTS code, test, documentation, dependency, Agent workflow, multistep model, or release change. Honors targeted-tests-only development scope and selects affected checks, CRAN profiles, documentation generation, or explicit full validation."
---

<!-- Generated from .claude/skills/finnts-validation/SKILL.md. Do not edit directly. -->

# FinnTS Validation

Use this workflow after making changes or when asked what checks a FinnTS pull request needs.

Follow [AGENTS.md's validation scope](../../../AGENTS.md#validation-scope): targeted checks are the interactive development default; full checks qualify releases or satisfy an explicit full-validation request. Bind the scope in the plan and seek approval before expanding it. Targeted completion must not be described as package-wide or release validation.

## Procedure

1. Inspect the changed files without reverting unrelated work.
2. Read the matching `.claude/rules/*.md` files and `.github/agent-guides/validation-matrix.md`.
3. Select `targeted` or `full` validation scope. Identify the smallest executable check that can disprove the implementation hypothesis and the affected-contract follow-up checks.
4. Run that focused check before making adjacent edits. If it fails, repair the same behavioral slice and rerun it.
5. Regenerate roxygen output only when roxygen source changed. Inspect generated `NAMESPACE` and `man/*.Rd` changes; never edit them directly.
6. Run the deterministic CRAN profile for changes to skips, credentials, Agent reasoning, or provider integrations.
7. In targeted scope, stop after the approved affected checks, documentation generation and integrity review. Do not run the unfiltered suite or `devtools::check()` merely because public API, metadata, or multiple files changed. In full scope, use the matrix's full follow-up checks.
8. Report commands run, outcomes, selected scope, skipped live integrations, and checks not run. For targeted completion, say "Targeted tests passed; full validation not run by request." Retain any known earlier package-check issues.

After changing canonical rules or this skill, run `Rscript tools/generate-agent-adapters.R` before validation. After changing instruction discovery, run the representative cases in `.github/agent-guides/evaluation-cases.md` in GitHub Copilot, Claude Code, Codex, and Cursor as applicable.

## Guardrails

- Never expose credentials or print secret environment variables.
- Never remove `skip_on_cran()` to force a local integration test.
- Never treat a credential-based skip as live-provider validation.
- Do not run the full suite repeatedly when a focused test can distinguish the current failure.
- Never turn a targeted-only request into a full suite, R CMD check, or broad installed regression run without approval.
- Do not modify package behavior merely to make environmental or optional-package checks pass.
