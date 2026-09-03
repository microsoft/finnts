# CLAUDE.md - Claude Code project memory for finnts

@AGENTS.md

## Claude Code

- Start nontrivial work with a concise plan naming the files likely to change.
- Load and follow matching path-scoped files under `.claude/rules/`; use `/context` when diagnosing whether instructions were loaded.
- Keep project-specific instructions version-controlled. Put personal or machine-specific preferences in an ignored `CLAUDE.local.md`, never in this file.
- Keep this file limited to Claude-specific behavior. Shared package guidance belongs in `AGENTS.md` or a scoped rule.
