# CLAUDE.md — Claude Code project memory for finnts

@AGENTS.md

## Claude-specific working agreements
- Start by summarizing the plan (bullets) and the files you’ll touch.
- Prefer running `devtools::test()` after code changes, and `devtools::check()` before finalizing.
- If you change roxygen comments, run `devtools::document()` and include generated diffs.
- Keep changes minimal: avoid drive-by refactors unless explicitly requested.
- If a request is ambiguous, ask the smallest number of clarifying questions needed, then proceed conservatively.

## Notes
- If this repo grows, move large topic rules into `.claude/rules/*.md` (testing, style, release).
