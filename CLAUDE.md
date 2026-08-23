# CLAUDE.md — Claude Code project memory for finnts

@AGENTS.md

## Claude-specific working agreements
- Start by summarizing the plan (bullets) and the files you’ll touch.
- Prefer running `devtools::test()` after code changes, and `devtools::check()` before finalizing.
- If you change roxygen comments, run `devtools::document()` and include generated diffs.
- Keep changes minimal: avoid drive-by refactors unless explicitly requested.
- If a request is ambiguous, ask the smallest number of clarifying questions needed, then proceed conservatively.

## Notes
- Agent code must create fresh, empty-history driver and reasoning Chat clones for every time-series combo. Parallel code must preserve the ellmer 0.4+ serialization path through `foreach` and avoid internal ellmer APIs or provider credential mutation.
- PSOCK tests under `devtools::test()` may need a test-only `.export` for internal helpers because `pkgload::load_all()` differs from an installed namespace. Verify the installed package in a temporary library before adding `.export` to production code.
- If this repo grows, move large topic rules into `.claude/rules/*.md` (testing, style, release).
