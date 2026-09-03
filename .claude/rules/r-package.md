---
paths:
  - "R/**/*.R"
  - "DESCRIPTION"
  - "NAMESPACE"
  - "man/**/*.Rd"
  - "vignettes/**/*.{Rmd,qmd}"
  - "NEWS.md"
---

# R Package Rules

- Keep exported functions stable. Ask before introducing a breaking API change, and document an approved breaking change in `NEWS.md`.
- Prefer small, composable functions and packages already declared in `Depends`, `Imports`, or `Suggests`.
- Add a package dependency only for a concrete feature when existing dependencies and a small maintainable implementation are inadequate, unsafe, or would recreate substantial mature functionality. Do not add dependencies for bug fixes, documentation, formatting, developer convenience, or trivial helpers, and do not vendor third-party source.
- A dependency-changing pull request must explain alternatives, necessity, maintenance/security/license implications, placement in `Imports` or `Suggests`, and version constraints. Validate it with a clean-library installation and `devtools::check()`.
- Update roxygen comments for public API changes, then run `R -q -e 'devtools::document()'`. Never hand-edit `NAMESPACE` or `man/*.Rd`.
- Add a `NEWS.md` entry and bump `DESCRIPTION` only for a release or user-visible behavior change. Internal agent-guidance maintenance alone does not require either change.
- Use short `# description` comments only where the code is not self-explanatory. Do not add decorative comment separators.
- Keep errors actionable and consistent. Use `stop()` for user-facing errors, `warning()` for recoverable conditions, and `message()` for informational output.
- Avoid unnecessary copies in tight loops; vectorize when it improves clarity or performance.
- Never add code that deletes files.
