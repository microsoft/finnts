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
- Add a `NEWS.md` entry for user-visible behavior changes; the `DESCRIPTION` version may advance for a release or user-visible change. Internal agent-guidance maintenance alone requires neither a NEWS entry nor a version change.
- Keep exactly one 9000-series development-version section in `NEWS.md`, aligned with `DESCRIPTION`. When advancing the development version (for example, `.9005` to `.9006`), rename the existing development heading and merge new notes into it; never retain separate sections for successive unreleased development versions. Preserve all accumulated unreleased notes and released-version history.
- Use short `# description` comments only where the code is not self-explanatory. Do not add decorative comment separators.
- Keep errors actionable and consistent. Use `stop()` for user-facing errors, `warning()` for recoverable conditions, and `message()` for informational output.
- Avoid unnecessary copies in tight loops; vectorize when it improves clarity or performance.
- Never add code that deletes files.

## Function Documentation

- Document every function, including internal package functions, nested helpers, and test helpers. Internal visibility is not a reason to omit documentation. Add missing documentation when creating or modifying a function; keep unrelated repository-wide backfills as separately scoped work.
- Exported functions need roxygen documentation covering purpose, parameters, return value, relevant errors and side effects, and examples when useful. Regenerate the public help with `devtools::document()` when roxygen changes.
- Internal functions need a concise contract immediately above the definition, using ordinary `#` comments or existing internal roxygen with `@noRd`. Explain why the helper exists, its inputs and returned value, and any non-obvious shape, units, missing-value, ordering, or identity assumptions. Describe storage writes, cache mutation, errors, and other side effects when applicable. Do not export a helper merely to document it.
- Keep the amount of documentation proportional to complexity. A simple helper can use a short purpose/input/output block; a policy or orchestration function needs its decision boundary, invariants, and failure behavior. Document anonymous callbacks in the enclosing contract or immediately before the callback so their role and assumptions are clear.
- Explain the rationale before non-obvious algorithm or policy blocks, especially thresholds, metric units, tie-breaking, transformation order, restart reuse, and artifact ownership. Do not narrate obvious assignments or add decorative separators.
- Update the function contract and affected workflow documentation in the same change as behavior. Link to the owning guide and focused tests for complex shared policies instead of copying a long algorithm description into every caller. During review, check that the documentation matches the implementation, not just that a comment exists.
