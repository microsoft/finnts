## R CMD check results
There were no ERRORs or WARNINGs.

There were 3 NOTEs:

* checking CRAN incoming feasibility ... NOTE
  Suggests or Enhances not in mainstream repositories:
    vip
  Availability using Additional_repositories specification:
    vip   yes   https://bgreenwell.r-universe.dev

  `vip` is an optional feature-selection dependency available from the
  maintainer's CRAN-like r-universe repository declared in DESCRIPTION. Its
  optional use and the checks performed without it are described below.

* checking package dependencies ... NOTE
  Imports includes 39 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

  This package does leverage many outside packages. The main feature of this package is 
  that it consolidates a lot of different models into one package to run them automatically. 
  So having many required packages is important to the package.

* checking for future file timestamps ... NOTE
  unable to verify current time

  The check host was unable to verify its current time. No future-dated
  package files were reported.

## Optional suggested package

`vip (>= 0.5.0)` is listed in `Suggests` and is available from its maintainer's
CRAN-like r-universe repository. The repository is declared in DESCRIPTION as:

```
Additional_repositories: https://bgreenwell.r-universe.dev
```

All `vip` usage is conditional. The package installs, loads, and passes its
CRAN-style test profile without `vip`. Feature selection reports actionable
installation guidance only when requested, while model summaries retain all
non-importance sections. A dedicated CI job verifies this no-`vip` path with
`_R_CHECK_FORCE_SUGGESTS_=false` while also excluding the other optional
feature-selection packages (`Boruta`, `corrr`, and `ranger`).

`vip` requires R 4.1 or newer, while FinnTS continues to support its core
workflows on R 4.0 without this optional package.

`ranger` is also declared directly in `Suggests` because FinnTS invokes its
parsnip engine during optional feature selection and uses Boruta's ranger
importance adapter. Boruta 10.0 moved `ranger` from a strong dependency to
`Suggests` and changed its default provider to `fru`; relying on that default
both changed established behavior and produced cross-platform failures on the
package's prepared feature data. Pinning an older Boruta release was rejected
because it would prevent normal dependency updates. `ranger` is a mature CRAN
package under GPL-3 and remains optional, so this declaration adds no new
dependency to FinnTS installation or its core workflows.

## Downstream dependencies
There are currently no downstream dependencies for this package
