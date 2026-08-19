## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking dependencies in R code ... NOTE
  Imports includes 39 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

  This package does leverage many outside packages. The main feature of this package is 
  that it consolidates a lot of different models into one package to run them automatically. 
  So having many required packages is important to the package. 

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
`_R_CHECK_FORCE_SUGGESTS_=false`.

`vip` requires R 4.1 or newer, while FinnTS continues to support its core
workflows on R 4.0 without this optional package.

## Downstream dependencies
There are currently no downstream dependencies for this package
