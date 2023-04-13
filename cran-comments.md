
## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking dependencies in R code ... NOTE
  Imports includes 28 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

  This package does leverage many outside packages. The main feature of this package is 
  that it consolidates a lot of different models into one package to run them automatically. 
  So having many required packages is important to the package. 
  
CRAN Reviewer Comments:

* Please proof-read your description text.
  Currently it reads: "... Finn want built to meet the needs of..."
  Maybe it should be: "... Finn was built to meet the needs of..."
  
  This is now fixed in the description file.
  
* If there are references describing the methods in your package, please add these in the description     field of your DESCRIPTION file in the form
  authors (year) <doi:...>
  authors (year) <arXiv:...>
  authors (year, ISBN:...)
  or if those are not available: <https:...>
  with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
  (If you want to add a title as well please put it in quotes: "Title")
  
  Package was built on top on existing CRAN packages, shown in dependency list of description file.
  
* Please do not modify the .GlobalEnv. This is not allowed by the CRAN policies. e.g.:                    R/forecast_models.R

  This is required to run parallel tasks within Azure Batch, any does not run when using the package on   a users local machine, only in the cloud. The use of Azure Batch is now deprecated, meaning in the      next major release, v0.3.0, this code will be removed. 
  
* Please ensure that you do not use more than 2 cores in your examples, vignettes, etc.

  By default no parallel processing is ran in vignettes or tests. 

## Downstream dependencies
There are currently no downstream dependencies for this package
