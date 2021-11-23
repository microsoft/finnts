## R CMD check results
There were no ERRORs or WARNINGs. 

There was 2 NOTES:

* checking dependencies in R code ... NOTE
  Imports includes 36 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

  This package does leverage many outside packages. The main feature of this package is 
  that it consolidates a lot of different models into one package to run them automatically. 
  So having many required packages is important to the package. 
  
* Suggests or Enhances not in mainstream repositories:
  doAzureParallel, rAzureBatch
  
  These packages are only available via GitHub and are integral to the package working at scale in a cloud environment. By default they are optional and listed under Suggests. 

* Also this is my first cran submission. 

## Downstream dependencies
There are currently no downstream dependencies for this package