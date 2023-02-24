## R CMD check results
There were no ERRORs or WARNINGs. 

There was 2 NOTEs:

* checking dependencies in R code ... NOTE
  Imports includes 35 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

  This package does leverage many outside packages. The main feature of this package is 
  that it consolidates a lot of different models into one package to run them automatically. 
  So having many required packages is important to the package. 
  
* checking R code for possible problems ... NOTE
   Found the following assignments to the global environment:
   File 'finnts/R/prep_data.R':
     assign(name, context[[name]], envir = .GlobalEnv)
  
  This assignment is ran when submitting tasks to run in spark by using sparklyr::spark_apply(). 
  You have to specify what variables get exported to the cluster then load them back into the 
  environment within the cluster using assign(). 

## Downstream dependencies
There are currently no downstream dependencies for this package
