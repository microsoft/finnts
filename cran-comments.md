
## Previous Submission Feedback

> Please do not start the description with "This package", package name, 
title or similar.

Updated the description file to reflect the feedback. 

> \dontrun{} should only be used if the example really cannot be executed 
(e.g. because of missing additional software, missing API keys, ...) by 
the user. That's why wrapping examples in \dontrun{} adds the comment 
("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the examples if they are executable in < 5 sec, or replace 
\dontrun{} with \donttest{}.

Changed the example in "forecast_time_series" function from a \dontrun{} wrapper to a \donttest{} one. 

> Please add small executable examples in your Rd-files to illustrate the 
use of the exported function but also enable automatic testing.

Added examples to all exported functions. Wrapped them inside \dontrun{} because they are functions that run machine learning models that take more than 5 seconds to run. 

> Please ensure that you do not use more than 2 cores in your examples, 
vignettes, etc.

Parallel processing is turned off in all examples and vignettes. So the code will execute on one core. 

> Please do not set a seed to a specific number within a function.

Created an argument in "forecast_time_series" function called "seed" to allow a user to set their own seed number. 

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:

* checking dependencies in R code ... NOTE
  Imports includes 29 non-default packages.
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
