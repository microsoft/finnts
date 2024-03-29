
# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  
  # CRAN OMP THREAD LIMIT
  Sys.setenv("OMP_THREAD_LIMIT" = 1)
  
  # This defines the model database
  
  make_xgboost_multistep()
  
  make_glmnet_multistep()
  
}