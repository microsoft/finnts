# define global variables to prevent notes in R CMD Check
utils::globalVariables(c(".id", ".key", ".model_desc", ".pred", ".resample_id", "All", "Best_Model", "Combo",
                  "Combo_Test_Date", "Combo_Total", "Count", "Date", "Date_Adj", "Date_Adj_half",
                  "Date_Adj_index.num", "Date_Adj_quarter", "Date_Adj_year", "Date_Day", "FCST",
                  "Horizon", "MAPE", "Model", "Number", "Number_Char", "Origin", "Residual",
                  "Residual_Std_Dev", "Rolling_MAPE", "Slice", "Sum", "Target", "Type", "Variable", 
                  "cluster", "frequency", "gluon_ts_frequency", "hi.80", "hi.95", "i", "lo.80", "lo.95",
                  "weighted_MAPE", "where", "as2"))

#' @importFrom magrittr %>%
NULL

#' @importFrom methods formalArgs
NULL

#' @importFrom stats sd
NULL

#' @importFrom foreach %do% %dopar%
NULL

#' @importFrom lubridate %m+%
NULL

#' @import modeltime
NULL

# * cbind.fill custom function ----
#create function to cbind dataframes that contain different amounts of rows
#https://github.com/cvarrichio/rowr/blob/master/R/rowr.R

vert<-function(object)
{
  #result<-as.data.frame(cbind(as.matrix(object)))
  if(is.list(object))
    object<-cbind(object)
  object<-data.frame(object)
  
  return(object)
}

len <- function(data)
{
  result<-ifelse(is.null(nrow(data)),length(data),nrow(data))
  return(result)
}

buffer<-function(x,length.out=len(x),fill=NULL,preserveClass=TRUE)
{
  xclass<-class(x)
  input<-lapply(vert(x),unlist)
  results<-as.data.frame(lapply(input,rep,length.out=length.out))
  if(length.out>len(x) && !is.null(fill))
  {
    results<-t(results)
    results[(length(unlist(x))+1):length(unlist(results))]<-fill
    results<-t(results)
  }
  if(preserveClass)
    results<-as2(results,xclass)
  return(results)   
}

cbind.fill <- function(...,fill=NA)
{
  inputs<-list(...)
  inputs<-lapply(inputs,vert)
  maxlength<-max(unlist(lapply(inputs,len)))
  bufferedInputs<-lapply(inputs,buffer,length.out=maxlength,fill,preserveClass=FALSE)
  return(Reduce(cbind.data.frame,bufferedInputs))
}