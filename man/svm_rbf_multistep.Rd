% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multistep_svm_rbf.R
\name{svm_rbf_multistep}
\alias{svm_rbf_multistep}
\title{SVM-RBF Multistep Horizon}
\usage{
svm_rbf_multistep(
  mode = "regression",
  cost = NULL,
  rbf_sigma = NULL,
  margin = NULL,
  lag_periods = NULL,
  external_regressors = NULL,
  forecast_horizon = NULL,
  selected_features = NULL
)
}
\arguments{
\item{mode}{A single character string for the type of model.
The only possible value for this model is "regression".}

\item{cost}{A positive number for the cost of predicting
a sample within or on the wrong side of the margin.}

\item{rbf_sigma}{A positive number for radial basis function.}

\item{margin}{A positive number for the epsilon in the SVM
insensitive loss function.}

\item{lag_periods}{lag periods}

\item{external_regressors}{external regressors}

\item{forecast_horizon}{forecast horizon}

\item{selected_features}{selected features}
}
\value{
Get Multistep Horizon SVM-RBF model
}
\description{
SVM-RBF Multistep Horizon
}
\keyword{internal}
