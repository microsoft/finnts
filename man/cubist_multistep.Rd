% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multistep_cubist.R
\name{cubist_multistep}
\alias{cubist_multistep}
\title{CUBIST Multistep Horizon}
\usage{
cubist_multistep(
  mode = "regression",
  committees = NULL,
  neighbors = NULL,
  max_rules = NULL,
  lag_periods = NULL,
  external_regressors = NULL,
  forecast_horizon = NULL,
  selected_features = NULL
)
}
\arguments{
\item{mode}{A single character string for the type of model.
The only possible value for this model is "regression".}

\item{committees}{committees}

\item{neighbors}{neighbors}

\item{max_rules}{max rules}

\item{external_regressors}{external regressors}

\item{forecast_horizon}{forecast horizon}

\item{selected_features}{selected features}
}
\value{
Get Multistep Horizon CUBIST model
}
\description{
CUBIST Multistep Horizon
}
\keyword{internal}
