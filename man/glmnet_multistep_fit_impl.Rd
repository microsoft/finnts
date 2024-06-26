% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multistep_glmnet.R
\name{glmnet_multistep_fit_impl}
\alias{glmnet_multistep_fit_impl}
\title{Bridge GLMNET Multistep Modeling function}
\usage{
glmnet_multistep_fit_impl(
  x,
  y,
  alpha = 0,
  lambda = 1,
  lag_periods = NULL,
  external_regressors = NULL,
  forecast_horizon = NULL,
  selected_features = NULL
)
}
\arguments{
\item{x}{A dataframe of xreg (exogenous regressors)}

\item{y}{A numeric vector of values to fit}

\item{alpha}{alpha}

\item{lambda}{lambda}

\item{lag_periods}{lag periods}

\item{external_regressors}{external regressors}

\item{forecast_horizon}{forecast horizon}

\item{selected_features}{selected features}
}
\description{
Bridge GLMNET Multistep Modeling function
}
\keyword{internal}
