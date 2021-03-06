% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GWLelast.est.R
\name{GWLelast.est}
\alias{GWLelast.est}
\title{GWLelast.est}
\usage{
GWLelast.est(x, y, coords, D = NULL, alpha = 1, lambda = NULL,
  nlambda = NULL, gweight = c("gwr.Gauss", "gwr.bisquare"),
  longlat = TRUE, bw = bw)
}
\arguments{
\item{x}{Covariates.}

\item{y}{Outcome binary variable.}

\item{coords}{2 columns matrix including "longitude" and "latitude".}

\item{D}{Distance matrix.}

\item{alpha}{The elasticnet mixing parameter [0,1] in glmnet package.}

\item{lambda}{Optional user-supplied lambda sequence in glmnet package.}

\item{nlambda}{The number of lambda values in glmnet package.}

\item{gweight}{geographical kernel function in spgwr package.}

\item{longlat}{Indicate if the coords parameter are sperically calculated.}

\item{bw}{bandwidth of geographical kernel function.}
}
\value{
model: Fitted model at location i.

error: Cross validation error.
}
\description{
Fitting geographically weighted logistic elastic net regression
}
