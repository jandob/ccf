% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccf.R
\name{predict.canonical_correlation_forest}
\alias{predict.canonical_correlation_forest}
\title{Prediction from canonical correlation forest}
\usage{
\method{predict}{canonical_correlation_forest}(object, newdata,
  verbose = FALSE, ...)
}
\arguments{
\item{object}{An object of class \code{canonical_correlation_forest}, as created
by the function \code{\link{canonical_correlation_forest}}.}

\item{newdata}{A data frame or a matrix containing the test data.}

\item{verbose}{Optional argument to control if additional information are
printed to the output. Default is \code{FALSE}.}

\item{...}{Additional parameters passed on to prediction from individual
canonical correlation trees.}
}
\description{
Performs predictions on test data for a trained canonical correlation forest.
}
