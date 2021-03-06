% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccf.R
\name{canonical_correlation_forest}
\alias{canonical_correlation_forest}
\alias{canonical_correlation_forest.default}
\alias{canonical_correlation_forest.formula}
\title{Canonical correlation forest}
\usage{
canonical_correlation_forest(x, y = NULL, ntree = 200, verbose = FALSE,
  ...)

\method{canonical_correlation_forest}{default}(x, y = NULL, ntree = 200,
  verbose = FALSE, projectionBootstrap = FALSE, ...)

\method{canonical_correlation_forest}{formula}(x, y = NULL, ntree = 200,
  verbose = FALSE, ...)
}
\arguments{
\item{x}{Numeric matrix (n * p) with n observations of p variables}

\item{y}{Numeric matrix with n observations of q variables}

\item{ntree}{Number of trees the forest will be composed of}

\item{verbose}{Optional argument to control if additional information are
printed to the output. Default is \code{FALSE}.}

\item{...}{Further arguments passed to or from other methods.}

\item{projectionBootstrap}{Use projection bootstrapping. (default \code{FALSE})}
}
\value{
returns an object of class "canonical_correlation_forest",
where an object of this class is a list containing the following
components:
\itemize{
  \item{x,y}{The original input data}
  \item{y_encoded}{The encoded \code{y} variable in case of classification tasks.}
  \item{forest}{a vector of length ntree with objects of class
   \code{canonical_correlation_tree}.}
}
}
\description{
This function computes a classifier based on a canonical correlation forest. It
expects its input in matrix form or as formula notation.
}
\examples{
data(spirals)

d_train <- spirals[1:1000, ]
d_test <- spirals[-(1:1000), ]

# compute classifier on training data
## variant 1: matrix input
m1 <- canonical_correlation_forest(d_train[, c("x", "y")], d_train$class, ntree = 20)
## variant 2: formula notation
m2 <- canonical_correlation_forest(class ~ ., d_train)

# compute predictive accuracy
get_missclassification_rate(m1, d_test)
get_missclassification_rate(m2, d_test)
}
\references{
Rainforth, T., and Wood, F. (2015): Canonical correlation forest,
arXiv preprint, arXiv:1507.05444, \url{https://arxiv.org/pdf/1507.05444.pdf}.
}
