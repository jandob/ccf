#' @source Idea: \url{http://gastonsanchez.com/how-to/2014/01/15/Center-data-in-R/}
center_colmeans <- function(x) {
  x_center <- colMeans(x)
  return(x - rep(x_center, rep.int(nrow(x), ncol(x))))
}

#' Canonical correlation analysis
#'
#' Canonical correlation analysis (CCA) finds pairs of vectors \eqn{(w,v)} such that projections
#' \eqn{Xw} and \eqn{Yv} have maximal possible correlations. The pairs are ordered in decreasing
#' order of the correlations. In addition, projection vectors are normalized such that the
#' variance of \eqn{Xw} and of \eqn{Yv} is equal to \eqn{1}. This means that projections are
#' not only correlated, but "on the same scale" and hence can be directly compared.
#' @param x Matrix of size n-by-p with n observations from p variables. Alternatively, data
#' frames and numeric vectors are supported and automatically converted.
#' @param y Matrix of size n-by-p with n observations from p variables. Alternatively, data
#' frames and numeric vectors are supported and automatically converted.
#' @param epsilon Numeric value usued as tolerance threshold for rank reduction of the
#' input matrices. Default is \code{1e-4}.
#' @return A list containing the following components
#' \itemize{
#'   \item{xcoef}{Estimated estimated coefficients for the \code{x} variable.}
#'   \item{ycoef}{Estimated estimated coefficients for the \code{y} variable.}
#'   \item{cor}{Matrix with correlation coefficients.}
#' }
#' @examples
#' library(MASS)
#' library(pracma)
#'
#' X <- mvrnorm(1000, mu = c(0, 0), Sigma = eye(2))
#' cca <- canonical_correlation_analysis(X, X)
#' cca
#'
#' X <- mvrnorm(1000, mu = c(1, 2),
#'              Sigma = matrix(c(1.5, 0.5, 0.5, 1.5), ncol = 2))
#' cca <- canonical_correlation_analysis(X, X)
#' cca
#' @export
canonical_correlation_analysis <- function(x, y, epsilon = 1e-4) {
  if (is.data.frame(x) || is.vector(x)) {
    x <- as.matrix(x)
  }
  if (is.data.frame(y) || is.vector(y)) {
    y <- as.matrix(y)
  }

  if (!is.matrix(x) || !is.matrix(y)) {
    stop("Arguments 'x' and 'y' must be of type matrix or data frame.")
  }

  # mean centering
  x <- center_colmeans(x)
  y <- center_colmeans(y)

  # QR decomposition (https://cran.r-project.org/doc/contrib/Hiebeler-matlabR.pdf)
  qr_decomp_x <- qr(x)
  q_x <- qr.Q(qr_decomp_x)
  r_x <- qr.R(qr_decomp_x)

  qr_decomp_y <- qr(y)
  q_y <- qr.Q(qr_decomp_y)
  r_y <- qr.R(qr_decomp_y)

  # reduce Q and R to full rank
  # TODO check if diag does the same as in matlab
  rank_x <- qr_decomp_x$rank
  rank_y <- qr_decomp_y$rank

  # TODO if rank == 0

  if (rank_x < ncol(x)) {
    q_x <- q_x[, 1:rank_x]
  }
  r_x <- r_x[1:rank_x, 1:rank_x]

  if (rank_y < ncol(y)) {
    q_y <- q_y[, 1:rank_y]
  }
  r_y <- r_y[1:rank_y,1:rank_y]

  number_of_coefficient_pairs = min(rank_x, rank_y)

  # select which decomposition is faster
  if (rank_x >= rank_y) {
    svd <- svd(t(q_x) %*% q_y)
    L <- svd$u
    D <- svd$d
    M <- svd$v
  } else {
    svd <- svd(t(q_y) %*% q_x)
    M <- svd$u
    D <- svd$d
    L <- svd$v
  }

  # Remove meaningless components in L and M
  # Note solve(x) == x^-1
  A <- solve(r_x, L[, 1:number_of_coefficient_pairs] * sqrt(nrow(x) - 1))
  B <- solve(r_y, M[, 1:number_of_coefficient_pairs] * sqrt(nrow(x) - 1))

  # restore full size
  A <- rbind(A, matrix(0, ncol(x) - rank_x, number_of_coefficient_pairs))
  B <- rbind(B, matrix(0, ncol(y) - rank_y, number_of_coefficient_pairs))

  correlations <- diag(D)

  # normalize (needed?)
  A <- apply(A, 1, function(x) {x / sqrt(colSums(A ^ 2))})
  B <- apply(B, 1, function(x) {x / sqrt(colSums(B ^ 2))})
  # TODO svd returns negative matrix in MATLAB

  return(list(xcoef = as.matrix(A),
              ycoef = as.matrix(B),
              cor = as.matrix(diag(D))))
}
