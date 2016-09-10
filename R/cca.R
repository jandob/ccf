#' Canonical correlation analysis
#'
#' Canonical correlation analysis (CCA) finds pairs of vectors \eqn{(w,v)} such that projections
#' \eqn{Xw} and \eqn{Yv} have maximal possible correlations. The pairs are ordered in decreasing
#' order of the correlations. In addition, projection vectors are normalized such that the
#' variance of \eqn{Xw} and of \eqn{Yv} is equal to \eqn{1}. This means that projections are
#' not only correlated, but "on the same scale" and hence can be directly compared.
#' @param x Matrix of size n-by-p with n observations from p variables.
#' @param y Matrix of size n-by-p with n observations from p variables.
#' @param epsilon Numeric value usued as tolerance threshold for rank reduction of the
#' input matrices. Default is \code{1e-4}.
#' @return A list containing the following components
#'         \item{xcoef}{Estimated estimated coefficients for the \code{x} variable.}
#'         \item{ycoef}{Estimated estimated coefficients for the \code{y} variable.}
#'         \item{cor}{Matrix with correlation coefficients.}
#' @examples
#' library(MASS)
#' library(pracma)
#' X <- mvrnorm(1000, mu = c(0, 0), Sigma = eye(2))
#' cca <- canonical_correlation_analysis(X, X)
#' cca
#'
#' X <- mvrnorm(1000, mu = c(1, 2),
#'              Sigma = matrix(c(1.5, 0.5, 0.5, 1.5), ncol = 2))
#' cca <- canonical_correlation_analysis(X, X)
#' cca
#' @export
canonical_correlation_analysis = function(X, Y, epsilon = 1e-4) {
  if (!is.matrix(X) || !is.matrix(Y)) {
    stop("Arguments 'X' and 'Y' must be of type matrix.")
  }

  # alternative
  #cca = cancor(X, as.matrix(Y))

  #X = as.matrix(X)
  #Y = as.matrix(Y)
  # colMeans() from http://gastonsanchez.com/how-to/2014/01/15/Center-data-in-R/
  center_colmeans <- function(x) {
    x_center = colMeans(x)
    x - rep(x_center, rep.int(nrow(x), ncol(x)))
  }
  # mean centering
  X = center_colmeans(X)
  Y = center_colmeans(Y)

  # QR decomposition (https://cran.r-project.org/doc/contrib/Hiebeler-matlabR.pdf)
  qr_decomp = qr(X)
  q_X = qr.Q(qr_decomp)
  r_X = qr.R(qr_decomp)
  #p_X = qr_decomp$pivot
  qr_decomp = qr(Y)
  q_Y = qr.Q(qr_decomp)
  r_Y = qr.R(qr_decomp)
  #p_Y = qr_decomp$pivot

  # reduce Q and R to full rank
  # TODO check if diag does the same as in matlab
  rank_X = sum(abs(diag(r_X)) >= (epsilon * abs(r_X[1, 1])))
  rank_Y = sum(abs(diag(r_Y)) >= (epsilon * abs(r_Y[1, 1])))
  # TODO if rank == 0

  if (rank_X < ncol(X)) {
    q_X = q_X[, 1:rank_X]
  }
  r_X = r_X[1:rank_X,1:rank_X]
  if (rank_Y < ncol(Y)) {
    q_Y = q_Y[, 1:rank_Y]
  }
  r_Y = r_Y[1:rank_Y,1:rank_Y]

  number_of_coefficient_pairs = min(rank_X, rank_Y)

  # select which decomposition is faster
  if (rank_X >= rank_Y) {
    svd = svd(t(q_X) %*% q_Y)
    L = svd$u
    D = svd$d
    M = svd$v
  } else {
    svd = svd(t(q_Y) %*% q_X)
    M = svd$u
    D = svd$d
    L = svd$v
  }
  # remove meaningless components
  #L = L[,1:number_of_coefficient_pairs]
  #M = M[,1:number_of_coefficient_pairs]

  # note solve(X) == X^-1
  # A = mldivide(r_X, L[,1:number_of_coefficient_pairs] * sqrt(nrow(X)-1)) #version from MATLAB
  A = solve(r_X) %*% L[, 1:number_of_coefficient_pairs] * sqrt(nrow(X) - 1)
  B = solve(r_Y) %*% M[, 1:number_of_coefficient_pairs] * sqrt(nrow(X) - 1)

  correlations = diag(D)

  # restore full size
  A = rbind(A, matrix(0,ncol(X) - rank_X, number_of_coefficient_pairs))
  B = rbind(B, matrix(0,ncol(Y) - rank_Y, number_of_coefficient_pairs))

  # normalize (needed?)
  A = apply(A, 1, function(x) {x / sqrt(colSums(A ^ 2))}) #MATLAB: bsxfun(@rdivide,projMat,sqrt(sum(projMat.^2,1)));
  # TODO svd returns negative matrix in MATLAB

  l = list(xcoef = as.matrix(A), ycoef = as.matrix(B), cor = as.matrix(correlations))
  return(l)
  #TODO
}
