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

  # QR decomposition
  # (https://cran.r-project.org/doc/contrib/Hiebeler-matlabR.pdf)
  qrDecompX <- qr(x, tol = epsilon)
  qX <- qr.Q(qrDecompX)
  rX <- qr.R(qrDecompX)
  pX <- qrDecompX$pivot
  rankX <- qrDecompX$rank

  qrDecomp <- qr(y, tol = epsilon)
  qY <- qr.Q(qrDecomp)
  rY <- qr.R(qrDecomp)
  pY <- qrDecomp$pivot
  rankY <- qrDecomp$rank

  # reduce Q and R to full rank
  if (rankX == 0) {
    matrixA = matrix(0, ncol(x), 1)
    matrixA[1, ] = 1
    return(list(xcoef = matrixA,
                ycoef = NULL,
                cor = NULL)
    )
  }

  if (rankX < ncol(x)) {
    qX <- qX[, 1:rankX]
  }
  rX <- rX[1:rankX, 1:rankX]
  if (rankY < ncol(y)) {
    qY <- qY[, 1:rankY]
  }
  rY <- rY[1:rankY, 1:rankY]

  numberOfCoefficientPairs = min(rankX, rankY)

  # select which decomposition is faster
  if (rankX >= rankY) {
    svd <- svd(t(qX) %*% qY)
    L <- svd$u
    D <- svd$d
    M <- svd$v
  } else {
    svd <- svd(t(qY) %*% qX)
    M <- svd$u
    D <- svd$d
    L <- svd$v
  }

  # Remove meaningless components in L and M
  # Note solve(x) == x^-1
  A <- solve(rX) %*% L[, 1:numberOfCoefficientPairs] * sqrt(nrow(x) - 1)
  B <- solve(rY) %*% M[, 1:numberOfCoefficientPairs] * sqrt(nrow(x) - 1)

  # restore full size
  A <- rbind(A, matrix(0, ncol(x) - rankX, numberOfCoefficientPairs))
  B <- rbind(B, matrix(0, ncol(y) - rankY, numberOfCoefficientPairs))

  correlations <- diag(D)

  # restore order
  A = A[pX, , drop = FALSE] #nolint
  B = B[pY, , drop = FALSE] #nolint

  # normalize
  A = scale(A, center = FALSE, scale = sqrt(colSums(A ^ 2)))

  # convert to matrices and restore dimension names
  matrixA = as.matrix(A)
  matrixB = as.matrix(B)
  rownames(matrixA) = colnames(x)
  rownames(matrixB) = colnames(y)

  return(list(xcoef = matrixA,
              ycoef = matrixB,
              cor = as.matrix(correlations)) #TODO add dimnames
  )

}
