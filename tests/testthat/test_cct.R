context("Canonical Correlation Analysis")

library(pracma)


test_that("ccf", {
  diagonal <- as.data.frame(matrix(c(0, 0, 1, 1, 2, 2), ncol = 2, byrow = TRUE))
  colnames(diagonal) = c("x", "y")

  diagonal_upper <- diagonal_lower <- diagonal
  diagonal_upper$y <- diagonal$y + 1
  diagonal_lower$x <- diagonal$x + 1

  X <- rbind(diagonal, diagonal_upper, diagonal_lower)
  Y <- matrix(c(rep(1, nrow(diagonal)),
                rep(2, nrow(diagonal)),
                rep(3, nrow(diagonal))),
              ncol = 1)
  X <- as.matrix(X)

  cct <- canonical_correlation_tree(X, one_hot_encode(Y))

  plot(cct, X, Y)
})
