#library(ccf)
library(pracma)
context("Canonical Correlation Analysis")

test_that("ccf", {
  diagonal = as.data.frame(rbind(c(0,0),c(1,1),c(2,2)))
  colnames(diagonal) = c("x","y")
  diagonal_upper = diagonal_lower = diagonal
  diagonal_upper$y = diagonal$y + 1
  diagonal_lower$x = diagonal$x + 1
  X = rbind(diagonal,diagonal_upper, diagonal_lower)
  Y = rbind(ones(nrow(diagonal), 1),
            2 * ones(nrow(diagonal), 1),
            3 * ones(nrow(diagonal), 1))
  X = as.matrix(X)
  cct = canonical_correlation_tree(X,one_hot_encode(Y))
  plot(cct, X, Y)
})
