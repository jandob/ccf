context("Canonical Correlation Analysis")
library(pracma)

test_that("ccf 2d", {
  diagonal = as.data.frame(rbind(c(0, 0), c(1, 1), c(2, 2)))
  colnames(diagonal) = c("x", "y")
  diagonal_upper = diagonal_lower = diagonal
  diagonal_upper$y = diagonal$y + 1
  diagonal_lower$x = diagonal$x + 1
  X = rbind(diagonal, diagonal_upper, diagonal_lower)
  Y = rbind(1 * ones(nrow(diagonal), 1),
            2 * ones(nrow(diagonal), 1),
            3 * ones(nrow(diagonal), 1))
  X = as.matrix(X)
  set.seed(42)
  ccf = canonical_correlation_forest(X, one_hot_encode(Y), ntree = 10)
  error_ccf = get_missclassification_rate(ccf, cbind(X, Y))
  expect_that(error_ccf, equals(0))
})
test_that("ccf spiral with projection bootstrap", {
  data(spirals)
  d <- spirals
  colnames(d) <- c("x", "y", "z")
  d$z <- as.factor(d$z)

  d_train <- d[1:100, ]
  d_test <- d[101:1000, ]

  # convert to matrices
  X <- cbind(d_train$x, d_train$y)
  Y <- d_train$z

  set.seed(42)

  ccf = canonical_correlation_forest(
    X, one_hot_encode(Y), projectionBootstrap = TRUE)
  error_ccf <- get_missclassification_rate(ccf, d_test)
  expect_true(error_ccf < 0.29)
})
