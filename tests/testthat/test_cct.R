context("Canonical Correlation Analysis")

library(pracma)

test_that("cct 2d", {
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
  cct = canonical_correlation_tree(X, one_hot_encode(Y))
  error_cct = get_missclassification_rate(cct, cbind(X, Y))
  expect_that(error_cct, equals(0))
})

test_that("cct 3d", {
  diagonal = as.data.frame(rbind(c(0, 0, 0), c(1, 1, 1), c(2, 2, 2)))
  colnames(diagonal) = c("x", "y", "z")
  diagonal_upper = diagonal_lower = diagonal_front = diagonal
  diagonal_upper$y = diagonal$y + 1
  diagonal_lower$x = diagonal$x + 1
  diagonal_front$z = diagonal$z + 1
  X = rbind(diagonal, diagonal_upper, diagonal_lower, diagonal_front)
  Y = rbind(1 * ones(nrow(diagonal), 1),
            2 * ones(nrow(diagonal), 1),
            3 * ones(nrow(diagonal), 1),
            4 * ones(nrow(diagonal), 1))
  X = as.matrix(X)
  cct = canonical_correlation_tree(X, one_hot_encode(Y))
  error_cct = get_missclassification_rate(cct, cbind(X, Y))
  expect_that(error_cct, equals(0))
})
test_that("cct 3d with projection bootstrap", {
  diagonal = as.data.frame(rbind(c(0, 0, 0), c(1, 1, 1), c(2, 2, 2)))
  colnames(diagonal) = c("x", "y", "z")
  diagonal_upper = diagonal_lower = diagonal_front = diagonal
  diagonal_upper$y = diagonal$y + 1
  diagonal_lower$x = diagonal$x + 1
  diagonal_front$z = diagonal$z + 1
  X = rbind(diagonal, diagonal_upper, diagonal_lower, diagonal_front)
  Y = rbind(1 * ones(nrow(diagonal), 1),
            2 * ones(nrow(diagonal), 1),
            3 * ones(nrow(diagonal), 1),
            4 * ones(nrow(diagonal), 1))
  X = as.matrix(X)
  set.seed(42)
  cct = canonical_correlation_tree(X, one_hot_encode(Y), projectionBootstrap = TRUE)
  error_cct = get_missclassification_rate(cct, cbind(X, Y))
  expect_that(error_cct, equals(0))
})
test_that("cct spiral with projection bootstrap", {
  data(spirals)
  d <- spirals
  colnames(d) <- c("x", "y", "z")
  d$z <- as.factor(d$z)

  d_train <- d[1:100,]
  d_test <- d[101:1000,]

  # convert to matrices
  X <- cbind(d_train$x,d_train$y)
  Y <- d_train$z

  set.seed(42)

  cct = canonical_correlation_tree(X, one_hot_encode(Y), projectionBootstrap = TRUE)
  error_cct <- get_missclassification_rate(cct, d_test)
  expect_true(error_cct < 0.38)
})
