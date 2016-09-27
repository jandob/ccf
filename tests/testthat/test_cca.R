context("Canonical Correlation Analysis")

library(pracma)
library(MASS)

test_that("cca 2d", {
  X = mvrnorm(1000, c(0, 0), eye(2))
  cca = canonical_correlation_analysis(X, X)
  expect_equal(cca$cor, eye(2))
})

test_that("cca 3d", {
  X = mvrnorm(1000, c(0, 0, 0), eye(3))
  cca = canonical_correlation_analysis(X, X)
  expect_equal(cca$cor, eye(3))
})

test_that("cca 10d", {
  X = mvrnorm(1000, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), eye(10))
  cca = canonical_correlation_analysis(X, X)
  expect_equal(cca$cor, eye(10))
})

test_that("projection to 0", {
  X = rbind(c(0, 0, 0),
            c(1, 1, 1),
            c(2, 2, 2),
            c(0, 0, 1),
            c(1, 1, 2),
            c(2, 2, 3))
  Y = rbind(matrix(rep(c(1, 0, 0, 0), each = 3), nrow = 3),
            matrix(rep(c(0, 0, 0, 1), each = 3), nrow = 3))
  cca = canonical_correlation_analysis(X, Y)
  expect_equal(cca$cor, eye(1))
})
