library(ccf)
library(pracma)
library(MASS)
context("Canonical Correlation Analysis")

test_that("cca", {
  X = mvrnorm(1000, c(0,0), eye(2))
  cca = canonical_correlation_analysis(X, X)
  expect_equal(cca$cor, eye(2))
})

