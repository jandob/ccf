#' An artificial spiral dataset
#'
#' A dataset containing 3 interwinding spirals. The variables of the
#' 10,000 datapoints are:
#'
#' \itemize{
#'   \item x numeric scalar x-coordinate
#'   \item y numeric scalar y-coordinate
#'   \item class integer either 1,2 or 3
#' }
#'
#' @docType data
#' @keywords datasets
#' @name spirals
#' @format A data frame with 10000 rows and 3 variables
#' @references \url{https://bitbucket.org/twgr/ccf/raw/49d5fce6fc006bc9a8949c7149fc9524535ce418/Datasets/spirals.csv}
NULL
#' Generic function for creating canonical correlation forests.
#' @param x Either a formula or a matrix.
#' @param ...	further arguments passed to or from other methods.
#' @export
canonical_correlation_forest = function(x, ...) {
  UseMethod("canonical_correlation_forest")
}
#' Defaultf S3 method.
#'
#' @param X numeric matrix (n * p) with n observations of p variables
#' @param Y_one_hot muneric matrix with n observations of q variables
#' (one_hot_encoded)
#' @param nr_of_trees nr of trees the forest will be composed of
#'
#' @return returns an object of class "canonical_correlation_forest",
#' where an object of class "canonical_correlation_forest" is a list
#' containing the following components:
#'    X,Y_one_hot The original input data
#'    forest a vector of length nr_of_trees with objects of class
#'    "canonical_correlation_tree"
#'
canonical_correlation_forest.default = function(X, Y_one_hot, nr_of_trees = 200) {
  forest = vector(mode = "list", length = nr_of_trees)
  for (i in 1:nr_of_trees) {
    cat(sprintf("\rtraining tree %d of %d", i, nr_of_trees))
    sampleIndices = sample(nrow(X),size = nrow(X), replace = T)
    XBag = X[sampleIndices,]
    YBag = Y_one_hot[sampleIndices,]
    forest[[i]] = canonical_correlation_tree(XBag,YBag)
    #YBag_decoded = Y[sampleIndices]
    #plotCCT(forest[[i]], XBag, YBag_decoded)
  }
  cat("\n")
  model = structure(list(X = X, Y_one_hot = Y_one_hot, forest = forest),
                    class = "canonical_correlation_forest")
  return(model)
}
#' @importFrom stats model.frame model.response model.matrix
canonical_correlation_forest.formula = function(formula, data = NULL, ...) {
  stopifnot(inherits(formula, "formula"), is.null(data))
  #m = match.call(expand.dots = FALSE)
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  # remove intercept, TODO what is it good for, regression? ...

  model_frame = stats::model.frame(formula, data = data)
  Y = stats::model.response(model_frame)
  X = stats::model.matrix(model.frame)
  Terms <- attr(model_frame, "terms")
  canonical_correlation_forest.default(X, Y, ...)
}
#' @export
predict.canonical_correlation_forest = function(object, newdata, ...) {
  #library(plyr)
  nr_of_trees = length(object$forest)
  treePredictions = matrix(NaN, nrow(newdata), nr_of_trees)
  for (i in 1:nr_of_trees) {
    cat(sprintf("\rprediction %d of %d", i, nr_of_trees))
    treePredictions[,i] = predict(object$forest[[i]], newdata)
  }
  cat("\n")
  cat("majority vote...")
  treePredictions = apply(treePredictions, 1,
                          function(row) {names(sort(table(row), decreasing = TRUE)[1])}
                    )
  cat("\rmajority vote done\n")
  return(treePredictions)
}
#' @export
plot.canonical_correlation_forest = function(...) {
  plot.canonical_correlation_tree(...)
}
