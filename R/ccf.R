#' Canonical Correlation Forest
#'
#' Generic function for creating canonical correlation forests.
#' @param x Either a formula or a matrix.
#' @param ...	further arguments passed to or from other methods.
#' @rdname ccf
#' @export
canonical_correlation_forest = function(x, ...) {
  UseMethod("canonical_correlation_forest")
}

#' Canonical Correlation Forest
#'
#' Default S3 method for canonical_correlation_forest.
#'
#' @param x numeric matrix (n * p) with n observations of p variables
#' @param Y_one_hot muneric matrix with n observations of q variables
#' (one_hot_encoded)
#' @param nr_of_trees nr of trees the forest will be composed of
#' @param ...	further arguments passed to or from other methods.
#'
#' @return returns an object of class "canonical_correlation_forest",
#' where an object of this class is a list containing the following
#' components:
#'   \item{X,Y_one_hot}{The original input data}
#'   \item{forest}{a vector of length nr_of_trees with objects of class
#'    "canonical_correlation_tree"}
#' @rdname ccf
#' @export
canonical_correlation_forest.default = function(x, Y_one_hot, nr_of_trees = 200, ...) {
  forest = vector(mode = "list", length = nr_of_trees)
  for (i in 1:nr_of_trees) {
    cat(sprintf("\rtraining tree %d of %d", i, nr_of_trees))
    sampleIndices = sample(nrow(x),size = nrow(x), replace = T)
    XBag = x[sampleIndices,]
    YBag = Y_one_hot[sampleIndices,]
    forest[[i]] = canonical_correlation_tree(XBag,YBag)
    #YBag_decoded = Y[sampleIndices]
    #plotCCT(forest[[i]], XBag, YBag_decoded)
  }
  cat("\n")
  model = structure(list(X = x, Y_one_hot = Y_one_hot, forest = forest),
                    class = "canonical_correlation_forest")
  return(model)
}

#' @importFrom stats model.frame model.response model.matrix
#' @export
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

# TODO: documentation
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
