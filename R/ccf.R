#' Canonical Correlation Forest
#'
#' @param x numeric matrix (n * p) with n observations of p variables
#' @param Y_one_hot muneric matrix with n observations of q variables
#' (one_hot_encoded)
#' @param nr_of_trees nr of trees the forest will be composed of
#' @param options list of options
#' @param ...	further arguments passed to or from other methods.
#'
#' @return returns an object of class "canonical_correlation_forest",
#' where an object of this class is a list containing the following
#' components:
#'   \item{X,Y_one_hot}{The original input data}
#'   \item{forest}{a vector of length nr_of_trees with objects of class
#'    "canonical_correlation_tree"}
#' @export
canonical_correlation_forest = function(
    x, Y_one_hot, nr_of_trees = 200, options=list(verbose = F), ...) {
  forest = vector(mode = "list", length = nr_of_trees)
  for (i in 1:nr_of_trees) {
    if (options$verbose) {
      cat(sprintf("\rtraining tree %d of %d", i, nr_of_trees))
    }
    sampleIndices = sample(nrow(x), size = nrow(x), replace = T)
    XBag = x[sampleIndices, , drop = F]
    YBag = Y_one_hot[sampleIndices, , drop = F]
    forest[[i]] = canonical_correlation_tree(XBag, YBag)
    #YBag_decoded = Y[sampleIndices]
    #plotCCT(forest[[i]], XBag, YBag_decoded)
  }
  cat("\n")
  model = structure(list(X = x, Y_one_hot = Y_one_hot, forest = forest),
                    class = "canonical_correlation_forest")
  return(model)
}

#' Predictions from a Canonical Correlation Forest
#'
#' ## S3 method for class 'canonical_correlation_forest'
#'
#' @param object fitted model of class canonical_correlation_forest
#' @param newData numeric matrix (n * p) with n observations of p variables.
#'    Note: n and p have to match the dimensions of the training data.
#' @param options list of options
#' @param ...	further arguments passed to or from other methods.
#' @export
predict.canonical_correlation_forest = function(
    object, newData, options=list(verbose = F), ...) {
  nr_of_trees = length(object$forest)
  treePredictions = matrix(NaN, nrow(newData), nr_of_trees)
  for (i in 1:nr_of_trees) {
    if (options$verbose) {
      cat(sprintf("\rprediction %d of %d", i, nr_of_trees))
    }
    treePredictions[, i] = predict(object$forest[[i]], newData)
  }
  if (options$verbose) {
    cat("\n")
    cat("majority vote...")
  }
  treePredictions = apply(treePredictions, 1,
    function(row) {
      names(sort(table(row), decreasing = TRUE)[1])
    }
  )
  if (options$verbose) {
    cat("\rmajority vote done\n")
  }
  return(treePredictions)
}
#' @export
plot.canonical_correlation_forest = function(...) {
  plot.canonical_correlation_tree(...)
}
