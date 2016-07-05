#' @export
canonical_correlation_forest = function(x, ...) {
  UseMethod("canonical_correlation_forest")
}

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
canonical_correlation_forest.formula = function(formula, data = NULL, ...) {
  stopifnot(inherits(formula, "formula"), is.null(data))
  #m = match.call(expand.dots = FALSE)
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  # remove intercept, TODO what is it good for, regression? ...

  model_frame = model.frame(formula, data = data)
  Y = model.response(model_frame)
  X = model.matrix(model.frame)
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
