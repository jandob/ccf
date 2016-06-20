#' @export
canonical_correlation_forest = function(X, Y_one_hot, nr_of_trees = 200) {
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
#' @export
predict.canonical_correlation_forest = function(model, newdata) {
  library(plyr)
  nr_of_trees = length(model$forest)
  treePredictions = matrix(NaN, nrow(newdata), nr_of_trees)
  for (i in 1:nr_of_trees) {
    cat(sprintf("\rprediction %d of %d", i, nr_of_trees))
    treePredictions[,i] = predict(model$forest[[i]], newdata)
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
  ccf::plot.canonical_correlation_tree
}
