#' Canonical correlation forest
#'
#' This function computes a classifier based on a canonical correlation forest. It
#' expects its input in matrix form or as formula notation.
#' @param x numeric matrix (n * p) with n observations of p variables
#' @param Y_one_hot muneric matrix with n observations of q variables
#' (one_hot_encoded)
#' @param ntree nr of trees the forest will be composed of
#' @param ...	further arguments passed to or from other methods.
#'
#' @return returns an object of class "canonical_correlation_forest",
#' where an object of this class is a list containing the following
#' components:
#'   \item{X,Y_one_hot}{The original input data}
#'   \item{forest}{a vector of length ntree with objects of class
#'    "canonical_correlation_tree"}
#' @references Rainforth, T., and Wood, F. (2015): Canonical correlation forest,
#' arXiv preprint, arXiv:1507.05444, \url{https://arxiv.org/pdf/1507.05444.pdf}.
#' @rdname ccf
#' @export
canonical_correlation_forest = function(x, ...) {
  UseMethod("canonical_correlation_forest", x)
}


#' @rdname ccf
#' @export
canonical_correlation_forest.default = function(x, Y_one_hot, ntree = 200, ...) {
  forest = vector(mode = "list", length = ntree)
  for (i in 1:ntree) {
    cat(sprintf("\rtraining tree %d of %d", i, ntree))
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

#' Prediction from canonical correlation forest
#'
#' Performs predictions on test data for a trained canonical correlation forest.
#' @param object An object of class \code{canonical_correlation_forest}, as created
#' by the function \code{\link{canonical_correlation_forest}}.
#' @param newdata A data frame or a matrix containing the test data.
#' @param verbose Optional argument to control if additional information are
#' printed to the output. Default is \code{FALSE}.
#' @export
predict.canonical_correlation_forest = function(object, newdata, ...) {
  if (missing(newdata)) {
    stop("Argument 'newdata' is missing.")
  }

  ntree <- length(object$forest)
  treePredictions <- matrix(NA, nrow = nrow(newdata), ncol = ntree)

  for (i in 1:ntree) {
    if (!missing(verbose) && verbose == TRUE) {
      cat("Prediction", i, "of", ntree, "\n")
    }

    treePredictions[, i] <- predict(object$forest[[i]], newdata)
  }

  if (!missing(verbose) && verbose == TRUE) {
    cat("\nMajority vote")
  }

  treePredictions <- apply(treePredictions, 1, function(row) { names(max(table(row))) })

  return(treePredictions)
}

#' @export
plot.canonical_correlation_forest = function(...) {
  plot.canonical_correlation_tree(...)
}
