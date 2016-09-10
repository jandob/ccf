#' Canonical correlation forest
#'
#' This function computes a classifier based on a canonical correlation forest. It
#' expects its input in matrix form or as formula notation.
#' @param x Numeric matrix (n * p) with n observations of p variables
#' @param y Numeric matrix with n observations of q variables
#' @param ntree Number of trees the forest will be composed of
#' @param verbose Optional argument to control if additional information are
#' printed to the output. Default is \code{FALSE}.
#' @param ...	Further arguments passed to or from other methods.
#' @return returns an object of class "canonical_correlation_forest",
#' where an object of this class is a list containing the following
#' components:
#' \itemize{
#'   \item{x,y}{The original input data}
#'   \item{y_encoded}{The encoded \code{y} variable in case of classification tasks.}
#'   \item{forest}{a vector of length ntree with objects of class
#'    \code{canonical_correlation_tree}.}
#' }
#' @examples
#' data(spirals)
#'
#' d_train <- spirals[1:1000, ]
#' d_test <- spirals[-(1:1000), ]
#'
#' # compute classifier on training data
#' ## variant 1: matrix input
#' m1 <- canonical_correlation_forest(d_train[, c("x", "y")], d_train$class, ntree = 200)
#' ## variant 2: formula notation
#' m2 <- canonical_correlation_forest(class ~ ., d_train)
#'
#' # compute predictive accuracy
#' get_missclassification_rate(m1, d_test)
#' get_missclassification_rate(m1, d_test)
#' @references Rainforth, T., and Wood, F. (2015): Canonical correlation forest,
#' arXiv preprint, arXiv:1507.05444, \url{https://arxiv.org/pdf/1507.05444.pdf}.
#' @rdname ccf
#' @export
canonical_correlation_forest = function(x, y = NULL,
                                        ntree = 200, verbose = FALSE, ...) {
  UseMethod("canonical_correlation_forest", x)
}


#' @rdname ccf
#' @export
canonical_correlation_forest.default = function(x, y = NULL,
                                                ntree = 200, verbose = FALSE, ...) {
  forest = vector(mode = "list", length = ntree)

  if (is.null(y)) {
    stop("CCF requires y variable.")
  }

  # TODO: one_hot_encoding
  if (is.factor(y)) {
    y_encoded <- one_hot_encode(y)
    y_use <- y_encoded
  } else if (is.integer(y)) {
    y_encoded <- one_hot_encode(y)
    y_use <- y_encoded
  } else {
    y_encoded <- NULL
    y_use <- y
  }


  for (i in 1:ntree) {
    if (verbose) {
      cat("Training tree", i, "of", ntree)
    }

    sample_idx = sample(nrow(x), size = nrow(x), replace = TRUE)

    x_bag = x[sample_idx, , drop = FALSE]

    if (is.vector(y)) {
      y_bag <- y_use[sample_idx, drop = FALSE]
    } else {
      y_bag <- y_use[sample_idx, , drop = FALSE]
    }

    forest[[i]] = canonical_correlation_tree(x_bag, y_bag)

    #TODO: remove commented code
    #YBag_decoded = Y[sample_idx]
    #plotCCT(forest[[i]], XBag, YBag_decoded)
  }

  model <- structure(list(x = x, y = y, y_encoded = y_encoded,
                          ntree = ntree, forest = forest),
                     class = "canonical_correlation_forest")
  return(model)
}

#' @importFrom stats model.frame model.response model.matrix
#' @rdname ccf
#' @export
canonical_correlation_forest.formula = function(x, y = NULL, ntree = 200, verbose = FALSE, ...) {
  formula <- x
  data <- y

  # TODO: by default, take the data from formula object
  if (is.null(data)) {
    stop("CCF based on formula requires a data attribute.")
  }

  #TODO: what are the comments good for?

  #m = match.call(expand.dots = FALSE)
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  # remove intercept, TODO what is it good for, regression? ...

  model_frame <- model.frame(formula, data = data)

  x = model.matrix(model.frame)
  y = model.response(model_frame)

  canonical_correlation_forest.default(x, y, ...)
}

#' Prediction from canonical correlation forest
#'
#' Performs predictions on test data for a trained canonical correlation forest.
#' @param object An object of class \code{canonical_correlation_forest}, as created
#' by the function \code{\link{canonical_correlation_forest}}.
#' @param newdata A data frame or a matrix containing the test data.
#' @param verbose Optional argument to control if additional information are
#' printed to the output. Default is \code{FALSE}.
#' @param ... Additional parameters passed on to prediction from individual
#' canonical correlation trees.
#' @export
predict.canonical_correlation_forest = function(object, newdata, verbose = FALSE, ...) {
  if (missing(newdata)) {
    stop("Argument 'newdata' is missing.")
  }

  ntree <- length(object$forest)
  treePredictions <- matrix(NA, nrow = nrow(newdata), ncol = ntree)

  # TODO: make more efficient with apply or dplyr
  for (i in 1:ntree) {
    if (verbose) {
      cat("Prediction", i, "of", ntree, "\n")
    }

    treePredictions[, i] <- predict(object$forest[[i]], newdata, ...)
  }

  if (verbose) {
    cat("\nMajority vote")
  }

  treePredictions <- apply(treePredictions, 1, function(row) { names(max(table(row))) })

  return(treePredictions)
}

#' Visualization of canonical correlation forest
#'
#' TODO: document
#' @param ...	Further arguments passed to or from other methods.
#' @export
plot.canonical_correlation_forest = function(...) {
  plot.canonical_correlation_tree(...)
}
