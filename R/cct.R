setupLeaf = function(y) {
  if (is.matrix(y)) {
    countsNode <- colSums(y)
    maxCounts <- max(countsNode)
    equalMaxCounts <- maxCounts == countsNode

    if (sum(equalMaxCounts) == 1) {
      classIndex <- which(equalMaxCounts) #TODO check this
    } else {
      TODO("TODO multiple max tie breaking")
    }
  } else {
    countsNode <- sum(y)
    classIndex <- 1
  }

  return(list(isLeaf = TRUE,
              classIndex = classIndex,
              trainingCounts = countsNode))
}

#' @importFrom utils head tail
find_best_split = function(X, Y, xVariationTolerance = 1e-10) {
  numberOfProjectionDirections = ncol(X)
  splitGains = matrix(NA, numberOfProjectionDirections, 1)
  splitIndices = matrix(NA, numberOfProjectionDirections, 1)
  # iterate over all dimensions in X
  for (i in seq(1, numberOfProjectionDirections)) {
    # sort by the value of the current dimension (feature) i
    sortOrder = order(X[, i])
    X_sorted = X[sortOrder, i] # vector of values from dimension i
    # matrix of labels (also sorted according to current dimension)
    Ysorted = Y[sortOrder, ]
    # So we have something like this (Note that Y is actually one-hot encoded
    # and therefore a matrix)
    # X_sorted: 64  65   68    69    70   71   72   72   75   75   80    81   83   85 #nolint
    # Y_sorted: red blue green green blue red  red  blue red  blue green red  red  blue #nolint

    # For every possible split_point (corresponds to value in X)
    # count the number of classes that occur in the partition
    # where X < split_point
    # e.g. for split_point = 72: (2 red, 2 blue, 1 green)
    LeftCumCounts = apply(Ysorted, 2, cumsum)
    total_counts = utils::tail(LeftCumCounts, 1)
    # Do the same for X > split_point.
    # We can just substract each row from total_counts
    RightCumCounts =
      sweep(LeftCumCounts, MARGIN = 2, total_counts, FUN = "-") * -1
    uniquePoints = c(diff(X_sorted) > xVariationTolerance, recursive = F)
    # proportion of classes to the left/right of split points
    pL = LeftCumCounts / rowSums(LeftCumCounts)
    pR = RightCumCounts / rowSums(RightCumCounts)

    pLProd = pL * log2(pL)
    pLProd[pL == 0] = 0
    metricLeft = -apply(pLProd, 1, sum)
    pRProd = pR * log2(pR)
    pRProd[pR == 0] = 0
    metricRight = -apply(pRProd, 1, sum)

    metricCurrent = utils::tail(metricLeft, 1)
    metricLeft[!uniquePoints] = Inf
    metricRight[!uniquePoints] = Inf
    N = nrow(X)
    metricGain = metricCurrent - (seq(1, N) * metricLeft +
                                  rev(seq(0, N - 1)) * metricRight
                                 ) / N

    # sample from equally best splits
    metricGainWOLast = utils::head(metricGain, -1)
    maxGain = max(metricGainWOLast)

    # equalMaxIndices correspond to indices of metricGain
    equalMaxIndices = which(abs(metricGainWOLast - maxGain) < 10 * eps)
    maxIndex = random_element(equalMaxIndices)

    splitGains[i] = metricGainWOLast[maxIndex]
    splitIndices[i] = maxIndex
  }
  maxGain = max(splitGains)

  # equalMaxIndices correspond to indices of splitGains
  equalMaxIndices = which(abs(splitGains - maxGain) < 10 * eps)
  splitDir = equalMaxIndices[1]
  splitIndex = splitIndices[splitDir]

  X = X[, splitDir]
  X_sorted = sort(X)
  X_sortedLeftPartition = X_sorted[splitIndex]
  X_sorted = X_sorted - X_sortedLeftPartition
  partitionPoint = X_sorted[splitIndex] * 0.5 + X_sorted[splitIndex + 1] * 0.5
  partitionPoint = partitionPoint + X_sortedLeftPartition
  lessThanPartPoint = X <= partitionPoint
  return(list(
    partitionPoint = partitionPoint,
    splitDir = splitDir,
    gain = maxGain,
    lessThanPartPoint = lessThanPartPoint
  ))
}

#' Computes a canonical correlation tree
#'
#' This function computes a single canonical correlation tree given its input values.
#' @param X Predictor matrix of size \eqn{n \times p} with \eqn{n} observations and \eqn{p}
#' variables.
#' @param Y Predicted values as a matrix of size \eqn{n \times p} with \eqn{n} observations
#' and \eqn{p} variables.
#' @param depth Depth of subtree.
#' @param minPointsForSplit Optional parameter setting the threshold when to construct a
#' leaf (default: 2). If the number of data points is smaller than this value, a leaf is
#' constructed.
#' @param maxDepthSplit Optional parameter controlling the construction of leaves after a
#' certain depth (default: \code{Inf}). If the current depth is greater than this value,
#' a leaf is constructed.
#' @param xVariationTolerance Features with variance less than this value are not considered
#' for splitting at tree nodes. (default \code{1e-10})
#' @param projectionBootstrap Use projection bootstrapping. (default \code{FALSE})
#' @param ancestralProbs Probabilities of ancestors. Default is \code{NULL} as these are
#' then calculated automatically.
#' @return Function returns an object of class \code{canonical_correlation_tree},
#' where the object is a list containing at the following components:
#' \itemize{
#'   \item{isLeaf}{Boolean whether the tree is a leaf itself.}
#'   \item{trainingCounts}{Number of training examples for constructing this tree (i.e.
#'   number of rows in input argument \code{X}).}
#'   \item{indicesFeatures}{Feature indices which the node received, as needed for
#'   prediction.}
#'   \item{decisionProjection}{Numeric matrix containing the projection matrix that was
#'      used to find the best split point.}
#'   \item{refLeftChild}{Reference to the left subtree.}
#'   \item{refRightChild}{Reference to the right subtree.}
#' }
#' @export
canonical_correlation_tree = function(
    X, Y,
    depth = 0,
    minPointsForSplit = 2,
    maxDepthSplit = Inf,
    xVariationTolerance = 1e-10,
    projectionBootstrap = FALSE,
    ancestralProbs = NULL) {
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }
  if (is.data.frame(Y)) {
    Y <- as.matrix(Y)
  }

  if (nrow(X) == 1
      || nrow(X) < minPointsForSplit
      || depth > maxDepthSplit) {
    # Return if one training point or max tree size options fulfilled
    return(setupLeaf(Y))
  } else if (is.matrix(Y) && ncol(Y) > 1) {
    # Return if pure node
    # TODO Zahl aus dem Hut?
    # Check if only one class is represented.
    if (sum(abs(colSums(Y)) > 1e-12) == 1) {
      return(setupLeaf(Y))
    }
  } else if (all(Y == 0)) {
    # only one column in Y and all zeros
    # ie binary classification
    return(setupLeaf(Y))
  }

  # TODO feature selection
  # TODO other stop conditions
  if (nrow(X) == 2) {
    if (all(Y[1, ] == Y[2, ])) {
      # same class so setupLeaf
      return(setupLeaf(Y))
    }
    # split in the centor of vector between the two points
    projection_matrix = t(X[2, , drop = F] - X[1, , drop = F]) #nolint
    partitionPoint = 0.5 * (X[2, ] %*% projection_matrix +
                           X[1, ] %*% projection_matrix)
    lessThanPartPoint = (X %*% projection_matrix) <= partitionPoint[1]
    best_split = list(partitionPoint = partitionPoint[1],
                      splitDir = 1,
                      lessThanPartPoint = lessThanPartPoint)
  } else {
    if (projectionBootstrap) {
      sampleIndices = sample(nrow(X), size = nrow(X), replace = T)
      XSampled = X[sampleIndices, , drop = F] #nolint
      YSampled = Y[sampleIndices, , drop = F] #nolint
      # Check if only one class is represented.
      if (sum( abs(colSums(YSampled)) > 1e-12 ) == 1) {
        return(setupLeaf(Y))
      }
      cca = canonical_correlation_analysis(XSampled, YSampled)
    } else {
      cca = canonical_correlation_analysis(X, Y)
    }

    projection_matrix = cca$xcoef

    # U are the feature vectors in the projected space

    U = X %*% projection_matrix

    bUVaries = apply(U, 2, stats::var) > xVariationTolerance
    if (!any(bUVaries)) {
      return(setupLeaf(Y))
    }

    best_split = find_best_split(
      U[, bUVaries, drop = F], Y, xVariationTolerance
    )
    if (best_split$gain < 0) {
      tree = setupLeaf(Y)
      return(tree)
    }
  }

  # each partition can have multiple classes
  countsNode = colSums(Y)
  nonZeroCounts = sum(countsNode > 0)
  uniqueNonZeroCounts = length(unique(countsNode)[unique(countsNode) != 0])
  if (uniqueNonZeroCounts == nonZeroCounts || is.null(ancestralProbs)) {
    ancestralProbs = countsNode / sum(countsNode)
  } else {
    ancestralProbs = rbind(ancestralProbs, countsNode / sum(countsNode))
  }
  treeLeft = canonical_correlation_tree(
    X[best_split$lessThanPartPoint, , drop = F], #nolint
    Y[best_split$lessThanPartPoint, , drop = F], #nolint
    depth = depth + 1,
    minPointsForSplit = minPointsForSplit,
    maxDepthSplit = maxDepthSplit,
    ancestralProbs = ancestralProbs
  )
  treeRight = canonical_correlation_tree(
    X[!best_split$lessThanPartPoint, , drop = F], #nolint
    Y[!best_split$lessThanPartPoint, , drop = F], #nolint
    depth = depth + 1,
    minPointsForSplit = minPointsForSplit,
    maxDepthSplit = maxDepthSplit,
    ancestralProbs = ancestralProbs
  )
  model = structure(
    list(isLeaf = F,
         trainingCounts = countsNode,
         #TODO features that the node got, needed for prediction;
         # for now all nodes get all features
         #indicesFeatures = indicesFeatures,
         decisionProjection =
           projection_matrix[, best_split$splitDir, drop = FALSE],
         partitionPoint = best_split$partitionPoint,
         depth = depth,
         refLeftChild = treeLeft,
         refRightChild = treeRight
    )
  , class = "canonical_correlation_tree")
  return(model)
}

#' @export
predict.canonical_correlation_tree = function(object, newData, probClass = NULL, ...){
  tree = object
  if (tree$isLeaf) {
    if(!is.null(probClass)){
      return(tree$trainingCounts[names(tree$trainingCounts) == probClass]/sum(tree$trainingCounts))
    }else{
      return(tree$classIndex)
    }
  }
  nr_of_features = length(tree$decisionProjection)
  # TODO use formula instead of all but last column
  X = as.matrix(
    newData[, 1:nr_of_features, drop = FALSE], ncol = nr_of_features)

  # TODO center_colmeans / input processing

  # transform training data corresponding to the node we are in
  U = X %*% tree$decisionProjection
  lessThanPartPoint = U <= tree$partitionPoint

  currentNodeClasses = matrix(nrow = max(nrow(X), 1))
  if (any(lessThanPartPoint)) {
    currentNodeClasses[lessThanPartPoint, ] =
      predict.canonical_correlation_tree(tree$refLeftChild,
                                       X[lessThanPartPoint, ,drop = FALSE], probClass = probClass) #nolint
  }
  if (any(!lessThanPartPoint)) {
    currentNodeClasses[!lessThanPartPoint, ] =
      predict.canonical_correlation_tree(tree$refRightChild,
                                       X[!lessThanPartPoint, ,drop = FALSE], probClass = probClass) #nolint
  }
  return(currentNodeClasses)
}

#' @export
plot.canonical_correlation_tree = function(x, dataX, dataY, ...) {
  TODO("check if plotable", return = T)
  plot_decision_surface(x, dataX, dataY)
}
