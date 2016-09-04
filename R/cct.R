setupLeaf = function(Y) {
  countsNode = colSums(Y)
  maxCounts = max(countsNode)
  equalMaxCounts = maxCounts == countsNode
  if (sum(equalMaxCounts) == 1) {
    classIndex = which(equalMaxCounts) #TODO check this
  } else {
    classIndex = which(equalMaxCounts)[1]
  }
  return(list(
    isLeaf = T,
    classIndex = classIndex,
    trainingCounts = countsNode
  ))
}

#' @importFrom utils head tail
find_best_split = function(X, Y, XVariationTolerance) {
  numberOfProjectionDirections = ncol(X)
  splitGains = matrix(NA, numberOfProjectionDirections, 1)
  splitIndices = matrix(NA, numberOfProjectionDirections, 1)
  # iterate over all dimensions in X
  for (i in seq(1, numberOfProjectionDirections)) {
    # sort by the value of the current dimension (feature) i
    sortOrder = order(X[, i])
    X_sorted = X[sortOrder, i] # vector of values from dimension i
    Ysorted = Y[sortOrder, ] # matrix of labels (also sorted according to current dimension)
    # So we have something like this (Note that Y is actually one-hot encoded and therefore a matrix)
    # X_sorted: 64  65   68    69    70   71   72   72   75   75   80    81   83   85
    # Y_sorted: red blue green green blue red  red  blue red  blue green red  red  blue

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
    uniquePoints = c(diff(X_sorted) > XVariationTolerance, recursive = F)
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
  #splitDir = random_element(equalMaxIndices)
  splitDir = equalMaxIndices[1]
  splitIndex = splitIndices[splitDir]

  X = X[, splitDir]
  X_sorted = sort(X)
  X_sortedLeftPartition = X_sorted[splitIndex]
  X_sorted = X_sorted - X_sortedLeftPartition
  partitionPoint = X_sorted[splitIndex] * 0.5 + X_sorted[splitIndex + 1] * 0.5
  partitionPoint = partitionPoint + X_sortedLeftPartition
  # X_sorted = X_sorted + X_sortedLeftPartition
  lessThanPartPoint = X <= partitionPoint
  return(list(
    partitionPoint = partitionPoint,
    splitDir = splitDir,
    gain = maxGain,
    lessThanPartPoint = lessThanPartPoint
  ))
}
#' Creates a canonical correlation tree.
#'
#' @param X numeric matrix (n * p) wiht n observations of p variables
#' @param Y numeric matrix (n * q) wiht n observations of q variables
#' @param depth The subtree depth
#' @param options list containing the following options for the tree
#' construction:
#' \describe{
#'    \item{minPointsForSplit}{If the number of datapoints is smaller than
#'        this value, a leaf is constructed.}
#'    \item{maxDepthSplit}{If the current depth is grater than
#'        this value, a leaf is constructed.}
#' }
#' @return returns an object of class "canonical_correlation_tree",
#' where the object is a list containing at the following components:
#'   \item{isLeaf}{boolean wether the tree is a leaf}
#'   \item{trainingCounts}{number of training examples this tree got (ncol(X))}
#'   \item{iIn}{feature indices that the node got, needed for prediction}
#'   \item{decisionProjection}{ numeric matrix The projection matrix that was
#'      used to find the best split point}
#'   \item{lessthanChild}{Reference to the left subtree}
#'   \item{greaterthanChild}{Reference to the right subtree}
#'
#' @export
#' @importFrom stats var
canonical_correlation_tree = function(X, Y, depth = 0, options = list(
                                          minPointsForSplit = 2,
                                          maxDepthsplit = Inf,
                                          XVariationTolerance = 1e-10,
                                          projectionBootstrap = F
                                     )) {
  if (nrow(X) == 1
      || nrow(X) < options$minPointsForSplit
      || depth > options$maxDepthsplit) {
    # Return if one training point or max tree size options fulfilled
    return(setupLeaf(Y))
  } else if (ncol(Y) > 1) {
    # Return if pure node
    # TODO Zahl aus dem Hut?
    # Check if only one class is represented.
    if (sum( abs(colSums(Y)) > 1e-12 ) == 1) {
      return(setupLeaf(Y))
    }
  } else if ( !any(Y) ) {
    # only one column in Y and all zeros
    # i.e. binary classification
    return(setupLeaf(Y))
  }
  # TODO feature subsampling

  # TODO other stop conditions
  if (nrow(X) == 2) {
    if (all(Y[1, ] == Y[2, ])) {
      # same class so setupLeaf
      return(setupLeaf(Y))
    }
    # split in the center of vector between the two points
    projection_matrix = t(X[2, , drop = F] - X[1, , drop = F])
    partitionPoint = 0.5 * (X[2, ] %*% projection_matrix +
                           X[1, ] %*% projection_matrix)
    lessThanPartPoint = (X %*% projection_matrix) <= partitionPoint[1]
    best_split = list(partitionPoint = partitionPoint[1],
                      splitDir = 1,
                      lessThanPartPoint = lessThanPartPoint)
  } else {
    if (options$projectionBootstrap) {
      sampleIndices = sample(nrow(X), size = nrow(X), replace = T)
      XSampled = X[sampleIndices, , drop = F]
      YSampled = Y[sampleIndices, , drop = F]
      # Check if only one class is represented.
      if (sum( abs(colSums(YSampled)) > 1e-12 ) == 1) {
        return(setupLeaf(Y))
      }
      cca = canonical_correlation_analysis(XSampled, YSampled)
    } else {
      cca = canonical_correlation_analysis(X, Y)
    }

    #B = cca$ycoef # not used
    #p = cca$cor   # not used
    projection_matrix = cca$xcoef

    # U are the feature vectors in the projected space
    U = X %*% projection_matrix
    # U = -1*U # debug for MATLAB correspondence

    bUVaries = apply(U, 2, stats::var) > options$XVariationTolerance
    if (!any(bUVaries)) {
      return(setupLeaf(Y))
    }

    best_split = find_best_split(
      U[, bUVaries, drop = F], Y, options$XVariationTolerance
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
  if (uniqueNonZeroCounts == nonZeroCounts || is.null(options$ancestralProbs)) {
    options$ancestralProbs = countsNode / sum(countsNode)
  } else {
    options$ancestralProbs =
      rbind(options$ancestralProbs, countsNode / sum(countsNode))
  }
  treeLeft = canonical_correlation_tree(
    X[best_split$lessThanPartPoint, , drop = F],
    Y[best_split$lessThanPartPoint, , drop = F],
    depth = depth + 1,
    options = options
  )
  treeRight = canonical_correlation_tree(
    X[!best_split$lessThanPartPoint, , drop = F],
    Y[!best_split$lessThanPartPoint, , drop = F],
    depth = depth + 1,
    options = options
  )

  model = structure(
    list(isLeaf = F,
         trainingCounts = countsNode,
         #iIn = iIn, #TODO features that the node got, needed for prediction; for now all nodes get all features
         decisionProjection = projection_matrix[, best_split$splitDir],
         partitionPoint = best_split$partitionPoint,
         lessthanChild = treeLeft,
         greaterthanChild = treeRight,
         depth = depth
    )
  , class = "canonical_correlation_tree")
  return(model)
}
#' Predictions from a Canonical Correlation Tree
#'
#' ## S3 method for class 'canonical_correlation_tree'
#'
#' @param object fitted model of class canonical_correlation_tree
#' @param newData numeric matrix (n * p) with n observations of p variables.
#'    Note: n and p have to match the dimensions of the training data.
#' @param ...	further arguments passed to or from other methods.
#' @export
predict.canonical_correlation_tree = function(object, newData, ...){
  tree = object
  nr_of_features = length(tree$decisionProjection)
  # use all but last column
  X = newData[, 1:nr_of_features, drop = F]
  X = as.matrix(X, ncol = nr_of_features)
  if (tree$isLeaf) {
    return(tree$classIndex)
  }
  # TODO center_colmeans / input processing

  # transform training data corresponding to the node we are in
  U = X %*% tree$decisionProjection
  lessThanPartPoint = U <= tree$partitionPoint

  currentNodeClasses = matrix(nrow = max(nrow(X), 1))
  if (any(lessThanPartPoint)) {
    currentNodeClasses[lessThanPartPoint, ] =
      predict.canonical_correlation_tree(tree$lessthanChild,
                                       X[lessThanPartPoint, , drop = F])
  }
  if (any(!lessThanPartPoint)) {
    currentNodeClasses[!lessThanPartPoint, ] =
      predict.canonical_correlation_tree(tree$greaterthanChild,
                                       X[!lessThanPartPoint, , drop = F])
  }
  return(currentNodeClasses)
}

#' Decision surface for a Canonical Correlation Tree
#'
#' Plots the decision surface for a CCT, training dataa has to be 2d.
#'
#' @param x fitted model of class canonical_correlation_tree
#' @param dataX numeric matrix (n * p) with n observations of p variables.
#'    Note: n and p have to match the dimensions of the training data.
#' @param dataY classes of the n observations
#' @param ...	further arguments passed to or from other methods.
#' @export
plot.canonical_correlation_tree = function(x, dataX, dataY, ...) {
  plot_decision_surface(x, dataX, dataY)
}
