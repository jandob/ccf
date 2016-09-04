
k_fold_cross_validation = function(data, k = 10) {
  library(caret)
  folds = caret::createFolds(data$X[,1], k = k, list = T, returnTrain = F)
  errors = list()
  for (foldIndex in seq(folds)) {
    foldTest = unlist(folds[foldIndex])
    XTest = data$X[foldTest, , drop = F]
    YTest = data$Y[foldTest, , drop = F]

    foldTrainIndices = seq(folds)[-foldIndex]
    foldTrain = do.call(c, folds[foldTrainIndices])
    XTrain = data$X[foldTrain, , drop = F]
    YTrain = data$Y[foldTrain, , drop = F]

    model = canonical_correlation_forest(XTrain,one_hot_encode(YTrain))
    error = get_missclassification_rate(model, cbind(XTest, YTest))
    print(paste("fold", foldIndex,": error:", error))
    errors[foldIndex] = error
  }
  errors = unlist(errors)
  print(paste("mean:", mean(errors), "standard deviation:", sd(errors)))
}

# compatible csv datasets are available at
# https://bitbucket.org/twgr/ccf/src/49d5fce6fc006bc9a8949c7149fc9524535ce418/Datasets/?at=master
files = list.files(file.path(getwd(), "data"), pattern = "\\.csv$")
for (file_name in files) {
  if (file_name == "skinSeg.csv") {
    # currently not working (recursion to deep)
    next
  }
  if (file_name %in% list("letter.csv", "")) {
    # takes long
    next
  }
  file_path = file.path(getwd(), "data", file_name)
  print(paste("dataset: ", file_path))
  data = load_csv_data_set(file_path)
  k_fold_cross_validation(data, k = 10)
}





#cross val
