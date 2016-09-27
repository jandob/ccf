random_element <- function(x) {
  if (length(x) > 1) {
    return(sample(x, 1))
  }
  return(x)
}

eps <- 1 - 3 * ((4 / 3) - 1)

#' @importFrom stats model.matrix
one_hot_encode <- function(data) {
  if (!is.data.frame(data)) {
    data <- data.frame(data)
    colnames(data) <- "class"
  }

  data$class <- as.factor(data$class)

  # This trick only works with factors
  return(model.matrix(~ class - 1, data = data))
}

one_hot_decode <- function(X_one_hot) {
  return(apply(X_one_hot, 1, function(row){which(row == 1)}))
}

TODO <- function(message = "TODO", return = NULL) {
  print(paste("TODO", message))

  if (is.null(return)) {
    stop()
  }

  return(return)
}
generate_2d_data_plot <- function(data = NULL,
                                  data_raster = NULL,
                                  interpolate = F,
                                  title = "") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for plotting to work. Please install it.",
         call. = FALSE)
  }
  blank <- ggplot2::element_blank()
  g <- ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::coord_fixed(ratio = 0.8) +
    ggplot2::theme(axis.ticks = blank,
                   panel.grid.major = blank,
                   panel.grid.minor = blank,
                   axis.text = blank,
                   axis.title = blank,
                   legend.position = "none") +
    ggplot2::geom_raster(
      ggplot2::aes(x = data_raster$x,
                   y = data_raster$y,
                   fill = as.character(data_raster$z)),
      interpolate = interpolate,
      alpha = 0.5,
      show.legend = F) +
    ggplot2::geom_point(
      ggplot2::aes(x = data$x,
                   y = data$y,
                   color = as.character(data$z)),
      size = 2) +
    ggplot2::ggtitle(title)
    #ggplot2::xlim(x_min, x_max) +
    #ggplot2::ylim(y_min, y_max)

  return(g)
}
#' Helper function to plot classifier decision surface.
#'
#' This function generates a plot (ggplot2) of the decision surface for a 2d classifier.
#' @param model a model object for which prediction is desired. E.g. object of class
#' \code{canonical_correlation_forrest}, \code{canonical_correlation_tree},
#' \code{tree} or \code{randomForest}.
#' @param X Numeric matrix (n * 2) with n observations of 2 variables
#' @param Y Numeric matrix with n observations of 1 variable
#' @param title Title text for the plot.
#' @param interpolate If TRUE interpolate linearly, if FALSE (the default)
#' don't interpolate.
#' @param ...	Further arguments passed to model.predict()
#' @importFrom stats predict
#' @export
plot_decision_surface <- function(model, X, Y, title = NULL,
                                  interpolate = FALSE, ...) {
  data <- data.frame(x = X[, 1], y = X[, 2], z = Y)

  # TODO-SF: better use expand?
  x_min <- min(data$x) * 1.2
  x_max <- max(data$x) * 1.2
  y_min <- min(data$y) * 1.2
  y_max <- max(data$y) * 1.2
  resolution <- 400
  grid <- expand.grid(x = seq(x_min, x_max, length.out = resolution),
                      y = seq(y_min, y_max, length.out = resolution))
  predictions <- predict(model, grid, ...)

  data_raster <- data.frame(x = grid$x, y = grid$y, z = predictions)
  plot_object <- generate_2d_data_plot(data,
                                      data_raster,
                                      interpolate = interpolate,
                                      title = title)
  return(plot_object)
}
#' Helper function to print prediction accuracy for a model.
#' @param model a model object for which prediction is desired. E.g. object of class
#' \code{canonical_correlation_forrest}, \code{canonical_correlation_tree},
#' \code{tree} or \code{randomForest}.
#' @param data_test A data frame or a matrix containing the test data.
#' @param ...	Further arguments passed to model.predict()
#' @importFrom stats predict
#' @export
get_missclassification_rate <- function(model, data_test, ...) {
  predictions <- as.matrix(stats::predict(model, data_test, ...))
  # TODO use formula instead of last column
  actual <- data_test[, ncol(data_test)]
  return(mean(actual != predictions))
}

#' @importFrom stats predict
load_csv_data <- function(data_set_path) {
  data <- as.matrix(utils::read.csv(data_set_path, header = FALSE,
                                    sep = ",", quote = "\"",
                                    dec = ".", fill = TRUE, comment.char = ""))
  nr_of_features <- ncol(data) - 1

  return(list(X = data[, 1:nr_of_features],
              Y = data[, ncol(data), drop = FALSE]))
}
