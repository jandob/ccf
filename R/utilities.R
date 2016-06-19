random_element = function(x) {
  if (length(x) > 1) {
    return(sample(x, 1))
  }
  return(x)
}

eps = 1 - 3*(4/3 - 1)

one_hot_encode = function(data) {
  if (!is.data.frame(data)) {
    data = data.frame(data)
    colnames(data) = "label"
  }
  data$label = as.factor(data$label)
  # This trick only works with factors
  return(model.matrix(~label-1, data = data))
}

one_hot_decode = function(X_one_hot) {
  X = apply(X_one_hot, 1, function(row){which(row == 1)})
  return(X)
}
TODO = function(message = "TODO", return=NULL) {
  print(paste("TODO", message))
  if (!is.null(return)) {
    return(return)
  }
  stop()
}
generate_2d_data_plot = function(data = NULL,
                                 data_raster = NULL,
                                 interpolate = F,
                                 title = "") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for plotting to work. Please install it.",
         call. = FALSE)
  }
  #x_min = min(data$x, data_raster$x)
  #x_max = max(data$x, data_raster$x)
  #y_min = min(data$y, data_raster$y)
  #y_max = max(data$y, data_raster$y)
  blank = ggplot2::element_blank()
  g = ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::coord_fixed(ratio = 0.8) +
    ggplot2::theme(axis.ticks = blank,
                   panel.grid.major = blank,
                   panel.grid.minor = blank,
                   axis.text = blank,
                   axis.title = blank,
                   legend.position = 'none') +
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
plot_decision_surface = function(model, X, Y, title = NULL, interpolate = F, ...) {
  data = data.frame(x = X[,1], y = X[,2], z = Y)

  x_min <- min(data$x) * 1.2
  x_max <- max(data$x) * 1.2
  y_min <- min(data$y) * 1.2
  y_max <- max(data$y) * 1.2
  resolution <- 400
  grid <- expand.grid(x = seq(x_min, x_max, length.out = resolution),
                      y = seq(y_min, y_max, length.out = resolution))
  predictions = predict(model, grid, ...)

  data_raster = data.frame(x = grid$x, y = grid$y, z = predictions)
  plot_object = generate_2d_data_plot(data,
                                      data_raster,
                                      interpolate = interpolate,
                                      title = title)
  return(plot_object)
}
get_missclassification_rate = function(model, data_test, ...) {
  predictions = as.matrix(predict(model, data_test, ...))
  # TODO use formula instead of last column
  actual = data_test[,ncol(data_test)]
  return(mean(actual != predictions))
}
