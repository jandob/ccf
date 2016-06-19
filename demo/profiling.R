#profiling
data = ccf::spirals
colnames(data) = c("x", "y", "z")
# sample 1000 data points
data = data[sample(nrow(data), 1000), ]
generate_2d_data_plot(data)

# convert to matrices
X = cbind(data$x,data$y)
Y = data$z

library(profvis)

profvis::profvis({
  model = canonical_correlation_tree(X, one_hot_encode(Y))
})

# benchmark for the right_cum_counts in find_best_split()
v = seq(1,1000)
m = cbind(v,v,v)
total = c(1,2,3)
microbenchmark::microbenchmark(
  sweep(m, MARGIN = 2, total, FUN = '-')*-1,
  t(apply(m, 1, function(x) {total - x})),
  apply((apply(apply(m, 2, rev), 2, cumsum)),2,rev),
  (apply(m[nrow(m):1,], 2, cumsum))[nrow(m):1,]
)
