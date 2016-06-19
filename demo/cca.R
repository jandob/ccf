# CCA demo
library(ccf)
library(pracma)
library(MASS)
library(grid)
gen_plot = function(X,Y) {
  cca = canonical_correlation_analysis(X, one_hot_encode(Y))
  data_plot = generate_2d_data_plot(data.frame(x = X[,1], y = X[,2], z = Y)) +
    geom_segment(aes(x = 0, y = 0, xend = x, yend = y),
                 data = data.frame(x = cca$xcoef[1,], y = cca$xcoef[2,]),
                 arrow = arrow(length=unit(0.3,"cm")))
  return(data_plot)
}
nr_of_points = 400
X = mvrnorm(nr_of_points, c(0,0), eye(2))
Y = rbind(ones(nr_of_points, m = 1))
Y[X[,1] < X[,2]] = 2
plot_1 = gen_plot(X, Y)

Y[X[,1] < X[,2] & -X[,1] > X[,2]] = 3
plot_2 = gen_plot(X, Y)

Y[X[,1] > X[,2] & -X[,1] > X[,2]] = 4
plot_3 = gen_plot(X, Y)

#print(plot_1)
#print(plot_2)
#print(plot_3)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))

print(plot_1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot_2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot_3, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
