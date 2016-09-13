# CCF demo
data(spirals)
d <- spirals
colnames(d) <- c("x", "y", "z")
d$z <- as.factor(d$z)

d_train <- d[1:1000,]
d_test <- d[1001:10000,]

# sample 1000 d points
#d <- d[sample(nrow(d), 1000), ]

generate_2d_data_plot(d_train)


# convert to matrices
X <- cbind(d_train$x,d_train$y)
Y <- d_train$z

# classify with a standard binary decision tree
library("tree")
model <- tree(as.factor(z)~., d_train)
error_tree <- get_missclassification_rate(model,
                                         d_test,
                                         type = 'class')
print(paste("tree missclassification rate:", error_tree))
plot_tree <- plot_decision_surface(model, X, Y,
                                  type = 'class',
                                  title = "Single CART")

# classify with random forest
library("randomForest")
model <- randomForest(as.factor(z)~., d_train, ntree = 200)
error_rf <- get_missclassification_rate(model,
                                         d_test,
                                         type = 'class')
print(paste("rf missclassification rate:", error_rf))
plot_rf <- plot_decision_surface(model, X, Y,
                                type = 'class',
                                title = "RF with 200 Trees")

# classify with oblique tree
#library("oblique.tree")
#model <- oblique.tree(z~., d = d_train)

#predict(model, d.frame(x=X[,1], y=X[,2], z=0), type = "class")
#plot_tree <- plot_decision_surface(model, X, Y,
#                                  type = 'class',
#                                  title = "Oblique Tree")

# classify with rotation forest
#library("rotationForest")
#model <- rotationForest(X, one_hot_encode(as.factor(Y)))
#d(iris)
#y <- as.factor(one_hot_encode(iris$species[1:100]))
#x <- iris[1:100,-5]
#rF <- rotationForest(x,y)
#predict(object=rF,newd=x)

# classify with single CCT
model <- canonical_correlation_tree(X, one_hot_encode(Y))
error_cct <- get_missclassification_rate(model, d_test)
print(paste("cct missclassification rate:", error_cct))
plot_cct <- plot_decision_surface(model, X, Y, title = "Single CCT")

# classify with CCF
model <- canonical_correlation_forest(X, one_hot_encode(Y), ntree = 200)
canonical_correlation_forest(as.factor(z)~., d_train)
error_ccf <- get_missclassification_rate(model, d_test)
print(paste("ccf missclassification rate:", error_ccf))
plot_ccf <- plot_decision_surface(model, X, Y, title = "CCF with 200 Trees")

library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

print(plot_tree, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot_rf, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot_cct, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot_ccf, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))


