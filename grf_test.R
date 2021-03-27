# Train a causal forest.
n <- 500
p <- 10
X <- matrix(rnorm(n * p), n, p)
W <- rbinom(n, 1, 0.5)
Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
c.forest <- causal_forest(X, y, W)
X_test = X %>% slice_sample(n = 100)

# Predict using the forest.
X.test <- matrix(0, 101, p)
X.test[, 1] <- seq(-2, 2, length.out = 101)
c.pred <- predict(c.forest, X.test)

# Predict on out-of-bag training samples.
c.pred <- predict(c.forest)

# Predict with confidence intervals; growing more trees is now recommended.
c.forest <- causal_forest(X, y, W, num.trees = 4000)
c.pred <- predict(c.forest, X_test,  estimate.variance = TRUE)
c.pred$upper = c.pred[,1] + qnorm(1-0.05/2) * c.pred[,2]
c.pred$lower = c.pred[,1] - qnorm(1-0.05/2) * c.pred[,2]

plot(X_test[,"grocery"],c.pred$predictions)
matlines(X_test[,"grocery"], c.pred[,3:4], col = "blue", lty=2)

library(ggplot2)
data = data.frame(X = X.test[,1], Y = c.pred[,1], Y_var = c.pred)