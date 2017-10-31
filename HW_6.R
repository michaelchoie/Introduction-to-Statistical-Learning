#####################################################
# HW 6 Question 8
#####################################################

set.seed(1)
x <- rnorm(n = 100)
error <- rnorm(n = 100)
y <- 5 + 3 * x - 2 * x^2 + 8 * x^3 + error

# Perform best subset selection, try from predictors X, X^2 ... X^10
library(leaps)
dataset <- data.frame(x,y)
regfit.full <- regsubsets(y ~ poly(x, 10, raw = T), data = dataset, nvmax = 10)
regfit.summary <- summary(regfit.full)

par(mfrow = c(2,2))

plot(regfit.summary$cp, xlab = "Num of Variables", ylab = "Cp", type = "l")
which.min(regfit.summary$cp)
# cex = size of point, pch = type of symbol
points(3, regfit.summary$cp[3], col = "red", cex = 2, pch = 20)

plot(regfit.summary$bic, xlab = "Num of Variables", ylab = "BIC", type = "l")
which.min(regfit.summary$bic)
points(3, regfit.summary$bic[3], col = "red", cex = 2, pch = 20)

plot(regfit.summary$adjr2, xlab = "Num of Variables", ylab = "Adj Rsq", type = "l")
which.max(regfit.summary$adjr2)
points(3, regfit.summary$adjr2[3], col = "red", cex = 2, pch = 20)

coef(regfit.full, 3)

# Forward stepwise regression

regfit.fwd <- regsubsets(y~poly(x, 10, raw = T), data = dataset, nvmax = 10,
                         method = "forward")
fwd.summary <- summary(regfit.fwd)
which.min(fwd.summary$cp)
which.min(fwd.summary$bic)
which.max(fwd.summary$adjr2)

par(mfrow = c(2,3))

plot(fwd.summary$cp, xlab = "Num of Variables", ylab = "Cp", type = "l")
points(4, fwd.summary$cp[4], col = "red", cex = 2, pch = 20)
plot(fwd.summary$bic, xlab = "Num of Variables", ylab = "BIC", type = "l")
points(4, fwd.summary$bic[4], col = "red", cex = 2, pch = 20)
plot(fwd.summary$adjr2, xlab = "Num of Variables", ylab = "Adj Rsq", type = "l")
points(4, fwd.summary$adjr2[4], col = "red", cex = 2, pch = 20)

coef(regfit.fwd, 4)

# Backward stepwise regression

regfit.bwd <- regsubsets(y~poly(x, 10, raw = T), data = dataset, nvmax = 10,
                         method = "backward")
bwd.summary <- summary(regfit.bwd)
which.min(bwd.summary$cp)
which.min(bwd.summary$bic)
which.max(bwd.summary$adjr2)

plot(bwd.summary$cp, xlab = "Num of Variables", ylab = "Cp", type = "l")
points(3, bwd.summary$cp[3], col = "red", cex = 2, pch = 20)
plot(bwd.summary$bic, xlab = "Num of Variables", ylab = "BIC", type = "l")
points(3, bwd.summary$bic[3], col = "red", cex = 2, pch = 20)
plot(bwd.summary$adjr2, xlab = "Num of Variables", ylab = "Adj Rsq", type = "l")
points(3, bwd.summary$adjr2[3], col = "red", cex = 2, pch = 20)

coef(regfit.bwd, 3)

# Lasso Regression

library(glmnet)
x.matrix <- model.matrix(y~poly(x, 10, raw = T), data = dataset, nvmax = 10)[, -1]
lasso.fit <- cv.glmnet(x.matrix, y, alpha = 1)
plot(lasso.fit)
lasso.fit$lambda.min

best.lasso <- glmnet(x.matrix, y, alpha = 1)
predict(best.lasso, s = lasso.fit$lambda.min, type = "coefficients")

# Perform best subset and lasso

y <- 5 + 3 * x^7 + error
dataset <- data.frame(x=x, y=y)
regfit.full <- regsubsets(y ~ poly(x, 10, raw = T), data = dataset, nvmax = 10 )
regfit.summary <- summary(regfit.full)
which.min(regfit.summary$cp)
which.min(regfit.summary$bic)
which.min(regfit.summary$adjr2)
coef(regfit.full, id = 3)
coef(regfit.full, id = 1)

x.matrix <- model.matrix(y~poly(x, 10, raw = T), data = dataset, nvmax = 10)[, -1]
lasso.fit <- cv.glmnet(x.matrix, y, alpha = 1)
best.lasso <- glmnet(x.matrix, y, alpha = 1)
predict(best.lasso, s = lasso.fit$lambda.min, type = "coefficients")

#####################################################
# HW 6 Question 9
#####################################################

library(ISLR)
set.seed(1)
college <- data.frame(College)

# Partition data
train <- sample(1:nrow(college), nrow(college)*.8)
test <- -train

# Fit linear model using least squares
# Test MSE = 1,075,064
lm.fit <- lm(Apps~., data = college, subset = train)
lm.pred <- predict(lm.fit, college[test, ])
lm.mse <- mean((college$Apps[test] - lm.pred)^2)

# Fit ridge regression model with lambda chosen by CV
# Test MSE = 1,075,062
x.train <- model.matrix(Apps ~ ., data = college[train, ])[, -1]
x.test <- model.matrix(Apps ~ ., data = college[test, ])[, -1]
grid <- 10^seq(10, -2, length = 100)
ridge.fit <- cv.glmnet(x.train, college$Apps[train], alpha = 0, lambda = grid,
                       thresh = 1e-12)
ridge.pred <- predict(ridge.fit, s = ridge.fit$lambda.min, newx = x.test)
ridge.mse <- mean((ridge.pred - college$Apps[test])^2)

# Fit lasso regression model with lambda chosen by CV
# Test MSE = 1,112,058
# Got rid of 5 columns
lasso.fit <- cv.glmnet(x.train, college$Apps[train], alpha = 1, lambda = grid,
                       thresh = 1e-12)
lasso.pred <- predict(lasso.fit, s= lasso.fit$lambda.min, newx = x.test)
lasso.mse <- mean((lasso.pred - college$Apps[test])^2)
x <- model.matrix(Apps ~ ., data = college)[, -1]
lasso.mod <- glmnet(x, college$Apps, alpha = 1)
predict(lasso.mod, s = lasso.fit$lambda.min, type = "coefficients")

# Fit PCR model with M chosen by CV
# Test MSE = 984,161.3
# M = 17
library(pls)
pcr.fit <- pcr(Apps ~ ., data = college[train, ], scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

pcr.fit <- pcr(Apps ~ ., data = college[test,], scale = TRUE)
pcr.pred <- predict(pcr.fit, newx = x.test, ncomp = 17)
pcr.mse <- mean((data.frame(pcr.pred) - college$Apps[test])^2)

# Fit PLS model with M chosen by CV
# Test MSE = 984,172.1
# M = 13
pls.fit <- plsr(Apps ~ ., data = college[train, ], scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls.fit <- plsr(Apps ~ ., data = college[test, ], scale = TRUE)
pls.pred <- predict(pls.fit, newx = x.test, ncomp = 13)
pls.mse <- mean((data.frame(pls.pred) - college$Apps[test])^2)

# Compare test errors of all 5 approaches
barplot(c(lm.mse, ridge.mse, lasso.mse, pcr.mse, pls.mse), col = "blue",
        names.arg = c("linear", "ridge", "lasso", "pcr", "pls"),
        main = "Test MSE for All Models")

#####################################################
# HW 6 Question 10
#####################################################

# Generate a data set of p=20, n=1000, and response vector
# Beta has some elements = 0
set.seed(1)
dataset <- matrix(data = rnorm(1000*20), nrow = 1000, ncol = 20)
beta <- rnorm(20)
beta[sample(seq(20), 5)] <- 0
error <- rnorm(20)
y <- dataset %*% beta + error

# Partition data 90/10 split
train <- sample(seq(nrow(dataset)), nrow(dataset) * .9)
test <- -train

# Perform best subset and plot best model train/test MSE for each size
regfit.full <- regsubsets(y ~ ., data = data.frame(x = dataset[train, ], y = y[train]),
                          nvmax = 20)
train.mse <- rep(0, 20)

# Need to do matrix multiplication of betas and chosen columns
dataset.cols <- colnames(dataset, do.NULL = FALSE, prefix = "x.")
train.errors <- rep(0, 20)
test.errors <- rep(0,20)

computeErrors <- function(dataset, vecError, partition) {
    for (i in seq(ncol(dataset))) {
        coefi <- coef(regfit.full, id = i)
        pred <- as.matrix(dataset[partition, dataset.cols %in% names(coefi)]) %*% coefi[-1]
        vecError[i] <- mean((y[partition] - pred)^2)
    }
    return(vecError)
}

train.errors <- computeErrors(dataset, train.errors, train)
test.errors <- computeErrors(dataset, test.errors, test)

par(mfrow = c(1,2))
plot(train.errors, xlab = "# of Columns", ylab = "Training MSE", pch = 19, type = "b")
plot(test.errors, xlab = "# of Columns", ylab = "Testing MSE", pch = 19, type = "b")

# Find model with smallest test MSE
which.min(test.errors)
coef(regfit.full, id = 16)

# Create plot with RMSE
full.errors <- rep(0, 20)
full.errors <- computeErrors(dataset, full.errors, seq(nrow(dataset)))
full.errors <- sapply(full.errors, function(x) return(sqrt(x)))
plot(full.errors, xlab = "# of Columns", ylab = "RMSE", pch = 19, type = "b")

#####################################################
# HW 6 Question 11
#####################################################

# Predict per capita crime rate
library(MASS)
library(leaps)
library(glmnet)
library(pls)

set.seed(1)
boston <- data.frame(Boston)
train <- sample(seq(nrow(boston)), .8*nrow(boston))
test <- -train

# Conduct best subset
predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}

k <- 10
folds <- sample(rep(1:k, length = nrow(boston)))
cv.errors <- matrix(NA, k, ncol(boston) - 1)

for (j in seq(k)) {
    best.fit <- regsubsets(crim ~ ., data = boston[folds != j, ], nvmax = 14)
    for (i in seq(ncol(boston) - 1)) {
        pred <- predict.regsubsets(best.fit, boston[folds == j, ], id = i)
        cv.errors[j, i] <- mean((boston$crim[folds == j] - pred)^2)
    }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
subset.mse <- mean.cv.errors[which.min(mean.cv.errors)]

# Ridge
X <- model.matrix(crim ~ ., boston)
Y <- boston$crim

cv.out <- cv.glmnet(X[train, ], Y[train], alpha = 0)
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(X[train,], Y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = cv.out$lambda.min, newx = X[test, ])
ridge.mse <- mean((ridge.pred - Y[test])^2)

# Lasso
cv.out <- cv.glmnet(X[train, ], Y[train], alpha = 1)
lasso.mod <- glmnet(X[train,], Y[train], alpha = 0, lambda = grid, thresh = 1e-12)
lasso.pred <- predict(lasso.mod, s = cv.out$lambda.min, newx = X[test, ])
lasso.mse <- mean((lasso.pred - Y[test])^2)

# PCR
pcr.fit <- pcr(crim ~ ., data = boston, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred <- predict(pcr.fit, boston[test, ], ncomp = 8)
pcr.mse <- mean((pcr.pred - Y[test])^2)

# Evaluate performance of models
barplot(c(subset.mse, ridge.mse, lasso.mse, pcr.mse), col = "blue",
        names.arg = c("best subset", "ridge", "lasso", "pcr"),
        main = "Test MSE for All Models")

# Does chosen model use all or subset of features - why or why not
which.min(mean.cv.errors)  # 12 features
