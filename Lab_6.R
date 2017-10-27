#####################################################
# LAB 6.5.1 Best Subset Selection
#####################################################

# Load in data
library(ISLR)
hitters <- data.frame(Hitters)
str(hitters)

# Remove any NA values
sum(is.na(hitters))
hitters <- na.omit(hitters)

# in leaps library, use regsubsets() to perform best subset selection in terms of RSS
# nvmax() to specify how many variables we want, default = up to 8
library(leaps)
regfit.full <- regsubsets(x = Salary~., data = hitters, nvmax = ncol(hitters))

# Summary returns summary statistics i.e R^2, RSS, AIC, BIC, Cp, etc.
reg.summary <- summary(regfit.full)
reg.summary$rsq

# Plot summary statistics r squared and adj r squared
# type = "l" means to connect dots with line
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adj Rsq", type = "l")

# Plot points
# points() puts points on a plot that has already been created
# which.max() identifies maximum point in vector
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

# Plot summary statistics Cp and BIC
# Indicate smallest point
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

# regsubsets() has a built-in plot command
par(mfrow=c(2,2))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

# use coef() to see coefficient estimates associated with variables
coef(regfit.full, 6)

#####################################################
# LAB 6.5.2 Forward and Backward Stepwise Selection
#####################################################

# Can use regsubsets() to do forward/backward stepwise regression
regfit.fwd <- regsubsets(Salary ~ ., data = hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

# Identical variables but different coefficients
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

#####################################################
# LAB 6.5.3 Choosing Among Models Using the Validation Set Approach and Cross-Validation
#####################################################

set.seed(1)
train <- sample(x = c(TRUE, FALSE), size = nrow(hitters), replace = TRUE)
test <- !train

regfit.best <- regsubsets(Salary ~., data = hitters[train,], nvmax = 19)
# model.matrix() builds an "X" matrix with intercept column
test.mat <- model.matrix(Salary ~., data = hitters[test,])

val.errors <- rep(NA, 19)
for (i in 1:19) {
    coefi <- coef(regfit.best, id=i)
    # %*% multiplies matricies
    pred <- test.mat[, names(coefi)] %*% coefi
    val.errors[i] <- mean((hitters$Salary[test] - pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best, 10)

predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}

# Do cross validation to determine best # variable subset model
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(hitters), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
    best.fit <- regsubsets(Salary~., data = hitters[folds!=j, ], nvmax=19)
    for (i in 1:19) {
        pred <- predict.regsubsets(best.fit, hitters[folds == j, ], id = i)
        cv.errors[j, i] <- mean((hitters$Salary[folds==j]-pred)^2)
    }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')
reg.best <- regsubsets(Salary ~ ., data = hitters, nvmax=19)
coef(reg.best, 11)

#####################################################
# LAB 6.6.1 Ridge Regression
#####################################################

# glmnet() function requires one to pass both x matrix and y vector
# can only take in numerical values so converts nominal to dummy vars
library(glmnet)
x <- model.matrix(Salary ~ ., hitters)[, -1]
y <- hitters$Salary

# Range from lambda 10^10 to 10^-2
grid <- 10^seq(10, -2, length = 100)
# alpha = 0 is ridge, alpha = 1 is lasso
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
# coef can access ridge's coefficient matrix
# 20 rows - 19 features + intercept - and 100 rows - for each lambda
coef(ridge.mod)

# Clearly, as lambda increases, coefficients decrease
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

ridge.mod$lambda[60]
coef(ridge.mod)[,60]

# Use predict() to try arbitrary lambda, like 50:
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]

# 2 ways to randomly partition data
# The first is to produce a random vector of TRUE, FALSE elements and select the observations corresponding to TRUE for the training data.
# The second is to randomly choose a subset of numbers between 1 and n; these can then be used as the indices for the training observations.
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- -train
y.test <- y[test]

ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)

# If we fit a model of just intercept, would've predicted each test observation
# using mean of training observations
mean((mean(y[train])-y.test)^2)

# 1e10 means 10^10 - i.e case where ridge is like intercept only model
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2)

# Any benefit to performing ridge regression with λ = 4 instead of just performing least squares regression?
# Recall that least squares is simply ridge regression with λ = 0
ridge.pred <- predict(ridge.mod, s=0, newx = x[test,])
mean((ridge.pred - y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod, s=0, type='coefficients')[1:20,]

# cv.glmnet() provides built-in cross validation function
# nfolds argument for number of folds
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out, s = bestlam, type = "coefficients")[1:20, ]

#####################################################
# LAB 6.6.2 Lasso Regression
#####################################################

# Fit lasso model
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# Perform CV and get test error
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, s = bestlam, type = "coefficients")[1:20, ]

#####################################################
# LAB 6.7.1 Principle Components Analysis
#####################################################

# pcr() is part of pls library
library(pls)
set.seed(2)

# scale = TRUE means you standardize features, necessary because otherwise higher scaled
#         variables will have higher variances, which messes up PCA
# validation = CV means you do CV to determine optimal # of principal components
pcr.fit <- pcr(Salary~., data = hitters, scale = TRUE, validation = "CV")
summary(pcr.fit)

# can plot cv scores using validationplot()
# val.type = "MSEP" gives scores in relation to MSE
validationplot(pcr.fit, val.type = "MSEP")

set.seed(1)
pcr.fit <- pcr(Salary~., data = hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred <- predict(pcr.fit, hitters[test, ], ncomp = 7)
mean((pcr.pred - y.test)^2)

pcr.fit <- pcr(y~x, scale = TRUE, validation = "CV")
summary(pcr.fit)

#####################################################
# LAB 6.7.2 Partial Least Squares
#####################################################

# plsr() is part of pls library
set.seed(1)
pls.fit <- plsr(Salary~., data = hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)

pls.pred <- predict(pls.fit, hitters[test, ], ncomp = 2)
mean((pcr.pred - y.test)^2)

pls.fit <- plsr(y~x, scale = TRUE, validation = "CV", ncomp = 2)
summary(pls.fit)

# Less vars in PLS than in PCR
# PCR only attempts to maximize the amount of variance explained in the predictors
# PLS searches for directions that explain variance in both the predictors and the response.
