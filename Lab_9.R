########################################################################################
# LAB 9.6.1 Support Vector Classifier
########################################################################################

set.seed(1)
library(e1071)

x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1

plot(x, col = (3-y))

# Must encode response as factor to perform classification for SVM function
# Cost specifies cost of violation to the margin (big cost = small margins, few support vectors)
# Scale = F means no standardization [mean = 0, sd = 1]
dat <- data.frame(x = x, y = as.factor(y))
svm.fit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = F)
plot(svm.fit, dat)
svm.fit$index
summary(svm.fit)

# Lower cost means wider margins
svm.fit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = F)
plot(svm.fit, dat)
svm.fit$index
summary(svm.fit)

# tune() performs 10 fold cross validation
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

# Generate test data set
xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = T)
xtest[ytest == 1] <- xtest[ytest == 1] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

# Separate data further
x[y == 1, ] <- x[y == 1, ] + 0.5
plot(x, col = (y + 5)/2, pch = 19)

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

########################################################################################
# LAB 9.6.2 Support Vector Machine
########################################################################################

set.seed(1)
library(e1071)
x <- matrix(rnorm(400), ncol = 2)
x[1:100, ] <- x[1:100] + 2
x[101:150, ] <- x[101:150] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
train <- sample(200, 100)

plot(x, col = y)

# Gamma = controls how nonlinear boundary is
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train, ])
summary(svmfit)

# Recreate model with high cost (reduce # of training errors)
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train, ])

tune.out <- tune(svm, y ~ ., data = dat[train, ], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
table(true = dat[-train, "y"], pred = predict(tune.out$best.model, newx = dat[-train, ]))
(22 + 19)/100 # 41% error rate

########################################################################################
# LAB 9.6.3 ROC Curves
########################################################################################

# ROCR package to produce ROC curves
# Pred = vector with numerical scores for each observation
# Truth = vector with class label for each observation

# prediction() = function that transform input data into standardized format
# performance() = many different kinds of valuations on predictor object (tpr = true positive rate, fpr = false positive rate)

library(ROCR)
rocplot = function(pred, truth, ...) {
    predob <- prediction(pred, truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
}

# SVM/SVC output class labels, but can also output fitted values (decision.values = T)
# Fitted values are the numerical scores used to obtain the class labels
# +/- status of fitted value indicate class
# attributes() = returns object's attribute list

# Train data plot
svmfit.opt <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 2, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.opt, dat[train, ], decision.values = T))$decision.values
par(mfrow = c(1, 2))
rocplot(fitted, dat[train, "y"], main = "Training Data")

svmfit.flex <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 50, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.flex, dat[train, ], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col = "red")

# Test data plot
fitted <- attributes(predict(svmfit.opt, dat[-train, ], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], main = "Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train, ], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col = "red")

########################################################################################
# LAB 9.6.4 SVM with Multiple Classes
########################################################################################

# svm() automatically conducts one-vs-one approach for multinomial target vars

set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1,1))
plot(x, col = (y + 1))

svmfit <- svm(y ~ ., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

########################################################################################
# LAB 9.6.5 Application to Gene Expression Data
########################################################################################

library(ISLR)

# Train/Test split is approximately 80/20
dim(Khan$xtrain)
dim(Khan$xtest)

# Multiple levels in response variable
table(Khan$ytest)

# Use linear kernel, as there are a very large number of features relative to the number of observations.
# Additional flexibility that will result from using a polynomial or radial kernel is unnecessary
dat <- data.frame(x = Khan$xtrain, y= as.factor(Khan$ytrain))
out <- svm(y ~ ., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y)

dat.te <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te <- predict(out, dat.te)
table(pred.te, dat.te$y)
