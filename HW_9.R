########################################################################################
# HW 9 Question 4
########################################################################################

# Generate a simulated two-class data set with 100 observations and two features in which there is a
# visible but non-linear separation between the two classes.

set.seed(1)
library(e1071)

x <- rnorm(100)
y <- 3 + 5*x^2 + rnorm(100)
train <- sample(100, 50)
y[train] <- y[train] + 3
y[-train] <- y[-train] - 3
plot(x[train], y[train], pch = "+", lwd = 4, col = "red", xlab = "X", ylab = "Y", ylim = c(-3, 35))
points(x[-train], y[-train], pch = "o", lwd = 4, col = "blue")

z <- rep(0, 100)
z[train] <- 1
partition <- c(sample(train, 25), sample(setdiff(1:100, train), 25))

dat <- data.frame(x = x, y = y, z = z)

# Try Support Vector Classifier
tune.out <- tune(svm, z ~ ., data = dat[partition, ], kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out$best.model) # cost = 1
svc <- svm(z ~ ., data = dat[partition, ], kernel = "linear", cost = 1, decision.values = T)

# Try SVM w/ polynomial kernel
tune.out <- tune(svm, z ~ ., data = dat[partition, ], kernel = "polynomial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000)))
summary(tune.out$best.model) # degree = 3, cost = 0.1
svm.poly <- svm(z ~ ., data = dat[partition, ], kernel = "polynomial", degree = 3, cost = 0.1, decision.values = T)

# Try SVM w/ radial kernel
tune.out <- tune(svm, z ~ ., data = dat[partition, ], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out$best.model) # gamma = 0.5, cost =10
svm.rad <- svm(z ~ ., data = dat[partition, ], kernel = "radial", gamma = 0.5, cost = 10, decision.value = T)

# Make plots and report training and test error rates in order to back up your assertions.
library(ROCR)
rocplot = function(pred, truth, ...) {
    predob <- prediction(pred, truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
}

par(mfrow = c(1, 2))

# Train data plot
fitted <- attributes(predict(svc, dat[partition, ], decision.values = T))$decision.values
rocplot(fitted, dat[partition, "z"], main = "Training Data", lwd = 2)

fitted <- attributes(predict(svm.poly, dat[partition, ], decision.values = T))$decision.values
rocplot(fitted, dat[partition, "z"], add = T, col = "red", lwd = 2)

fitted <- attributes(predict(svm.rad, dat[partition, ], decision.values = T))$decision.values
rocplot(fitted, dat[partition, "z"], add = T, col = "blue", lwd = 2)

legend("bottomright", legend = c("Linear", "Polynomial", "Radial"), col = c("black", "red", "blue"),
       lty = 1, lwd = 2, cex = 0.8)

# Test data plot
fitted <- attributes(predict(svc, dat[-partition, ], decision.values = T))$decision.values
rocplot(fitted, dat[-partition, "z"], main = "Test Data", lwd = 2)

fitted <- attributes(predict(svm.poly, dat[-partition, ], decision.values = T))$decision.values
rocplot(fitted, dat[-partition, "z"], add = T, col = "red", lwd = 2)

fitted <- attributes(predict(svm.rad, dat[-partition, ], decision.values = T))$decision.values
rocplot(fitted, dat[-partition, "z"], add = T, col = "blue", lwd = 2)

legend("bottomright", legend = c("Linear", "Polynomial", "Radial"), col = c("black", "red", "blue"),
       lty = 1, lwd = 2, cex = 0.8)

########################################################################################
# HW 9 Question 5
########################################################################################

# We have seen that we can fit an SVM with a non-linear kernel in order to perform classification
# using a non-linear decision boundary. We will now see that we can also obtain a non-linear decision boundary
# by performing logistic regression using non-linear transformations of the features.

set.seed(2)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1*(x1^2 - x2^2 > 0)

plot(x = x1[y == 0], y = x2[y == 0], col = c("red", "blue"), pch = "o")
points(x = x1[y != 0], y = x2[y != 0], col = c("red", "blue"), pch = 4)

lm.fit <- glm(y ~ x1 + x2, family = "binomial")
summary(lm.fit)

data <- data.frame(x1 = x1, x2 = x2, y = y)
lm.prob <- predict(lm.fit, data, type = "response")
lm.pred <- ifelse(lm.prob > 0.47, 1, 0)
data.pos <- data[lm.pred == 1, ]
data.neg <- data[lm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", pch = "o")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

# Now fit model using non-linear functions of X1 and X2 as predictors (e.g. X12, X1 ×X2, log(X2), and so forth)
lm.fit <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), data = data, family = "binomial")
lm.prob <- predict(lm.fit, data, type = "response")
lm.pred <- ifelse(lm.prob > 0.5, 1, 0)
data.pos <- data[lm.pred == 1, ]
data.neg <- data[lm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", pch = "o")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

# Now fit model using SVC
library(e1071)
svm.fit <- tune(svm, y ~ x1 + x2, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(svm.fit$best.model)
svm.fit <- svm(as.factor(y) ~ x1 + x2, kernel = "linear", cost = 0.01)
svm.pred <- predict(svm.fit, data)
data.pos <- data[svm.pred == 1, ]
data.neg <- data[svm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", pch = "o")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

# Now fit model using SVM
svm.fit <- tune(svm, y ~ x1 + x2, kernel = "radial", ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100), gamma = c(0.5, 1, 2, 3, 4)))
summary(svm.fit$best.model)
svm.fit <- svm(as.factor(y) ~ x1 + x2, kernel = "radial", cost = 5, gamma = 4)
svm.pred <- predict(svm.fit, data)
data.pos <- data[svm.pred == 1, ]
data.neg <- data[svm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", pch = "o")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

########################################################################################
# HW 9 Question 6
########################################################################################

# Investigate claim that a SVC with a small value of cost that misclassifies a couple of training observations
# may perform better on test data than one with a huge value of cost that does not misclassify any

set.seed(1)
library(e1071)

# runif() gives random number from uniform distribution (vs rnorm which gives from normal distribution)
# noise points along the line 5x−4y−50=0
x.one <- runif(500, 0, 90)
y.one <- runif(500, x.one + 10, 100)
x.one.noise <- runif(50, 20, 80)
y.one.noise <- 5/4 * (x.one.noise -10) + 0.1

x.zero <- runif(500, 10, 100)
y.zero <- runif(500, 0, x.zero - 10)
x.zero.noise <- runif(50, 20, 80)
y.zero.noise <- 5/4 * (x.zero.noise -10) - 0.1

class.one <- 1:550
x <- c(x.one, x.one.noise, x.zero, x.zero.noise)
y <- c(y.one, y.one.noise, y.zero, y.zero.noise)

plot(x[class.one], y[class.one], col = "blue", pch = "+", ylim = c(0, 100))
points(x[-class.one], y[-class.one], col = "red", pch = 4)

z <- rep(0, length(x))
z[class.one] <- 1
data <- data.frame(x, y, z)

# Fit SVC using CV to determine optimal cost

tune.out <- tune(svm, z ~ ., data = data, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
which.min(tune.out$performances$error)
tune.out$performances$error[7]

# Create test data
x.test <- runif(1000, 0, 100)
class.one <- sample(1000, 500)
y.test <- rep(NA, 1000)

y.test[class.one] <- sapply(class.one, function(x) runif(1, x.test[x], 100))
y.test[setdiff(1:1000, class.one)] <- sapply(setdiff(1:1000, class.one), function(x) runif(1, 0, x.test[x]))

plot(x.test[class.one], y.test[class.one], col = "blue", pch = "+", ylim = c(0, 100))
points(x.test[-class.one], y.test[-class.one], col = "red", pch = 4)

z.test <- rep(0, 1000)
z.test[class.one] <- 1

test.errors <- rep(0, 7)
costs <- c(0.001, 0.01, 0.1, 1,5,10,100)
data.test <- data.frame(x = x.test, y = y.test, z = z.test)

for (i in 1:length(costs)) {
    svm.fit = svm(as.factor(z) ~ ., data = data, kernel = "linear", cost = costs[i])
    svm.predict = predict(svm.fit, data.test)
    test.errors[i] = sum(svm.predict != data.test$z)
}

data.frame(cost = costs, `test misclass` = test.errors)

########################################################################################
# HW 9 Question 7
########################################################################################

# Use support vector approach in order to predict whether a given car gets high or low gas mileage

library(ISLR)
library(e1071)
Auto <- data.frame(Auto)

# Create a binary variable that takes on a 1 for cars with gas mileage above the median, and a 0 for cars with gas mileage below the median.
Auto$mileage <- as.factor(ifelse(Auto$mpg >= median(Auto$mpg), 1, 0))

# Fit a support vector classifier to the data with various values of cost, in order to predict whether a car gets high or low gas mileage.
# Report the cross-validation errors associated with different values of this parameter.
tune.out <- tune(svm, mileage ~ ., data = Auto, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10)))
summary(tune.out)
svc <- tune.out$best.model

# Use SVMs with radial and polynomial basis kernels, with different values of gamma and degree and cost

tune.out <- tune(svm, mileage ~ ., data = Auto, kernel = "polynomial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10), degree = c(2, 3, 4)))
summary(tune.out)
svm.poly <- tune.out$best.model

# Try SVM w/ radial kernel
tune.out <- tune(svm, mileage ~ ., data = Auto, kernel = "radial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
svm.rad <- tune.out$best.model

# Make some plots
plotpairs = function(fit) {
    sapply(names(Auto)[!(names(Auto) %in% c("mpg", "mileage", "name"))], function(x) plot(fit, Auto, as.formula(paste("mpg~", x, sep = ""))))
}
plotpairs(svc)
plotpairs(svm.poly)
plotpairs(svm.rad)

########################################################################################
# HW 9 Question 8
########################################################################################

set.seed(1)
library(ISLR)
OJ <- data.frame(OJ)
train <- sample(nrow(OJ), 800)
test <- -train

# Fit a support vector classifier to the training data using cost=0.01,
# with Purchase as the response and the other variables as predictors.
# Use the summary() function to produce summary statistics, and describe the results obtained.

svc <- svm(Purchase ~ ., data = OJ[train,], kernel = "linear", cost = 0.01)
summary(svc)

table(predict(svc, newdata = OJ[train,]), OJ$Purchase[train])
(55+78)/800 # Train Error: 16.63%
table(predict(svc, newdata = OJ[test,]), OJ$Purchase[test])
(18+31)/270 # Test Error: 18.15%

# Conduct CV to find optimal cost hyperparameter

tune.out <- tune(svm, Purchase ~ ., data = OJ[train,], kernel = "linear",
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
tune.out$best.model # Cost = 5

svc <- tune.out$best.model
table(predict(svc, newdata = OJ[train,]), OJ$Purchase[train])
(56+69)/800 # Train Error: 15.63%
table(predict(svc, newdata = OJ[test,]), OJ$Purchase[test])
(18+30)/270 # Test Error: 17.78%

# Conduct CV for polynomial and radial SVM as well

tune.out <- tune(svm, Purchase ~ ., data = OJ[train,], kernel = "polynomial", degree = 2,
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
tune.out$best.model # Cost = 10

svm.poly <- tune.out$best.model
table(predict(svm.poly, newdata = OJ[train,]), OJ$Purchase[train])
(40+79)/800 # Train Error: 14.88%
table(predict(svm.poly, newdata = OJ[test,]), OJ$Purchase[test])
(17+32)/270 # Test Error: 18.15%

tune.out <- tune(svm, Purchase ~ ., data = OJ[train,], kernel = "radial",
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
tune.out$best.model # Cost = 1

svm.rad <- tune.out$best.model
table(predict(svm.rad, newdata = OJ[train,]), OJ$Purchase[train])
(39+77)/800 # Train Error: 14.5%
table(predict(svm.rad, newdata = OJ[test,]), OJ$Purchase[test])
(18+28)/270 # Test Error: 17.04%