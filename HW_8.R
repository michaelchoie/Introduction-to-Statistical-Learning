########################################################################################
# HW 8 Question 7
########################################################################################

# Create random forest on Boston data and record test RMSE for range of tuning params
set.seed(1)
library(randomForest)
library(MASS)

boston <- data.frame(Boston)

train <- sample(nrow(boston), .8*nrow(boston))
test <- -train

x.train <- boston[train, -14]
x.test <- boston[test, -14]
y.train <- boston[train, 14]
y.test <- boston[test, 14]

mtry.grid <- 1:(ncol(boston) - 1)
vecRF <- sapply(mtry.grid, function(mtry) mean(randomForest(x.train, y.train, xtest = x.test,
                                                            ytest = y.test, mtry = mtry, ntree = 500)$test$mse))
which.min(vecRF)
plot(vecRF, lwd = 2, type = "b", pch = 15, xlab = "Number of Predictors Considered",
     ylab = "Test MSE")
points(12, vecRF[12], col = "red", pch = 20, cex = 3)

# Plot lines over # of trees for lowest Test MSE models
vecRF <- sapply(mtry.grid[c(6, 9, 12)], function(mtry) randomForest(x.train, y.train, xtest = x.test,
                                                       ytest = y.test, mtry = mtry, ntree = 500)$test$mse)
range(vecRF)
plot(1:500, vecRF[,1], col = "red", type = "l", xlab = "Number of Trees", ylab = "Test MSE",
     ylim = c(7, 30), lwd = 2)
lines(1:500, vecRF[,2], col = "green", type = "l", lwd = 2)
lines(1:500, vecRF[,3], col = "blue", type = "l", lwd = 2)
legend("topright", c("m = 6", "m = 9", "m = 12"), col = c("red", "green", "blue"),
       lty = 1, lwd = 2)

########################################################################################
# HW 8 Question 8
########################################################################################

# Predict sales using regression trees
library(ISLR)
library(tree)
set.seed(1)
carseats <- data.frame(Carseats)
train <- sample(nrow(carseats), .8 * nrow(carseats))
test <- -train

tree.carseats <- tree(Sales ~ ., data = carseats, subset = train)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

yhat <- predict(tree.carseats, newdata = carseats[test, ])
mean((yhat - carseats$Sales[test])^2) # MSE = 4.817

# Use CV to determine optimal level of tree complexity
cv.carseats <- cv.tree(tree.carseats)
par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
cv.carseats$size[which.min(cv.carseats$dev)] # 9 terminal nodes

# Does pruning improve test MSE?
prune.carseats <- prune.tree(tree.carseats, best = 9)
dev.off()
plot(prune.carseats)
text(prune.carseats, pretty = 0)

yhat <- predict(prune.carseats, newdata = carseats[test, ])
mean((yhat - carseats$Sales[test])^2) # MSE = 4.831

# Use bagging, calculate test MSE, use importance()
library(randomForest)
bag.carseats <- randomForest(Sales ~ ., data = carseats, subset = train, mtry = 10,
                             importance = T)
yhat.bag <- predict(bag.carseats, newdata = carseats[test, ])
mean((yhat.bag - carseats$Sales[test])^2) # MSE = 2.066

importance(bag.carseats)

# Use random forest, calculate test MSE, use importance()
sales.grid <- 1:(ncol(carseats) - 1)
x.train <- carseats[train, -1]
x.test <- carseats[test, -1]
y.train <- carseats[train, 1]
y.test <- carseats[test, 1]
vecRF <- sapply(sales.grid, function(mtry) mean(randomForest(x.train, y.train, xtest = x.test,
                                                             ytest = y.test, mtry = mtry, ntree = 500)$test$mse))
plot(vecRF, type = "b", pch = 15)
points(which.min(vecRF), vecRF[which.min(vecRF)], col = "red", pch = 20, cex = 3)
vecRF[which.min(vecRF)] # MSE = 2.105
importance(randomForest(Sales ~ ., data = carseats, mtry = 8, ntree = 500, importance = T))

########################################################################################
# HW 8 Question 9
########################################################################################

set.seed(1)
library(ISLR)
oj <- data.frame(OJ)
str(oj)

train <- sample(nrow(oj), .8 * nrow(oj))
test <- -train

# Fit data to tree; Purchase as target
# Training error rate: 17.29%
# Terminal nodes: 8
tree.oj <- tree(Purchase ~ ., data = oj, subset = train)
summary(tree.oj)

# Look into terminal node
# For terminal node (7): criteria is LoyalCH > 0.76, 290 obs in that bin,
# deviance in bin is 99.94, classification in CH, 95.9% of obs are CH, remaining
# 4.1% of obs are MM
tree.oj

plot(tree.oj)
text(tree.oj, pretty = 0)

# Predict data, create table, what is test error rate?
oj.pred <- predict(tree.oj, newdata = oj[test, ], type = "class")
table(oj.pred, oj$Purchase[test])
(9 + 36) / (.2 * nrow(oj)) # Error rate = 21.02%

# Find optimal tree size using CV
cv.oj <- cv.tree(tree.oj, FUN = prune.misclass)
plot(cv.oj$size, cv.oj$dev, type = "b", pch = 15)
points(cv.oj$size[which.min(cv.oj$dev)], cv.oj$dev[which.min(cv.oj$dev)], col = "red",
       pch = 20, cex = 3)

# Produce pruned tree associated with lowest error rate
prune.oj <- prune.tree(tree.oj, best = 8)
yhat <- predict(prune.oj, newdata = oj[test, ], type = "class")
table(yhat, oj$Purchase[test])
(9 + 36)/(nrow(oj)*.2) # Error rate: 21.03%

summary(prune.oj)
summary(tree.oj)

########################################################################################
# HW 8 Question 10
########################################################################################

# Predict Salary
set.seed(1)
library(gbm)
hitters <- data.frame(Hitters)
sum(is.na(hitters))
hitters <- na.omit(hitters)
hitters$Salary <- log(hitters$Salary)

# Partition 200 to train, rest to test
train <- sample(1:263, 200)
test <- -train

# Boost on 1000 trees for range of shrinking parameters
lambda.grid <- 10^seq(-10, -0.2, by = 0.1)
vecTrainErr <- rep(NA, length(lambda.grid))
vecTestErr <- rep(NA, length(lambda.grid))

for (i in 1:length(lambda.grid)) {
    boost.hitters <- gbm(Salary ~ ., data = hitters[train, ], distribution = "gaussian",
                         n.trees = 1000, interaction.depth = 4, shrinkage = lambda.grid[i])
    train.pred <- predict(boost.hitters, hitters[train, ], n.trees = 1000)
    test.pred <- predict(boost.hitters, hitters[test, ], n.trees = 1000)
    vecTrainErr[i] <- mean((train.pred - hitters$Salary[train])^2)
    vecTestErr[i] <- mean((test.pred - hitters$Salary[test])^2)
}

min(vecTestErr) # MSE = .129

# Plot
plot(lambda.grid, vecTrainErr, type = "l", lwd = 2, col = "red")
lines(lambda.grid, vecTestErr, type = "l", lwd = 2, col = "blue")
legend("topright", c("Training Error", "Test Error") , col = c("red", "blue"), lty = 1, lwd = 2)

# Compare with linear regression
lm.hitters <- lm(Salary ~ ., data = hitters, subset = train)
pred.hitters <- predict(lm.hitters, hitters[test, ])
mean((pred.hitters - hitters$Salary[test])^2) # MSE = .394

# Compare with lasso regression
library(glmnet)
x <- model.matrix(Salary ~ ., data = hitters)[, -1]
y <- hitters$Salary

lasso.fit <- glmnet(x[train,], y[train], alpha = 1)
lasso.pred <- predict(lasso.fit, s = 0.01, newx = x[test,])
mean((lasso.pred - y[test])^2) # MSE = .361

# Most important variables?
best.boost <- gbm(Salary ~ ., data = hitters, distribution = "gaussian", n.trees = 1000,
                  interaction.depth = 4, shrinkage = lambda.grid[which.min(vecTestErr)])
summary(best.boost) # CAtBat, CHits, CRuns

# Compare with bagging
x.train <- hitters[train, -19]
x.test <- hitters[test, -19]
y.train <- hitters[train, 19]
y.test <- hitters[test, 19]
bag.hitters <- randomForest(x.train, y.train, xtest = x.test, ytest = y.test,
                            mtry = ncol(hitters)-1, ntree = 1000)
mean(bag.hitters$mse) # MSE = .217

########################################################################################
# HW 8 Question 11
########################################################################################

set.seed(1)
library(ISLR)
library(gbm)
caravan <- data.frame(Caravan)
caravan$Purchase <- ifelse(caravan$Purchase == "No", 0, 1)
train <- sample(nrow(caravan), 1000)
test <- -train

# Fit boost model w/ Purchase as response, find most important vars
boost.caravan <- gbm(Purchase ~ ., data = caravan[train, ], distribution = "bernoulli",
                     n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)
summary(boost.caravan) # PPERSAUT, PBRAND, MBERABG

# Predict on testing data
boost.pred <- predict(boost.caravan, caravan[test, ], n.trees = 1000, type = "response")
boost.pred <- ifelse(boost.pred > 0.2, "Yes", "No")
table(boost.pred, caravan$Purchase[test])
60 / (316 + 60) # 15.96% TP rate

# Compare with logistic regression
lm.caravan <- glm(Purchase ~ ., data = caravan, subset = train, family = "binomial")
summary(lm.caravan)
lm.pred <- predict(lm.caravan, caravan[test, ], type = "response")
lm.pred <- ifelse(lm.pred > 0.2, "Yes", "No")
table(lm.pred, caravan$Purchase[test])
66/ (396 + 66) # 14.29% TP rate

# Compare with k-nn
# Need to standardize variables
library(class)
standardized.x <- scale(caravan[, -ncol(caravan)])
knn.pred <- knn(standardized.x[train,], standardized.x[test,], caravan$Purchase[train],
                k=3)
table(knn.pred, caravan$Purchase[test])
22/ (101 + 22) # 17.89% TP rate

########################################################################################
# HW 8 Question 12
########################################################################################

# Analyze weekly stock data

set.seed(1)
library(ISLR)
weekly <- data.frame(Weekly)
train <- sample(nrow(weekly), .8*nrow(weekly))
test <- -train

# Bagging
library(randomForest)
ncol(weekly)
x.train <- weekly[train, -ncol(weekly)]
x.test <- weekly[test, -ncol(weekly)]
y.train <- weekly[train, ncol(weekly)]
y.test <- weekly[test, ncol(weekly)]

bag.weekly <- randomForest(Direction ~ . - Today, data = weekly, subset = train, mtry = 7,
                           n.tree = 1000, importance = T)
yhat.bag <- predict(bag.weekly, newdata = weekly[test, ])
table(yhat.bag, weekly[test, 9])
(35+75)/(.2 * nrow(weekly)) # Accuracy = 50.51%
importance(bag.weekly)

# Boosting
library(gbm)
lambda.grid <- 10^seq(-10, -0.2, by = 0.1)

weekly$Direction <- ifelse(weekly$Direction == "Down", 0, 1)

boosting <- function(x, partition) {
    boost.weekly <- gbm(Direction ~ . - Year - Today, data = weekly[partition, ], distribution = "bernoulli",
                        n.trees = 1000, shrinkage = x)
    yhat.boost <- predict(boost.weekly, newdata = weekly[-partition, ], n.trees = 1000)
    pred <- rep(0, length(yhat.boost))
    pred[yhat.boost > 0.5] <- 1
    return(mean(pred != weekly$Direction[-partition]))
}

bestLambda <- which.min(sapply(lambda.grid, function(x) boosting(x, train)))
boost.weekly <- gbm(Direction ~ . - Year - Today, data = weekly[train, ], distribution = "bernoulli",
                    n.trees = 1000, shrinkage = bestLambda)
yhat.boost <- predict(boost.weekly, newdata = weekly[test, ], n.trees = 1000)
pred <- rep(0, length(yhat.boost))
pred[yhat.boost > 0.5] <- 1
table(pred, weekly$Direction[test])
(65+42)/(nrow(weekly)*.2) # Accuracy = 49.13%

# Log Regression

glm.fit <- glm(Direction ~ . - Year - Today, data = weekly[train, ], family = "binomial")
glm.probs <- predict(glm.fit, weekly[test, ], type = "response")
glm.pred <- rep(0, length(glm.pred))
glm.pred[glm.probs > .5] <- 1
table(glm.pred, weekly$Direction[test])
(4 + 104)/(nrow(weekly)*.2) # Accuracy = 49.59%

