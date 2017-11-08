########################################################################################
# LAB 8.3.1 Fitting Classification Trees
########################################################################################

library(ISLR)
library(tree)
attach(Carseats)
High <- ifelse(Sales <= 8, "No", "Yes")
carseats <- data.frame(Carseats, High)

tree.carseats <- tree(High ~ . - Sales, carseats)
summary(tree.carseats)

# text() displays node labels;
# pretty = 0 includes category names for qualititative vars
plot(tree.carseats)
text(tree.carseats, pretty = 0)

# Argument type="class" instructs R to return the actual class prediction
set.seed(1)
train <- sample(nrow(carseats), .8*nrow(carseats))
test <- -train
tree.carseats <- tree(High ~ . - Sales, carseats, subset = train)
tree.pred <- predict(tree.carseats, carseats[test, ], type = "class")
table(tree.pred, carseats$High[test])
(32 + 28)/(.2 * 400) # 75% accuracy

# Use cv.tree to do cross validation
# FUN=prune.misclass indicates use of classification error rate to guide the
# cross-validation and pruning process, rather than deviance (default metric)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)

# size = number of terminal nodes
# dev = cv error rate
# k = cost complexity parameter
cv.carseats

cv.carseats$size[which.min(cv.carseats$dev)]
par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# Use prune.misclass to get to optimal terminal node #
prune.carseats <- prune.misclass(tree.carseats, 18)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred <- predict(prune.carseats, carseats[test, ], type = "class")
table(tree.pred, carseats$High[test])
(43+18)/(.2*nrow(carseats)) # 76.25% accuracy

########################################################################################
# LAB 8.3.2 Fitting Regression Test
########################################################################################

# Fit decision tree to Boston data
set.seed(1)
library(MASS)
boston <- data.frame(Boston)
train <- sample(nrow(Boston), .8*nrow(Boston))
test <- -train

tree.boston <- tree(medv ~ ., boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0)

yhat <- predict(tree.boston, newdata = boston[test, ])
boston.test <- boston[test, "medv"]
plot(yhat, boston.test)
abline(0,1)
sqrt(mean((yhat - boston.test)^2)) # RMSE = 3.6

########################################################################################
# LAB 8.3.3 Bagging and Random Forests
########################################################################################

# Apply bagging and random forest to Boston data
set.seed(1)
library(randomForest)

# mtry means how many predictors should be considered at each split
bag.boston <- randomForest(medv ~ ., data = boston, subset = train, mtry = 13,
                           importance = T)
bag.boston

yhat.bag <- predict(bag.boston, newdata = boston[test, ])
plot(yhat.bag, boston.test)
abline(0,1)
sqrt(mean((yhat.bag - boston.test)^2)) # RMSE = 2.896

# ntree specifies how many trees to be created in ensemble
bag.boston <- randomForest(medv ~ ., data = boston, subset = train, mtry = 13,
                           ntree = 25, importance = T)
yhat.bag <- predict(bag.boston, newdata = boston[test, ])
plot(yhat.bag, boston.test)
abline(0, 1)
sqrt(mean((yhat.bag - boston.test)^2)) # RMSE = 2.975

# Do random forest rather than bagging now...
rf.boston <- randomForest(medv ~ ., data = boston, subset = train, mtry = 6,
                          importance = T)
yhat.rf <- predict(rf.boston, newdata = boston[test, ])
plot(yhat.rf, boston.test)
abline(0, 1)
sqrt(mean((yhat.rf - boston.test)^2)) # RMSE = 2.849

# importance() shows importance of the variables in forest
# %IncMSE shows mean decrease in accuracy on OOB samples when given var is excluded
# IncNodePurity shows total decrease in node purity that results from splits over
#   that var, averaged over all trees [RSS for regression trees, deviance for classification]
# varImpPlot visualizes these two categories
importance(rf.boston)
varImpPlot(rf.boston)

########################################################################################
# LAB 8.3.4 Boosting
########################################################################################

# Fit gradient boosted model on Boston data
# distribution = "gaussian" because regression
# would've done distribution = "bernoulli" if binary classification problem
set.seed(1)
library(gbm)
boost.boston <- gbm(medv ~ ., data = boston[train, ], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

# Plot partial dependence plots
# Shows marginal effect of selected variables after integrating out the other variables
par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

# Predict values
yhat.boost <- predict(boost.boston, newdata = boston[test, ], n.trees = 5000)
sqrt(mean((yhat.boost - boston$medv[test])^2)) # RMSE = 2.876

# Use different shrinking penalty term (lambda)
boost.boston <- gbm(medv ~ ., data = boston[train, ], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.boston, newdata = boston[test, ], n.trees = 5000)
sqrt(mean((yhat.boost - boston$medv[test])^2)) # RMSE = 2.901
