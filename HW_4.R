#####################################################
# HW 4 Question 10 
#####################################################

library(ISLR)
Weekly <- data.frame(Weekly)
str(Weekly)
pairs(Weekly)
cor(Weekly[, -9])
# Only seems to be a pattern for Year and Volume

glm.fit <- glm(formula = Direction ~ . - Year - Today, data = Weekly, 
               family = "binomial")
summary(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep("Down", 1089)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Weekly$Direction)
mean(glm.pred == Weekly$Direction)

# Fit log model on only subset of data using one predictor

glm.fit2 <- glm(formula = Direction ~ Lag2, data = Weekly[Weekly$Year <= 2008, ],
                family = "binomial")
summary(glm.fit2)
glm.probs2 <- predict(glm.fit2, Weekly[Weekly$Year > 2008, ], type = "response")
glm.pred2 <- rep("Down", length(glm.probs2))
glm.pred2[glm.probs2 > 0.5] <- "Up"
table(glm.pred2, Weekly$Direction[Weekly$Year > 2008])
mean(glm.pred2 == Weekly$Direction[Weekly$Year > 2008])

# Fit LDA on same subset of data

glm.lda <- lda(formula = Direction ~ Lag2, data = Weekly[Weekly$Year <= 2008,])
lda.pred <- predict(glm.lda, Weekly[Weekly$Year > 2008, ])
plot(glm.lda)
lda.class <- lda.pred$class
table(lda.class, Weekly$Direction[Weekly$Year > 2008])
mean(lda.class == Weekly$Direction[Weekly$Year > 2008])
# Accuracy did go up

# Fit QDA on same subset of data

attach(Weekly)
train = (Year < 2009)
Direction.0910 = Direction[!train]
Weekly.0910 = Weekly[!train, ]
glm.qda <- qda(formula = Direction ~ Lag2, data = Weekly, subset = train)
qda.pred <- predict(glm.qda, Weekly.0910)$class
table(qda.pred, Direction.0910)
mean(qda.pred == Direction.0910)
# Accuracy still 58.6 even though it only chose one classification

# Fit KNN on same subset of data
# Need to standardize data

library(class)
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Direction = Direction[train]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)

mean1 <- mean(lda.class == Weekly$Direction[Weekly$Year > 2008])
mean2 <- mean(qda.pred == Direction.0910)
mean3 <- mean(knn.pred == Direction.0910)
mean.all <- data.frame(c("lda","qda","knn"),c(mean1, mean2, mean3))
plot(mean.all)

#####################################################
# HW 4 Question 11
#####################################################

# Create Auto dataframe with binary column
Auto <- data.frame(Auto[1:9])
mpg01 <- ifelse(Auto$mpg >= median(Auto$mpg), 1, 0)
Auto <- cbind(Auto, mpg01)

# Check variable relationships
cor(Auto[, -9])
pairs(Auto)

# Partition data into test and training set
sample <- sample.int(n = nrow(Auto), size = floor(.75*nrow(Auto)), replace = F)
train <- Auto[sample, ]
test <- Auto[-sample, ]

# Perform LDA
# cylinders, displacement, horsepower, weight, origin
auto.lda <- lda(formula = mpg01 ~ cylinders + displacement + horsepower + weight + origin,
                data = train)

lda.predict <- predict(auto.lda, test)$class
table(lda.predict, test$mpg01)
mean(lda.predict == test$mpg01)

# Perform QDA
auto.qda <- qda(formula = mpg01 ~ cylinders + displacement + horsepower + weight + origin,
                data = train)
qda.predict <- predict(auto.qda, test)$class
table(qda.predict, test$mpg01)
mean(qda.predict == test$mpg01)

# Perform logistic regression
auto.log <- glm(formula = mpg01 ~ cylinders + displacement + horsepower + weight + origin,
                data = train, family = "binomial")
log.probs <- predict(auto.log, test, type = "response")
log.predict <- ifelse(log.probs > .5, 1, 0)
table(log.predict, test$mpg01)
mean(log.predict == test$mpg01)

# Perform KNN 
library(class)
train.X <- train[, c("cylinders", "weight", "displacement", "horsepower")]
test.X <- test[, c("cylinders", "weight", "displacement", "horsepower")]
train.mpg01 <- train$mpg01
set.seed(1)
knn.pred <- knn(train.X, test.X, train.mpg01, k = 1)
mean(knn.pred != test$mpg01)

knn.pred <- knn(train.X, test.X, train.mpg01, k = 10)
mean(knn.pred != test$mpg01)

knn.pred <- knn(train.X, test.X, train.mpg01, k = 100)
mean(knn.pred != test$mpg01)

#####################################################
# HW 4 Question 12
#####################################################

Power <- function() {
  2^3
}
print(Power())

Power2 <- function(x,a) { 
  x^a
}
print(Power2(3,8))
print(Power2(10,3))
print(Power2(8,17))
print(Power2(131,3))

Power3 <- function(x,a) {
  power <- x^a
  return(power)
}

x <- 1:10
plot(x, Power3(x, 2), log = "xy", xlab = "Log x", ylab = "Log x^2", 
     main = "Log x^2 vs. Log x")

PlotPower = function(x, a) {
  plot(x, Power3(x, a))
}

PlotPower(1:10, 3)

#####################################################
# HW 4 Question 13
#####################################################

Boston <- data.frame(Boston)
# Predict whether a given suburb has a crime rate above or below the median. 

# Check out dataset relationships
str(Boston)
cor(Boston)
pairs(Boston)

# Create a binary classification variable for crime
crime01 <- ifelse(Boston$crim <= median(Boston$crim), 0, 1)
Boston <- cbind(Boston, crime01)

# Partition the dataset

train <- sample.int(n = nrow(Boston), size = floor(.75*nrow(Boston)), 
                    replace = F)
train.Boston <- Boston[train, ]
test.Boston <- Boston[-train, ]

# Use subset of data that is most correlated with crime01
# indus, nox, age, dis, rad, tax
cor(Boston)

# Logistic Regression
log.crim <- glm(formula = crime01 ~ indus + nox + age + dis + rad + tax, 
                data = train.Boston, family = "binomial")
summary(log.crim)

log.probs <- predict(log.crim, test.Boston, type = "response")
log.predict <- ifelse(log.probs < .5, 0, 1)
table(log.predict, test.Boston$crime01)
mean(log.predict == test.Boston$crime01)

par(mfrow=c(2,2))
plot(log.crim)

# LDA
lda.crim <- lda(formula = crime01 ~ indus + nox + age + dis + rad + tax, 
                data = train.Boston)
lda.predict <- predict(lda.crim, test.Boston)$class
table(lda.predict, test.Boston$crime01)
mean(lda.predict == test.Boston$crime01)

# QDA
qda.crim <- qda(formula = crime01 ~ indus + nox + age + dis + rad + tax,
                 data = train.Boston)
qda.predict <- predict(qda.crim, test.Boston)$class
table(qda.predict, test.Boston$crime01)
mean(qda.predict == test.Boston$crime01)

# KNN
library(class)
attach(Boston)
train.X <- cbind(indus, nox, age, dis, rad, tax)[train, ]
test.X <- cbind(indus, nox, age, dis, rad, tax)[-train, ]
train.crim <- Boston$crime01[train]
knn.crim <- knn(train.X, test.X, train.crim, k=5)
table(knn.crim, test.Boston$crime01)  
mean(knn.crim == test.Boston$crime01)

knn.crim <- knn(train.X, test.X, train.crim, k=10)
table(knn.crim, test.Boston$crime01)  
mean(knn.crim == test.Boston$crime01)

knn.crim <- knn(train.X, test.X, train.crim, k=3)
table(knn.crim, test.Boston$crime01)  
mean(knn.crim == test.Boston$crime01)
