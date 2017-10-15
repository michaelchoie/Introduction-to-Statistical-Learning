#####################################################
# LAB 4.6.1: Stock Market Data
#####################################################

# Load library & data
library(ISLR)
Smarket <- data.frame(Smarket)

# View structure
str(Smarket)
summary(Smarket)

# Create correlation matrix (appears to be little collinearity)
# Meaning, no relation between previous and current day's returns
cor(Smarket[, -9])

# Plot data of biggest correlation (volume and time)
par(mfrow = c(1,2))
plot(Smarket$Volume)
plot(Smarket$Year, Smarket$Volume)

#####################################################
# LAB 4.6.2: Logistic Regression
#####################################################

# Run logistic regression to predict direction
glm.fit <- glm(formula = Direction ~ . - Year - Today, data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef

# Get p-values
summary(glm.fit)$coef[, 4]

# type="response" option tells R to output probabilities of the form P(Y = 1|X)
# as opposed to other information such as the logit.
# If no data set is supplied to the predict() function
# then probabilities are computed for the training data that was used to fit the logistic regression
# values corresponds with prob of market going up, as seen with contrasts()
glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]
contrasts(Smarket$Direction)

# In order to make a prediction as to whether the market will go up or down on a particular day
# we must convert these predicted probabilities into class labels, Up or Down.
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > 0.5] <- "Up"

# table()  can be used to produce a confusion matrix 
# in order to determine how many observations were correctly or incorrectly classified.
# Accuracy rate is 52.16%
table(glm.pred, Smarket$Direction)
mean(glm.pred == Smarket$Direction)

Smarket.2005 <- Smarket[!Smarket$Year < 2005, ]
dim(Smarket.2005)
Direction.2005 <- Smarket$Direction[!Smarket$Year < 2005]

# Fit logistic regression on the subset of data
train <- (Smarket$Year < 2005)
glm.fit <- glm(formula = Direction ~ . - Year - Today, data = Smarket, family = "binomial", 
               subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Smarket.2005$Direction)
mean(glm.pred == Smarket.2005$Direction)

glm.fit <- glm(formula = Direction ~ Lag1 + Lag2, data = Smarket, family = "binomial", subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Smarket.2005$Direction)
mean(glm.pred == Smarket.2005$Direction)
predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")

#####################################################
# LAB 4.6.3: Linear Discriminant Analysis
#####################################################

library(MASS)
lda.fit <- lda(formula = Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)

# If −0.642 × Lag1 − 0.514 × Lag2 is large, then the LDA classifier will predict a market increase, 
# and if it is small, then the LDA classifier will predict a market decline.

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[, 1] >= 0.5)
sum(lda.pred$posterior[, 1] < 0.5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]
sum(lda.pred$posterior[, 1] > 0.9)

#####################################################
# LAB 4.6.4: Quadratic Discriminant Analysis
#####################################################

qda.fit <- qda(formula = Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Smarket.2005$Direction)
mean(qda.class == Smarket.2005$Direction)

#####################################################
# LAB 4.6.5: k Nearest Neighbors 
#####################################################

# Rather than a two-step approach in which we first fit the model 
# and then we use the model to make predictions,
# knn() forms predictions using a single command
# cbind binds together columns into single matrix
attach(Smarket)
library(class)
train.X <- cbind(Lag1, Lag2)[train, ] 
test.X <- cbind(Lag1, Lag2 )[!train, ]
train.Direction <- Direction[train]

# A seed must be set in order to ensure reproducibility of results
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Smarket.2005$Direction)
mean(knn.pred == Smarket.2005$Direction)

# Try with 3 neighbors instead
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Smarket.2005$Direction)
mean(knn.pred == Smarket.2005$Direction)

#####################################################
# LAB 4.6.6: Caravan Insurance Data
#####################################################

# Need to standardize data so that distane metrics work well
# Means mean = 0 and standard deviation = 1
# scale() does exactly that

attach(Caravan)
summary(Purchase)

standardized.X <- scale(Caravan[, -86]) # exclude factor variable
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])

# Evaluate performance on test data
test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)
mean(knn.pred != test.Y)
mean(test.Y != "No")

table(knn.pred, test.Y)
mean(knn.pred == test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)

glm.fit <- glm(formula = Purchase ~ ., data = Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fit, Caravan[test, ], type="response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs >.5] <- "Yes"
table(glm.pred, test.Y)
glm.pred <- rep("No",1000)
glm.pred[glm.probs > 0.25] <- " Yes"
table(glm.pred, test.Y)
