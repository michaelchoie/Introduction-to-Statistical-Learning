#####################################################
# HW 5 Question 5
#####################################################

# Fit logistic regression to predict probability of default
library(ISLR)
default <- data.frame(Default)

set.seed(1)
train <- sample.int(n = nrow(default), size = floor(.75*nrow(default)))
log.fit <- glm(formula = default ~ income + balance, data = default, family = "binomial", 
               subset = train)
log.probs <- predict(log.fit, default[-train, ], type = "response")
log.predict <- ifelse(log.probs <=0.5, "No", "Yes")
table(log.predict, default$default[-train])
mean(log.predict == default$default[-train])

# Fit more logistic regressions using different splits

log.Split <- function(x) { 
  train <- sample.int(n = nrow(default), size = floor(x*nrow(default)))
  log.fit <- glm(formula = default ~ income + balance, data = default, family = "binomial", 
                 subset = train)
  log.probs <- predict(log.fit, default[-train, ], type = "response")
  log.predict <- ifelse(log.probs <=0.5, "No", "Yes")
  return(mean(log.predict == default$default[-train]))
}
log.Split(.8)
log.Split(.9)
log.Split(.95)

# It appears they all have around 2.3% error rate

# Fit logistic regression using dummy var for student

# default$student <- ifelse(default$student == "No", 0, 1)
log.fit <- glm(formula = default ~ income + balance + student, data = default, family = "binomial",
               subset = train)
log.pred <- predict(log.fit, default[-train,], type = "response")
log.probs <- ifelse(log.pred <= .5, "No", "Yes")
mean(log.probs == default$default)

# It lead to a reduction in accuracy

#####################################################
# HW 5 Question 6
#####################################################

set.seed(1)

# Use summary() and glm() to calculate standard errors for coefficients
log.fit <- glm(formula = default ~ income + balance, data = default, family = "binomial")
summary(log.fit)

# Use bootstrap to calculate standard errors
boot.fn <- function(input, index) {
  return(coef(glm(formula = default ~ income + balance, data = input, family = "binomial",
                 subset = index)))
}
library(boot)
boot(default, boot.fn, 1000)
# We obtain similar coefficients as with the glm()

#####################################################
# HW 5 Question 7
#####################################################

# Performing LOOCV using glm(), predict.glm(), and a for loop
library(ISLR)
weekly <- data.frame(Weekly)

log.fit1 <- glm(formula = Direction ~ Lag1 + Lag2, data = weekly, family = "binomial") 
log.fit2 <- glm(formula = Direction ~ Lag1 + Lag2, data = weekly[2:nrow(weekly), ], 
                family = "binomial")

ifelse(predict(log.fit2, weekly[1, ], type = "response") > 0.5, "Up", "Down")
weekly[1, ]
# Predicted Up but was actually Down

sum <- rep(0, nrow(weekly))
for (i in 1:nrow(weekly)) { 
  log.fit <- glm(formula = Direction ~ Lag1 + Lag2, data = weekly[-i, ], family = "binomial")
  sum[i] <- ifelse(ifelse(predict(log.fit, weekly[i, ], type = "response") > 0.5, "Up", "Down") 
            == weekly$Direction[i], 1, 0)
}
print(sum(sum))
print(mean(sum))
# There were 599 predictive errors, translating to a 55% accuracy rate

#####################################################
# HW 5 Question 8
#####################################################

# Perform cross validation
set.seed(1)
x <- rnorm(100)
y <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)
# n = 100; p = 2; y = x - 2x^2 + error term

plot(x,y)
# quadratic plot

library(boot)
data <- data.frame(x,y)
glm.fit <- glm(formula = y ~ x) 
cv.glm(data, glm.fit)$delta  

glm.fit <- glm(formula = y ~ poly(x,2)) 
cv.glm(data, glm.fit)$delta  

glm.fit <- glm(formula = y ~ poly(x,3)) 
cv.glm(data, glm.fit)$delta  

glm.fit <- glm(formula = y ~ poly(x,4)) 
cv.glm(data, glm.fit)$delta  

set.seed(100)
glm.fit <- glm(formula = y ~ x) 
cv.glm(data, glm.fit)$delta  

glm.fit <- glm(formula = y ~ poly(x,2)) 
cv.glm(data, glm.fit)$delta  

glm.fit <- glm(formula = y ~ poly(x,3)) 
cv.glm(data, glm.fit)$delta  

glm.fit <- glm(formula = y ~ poly(x,4)) 
cv.glm(data, glm.fit)$delta  

# the results are exactly the same, because LOOCV performs on a fold of a single observation
# for every observation, therefore, there isn't any variance in the results 
# the third power polynomial had the least error - wasn't what I expected due to the 
# shape of the scatterplot of x and y

summary(glm.fit)
# I agree with the results here as only the polynomials of the linear and quadratic terms are 
# extremely significant - albeit the third power is too, it is less so than the former two

#####################################################
# HW 5 Question 9
#####################################################

library(MASS)
set.seed(1)
boston <- data.frame(Boston)
medv.mean <- mean(boston$medv)
medv.err <- sd(boston$medv)/sqrt(length(boston$medv))

# Bootstrap method to calculate standard error
library(boot)
bstrap <- boot(boston$medv, function(data, index) return(mean(data[index])), 100)
# It generates the same mean but a lower standard error

# Create 95% confidence interval
t.test(boston$medv)
c(bstrap$t0 - 2*0.453947,bstrap$t0 + 2*0.453947)
# Bootstrap method creates a slightly large confidence interval

# Estimate median value of mean
medv.median <- median(boston$medv)

# Bootstrap method to calculate standard error
bstrap <- boot(boston$medv, function(data, index) return(median(data[index])), 100)
bstrap
# Same median with a small std error

# Find estimate of 10th percentile of medv in data 
medv10 <- quantile(boston$medv, 0.1)
medv10

# Bootstrap method to calculate standad error of 10th percentile 
bstrap <- boot(boston$medv, function(data, index) return(quantile(data[index], 0.1)), 100)
bstrap
