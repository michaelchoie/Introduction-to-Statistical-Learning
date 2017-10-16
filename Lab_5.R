#####################################################
# LAB 5.3.1 Validation Set Approach
#####################################################

library(ISLR)
set.seed(1)
train <- sample(392, 196)

attach(Auto)
lm.fit <- lm(formula = mpg ~ horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(formula = mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(formula = mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train <- sample(392, 196)

attach(Auto)
lm.fit <- lm(formula = mpg ~ horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(formula = mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(formula = mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#####################################################
# LAB 5.3.2 LOOCV
#####################################################

library(boot)
glm.fit <- glm(formula = mpg ~ horsepower, data = Auto)
coef(glm.fit)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

cv.error <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(formula = mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

#####################################################
# LAB 5.3.3 K-Fold Cross Validation
#####################################################

set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(formula = mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1] 
}
cv.error.10

#####################################################
# LAB 5.3.4 Bootstrap
#####################################################

Portfolio <- data.frame(Portfolio)

# Estimate accuracy of statistic of interest

alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))
boot(Portfolio, alpha.fn, R = 1000)

# Estimate accuracy of linear model

boot.fn <- function(data, index) {
  return(coef(lm(formula = mpg ~ horsepower, data = data, subset = index)))
}
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower ,data=Auto))$coef

# There is a difference between bootstrap coefficients and linear model coefficients
# This is because the bootstrap does not rely on same assumptions as lm
# In particular, reliance of variance estimate, and that the predictors are fixed
# and all variance is attributed to variance of irreducible error
# Therefore, it gives more accurate estimates of standard errors

boot.fn <- function(data, index) {
  return(coef(lm(formula = mpg ~ I(horsepower^2), data = data, subset = index)))
}
set.seed(1)
boot(Auto, boot.fn, 1000)

summary(lm(mpg~I(horsepower^2) ,data=Auto))$coef
