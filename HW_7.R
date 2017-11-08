########################################################################################
# HW 7 Question 6
########################################################################################
library(ISLR)
wage <- data.frame(Wage)

# Perform polynomial regression to predict wage using age
# Use cross validation to determine optimal degree
library(boot)
set.seed(1)
cv.error <- rep(0, 10)
for (i in 1:10) {
    poly.fit <- glm(wage ~ poly(age, i), data = wage)
    cv.error[i] <- cv.glm(wage, poly.fit)$delta[2]
}
which.min(cv.error)

# Make plot of the polynomial fit to data
plot(x = c(1:10), y = cv.error, pch = 15, type = "b")
title("Cross Validation")

# How does this compare to using ANOVA hypothesis testing?
fit.1 <- lm(wage ~ age, data = wage)
fit.2 <- lm(wage ~ poly(age, 2), data = wage)
fit.3 <- lm(wage ~ poly(age, 3), data = wage)
fit.4 <- lm(wage ~ poly(age, 4), data = wage)
fit.5 <- lm(wage ~ poly(age, 5), data = wage)
fit.6 <- lm(wage ~ poly(age, 6), data = wage)
fit.7 <- lm(wage ~ poly(age, 7), data = wage)
fit.8 <- lm(wage ~ poly(age, 8), data = wage)
fit.9 <- lm(wage ~ poly(age, 9), data = wage)
fit.10 <- lm(wage ~ poly(age, 10), data = wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

# Less complex is better, so use cubic polynomial
plot(wage ~ age, data = wage, col = "darkgrey")
age.grid <- seq(from = range(wage$age)[1], to = range(wage$age)[2])
preds <- predict(lm(wage ~ poly(age, 3), data = wage), newdata = list(age = age.grid), se = T)
se.bands <- cbind(preds$fit + 2 * preds$se, preds$fit - 2 * preds$se )
lines(age.grid, preds$fit, lwd = 3, col = "red")
matlines(age.grid, se.bands, lwd = 3, col = "blue", lty = 3)

# Fit step function
# Use cross validation to determine cuts
cv.error <- rep(0, 10)
for (i in 2:10) {
    wage$age.cut <- cut(wage$age, i)
    lm.fit <- glm(wage ~ age.cut, data = wage)
    cv.error[i] <- cv.glm(wage, lm.fit, K = 10)$delta[2]
}

# Make plot of the fit
plot(2:10, cv.error[-1], xlab = "Cuts", ylab = "CV.Error", type = "b", pch = 16, lwd = 2)

# Fit model and plot using 8 cuts
fit.step <- lm(wage ~ cut(age, 8), data = wage)
preds <- predict(fit.step, newdata = list(age = age.grid), se = T)
se.bands <- cbind(preds$fit + 2 * preds$se, preds$fit - 2 * preds$se)
plot(wage ~ age, data = wage, col = "darkgrey")
lines(age.grid, preds$fit, lwd= 3)
matlines(age.grid, se.bands, lwd = 3, col = "red", lty = 3)

########################################################################################
# HW 7 Question 7
########################################################################################

# Explore relationships between wage and other predictor variables
str(wage)
par(mfrow = c(2, 4))
plot(wage$age, wage$wage)
plot(wage$maritl, wage$wage)
plot(wage$race, wage$wage)
plot(wage$education, wage$wage)
plot(wage$region, wage$wage)
plot(wage$jobclass, wage$wage)
plot(wage$health, wage$wage)
plot(wage$health_ins, wage$wage)

# Interested in wage discrepency amongst ethnicities, linked to education?
# Do a 2 way ANOVA test with pairwise comparisons
# Do a linear model with interaction terms
# Deviance = a measure that plays the role of RSS for a broader class of models
# The deviance is negative two times the maximized log-likelihood
anova(lm(wage~race*education, data = wage))
pairwise.t.test(wage$wage, wage$race, p.adj = "none")
pairwise.t.test(wage$wage, wage$education, p.adj = "none")
summary(lm(wage ~ race + education, data = wage))
deviance(lm(wage ~ race + education, data = wage))

# Splines cannot be fitted on categorical data; no interpretation of taking exponent of a level
# Do GAM
summary(gam(wage ~ race + education + s(age, 4), data = wage))
deviance(gam(wage ~ race + education + s(age, 4), data = wage))

########################################################################################
# HW 7 Question 8
########################################################################################

# Explore Auto dataset
auto <- data.frame(Auto)
str(auto)
pairs(auto[, -ncol(auto)])

# Polynomial regression
cv.error <- rep(0, 10)
deviance <- rep(0, 10)
poly.models <- list()

par(mfrow = c(1,2))

for (i in 1:10) {
    poly.models[[i]] <- lm(mpg ~ poly(horsepower, i), data = auto)
    deviance[i] <- deviance(poly.models[[i]])
}
plot(deviance, pch = 15, type = "b")
anova(poly.models[[1]], poly.models[[2]], poly.models[[3]], poly.models[[4]])

for (i in 1:10) {
    poly.models[[i]] <- glm(mpg ~ poly(horsepower, i), data = auto)
    cv.error[i] <- cv.glm(auto, poly.models[[i]], K = 10)$delta[2]
}
plot(cv.error, pch = 15, type = "b")
which.min(cv.error)

# Step regression
cv.errors <- rep(NA, 10)
for (i in 2:10) {
    auto$cut <- cut(auto$horsepower, i)
    fit <- glm(mpg ~ cut, data = auto)
    cv.errors[i] <- cv.glm(auto, fit, K = 10)$delta[2]
}
which.min(cv.errors)
plot(cv.errors, pch = 15, type = "b", lwd = 3)

# Spline regression
library(splines)
cv.errors <- rep(NA, 10)
for (i in 3:10) {
    fit <- glm(mpg ~ ns(horsepower, df = i), data = auto)
    cv.errors[i] <- cv.glm(auto, fit, K = 10)$delta[2]
}
which.min(cv.errors)
plot(cv.errors, pch = 15, type = "b", lwd = 3)

# GAM
fit <- gam(mpg ~ s(displacement, 2) + s(horsepower,2) + s(acceleration,2), data = auto)
summary(fit)

########################################################################################
# HW 7 Question 9
########################################################################################

boston <- data.frame(Boston)
str(boston)

# Predict nox using dis via cubic regression model
# Plot data
poly.fit <- lm(nox ~ poly(dis, 3), data = boston)
summary(poly.fit)
plot(boston$dis, boston$nox, xlim = range(boston$dis), cex = 0.75, col = "darkgrey")

dis.grid <- seq(from = range(boston$dis)[1], to = range(boston$dis)[2])
pred <- predict(poly.fit, newdata = list(dis = dis.grid), se = T)

lines(dis.grid, pred$fit, col = "red", lwd = 3)

# Plot data at different polynomial degrees, report RSS for each
vecRSS <- rep(NA, 10)
par(mfrow = c(2, 5))

for (i in 1:10) {
    poly.fit <- lm(nox ~ poly(dis, i), data = boston)
    pred <- predict(poly.fit, newdata = list(dis = dis.grid), se = T)
    vecRSS[i] <- sum(poly.fit$residuals^2)
    plot(boston$dis, boston$nox, xlim = range(boston$dis), cex = 0.75, col = "darkgrey")
    lines(dis.grid, pred$fit, col = "red", lwd = 3)
}
which.min(vecRSS)

# Use CV to validate optimal degree of polynomial
vecCV <- rep(NA, 10)

for (i in 1:10) {
    poly.fit <- glm(nox ~ poly(dis, i), data = boston)
    vecCV[i] <- cv.glm(boston, poly.fit, K = 10)$delta[2]
}
plot(vecCV, pch = 15, type = "b")
which.min(vecCV)

# Fit a regression spline using 4 DF
library(splines)
spline.fit <- lm(nox ~ bs(dis, df = 4), data = boston)
attr(bs(boston$dis, df = 4), "knots")
summary(spline.fit)
plot(boston$dis, boston$nox, xlim = range(boston$dis), cex = 0.75, col = "darkgrey")
lines(dis.grid, predict(spline.fit, newdata = list(dis = dis.grid), se = T)$fit, col = "red", lwd = 3)

# Fit spline on range of DF
vecRSS <- rep(NA, 16)
for (i in 3:16) {
    spline.fit <- lm(nox ~ bs(dis, df = i), data = boston)
    vecRSS[i] <- sum(spline.fit$residuals^2)
}
vecRSS[-c(1,2)]
plot(3:16, vecRSS[-c(1,2)], pch = 15, type = "b")

# Perform CV to validate optimal DF
cv.error <- rep(NA, 16)
for (i in 3:16) {
    spline.fit <- glm(nox ~ bs(dis, df = i), data = boston)
    cv.error[i] <- cv.glm(boston, spline.fit, K = 10)$delta[2]
}
cv.error
plot(3:16, cv.error[-c(1,2)], pch = 15, type = "b")

########################################################################################
# HW 7 Question 10
########################################################################################

set.seed(1)
library(ISLR)
library(leaps)
college <- data.frame(College)
str(college)
train <- sample(nrow(college), .8*nrow(college))
test <- -train

# Perform forward stepwise regression to identify subset of variables
# out of state tuition as dependent variable
regfit.fwd <- regsubsets(Outstate ~ ., data = college, subset = train,
                         nvmax = ncol(college)-1, method = "forward")
reg.summary <- summary(regfit.fwd)

par(mfrow = c(1,3))
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adj R-Squared", pch = 15, type = "b")
which.max(reg.summary$adjr2)
points(13, reg.summary$adjr2[13], col = "red", pch = 20, cex = 3)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", pch = 15, type = "b")
which.min(reg.summary$cp)
points(13, reg.summary$cp[13], col = "red", pch = 20, cex = 3)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", pch = 15, type = "b")
which.min(reg.summary$bic)
points(10, reg.summary$bic[10], col = "red", pch = 20, cex = 3)

regfit.fwd <- regsubsets(Outstate ~ ., data = college, method = "forward")
subset.features <- coef(regfit.fwd, id = 6)

# Fit GAM using subset of features
library(gam)
gam.fit <- gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) + s(perc.alumni, df = 2)
               + s(Expend, df = 2) + s(Grad.Rate, df =2), data = college, subset = train)
par(mfrow = c(2,3))
plot(gam.fit, se= T, col = "blue")

# Evaluate on test data
# Non-parametric ANOVA test shows a strong evidence of non-linear relationships of
# Expend, Room.Board, and PhD; Less so for Grad.Rate
gam.pred <- predict(gam.fit, college[test, ])
gam.err <- mean((gam.pred - college$Outstate[test])^2)
gam.rss <- 1 - gam.err / mean((College$Outstate[test] - mean(College$Outstate[test]))^2)
summary(gam.fit)

########################################################################################
# HW 7 Question 11
########################################################################################

set.seed(1)
X1 <- rnorm(100)
X2 <- rnorm(100)
error <- rnorm(100)
Y <- 0.5 + 1.3 * X1 - 2.4 * X2 + error

beta0 <- rep(NA, 1000)
beta1 <- rep(NA, 1000)
beta2 <- rep(NA, 1000)

beta1[1] <- 3

# Y = B0 + B1X1 + B2X2 + error
# Y - B1X1 = B0 + B2X2 + error
# Y - B2X2 = B0 + B1X1 + error

for (i in 1:1000) {
    a <- Y - beta1[i] * X1
    beta2[i] <- lm(a ~ X2)$coef[2]

    a <- Y - beta2[i] * X2
    lm.fit <- lm(a ~ X1)
    if (i < 1000) {
        beta1[i + 1] <- lm.fit$coef[2]
    }
    beta0[i] <- lm.fit$coef[1]
}
plot(1:1000, beta0, xlab = "Number of Iterations", ylab = "Betas", type = "l", lwd = 3,
     col = "green", ylim = c(min(beta0, beta1, beta2), max(beta0, beta1, beta2)))
lines(1:1000, beta1, col = "red", lwd = 3)
lines(1:1000, beta2, col = "blue", lwd =3)
legend("topright", y= c("Beta0", "Beta1", "Beta2"), lty = 1, col = c("Green", "Red", "Blue"),
       cex = 0.5)

# Perform multiple linear regression
lm.fit <- lm(Y ~ X1 + X2)
abline(h = lm.fit$coefficients[1], lty = "dashed", col = "black")
abline(h = lm.fit$coefficients[2], lty = "dashed", col = "black")
abline(h = lm.fit$coefficients[3], lty = "dashed", col = "black")

# How many backfitting iterations to get to "good" approximation of multiple linear regression
# 1
which(round(beta1, 4) != round(lm.fit$coefficients[2], 4))

########################################################################################
# HW 7 Question 12
########################################################################################

# With p = 100, show that one can approximate the multiple linear regression coefficient
# estimates by repeatedly performing backfitting procedure.
# How many backfitting iterations are requiredto obtain a “good” approximation?

set.seed(1)
x <- matrix(ncol = 100, nrow = 1000)
coef <- rep(NA, 100)

for (i in 1:100) {
    x[, i] <- rnorm(1000)
    coef[i] <- rnorm(1) * 100
}

y <- x %*% coef + rnorm(1000)

beta <- rep(0, 100)
max_iterations = 1000
errors = rep(NA, max_iterations + 1)
iter = 2
errors[1] = Inf
errors[2] = sum((y - x %*% beta)^2)
threshold = 1e-04
while (iter < max_iterations && errors[iter - 1] - errors[iter] > threshold) {
    for (i in 1:100) {
        a = y - x %*% beta + beta[i] * x[, i]
        beta[i] = lm(a ~ x[, i])$coef[2]
    }
    iter = iter + 1
    errors[iter] = sum((y - x %*% beta)^2)
    print(c(iter - 2, errors[iter - 1], errors[iter]))
}

plot(1:11, errors[3:13])
