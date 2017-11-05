########################################################################################
# LAB 7.8.1 Polynomial Regression and Step Functions
########################################################################################

library(ISLR)
wage <- data.frame(Wage)

# The function returns a matrix whose columns are a basis of orthogonal polynomials
# Means that each column is a linear orthogonal combination of the variables age, age^2, etc.
fit <- lm(wage ~ poly(age, 4), data = wage)
coef(summary(fit))

# Obtain coefficients for variables directly with raw = TRUE
fit2 <- lm(wage ~ poly(age, 4, raw = TRUE), data = wage)
coef(summary(fit2))

# Create polynomial basis functions on the fly
# I() is a wrapper function, as "^" symbol has special meaning in functions
# 2b a more compact version of 2a
# Any function call inside a formula serves as a wrapper
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = wage)
coef(fit2a)
fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data = wage)
coef(fit2b)

agelims <- range(wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)

# mar and oma control margins of plot
par(mfrow = c(2,2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(wage$age, wage$wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

preds2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)
max(abs(preds$fit - preds2$fit))

# Decide on degree of polynomial using ANOVA
# Test if model 1 is enough to explain data or a more complex model 2 is necessary
# Must use nested models (model 1 is subset of model 2)

fit.1 <- lm(wage ~ age, data = wage)
fit.2 <- lm(wage ~ poly(age,2), data = wage)
fit.3 <- lm(wage ~ poly(age,3), data = wage)
fit.4 <- lm(wage ~ poly(age,4), data = wage)
fit.5 <- lm(wage ~ poly(age,5), data = wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# Can get p values more succintly using coef (cuz poly() makes orthogonal polynomials)
coef(summary(fit.5))

# ANOVA works whether or not orthogonal polynomials are used; also when other terms are included
fit.1 <- lm(wage ~ education + age, data = wage)
fit.2 <- lm(wage ~ education + poly(age,2), data = wage)
fit.3 <- lm(wage ~ education + poly(age,3), data = wage)
anova(fit.1, fit.2, fit.3)
# But instead of using hypothesis tests/ANOVA, can just use polynomial terms and cross validation

# Predict if individual makes 250k a year
# Use I() to create binomial variable on the fly
# glm() converts that created boolean var to 1 or 0
# glm() uses default of type = "link", meaning we get predictions for logit
fit <- glm(I(wage > 250) ~ poly(age, 4), data = wage, family = "binomial")
preds <- predict(fit, newdata = list(age = age.grid), se = T)
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

# Directly predict probabilities, but wrong because confidence intervals
# make us end up with negative probabilities
# preds <- predict(fit, newdata = list(age = age.grid), type = "response", se = T)

# Plot logistic regression
# jitter() scatters dots a bit soobseratins with same age don't cover each other up
# called a rug plot
plot(wage$age, I(wage$wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(wage$age), I((wage$wage >250)/5), cex = 0.5, pch = "l", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Fit a step function using cut() - automatically chooses cut off points
# Can specify cut off points using breaks option
# cut() returns ordinal variable, and lm() creates dummy vars for regression
table(cut(wage$age, 4))
fit <- lm(wage ~ cut(age, 4), data = wage)
coef(summary(fit))

########################################################################################
# LAB 7.8.2 Splines
########################################################################################

# Must create matrix of basis functions
# bs() creates this matrix with specified set of knots
# by default, bs() creates cubic splines
# has spline with 6 basis functions (3 knots, 7 degrees of freedom: intercept + basis functions)
library(splines)
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(wage$age, wage$wage, col = "grey")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2*pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2*pred$se, lty = "dashed")

# use df option to create uniform splines
# bs() has degree argument, used to create something other than cubic spline
dim(bs(wage$age, knots = c(25, 40, 60)))
dim(bs(wage$age, df = 6))
attr(bs(wage$age, df=6), "knots")

# Fit natural spline using ns()
fit2 <- lm(wage ~ ns(age, df = 4), data = wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

# Fit smoothing spline using smooth.spline()
plot(wage$age, wage$wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(wage$age, wage$wage, df = 16)
fit2 <- smooth.spline(wage$age, wage$wage, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"),
       lty = 1, lwd = 2, cex = 0.8)

# Fit local regression using loess()
# Span in this case means 20% or 50% of population in each neighborhood
# Larger span = smoother fit
plot(wage$age, wage$wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = 0.2, data = wage)
fit2 <- loess(wage ~ age, span = 0.5, data = wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"),
       lty = 1, lwd = 2, cex = 0.8)

########################################################################################
# LAB 7.8.3 GAM's
########################################################################################

# Fit GAM using natural splines
# GAM is a linear regression with appropriate basis functions
# Must use gam library to fit with smoothing splines
# s() is smoothing spline function - indicate degrees of freedom
# use plot.gam when we fitted model using lm. plot can recognize gam fitted models
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = wage)
library(gam)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = wage)
par(mfrow = c(1,3))
plot(gam.m3, se = TRUE, col = "blue")
plot.gam(gam1, se = TRUE, col = "red")

# Use ANOVA to determine which model is best: GAM w/o year, linear function of year
# or spline function of year
# Based on test, model 2 is best
gam.m1 <- gam(wage ~ s(age, 5) + education, data = wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")

summary(gam.m3)

# Use lo() to do local regression within GAM
preds <- predict(gam.m2, newdata = wage)
gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = wage)
plot.gam(gam.lo, se = TRUE, col = "green")

# Can also use lo() to create interactions before calling GAM
# Plot 2-D surface using akima
gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = wage)
library(akima)
plot(gam.lo.i)

# Plot logistic regression GAM
gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = "binomial",
              data = wage)
par(mfrow = c(1,3))
plot(gam.lr, se = T, col = "green")

table(wage$education, I(wage$wage > 250))

gam.lr.s <- gam(I(wage > 250) ~ year + s(age, df= 5) + education, family = "binomial",
                data = wage, subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = T, col = "green")

