#####################################################
# LAB 3.6.1: Libraries
#####################################################

library(MASS)
library(ISLR)

#####################################################
# LAB 3.6.2: Simple Linear Regression
#####################################################

Boston <- data.frame(Boston)

# Fit linear model
lm.fit <- lm(formula = medv ~ lstat, data = Boston)

# Show summary statistics
summary(lm.fit)

# Show intercept & coefficient
names(lm.fit)
lm.fit$coefficients

# Show confidence interval for coefficient estimates
confint(lm.fit)

# Predict confidence and prediction intervals on new data
predict(lm.fit, data.frame(lstat = (c(5,10,15))), interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5,10,15))), interval = "prediction")

# Plot linear models 
attach(Boston)
plot(x = lstat, y = medv)
abline(lm.fit)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(x = lstat, y = medv, col = "red")
plot(x = lstat, y = medv, pch = 20)
plot(x = lstat, y= medv, pch = "+")
plot(x = 1:20, y = 1:20, pch = 1:20)

# Look at multiple plots at the same time
par(mfrow = c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#####################################################
# LAB 3.6.3 Multiple Linear Regression
#####################################################

lm.fit <- lm(formula = medv ~ lstat + age, data = Boston)
lm.fit <- lm(formula = medv ~ ., data = Boston)
summary(lm.fit)

library(car)
vif(lm.fit)

lm.fit1 <- lm(formula = medv ~ . - age, data = Boston)
summary(lm.fit1)

lm.fit1 <- update(lm.fit, ~. -age)

#####################################################
# LAB 3.6.4 Interaction Terms
#####################################################

# Simultaneously includes lstat, age, and the interaction term lstat×age as predictors
summary(lm(formula = medv ~ lstat * age, data = Boston))

#####################################################
# LAB 3.6.5 Non-linear Transformations of Predictors 
#####################################################

# The function I() is needed since the ^ has a special meaning I() in a formula
lm.fit2 <- lm(formula = medv ~ lstat + I(lstat^2))
summary(lm.fit2)

# Quantify to extent it is a better model 
# The null hypothesis is that the two models fit the data equally well
# The alternative hypothesis is that the full model is superior.
lm.fit <- lm(formula = medv ~ lstat)
anova(lm.fit, lm.fit2)

# Graph it
par(mfrow = c(2,2))
plot(lm.fit2)

lm.fit5 <- lm(formula = medv ~ poly(lstat, 5))
summary(lm.fit5)

summary(lm(formula = medv ~ log(rm), data = Boston))

#####################################################
# LAB 3.6.6 Qualitative Predictors
#####################################################

Carseats <- data.frame(Carseats)
names(Carseats)

# Given a qualitative variable such as Shelveloc, R generates dummy variables

lm.fit <- lm(formula = Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# contrasts() function returns coding for dummy variables
contrasts(Carseats$ShelveLoc)

#####################################################
# LAB 3.6.7 Writing Functions
#####################################################

LoadLibraries <- function() {
  library(ISLR)
  library(MASS)
  print("The Libraries have been loaded.")
}