#####################
## Chapter 3
####################
## Libraries
## What are libraries?
library(MASS)
library(ISLR2)
install.packages("ISLR2")

###########################
## Simple Linear Regression
##########################
###
head(Boston)
### Let's take a closer look into the dataset
attach(Boston)
###
lm.fit <- lm(medv ~ lstat)
###
lm.fit <- lm(medv ~ lstat, data = Boston)

lm.fit <- lm(medv ~ lstat)
###
lm.fit
summary(lm.fit)
###
names(lm.fit)

coef(lm.fit)
###
confint(lm.fit)
###
### Returns the "confidence interval".
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "confidence")
### Returns the "prediction interval"
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction")
### What is the difference between "confidence" and "prediction" intervals?

###
plot(lstat, medv)
abline(lm.fit)
###
abline(lm.fit, lwd = 5) # line width
abline(lm.fit, lwd = 3, col = "red")  # line color
plot(lstat, medv, col = "red") # point color
plot(lstat, medv, pch = 20)  # plotting symbol
plot(lstat, medv, pch = "+")

plot(1:20, 1:20, pch = 1:20) # different 20 types symbol

par(mfrow = c(2, 2))
plot(lm.fit)
par(mfrow = c(3, 3))
plot(lm.fit)

## Multiple Linear Regression

###
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
###
lm.fit <- lm(medv ~ ., data = Boston) # using all variable
summary(lm.fit)
summary_lm.fit = summary(lm.fit)
###
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)
###
lm.fit1_v2 <- update(lm.fit, ~ . - age) # except age variable
summary(lm.fit1_v2)
