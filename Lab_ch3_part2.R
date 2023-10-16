rm(list = ls(all=TRUE))
library(MASS)
library(ISLR2)
############
## Diagnostics
################
attach(Boston)
lm.fit <- lm(medv ~ lstat, data = Boston)
summary(lm.fit)
names(lm.fit)

plot(Boston$lstat, Boston$medv)
abline(lm.fit, col=2, lwd=2)

predict(lm.fit)
residuals(lm.fit)
rstudent(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

### Leverage statistics
hatvalues(lm.fit)
plot(hatvalues(lm.fit))
# When complete, 
# a suite of functions that can be used to compute 
# some of the regression (leave-one-out deletion) diagnostics, for the VGLM class.
which.max(hatvalues(lm.fit))

#########################
## Multiple Linear Regression
##########################
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
###
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
summary_lm.fit = summary(lm.fit)
###
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)
summary_lm.fit1 = summary(lm.fit1)
###
lm.fit1_v2 <- update(lm.fit, ~ . - age)
summary(lm.fit1_v2)
summary_lm.fit1_v2  = summary(lm.fit1_v2)

### Are lm.fit1 and lm.fit1_v2 the same?
coef(summary_lm.fit1)
coef(summary_lm.fit1_v2)
coef(summary_lm.fit1) == coef(summary_lm.fit1_v2)
###
library(car)
vif(lm.fit)

#########
## Interaction Terms
##########
summary(lm(medv ~ lstat * age, data = Boston))

## Non-linear Transformations of the Predictors
########3
summary(lm(medv ~ lstat + lstat^2, data=Boston))
summary(lm(medv ~ .^2, data = Boston)) # ^2 is interaction term

lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data=Boston) # square term using I()
summary(lm.fit2)
lm.fit2_v2 <- lm(medv ~ poly(lstat,2), data=Boston) # lstat + I(lstat^2)
summary(lm.fit2_v2)
lm.fit2_v3 <- lm(medv ~ poly(lstat,2, raw=T), data=Boston) # orthogonal polynomial 
summary(lm.fit2_v3)
#########
par(mfrow = c(2, 2))
plot(lm.fit2)
###
lm.fit5 <- lm(medv ~ poly(lstat, 5), data=Boston)
summary(lm.fit5)
###
summary(lm(medv ~ log(rm), data = Boston))

## Qualitative Predictors
###
head(Carseats)
class(Carseats$Income)
class(Carseats$ShelveLoc)
###
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, 
             data = Carseats)
summary(lm.fit)
###
attach(Carseats)
contrasts(ShelveLoc)

## Writing  Functions

###
add
add()
###
add <- function(a,b) {
  val = a + b
  return(val)
}
###
add
###
add(1,2);
add(3,1)
###
