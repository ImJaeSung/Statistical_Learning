####################################
# Lab: Cross-Validation and the Bootstrap
###############################
## The Validation Set Approach

###
library(ISLR2)

set.seed(1)
## Split the data index
train <- sample(392, 196) # Out of 1 to 392, 196 random sampling
train
###
dim(Auto)
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
###
attach(Auto)
predict(lm.fit)
predict(lm.fit, Auto) # sorting by Auto index
mean((mpg - predict(lm.fit, Auto))[-train]^2)
###
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
###
set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

## Leave-One-Out Cross-Validation

###
glm.fit <- glm(mpg ~ horsepower, data = Auto) # family = 'binomial' not in glm:lm = glm 
coef(glm.fit)
###
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
###
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err
cv.err$delta # result for cross-validation
###
cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

cv.error
## $k$-Fold Cross-Validation
###
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

## The Bootstrap


### Estimating the Accuracy of a Statistic of Interest

###
alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}
###
Portfolio
alpha.fn(Portfolio, 1:100)

###
set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T)) # In 1~100, 100 observation sampling with replacement
###
boot(Portfolio, alpha.fn, R = 1000) # 1000 bootstrap estimate

### Estimating the Accuracy of a Linear Regression Model

###
boot.fn <- (data, index)
  coef(lm(mpg ~ horsepower, data = data, subset = index)) # don't need {}
boot.fn(Auto, 1:392)
nrow(Auto)
###
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))
###
boot(Auto, boot.fn, 1000) # 1000 bootstrap estimate
###
summary(lm(mpg ~ horsepower, data = Auto))$coef
###
boot.fn <- function(data, index)
  coef(
    lm(mpg ~ horsepower + I(horsepower^2), 
       data = data, subset = index)
  )
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(
  lm(mpg ~ horsepower + I(horsepower^2), data = Auto)
)$coef
###
