# chap5-8
####(a)####
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x-2*x^2+rnorm(100)

length(y) #n =100

####(b)#####
plot(x, y)

####(c)####
data = data.frame(x, y)
set.seed(1)

library(boot)
#i
glm.fit <- glm(y~x)
cv.glm(data, glm.fit)$delta

#ii
glm.fit.2 <- glm(y~poly(x, 2))
cv.glm(data, glm.fit.2)$delta

#iii
glm.fit.3 <- glm(y~poly(x, 3))
cv.glm(data, glm.fit.3)$delta

#iiii
glm.fit.4 <- glm(y~poly(x, 4))
cv.glm(data, glm.fit.4)$delta

set.seed(1)
for (i in 1:4){
  glm.fit <- glm(y~poly(x, i), data = data)
  cv.error[i] <- cv.glm(data, glm.fit)$delta[1]
}

cv.error

####(d)####
set.seed(42)

#i
glm.fit <- glm(y~x)
cv.glm(data, glm.fit)$delta

#ii
glm.fit.2 <- glm(y~poly(x, 2))
cv.glm(data, glm.fit.2)$delta

#iii
glm.fit.3 <- glm(y~poly(x, 3))
cv.glm(data, glm.fit.3)$delta

#iiii
glm.fit.4 <- glm(y~poly(x, 4))
cv.glm(data, glm.fit.4)$delta
cv.error <- rep(0, 4)

set.seed(42)
for (i in 1:4){
  glm.fit <- glm(y~poly(x, i), data = data)
  cv.error[i] <- cv.glm(data, glm.fit)$delta[1]
}

cv.error

set.seed(30)

#i
glm.fit <- glm(y~x, data = data)
cv.glm(data, glm.fit)$delta

#ii
glm.fit.2 <- glm(y~poly(x, 2), data = data)
cv.glm(data, glm.fit.2)$delta

#iii
glm.fit.3 <- glm(y~poly(x, 3), data = data)
cv.glm(data, glm.fit.3)$delta

#iiii
glm.fit.4 <- glm(y~poly(x, 4), data = data)
cv.glm(data, glm.fit.4)$delta

set.seed(30)
for (i in 1:4){
  glm.fit <- glm(y~poly(x, i), data = data)
  cv.error[i] <- cv.glm(data, glm.fit)$delta[1]
}

cv.error

####(f)####


summary(glm.fit)
summary(glm.fit.2)
summary(glm.fit.3)
summary(glm.fit.4)


# chap6-9
####(a)####
install.packages('ISLR')
library(ISLR2)
College
set.seed(11)
train.size <- dim(College)[1]/2
train <- sample(1:dim(College)[1], train.size)
test <- -train
College.train <- College[train,]
College.test <- College[test,]

####(b)####
lm.fit <- lm(Apps~. , data = College.train)
lm.pred <- predict(lm.fit, College.test)
real <- College.test[, 'Apps']
mean((real - lm.pred)^2)

####(c)####
#install.packages('glmnet')
library(glmnet)

train.mat <- model.matrix(Apps~. , data = College.train)
test.mat <- model.matrix(Apps~. , data = College.test)
cv.ridge <- cv.glmnet(train.mat, College.train[, 'Apps'],
                       alpha = 0)
lambda.best <- cv.ridge$lambda.min
lambda.best

mod.ridge <- cv.glmnet(train.mat, College.train[, 'Apps'],
                       alpha = 0, thresh = 1e-12)

ridge.pred <- predict(mod.ridge, newx = test.mat, s = lambda.best)
mean((College.test[, "Apps"] - ridge.pred)^2)

####(d)####
cv.lasso <- cv.glmnet(train.mat, College.train[, "Apps"], 
                      alpha = 1)
lambda.best <- cv.lasso$lambda.min
lambda.best

mod.lasso <- cv.glmnet(train.mat, College.train[, "Apps"], 
                      alpha = 1, thres = 1e-12)
lasso.pred <- predict(mod.lasso, newx = test.mat, s = lambda.best)
mean((College.test[, "Apps"] - lasso.pred)^2)
predict(mod.lasso, s = lambda.best, type = "coefficients")

# chap8-10
####(a)####
set.seed(1)
p <- 20; n = 1000
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
beta[1] <- 0
beta[6] <- 0
beta[9] <- 0
beta[12] <- 0
beta[13] <- 0
eps <- rnorm(p)
y <- x %*% beta + eps

####(b)####
train <- sample(seq(1000), 100, replace = FALSE)
y.train <- y[train, ]
y.test <- y[-train, ]
x.train <- x[train, ]
x.test <- x[-train, ]

####(c)####
library(leaps)
regfit.full <- regsubsets(y ~ ., data = data.frame(x = x.train, y = y.train), 
                         nvmax = p)
val.errors = rep(NA, p)
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")
train.mat <- model.matrix(y~., data = data.frame(x = x.train, y = y.train))
for (i in 1:p) {
  coefi = coef(regfit.full, id = i)
  pred = train.mat[, names(coefi)] %*% coefi
  val.errors[i] = mean((y.train - pred)^2)
}
plot(val.errors, ylab = "Training MSE", pch = 19, type = "b")

####(d)####
test.mat <- model.matrix(y~. , data =  data.frame(x = x.test, y = y.test))
test.errors = rep(NA, p)
for (i in 1:p) {
  coefi = coef(regfit.full, id = i)
  pred = test.mat[, names(coefi)] %*% coefi
  test.errors[i] = mean((y.test - pred)^2)
}

plot(test.errors, ylab = "Test MSE", pch = 19, type = "b")

####(e)####
which.min(val.errors)
which.min(test.errors)

####(f)####
coef(regfit.full, id = which.min(test.errors))

####(g)####
val.errors = rep(NA, p)
x_cols <- colnames(x, do.NULL = FALSE, prefix = 'x.')
for (i in 1:p) {
    coefi = coef(regfit.full, id = i)
    val.errors[i] = sqrt(sum((beta[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) 
                + sum(beta[!(x_cols %in% names(coefi))])^2)
}
plot(val.errors, type = 'b', 
     xlab = "number of coefficients", 
     ylab = "error between estimated and true coefficients")

which.min(val.errors)

