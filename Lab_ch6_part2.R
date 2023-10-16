## PCR and PLS Regression


### Principal Components Regression

###
library(ISLR2)
library(pls)

### Continuing from part 1
Hitters <- na.omit(Hitters)
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)

x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary
y.test <- y[test]

set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE,
               validation = "CV")
###
summary(pcr.fit)
###
validationplot(pcr.fit, val.type = "MSEP")
###
set.seed(1)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train,
               scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
###
pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 5)
mean((pcr.pred - y.test)^2)
# 143,674
###
pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 5)
summary(pcr.fit)

### Partial Least Squares

###
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
###
pls.pred <- predict(pls.fit, x[test, ], ncomp = 1)
mean((pls.pred - y.test)^2)
###
pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE,
                ncomp = 1)
summary(pls.fit)
###
