## Linear Discriminant Analysis

###
library(ISLR2)
library(MASS)

attach(Smarket)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
Direction.2005 <- Direction[!train]

lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket,
               subset = train)
lda.fit
plot(lda.fit)

mean(Direction[train]=="Down")
###
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

###
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
###
sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)
###
lda.pred$posterior[1:20, 1]
lda.class[1:20]
###
sum(lda.pred$posterior[, 1] > .9)
range(lda.pred$posterior[, 1])

## Quadratic Discriminant Analysis

###
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket,
               subset = train)
qda.fit

###
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)
(30+121)/(30+20+81+121)
## Naive Bayes
###
library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket,
                     subset = train)
nb.fit
###
mean(Lag1[train][Direction[train] == "Down"])
sd(Lag1[train][Direction[train] == "Down"])
###
nb.class <- predict(nb.fit, Smarket.2005)
table(nb.class, Direction.2005)
mean(nb.class == Direction.2005)
###
nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]

#########################################
## Preview of Ch. 5
#########################################
set.seed(1)
### validation set approach
lda.err.rate = rep(NA, 100)
glm.err.rate = rep(NA, 100)

for (i in 1:100){
  random_index = sample.int(nrow(Smarket))
  
  validation_set = random_index[1:(nrow(Smarket)/2)]
  index_vec = rep(TRUE, nrow(Smarket))
  index_vec[validation_set] = FALSE
  # 
  lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket,
              subset = index_vec)
  lda.pred.validation = predict(lda.fit, Smarket[validation_set,])
  # recording the error rate of the current split
  lda.err.rate[i] = mean(lda.pred.validation$class == Smarket$Direction[validation_set])
  #  
  glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Smarket,
                 subset = index_vec, family = "binomial")
  pred.validation = predict(glm.fit, Smarket[validation_set,], "response")
  pred.class = rep("Down", length(validation_set))
  pred.class[pred.validation>.5] = "Up"
  # recording the error rate of the current split
  glm.err.rate[i] = mean(pred.class  == Smarket$Direction[validation_set])
}

plot(glm.err.rate, pch=16, xlab="realization", ylab="error rate")
points(lda.err.rate, pch=16, col=2)
