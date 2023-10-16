##### Q13 #####
library(ISLR2)
summary(Weekly)
pairs(Weekly)
head(Weekly)
cor(Weekly[,-9])

attach(Weekly)
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Weekly,
               family = binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit, type = "response")
n <- length(glm.probs)
glm.pred <- rep("Down", n)
glm.pred[glm.probs > .5] = "Up" 
table(glm.pred, Direction)

names(Weekly)
range(Year)
train <- (Year < 2009)
Weekly.2008 <- Weekly[!train,]
dim(Weekly)
dim(Weekly.2008)
glm.fit2 <- glm(Direction~Lag2,
                data = Weekly,
                family = binomial,
                subset = train)
glm.probs2 <- predict(glm.fit2, Weekly.2008, type = "response")
n2 <- length(glm.probs2)
glm.pred2 <- rep("Down", n2)
glm.pred2[glm.probs2 > .5] = "Up"
Direction.2008 <- Direction[!train]
table(glm.pred2, Direction.2008)
mean(glm.pred2 == Direction.2008)

##### LDA #####
library(MASS)
lda.fit <- lda(Direction~Lag2,
               data = Weekly,
               subset = train)
lda.pred <- predict(lda.fit, Weekly.2008)
names(lda.pred)
table(lda.pred$class, Direction.2008)
mean(lda.pred$class == Direction.2008)

####QDA####
qda.fit <- qda(Direction~Lag2,
               data = Weekly,
               subset = train)
qda.pred <- predict(qda.fit, Weekly.2008)
qda.class <- qda.pred$class
table(qda.class, Direction.2008)
mean(qda.class == Direction.2008)

####knn(k=1)####
library(class)
train
train.input <- as.matrix(Lag2[train])
test.input <- as.matrix(Lag2[!train])
train.target <- Direction[train]
knn.pred <- knn(train.input, test.input, train.target, k=1)
table(knn.pred, Direction.2008)
mean(knn.pred == Direction.2008)

#### naive Bayes ####
library(e1071)
nb.fit <- naiveBayes(Direction~Lag2,
                     data = Weekly,
                     subset = train)
nb.fit
nb.class <- predict(nb.fit, Weekly.2008)
table(nb.class, Direction.2008)
mean(nb.class == Direction.2008)

abs(cor(Weekly[,-9]))

#### Year:Volume ####
glm.fit3 <- glm(Direction~Year*Volume,
                data = Weekly,
                family = binomial,
                subset = train)
glm.probs3 <- predict(glm.fit3, Weekly.2008, type = "response")
n3 <- length(glm.probs3)
glm.pred3 <- rep("Down", n3)
glm.pred3[glm.probs3 > .5] = "Up"
table(glm.pred3, Direction.2008)
mean(glm.pred3 == Direction.2008)

lda.fit2 <- lda(Direction~Year*Volume,
                data = Weekly,
                subset = train)
lda.pred2 <- predict(lda.fit2, Weekly.2008)
lda.class2 <- lda.pred2$class
mean(lda.class2 == Direction.2008)

qda.fit2 <- qda(Direction~Year*Volume,
                data = Weekly,
                subset = train)
qda.pred2 <- predict(qda.fit2, Weekly.2008)
qda.class2 <- qda.pred2$class
mean(qda.class2 == Direction.2008)

#### Lag2:Lag3 ####
glm.fit4 <- glm(Direction~Lag2*Lag3,
                data = Weekly,
                family = binomial,
                subset = train)
glm.probs4 <- predict(glm.fit4, Weekly.2008, type = "response")
n4 <- length(glm.probs4)
glm.pred4 <- rep("Down", n3)
glm.pred4[glm.probs4 > .5] = "Up"
table(glm.pred4, Direction.2008)
mean(glm.pred4 == Direction.2008)

lda.fit3 <- lda(Direction~Lag2*Lag3,
                data = Weekly,
                subset = train)
lda.pred3 <- predict(lda.fit3, Weekly.2008)
lda.class3 <- lda.pred3$class
mean(lda.class3 == Direction.2008)
summary(lda.fit3)
qda.fit3 <- qda(Direction~Lag2*Lag3,
                data = Weekly,
                subset = train)
qda.pred3 <- predict(qda.fit3, Weekly.2008)
qda.class3 <- qda.pred3$class
mean(qda.class3 == Direction.2008)

#### Lag1:Lag2 ####
glm.fit5 <- glm(Direction~Lag1*Lag2,
                data = Weekly,
                family = binomial,
                subset = train)
glm.probs5 <- predict(glm.fit5, Weekly.2008, type = "response")
n5 <- length(glm.probs5)
glm.pred5 <- rep("Down", n5)
glm.pred5[glm.probs5 > .5] = "Up"
table(glm.pred5, Direction.2008)
mean(glm.pred5 == Direction.2008)

lda.fit4 <- lda(Direction~Lag1*Lag2,
                data = Weekly,
                subset = train)
lda.pred4 <- predict(lda.fit4, Weekly.2008)
lda.class4 <- lda.pred4$class
mean(lda.class4 == Direction.2008)

qda.fit4 <- qda(Direction~Lag1*Lag2,
                data = Weekly,
                subset = train)
qda.pred4 <- predict(qda.fit4, Weekly.2008)
qda.class4 <- qda.pred4$class
mean(qda.class4 == Direction.2008)

#### QDA with sqrt(abs(independent variable)) ####
qda.fit5 <- qda(Direction~Lag1+sqrt(abs(Lag1)),
                data = Weekly,
                subset = train)
qda.preds5 <- predict(qda.fit5, Weekly.2008)
qda.class5 <- qda.preds5$class
table(qda.class5, Direction.2008)

qda.fit6 <- qda(Direction~Lag2+sqrt(abs(Lag2)),
                data = Weekly,
                subset = train)
qda.preds6 <- predict(qda.fit6, Weekly.2008)
qda.class6 <- qda.preds6$class
table(qda.class6, Direction.2008)
mean(qda.class6 == Direction.2008)

qda.fit7 <- qda(Direction~Lag3+sqrt(abs(Lag3)),
                data = Weekly,
                subset = train)
qda.preds7 <- predict(qda.fit7, Weekly.2008)
qda.class7 <- qda.preds7$class
table(qda.class7, Direction.2008)
mean(qda.class7 == Direction.2008)

qda.fit8 <- qda(Direction~Lag4+sqrt(abs(Lag4)),
                data = Weekly,
                subset = train)
qda.preds8 <- predict(qda.fit8, Weekly.2008)
qda.class8 <- qda.preds8$class
table(qda.class8, Direction.2008)
mean(qda.class8 == Direction.2008)

qda.fit9 <- qda(Direction~Lag5+sqrt(abs(Lag5)),
                data = Weekly,
                subset = train)
qda.preds9 <- predict(qda.fit9, Weekly.2008)
qda.class9 <- qda.preds9$class
table(qda.class9, Direction.2008)
mean(qda.class9 == Direction.2008)

qda.fit10 <- qda(Direction~Volume+sqrt(abs(Volume)),
                data = Weekly,
                subset = train)
qda.preds10 <- predict(qda.fit10, Weekly.2008)
qda.class10 <- qda.preds5$class
table(qda.class10, Direction.2008)
mean(qda.class10 == Direction.2008)

#### knn with k=10 ####
knn.pred2 <- knn(train.input, test.input, train.target, k=10)
table(knn.pred2, Direction.2008)
mean(knn.pred2 == Direction.2008)

#### knn with k=50 ####
knn.pred3 <- knn(train.input, test.input, train.target, k=50)
table(knn.pred3, Direction.2008)
mean(knn.pred3 == Direction.2008)

##### knn with k=100 ####
knn.pred4 <- knn(train.input, test.input, train.target, k=100)
table(knn.pred4, Direction.2008)
mean(knn.pred4 == Direction.2008)

mean = c()
for(i in 1:100){
  knn.pred5 <- knn(train.input, test.input, train.target, k=i)
  table(knn.pred5, Direction.2008)
  new_mean = mean(knn.pred5 == Direction.2008)
  mean = append(mean, new_mean)
}
mean
max <- max(mean)
x <- which(mean == max)

#### knn with k=72 ####
knn.pred <- knn(train.input, test.input, train.target, k=x)
table(knn.pred, Direction.2008)
mean(knn.pred == Direction.2008)


##### Q12 #####