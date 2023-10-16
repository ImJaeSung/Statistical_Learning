#### 6-9 ####
install.packages('pls')
library(pls)
library(ISLR2)

set.seed(2)
train.size <- dim(College)[1]/2
train <- sample(1:dim(College)[1], train.size)
test <- -train
College.train <- College[train,]
College.test <- College[test,]

#####(e)#####
set.seed(1)
pcr.fit <- pcr(Apps ~ ., data = College.train,
               scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
summary(pcr.fit)# 10 random segments

pcr.pred <- predict(pcr.fit, College.test, ncomp = 9)
mean((College.test[, "Apps"] - pcr.pred)^2)

#####(f)######
set.seed(1)
pls.fit <- plsr(Apps ~ ., data = College.train,
                scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
###
pls.pred <- predict(pls.fit, College.test , ncomp = 12)
mean((College.test[, "Apps"] - pls.pred)^2)


#### 7-1 ####
library(ISLR2)
data(Boston)
set.seed(1)

####(a)####
lm.fit <-lm(nox~poly(dis ,3), data = Boston)
summary(lm.fit)

lm.fit2 <-lm(nox~poly(dis ,3, raw=T), data = Boston)
summary(lm.fit2)

attach(Boston)
dislims <- range(dis)
dis.grid <- seq(from=dislims[1], to=dislims[2])

preds <- predict(lm.fit, newdata = list(dis=dis.grid), se=TRUE)
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

par(mfrow=c(1,1),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(dis, nox ,xlim=dislims ,cex =.5,col="darkgrey")
title(" Degree -3 Polynomial ",outer=T)
lines(dis.grid ,preds$fit ,lwd=2,col="blue")
matlines(dis.grid ,se.bands ,lwd=1, col=" blue",lty=3)

###raw = T 차이###
preds2 <- predict(lm.fit2, newdata =list(dis=dis.grid), se=TRUE)
max(abs(preds$fit -preds2$fit ))

#####(b)#####
par(mfrow = c(3, 4))
RSS <- rep(NA, 10)
for (i in 1:10){
  lm.fit <-lm(nox~poly(dis ,i), data = Boston)
  
  dislims <- range(dis)
  dis.grid <- seq(from=dislims[1], to=dislims[2])
  
  preds <- predict(lm.fit, newdata = list(dis=dis.grid), se=TRUE)
  se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
  
  plot(dis, nox ,xlim=dislims ,cex =.5,col="darkgrey")
  title(" Degree of 1~10 Polynomial ", outer=T)
  lines(dis.grid ,preds$fit ,lwd=2,col="blue")
  matlines(dis.grid ,se.bands ,lwd=1, col=" blue",lty=3)
  
  RSS[i] = sum(lm.fit$residuals^2)
}

RSS
par(mfrow = c(1, 1))
plot(RSS,  xlab = "Degree")
title("RSS of ploymonial", outer = T)
lines(RSS, lty = 1)

####(c)####
library(boot)
set.seed(1)
cv.error <- rep(NA, 10)
for (i in 1:10) {
  glm.fit <- glm(nox ~ poly(dis, i), data = Boston)
  cv.error[i] <- cv.glm(Boston, glm.fit, K = 10)$delta[2]
}
cv.error
par(mfrow = c(1, 1))
plot(cv.error, xlab = "Degree", ylab = "CV error", type = "l", lwd = 2)
title("CV.error according to degree in 10-fold", outer = T)

##pick degree = 4
lm.fit <-lm(nox~poly(dis ,4), data = Boston)
summary(lm.fit)

####(d)####
range(dis) # knot 4,8,12

library(splines)
set.seed(1)
lm.fit <- lm(nox~bs(dis, df=4, knots=c(4,8,12)), data=Boston)
summary(lm.fit)
pred <- predict(lm.fit ,newdata =list(dis=dis.grid),se=T)
plot(dis ,nox ,col="gray")
lines(dis.grid ,pred$fit ,lwd=2)
lines(dis.grid ,pred$fit +2*pred$se ,lty="dashed")
lines(dis.grid ,pred$fit -2*pred$se ,lty="dashed")

dim(bs(dis ,knots=c(4,8,12)))
dim(bs(dis ,df=4))
attr(bs(dis ,df=4) ,"knots")

####(e)####
set.seed(1)
par(mfrow = c(3, 4))
RSS <- rep(NA, 12)
for (i in 3:14){
  lm.fit <- lm(nox~bs(dis, df=i), data=Boston)
  pred <- predict(lm.fit ,newdata =list(dis=dis.grid),se=T)
  plot(dis ,nox ,col="gray")
  title(" Degree of 3~14 Splines ", outer=T)
  lines(dis.grid ,pred$fit ,lwd=2)
  lines(dis.grid ,pred$fit +2*pred$se ,lty="dashed")
  lines(dis.grid ,pred$fit -2*pred$se ,lty="dashed")
  RSS[i] <- sum(lm.fit$residuals^2)
}

RSS[-c(1, 2)]
par(mfrow = c(1, 1))
plot(RSS,  xlab = "DF")
title("RSS of splines", outer = T)
lines(RSS, lty = 1)



####(f)####
set.seed(1)
cv.error <- rep(NA, 12)
for (i in 3:14) {
  glm.fit <- glm(nox ~ bs(dis, df=i), data = Boston)
  cv.error[i] <- cv.glm(Boston, glm.fit, K = 10)$delta[2]
}
cv.error[-c(1,2)]
plot(cv.error, xlab = "DF", ylab = "CV error", type = "l", lwd = 2)
title("CV.error according to DF in 10-fold", outer = T)

lm.fit <- lm(nox~bs(dis, df=6), data=Boston)      
summary(lm.fit)
