#2.4 10 data description
#data : Boston
#CRIM : 1인당 범죄율
#ZN : 25,000초과하는 거주지역의 비율
#INDUS : 비소매상업지역이 점유하고 있는 토지의 비율
#CHAS : 찰스강 경계는 1, 아니면 0
#NOX : 10PPM당 일산화질소
#RM : 1가구당 평균 방의 개수
#AGE : 1940년 이전에 건축된 소유주택의 비율
#DIS : 직업센터까지의 접근성 지수
#RAD : 방사형도로까지 접근성 지수
#TAX : 재산세율
#PTRATIO : 학생/교사 비율
#B : 흑인의 비율
#LSTAT : 하위계층 비율
#MEDV : 본인 소유의 주택가격(1,000달러 단위)

#2.4 10
library(MASS)
?Boston

attach(Boston)
pairs(Boston)

plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)
plot(Boston$rad, Boston$crim)
plot(Boston$tax, Boston$crim)
plot(Boston$ptratio, Boston$crim)

par(mfrow=c(1,3))
hist(Boston$crim[Boston$crim>1], breaks=25)
hist(Boston$tax, breaks=25)
hist(Boston$ptratio, breaks=25)

df <- Boston[Boston$chas == 1.0, ]
nrow(df)

median(ptratio)

Boston[min(medv), ]
summary(Boston)

rm.7 <- Boston[Boston$rm > 7, ]
nrow(rm.7)
rm.8 <- Boston[Boston$rm > 8, ]
nrow(rm.8)
summary(rm.8)
summary(Boston)

#3.7 13
set.seed(1)
x <- rnorm(100, 0, 1)
x
?rnorm
eps <- rnorm(100, 0, sqrt(0.25))
eps

y <- -1+0.5*x+eps
y
length(y)
 
plot(x, y)

lm.fit <- lm(y~x)
summary(lm.fit)

plot(x, y)
abline(lm.fit, col = 'red')
abline(-1, 0.5, lwd = 3, col = 'blue')
legend(-1, legend = c("model fit", "pop. regression"), col = c('red' ,'blue'), lwd=3)

lm.fit2 <- lm(y~x+I(x^2))
summary(lm.fit2)

set.seed(1)
eps1 = rnorm(100, 0, 0.125)
x1 = rnorm(100, 0, 1)
y1 = -1 + 0.5*x1 + eps1
plot(x1, y1)
lm.fit3 = lm(y1~x1)
summary(lm.fit3)
abline(lm.fit3, col = 'red')
abline(-1, 0.5, lwd = 3, col = 'blue')
legend(-1, legend = c("model fit", "pop. regression"), col = c('red' ,'blue'), lwd=3)

set.seed(1)
eps2 = rnorm(100, 0, 1)
x2 = rnorm(100, 0, 1)
y2 = -1 + 0.5*x2 + eps2
plot(x2, y2)
lm.fit4 = lm(y2~x2)
summary(lm.fit4)
abline(lm.fit4, col = 'red')
abline(-1, 0.5, lwd = 3, col = 'blue')
legend(-1, legend = c("model fit", "pop. regression"), col = c('red' ,'blue'), lwd=3)

confint(lm.fit)
confint(lm.fit3)
confint(lm.fit4)

#3.7 14
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1+rnorm(100)/10
y <- 2+2*x1+0.3*x2+rnorm(100)

cor(x1, x2)
plot(x1, x2)

lm.fit <- lm(y~x1+x2)
summary(lm.fit)

lm.fit2 <- lm(y~x1)
summary(lm.fit2)

lm.fit3 <- lm(y~x2)
summary(lm.fit3)

x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y = c(y, 6)
lm.fit <- lm(y~x1+x2)
summary(lm.fit)
lm.fit2 <- lm(y~x1)
summary(lm.fit2)

lm.fit3 <- lm(y~x2)
summary(lm.fit3)

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit3)

par(mfrow = c(3,1))
plot(predict(lm.fit), rstudent(lm.fit))
plot(predict(lm.fit2), rstudent(lm.fit2))
plot(predict(lm.fit3), rstudent(lm.fit3))
