###################
###################
# Chapter 7
####################
####################
library(ISLR)
attach(Wage)
##############
## Polynomials
###############
# fit a polynomial model with degree 4 and check the coefficient estimation
fit=lm(wage~poly(age ,4) ,data=Wage)
coef(summary (fit))
# compare the results with fit1. What is the difference?
fit2=lm(wage~poly(age ,4, raw=T),data=Wage)
coef(summary(fit2))
# compare the results with fit2
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)
fit2b=lm(wage~cbind(age ,age^2,age^3, age ^4),data=Wage)
coef(fit2b)
### Draw a plot of the raw data with its fitted line
## set the range of the plot
agelims =range(age)
## fit the predict values
age.grid=seq(from=agelims [1],to=agelims [2])
## fit the predict values
## evaluate standard error of each point
preds=predict (fit ,newdata =list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
## set the plot parameters
# this prepares two plots side by side
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age ,wage ,xlim=agelims ,cex =.5,col="darkgrey")
## make a title for the plots
title(" Degree -4 Polynomial ",outer=T)
lines(age.grid ,preds$fit ,lwd=2,col="blue")
matlines(age.grid ,se.bands ,lwd=1, col=" blue",lty=3)
## check out the maximum difference in the predictions between fit1 and fit2
preds2=predict(fit2 ,newdata =list(age=age.grid),se=TRUE)
max(abs(preds$fit -preds2$fit ))

# classification with polynomial model
fit=glm(I(wage >250)~poly(age ,4),data=Wage , family=binomial )
preds=predict (fit ,newdata =list(age=age.grid),se=T)
# check out the predicted probabilities
pfit=exp(preds$fit )/(1+exp(preds$fit ))
se.bands.logit = cbind(preds$fit +2* preds$se.fit , preds$fit -2*
                           preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

preds=predict (fit ,newdata =list(age=age.grid),type="response",se=T)

# plot the classification resutls
plot(age ,I(wage >250),xlim=agelims ,type="n",ylim=c(0,.2))
points(jitter(age), I((wage >250)/5),cex=.5,pch ="|", col="darkgrey")
lines(age.grid ,pfit ,lwd=2, col ="blue")
matlines (age.grid ,se.bands ,lwd=1, col=" blue",lty=3)

# fit the step function
table(cut(age ,4))
fit=lm(wage~cut(age ,4),data=Wage)
coef(summary (fit))

##############
## Splines
################
library(splines)
# fit a cubic spline (default) model with 3 knots
fit=lm(wage~bs(age ,knots=c(25,40,60) ),data=Wage)
pred=predict(fit ,newdata =list(age=age.grid),se=T)
plot(age ,wage ,col="gray")
lines(age.grid ,pred$fit ,lwd=2)
lines(age.grid ,pred$fit +2*pred$se ,lty="dashed")
lines(age.grid ,pred$fit -2*pred$se ,lty="dashed")

dim(bs(age ,knots=c(25,40,60) ))
dim(bs(age ,df=6))
attr(bs (age ,df=6) ,"knots")

# fit a natural spline model
fit2=lm(wage~ns(age ,df=4),data=Wage)
pred2=predict (fit2 ,newdata=list(age=age.grid),se=T)
lines(age.grid , pred2$fit ,col="red",lwd=2)

plot(age ,wage ,xlim=agelims ,cex =.5,col="darkgrey")
title("Smoothing Spline")
# fit a smoothing spline model with df=16
fit=smooth.spline(age ,wage ,df=16) 
# fit a smoothing spline model with data-driven degrees of freedom
fit2=smooth.spline (age ,wage ,cv=TRUE)
fit2$df

lines(fit, col="red",lwd =2)
lines(fit2, col="blue",lwd=2)
legend ("topright",legend=c("16 DF" ,"6.8 DF"), col=c("red","blue"),lty=1,lwd=2, cex =.8)

# fit a local regression with different "spans"
plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title("Local Regression ")
fit=loess(wage~age ,span=.2,data=Wage)
fit2=loess(wage~age ,span=.5,data=Wage)
lines(age.grid ,predict (fit ,data.frame(age=age.grid)),
        col="red",lwd=2)
lines(age.grid ,predict (fit2 ,data.frame(age=age.grid)),
        col="blue",lwd=2)
legend ("topright",legend=c("Span=0.2"," Span=0.5"), col=c("red","blue"),lty=1,lwd=2, cex =.8)

##############
## GAM
################
# using natural splines
gam1=lm(wage~ns(year ,4)+ns(age ,5)+education ,data=Wage)
library (gam)
# using smoothing splines
gam.m3=gam(wage~s(year ,4)+s(age ,5)+education ,data=Wage)
par(mfrow=c(1,3))
# 
plot(gam.m3, se=TRUE ,col ="blue")
plot.Gam(gam1 , se=TRUE , col="red")

# using local regressions
gam.lo=gam(wage~s(year ,df=4)+lo(age ,span =0.7)+education, data=Wage)
plot.Gam(gam.lo, se=TRUE , col =" green")
