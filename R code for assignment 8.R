rm(list=ls())
load("ECON8010F10gpa1.RData")
attach(data)
#OLS model relating colGPA to hsGPA, ACT, skipped, and PC
ols = lm(colGPA~hsGPA+ACT+skipped+PC,data=data)
summary(ols)
#Breusch-Pagan test for heteroskedasticity
# install.packages("lmtest")
library(lmtest)
bptest(ols, data=data)
#white's test for heteroskedasticity
data$residualSQ<-ols$residuals^2
data$fit<-ols$fitted.values
# regression of the squared OLS residuals
WhiteOLS<-lm(residualSQ ~fit+I(fit^2), data=data)
# k+1: the number of explanatory variables + 1 (for intercept)
Wkplus1<-nrow(summary(WhiteOLS)$coef)
# obsn: the number of observations
Wobsn<-WhiteOLS$df.residual+Wkplus1
Wobsn

# LM statistics for White test
WhiteLM<-Wobsn*summary(WhiteOLS)$r.squared 
WhiteLM

# Calculating p-value from Chi-squared distribution
pchisq(WhiteLM, df=2, lower.tail=FALSE)
#Calculating robust standard error for ols
# install.packages("sandwich")
library(sandwich)
# install.packages("lmtest")
library(lmtest)
# HC0 option is for textbook white robust standard error
robustse<-coeftest(ols, vcov = vcovHC(ols, "HC0"))
robustse
##  FWLS/FGLS
#step 1. save u_hat from the original OLS model
u_hat<-ols$residuals
#step 2. generate log u_hat
lu_hat_sq<-log(u_hat^2)
#step 3. regress log u_hat on x
ols2<-lm(lu_hat_sq~ hsGPA+ACT+skipped+PC, data=data)
fv<-ols2$fitted.values
#step 4. exponentiate the fitted values
efv<-exp(fv)
#step 5. WLS
fwls<-lm(colGPA~hsGPA+ACT+skipped+PC, weights=1/(efv), data=data)
summary(fwls)