rm(list=ls())
load("ECON8010F12beauty.RData")
attach(data)
#fraction having above average looks (abvavg=1/N)
sum(with(data, data$abvavg==1))
sum(with(data, data$abvavg==1))/nrow(data)
#Test the null hypothesis that the population fractions of above-average-looking women and men
#are the same at the 5% level.
ols <- lm(abvavg~female,data = data)
summary(ols)
#regression model for married
ols2 <-lm(married~belavg+abvavg+bigcity+educ,data=data)
summary(ols2)
## The percent correctly predicted
data$LPM.fitted <- ols2$fitted.values

data$fit.married[data$LPM.fitted>=0.5]<-1
data$fit.married[data$LPM.fitted<0.5]<-0

sum(with(data, fit.married==married))
sum(with(data, fit.married==married))/nrow(data)
#Check if there are observations who have the predicted probabilities outside [0,1]
summary(data$LPM.fitted)
#Chow test to test whether we have to specify the model (A) differently for men and women
# at the 5% level.
#Overall model is ols2
SSRA2 <- sum(ols2$residuals^2)
#Find SSR for male only
ols2M <-lm(married~belavg+abvavg+bigcity+educ,data=data,subset=(female==0))
SSRA2M <- sum(ols2M$residuals^2)
#find SSR for female only
ols2N <-lm(married~belavg+abvavg+bigcity+educ,data=data,subset=(female==1))
SSRA2N <- sum(ols2N$residuals^2)
#Calculating Chow statistic
kplus1<-nrow(summary(ols2)$coef)
obsn<-ols2$df.residual+kplus1
ChowS <-((SSRA2 - (SSRA2M+SSRA2N))/(kplus1))/((SSRA2M+SSRA2N)/(obsn-2*(kplus1)))
FC<-qf(.95, df1=kplus1, df2=(obsn-2*(kplus1))) 
ChowP<-pf(ChowS,df1=kplus1, df2=(obsn-2*(kplus1)),lower.tail=FALSE)
ChowP