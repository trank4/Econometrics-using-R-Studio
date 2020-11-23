rm(list=ls())
load("ECON8010F13apple.RData")
attach(data)
#define ecobuy
data <- transform(data,ecobuy = as.numeric(ecolbs>0))

#percent correctly predicted for LPM, probit, logit models
#define hhsize as a categorical variable
data <- transform(data,hhsize.f = as.factor(hhsize))
#LPM
lpm = lm(ecobuy~ecoprc+regprc+faminc+hhsize.f+educ+age,data=data)
data<-transform(data,ecobuy_fitted_lp=fitted.values(lpm))
data<-transform(data,ecobuy_tilde_lp=as.numeric(ecobuy_fitted_lp>=0.5))
data<-transform(data,matched_lp=as.numeric(ecobuy_tilde_lp==ecobuy))
obsn <- lpm$df.residual+nrow(summary(lpm)$coef)
correctly_pred_lpm=sum(data$matched_lp)/obsn
correctly_pred_lpm
#probit
probit <- glm(ecobuy~ecoprc+regprc+faminc+hhsize.f+educ+age,family=binomial(link='probit'),data=data)
data<-transform(data,ecobuy_fitted_probit=fitted.values(probit))
data<-transform(data,ecobuy_tilde_probit=as.numeric(ecobuy_fitted_probit>=0.5))
data<-transform(data,matched_probit=as.numeric(ecobuy_tilde_probit==ecobuy))
obsn <- probit$df.residual+nrow(summary(probit)$coef)
correctly_pred_prob=sum(data$matched_probit)/obsn
correctly_pred_prob
#logit
logit <- glm(ecobuy~ecoprc+regprc+faminc+hhsize.f+educ+age,family=binomial(link='logit'),data=data)
data<-transform(data,ecobuy_fitted_logit=fitted.values(logit))
data<-transform(data,ecobuy_tilde_logit=as.numeric(ecobuy_fitted_logit>=0.5))
data<-transform(data,matched_logit=as.numeric(ecobuy_tilde_logit==ecobuy))
obsn <- logit$df.residual+nrow(summary(logit)$coef)
correctly_pred_log=sum(data$matched_logit)/obsn
correctly_pred_log
#AME, PEA probit and logit for ecoprc
#install.packages("mfx")
library(mfx)
ame_prob=probitmfx(formula=ecobuy~ecoprc+regprc+faminc+hhsize.f+educ+age,data=data,atmean=FALSE)
ame_prob$mfxest[1]
#PEA for probit
pea_prob=probitmfx(formula=ecobuy~ecoprc+regprc+faminc+hhsize.f+educ+age,data=data,atmean=TRUE)
pea_prob$mfxest[1]
#AME for logit
ame_log=logitmfx(formula=ecobuy~ecoprc+regprc+faminc+hhsize.f+educ+age,data=data,atmean=FALSE)
ame_log$mfxest[1]
#PEA for logit
pea_log=logitmfx(formula=ecobuy~ecoprc+regprc+faminc+hhsize.f+educ+age,data=data,atmean=TRUE)
pea_log$mfxest[1]
#REport tables
library(stargazer)
stargazer(lpm, probit, logit,
          add.lines=list(c("% correctly predicted LPM:",correctly_pred_lpm),
          c("% correctly predicted Probit:",correctly_pred_prob),
          c("% correctly predicted Logit:",correctly_pred_log)),
          type="text",title="Table. Ecobuy")
ame_prob
pea_prob
ame_log
pea_log
#Test if eco apple is normal good (increase demand as income increase and viceversa)
#library("car")
#library(lmtest)
#hypothesis test using lpm for one tail test faminc > 0
#t-statistic of faminc
#from lpm we have t-value for faminc
faminc_stat = 0.976
#critical value for one sided test
alpha = 0.1
qt(1-alpha,lpm$df.residual)
#since faminc_stat < critical value, we fail to reject H0.
# Test whether the family income and household size variables are 
#jointly significant at the 5% level for each model.
#LPM
linearHypothesis(lpm,c('faminc=0',
                       'hhsize.f2=0',
                       'hhsize.f3=0',
                       'hhsize.f4=0',
                       'hhsize.f5=0',
                       'hhsize.f6=0',
                       'hhsize.f7=0',
                       'hhsize.f8=0',
                       'hhsize.f9=0'
                       ))
#Probit
probitR <- glm(ecobuy~ecoprc+regprc+educ+age,family=binomial(link='probit'),data=data)
waldtest(probit,probitR,test='Chisq')
lrtest(probit,probitR)
anova(probit,probitR,test='Rao')
#Logit
logitR <- glm(ecobuy~ecoprc+regprc+educ+age,family=binomial(link='logit'),data=data)
waldtest(logit,logitR,test='Chisq')
lrtest(logit,logitR)
anova(logit,logitR,test='Rao')
