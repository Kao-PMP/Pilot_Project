library(survival)
library(dplyr)
library(tidyr)
library(lattice)
library(splines)
library(ggplot2)

hyp=read.csv("/mnt/workspace/DCRI/Data/analysis_ds_clusters.csv")
train=hyp[which(hyp$train==1),]


cox_sp=coxph(Surv(tdeath, death)~BP.s, data=train)
#quantile(train2$BP.s, c(0.01, 0.99), na.rm=T)
#quantile(train2$BP.d, c(0.01, 0.99), na.rm=T)

BP.s=data.frame(seq(105, 180, 1))
colnames(BP.s)='BP.s'

surv_probs = survfit(cox_sp, BP.s)
summ=summary(surv_probs, times=1825)
out=t(summ$surv)
colnames(out)='Surv'
Fail=1-out[,1]
out=cbind(out, Fail)

final=cbind(BP.s, out)
final=final[,-2]

p1=qplot(x=BP.s, y=Fail, data=final, geom='line', ylim=c(0.05, 0.2), xlab='Systolic BP', ylab='Predicted Risk of Death at 5 Years')


cox_sp2=coxph(Surv(tdeath, death)~BP.d, data=train)
#quantile(train2$BP.s, c(0.01, 0.99), na.rm=T)
#quantile(train2$BP.d, c(0.01, 0.99), na.rm=T)

BP.d=data.frame(seq(55, 100, 1))
colnames(BP.d)='BP.d'

surv_prob2 = survfit(cox_sp2, BP.d)
summ=summary(surv_prob2, times=1825)
out=t(summ$surv)
colnames(out)='Surv'
Fail=1-out[,1]
out=cbind(out, Fail)

final2=cbind(BP.d, out)
final2=final2[,-2]

p2=qplot(x=BP.d, y=Fail, data=final2, geom='line', ylim=c(0.05, 0.2), xlab='Diastolic BP', ylab='Predicted Risk of Death at 5 Years')



