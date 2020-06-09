library(survival)
library(dplyr)
library(tidyr)
library(lattice)
library(splines)
library(risksetROC)
source('/mnt/workspace/DCRI/Progs/get_auc.R')

hyp=read.csv("/mnt/workspace/DCRI/Data/analysis_ds_clusters.csv")
hyp$cluster1=factor(hyp$cluster1, levels=1:4)
hyp$cluster2=factor(hyp$cluster2, levels=1:5)
hyp$type_hyp=relevel(hyp$type_hyp, ref='Controlled')

#pulse pressure computation
hyp$pp = hyp$BP.s - hyp$BP.d

train=hyp[which(hyp$train==1),]
test=hyp[which(hyp$test==1),]

cox1=coxph(Surv(tdeath, death)~BP.s + BP.d + BP.s*BP.d, data=train)
cox1_test = test[, c('tdeath','death','BP.d','BP.s')]
#quantile(train2$BP.s, c(0.01, 0.99), na.rm=T)
#quantile(train2$BP.d, c(0.01, 0.99), na.rm=T)

BP.s=seq(105, 180, 1)
BP.d=seq(55, 100, 1)
ND = merge(BP.s, BP.d, all=T)
colnames(ND)=c('BP.s', 'BP.d')

surv_probs = survfit(cox1, ND)
summ=summary(surv_probs, times=1825)
out=t(summ$surv)
colnames(out)='Surv'
Fail=1-out[,1]
out=cbind(out, Fail)

final=cbind(ND, out)
final=final[,-3]


wide = final %>% spread(BP.s, Fail)
mat=as.matrix(wide[, 2:77])

row.names(mat) = t(wide[,1])
palette <- colorRampPalette(c("red", "blue"))( 100 )
my.at.1 <- seq(0, 0.25, length.out=99)


p0=levelplot(t(mat), at=my.at.1,
             col.regions=palette, xlab='Systolic BP', cex.lab=0.8,
             ylab='Diastolic BP', main=list("Risk by SBP and DBP", cex=0.8),
             scales=list(x=list(at=c(1, 26, 51, 76), labels=c('105', '130', '155', '180')),
                         y=list(at=c(1, 11, 21, 31, 41)), labels=c('55', '65', '75', '85','95')))

#p0
c1a=concordance(Surv(tdeath, death)~ predict(cox1, newdata=cox1_test, type='risk'), data=data.frame(cox1_test))
c1af=1-c1a$concordance
cox1_test=cox1_test[which(complete.cases(cox1_test)),]
cox1auc1=get_auc(cox=cox1, xtest=cox1_test, time=cox1_test$tdeath, status=cox1_test$death, t_eval=365 )
cox1auc3=get_auc(cox=cox1, xtest=cox1_test, time=cox1_test$tdeath, status=cox1_test$death, t_eval=1095 )
c1a_final=list(c1af=c1af, cox1auc1=cox1auc1, cox1auc3=cox1auc3)
#
cox1b=coxph(Surv(tdeath, death)~BP.s + BP.d + BP.s*BP.d + age +Sex + Race2 +HxMIStr+HxDM+study.1+Toba+HMG, data=train)
cox1b_test=test[,c('tdeath','death','BP.s','BP.d', 'age', 'Sex', 'Race2', 'HxMIStr', 'HxDM', 'study.1', 'Toba', 'HMG' )]
cox1b_test=cox1b_test[which(complete.cases(cox1b_test)),]
c1b=concordance(Surv(tdeath, death)~ predict(cox1b, newdata=cox1b_test, type='risk'), data=data.frame(cox1b_test))
c1b=1-c1b$concordance
cox1bauc1=get_auc(cox=cox1b, xtest=cox1b_test, time=cox1b_test$tdeath, status=cox1b_test$death, t_eval=365 )
cox1bauc3=get_auc(cox=cox1b, xtest=cox1b_test, time=cox1b_test$tdeath, status=cox1b_test$death, t_eval=1095 )
c1b_final=list(c1b=c1b, cox1bauc1=cox1bauc1, cox1bauc3=cox1bauc3)


cox2a=coxph(Surv(tdeath, death)~pp, data=train)
cox2b=coxph(Surv(tdeath, death)~pp+ age +Sex + Race2 +HxMIStr+HxDM+study.1+Toba+HMG, data=train)
cox3a=coxph(Surv(tdeath, death)~relevel(type_hyp, ref='Controlled'), data=train)
cox3b=coxph(Surv(tdeath, death)~relevel(type_hyp, ref='Controlled') + age +Sex + Race2 +HxMIStr+HxDM+study.1+Toba+HMG, data=train)
cox4a=coxph(Surv(tdeath, death)~cluster1, data=train)
cox4b=coxph(Surv(tdeath, death)~cluster1+ age +Sex + Race2 +HxMIStr+HxDM+study.1+Toba+HMG, data=train)
cox5a=coxph(Surv(tdeath, death)~cluster2, data=train)
cox5b=coxph(Surv(tdeath, death)~cluster2+ age +Sex + Race2 +HxMIStr+HxDM+study.1+Toba+HMG, data=train)


model=function(xvars){
  xtrain=train[,xvars]
  xtest=test[, xvars]
  xtest=xtest[which(complete.cases(xtest)),]
  cox=coxph(Surv(tdeath, death)~., data=xtrain)
  c1=concordance(Surv(tdeath, death)~ predict(cox, newdata=xtest, type='risk'), data=data.frame(xtest))
  finalc=1-c1$concordance
  auc1=get_auc(cox=cox, xtest=xtest, time=xtest$tdeath, status=xtest$death, t_eval=365 )
  auc3=get_auc(cox=cox, xtest=xtest, time=xtest$tdeath, status=xtest$death, t_eval=1095 )
  return(list(finalc=finalc, auc1=auc1, auc3=auc3))
}
c2a=model(xvars=c('tdeath','death','pp'))
c2b=model(xvars=c('tdeath','death','pp', 'age', 'Sex', 'Race2', 'HxMIStr', 'HxDM', 'study.1', 'Toba', 'HMG' ))
c3a=model(xvars=c('tdeath','death','type_hyp'))
c3b=model(xvars=c('tdeath','death','type_hyp', 'age', 'Sex', 'Race2', 'HxMIStr', 'HxDM', 'study.1', 'Toba', 'HMG' ))
c4a=model(xvars=c('tdeath','death','cluster1'))
c4b=model(xvars=c('tdeath','death','cluster1', 'age', 'Sex', 'Race2', 'HxMIStr', 'HxDM', 'study.1', 'Toba', 'HMG' ))
c5a=model(xvars=c('tdeath','death','cluster2'))
c5b=model(xvars=c('tdeath','death','cluster2', 'age', 'Sex', 'Race2', 'HxMIStr', 'HxDM', 'study.1', 'Toba', 'HMG' ))




