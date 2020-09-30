#!/usr/bin/env python
# coding: utf-8

# Purpose: Cox model with SBP and DBP in relation to all cause death  
# Programmer:Hillary Mulder  
# Date:August 2019  
# 
# Modification:  
# 

# In[17]:


library(survival)
library(dplyr)
library(tidyr)
library(lattice)
library(splines)

hyp=read.csv("Data/analysis_ds_clusters.csv")
hyp$cluster1=factor(hyp$cluster1, levels=1:4)
hyp$cluster2=factor(hyp$cluster2, levels=1:5)

#pulse pressure computation
hyp$pp = hyp$BP.s - hyp$BP.d

train=hyp[which(hyp$train==1),]
test=hyp[which(hyp$test==1),]

colnames(hyp)

hyp %>%
    select(study.1, BP.s, BP.d, dthDays) %>%
    group_by(study.1) %>%
    summarise(days_mean = mean(dthDays, na.rm=T), days_median = median(dthDays, na.rm=T), min_days=min(dthDays, na.rm=T), max_days=max(dthDays, na.rm=T))

#make the matrix a bit smaller
for_cox=hyp[, c('person_id','BP.s','BP.d', 'type_hyp','death','tdeath', 'study.1','cluster1','cluster2','train','test')]


train2=for_cox[which(for_cox$train==1),]
test2=for_cox[which(for_cox$test==1),]


# In[4]:


cox1=coxph(Surv(tdeath, death)~BP.s + BP.d + BP.s*BP.d, data=train)
predictions = predict(cox1, test, type='risk')
#to do: split into train/test (75/25) sets using stratified random sampling with trials as stratification variable
#cov=test[,c('BP.s','BP.d')]
#s2 = predictProb.coxph(cox1, Surv(test$tdeath, test$death), cov, times=365 )
#full=cbind(test, s2)
#full$failure=1-full$s2

hm = data.frame(x=test$BP.s, y=test$BP.d, z=predictions)
hm2 = hm %>%
    group_by(x, y) %>%
    summarize(mean_risk = mean(z, na.rm=T))

x=seq(83, 234, 1)
y=seq(42, 129, 1)
fake = data.frame(merge(x, y, all=T), mean_risk=NA)
hm2=rbind(data.frame(hm2), fake)
hm_final = hm2 %>%
         group_by(x, y) %>%
         summarize(risk = mean(mean_risk, na.rm=T))

hm.wide = hm_final %>% spread(x, risk)
hm_mat=as.matrix(hm.wide[, 2:152])

row.names(hm_mat) = t(hm.wide[,1])
palette <- colorRampPalette(c("white", "red", "blue"))( 100 )
my.at.1 <- seq(0, 3.1, length.out=99)


p0=levelplot(t(hm_mat), at=my.at.1,
             col.regions=palette, xlab='Systolic BP', cex.lab=0.8,
             ylab='Diastolic BP', main=list("Risk by SBP and DBP", cex=0.8),
              scales=list(x=list(at=c(16, 36, 56, 76, 96, 116, 136), labels=c('100', '120', '140', '160', '180', '200', '220')),
              y=list(at=c(7, 32, 57, 82), labels=c('50', '75', '100', '125'))))

p0


# In[20]:


cox1b=coxph(Surv(tdeath, death)~BP.s + BP.d + BP.s*BP.d + age +Sex + Race2 +HxMIStr+HxDM+study.1, data=train)
summary(cox1b)


# In[21]:



cox2=coxph(Surv(tdeath, death)~pp, data=train)
cox2b=coxph(Surv(tdeath, death)~pp+ age +Sex + Race2 +HxMIStr+HxDM+study.1, data=train)
summary(cox2)
summary(cox2b)


# In[22]:


cox3=coxph(Surv(tdeath, death)~type_hyp, data=train)
cox3b=coxph(Surv(tdeath, death)~type_hyp+ age +Sex + Race2 +HxMIStr+HxDM+study.1, data=train)
summary(cox3)
summary(cox3b)


# In[24]:


cox4=coxph(Surv(tdeath, death)~cluster1, data=train)
cox4b=coxph(Surv(tdeath, death)~cluster1+ age +Sex + Race2 +HxMIStr+HxDM+study.1, data=train)
summary(cox4)
summary(cox4b)


# In[25]:


cox5=coxph(Surv(tdeath, death)~cluster2, data=train)
cox5b=coxph(Surv(tdeath, death)~cluster2+ age +Sex + Race2 +HxMIStr+HxDM+study.1, data=train)
summary(cox5)
summary(cox5b)

