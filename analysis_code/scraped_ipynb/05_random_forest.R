#!/usr/bin/env python
# coding: utf-8

# Purpose: Random Forest for survival  
# Programmer:Hillary Mulder  
# Date: October 2019  
# Modification History:  
# 

# In[3]:


#library(tidyverse)
library(ranger)
library(survival)
library(dplyr)
library(caret)


# In[2]:


hyp=read.csv("Data/analysis_ds.csv")
hyp$allhat=ifelse(hyp$study.1=='ALLHAT', 1, 0)
hyp$aimhigh=ifelse(hyp$study.1=='AIMHIGH', 1, 0)
hyp$accord=ifelse(hyp$study.1=='ACCORD', 1, 0)

train=hyp[which(hyp$train==1),]
test=hyp[which(hyp$test==1),]

#train2=train[complete.cases(train[, c(1:3, 5, 6, 9:19, 20:22, 50:53)]), c(1:3, 5, 6, 9:19, 20:22, 50:53)]
#colnames(train2)
train2=train[complete.cases(train[, c(2, 3, 5, 6, 9:19, 20:22, 56:58)]), c(2, 3, 5, 6, 9:19, 20:22, 56:58)]
colnames(train2)
test2=test[complete.cases(test[, c(2, 3, 5, 6, 9:19, 20:22, 56:58)]), c(2, 3, 5, 6, 9:19, 20:22, 56:58)]
ppr=preProcess(train2, method=c('center', 'scale'))
trainx=predict(ppr, newdata=train2)
trainx=data.frame(trainx, train2[,52:53])

testx=predict(ppr, newdata=test2)
testx=data.frame(testx, test2[,52:53])


# In[ ]:


r0 = ranger(Surv(tdeath, death)~age+Sex+Race2+BMI+Toba+Htn+HxDM+HxMIStr+revasc+BP.s+BP.d+
            LDL+HDL+TChol+Trig+HMG+asprin+antihyp+study.1,
           data=train2, seed=354, replace=F, verbose=T, num.threads=10, num.trees=100, importance="none")

