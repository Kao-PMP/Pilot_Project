#!/usr/bin/env python
# coding: utf-8

# Purpose: MLP - death as binary  
# Programmer:Hillary Mulder  
# Date: October 2019  
# Modification History:  

# In[15]:


library(RSNNS)
library(caret)


# In[43]:


cStat <-  function(pred, class, FLIP = TRUE){
  tab <- table(class)
  cind <- wilcox.test(pred ~ class)$statistic/tab[1]/tab[2]
  if(FLIP){
    if(cind < .5) cind <- 1 - cind
  }
  return(cind)
}


# In[29]:


hyp=read.csv("Data/analysis_ds.csv")
hyp$allhat=ifelse(hyp$study.1=='ALLHAT', 1, 0)
hyp$aimhigh=ifelse(hyp$study.1=='AIMHIGH', 1, 0)
hyp$accord=ifelse(hyp$study.1=='ACCORD', 1, 0)



train=hyp[which(hyp$train==1),]
test=hyp[which(hyp$test==1),]

#colnames(train)
train2=train[complete.cases(train[, c(2, 3, 5, 6, 9:19, 20:22, 52, 56:58)]), c(2, 3, 5, 6, 9:19, 20:22, 52, 56:58)]
train_input=as.matrix(train2[,c(1:18, 20:22)])
train_target=decodeClassLabels(train2[,19])
#train_target
#colnames(train2)
#colnames(train_input)
test2=test[complete.cases(test[, c(2, 3, 5, 6, 9:19, 20:22, 52, 56:58)]), c(2, 3, 5, 6, 9:19, 20:22, 52, 56:58)]
test_input=as.matrix(test2[,c(1:18, 20:22)])
test_target=decodeClassLabels(test2[,19])


# In[17]:


#train_input[,c(1,4,10:15)]=scale(train_input[,c(1,4,10:15)], center=FALSE)
#test_input[,c(1,4,10:15)]=scale(test_input[,c(1,4,10:15)], center=FALSE)
ppr=preProcess(train_input, method=c('center', 'scale'))
train_inputx=predict(ppr, newdata=train_input)
test_inputx=predict(ppr, newdata=test_input)


# In[30]:


summary(train_inputx)
head(train_target)


# In[31]:


mlp1=mlp(train_inputx, train_target, size=5, learnFuncParams=c(0.1), maxit=50, outputActFunc="Act_Logistic",
        inputsTest=test_inputx, targetsTest=test_target)


# In[32]:


summary(mlp1)
mlp1
weightMatrix(mlp1)
extractNetInfo(mlp1)


# In[45]:


par(mfrow=c(2,2))
plotIterativeError(mlp1)

predictions <- predict(mlp1, test_inputx)

head(fitted.values(mlp1))
#dim(predictions)
plotRegressionError(predictions[,2], test_target[,2])

plotROC(fitted.values(mlp1)[,2], train_target[,2])
plotROC(predictions[,2], test_target[,2])

cStat(fitted.values(mlp1)[,2], train_target[,2])
cStat(predictions[,2], test_target[,2])


# In[50]:


mlp2=mlp(train_inputx, train_target, size=5, learnFunc='Rprop', learnFuncParams=c(0.1), maxit=50, outputActFunc="Act_Logistic",
        inputsTest=test_inputx, targetsTest=test_target)


# In[51]:


summary(mlp2)
weightMatrix(mlp2)
extractNetInfo(mlp2)
predictions2 <- predict(mlp2, test_inputx)
cStat(fitted.values(mlp2)[,2], train_target[,2])
cStat(predictions2[,2], test_target[,2])
plotROC(predictions2[,2], test_target[,2])

