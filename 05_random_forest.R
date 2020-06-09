library(ranger)
library(survival)
library(dplyr)
library(caret)

hyp=read.csv("Data/analysis_ds.csv")
hyp$allhat=ifelse(hyp$study.1=='ALLHAT', 1, 0)
hyp$aimhigh=ifelse(hyp$study.1=='AIMHIGH', 1, 0)
hyp$accord=ifelse(hyp$study.1=='ACCORD', 1, 0)

train=hyp[which(hyp$train==1),]
test=hyp[which(hyp$test==1),]

#train2=train[complete.cases(train[, c(1:3, 5, 6, 9:19, 20:22, 50:53)]), c(1:3, 5, 6, 9:19, 20:22, 50:53)]
#colnames(train2)
train2=train[complete.cases(train[, c(2, 3, 5, 6, 9:19, 20:22,52,53, 56:58)]), c(2, 3, 5, 6, 9:19, 20:22,52,53, 56:58)]
colnames(train2)
test2=test[complete.cases(test[, c(2, 3, 5, 6, 9:19, 20:22,52,53, 56:58)]), c(2, 3, 5, 6, 9:19, 20:22,52,53, 56:58)]
ppr=preProcess(train2[,c(1:18, 21:23)], method=c('center', 'scale'))
trainx=predict(ppr, newdata=train2[,c(1:18, 21:23)])
trainx2=data.frame(trainx, train2[,19:20])

testx=predict(ppr, newdata=test2[,c(1:18, 21:23)])
testx=data.frame(testx, test2[,19:20])


r0 = ranger(Surv(tdeath, death)~., data=trainx2, seed=354, replace=F, verbose=T, 
            num.threads=10, num.trees=50, importance="none", probability=T)


