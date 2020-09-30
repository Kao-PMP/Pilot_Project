
setwd('/mnt/workspace/DCRI/Data')
library(dplyr)

aimhigh=read.csv('aimhigh.csv')
bari=read.csv('bari2d.csv')
allhat=read.csv('allhat.csv')
accord=read.csv('accord.csv')

colnames(aimhigh)
nrow(aimhigh)
table(aimhigh$study.1)

colnames(accord)
nrow(accord)
table(accord$study.1)

colnames(allhat)
nrow(allhat)
table(allhat$study.1)

colnames(bari)
nrow(bari)
table(bari$study.1)

head(aimhigh)

dat=rbind(aimhigh, allhat) %>% rbind(accord) %>% rbind(bari)
head(dat)
table(dat$study.1, useNA="ifany")
table(dat$antihyp)
nrow(dat)
summary(dat$BP.s)
table(dat$dthStat, dat$study.1, useNA="ifany")

missing=subset(dat, person_id==NA)
nrow(missing)

hyp=subset(dat, BP.s>=130 | BP.d>=80 | Htn==1 | antihyp==1)

hyp$type_hyp=ifelse(hyp$BP.s>=130 & hyp$BP.d<80, 'Systolic',
    ifelse(hyp$BP.s<130 & hyp$BP.d>=80,'Diastiolic',
           ifelse(hyp$BP.s>=130 & hyp$BP.d>=80,'Both','Controlled')))
table(hyp$Htn, hyp$study.1, useNA="ifany")
hyp$Htn=ifelse(is.na(hyp$Htn) & hyp$study.1=='ACCORD', 0, hyp$Htn)
table(hyp$Htn, hyp$study.1, useNA="ifany")

table(hyp$study.1)
table(hyp$antihyp)
table(hyp$type_hyp)
colnames(hyp)

write.csv(hyp, 'analysis_ds.csv', row.names=FALSE)

anly=read.csv('analysis_ds.csv')

table(anly$study.1)
nrow(anly)

sub=anly[anly$study.1=='',]

nrow(sub)
head(sub)
