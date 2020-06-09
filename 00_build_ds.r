
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
#head(dat)
#table(dat$study.1, useNA="ifany")
#table(dat$antihyp)
#nrow(dat)
#summary(dat$BP.s)
#table(dat$dthStat, dat$study.1, useNA="ifany")

#missing=subset(dat, person_id==NA)
#nrow(missing)

hyp=subset(dat, BP.s>=130 | BP.d>=80 | Htn==1 | antihyp==1)

hyp$type_hyp=ifelse(hyp$BP.s>=130 & hyp$BP.d<80, 'Systolic',
    ifelse(hyp$BP.s<130 & hyp$BP.d>=80,'Diastiolic',
           ifelse(hyp$BP.s>=130 & hyp$BP.d>=80,'Both','Controlled')))
#table(hyp$Htn, hyp$study.1, useNA="ifany")
hyp$Htn=ifelse(is.na(hyp$Htn) & hyp$study.1=='ACCORD', 0, hyp$Htn)
#table(hyp$Htn, hyp$study.1, useNA="ifany")

hyp$dthStat=ifelse(is.na(hyp$dthStat) & hyp$study.1=='AIMHIGH', 0, hyp$dthStat)
hyp$dthStat=ifelse(hyp$study.1=='ACCORD', 1-hyp$dthStat, hyp$dthStat)
#https://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame

hyp %>%
    select(study.1, BP.s, BP.d, dthDays) %>%
    group_by(study.1) %>%
    summarise(days_mean = mean(dthDays, na.rm=T), days_median = median(dthDays, na.rm=T), min_days=min(dthDays, na.rm=T), max_days=max(dthDays, na.rm=T))

#1825 max days - censor for 5 year death
hyp$death = ifelse(hyp$dthDays>1825, 0, hyp$dthStat)
hyp$tdeath = ifelse(hyp$dthDays>1825, 1825, hyp$dthDays)
hyp$tdeath = ifelse(hyp$dthDays==0, 0.1, hyp$dthDays)
hyp %>%
    select(study.1, BP.s, BP.d, tdeath) %>%
    group_by(study.1) %>%
    summarise(days_mean = mean(tdeath, na.rm=T), days_median = median(tdeath, na.rm=T), min_days=min(tdeath, na.rm=T), max_days=max(tdeath, na.rm=T))


set.seed(150)
train <- hyp %>% 
    select(study.1, person_id) %>%
    group_by(study.1) %>%
    sample_frac(0.75)

test <- hyp %>%
    select(study.1, person_id) %>%
   filter(!person_id %in% train$person_id)

train$train=1
test$test=1


head(train)
head(test)
nrow(train)
nrow(test)

head(hyp)
final = hyp %>% left_join(train, by=c("person_id",'study.1')) %>% left_join(test, by=c("person_id",'study.1'))
head(final)

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
