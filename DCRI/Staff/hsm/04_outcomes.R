# Purpose: Cox model with SBP and DBP in relation to all cause death
# Programmer:Hillary Mulder
# Date:August 2019
#
# Modification:
#############################

library(tidyverse)
#library(ranger)
library(survival)
library(dplyr)
library(sampling)

hyp=read.csv("/mnt/workspace/DCRI/Data/analysis_ds.csv")
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

#make the matrix a bit smaller
for_cox=hyp[, c('person_id','BP.s','BP.d', 'death','tdeath', 'study.1')]
#pulse pressure computation
for_cox$pp = for_cox$BP.s - for_cox$BP.d

set.seed(150)
train <- for_cox %>%
    group_by(study.1) %>%
    sample_frac(0.75)

test <- for_cox %>%
    filter(!person_id %in% train$person_id)

cox1=coxph(Surv(tdeath, death)~BP.s + BP.d, data=train)
predictions = predict(cox1, test, type='risk')
#to do: split into train/test (75/25) sets using stratified random sampling with trials as stratification variable
#cov=test[,c('BP.s','BP.d')]
#s2 = predictProb.coxph(cox1, Surv(test$tdeath, test$death), cov, times=365 )
#full=cbind(test, s2)
#full$failure=1-full$s2

library(tidyr)
library(lattice)

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
