###############################################
#Title: Baseline demographics
#Programmer: Hillary Mulder
#
#Date: 3/12/19
#Notes:
################################################

setwd('/mnt/workspace/DCRI/Data')
library(dplyr)
library(boot)
library(table1)
library(Hmisc)

#will update read.csv with new file once complete
all=read.csv('analysis_ds.csv')

#hyp=dat[dat$BP.s>=130 | dat$BP.d>=80 | dat$Htn==1 | dat$antihyp==1, ]

#hyp$type_hyp=ifelse(hyp$BP.s>=130 & hyp$BP.d<80, 'Systolic',
#    ifelse(hyp$BP.s<130 & hyp$BP.d>=80,'Diastiolic',
#           ifelse(hyp$BP.s>=130 & hyp$BP.d>=80,'Both','Controlled')))


#dat[dat$study=='BARI2D', 'HxDM'] = 1
#dat[dat$study=='ALLHAT', 'Htn'] = 1
#dat[dat$study=='ACCORD', 'BMI'] = dat[dat$study=='ACCORD', 'weight'] / (dat[dat$study=='ACCORD', 'height']/100)**2
#dat[dat$study=='AIMHIGH', 'TChol'] = dat[dat$study=='AIMHIGH', 'LDL'] + dat[dat$study=='AIMHIGH', 'HDL'] + dat[dat$study=='AIMHIGH', 'Trig']*4/5

#convert to factors
#sex=2 is female
dat$Sex = factor(dat$Sex, levels=1:2, labels=c("Male", "Female"))
dat$Race = factor(dat$Race, levels=1:2, labels=c('Non-Black', 'Black'))
dat$Toba = factor(dat$Toba, levels=0:1, labels=c('No', 'Yes'))
dat$Htn = factor(dat$Htn, levels=0:1, labels=c('No', 'Yes'))
dat$HxDM = factor(dat$HxDM, levels=0:1, labels=c('No', 'Yes'))
#dat$HxCVD = factor(dat$HxCVD, levels=0:1, labels=c('No', 'Yes'))
dat$HxMIStr = factor(dat$HxMIStr, levels=0:1, labels=c('No', 'Yes'))
dat$HMG = factor(dat$HMG, levels=0:1, labels=c('No', 'Yes'))
dat$asprin = factor(dat$asprin, levels=0:1, labels=c('No', 'Yes'))

#add labels
label(dat$study)="Clinical Trial"
label(dat$type_hyp)='Type of Hypertension'
label(dat$age)="Age (years)"
label(dat$Sex)="Sex"
label(dat$Race)="Race"
label(dat$weight)='Weight (kg)'
label(dat$height)="Height (cm)"
label(dat$Toba)='Tobacco Use'
label(dat$Htn)="Hypertension"
label(dat$HxDM)='History of Diabetes'
#label(dat$HxCVD)='History of Cardiovascular Disease'
label(dat$HxMIStr)='History of MI or Stroke'
label(dat$BP.s)="Systolic Blood Pressure"
label(dat$BP.d)='Diastolic Blood Pressure'
label(dat$TChol)='Total Cholesterol'
label(dat$Trig)='Triglycerides'
label(dat$HMG)='Statins'
label(dat$asprin)='Aspirin'

tab1=table1( ~age + Sex + Race + BMI + weight + height +
               Toba + Htn + HxDM + HxMIStr + revasc
            + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin
           | study, data=hyp)
write.csv(tab1, file='~/DCRI/Output/01a_baseline.csv')

accord_tb=table1( ~age + Sex + Race + BMI + weight + height +
                      Toba + Htn + HxDM + HxMIStr + revasc
                  + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin
                  | type_hyp, data=hyp[hyp$study=='ACCORD',])
write.csv(accord_tb, file='~/DCRI/Output/01b_accord.csv')

aimhigh_tb=table1( ~age + Sex + Race + BMI + weight + height +
                      Toba + Htn + HxDM + HxMIStr + revasc
                  + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin
                  | type_hyp, data=hyp[hyp$study=='AIMHIGH',])
write.csv(aimhigh_tb, file='~/DCRI/Output/01c_aimhigh.csv')

allhat_tb=table1( ~age + Sex + Race + BMI + weight + height +
                      Toba + Htn + HxDM + HxMIStr + revasc
                  + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin
                  | type_hyp, data=hyp[hyp$study=='ALLHAT',])
write.csv(allhat_tb, file='~/DCRI/Output/01d_allhat.csv')

bari_tb=table1( ~age + Sex + Race + BMI + weight + height +
                      Toba + Htn + HxDM + HxMIStr + revasc
                  + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin
                  | type_hyp, data=hyp[hyp$study=='BARI2D',])
write.csv(bari_tb, file='~/DCRI/Output/01e_bari2d.csv')

