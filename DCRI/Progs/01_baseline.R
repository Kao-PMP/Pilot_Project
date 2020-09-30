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

all=read.csv('analysis_ds.csv')

#convert to factors
#sex=2 is female
all$Sex = factor(all$Sex, levels=1:2, labels=c("Male", "Female"))
all$Race2 = factor(all$Race2, levels=1:2, labels=c('Non-Black', 'Black'))
all$Toba = factor(all$Toba, levels=0:1, labels=c('No', 'Yes'))
all$Htn = factor(all$Htn, levels=0:1, labels=c('No', 'Yes'))
all$HxDM = factor(all$HxDM, levels=0:1, labels=c('No', 'Yes'))
#all$HxCVD = factor(all$HxCVD, levels=0:1, labels=c('No', 'Yes'))
all$HxMIStr = factor(all$HxMIStr, levels=0:1, labels=c('No', 'Yes'))
all$HMG = factor(all$HMG, levels=0:1, labels=c('No', 'Yes'))
all$asprin = factor(all$asprin, levels=0:1, labels=c('No', 'Yes'))
all$revasc = factor(all$revasc, levels=0:1, labels=c('No', 'Yes'))

#add labels
label(all$study.1)="Clinical Trial"
label(all$type_hyp)='Type of Hypertension'
label(all$age)="Age (years)"
label(all$Sex)="Sex"
label(all$Race2)="Race"
label(all$weight)='Weight (kg)'
label(all$height)="Height (cm)"
label(all$Toba)='Tobacco Use'
label(all$Htn)="Hypertension"
label(all$HxDM)='History of Diabetes'
#label(dat$HxCVD)='History of Cardiovascular Disease'
label(all$HxMIStr)='History of MI or Stroke'
label(all$BP.s)="Systolic Blood Pressure"
label(all$BP.d)='Diastolic Blood Pressure'
label(all$TChol)='Total Cholesterol'
label(all$Trig)='Triglycerides'
label(all$HMG)='Statins'
label(all$asprin)='Aspirin'
label(all$revasc)='Prior Revascularization'

tab1=table1( ~age + Sex + Race2 + BMI + weight + height +
               Toba + Htn + HxDM + HxMIStr + revasc
            + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin
           | study.1, data=all)
write.table(tab1, file='/mnt/workspace/DCRI/Output/01a_baselinev2.html', quote = FALSE,col.names = FALSE, row.names = FALSE)
#write.csv(tab1, file='~/DCRI/Output/01a_baseline.csv', row.names = F)

accord_tb=table1( ~age + Sex + Race2 + BMI + weight + height +
                      Toba + Htn + HxDM + HxMIStr + revasc
                  + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin
                  | type_hyp, data=all[which(all$study.1=='ACCORD'),])
write.table(accord_tb, file='/mnt/workspace/DCRI/Output/01b_accordv2.html', quote = FALSE,col.names = FALSE, row.names = FALSE)
#write.csv(accord_tb, file='~/DCRI/Output/01b_accord.csv')

aimhigh_tb=table1( ~age + Sex + Race2 + BMI + weight + height +
                      Toba + Htn + HxDM + HxMIStr + revasc
                  + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin
                  | type_hyp, data=all[which(all$study.1=='AIMHIGH'),])
write.table(aimhigh_tb, file='/mnt/workspace/DCRI/Output/01c_aimhighv2.html', quote = FALSE,col.names = FALSE, row.names = FALSE)
#write.csv(aimhigh_tb, file='~/DCRI/Output/01c_aimhigh.csv')

allhat_tb=table1( ~age + Sex + Race2 + BMI + weight + height +
                      Toba + Htn + HxDM + HxMIStr + revasc
                  + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin
                  | type_hyp, data=all[which(all$study.1=='ALLHAT'),])
write.table(allhat_tb, file='/mnt/workspace/DCRI/Output/01d_allhatv2.html', quote = FALSE,col.names = FALSE, row.names = FALSE)
#write.csv(allhat_tb, file='~/DCRI/Output/01d_allhat.csv')

bari_tb=table1( ~age + Sex + Race2 + BMI + weight + height +
                      Toba + Htn + HxDM + HxMIStr + revasc
                  + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin
                  | type_hyp, data=all[which(all$study.1=='BARI2D'),])
write.table(bari_tb, file='/mnt/workspace/DCRI/Output/01e_bari2dv2.html', quote = FALSE,col.names = FALSE, row.names = FALSE)
#write.csv(bari_tb, file='~/DCRI/Output/01e_bari2d.csv')

