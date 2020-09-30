
library(dplyr)
library(boot)
library(table1)
library(Hmisc)

data=read.csv('/mnt/workspace/DCRI/Data/analysis_ds_clusters.csv')


table(data$cluster1, data$cluster2)

data$Sex = factor(data$Sex, levels=1:2, labels=c("Male", "Female"))
data$Race2 = factor(data$Race2, levels=1:2, labels=c('Non-Black', 'Black'))
data$Toba = factor(data$Toba, levels=0:1, labels=c('No', 'Yes'))
data$Htn = factor(data$Htn, levels=0:1, labels=c('No', 'Yes'))
data$HxDM = factor(data$HxDM, levels=0:1, labels=c('No', 'Yes'))
#all$HxCVD = factor(all$HxCVD, levels=0:1, labels=c('No', 'Yes'))
data$HxMIStr = factor(data$HxMIStr, levels=0:1, labels=c('No', 'Yes'))
data$HMG = factor(data$HMG, levels=0:1, labels=c('No', 'Yes'))
data$asprin = factor(data$asprin, levels=0:1, labels=c('No', 'Yes'))
data$revasc = factor(data$revasc, levels=0:1, labels=c('No', 'Yes'))
data$cluster1 = factor(data$cluster1, levels=1:4, labels=c('G1', 'G2','G3','G4'))
data$cluster2 = factor(data$cluster2, levels=1:5, labels=c('G1', 'G2','G3','G4','G5'))

#add labels
label(data$study.1)="Clinical Trial"
label(data$type_hyp)='Type of Hypertension'
label(data$age)="Age (years)"
label(data$Sex)="Sex"
label(data$Race2)="Race"
#label(data$weight)='Weight (kg)'
#label(data$height)="Height (cm)"
label(data$Toba)='Tobacco Use'
label(data$Htn)="Hypertension"
label(data$HxDM)='History of Diabetes'
#label(dat$HxCVD)='History of Cardiovascular Disease'
label(data$HxMIStr)='History of MI or Stroke'
label(data$BP.s)="Systolic Blood Pressure"
label(data$BP.d)='Diastolic Blood Pressure'
label(data$TChol)='Total Cholesterol'
label(data$Trig)='Triglycerides'
label(data$HMG)='Statins'
label(data$asprin)='Aspirin'
label(data$revasc)='Prior Revascularization'

train=data[which(data$train==1 & is.na(data$cluster1)==F ),]

tab1=table1( ~age + Sex + Race2 + BMI + 
               Toba + Htn + HxDM + HxMIStr + revasc
            + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin + study.1
           | cluster1, data)
write.table(tab1, file='/mnt/workspace/DCRI/Output/03_cluster1.html', quote = FALSE,col.names = FALSE, row.names = FALSE)

tab2=table1( ~age + Sex + Race2 + BMI + 
               Toba + Htn + HxDM + HxMIStr + revasc
            + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin + study.1
           | cluster2, data)
write.table(tab2, file='/mnt/workspace/DCRI/Output/03_cluster2.html', quote = FALSE,col.names = FALSE, row.names = FALSE)

tab3=table1( ~age + Sex + Race2 + BMI + 
               Toba + Htn + HxDM + HxMIStr + revasc
             + BP.s + BP.d + LDL + HDL + TChol + Trig + HMG + asprin + study.1
             | cluster1, train)
write.table(tab3, file='/mnt/workspace/DCRI/Output/03_cluster1_train.html', quote = FALSE,col.names = FALSE, row.names = FALSE)

