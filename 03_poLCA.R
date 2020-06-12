
#library(cluster)
library(caret)
#library(purrr)
library(dplyr)
#library(boot)
#library(table1)
#library(Hmisc)
#library(sfsmisc)
library(data.table)
source("poLCA_iterate2.R")

data=read.csv("Data/analysis_ds.csv")
##train=data[which(data$train==1),]

# exclude inputs to derived values
train_columns=c(1:3, 5, 6, 9:19, 20:22, 50, 51)
# [1] "person_id" "age"       "Sex"       "Race2"     "BMI"       "Toba"     
# [7] "Htn"       "HxDM"      "HxMIStr"   "revasc"    "BP.s"      "BP.d"     
#[13] "LDL"       "HDL"       "TChol"     "Trig"      "HMG"       "asprin"   
#[19] "antihyp"   "study.1"   "type_hyp" 
####base_nmiss=train[, train_columns]
base_nmiss=data[, train_columns]

# print(summary(cut(base_nmiss$LDL, 10)))


# poLCA needs categorized variables :BMI, BP.s, BP.d, LDL, HDL, TChol, Trig
base_nmiss$age <- cut(base_nmiss$age, breaks=c(0, 55, 65, Inf), include.lowest=T, labels=c("low", "med", "high"))
base_nmiss$BMI <- cut(base_nmiss$BMI, breaks=c(0, 18.5, 25, 30, Inf), include.lowest=T, 
	 		labels=c("underweight", "normal", "overweight", "obese"))
base_nmiss$BP.s <- cut(base_nmiss$BP.s, breaks=c(0, 90, 140, Inf), include.lowest=T, labels=c("low", "med", "high"))

#base_nmiss$BP.d <- cut(base_nmiss$BP.d, breaks=c(0, 90, 140, Inf), include.lowest=T, labels=c("low", "med", "high"))
# no high BP.d levels cause length of dimnames error from poLCA?
base_nmiss$BP.d <- cut(base_nmiss$BP.d, breaks=c(0, 90, Inf), include.lowest=T, labels=c("low", "med"))

base_nmiss$LDL <- cut(base_nmiss$LDL, 3, include.lowest=T, labels=c("low", "med", "high"))
base_nmiss$HDL <- cut(base_nmiss$HDL, 3, include.lowest=T, labels=c("low", "med", "high"))
base_nmiss$TChol <- cut(base_nmiss$TChol, 3, include.lowest=T, labels=c("low", "med", "high"))
base_nmiss$Trig <- cut(base_nmiss$Trig, 3, include.lowest=T, labels=c("low", "med", "high"))

for (col in names(base_nmiss)) {
    base_nmiss[,col] = as.factor(base_nmiss[,col])
}

base_nmiss=base_nmiss[complete.cases(base_nmiss),]
print(summary(base_nmiss))
polca_formula_all <- cbind(age, Sex, Race2, BMI, Toba, Htn, HxDM, 
	HxMIStr, revasc, BP.s, BP.d, LDL, HDL, TChol, Trig, 
	HMG, asprin, antihyp, study.1, type_hyp)~1 
polca_formula_1 <- cbind(age, Sex, Race2, BMI, Toba, Htn, HxDM, 
	HxMIStr, revasc, BP.s, BP.d, LDL, HDL, TChol, Trig, 
	HMG, asprin, antihyp )~1 
#polca_formula_2 <- cbind(age, Sex, BMI, Toba, HxMIStr, revasc, BP.s, BP.d, LDL, HDL, TChol, Trig, antihyp )~1 
# only X^2 shows 3 and 7
#
polca_formula_3 <- cbind(age, Sex, BMI, Toba, HxMIStr, revasc, LDL, antihyp )~1 
#model <- poLCA_iterate(f=polca_formula_3, dat=base_nmiss, maxclass=8, reps=10)
# much more clear 3 in AiC and BiC
#
#polca_formula_4 <- cbind(age, Sex, Race2, BMI, Toba, HxMIStr, revasc, LDL, antihyp )~1 
# not so good, perhaps race instead of race2?

polca_model <- poLCA(f=polca_formula_3, dat=base_nmiss, nclass=3, nrep=20, na.rm=FALSE, verbose=T, graphs=T)
base_nmiss$polca_class <- polca_model$predclass
print(summary(base_nmiss[base_nmiss$polca_class == 1, ]))
print(summary(base_nmiss[base_nmiss$polca_class == 2, ]))
print(summary(base_nmiss[base_nmiss$polca_class == 3, ]))

set.seed(123)

# Show the clusters.  XXX doesn't work for factors.
#print("a------------------------------------")
#base_nmiss %>%
#  group_by(polca_class) %>%
#  summarise_all("mean")

warnings()

# Show how the clusters split out among the studies.
check <- data.frame(person_id=base_nmiss$person_id, study=base_nmiss$study.1, cluster0=base_nmiss$polca_class)
table(check$study, check$cluster0)

