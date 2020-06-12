
library(caret)
#library(purrr)
library(dplyr)
#library(boot)
#library(table1)
#library(Hmisc)
#library(sfsmisc)
library(data.table)
library(dbscan)

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
base_nmiss=base_nmiss[complete.cases(base_nmiss),]
print(summary(base_nmiss))
#fewer_columns=c("person_id", "Htn", "BP.s", "BP.d", "HDL", "TChol", "Trig", "HMG", "aspirin",  "study.1", "type_hype")
fewer_columns=c(1, 7, 11,  14, 15, 16, 17, 18, 20,21 )
base_nmiss <- base_nmiss[,-fewer_columns]

numeric_data <- as.matrix(sapply(base_nmiss, as.numeric))
print(summary(numeric_data))
print("a----------------")
db <- dbscan(numeric_data, eps=13, minPts=4)
# eps # clusters  # noise points
#   2   121       32353
#   4   158        4886     
#   5    46        2049
#   6    17         961
#   7     3         457
#   8     3         250
#  10     4          94
#  12     3          46
#  13     3          36
#  14     2          23
#  15     1          14
print("b----------------")
print(db)
print("c----------------")
base_nmiss$cluster = db$cluster
print("--- 1 ---")
print(summary(base_nmiss[base_nmiss$cluster == 1, ]))
print("--- 2 ---")
print(summary(base_nmiss[base_nmiss$cluster == 2, ]))
print("--- 3 ---")
print(summary(base_nmiss[base_nmiss$cluster == 3, ]))
print("d----------------")
print(pairs(numeric_data, col=db$cluster + 1L))
#lof <- lof(numeric_data, k=4)
#print(lof)


#base_nmiss$polca_class <- polca_model$predclass
#print(summary(base_nmiss[base_nmiss$polca_class == 1, ]))
#print(summary(base_nmiss[base_nmiss$polca_class == 2, ]))
#print(summary(base_nmiss[base_nmiss$polca_class == 3, ]))

# Show how the clusters split out among the studies.
#check <- data.frame(person_id=base_nmiss$person_id, study=base_nmiss$study.1, cluster0=base_nmiss$polca_class)
#table(check$study, check$cluster0)

