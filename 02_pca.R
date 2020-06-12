
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

data=read.csv("Data/analysis_ds.csv")

# exclude inputs to derived values
print(names(data))
#train_columns=c(1:3, 5, 6, 9:19, 20:22, 50, 51)
#train_columns=c(2,3, 5, 6, 9:19, 20:22)
train_columns=c(2,3, 6, 7,8, 9, 13,14) # age, sex, bmi, toba, htm, hxdm, 
#train_columns_2=c(2,3, 4,6, 9:19)
# [1] "person_id" "age"       "Sex"       "Race2"     "BMI"       "Toba"     
# [7] "Htn"       "HxDM"      "HxMIStr"   "revasc"    "BP.s"      "BP.d"     
#[13] "LDL"       "HDL"       "TChol"     "Trig"      "HMG"       "asprin"   
#[19] "antihyp"   "study.1"   "type_hyp" 
base_nmiss=data[, train_columns]
print(names(base_nmiss))
base_nmiss=base_nmiss[complete.cases(base_nmiss),]
nms <- names(base_nmiss)
numeric_data <- as.matrix(sapply(base_nmiss, as.numeric))
names(numeric_data) <- nms
print(summary(numeric_data))

pca <- prcomp(numeric_data, center=T, scale. = T)
summary(pca)
print(pca$rotation)
ggbiplot(pca)
print(pca$center)
#print(pca$scale)
#print(pca$x)


