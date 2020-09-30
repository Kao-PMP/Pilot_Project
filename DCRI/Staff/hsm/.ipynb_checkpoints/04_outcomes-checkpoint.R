# Purpose:
# Programmer:
# Date:
#
# Modification:
#############################

library(tidyverse)
library(ranger)

data=read.csv("/mnt/workspace/DCRI/Data/analysis_ds.csv")

#to do: split into train/test (75/25) sets using stratified random sampling with trials as stratification variable
#then use 10 fold cv within the training set


