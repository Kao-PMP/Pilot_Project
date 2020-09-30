
library(cluster)
library(caret)
library(purrr)
library(dplyr)
library(boot)
library(table1)
library(Hmisc)
library(sfsmisc)

data=read.csv("Data/analysis_ds.csv")
train=data[which(data$train==1),]

# exclude inputs to derived values
train_columns=c(1:3, 5, 6, 9:19, 20:22, 50, 51)
#base_nmiss=train[complete.cases(train[, train_columns]), train_columns]
base_nmiss=train[complete.cases(train[, c(1:3, 5, 6, 9:19, 20:22, 50, 51)]), c(1:3, 5, 6, 9:19, 20:22, 50, 51)]
studyn=base_nmiss$study.1
enrolid=base_nmiss[,1]

# remove Race2, Htn, HxDM, type_hype, study.1
base_nmiss=base_nmiss[, -c(1,7,8,20,21)]
print(names(base_nmiss))
####polca_formula_3 <- cbind(age, Sex, BMI, Toba, HxMIStr, revasc, LDL, antihyp )~1 
base_nmiss=base_nmiss[, -c(3,8,9,11,12,13,14,15)]
print(names(base_nmiss))
ppr=preProcess(base_nmiss, method=c('center','scale'))


#apply transformations
base_scale=predict(ppr, newdata=base_nmiss)

#base_scale=scale(base_nmiss)

# JUST USE IT ALL
test=data
#test=data[which(data$test==1),]
print(dim(test))
test_nmiss=test[complete.cases(test[, c(1:3, 5, 6, 9:19, 20:22, 50, 51)]), c(1:3, 5, 6, 9:19, 20:22, 50, 51)]
test_studyn=test_nmiss$study.1
test_enrolid=test_nmiss[,1]
test_nmiss=test_nmiss[, -c(1,7,8,20,21)]
#test_scale=scale(_nmiss)
test_scale=predict(ppr, newdata=test_nmiss)
set.seed(123)

kmax=15

## function to compute total within-cluster sum of square 
#wss <- function(k) {
#  kmeans(base_scale, k, iter.max=20, nstart = 4)$tot.withinss
#}
#
## Compute and plot wss for k = 1 to k = 15
#k.values <- 1:kmax
#
## extract wss for 1-15 clusters
#wss_values <- map_dbl(k.values, wss)
#
#plot(k.values, wss_values,
#       type="b", pch = 19, frame = FALSE, 
#       xlab="Number of clusters K",
#       ylab="Total within-clusters sum of squares")

# going to use 3 or 5 clusters

set.seed(123)
print(dim(base_scale))
cluster1 <- kmeans(base_scale, 3, iter.max=15, nstart = 6)
cluster2 <- kmeans(base_scale, 5, iter.max=15, nstart = 6)


# Show the clusters.
base_nmiss %>%
  mutate(Cluster = cluster1$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#base_nmiss %>%
#  mutate(Cluster = cluster2$cluster) %>%
#  group_by(Cluster) %>%
#  summarise_all("mean")

#test_nmiss=test[complete.cases(test[, c(1:3, 5, 6, 9:19, 20:22, 50, 51)]), c(1:3, 5, 6, 9:19, 20:22, 50, 51)]
####polca_formula_3 <- cbind(age, Sex, BMI, Toba, HxMIStr, revasc, LDL, antihyp )~1 
#base_nmiss$Sex <- as.factor(base_nmiss$Sex)
#base_nmiss$Toba <- as.factor(base_nmiss$Toba)
#base_nmiss$HxMIStr <- as.factor(base_nmiss$HxMIStr)
#base_nmiss$revasc <- as.factor(base_nmiss$revasc)
#base_nmiss$antihyp <- as.factor(base_nmiss$antihyp)
#base_nmiss$cluster <- as.factor(cluster1$cluster)
base_nmiss$Sex <- as.numeric(base_nmiss$Sex)
base_nmiss$Toba <- as.numeric(base_nmiss$Toba)
base_nmiss$HxMIStr <- as.numeric(base_nmiss$HxMIStr)
base_nmiss$revasc <- as.numeric(base_nmiss$revasc)
base_nmiss$antihyp <- as.numeric(base_nmiss$antihyp)
base_nmiss$cluster <- as.numeric(cluster1$cluster)
base_nmiss <- base_nmiss[complete.cases(base_nmiss),]
print(summary(base_nmiss[base_nmiss$cluster == 1,]))
print(summary(base_nmiss[base_nmiss$cluster == 2,]))
print(summary(base_nmiss[base_nmiss$cluster == 3,]))


# Show how the clusters split out among the studies.
#table(base_nmiss$studyn, final$cluster)
#head(check)
check=data.frame(person_id=enrolid, study=studyn, cluster1=cluster1$cluster, cluster2=cluster2$cluster)
table(check$study, check$cluster1)
table(check$study, check$cluster2)
table(check$study)
table(check$cluster2)



stop("OK")



# Predict...stuff.
# Hang on, are we trying to predict study membership from the clusters?
# ...what does that mean?
#table(base_nmiss$studyn, final$cluster)
predict.kmeans <- function(object,
                           newdata,
                           method = c("centers", "classes")) {
  method <- match.arg(method)
  centers <- object$centers
  ss_by_center <- apply(centers, 1, function(x) {
    colSums((t(newdata) - x) ^ 2)
  })
  best_clusters <- apply(ss_by_center, 1, which.min)
  if (method == "centers") {
    centers[best_clusters, ]
  } else {
    best_clusters
  }
}

# predict by classes
# for refernce the test_scale is composed of these columns: Race2, Htn, HxDM, type_hype, study.1
# TODO might be more interesting: Race2, HxDM, type_hype
test_nmiss2=test_nmiss[, -c(1,7,8,20,21)]
#test_scale=scale(_nmiss)
test_scale=predict(ppr, newdata=test_nmiss)
print("classes")
pred1=predict.kmeans(cluster1, newdata=test_scale, method="classes")
pred2=predict.kmeans(cluster2, newdata=test_scale, method="classes")
std=data.frame(person_id=test_enrolid, study=test_studyn, cluster1=pred1, cluster2=pred2)
table(std$study, std$cluster1)
#table(std$study, std$cluster2)

# predict by centers
print("centers")
pred1=predict.kmeans(cluster1, newdata=test_scale, method="centers")
pred2=predict.kmeans(cluster2, newdata=test_scale, method="centers")
std=data.frame(person_id=test_enrolid, study=test_studyn, cluster1=pred1, cluster2=pred2)
table(std$study, std$cluster1)
#table(std$study, std$cluster2)



#train2=left_join(x=train, y=check, by='person_id')
#colnames(train)
#head(train)
#test2=left_join(x=test, y=std, by='person_id')

#all=rbind(train2, test2)
#head(all)
#write.csv(all, file='Data/analysis_ds_clusters.csv', quote = FALSE,  row.names = FALSE)
