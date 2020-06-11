
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
ppr=preProcess(base_nmiss, method=c('center','scale'))


#apply transformations
base_scale=predict(ppr, newdata=base_nmiss)

#base_scale=scale(base_nmiss)

test=data[which(data$test==1),]
test_nmiss=test[complete.cases(test[, c(1:3, 5, 6, 9:19, 20:22, 50, 51)]), c(1:3, 5, 6, 9:19, 20:22, 50, 51)]
test_studyn=test_nmiss$study.1
test_enrolid=test_nmiss[,1]
test_nmiss=test_nmiss[, -c(1,7,8,20,21)]
#test_scale=scale(_nmiss)
test_scale=predict(ppr, newdata=test_nmiss)
set.seed(123)

kmax=15

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(base_scale, k, iter.max=20, nstart = 4)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:kmax

# extract wss for 1-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")

# going to use 4 or 5 clusters, or 2

set.seed(123)
cluster0 <- kmeans(base_scale, 2, iter.max=15, nstart = 6)
cluster1 <- kmeans(base_scale, 4, iter.max=15, nstart = 6)
cluster2 <- kmeans(base_scale, 5, iter.max=15, nstart = 6)


# Show the clusters.
base_nmiss %>%
  mutate(Cluster = cluster0$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

base_nmiss %>%
  mutate(Cluster = cluster1$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

base_nmiss %>%
  mutate(Cluster = cluster2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


# Show how the clusters split out among the studies.
#table(base_nmiss$studyn, final$cluster)
#head(check)
check=data.frame(person_id=enrolid, study=studyn, cluster0=cluster0$cluster, cluster1=cluster1$cluster, cluster2=cluster2$cluster)
table(check$study, check$cluster0)
table(check$study, check$cluster1)
table(check$study, check$cluster2)


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
pred0=predict.kmeans(cluster0, newdata=test_scale, method="classes")
pred2=predict.kmeans(cluster1, newdata=test_scale, method="classes")
pred2=predict.kmeans(cluster2, newdata=test_scale, method="classes")
std=data.frame(person_id=test_enrolid, study=test_studyn, cluster0=pred0, cluster1=pred1, cluster2=pred2)
#head(std)
table(std$study, std$cluster0)
table(std$study, std$cluster1)
table(std$study, std$cluster2)

# predict by centers
pred2=predict.kmeans(cluster0, newdata=test_scale, method="centers")
pred4=predict.kmeans(cluster1, newdata=test_scale, method="centers")
pred5=predict.kmeans(cluster2, newdata=test_scale, method="centers")
std=data.frame(person_id=test_enrolid, study=test_studyn, cluster0=pred2, cluster1=pred4, cluster2=pred5)
#head(std)
table(std$study, std$cluster0)
table(std$study, std$cluster1)
table(std$study, std$cluster2)

# wtf?
#train2=left_join(x=train, y=check, by='person_id')
#colnames(train)
#head(train)
#test2=left_join(x=test, y=std, by='person_id')

#all=rbind(train2, test2)
#head(all)
#write.csv(all, file='Data/analysis_ds_clusters.csv', quote = FALSE,  row.names = FALSE)
