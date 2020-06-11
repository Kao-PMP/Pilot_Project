# This is a copy of 03_clusters that explores different ways of detecting the shoulder.
# See note below for page from Sherry Towers' web blog describing these (unpublished) methods.
# Chris Roeder
# June, 2020
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
#base_nmiss=train[complete.cases(train[, c(train_columns, train_columns)])]
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

### function to compute total within-cluster sum of square 
##wss <- function(k) {
##  kmeans(base_scale, k, iter.max=20, nstart = 4)$tot.withinss
##}
##
### Compute and plot wss for k = 1 to k = 15
##k.values <- 1:kmax
##
### extract wss for 1-15 clusters
##wss_values <- map_dbl(k.values, wss)
##
##plot(k.values, wss_values,
##       type="b", pch = 19, frame = FALSE, 
##       xlab="Number of clusters K",
##       ylab="Total within-clusters sum of squares")

# cribbing from sherrytowers.com/2013/10/24/k-means-clustering
kmfit = list()
totwss = rep(0, kmax)
for (i in 1:kmax) {
  klus = kmeans(base_scale, i, iter.max=20, nstart = 4)
  totwss[i] = klus$tot.withinss
  kmfit[[i]] <- klus
}
print("R-Squared")
n=nrow(base_scale)
rsq = 1-(totwss*(n-1))/(totwss[1]*(n-seq(1, kmax)))

# try to find the elbow.. COMINGS-elbow?
# minimum distance from (k,aic) to (1,min(aic)), but normalize points to [0,1], then it's
# minimum distance from (k,aic) to (0,0) or min(sqrt(k^2 + aic^2))

# first normalize the rsq by scaling, then shifting down by the minimum value
vrsq = (rsq-min(rsq)) / (max(rsq)-min(rsq))
k = seq(1,length(vrsq))
# same for other dimentions
vk = (k - min(k)) / (max(k) - min(k))
# here's the distance from (0,0) ...but why vrsq-1? it flips direction??/ 
#  higher value is better? flip it to make it closer to 0?
dsq = (vk)^2 + (vrsq-1)^2
nclus=which.min(dsq) 

# PLOT RSQ
mult.fig(1, main="R-Squared")
plot(seq(1, kmax), rsq, xlab="number of clusters", ylab="adjusted r-squared", pch=20, cex=2)
points(nclus, rsq[nclus], col=2, cex=2)

# Plot the normalied and flipped VRSQ, from which the dsk is calculated
mult.fig(1, main="VRSQ")
plot(seq(1, kmax), 1-vrsq, xlab="number of clusters", ylab="normalized adjusted r-squared", pch=20, cex=2)

cat("the elbow by rsq is ", nclus, dsq[nclus])
# PLOT DSQ
mult.fig(1, main="dsq?")
plot(seq(1, kmax), dsq, xlab="number of clusters", ylab="adjusted r-squared", pch=20, cex=2)
points(nclus, dsq[nclus], col=2, cex=2)

# Plot sqrt(DSQ) ...the distance
mult.fig(1, main="sqrt(dsq)")
plot(seq(1, kmax), sqrt(dsq), xlab="number of clusters", ylab="sqrt of adjusted r-squared", pch=20, cex=2)
points(nclus, sqrt(dsq[nclus]), col=2, cex=2)



# AIC also cribbing from sherrytowers.com/2013/10/24/k-means-clustering
kmeansAIC = function(fit) {
	m = ncol(fit$centers)
	n = length(fit$cluster)
	k = nrow(fit$centers)	
	D = fit$tot.withinss
	return (D + 2 * m * k)
}
length(kmfit)
aic=sapply(kmfit, kmeansAIC)
length(aic)
mult.fig(1,main="AiC")
plot(seq(1,kmax), aic, xlab="number of clusters", ylab="AiC", pch=20, cex=2)

# find the elbow, this time, not using distance from (0,0), rather
# the biggest change in AiC before that point and after.
v = -diff(aic)
print(v)
nv = length(v)
fom = v[1:(nv-1)]/v[2:nv] # FigureOfMerit
print(fom)
nclus = which.max(fom) + 1
cat("the elbow by AiC is ", nclus, fom[nclus])
points(nclus, aic[nclus], col=2, cex=2)
