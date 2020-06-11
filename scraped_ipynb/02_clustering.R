#!/usr/bin/env python
# coding: utf-8

# Program: 02_clustering.R  
# Date: July 3, 2019  
# Programmer: Hillary Mulder  
# Purpose: Use clustering techniques for BP type identification

# In[1]:


data=read.csv("Data/analysis_ds.csv")
data=data[which(data$train==1),]
colnames(data)
dim(data)


# Hierarchical clustering with baseline characteristics - Need to identify the optimal linkage type and number of clusters

# In[2]:


base_nmiss=data[complete.cases(data[, c(1:3, 5, 6, 9:19, 20:22, 50, 51)]), c(1:3, 5, 6, 9:19, 20:22, 50, 51)]
base_nmiss$studyn=as.numeric(base_nmiss$study.1)
base_nmiss=base_nmiss[, -c(7,8,20,21,22)]
summary(base_nmiss)
nrow(base_nmiss)
#set.seed(120)
#rs=sample_n(base_nmiss, size=1500, replace=F)
#nrow(rs)


# In[3]:


m = c("average", "single", "complete", "ward")
names(m) = c("average", "single", "complete", "ward")
#ac=function(x){
#    agnes(rs, method=x)$ac
#}
#map_dbl(m, ac)
#hc=agnes(rs, method='ward')
#pltree(hc, cex=0.6, hang=-1, main='Dendrogram of AGNES')


# In[4]:


d=dist(base_nmiss, method='euclidean')
hc2=hclust(d, method='ward.D2')


# In[5]:


sub_grp = cutree(hc2, k=3)
table(sub_grp)
plot(hc2, cex=0.6, xaxt='n')
rect.hclust(hc2, k=4, border=2:5)


# In[ ]:





# In[ ]:





# In[ ]:




