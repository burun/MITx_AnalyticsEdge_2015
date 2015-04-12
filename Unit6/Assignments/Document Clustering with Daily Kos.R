#!/usr/bin/Rscript
# Date: 12-04-15
# Author: Liang


# Unit 6
# DOCUMENT CLUSTERING WITH DAILY KOS


# PROBLEM 1
# PROBLEM 1.1 - HIERARCHICAL CLUSTERING
# Running the dist function will probably take you a while. Why? 
dailykos = read.csv("dailykos.csv")
dailykosDist = dist(dailykos, method="euclidean")
dkClust = hclust(dailykosDist, method="ward.D")


# We have a lot of observations, so it takes a long time to compute the distance between each pair of observations. 
# We have a lot of variables, so the distance computation is long. 

# PROBLEM 1.2 - HIERARCHICAL CLUSTERING
# which of the following seem like good choices for the number of clusters?
plot(dkClust)
# 2 & 3

# PROBLEM 1.3 - HIERARCHICAL CLUSTERING
# what are good choices for the number of clusters?
# 7 & 8

# PROBLEM 1.4 - HIERARCHICAL CLUSTERING
dkGroups = cutree(dkClust, k = 7)
dkCluster1 = subset(dailykos, dkGroups == 1)
dkCluster2 = subset(dailykos, dkGroups == 2)
dkCluster3 = subset(dailykos, dkGroups == 3)
dkCluster4 = subset(dailykos, dkGroups == 4)
dkCluster5 = subset(dailykos, dkGroups == 5)
dkCluster6 = subset(dailykos, dkGroups == 6)
dkCluster7 = subset(dailykos, dkGroups == 7)

# How many observations are in cluster 3?
table(dkGroups)

# PROBLEM 1.5 - HIERARCHICAL CLUSTERING
# What is the most frequent word in this cluster, in terms of average value?
tail(sort(colMeans(dkCluster1)))

# PROBLEM 1.6 - HIERARCHICAL CLUSTERING
# Which words best describe cluster 2?
tail(sort(colMeans(dkCluster2)))
tail(sort(colMeans(dkCluster3)))
tail(sort(colMeans(dkCluster4)))
tail(sort(colMeans(dkCluster5)))
tail(sort(colMeans(dkCluster6)))
tail(sort(colMeans(dkCluster7)))


# PROBLEM 2
# PROBLEM 2.1 - K-MEANS CLUSTERING
# How many observations are in Cluster 3?
# Which cluster has the most observations?
# Which cluster has the fewest number of observations?
set.seed(1000)
dkKmeans = kmeans(dailykos, 7)
dkkCluster = dkKmeans$cluster
table(dkkCluster)

# PROBLEM 2.2 - K-MEANS CLUSTERING
# Which k-means cluster best corresponds to the Iraq War?
# Which k-means cluster best corresponds to the democratic party?
dkkCluster1 = subset(dailykos, dkkCluster==1)
dkkCluster2 = subset(dailykos, dkkCluster==2)
dkkCluster3 = subset(dailykos, dkkCluster==3)
dkkCluster4 = subset(dailykos, dkkCluster==4)
dkkCluster5 = subset(dailykos, dkkCluster==5)
dkkCluster6 = subset(dailykos, dkkCluster==6)
dkkCluster7 = subset(dailykos, dkkCluster==7)
tail(sort(colMeans(dkkCluster1)))
tail(sort(colMeans(dkkCluster2)))
tail(sort(colMeans(dkkCluster3)))
tail(sort(colMeans(dkkCluster4)))
tail(sort(colMeans(dkkCluster5)))
tail(sort(colMeans(dkkCluster6)))
tail(sort(colMeans(dkkCluster7)))

# PROBLEM 2.3 - K-MEANS CLUSTERING
# PROBLEM 2.4
# PROBLEM 2.5
# PROBLEM 2.6
table(dkGroups, dkkCluster)



