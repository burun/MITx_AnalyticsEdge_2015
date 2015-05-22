#!/usr/bin/Rscript
# Date: 22-05-15
# Author: Liang


# Final Exam
# Understanding Customers of Hubway

# Problem 1 - Reading in the Data
# How many observations are in this dataset?
Hubway = read.csv("HubwayTrips.csv")
str(Hubway)

# Problem 2 - Average Duration
# What is the average duration (in seconds) of all trips in this dataset?
summary(Hubway$Duration)
# What is the average duration (in seconds) of trips taken on the weekdays?
tapply(Hubway$Duration, Hubway$Weekday, mean)

# Problem 3 - Time of Day
# How many trips were taken in the morning?
table(Hubway$Morning)
# How many trips were taken in the afternoon?
table(Hubway$Afternoon)
# How many trips were taken in the evening?
table(Hubway$Evening)

# Problem 4 - Gender Distribution
# In this dataset, what proportion of trips are taken by male users?
table(Hubway$Male)
136505/(136505+48685)

# Problem 5 - Importance of Normalizing
# which variable would you expect to dominate in the distance calculations?
# Duration

# Problem 6 - Normalizing the Data
# What is the maximum value of Duration in the normalized dataset?
# What is the maximum value of Age in the normalized dataset?
library(caret)
preproc = preProcess(Hubway)
HubwayNorm = predict(preproc, Hubway)
summary(HubwayNorm)

# Problem 7 - Hierarchical Clustering
# Why do you think hierarchical clustering might have a problem with this dataset?
# We might have too many observations in our dataset for Hierarchical clustering to handle

# Problem 8 - K-Means Clustering
# How many observations are in the smallest cluster?
set.seed(5000)
HubwayKmeans = kmeans(HubwayNorm, 10)
HubwayCluster = HubwayKmeans$cluster
table(HubwayCluster)

# Problem 9 - Understanding the Clusters
# Which cluster best fits the description "trips taken by female users on weekday evenings"?
HubwayCluster1 = subset(HubwayNorm, HubwayCluster==1)
HubwayCluster2 = subset(HubwayNorm, HubwayCluster==2)
HubwayCluster3 = subset(HubwayNorm, HubwayCluster==3)
HubwayCluster4 = subset(HubwayNorm, HubwayCluster==4)
HubwayCluster5 = subset(HubwayNorm, HubwayCluster==5)
HubwayCluster6 = subset(HubwayNorm, HubwayCluster==6)
HubwayCluster7 = subset(HubwayNorm, HubwayCluster==7)
HubwayCluster8 = subset(HubwayNorm, HubwayCluster==8)
HubwayCluster9 = subset(HubwayNorm, HubwayCluster==9)
HubwayCluster10 = subset(HubwayNorm, HubwayCluster==10)
colMeans(HubwayCluster1)
colMeans(HubwayCluster2)
colMeans(HubwayCluster3)
colMeans(HubwayCluster4)
colMeans(HubwayCluster5)
colMeans(HubwayCluster6)
colMeans(HubwayCluster7)
colMeans(HubwayCluster8)
colMeans(HubwayCluster9)
colMeans(HubwayCluster10)

# Problem 10 - Understanding the Clusters
# Which cluster best fits the description "leisurely (longer than average) afternoon trips taken on the weekends"?
# same as before

# Problem 11 - Understanding the Clusters
# Which cluster best fits the description "morning trips taken by older male users"?
# same as before

# Problem 12 - Random Behavior
# If we ran k-means clustering a second time without making any additional calls to set.seed, we would expect:
# Different results from the first k-means clustering
# If we ran k-means clustering a second time, again running the command set.seed(5000) right before doing the clustering, we would expect:
# Identical results to the first k-means clustering
# If we ran k-means clustering a second time, running the command set.seed(4000) right before doing the clustering, we would expect:
# Different results from the first k-means clustering

# Problem 13 - The Number of Clusters
# Would they want to increase or decrease the number of clusters?
# Decrease the number of clusters

# Problem 14 - Increasing the Number of Clusters
# How many observations are in the smallest cluster?
# How many observations are in the largest cluster?
set.seed(8000)
HubwayKmeans2 = kmeans(HubwayNorm, 20)
HubwayCluster2 = HubwayKmeans2$cluster
table(HubwayCluster2)

# Problem 15 - Describing the Clusters
# Which clusters can be described as "shorter than average trips that occur on weekday evenings"?
HubwayCluster2_1 = subset(HubwayNorm, HubwayCluster2==1)
HubwayCluster2_2 = subset(HubwayNorm, HubwayCluster2==2)
HubwayCluster2_3 = subset(HubwayNorm, HubwayCluster2==3)
HubwayCluster2_4 = subset(HubwayNorm, HubwayCluster2==4)
HubwayCluster2_5 = subset(HubwayNorm, HubwayCluster2==5)
HubwayCluster2_6 = subset(HubwayNorm, HubwayCluster2==6)
HubwayCluster2_7 = subset(HubwayNorm, HubwayCluster2==7)
HubwayCluster2_8 = subset(HubwayNorm, HubwayCluster2==8)
HubwayCluster2_9 = subset(HubwayNorm, HubwayCluster2==9)
HubwayCluster2_10 = subset(HubwayNorm, HubwayCluster2==10)
HubwayCluster2_11 = subset(HubwayNorm, HubwayCluster2==11)
HubwayCluster2_12 = subset(HubwayNorm, HubwayCluster2==12)
HubwayCluster2_13 = subset(HubwayNorm, HubwayCluster2==13)
HubwayCluster2_14 = subset(HubwayNorm, HubwayCluster2==14)
HubwayCluster2_15 = subset(HubwayNorm, HubwayCluster2==15)
HubwayCluster2_16 = subset(HubwayNorm, HubwayCluster2==16)
HubwayCluster2_17 = subset(HubwayNorm, HubwayCluster2==17)
HubwayCluster2_18 = subset(HubwayNorm, HubwayCluster2==18)
HubwayCluster2_19 = subset(HubwayNorm, HubwayCluster2==19)
HubwayCluster2_20 = subset(HubwayNorm, HubwayCluster2==20)

colMeans(HubwayCluster2_1)
colMeans(HubwayCluster2_2)
colMeans(HubwayCluster2_3)
colMeans(HubwayCluster2_4)
colMeans(HubwayCluster2_5)
colMeans(HubwayCluster2_6)
colMeans(HubwayCluster2_7)
colMeans(HubwayCluster2_8)
colMeans(HubwayCluster2_9)
colMeans(HubwayCluster2_10)
colMeans(HubwayCluster2_11)
colMeans(HubwayCluster2_12)
colMeans(HubwayCluster2_13)
colMeans(HubwayCluster2_14)
colMeans(HubwayCluster2_15)
colMeans(HubwayCluster2_16)
colMeans(HubwayCluster2_17)
colMeans(HubwayCluster2_18)
colMeans(HubwayCluster2_19)
colMeans(HubwayCluster2_20)

# Problem 16 - Understanding Centroids
# Why do we typically use cluster centroids to describe the clusters?
# The cluster centroid captures the average behavior in the cluster, and can be used to summarize the general pattern in the cluster. 

# Problem 17 - Using a Visualization
# Which of the following visualizations could be used to observe the distribution of Age, broken down by cluster?
# A box plot of the variable Age, subdivided by cluster 
# ggplot with Age on the x-axis and the cluster number on the y-axis, plotting with geom_point() 
