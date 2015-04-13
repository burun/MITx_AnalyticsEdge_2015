#!/usr/bin/Rscript
# Date: 13-04-15
# Author: Liang


# Unit 6
# MARKET SEGMENTATION FOR AIRLINES


# PROBLEM 1
# PROBLEM 1.1 - NORMALIZING THE DATA
# Which TWO variables have (on average) the smallest values?
# Which TWO variables have (on average) the largest values?
airlines = read.csv("AirlinesCluster.csv")
summary(airlines)

# PROBLEM 1.2 - NORMALIZING THE DATA
# Why is it important to normalize the data before clustering?
# If we don't normalize the data, the clustering will be dominated by the variables that are on a larger scale.

# PROBLEM 1.3 - NORMALIZING THE DATA
# In the normalized data, which variable has the largest maximum value?
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

# PROBLEM 2
# PROBLEM 2.1 - HIERARCHICAL CLUSTERING
# which of the following is NOT a good choice for the number of clusters?
airlineDist = dist(airlinesNorm, method="euclidean")
airlineClust = hclust(airlineDist, method="ward.D")
plot(airlineClust)

# PROBLEM 2.2 - HIERARCHICAL CLUSTERING
# How many data points are in Cluster 1?
airGroups = cutree(airlineClust, k = 5)
table(airGroups)

# PROBLEM 2.3 - HIERARCHICAL CLUSTERING
# PROBLEM 2.4 - HIERARCHICAL CLUSTERING
# PROBLEM 2.5 - HIERARCHICAL CLUSTERING
# PROBLEM 2.6 - HIERARCHICAL CLUSTERING
# PROBLEM 2.7 - HIERARCHICAL CLUSTERING
# Compared to the other clusters, Cluster 1-5 has the largest average values in which variables (if any)? 
# How would you describe the customers in Cluster 1-5?
tapply(airlines$Balance, airGroups, mean)
tapply(airlines$QualMiles, airGroups, mean)
tapply(airlines$BonusMiles, airGroups, mean)
tapply(airlines$BonusTrans, airGroups, mean)
tapply(airlines$FlightMiles, airGroups, mean)
tapply(airlines$FlightTrans, airGroups, mean)
tapply(airlines$DaysSinceEnroll, airGroups, mean)


# PROBLEM 3
# PROBLEM 3.1 - K-MEANS CLUSTERING
set.seed(88)
airKmeans = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
airCluster = airKmeans$cluster
table(airCluster)

# PROBLEM 3.2 - K-MEANS CLUSTERING  
# Do you expect Cluster 1 of the K-Means clustering output to necessarily be similar to Cluster 1 of the Hierarchical clustering output?
str(airCluster)
tapply(airlines$Balance, airCluster, mean)
tapply(airlines$QualMiles, airCluster, mean)
tapply(airlines$BonusMiles, airCluster, mean)
tapply(airlines$BonusTrans, airCluster, mean)
tapply(airlines$FlightMiles, airCluster, mean)
tapply(airlines$FlightTrans, airCluster, mean)
tapply(airlines$DaysSinceEnroll, airCluster, mean)

airKmeans$centers[1,]
