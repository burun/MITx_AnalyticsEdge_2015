#!/usr/bin/Rscript
# Date: 13-04-15
# Author: Liang


# Unit 6
# PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT

# PROBLEM 1
# PROBLEM 1.1 - EXPLORING THE DATASET
# How many observations are in the dataset?
stocks = read.csv("StocksCluster.csv")
str(stocks)

# PROBLEM 1.2 - EXPLORING THE DATASET
# What proportion of the observations have positive returns in December?
table(stocks$PositiveDec>0)
6324/(5256+6324)

# PROBLEM 1.3 - EXPLORING THE DATASET
# What is the maximum correlation between any two return variables in the dataset? 
cor(stocks)
sort(cor(stocks))

# PROBLEM 1.4 - EXPLORING THE DATASET
# Which month (from January through November) has the largest mean return across all observations in the dataset?
summary(stocks)


# PROBLEM 2
# PROBLEM 2.1 - INITIAL LOGISTIC REGRESSION MODEL
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family="binomial")
trainPred = predict(StocksModel, data=stocksTrain, type="response")
table(stocksTrain$PositiveDec, trainPred>0.5)
(990+3640)/nrow(stocksTrain)

# PROBLEM 2.2 - INITIAL LOGISTIC REGRESSION MODEL
# What is the overall accuracy of the model on the test, again using a threshold of 0.5?
testPred = predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, testPred>0.5)
(417+1553)/nrow(stocksTest)

# PROBLEM 2.3 - INITIAL LOGISTIC REGRESSION MODEL  
# What is the accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)?
table(stocksTest$PositiveDec == 1)
1897/nrow(stocksTest)


# PROBLEM 3
# PROBLEM 3.1 - CLUSTERING STOCKS
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
# Why do we need to remove the dependent variable in the clustering phase of the cluster-then-predict methodology?
# Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology

# PROBLEM 3.2 - CLUSTERING STOCKS  
# What is the mean of the ReturnJan variable in normTrain?
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
summary(normTrain$ReturnJan)
summary(normTest$ReturnJan)

# PROBLEM 3.3 - CLUSTERING STOCKS
# Why is the mean ReturnJan variable much closer to 0 in normTrain than in normTest?
# The distribution of the ReturnJan variable is different in the training and testing set

# PROBLEM 3.4 - CLUSTERING STOCKS
# Which cluster has the largest number of observations?
set.seed(144)
stockKmeans = kmeans(normTrain, centers = 3)
stockCluster = stockKmeans$cluster
table(stockCluster)

# PROBLEM 3.5 - CLUSTERING STOCKS
# How many test-set observations were assigned to Cluster 2?
install.packages("flexclust")
library(flexclust)
km.kcca = as.kcca(stockKmeans, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

# PROBLEM 4
# PROBLEM 4.1 - CLUSTER-SPECIFIC PREDICTIONS
# Which training set data frame has the highest average value of the dependent variable?
stocksTrain1 = subset(stocksTrain, stockCluster == 1)
stocksTrain2 = subset(stocksTrain, stockCluster == 2)
stocksTrain3 = subset(stocksTrain, stockCluster == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
summary(stocksTrain1$PositiveDec)
summary(stocksTrain2$PositiveDec)
summary(stocksTrain3$PositiveDec)

# PROBLEM 4.2 - CLUSTER-SPECIFIC PREDICTIONS
# Which variables have a positive sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3 
# and a negative sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3? 
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family="binomial")
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family="binomial")
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family="binomial")
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

# PROBLEM 4.3 - CLUSTER-SPECIFIC PREDICTIONS
PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type="response")
PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type="response")
PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type="response")
# What is the overall accuracy of StocksModel1 on the test set stocksTest1, using a threshold of 0.5?
table(stocksTest1$PositiveDec, PredictTest1>0.5)
(30+774)/nrow(stocksTest1)
# What is the overall accuracy of StocksModel2 on the test set stocksTest2, using a threshold of 0.5?
table(stocksTest2$PositiveDec, PredictTest2>0.5)
(388+757)/nrow(stocksTest2)
# What is the overall accuracy of StocksModel3 on the test set stocksTest3, using a threshold of 0.5?
table(stocksTest3$PositiveDec, PredictTest3>0.5)
(49+13)/nrow(stocksTest3)

# PROBLEM 4.4 - CLUSTER-SPECIFIC PREDICTIONS
# What is the overall test-set accuracy of the cluster-then-predict approach, again using a threshold of 0.5?
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions>0.5)
(467+1544)/(467+1110+353+1544)

