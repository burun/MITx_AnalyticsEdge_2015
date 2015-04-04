#!/usr/bin/Rscript
# Date: 04-04-15
# Author: Liang


# Unit 4
# PREDICTING EARNINGS FROM CENSUS DATA

# PROBLEM 1
# PROBLEM 1.1 - A LOGISTIC REGRESSION MODEL
# Which variables are significant, or have factors that are significant?
census = read.csv("census.csv")
library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio=0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)
incomeLog = glm(over50k ~ ., data=train, family="binomial")
summary(incomeLog)

# PROBLEM 1.2 - A LOGISTIC REGRESSION MODEL
# What is the accuracy of the model on the testing set?
incomePred = predict(incomeLog, newdata=test, type="response")
table(test$over50k, incomePred>0.5)
(9051+1886)/nrow(test)

# PROBLEM 1.3 - A LOGISTIC REGRESSION MODEL
# What is the baseline accuracy for the testing set?
table(test$over50k)
9713/(9713+3078)

# PROBLEM 1.4 - A LOGISTIC REGRESSION MODEL
# What is the area-under-the-curve (AUC) for this model on the test set?
library(ROCR)
ROCRpred = prediction(incomePred, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)


# # PROBLEM 2
# PROBLEM 2.1 - A CART MODEL
# How many splits does the tree have in total?
library(rpart)
library(rpart.plot)
CARTmodel = rpart(over50k ~ ., data=train)
prp(CARTmodel)

# PROBLEM 2.2 - A CART MODEL
# Which variable does the tree split on at the first level (the very first split of the tree)?
# relationship

# PROBLEM 2.3 - A CART MODEL
# Which variables does the tree split on at the second level (immediately after the first split of the tree)?
# education and capitalgain

# PROBLEM 2.4 - A CART MODEL
# What is the accuracy of the model on the testing set?
incomePred_2 = predict(CARTmodel, newdata=test, type="class")
table(test$over50k, incomePred_2)
(9243+1596)/nrow(test)

# PROBLEM 2.5 - A CART MODEL
# Which of the following explanations for this behavior is most correct? 
incomePred_2 = predict(CARTmodel, newdata=test)
ROCRpred_2 = prediction(incomePred_2[,2], test$over50k)
# ROC of CART model
perf_2 = performance(ROCRpred_2, "tpr", "fpr")
# ROC of logistic regression model
plot(perf_2)
perf = performance(ROCRpred, "tpr", "fpr")
plot(perf)
# The probabilities from the CART model take only a handful of values (five, one for each end bucket/leaf of the tree); the changes in the ROC curve correspond to setting the threshold to one of those values

# PROBLEM 2.6 - A CART MODEL
# What is the AUC of the CART model on the test set?
as.numeric(performance(ROCRpred_2, "auc")@y.values)


# # PROBLEM 3
# PROBLEM 3.1 - A RANDOM FOREST MODEL
# What is the accuracy of the model on the test set, using a threshold of 0.5? 
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
library(randomForest)
incomeForest = randomForest(over50k ~ ., data=trainSmall)
incomePred_3 = predict(incomeForest, newdata=test)
table(test$over50k, incomePred_3)
(9614+1050)/(9614+99+2028+1050)

# PROBLEM 3.2 - A RANDOM FOREST MODEL
# Which of the following variables is the most important in terms of the number of splits?
vu = varUsed(incomeForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(incomeForest$forest$xlevels[vusorted$ix]))

# PROBLEM 3.3 - A RANDOM FOREST MODEL
# Which one of the following variables is the most important in terms of mean reduction in impurity?
varImpPlot(incomeForest)


# PROBLEM 4
# PROBLEM 4.1 - SELECTING CP BY CROSS-VALIDATION
# Which value of cp does the train function recommend?
library(caret)
library(e1071)
set.seed(2)
tr.control = trainControl(method = "cv", number = 10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
tr = train(over50k ~ ., data = train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr

# PROBLEM 4.2 - SELECTING CP BY CROSS-VALIDATION
# What is the prediction accuracy on the test set?
CARTmodel2 = rpart(over50k ~ ., data=train, cp=0.002)
incomePred_3 = predict(CARTmodel2, newdata=test, type="class")
table(test$over50k, incomePred_3)
(9178+1838)/nrow(test)

# PROBLEM 4.3 - SELECTING CP BY CROSS-VALIDATION
# How many splits are there?
prp(CARTmodel2)
