#!/usr/bin/Rscript
# Date: 04-04-15
# Author: Liang


# Unit 4
# LETTER RECOGNITION

# PROBLEM 1
# PROBLEM 1.1 - PREDICTING B OR NOT B
letters = read.csv("letters_ABPR.csv ")
str(letters)
letters$isB = as.factor(letters$letter == "B")
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio=0.5)
train = subset(letters, spl=TRUE)
test = subset(letters, spl=FALSE)
# What is the accuracy of this baseline method on the test set?
table(test$isB)
2350/(2350+766)

# PROBLEM 1.2 - PREDICTING B OR NOT B
# What is the accuracy of the CART model on the test set?
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
bPred = predict(CARTb, newdata=test, type="class")
table(test$isB, bPred)
(2280+665)/(2280+70+101+665)

# PROBLEM 1.3 - PREDICTING B OR NOT B  
# What is the accuracy of the model on the test set?
library(randomForest)
set.seed(1000)
BForest = randomForest(isB ~ . - letter, data = train)
bPred_2 = predict(BForest, newdata=test)
table(test$isB, bPred_2)


# PROBLEM 2
# PROBLEM 2.1 - PREDICTING THE LETTERS A, B, P, R
# What is the baseline accuracy on the testing set?
letters$letter = as.factor(letters$letter)
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, spl == TRUE)
test2 = subset(letters, spl == FALSE)
table(test2$letter)
401/nrow(test2)
str(train2)

# PROBLEM 2.2 - PREDICTING THE LETTERS A, B, P, R
# What is the test set accuracy of your CART model?
CARTl = rpart(letter ~ . - isB, data=train2, method="class")
lpred = predict(CARTl, newdata=test2, type="class")
table(test2$letter, lpred)
(348+318+363+340)/nrow(test2)

# PROBLEM 2.3 - PREDICTING THE LETTERS A, B, P, R
# What is the test set accuracy of your random forest model?
set.seed(1000)
LForest = randomForest(letter ~ . - isB, data = train2)
lPred_2 = predict(LForest, newdata=test2)
table(test2$letter, lPred_2)
(390+380+393+364)/nrow(test2)
