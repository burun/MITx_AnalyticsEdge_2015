#!/usr/bin/Rscript
# Date: 24-03-15
# Author: Liang


# Unit 3
# PREDICTING PAROLE VIOLATORS

# PROBLEM 1
# PROBLEM 1.1 - LOADING THE DATASET
# How many parolees are contained in the dataset?
parole = read.csv("parole.csv")
str(parole)

# PROBLEM 1.2 - LOADING THE DATASET
# How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)


# PROBLEM 2
# PROBLEM 2.1 - PREPARING THE DATASET
# Which variables in this dataset are unordered factors with at least three levels?
# state and crime

# PROBLEM 2.2 - PREPARING THE DATASET
# How does the output of summary() change for a factor variable as compared to a numerical variable?
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)


# PROBLEM 3
# PROBLEM 3.1 - SPLITTING INTO A TRAINING AND TESTING SET
# Roughly what proportion of parolees have been allocated to the training and testing sets?
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# PROBLEM 3.2 - SPLITTING INTO A TRAINING AND TESTING SET
# Now, suppose you re-ran lines [1]-[5] of Problem 3.1. What would you expect?
# The exact same training/testing set split as the first execution of [1]-[5]
# If you instead ONLY re-ran lines [3]-[5], what would you expect?
# A different training/testing set split from the first execution of [1]-[5]
# If you instead called set.seed() with a different number and then re-ran lines [3]-[5] of Problem 3.1, what would you expect?
# A different training/testing set split from the first execution of [1]-[5]


# PROBLEM 4
# PROBLEM 4.1 - BUILDING A LOGISTIC REGRESSION MODEL
# What variables are significant in this model? 
ParoleLog = glm(violator ~ ., data=train, family="binomial")
summary(ParoleLog)

# PROBLEM 4.2 - BUILDING A LOGISTIC REGRESSION MODEL
# What can we say based on the coefficient of the multiple.offenses variable?
exp(1.6119919)
# Our model predicts that a parolee who committed multiple offenses has 5.01 times higher odds of being a violator than a parolee who did not commit multiple offenses but is otherwise identical. 

# PROBLEM 4.3 - BUILDING A LOGISTIC REGRESSION MODEL
# According to the model, what are the odds this individual is a violator?
log_odds = -4.2411574 + 0.3869904 * 1 + 0.8867192 * 1 + 50 * (-0.0001756) + 3 * (-0.1238867) + 12 * 0.0802954 + 0.6837143
exp(log_odds)
# According to the model, what is the probability this individual is a violator?
exp(log_odds)/(1 + exp(log_odds))


# PROBLEM 5
# PROBLEM 5.1 - EVALUATING THE MODEL ON THE TESTING SET
# What is the maximum predicted probability of a violation?
pred_1 = predict(ParoleLog, newdata=test, type="response")
summary(pred_1)

# PROBLEM 5.2 - EVALUATING THE MODEL ON THE TESTING SET
# What is the model's sensitivity?
table(test$violator, pred_1>=0.5)
12/(11+12)
# What is the model's specificity?
167/(167+12)
# What is the model's accuracy?
(167+12)/(167+12+11+12)

# PROBLEM 5.3 - EVALUATING THE MODEL ON THE TESTING S
# What is the accuracy of a simple model that predicts that every parolee is a non-violator?
table(test$violator)
179/(179+23)

# PROBLEM 5.4 - EVALUATING THE MODEL ON THE TESTING SET
# Which of the following most likely describes their preferences and best course of action?
# The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5.

# PROBLEM 5.5 - EVALUATING THE MODEL ON THE TESTING SET
# Which of the following is the most accurate assessment of the value of the logistic regression model with a cutoff 0.5 to a parole board, based on the model's accuracy as compared to the simple baseline model?
# The model is likely of value to the board, and using a different logistic regression cutoff is likely to improve the model's value

# PROBLEM 5.6 - EVALUATING THE MODEL ON THE TESTING SET
# Using the ROCR package, what is the AUC value for the model?
library(ROCR)
ROCRpred = prediction(pred_1, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

# PROBLEM 5.7 - EVALUATING THE MODEL ON THE TESTING SET
# Describe the meaning of AUC in this context.
# The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator. 


# PROBLEM 6
# PROBLEM 6.1 - IDENTIFYING BIAS IN OBSERVATIONAL DATA
# How could we improve our dataset to best address selection bias?
# We should use a dataset tracking a group of parolees from the start of their parole until either they violated parole or they completed their term. 
