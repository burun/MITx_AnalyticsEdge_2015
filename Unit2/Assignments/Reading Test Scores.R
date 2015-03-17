#!/usr/bin/Rscript
# Date: 16-03-15
# Author: Liang


# Unit 2
# Reading Test Scores

# Problem 1
# Problem 1.1 - Dataset size
# How many students are there in the training set?
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)

# Problem 1.2 - Summarizing the dataset
# Using tapply() on pisaTrain, what is the average reading test score of males?
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# Problem 1.3 - Locating missing values
# Which variables are missing data in at least one observation in the training set? Select all that apply.
summary(pisaTrain)

# Problem 1.4 - Removing missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
# How many observations are now in the training set?
str(pisaTrain)
# How many observations are now in the testing set?
str(pisaTest)

# Problem 2
# Problem 2.1 - Factor variables
# Which of the following variables is an unordered factor with at least 3 levels? (Select all that apply.)
# raceeth 
# Which of the following variables is an ordered factor with at least 3 levels? (Select all that apply.)
# grade 

# Problem 2.2 - Unordered factors in regression models
# Which binary variables will be included in the regression model? (Select all that apply.)
# all these variables except for raceethWhite

# Problem 2.3 - Example unordered factors
# For a student who is Asian, which binary variables would be set to 0?
# all these variables except for raceethAsian
# For a student who is white, which binary variables would be set to 0?
# all these variables except for raceethWhite

# Problem 3
# Problem 3.1 - Building a model
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
# What is the Multiple R-squared value of lmScore on the training set?
lmScore = lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)

# Problem 3.2 - Computing the root-mean squared error of the model 
# What is the training-set root-mean squared error (RMSE) of lmScore?
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain))
sqrt(mean(lmScore$residuals^2))

# Problem 3.3 - Comparing predictions for similar students
# What is the predicted reading score of student A minus the predicted reading score of student B?
summary(lmScore)

# Problem 3.4 - Interpreting model coefficients
# What is the meaning of the coefficient associated with variable raceethAsian?
# Predicted difference in the reading score between an Asian student and a white student who is otherwise identical

# Problem 3.5 - Identifying variables lacking statistical significance
# Based on the significance codes, which variables are candidates for removal from the model?
summary(lmScore)

# Problem 4
# Problem 4.1 - Predicting on unseen data
# What is the range between the maximum and minimum predicted reading score on the test set?
predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)

# Problem 4.2 - Test set SSE and RMSE
# What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE = sum((predTest-pisaTest$readingScore)^2)
SSE
# What is the root-mean squared error (RMSE) of lmScore on the testing set?
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE

# Problem 4.3 - Baseline prediction and test-set SSE
# What is the predicted test score used in the baseline model?
mean(pisaTrain$readingScore)
# What is the sum of squared errors of the baseline model on the testing set?
SST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
SST

# Problem 4.4 - Test-set R-squared
# What is the test-set R-squared value of lmScore?
R2 = 1 - SSE/SST
R2
