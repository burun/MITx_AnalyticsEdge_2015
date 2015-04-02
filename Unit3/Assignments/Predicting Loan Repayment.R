#!/usr/bin/Rscript
# Date: 24-03-15
# Author: Liang


# Unit 3
# Predicting Loan Repayment

# PROBLEM 1
# PROBLEM 1.1 - PREPARING THE DATASET
# What proportion of the loans in the dataset were not paid in full? Please input a number between 0 and 1.
loans = read.csv("loans.csv")
str(loans)
table(loans$not.fully.paid)
1533/(1533+8045)

# PROBLEM 1.2 - PREPARING THE DATASET
# Which of the following variables has at least one missing observation?
summary(loans)

# PROBLEM 1.3 - PREPARING THE DATASET  
# Which of the following is the best reason to fill in the missing values for these variables instead of removing observations with missing data? 
# We want to be able to predict risk for all borrowers, instead of just the ones with all data reported

# PROBLEM 1.4 - PREPARING THE DATASET
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
# What best describes the process we just used to handle missing values?
# We predicted missing variable values using the available independent variables for each observation.
loans = read.csv("loans_imputed.csv")

# PROBLEM 2
# PROBLEM 2.1 - PREDICTION MODELS
# Which independent variables are significant in our model?
library(caTools)
set.seed(144)
spl = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, spl==TRUE)
test = subset(loans, spl==FALSE)
loansLog = glm(not.fully.paid ~ ., data=train, family="binomial")
summary(loansLog)

# PROBLEM 2.2 - PREDICTION MODELS
# What is the value of Logit(A) - Logit(B)?
-9.317e-03 * (700-710)
# What is the value of O(A)/O(B)?
exp(0.09317)

# PROBLEM 2.3 - PREDICTION MODELS
predicted.risk = predict(loansLog, newdata=test, type="response")
test$predicted.risk = predicted.risk
# What is the accuracy of the logistic regression model?
table(test$not.fully.paid, predicted.risk > 0.5)
(2400+3)/(2400+13+457+3)
# What is the accuracy of the baseline model?
table(test$not.fully.paid)
2413/(2413+460)

# PROBLEM 2.4 - PREDICTION MODELS
# Use the ROCR package to compute the test set AUC.
library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)


# PROBLEM 3
# PROBLEM 3.1 - A "SMART BASELINE"
# What is the most likely explanation for this difference?
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)

# PROBLEM 3.2 - A "SMART BASELINE"
# What is the highest predicted probability of a loan not being paid in full on the testing set?
pred_in_rate = predict(bivariate, newdata=test, type="response")
summary(pred_in_rate)
# With a logistic regression cutoff of 0.5, how many loans would be predicted as not being paid in full on the testing set?
table(test$not.fully.paid, pred_in_rate > 0.5)

# PROBLEM 3.3 - A "SMART BASELINE"
# What is the test set AUC of the bivariate model?
library(ROCR)
ROCRpred = prediction(pred_in_rate, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)


# PROBLEM 4
# PROBLEM 4.1 - COMPUTING THE PROFITABILITY OF AN INVESTMENT
# How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest? 
10*exp(0.06*3)

# PROBLEM 4.2 - COMPUTING THE PROFITABILITY OF AN INVESTMENT
# What is the profit to the investor if the investment is paid back in full?
# c * exp(rt) - c

# PROBLEM 4.3 - COMPUTING THE PROFITABILITY OF AN INVESTMENT
# What is the profit to the investor in this scenario?
# -c


# PROBLEM 5
# PROBLEM 5.1 - A SIMPLE INVESTMENT STRATEGY
# What is the maximum profit of a $10 investment in any loan in the testing set?
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
summary(test$profit)


# PROBLEM 6
# PROBLEM 6.1 - AN INVESTMENT STRATEGY BASED ON RISK
# What is the average profit of a $1 investment in one of these high-interest loans?
highInterest = subset(test, int.rate >= 0.15)
summary(highInterest$profit)
# What proportion of the high-interest loans were not paid back in full?
table(highInterest$not.fully.paid)
110/(327+110)

# PROBLEM 6.2 - AN INVESTMENT STRATEGY BASED ON RISK
# What is the profit of the investor, who invested $1 in each of these 100 loans?
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
