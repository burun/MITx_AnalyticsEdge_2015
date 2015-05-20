#!/usr/bin/Rscript
# Date: 20-05-15
# Author: Liang


# Final Exam
# Forecasting Airline Delays

# Problem 1 - Loading the Data
Airlines = read.csv('AirlineDelay.csv')
set.seed(15071)
spl = sample(nrow(Airlines), 0.7*nrow(Airlines))
AirlinesTrain = Airlines[spl,]
AirlinesTest = Airlines[-spl,]
# How many observations are in the training set AirlinesTrain?
str(AirlinesTrain)
# How many observations are in the testing set AirlinesTest?
str(AirlinesTest)

# Problem 2 - Method of Splitting the Data
# Why do we use a different approach here?
# The sample.split function is typically used to split data with a categorical dependent variable, and we have a continuous dependent variable. 

# Problem 3 - A Linear Regression Model
# What is the model's R-squared? 
lmAirlines = lm(TotalDelay~., data=AirlinesTrain)
summary(lmAirlines)

# Problem 4 - Checking for Significance
# which of the independent variables are significant at the p=0.05 level (at least one star)?
summary(lmAirlines)

# Problem 5 - Correlations
# What is the correlation between NumPrevFlights and PrevFlightGap in the training set?
cor(AirlinesTrain$NumPrevFlights, AirlinesTrain$PrevFlightGap)
# What is the correlation between OriginAvgWind and OriginWindGust in the training set?
cor(AirlinesTrain$OriginAvgWind, AirlinesTrain$OriginWindGust)

# Problem 6 - Importance of Correlations
# Why is it imporant to check for correlations between independent variables?
# Having highly correlated independent variables in a regression model can affect the interpretation of the coefficients. 

# Problem 7 - Coefficients
# what is the coefficient for HistoricallyLate?
summary(lmAirlines)

# Problem 8 - Understanding the Coefficients
# The coefficient for NumPrevFlights is 1.56. What is the interpretation of this coefficient?
# For an increase of 1 in the number of previous flights, the prediction of the total delay increases by approximately 1.56. 

# Problem 9 - Understanding the Model
# what is the absolute difference in predicted total delay given that one flight is on Thursday and the other is on Sunday?
1.571501-(-5.418356)
# what is the absolute difference in predicted total delay given that one flight is on Saturday and the other is on Sunday?
-4.506943-(-5.418356)

# Problem 10 - Predictions on the Test Set
# What is the Sum of Squared Errors (SSE) on the test set?
predictTest = predict(lmAirlines, newdata=AirlinesTest)
SSE = sum((AirlinesTest$TotalDelay - predictTest)^2)
SSE
SST = sum((AirlinesTest$TotalDelay - mean(AirlinesTrain$TotalDelay))^2)
SST
1 - SSE/SST

# Problem 11 - Evaluating the Model 
# which of the following are true?
# Since our R-squared values are low, we can conclude that our independent variables only explain a small amount of the variation in the dependent variable. 

# Problem 12 - A Classification Problem
# How many flights in the dataset Airlines had no delay?
Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))
summary(Airlines$DelayClass)
Airlines$TotalDelay = NULL
set.seed(15071)
library(caTools)
split = sample.split(Airlines$DelayClass, SplitRatio = 0.7)
AirlinesTrain2 = subset(Airlines, split==TRUE)
AirlinesTest2 = subset(Airlines, split==FALSE)

# Problem 13 - A CART Model
# How many split are in the resulting tree?
library(rpart)
library(rpart.plot)
AirlinesTree = rpart(DelayClass~., data=AirlinesTrain2)
prp(AirlinesTree)

# Problem 14 - Understanding the Model
# The CART model you just built never predicts one of the three outcomes. Which one?
# Major Delay

# Problem 15 - Training Set Accuracy
# What is the overall accuracy of the model? 
TreePredictTrain = predict(AirlinesTree, data = AirlinesTrain2, type = "class")
table(AirlinesTrain2$DelayClass, TreePredictTrain)
(361+3094)/nrow(AirlinesTrain2)

# Problem 16 - A Baseline Model
# What is the accuracy on the training set of a baseline model that predicts the most frequent outcome (No Delay) for all observations?
table(AirlinesTrain2$DelayClass)
3282/nrow(AirlinesTrain2)

# Problem 17 - Testing Set Accuracy
# What is the overall accuracy of the model on the testing set?
TreePredictTest = predict(AirlinesTree, newdata = AirlinesTest2, type = "class")
table(AirlinesTest2$DelayClass, TreePredictTest)
(153+1301)/nrow(AirlinesTest2)

# Problem 18 - Understanding the Model
# What can you conclude from the CART model?
# Out of the independent variables in our dataset, the best predictor of future delays is historical delays. 
# While delays are hard to predict, using historical data can be helpful.
