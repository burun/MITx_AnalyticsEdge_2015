#!/usr/bin/Rscript
# Date: 16-03-15
# Author: Liang


# Unit 2
# Detecting Flu Epidemics via Search Engine Query Data 

# Problem 1
# Problem 1.1 - Understanding the Data
# which week corresponds to the highest percentage of ILI-related physician visits?
# Which week corresponds to the highest percentage of ILI-related query fraction?
FluTrain = read.csv("FluTrain.csv")
summary(FluTrain)
sort(tapply(FluTrain$ILI, FluTrain$Week, max))
sort(tapply(FluTrain$Queries, FluTrain$Week, max))

# or
subset(FluTrain, Queries == max(Queries))
subset(FluTrain, ILI == max(ILI))

# Problem 1.2 - Understanding the Data
# What best describes the distribution of values of ILI?
hist(FluTrain$ILI)

# Problem 1.3 - Understanding the Data
# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?
plot(FluTrain$Queries, log(FluTrain$ILI))

# Problem 2
# Problem 2.1 - Linear Regression Model
# which model best describes our estimation problem?
# log(ILI) = intercept + coefficient x Queries, where the coefficient is positive

# Problem 2.2 - Linear Regression Model
# What is the training set R-squared value for FluTrend1 model (the "Multiple R-squared")?
FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)
 
# Problem 2.3 - Linear Regression Model
# What is the relationship we infer from our problem?
cor(log(FluTrain$ILI), FluTrain$Queries)

# Problem 3
# Problem 3.1 - Performance on the Test Set
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
# What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
# What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012?
(FluTest$ILI[11] - 2.187378)/FluTest$ILI[11]
# What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of ILI-related physician visits, on the test set?
SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
RMSE

# Problem 4
# Problem 4.1 - Training a Time Series Model
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
# How many values are missing in the new ILILag2 variable?
sum(is.na(FluTrain$ILILag2))
# or
summary(FluTrain$ILILag2)

# Problem 4.2 - Training a Time Series Model
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
# There is a strong positive relationship between log(ILILag2) and log(ILI). 

# Problem 4.3 - Training a Time Series Model 
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)

# Problem 4.4 - Training a Time Series Model 
# FluTrend2 is a stronger model than FluTrend1 on the training set.

# Problem 5
# Problem 5.1 - Evaluating the Time Series Model in the Test Set
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)

# Problem 5.2 - Evaluating the Time Series Model in the Test Set
# Which value should be used to fill in the ILILag2 variable for the first observation in FluTest?
# The ILI value of the second-to-last observation in the FluTrain data frame. 
# Which value should be used to fill in the ILILag2 variable for the second observation in FluTest?
# The ILI value of the last observation in the FluTrain data frame. 

# Problem 5.3 - Evaluating the Time Series Model in the Test Set
# What is the new value of the ILILag2 variable in the first row of FluTest?
# What is the new value of the ILILag2 variable in the second row of FluTest?
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]
FluTest$ILILag2[1]
FluTest$ILILag2[2]

# Problem 5.4 - Evaluating the Time Series Model in the Test Set
# What is the test-set RMSE of the FluTrend2 model?
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE2 = sum((FluTest$ILI - PredTest2)^2)
RMSE2 = sqrt(SSE2/nrow(FluTest))
RMSE2

# Problem 5.5 - Evaluating the Time Series Model in the Test Set
# Which model obtained the best test-set RMSE?
# FluTrend2

