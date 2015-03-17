#!/usr/bin/Rscript
# Date: 15-03-15
# Author: Liang


# Unit 2
# Climate Change

# Problem 1
# Problem 1.1 - Creating Our First Model
# Enter the model R2 (the "Multiple R-squared" value)
climate = read.csv("climate_change.csv")
training_model = subset(climate, Year < 2007)
test_model = subset(climate, Year > 2006)
TempReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=training_model)
summary(TempReg)

# Problem 1.2 - Creating Our First Model
# Which variables are significant in the model?
summary(TempReg)

# Problem 2
# Problem 2.1 - Understanding the Model
# Which of the following is the simplest correct explanation for this contradiction?
# N2O and CFC.11 are correlated with other variables in the data set

# Problem 2.2 - Understanding the Model
# Which of the following independent variables is N2O highly correlated with (absolute correlation greater than 0.7)?
# Which of the following independent variables is CFC.11 highly correlated with? Select all that apply.
cor(training_model)

# Problem 3 - Simplifying the Model
# Enter the coefficient of N2O in this reduced model
# Enter the model R2
TempReg2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data=training_model)
summary(TempReg2)

# Problem 4 - Automatically Building the Model
# Enter the R2 value of the model produced by the step function
# Which of the following variable(s) were eliminated from the full model by the step function?
ClimateStepModel = step(TempReg)
summary(ClimateStepModel)

# Problem 5 - Testing on Unseen Data
# Enter the testing set R2
TempPredictions = predict(ClimateStepModel, newdata=test_model)
SSE = sum((TempPredictions-test_model$Temp)^2)
SST = sum((mean(training_model$Temp)-test_model$Temp)^2)
R2 = 1-SSE/SST
R2

