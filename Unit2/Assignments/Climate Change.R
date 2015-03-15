# Unit 2
# Climate Change

# Problem 1
# Problem 1.1 - Creating Our First Model
climate = read.csv("climate_change.csv")
training_model = subset(climate, Year < 2007)
test_model = subset(climate, Year > 2006)
TempReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=training_model)
summary(TempReg)

# Problem 1.2 - Creating Our First Model
summary(TempReg)

# Problem 2
# Problem 2.1 - Understanding the Model
# N2O and CFC.11 are correlated with other variables in the data set

# Problem 2.2 - Understanding the Model
cor(training_model)

# Problem 3 - Simplifying the Model
TempReg2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data=training_model)
summary(TempReg2)

# Problem 4 - Automatically Building the Model
ClimateStepModel = step(TempReg)
summary(ClimateStepModel)

# Problem 5 - Testing on Unseen Data
TempPredictions = predict(ClimateStepModel, newdata=test_model)
SSE = sum((TempPredictions-test_model$Temp)^2)
SST = sum((mean(training_model$Temp)-test_model$Temp)^2)
R2 = 1-SSE/SST
R2

