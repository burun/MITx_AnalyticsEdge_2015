#!/usr/bin/Rscript
# Date: 21-05-15
# Author: Liang


# Final Exam
# Predicting Sales on eBay

# Problem 1 - Loading the Data 
# What proportion of all shoes were sold?
eBay = read.csv("https://courses.edx.org/c4x/MITx/15.071x_2/asset/ebay.csv", stringsAsFactors=FALSE)
table(eBay$sold)
799/(799+2997)

# Problem 2 - Missing Values
# Which of the numerical variables has at least one missing value?
summary(eBay)

# Problem 3 - Most common shoe size
# What is the most common shoe size in the dataset?
table(eBay$size)

# Problem 4 - Converting Variables to Factors
# Which of the following methods requires the dependent variable be stored as a factor variable when training a model for classification?
eBay$sold = as.factor(eBay$sold)
eBay$condition = as.factor(eBay$condition)
eBay$heel = as.factor(eBay$heel)
eBay$style = as.factor(eBay$style)
eBay$color = as.factor(eBay$color)
eBay$material = as.factor(eBay$material)
# Random forest (randomForest)

# Problem 5 - Splitting into a Training and Testing Set
# Why do we use the sample.split() function to split into a training and testing set? 
set.seed(144)
library(caTools)
spl = sample.split(eBay$sold, 0.7)
Train = subset(eBay, spl == TRUE)
Test = subset(eBay, spl == FALSE)
# It balances the dependent variable between the training and testing sets

# Problem 6 - Training a Logistic Regression Model
# Which of the following characteristics of a shoe are statistically significantly (p < 0.05, aka at least a * in the regression summary) associated with a lower chance of an item being sold?
glmEbay = glm(sold~biddable + startprice + condition + heel + style + color + material, data=Train, family="binomial")
summary(glmEbay)

# Problem 7 - Predicting Using a Logistic Regression Model
# What is the predicted probability that this shoe will be sold according to the logistic regression model?
x = 0.5990788+(-0.0044423)*100+(-0.4952981)+0.1224260+0.2226547+(-1.1078098)
# y = exp(x) * (1-y)

# Problem 8 - Interpreting Model Coefficients
# What is the meaning of the coefficient labeled "styleStiletto" in the logistic regression summary output?
# Stilettos are predicted to have 129.9% higher odds of being sold than an otherwise identical open-toed shoe. 

# Problem 9 - Obtaining Test Set Predictions
# how many observations does the logistic regression model make a different prediction than the naive baseline model?
glmpred = predict(glmEbay, newdata=Test, type="response")
table(Test$sold, glmpred >= 0.5)
table(Test$sold)
(877+182-899)/2

# Problem 10 - Computing Test-Set AUC
# What is the test-set AUC of the logistic regression model?
library(ROCR)
ROCRpred = prediction(glmpred, Test$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Problem 11 - Computing Test-Set AUC
# What is the meaning of the AUC?
# The proportion of the time the model can differentiate between a randomly selected shoe that was sold and a randomly selected shoe that was not sold 

# Problem 12 - ROC Curves
# Which logistic regression threshold is associated with the upper-right corner of the ROC plot?
# 0

# Problem 13 - ROC Curves
# At roughly which logistic regression cutoff does the model achieve a true positive rate of 80% and a false positive rate of 50%?
ROCRpred = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred, colorize=TRUE)
# 0.16

# Problem 14 - Cross-Validation to Select Parameters
# Which of the following best describes how 10-fold cross-validation works when selecting between 3 different parameter values?
# 30 models are trained on subsets of the training set and evaluated on a portion of the training set 

# Problem 15 - Cross-Validation for a CART Model
# What cp value maximizes the cross-validation accuracy?
set.seed(144)
library(caret)
library(e1071)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid( .cp = (0:50)*0.001)
tr = train(sold ~ biddable+ startprice + condition + heel + style + color + material, data = Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr

# Problem 16 - Train CART Model
# What variable is used most frequently as a split in the tree?
library(rpart)
library(rpart.plot)
CARTebay = rpart(sold ~ biddable+ startprice + condition + heel + style + color + material, data = Train, cp=0.005)
prp(CARTebay)

# Problem 17 - Building a Corpus from Item Descriptions
# How many unique word stems are in dtm?
library(tm)
corpusDescription = Corpus(VectorSource(eBay$description))
corpusDescription = tm_map(corpusDescription, tolower)
corpusDescription = tm_map(corpusDescription, PlainTextDocument)
corpusDescription = tm_map(corpusDescription, removePunctuation)
corpusDescription = tm_map(corpusDescription, removeWords, stopwords("english"))
corpusDescription = tm_map(corpusDescription, stemDocument)
dtmDescription = DocumentTermMatrix(corpusDescription)
str(dtmDescription)

# Problem 18 - Removing Sparse Terms
# How many unique terms are in spdtm? 
spdtm = removeSparseTerms(dtmDescription, 0.9)
spdtm

# Problem 19 - Evaluating Word Frequencies in a Corpus
# Which word stem appears the most frequently across all descriptions? 
spdtm = as.data.frame(as.matrix(spdtm))
DescriptionSparse = spdtm
colnames(DescriptionSparse) = make.names(colnames(DescriptionSparse))
sort(colSums(DescriptionSparse))

# Problem 20 - Adding Data from Original Data Frame
# How many variables are in testText? 
colnames(spdtm) = paste0("D", colnames(spdtm))
spdtm$sold = eBay$sold
spdtm$biddable = eBay$biddable
spdtm$startprice = eBay$startprice
spdtm$condition = eBay$condition
spdtm$heel = eBay$heel
spdtm$style = eBay$style
spdtm$color = eBay$color
spdtm$material = eBay$material

spl = sample.split(spdtm$sold, 0.7)
trainText = subset(spdtm, spl == TRUE)
testText = subset(spdtm, spl == FALSE)
str(testText)

# Problem 21 - Training Another Logistic Regression Model 
# How many of the word frequencies from the description text (variables beginning with the letter "D") are significant at the p=0.05 level?
glmText = glm(sold~., data=trainText, family="binomial")
summary(glmText)

# Problem 22 - Test Set AUC of New Logistic Regression Model 
# What is the training-set AUC of the new logistic regression model? 
predictTrainText = predict(glmText, type="response", data=trainText)
ROCRpred = prediction(predictTrainText, trainText$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)
# What is the test-set AUC of the new logistic regression model? 
predictTestText = predict(glmText, type="response", newdata=testText)
ROCRpred = prediction(predictTestText, testText$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Problem 23 - Assessing Overfitting of New Model
# What is the most accurate description of the new logistic regression model? 
# glmText is overfitted, and removing variables would improve its test-set performance. 
