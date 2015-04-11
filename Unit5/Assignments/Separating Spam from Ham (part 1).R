#!/usr/bin/Rscript
# Date: 11-04-15
# Author: Liang


# Unit 5
# SEPARATING SPAM FROM HAM (PART 1)


# PROBLEM 1
# PROBLEM 1.1 - LOADING THE DATASE
# How many emails are in the dataset?
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)

# PROBLEM 1.2 - LOADING THE DATASET
# How many of the emails are spam?
table(emails$spam)

# PROBLEM 1.3 - LOADING THE DATASET
# Which word appears at the beginning of every email in the dataset?
# subject

# PROBLEM 1.4 - LOADING THE DATASET
# Could a spam classifier potentially benefit from including the frequency of the word that appears in every email?
# Yes -- the number of times the word appears might help us differentiate spam from ham. 

# PROBLEM 1.5 - LOADING THE DATASET
# How many characters are in the longest email in the dataset 
summary(nchar(emails$text))

# PROBLEM 1.6 - LOADING THE DATASET
# Which row contains the shortest email in the dataset?
which.min(nchar(emails$text))


PROBLEM 2
PROBLEM 2.1 - PREPARING THE CORPUS
# How many terms are in dtm?
library(tm)
corpusText = Corpus(VectorSource(emails$text))
corpusText = tm_map(corpusText, tolower)
corpusText = tm_map(corpusText, PlainTextDocument)
corpusText = tm_map(corpusText, removePunctuation)
corpusText = tm_map(corpusText, removeWords, stopwords("english"))
corpusText = tm_map(corpusText, stemDocument)

dtmText = DocumentTermMatrix(corpusText)
dtmText = as.data.frame(as.matrix(dtmText))
str(dtmText)

# PROBLEM 2.2 - PREPARING THE CORPUS
# How many terms are in spdtm?
spdtm = DocumentTermMatrix(corpusText)
spdtm = removeSparseTerms(spdtm, 0.95)
spdtm = as.data.frame(as.matrix(spdtm))
str(spdtm)

# PROBLEM 2.3 - PREPARING THE CORPUS
# What is the word stem that shows up most frequently across all the emails in the dataset?
emailsSparse = spdtm
colnames(emailsSparse) = make.names(colnames(emailsSparse))
sort(colSums(emailsSparse))

# PROBLEM 2.4 - PREPARING THE CORPUS
# How many word stems appear at least 5000 times in the ham emails in the dataset?
emailsSparse$spam = emails$spam
table(colSums(emailsSparse[emailsSparse$spam==0,names(emailsSparse) !="spam"]) >= 5000)

# PROBLEM 2.5 - PREPARING THE CORPUS
# How many word stems appear at least 1000 times in the spam emails in the dataset?
table(colSums(emailsSparse[emailsSparse$spam==1,names(emailsSparse) !="spam"]) >= 1000)

# PROBLEM 2.6 - PREPARING THE CORPUS
# What does this likely imply?
# The frequencies of these most common words are likely to help differentiate between spam and ham.

# PROBLEM 2.7 - PREPARING THE CORPUS
# What does this mean about the applicability of the text analytics models we will train for the spam filtering problem?
# The models we build are personalized, and would need to be further tested before being used as a spam filter for another person. 


# PROBLEM 3
# PROBLEM 3.1 - BUILDING MACHINE LEARNING MODELS
emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam ~ ., data=train, family="binomial")
trainLogPred = predict(spamLog, data=train, type="response")

# How many of the training set predicted probabilities from spamLog are less than 0.00001?
table(trainLogPred < 0.00001)
# How many of the training set predicted probabilities from spamLog are more than 0.99999?
table(trainLogPred > 0.99999)
# How many of the training set predicted probabilities from spamLog are between 0.00001 and 0.99999?
table(trainLogPred >= 0.00001 & trainLogPred <= 0.99999)


# PROBLEM 3.2 - BUILDING MACHINE LEARNING MODELS
# How many variables are labeled as significant (at the p=0.05 level) in the logistic regression summary output?
summary(spamLog)

# PROBLEM 3.3 - BUILDING MACHINE LEARNING MODELS
# How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree?
library(rpart)
library(rpart.plot)
spamCART = rpart(spam~., data=train, method="class")
prp(spamCART)

# PROBLEM 3.4 - BUILDING MACHINE LEARNING MODELS
# What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(train$spam, trainLogPred > 0.5)
(3052+954)/(3052+954+4)

# PROBLEM 3.5 - BUILDING MACHINE LEARNING MODELS
# What is the training set AUC of spamLog?
library(ROCR)
LogROCRpred = prediction(trainLogPred, train$spam)
as.numeric(performance(LogROCRpred, "auc")@y.values)

# PROBLEM 3.6 - BUILDING MACHINE LEARNING MODELS
# What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions?
trainCARTpred = predict(spamCART, data=train)
trainCARTpred.prob = trainCARTpred[,2]
table(train$spam, trainCARTpred.prob >= 0.5)
(2885+894)/nrow(train)

# PROBLEM 3.7 - BUILDING MACHINE LEARNING MODELS
# What is the training set AUC of spamCART?
trainpredROCR = prediction(trainCARTpred.prob, train$spam)
performance(trainpredROCR, "auc")@y.values

# PROBLEM 3.8 - BUILDING MACHINE LEARNING MODELS
# What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions?
library(randomForest)
set.seed(123)
spamRF = randomForest(spam~., data=train)
trainRFPred = predict(spamRF, data=train, type="prob")[,2]
table(train$spam, trainRFPred>0.5)
(3013+914)/(3013+39+44+914)

# PROBLEM 3.9 - BUILDING MACHINE LEARNING MODELS
# What is the training set AUC of spamRF?
trainRFROCR = prediction(trainRFPred, train$spam)
performance(trainRFROCR, "auc")@y.values

# PROBLEM 3.10 - BUILDING MACHINE LEARNING MODELS
# Which model had the best training set performance, in terms of accuracy and AUC?
# Logistic regression


# PROBLEM 4
# PROBLEM 4.1 - EVALUATING ON THE TEST SET
# What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
testLogPred = predict(spamLog, newdata=test, type="response")
table(test$spam, testLogPred > 0.5)
(1257+376)/nrow(test)

# PROBLEM 4.2 - EVALUATING ON THE TEST SET
# What is the testing set AUC of spamLog?
LogROCRpred_test = prediction(testLogPred, test$spam)
as.numeric(performance(LogROCRpred_test, "auc")@y.values)

# PROBLEM 4.3 - EVALUATING ON THE TEST SET
# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
testCARTpred = predict(spamCART, newdata=test)[,2]
table(test$spam, testCARTpred >= 0.5)
(1228+386)/nrow(test)

# PROBLEM 4.4 - EVALUATING ON THE TEST SET  
# What is the testing set AUC of spamCART?
testpredROCR = prediction(testCARTpred, test$spam)
performance(testpredROCR, "auc")@y.values

# PROBLEM 4.5 - EVALUATING ON THE TEST SET
# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
testRFPred = predict(spamRF, newdata=test, type="prob")[,2]
table(test$spam, testRFPred>0.5)
(1290+385)/nrow(test)

# PROBLEM 4.6 - EVALUATING ON THE TEST SET
# What is the testing set AUC of spamRF?
testRFROCR = prediction(testRFPred, test$spam)
performance(testRFROCR, "auc")@y.values

# PROBLEM 4.7 - EVALUATING ON THE TEST SET
# Which model had the best testing set performance, in terms of accuracy and AUC?
# Random forest

# PROBLEM 4.8 - EVALUATING ON THE TEST SET
# Which model demonstrated the greatest degree of overfitting?
# Logistic regression
