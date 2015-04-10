#!/usr/bin/Rscript
# Date: 10-04-15
# Author: Liang


# Unit 5
# AUTOMATING REVIEWS IN MEDICINE

# PROBLEM 1
# PROBLEM 1.1 - LOADING THE DATA
# How many characters are there in the longest abstract?
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE, encoding="latin1")
summary(trials)
str(trials)
summary(nchar(trials$abstract)) 

# PROBLEM 1.2 - LOADING THE DATA
# How many search results provided no abstract?
table(nchar(trials$abstract)==0)


# PROBLEM 2
# PROBLEM 2.1 - PREPARING THE CORPUS
library(tm)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

# How many terms remain in dtmTitle after removing sparse terms (aka how many columns does it have)?
str(dtmTitle)
# How many terms remain in dtmAbstract?
str(dtmAbstract)

# PROBLEM 2.2 - PREPARING THE CORPUS
# What is the most likely reason why dtmAbstract has so many more terms than dtmTitle?
# Abstracts tend to have many more words than titles

# PROBLEM 2.3 - PREPARING THE CORPUS
# What is the most frequent word stem across all the abstracts?
sort(colSums(dtmAbstract))


# PROBLEM 3
# PROBLEM 3.1 - BUILDING A MODEL
# What was the effect of these functions?
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
# Adding the letter T in front of all the title variable names and adding the letter A in front of all the abstract variable names.

# PROBLEM 3.2 - BUILDING A MODEL
# How many columns are in this combined data frame?
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trials = trials$trial
str(dtm)

# PROBLEM 3.3 - BUILDING A MODEL
# What is the accuracy of the baseline model on the training set? 
library(caTools)
set.seed(144)
spl = sample.split(dtm$trials, 0.7)

train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
table(train$trials)
730/(730+572)

# PROBLEM 3.4 - BUILDING A MODEL
# What is the name of the first variable the model split on?
library(rpart)
library(rpart.plot)
trialCART = rpart(trials~., data=train, method="class")
prp(trialCART)

# PROBLEM 3.5 - BUILDING A MODEL
# What is the maximum predicted probability for any result?
predTrain = predict(trialCART, data=train)
max(predTrain[,2])

# PROBLEM 3.6 - BUILDING A MODEL
# Without running the analysis, how do you expect the maximum predicted probability to differ in the testing set?
# The maximum predicted probability will likely be exactly the same in the testing set.

# PROBLEM 3.7 - BUILDING A MODEL
predTrain.prob = predTrain[,2]
table(train$trials, predTrain.prob >= 0.5)
# What is the training set accuracy of the CART model?
(631+441)/(631+99+131+441)
# What is the training set sensitivity of the CART model?
441/(131+441)
# What is the training set specificity of the CART model?
631/(631+99)


# PROBLEM 4
# PROBLEM 4.1 - EVALUATING THE MODEL ON THE TESTING SET
# What is the testing set accuracy, assuming a probability threshold of 0.5 for predicting that a result is a clinical trial?
predTest = predict(trialCART, newdata=test)
predTest.prob = predTest[,2]
table(test$trials, predTest.prob >= 0.5)
(261+162)/(261+52+83+162)

# PROBLEM 4.2 - EVALUATING THE MODEL ON THE TESTING SET
# Using the ROCR package, what is the testing set AUC of the prediction model?
library(ROCR)
predROCR = prediction(predTest.prob, test$trials)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values


# PART 5: DECISION-MAKER TRADEOFFS
# PROBLEM 5.1 - DECISION-MAKER TRADEOFFS
# What is the cost associated with the model in Step 1 making a false negative prediction?
# A paper that should have been included in Set A will be missed, affecting the quality of the results of Step 3.

# PROBLEM 5.2 - DECISION-MAKER TRADEOFFS
# What is the cost associated with the model in Step 1 making a false positive prediction?
# A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the process but not affecting the quality of the results of Step 3.

# PROBLEM 5.3 - DECISION-MAKER TRADEOFFS
# Given the costs associated with false positives and false negatives, which of the following is most accurate?
# A false negative is more costly than a false positive; the decision maker should use a probability threshold less than 0.5 for the machine learning model.
