#!/usr/bin/Rscript
# Date: 06-04-15
# Author: Liang


# Unit 5
# DETECTING VANDALISM ON WIKIPEDIA

# PROBLEM 1
# PROBLEM 1.1 - BAGS OF WORDS
# How many cases of vandalism were detected in the history of this page?
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

# PROBLEM 1.2 - BAGS OF WORDS
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusRemoved, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
# How many terms appear in dtmAdded?
dtmAdded

# PROBLEM 1.3 - BAGS OF WORDS
# How many terms appear in sparseAdded?
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# PROBLEM 1.4 - BAGS OF WORDS
# How many words are in the wordsRemoved data frame?
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)


# PROBLEM 1.5 - BAGS OF WORDS
# What is the accuracy on the test set of a baseline method that always predicts "not vandalism" (the most frequent outcome)?
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)
table(test$Vandal)
618/(618+545)

# PROBLEM 1.6 - BAGS OF WORDS
# What is the accuracy of the model on the test set, using a threshold of 0.5? 
library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal~., data=train, method="class")
pred = predict(wikiCART, newdata=test)
pred.prob = pred[,2]
table(test$Vandal, pred.prob >= 0.5)
(618+12)/(618+12+533)

# PROBLEM 1.7 - BAGS OF WORDS
# Plot the CART tree. How many word stems does the CART model use?
prp(wikiCART)

# PROBLEM 1.8 - BAGS OF WORDS
# what is the best explanation of these results?
# Although it beats the baseline, bag of words is not very predictive for this problem. 



