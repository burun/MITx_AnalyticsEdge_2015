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


# PROBLEM 2
# PROBLEM 2.1 - PROBLEM-SPECIFIC KNOWLEDGE
# Based on this new column, how many revisions added a link?
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

# PROBLEM 2.2 - PROBLEM-SPECIFIC KNOWLEDGE
# What is the new accuracy of the CART model on the test set, using a threshold of 0.5?
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
HTTPCart = rpart(Vandal ~ ., data=wikiTrain2)
HTTPpred = predict(HTTPCart, newdata=wikiTest2)
HTTPpred.prob = HTTPpred[,2]
table(wikiTest2$Vandal, HTTPpred.prob >= 0.5)
(609+57)/(609+57+488)

############### PROBLEM 2.3 - PROBLEM-SPECIFIC KNOWLEDGE
# What is the average number of words added?
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

# PROBLEM 2.4 - PROBLEM-SPECIFIC KNOWLEDGE
# What is the new accuracy of the CART model on the test set?
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART3)
(514+248)/(514+104+297+248)


# PROBLEM 3
# PROBLEM 3.1 - USING NON-TEXTUAL DATA
# What is the accuracy of the model on the test set?
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
testPredictCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, testPredictCART4)
(595+241)/(595+23+304+241)

# PROBLEM 3.2 - USING NON-TEXTUAL DATA
# Plot the CART tree. How many splits are there in the tree?
prp(wikiCART4)
