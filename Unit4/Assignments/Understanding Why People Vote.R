#!/usr/bin/Rscript
# Date: 03-04-15
# Author: Liang


# Unit 4
# UNDERSTANDING WHY PEOPLE VOTE

# PROBLEM 1
# PROBLEM 1.1 - EXPLORATION AND LOGISTIC REGRESSION
# What proportion of people in this dataset voted in this election?
vote = read.csv("gerber.csv")
table(vote$voting)
108696/(108696+235388)

# PROBLEM 1.2 - EXPLORATION AND LOGISTIC REGRESSION
# Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
str(vote)
table(vote$voting[vote$hawthorne==1])
table(vote$voting[vote$civicduty==1])
table(vote$voting[vote$neighbors==1])
table(vote$voting[vote$self==1])

# PROBLEM 1.3 - EXPLORATION AND LOGISTIC REGRESSION
# Which of the following coefficients are significant in the logistic regression model? 
voteLog = glm(voting ~ hawthorne + civicduty + neighbors + self, data=vote, family="binomial")
summary(voteLog)

# PROBLEM 1.4 - EXPLORATION AND LOGISTIC REGRESSION
# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
votepred_1 = predict(voteLog, vote, type="response")
table(vote$voting, votepred_1 >= 0.3)
(134513+51966)/(134513+100875+56730+51966)

# PROBLEM 1.5 - EXPLORATION AND LOGISTIC REGRESSION
# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(vote$voting, votepred_1 >= 0.5)
235388/(235388+108696)

# PROBLEM 1.6 - EXPLORATION AND LOGISTIC REGRESSION
# What is happening here?
library(ROCR)
ROCRpred = prediction(votepred_1, vote$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)


# PROBLEM 2
# PROBLEM 2.1 - TREES
# Plot the tree. What happens, and if relevant, why?
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=vote)
prp(CARTmodel)

# PROBLEM 2.2 - TREES
# What do you observe about the order of the splits?
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=vote, cp=0.0)
prp(CARTmodel2)

# PROBLEM 2.3 - TREES
# Using only the CART tree plot, determine what fraction (a number between 0 and 1) of "Civic Duty" people voted:
votepred_2 = predict(CARTmodel, data = vote)
table(vote$voting[vote$civicduty==1], votepred_2[vote$civicduty==1])
12021/(26197+12021)

# PROBLEM 2.4 - TREES
# In the control group, which gender is more likely to vote?
# In the "Civic Duty" group, which gender is more likely to vote?
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=vote, cp=0.0)
prp(CARTmodel3)


# PROBLEM 3
# PROBLEM 3.1 - INTERACTION TERMS
CARTmodel4 = rpart(voting ~ control, data=vote, cp=0.0)
CARTmodel5 = rpart(voting ~ control + sex, data=vote, cp=0.0)
prp(CARTmodel4, digits = 6)
abs(0.296638-0.34)

# PROBLEM 3.2 - INTERACTION TERMS
# determine who is affected more by NOT being in the control group 
prp(CARTmodel5, digits=6)

# PROBLEM 3.3 - INTERACTION TERMS
# Interpret the coefficient for "sex":
voteLog_sexcontrol = glm(voting ~ sex + control, data=vote, family="binomial")
summary(voteLog_sexcontrol)

# PROBLEM 3.4 - INTERACTION TERMS
# What is the absolute difference between the tree and the logistic regression for the (Woman, Control) case?
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(voteLog_sexcontrol, newdata=Possibilities, type="response")
prp(CARTmodel5, digits=6)
abs(0.2908065-0.290456)

# PROBLEM 3.5 - INTERACTION TERMS
# How do you interpret the coefficient for the new variable in isolation?
voteLog_sexcontrol2 = glm(voting ~ sex + control + sex:control, data=vote, family="binomial")
summary(voteLog_sexcontrol2)
# If a person is a woman and in the control group, the chance that she voted goes down

# PROBLEM 3.6 - INTERACTION TERMS
# Now what is the difference between the logistic regression model and the CART model for the (Woman, Control) case? 
predict(voteLog_sexcontrol2, newdata=Possibilities, type="response")
abs(0.2904558 -0.290456)

# PROBLEM 3.7 - INTERACTION TERMS
# Should we always include all possible interaction terms of the independent variables when building a logistic regression model?
# No
