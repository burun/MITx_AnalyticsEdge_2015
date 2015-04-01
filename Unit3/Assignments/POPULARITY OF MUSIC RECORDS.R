#!/usr/bin/Rscript
# Date: 23-03-15
# Author: Liang


# Unit 3
# POPULARITY OF MUSIC RECORDS

# PROBLEM 1
# PROBLEM 1.1 - UNDERSTANDING THE DATA
# How many observations (songs) are from the year 2010?
songs = read.csv("songs.csv")
str(songs)
table(songs$year)

# PROBLEM 1.2 - UNDERSTANDING THE DATA
# How many songs does the dataset include for which the artist name is "Michael Jackson"?
table(songs$artistname == "Michael Jackson")
# or
MJ = subset(songs, artistname == "Michael Jackson")
str(MJ)

# PROBLEM 1.3 - UNDERSTANDING THE DATA
# Which of these songs by Michael Jackson made it to the Top 10? Select all that apply.
MJ[c("songtitle", "Top10")]

# PROBLEM 1.4 - UNDERSTANDING THE DATA
# What are the values of this variable that occur in our dataset?
# Which timesignature value is the most frequent among songs in our dataset?
table(songs$timesignature)

# PROBLEM 1.5 - UNDERSTANDING THE DATA
# Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
songs$songtitle[which.max(songs$tempo)]


# PROBLEM 2
# PROBLEM 2.1 - CREATING OUR PREDICTION MODEL
# How many observations (songs) are in the training set?
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
str(SongsTrain)

# PROBLEM 2.2 - CREATING OUR PREDICTION MODEL
# Looking at the summary of your model, what is the value of the Akaike Information Criterion (AIC)?
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

# PROBLEM 2.3 - CREATING OUR PREDICTION MODEL
# What does the model suggest?
# The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10

# PROBLEM 2.4 - CREATING OUR PREDICTION MODEL
# What does Model 1 suggest in terms of complexity?
# Mainstream listeners tend to prefer less complex songs

# PROBLEM 2.5 - CREATING OUR PREDICTION MODEL
# By inspecting the coefficient of the variable "loudness", what does Model 1 suggest?
# Mainstream listeners prefer songs with heavy instrumentation 
# No


# PROBLEM 3
# PROBLEM 3.1 - BEWARE OF MULTICOLLINEARITY ISSUES!
# What is the correlation between the variables "loudness" and "energy" in the training set?
cor(SongsTrain$loudness, SongsTrain$energy)

# PROBLEM 3.2 - BEWARE OF MULTICOLLINEARITY ISSUES!  
# Look at the summary of SongsLog2, and inspect the coefficient of the variable "energy". What do you observe?
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
# subtracting the variable from the model formula will always work when you want to remove numeric variables.
summary(SongsLog2)
# Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1.

# PROBLEM 3.3 - BEWARE OF MULTICOLLINEARITY ISSUES!
# do we make the same observation about the popularity of heavy instrumentation as we did with Model 2?
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)
# Yes


# PROBLEM 4
# PROBLEM 4.1 - VALIDATING OUR MODEL
# What is the accuracy of Model 3 on the test set, using a threshold of 0.45?
pred3 = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, pred3 >= 0.45)
(309+19)/(309+5+40+19)

# PROBLEM 4.2 - VALIDATING OUR MODEL
# What would the accuracy of the baseline model be on the test set?
table(SongsTest$Top10)
314/(309+5+40+19)

# PROBLEM 4.3 - VALIDATING OUR MODEL
# How many songs does Model 3 correctly predict as Top 10 hits in 2010, using a threshold of 0.45?
# How many non-hit songs does Model 3 predict will be Top 10 hits, using a threshold of 0.45?
table(SongsTest$Top10, pred3 >= 0.45)

# PROBLEM 4.4 - VALIDATING OUR MODEL
# What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?
19/(40+19)
# What is the specificity of Model 3 on the test set, using a threshold of 0.45?
309/(309+5)

# PROBLEM 4.5 - VALIDATING OUR MODEL
# What conclusions can you make about our model? (Select all that apply.)
# Model 3 favors specificity over sensitivity.
# Model 3 provides conservative predictions, and predicts that a song will make it to the Top 10 very rarely. So while it detects less than half of the Top 10 songs, we can be very confident in the songs that it does predict to be Top 10 hits.
