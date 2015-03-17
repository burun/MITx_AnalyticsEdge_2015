#!/usr/bin/Rscript
# Date: 07-03-15
# Author: Liang


# Problem 1
# Problem 1.1 - Loading the Data
# How many rows of data (observations) are in this dataset?
str(mvt)

# Problem 1.2 - Loading the Data
# How many variables are in this dataset?
str(mvt)

# Problem 1.3 - Loading the Data
# Using the "max" function, what is the maximum value of the variable "ID"?
max(mvt$ID)

# Problem 1.4 - Loading the Data
# What is the minimum value of the variable "Beat"?
min(mvt$Beat)

# Problem 1.5 - Loading the Data
# How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
summary(mvt)

# Problem 1.6 - Loading the Data 
# How many observations have a LocationDescription value of ALLEY?
summary(mvt)
table(mvt$LocationDescription)

# Problem 2
# Problem 2.1 - Understanding Dates in R 
# In what format are the entries in the variable Date?
mvt$Date[1]

# Problem 2.2 - Understanding Dates in R
# What is the month and year of the median date in our dataset? 
summary(DateConvert)

# Problem 2.3 - Understanding Dates in R 
# In which month did the fewest motor vehicle thefts occur?
table(mvt$Month)

# Problem 2.4 - Understanding Dates in R
# On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)

# Problem 2.5 - Understanding Dates in R
# Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvt$Arrest,mvt$Month)

# Problem 3
# Problem 3.1 - Visualizing Crime Trends
# In general, does it look like crime increases or decreases from 2002 - 2012?
# In general, does it look like crime increases or decreases from 2005 - 2008?
# In general, does it look like crime increases or decreases from 2009 - 2011?
hist(mvt$Date, breaks=100)

# Problem 3.2 - Visualizing Crime Trends
# Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half of the time period?
boxplot(mvt$Date ~ mvt$Arrest)

# Problem 3.3 - Visualizing Crime Trends
# For what proportion of motor vehicle thefts in 2001 was an arrest made? 
table(mvt$Arrest, mvt$Year)

# Problem 3.4 - Visualizing Crime Trends
# For what proportion of motor vehicle thefts in 2007 was an arrest made? 
table(mvt$Arrest, mvt$Year)

# Problem 3.5 - Visualizing Crime Trends
# For what proportion of motor vehicle thefts in 2012 was an arrest made? 
table(mvt$Arrest, mvt$Year)

# Problem 4
# Problem 4.1 - Popular Locations
# Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category?
sort(table(mvt$LocationDescription))

# Problem 4.2 - Popular Locations
# How many observations are in Top5?
Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")

# Problem 4.3 - Popular Locations
# One of the locations has a much higher arrest rate than the other locations. Which is it?
table(Top5$LocationDescription, Top5$Arrest)

# Problem 4.4 - Popular Locations
# On which day of the week do the most motor vehicle thefts at gas stations happen?
table(Top5$LocationDescription, Top5$Weekday)

# Problem 4.5 - Popular Locations 
# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
table(Top5$LocationDescription, Top5$Weekday)


