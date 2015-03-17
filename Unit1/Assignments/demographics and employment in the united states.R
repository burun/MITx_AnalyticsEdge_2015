#!/usr/bin/Rscript
# Date: 08-03-15
# Author: Liang


# demographics and employment in the united states

# Problem 1
# Problem 1.1 - Loading and Summarizing the Dataset
# How many interviewees are in the dataset?
CPS = read.csv("CPSData.csv")
str(CPS)

# Problem 1.2 - Loading and Summarizing the Dataset 
# Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment?
table(CPS$Industry)

# Problem 1.3 - Loading and Summarizing the Dataset
# Which state has the fewest interviewees?
sort(table(CPS$State))

# Problem 1.4 - Loading and Summarizing the Dataset
# What proportion of interviewees are citizens of the United States?
table(CPS$Citizenship)

# Problem 1.5 - Loading and Summarizing the Dataset
# For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
table(CPS$Race, CPS$Hispanic)


# Problem 2
# Problem 2.1 - Evaluating Missing Values 
# Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPS)

# Problem 2.2 - Evaluating Missing Values
# Which is the most accurate
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

# Problem 2.3 - Evaluating Missing Values
# How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)? 
table(CPS$State, is.na(CPS$MetroAreaCode))

# Problem 2.4 - Evaluating Missing Values
# Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))

# Problem 2.5 - Evaluating Missing Values 
# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
# Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all interviewees were non-metropolitan?
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))


# Problem 3
# Problem 3.1 - Integrating Metropolitan Area Data
# How many observations (codes for metropolitan areas) are there in MetroAreaMap?
# How many observations (codes for countries) are there in CountryMap?
Country = read.csv("CountryCodes.csv")
str(Country)
Metro = read.csv("MetroAreaCodes.csv")
str(Metro)

# Problem 3.2 - Integrating Metropolitan Area Data
# What is the name of the variable that was added to the data frame by the merge() operation?
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)

# Problem 3.3 - Integrating Metropolitan Area Data
# Which of the following metropolitan areas has the largest number of interviewees?
table(CPS$MetroArea)

# Problem 3.4 - Integrating Metropolitan Area Data
# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

# Problem 3.5 - Integrating Metropolitan Area Data 
# Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether an interviewee is Asian, determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

# Problem 3.6 - Integrating Metropolitan Area Data 
# determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma.
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

# Problem 4
# Problem 4.1 - Integrating Country of Birth Data 
# What is the name of the variable added to the CPS data frame by this merge operation?
# How many interviewees have a missing value for the new country of birth variable?
CPS = merge(CPS, Country, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)
summary(CPS)

# Problem 4.2 - Integrating Country of Birth Data
# Among all interviewees born outside of North America, which country was the most common place of birth?
summary(CPS)

# Problem 4.3 - Integrating Country of Birth Data
# What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States? 
tapply(CPS$Country=="United States", CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA", summary, rm.na=TRUE)
1668/(1668+3736)

# Problem 4.4 - Integrating Country of Birth Data
# Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India? Hint -- remember to include na.rm=TRUE if you are using tapply() to answer this question.
tapply(CPS$MetroArea, CPS$Country=="India", summary, rm.na=TRUE)
# In Brazil?
tapply(CPS$MetroArea, CPS$Country=="Brazil", summary, rm.na=TRUE)
# In Somalia?
tapply(CPS$MetroArea, CPS$Country=="Somalia", summary, rm.na=TRUE)
