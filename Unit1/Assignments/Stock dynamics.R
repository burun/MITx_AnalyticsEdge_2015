#!/usr/bin/Rscript
# Date: 08-03-15
# Author: Liang


# Stock dynamics

# Problem 1
# Problem 1.1 - Summary Statistics 
# How many observations are there in each data set? 
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
Boeing = read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

str(IBM)

# Problem 1.2 - Summary Statistics
# What is the earliest year in our datasets?
str(IBM)

# Problem 1.3 - Summary Statistics
# What is the latest year in our datasets?
summary(IBM)

# Problem 1.4 - Summary Statistics
# What is the mean stock price of IBM over this time period?
summary(IBM)

# Problem 1.5 - Summary Statistics
# What is the minimum stock price of General Electric (GE) over this time period?
summary(GE)

# Problem 1.6 - Summary Statistics
# What is the maximum stock price of Coca-Cola over this time period?
summary(CocaCola)

# Problem 1.7 - Summary Statistics
# What is the median stock price of Boeing over this time period?
summary(Boeing)

# Problem 1.8 - Summary Statistics
# What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)


# Problem 2
# Problem 2.1 - Visualizing Stock Dynamics
# Around what year did Coca-Cola has its highest stock price in this time period?
plot(CocaCola$Date, CocaCola$StockPrice, type="l")

# Problem 2.2 - Visualizing Stock Dynamics
# In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more? 
lines(ProcterGamble$Date, ProcterGamble$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

# Problem 2.3 - Visualizing Stock Dynamics
# Around 1983, the stock for one of these companies (Coca-Cola or Procter and Gamble) was going up, while the other was going down. Which one was going up?
abline(v=as.Date(c("1983-01-01")), lwd=2)


# Problem 3
# Problem 3.1 - Visualizing Stock Dynamics 1995-2005
# Which stock fell the most right after the technology bubble burst in March 2000?
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
abline(v=as.Date(c("2000-03-01")), lwd=2)

# Problem 3.2 - Visualizing Stock Dynamics 1995-2005
# Which stock reaches the highest value in the time period 1995-2005?
# Code are the same as before

# Problem 3.3 - Visualizing Stock Dynamics 1995-2005
# Comparing September 1997 to November 1997, which companies saw a decreasing trend in their stock price? 
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-30")), lwd=2)

# Problem 3.4 - Visualizing Stock Dynamics 1995-2005
# In the last two years of this time period (2004 and 2005) which stock seems to be performing the best, in terms of increasing stock price?
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-31")), lwd=2)

# Problem 4.1 - Monthly Trends
# For IBM, compare the monthly averages to the overall average stock price. In which months has IBM historically had a higher stock price (on average)? Select all that apply.
tapply(IBM$StockPrice, months(IBM$Date), mean, na.rm=TRUE)
summary(IBM$StockPrice)

# Problem 4.2 - Monthly Trends
# General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this?
tapply(GE$StockPrice, months(GE$Date), mean, na.rm=TRUE)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean, na.rm=TRUE)

# Problem 4.3 - Monthly Trends
# For the months of December and January, every company's average stock is higher in one month and lower in the other. In which month are the stock prices lower?
# Code are the same as before