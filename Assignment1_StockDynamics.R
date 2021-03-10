# Assignment 1
# Stock Dynamics

# Call the data frames "IBM", "GE", "ProcterGamble", "CocaCola", and "Boeing", respectively. 
# Each data frame has two variables, described as follows:
  
#   Date: the date of the stock price, always given as the first of the month.
#   StockPrice: the average stock price of the company in the given month.

IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

# Convert dates into a format that R can understand
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

# 480 observations in each data set

str(IBM)
summary(IBM)
sd(IBM$StockPrice)

str(GE)
summary(GE)
sd(GE$StockPrice)

str(CocaCola)
summary(CocaCola)
sd(CocaCola$StockPrice)

str(ProcterGamble)
summary(ProcterGamble)
sd(ProcterGamble$StockPrice)

str(Boeing)
summary(Boeing)
sd(Boeing$StockPrice)

# Mean StockPrice of IBM
mean(IBM$StockPrice)

# Minimum StockPrice of GE
min(GE$StockPrice)

# Maximum StockPrice of CocaCola
max(CocaCola$StockPrice)

# Median StockPrice of Boeing
median(Boeing$StockPrice)


# Date vs StockPrice for Coca Cola
plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")

# CocaCola's highest price appears to be around 1973
# CocaCola's lowest price appears to be around 1980

# Add the line for Procter & Gamble
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")

# Generates a vertical line at March 1, 2000
abline(v=as.Date(c("2000-03-01")), lwd=2)
# Procter & Gamble's stock price dropped more due to the March 200 tech bubble burst

# Around 1983, Coca Cola's price was going up; Procter & Gamble's was going down
# Coca Cola's price is generally lower than Procter & Gamble's

# Plot the CocaCola stock prices from 1995 through 2005
# Plot the other companies stock prices from 1995 through 2005 with lines()
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col = "blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col = "green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col = "orange")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col = "black")

# GE's stock price fell the most after the 2000 tech bubble burst
# IBM's stock price reaches the highest value in the time period

# Add vertical lines at September 1997 and November 1997
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

# Boeing and Procter & Gamble show a decrease in price between Sept 1997 and Nov 1997

abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-01")), lwd=2)

# Boeing appears to have the biggest increase in price between 2004 and 2005

# Use the tapply command to calculate the mean stock price of IBM, sorted by months
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)

tapply(GE$StockPrice, months(GE$Date), mean)
mean(GE$StockPrice)

tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
mean(CocaCola$StockPrice)

tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
mean(ProcterGamble$StockPrice)

tapply(Boeing$StockPrice, months(Boeing$Date), mean)
mean(Boeing$StockPrice)

# GE and Coca Cola both have their highest average stock price in April
which.max(tapply(GE$StockPrice, months(GE$Date), mean))
which.max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))

# Company stocks are generally higher in January compared to December

