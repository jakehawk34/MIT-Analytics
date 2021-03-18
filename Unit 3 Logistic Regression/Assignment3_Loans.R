# Assignment 3
# Predicting Loan Repayment

loans = read.csv("loans.csv")
str(loans)
summary(loans)

table(loans$not.fully.paid)
mean(loans$not.fully.paid)

missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
str(missing)
nrow(missing)
mean(missing$not.fully.paid)

# Note that to do this imputation, we set vars.for.imputation to all variables in the data frame except for not.fully.paid, 
# to impute the values using all of the other independent variables.
loans_imputed = read.csv("loans_imputed.csv")

# Split the data into train and test
set.seed(144)
split = sample.split(loans_imputed$not.fully.paid, SplitRatio = 0.7)
train = subset(loans_imputed, split == TRUE)
test = subset(loans_imputed, split == FALSE)

# Create a logistic regression model for the training set using all independent variables
mod1 = glm(not.fully.paid ~ ., data = train, family=binomial)
summary(mod1)

# Consider two loan applications, which are identical other than the fact that the borrower in Application A has FICO credit score 700 
# while the borrower in Application B has FICO credit score 710.
# log(oddsA) - log(oddsB)
logOddsA = -9.317 * 10^-3 * (700)
logOddsB = -9.317 * 10^-3 * (710)
logOddsA - logOddsB
# oddsA are the odds that Application A is not fully paid back and oddsB are the odds that Application B is not fully paid back
# oddsA / oddsB
oddsA = exp(logOddsA)
oddsB = exp(logOddsB)
oddsA / oddsB

# Predict the probability of the test set loans not being paid back in full
# Store these predicted probabilities in a variable named predicted.risk and add it to your test set
predicted.risk = predict(mod1, newdata = test, type = "response")
table(test$not.fully.paid, predicted.risk > 0.5)
test$predicted.risk = predicted.risk

# Accuracy of the logistic regression model
(2400 + 3) / (2400 + 13 + 457 + 3)
# Accuracy of the baseline model
table(test$not.fully.paid)
2413 / (2413 + 460)

# Use the ROCR package to compute the test set AUC.
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
ROCRperf = as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf

# Using the training set, build a bivariate logistic regression model (aka a logistic regression model with a single independent variable) 
# that predicts the dependent variable not.fully.paid using only the variable int.rate.
mod2 = glm(not.fully.paid ~ int.rate, data = train, family=binomial)
summary(mod2)

# Make test set predictions with the bivariate model, mod2
predicted.risk2 = predict(mod2, newdata = test, type = "response")
max(predicted.risk2)
table(test$not.fully.paid, predicted.risk2 > 0.5)
2413 / (2413 + 460)

# Test set AUC of the bivariate model
ROCRpred2 = prediction(predicted.risk2, test$not.fully.paid)
ROCRperf2 = as.numeric(performance(ROCRpred2, "auc")@y.values)
ROCRperf2

# How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, 
# using continuous compounding of interest?
10 * exp(0.06 * 3)

# Profit if the investor is paid back in full
# c * exp(r * t) - c

# In the previous subproblem, we concluded that an investor who invested c dollars in a loan with interest rate r for t years 
# makes c * (exp(rt) - 1) dollars of profit if the loan is paid back in full 
# and -c dollars of profit if the loan is not paid back in full (pessimistically).
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit) * 10

# We will analyze an investment strategy in which the investor only purchases loans with a high interest rate (a rate of at least 15%), 
# but amongst these loans selects the ones with the lowest predicted risk of not being paid back in full. 
# We will model an investor who invests $1 in each of the most promising 100 loans
highInterest = subset(test, int.rate >= 0.15)
# Average profit of a $1 investment in highInterest loans
mean(highInterest$profit)
# Proportion of loans in highInterest not paid back in full
mean(highInterest$not.fully.paid)

# Next, we will determine the 100th smallest predicted probability of not paying in full by sorting the predicted risks in increasing order 
# and selecting the 100th element of this sorted list. 
# Find the highest predicted risk that we will include by typing the following command into your R console:
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

# Use the subset() function to build a data frame called selectedLoans consisting of the high-interest loans with predicted risk not exceeding the cutoff we just computed. 
# Check to make sure you have selected 100 loans for investment.
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
# What is the profit of the investor, who invested $1 in each of these 100 loans?
sum(selectedLoans$profit)
# How many of the selectedLoans loans were not fully paid back?
table(selectedLoans$not.fully.paid)

# We have now seen how analytics can be used to select a subset of the high-interest loans that were paid back at only a slightly lower rate than average, 
# resulting in a significant increase in the profit from our investor's $100 investment. 
# Although the logistic regression models developed in this problem did not have large AUC values, 
# we see that they still provided the edge needed to improve the profitability of an investment portfolio.



