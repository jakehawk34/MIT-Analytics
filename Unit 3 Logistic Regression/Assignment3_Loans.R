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




