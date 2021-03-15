# Assignment 2
# Forecasting Elantra Sales

elantra = read.csv("elantra.csv")
train = subset(elantra, elantra$Year <= 2012)
test = subset(elantra, elantra$Year > 2012)


# Build a linear regression model to predict monthly Elantra sales 
# using Unemployment, CPI_all, CPI_energy and Queries as the independent variables
SalesReg = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = train)
summary(SalesReg)
# R-squared = 0.4282

# To incorporate the seasonal effect due to the month, build a new linear regression model that predicts monthly Elantra sales 
# using Month as well as Unemployment, CPI_all, CPI_energy and Queries.
SalesReg2 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = train)
summary(SalesReg2)
# R-squared = 0.4344

# In the new model, given two monthly periods that are otherwise identical in Unemployment, CPI_all, CPI_energy and Queries, 
# what is the absolute difference in predicted Elantra sales given that one period is in January and one is in March?
110.69 * 2

# In the new model, given two monthly periods that are otherwise identical in Unemployment, CPI_all, CPI_energy and Queries, 
# what is the absolute difference in predicted Elantra sales given that one period is in January and one is in May?
110.69 * 4

# Re-run the regression with the Month variable modeled as a factor variable.
train$MonthFactor = as.factor(train$Month)
test$MonthFactor = as.factor(test$Month)
SalesReg3 = lm(ElantraSales ~ MonthFactor + Unemployment + CPI_all + CPI_energy + Queries, data = train)
summary(SalesReg3)

# MonthFactor, Unemployment, CPI_all, and CPI_energy are significant variables

# Which of the following variables is CPI_energy highly correlated with?
cor(train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

# Which of the following variables is Queries highly correlated with? 
cor(train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

# Let us now simplify our model. We will do this by iteratively removing variables, one at a time.
SalesReg4 = lm(ElantraSales ~ MonthFactor + Unemployment + CPI_all + CPI_energy, data = train)
summary(SalesReg4)

# Using the model from Problem 6.1, make predictions on the test set. 
# What is the sum of squared errors of the model on the test set?
PredTest1 = predict(SalesReg4, newdata = test)
SSE = sum((PredTest1 - test$ElantraSales)^2)
SSE

# What would the baseline method predict for all observations in the test set?
mean(train$ElantraSales)

# What is the test set R-Squared?
SST = sum((mean(train$ElantraSales) - test$ElantraSales)^2)
R2 = 1 - SSE / SST
R2

# What is the largest absolute error that we make in our test set predictions?
max(abs(PredTest1 - test$ElantraSales))

# In which period (Month,Year pair) do we make the largest absolute error in our prediction?
which.max(abs(PredTest1 - test$ElantraSales))
c(test$Month[5], test$Year[5])


