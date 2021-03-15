# Assignment 2
# Detecting Flu Epidemics via Search Engine Query Data

FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
summary(FluTrain)
# Week that corresponds to the highest percentage of ILI-related physician visits
max(FluTrain$ILI)
which.max(FluTrain$ILI)
# Week corresponds to the highest percentage of ILI-related query fraction
max(FluTrain$Queries)
which.max(FluTrain$Queries)

FluTrain$Week[303]

# Plot the histogram of the dependent variable, ILI
hist(FluTrain$ILI)

# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?
plot(FluTrain$Queries, log(FluTrain$ILI))
# There is a positive, linear relationship between log(ILI) and Queries.

# What is the training set R-squared value for FluTrend1 model (the "Multiple R-squared")
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

# What is the relationship we infer from our problem?
cor(log(FluTrain$ILI), FluTrain$Queries)^2
# R-squared = 0.709
# Correlation = 0.842 -> Correlation^2 = 0.709

FluTest = read.csv("FluTest.csv")

# Normally, we would obtain test-set predictions from the model FluTrend1 using the code:

# PredTest1 = predict(FluTrend1, newdata=FluTest)

# However, the dependent variable in our model is log(ILI), so PredTest1 would contain predictions of the log(ILI) value. 
# We are instead interested in obtaining predictions of the ILI value. 
# We can convert from predictions of log(ILI) to predictions of ILI via exponentiation, or the exp() function. 
# The new code, which predicts the ILI value, is:

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

# Estimate for the percentage of ILI-related physician visits for the week of March 11, 2012
which(FluTest$Week == "2012-03-11 - 2012-03-17")
estimate = PredTest1[11]

# Relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012
observed = FluTest$ILI[11]

(observed - estimate) / observed

# What is the Root Mean Square Error (RMSE) between our estimates 
# and the actual observations for the percentage of ILI-related physician visits, on the test set
SSE = sum((FluTest$ILI - PredTest1)^2)
RMSE = sqrt(SSE / nrow(FluTest)) # 0.74906
sqrt(mean((PredTest1-FluTest$ILI)^2))

install.packages("zoo")
library(zoo)

# After installing and loading the zoo package, run the following commands to create the ILILag2 variable in the training set:
  
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)

# Plot the log of ILILag2 against the log of ILI
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

# Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable 
# as well as the log of the ILILag2 variable
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)
# R-squared = 0.9063

# FluTrend2 is a stronger model than FluTrend1 on the basis of coefficients and R-squared

# Modify the code from the previous subproblem to add an ILILag2 variable to the FluTest data frame.
ILILag2Test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2Test)
summary(FluTest)

# Fill in the missing values for ILILag2 in FluTest
FluTrain$ILI[416] # Second-to-last observation in 2011
FluTrain$ILI[417] # Last observation in 2011

FluTest$ILILag2[1] # First observation in 2012 (NA)
FluTest$ILILag2[2] # Second observation in 2012 (NA)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

FluTest$ILILag2[1] # First observation in 2012 replaced with ILI from second-to-last of 2011
FluTest$ILILag2[2] # Second observation in 2012 replaced with ILI from last of 2011

# Obtain test set predictions of the ILI variable from the FluTrend2 model
PredTest2 = exp(predict(FluTrend2, newdata = FluTest))
SSE_2 = sum((PredTest2 - FluTest$ILI)^2)
RMSE_2 = sqrt(SSE_2 / nrow(FluTest)) # 0.2942029
sqrt(mean((PredTest2 - FluTest$ILI)^2))

# Which model obtained the best test-set RMSE?
RMSE_2 < RMSE
# RMSE_2 is less than RMSE, so FluTrend2 produced less error than FluTrend1
