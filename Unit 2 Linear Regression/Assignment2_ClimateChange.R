# Assignment 2
# Climate Change

ClimateChange = read.csv("climate_change.csv", stringsAsFactors = TRUE)
str(ClimateChange)

# Training set of data up to and including 2006
train = subset(ClimateChange, Year <= 2006)
# Testing set of data after 2006
test = subset(ClimateChange, Year > 2006)
# Linear regression model to predict Temp 
# using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols as independent variables
TempReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)
summary(TempReg) # Multiple R-squared: 0.7509

# MEI, CO2, CFC.11, CFC.12, TSI, and Aerosols are all significant (p-value < 0.05)

# Current scientific opinion is that nitrous oxide and CFC-11 are greenhouse gases: 
# gases that are able to trap heat from the sun and contribute to the heating of the Earth. 
# However, the regression coefficients of both the N2O and CFC-11 variables are negative, 
# indicating that increasing atmospheric concentrations of either of these two compounds is associated with lower global temperatures.

# Compute the correlations between all the variables in the training set.
cor(train)
# N2O is highly correlated with CO2, CH4, and CFC.12
# CFC.11 is highly correlated with CH4 and CFC.12

# Build a new model to predict Temp with only MEI, TSI, Aerosols and N2O as independent variables
TempReg2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data = train)
summary(TempReg2)
# Coefficient of N2O is 0.02532, which is positive and has greater absolute value compared to the previous model's coefficient
# R-sqaured is 0.7261, slightly less than 0.7509 of the previous model

# Use the step function in R to derive a new model, with the full model as the initial model
TempStep = step(TempReg)
summary(TempStep)
# R-squared of the step model is 0.7508
# The step function removed CH4 from the regression equation of the new model

# Using the model produced from the step function, 
# calculate temperature predictions for the testing data set, using the predict function.
TempPredictions = predict(TempStep, newdata = test)
SSE = sum((TempPredictions - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE / SST
# Testing set R-squared is 0.6286


