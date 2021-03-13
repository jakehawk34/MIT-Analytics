# Unit 2: Linear Regression
# The Statistical Sommelier

wine = read.csv("wine.csv", stringsAsFactors = TRUE)
str(wine)
summary(wine)

# Build a linear regression model using AGST to predict Price
model1 = lm(Price ~ AGST, data = wine)
summary(model1)

# Compute the sum of squared errors (SSE) for the model
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# Add another variable to the regression model, HarvestRain
model2 = lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
# R-squared is greater for model2 (0.7074) compared to model1 (0.435)

SSE = sum(model2$residuals^2)
SSE
# The SSE for model2 (2.97) is better than the SSE for model1 (5.73)

# Add WinterRain, Age, FrancePop to the regression model
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
# R-squared is greater for model3 (0.8294) compared to model2 (0.7074)

SSE = sum(model3$residuals^2)
SSE
# The SSE for model3 (1.73) is better than the SSE for model2 (2.97)

# MLR model with HarvestRain and WinterRain to predict Price
model = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model)

# The Estimate column gives the coefficients for the independent variables and the intercept.
# The Standard Error column gives a measure of how much the coefficient is likely to vary
# from the estimate value.
# The t value is the estimate divided by the standard error.
# The last column of numbers gives a measure of how plausible it is that the coefficient is actually 0, given
# the data we used to build the model.
# We want independent variables with small values in the last column.
summary(model3)
# Use the codes under the regression table to determine whether an independent variable is significant
# Age and FrancePop are insignificant variables

# Remove FrancePop from the model
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4)
# Age is now very significant in this model
# Multicollinearity of Age and FrancePop

# Compute the correlation between WinterRain and Price
cor(wine$WinterRain, wine$Price)

# Compute the correlation between Age and FrancePop
cor(wine$Age, wine$FrancePop)

# Compute the correlation between all variables in data set
cor(wine)

# Remove Age and FrancePop from the model with all variables as independent variables
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(model5)

# Stick with model4 because it removes the issue of multicollinearity between Age and FrancePop
# but does not remove both variables like model5, which reduces the R-squared
# and the number of significant independent variables

wineTest = read.csv("wine_test.csv")
str(wineTest)

# Make predictions for these two test points
predictTest = predict(model4, newdata = wineTest)
predictTest

# Compute the R-squared value for the test set
SSE = sum((wineTest$Price - predictTest) ^2)
SST = sum((wineTest$Price - mean(wine$Price)) ^2)
1 - SSE / SST # R-squared

# Conclusion: A linear regression model is a simple, yet powerful model that can effectively 
# predict wine prices and often outperform the opinions of wine experts.



