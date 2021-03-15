# Unit 3: Logistic Regression
# Modeling the Expert: An Introduction to Logistic Regression

quality = read.csv("quality.csv", stringsAsFactors = TRUE)
str(quality)

table(quality$PoorCare)
# 98 patients received good care, 33 patients received poor care

# Our baseline model has an accuracy of about 75%
98/ 131

install.packages("caTools")
library(caTools)

# Use the caTools package to split the data set randomly into a training and test set
# Initialize the random number generator to 88 with set.seed()
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
# TRUE means that we should put an observation in the training set
# FALSE means that we should put an observation in the testing set
split

qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

# Build a logistic regression model using OfficeVisits and Narcotics as independent variables.
# We'll call our model QualityLog and use the "glm" function for "generalized linear model" to build
# our logistic regression model.
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family=binomial)
# family=binomial tells the glm function to build a logistic regression model
summary(QualityLog)

# AIC value. This is a measure of the quality of the model and is like Adjusted R-squared in that it accounts
# for the number of variables used compared. 
# The preferred model is the one with the minimum AIC.

# Make predictions on the training set using predict()
predictTrain = predict(QualityLog, type="response")
summary(predictTrain)

# Let's see if we're predicting higher probabilities for the actual poor care cases as we expect.
tapply(predictTrain, qualityTrain$PoorCare, mean)

# Quick Question 3
Question3 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family=binomial)
summary(Question3)

# We can convert the probabilities to predictions using what's called a threshold value, t.
# If the probability of poor care is greater than this threshold value, t, we predict poor quality care.
# But if the probability of poor care is less than the threshold value, t, then we predict good quality care.

# Confusion matrix / classification matrix
# The rows are labeled with the actual outcome, and the columns are labeled with the predicted outcome.
# Each entry of the table gives the number of data observations that fall into that category.

# So the number of true negatives, or TN, is the number of observations that are actually good care and for which we predict good care.
# The true positives, or TP, is the number of observations that are actually poor care and for which we predict poor care.
# These are the two types that we get correct.

# The false positives, or FP, are the number of data points for which we predict poor care, but they're actually good care.
# And the false negatives, or FN, are the number of data points for which we predict good care, but they're actually poor care.

# We can compute two outcome measures that help us determine what types of errors we are making.
# They're called sensitivity and specificity.

# Sensitivity is equal to the true positives divided by the true positives plus the false negatives,
# and measures the percentage of actual poor care cases that we classify correctly.
# This is often called the true positive rate.
# Specificity is equal to the true negatives divided by the true negatives plus the false positives,
# and measures the percentage of actual good care cases that we classify correctly.
# This is often called the true negative rate.

# A model with a higher threshold will have a lower sensitivity and a higher specificity
# A model with a lower threshold will have a higher sensitivity and a lower specificity

# Compute classification tables using different threshold values
# Threshold value of 0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)
# 70 cases with actual good care and predicted good care
# 10 cases with actual poor care and predicted poor care
# 4 cases with actual good care, but predicted poor care
# 15 cases with actual poor care, but predicted good care




