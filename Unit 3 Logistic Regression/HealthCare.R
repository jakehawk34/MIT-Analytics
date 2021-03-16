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
# 70 cases with actual good care and predicted good care (true positive)
# 10 cases with actual poor care and predicted poor care (true negative)
# 4 cases with actual good care, but predicted poor care (false positive)
# 15 cases with actual poor care, but predicted good care (false negative)

# Sensitivity or true positive rate
10 / (10 + 15)
# Specificity or true negative rate
70 / (70 + 4)

# Threshold value of 0.5
table(qualityTrain$PoorCare, predictTrain > 0.7)
# Sensitivity goes down
8 / (8 + 17)
# Specificity goes up
73 / (73 + 1)

# Threshold value of 0.3
table(qualityTrain$PoorCare, predictTrain > 0.3)
# Sensitivity goes up
13 / (13 + 12)
# Specificity goes down
67 / (67 + 7)

# Which threshold value should we choose?
# A Receiver Operator Characteristic curve, or ROC curve, can help you decide which value of the threshold is best.
# Sensitivity of true positive rate on the y-axis
# False positive or 1 - specificity rate on the x-axis
# ROC curve always starts at (0, 0), which corresponds to a threshold value of 1
# ROC curve always ends at (1, 1), which corresponds to a threshold value of 0

# To generate ROC curves in R, we need to install a new package.
install.packages("ROCR")
library(ROCR)

# Use the predictions from predictTrain to create the ROC curve
# The prediction function takes two arguments
# The first argument is the predictions we made with our model
# The second argument is the true outcomes of the data points
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
# We'll call the output of this ROCRperf, and use the performance function, 
# which takes as arguments the output of the prediction function, and then what we want on the x and y-axes.
# In this case, it's true positive rate, or "tpr", and false positive rate, or "fpr".
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
# Add colors to the ROCR plot
plot(ROCRperf, colorize = TRUE)
# Add threshold labels to the plot
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# So the Area Under the Curve shows an absolute measure

# N = number of observations
# Overall accuracy = (True Positives (TP) + True Negatives (TN)) / N
# Overall error rate = (False Positives (FP) + False Negatives (FN)) / N
# False negative error rate = FN / (FN + TP)
# False positive error rate = FP / (FP + TN)

# Quick Question 5
Question5 = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family=binomial)
predictTest = predict(Question5, type="response", newdata=qualityTest)
# You can compute the test set AUC by running the following two commands in R:
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

# Altogether, there has been 2,400 studies written using the Framingham data.

