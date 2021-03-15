# Assignment 2
# Reading Test Scores

pisaTrain = read.csv("pisa2009train.csv", stringsAsFactors = TRUE)
pisaTest = read.csv("pisa2009test.csv", stringsAsFactors = TRUE)

# Average reading test score of males
tapply(pisaTrain$readingScore, pisaTrain$male, mean)
# Males: 483.5325
# Females: 512.9406

summary(pisaTrain)

# Type the following commands into your R console to remove observations with any missing value from pisaTrain and pisaTest:

pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)
# 2414 observations in training set after na.omit
# 990 observations in testing set after na.omit

# race is an unordered factor, grade is an ordered factor

# To include unordered factors in a linear regression model, we define one level as the "reference level" and add a binary variable for each of the remaining levels. 
# In this way, a factor with n levels is replaced by n-1 binary variables. 
# The reference level is typically selected to be the most frequently occurring level in the dataset.
# Now, consider the variable "raceeth" in our problem, 
# which has levels "American Indian/Alaska Native", "Asian", "Black", "Hispanic", "More than one race", "Native Hawaiian/Other Pacific Islander", and "White". 
# Because it is the most common in our population, we will select White as the reference level.

# Set the reference level of the factor by typing the following two lines in your R console:
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")

pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# Now, build a linear regression model (call it lmScore) using the training set to predict readingScore using all the remaining variables.
# It would be time-consuming to type all the variables, but R provides the shorthand notation "readingScore ~ ." 
# to mean "predict readingScore using all the other variables in the data frame." 
# The period is used to replace listing out all of the independent variables.
lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)
# R-squared of 0.3251

# Training-set root-mean squared error (RMSE) of lmScore
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain))
RMSE # 73.37

# Compare predicted score of grade 11 student A and grade 9 student B
29.542707 * 11 - 29.542707 * 9 # 59.08541

# motherHS, motherWork, fatherHS, fatherWork, selfBornUS, motherBornUS, fatherBornUS, englishAtHome
# minutesPerWeekEnglish, studentsInEnglish, schoolHasLibrary, urban are all not significant

predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)
# range between the maximum and minimum predicted reading score on the test set
max(predTest) - min(predTest)

SSE_test = sum((predTest - pisaTest$readingScore)^2)
SST_test = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
RMSE_test = sqrt(SSE_test / nrow(pisaTest))

# sum of squared errors (SSE) of lmScore on the testing set
SSE_test

# the root-mean squared error (RMSE) of lmScore on the testing set
RMSE_test

# predicted test score used in the baseline model
baseline = mean(pisaTrain$readingScore)

# What is the sum of squared errors of the baseline model on the testing set? 
# HINT: We call the sum of squared errors for the baseline model the total sum of squares (SST)
SST_test

# test-set R-squared value of lmScore
R2_test = 1 - SSE_test / SST_test
R2_test



