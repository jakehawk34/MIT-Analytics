# Unit 3
# Recitation 3
# Election Forecasting: Predicting the Winner Before any Votes are Cast

polling = read.csv("PollingData.csv", stringsAsFactors = TRUE)
str(polling)
summary(polling)

table(polling$Year)
# The pollsters were so sure about the five missing states in 2012 that they did not conduct any polls in these states

# Simple Approaches to Missing Data
# 1. Delete the missing observations
#   - Throwing away over half of our data
#   - Want to predict for all states
# 2. Delete variables with missing data
#   - Want to retain data from Rasmussen/SurveyUSA
# 3. Fill missing data points with average values
#   - The average value for a poll will be close to 0
#   - If other polls in a state favor a candidate, the missing one probably would too

# Multiple imputation
# Fill in missing values based on the non-missing values
# We will use the Multiple Imputation by Chained Equations, or mice package.

install.packages("mice")
library(mice)

# So we're going to create a new data frame called simple, and that's just going to be our original polling data
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)  
  
set.seed(144)

imputed = complete(mice(simple))
summary(imputed)  

# Copy Rasmussen and SurveyUSA variables back into the original polling data frame
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA

summary(polling)

#Training set consists of data from 2004 and 2008, testing set contains data from 2012
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

# Baseline model will predict Republican every time
table(Train$Republican)

# So a reasonable smart baseline would be to just take one of the polls-- in our case,
# we'll take Rasmussen-- and make a prediction based on who the poll said was winning in the state.

# To compute the new smart baseline, we're going to use the sign function
# sign returns 1 for positive (Republican), -1 for negative (Democrat), and 0 for 0 (Inconclusive/Tie)
table(sign(Train$Rasmussen))

# So in this table, the rows are the true outcome -- 1 is for Republican, 0 is for Democrat --
# and the columns are the smart baseline predictions, -1, 0, 1
table(Train$Republican, sign(Train$Rasmussen))

# So as we can see, this model, with four mistakes and two inconclusive results out of the 100 training
# set observations is doing much, much better than the naive baseline, which simply was always predicting
# the Republican would win and made 47 mistakes on the same data.
str(Train)
cor(Train[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR", "Republican")])

# Build a logistic regression model using PropR
mod1 = glm(Republican ~ PropR, data = Train, family=binomial)
summary(mod1)

# Compute the predictions that the Republican is going to win on the training set
pred1 = predict(mod1, type = "response")
table(Train$Republican, pred1 >= 0.5)
# This model makes 4 mistakes, which is about the same as the smart baseline

# Look for a pair of variables with a relatively low correlation between each other since this might be
# the best chance at improving the model's accuracy

# Try a model with SurveyUSA and DiffCount since they have a relatively low correlation
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = Train, family=binomial)
summary(mod2)

# Compute the predictions that mod2 would make
pred2 = predict(mod2, type = "response")
table(Train$Republican, pred2 >= 0.5)

# Values are different from the recitation script due to newer version of R
# This version of R has different pseudo-random number generation method compared to the version used in the course

# We're going to want to table the testing set outcome variable, Republican, and
# compare that against the actual outcome of the smart baseline, 
# which as you recall would be the sign of the testing set's Rasmussen variables.
table(Test$Republican, sign(Test$Rasmussen))
# Four mistakes and two inconclusive results on the testing set

TestPrediction = predict(mod2, newdata = Test, type = "response")
table(Test$Republican, TestPrediction >= 0.5)

# Look at the one msitake the prediction made
subset(Test, TestPrediction >= 0.5 & Republican == 0)

# Rasmussen gave the Republican a two-point lead, SurveyUSA said both candidates were tied,
# DiffCount predicted the Republican would win 6 more polls, 
# and PropR shows that 2/3 of the polls predicted the Republican to win

# Reality: Barack Obama (D) won Florida in 2012 over Mitt Romney (R)
