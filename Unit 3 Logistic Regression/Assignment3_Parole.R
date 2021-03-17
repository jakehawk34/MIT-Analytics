# Assignment 3
# Predicting Parole Violators

parole = read.csv("parole.csv")
nrow(parole)
str(parole)
summary(parole)

# How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)

# Which variables in this dataset are unordered factors with at least three levels?
# male, race, state, crime and violator are unordered factors; state and crime have at least three levels

# Convert state and crime to factors
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)

# Split the data
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Train a logistic regression model on the training set
mod1 = glm(violator ~ ., data = train, family=binomial)
summary(mod1)
# race, state4, and multiple.offenses are significant variables

# Consider a parolee who is male, of white race, aged 50 years at prison release, 
# from the state of Maryland, served 3 months, had a maximum sentence of 12 months, 
# did not commit multiple offenses, and committed a larceny

# log(odds) or Logit
-4.2411574 + 0.3869904 * (1) + 0.8867192 * (1) + -0.0001756 * (50) + -0.1238867 * (3) + 0.0802954 * (12) + 0.6837143* (1)
# odds = e ^ log(odds)
exp(-1.700629)
# probability = 1 / (1 + e ^ -log(odds))
1 / (1 + exp(1.700629))

# Obtain the model's predicted probabilities for parolees in the testing set
pred1 = predict(mod1, newdata = test, type = "response")
max(pred1)

# Evaluate the model's predictions on the test set using a threshold of 0.5.
table(test$violator, pred1 >= 0.5)
# Sensitivity
12 / (11 + 12)
# Specificity
167 / (167 + 12)
# Accuracy
(167 + 12) / (167 + 12 + 11 + 12)

# Accuracy of a simple model that predicts that every parolee is a non-violator (baseline)
table(test$violator)
179 / (179 + 23)

# The parole board would experience more regret for releasing a prisoner who then violates parole (a negative prediction that is actually positive, or false negative) 
# than it would experience for denying parole to a prisoner who would not have violated parole (a positive prediction that is actually negative, or false positive).
# Decreasing the cutoff leads to more positive predictions, which increases false positives and decreases false negatives.

# The model is likely of value to the board, 
# and using a different logistic regression cutoff is likely to improve the model's value.
# Our model has 11 false negatives compared to 23 false negatives in the baseline model
# False negatives are more costly, so the board will value the model
# Decreasing the logistic regression cutoff threshold will decrease the number of false negatives

# Using the ROCR package, what is the AUC value for the model?
ROCRpred = prediction(pred1, test$violator)
ROCRperf = as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf

# Describe the meaning of AUC in this context
# The probability the model can correctly differentiate 
# between a randomly selected parole violator and a randomly selected parole non-violator.


