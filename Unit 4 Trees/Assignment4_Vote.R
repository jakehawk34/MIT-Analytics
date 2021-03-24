# Assignment 4
# Understanding Why People Vote

gerber = read.csv("gerber.csv")
mean(gerber$voting)

# Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$neighbors, mean)
tapply(gerber$voting, gerber$self, mean)

# Build a logistic regression model for voting using the four treatment group variables as the independent variables
vote.log = glm(voting ~ hawthorne + civicduty + neighbors + self, data = gerber, family="binomial")
summary(vote.log)

# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
log.predict = predict(vote.log, type="response") 
table(gerber$voting, log.predict > 0.3)
# Accuracy
(134513 + 51966) / nrow(gerber)

# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
log.predict = predict(vote.log, type="response") 
table(gerber$voting, log.predict > 0.5)
# Accuracy
235388 / nrow(gerber)

# Compare your previous two answers to the percentage of people who did not vote (the baseline accuracy) and compute the AUC of the model.
1 - mean(gerber$voting)
library(ROCR)
ROCRpred = prediction(log.predict, gerber$voting)
ROCRperf = as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf

# Build a CART tree for voting using all data and the same four treatment variables we used before.
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

# Force the complete tree to be built
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
# We saw in Problem 1 that the highest fraction of voters was in the Neighbors group, followed by the Self group, followed by the Hawthorne group, and lastly the Civic Duty group. 
# And we see here that the tree detects this trend.

# Make a new tree that includes the "sex" variable, again with cp = 0.0. Notice that sex appears as a split that is of secondary importance to the treatment group.
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

# In the "control" only tree, what is the absolute value of the difference in the predicted probability of voting between being in the control group versus being in a different group
Control = rpart(voting ~ control, data=gerber, cp=0.0)
Control.sex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(Control, digits = 6)
0.34 - 0.296638

# Now, using the second tree (with control and sex), determine who is affected more by NOT being in the control group (being in any of the four treatment groups)
prp(Control.sex, digits = 6)
0.345818 - 0.302795 # Men
0.334176 - 0.290456 # Women

control.sex.log = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(control.sex.log)

# The regression tree calculated the percentage voting exactly for every one of the four possibilities (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control). 
# However, logistic regression on the "sex" and "control" variables considers these variables separately, not jointly, and therefore did not do as well.
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(control.sex.log, newdata=Possibilities, type="response")
0.2908065 - 0.290456

# We're going to add a new term to our logistic regression now, that is the combination of the "sex" and "control" variables - 
# so if this new variable is 1, that means the person is a woman AND in the control group. 
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")
0.290456 - 0.2904558

# This example has shown that trees can capture nonlinear relationships that logistic regression can not, 
# but that we can get around this sometimes by using variables that are the combination of two variables.

# We should not use all possible interaction terms in a logistic regression model due to overfitting. Even in this simple problem, we have four treatment groups and two values for sex. 
# If we have an interaction term for every treatment variable with sex, we will double the number of variables. In smaller data sets, this could quickly lead to overfitting.
