# Unit 4: Trees
# The Supreme Court

# In 2002, Andrew Martin, a professor of political science at Washington University in 
# St. Louis, decided to instead predict decisions using a statistical model built from data.

# Martin used a method called classification and regression trees, or CART.

# He could have used logistic regression for this, but logistic regression models are not easily interpretable.
# The model coefficients in logistic regression indicate the importance and relative effect of variables,
# but do not give a simple explanation of how a decision is made.

# We'll discuss the method of CART and a related method called random forests

# In this lecture, we'll focus on predicting Justice Stevens' decisions.
# He is generally considered a justice who started out moderate, but became
# more liberal during his time on the Supreme Court-- although, he's a self-proclaimed conservative.
# In this problem, our dependent variable is whether or not Justice Stevens voted to reverse the lower court decision.
# This is a binary variable taking value 1 if Justice Stevens decided to reverse or overturn the lower court decision,
# and taking value 0 if Justice Stevens voted to affirm or maintain the lower court decision.

# Independent variables are six different properties
# Circuit court of origin
# Issue area of case
# Type of petitioner
# Type of respondent
# Ideological direction of lower court decision
# Whether petitioner argued that a law/practice was unconstitutional

# With a logistic regression model, it's difficult to understand which factors
# are more important due to things like the scalesvof the variables, and the possibility of multicollinearity.

# To predict the outcome for a new observation or case, you can follow the splits in the tree and at the end,
# you predict the most frequent outcome in the training set that followed the same path.
# Some advantages of CART are that it does not assume a linear model, like logistic regression
# or linear regression, and it's a very interpretable model.

# There are different ways to control how many splits are generated.
# One way is by setting a lower bound for the number of data points in each subset.
# In R, this is called the minbucket parameter, for the minimum number of observations in each bucket or subset.
# Overfitting occurs when the minbucket parameter is too small, so the CART fits the model almost too perfectly.
# Consequently, the model will probably not perform well on the test set or new data.
# Underfitting occurs when the minbucket is too large, causing the CART to be to simple and have poor accuracy.

# Instead of just taking the majority outcome to be the prediction, we can compute the percentage
# of data in a subset of each type of outcome.

# As an example, if we have a subset with 10 affirms and two reverses, then 87% of the data is affirm.
# Then, just like in logistic regression, we can use a threshold value to obtain our prediction.
# For this example, we would predict affirm with a threshold of 0.5 since the majority is affirm.
# But if we increase that threshold to 0.9, we would predict reverse for this example.

stevens = read.csv("stevens.csv", stringsAsFactors = TRUE)
summary(stevens)
str(stevens)

library(caTools)
set.seed(3000)

spl = sample.split(stevens$Reverse, SplitRatio = 0.7)

Train = subset(stevens, spl == TRUE)
Test = subset(stevens, spl == FALSE)

# In order to use the CART function, load and install the rpart and rpart.plot packages
install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

# Create the CART model using the rpart function
# First argument is the dependent variable followed by a tilde.
# Then, the independent variables separated by plus signs
# Give the data set to use
# Need to give to additional arguments
# method = "class" tells the model to build a classification tree, instead of a regression tree
# minbucket = 25 limits the tree so it doesn't overfit the training model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)

# Plot our tree using the prp function
prp(StevensTree)
# The first split of the tree is whether or not the lower court decision is liberal.
# If it is, then we move down the left branch.
# If the respondent is a criminal defendant, injured person, politician, state, or the United States, we predict 0, or affirm.
# If the respondent is not one of these types, we move to the next split in the tree.
# If the petitioner is a city, employee, employer, government official or politician, we predict 0, or affirm.
# If not, then we go to the next split to check the circuit court of origin.
# If the circuit court of origin is the 10th, 1st, 3rd, 4th, DC, or Federal, then we predict 0.
# If otherwise, then we predict 1 or reverse.
# This same process can be carried out on the other side of the tree for when a lower court decision leans conservative.

# Check how well our CART model makes predictions for the test set.
# The type="class" argument ensures we get the majority class predictions from our model
PredictCART = predict(StevensTree, newdata = Test, type = "class")

# Compute the accuracy of the model using a confusion matrix
table(Test$Reverse, PredictCART)
# Accuracy
(41 + 71) / (41 + 36 + 22 + 71)

# If you were to build a logistic regression model, you would get an accuracy of 0.665
# and a baseline model that always predicts Reverse, the most common outcome, has an accuracy of 0.547.

# Generate an ROCR curve
library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
PredictROC

# We'll use the second column as our probabilities to generate an ROC curve.
pred = prediction(PredictROC[,2], Test$Reverse)

# Now we need to use the performance function, where the first argument is the outcome of the prediction
# function, and then the next two arguments are true positive rate and false positive rate, what we want on the x and y-axes of our ROC curve.
perf = performance(pred, "tpr", "fpr")
plot(perf)

# Quick Question 4
as.numeric(performance(pred, "auc")@y.values)

StevensTree5 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=5)
prp(StevensTree5)
StevensTree100 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=100)
prp(StevensTree100)

# Random Forests

# This method was designed to improve the prediction accuracy of CART and works by building a large number of CART trees.
# Unfortunately, this makes the method less interpretable than CART, so often you need to decide if you value the interpretability or the increase in accuracy more.

# To prevent the same tree from being created every time, random forests only allows each tree to split on a random subset
# of the available independent variables, and each tree is built from what we call a bagged or bootstrapped sample of the data.
# This just means that the data used as the training data for each tree is selected randomly with replacement.

# Random Forest Parameters

# The first is the minimum number of observations in a subset, or the minbucket parameter from CART.
# When we create a random forest in R, this will be called nodesize.

# The second parameter is the number of trees to build, which is called ntree in R. This should not
# be set too small, but the larger it is the longer it will take. A couple hundred trees is typically plenty.

install.packages("randomForest")
library(randomForest)

# Build the random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize=25, ntree=200)
set.seed(3000)
# You should see an interesting warning message.
# The randomForest function does not have a method argument. So when we want to do a classification problem,
# we need to make sure outcome is a factor. Let's convert the variable Reverse to a factor variable

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize=25, ntree=200)
set.seed(3000)
# No warning message now, so the model is ready to make predictions

PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
# Accuracy
(42 + 74) / (42 + 35 + 19 + 74)

# Quick Question 5
set.seed(100)
Forest1 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize=25, ntree=200)
Predict1 = predict(Forest1, newdata = Test)
table(Test$Reverse, Predict1)
(41 + 74) / (41 + 36 + 19 + 74)

set.seed(200)
Forest2 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize=25, ntree=200)
Predict2 = predict(Forest2, newdata = Test)
table(Test$Reverse, Predict2)
(43 + 75) / (43 + 34 + 18 + 75)

# Optimal Trees

# Optimal trees is a new method that maintains interpretability, but delivers state-of-the-art performance
# CART belongs to a class of models often described as interpretable, white box, transparent,
# in that they provide insights in understanding the logic for decision making.
# In contrast, the random forest model, belonging to a class called black box models, also makes predictions, but the inner workings
# of how input translates to predictions is overly complex or unclear.
# CART only learns the splits one step at a time, leading to less-than-optimal trees
# Modern optimization techniques allow the entire tree to be trained in one step.
# Trees can be trained with parallel splits (one variable) or hyperplane splits (multiple variables)

# Cross-Validation
# In CART, the value of minbucket can affect the model's out-of-sample accuracy
# We'll use a method called K-fold Cross Validation to properly select the parameter value.
# First, split the training set into k equally-sized subsets, or folds.
# Then, estimate the model with k-1 folds. Use the model to compute predictions on the remaining fold.
# This fold is called the validation set.
# Repeat this process for every combination of k-1 folds and validation set. 
# Example, k=5, first model uses folds 1,2,3,4 and validation set is fold 5
# Second model uses folds 1,2,3,5 and validation set is fold 5, etc.
# So ultimately, cross validation builds many models, one for each fold and possible parameter value.

# When we use cross-validation in R, we use the complexity paramater, or cp.
# cp is similar to the Adjusted R-squared in linear regression and AIC in logistic regression,
# in that it measures the trade-off between model complexity and accuracy on the training set.

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

# Define how many folds we want
set.seed(3000)
numFolds = trainControl(method="cv", number=10)

# Pick the possible values for the cv parameter
# Define our parameters to test as values from 0.01 to 0.5 in increments of 0.01
cpGrid = expand.grid(.cp=seq(0.01, 0.5, 0.01))
# Perform cross-validation using the train function, and the method, trControl, and tuneGrid arguments filled in
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="rpart", trControl=numFolds, tuneGrid=cpGrid)

# Use the final value for the CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="class", cp=0.17)

# Make predictions on the test set using this model
PredictCV = predict(StevensTreeCV, newdata=Test, type="class")

# Create confusion matrix
table(Test$Reverse, PredictCV)
(59 + 64) / (59 + 18 + 29 + 64)
# Cross validation helps us make sure we're selecting a good parameter value, and often this will significantly increase the accuracy.

# Quick Question 6
prp(StevensTreeCV)
# The tree with the best accuracy only has one split. 

# The Model v. The Experts
# Martin's model consisted of two stages of CART trees.

# The first stage involved making predictions using two CART trees.
# One to predict a unanimous liberal decision and one to predict a unanimous conservative decision.
# If the trees gave conflicting responses or both predicted no, then they moved on to the next stage.
# It turns out that about 50% of Supreme Court cases result in a unanimous decision, so this was a nice first step to detect the easier cases.

# The second stage consisted of predicting the decision of each individual justice,
# and then use the majority decision of all nine justices as a final prediction for the case.

# For predicting the overall decision that was made by the Supreme Court, the models
# had an accuracy of 75%, while the experts only had an accuracy of 59%.
# So the models had a significant edge over the experts in predicting the overall case outcomes.

# For predicting the decision of individual justices, 
# the model and the experts had similar level of performance.






