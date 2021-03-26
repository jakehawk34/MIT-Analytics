# Assignment 4
# Letter Recognition

# One of the earliest applications of the predictive analytics methods we have studied so far in this class was to automatically recognize letters, 
# which post office machines use to sort mail. 
# In this problem, we will build a model that uses statistics of images of four letters in the Roman alphabet -- A, B, P, and R -- 
# to predict which letter a particular image corresponds to.

letters = read.csv("letters_ABPR.csv", stringsAsFactors = TRUE)
str(letters)
letters$isB = as.factor(letters$letter == "B")
set.seed(1000)

spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

# Before building models, let's consider a baseline method that always predicts the most frequent outcome, 
# which is "not B". What is the accuracy of this baseline method on the test set?
table(letters$isB)
2350 / nrow(letters)

# Now build a classification tree to predict whether a letter is a B or not, using the training set to build your model. 
# Remember to remove the variable "letter" out of the model.
CARTb = rpart(isB ~ . - letter, data=train, method="class")
predict.b = predict(CARTb, newdata=test, type="class")
table(test$isB, predict.b)
# Accuracy
(1118 + 340) / nrow(test)

# Now, build a random forest model to predict whether the letter is a B or not (the isB variable) using the training set. 
# You should use all of the other variables as independent variables, except letter
set.seed(1000)
library(randomForest)
LettersForest = randomForest(isB ~ . - letter, data=train)
PredictForest = predict(LettersForest, newdata=test)
# Accuracy
table(PredictForest, test$isB)
(1163 + 374) / nrow(test)

# Let us now move on to the problem that we were originally interested in, 
# which is to predict whether or not a letter is one of the four letters A, B, P or R.
letters$letter = as.factor(letters$letter)
set.seed(2000)
spl2 = sample.split(letters, SplitRatio = 0.5)
train2 = subset(letters, spl == TRUE)
test2 = subset(letters, spl == FALSE)

# In a multiclass classification problem, a simple baseline model is to predict the most frequent class of all of the options.
# What is the baseline accuracy on the testing set?
table(train2$letter)
table(test2$letter)
396 / nrow(test)

# Now build a classification tree to predict "letter", using the training set to build your model.
# What is the test set accuracy of your CART model? Use the argument type="class" when making predictions.
letterTree = rpart(letter ~. - isB, data = train2, method="class" )
treePredict = predict(letterTree, newdata = test2, type="class")
table(test2$letter, treePredict)
# Accuracy
(338 + 294 + 357 + 365) / nrow(test2)

# Now build a random forest model on the training data, 
# using the same independent variables as in the previous problem -- again, don't forget to remove the isB variable.
set.seed(1000)
letterForest = randomForest(letter ~. - isB, data = train2)
forestPredict = predict(letterForest, newdata = test2)
table(test2$letter, forestPredict)
# Accuracy
(381 + 372 + 391 + 385) / nrow(test2)

# CONCLUSIONS
# You should find this value rather striking, for several reasons. 
# The first is that it is significantly higher than the value for CART, highlighting the gain in accuracy that is possible from using random forest models. 
# The second is that while the accuracy of CART decreased significantly as we transitioned from the problem of predicting B/not B (a relatively simple problem) 
# to the problem of predicting the four letters (certainly a harder problem), the accuracy of the random forest model decreased by a tiny amount.

