# Assignment 6
# Predicting Stock Returns with Cluster-Then-Predict

stocks = read.csv("StocksCluster.csv")
summary(stocks)

mean(stocks$PositiveDec)

# Maximum correlation between any two variables in the dataset
cor(stocks)
# 0.19167279 between ReturnOct and ReturnNov


# Which month (from January through November) has the largest mean return across all observations in the dataset
# Which month has the smallest mean return
summary(stocks)

# April has the largest at 0.026308
# September has the smallest at -0.014721

# Initial Logistic Regression Model
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family="binomial")
PredictTrain = predict(StocksModel, type="response")
table(stocksTrain$PositiveDec, PredictTrain > 0.5)

# Accuracy on the training set
(990 + 3640) / nrow(stocksTrain)

# Accuracy of the model on the testing set
PredictTest = predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, PredictTest > 0.5)
(417 + 1553) / nrow(stocksTest)

# Accuracy on the test set of a baseline model that always predicts PostiveDec = 1
table(stocksTest$PositiveDec)
mean(stocksTest$PositiveDec)
# The model's test set accuracy is only slightly better than the baseline model.

# Clustering Stocks
limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

# We remove the dependent variable so we can effectively follow the cluster-then-predict method.
# Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology.

# In cases where we have a training and testing set, we'll want to normalize by the mean and standard deviation of the variables in the training set. 
# We can do this by passing just the training set to the preProcess function:
library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

mean(stocksTrain$ReturnJan)
mean(stocksTest$ReturnJan)

# Since normTest was constructed by subtracting by the mean ReturnJan value from the training set, 
# this explains why the mean value of ReturnJan is slightly negative in normTest.

# Run k-means clustering with 3 clusters on normTrain, storing the result in an object called km.
set.seed(144)
km = kmeans(normTrain, centers=3)
str(km)
table(km$cluster)

library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)

# Cluster-Specific Predictions

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

# Which variables have a positive sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3 
# and a negative sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3?
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family="binomial")
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family="binomial")
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family="binomial")

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type="response")
PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type="response")
PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type="response")

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)

(30 + 774) / nrow(stocksTest1)
(388 + 757) / nrow(stocksTest2)
(49 + 13) / nrow(stocksTest3)

# To compute the overall test-set accuracy of the cluster-then-predict approach, 
# we can combine all the test-set predictions into a single vector and all the true outcomes into a single vector:
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions > 0.5)
(467 + 1544) / length(AllOutcomes)

# We see a modest improvement over the original logistic regression model. 
# Since predicting stock returns is a notoriously hard problem, this is a good increase in accuracy. 
# By investing in stocks for which we are more confident that they will have positive returns (by selecting the ones with higher predicted probabilities), 
# this cluster-then-predict model can give us an edge over the original logistic regression model.




