# Assignment 4
# Predicting Earnings from Census Data

census = read.csv("census.csv", stringsAsFactors = TRUE)
str(census)
summary(census)

set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)

incomelm = glm(over50k ~ ., data = train, family="binomial")
summary(incomelm)

# What is the accuracy of the model on the testing set? Use a threshold of 0.5.
pred1 = predict(incomelm, newdata = test, type="response")
table(test$over50k, pred1 >= 0.5)
(9051 + 1888) / nrow(test)

# Baseline accuracy for the testing set?
table(train$over50k)
table(test$over50k)
9713 / nrow(test)

# AUC for this model on the testing set?
library(ROCR)
ROCRpred = prediction(pred1, test$over50k)
ROCRperf = as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf
# AUC for logistic regression
ROCRcurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCRcurve)
# Add colors to the ROCR plot
plot(ROCRcurve, colorize = TRUE)
# Add threshold labels to the plot
plot(ROCRcurve, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# Let us now build a classification tree to predict "over50k". 
# Use the training set to build the model, and all of the other variables as independent variables. 
# Use the default parameters, so don't set a value for minbucket or cp
set.seed(2000)
incomeTree = rpart(over50k ~ ., data = train, method="class")
prp(incomeTree)
# The tree splits on relationship at the first level.
# The tree splits on capitalgains and education at the second level.

# Accuracy of the model on the testing set
treePredict = predict(incomeTree, newdata=test, type="class")
table(test$over50k, treePredict)
(9243 + 1596) / nrow(test)

# Let us now consider the ROC curve and AUC for the CART model on the test set. 
# You will need to get predicted probabilities for the observations in the test set to build the ROC curve and compute the AUC. 
# Remember that you can do this by removing the type="class" argument when making predictions, and taking the second column of the resulting object.

# AUC for CART model
predCART = predict(incomeTree, newdata=test)[,2]
ROCRpred2 = prediction(predCART, test$over50k)
CARTcurve = performance(ROCRpred2, "tpr", "fpr")
plot(CARTcurve, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# AUC for the CART model
as.numeric(performance(ROCRpred2, "auc")@y.values)

# Take a smaller training set to use for the random forest model
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
incomeForest = randomForest(over50k ~ ., data = trainSmall)

# What is the accuracy of the model on the test set, using a threshold of 0.5?
forestPredict = predict(incomeForest, newdata = test)
table(test$over50k, forestPredict)
(8955 + 1953) / nrow(test)

# Although random forests are less interpretable than CART models, we can still compute metrics to gain insight into what they do.
# One metric that we can look at is the number of times, aggregated over all of the trees in the random forest model, 
# that a certain variable is selected for a split
vu = varUsed(incomeForest, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(incomeForest$forest$xlevels[vusorted$ix]))

# A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. 
# In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased.
varImpPlot(incomeForest)

# Selecting cp by cross-validation
set.seed(2)
tr.control = trainControl(method = "cv", number = 10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
tr = train(over50k ~ ., data = train, method="rpart", trControl=tr.control, tuneGrid=cartGrid)
tr

# Fit a CART model to the training data using the best cp value
bestCART = rpart(over50k ~ ., data = train, method="class", cp = 0.002)
best.predict = predict(bestCART, newdata = test, type="class")
table(test$over50k, best.predict)
# Accuracy
(9178 + 1838) / nrow(test)

# Plot the CART tree for this model
prp(bestCART)

# This highlights one important tradeoff in building predictive models. 
# By tuning cp, we improved our accuracy by over 1%, but our tree became significantly more complicated. 
# In some applications, such an improvement in accuracy would be worth the loss in interpretability. 
# In others, we may prefer a less accurate model that is simpler to understand and describe over a more accurate -- but more complicated -- model.
