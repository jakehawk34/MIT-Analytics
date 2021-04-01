# Assignment 5
# Separating Spam from Ham (Part 1)

emails = read.csv("emails.csv", stringsAsFactors = FALSE)
str(emails)
summary(emails)

table(emails$spam)

emails$text[1]
emails$text[2]

# How many characters are in the longest email in the dataset?
max(nchar(emails$text))

# Which row contains the shortest email in the dataset?
which.min(nchar(emails$text))

# Follow the standard steps to build and pre-process the corpus:
corpus = VCorpus(VectorSource(emails$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
summary(dtm)

# To obtain a more reasonable number of terms, limit dtm to contain terms appearing in at least 5% of documents, and store this result as spdtm 
spdtm = removeSparseTerms(dtm, 0.95)
summary(spdtm)

# Build a data frame called emailsSparse from spdtm, and use the make.names function to make the variable names of emailsSparse valid.
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

# What is the word stem that shows up most frequently across all the emails in the dataset?
sort(colSums(emailsSparse))

emailsSparse$spam = emails$spam

# How many word stems appear at least 5000 times in the ham emails in the dataset?
sort(colSums(subset(emailsSparse, spam == 0)))

# How many word stems appear at least 1000 times in the spam emails in the dataset?
sort(colSums(subset(emailsSparse, spam == 1))) # Do not count the dependent variable "spam"

emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
split = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, split == TRUE)
test = subset(emailsSparse, split == FALSE)

# Using the training set, train the following three machine learning models. The models should predict the dependent variable "spam", using all other available variables as independent variables. 
# Please be patient, as these models may take a few minutes to train.

# 1) A logistic regression model called spamLog. You may see a warning message here - we'll discuss this more later.
spamLog = glm(spam ~ ., data=train, family="binomial")
predTrainLog = predict(spamLog, type="response")

# 2) A CART model called spamCART, using the default parameters to train the model (don't worry about adding minbucket or cp). 
# Remember to add the argument method="class" since this is a binary classification problem.
spamCART = rpart(spam ~ ., data=train, method="class")
predTrainCART = predict(spamCART)[,2]

# 3) A random forest model called spamRF, using the default parameters to train the model (don't worry about specifying ntree or nodesize). 
# Directly before training the random forest model, set the random seed to 123 
# (even though we've already done this earlier in the problem, it's important to set the seed right before training the model so we all obtain the same results. 
# Keep in mind though that on certain operating systems, your results might still be slightly different).
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)
predTrainRF = predict(spamRF, type="prob")[,2]

table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)

# How many variables are labeled as significant (at the p=0.05 level) in the logistic regression summary output?
summary(spamLog)

# How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree?
prp(spamCART)

# What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(train$spam, predTrainLog > 0.5)
(3052 + 954) / nrow(train)

# Training set AUC of spamLog
trainLogROCR = prediction(predTrainLog, train$spam)
as.numeric(performance(trainLogROCR, "auc")@y.values)

# What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions?
table(train$spam, predTrainCART > 0.5)
(2885 + 894) / nrow(train)

# What is the training set AUC of spamCART?
trainCART.ROCR = prediction(predTrainCART, train$spam)
as.numeric(performance(trainCART.ROCR, "auc")@y.values)

# Training set accuracy of spamRF, using a threshold of 0.5 for predictions
table(train$spam, predTrainRF > 0.5)
(3015 + 916) / nrow(train)

# Training set AUC of spamRF
trainRF.ROCR = prediction(predTrainRF, train$spam)
as.numeric(performance(trainRF.ROCR, "auc")@y.values)

# What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
predTestLog = predict(spamLog, newdata=test, type="response")
table(test$spam, predTestLog > 0.5)
(1257 + 376) / nrow(test)

# Testing set AUC of spamLog
testLogROCR = prediction(predTestLog, test$spam)
as.numeric(performance(testLogROCR, "auc")@y.values)

# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
predTestCART = predict(spamCART, newdata=test)[,2]
table(test$spam, predTestCART > 0.5)
(1228 + 386) / nrow(test)

# Testing set AUC of spamCART
testCART.ROCR = prediction(predTestCART, test$spam)
as.numeric(performance(testCART.ROCR, "auc")@y.values)

# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]
table(test$spam, predTestRF > 0.5)
(1291 + 387) / nrow(test)

# Testing set AUC of spamRF
testRF.ROCR = prediction(predTestRF, test$spam)
as.numeric(performance(testRF.ROCR, "auc")@y.values)

# In terms of testing set performance, 
# the random forest outperformed logistic regression and CART in both measures, obtaining an impressive AUC of 0.997 on the test set.

# Both CART and random forest had very similar accuracies on the training and testing sets. 
# However, logistic regression obtained nearly perfect accuracy and AUC on the training set and had far-from-perfect performance on the testing set. 
# This is an indicator of overfitting.
