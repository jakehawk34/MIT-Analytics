# Assignment 5
# Automating Reviews in Medicine

trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
str(trials)
summary(trials)

# How many characters are there in the longest abstract? 
# (Longest here is defined as the abstract with the largest number of characters.)
max(nchar(trials$abstract))

# How many search results provided no abstract?
table(nchar(trials$abstract) == 0)

# Find the observation with the minimum number of characters in the title (the variable "title") out of all of the observations in this dataset. 
# What is the text of the title of this article?
which.min(nchar(trials$title))
trials$title[1258]

# 1) Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
corpusTitle = VCorpus(VectorSource(trials$title))
corpusAbstract = VCorpus(VectorSource(trials$abstract))

# 2) Convert corpusTitle and corpusAbstract to lowercase.
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))

# 3) Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

# 4) Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

# 5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
corpusTitle[[1]]$content
corpusAbstract[[2]]$content

# 6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# 7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

# 8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
ncol(dtmTitle)
ncol(dtmAbstract)

# Most frequent word across all of the abstracts
sort(colSums(dtmAbstract))


colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
colnames(dtmTitle)
colnames(dtmAbstract)

# Using cbind(), combine dtmTitle and dtmAbstract into a single data frame called dtm:
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
ncol(dtm)

# Build the training and testing sets
set.seed(144)
split = sample.split(dtm$trial, 0.7)
train = subset(dtm, split == TRUE)
test = subset(dtm, split == FALSE)
table(train$trial)
730 / nrow(train)

# Build a CART model
trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)

# Obtain the training set predictions for the model (do not yet predict on the test set). 
# Extract the predicted probability of a result being a trial 
# (recall that this involves not setting a type argument, and keeping only the second column of the predict output).
# Maximum predicted probability for any result
max(predict(trialCART)[,2]) 
predTrain = predict(trialCART)[,2]
summary(predTrain)

# For these questions, use a threshold probability of 0.5 to predict that an observation is a clinical trial.

# What is the training set accuracy of the CART model?
cm = table(train$trial, predict(trialCART)[,2] > 0.5)
sum(diag(cm)) / sum(cm)

# What is the training set sensitivity of the CART model?
441 / (131 + 441)

# What is the training set specificity of the CART model?
631 / (631 + 99)

# Evaluate the CART model on the testing set using the predict function and creating a vector of predicted probabilities predTest.
predTest = predict(trialCART, newdata=test)[,2]
table(test$trial, predTest > 0.5)
(261 + 162) / nrow(test)


# Using the ROCR package, what is the testing set AUC of the prediction model?
predROCR = prediction(predTest, test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values
plot(perfROCR, colorize = TRUE)

# What is the cost associated with the model in Step 1 making a false negative prediction?
# A paper that should have been included in Set A will be missed, affecting the quality of the results of Step 3.

# What is the cost associated with the model in Step 1 making a false positive prediction?
# A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the process but not affecting the quality of the results of Step 3.

# Given the costs associated with false positives and false negatives, which of the following is most accurate?
# A false negative is more costly than a false positive; the decision maker should use a probability threshold less than 0.5 for the machine learning model.






