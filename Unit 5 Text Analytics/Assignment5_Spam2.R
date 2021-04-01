# Assignment 5
# Separating Spam from Ham (Part 2 - OPTIONAL)

# Consider the case of an email provider using the spam filter we have developed. 
# The email provider moves all of the emails flagged as spam to a separate "Junk Email" folder, meaning those emails are not displayed in the main inbox. 
# The emails not flagged as spam by the algorithm are displayed in the inbox. 
# Many of this provider's email users never check the spam folder, so they will never see emails delivered there.

# A false negative means the model labels a spam email as ham. 
# This results in a spam email being displayed in the main inbox.

# A false positive means the model labels a ham email as spam. 
# This results in a ham email being sent to the Junk Email folder.

# A false negative is largely a nuisance (the user will need to delete the unsolicited email). 
# However a false positive can be very costly, since the user might completely miss an important email due to it being delivered to the spam folder. 
# Therefore, the false positive is more costly.

wordCount = rowSums(as.matrix(dtm))

# What would have occurred if we had instead created wordCount using spdtm instead of dtm?
# wordCount would have only counted some of the words, but would have returned a result for all the emails correct

hist(wordCount)
# The data is skew right -- there are a large number of small wordCount values and a small number of large values. correct

hist(log(wordCount))
# The data is not skewed -- there are roughly the same number of unusually large and unusually small log(wordCount) values.

# Create a variable called logWordCount in emailsSparse that is equal to log(wordCount). 
# Use the boxplot() command to plot logWordCount against whether a message is spam. 
emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount ~ emails$spam)
# logWordCount is slightly smaller in spam messages than in ham messages correct

train2 = subset(emailsSparse, split == TRUE)
test2 = subset(emailsSparse, split == FALSE)

spam2CART = rpart(spam ~ ., data=train2, method="class")
prp(spam2CART)

set.seed(123)
spam2RF = randomForest(spam ~ ., data=train2)

# Perform test-set predictions using the new CART and random forest models.
predTestCART2 = predict(spam2CART, newdata=test2)[,2]
table(test2$spam, predTestCART2 > 0.5)
(1214 + 384) / nrow(test2)
testCART2.ROCR = prediction(predTestCART2, test2$spam)
as.numeric(performance(testCART2.ROCR, "auc")@y.values)

predTestRF2 = predict(spam2RF, newdata=test2, type="prob")[,2]
table(test2$spam, predTestRF2 > 0.5)
(1297 + 383) / nrow(test2)
testRF2.ROCR = prediction(predTestRF2, test2$spam)
as.numeric(performance(testRF2.ROCR, "auc")@y.values)

# In this case, adding the logWordCounts variable did not result in improved results on the test set for the CART or random forest model.

# Another source of information that might be extracted from text is the frequency of various n-grams. 
# An n-gram is a sequence of n consecutive words in the document. 
# For instance, for the document "Text analytics rocks!", which we would preprocess to "text analyt rock", 
# the 1-grams are "text", "analyt", and "rock", 
# the 2-grams are "text analyt" and "analyt rock", 
# and the only 3-gram is "text analyt rock". n-grams are order-specific, meaning the 2-grams "text analyt" and "analyt text" are considered two separate n-grams. 
# We can see that so far our analysis has been extracting only 1-grams.
