# Assignment 5
# Detecting Vandalism on Wikipedia

wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)

wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

# 1) Create the corpus for the Added column, and call it "corpusAdded".
corpusAdded = VCorpus(VectorSource(wiki$Added))
corpusAdded
corpusAdded[[1]]$content

# 2) Remove the English-language stopwords.
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded[[1]]$content

# 3) Stem the words.
corpusAdded = tm_map(corpusAdded, stemDocument)
corpusAdded[[1]]$content

# 4) Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions, and call the new matrix sparseAdded.
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# Convert sparseAdded to a data frame called wordsAdded, and then prepend all the words with the letter A, by using the command:
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# Now repeat all of the steps we've done so far (create a corpus, remove stop words, stem the document, create a sparse document term matrix, and convert it to a data frame) 
# to create a Removed bag-of-words dataframe, called wordsRemoved, except this time, prepend all of the words with the letter R:
corpusRemoved = VCorpus(VectorSource(wiki$Removed))
corpusRemoved
corpusRemoved[[1]]$content
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)

# Combine the two data frames into a data frame called wikiWords with the following line of code:
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, split == TRUE)
wikiTest = subset(wikiWords, split == FALSE)

table(wikiTrain$Vandal)
table(wikiTest$Vandal)
618 / nrow(wikiTest)

# Build a CART model to predict Vandal, using all of the other variables as independent variables. 
# Use the training set to build the model and the default parameters (don't set values for minbucket or cp).
wikiCART = rpart(Vandal ~ ., data=wikiTrain, method="class")
predictCART = predict(wikiCART, newdata=wikiTest, type="class")
table(wikiTest$Vandal, predictCART)
(618 + 12) / nrow(wikiTest)

# Plot the CART tree. How many word stems does the CART model use?
prp(wikiCART)

# There is no reason to think there was anything wrong with the split. 
# CART did not overfit, which you can check by computing the accuracy of the model on the training set. 
# Over-sparsification is plausible but unlikely, since we selected a very high sparsity parameter. 
# The only conclusion left is simply that bag of words didn't work very well in this case.

# We weren't able to improve on the baseline using the raw textual information. 
# More specifically, the words themselves were not useful. 
# There are other options though, and in this section we will try two techniques - identifying a key class of words, and counting words.

# We can search for the presence of a web address in the words added by searching for "http" in the Added column. 
# The grepl function returns TRUE if a string is found in another string, e.g.
#   grepl("cat","dogs and cats",fixed=TRUE) # TRUE
#   grepl("cat","dogs and rats",fixed=TRUE) # FALSE

# Create a copy of your dataframe from the previous question:
wikiWords2 = wikiWords

#Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

# Create a new CART model using this new variable as one of the independent variables.
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
predictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, predictCART2)
(609 + 57) / nrow(wikiTest2)

# Another possibility is that the number of words added and removed is predictive, 
# perhaps more so than the actual words themselves. 
# We already have a word count available in the form of the document-term matrices (DTMs).

# Sum the rows of dtmAdded and dtmRemoved and add them as new variables in your data frame wikiWords2 
# (called NumWordsAdded and NumWordsRemoved) by using the following commands:
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)


# What is the new accuracy of the CART model on the test set?
wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
predictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, predictCART3)
(514 + 248) / nrow(wikiTest3)

# We have two pieces of "metadata" (data about data) that we haven't yet used. Make a copy of wikiWords2, and call it wikiWords3:
wikiWords4 = wikiWords2

#Then add the two original variables Minor and Loggedin to this new data frame:
wikiWords4$Minor = wiki$Minor
wikiWords4$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, split==TRUE)
wikiTest4 = subset(wikiWords3, split==FALSE)

# Build a CART model using all the training data. What is the accuracy of the model on the test set?
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
predictCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictCART4)
(595 + 241) / nrow(wikiTest4)

# There is a substantial difference in the accuracy of the model using the meta data. 
# Is this because we made a more complicated model?

# Plot the CART tree. How many splits are there in the tree?
prp(wikiCART4)

# By adding new independent variables, 
# we were able to significantly improve our accuracy without making the model more complicated!

