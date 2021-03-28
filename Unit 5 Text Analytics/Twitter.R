# Unit 5: Text Analytics
# Turning Tweets into Knowledge: An Introduction to Text Analytics

# Twitter is a social networking and communication website founded in 2006.
# Celebrities, politicians, and companies connect with fans and customers using Twitter.
# Everyone is watching.
# Managing public perception in an age of instant communication is essential.
# Reacting to changing sentiment, identifying offensive posts, determining topics of interest are indeed crucial.

# Until now, we have seen data that are structured, numerical, or categorical.
# On the other hand, tweets are loosely structured. They are often textual.
# They have poor spelling, often contain non-traditional grammar, and they are multilingual.

# The field that addresses how computers understand text is called Natural Language Processing.
# The goal is to understand and derive meaning.

# In this lecture, we'll be trying to understand sentiment of tweets about the company Apple.

# Our challenge in this lecture is to see if we can correctly classify tweets as being negative, positive, or neither about Apple.

# We would like to label thousands of tweets. And we know that two people might disagree over
# the correct classification of a tweet. So to do this efficiently, one option is to use the Amazon Mechanical Turk.
# So what is the Amazon Mechanical Turk? It allows people to break tasks down into small components
# and then enables them to distribute these tasks online to be solved by people all over the world.
# People can sign up to perform the available tasks for a fee. As the task creator, we pay the workers a fixed amount per completed task.
# For example, we might pay $0.02 for a single classified tweet. The Amazon Mechanical Turk serves as a broker
# and takes a small cut of the money.

# In this lecture, we'll use a technique called Bag of Words to build text analytics models.
# It just counts the number of times each word appears in the text and uses these counts as the independent variables.
# Preprocessing the text can dramatically improve the performance of the Bag of Words method.

# Computers are very literal by default.
# Apple with just an uppercase A, APPLE all in uppercase letters, or ApPLe with a mixture of uppercase and lowercase letters 
# will all be counted separately.
# Punctuation can also cause problems.
# The basic approach is to deal with this is to remove everything that isn't a standard number or letter.
# However, sometimes punctuation is meaningful. In the case of Twitter, @Apple denotes a message to Apple,
# and #Apple is a message about Apple.
# In our case, we will remove all punctuation, so @Apple, Apple with an exclamation point, Apple with dashes will all count as just Apple.

# Another preprocessing task we want to do is to remove unhelpful terms.
# Many words are frequently used but are only meaningful in a sentence.
# These are called stop words. Examples are the, is, at, and which.
# It's unlikely that these words will improve the machine learning prediction quality,
# so we want to remove them to reduce the size of the data.

# There are some potential problems with this approach.
# Sometimes, two stop words taken together have a very important meaning.
# Examples are "The Who" and "Take That"

# So while removing stop words sometimes is not helpful, it generally is a very helpful preprocessing step.

# Lastly, an important preprocessing step is called stemming. This step is motivated by the desire
# to represent words with different endings as the same word.
# We probably do not need to draw a distinction between argue, argued, argues, and arguing.
# They could all be represented by a common stem, argu. The algorithmic process of performing this reduction is called stemming.

# One approach is to build a database of words and their stems.
# A pro is that this approach handles exceptions very nicely, since we have defined all of the stems.
# However, it won't handle new words at all, since they are not in the database. This is especially bad for problems
# where we're using data from the internet, since we have no idea what words will be used.

# A different approach (Porter Stemmer) is to write a rule-based algorithm.
# In this approach, if a word ends in things like ed, ing, or ly, we would remove the ending.
# A pro of this approach is that it handles new or unknown words well. However, there are many exceptions,
# and this approach would miss all of these. Words like child and children would be considered different,
# but it would get other plurals, like dog and dogs.

# Always use the extra argument "stringsAsFactors = FALSE" when working on text analytics problems
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

# We're more interested in being able to detect the tweets with clear negative sentiment,
# so let's define a new variable in our data set tweets called Negative.
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

# Pre-process our text data so that we can use the bag of words approach
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

# We'll need to convert our tweets to a corpus for pre-processing. tm can create a corpus in many different ways,
# but we'll create it from the tweet column of our data frame using two functions, Corpus and VectorSource.
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]$content

# Pre-processing is easy in tm.
# Each operation, like stemming or removing stop words, can be done with one line in R, where we use the tm_map function.

# Let's try it out by changing all of the text in our tweets to lowercase.
corpus = tm_map(corpus, tolower)
corpus[[1]]$content

# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content

# Now we want to remove the stop words in our tweets. tm provides a list of stop words for the English language.
stopwords("english")[1:10]
length(stopwords("english"))

# Removing words can be done with the removeWords argument to the tm_map function, but we need one extra argument
# this time-- what the stop words are that we want to remove.
# We'll remove all of these English stop words, but we'll also remove the word "apple"
# since all of these tweets have the word "apple" and it probably won't be very useful in our prediction problem.
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content

# Lastly, we want to stem our document with the stemDocument argument.
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

# We're now ready to extract the word frequencies to be used in our prediction problem.
# The tm package provides a function called DocumentTermMatrix that generates a matrix where
# the rows correspond to documents, in our case tweets, and the columns correspond to words in those tweets.
# The values in the matrix are the number of times that word appears in each document.
# Let's go ahead and generate this matrix and call it "frequencies."
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 205:215])

# We can look at what the most popular terms are, or words, with the function findFreqTerms.
findFreqTerms(frequencies, lowfreq = 20)

# Remove some terms that don't appear very often.
# The sparsity threshold, the second argument in removeSparseTerms, works as follows.
# If we say 0.98, this means to only keep terms that appear in 2% or more of the tweets.
# If we say 0.99, that means to only keep terms that appear in 1% or more of the tweets.
# If we say 0.995, that means to only keep terms that appear in 0.5% or more of the tweets, about six or more tweets.
sparse = removeSparseTerms(frequencies, 0.995)
sparse

# Now let's convert the sparse matrix into a data frame that we'll be able to use for our predictive models.
tweetsSparse = as.data.frame(as.matrix(sparse))

# Since R struggles with variable names that start with a number, and we probably have some words here that start with a number,
# let's run the make.names function to make sure all of our words are appropriate variable names.
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add the dependent variable to this data set
tweetsSparse$Negative = tweets$Negative

# Split data into a training and testing set
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)

# Quick Question 5
findFreqTerms(frequencies, lowfreq = 100)

# Our data is now ready, and we can build our predictive model.
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)
# If "freak", "hate", or "wtf" is in the tweet, predict TRUE for Negative. Otherwise, predict FALSE
predictCART = predict(tweetCART, newdata=testSparse, type="class")
cm1 = table(testSparse$Negative, predictCART)
cm1
sum(diag(cm1)) / sum(cm1)
(294 + 18) / nrow(testSparse)

# Baseline that always predicts negative
table(testSparse$Negative)
300 / nrow(testSparse)

# Random forest model
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse)
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)
# Accuracy
(293 + 21) / nrow(testSparse)
# This is a little better than our CART model, but due to the interpretability of our CART model,
# I'd probably prefer it over the random forest model.

# Quick Question 6
set.seed(123)
tweetLog = glm(Negative ~ ., data=trainSparse, family="binomial")
predictions = predict(tweetLog, newdata=testSparse, type="response")
table(testSparse$Negative, predictions > 0.5)
(251 + 35) / nrow(testSparse)
# If you were to compute the accuracy on the training set instead, you would see that the model does really well on the training set - this is an example of over-fitting. 
# The model fits the training set really well, but does not perform well on the test set. 
# A logistic regression model with a large number of variables is particularly at risk for overfitting.

# CONCLUSIONS
# Analytical sentiment analysis we have seen can replace more labor-intensive methods like polling.
# Text analytics can also deal with the massive amounts of unstructured data being generated on the internet.
# Computers are becoming more and more capable of interacting with humans and performing human tasks.

