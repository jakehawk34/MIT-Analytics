# Assignment 7
# Visualizing Text Data Using Word Clouds

tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)

# Pre-process the data, without stemming or removing sparse terms.

corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))

dtm = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(dtm))
str(allTweets)

# Although we typically stem words during the text preprocessing step, we did not do so here. 
# What is the most compelling rationale for skipping this step when visualizing text data?

# It will be easier to read and understand the word cloud if it includes full words instead of just the word stems.

install.packages("wordcloud")
library(wordcloud)

# Which function can we apply to allTweets to get a vector of the words in our dataset?
colnames(allTweets)

# Which function should we apply to allTweets to obtain the frequency of each word across all tweets?
colSums(allTweets)

# Use allTweets to build a word cloud. Make sure to check out the help page for wordcloud if you are not sure how to do this.
?wordcloud
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))
# apple is the largest word in the cloud

# Repeat the steps to load and pre-process the corpus, this time removing the most frequent word in addition to all elements of stopwords("english") 
# in the call to tm_map with removeWords.
corpus2 = Corpus(VectorSource(tweets$Tweet))

corpus2 = tm_map(corpus2, tolower)
corpus2 = tm_map(corpus2, removePunctuation)
corpus2 = tm_map(corpus2, removeWords, c("apple", stopwords("english")))

# Replace allTweets with the document-term matrix of this new corpus -- 
# we will use this updated corpus for the remainder of the assignment.
dtm2 = DocumentTermMatrix(corpus2)

allTweets = as.data.frame(as.matrix(dtm2))
str(allTweets)

# Create a word cloud with the updated corpus.
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))
# iphone is the largest word in the cloud

?wordcloud

# A word cloud that is only based on the negative tweets
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale=c(2, 0.25))

# The function brewer.pal() returns color palettes from the ColorBrewer project when provided with appropriate parameters, 
# and the function display.brewer.all() displays the palettes we can choose from.
?display.brewer.all
display.brewer.pal(8, "Set2")
display.brewer.pal(8, "Accent")
display.brewer.pal(9, "YlOrRd")

# YlOrRd is a "sequential palette," with earlier colors begin lighter and later colors being darker. 
# Therefore, it is a good palette choice for indicating low-frequency vs. high-frequency words.

display.brewer.pal(9, "Greys")

# In sequential palettes, sometimes there is an undesirably large contrast between the lightest and darkest colors.
wordcloud(colnames(allTweets), colSums(allTweets), colors=brewer.pal(9, "Blues"), scale=c(2, 0.25))

# Two possible commands to remove the first four color elements of the 9-color palette.
wordcloud(colnames(allTweets), colSums(allTweets), colors=brewer.pal(9, "Blues")[-1:-4], scale=c(2, 0.25))
wordcloud(colnames(allTweets), colSums(allTweets), colors=brewer.pal(9, "Blues")[5:9], scale=c(2, 0.25))

# Experimenting with wordcloud parameters.
wordcloud(colnames(allTweets), colSums(allTweets), min.freq=10, random.order=FALSE, colors=brewer.pal(9, "Purples")[5:9], scale=c(2, 0.25))


          