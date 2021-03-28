# Unit 5: Text Analytics
# Predictive Coding: Bringing Text Analytics to the Courtroom (Recitation)

# We'll start with the story of Enron, the United States energy company based out of Houston, Texas that was involved
# in a number of electricity production and distribution markets.

# In the early 2000s, Enron was a hot company, with a market capitalization exceeding $60 billion,
# and Forbes magazine ranked it as the most innovative US company six years in a row.
# Now, all that changed in 2001 with the news of widespread accounting fraud at the firm.
# This massive fraud led to Enron's bankruptcy, the largest ever at the time, and led to Enron's accounting firm, Arthur Andersen, dissolving.

# FERC's investigation into Enron will be the topic of today's recitation.

# Now, Enron was a huge company, and its corporate servers contained millions of emails and other electronic files.
# Sifting through these documents to find the ones relevant to an investigation is no simple task.
# In law, this electronic document retrieval process is called the eDiscovery problem, and relevant files are called responsive documents.

# Now, as part of its investigation, the FERC released hundreds of thousands of emails
# from top executives at Enron creating the largest publicly available set of emails today.
# We will use this data set called the Enron Corpus to perform predictive coding in this recitation.
# Our data set contains just two fields-- email, which is the text of the email in question,
# and responsive, which is whether the email relates to energy schedules or bids.

# Explore the basic aspects of the data
emails = read.csv("energy_bids.csv", stringsAsFactors = FALSE)
str(emails)

emails$email[1]
emails$responsive[1] # Not responsive

emails$email[2]
emails$responsive[2] # Responsive

table(emails$responsive)

# Pre-processing
corpus = Corpus(VectorSource(emails$email))
corpus[[1]]$content

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

# Bag of Words
dtm = DocumentTermMatrix(corpus)
dtm
dtm = removeSparseTerms(dtm, 0.97)
dtm

labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive
str(labeledTerms)

# Building Models
set.seed(144)
spl = sample.split(labeledTerms$responsive, SplitRatio = 0.7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

emailCART = rpart(responsive ~ ., data=train, method="class")
prp(emailCART)

# Evaluating the Model
pred = predict(emailCART, newdata=test)
pred[1:10,]
pred.prob = pred[,2]
# Threshold of 0.5
table(test$responsive, pred.prob >= 0.5)
# Accuracy
(195 + 25) / nrow(test)
# Baseline model
table(test$responsive)
215 / nrow(test)

# Typically, a human will still have to manually review all of the predicted responsive documents
# to make sure they are actually responsive.
# Therefore, if we have a false positive, in which a non-responsive document is labeled as responsive, the mistake translates
# to a bit of additional work in the manual review process but no further harm, since the manual review process
# will remove this erroneous result.
# But on the other hand, if we have a false negative, in which a responsive document is labeled as non-responsive
# by our model, we will miss the document entirely in our predictive coding process.
# Therefore, we're going to assign a higher cost to false negatives than to false positives, which makes this a good time to look
# at other cut-offs on our ROC curve.

# The ROC Curve
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values
# The model can distinguish between a randomly selected responsive and non-responsive model about 80% of the time.

# Predictive Coding Today

# In legal systems, it's difficult to change existing practice because of laws reliance on past precedent, which
# causes current decisions to be made on the basis of past ones.
# However, this status quo seems to be starting to change.
# In 2012, a US District Court ruled that predictive coding was a legitimate eDiscovery tool, which
# may pave the way for its expanded use in coming years.



