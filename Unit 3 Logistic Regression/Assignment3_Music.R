# Assignment 3
# Popularity of Music Records

songs = read.csv("songs.csv", stringsAsFactors = TRUE)
str(songs)
summary(songs)

# 373 songs from 2010 in the data set
nrow(subset(songs, year == 2010))

# 18 songs include "Michael Jackson" in the artist name
MichaelJackson = subset(songs, artistname == "Michael Jackson")
nrow(subset(songs, artistname == "Michael Jackson"))

# 5 songs by Michael Jackson made the Top 10
MichaelJacksonTop10 = subset(MichaelJackson, Top10 == 1)
MichaelJackson[c("songtitle", "Top10")]

# Values that the time signature variable can be (0, 1, 3, 4, 5, 7)
# 4 is the most frequent time signature value in the data set
table(songs$timesignature)

# The song with the highest tempo in the data set (Wanna Be Startin' Somethin')
TempoMax = subset(songs, tempo == max(tempo))
TempoMax$songtitle
which.max(songs$tempo)
songs$songtitle[6206]

# Training set with songs released up to and including 2009
# Testing set with songs released in 2010
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)

# Let's suppose that, except for the outcome variable Top10, all other variables in the training set are inputs to Model 1. 
# Then, we can use the formula:
# SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

# However, in our case, we want to exclude some of the variables in our dataset from being used as independent variables 
# ("year", "songtitle", "artistname", "songID", and "artistID"). To do this, we can use the following trick. 
# First define a vector of variable names called nonvars - these are the variables that we won't use in our model.
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

# To remove these variables from your training and testing sets, type the following commands in your R console:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

# Akaike Information Criterion (AIC) for this model is 4827.2
summary(SongsLog1)

# The model suggests that the higher our confidence about time signature, key and tempo, 
# the more likely the song is to be in the Top 10

# If the confidence is low for the time signature, tempo, and key, 
# then the song is more likely to be complex.
# The model suggests that mainstream listeners prefer less complex songs

# Songs with heavier instrumentation tend to be louder (have higher values in the variable "loudness") 
# and more energetic (have higher values in the variable "energy").
# The model has a positive coefficient for loudness, but a negative coefficient for energy

# The correlation between loudness and energy in the training set is about 0.74
cor(SongsTrain$loudness, SongsTrain$energy)
# Given that these two variables are highly correlated, Model 1 suffers from multicollinearity. 
# To avoid this issue, we will omit one of these two variables and rerun the logistic regression.

# Create Model 2, which is Model 1 without the independent variable "loudness".
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
# Model 2 suggests that songs with high energy levels tend to be more popular. 
# This contradicts our observation in Model 1.

# Create Model 3, which is Model 1 without the independent variable "energy".
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Make predictions on the test set using Model 3. 
# What is the accuracy of Model 3 on the test set, using a threshold of 0.45
SongPrediction = predict(SongsLog3, newdata = SongsTest, type = "response")
table(SongsTest$Top10, SongPrediction >= 0.45)

# Accuracy of Model 3
(309 + 19) / (309 + 5 + 40 + 19)

# Accuracy of a baseline model on the test set (Always predict a song is not a Top 10 hit)
table(SongsTest$Top10)
(314) / (309 + 5 + 40 + 19)

# Model 3 correctly predicts 19 songs that will be Top 10 hits
# Model 3 incorrectly predict 5 non-hit songs will be in the Top 10

# Sensitivity of Model 3
(19) / (19 + 40)

# Specificity of Model 3
(309) / (309 + 5)

# Conclusions about our model
# Model 3 has a very high specificity, meaning that it favors specificity over sensitivity. 
# While Model 3 only captures less than half of the Top 10 songs, it still can offer a competitive edge, 
# since it is very conservative in its predictions.




