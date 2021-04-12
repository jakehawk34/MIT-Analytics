# Assignment 7
# Election Forecasting Revisited

statesMap = map_data("state")
str(statesMap)
table(statesMap$group)

# Draw a map of the United States
ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black")

# Color the map according to our 2012 presidential election predictions from Unit 3.
polling = read.csv("PollingImputed.csv")
str(polling)

Train = subset(polling, Year < 2012)
Test = subset(polling, Year == 2012)

# Create a logistic regression model
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")

# Create a vector of Republican/Democrat predictions 
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

# Now, put the predictions and state labels in a data.frame so that we can use ggplot
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(TestPredictionBinary)
mean(TestPrediction)

# Convert the Test.State variable to lowercase
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
str(statesMap)
str(predictionDataFrame)

# Merge the two data frames
predictionMap = merge(statesMap, predictionDataFrame, by="region")

# Make sure that the observations are in order so that the map is drawn correctly.
predictionMap = predictionMap[order(predictionMap$order),]

# When we merge data, it only merged the observations that exist in both data sets. 
# So since we are merging based on the region variable, 
# we will lose all observations that have a value of "region" that doesn't exist in both data frames.

# Now we can color the United States based on our predictions.
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

# Replot the map with discrete outcomes
# Blue for Democrat, Red for Republican
ggplot(predictionMap, aes(x = long, y =lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color="black") + 
  scale_fill_gradient(low="blue", high="red", guide="legend", breaks=c(0,1), labels=c("Democrat", "Republican"), name="Prediction 2012")

# Alternatively, we could plot the probabilities instead of the binary predictions.
ggplot(predictionMap, aes(x = long, y =lat, group = group, fill = TestPrediction)) +
  geom_polygon(color="black") + 
  scale_fill_gradient(low="blue", high="red", guide="legend", breaks=c(0,1), labels=c("Democrat", "Republican"), name="Prediction 2012")

# The two maps look very similar. 
# This is because most of our predicted probabilities are close to 0 or close to 1.

# We incorrectly predicted Florida by predicting that it would be won by the Republican party. 

# What was our predicted probability for the state of Florida?
predictionDataFrame
subset(predictionDataFrame, region="florida")

# Our prediction model did not do a very good job of correctly predicting the state of Florida, 
# and we were very confident in our incorrect prediction. 

# Explore the different parameters of geom_polygon to create different versions of the US map.
?geom_polygon

# linetype = 3
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", linetype=3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
# size = 3
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", size=3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
# alpha = 0.3
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", alpha=0.3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


