# Recitation 4
# Location, Location, Location: Regression Trees for Housing Data

# So in this recitation, we will explore the data set with the aid of trees.
# We will compare linear regression with regression trees.
# We will discuss what the cp parameter means that we brought up when we did cross-validation in the lecture,
# and we will apply cross-validation to regression trees.

# LON and LAT are the longitude and latitude of the center of the census tract.
# MEDV is the median value of owner-occupied homes, measured in thousands of dollars.
# CRIM is the per capita crime rate.
# ZN is related to how much of the land is zoned for large residential properties.
# INDUS is the proportion of the area used for industry.
# CHAS is 1 if a census tract is next to the Charles River, which I drew before.
# NOX is the concentration of nitrous oxides in the air, a measure of air pollution.
# RM is the average number of rooms per dwelling.
# AGE is the proportion of owner-occupied units built before 1940.
# DIS is a measure of how far the tract is from centers of employment in Boston.
# RAD is a measure of closeness to important highways.
# TAX is the property tax per $10,000 of value.
# PTRATIO is the pupil to teacher ratio by town.

boston = read.csv("boston.csv")
str(boston)
plot(boston$LON, Boston$LAT)

# We want to show all the points that lie along the Charles River with a different color
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="blue", pch=19)

# Show where MIT is on the plot
points(boston$LON[boston$TRACT==3531], boston$LAT[boston$TRACT==3531], col="red", pch=19)

# Look at the distribution of air pollution using the NOX variable
summary(boston$NOX)

# Look at just the census tracts that have above-average air pollution
points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=0.55], col="green", pch=19)

# Look at how housing prices vary in the Boston area
plot(boston$LON, boston$LAT)
summary(boston$MEDV)

# Plot areas of Boston with housing prices above the median value
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)

# Try fitting a linear regression model
latlonlm = lm(MEDV ~ LAT + LON, data = boston)
summary(latlonlm)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values>=21.2], boston$LAT[latlonlm$fitted.values>=21.2], col="blue", pch="$")
# The linear regression model says Boston is mostly above the median housing prices
# The model completely ignores the right side of the plot.

# Try regression trees
library(rpart)
library(rpart.plot)

latlontree = rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree)
# In regression trees, we predict the average of the median house price in the bucket/leaf

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)
# Predict what the tree thinks is above the median house price
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>=21.2], boston$LAT[fittedvalues>=21.2], col="blue", pch="$")
# We're able to make nonlinear predictions with longitude and latitude.
# However, the tree was quite complicated, so it might be overfitting the data.
# We can fix this by changing the minbucket value in our tree

latlontree = rpart(MEDV ~ LAT + LON, data = boston, minbucket=50)
plot(latlontree)
text(latlontree)
# Far fewer splits and easier to interpret
plot(boston$LON, boston$LAT)

plot(latlontree)
text(latlontree)
# Identify the low value area in Boston using the values from the tree splits
abline(v = -71.07)
abline(h = 42.17)
abline(h = 42.21)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)

# Show that regression trees can predict housing prices better than linear regression
library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)

linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
summary(linreg)

linreg.predict = predict(linreg, newdata = test)
linreg.sse = sum((test$MEDV - linreg.predict)^2) # SSE

# Can we beat this SSE using a tree?
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
prp(tree)
# RM appears three times, NOX appear two times, CRIM and AGE each appear once
# Variables that had importance in linear regression that aren't in the tree are: PTRATIO, TAX, RAD, DIS and CHAS
tree.pred = predict(tree, newdata = test)
tree.sse = sum((test$MEDV - tree.pred)^2)
# Regression trees are not as good as linear regression for this problem.

# The CP Parameter - "Complexity Parameter"
# Intuition tells us that a more complex tree is bad for generalization, so we should punish complexity.
# Our new goal is to find a tree that minimizes the sum of the RSS at each leaf, plus lambda, times S, for the number of splits.
# Then we can define cp=lambda/RSS(no splits). When you're actually using cp in your R code,
# you don't need to think exactly what it means-- just that small numbers of cp encourage large trees, and large values of cp encourage small trees.

# Build one last tree using cross-validation

library(caret)
library(e1071)

# tr tries a range of values for cp
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = (0:10)*0.001)
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method="rpart", trControl=tr.control, tuneGrid=cp.grid)
tr

best.tree = tr$finalModel
prp(best.tree)
best.tree.pred = predict(best.tree, newdata = test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)

best.tree.sse # 3675.766
tree.sse # 4328.988
linreg.sse # 3037.088

# Trees aren't always the best method you have available to you. But you should always try cross validating them to get as much performance out of them as you can.


