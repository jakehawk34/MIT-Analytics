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

