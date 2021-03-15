# Assignment 2
# State Data

# Load the data set and convert it to a data frame by running the following two commands in R:

data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)

# Plot all of the states' centers with latitude on the y axis (the "y" variable in our dataset) and longitude on the x axis (the "x" variable in our dataset)
plot(statedata$x, statedata$y)

# Using the tapply command, determine which region of the US (West, North Central, South, or Northeast) 
# has the highest average high school graduation rate of all the states in the region:
tapply(statedata$HS.Grad, statedata$state.region, mean)

# Make a boxplot of the murder rate by region
boxplot(statedata$Murder ~ statedata$state.region)

# You should see that there is an outlier in the Northeast region of the boxplot you just generated. 
# Which state does this correspond to?
NortheastData = subset(statedata, state.region == "Northeast")

# Build a model to predict life expectancy by state using the state statistics we have in our dataset
LifeExpReg = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(LifeExpReg)

plot(statedata$Income, statedata$Life.Exp)

# You should be able to find a good model with only 4 independent variables, instead of the original 7. 
# Which variables does this model contain?
LifeExpReg2 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpReg2)
LifeExpReg3 = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpReg3)
LifeExpReg4 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpReg4)

# Which state do we predict to have the lowest life expectancy? (Hint: use the sort function)
which.min(predict(LifeExpReg4)) # Alabama

# Which state actually has the lowest life expectancy? (Hint: use the which.min function)
which.min(statedata$Life.Exp)
statedata$state.name[40] # South Carolina

# Which state do we predict to have the highest life expectancy?
which.max(predict(LifeExpReg4)) # Washington

# Which state actually has the highest life expectancy?
which.max(statedata$Life.Exp)
statedata$state.name[11] # Hawaii

# For which state do we make the smallest absolute error?
which.min(abs(LifeExpReg4$residuals)) # Indiana

# For which state do we make the largest absolute error?
which.max(abs(LifeExpReg4$residuals)) # Hawaii


