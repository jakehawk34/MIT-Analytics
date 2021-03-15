# Recitation 2
# NBA

NBA = read.csv("NBA_train.csv")
str(NBA)

# How many games does a team need to win in order to make the playoffs?
table(NBA$W, NBA$Playoffs)

NBA$PTSdiff = NBA$PTS - NBA$oppPTS

# Scatter plot to see if there is a linear relationship between number of wins and points difference
plot(NBA$PTSdiff, NBA$W)

# Linear regression model to predict wins using points difference
WinsReg = lm(W ~ PTSdiff, data = NBA)
summary(WinsReg)
# Regression equation: W = 41 + 0.0326 * PTSdiff >= 42 wins
# PTSdiff
(42 - 41) / 0.0326
# PTSdiff needs to be at least 30.67 to win 42 games


# Build a regression model to predict PTS
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg)

# Calculate the sum of squared errors (SSE)
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
SSE

# Calculate the root mean squared error (RMSE)
RMSE = sqrt(SSE / nrow(NBA))
RMSE

mean(NBA$PTS)

# Create a new regression model to predict PTS removing TOV 
# because its p-value is the highest of the independent variables
PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(PointsReg2)

# Create a new regression model to predict PTS removing DRB 
# because its p-value is the highest of the remaining independent variables
PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary(PointsReg3)

# Create a new regression model to predict PTS removing BLK 
# because its p-value is the highest of the remaining independent variables
PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4)

# Calculate the SSE and RMSE for PointsReg4 and compare them with the values from the original model
SSE_4 = sum(PointsReg4$residuals^2)
SSE_4

RMSE_4 = sqrt(SSE_4 / nrow(NBA))
RMSE_4

# From PointsReg to PointsReg4, the new model is better because it is simpler, easier to interpret
# and has about the same amount of error

# Load the NBA testing set
NBA_test = read.csv("NBA_test.csv")

# Predict how many points there will be in the 2012-2013 NBA season
PointsPredictions = predict(PointsReg4, newdata = NBA_test)

# Compute the out of sample R-squared
SSE = sum((PointsPredictions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE / SST
RMSE = sqrt(SSE / nrow(NBA_test))
RMSE

# In sample R-squared was 0.8991, so the out of sample R-squared (0.8127) is slightly less
# In sample RMSE was about 184.5 points, so the out of sample RMSE (196.3 points) is slightly more
