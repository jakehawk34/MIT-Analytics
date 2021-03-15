# Unit 2: Linear Regression
# Moneyball: The Power of Sports Analytics

# How many games does a team need to win the regular season to make the playoffs?
# Paul DePodesta calculated that the A's needed 95 wins to make the playoffs

# How does a team win games? By scoring more runs than the other team.
# But how many more runs do you need?
# The Oakland A's calculated that they needed 135 more runs than their opponents to win 95 games

baseball = read.csv("baseball.csv", stringsAsFactors = TRUE)
str(baseball)

# To confirm the claims made in Moneyball, subset the data to observations before 2002
moneyball = subset(baseball, Year < 2002)

# Create a new variable to represent the difference between runs scored and runs allowed
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Check to see if there's a relationship between Wins and Run Difference
plot(moneyball$RD, moneyball$W)

# Create a linear regression model to predict Wins using Run Difference
WinsReg = lm(W ~ RD, data = moneyball)
summary(WinsReg)
# R-squared is 0.88
# This is a strong model to predict Wins using Run Difference (RD)

# Wins = 80.8814 + 0.1058 * (RD) >= 95 Wins
# This equation is only true if RD = (95 - 80.8814) / 0.1058 = 133.4
# The value of 133.4 RD is very close to the claim of 135 run difference necessary to win 95 games
# The linear regression model verifies DePodesta's claim

# How does a team score more runs?
# The A's discovered that two statistics were significantly more important than anything else
# 1. On-Base Percentage (OBP)
# 2. Slugging Percentage (SLG)

str(moneyball)
# Create a linear regression model to predict runs scored (RS) with OBP, SLG, and Batting Average (BA)
RunsReg = lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg)
# Multicollinearity between the three independent variables
# Remove BA from the model
RunsReg = lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg)
# We can see that OBP has a larger coefficient than SLG
# This means that OBP is more important than SLG for predicting runs scored

# Create a linear regression model to predict runs allowed (RA) with OOBP and OSLG
OppRunsReg = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(OppRunsReg)

# Quick Question: Runs Scored
-804.63 + 2737.77 * 0.311 + 1584.91 * 0.405
# Quick Question: Runs Allowed
-837.38 + 2913.6 * 0.297 + 1514 * 0.370

# Using the 2001 regular season statistics for the Oakland A's batters,
# we can predict the OBP and SLG for the 2002 season: 0.339 OBP and 0.430 SLG
# 2002 Oakland A's Runs Scored Prediction:
-804.63 + 2737.77 * (0.339) + 1584.91 * (0.430)
# 805 runs scored

# Using the 2001 regular season statistics for the Oakland A's pitchers,
# we can predict the OOBP and OSLG for the 2002 season: 0.307 OOBP and 0.373 OSLG
# 2002 Oakland A's Runs Allowed Prediction:
-837.38 + 2913.6 * (0.307) + 1514 * (0.373)
# 622 runs allowed

# Predicted Runs Difference is 805 - 622 = 183 runs
# We can plug in the Oakland A's predicted Runs Difference to predict Wins for the 2002 season
# 2002 Oakland A's Wins Prediction:
80.8814 + 0.1058 * (183)
# 100 wins

# Paul DePodesta predicted the 2002 Oakland A's would
# score 800-820 runs, allow 650-670 runs, and win 93-97 games
# The Oakland A's ended up having 800 runs scored, 653 runs allowed and 103 wins in 2002

# Quick Question: World Series
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)
plot(teamRank, wins2012)
plot(teamRank, wins2013)

