# Assignment 3
# Predicting the Baseball World Series Champion

baseball = read.csv("baseball.csv")

# Identify the total number of years included in this dataset
length(table(baseball$Year))
length(unique(baseball$Year))

# Select teams that only made the playoffs using the subset function
baseball = subset(baseball, Playoffs == 1)

# Through the years, different numbers of teams have been invited to the playoffs.
table(baseball$Year)
table(table(baseball$Year))

# It's much harder to win the World Series if there are 10 teams competing for the championship versus just two. 
# Therefore, we will add the predictor variable NumCompetitors to the baseball data frame. 
# NumCompetitors will contain the number of total teams making the playoffs in the year of a particular team/year pair
PlayoffTable = table(baseball$Year)
names(PlayoffTable)
str(PlayoffTable)

# Which function call returns the number of playoff teams in 1990 and 2001?
PlayoffTable[c("1990", "2001")]

# Putting it all together, we want to look up the number of teams in the playoffs for each team/year pair in the dataset, 
# and store it as a new variable named NumCompetitors in the baseball data frame.
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
str(baseball)

# How many playoff team/year pairs are there in our dataset from years where 8 teams were invited to the playoffs?
table(baseball$NumCompetitors)

# How many observations do we have in our dataset where a team did NOT win the World Series?
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)

# Which of the following variables is a significant predictor of the WorldSeries variable in a bivariate logistic regression model?
year.mod = glm(WorldSeries ~ Year, data = baseball, family=binomial)
RS.mod = glm(WorldSeries ~ RS, data = baseball, family=binomial)
RA.mod = glm(WorldSeries ~ RA, data = baseball, family=binomial)
W.mod = glm(WorldSeries ~ W, data = baseball, family=binomial)
OBP.mod = glm(WorldSeries ~ OBP, data = baseball, family=binomial)
SLG.mod = glm(WorldSeries ~ SLG, data = baseball, family=binomial)
BA.mod = glm(WorldSeries ~ BA, data = baseball, family=binomial)
RankSeason.mod = glm(WorldSeries ~ RankSeason, data = baseball, family=binomial)
G.mod = glm(WorldSeries ~ G, data = baseball, family=binomial)
OOBP.mod = glm(WorldSeries ~ OOBP, data = baseball, family=binomial)
OSLG.mod = glm(WorldSeries ~ OSLG, data = baseball, family=binomial)
NumCompetitors.mod = glm(WorldSeries ~ NumCompetitors, data = baseball, family=binomial)
League.mod = glm(WorldSeries ~ League, data = baseball, family=binomial)

summary(year.mod)
summary(RS.mod)
summary(RA.mod)
summary(W.mod)
summary(OBP.mod)
summary(SLG.mod)
summary(BA.mod)
summary(RankSeason.mod)
summary(G.mod)
summary(OOBP.mod)
summary(OSLG.mod)
summary(NumCompetitors.mod)
summary(League.mod)

# Build a model using all of the variables that you found to be significant in the bivariate models
multivariate = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family=binomial)
summary(multivariate)

# Correlation between variables from bivariate model
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

YearRA.mod = glm(WorldSeries ~ Year + RA, data = baseball, family=binomial)
YearRank.mod = glm(WorldSeries ~ Year + RankSeason, data = baseball, family=binomial)
YearCompetitors.mod = glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family=binomial)
RA.Rank.mod = glm(WorldSeries ~ RA + RankSeason, data = baseball, family=binomial)
RA.Competitors.mod = glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family=binomial)
CompetitorsRank.mod = glm(WorldSeries ~ NumCompetitors + RankSeason, data = baseball, family=binomial)

summary(YearRA.mod)
summary(YearRank.mod)
summary(YearCompetitors.mod)
summary(RA.Rank.mod)
summary(RA.Competitors.mod)
summary(CompetitorsRank.mod)

# None of the models with two independent variables had both variables significant, 
# so none seem promising as compared to a simple bivariate model. 
# Indeed the model with the lowest AIC value is the model with just NumCompetitors as the independent variable.
# This seems to confirm the claim made by Billy Beane in Moneyball that all that matters in the Playoffs is luck, 
# since NumCompetitors has nothing to do with the quality of the teams!
