# Final Exam
# Forecasting National Parks Visits

visits = read.csv("park_visits.csv")
str(visits)

visits2016jul = subset(visits, Year == 2016 & Month == 7)

# Which park type has the most number of parks?
sort(table(visits2016jul$ParkType))

# Which specific park has the most number of visitors?
which.max(visits2016jul$logVisits)
visits2016jul$ParkName[138]

# Which region has the highest average log visits in July 2016?
tapply(visits2016jul$logVisits, visits2016jul$Region, mean)

# What is the correlation between entrance fee (the variable cost) and the log visits in July 2016?
cor(visits2016jul$cost, visits2016jul$logVisits)

# Time Series Plot of Visits
ys = subset(visits, ParkName == "Yellowstone NP")

ys_ts=ts(ys$logVisits,start=c(2010,1),freq=12)

plot(ys_ts)

# Missing Values
colSums(is.na(visits))

# To deal with the missing values, we will simply remove the observations with the missing values first 
visits = visits[rowSums(is.na(visits)) == 0, ]
nrow(visits)

# Predicting Visits
visits$Month = as.factor(visits$Month)

train = subset(visits, Year <= 2014)
test = subset(visits, Year > 2014)

mod = lm(logVisits ~ laglogVisits, data=train)
summary(mod)

# Out of Sample R^2 of testing set
pred = predict(mod, newdata=test)
SSE = sum((pred - test$logVisits)^2)
SST = sum((mean(train$logVisits) - test$logVisits)^2)

R2 = 1 - SSE/SST
R2



