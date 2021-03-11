# Assignment 1
# An Analytical Detective

mvt = read.csv("mvtWeek1.csv")
str(mvt)
summary(mvt)

# Maximum of ID
max(mvt$ID)

# Minimum of Beat
min(mvt$Beat)

# Number of observations with Arrest equal to TRUE
15536

# Number of observations with LocationDescription equal to ALLEY
Alley = (match(mvt$LocationDescription, "ALLEY") == 1)
summary(Alley)

# First entry in Date variable
mvt$Date[1]

# Convert characters into a Date object
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

# Extract the month and day of the week
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

# Month in which the least amount of motor vehicle thefts occurred
which.min(table(mvt$Month))

# Weekday on which the most motor vehicle thefts occurred
which.max(table(mvt$Weekday))

# Month with the most arrests made
table(mvt$Month, mvt$Arrest)

# Histogram of the Date variable
hist(mvt$Date, breaks = 100)

# Box plot of the Date variable sorted by Arrest variable
boxplot(mvt$Date ~ mvt$Arrest)

# Proportion of motor vehicle thefts in 2001 where an arrest was made
Arrests2001 = subset(mvt, mvt$Year == 2001)
table(Arrests2001$Arrest)
(2152) / (18517 + 2152)

# Proportion of motor vehicle thefts in 2007 where an arrest was made
Arrests2007 = subset(mvt, mvt$Year == 2007)
table(Arrests2007$Arrest)
(1212) / (1212 + 13068)

# Proportion of motor vehicle thefts in 2012 where an arrest was made
Arrests2012 = subset(mvt, mvt$Year == 2012)
table(Arrests2012$Arrest)                                      
(550) / (550 + 13542)

# Top five locations where motor vehicle thefts occur (excluding OTHER)
locations = sort(table(mvt$LocationDescription))
length(locations)
locations[73:78]

# Top five locations data set
Top5 = subset(mvt, mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL" |
                    mvt$LocationDescription == "GAS STATION" |
                    mvt$LocationDescription == "ALLEY" |
                    mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
                    mvt$LocationDescription == "STREET")
str(Top5)

Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)

249 / (2059+249) # ALLEY Arrest rate
132 / (1543+132) # DRIVEWAY
439 / (439+1672) # GAS STATION
1603 / (1603+13249) # PARKING LOT/GARAGE
11595 / (11595+114969) # STREET

# Day of the week where most vehicle thefts occur at gas stations
table(Top5$LocationDescription == "GAS STATION", Top5$Weekday)

#Day of the week where fewest vehicle thefts occur in residential driveways
table(Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL", Top5$Weekday)
