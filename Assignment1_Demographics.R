# Assignment 1
# Demographics and Employment in the United States

CPS = read.csv("CPSData.csv", stringsAsFactors = TRUE)
summary(CPS)
str(CPS)

which.max(table(CPS$Industry))
# Educational and health services is the most common industry of employment

sort(table(CPS$State))
# State with the fewest interviewees: New Mexico
# State with the most interviewees: California

table(CPS$Citizenship)
(116639 + 7073) / (116639 + 7073 + 7590)
# 0.942 or 94.2 % of interviewees are US citizens

# Breakdown of race and hispanic ethnicity
table(CPS$Race, CPS$Hispanic)
# American Indian, Black, Multiracial and White race all have 250+ of Hispanic ethnicity

# MetroAreaCode, Married, Education, EmploymentStatus, Industry have at least one NA value

# Breakdown of whether Married is missing based on the reported value of the other variables
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
# Married value being missing is connected to the reported Age value

# MetroAreaCode is missing if an interviewee does not live in a metropolitan area
table(CPS$State, is.na(CPS$MetroAreaCode))
# Alaska and Wyoming have all interviewees living in a non-metro area
# Rhode Island, New Jersey and DC have all interviewees living in a metro area

table(CPS$Region, is.na(CPS$MetroAreaCode))
# Midwest have the largest proportion of interviewees living in a non-metro area

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))
# Wisconsin has about 30 % of interviewees living in non-metro areas
# Ignoring states with all non-metro interviewees, Montana has the highest percentage (~83%)

MetroAreaMap = read.csv("MetroAreaCodes.csv", stringsAsFactors = TRUE)
CountryMap = read.csv("CountryCodes.csv", stringsAsFactors = TRUE)


# The following command merges the two data frames on these columns, 
# overwriting the CPS data frame with the result:
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

# The first two arguments determine the data frames to be merged 
# (they are called "x" and "y", respectively, in the subsequent parameters to the merge function). 
# by.x="MetroAreaCode" means we're matching on the MetroAreaCode variable from the "x" data frame (CPS), 
# while by.y="Code" means we're matching on the Code variable from the "y" data frame (MetroAreaMap). 
# Finally, all.x=TRUE means we want to keep all rows from the "x" data frame (CPS), 
# even if some of the rows' MetroAreaCode doesn't match any codes in MetroAreaMap

summary(CPS)
str(CPS)

sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
# Laredo, TX has the highest proportion of interviewees of Hispanic ethnicity

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))
# Four metropolitan areas have at least 20 % of interviewees of Asian racial background

# To get mean (and related functions, like sum) to ignore missing values, you can pass the parameter na.rm=TRUE. 
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE))
# Iowa City, IA has the smallest proportion of interviewees who have received no high school diploma

# Merge in the country of birth information from the CountryMap data frame, replacing the CPS data frame with the result
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)
summary(CPS)

sort(table(CPS$Country))
# Among all interviewees born outside of North America, Philippines was the most common place of birth

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
1668 / (3736 + 1668)
# 0.309 of interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area 
# have a country of birth that is not the United States

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm = TRUE))
# New York has the most Indian-born interviewees
# Boston has the most Brazilian-born interviewees
# Minneapolis has the most Somalian-born interviewees



