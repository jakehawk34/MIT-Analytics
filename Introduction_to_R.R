8*6 # Multiplication
2^16 # Exponents
sqrt(2) # Square root
abs(-65)  # Absolute value
SquareRoot2 = sqrt(2) # Variable assignment
SquareRoot2
HoursYear <- 365*24
HoursYear
ls() # List all current objects

c(2,3,5,8,13) # Create a vector
Country = c("Brazil","China","India","Switzerland","USA")  # Assign a vector to a variable
LifeExpectancy = c(74,76,65,83,79) # Assign corresponding life expectancy values
Country
LifeExpectancy
Country[1] # Access first index value
LifeExpectancy[3] # Access third index value
seq(0,100,2) # Create a sequence of all even numbers from 0 to 100
CountryData = data.frame(Country,LifeExpectancy) # Create a dataframe with country and life expectancy data
CountryData
CountryData$Population = c(199000, 1390000, 1240000, 7997, 318000) # Create a new column in the dataframe

Country = c("Australia","Greece") # Concatenate new country values
LifeExpectancy = c(82,81) # Concatenate new life expectancy values
Population = c(23050, 11125) # New population values
NewCountryData = data.frame(Country, LifeExpectancy, Population) # Second country data frame
NewCountryData

AllCountryData = rbind(CountryData, NewCountryData) # Use rbind to combine two data frames with identical columns
AllCountryData

getwd() # Show current working directory

WHO = read.csv('WHO.csv') # read csv file into WHO object

str(WHO)

summary(WHO)

# subset takes two arguments: first is the data frame, second is the criteria that we want to check
# Take observations whose region is exactly equal to Europe
WHO_Europe = subset(WHO, Region == "Europe") 

write.csv(WHO_Europe, "WHO_Europe.csv")
ls() #WHO_Europe is in the environment

# Remove WHO_Europe from the environment
rm(WHO_Europe)

ls() #WHO_Europe is no longer there

Under15
WHO$Under15

mean(WHO$Under15) # mean of % of pop. under 15
sd(WHO$Under15) # standard deviation

summary(WHO$Under15) # Summary of Under 15 variable

# Returns row number of the observation with minimum % of pop. under 15
which.min(WHO$Under15)

WHO$Country[86] # Gives the name of the country (Japan)

# Returns row number of the observation with maximum % of pop. under 15
which.max(WHO$Under15)

WHO$Country[124] # Gives the name of the country (Niger)

# Scatter plot of GNI vs. FertilityRate
plot(WHO$GNI, WHO$FertilityRate)

# Use the subset function to identify countries with a GNI > 10000
# and a fertility rate > 2.5
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)

# See how many rows of data are in the subset using nrow
nrow(Outliers)

# Output the country names of the outliers
Outliers[c("Country", "GNI", "FertilityRate")]

# Mean value of the Over 60 variable
mean(WHO$Over60)

# Country with the smallest % of the pop. over 60
which.min(WHO$Over60)
WHO$Country[183]

# Country with the largest literacy rate
which.max(WHO$LiteracyRate)
WHO$Country[44]


# CellularSubscribers histogram
hist(WHO$CellularSubscribers)

# Box plot of LifeExpectancy sorted by Region
boxplot(WHO$LifeExpectancy ~ WHO$Region)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expectancy", main = "Life Expectancy of Countries by Region")

# Summary table of Region variable
table(WHO$Region)

# Splits the data by region and computes the mean of Over60
tapply(WHO$Over60, WHO$Region, mean)

# Splits the data by region and computes the min of LiteracyRate, excluding null values
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm = TRUE)
