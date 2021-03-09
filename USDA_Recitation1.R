#Understanding Food: Nutritional Education wtih Data (Recitation 1)

USDA = read.csv("USDA.csv")
str(USDA)
summary(USDA)

USDA$Sodium

# Gives the observation number with the highest Sodium value
which.max(USDA$Sodium)

# Gives the names of all the variables in the USDA data frame
names(USDA)

# Item name with the highest Sodium value
USDA$Description[265]

# Subset of USDA data frame with foods that have greater than 10000 mg of Sodium
HighSodium = subset(USDA, Sodium > 10000)
nrow(HighSodium)

# Get the names of the foods in the HighSodium subset
HighSodium$Description

# Track down the word "CAVIAR" in the description vector of USDA
match("CAVIAR", USDA$Description)

# Get Sodium level in CAVIAR
USDA$Sodium[4154]

# Alternate way to get CAVIAR's Sodium level
USDA$Sodium[match("CAVIAR", USDA$Description)]

summary(USDA$Sodium)
sd(USDA$Sodium) # Forgot to remove NA values

sd(USDA$Sodium, na.rm=TRUE) # NA values removed

# Scatter plot of Protein on x-axis and Total Fat on y-axis
plot(USDA$Protein, USDA$TotalFat)
plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs Fat")

# Histogram of VitaminC
# Most values ar between 0 and 100
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels")
# Histogram zooms into the 0-100 cell instead of breaking it up
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels", xlim = c(0, 100))
# Breaks 2000 mg range of data into 100, 20-mg levels and shows us the first five levels
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels", xlim = c(0, 100), breaks = 100)
# Set breaks to 2000
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels", xlim = c(0, 100), breaks = 2000)
# Conclusion: more than 5,000 foods have less than 1 mg of Vitamin C

# Box plot for Sugar
boxplot(USDA$Sugar, main = "Box Plot of Sugar Levels", ylab = "Sugar (g)")
# There exist some foods that have nearly 100 g of sugar in 100 g

# Add a new variable to the data frame
# 1 if a food has higher than the average Sodium content, 0 for lower
USDA$Sodium[1] > mean(USDA$Sodium, na.rm = TRUE)
USDA$Sodium[50] > mean(USDA$Sodium, na.rm = TRUE)

# Creates a vector with 1 to represent TRUE, 0 to represent FALSE
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
str(HighSodium)

# Adds the HighSodium variable to the USDA data frame
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
str(USDA)

# Add HighProtein, HighFat, HighCarbs variables
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))
str(USDA)

# Understand our data and the relationships using tables and tapply
table(USDA$HighSodium)

# Table with high sodium and high fat indicators
table(USDA$HighSodium, USDA$HighFat)

# Average amount of iron sorted by high and low protein
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)
# High protein foods have an average of 3.20 mg of iron 
# Low protein foods have an average of 2.55 mg of iron

# Maximum Vitamin C level sorted by high and low carbohydrates
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm = TRUE)
# High carb foods have a max of 2400 mg of Vitamin C
# Low carb foods have a max of 1678 mg of Vitamin C

tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm = TRUE)


