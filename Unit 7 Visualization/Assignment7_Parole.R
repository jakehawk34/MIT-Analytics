# Assignment 7
# Visualizing Attributes of Parole Violators 

parole = read.csv("parole.csv")

# Since male, state, and crime are all unordered factors, convert them to factor variables
parole$male = as.factor(parole$male)

parole$state = as.factor(parole$state)

parole$crime = as.factor(parole$crime)

table(parole$male == 0, parole$violator)
14 / (64 + 14)

kentucky = subset(parole, state == 2)
table(kentucky$crime)

# Creating a Basic Histogram

# Create a histogram to find out the distribution of the age of parolees
ggplot(data = parole, aes(x = age)) + 
  geom_histogram(binwidth = 5, boundary = 0, color = 'black', fill = 'cornflowerblue')

# Redo the histogram, adding the following argument to the geom_histogram function: color="blue".
ggplot(data = parole, aes(x = age)) + 
  geom_histogram(binwidth = 5, boundary = 0, color = 'blue', fill = 'cornflowerblue')

# Adding Another Dimension

# Another option would be to stick with histograms, but to create a separate histogram for each gender.
ggplot(data = parole, aes(x = age)) + 
  geom_histogram(binwidth = 5, boundary = 0) + facet_grid(male ~ .)

# Now change the facet_grid argument to be ".~male" instead of "male~."
ggplot(data = parole, aes(x = age)) + 
  geom_histogram(binwidth = 5, boundary = 0) + facet_grid(. ~ male)

# An alternative to faceting is to simply color the different groups differently. 
# To color the data points by group, we need to tell ggplot that a property of the data (male or not male) 
# should be translated to an aesthetic property of the histogram. 
# We can do this by setting the fill parameter within the aesthetic to male.

ggplot(data = parole, aes(x = age, fill = male)) + 
  geom_histogram(binwidth = 5, boundary = 0)

colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data = parole, aes(x = age, fill = male)) + 
  geom_histogram(binwidth = 5, boundary = 0) + 
  scale_fill_manual(values=colorPalette)

# An alternative to a single, stacked histogram is to create two histograms and overlay them on top of each other. 
# This is a simple adjustment to our previous command.

ggplot(data = parole, aes(x = age, fill = male)) + 
  geom_histogram(binwidth = 5, boundary = 0, position = "identity", alpha=0.5) + 
  scale_fill_manual(values=colorPalette)

# There are no female parolees in the age groups 15-19, 55-59, and 65-69

# Time Served

# Now let's explore another aspect of the data: the amount of time served by parolees. 
# Create a basic histogram like the one we created in Problem 2, 
# but this time with time.served on the x-axis. Set the bin width to one month.

ggplot(data = parole, aes(x = time.served)) + 
  geom_histogram(binwidth = 1, boundary = 0, color = 'black', fill = 'cornflowerblue')

# Change the binwidth to 0.1 months.
ggplot(data = parole, aes(x = time.served)) + 
  geom_histogram(binwidth = 0.1, boundary = 0, color = 'black', fill = 'cornflowerblue')

# Be careful when choosing the binwidth - it can significantly affect the interpretation of a histogram! 
# When visualizing histograms, it is always a good idea to vary the bin size in order to understand the data at various granularities.

# Now, suppose we suspect that it is unlikely that each type of crime has the same distribution of time served.
ggplot(data = parole, aes(x = time.served)) + 
  geom_histogram(binwidth = 1, boundary = 0, color = 'black', fill = 'cornflowerblue') +
  facet_grid(. ~ crime)

# Now, instead of faceting the histograms, overlay them.
ggplot(data = parole, aes(x = time.served, fill = crime)) + 
  geom_histogram(binwidth = 0.1, boundary = 0, position = "identity", alpha = 0.5) +
  scale_fill_manual(values=colorPalette)

# While overlaying the plots is allowed and lets us observe some attributes of the plots like the most common crime type, 
# it can be hard to tell them apart and if they have similar values it can be hard to read.  
