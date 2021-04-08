# Unit 7: Visualization
# Visualizing the World: An Introduction to Visualization

WHO = read.csv("WHO.csv")
str(WHO)

# In week one, the very first plot we made in R was a scatterplot of fertility rate versus gross national income.
plot(WHO$GNI, WHO$FertilityRate)

# Now, let's redo this scatterplot, but this time using ggplot.

# Object with the data and aesthetic mapping
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()
# No data set name witht the dollar sign on x and y axes
# Nice grid lines in the background and solid points

# Line graph
scatterplot + geom_line()

# We can add other options, like the color, shape, and size of the points.
# Blue triangles with size 3
scatterplot + geom_point(color="blue", size=3, shape=17)

# Dark red stars
scatterplot + geom_point(color="darkred", size=3, shape=8)

# Add a title to the scatter plot
scatterplot + geom_point(color="darkred", size=3, shape=8) + ggtitle("Fertility Rate vs. Gross National Income")

# Save the plot to a file
fertilityGNIplot = scatterplot + geom_point(color="darkred", size=3, shape=8) + ggtitle("Fertility Rate vs. Gross National Income")
pdf("MyPlot.pdf")
print(fertilityGNIplot)
dev.off()

# Quick Question 4
scatterplot + geom_point(color="red", size=3, shape=15)

# Now, let's color the points by Region instead
# Coloring by a factor variable
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

# Let's now instead color the points according to the country's life expectancy.
# Coloring by a numerical variable
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

# Whether the fertility rate of a country was a good predictor of the percentage of the population under 15
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()

# We suspect that a log transformation of FertilityRate will be better.
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

# Simple linear regression model using log(FertilityRate) to predict Under15
model = lm(Under15 ~ log(FertilityRate), data=WHO)
summary(model)
# It looks like log(FertilityRate) is a great predictor of Under15 population

# So now, let's add this regression line to our plot.
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm")

# By default, ggplot will draw a 95% confidence interval shaded around the line.
# We can change this by specifying options within the statistics layer.
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", level=0.99)

# Take away the confidence interval altogether
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", se=FALSE)

# Orange linear regression line
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", se=FALSE, color="orange")

# So what is the edge of visualizations?
# The WHO data that we used here is used by citizens, policymakers, and organizations around the world.
# Visualizing the data facilitates the understanding of global health trends at a glance.
# By using ggplot in R, we're able to visualize data for exploration, modeling, and sharing analytics results.

# Quick Question 5
ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point()

# One region in particular has a lot of countries with a very low fertility rate and a very low percentage of the population under 15. 
# Which region is it? ANSWER: Europe


