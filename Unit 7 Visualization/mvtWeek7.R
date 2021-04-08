# Unit 7: Visualization
# The Analytical Policeman: Visualization for Law and Order
# Geographical Hot Spot Map for Motor Vehicle Thefts in Chicago

# A great way to visualize information in a two-dimensional table is with a heat map.
# Heat maps visualize data using three attributes.
# Two of the attributes are on the x and y-axes, typically displayed horizontally and vertically.
# The third attribute is represented by shades of color.
# In this example, lower values in the third attribute correspond to colors closer to blue,
# and higher values in the third attribute correspond to colors closer to red.

# In this lecture, we'll use Chicago motor vehicle theft data to explore patterns of crime, 
# both over days of the week, and over hours of the day.
# We're interested in analyzing the total number of car thefts that occur in any particular hour
# of a day of the week over our whole data set.

mvt = read.csv("mvt.csv", stringsAsFactors = FALSE)
str(mvt)

# We want to first convert the Date variable to a format that R will recognize so that we can extract the day of the week
# and the hour of the day.
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")

# In this format, we can extract the hour and the day of the week from the Date variable,
# and we can add these as new variables to our data frame.
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

# Line plot with just one line and a value for every day of the week.
table(mvt$Weekday)
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)
# Var1 gives the day of the week, Freq gives the total amount of crime on each day.

# Now, we're ready to make our plot using ggplot2.
# So type ggplot, and then we need to give the name of our data, which is WeekdayCounts.
# And then we need to define our aesthetic. So our aesthetic should have x = Var1,
# since we want the day of the week on the x-axis, and y = Freq, since we want the frequency,
# the number of crimes, on the y-axis.
# Now, we just need to add geom_line(aes(group=1)).
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1))

# Our days of the week are a little out of order because ggplot put them in alphabetical order.

# But we actually want the days of the week in chronological order
# to make this plot a bit easier to read.
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1))

# The last thing we'll want to do to our plot is just change the x- and y-axis labels, since they're not very helpful as they are now.
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1)) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

# Quick Question 3

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1), linetype=2)
# Makes the line dashed

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1), alpha=0.3)
# Makes the line lighter

# We will create a line plot where each day of the week has a separate line and
# hour is on the x-axis

table(mvt$Weekday, mvt$Hour)

DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)

# Let's convert the second variable, Var2, to actual numbers and call it Hour,
# since this is the hour of the day, and it makes sense that it's numerical.
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

# Create the plot
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group=Var1))

# Change the colors of the lines to correspond to the days of the week
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group=Var1, color=Var1), size=2)

# Let's instead visualize the same information with a heat map.

# Fix the order of the days to show in chronological order, instead of alphabetical order
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill=Freq))

# Change the label on the legend
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y=element_blank())

# Change the color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y=element_blank())

# In this video, we'll plot crime on a map of Chicago.
# First, we need to install and load two new packages, the maps package and the ggmap package.

install.packages("maps")
library(maps)
install.packages("ggmap")
library(ggmap)

# Now, let's load a map of Chicago into R.


if(!requireNamespace("devtools")) 
install.packages("devtools") 
devtools::install_github("dkahle/ggmap", ref = "tidyup", force = TRUE)
library(ggmap) 
register_google(key = "AIzaSyDFGlIFPbjeZPTEFJZVGBOcm_ZoTkuQidE")

chicago = get_map(location = "chicago", zoom = 11)

# Let's take a look at the map by using the ggmap function.
ggmap(chicago)

# Plot the first 100 motor vehicle thefts on the map
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x = Longitude, y = Latitude))

# Let's round our latitude and longitude to two digits of accuracy and create a crime counts data
# frame for each area to see if an area has a high crime rate.
LatLonCounts = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
str(LatLonCounts)

# Convert our longitude and latitude variables to numbers
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

# Now, let's plot these points on our map, making the size and color of the points
# depend on the total number of motor vehicle thefts.
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x = Long, y = Lat, color = Freq, size = Freq))

# Change the color scheme so red shows high crime areas and yellow show low crime areas.
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x = Long, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low="yellow", high="red")

# We can also use geom_tile to make something that looks more like a traditional heat map.
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")

# We've created a geographical heat map, which in our case shows a visualization of the data,
# but it could also show the predictions of a model.

# Quick Question 5

# In the previous video, our heatmap was plotting squares out in the water, which seems a little strange. 
# We can fix this by removing the observations from our data frame that have Freq = 0.
LatLonCounts2 = subset(LatLonCounts, Freq > 0)
ggmap(chicago) + geom_point(data=LatLonCounts2, aes(x = Long, y = Lat, color = Freq, size = Freq))
ggmap(chicago) + geom_point(data=LatLonCounts2, aes(x = Long, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low="yellow", high="red")
ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x = Long, y = Lat, alpha = Freq), fill="red")
nrow(LatLonCounts) - nrow(LatLonCounts2) # 952 rows were removed
