# Unit 7: Visualization
# The Analytical Policeman: Visualization for Law and Order
# Murder Heatmap on the United States

murders = read.csv("murders.csv")
str(murders)

# A map of the United States is included in R
statesMap = map_data("state")
str(statesMap)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill="white", color="black") 

# Before we can plot our data on this map, we need to make sure that the state names are
# the same in the murders data frame and in the statesMap data frame.
# So let's create a new variable called region in our murdersdata frame 
# to match the state name variable in the statesMap data frame.
murders$region = tolower(murders$State)

# Join the statesMap data frame with the murders data frame on the region variable
# First two arguments are the data frames to be merged, third argument is the variable to merge on.
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

# So now, let's plot the number of murders on our map of the United States.
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Murders)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")

# So it looks like California and Texas have the largest number of murders.
# But is that just because they're the most populous states?
# Let's create a map of the population of each state to check.
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Population)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")

# Both maps look very similar.
# So we need to plot the murder rate instead of murders to ensure we aren't just plotting population.

# Create a new variable for the murder rate
murderMap$MurderRate = murderMap$Murders / murderMap$Population * 100000

# Now let's redo our plot with the fill equal to MurderRate.
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")

# There aren't really any red states.
# Why? It turns out that Washington, DC is an outlier with a very high murder rate,
# but it's such a small region on the map that we can't even see it.

# So let's redo our plot, removing any observations with murder rates above 10, 
# which we know will only exclude Washington, DC.
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend", limits=c(0,10))

# Now, we can see that Louisiana, Missouri, and Maryland are significantly red compared to the rest of states.

# Quick Question 6

# Redo the map from Video 6, but this time fill each state with the variable GunOwnership. 
# This shows the percentage of people in each state who own a gun.
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=GunOwnership)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")
