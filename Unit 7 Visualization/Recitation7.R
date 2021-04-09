# Unit 7: Visualization
# The Good, the Bad, and the Ugly: Visualization Recitation (Recitation)

# So what is the difference between a good visualization and a bad visualization then?
# I would argue that a good visualization clearly and accurately conveys the key messages in the data.
# A bad visualization will obfuscate the data either through ignorance or malice.

intl = read.csv("intl.csv")
str(intl)

ggplot(intl, aes(x=Region, y=PercentOfIntl)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=PercentOfIntl))

# What we need to do is make Region an ordered factor instead of an unordered factor.
# Region is reordered based on decreasing order of PercentOfIntl.
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)

# Multiply all values by 100 so they're no longer between 0 and 1.
intl$PercentOfIntl = intl$PercentOfIntl * 100

# So we've got our labels vjust-ed above the columns.
# The bars themselves are dark blue.
# The numbers are now between 0 and 100, instead of zero and one.
# We can read all the text labels.
# And it's generally a lot more readable than the pie plot or our original ggplot, at that.
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + 
  geom_bar(stat="identity", fill="dark blue") + 
  geom_text(aes(label=PercentOfIntl), vjust=-0.4) + 
  ylab("Percent of International Students") + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# So now we're going to try plotting a world map with a new data set 
# that has the number of international students from each country.

intlall = read.csv("intlall.csv", stringsAsFactors = FALSE)
str(intlall)

# Change all NA's to zeros
intlall[is.na(intlall)] = 0
str(intlall)

# Load the world map
world_map = map_data("world")
str(world_map)

# Merge the world_map and intlall data frames into one
world_map = merge(world_map, intlall, by.x="region", by.y="Citizenship")
str(world_map)

# Plot the world map
install.packages("mapproj")
library(mapproj)

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white", color="black") + 
  coord_map("mercator")

# We have to reorder the data in the correct order to create a correct map.
# Order the rows based on the group variable and then the order variable.
# The group is equivalent to the country basically.
# The order is the correct order for the border points.
world_map = world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill="white"), color="black") + 
  coord_map("mercator")

# Some of the countries are missing.
table(intlall$Citizenship)

# Change the MIT data frame so China (People'S Republic Of) to China
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"
table(intlall$Citizenship)

# Merge and reorder again
world_map = merge(map_data("world"), intlall, by.x="region", by.y="Citizenship")
world_map = world_map[order(world_map$group, world_map$order),]
str(world_map)

# Plot the map
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=Total), color="black") + 
  coord_map("mercator")

ggplot(world_map[abs(world_map$long) < 180,], aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("mercator")

# Orthographic projection
ggplot(world_map[abs(world_map$long) < 180,], aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(20, 30, 0))

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(20, -75, 0))

# Scales
households = read.csv("households.csv")
str(households)

# First two columns of the data frame
households[,1:2]

# Now, let's look at the first few rows of our melted households data frame.
head(melt(households, id="Year"))

# First three columns of the data frame
households[,1:3]

# First ten rows of our melted households data frame.
melt(households, id="Year")[1:10,]

# Plot the melted data frame
ggplot(melt(households, id="Year"), aes(x=Year, y=value, color=variable)) + 
  geom_line(size=2) + 
  geom_point(size=5) + 
  ylab("Percentage of Households")



