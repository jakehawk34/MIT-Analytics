# Assignment 7
# Visualizing Network Data

users = read.csv("users.csv")
edges = read.csv("edges.csv")

str(users)
str(edges)

# Average number of friends per user
2 * nrow(edges) / nrow(users)

# B is the most common locale
table(users$locale)

# Genders A and B have attended schools A and AB
table(users$school, users$gender)

# Install and load the igraph package
install.packages("igraph")
library(igraph)

?graph.data.frame
g = graph.data.frame(edges, FALSE, users) 
plot(g, vertex.size=5, vertex.label=NA)

# In this graph, there are a number of groups of nodes where all the nodes in each group are connected 
# but the groups are disjoint from one another, forming "islands" in the graph. 
# Such groups are called "connected components," or "components" for short.

# How many connected components with at least 2 nodes are there in the graph?
# 4

# How many users are there with no friends in the network?
# 7

# We can use degree(g) to compute the degree of all the nodes in our graph g.
degree(g)
table(degree(g) >= 10)

# We will change the size of the vertices so the vertices with high degrees are larger. 
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

# What is the largest size we assigned to any node in our graph?
max(V(g)$size)

# What is the smallest size we assigned to any node in our graph?
min(V(g)$size)

# To change the color, we will update the attribute V(g)$color.
# We can update the colors by setting the color to black for all vertices, 
# then setting it to red for the vertices with gender A and setting it to gray for the vertices with gender B:

V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

# The highest degree users are Gender B.

# Now, color the vertices based on the school that each user in our network attended.
V(g)$color = "black"

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label=NA)

# The two users who attended both schools A and B Facebook are friends with each other.
# Some, but not all, of the high-degree users attended school A.\

# Now, color the vertices based on the locale of the user.
V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label=NA)

# The large connected component is most associated with Locale B.
# The 4-user connected component is most associated with Locale A.

?igraph.plotting
# Which igraph plotting function would enable us to plot our graph in 3-D?
# rglplot

# What parameter to the plot() function would we use to change the edge width when plotting g?
# edge.width



