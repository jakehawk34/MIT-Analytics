# Unit 6: Clustering
# Recommendations Worth a Million: An Introduction to Clustering

# Introduction to Netflix

# Netflix is an online DVD rental and streaming video service.
# Customers can receive movie rentals by mail, and they can also watch selected movies and TV shows online.
# Netflix has more than 40 million subscribers worldwide and has an annual revenue of $3.6 billion.

# From 2006 through 2009, Netflix ran a contest asking the public to submit algorithms to predict user ratings for movies.
# This algorithm would be useful for Netflix when making recommendations to users.
# Netflix provided a training data set of about 100 million user ratings and a test data set of about three million user ratings.
# They offered a grand prize of one million dollars to the team who could beat Netflix's current algorithm,
# called Cinematch, by more than 10% measured in terms of root mean squared error.

# The contest had a few rules.
# One was that if the grand prize was not yet reached, progress prizes of $50,000 per year
# would be awarded for the best result so far, as long as it was at least a 1% improvement over the previous year.
# Another rule was that teams must submit their code and a description of the algorithm to be awarded any prizes.
# And lastly, if a team met the 10% improvement goal, a last call would be issued, and 30 days
# would remain for all teams to submit their best algorithm.

# On June 26, 2009, the team BellKor's Pragmatic Chaos, composed of members from three different original teams,
# submitted a 10.05% improvement over Cinematch, signaling the last call for the contest.
# Other teams had 30 days to submit algorithms before the contest closed.
# These 30 days were filled with intense competition and even more progress.

# Recommendation Systems

# When predicting user ratings, what data could be useful?
# There are two main types of data that we could use.
# The first is that for every movie in Netflix's database, we have a ranking from all users who have ranked that movie.
# The second is that we know facts about the movie itself--
# the actors in the movie, the director, the genre classifications of the movie, the year it was released, et cetera.

# This technique of using other user's ratings to make predictions is called collaborative filtering.
# Note that we're not using any information about the movie itself here, just the similarity between users.

# Note that we're not using the ratings of other users at all here, just information about the movie.
# This technique is called content filtering. There are strengths and weaknesses to both types of recommendation systems.

# Collaborative filtering can accurately suggest complex items without understanding the nature of the items.
# It didn't matter at all that our items were movies in the collaborative filtering example.
# We were just comparing user ratings. However, this requires a lot of data about the user to make accurate recommendations.
# Also, when there are millions of items, it needs a lot of computing power to compute the user similarities.

# On the other hand, content filtering requires very little data to get started.
# But the major weakness of content filtering is that it can be limited in scope.
# You're only recommending similar things to what the user has already liked.
# So the recommendations are often not surprising or particularly insightful.

# Netflix actually uses what's called a hybrid recommendation system.
# They use both collaborative and content filtering.

# Movie Data and Clustering

# movielens.org is a movie recommendation website run by the GroupLens research lab at the University of Minnesota.
# They collect user preferences about movies and do collaborative filtering to make recommendations
# to users, based on the similarities between users.

# There are 18 different genres as well as an unknown category.

# Clustering is different from the other analytics methods we've covered so far.
# It's called an unsupervised learning method.
# This means that we're just trying to segment the data into similar groups, instead of trying to predict an outcome.
# A clustering algorithm does not predict anything. However, clustering can be used to improve predictive methods.
# You can cluster the data into similar groups and then build a predictive model for each group.

# In this class, we'll cover hierarchical clustering and K-means clustering.
# In this lecture, we'll discuss hierarchical clustering.

# Computing Distances

# So how does clustering work?
# The first step in clustering is to define the distance between two data points.
# The most popular way to compute the distance is what's called Euclidean distance.
# This is the standard way to compute distance that you might have seen before.

# In our movie lens dataset, we have binary vectors for each movie, classifying that movie into genres.
# The movie Toy Story is categorized as an animation, comedy, and children's movie.
# So the data for Toy Story has a 1 in the spot for these three genres and a 0 everywhere else.
# The movie Batman Forever is categorized as an action, adventure, comedy, and crime movie.
# So Batman Forever has a 1 in the spot for these four genres and a 0 everywhere else.

# We just discussed how to compute the distance between two individual points, but how do we compute
# the distance between groups of points?
# One way of doing this is by using what's called the minimum distance.
# This defines the distance between clusters as the distance between the two data points in the clusters that are closest together.

# Alternatively, we could use maximum distance.
# This one computes the distance between the two clusters as the distance between the two points that are the farthest apart.

# The most common distance metric between clusters is called centroid distance.
# And this is what we'll use. It defines the distance between clusters by computing the centroid of the clusters.
# The centroid is just the data point that takes the average of all data points in each component.
# This takes all data points in each cluster into account and can be thought of as the middle data point.

# When we are computing distances, it's highly influenced by the scale of the variables.
# To handle this, it's customary to normalize the data first. We can normalize by subtracting the mean of the data
# and dividing by the standard deviation.

# Hierarchical Clustering

# In hierarchical clustering, the clusters are formed by each data point starting in its own cluster.
# So at the end of hierarchical clustering, all of our data points are in a single cluster.
# The hierarchical cluster process can be displayed through what's called a dendrogram.
# The data points are listed along the bottom, and the lines show how the clusters were combined.
# The height of the lines represents how far apart the clusters were when they were combined.

# The easiest way to pick the number of clusters you want is to draw a horizontal line across the dendrogram.
# The number of vertical lines that line crosses is the number of clusters there will be.

# After selecting the number of clusters you want, you should analyze your clusters to see if they're meaningful.
# This can be done by looking at basic statistics in each cluster, like the mean, maximum, and minimum values
# in each cluster and each variable.
# You can also check to see if the clusters have a feature in common that was not used in the clustering, like an outcome variable.

# Getting the Data

# Our data is not a CSV file. It's a text file, where the entries are separated by a vertical bar.
# So we'll call our data set movies, and then we'll use the read.table function, where
# the first argument is the name of our data set in quotes.
# The second argument is header=FALSE. This is because our data doesn't have a header or a variable name row.
# And then the next argument is sep="|" , which can be found above the Enter key on your keyboard.
# We need one more argument, which is quote="\"".
movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"")
str(movies)

# We can add the column names manually by using the c() function
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)

# We won't be using the ID, ReleaseDate, VideoReleaseDate, or IMDB variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# And there are a few duplicate entries in our data set, so we'll go ahead and remove them with the unique function.
movies = unique(movies)
str(movies)

# Quick Question 6
table(movies$Comedy == 1)
table(movies$Western == 1)
table(movies$Romance == 1 & movies$Drama == 1)

# Hierarchical Clustering in R

# There are two steps to hierarchical clustering.
# First we have to compute the distances between all data points, and then we need to cluster the points.

distances = dist(movies[2:20], method="euclidean")
clusterMovies = hclust(distances, method = "ward.D")

# Now let's plot the dendrogram of our clustering algorithm by typing plot, and then in parentheses clusterMovies.
plot(clusterMovies)

# It looks like maybe three or four clusters would be a good choice according to the dendrogram, but let's keep our application in mind, too.
# We probably want more than two, three, or even four clusters of movies to make recommendations to users.
# It looks like there's a nice spot down here where there's 10 clusters. This is probably better for our application.

clusterGroups = cutree(clusterMovies, k = 10)

# So what does this do? It divides our data points into the 10 clusters and then computes the average value
# of the action variable for each cluster.
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

# Let's figure out what cluster Men in Black is in.
subset(movies, Title == "Men in Black (1997)")
clusterGroups[257]
# Men in Black went into Cluster 2 (Action-Adventure-SciFi)

cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title[1:10]

# Quick Question 7
clusterGroups2 = cutree(clusterMovies, k = 2)
spl = split(movies[2:20], clusterGroups2)
lapply(spl, colMeans)

# In this video, we explain how you can find the cluster centroids by using the function "tapply" for each variable in the dataset. 
# While this approach works and is familiar to us, it can be a little tedious when there are a lot of variables. 
# An alternative approach is to use the colMeans function. With this approach, you only have one command for each cluster instead of one command for each variable. 
# If you run the following command in your R console, you can get all of the column (variable) means for cluster 1:

# colMeans(subset(movies[2:20], clusterGroups == 1))

# You can repeat this for each cluster by changing the clusterGroups number. 
# However, if you also have a lot of clusters, this approach is not that much more efficient than just using the tapply function.

# A more advanced approach uses the "split" and "lapply" functions. 
# The following command will split the data into subsets based on the clusters:
  
# spl = split(movies[2:20], clusterGroups)

# Then you can use spl to access the different clusters, because

# spl[[1]]

# is the same as

# subset(movies[2:20], clusterGroups == 1)

# so colMeans(spl[[1]]) will output the centroid of cluster 1. 
# But an even easier approach uses the lapply function. The following command will output the cluster centroids for all clusters:
  
# lapply(spl, colMeans)

# The lapply function runs the second argument (colMeans) on each element of the first argument (each cluster subset in spl). 
# So instead of using 19 tapply commands, or 10 colMeans commands, we can output our centroids with just two commands: one to define spl, and then the lapply command.


# CONCLUSION
# So now let's go back to the Netflix prize.
# 29 days after last call was announced, on July 25, 2009, the team The Ensemble submitted a 10.09% improvement, beating
# the 10.05% improvement that was submitted by Bellkor's Pragmatic Chaos to signal last call.
# But by the time Netflix stopped accepting submissions the next day, Bellkor's Pragmatic Chaos had also submitted a 10.09% improvement, 
# and The Ensemble had submitted a 10.10% improvement.
# To really test the algorithms, Netflix tested them on a private test set that the teams had never seen before.
# This is the true test of predictive ability.
# On September 18, 2009, Netflix announced that the winning team was Bellkor's Pragmatic Chaos.
# They won the competition and the $1 million grand prize.
