# Assignment 6
# Market Segmentation for Airlines

airlines = read.csv("AirlinesCluster.csv")
str(airlines)
summary(airlines)
# BonusTrans and FlightTrans have the smallest average values from the variables.
# Balance and BonusMiles have the largest average values from the variables.

# Normalize the data
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
str(airlinesNorm)
summary(airlinesNorm)
sd(airlinesNorm$Balance)
# FlightMiles has the largest maximum value and DaysSinceEnroll has the smallest minimum value.

# Hierarchical Clustering
distances = dist(airlinesNorm, method="euclidean")
airlinesHierClust = hclust(distances, method="ward.D")

# Plot the dendrogram
plot(airlinesHierClust)
# 6 is probably not a good choice for the number of clusters because it looks difficult to create.

# Divide the data points into 5 clusters
hierClusters = cutree(airlinesHierClust, k=5)
hierClusters1 = subset(airlinesNorm, hierClusters == 1)

# Now, use tapply to compare the average values in each of the variables for the 5 clusters (the centroids of the clusters). 
# You may want to compute the average values of the unnormalized data so that it is easier to interpret.
tapply(airlines$Balance, hierClusters, mean)
tapply(airlines$QualMiles, hierClusters, mean)
tapply(airlines$BonusMiles, hierClusters, mean)
tapply(airlines$BonusTrans, hierClusters, mean)
tapply(airlines$FlightMiles, hierClusters, mean)
tapply(airlines$FlightTrans, hierClusters, mean)
tapply(airlines$DaysSinceEnroll, hierClusters, mean)

# Cluster 1 mostly contains customers with few miles, but who have been with the airline the longest.
# Cluster 2 contains customers with a large amount of miles, mostly accumulated through flight transactions.
# Cluster 3 mostly contains customers with a lot of miles, and who have earned the miles mostly through bonus transactions.
# Cluster 4 customers have the smallest value in DaysSinceEnroll, but they are already accumulating a reasonable number of miles.
# Cluster 5 customers have lower than average values in all variables.

# K-Means Clustering
set.seed(88)
airlinesKMeans = kmeans(airlinesNorm, centers=5, iter.max=1000)
str(airlinesKMeans)

# There are two clusters with more than 1000 observations.

airlinesKMeans$centers

# The clusters are not displayed in a meaningful order, 
# so while there may be a cluster produced by the k-means algorithm that is similar to Cluster 1 produced by the Hierarchical method, 
# it will not necessarily be shown first.







