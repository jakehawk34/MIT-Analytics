# Assignment 6
# Document Clustering with Daily Kos

# Build a hierarchical clustering model
dailykos = read.csv("dailykos.csv")
str(dailykos)

distance = dist(dailykos, method="euclidean")

kosHierClust = hclust(distance, method = "ward.D")

# 2 and 3 appear to be good cluster choices because there is a lot of space between the horizontal lines.
plot(kosHierClust)

# Thinking about the application, it is probably better to show the reader more categories than 2 or 3. 
# These categories would probably be too broad to be useful. 
# Seven or eight categories seems more reasonable.

# Let's pick 7 clusters. This number is reasonable according to the dendrogram, and also seems reasonable for the application. 
# Use the cutree function to split your data into 7 clusters.
hierClusters = cutree(kosHierClust, k=7)
hierClusters

hierCluster1 = subset(dailykos, hierClusters == 1)
hierCluster2 = subset(dailykos, hierClusters == 2)
hierCluster3 = subset(dailykos, hierClusters == 3)
hierCluster4 = subset(dailykos, hierClusters == 4)
hierCluster5 = subset(dailykos, hierClusters == 5)
hierCluster6 = subset(dailykos, hierClusters == 6)
hierCluster7 = subset(dailykos, hierClusters == 7)

# This computes the mean frequency values of each of the words in cluster 1, 
# and then outputs the 6 words that occur the most frequently.
tail(sort(colMeans(hierCluster1)))

# Cluster 2 has november, poll, vote, challenge, bush, democrat in its tail
tail(sort(colMeans(hierCluster2)))
tail(sort(colMeans(hierCluster3)))
tail(sort(colMeans(hierCluster4)))
# Cluster 5 is the cluster most related to the war in Iraq
tail(sort(colMeans(hierCluster5)))
tail(sort(colMeans(hierCluster6)))
# Cluster 7 is the cluster most related to the Democratic Party
tail(sort(colMeans(hierCluster7)))

# Now, run k-means clustering, setting the seed to 1000 right before you run the kmeans function. 
# Again, pick the number of clusters equal to 7. You don't need to add the iters.max argument.
RNGkind(sample.kind = "Rounding")
set.seed(1000)
kosKMC = kmeans(dailykos, centers=7)
str(kosKMC)
table(kosKMC$cluster)

KMeansCluster1 = subset(dailykos, kosKMC$cluster == 1)
KMeansCluster2 = subset(dailykos, kosKMC$cluster == 2)
KMeansCluster3 = subset(dailykos, kosKMC$cluster == 3)
KMeansCluster4 = subset(dailykos, kosKMC$cluster == 4)
KMeansCluster5 = subset(dailykos, kosKMC$cluster == 5)
KMeansCluster6 = subset(dailykos, kosKMC$cluster == 6)
KMeansCluster7 = subset(dailykos, kosKMC$cluster == 7)

# Now, output the six most frequent words in each cluster, like we did in the previous problem, for each of the k-means clusters.
tail(sort(colMeans(KMeansCluster1)))
# Cluster 2 best corresponds to the Democratic Party
tail(sort(colMeans(KMeansCluster2)))
# Cluster 3 corresponds to the war in Iraq the best
tail(sort(colMeans(KMeansCluster3)))
tail(sort(colMeans(KMeansCluster4)))
tail(sort(colMeans(KMeansCluster5)))
tail(sort(colMeans(KMeansCluster6)))
tail(sort(colMeans(KMeansCluster7)))

# Use the table function to compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering.
table(hierClusters, kosKMC$cluster)

# 116 (80.6%) of the observations that fell in K-Means Cluster 2 are also in Hierarchical Cluster 7. 

# 171 (61.7%) of the observations that fell in K-Means Cluster 3 are also in Hierarchical Cluster 5.

# No more than 123 (39.9%) of the observations that fell in K-Means Cluster 7 fall in any hierarchical clusters.

# 320 (97.3%) of the observations that fell in K-Means Cluster 6 are also in Hierarchical Cluster 2. 



