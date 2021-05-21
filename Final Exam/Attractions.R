# Final Exam
# Understanding User Ratings

ratings = read.csv("ratings.csv")
str(ratings)
summary(ratings)

# Deal with the missing values
ratings = ratings[rowSums(is.na(ratings)) == 0, ]

summary(ratings)

# Preparing the Data
points = ratings[2:24]
str(points)

# Normalize the Data
library(caret)

preproc = preProcess(points)

pointsnorm = predict(preproc, points)

summary(pointsnorm)

# Clustering
distances = dist(pointsnorm, method = "euclidean")

dend = hclust(distances, method = "ward.D")

plot(dend, labels = FALSE)

# K-Means Clustering
RNGkind(sample.kind = "Rounding")
set.seed(100)

pointsKMC = kmeans(pointsnorm, centers = 4)
str(pointsKMC)
table(pointsKMC$cluster)


# Understanding the Clusters
Cluster1 = subset(pointsnorm, pointsKMC$cluster == 1)
Cluster2 = subset(pointsnorm, pointsKMC$cluster == 2)
Cluster3 = subset(pointsnorm, pointsKMC$cluster == 3)
Cluster4 = subset(pointsnorm, pointsKMC$cluster == 4)

sort(colMeans(Cluster1))
sort(colMeans(Cluster2))
sort(colMeans(Cluster3))
sort(colMeans(Cluster4))

# Which cluster has the user with the lowest average rating in restaurants?
# Cluster 4

# Which of the clusters is best described as "users who have mostly enjoyed churches, pools, gyms, bakeries, and cafes"?
# Cluster 1

# Which cluster seems to enjoy being outside, but does not enjoy as much going to the zoo or pool?
# Cluster 2




