# Unit 6: Clustering
# Recitation 6
# Seeing the Big Picture: Segmenting Images to Create Data (Recitation)

# Image Segmentation

# In this recitation, we will see how to apply clustering techniques to segment images,
# with the main application being geared towards medical image segmentation.

# Image segmentation is the process of partitioning digital images into regions, or segments,
# that share the same visual characteristics, such as color, intensity, or texture.
# The segments should also be meaningful, as in they should correspond to particular surfaces, objects, or even parts of an object.

# In few words, the goal of image segmentation is to modify the representation of an image from pixel data
# into something meaningful to us and easier to analyze.

# Various methods have been proposed to segment images.

# Clustering methods are used to group the points into clusters according to their characteristic features,
# for instance, intensity values.
# These clusters are then mapped back to the original spatial domain to produce a segmentation of the image.

# Another technique is edge detection, which is based on detecting discontinuities or boundaries.
# For instance, in a gray-scale image, a boundary would correspond to an abrupt change in the gray level.

# Instead of finding boundaries of regions in the image, there are other techniques called region growing methods, 
# which start dividing the image into small regions.
# Then, they sequentially merge these regions together if they are sufficiently similar.

# In this recitation, our focus is on clustering methods.
# In particular, we will review hierarchical and k-means clustering techniques and how to use them in R.

# Clustering Pixels

# Grayscale images are represented as a matrix of pixel intensity values that range from zero to one.
# The intensity value zero corresponds to the absence of color, or black, and the value one corresponds to white.
# For 8 bits per pixel images, we have 256 color levels ranging from zero to one.

# Grayscale image segmentation can be done by clustering pixels according to their intensity values.
# So we can think of our clustering algorithm as trying to divide the spectrum of intensity values from zero to one into intervals, or clusters.

# Our observations should be all of the 7 by 7 intensity values.
# Hence, we should have 49 observations. And we only have one variable, which is the pixel intensity value.
# So in other words, the input to the clustering algorithm should be a vector containing 49 elements, or intensity values.

# A crucial step before feeding the intensity values to the clustering algorithm is morphing our data.
# We should modify the matrix structure and lump all the intensity values into a single vector.
# We will see that we can do this in R using the as.vector function.
# Now, once we have the vector, we can simply feed it into the clustering algorithm and assign each element in the vector to a cluster.

flower = read.csv("flower.csv", header=FALSE)
str(flower)

flowerMatrix = as.matrix(flower)
str(flowerMatrix)

flowerVector = as.vector(flowerMatrix)
str(flowerVector)

flowerVector2 = as.vector(flower)
str(flowerVector2)

# So converting the data to a matrix and then to the vector is a crucial step.

# Create the distance matrix, which computes the difference between every two intensity values in our flower vector.
distance = dist(flowerVector, method="euclidean")

# Hierarchical Clustering

# As a reminder, the Ward’s method is a minimum variance method, which tries to find compact and spherical clusters.
# We can think about it as trying to minimize the variance within each cluster and the distance among clusters.
clusterIntensity = hclust(distance, method = "ward.D")

plot(clusterIntensity)

# We can actually visualize the cuts by plotting rectangles around the clusters on this tree.
rect.hclust(clusterIntensity, k=3, border="red")
flowerClusters = cutree(clusterIntensity, k=3)
flowerClusters

# To find the mean intensity value of each of our clusters, we can use the tapply function and ask R to group
# the values in the flower vector according to the flower clusters, and then apply the mean statistic to each of the groups.
tapply(flowerVector, flowerClusters, mean)

# To output an image, we can use the image function in R, which takes a matrix as an input.
# But the variable flowerClusters, as we just saw, is a vector.
dim(flowerClusters) = c(50, 50)
image(flowerClusters, axes=FALSE)

image(flowerClusters, axes=FALSE, col = grey(seq(0, 1, length=256)))

# We suggest that you not run the "dist" function the first time it comes up in the video if you are following along in R. 
# We apologize for the inconvenience.

# MRI Image

healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

# To see the MRI image, we can use the image function in R, which takes as an input the healthy matrix.
image(healthyMatrix, axes=FALSE, col = grey(seq(0, 1, length=256)))

healthyVector = as.vector(healthyMatrix)
str(healthyVector)

# The healthy vector has 365,636 elements.
# Let's call this number n. And remember, from our previous video, that for R to calculate the pairwise distances,
# it would actually need to calculate n*(n-1)/2 and then store them in the distance matrix.
# Let's see how big this number is.
n = 365636
n*(n-1)/2
# The bad news now is that we cannot use hierarchical clustering.

# K-Means Clustering

# We will try to segment the MRI image using the k-means clustering algorithm.

# The first step in k-means clustering involves specifying the number of clusters, k.
# The first input is whatever we are trying to cluster.
# The second argument is the number of clusters, and we can specify it using the argument centers, and that would be equal to k.
# Since the k-means is an iterative method that could take very long to converge, we need to set a maximum number of iterations.
k=5
set.seed(1)
KMC = kmeans(healthyVector, centers=k, iter.max=1000)
str(KMC)
# Output the segmented image by extracting it from KMC using dollar notation.
healthyClusters = KMC$cluster

# Mean intensity value of the second cluster
KMC$centers[2]

# Let us output the segmented image and see what we get.
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes=FALSE, col = rainbow(k))

# We see that k-means algorithm was able to segment the image in 5 different clusters.

### SCREE PLOTS ###
# To create the scree plot, the clustering algorithm is run with a range of values for the number of clusters. 
# For each number of clusters, the within-cluster sum of squares can easily be extracted when using k-means clustering. 
# For example, suppose that we want to cluster the MRI image from this video into two clusters. 
# We can first run the k-means algorithm with two clusters:

NumClusters = seq(2,10,1)
SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))
plot(NumClusters, SumWithinss, type="b")

# Detecting Tumors

tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# Now, we will not run the k-means algorithm again on the tumor vector.
# Instead, we will apply the k-means clustering results that we found using the healthy brain image on the tumor vector.
# In other words, we treat the healthy vector as training set and the tumor vector as a testing set.

install.packages("flexclust")
library(flexclust)

# The flexclust package contains the object class KCCA, which stands for K-Centroids Cluster Analysis.
# We need to convert the information from the clustering algorithm to an object of the class KCCA.
# And this conversion is needed before we can use the predict function on the test set tumorVector.

KMC.kcca = as.kcca(KMC, healthyVector)

# We can cluster the pixels in the tumorVector using the predict function.
tumorClusters = predict(KMC.kcca, newdata=tumorVector)

# To output the segmented image, we first need to convert the tumor clusters to a matrix.
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col = rainbow(k))

# The tumor can be identified as the green substance that was not present in the healthy scan.
# We do not see substantial green regions in the healthy brain image, apart from around the eyes.







