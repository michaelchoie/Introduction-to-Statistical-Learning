########################################################################################
# LAB 10.4 Principal Component Analysis
########################################################################################

USArrests <- data.frame(USArrests)
summary(USArrests)
apply(USArrests, 2, var)

# Must standardize variables otherwise variable with highest mean/variance skews results
# Use prcomp() to perform PCA
# By default, makes mean = 0, but scale = T makes SD = 1

pr.out <- prcomp(USArrests, scale = T)

# center = mean prior to standardization
# scale = SD prior to standardization
# rotation = principal component loadings matrix (called rotation because if you multiply X by rotation matrix,
#            you get the coordinates of the rotated coordinate system)
# x = matrix with principal component scores as columns

pr.out
dim(pr.out$x)

# scale = 0 ensures arrows are scaled to represent the loadings
# sdev = standard deviation of principal components
# use sdev to get proportion of variance explained
# cumsum() gives cumulative sum of the elements a numeric vector

biplot(pr.out, scale = 0)
pr.out$rotation <- -pr.out$rotation
pr.out$x <- pr.out$x
biplot(pr.out, scale = 0)

pr.out$sdev
pr.var <- pr.out$sdev^2
pve <- pr.var / sum(pr.var)
pve

plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b", pch = 15)
lines(cumsum(pve), type = "b", pch = 15, col = "blue", lty = 2)
legend(2.0, 0.8, legend = c("Proportion of Variance Explained", "Cumulative Proportion of Variance Explained"),
       col = c("black", "blue"), lty = 1:2, cex = 0.8)

########################################################################################
# LAB 10.5.1 K-Means Clustering
########################################################################################

# Create sample data with distinct 2 clusters

set.seed(3)
x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

# Use kmeans() to perform the clustering algorithm
# cluster assignments stored in km.out$cluster
# nstart = how many random assignment sets that K-means performs on (recommended 20 or 50)
# tot.withinss = total within cluster sum of squares
# withinss = individual within cluster sum of squares

km.out <- kmeans(x, 2, nstart = 20)
km.out$cluster

plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K = 2",
     xlab = "", ylab = "", pch = 20, cex = 2)

set.seed(2)
km.out <- kmeans(x, 3, nstart = 20)
km.out

set.seed(4)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss

########################################################################################
# LAB 10.5.2 Hierarchical Clustering
########################################################################################

# hclust() performs hierarchical clustering algo
# dist() computes inter-observation Euclidean distance matrix
# method = "linkage" [i.e complete, average, single]
# height represents of fusion represents how different two observations are

hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

par(mfrow = c(1,3))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = .9)

# cutree() to see cluster labels given a cut of the dendrogram

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

# scale() to scale features prior to clustering

xsc <- scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features")

# as.dist() to convert square symmetric matrix into form hclust() recognizes as distance matrix (for correlation distance)
# data must have >= 3 features since absolute correlation b/w any 2 observations w/ measurements on 2 features = 1
#   (i.e, two points on a plane, is there linear relationship? (can there be a line drawn to connect them?)
#         ofc! thus correlation = +1/-1))
# t() = transpose

x <- matrix(rnorm(30 * 3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")

########################################################################################
# LAB 10.6.1 PCA on NCI60 Data
########################################################################################

library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)
nci.labs[1:4]
table(nci.labs)

pr.out <- prcomp(nci.data, scale = T)

# rainbow() creates vector with continguous colors

Cols <- function(vec) {
    col <- rainbow(length(unique(vec)))
    return(col[as.numeric(as.factor(vec))])
}

# Plot points of principal components colored by cancer types

par(mfrow = c(1,2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1,3)], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")

summary(pr.out)

# Plot proportion of variance explained

plot(pr.out)

# Better to plot proportion of variance and cumulative proportion of variance for each principal component
# This is called a skree plot
# Elbow at around 7th principal component - suggests little benefit examining more than that

pve <- pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1,2))

plot(pve, type = "o", ylab = "PVE", xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component", col = "brown3")

# PVE explained directly via summary(pr.out)$importance[2,]
# Cumulative PVE explained via summary(pr.out$importance[3,])

########################################################################################
# LAB 10.6.2 Clustering on Observations of NCI60 Data
########################################################################################

sd.data <- scale(nci.data)

par(mfrow = c(1,3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "average"), main = "Average Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"), main = "Single Linkage", xlab = "", sub = "", ylab = "")

hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

par(mfrow = c(1,1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

# How does hierarchical clustering compare to k-means when K=4?

set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)

# Perform hierarchical clustering only on first few principal components

hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs, main = "Hierarchical Clustering on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)
