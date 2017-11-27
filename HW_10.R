########################################################################################
# HW 10 Question 7
########################################################################################

# Show (1 - correlation) distance matrix and squared Euclidean distance matrix are proportional

set.seed(1)
library(ISLR)
USArrests <- data.frame(USArrests)

sd.data <- scale(USArrests)
data.dist <- dist(sd.data)^2
data.cor <- as.dist(1-cor(t(sd.data)))
summary(data.cor/data.dist)

########################################################################################
# HW 10 Question 8
########################################################################################

# Calculate PVE via sdev and prcomp()

pr.out <- prcomp(USArrests, scale = T)
pve <- pr.out$sdev^2/sum(pr.out$sdev^2)
pve

loadings = pr.out$rotation
pve2 = rep(NA, 4)
dmean = apply(USArrests, 2, mean)
dsdev = sqrt(apply(USArrests, 2, var))
dsc = sweep(USArrests, MARGIN=2, dmean, "-")
dsc = sweep(dsc, MARGIN=2, dsdev, "/")
for (i in 1:4) {
    proto_x = sweep(dsc, MARGIN=2, loadings[,i], "*")
    pc_x = apply(proto_x, 1, sum)
    pve2[i] = sum(pc_x^2)
}
pve2 = pve2/sum(dsc^2)
pve2

########################################################################################
# HW 10 Question 9
########################################################################################

set.seed(1)
library(ISLR)
USArrests <- data.frame(USArrests)

data.dist <- dist(USArrests)

hclust.complete <- hclust(data.dist)
plot(hclust.complete)

cutree(hclust.complete, 3)
table(cutree(hclust.complete, 3))

sd.data <- scale(USArrests)
data.dist <- dist(sd.data)

sd.hclust.complete <- hclust(data.dist)
plot(sd.hclust.complete)

cutree(sd.hclust.complete, 3)
table(cutree(sd.hclust.complete, 3))

table(cutree(sd.hclust.complete,3), cutree(hclust.complete,3))

########################################################################################
# HW 10 Question 10
########################################################################################

set.seed(2)
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1

pca.out <- prcomp(x)
pca.out

pca.out$x[,1:2]
plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19)

km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

km.out = kmeans(x, 2, nstart=20)
km.out$cluster

km.out = kmeans(x, 4, nstart=20)
km.out$cluster

km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

km.out = kmeans(scale(x), 3, nstart=20)
km.out$cluster

########################################################################################
# HW 10 Question 11
########################################################################################

data = read.csv("./Ch10Ex11.csv", header=F)
dim(data)

dd = as.dist(1 - cor(data))

plot(hclust(dd, method="complete"))
plot(hclust(dd, method="single"))
plot(hclust(dd, method="average"))

pr.out = prcomp(t(data))
summary(pr.out)

total_load = apply(pr.out$rotation, 1, sum)
indices = order(abs(total_load), decreasing=T)
indices[1:10]

total_load[indices[1:10]]




