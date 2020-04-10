# k-manes
install.packages("cluster")
library(cluster)

zooDF <- read.csv("C:/Users/Joyce/Desktop/Syracuse/IST707/Classwork/week4/zoo.csv")
str(zooDF)

# unlabel zoo data
unlabel_zooDF <- zooDF[, c(2:17)]
str(unlabel_zooDF)

# Weka
library(RWeka)
model_rweka <- SimpleKMeans (unlabel_zooDF, control=Weka_control(N=7,I=500,S=100))
model_rweka


# now do with R
model_r <- kmeans(unlabel_zooDF, 7)
model_r

# print the centroids
model_r$centers
cluster_assignment <- data.frame(zooDF, model_r$cluster)
View(cluster_assignment)
cluster_assignment

# visualize
plot(zooDF$type ~ jitter(model_r$cluster, 1),
     pch=21, col=as.factor(zooDF$milk))

clusplot(unlabel_zooDF, model_r$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


d = dist(as.matrix(unlabel_zooDF))
hc=hclust(d)
plot(hc)

# on iris data
irisDF <- read.csv("C:/Users/Joyce/Desktop/Syracuse/IST707/Classwork/week4/iris.csv")
str(irisDF)

d = dist(as.matrix(irisDF),method="euclidean")
hc=hclust(d)
plot(hc)

fit <- hclust(d, method="ward.D2")
rect.hclust(fit, k=4)
plot(fit)
