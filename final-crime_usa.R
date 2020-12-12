# Q: Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.

# Loading the data
crime_data <- read.csv("D:\\Dataset-Clustering\\crime_data.csv")
cor(crime_data[,2:5])
pairs(crime_data[,2:5])

# Normalisation of data
crime_usa <- scale(crime_data[,2:5])
summary(crime_usa)

# Hierarchical Clustering 
d <- dist(crime_usa, method = "euclidean") #Computing the distance matrix
fit <- hclust(d, method="centroid") # Building the algorithm
# Tried with all the methods like Complete, Average and Centroid, found centroid is suitable
plot(fit,hang=-1) # display dendrogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
membership<-as.matrix(groups)
View(membership)
final<-cbind(membership,crime_data[,2:5])
View(aggregate(final,by=list(membership),FUN=mean))
# draw dendrogram with red borders around the 3 clusters 
rect.hclust(fit, k=3, border="red")
#Attach the cluster numbers to Area_usa
clusters=data.frame('Area_Usa'=crime_data[,1],'Cluster' =groups)
View(clusters)
# K-Means Clustering
# Elbow method
install.packages('factoextra')
library(factoextra)
fviz_nbclust(crime_data[,-1], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
# K-Means Algorithm
km<-kmeans(crime_data[,-1],3)
km$centers
km$cluster
clust <- data.frame ("Type"=crime_data[,1],"cluster"= km$cluster)
View(clust)
##Animation
install.packages("animation")
library(animation)
windows()
km <- kmeans.ani(crime_data[,-1], 3)
# Conclusion : Three clusters are well distinguished.
     