# Q: Perform clustering (Both hierarchical and K means clustering) for the airlines
# data to obtain optimum number of clusters. Draw the inferences from the 
# clusters obtained. 
EastWestAirlines<-read.csv("D:\\Data\\IN102385\\Downloads\\EastWestAirlines.csv")
summary(EastWestAirlines[,2:12])
cor(EastWestAirlines[,2:12])
pairs(EastWestAirlines[,2:12])
# Normalization of data for Hierarchical Clustering
mydata <- scale(EastWestAirlines[,2:12])
# Hierarchical Clustering Algorithm 
d <- dist(mydata, method = "euclidean") #Computing the distance natrix
fit <- hclust(d, method="average") # Building the algorithm 
plot(fit,hang=-1) # display dendrogram
groups<- cutree(fit, k=5) # cut tree into 5 clusters
membership<-as.matrix(groups)
final<-cbind(membership, EastWestAirlines)
View(aggregate(final[,-2], by=list(membership), FUN = mean))
# draw dendrogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
#Attach the cluster numbers to Airlines
clusters=data.frame('Airlines'=EastWestAirlines[,1],'Cluster' =clusters)
View(clusters)
# Hierarchical Cluster for large datasize like 3999 looks to be very complicated.
# K-means can give us correct clustering 

#K-Means Clustering
# Elbow method
install.packages("factoextra")
library(factoextra)
fviz_nbclust(EastWestAirlines[,-1],kmeans,method = "wss")+labs(subtitle = "Elbow method")
# k-means algorithm
km<-kmeans(EastWestAirlines[,-1],5)
km$centers
km$cluster
clust<-data.frame("Type"=EastWestAirlines[,1],"clusters"=km$cluster)
View(clust)
#Animation
install.packages("animation")
library(animation)
windows()
km<-kmeans.ani(EastWestAirlines[,2:12],5)

#Conclusion:
# K-means clustering gives well distinct 5 clusters which can be seen in animation
# K-Means found to be a better algorithm for large size of data