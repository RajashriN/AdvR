##lifecycylesavings

data <- LifeCycleSavings

data.use <- LifeCycleSavings[,c(2:3)]
library(mclust)
fit <- Mclust(data.use)
plot(fit) # plot results 

# K-Means Clustering with 5 clusters
fit <- kmeans(data.use, 4)

# Cluster Plot against principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(data.use, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 
library(fpc)
plotcluster(data.use, fit$cluster)

##Validating clusters by elbow plot

##here we have elbow at k = 4 which suggestes we need to have 4 clusters


#Create a vector for storing the sse

data.frame(data.use)
sse=vector('numeric')
for(i in 2:15){
  #k-means function in R has a feature withinss which stores sse for each cluster group
  sse[i-1]=sum(kmeans(data[,2:3],centers=i)$withinss)
}
#Converting the sse to a data frame and storing corresponding value of k
sse=as.data.frame(sse)
sse$k=seq.int(2,15)
#Making the plot. This plot is also known as screeplot
plot(sse$k,sse$sse,type="b")
