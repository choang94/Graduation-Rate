#Clustering
install.packages("ISLR")
library(ISLR)
data = College
str(data)
#remove categorical variable
data = data[,-1]

sum(is.na(data))
#standardize data to mean 0 and sd = 1
scale.data = scale(data)
summary(scale.data)

#k-means clustering
set.seed(1)
km = kmeans(scale.data,2,nstart = 10)
km$cluster #cluster assignments
km$withinss #WSS for each cluster
km$tot.withinss #total WSS summed across all clusters
#total WSS summed across all clusters is 10456.06, which is very high
km$totss #total sum of square for the entire dataset

clust.data = cbind(cluster = km$cluster, data)

head(clust.data)
aggregate(.~cluster, data = clust.data,FUN = mean)
#group 1 has less application, acceptance and lower graduation rate and tuition
#group 2, on the other hand, has much higher amount of apps, acceptance, gradrate
clust.data[clust.data$cluster == 1,]
clust.data[clust.data$cluster ==2,]

sum(clust.data$cluster == 1)
sum(clust.data$cluster == 2)
#Hierarchical clustering
#calculate distance matrix from standardized data
hc.dist = dist(scale.data)
#cluster using the "complete"linkage method
hc = hclust(hc.dist,method = "complete")
hc
plot(hc)
#2 clusters 
hc.cut = cutree(hc,2)
hc.cut

hc.data = cbind(cluster = hc.cut,data)
aggregate(.~cluster,data = hc.data, FUN = mean)
#group 1 has much lower amount of apps, accept, enroll, but higher outstate tuition
#and grad.rate is not much lower than group 2, which is a bit different from kmeans
#group 2 has much higher amount of apps, accept and enroll but lower outstate 
#tuition. Compared to Kmeans, hc is only a bit different but overall the 2 clusters 
#are similar. 

hc.data["Boston University",]
#Boston University is group 2
sum(hc.data$cluster == 1)
sum(hc.data$cluster == 2)
