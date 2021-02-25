
#-In PCA your analysis was good,
#As we mentioned in the statement , 
#you need to do clustering on the whole data set and as well as on the pca scores data and 
#compare whether the number of clusters formed are the same or not 
#in both cases,so please resend it.

#:::::::::::::::::::::sum1::::::::::::::::::::::::::
#import data
wine <- read.csv("D:/TanmayDataScience/assignments/9PCA/wine (1).csv")
View(wine)
str(wine)
#deleting the first column which has ID's
View(wine[-1]) 
wine <- wine[,-1]
#attach(wine1)
cor(wine)

#applying princomp() for pca
pca<-princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)
str(pca)
summary(pca)
loadings(pca)##factor analysis


scores<- pca$scores[,1:7]##Top 7 scores shows about 89% of whole data

#ploting pca
plot(pca)#comp1 have highest variance 
biplot(pca)

##cumsum plot with abline.
plot(cumsum(pca$sdev*pca$sdev)*100/(sum(pca$sdev*pca$sdev)),type="b")
abline(v = 7, col="magenta", lty=5)
abline(h = 89.336795, col="cyan", lty=5)

#selecting scores
pca$scores[,1:7] 

#binding scores with original data
wine<-cbind(wine,pca$scores[,1:7])
View(wine)

#performing clustering on scores.
clust<-wine[,15:21] 

##normalization 
normalize<-scale(clust)

#distance 
d<-dist(normalize,method = "euclidean") #Using euclidean distance method

#performing heirarchical clustering 
fit<-hclust(d,method="complete") #using complete linkage
plot(fit)
plot(fit,hang = -1)

#screeplot 
screeplot(pca)
screeplot(pca, type = "l", npcs = 15, main = "Screeplot of the PCa") #here scree plot suggests 5 clusters

# Cutting the dendogram for 4 clusters
group<-cutree(fit,4) 

#Membership
membership<-as.matrix(group) 
View(membership)

##binding the membership with the original data
final<-cbind(membership,wine)
View(final)

#finalstep
View(aggregate(final[,-c(2,15:21)],by=list(membership),FUN=mean))

###
##
#optimal number of k is 4 for PCA
##
###

######Performing clustering on the same dataset(wine.csv)

#Normalizing the whole data data
normalize<-scale(wine[,1:13])

#calculate distance matrix(default is Euclidean distance)
distance<-dist(normalize,method = "euclidean")
distance
print(distance,digits = 3) #printing 1st 3 digits

##Scree Plot (elbow plot)
library(factoextra)
fviz_nbclust(normalize, hcut, method = "wss")##optimal number is 3 using Scree Plot

#Hierarchical agglomerative clustering using default "complete" linkage
hc.c<-hclust(distance,method="complete")
plot(hc.c)

plot(hc.c, hang = -1)
rect.hclust(hc.c, k=3, border="red")

#Cluster membership
member.c<-cutree(hc.c,3)

table(member.c)

#Characterizing clusters
aggregate(normalize,list(member.c),mean)
aggregate(wine[,-c(1,1)],list(member.c),mean)

#Scree Plot (elbow plot)
library(factoextra)
fviz_nbclust(normalize, kmeans, method = "wss")#optimal number is 3 using Scree Plot

#K-means clustering
kc<-kmeans(normalize,3) #with 3clusters(k=3)
kc

#final values

final <- kmeans(normalize, 3)
print(final)

##here, 3 are the optimal numbers of clusters,

# The Optimal numbers of k in PCA is 4 by scree plot.
# The Optimal numbers of k in clustering is 3 by scree plot.


