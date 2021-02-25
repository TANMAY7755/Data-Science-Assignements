#::::::::::::::::::::::::::::::sum1::::::::::::::::::
#dataset zoo.csv
#Import data
glass <- read.csv(file.choose())
View(glass)
str(glass)

#replacing numbers with given data 
glass$Type[glass$Type == 1] <- 'building_windows_F'
glass$Type[glass$Type == 2] <- 'building_windows_NF'
glass$Type[glass$Type == 3] <- 'vehicle_windows_F'
glass$Type[glass$Type == 4] <- 'vehicle_windows_NF' #zero
glass$Type[glass$Type == 5] <- 'containers'
glass$Type[glass$Type == 6] <- 'tableware'
glass$Type[glass$Type == 7] <- 'headlamps'
glass$Type <- factor(glass$Type)#into factors for further process

#Normalize 
normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x))) }

glass_norm<-as.data.frame(lapply(glass[,1:9], normalize))

head(glass_norm)

#Data partition
set.seed(123)
data<-sample(1:nrow(glass_norm),size = nrow(glass_norm)*0.7,replace = FALSE)
glass_train<-glass_norm[data,]
glass_test<-glass_norm[-data,]

#creating labels for y variable
glass_train_label<-glass_norm[data,9]
glass_test_label<-glass_norm[-data,9]

#lib
library(class)

NROW(glass_train_label) #no. of obs (then  take sq. root of the  number )

knn_12<-knn(train = glass_train,test = glass_test, cl = glass_train_label, k = 12)
knn_13<-knn(train = glass_train,test = glass_test, cl = glass_train_label, k = 13)

#accuracy
acc_12<-100*sum(glass_test_label == knn_12)/NROW(glass_test_label)#75.38462
acc_12
acc_13<-100*sum(glass_test_label == knn_13)/NROW(glass_test_label)#75.38462
acc_13

table(knn_12,glass_test_label)
knn_12

table(knn_13,glass_test_label)
knn_13

##ConfusionMatrix

library(caret)
confusionMatrix(table(knn_12,glass_test_label))

i=1
k.optm=1
for(i in 1:28){
  knn.mod<-knn(train = glass_train,test = glass_test,cl=glass_train_label, k = i)
  k.optm[i]<-100*sum(glass_test_label == knn.mod)/NROW(glass_test_label)
  k=i
  cat(k,'=',k.optm[i],'\n')
}

plot(k.optm,type='b',xlab="k-val",ylab="Accuracy")

#k = 26 is the optimum number of k 


#::::::::::::::::::::::::::::::sum2::::::::::::::::::

#dataset Zoo.csv
#Import data
zoo <- read.csv(file.choose())
zoo <- zoo[-1]
View(zoo)

zoo$type <- factor(zoo$type)#converting the y var. in factor format
str(zoo)

#Data partition
set.seed(123)
data1<-sample(1:nrow(zoo),size = nrow(zoo)*0.7,replace = FALSE)
zoo_train<-zoo[data1,]
zoo_test<-zoo[-data1,]

#creating labels for y variable
zoo_train_label<-zoo[data1,17]
zoo_test_label<-zoo[-data1,17]

#lib
library(class)

NROW(zoo_train_label) #no. of obs

knn_5<-knn(train = zoo_train,test = zoo_test, cl = zoo_train_label, k = 5)#k = 5
knn_8<-knn(train = zoo_train,test = zoo_test, cl = zoo_train_label, k = 8)#k = 8
knn_3<-knn(train = zoo_train,test = zoo_test, cl = zoo_train_label, k = 3)#k = 3

#Accuracy
acc_5<-100*sum(zoo_test_label == knn_5)/NROW(zoo_test_label)
acc_5  #87.09677
acc_8<-100*sum(zoo_test_label == knn_8)/NROW(zoo_test_label)
acc_8  #83.87097 accuracy
acc_3<-100*sum(zoo_test_label == knn_3)/NROW(zoo_test_label)
acc_3

# for k = 5
table(knn_5,zoo_test_label)

# for k = 8
table(knn_8,zoo_test_label)

# for k = 3
table(knn_3,zoo_test_label)


#ConfusionMAtrix
library(caret)
confusionMatrix(table(knn_5,zoo_test_label)) #accuracy check

i=1
k.optm=1
for(i in 1:28){
  knn.mod<-knn(train = zoo_train,test = zoo_test,cl=zoo_train_label, k = i)
  k.optm[i]<-100*sum(zoo_test_label == knn.mod)/NROW(zoo_test_label)
  k=i
  cat(k,'=',k.optm[i],'\n')     
}                   #list of k values with accuracies

plot(k.optm,type='b',xlab="k-val",ylab="Accuracy")

#k = 5 is the optimum number of k which gives an accuracy of 87%
















