#:::::::::::::::::::::::SUM1:::::::::::::::::::::::::::
#Import dataset
#dataset company_data.csv
company<- read.csv(file.choose())
View(company)
str(company)

#convert the response var into categorical variable
company$Sales <- cut(company$Sales, c(0, 8, 17), 
                     labels=c("Low", "High"), include.lowest=T)
table(company$Sales)

#data partition
set.seed(222)
data<-sample(2,nrow(company),replace = TRUE , prob = c(0.8,0.2))
cd_train<-company[data==1,]
cd_test<-company[data==2,]

#model
library(randomForest)
set.seed(333)
rf_mod<-randomForest(Sales ~., data = cd_train,
                     mtry = 6,
                     importance = TRUE, 
                     proximity = TRUE)
plot(rf_mod)
print(rf_mod)

#prediction and accuracy(train)
library(caret)
pred<-predict(rf_mod,cd_train)#accuracy = 1 
confusionMatrix(pred, cd_train$Sales)

#prediction and accuracy (test)
pred1<-predict(rf_mod, newdata = cd_test)
confusionMatrix(pred1,cd_test$Sales) #accuracy = 87.69%

#tuning
tune<-tuneRF(cd_train[,-1],cd_train[,1],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 400,
       improve = 0.05)

# No. of nodes for the trees
hist(treesize(rf_mod),
     main = "No. of Nodes for the Trees",
     col = "green") #45 

# Variable Importance
varImpPlot(rf_mod,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf_mod)
varUsed(rf_mod)
MDSplot(rf_mod,cd_train$Sales)
#

#:::::::::::::::::::::::SUM2:::::::::::::::::::::::::::

##dataset (fraud_check)

#import
Fraud <- read.csv("D:/TanmayDataScience/assignments/15Random forest/Fraud_check.csv")
View(Fraud)
str(Fraud)

#converting Ranges to factors(levels)
summary(Fraud$Taxable.Income)
Fraud$Taxable.Income <- cut(Fraud$Taxable.Income , c(10003, 30000, 99619 ), 
                     labels=c("Risky", "Good"), include.lowest=T)

#data part
set.seed(222)
data<-sample(2, nrow(Fraud), replace = TRUE, prob = c(0.8, 0.2))
Fraud_train<- Fraud[data==1,]
Fraud_test<-Fraud[data==2,]

#model
library(randomForest)
set.seed(333)
model<-randomForest(Taxable.Income~., data = Fraud_train,
                    mtry = 1,
                    importance = TRUE, 
                    proximity = TRUE)
plot(model)
print(model) 

#prediction and accuracy(train)
library(caret)
pred1<-predict(model,Fraud_train)#accuracy = 78.95    
confusionMatrix(pred1, Fraud_train$Taxable.Income)

#prediction and accuracy (test)
pred2<-predict(model, newdata = Fraud_test)
confusionMatrix(pred2,Fraud_test$Taxable.Income) #accuracy = 81.13  

#tuning
tune1<-tuneRF(Fraud_train[,-3],Fraud_train[,3],
             stepFactor = 0.5,
             plot = TRUE,
             ntreeTry = 200,
             improve = 0.05)

# No. of nodes for the trees
hist(treesize(model),
     main = "No. of Nodes for the Trees",
     col = "blue")

# Variable Importance
varImpPlot(model,
           sort = T,
           n.var = 2,
           main = "Top 2 - Variable Importance")
importance(model)
varUsed(model)
MDSplot(model,Fraud_train$Taxable.Income)

#:::::::::::::::::::::::SUM3:::::::::::::::::::::::::::
#dataset(iris)
data("iris")
View(iris)
str(iris)

#data partition
set.seed(222)
data<-sample(2,nrow(iris),replace = TRUE , prob = c(0.8,0.2))
iris_train <-iris[data==1,]
iris_test<- iris[data==2,]

#Lib
library(randomForest)
mod1<-randomForest(Species~., data = iris_train,
                   mtry = 2,
                   importance = TRUE, 
                   proximity = TRUE)
plot(mod1)
print(mod1) 

#prediction and accuracy(train)
library(caret)
pred12<-predict(mod1,iris_train)#Accuracy : 1   
confusionMatrix(pred12, iris_train$Species)

#prediction and accuracy (test)
pred13<-predict(mod1, newdata = iris_test)
confusionMatrix(pred13,iris_test$Species) #Accuracy : 93.33 %

#tuning
tune1<-tuneRF(iris_train[,-4],iris_train[,4],
              stepFactor = 0.5,
              plot = TRUE,
              ntreeTry = 100,
              improve = 0.05)

# No. of nodes for the trees
hist(treesize(mod1),
     main = "No. of Nodes for the Trees",
     col = "red")

# Variable Importance
varImpPlot(mod1,
           sort = T,
           n.var = 3,
           main = "Top 3 - Variable Importance")
importance(mod1)
varUsed(mod1)
MDSplot(mod1,Fraud_train$Taxable.Income)
###








