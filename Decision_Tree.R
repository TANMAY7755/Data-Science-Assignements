#::::::::::::::::::::::::::::::sum1::::::::::::::::::
#import dataset
#Dataset company_data.csv
company <- read.csv(file.choose())
View(company)
str(company)

#CUT function to cut the $scale in categorical variables.
company$Sales <- cut(company$Sales, c(0, 8, 17), 
                    labels=c("Low", "High"), include.lowest=T)
company$Sales<-factor(company$Sales)# convert into factor 

#data partition
set.seed(222)
data<- sample(2, nrow(company), replace = TRUE, prob = c(0.8, 0.2))
comp_train <-company [data==1,]
comp_validate<- company [data==2,]

#model
library(C50)
model_2 <-C5.0(comp_train[,-1],comp_train$Sales)
windows()
plot(model_2)

#Prediction and accuracy
pred_train <- predict(model_2,comp_train)
mean(comp_train$Sales==pred_train) #94.029% Accuracy

#confusionMatrix
library(caret)
confusionMatrix(pred_train,comp_train$Sales)

# predicting on test data
pred_test <- predict(model_2,newdata=comp_validate) # predicting on test data
mean(pred_test==comp_validate$Sales) #83.076 Accuracy
confusionMatrix(pred_test,comp_validate$Sales)

#crossTAB
library(gmodels)
CrossTable(comp_validate$Sales,pred_test)

## Using tree function 
library(tree)

# Building a model on training data 
comp_tree <- tree(Sales~.,data=comp_train)
plot(comp_tree)
text(comp_tree,pretty = 0)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(comp_tree,newdata=comp_validate))
pred_tree["final"] <- NULL
pred_test_df <- predict(comp_tree,newdata=comp_validate)

pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
cv.tree

mean(pred_tree$final==comp_validate$Sales) # Accuracy = 67.69%
CrossTable(comp_validate$Sales,pred_tree$final)

#CrossValidate and pruning
cv.company<-cv.tree(comp_tree, FUN = prune.misclass)
cv.company
plot(cv.company)
prune.company<-prune.misclass(comp_tree, best = 12)
plot(prune.company)
text(prune.company, pretty=0)
#

#::::::::::::::::::::::::::::::sum2::::::::::::::::::
#
#dataset fraud_check.csv
#Import
fraud<-read.csv(file.choose())
View(fraud)
str(fraud)
summary(fraud$Taxable.Income)

#CUT function to cut the $scale in categorical variables.
fraud$Taxable.Income <- cut(fraud$Taxable.Income, c(10003, 30000, 99619 ), 
                            labels=c( "Risky","Good"), include.lowest=T)

# convert into factor 
fraud[sapply(fraud, is.character)] <- lapply(fraud[sapply(fraud, is.character)], as.numeric)

#normalize
fraud$City.Population<-scale(fraud$City.Population)

#Data partition
set.seed(222)
data<- sample(2, nrow(fraud), replace = TRUE, prob = c(0.8, 0.2))
fraud_train <-fraud[data==1,]
fraud_validate<- fraud[data==2,]

#model 
library(C50)
mod_1 <-C5.0(fraud_train [,-3],fraud_train$Taxable.Income)
windows()
plot(mod_1)

#Prediction and accuracy
pred_train1 <- predict(mod_1,fraud_train)
mean(fraud$Taxable.Income==pred_train1) #79.33% Accuracy
library(caret)
confusionMatrix(pred_train1, fraud_train$Taxable.Income)

# predicting on test data
pred_test1 <- predict(mod_1,newdata=fraud_validate) # predicting on test data
mean(pred_test1==fraud_validate$Taxable.Income) #81.13 Accuracy
confusionMatrix(pred_test1,fraud_validate$Taxable.Income)

#crossTAb
library(gmodels)
CrossTable(fraud_validate$Taxable.Income,pred_test1)

#### Using tree function 
library(tree)
# Building a model on training data 
fraud_tree<-tree(Taxable.Income ~.,data=fraud_train,control = tree.control(nobs = 494, mincut = 1, minsize = 2,mindev = 0.00))
plot(fraud_tree)
text(fraud_tree,pretty = 0)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(fraud_tree,newdata=fraud_validate))
pred_tree["final"] <- NULL
pred_test_df <- predict(fraud_tree,newdata=fraud_validate)

pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

#Accuracy
mean(pred_tree$final==fraud_validate$Taxable.Income) # Accuracy = 67.69%
CrossTable(fraud_validate$Taxable.Income,pred_tree$final)

#pruning the tree 
prune.fraud<-prune.misclass(fraud_tree, best = 12)
plot(prune.fraud)
text(prune.fraud, pretty=0)
#

#:::::::::::::::::::::::::::::SUM3::::::::::::::::::

#
#dataset iris
#iris using party package
data("iris")
View(iris)
str(iris)

#data partition
set.seed(222)
data<-sample(2,nrow(iris),replace = TRUE , prob = c(0.8,0.2))
iris_train <-iris[data==1,]
iris_validate<- iris[data==2,]

#Lib
library(party)

#model
iris_tree<-ctree(Species ~.,iris_train)
plot(iris_tree)

#predict
pred_iris<-predict(iris_tree,iris_train)
pred_iris
tab<-table(pred_iris,iris_train$Species)
tab

#Accuracy
sum(diag(tab)/sum(tab))#96.67 accuracy

#test
pred_iris1<-predict(iris_tree,iris_validate)
pred_iris1
tab1<-table(pred_iris1,iris_validate$Species)
tab1

#Accuracy
sum(diag(tab1)/sum(tab1))#93.34 accuracy






