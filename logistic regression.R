#:::::::::::::::::::::::::sum1::::::::::::::::::::::::::::
#import dataset.
bank.full <- read.csv(file.choose())
View(bank.full)

#calculating and omiting NA values.
sum(is.na(bank.full))
bank.full <- na.omit(bank.full)
dim(bank.full)

#shows columns names.
colnames(bank.full)

#linear model
mod_lm <- lm(yes_no~.,data=bank.full)# Preparing a linear regression 
summary(mod_lm)

#Glm model for logistic reg.
model1<- glm(y~.,data=bank.full,family = "binomial")

#To calculate the odds ratio manually.
exp(coef(model1))

#fitting of glm model.
fit<-fitted(model1)
exp(fit)

 #Confusion matrix table 
prob <- predict(model1,data = bank.full,type="response")
summary(model1)

#Confusion matrix table with threshold value as 0.5 
confusion<-table(prob>0.5,bank.full$y)
confusion

#Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
#Accuracy = 0.901838

#Creating empty vectors
pred_values <- NULL
yes_no <- NULL
# Creating new column to store the above values
bank.full[,"prob"] <- prob
bank.full[,"pred_values"] <- pred_values
bank.full[,"yes_no"] <- yes_no

table(bank.full$y,bank.full$pred_values)
# ROC Curve  
library(ROCR)
rocrp<-prediction(prob,bank.full$y)
rocrp<-performance(rocrp,'tpr','fpr')

str(rocrp)
#plot of ROC curve
plot(rocrp,colorize=T,text.adj=c())
##Getting cutt off or threshold value along with true positive and false positive rates in a data frame
str(rocrp)
rocr_cutoff1 <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff1) <- c("cut_off","FPR","TPR")
View(rocr_cutoff1)
 
library(dplyr)
rocr_cutoff1$cut_off <- round(rocr_cutoff1$cut_off,6)
rocr_cutoff1 <- arrange(rocr_cutoff1,desc(TPR))
View(rocr_cutoff1)

#RoC curve of this model has strong predictors. 
