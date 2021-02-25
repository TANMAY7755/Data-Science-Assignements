#:::::::::::::::::::::::SUM1:::::::::::::::::::::::::::
#Import dataset
#Dataset "Salary_data.csv"
Salary_Test<- read.csv("D:/TanmayDataScience/assignments/16naive bayess/SalaryData_Test (1).csv")
View(Salary_Test)
Salary_Train<- read.csv("D:/TanmayDataScience/assignments/16naive bayess/SalaryData_Train (1).csv")
View(Salary_Train)
str(Salary_Train)
str(Salary_Test)

#LIbrary 
library(mlbench)
library(ggplot2)
library(e1071)

#converting into factors for train and test data
Salary_Train[sapply(Salary_Train, is.character)] <- lapply(Salary_Train[sapply(Salary_Train, is.character)], as.factor)
Salary_Test[sapply(Salary_Test, is.character)] <- lapply(Salary_Test[sapply(Salary_Test, is.character)], as.factor)

#Visualization with each variable
ggplot(data=Salary_Train,aes(x=Salary_Train$Salary, y = Salary_Train$age, fill = Salary_Train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
plot(Salary_Train$workclass,Salary_Train$Salary)
plot(Salary_Train$education,Salary_Train$Salary)
plot(Salary_Train$educationno,Salary_Train$Salary)
plot(Salary_Train$maritalstatus,Salary_Train$Salary)
plot(Salary_Train$occupation,Salary_Train$Salary)
plot(Salary_Train$relationship,Salary_Train$Salary)
plot(Salary_Train$race,Salary_Train$Salary)
plot(Salary_Train$sex,Salary_Train$Salary)
ggplot(data=Salary_Train,aes(x=Salary, y = capitalgain, fill =Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=Salary_Train,aes(x=Salary, y =hoursperweek, fill =Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
plot(Salary_Train$native,Salary_Train$Salary)
ggplot(data=Salary_Train,aes(x = age, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'black')

ggplot(data=Salary_Train,aes(x = workclass, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'black')

#Model 
model1<-naiveBayes(Salary~.,data=Salary_Train)
pred1<-predict(model, Salary_Train$Salary)
mean(pred1==Salary_Train$Salary)

#Accuracy = 75.10693

model2<-naiveBayes(Salary~.,data=Salary_Test)
pred2<-predict(model2,Salary_Test$Salary)
mean(pred2==Salary_Test$Salary)
#Accuracy = 75.43161

#Both test and train have same accuracy of 75% (consistent)
model<-naiveBayes(Salary_Train$Salary~.,data=Salary_Train[,-14])
pred<-predict(model,Salary_Test[,-14])
mean(pred == Salary_Test[,14]) #Accuracy = 81.93227
table(pred)
table(Salary_Test[,14])
confusionMatrix(pred,Salary_Test$Salary)
