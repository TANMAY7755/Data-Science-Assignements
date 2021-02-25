#:::::::::::::::::::::sum1:::::::::::::::::::;

#da5aset Salarydata.csv
#Import data
SalaryData_Train <-read.csv(file.choose())
View(SalaryData_Train)
SalaryData_Test <-read.csv(file.choose())
View(SalaryData_Test)
SalaryData_Train[['lettr']] = factor(SalaryData_Train[['lettr']])

#checking structure of the data
str(SalaryData_Train)
str(SalaryData_Test)

#converting chars to factors for further process
SalaryData_Train[sapply(SalaryData_Train, is.character)] <- lapply(SalaryData_Train[sapply(SalaryData_Train, is.character)], as.factor)
str(SalaryData_Train)#train data
SalaryData_Test[sapply(SalaryData_Test, is.character)] <- lapply(SalaryData_Test[sapply(SalaryData_Test, is.character)], as.factor)
str(SalaryData_Test)#test data

#invoking lib's
library(kernlab)
library(caret)

#vanilla dot
mod1<-ksvm(SalaryData_Train$Salary~., 
             data= SalaryData_Train, kernel = "vanilladot")
mod1

# kernel = rfdot 
mod_rfdot<-ksvm(Salary ~.,data = SalaryData_Train,kernel = "rbfdot")
pred_rfdot<-predict(mod_rfdot,newdata=SalaryData_Test)
mean(pred_rfdot==SalaryData_Test$Salary) # 82.98805

# kernel =vanilladot 
mod_vdot<-ksvm(Salary ~.,data = SalaryData_Train,kernel = "vanilladot")
pred_vdot<-predict(mod_vdot,newdata=SalaryData_Test)
mean(pred_vdot==SalaryData_Test$Salary) #81.2085

# kernel = polydot
mod_poly<-ksvm(Salary ~.,data = SalaryData_Train,kernel = "polydot")
pred_poly<-predict(mod_poly,newdata = SalaryData_Test)
mean(pred_poly==SalaryData_Test$Salary) #81.2085

# kernal = besseldot
mod_bessel<-ksvm(Salary ~.,data = SalaryData_Train,kernel = "besseldot")
pred_bessel<-predict(mod_bessel,newdata=SalaryData_Test)
mean(pred_bessel==SalaryData_Test$Salary) #83.02125

#Here besseldot model gives out more accuracy of 83.02%

#::::::::::::::::::::::::::::::sum2::::::::::::::::::::::::::::

#dataset forestfires.csv
#import data
forestfires <-read.csv(file.choose())
View(forestfires)

#checking structure of the data
str(forestfires)

#conversion of char to factors.
forestfires[sapply(forestfires, is.character)] <- lapply(forestfires[sapply(forestfires, is.character)], as.factor)
str(forestfires)

#splitting data into train data and test data
fires_train<-forestfires[1:410,]
fires_test<-forestfires[411:517,]

#invoke libraries
library(kernlab)
library(caret)

#Normalize the data
as.data.frame(sapply(forestfires, function(i) if(is.numeric(i)) scale(i) else i))

# kernel = rfdot 
model_rbf<-ksvm(size_category ~.,data = fires_train,kernel = "rbfdot")
pred_rbf<-predict(model_rbf,newdata=fires_test)
mean(pred_rbf==fires_test$size_category) #71.02804

# kernel =vanilladot 
model_vd<-ksvm(size_category ~.,data = fires_train,kernel = "vanilladot")
pred_vd<-predict(model_vd,newdata=fires_test)
mean(pred_vd==fires_test$size_category) #97.19626

# kernel = polydot
model_poly<-ksvm(size_category ~.,data = fires_train,kernel = "polydot")
pred_pd<-predict(model_poly,newdata = fires_test)
mean(pred_pd==fires_test$size_category) #97.19626

# kernal = besseldot
model_bd<-ksvm(size_category ~.,data = fires_train,kernel = "besseldot")
pred_bd<-predict(model_bd,newdata=fires_test)
mean(pred_bd==fires_test$size_category)#71.02804

#Here best fitted models are vanilladot and polydot which gives out same accuracy of 97.19%
