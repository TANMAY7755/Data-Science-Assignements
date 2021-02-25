#::::::::::::::::::sum1::::::::::::::::::

#Import dataset
Salary_Data <- read.csv(file.choose())
View(Salary_Data)
attach(Salary_Data)
#Plot
plot(Salary_Data, main = "scatterplot")
#Correlation
cor(YearsExperience , Salary)
#0.9782416
#Simple Regression
reg <-lm(Salary ~ YearsExperience, Salary_Data)
summary(reg)
#R-sq = 0.957
#Confidence Interval for 95%
confint(reg, level = 0.95)
#Prediction of the given dataset
predict(reg,interval="prediction")
#Prediction for the new vlaues 
new.yearexp<- data.frame(YearsExperience = c(12 ,13.5, 20))
predict(reg, newdata = new.yearexp, interval = "prediction")

# Mathematical  formula for Prediction for hike
##Salary_Hike = (Intercept) + (YearsExperience)*X1
#for prediction assume any value for x1
#Salary_Hike = (25792.2) + (9450)*X1
##Salary_Hike = (25792.2) + (9450)*20 = 214,792

#Here, YearsExperience is more significant variable.
#Mul R_squared is 0.957 which is greater than 0.8, so the model built is very good 
#and has a strong correlation.


#::::::::::::::::::::::::::::;sum2;:::::::::::::::::::::::::::::::
  
#Import dataset
delivery_time <- read.csv(file.choose())
View(delivery_time)
attach(delivery_time)
#Plot
plot(delivery_time, main = "scatterplot")
#Simple Regression
reg1<-lm(Delivery.Time ~ Sorting.Time, delivery_time)
summary(reg1)
#Rsq:0.6823
#Tranformation required as the model is not good
#For deleteion of influencing observation
influence.measures(reg1)
mod1<-lm(Delivery.Time ~.,data=delivery_time[-c(5,21),])
summary(mod1)
#R-squared:  0.7716
#Confidence interval
confint(mod1, level = 0.95)
#Predict
predict(mod1,interval="prediction")
#As the R-sq value is 0.6823 we need tranformation.
#Logarithmic transformation
logmod1<-lm(Delivery.Time ~ log(Sorting.Time),data = delivery_time[-c(5,21),])
summary(logmod1)
#R-squared:  0.8107
#Confidence Interval
confint(logmod1,level=0.95)
#Predict#
predict(logreg1,interval="predict")
#exponential transformation
expmod1<-lm(log(Delivery.Time)~Sorting.Time,data = delivery_time[-c(5,21),]) 
summary(expmod1)
#R-squared:0.7716
#Confidence Interval
confint(expmod1,level=0.95)
#Predict
predict(expmod1,interval="predict")

#Thus,after deletion of 5th and 21st observations 
#and by performing Logarithmic transformation we get a Rsquare of 0.8107
#so as the model is above 0.8 we can accept the built model  


