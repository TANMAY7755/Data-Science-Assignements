#:::::::::::::::::::::::::SUM1:::::::::::::::::::::::

#Import
#LIBRARY
library(readr)
setwd("D:\\TanmayDataScience\\assignments\\18forcasting")
PlasticSales <- read.csv(file.choose())
View(PlasticSales) # read the cocacola data
# Seasonality 12 months 
windows()
plot(PlasticSales$Sales,type="o")
# So creating 12 dummy variables 

A<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
View(A)
42
colnames(A)<-month.abb # Assigning month names 
View(A)
psales<-cbind(PlasticSales,A)
View(psales)
colnames(psales)[2]<-"sales"
colnames(psales)
psales["t"]<- 1:60
View(psales)
psales["log_sales"]<-log(psales["sales"])
psales["t_sq"]<-psales["t"]*psales["t"]
attach(psales)

train<-psales[1:45,]

test<-psales[46:60,]

########################### LINEAR MODEL #############################

linear_mod<-lm(sales~t,data=train)
summary(linear_mod)
linear_pred<-data.frame(predict(linear_mod,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 241.3659

#1

######################### Exponential #################################
#2
expo_mod<-lm(log_sales~t,data=train)
summary(expo_mod)
expo_pred<-data.frame(predict(expo_mod,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 247.9056

######################### Quadratic ####################################
#3
Quad_mod<-lm(sales~t+t_sq,data=train)
summary(Quad_mod)
Quad_pred<-data.frame(predict(Quad_mod,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 276.164

######################### Additive Seasonality #########################
#4
sea_add_mod<-lm(sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_mod)
sea_add_pred<-data.frame(predict(sea_add_mod,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 257.1331

######################## Additive Seasonality with Linear #################
#5
Add_sea_Linear_mod<-lm(sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_mod)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_mod,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 117.0469

######################## Additive Seasonality with Quadratic #################
#6
Add_sea_Quad_mod<-lm(sales~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_mod)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_mod,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 151.1677

######################## Multiplicative Seasonality #########################
#7
multi_sea_mod<-lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_mod)
multi_sea_pred<-data.frame(predict(multi_sea_mod,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 261.539

######################## Multiplicative Seasonality Linear trend ##########################
#8
multi_add_sea_mod<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_mod) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_mod,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 133.2998

# Preparing table on mod and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("mod","RMSE")
View(table_rmse)
###"rmse_Add_sea_Linear" has less residueals
# Additive seasonality with Quadratic has least RMSE value

new_mod <- lm(sales~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=psales)


resid <- residuals(new_mod)
resid
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive mod on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 15)
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(psales,file="psales.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
library(readxl)
#read.csv("D:/TanmayDataScience/assignments/18forcasting/psales.csv")
#test_data<-read.csv(("D:/TanmayDataScience/assignments/18forcasting/psales.csv"),1)
View(test)
pred_new<-data.frame(predict(new_mod,newdata=test,interval = 'predict'))
View(pred_new)
pred_new_sales <- pred_new$fit+pred_res$pred
View(pred_new_sales)
###########
##################
####################################################SUM2##############
library(readxl)
library(readr)
airline <- read.csv(file.choose())
View(airline)
#Seasonality 12 months 
windows()
plot(airline$Passengers,type="o")
# So creating 12 dummy variables 

X1<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X1)

colnames(X1)<-month.abb # Assigning month names 
View(X1)
data<-cbind(airline,X1)
View(data)
colnames(data)[2]<-"Passengers"
colnames(data)
data["t"]<- 1:96
View(data)
data["log_Passengers"]<-log(data["Passengers"])
data["t_sq"]<-data["t"]*data["t"]
attach(data)

train<-data[1:76,]

test<-data[77:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear #57.00015
######################### Exponential #################################
expo_model<-lm(log_Passengers~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.62154
######################### Quadratic ####################################
Quad_model<-lm(Passengers~t+t_sq,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad #58.49427
######################### Additive Seasonality #########################
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #132.2541
######################## Additive Seasonality with Linear #################
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear #40.30287
######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model<-lm(Passengers~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 39.75977
######################## Multiplicative Seasonality #########################
multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea #137.6109
######################## Multiplicative Seasonality Linear trend ##########################
multi_add_sea_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 11.78425

#rmse_multi_add_sea
# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
#Rmse_multi_add_sea has lowest residuals
# 

new_model <- lm(Passengers~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=data)


resid <- residuals(new_model)
resid
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

V <- arima(resid, order=c(1,0,0))
str(V)

View(data.frame(res=resid,newresid=V$residuals))
windows()
acf(V$residuals,lag.max = 10)
pred_res<- predict(arima(V$residuals,order=c(1,0,0)),n.ahead = 20)
str(pred_res)
pred_res$pred
acf(V$residuals)
write.csv(data,file="data.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
library(readxl)
#test_data<-read_excel(file.choose(),1)
View(test)
pred_new<-data.frame(predict(new_model,newdata=test,interval = 'predict'))
View(pred_new)
pred_new2<- pred_new$fit+pred_res$pred
View(pred_new2)
###############
######################
###########################################SUM3
library(readxl)
library(readr)
cola <- read.table(file = "clipboard", 
                   sep = "\t", header=TRUE)
View(cola)
#Seasonality 12 months 
windows()
plot(cola$Sales,type="o")
# So creating 12 dummy variables 

X2<- data.frame(outer(rep(month.abb,length = 42), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X2)

colnames(X2)<-month.abb # Assigning month names 
View(X2)
data<-cbind(cola,X2)
View(data)
colnames(data)[2]<-"Sales"
colnames(data)
data["t"]<- 1:42
View(data)
data["log_Sales"]<-log(data["Sales"])
data["t_sq"]<-data["t"]*data["t"]
attach(data)

train<-data[1:25,]

test<-data[36:42,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear #1191.915
######################### Exponential #################################
expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 985.8998
######################### Quadratic ####################################
Quad_model<-lm(Sales~t+t_sq,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad #825.0423
######################### Additive Seasonality #########################
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #2298.906
######################## Additive Seasonality with Linear #################
Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear #1127.678
######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model<-lm(Sales~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 1282.858
######################## Multiplicative Seasonality #########################
multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea #2315.581
######################## Multiplicative Seasonality Linear trend ##########################
multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 831.5981

#rmse_multi_add_sea
# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
#Rmse_multi_add_sea has lowest residuals
# 

new_model <- lm(Sales~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=data)


resid <- residuals(new_model)
resid
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

L<- arima(resid, order=c(1,0,0))
str(L)

View(data.frame(res=resid,newresid=L$residuals))
windows()
acf(L$residuals,lag.max = 10)
pred_res<- predict(arima(L$residuals,order=c(1,0,0)),n.ahead = 20)
str(pred_res)
pred_res$pred
acf(L$residuals)
write.csv(data,file="data.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
library(readxl)
#test_data<-read_excel(file.choose(),1)
View(test)
pred_new<-data.frame(predict(new_model,newdata=test,interval = 'predict'))
View(pred_new)
pred_new2<- pred_new$fit+pred_res$pred
View(pred_new2)
