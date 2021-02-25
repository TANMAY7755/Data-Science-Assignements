#:::::::::::::::sum1:::::::::::::::::

##Import dataset
startupFifty <- read.csv("C:/Users/WINDOWS10/Desktop/TanmayDataScience/assignments/5MULTI LINEAR/startupFifty.csv")
View(startupFifty)
attach(startupFifty)

library(varhandle)
strt<-to.dummy(startupFifty$State,"1")
show(strt)

##Plot of all varibles
plot(x=R.D.Spend, y=Profit)
plot(x=State, y=Profit)
plot(x=Administration, y=Profit)
plot(x=Marketing.Spend, y=Profit)

#Model for R.D.Spend
Startmod1<-lm(Profit ~ R.D.Spend, data = startupFifty)
summary(Startmod1)

#Model for Administration
Startmod2<-lm(Profit ~ Administration, data = startupFifty)
summary(Startmod2)

#Model for Marketing.Spend
Startmod3<-lm(Profit ~ Marketing.Spend, data = startupFifty)
summary(Startmod3)

#Model for Marketing.Spend
Startmod4<-lm(Profit ~ State, data = startupFifty)
summary(Startmod4)

#Model for R.D.Spend+Marketing.Spend
Startmod6<-lm(Profit ~ R.D.Spend+Marketing.Spend, data = startupFifty)
summary(Startmod66)

#The Linear Model of interest with all the columns
Startmod<-lm(Profit~., data = startupFifty)
summary(Startmod)

#Performing Deletion Diagnostics for identifying influential observations
influence.measures(Startmod)

##plotting Influential measures
library(car)
influenceIndexPlot(Startmod,id.n=3)
influencePlot(Startmod,id.n=3)

# Regression after deleting the 50th observation and administraion col, which is influential observations
model_2<-lm(Profit ~.-Administration ,data = startupFifty[-c(46,47,49,50),])
summary(model_2)

## Final model
finalmodel<-lm(Profit~.-Administration,data=startupFifty[-c(46,47,49,50),])
summary(finalmodel)
#Multiple R-squared:  0.9608

##prediction for the finalmodel
predict(finalmodel,interval="predict")

##ploting of final model
plot(finalmodel)
hist(residuals(finalmodel))

#Here,R-squared = 0.9608 and R.D.Spend is a significant variable
#which is greater than 0.8, so the model built is very good 
#and has a strong correlation.


#::::::::::::::::::::::::::::SUM2::::::::::::::::::::::::::::::::::::
  
  
##Import dataset
View(ToyotaCorolla)
Corolla<-ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

#summary of dataset
summary(Corolla)

#Find the correlation b/n Output
pairs(Corolla)

#Correlation Coefficient matrix
cor(Corolla)

#Partial Correlation matrix
library(corpcor)
cor2pcor(cor(Corolla))

#The Linear Model with all the variables
mod.cor <- lm(Price~.,data=Corolla)
summary(mod.cor)

#Model based on only Age_08_04
model.car1<-lm(Price~ Age_08_04, data = Corolla)
summary(model.car1)

#Model based on only KM
model.car2<-lm(Price~ KM, data = Corolla)
summary(model.car2)

#Model based on only HP
model.car3<-lm(Price~HP, data = Corolla)
summary(model.car3)

#Model based on only cc
model.car4<-lm(Price~ cc, data = Corolla)
summary(model.car4)

#Model based on only Doors
model.car5<-lm(Price~ Doors, data = Corolla)
summmary(model.car5)

#Model based on only Gears 
model.car6<-lm(Price~ Gears , data = Corolla)
summary(model.car6)

#Model based on only Quarterly_Tax
model.car7<-lm(Price~ Quarterly_Tax , data = Corolla)
summary(model.car7)

#Model based on only Weight
model.car8<-lm(Price~ Weight , data = Corolla)
summary(model.car8)

#VIF to check collinearity
library(car)
vif(mod.cor)

avPlots(mod.cor,id.n=2,id.cex=0.7)

#Deletion Diagnostics for identifying influential observations
influence.measures(mod.cor)
influenceIndexPlot(mod.cor,id.n=3)
influencePlot(mod.cor,id.n=3)

#Regression after deleting the 81,222,602,961 observations.
finalmodcor<-lm(Price~.,data=Corolla[-c(81,222,602,961),])
summary(finalmodcor)

#ploting final model
plot(finalmodcor)
hist(residuals(finalmodcor))

#Here,Mul R_squared is 0.8894 which is greater than 0.8, so the model built is acceptable 
#and has an average correlation.


