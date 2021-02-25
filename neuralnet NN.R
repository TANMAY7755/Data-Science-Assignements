#::::::::::::::::::::::::::::::sum1::::::::::::::::::
#Import dataset
forestfires<-read.csv("D:/TanmayDataScience/assignments/12NN/forestfires (2).csv")
View(forestfires)
str(forestfires)

#factor
forestfires[sapply(forestfires, is.character)] <- lapply(forestfires[sapply(forestfires, is.character)], as.factor)

##Numeric 
forestfires$day<-as.numeric(forestfires$day)
forestfires$month<-as.numeric(forestfires$month)
forestfires$size_category<-as.numeric(forestfires$size_category)

#norm
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfires_norm<-as.data.frame(lapply(forestfires,FUN=normalize))

#Data partition
set.seed(222)
data11<- sample(2, nrow(forestfires_norm ), replace = TRUE, prob = c(0.8, 0.2))
forestfires_train <-forestfires_norm [data11==1,]
forestfires_test<- forestfires_norm [data11==2,]

#lib
library(neuralnet) 
colnames(forestfires)

# Building model
forestfires_model <- neuralnet(formula = area ~ month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain+size_category,data = forestfires_train)
str(forestfires_model)
plot(forestfires_model)

# compute function 
set.seed(12323)
model_results <- compute(forestfires_model,forestfires_test[-11])
str(model_results)
predicted_area <- model_results$net.result

# predicted_area
cor(predicted_area,forestfires_test$area)
plot(predicted_area,forestfires_test$area)

#model2 wuth hidden layers
set.seed(333)
model_2<-neuralnet(formula = area ~ month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain+size_category,
                   data = forestfires_train,  
                   hidden = c(25,2),
                   lifesign = 'full',
                   rep = 3,
                   linear.output = TRUE )
plot(model_2, rep='best') #ploting best model out of rep = 3
model_2$net.result

# model_results$neurons
set.seed(333)
model_results_2<-compute(model_2,forestfires_test[-11])
predicted_area_2<-model_results_2$net.result
cor(predicted_area_2,forestfires_test$area)#69.4231 corr
plot(predicted_area_2,forestfires_test$area)

