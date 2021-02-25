#:::::::::::::::::::::sum1:::::::::::::::::::::
#import data
book <- read.csv("D:/TanmayDataScience/assignments/10reccomendation sys/book.csv")
#deleting book$X which is an index variable.
book1<- subset(book,select = -X)
View(book1)
str(book1)
#libraries
library("recommenderlab")
library(caTools)

#hist
hist(book1$Book.Rating)

#converting in realRatingMatrix to build recommendation engine.
book_matrix <- as(book1, 'realRatingMatrix')

#Popularity based model
model1 <- Recommender(book_matrix , method="POPULAR")
#Predictions for two users 
recommended_items1 <- predict(model1, book_matrix[1001:1002], n=5)
as(recommended_items1, "list")

#User Based Collaborative Filtering (UBCF) model
model2 <- Recommender(book_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(model2, book_matrix[1001:1002], n=5)
as(recommended_items2, "list")

#prediction for ratings type
predicted_rating<- predict(model1, book_matrix[1001:1002], type="ratings")
as(predicted_rating,"list")
as( book_matrix[1001:1002], "list")
