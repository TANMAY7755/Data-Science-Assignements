#;::::::::::::::::SUM1::::::::::::::::::

#::::::::::::::::::::::::Amazon reviews extraction for emotion mining::::::::::::::
######product reiew######
setwd("D:\\TanmayDataScience\\text min")
library(rvest)
library(XML)
library(magrittr)
#URL for extraction
url<-"https://www.amazon.in/OnePlus-Nord-Gray-256GB-Storage/dp/B08697MJD8/ref=lp_21827649031_1_1?s=electronics&ie=UTF8&qid=1605629922&sr=1-1"

amazon_reviews <- NULL
for (i in 1:10){
  url1 <- read_html(as.character(paste(url,i,sep="=")))
  rev <- url1 %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"nord.txt",row.names = F)

#URL
samp_url <- "https://www.amazon.in/OnePlus-Nord-Gray-256GB-Storage/dp/B08697MJD8/ref=lp_21827649031_1_1?s=electronics&ie=UTF8&qid=1605629922&sr"
i=1
p=1
predator <- NULL
while(p>0){
  t_url <- read_html(as.character(paste(samp_url,i,sep="=")))
  rev <- t_url %>%
    html_nodes(".review-text") %>%
    html_text()
  predator <- c(predator,rev)
  i <- i+1
  p=length(rev)
}

length(predator)
print(predator)
#LIB
library(tm)

#text<- readLines(predator)
txt<- as.character(predator)
corpus<- Corpus(VectorSource(txt))

#CORPUS 
x1 <- tm_map(corpus, tolower)
inspect(x1[1])

#Prepocessing data(Cleaning punctuation, emoticon, numbers, stopwords)
x1<- tm_map(x1, removePunctuation)
x1<- tm_map(x1, removeEmo)
x1<-tm_map(x1, removeNumbers)
x1 <- tm_map(x1, removeWords , stopwords('english'))
inspect(x1[1])
x1<- tm_map(x1, removeWords, c("can", "given", "might", "also","cameraera"))
x1<- tm_map(x1, gsub, pattern = "phones", replacement = "phone" )
x1<- tm_map(x1, gsub, pattern = "cam", replacement = "camera" )

x1<- tm_map(x1, stripWhitespace) #Strip extra spaces
inspect(x1[1])

#converting into TDM matrix
tdm<- TermDocumentMatrix(x1) 
dtm<- t(tdm)
tdm<- as.matrix(tdm)
tdm
z<-rowSums(tdm)
z
z1<- subset(z, z >= 100)
z1
windows()
barplot(z1, las = 3, col = rainbow(20))
#Lib
library(wordcloud)

#wordcloud
wordcloud(words = names(z1), freq = z1)
wordcloud(words = names(z1), freq = z1, random.order = F, max.words = 150, col =rainbow(20), scale = c(3,1), rot.per = 0.3)

#importing positive and negative words  
pos.words = scan(file.choose(), what = "character", comment.char = ";")
neg.words = scan(file.choose(), what = "character", comment.char = ";")

#matching of pos
pos.matches = match(names(z1),c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- z1[pos.matches]
p_names <- names(freq_pos)

#wordcloud for positive words
wordcloud(p_names, freq_pos, scale = c(4,1), col = rainbow(20))

#matching of pos
neg.matches = match(names(z1),c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- z1[neg.matches]
n_names <- names(freq_neg)
#wordcloud for positive words
wordcloud(n_names, freq_neg, scale = c(4,1), col = brewer.pal(8,"Dark2"))


#:::::::::::;::::::::::::::::SUM2:::::::::::::::::::::::::::::::::

#::::::::::::::::Tweet data extraction from twitter for sentimental analysis::::::::::::::
#LIBRARies
library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)

#Generated keys and url from twitter api develpoer account
cred <- OAuthFactory$new(consumerKey='ulb4xAtUFS7fqCyCkqIKkemJ5', # Consumer Key (API Key)
                         consumerSecret='2FVV5r8gJ2SQEWwMRmRrNxPukWUkrCHg4vCBP0Sk1qUwpizs86', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

setup_twitter_oauth("ulb4xAtUFS7fqCyCkqIKkemJ5", # Consumer Key (API Key)
                    "2FVV5r8gJ2SQEWwMRmRrNxPukWUkrCHg4vCBP0Sk1qUwpizs86", #Consumer Secret (API Secret)
                    "1245731465008046080-boci5VkPoHf1jLn0FGp4aYlmdSuY5V",  # Access Token
                    "JCilXbLi8s3CKhuPnJGYTSvoneQZNkD2QUejkgrcOV97P")  #Access Token Secret
tweets<-userTimeline("ANI", n = 1000) # "ANI"twitter handle 

#tweets in DF
tweetsDF<-twListToDF(tweets)
write.csv(tweetsDF , "Tweets.csv")
View(tweetsDF)
text1<-searchTwitter("CBI", n = 100, lang = "en", resultType = "recent")#CBI from ANI tweets
tweetsDF<-twListToDF(text1)

library(tm)#

#Corpus 
corpus <- iconv(tweetsDF$text, to='UTF-8', sub = "byte")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Pre-proccessing and Clean text for further analysis
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleantext<- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleantext[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)#to remove urls in from corpus
cleantext <- tm_map(cleantext, content_transformer(removeURL))
inspect(cleantext[1:5])

cleantext<- tm_map(cleantext, removeWords, c('aapl', 'apple'))

cleantext <- tm_map(cleantext, stripWhitespace)
inspect(cleantext[1:5])

# Term document matrix(TDM)
tdm <- TermDocumentMatrix(cleantext)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Bar plot
z<- rowSums(tdm)
z<- subset(z, z>=5)
barplot(z,las = 3, col = rainbow(20))

# Word cloud
library(wordcloud)
z<- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(z),
          freq = z,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3)) 

#WORDCLOUD  with different shapes 
library(wordcloud2)
z<- data.frame(names(z), z)
colnames(z) <- c('word', 'freq')
wordcloud2(z,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

letterCloud(z,
            word = "apple",
            size=1)

# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
apple <- read.csv(file.choose(), header = T)
tweets1 <- iconv(tweetsDF$text, to='UTF-8', sub = "byte")


# Obtain sentiment scores
a <- get_nrc_sentiment(tweets1 )
head(a)
tweets1[1]

# Bar plot
barplot(colSums(a),
        las = 3,
        col = rainbow(20),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')
