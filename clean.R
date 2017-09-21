library(tm)
library(readr)
library(boot)
setwd('D:/Fall Semester/SYS6018/competition3')
train <- read_csv("train.csv")
test  <-  read_csv("test.csv")
g <- function(train){
  
  
  tweets = VCorpus(DataframeSource(train))
  inspect(tweets[1:2])
  tweets[[1]]$content
  
  
  
  
  tweets.clean = tm_map(tweets, removeNumbers)                      # remove numbers
  f <- content_transformer(function(x, pattern) gsub(pattern, "", x))     # build regex function
  tweets.clean = tm_map(tweets.clean, f,"@\\S+")                          # remove usernames
  tweets.clean = tm_map(tweets.clean, stripWhitespace)                          # remove extra whitespace
  tweets.clean = tm_map(tweets.clean, removePunctuation)                  # remove punctuation
  tweets.clean = tm_map(tweets.clean, content_transformer(tolower))       # ignore case
  tweets.clean = tm_map(tweets.clean, removeWords, stopwords("english"))  # remove stop words
  tweets.clean = tm_map(tweets.clean, stemDocument) 
  
  tweets[[1]]$content
  tweets.clean[[1]]$content
  
  tweets.clean.tfidf = DocumentTermMatrix(tweets.clean, control = list(weighting = weightTfIdf))
  return(tweets.clean.tfidf)
  # return(as.matrix(tweets.clean.tfidf))
}