#This file contains a function to clean the data-set and return a Document Term Matrix to perform Random Forest

#Load the packages
library(tm)
library(readr)
library(boot)

#Function Definition
g <- function(train){
  
  #Creates the corpus
  tweets = VCorpus(DataframeSource(train))
  inspect(tweets[1:2])
  tweets[[1]]$content
  # remove numbers
  tweets.clean = tm_map(tweets, removeNumbers)
  # build regex function
  f <- content_transformer(function(x, pattern) gsub(pattern, "", x))     
  # remove usernames
  tweets.clean = tm_map(tweets.clean, f,"@\\S+")                          
  # remove extra whitespace
  tweets.clean = tm_map(tweets.clean, stripWhitespace)                    
  # remove punctuation
  tweets.clean = tm_map(tweets.clean, removePunctuation)
  # ignore case
  tweets.clean = tm_map(tweets.clean, content_transformer(tolower))       
  # remove stop words
  tweets.clean = tm_map(tweets.clean, removeWords, stopwords("english"))  
  tweets.clean = tm_map(tweets.clean, stemDocument) 
  #Creates and returns the document term matrix
  tweets.clean.tfidf = DocumentTermMatrix(tweets.clean, control = list(weighting = weightTfIdf))
  return(tweets.clean.tfidf)
}