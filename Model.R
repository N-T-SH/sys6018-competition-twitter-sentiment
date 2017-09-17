library(tm)
library(readr)

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
tweets.clean.tfidf
return(as.matrix(tweets.clean.tfidf))
}
tweets.90 = g(train)


tweets.90 = removeSparseTerms(tweets.clean.tfidf, 0.99)  # remove terms that are absent from at least 99% of documents (keep most terms)
tweets.90
tweets.train = as.data.frame(as.matrix(tweets.90))
train.new = cbind(tweets.train,train$sentiment)

test.90 <- as.data.frame(g(test))

train.new$fbi <-  NULL
train.new$januari <-  NULL
train.new$univers <-  NULL

model2 <- lm(`train$sentiment`~.,data = train.new)
install.packages("rJava")
library(RWeka)
test.90 = test.90[colnames(test.90) %in% colnames(train.new)]
train.new2 = train.new
train.new2$`train$sentiment` = NULL
model3 <-  knn.reg(train = train.new2, test = test.90, y = `train$sentiment`)
summary(model)

guesses  = predict(model2,newdata = test.90)
guesses = round(guesses,digits = 0)
guesses = cbind(test$id,guesses)
colnames(guesses) <- c("id","sentiment")
write.csv(guesses,"sentiment_guesses.csv",row.names = F)
