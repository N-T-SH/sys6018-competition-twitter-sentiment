
library(tm)
library(readr)
library(boot)

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
tweets.90 = g(train)


tweets.90 = removeSparseTerms(tweets.90, 0.99)  # remove terms that are absent from at least 99% of documents (keep most terms)
tweets.90
tweets.train = as.data.frame(as.matrix(tweets.90))
train.new = cbind(tweets.train,train$sentiment)

test.90 <- as.data.frame(as.matrix(g(test)))

train.new$fbi <-  NULL
train.new$januari <-  NULL
train.new$univers <-  NULL

model2 <- glm(`train$sentiment`~cant+car+come+dont+excit+googl+insur+less+need+
                safer+save+soon+thing+want+wait+warn+wrong,data = train.new)
summary(model2)
cv.error = cv.glm(train.new,model2)$delta[1] #[1] 0.6508767 0.6507858 with all variables
#0.5535677 with 20 best variables
#0.5529536 with 17 best variables, so for now, use just these variables


install.packages("RWeka")
library(RWeka)
test.90 = test.90[colnames(test.90) %in% colnames(train.new)]
# train.new2 = train.new
# train.label <- train.new2$`train$sentiment`
# train.new2$`train$sentiment` = NULL
model3 <-  IBk(`train$sentiment`~.,data = train.new, control = Weka_control(K = 15, X = TRUE))
summary(model2)



guesses  = predict(model2,test.90)
guesses = round(guesses,digits = 0)
guesses = cbind(test$id,guesses)
colnames(guesses) <- c("id","sentiment")
write.csv(guesses,"sentiment_guesses.csv",row.names = F)

