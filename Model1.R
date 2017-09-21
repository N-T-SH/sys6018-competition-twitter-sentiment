# install.packages("RWeka")
library(tm)
library(readr)
library(RWeka)


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
# tweets.90
tweets.train = as.data.frame(as.matrix(tweets.90))
train.new = cbind(tweets.train,train$sentiment)

test.90 <- as.data.frame(as.matrix(g(test)))

train.new$fbi <-  NULL
train.new$januari <-  NULL
train.new$univers <-  NULL
test.90 = test.90[colnames(test.90) %in% colnames(train.new)]
# Linear Model
model2 <- lm(`train$sentiment`~.,data = train.new)

# KNN from package
test.90 = test.90[colnames(test.90) %in% colnames(train.new)]
model3 <-  IBk(`train$sentiment`~.,data = train.new, control = Weka_control(K = 15, X = TRUE))
summary(model3)

# KNN from scratch

# Splitting data into training and testing for cross validation
dim(train.new)
colnames(train.new)[colnames(train.new) == 'train$sentiment'] <- 'sentiment'
train.new <- train.new[sample(nrow(train.new)),]
knn.train <- train.new[1:as.integer(0.7*nrow(train.new)),]
knn.test <- train.new[as.integer(0.7*nrow(train.new) +1):nrow(train.new),]
write.csv(knn.train,"knn.csv",row.names = F)

# Function for calculating distance
Dist <- function(x, y){
  dist = 0
  for(i in c(1:(length(x)-1) ))
  {
    dist = dist + (x[[i]]-y[[i]])^2
  }
  dist = sqrt(dist)
  return(dist)
}

# KNN Function
knn_predict <- function(knn.test, knn.train, k = 15){
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(knn.test))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    good = 0              #good & bad variable initialization with 0 value
    bad = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(knn.train))){
      
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, Dist(knn.test[i,], knn.train[j,]))
      
      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, knn.train[j,'sentiment'])
    }
    
    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
    
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu <- eu[1:k,]               #eu dataframe with top K neighbors
    
    #Loop 3: loops over eu and counts classes of neibhors.
    p <- tail(names(sort(table(eu$eu_char))), 1)
    pred <- c(pred,p)
    
  }
return(pred) #return pred vector
}
# Testing cross validation performance


accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,'sentiment'] == test_data[i,'PredictedSentiment']){ 
      correct = correct +1 
    }
  }
  accu = correct/nrow(test_data) * 100  
  return(accu)
}

# Checking the model for accuracy
K = 15
knn.test[,'PredictedSentiment'] <- knn_predict(knn.test, knn.train, K) #calling knn_predict()
print(accuracy(knn.test))


test.90[,'sentiment'] <- knn_predict(test.90, knn.train, K) #calling knn_predict()
guesses <- cbind(test$id,test.90[,'sentiment'])
colnames(guesses) <- c("id","sentiment")
write.csv(guesses,"sentiment_guesses_KNN.csv",row.names = F)

# Prediction and Writing output to File
guesses  = predict(model3,test.90)
guesses = round(guesses,digits = 0)
guesses = cbind(test$id,guesses)
colnames(guesses) <- c("id","sentiment")
write.csv(guesses,"sentiment_guesses.csv",row.names = F)
