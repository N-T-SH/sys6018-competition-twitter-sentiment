
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

guesses  = predict(model2,test.90)
guesses = round(guesses,digits = 0)
guesses = cbind(test$id,guesses)
colnames(guesses) <- c("id","sentiment")
write.csv(guesses,"sentiment_guesses.csv",row.names = F)


# --------- KNN from scratch ------------

# Function for calculating distance b/w all variables of two entries except the response variable
Dist <- function(x, y){
  dist = 0
  for(i in c(1:(length(x)-1) ))
  {
    dist = dist + (x[i]-y[i])^2
  }
  dist = sqrt(dist)
  return(dist)
}

# Function to calculate predicted class for single record
knn_calc <- function(knn.calc, knn.train, k){
  eu_dist =c()          
  eu_char = c()
  # Calculating distance and their corresponding class
  eu_dist <- apply(knn.train,1,function(x,y) Dist(y, x), y = knn.calc)
  eu_char <- knn.train[,'sentiment']
  eu <- data.frame(eu_char, eu_dist) 
  # Sorting eu dataframe to get top K neighbors
  eu <- eu[order(eu$eu_dist),]       
  eu <- eu[1:k,]               
  # Takes the name of categorical variable with top count of neighbors
  p <- tail(names(sort(table(eu$eu_char))), 1)
  return(p)
  
}

# KNN Function applies knn_calc to each record in uncategorized data
knn_predict <- function(knn.test, knn.train, k){
  pred <- c()  
  pred <- apply(knn.test,1,function(x,y,k) knn_calc(x,y,k), y = knn.train, k = k)
  return(pred) 
}

# Testing cross validation performance
accuracy <- function(test_data){
  correct = 0
  if(test_data[,'sentiment'] == test_data[,'PredictedSentiment']){ 
    correct = correct +1 
  }
  accu = correct/nrow(test_data) * 100  
  return(accu)
}

# Leave one out cross validation
loocv <- function(x, train.new, k){
  knn.train <- train.new[-x,]
  knn.test <- train.new[x,]
  knn.test[,'PredictedSentiment'] <- knn_predict(knn.test, knn.train, k) #calling knn_predict()
  return(accuracy(knn.test))
  
}


# Renaming sentiment column in training data
colnames(train.new)[colnames(train.new) == 'train$sentiment'] <- 'sentiment'
# Creating sample for LOOCV
leaveout <- sample(nrow(train.new))
# Generating sample data for different values in k
# Function to check different values of k
k = 15
accr <- sapply(leaveout, function(x,train.new,k) loocv(x,train.new,k), train.new = train.new, k = k)
ma <- mean(accr)
# at k = 2, accuracy is  47.0948
# at k = 10, accuracy is 60.44852
# at k = 15, accuracy is 60.75433 <- chosen
# at k = 20, accuracy is 60.95821
# at k = 30, accuracy is 61.46789

# Generating classes for unclassified data
test.90[,'sentiment'] <- knn_predict(test.90, knn.train, K) #calling knn_predict()
guesses <- cbind(test$id,test.90[,'sentiment'])
colnames(guesses) <- c("id","sentiment")
# Writing output file for KNN predictions
write.csv(guesses,"sentiment_guesses_KNN.csv",row.names = F)
# KNN score on Kaggle with k = 15
# Public Leaderboard 0.65102
# Private Leaderboard 0.68507
