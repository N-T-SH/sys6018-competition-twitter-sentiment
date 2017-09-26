#Performs random forest
#Imports the libraries
library(tm)
library(readr)
library(boot)
library(caret)
library(randomForest)
#For cleaning the file
source('clean.R')

#Loads the data
train <- read_csv("train.csv")
test  <-  read_csv("test.csv")
train.clean<-g(train)

#Removes sparse terms. We could have put this in the cleaning function in
#clean.R but we wanted to try different alternatives to 0.99. We tried values
#between 0.98 and 0.995 but found 0.99 worked best
tweets.clean = removeSparseTerms(train.clean, 0.99)

#Converts to data frame
tweets.clean = as.data.frame(as.matrix(tweets.clean))
#Adds the sentiment column
train.new = cbind(tweets.clean,train$sentiment)

#Partitions the data for cross-validation
trainIndex <- createDataPartition(train.new$`train$sentiment`, p = 0.9, list = FALSE, times = 1)
tweets_train <- train.new[trainIndex, ]
tweets_cv <- train.new[-trainIndex, ]

#We settled on the predictors after several rounds of trial and error to see what worked best
forestmodel<-randomForest(`train$sentiment`~cant+car+come+dont+excit+googl+insur+less+need+safer+save+soon+thing+want+wait+warn+wrong,distribution="gaussian",data=train.new)

summary(forestmodel)
#Cross-validation
preds<-predict(forestmodel,tweets_cv)
summary(lm(preds ~ tweets_cv$`train$sentiment`))$r.squared
#R-squared comes to be 0.3, this was the best R-squared value we could find

#Cleans the test dataset
test.clean<-g(test)
#Removes sparse terms
test.clean=removeSparseTerms(test.clean,0.99)
test.clean=as.data.frame(as.matrix(test.clean))
#Sets various terms that were not in the data to 0.
test.clean$excit<-rep(0,979)
test.clean$safer<-rep(0,979)
test.clean$warn<-rep(0,979)
test.clean$wrong<-rep(0,979)
test.clean$save<-rep(0,979)
#Generates predictions
prediction<-predict(forestmodel,test.clean)
guesses = round(prediction,digits = 0)
#Creates the output file
guesses = cbind(test$id,guesses)
colnames(guesses) <- c("id","sentiment")
write.csv(guesses,"randomforest.csv",row.names = F)