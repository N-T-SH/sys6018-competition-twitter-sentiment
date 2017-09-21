library(tm)
library(readr)
library(boot)
library(caret)
library(randomForest)
source('clean.R')
setwd('D:/Fall Semester/SYS6018/competition3')
train <- read_csv("train.csv")
test  <-  read_csv("test.csv")
train.clean<-g(train)

tweets.clean = removeSparseTerms(train.clean, 0.99)
tweets.clean = as.data.frame(as.matrix(tweets.clean))
train.new = cbind(tweets.clean,train$sentiment)

trainIndex <- createDataPartition(train.new$`train$sentiment`, p = 0.9, list = FALSE, times = 1)
tweets_train <- train.new[trainIndex, ]
tweets_cv <- train.new[-trainIndex, ]

forestmodel<-randomForest(`train$sentiment`~cant+car+come+dont+excit+googl+insur+less+need+safer+save+soon+thing+want+wait+warn+wrong,distribution="gaussian",data=train.new)
summary(forestmodel)

preds<-predict(forestmodel,tweets_cv)
summary(lm(preds ~ tweets_cv$`train$sentiment`))$r.squared

test.clean<-g(test)
test.clean=removeSparseTerms(test.clean,0.99)
test.clean=as.data.frame(as.matrix(test.clean))
test.clean$excit<-rep(0,979)
test.clean$safer<-rep(0,979)
test.clean$warn<-rep(0,979)
test.clean$wrong<-rep(0,979)
test.clean$save<-rep(0,979)
prediction<-predict(forestmodel,test.clean)
guesses = round(prediction,digits = 0)
guesses = cbind(test$id,guesses)
colnames(guesses) <- c("id","sentiment")
write.csv(guesses,"gbm.csv",row.names = F)