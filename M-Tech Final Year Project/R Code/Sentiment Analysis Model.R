library(tm)
#library(RTextTools)
library(readr)
library(e1071)
library(dplyr)
library(caret)
df<- read.csv("dataset/labelledTweets_DS.csv", stringsAsFactors = FALSE)
#labelledTweets_DS.csv Tweet_Dataset.csv
set.seed(1)

df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]

df$class <- as.factor(df$class)
corpus <- Corpus(VectorSource(df$text))
corpus
inspect(corpus[1:3])
corpus.clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords(kind="en")) %>%
    tm_map(stripWhitespace)
dtm <- DocumentTermMatrix(corpus.clean)
partition<-createDataPartition(df$class, p=0.75, list=FALSE)

df.train <- df[partition,]
df.test <- df[-partition,]

dtm.train <- dtm[partition,]
dtm.test <- dtm[partition,]

corpus.clean.train <- corpus.clean[partition]
corpus.clean.test <- corpus.clean[partition]

fivefreq <- findFreqTerms(dtm.train, 5)
dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

convert_count <- function(x) {
    y <- ifelse(x > 0, 1,0)
    y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
    y
}
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)
#--------------------------------------Naive Bayes Algorithm-----------------------------------------
system.time( NB_classifier <- naiveBayes(trainNB, df.train$class, laplace = 1) )
system.time( NB_pred <- predict(NB_classifier, newdata=testNB) )
#table("Predictions"= NB_pred,  "Actual" = df.test$class )
NB_conf.mat <- confusionMatrix(NB_pred, df.test$class)

NB_conf.mat
#conf.mat$byClass
#conf.mat$overall
NB_conf.mat$overall['Accuracy']

#_____________________________Support Vectore machine (SVM)-----------------------------------------
convert_count <- function(x) {
    y <- ifelse(x > 0, 1,0)
    #y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
    y
}
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)
trainNB<- as.data.frame(trainNB)
testNB<- as.data.frame(testNB)
train_SVM<-cbind(class=factor(df.train$class), trainNB)
test_SVM<- cbind(class=factor(df.test$class), testNB)
train_SVM<-as.data.frame(train_SVM)
test_SVM<-as.data.frame(test_SVM)
system.time( SVM_classifier <- svm(class~.,data = train_SVM) )
system.time( SVM_pred <- predict(SVM_classifier, na.omit(test_SVM)) )
SVM_conf.mat <- confusionMatrix(SVM_pred, test_SVM$class,positive = "Positive")
SVM_conf.mat
#table("Predictions"= NB_pred,  "Actual" = df.test$class )
SVM_conf.mat$overall['Accuracy']

