library(tm)
#library(RTextTools)
library(readr)
library(e1071)
library(dplyr)
library(caret)
#df<- read.csv(file.choose(), stringsAsFactors = FALSE)
df<- read.csv("dataset/labelledTweets_DS.csv", stringsAsFactors = FALSE)
#set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
df<-df[1:2000,]
df$class <- as.factor(df$class)
corpus <- Corpus(VectorSource(df$text))
corpus.clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords(kind="en")) %>%
    tm_map(stripWhitespace)
dtm <- DocumentTermMatrix(corpus.clean)
df.train <- df[1:300,]
df.test <- df[301:2000,]

dtm.train <- dtm[1:300,]
dtm.test <- dtm[301:2000,]

corpus.clean.train <- corpus.clean[1:300]
corpus.clean.test <- corpus.clean[301:2000]
dim(dtm.train)
fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))
dim(dtm.train.nb)
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
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
SVM_conf.mat <- confusionMatrix(SVM_pred, test_SVM$class,positive = "Pos")
SVM_conf.mat
table("Predictions"= SVM_pred,  "Actual" = df.test$class )
SVM_conf.mat$overall['Accuracy']

