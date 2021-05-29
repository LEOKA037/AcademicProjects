library(tm)
library(wordcloud)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(syuzhet)
library(readxl)
#select speech text file
Speech <- read_excel("dataset/modi.xlsx",col_names = FALSE)
colnames(Speech)<-"text"
#function to pre Process tweets
Preprocess.text = function(Tweets){
    Tweets2<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",Tweets)
    Tweets2<-gsub("http[^[:blank:]]+","",Tweets2)
    Tweets2<-gsub("@\\w+","",Tweets2)
    #Tweets2<-gsub("[A-z]","",Tweets2)
    Tweets2<-gsub('[[:punct:]]', '',Tweets2)
    #Tweets2<-gsub("[^[:alnum:]]","",Tweets2)
    text<-tolower(Tweets2)
    return(text)
}
temp<-Preprocess.text(Speech$text)

docs <- Corpus(VectorSource(temp))
docs <- tm_map(docs, removeWords, 
               c("the", "a", "have", "is", "i","are", "will","and","that","his","with",
                 "and",  "that", "his","for","this" ,"but","who" , "not","has","had","was",
                 "were")) 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq,scale=c(3,.3),min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))


    Sent.matrix<-get_nrc_sentiment(temp)
#Speech.Data<-rbind(Speech.Data,cbind(Speech,Sent.matrix))
Sent.matrix<-round(prop.table(Sent.matrix)*100,2)
Sent.matrix<-data.frame(colSums(Sent.matrix))
colnames(Sent.matrix)<- "Score"
Sent.matrix<- cbind("Sentiment"=rownames(Sent.matrix),Sent.matrix)
rownames(Sent.matrix)<-NULL
Speech.Sentiment<-data.frame(Sent.matrix)
Speech.Nrc<-data.frame(Sent.matrix[9:10,])


ggplot(data=Speech.Sentiment,aes(x=Sentiment,y=Score))+
    geom_bar(aes(fill=Sentiment), stat = "identity",width = .7)+
    theme(legend.position = "none")+
    xlab("Emotions")+ylab("Score ( in % value)")+
    ggtitle("Total Avg emotions words used in speech Modi") 

ggplot(data=Speech.Nrc,aes(x=Sentiment,y=Score))+
    geom_bar(aes(fill=Sentiment), stat = "identity",width = .5 )+
    theme(legend.position = "none")+
    xlab("Sentiment")+ylab("Score ( in % value)")+
    ggtitle("Total Positive negative words used in speech") 

##Per_of
sent.value<- get_sentiment(temp)
#Speech.DF<-cbind(Speech,Sent.matrix,sent.value)
