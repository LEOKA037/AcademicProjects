library(syuzhet)
#library(twitteR)
library(tidyverse)
#sourcing r script for Twiter API
#source('G:\Mtech Project\M.Tech-Thesis-Project-master\R_Script\twittwr_data_extrac.R')

#sourcing r script for function
#source('G:\Mtech Project\M.Tech-Thesis-Project-master\R_Script\functions.R')

# Narendra Modi Tweets Analysis
# N_modi <- userTimeline("narendramodi",n=3200,includeRts = T)

# N_modi <- data.frame(read.csv("dataset/Narendrahashtag.csv", header = TRUE))
# N_modi.DF <- twListToDF(N_modi)
# N_modi.DF<-N_modi.DF[ ,-c(2,4,7:10,15:16)]
# N_modi.DF$text<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",N_modi.DF$text)

N_modi.DF <- data.frame(read.csv("dataset/Narendra_modijune.csv", header = TRUE))
N_modi.DF$text<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",N_modi.DF$text)
N_modi_HashTags<-extract.hashTag(N_modi.DF$text)
  
#tweets preprocessing
    tweet<-Preprocess.text(N_modi.DF$text)
    N_modi_Sent.matrix<-get_nrc_sentiment(tweet)
    N_modi_SM<-data.frame(colSums(N_modi_Sent.matrix))
    colnames(N_modi_SM)<- "Score"
    N_modi_SM<- cbind("Sentiment"=rownames(N_modi_SM),N_modi_SM)
    rownames(N_modi_SM)<-NULL
    
    ggplot(data=N_modi_SM,aes(x=Sentiment,y=Score))+
        geom_bar(aes(fill=Sentiment), stat = "identity",width = .9)+
        theme(legend.position = "none")+
        xlab("Sentiment")+ylab("Score")+
        ggtitle("\nSentiment Analysis of Narendra Modi Twitter Timeline Tweet's\n") 
    
    sent.value<- get_sentiment(tweet)
    N_modi.DF<-cbind(N_modi.DF,N_modi_Sent.matrix,sent.value)
    Tweet_Class<-c("Postive","Negative","Neutral")
    N_modi.DF<- mutate(N_modi.DF,class=sapply(N_modi.DF$sent.value,
    function(x){
        if(x==0){
            x<-"Neutral"
            return(x)
        }
       ifelse(x<0,"Negative","Postive")
    }))
    
    Class_NM<-cbind(Tweet_Class,"Score"=c(sum(str_count(N_modi.DF$class,"Postive")),
                                          sum(str_count(N_modi.DF$class,"Negative")),
                                          sum(str_count(N_modi.DF$class,"Neutral"))))
    Class_NM<-as.data.frame(Class_NM)
    ggplot(data=Class_NM,aes(x=Tweet_Class,y=Score))+
        geom_bar(aes(fill=Tweet_Class), stat = "identity",width = .8)+
        theme(legend.position = "none")+
        xlab("Sentiment")+ylab("Score")+
        labs(title="\nNarendra Modi Twitter Timeline Tweet's Class\n") 
    
    #write.csv(N_modi.DF,"Narendra_modijune.csv")
    #write.csv(N_modi_HashTags,"Narendrahashtag.csv")
    
# Rahul Gandhi Tweets Analysis
    # R_Gandhi <- userTimeline("RahulGandhi", n=3200,includeRts = T)
    # R_Gandhi.DF <- twListToDF(R_Gandhi)
    # R_Gandhi.DF<-R_Gandhi.DF[ ,-c(2,4,7:10,15:16)]
R_Gandhi.DF<- data.frame(read.csv("dataset/Rahul_gandhitimelin.csv", header = TRUE))
R_Gandhi.DF$text<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",R_Gandhi.DF$text)
R_Gandhi_HashTags<-extract.hashTag(R_Gandhi.DF$text)
    
    #tweets preprocessing
    tweet<-Preprocess.text(R_Gandhi.DF$text)
    R_Gandhi_Sent.matrix<-get_nrc_sentiment(tweet)
    R_Gandhi_SM<-data.frame(colSums(R_Gandhi_Sent.matrix))
    colnames(R_Gandhi_SM)<- "Score"
    R_Gandhi_SM<- cbind("Sentiment"=rownames(R_Gandhi_SM),R_Gandhi_SM)
    rownames(R_Gandhi_SM)<-NULL
    
    ggplot(data=R_Gandhi_SM,aes(x=Sentiment,y=Score))+
        geom_bar(aes(fill=Sentiment), stat = "identity",width = .9)+
        theme(legend.position = "none")+
        xlab("Sentiment")+ylab("Score")+
        labs(title="\nSentiment Analysis of Rahul Gandhi Twitter Timeline Tweet's\n") 
    
    R_Gandhi_sent.value<- get_sentiment(tweet)
    R_Gandhi.DF<-cbind(R_Gandhi.DF,R_Gandhi_Sent.matrix,R_Gandhi_sent.value)
    
    R_Gandhi.DF<- mutate(R_Gandhi.DF,class=sapply(R_Gandhi.DF$R_Gandhi_sent.value,
    function(x){
        if(x==0){
        x<-"Neutral"
        return(x)
        }
        ifelse(x<0,"Negative","Postive")
      }))

    Class_RG<-cbind(Tweet_Class,"Score"=c(sum(str_count(R_Gandhi.DF$class,"Postive")),
                        sum(str_count(R_Gandhi.DF$class,"Negative")),
                        sum(str_count(R_Gandhi.DF$class,"Neutral"))))
    Class_RG<-as.data.frame(Class_RG)
    ggplot(data=Class_RG,aes(x=Tweet_Class,y=Score))+
        geom_bar(aes(fill=Tweet_Class), stat = "identity",width = .8)+
        theme(legend.position = "none")+
        xlab("Sentiment")+ylab("Score")+
        labs(title="\nRahul Gandhi Twitter Timeline Tweet's Class\n") 
    
   # write.csv(R_Gandhi.DF,"Rahul_gandhitimelin.csv")
   # write.csv(R_Gandhi_HashTags,"R_Gandhi_HashTags.csv")

    
   # most.positive <- word.df[sent.value == max(sent.value)]
   # most.negative <- word.df[sent.value <= min(sent.value)]
