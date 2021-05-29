#library Used
library(syuzhet)
library(ggplot2)
library(twitteR)

#------------------User tweets analysis for a perticular STRING--------------------------#

#source('C:/ThesisProject/R_Script/twitter_auth.R) for twitter API

#Tweet extraction using a string, i'm taking @narendramodi as string and tweets are in english only*
User_Data <-searchTwitter("Karnataka Election",n=10000 )
User_Data <-strip_retweets(User_Data)
User_DF<-twListToDF(User_Data)
#Text cleaning function preproccesing
Prep.text = function(Tweets){
    Tweets2<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",Tweets)
    Tweets2<-gsub("http[^[:blank:]]+","",Tweets2)
    Tweets2<-gsub("@\\w+","",Tweets2)
    Tweets2<-gsub('[[:punct:]]', '',Tweets2)
    text<-tolower(Tweets2)
    return(text)
}
User_tweet <- Prep.text(User_DF$text)
#Sentiment analysis and ploting data
User_Data_Sentiment_emostion <- get_nrc_sentiment(User_tweet)
plot_data<- data.frame(colSums(User_Data_Sentiment_emostion[,]))
names(plot_data)<- "Score"
plot_data<- cbind("Sentiment"=rownames(plot_data),plot_data)
rownames(plot_data)<- NULL
ggplot(data=plot_data,aes(x=Sentiment,y=Score))+
       geom_bar(aes(fill=Sentiment), stat = "identity" )+
       theme(legend.position = "none")+
       xlab("Sentiment")+ylab("Score")+ggtitle("Total sentiment score for @narendramodi used in tweets") 
User_Data_Sentiment_Score<- get_sentiment(User_tweet)
User_data_string<-cbind(User_DF,User_Data_Sentiment_emostion,User_Data_Sentiment_Score)
Tweet_Class<-c("Postive","Negative","Neutral")
User_data_string<-mutate(User_data_string,class=sapply(User_data_string$User_Data_Sentiment_Score,
                              function(x){
                                  if(x==0){
                                      x<-"Neutral"
                                      return(x)
                                  }
                                  ifelse(x<0,"Negative","Postive")
                              }))
write.csv(User_data_string,"@narendramodi_10_User_data_string.csv")
