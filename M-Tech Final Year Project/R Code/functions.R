
# Function to extract Hashtags and there frquency

extract.hashTag = function(Tweets){
    
    hash.pattern = "#[[:alpha:]]+"
    have.hash = grep(x = Tweets, pattern = hash.pattern)
    
    hash.matches = gregexpr(pattern = hash.pattern,
                            text = Tweets[have.hash])
    extracted.hash = regmatches(x = Tweets[have.hash], m = hash.matches)
    
    Data = data.frame(table(tolower(unlist(extracted.hash))))
    colnames(Data) = c("tag","freq")
    Data = Data[order(Data$freq,decreasing = TRUE),]
    return(Data)
}

#function to pre Process tweets
Preprocess.text = function(Tweets){
    Tweets2<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",Tweets)
    Tweets2<-gsub("http[^[:blank:]]+","",Tweets2)
    Tweets2<-gsub("@\\w+","",Tweets2)
    Tweets2<-gsub('[[:punct:]]', '',Tweets2)
    #Tweets2<-gsub("[^[:alnum:]]","",Tweets2)
    text<-tolower(Tweets2)
    #text<-data.frame(text)
    return(text)
}



Temp<-as.data.frame(rep(Top_Hashtags[1],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter("#EveryChildALIVE",n=5000))
data1<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[2],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[2],n=5))
Data2<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[3],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[3],n=5))
Data3<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[4],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[4],n=5))
Data4<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[5],each=5))
colnames(Temp)<-"HashTag"
Data<-searchTwitter(Top_Hashtags[5],n=5)
Data5<-cbind(Temp,Data)
BJP_part<- rbind(data1,Data2,Data3,Data4)





Temp<-as.data.frame(rep(Top_Hashtags[1],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[1],n=5))
data1<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[2],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[2],n=5))
Data2<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[3],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[3],n=5))
Data3<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[4],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[4],n=5))
Data4<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[5],each=5))
colnames(Temp)<-"HashTag"
Data<-searchTwitter(Top_Hashtags[5],n=5)
Data5<-cbind(Temp,Data)
Con_part<- rbind(data1,Data2,Data3,Data4)




Temp<-as.data.frame(rep(Top_Hashtags[1],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[1],n=5))
data1<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[2],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[2],n=5))
Data2<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[3],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[3],n=5))
Data3<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[4],each=5))
colnames(Temp)<-"HashTag"
Data<-twListToDF(searchTwitter(Top_Hashtags[4],n=5))
Data4<-cbind(Temp,Data)

Temp<-as.data.frame(rep(Top_Hashtags[5],each=5))
colnames(Temp)<-"HashTag"
Data<-searchTwitter(Top_Hashtags[5],n=5)
Data5<-cbind(Temp,Data)
Aap_part<- rbind(data1,Data2,Data3,Data4)


#tweets preprocessing
#tweet<-Preprocess.Tweets(Bjp_tweets.DF$text)
#Sent.matrix<-get_nrc_sentiment(as.vector(tweet$text))
#sent.value<- get_sentiment(as.vector(tweet$text))
#Bjp_tweets.DF<-cbind(Bjp_tweets.DF,Sent.matrix,sent.value)

#Hash Tag Analysis


#tweets preprocessing
#tweet<-Preprocess.Tweets(Cong.DF$text)
#sent.value<- get_sentiment(as.vector(tweet$text))
#Cong.DF<-cbind(Cong.DF,Sent.matrix,sent.value)



#tweets preprocessing
#tweet<-Preprocess.Tweets(Aap_DF$text)
#Sent.matrix<-get_nrc_sentiment(as.vector(tweet$text))
#sent.value<- get_sentiment(as.vector(tweet$text))
#Aap_DF<-cbind(Aap_DF,Sent.matrix,sent.value)


# Arvind Kejriwal Tweets Analysis 
A_kej <- userTimeline("ArvindKejriwal", n=1000)
A_kej.no <- length(A_kej)
A_kej.DF <- twListToDF(A_kej)
A_kej.DF<-A_kej.DF[ ,-c(2,4,7:10,13:16)]
A_kej_HashTags<-extract.hashTag(A_kej.DF$text)

#tweets preprocessing
tweet<-Preprocess.Tweets(A_kej.DF$text)
Sent.matrix<-get_nrc_sentiment(as.vector(tweet$text))
sent.value<- get_sentiment(as.vector(tweet$text))
A_kej.DF<-cbind(A_kej.DF,Sent.matrix,sent.value)
A_kej.DF<- mutate(A_kej.DF,class=sapply(A_kej.DF$sent.value,
                                        function(x){
                                            if(x==0){
                                                x<-"Neutral"
                                                return(x)
                                            }
                                            ifelse(x<0,"Negative","Postive")
                                        }))


