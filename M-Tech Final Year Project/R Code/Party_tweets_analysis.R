library(syuzhet)
library(twitteR)
#Hash Tags used by BJP CONG & AAP official tweeter handle

#Bjp
Bjp_tweets<- userTimeline("BJPKarITCell",n=3200,includeRts = T)
Bjp_tweets.DF<-twListToDF(Bjp_tweets)
Bjp_HashTags<-extract.hashTag(Bjp_tweets.DF$text)
Top_Hashtags<-head(Bjp_HashTags$tag,10)
write.csv(Bjp_tweets.DF,"Bjp_tweets.DF.csv")
write.csv(Bjp_HashTags,"Bjp_HashTags.csv")
write.csv(Top_Hashtags,"Top_Hashtags.csv")
#cong
Cong.DF<-twListToDF(userTimeline("INCIndia",n=3200,includeRts = T))
cong_HashTags<-extract.hashTag(Cong.DF$text)
Top_Hashtags<-as.vector(head(cong_HashTags$tag,10))
write.csv(Cong.DF,"cong_tweets.DF.csv")
write.csv(cong_HashTags,"cong_HashTags.csv")
write.csv(Top_Hashtags,"Top_cong_Hashtags.csv")
#AAP
Aap_DF<-twListToDF(userTimeline("AamAadmiParty",n=1000,includeRts = T))
Aap_HashTag<- extract.hashTag(Aap_DF$text)
Top_Hashtags<-as.vector(head(Aap_HashTag$tag,5))
