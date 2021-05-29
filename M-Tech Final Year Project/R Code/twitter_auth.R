#libraries which is used in this script
library(twitteR)
library(ROAuth)
library(readr)
library(dplyr)

#connecting to api
download.file(url= "http://curl.haxx.se/ca/cacert.pem", destfile= "cacert.pem")
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

#twitter app cridentials of accessing twits
consumer_key<- "<>"
consumer_secret <- "<>"
access_token <- "372578663-<>"
access_secret <- "<>"

# Conection with HandShaking
Cred <- OAuthFactory$new(consumerKey = consumer_key,
                         consumerSecret=consumer_secret,
                         requestURL = reqURL,
                         accessURL = authURL)

Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl'))


save(Cred, file='twitter authentication.Rdata')
load('twitter authentication.Rdata')
registerTwitterOAuth(Cred)


#set up to Direct authenticate
setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)

#Translater API
