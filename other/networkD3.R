rm(list=ls())
setwd("~/R/Springboard-Capstone")
source("functions.R")
tweets.data <- read.csv("how-isis-uses-twitter/tweets.csv")
library(dplyr)
library(stringr)
library(twitteR)
library(igraph)

########################################################################
########################################################################
########################################################################

library(networkD3)

# Build a network of users and who they re-tweet.

username <- as.character(tweets.data$username)
tweets <- as.character(tweets.data$tweets)
retweets = data.frame(username, tweets)


# regular expressions to find retweets
grep("(RT|via)((?:\\b\\W*@\\w+)+)", retweets$tweets, ignore.case=TRUE, value=TRUE)

# which tweets are retweets
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", retweets$tweets, ignore.case=TRUE)

# show retweets (these are the ones we want to focus on)
retweets$tweets[rt_patterns]

# create list to store user names
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

# for loop
for (i in 1:length(rt_patterns))
{ 
  i = 1
  # get tweet with retweet entity
  twit = retweets$tweets[[rt_patterns[i]]]
  # get retweet source 
  poster = str_extract_all(retweets$tweets, "(RT|via)((?:\\b\\W*@\\w+)+)") 
  #remove ':'
  poster = gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  # name of retweeting user 
  who_retweet[[i]] = rep(retweets$username, length(poster)) 
}

# unlist
who_post = unlist(who_post)
who_retweet = unlist(who_retweet)

network = data.frame(who_post, who_retweet)

simpleNetwork(network = -500 , opacity = 0.6, zoom = T, fontSize = 10)

########################################################################
########################################################################
########################################################################

#---------------------- Things to try --------------------------
# ??? Diagram of #hashtags
# https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
# ??? # of accounts by month
# Classification: Retweets, URL (these can be changed to binary variables)
# Classifiers to try: MultinomialNB, KNeighbors, RandomForest GradientBoosting
# Kaggle: Sentiment Analysis: Which clergy do pro-ISIS fanboys quote the most and 
# which ones do they hate the most? Search the tweets for names of prominent clergy and 
# classify the tweet as positive, negative, or neutral and if negative, include the reasons why. 
# Examples of clergy they like the most: "Anwar Awlaki", "Ahmad Jibril", "Ibn Taymiyyah", "
# Abdul Wahhab". Examples of clergy that they hate the most: "Hamza Yusuf", "Suhaib Webb", 
# "Yaser Qadhi", "Nouman Ali Khan", "Yaqoubi".
