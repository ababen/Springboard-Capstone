########################################################################
########################################################################
########################################################################

# library(igraph) <- Not being used

library(networkD3)
# Sample
# src <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
# target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
# networkData <- data.frame(src, target)
# attacks should be a vector of all dates of attacks. 86 in length

# Build a network of users and who they re-tweet.
library(networkD3)

username<- as.character(tweets.data$username)
dm_tweets <- as.character(tweets.data$tweets)
dm_txt = sapply(dm_tweets, function(x) x$getText())


tweets <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", '', tweets)
#here we get tweets which are retweets
# rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweets, ignore.case = TRUE)
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweets, ignore.case = TRUE, value = TRUE)

# create list to store user names
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

# for loop
for (i in 1:length(rt_patterns))
{ 
  # get tweet with retweet entity
  twit = tweets[[rt_patterns[i]]]
  # get retweet source 
  poster = str_extract_all(twit$getText(),
                           "(RT|via)((?:\\b\\W*@\\w+)+)") 
  #remove ':'
  poster = gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  # name of retweeting user 
  who_retweet[[i]] = rep(twit$getScreenName(), length(poster)) 
}

# unlist
who_post = unlist(who_post)
who_retweet = unlist(who_retweet)



# isis_sub <- isis[rt_patterns
# u <- isis_sub$username
rt_patterns <- gsub(':','',rt_patterns)
rt_patterns <- strsplit(rt_patterns, " ")
at_who <- lapply(rt_patterns, function(xx)xx[grepl("@[[:alnum:]]", xx)])
#here we remove the @ character
at_who <- str_extract_all(at_who,'(?<=@)\\w+')
username = unlist(username)
at_who <- unlist(at_who)
df <- cbind(username,at_who)
df <- unique(df)
View(df_df)
df_df <- as.data.frame(df)

simpleNetwork(df_df,charge = -500 , opacity = 0.6, zoom = T, fontSize = 10)

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
