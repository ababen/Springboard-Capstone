# Load dataset
rm(list=ls())
setwd("~/R/Springboard-Capstone")
source("functions.R")
tweets.data <- read.csv("how-isis-uses-twitter/tweets.csv")
  eig
# Dataset from https://github.com/Dpananos/How-Isis-Uses-Twitter/blob/master/attack_dates.csv
attacks.data <- read.csv("other/attack_dates.csv")

library(tm)

# Removing links, retweets, hashtags, @people, punctuations, numbers, emojis, non-english 
#characters and spaces
tweet <- as.character(tweets.data$tweets)
tweet = tolower(tweet)
tweet.clean = clean.string(tweet)

corpus <- Corpus(VectorSource(tweet))
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), stopwords('SMART'), 
                                          'required', 'responded'))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
tdm <- TermDocumentMatrix(corpus) 
dtm <- DocumentTermMatrix(corpus)

tdm.sparse <- removeSparseTerms(tdm, 0.98)
tweet.freq_terms <- findFreqTerms(tdm, lowfreq = 250)
tweet.term_freq <- rowSums(as.matrix(tdm))
tweet.term_freq <- subset(tweet.term_freq, tweet.term_freq >= 250)
tdm.freq <- data.frame(term = names(tweet.term_freq), freq = tweet.term_freq)

# Plotting 

library(ggplot2)
library(plotly)

### Analysis begins ###

library(dplyr)

tweets.data %>% 
  select(username, followers, tweets) %>%
  group_by(username) %>%
  summarize(n_followers = max(followers), n_tweets = n()) %>%
  arrange(desc(n_tweets)) %>%
  top_n(10, n_tweets) -> tweets.fans 
View(tweets.fans)

library(grid)

# Show the disparity between volume of tweets

tweets.fans %>%
  ggplot(aes(x = n_tweets, y = n_followers)) +
  geom_point(color = "Blue", shape = 1, size = 6, stroke = 1.25, aes()) + 
  geom_text(aes(label = ifelse(n_followers > 5000 | n_tweets > 500, as.character(username), " ")), vjust = 2) +
  scale_fill_manual() +
  xlab("No. of Tweets") +
  ylab("No. of Followers") +
  ggtitle("Top users") +
  theme_light()

# Look up all #hashtags by frequency

library(stringr)

tweets.data %>%
  mutate(tweets.hash = str_extract(tweets, "#\\w+")) %>%
  select(tweets.hash) %>%
  filter(!is.na(tweets.hash)) %>%
  group_by(tweets.hash) %>%
  summarize(n_hashtags = n()) %>%
  arrange(desc(n_hashtags)) %>%
  head(26) -> tweets.hash26
  tweets.hash = head(tweets.hash26, 20)
View(tweets.hash)

# Look up all @users (including name) by frequency

tweets.data %>% 
  select(name, username, followers, tweets) %>%
  group_by(name, username) %>%
  summarize(n_followers = max(followers), n_tweets = n()) %>%
  filter(n_tweets > 100) %>%
  arrange(desc(n_tweets)) %>%
  head(20) -> tweets.users
View(tweets.users)

# ??? Tweets by month, tweets by day, Most followed users and location, 

# Going to cros-reference tweets with terrorist attacks

library(lubridate)

attacks.data$MonthDayYear = parse_date_time(attacks.data$Date, orders="%m/%d/%Y")
 
 attacks.data %>%
  mutate(year = format(MonthDayYear, "%Y"), 
         month = format(MonthDayYear, "%m"), 
         day = format(MonthDayYear, "%d"))-> attacks.data

 tweets.data %>%
   mutate(time = parse_date_time(time, orders="%m/%d/%Y %H:%M"),
          year = format(time, "%Y"),
          month = format(time, "%m"),
          day = format(time, "%d"), 
          hour = format(time, "%H")) -> tweets.time 
 
 ###################################################
 ####### To Do: Add title #########
 ####### To Do: I would love to redo this to be more timeseries #########
 
tweets.time %>%
   group_by(year) %>%
   ggplot(aes(month, fill = year)) +
   geom_bar(position = "stack") +
   ggtitle("Tweet Frequency") +
   theme(axis.text = element_text(size = 14), 
         axis.title = element_text(size = 16), 
         legend.title=element_text(size = 14),
         legend.key.size=unit(1,'cm'),
         legend.text=element_text(size = 14)) 

attacks.data %>%
  group_by(year) %>%
  filter(year > 2014) %>%
  ggplot(aes(x = month, y = day, fill = year)) +
  geom_point() +
  ggtitle("Major Attacks") +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.title=element_text(size = 14),
        legend.key.size=unit(1,'cm'),
        legend.text=element_text(size = 14)) 

tdm.freq %>%
  arrange(term) %>%
  ggplot(aes(x = term, y = freq)) + 
    geom_bar(stat = "identity", fill = "Light Blue") + 
    coord_flip() + 
    labs(title = "Top 20 words in ISIS tweets") +
    theme_classic() -> plot_top20words
  ggplotly(plot_top20words)

library(wordcloud)
wordcloud(words = tdm.freq$term, freq = tdm.freq$freq, random.order = FALSE, rot.per = 0.25, 
          colors = brewer.pal(8, "Dark2"))

### Code from Amit
tdm.sparse = removeSparseTerms(tdm, 0.98)

# #The above code will give you a distance matrix between the main keywords
distMatrix = dist(scale(as.matrix(tdm.sparse)),method = "cosine")

fit = hclust(distMatrix,method = 'ward.D2')
fit.cut = cutree(fit, k = 2)
plot(fit)

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