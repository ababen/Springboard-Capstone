# Load dataset
rm(list=ls())
setwd("~/R/Springboard-Capstone")
source("functions.R")
tweets.data <- read.csv("how-isis-uses-twitter/tweets.csv")

# Dataset from https://github.com/Dpananos/How-Isis-Uses-Twitter/blob/master/attack_dates.csv
attacks.data <- read.csv("other/attack_dates.csv")

library(tm)

# Removing links, retweets, hashtags, @people, punctuations, numbers, emojis, non-english characters and spaces
tweet <- as.character(tweets.data$tweets)
tweet = tolower(tweet)
tweet.clean = clean.string(tweet)

corpus <- Corpus(VectorSource(tweet))
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), stopwords('SMART'), 'required', 'responded'))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
tdm <- TermDocumentMatrix(corpus) 
dtm <- DocumentTermMatrix(corpus)
tdm.sparse <- removeSparseTerms(tdm, 0.98)

plot(tdm.sparse)
# install.packages("Rgraphviz") <-remove this

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
  top_n(10, n_tweets) -> tweets.fans ## Add column here with percent of total.
plot.data.frame(tweets.fans)

#----------------------Remove this------------------------------
k = 0
wss = numeric(15)
for (k in 1:15){
  wss[k] = sum(kmeans(as.matrix(tweets.fans[ , 2:3]), centers = k, nstart = 25)$withinss)
  
}


#----------------------Remove this------------------------------

# Most tweets and influencer
####### To Do: Insert Caption, get more names to appear #########
set.seed(1)
ISIS_Cluster = kmeans(as.matrix(tweets.fans[ , 2:3]), 4, nstart =  20)
tweets.fans %>%
  ggplot(aes(n_tweets, n_followers)) +
  geom_point(aes(color = factor(ISIS_Cluster$cluster))) +
  geom_text(aes(label = ifelse(n_followers > 7000 | n_tweets > 7000, as.character(username), " ")), vjust = 1.5)

# Look up all #hashtags by frequency

library(stringr)

tweets.hash = tweets.data %>%
  mutate(tweets.hash = str_extract(tweets, "#\\w+")) %>%
  select(tweets.hash) %>%
  filter(!is.na(tweets.hash)) %>%
  ## unnest(hash) %>%
  group_by(tweets.hash) %>%
  summarize(n_hashtags = n()) %>%
  arrange(desc(n_hashtags))
tweets.hash %>%
  head(20)

# Look up all @users by frequency

###################################################
####### To Do: Show only the top few rows #########

tweets.data %>% 
  select(name, username, followers, tweets) %>%
  group_by(name, username) %>%
  summarize(n_followers = max(followers), n_tweets = n()) %>%
  filter(n_tweets > 100) %>%
  arrange(desc(n_tweets)) -> tweets.users
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
  ggplot(aes(x = month, y = day, fill = year)) +
  geom_point() +
  ggtitle("Major Attacks") +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.title=element_text(size = 14),
        legend.key.size=unit(1,'cm'),
        legend.text=element_text(size = 14)) 

ggplot(tdm.freq, aes(x = term, y = freq)) + #reorder(term.freq)
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Top 20 words in ISIS tweets") +
  theme_classic() -> plot_top20words
ggplotly(plot_top20words)

library(wordcloud)
wordcloud(words = tdm.freq$term, freq = tdm.freq$freq, random.order = FALSE, rot.per = 0.25, 
          colors = brewer.pal(8, "Dark2"))

### Code from Amit

# library(tm) <-remove this

tdm.sparse = removeSparseTerms(tdm, 0.98)

# #The above code will give you a distance matrix between the main keywords
distMatrix = dist(scale(as.matrix(tdm.sparse)),method = "cosine")

fit = hclust(distMatrix,method = 'ward.D2')
fit.cut = cutree(fit, k = 2)
plot(fit)

# library(igraph) <- Not being used

library(networkD3)
# attacks should be a vector of all dates of attacks. 86 in length
attacks = attacks.data$MonthDayYear
src <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")

tweets = as.POSIXct(strptime(tweets.time$time, "%Y-%m-%d"))
target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")

networkData <- data.frame(src, target)
networkData <- data.frame(attacks, tweets)

# inner_join(src, target, by = src) I tried all sorts of combinations and different ways including plyr

simpleNetwork(networkData, zoom = TRUE)

# with sans-serif 
# simpleNetwork(networkData, fontFamily = "sans-serif")

# with another font 
# simpleNetwork(networkData, fontFamily = "fantasy")

#---------------------- Things to try --------------------------
# ??? Diagram of #hashtags
# https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
# ??? # of accounts by month
# Classification: Retweets, URL (these can be changed to binary variables)
# Classifiers to try: MultinomialNB, KNeighbors, RandomForest GradientBoosting
# Kaggle: Sentiment Analysis: Which clergy do pro-ISIS fanboys quote the most and which ones do they hate the most? Search the tweets for names of prominent clergy and classify the tweet as positive, negative, or neutral and if negative, include the reasons why. Examples of clergy they like the most: "Anwar Awlaki", "Ahmad Jibril", "Ibn Taymiyyah", "Abdul Wahhab". Examples of clergy that they hate the most: "Hamza Yusuf", "Suhaib Webb", "Yaser Qadhi", "Nouman Ali Khan", "Yaqoubi".
