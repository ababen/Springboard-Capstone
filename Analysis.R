# Load dataset
rm(list=ls())
setwd("~/R/Springboard-Capstone")
source("functions.R")
tweets.data <- read.csv("how-isis-uses-twitter/tweets.csv")

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
View(tweets.fans)

#----------------------Remove this------------------------------
k = 0
wss = numeric(15)
for (k in 1:15){
  wss[k] = sum(kmeans(as.matrix(tweets.fans[ , 2:3]), centers = k, nstart = 25)$withinss)
  
}

set.seed(1)
ISIS_Cluster = kmeans(as.matrix(tweets.fans[ , 2:3]), 4, nstart =  20)
tweets.fans %>%
  ggplot(aes(n_tweets, n_followers)) +
    geom_point(aes(color = factor(ISIS_Cluster$cluster))) +
    geom_text(aes(label = ifelse(n_followers > 7000 | n_tweets > 7000, as.character(username), " ")), vjust = 1.5)
#----------------------Remove this------------------------------

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

tweets.users = tweets.data %>% 
  select(name, username, followers, tweets) %>%
  group_by(name, username) %>%
  summarize(n_followers = max(followers), n_tweets = n()) %>%
  filter(n_tweets > 100) %>%
  arrange(desc(n_tweets))
View(tweets.users)

# ??? Tweets by month, tweets by day, Most followed users and location, 

library(lubridate)

 tweets.data %>%
  mutate(time = parse_date_time(time, orders="%m/%d/%Y %H:%M"),
         year = format(time, "%Y"),
         month = format(time, "%m"),
         day = format(time, "%d"), 
         hour = format(time, "%H")) -> tweets.time

tweets.time %>%
   group_by(year) %>%
   ggplot(aes(month, fill=year)) +
   geom_bar(position="stack") +
   theme(axis.text=element_text(size=14), 
         axis.title=element_text(size=16), 
         legend.title=element_text(size=14),
         legend.key.size=unit(1,'cm'),
         legend.text=element_text(size=14)) 
 
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

#----------------------Remove this------------------------------
#
#k = 3
#kMeans = kmeans(tdm.sparse, k)
#round(kMeans$centers, digits = 3)

#for(i in 1:k){
#  cat(paste(" cluster", i, ": ", sep = ""))
#  s = sort(kMeans$centers[i, ], decreasing = T)
#  cat(names(s)[1:3], "\n")
#}
#----------------------Remove this------------------------------

library(igraph)


library(networkD3)

#---------------------- Things to try --------------------------
# ??? Diagram of words
# ??? Diagram of #hashtags
# https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
# ??? # of accounts by month
# Classification: Retweets, URL (these can be changed to binary variables)
# Classifiers to try: MultinomialNB, KNeighbors, RandomForest GradientBoosting
# Kaggle: Sentiment Analysis: Which clergy do pro-ISIS fanboys quote the most and which ones do they hate the most? Search the tweets for names of prominent clergy and classify the tweet as positive, negative, or neutral and if negative, include the reasons why. Examples of clergy they like the most: "Anwar Awlaki", "Ahmad Jibril", "Ibn Taymiyyah", "Abdul Wahhab". Examples of clergy that they hate the most: "Hamza Yusuf", "Suhaib Webb", "Yaser Qadhi", "Nouman Ali Khan", "Yaqoubi".