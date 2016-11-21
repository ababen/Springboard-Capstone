# Load dataset
rm(list=ls())
setwd("~/R/Springboard-Capstone")
source("clean.R")
tweets.data <- read.csv("how-isis-uses-twitter/tweets.csv")

tweet <- as.character(tweets.data$tweets)

library(tm)

# Removing links, retweets, hashtags, @people, punctuations, numbers, emojis, non-english characters and spaces
tweet.clean = clean.string(tweet)

corpus <- Corpus(VectorSource(tweet.clean))
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), stopwords('SMART'), 'required', 'responded'))
corpus <- tm_map(corpus, removeNumbers, removePunctuation, removeSparseTerms, stripWhitespace)
tdm <- TermDocumentMatrix(corpus) 
dtm <- DocumentTermMatrix(corpus)

tweet.freq_terms <- findFreqTerms(tdm, lowfreq = 250)
tweet.term_freq <- rowSums(as.matrix(tdm))
tweet.term_freq <- subset(tweet.term_freq, tweet.term_freq >= 250)
tdm.freq <- data.frame(term = names(tweet.term_freq), freq = tweet.term_freq)

# Plotting 

# plot(tdm, terms = sample(Terms(tdm), 20), corThreshold = 0.7) >- This does not work in 3.3.2

library(ggplot2)
library(plotly)

### Analysis begins ###

# !!!!!!!!!!!!! I am having problems removing "the", "..." and "this" from this dataset.

library(dplyr)

tweets.fans = tweets.data %>% 
  select(username, followers, tweets) %>%
  group_by(username) %>%
  summarize(n_followers = max(followers), n_tweets = n())

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

tweets.time = tweets.data$time
tweets.time = mdy_hm(tweets.time)
# tweets.time = as.data.frame(tweets.data$time)
tweets.time$year = year(tweets.time)
tweets.time$month = month(tweets.time) 
tweets.time$day = day(tweets.time)

# tweets.time = mutate(tweets.time, parse_date_time(tweets.time, orders = "%m/%d/%Y %H:%M"), year = format(tweets.data$time, "%Y"), month = format(tweets.data$time, "%m"), day = format(tweets.data$time, "%d"), hour = format(tweets.data$time, "%H"))

ggplot(tweets.time, aes(day(tweets.time))) + 
    geom_bar(fill = "lightblue") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ggtitle("# of tweets published daily") + xlab("") + ylab("")

ggplotly(plot.time)

# ??? # of accounts by month

# ??? Diagram of words

# ??? Diagram of #hashtags

# Classification: Retweets, URL (these can be changed to binary variables)
# Classifiers to try: MultinomialNB, KNeighbors, RandomForest GradientBoosting

# !!!!!!!!!!!!! I am having problems removing "the", "..." and "this" from this dataset.

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

library(tm)

tdm.sparse = removeSparseTerms(tdm, 0.98)

# #The above code will give you a distance matrix between the main keywords
distMatrix = dist(scale(as.matrix(tdm.sparse)),method = "cosine")

library(igraph)

fit = hclust(distMatrix,method = 'ward.D2')
fit.cut = cutree(fit, k = 2)
plot(fit)
plot(fit.cut)

k = 8
kMeans = kmeans(tdm.sparse, k)
round(kMeans$centers, digits = 3)

for(i in 1:k){
  cat(paste(" cluster", i, ": ", sep = ""))
  s = sort(kMeans$centers[i, ], decreasing = T)
  cat(names(s)[1:3], "\n")
}

# ??? Look up all #hashtags by frequency
# ??? Look up all @users by frequency

# ??? Tweets by month, tweets by day, Most followed users and location, 

# ??? Diagram of words
# ??? Diagram of #hashtags

# Classification: Retweets, URL (these can be changed to binary variables)

# Classifiers to try: MultinomialNB, KNeighbors, RandomForest GradientBoosting

# AMB: I dont now where I was going with this.
# Train the model
## mat = as.matrix(tdm)
## classifier = naiveBayes(mat[1:10,], as.factor(tdm[1:10,2]))

# Test the validity
## predicted = predict(classifier, mat[11:15,]); predicted
## table(tdm[11:15, 2], predicted)
## recall_accuracy(tdm[11:15, 2], predicted)

# Show difference Cross Validation Mean Scores between Naive Bayes, KN Neighbors, Random Forrest and Gradient Boosting

# Develop an ISIS vocabulary for sentiment analysis.

# Kaggle: Sentiment Analysis: Which clergy do pro-ISIS fanboys quote the most and which ones do they hate the most? Search the tweets for names of prominent clergy and classify the tweet as positive, negative, or neutral and if negative, include the reasons why. Examples of clergy they like the most: "Anwar Awlaki", "Ahmad Jibril", "Ibn Taymiyyah", "Abdul Wahhab". Examples of clergy that they hate the most: "Hamza Yusuf", "Suhaib Webb", "Yaser Qadhi", "Nouman Ali Khan", "Yaqoubi".

# Next step: work on using Predictive Coding and SentimentAnalysis.R on the project. #



# To Do
# 1. Calculate sentiment for each tweet.
#   a. Use keyword dictionaries to determine postivie/neutral/negative words.
# 2. Add up sentiment over time.
# 3. Plot time series
