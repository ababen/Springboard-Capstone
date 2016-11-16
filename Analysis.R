# Load dataset
rm(list=ls())
setwd("~/R/Springboard-Capstone")
tweets <- read.csv("how-isis-uses-twitter/tweets.csv")

tweet <- as.character(tweets$tweets) ## Not used

# Removing links, retweets, hashtags, @people, punctuations, numbers, emojis, non-english characters and spaces

clean.string <- function(x)
{
  library(stringr)
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet = gsub("#\\w+", " ", tweet)
  tweet = gsub("@\\w+", " ", tweet)
  tweet = gsub("[[:punct:]]", " ", tweet)
  tweet = gsub("[[:digit:]]", " ", tweet)
  tweet <- str_replace_all(tweet,"[^[:graph:]]"," ")
  tweet <- str_replace_all(tweet,'https'," ")
  tweet <- str_replace_all(tweet,'amp'," ")
  tweet1 <- grep('tweet',iconv(tweet,'latin1','ASCII',sub='tweet'))
  tweet  <- tweet[-tweet1]
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  rm(tweet1)
  return(x)
}

tweet = clean.string(tweet)

library(tm)
corp <- Corpus(VectorSource(tweet))
corp <- tm_map(corp,removeWords,c(stopwords('english'),stopwords('SMART'),'required','responded'))
tdm <- TermDocumentMatrix(corp) 
dtm <- DocumentTermMatrix(corp)

freq.terms <- findFreqTerms(tdm,lowfreq=250)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 250)
df <- data.frame(term = names(term.freq), freq = term.freq)

library("ggplot2")

### Analysis begins ###

ggplot(df, aes(x = term, y=freq, reorder(term.freq))) +
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title="Top 20 words in ISIS tweets")

library(wordcloud)
wordcloud(words = df$term, freq = df$freq, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

# Code from Amit

library(proxy) # Library required to use method = cosine in dist()

cosine <- function(x)
{
  cp <- crossprod(x)
  rtdg <- sqrt(diag(cp))
  cos <- cp / tcrossprod(rtdg)
  return(cos)
}

cleanMatrix = removeSparseTerms(dtm,0.98)
distMatrix = dist(scale(as.matrix(cleanMatrix)),method = "cosine")

# #The above code will give you a distance matrix between the main keywords

fit = hclust(distMatrix,method = 'ward.D2')

fit.cut = cutree(fit, k=2)

plot(fit)
plot(fit.cut)

## Pruning the clusters



# To Do
# 1. Calculate sentiment for each tweet.
#   a. Use keyword dictionaries to determine postivie/neutral/negative words.
# 2. Add up sentiment over time.
# 3. Plot time series
# 4. Create a distance function and pass to hclust()
# 5. Create Levenshtein distance/Jacobian distance from Document Matrix
# 6. Create a Dendrogram (if too many clusters), need to cut the roots of the tree - to get a better visualization.
# 7. Pruning the cluster: dont use too many clusters, find the optimal amount of clusters.
# 8. Use "k-means" for recommendations around those words/clusters.