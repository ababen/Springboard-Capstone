# Load dataset
rm(list=ls())
setwd("~/R/Springboard-Capstone")
source("clean.R")
tweets <- read.csv("how-isis-uses-twitter/tweets.csv")

tweet <- as.character(tweets$tweets)

# Removing links, retweets, hashtags, @people, punctuations, numbers, emojis, non-english characters and spaces
tweet = clean.string(tweet)

library(tm)
corpus <- Corpus(VectorSource(tweet))
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), stopwords('SMART'), 'required', 'responded'))
corpus <- tm_map(corpus, removeWords, c("the","&amp","amp"))
tdm <- TermDocumentMatrix(corpus) 
dtm <- DocumentTermMatrix(corpus)
tdm = removeSparseTerms(dtm, 0.98)

freq.terms <- findFreqTerms(tdm, lowfreq = 250)
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


distMatrix = dist(scale(as.matrix(cleanMatrix)),method = "cosine")

# #The above code will give you a distance matrix between the main keywords

fit = hclust(distMatrix,method = 'ward.D2')

fit.cut = cutree(fit, k=2)

# plot(fit) # Having performance issues with plotting.
# plot(fit.cut)

################# Next step: work on using Predictive Coding and SentimentAnalysis.R on the project. ##########
################# Trying to download Twitter data for ISIS. ###################################################

## Pruning the clusters

# AMB: I dont now where I was going with this.
# Train the model
## mat = as.matrix(tdm)
## classifier = naiveBayes(mat[1:10,], as.factor(tdm[1:10,2]))

# Test the validity
## predicted = predict(classifier, mat[11:15,]); predicted
## table(tdm[11:15, 2], predicted)
## recall_accuracy(tdm[11:15, 2], predicted)

# To Do
# 1. Calculate sentiment for each tweet.
#   a. Use keyword dictionaries to determine postivie/neutral/negative words.
# 2. Add up sentiment over time.
# 3. Plot time series
# 4. Create a distance function and pass to hclust()
# 5. Create Levenstein distance/Jarcovian distance from Document Matrix
# 6. Create a Dendrogram (if too many clusters), need to cut the roots of the tree - to get a better visualization.
# 7. Pruning the cluster: dont use too many clusters, find the optimal amount of clusters.
# 8. Use "k-means" for recommendations around those words/clusters.