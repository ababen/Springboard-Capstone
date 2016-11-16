library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(stringr)
library(tm)
library(igraph)
library(networkD3)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#system("ls ../input")
###########################################################################################
#Here we try to find the most frequently occuring terms in the tweets
#The tweets are cleaned by removing links, punctautions,@people, numbers and graphics
# We also remove non english words from the text
#The text is then converted to a corpus to figure out the frequent terms that occur
setwd("~/R/Springboard-Capstone")
isis <- read_csv('Springboard-Capstone/how-isis-uses-twitter/tweets.csv')
tweet <- as.character(isis$tweets)
#removing links
tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
#retweet
tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
# removing hashtags
tweet = gsub("#\\w+", " ", tweet)
# removing @people
tweet = gsub("@\\w+", " ", tweet)
#removing punctuations
tweet = gsub("[[:punct:]]", " ", tweet)
#removing numbers
tweet = gsub("[[:digit:]]", " ", tweet)
#removing emojis
tweet<-str_replace_all(tweet,"[^[:graph:]]"," ")
tweet <- str_replace_all(tweet,'https'," ")
tweet <- str_replace_all(tweet,'amp'," ")
# removing non-english characters
tweet1 <- grep('tweet',iconv(tweet,'latin1','ASCII',sub='tweet'))
tweet<-tweet[-tweet1]
#removing spaces
tweet = gsub("[ \t]{2,}", " ", tweet)
tweet = gsub("^\\s+|\\s+$", "", tweet)
tweet = tolower(tweet)
corp <- Corpus(VectorSource(tweet))
corp <- tm_map(corp,removeWords,c(stopwords('english'),stopwords('SMART'),'required','responded'))
DocTermMatrix <- TermDocumentMatrix(corp) 

freq.terms <- findFreqTerms(DocTermMatrix,lowfreq=250)
term.freq <- rowSums(as.matrix(DocTermMatrix))
term.freq <- subset(term.freq, term.freq >= 250)
df <- data.frame(term = names(term.freq), freq = term.freq)



cosine <- function(x)
{
  cp <- crossprod(x)
  rtdg <- sqrt(diag(cp))
  cos <- cp / tcrossprod(rtdg)
  return(cos)
}


library(igraph)
cleanMatrix = removeSparseTerms(DocTermMatrix,0.98)
distMatrix = dist(scale(cleanMatrix),method = "cosine")

library(slam)
distMatrix <- crossprod_simple_triplet_matrix(tdm)/(sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2))))
## http://stackoverflow.com/questions/29750519/r-calculate-cosine-distance-from-a-term-document-matrix-with-tm-and-proxy

#The above code will give you a distance matrix between the main keywords

fit = hclust(distMatrix,method = "ward.D2")
distMatrix
