# Load dataset
tweet <- read.csv("~/Springboard-Capstone/how-isis-uses-twitter/tweets.csv")

tweet <- as.character(tweet$tweets)

library(stringr)
# Removing links, retweets, hashtags, @people, punctuations, numbers, emojis, non-english characters and spaces
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
# Analysis begins
ggplot(df, aes(x = term, y=freq, reorder(term.freq))) +
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title="Top 20 words in ISIS tweets")

library(wordcloud)
wordcloud(words = df$term, freq = df$freq, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

# Force commit