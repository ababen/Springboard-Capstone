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
  tweet <- str_replace_all(tweet,"the"," ")
  tweet <- str_replace_all(tweet,"..."," ")
  tweet <- str_replace_all(tweet,'&amp'," ")
  # tweet1 <- grep('tweet',iconv(tweet,'latin1','ASCII',sub='tweet'))
  tweet  <- tweet[-tweet1]
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
 # rm(tweet1)
  return(x)
}