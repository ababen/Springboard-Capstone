# Unused Code

wordbank = c()
for(i in 1:nrow(data)){
  wordbank <- c(wordbank, strsplit(toString(tolower(data$tweet[i]))," ")[[1]])
}

wordbank <- data.frame(wordbank)
names(wordbank) <- c("word")
wordcounts <-
  wordbank %>%
  group_by(word) %>%
  summarize(freq=n()) %>%
  arrange(desc(freq))

wordcounts[] <- lapply(wordcounts, as.character)
wordcounts <- data.frame(wordcounts)

stopwords <- c("i","me","my","myself","we","our","ours","ourselves","you","your","yours","yourself","yourselves","he","him","his","himself","she","her","hers","herself","it","its","itself","they","them","their","theirs","themselves","what","which","who","whom","this","that","these","those","am","is","are","was","were","be","been","being","have","has","had","having","do","does","did","doing","a","an","the","and","but","if","or","because","as","until","while","of","at","by","for","with","about","against","between","into","through","during","before","after","above","below","to","from","up","down","in","out","on","off","over","under","again","further","then","once","here","there","when","where","why","how","all","any","both","each","few","more","most","other","some","such","no","nor","not","only","own","same","so","than","too","very","can","will","just","don't","should","now", "without", "used", "man", "didn't", "come", "give", "near", "today", "people", "city", "new", "de", "it's",  "see", "many", "may",  "back", "make", "via", "still", "even", "un", "want", "also", "take", "say", "get", "least", "another", "several", "since", "&amp;", "said")
stopwords <- data.frame(stopwords)
names(stopwords) <- c("word")

wc_ns <- subset(wordcounts, !(word %in% stopwords$word))
wc_ns <- wc_ns[-c(1,3,4,20),] # a few nonwords
wc_ns$word <- as.character(wc_ns$word)
wc_ns$word <- factor(wc_ns$word, levels=rev(unique(wc_ns$word))) 
wc_ns$freq <- as.numeric(wc_ns$freq)

wc_ns$word <- tm_map(wc_ns$word, removePunctuation)