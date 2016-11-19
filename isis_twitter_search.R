setwd("~/R/Springboard-Capstone")

# install.packages("base64enc")
# devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
# devtools::install_github("mkearney/rtweet")
# devtools::install_github("rstudio/httpuv")
# install.packages("twitteR")
# install.packages("rtweet")
# install.packages("ROAuth")
# install.packages("RCurl")
# install.packages("Rjsonio")
# install.packages("stringr")
# install.packages("httr")
# install.packages("httpuv")
# library(base64enc)
# library(twitteR)
library(rtweet)
# library(ROAuth)
library(RCurl)
library(RJSONIO)
library(stringr)
library(httr)
library(httpuv)

# Declare Twitter API Credentials
source("auth.R")

# Create Twitter Connection
twitter_token <- create_token(app = "springboard_isis_tweets", # whatever you named app
                              consumer_key = consumer_key_me,
                              consumer_secret = consumer_secret_me)

# Run Twitter Search. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).
tw <- search_tweets("Obamacare OR ACA OR 'Affordable Care Act' OR #ACA", n = 10000, token = twitter_token, lang = "en")

########## I searched for Obamacare as a proof of concept, because I was having issues targeting ISIS tweets.

# Transform tweets list into a data frame
write.csv(tw, file = "tw.csv")
