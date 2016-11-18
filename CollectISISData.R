setwd("~/R/Springboard-Capstone")

install.packages("base64enc")
# devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
devtools::install_github("mkearney/rtweet")
devtools::install_github("rstudio/httpuv")
install.packages("twitteR")
install.packages("rtweet")
install.packages("ROAuth")
install.packages("RCurl")
install.packages("Rjsonio")
install.packages("stringr")
install.packages("httr")
install.packages("httpuv")
library(base64enc)
library(twitteR)
library(rtweet)
library(ROAuth)
library(RCurl)
library(RJSONIO)
library(stringr)
library(httr)
library(httpuv)

# Declare Twitter API Credentials
source("auth.R")

twitter_token <- create_token(app = "springboard_isis_tweets", # whatever you named app
                              consumer_key = "zjxkSdGnqDCaJy0m5fd7U9y08",
                              consumer_secret = "8rVJS1SopYIVqHDj0qSk8pFPfmbw1pLbesP1C4uWnYkxbmy7Hs")




# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

setup_twitter_oauth(api_key, api_secret, token, token_secret, credentials_file=NULL)

myapp <- oauth_app("twitter", key = "zjxkSdGnqDCaJy0m5fd7U9y08", secret = "8rVJS1SopYIVqHDj0qSk8pFPfmbw1pLbesP1C4uWnYkxbmy7Hs")

twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

req <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json",
           config(token = twitter_token))
stop_for_status(req)
content(req)

# Run Twitter Search. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).

tweets <- searchTwitter("Obamacare OR ACA OR 'Affordable Care Act' OR #ACA", n=100, lang="en", since="2014-08-20")

# Transform tweets list into a data frame
tweets.df <- twListToDF(tweets)

