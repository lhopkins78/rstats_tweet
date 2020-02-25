#load packages
library(formattable)
library(tidyverse)
library(rtweet)
library(httr)
library(tidyverse)

#log in to Twitter API
api_key <- "xxxxxxx"
api_secret_key <- "xxxxxxx"
access_token <- "xxxxxxx"
access_token_secret <- "xxxxxxx"

## authenticate via web browser
token <- create_token(
  app = "xxxxxxx",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

setup_twitter_oauth(
  api_key, api_secret_key, access_token, access_token_secret)

#a function to download tweet data
search_tweet <- function(x,y) {
  search_tweets(x, n=y, include_rts = F)
}

#fetch twitter data
rstats <- search_tweet("#rstats", 10000)

#table of tweets - top 20
rstats_tab <- rstats %>% mutate(date = ymd(substr(created_at,1,10))) %>% 
  select(screen_name, date, text, retweet_count, favorite_count) %>%
  arrange(desc(retweet_count)) %>% slice(1:20) %>%
  formattable(align="l")
rstats_tab
library(htmlwidgets)
library(webshot)
saveWidget(rstats_tab, "temp.html")
webshot("temp.html", "temp.png")
