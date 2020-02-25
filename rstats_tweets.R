install.packages("formattable")
install.packages("data.table")
library(data.table)
library(formattable)
library(tidyverse)

search_tweet <- function(x,y) {
  search_tweets(x, n=y, include_rts = F)
}

rstats <- search_tweet("#rstats", 10000)

rstats %>% mutate(date = ymd(substr(created_at,1,10))) %>% group_by(date) %>%
  summarize(count=n(), retweets=sum(retweet_count), favourites= sum(favorite_count))

rstats_tab <- rstats %>% mutate(date = ymd(substr(created_at,1,10))) %>% 
  select(screen_name, date, text, retweet_count, favorite_count) %>%
  arrange(desc(retweet_count)) %>% slice(1:20) %>%
  formattable(align="l")
rstats_tab
library(htmlwidgets)
library(webshot)
saveWidget(rstats_tab, "temp.html")
webshot("temp.html", "temp.png")
