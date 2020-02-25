install.packages("formattable")
install.packages("data.table")
library(data.table)
library(formattable)
library(tidyverse)

search_tweet <- function(x,y) {
  search_tweets(x, n=y, include_rts = F)
}

tezos <- search_tweet("#tezos", 2000)

tezos %>% mutate(date = ymd(substr(created_at,1,10))) %>% group_by(date) %>%
                   summarize(retweets=sum(retweet_count), favourites= sum(favorite_count))

tezos %>% mutate(date = ymd(substr(created_at,1,10))) %>% group_by(date) %>%
  group_by(screen_name) %>%
  summarize(tweets = n(), retweets=sum(retweet_count), favourites= sum(favorite_count)) %>%
  arrange(desc(retweets)) %>%
  ggplot(aes(x=favourites, y=retweets)) + geom_point()

rstats <- search_tweet("#rstats", 10000)

rstats %>% mutate(date = ymd(substr(created_at,1,10))) %>% group_by(date) %>%
  summarize(count=n(), retweets=sum(retweet_count), favourites= sum(favorite_count))


customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

rstats_tab <- rstats %>% mutate(date = ymd(substr(created_at,1,10))) %>% 
  select(screen_name, date, text, retweet_count, favorite_count) %>%
  arrange(desc(retweet_count)) %>% slice(1:20) %>%
  formattable(align="l")
rstats_tab
library(htmlwidgets)
library(webshot)
saveWidget(rstats_tab, "temp.html")
webshot("temp.html", "temp.png")


rstats %>% 
  group_by(screen_name) %>%
  summarize(tweets = n(), retweets=sum(retweet_count), favourites= sum(favorite_count)) %>%
  mutate(impact = retweets+favourites, impact_per_tweet=impact/tweets) %>%
  arrange(desc(impact)) %>% slice(1:20) %>%
  ggplot(aes(x=impact, y=reorder(screen_name, impact), size=impact_per_tweet)) + geom_point() +
  scale_x_log10() + theme_minimal() +
  labs(title="Top 10 rstats tweeters, last 10,000 #rstats tweets", y="", x="impact (retweets + favourites)")
