library(shiny)
library(twitteR)
library(rtweet)
library(httr)
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)
library(formattable)



api_key <- "bg6K7uek7nNNlV5h73PZCan3z"
api_secret_key <- "NioJmyJV0FOFy8g7PiDqzlTrLkhBVLLDZZA07F0pvJNRP0lmj3"
access_token <- "1718329370-k1EeTQXEwpfwH0NSzserwlZUzqMh3ZTsyOSPMC0"
access_token_secret <- "NOuaJtwrIDRnHiQbeOuHUQj8XEeVXQXE8KeHw5wGE6uL7"

## authenticate via web browser
token <- create_token(
    app = "ljhopkins_analysis",
    consumer_key = api_key,
    consumer_secret = api_secret_key,
    access_token = access_token,
    access_secret = access_token_secret)

setup_twitter_oauth(
    api_key, api_secret_key, access_token, access_token_secret)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Top tweets"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("search_term",
                      "Search term"),
            submitButton("submit"),
            sliderInput("bins",
                        "Number of tweets to display:",
                        min = 5,
                        max = 50,
                        value = 5)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(type="tabs",
            tabPanel("Top tweets", 
                     tableOutput("tweet_tab")),
            tabPanel("Top tweeters", plotOutput("tweeters")),
            tabPanel("Locations")
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    search_tweet <- function(x,y) {
        search_tweets(x, n=y, include_rts = F)
    }
    
    twt <- search_tweets(input$search_term,1000)
    
    output$tweet_tab <- renderTable({
        
        twt %>% mutate(date = ymd(substr(created_at,1,10))) %>% 
        select(screen_name, text, retweet_count, favorite_count) %>%
        arrange(desc(retweet_count)) %>% slice(1:input$bins) %>%
        formattable(align="l")
        })
    
    output$tweeters <- renderPlot({
        
        twt %>% group_by(screen_name) %>% summarise(count=n()) %>% 
            ggplot(aes(x=reorder(screen_name, count),y=count)) + 
            geom_point() + geom_segment(aes(x=x,xend=x,y=0,yend=y)) + coord_flip() + theme_minimal() +
            labs(x="count", y="" )
    
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
