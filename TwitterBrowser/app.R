#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#TODO: Add a tab for bookmarks (when that API is ready)

library(shiny)
library(rtweet)
library(tidyverse)
library(tidytext)
library(wordcloud2)
token <- readRDS("twitter_token.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Amanda's Twitter <3 Browser"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("n",
                  "How many of the user's most recent Twitter likes would you like to retrieve?",
                  min = 1,
                  max = 3000,
                  value = 100),
      wordcloud2Output("favs_wordcloud")
    ),
    
    
    mainPanel(
      #img(src='twitterlogo_small.png')
      div(img(src='twitterlogo_small.png'), style="text-align: center;"),
      dataTableOutput("favoritesTable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  favorites <- reactive({
     get_favorites("DrAmandaRP", n = input$n, since_id = "469871753381302272", token = token)
  })
  
  output$favoritesTable <- renderDataTable({
  
    #get_favorites("DrAmandaRP", n = input$n, since_id = "469871753381302272", token = token)
    
    favorites() %>% 
      select(status_id, created_at, text, hashtags, name, screen_name) %>%
      mutate(hashtags = replace(hashtags, is.na(hashtags), " ")) %>%
      mutate(hashtags = map(hashtags, ~paste0(., collapse = " "))) %>%
      mutate(Link = str_c("<a href='https://twitter.com/", screen_name, "/status/", status_id, "'>link</a>")) %>%
      select(-status_id) %>%
      arrange(desc(created_at)) %>%
      mutate(created_at = as.character(created_at)) %>%
      rename(Date = created_at, Text = text, Hashtags = hashtags, Name = name, Handle = screen_name)
  })
  
  output$favs_wordcloud <- renderWordcloud2({
    
    #favorites <- get_favorites("DrAmandaRP", n = input$n, since_id = "469871753381302272", token = token)
    
    favorites() %>% 
        select(text) %>%
        unnest_tokens(word, text) %>%
        count(word, sort = TRUE) %>%
        anti_join(stop_words) %>%
        filter(!word %in% c("t.co", "https", "http")) %>%
        wordcloud2(shape = "rectangle")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
