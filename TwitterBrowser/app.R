#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rtweet)
library(tidyverse)
token <- readRDS("~/twitter_token.rds")
#blogdown::shortcode('tweet', favorites[[1,"status_id"]])


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Twitter <3 Browser"),
   
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        textInput("user", "Twitter handle: ", value = "DrAmandaRP"),
        
         sliderInput("n",
                     "How many of the user's most recent Twitter likes would you like to retrieve?",
                     min = 1,
                     max = 3000,
                     value = 100)
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
   
  output$favoritesTable <- renderDataTable({
    
    favorites <- get_favorites(input$user, n = input$n, since_id = "469871753381302272", token = token)
    
    favorites %>% 
      select(status_id, created_at, text, hashtags, name, screen_name) %>%
      mutate(hashtags = replace(hashtags, is.na(hashtags), " ")) %>%
      mutate(hashtags = map(hashtags, ~paste0(., collapse = " "))) %>%
      mutate(Link = str_c("<a href='https://twitter.com/", screen_name, "/status/", status_id, "'>link</a>")) %>%
      select(-status_id) %>%
      arrange(desc(created_at)) %>%
      mutate(created_at = as.character(created_at)) %>%
      rename(Date = created_at, Text = text, Hashtags = hashtags, Name = name, Handle = screen_name)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

