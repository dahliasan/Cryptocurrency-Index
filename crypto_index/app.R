#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(anytime)

# Get coin list via API request
url_cl <- 'http://coincap.io/map'
coinlist <- GET(url = url_cl)
coinlist_content <- content(coinlist)
coinlist_df <- map(coinlist_content, function(x){
  if('name' %in% names(x)) df <- data_frame(coinname = x$name, symbol = x$symbol) else
    df <- data_frame(coinname = NA, symbol = x$symbol)
  return(df)
}) %>% bind_rows()

# Select coins you want
# e.g. BTC, ETH, LTC
# coins <- c('BTC', 'ETH', 'LTC')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cryptocurrency Index"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
         selectInput(inputId = 'coins',
                     label = 'Select coins:', 
                     choices = coinlist_df$symbol,
                     selected = c('BTC', 'ETH', 'LTC'), 
                     multiple = TRUE, 
                     selectize = TRUE),
         submitButton("Update View", icon("refresh"))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("indexPlot"),
         textOutput('wrongDates')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # User selected reactive data
  filtered_data <- reactive({
    req(input$coins)
    
    # User selected data
    coins_df <- coinlist_df %>% filter(symbol %in% input$coins)
    
    # Get historical market cap, volume, and price up to present
    # in this case we get the 365 day history
    
    url_hist <- 'http://coincap.io/history/365day/'
    hist <- map(coins_df$symbol, ~GET(url = paste(url_hist, ., sep = '')))
    hist_content <- map(hist, ~content(.))
    hist <- map2(hist_content, split(coins_df, 1:nrow(coins_df)), function(x,y){
      df <- map(x$market_cap, ~ data_frame(date = .[[1]], marketcap = .[[2]])) %>% bind_rows()
      df$price <- map(x$price, ~.[[2]]) %>% unlist()
      df$vol <- map(x$volume, ~.[[2]]) %>% unlist()
      df$datetime <- anytime::anytime(df$date/1000, tz = 'GMT')
      df$date <- anytime::utcdate(df$date/1000, tz = 'GMT')
      df$symbol <- y$symbol
      df$coinname <- y$coinname
      df <- df %>% select(coinname, symbol, date, datetime, marketcap, price, vol)
      return(df)
    }) %>% bind_rows()
    
    # For some reason sometimes CoinCap API gives > 1 historical data for each coin
    # If we find there is > 1 historical data, we will choose the latest one
    data <- split(hist, hist$date)
    check <- map(data, ~nrow(.) == nrow(coins_df)) %>% reduce(c)
    wrong <- which(check != TRUE)
    if(length(wrong > 1)){
      for(i in length(wrong)){
        data[[wrong[i]]] <- data[[wrong[i]]] %>% group_by(symbol) %>% filter(datetime == max(datetime)) %>% ungroup()
      }
    }
    
    # Calculate weights of coins
    data <- map(data, function(x){
      x <- x %>% mutate(weight = marketcap/sum(marketcap))
    })
    
    # Calculate index
    index <- data %>% bind_rows() %>% group_by(date) %>% summarise(index = sum(price * weight *100) / sum(price))
    
    # Print dates that have wrong indexes and remove them. 
    check <- map(data, ~nrow(.) == nrow(coins_df)) %>% reduce(c)
    wrong <- which(check != TRUE)
    if(length(wrong) > 0){
      wrong_dates <- map(wrong, ~data[[.]]$date[1]) %>% reduce(c)
      index <- index %>% filter(!date %in% wrong_dates)
      out <- list(index, wrong_dates)
      names(out) <- c('index', 'wrong_dates')
      out
    } else index
  
    
  })

  # Instructions to make plot
  output$indexPlot <- renderPlot({
    index <- filtered_data()
    if(is.data.frame(index) == FALSE) index <- index$index 
    # Plot
    index %>% 
      ggplot(aes(x = date, y = index)) + 
      geom_line() + 
      scale_x_date(date_breaks = '1 month', date_labels = '%b %y') + 
      labs(title = paste(paste(input$coins, collapse = ' '), 'Index', sep = ' '))
  })

  # Print removed index dates if any 
  output$wrongDates <- renderText({
    index <- filtered_data()
    if(is.data.frame(index) == FALSE) paste('Dates removed due to missing data for some coin(s):', 
                                            paste(index$wrong_dates, collapse = ', '), sep = ' ') else "No missing data points."
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

