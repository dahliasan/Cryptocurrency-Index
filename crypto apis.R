library(tidyverse)
library(httr)
library(xml2)
library(jsonlite)
library(anytime)


# Load API ----------------------------------------------------------------

# Crypto Compare API request
url_cl <- 'https://www.cryptocompare.com/api/data/coinlist/'
coinlist <- GET(url = url_cl)
coinlist_content <- content(coinlist)
coinlist_df <- map(coinlist_content$Data, ~as_tibble(.)) %>% bind_rows() %>% 
  select(Id, Name, Symbol, CoinName, FullName, TotalCoinSupply)

# Select coins you want
# e.g. BTC, ETH, LTC
coins <- c('BTC', 'ETH', 'LTC')
coins_df <- coinlist_df %>% filter(Symbol %in% coins)

# Get coin supply to calculate market cap
url_snapshot <- 'https://www.cryptocompare.com/api/data/coinsnapshotfullbyid/'
snap <- map(coins_df$Id, ~GET(url = paste(url_snapshot, '?id=', ., sep = '')))
snap_content <- map(snap, ~content(.))
coins_df$TotalCoinsMined <- map(snap_content, ~.$Data$General$TotalCoinsMined) %>% unlist()

# Get all historical coin prices up to present
url_price <- 'https://min-api.cryptocompare.com/data/histoday'
tocurrency <- 'USD'
histprice <- map(coins_df$Symbol, ~GET(url = paste(url_price, '?fsym=', ., '&tsym=', tocurrency, '&allData=TRUE',  sep = '')))
histprice_content <- map(histprice, ~content(.))
histprice_df <- map(histprice_content, function(x){
  allprice <- map(x$Data, ~ data_frame(xDate = anytime::anytime(.$time, tz = 'GMT'),
                                       Price = .$close)) %>% bind_rows()
  return(allprice)
}) 

# Coin Market Cap API request
url_cmc <- 'https://api.coinmarketcap.com/v2/ticker/?limit=0'
cmc <- GET(url = url_cmc)
cmc_content <- content(cmc) 
cmc_df <- map(cmc_content$data, function(x){
  df <- as_tibble(x[c('id', 'name', 'symbol', 'website_slug', 'rank', 'circulating_supply', 'total_supply')])
  df <- df %>% mutate(price = x$quotes$USD$price, market_cap = x$quotes$USD$market_cap) 
}) %>% bind_rows()

# Calculate index of top 30
t30 <- cmc_df %>% filter(rank < 31)

# Calculate weights of top 30
t30 <- t30 %>% mutate(weight = market_cap/sum(market_cap))
current_index <- t30 %>% summarise(index = sum(price * weight *100) / sum(price))
