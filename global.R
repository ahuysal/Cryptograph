#Load libraries and helper file
source("./helpers.R")
library(shiny)
library(tidyverse)
library(dplyr)
library(shinydashboard)
library(DT)
library(googleVis)
library(leaflet)
library(RSQLite)
library(data.table)
library(sp)
library(plotly)
library(plyr)

# Read in coin market CSV file
all_coins = read.csv(file = './data/crypto-markets.csv', header = T, sep = ',', stringsAsFactors = F)

# Take top 100 coins according to market capitalization
all_coins = all_coins %>% filter(ranknow %in% 1:100) %>% mutate(slug = as.factor(slug), symbol = as.factor(symbol), name = as.factor(name), date = as.Date(date))

# log(0) == -Inf. Handle it! and round to 3 decimals
all_coins = all_coins %>% mutate(ln_open = round(if_else(is.infinite(log(open)), 0, log(open)), 3), ln_high = round(if_else(is.infinite(log(high)), 0, log(high)), 3), 
                                 ln_low = round(if_else(is.infinite(log(low)), 0, log(low)), 3), ln_close = round(if_else(is.infinite(log(close)), 0, log(close)), 3), 
                                 ln_volume = round(if_else(is.infinite(log(volume)), 0, log(volume)), 3),
                                 ln_market = round(if_else(is.infinite(log(market)), 0, log(market)), 3)
                                 )

# Calculate min. available date and corresponding price for each coin 
min_dates = data.frame(all_coins %>% group_by(name) %>% slice(which.min(date)))
min_date_coins = min_dates %>% inner_join(all_coins, by = c('name','date')) %>% select(name, date, close = close.x)

# List of coins
names = levels(all_coins$name)

# List of attributes of coins
# coin names, spread and close_ratio out
attrs = colnames(all_coins)[c(-1:-5)][-7][-7]


# Tab3 - Map

# Load BTC accepting venues, tidy category field
df_venues = read.csv(file = './data/list_of_venues.csv', header = T, sep = ',', stringsAsFactors = F)
df_venues = df_venues %>% mutate(category = toupper(category))
df_venues = df_venues %>% filter(!(category %in% c('DRUG STORE','EDUCATIONAL BUSINESS','TRAVEL AGENCY')))
df_venues$category = ifelse(df_venues$category == 'TREZOR RETAILER', 'SHOPPING', df_venues$category)
df_venues = df_venues %>% mutate(category = as.factor(category))
df_venues = df_venues %>% mutate(YM = as.character(format(as.Date(createDate), '%Y%m')))


# Marker colors for Bitcoin accepting venues leaflet map
venue_categories = levels(df_venues$category)
markerColors = sapply(df_venues$category, function(category) {
  if(category == venue_categories[1]) {
    "red"
  } else if(category == venue_categories[2]) {
    "darkred"
  } else if(category == venue_categories[3]) {
    "lightred"
  } else if(category == venue_categories[4]) {
    "orange"
  } else if(category == venue_categories[5]) {
    "beige"
  } else if(category == venue_categories[6]) {
    "green"
  } else if(category == venue_categories[7]) {
    "darkgreen"
  } else if(category == venue_categories[8]) {
    "lightgreen"
  } else if(category == venue_categories[9]) {
    "blue"
  } else if(category == venue_categories[10]) {
    "darkblue"
  } else {
    "black"
  }
  
})

# Create a data frame For "Type of Stores" Donut Chart - (Stores Tab)
df_venues_cat_count = as.data.frame(df_venues %>% dplyr::group_by(category) %>% dplyr::summarise(CNT=n()))

# Do I need them?
df_venues_cnt_by_date = as.data.frame(df_venues %>% dplyr::group_by(YM) %>% dplyr::summarise(CNT=n()))
df_venues_cnt_by_date = df_venues_cnt_by_date %>% mutate(date = as.Date(paste0(YM,'01'), '%Y%m%d'))


# Create a data frame For "Store Types by Year" Donut Chart - (Stores Tab)
df_venues_combo_chart = df_venues %>% filter(!(category %in% c('DEFAULT'))) %>% mutate(category = as.character(category), YEAR = year(as.Date(createDate)))
df_venues_combo_chart$category = ifelse(df_venues_combo_chart$category %in% c('CAFE','GROCERY'), 'FOOD', df_venues_combo_chart$category)
df_venues_combo_chart$category = ifelse(df_venues_combo_chart$category == 'NIGHTLIFE', 'ATTRACTION', df_venues_combo_chart$category)
df_venues_combo_chart$category = ifelse(df_venues_combo_chart$category %in% c('LODGING','SPORTS','TRANSPORT'), 'OTHER', df_venues_combo_chart$category)
df_venues_combo_chart = as.data.frame(df_venues_combo_chart %>% dplyr::group_by(YEAR, category) %>% dplyr::summarise(CNT=n()))
df_venues_combo_chart = df_venues_combo_chart %>% spread(key = category, value = CNT, fill = 0)


#Popularity by country#

# Read in bitcoin popularity by country CSV file
pop_btc = read.csv(file = './data/bitcoin_interest_by_country_5_years.csv', header = T, sep = ',', stringsAsFactors = F)

# Google geochart does not allow null values. Eliminate nulls!
pop_btc = pop_btc %>% filter(!is.na(Popularity))

# Group popularity index to visualize by continent
pop_btc = pop_btc %>% mutate(PopGroup = (if_else(
  Popularity <= 10,
  '0-10',
  if_else(
    Popularity <= 20,
    '10-20',
    if_else(
      Popularity <= 40,
      '20-40',
      if_else(Popularity <= 60, '40-60', '60-100')
    )
  )
)))

# Read in country-continent mapping CSV
continent = read.csv(file = './data/countryContinent.csv', header = T, sep = ',', stringsAsFactors = F)
continent = continent %>% mutate(Country = country)

# Add Continent to the popularity data to visualize in the faceted piechart 
pop_btc=pop_btc %>% inner_join(continent, by = 'Country') %>% select(Country, continent, Popularity, PopGroup)
pop_btc_group = as.data.frame(pop_btc %>% dplyr::group_by(continent, PopGroup) %>% dplyr::summarise(tot=n()))
pop_btc_group = pop_btc_group %>% arrange(continent, PopGroup)

continent_tot = data.frame(pop_btc_group %>% dplyr::group_by(continent) %>% dplyr::summarise(total = sum(tot)))
pop_btc_group_continent = pop_btc_group %>% inner_join(continent_tot, by = 'continent')
pop_btc_group_continent_ratio = pop_btc_group_continent %>% mutate(ratio = round(tot/total,2))
pop_btc_group_continent_ratio = pop_btc_group_continent_ratio %>% mutate(ratio = if_else(continent == 'Africa' & PopGroup == '0-10', 0.34, ratio))


# Add color to the top20 countries for gvisBarChart
pop_btc_top20 = pop_btc %>% head(20)
pop_btc_top20$Popularity.style = c('red','blue','gold','yellow', 'green', 'navy', 'teal', 'olive', 'lime', 'orange', 'fuchsia', 'purple', 'maroon', 'black', 'light-blue', 'green', 'orange', 'red', 'olive', 'black')

#Popularity by country ends#


# Tab-4 Volatility Begins

# Read in volatility CSV file
df_daily_vol = read.csv(file = './data/daily_volatility.csv', header = T, sep = ',', stringsAsFactors = T)
df_daily_vol = df_daily_vol %>% mutate(ym = as.character(ym))

# year/month checkbox group input
unique_ym = unlist(df_daily_vol %>% select(ym) %>% distinct())
names(unique_ym) = NULL

# Initialize a list to keep updated list of ym wrt selected coins
L=list()

# Get volatility types to use in histogram as y-axis
vol_types = colnames(df_daily_vol)[c(-1,-2)]

#Tab-4 Volatility Ends



#Bubble Chart Begins

# Create a data frame with only the necessary columns
vol_bubble = df_daily_vol %>% 
                transmute(name = coin, yearmonth = as.integer(ym), 
                          daily_vol, annualized_vol, monthly_vol)

# Add yearmonth to join with vol_bubble and plot bubble chart
all_coins = all_coins %>% 
              mutate(yearmonth = as.integer(format(as.Date(all_coins$date), '%Y%m')))

# Top 20 coins market cap. by yearmonth
market_bubble = data.frame(all_coins %>% 
                             filter(ranknow %in% 1:21 & yearmonth >= 201710 & name != 'Bitcoin SV') %>%
                             dplyr::group_by(name, yearmonth) %>%
                             dplyr::summarise(avg_market = mean(market)))

df_bubble = market_bubble %>% 
            inner_join(vol_bubble, by = c('name','yearmonth'))

#Bubble Chart Ends



############
###SQLite###
############

all_coins_sqlite = all_coins %>% mutate(date = as.character(date))

#SQLite DB name
dbname = "./data/cryptograph.sqlite"

#SQLite venues table name
tbl_venues = "venues"

#SQLite coins table name
tbl_coins = "coins"

#connect to database
conn <- dbConnect(drv = SQLite(), 
                  dbname = dbname)

#Drop table
dbRemoveTable(conn, tbl_venues)
dbRemoveTable(conn, tbl_coins)

######################
#Write to tables ends#
######################

#write table
dbWriteTable(conn = conn,
             name = tbl_venues,
             value = df_venues)

#write table
dbWriteTable(conn = conn,
             name = tbl_coins,
             value = all_coins_sqlite)

#List tables
dbListTables(conn)

#Disconnect from DB
dbDisconnect(conn)

######################
#Write to tables ends#
######################

