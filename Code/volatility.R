library(tidyverse)
library(dplyr)

setwd("C:/Users/hilmiuysal/Desktop/NYC Data Science Academy/Projects/Shiny/Cryptograph/Code")

#Volatility calculation

#Create yearmonth variable
top100_coins = all_coins %>% select(name, date, close)
top100_coins = top100_coins %>% mutate(yearmonth = format(as.Date(top100_coins$date), '%Y%m'))

#Get unique coin names
unq_coins = unlist(top100_coins %>% select(name) %>% distinct())

#Init df to be returned
df_daily_vol = data.frame()

#Loop for every coin
for (coin in unq_coins) {
  #Get unique yearmonth for each coin
  unq_ym = unlist(top100_coins %>% filter(name == coin) %>% select(yearmonth) %>% distinct())
  df_sub = top100_coins %>% filter(name == coin)
  
  for (ym in unq_ym) {
    #Get vector of prices for coin and yearmonth
    close = df_sub %>% filter(yearmonth == ym) %>% pull(close)
    #Calculate 1-day lagged price
    close_lag_1 = lag(close, 1, default = close[1])
    #Calculate daily percentage changes in price
    close_diff_perc = (close - close_lag_1)/close_lag_1*100
    #Calculate sd of daily percentage changes in price = Daily volatility for given coin & yearmonth
    daily_vol = round(sd(close_diff_perc), 3)
    #Combine coin name, yearmonth and Daily volatility in a 1 row x 3 columns df
    df_d = data.frame(coin, ym, daily_vol)
    #Append it to another df that will be returned
    df_daily_vol = rbind(df_daily_vol, df_d)
    
  }
  
}

#Crypto market is open all year 
annualization_factor = sqrt(360)
monthly_factor = sqrt(30)


#
df_daily_vol = df_daily_vol %>% mutate(annualized_vol = round(daily_vol*annualization_factor, 3), monthly_vol = round(daily_vol*monthly_factor, 3))

#Write to CSV
write.csv(x = df_daily_vol, file = '../data/daily_volatility.csv', row.names = FALSE)






