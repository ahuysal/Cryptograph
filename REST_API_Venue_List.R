#Import necessary libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)

#Get all venues as a list
list_of_venues = fromJSON(txt = 'https://coinmap.org/api/v1/venues/', flatten = T) 

#Create data frame from list
df_venues = data.frame(list_of_venues)

#Convert UNIX timestamp to date
df_venues = df_venues %>% mutate(venues.created_on = as.Date(as.POSIXct(venues.created_on, origin="1970-01-01")))

#Convert category to factor
df_venues = df_venues %>% mutate(venues.category = as.factor(venues.category))

#Rename columns
df_venues = df_venues %>% rename(id = venues.id, name = venues.name, createDate = venues.created_on, category = venues.category, lon=venues.lon, lat=venues.lat)

df_venues_final = df_venues %>% select(id, name, createDate, category, lon, lat)

#Review data frame
head(df_venues_final)
str(df_venues_final)
class(df_venues_final)

#Write to csv
setwd("C:/Users/hilmiuysal/Desktop/NYC Data Science Academy/Projects/Shiny/Sources")
write.csv(x = df_venues_final, file = 'list_of_venues.csv', row.names = F)
