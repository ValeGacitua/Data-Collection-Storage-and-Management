# load packages
library("tidyverse")
library("lubridate")
library("rio")
library("skimr")
library("zoo")


#############################
# Exercise 1 - Raw to Useful
#############################

# download data from github to environment 
df <- read_csv("https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2020/_00raw/10347359.csv", skip = 2,
               col_names=c("id", "date_hm", "ta", "lux", "useless1", "useless2", "useless3", "useless4"),
               col_types = c("ncddcccc"))

# exclude corrupted last two rows and delete unnecessary columns
df <- df %>%
  filter(id<3522) %>%
  select(-c(useless1, useless2, useless3, useless4))

# separate date and time
df <- df %>%
  mutate(date=substr(df$date_hm, start=1, stop=10)) %>%
  mutate(hm=substr(df$date_hm, start=12, stop=23)) %>%
  select(-date_hm)

# exclude data of last day
df <- df %>%
  filter(!(date=="01/07/2020"))

# fix the time format
df$date <- as.Date(df$date, "%m/%d/%Y")
df$hm <- substr(as.character(parse_date_time(df$hm, "%H:%M:%S %p")), start=12, stop=16)

# rearrange the columns
df <- df[,c(1,4,5,2,3)]

# save as .tsv
setwd("H:/Daten/Studium/2_Master/1_Semester/57170_Data_Collection_Storage_Management/Data")
write.table(df, file='10347359.tsv', sep='\t', quote=F, row.names = FALSE)

# add meta data by hand

