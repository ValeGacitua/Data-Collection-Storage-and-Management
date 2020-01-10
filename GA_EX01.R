
###EXERCISE 1###

###LOAD LIBRARIES###

install.packages("readr")
install.packages("dplyr")

library("readr")
library("tidyverse")
library("lubridate")
library("rio")
library("skimr")
library("dplyr")
library("zoo")
library("sf")
library("skimr")
library("padr")



###LOAD ORIGINAL HOBO FILE###

df <- read_csv("https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2020/_00raw/10347351.csv", skip = 2,
               col_names=c("id", "date_hm", "ta", "lux", "useless1", "useless2", "useless3", "useless4"),
               col_types = c("ncddcccc"))



###COLUMNS TREATMENT - SELECTING AND DATE TIME CONVERTING###

df <- df %>%
  select(-c(useless1, useless2, useless3, useless4))

df %>%
  select(id,date_hm,ta,lux)
  
df <- df %>%
  mutate(date=substr(df$date_hm, start=1, stop=10)) %>%
  mutate(hm=substr(df$date_hm, start=12, stop=23)) %>%

Hobo_File_R$date <- as.Date(Hobo_File_R$date, "%m/%d/%Y")
Hobo_File_R$hm <- substr(as.character(parse_date_time(Hobo_File_R$hm, "%H:%M:%S %p")), start=12, stop=16)
  
Hobo_File_R %>%
  select(id,date,hm,ta,lux)

Hobo_File_R_Final <- Hobo_File_R[c(1,5,6,3,4)]

write.table(Hobo_File_R_Final, file='Hobo_File_R_Final.tsv', row.names = FALSE, sep='\t')


              
###INSERT DATA INTO TSV FILE***

#HOBO_ID 10347351
#Location_Lat_lon 48.006 7.821
#Alt_above_ground 10
#Exposition S
#Influence_class 2     
  






