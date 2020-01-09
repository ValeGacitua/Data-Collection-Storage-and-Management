#-----------------------------------------------------
  # Exercise 1
  # Generate Consistent Raw Data Files
#-----------------------------------------------------


# import .csv file of climate data
library(readr)
X10347386 <- read_csv("C:/Users/Patrick Kacic/Documents/Environmental Sciences/1. Semester/Data Storage, Collection, Management/10347386.csv", 
                      col_types = cols(date = col_datetime(format = "%m/%d/%Y %H:%M:%S %p"), lux = col_double()), 
                      skip = 5)

# arrange as tibble
library(tidyverse)
library(dplyr)
data <- as_tibble(X10347386)
data
df <- data %>% select(-a) %>% 
  select(-b) %>% 
  select(-c) %>% 
  select(-d)
df

# define date
library(lubridate)
a1 <- ymd_hms(df$date)
head(a1)

# separate Date and Time
d1 <- data.frame(Date = format(a1, '%Y-%m-%d'), Time = format(a1, '%H:%M %p'))
head(d1)
d1

# add new coloumn with Date and Time, drop old coloumn "date"
df1 <- df %>% mutate(d1$Date) %>% 
  mutate(d1$Time) %>% 
  select(-date)
df2 <- df1[,c(1,4,5,2,3)] #change order
df2

# redefine names
names(df2) <- c("id", "date", "hm", "ta", "lux")
df2


# write_tsv(df2, "C:/Users/Patrick Kacic/Documents/Environmental Sciences/1. Semester/Data Storage, Collection, Management/10347386.tsv")

# X10347386 <- read_tsv("10347386.tsv", skip = 5)
# View(X10347386)
