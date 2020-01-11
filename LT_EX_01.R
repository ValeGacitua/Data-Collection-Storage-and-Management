setwd("~/Documents/Uni/Umwi/M.sc./Data_csm/Data/raw")
require(dplyr)
require(readr)
require(lubridate)
require(rio)

mydata <- read_csv("10350007.csv", col_names = c("id", "date", "ta", "lux", rep("NA", 6)), skip = 1)

# Format ----
mydata  <-  select(mydata, c("id", "date", "ta", "lux"))
mydata$date <- mdy_hms(mydata$date) # format datetime

# Select time of sampling
sample_start <- as.POSIXct("2019-12-14 18:00:00")
sample_end <- as.POSIXct("2020-01-06 21:50:00")

mydata <- mydata[mydata$date>=sample_start,]
mydata <- mydata[mydata$date<=sample_end,]

# Separate & sort
mydata <- mutate(mydata, hm = format(mydata$date, "%H:%M")) # Add hm col  
mydata$date <- as.Date(mydata$date, format = "%Y-%m-%d") # cut h:m from date col
mydata <- mydata[,c(1,2,5,3,4)] # sort cols
mydata$id <- c(1:length(mydata$id)) #reset id no.

# Export
export(x = mydata, file = "header_10350007.tsv", format = "tsv")
# test import
a1 <- read_tsv(file = "/home/leon/Documents/Uni/Umwi/M.sc./Data_csm/Data/header/10350007.tsv", skip = 5)
