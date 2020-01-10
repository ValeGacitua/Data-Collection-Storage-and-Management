setwd("~/Documents/Uni/Umwi/M.sc./Data_csm/Data/hourly")

require(dplyr)
require(lubridate)
require(data.table)
require(zoo)
require(ggplot2)
require(rio)

mydata <- read.csv(file = "/home/leon/Documents/Uni/Umwi/M.sc./Data_csm/Data/header/10350007.tsv", skip = 5, sep = "")
mydata <- mutate(mydata, DateTime = as.POSIXct(paste(mydata$date, mydata$hm))) # add DateTime in Posixct format

# Check if ta is in hobo range ----
which(mydata$ta >= 70) # max temp
which(mydata$ta <= -20) # min temp

which(mydata$lux >= 320000) # max temp
which(mydata$lux < 0) # min temp

# All values in possible measuring range

# Check for rate of change ----
mydata <- mutate(mydata, qc1 = ifelse(abs(mydata$ta-lag(mydata$ta, n = 1)) >= 1, 1, 0))
mydata[1, "qc1"] <- 0 # set the first value to 0

# Check miniumum variability ----

n_var <- 5 # max no. of no variability

var_df <- select(mydata, c("id", "ta")) # use intermediate df

setDT(var_df)

var_df[, matchPrev := fifelse((ta - shift(ta, 1)) == 0L, T, F, F)]
var_df[, g := cumsum(!matchPrev)] # grouping value, if value has changed
var_df[, count := cumsum(matchPrev), by = g]
var_df[, qc2 := fifelse(count >= n_var, 1L, 0L)]

mydata$qc2 <- var_df$qc2
# remove intermediates
 rm(var_df)
 rm(n_var)

# Old version
#
# nums <- mydata$id
# qc_dummy <- vector(mode = "list", length = length(nums))
# 
# for(i in 1:length(nums)) {
#   qc_dummy[[i]] <- ifelse(mydata[nums[i], 4] - mydata[nums[i-1], 4] == 0, 
#                           ifelse(mydata[nums[i], 4] - mydata[nums[i-2], 4] == 0,
#                                  ifelse(mydata[nums[i], 4] - mydata[nums[i-3], 4] == 0,
#                                         ifelse(mydata[nums[i], 4] - mydata[nums[i-4], 4] == 0,
#                                                ifelse(mydata[nums[i], 4] - mydata[nums[i-5], 4] == 0, 1, 0) ,0), 0) ,0) ,0) 
# }
# 
# mydata$qc2 <- as.vector(c(0,unlist(qc_dummy))) # first value of list is skipped by unlist (logi(0)) -> add 0

# Light intensity ----
lux_df <- select(mydata, c("id", "lux"))
setDT(lux_df)

lmin <- as.numeric(quantile(x = mydata$lux, probs = .95)) # min light threshold
lmax <- as.numeric(quantile(x = mydata$lux, probs = .99)) # max light threshold

# Minimum threshold
  
  lux_df <- mutate(lux_df, qc3_min := if_else(lux_df$lux > lmin, 1, 0, 0)) %>% 
    mutate(qc3_min2 := rollapply(qc3_min, 3, FUN = sum, align = "center", fill = NA))
    
  lux_df[which(lux_df$qc3_min2 >= 1), "qc3_min"] <- 1 
  
  # old version
  #
  # lux_df[, excThr1 := fifelse((shift(lux_df$lux, 1) > lmin), T, F, F)] # check for values exceeding threshold
  # 
  # # lead and lag indices; add -1 because of missing first value due to shift()
  # lag_val <- which(lux_df$excThr1 == T) - 2 # T_i-1
  # val <- which(lux_df$excThr1 == T) - 1 # T_i
  # lead_val <- which(lux_df$excThr1 == T) # T_i+1
  # 
  # # set qc3 values
  # lux_df[lag_val, 3] <- 1L
  # lux_df[val, 3] <- 1L
  # lux_df[lead_val, 3] <- 1L

# Maximum threshold

  lux_df <- mutate(lux_df, qc3_max := if_else(lux_df$lux > lmax, 1, 0, 0)) %>% 
    mutate(qc3_max2 := rollapply(qc3_max, 7, FUN = sum, align = "center", fill = NA))
  
  lux_df[which(lux_df$qc3_max2 >= 1), "qc3_max"] <- 1
  
  lux_df <- mutate(lux_df, qc3 := fifelse(lux_df$qc3_min >= 1 | lux_df$qc3_min >= 1, 1, 0, 0)) # combine qc3_min & qc3_max cols
  
  lux_df <- select(lux_df, id, lux, qc3)
  # old version
  #
  # lux_df[, excThr2 := fifelse((shift(lux_df$lux, 1) > lmax), T, F, F)] # check for values exceeding threshold
  # 
  # # lead and lag indices; add -1 because of missing first value due to shift()
  # lag_val3 <- which(lux_df$excThr2 == T) - 4 # T_i-3
  # lag_val2 <- which(lux_df$excThr2 == T) - 3 # T_i-2
  # lag_val1 <- which(lux_df$excThr2 == T) - 2 # T_i-1
  # val <- which(lux_df$excThr2 == T) - 1 # T_i
  # lead_val1 <- which(lux_df$excThr2 == T) # T_i+1
  # lead_val2 <- which(lux_df$excThr2 == T) + 1 # T_i+2
  # lead_val3 <- which(lux_df$excThr2 == T) + 2 # T_i+3
  # 
  # # set qc3 values
  # 
  # lux_df[lag_val3, 3] <- 1L
  # lux_df[lag_val2, 3] <- 1L
  # lux_df[lag_val1, 3] <- 1L
  # lux_df[val, 3] <- 1L
  # lux_df[lead_val1, 3] <- 1L
  # lux_df[lead_val2, 3] <- 1L
  # lux_df[lead_val3, 3] <- 1L

# Add qc3 to mydata
  mydata$qc3 <- lux_df$qc3
  
# Correct false flags
  mydata <- mutate(mydata, hrs = as.integer(format(mydata$DateTime, "%H"))) # create col with hour as integer
  
  dusk <- group_by(mydata, hrs) %>% filter(hrs %in% 0:6) %>% select(id) # select time from 00:00 till 06:00
  dawn <- group_by(mydata, hrs) %>% filter(hrs %in% 18:23) %>% select(id) # select time from 18:00 till 23:00
  
  mydata[dawn$id, "qc3"] <- 0
  mydata[dusk$id, "qc3"] <- 0
  
  # old version
  #
  # day <- mydata[which(between(mydata$hrs, lower = 6, upper = 18)), "id"] # select all id during day
  # mydata[mydata$id[!(mydata$hrs %in% day)], "qc3"] <- 0 # set false to zero
  
  mydata <- select(mydata, -hrs, -DateTime) # remove hrs & DateTime col
  
# remove intermediates
  rm(lux_df)
  # rm(lead_val)
  # rm(lead_val1)
  # rm(lead_val2)
  # rm(lead_val3)
  # rm(val)
  # rm(lag_val)
  # rm(lag_val1)
  # rm(lag_val2)
  # rm(lag_val3)
  rm(lmax)
  rm(lmin)
  # rm(day)
  rm(dusk)
  rm(dawn)

# Flagging ----

mydata <- mutate(mydata, qc_tot = apply(mydata[, c(6:8)], 1, sum))

# Summarise the qc cols
mydata_hourly <- select(mydata, ta, lux, qc1, qc2, qc3, qc_tot) %>% 
  mutate(DateTime = as.POSIXct(paste(mydata$date, mydata$hm))) %>%
  group_by(DateTime = cut(DateTime, breaks = "60 min")) %>% 
  summarise_all(list(~mean(.), ~sum(.))) # compute both mean and sum and select needed col later; ~FUN(.) creates nicer col names but only providing FUN also works
  
mydata_hourly <-  select(mydata_hourly, DateTime, ta = ta_mean, lux = lux_mean, qc_tot = qc_tot_sum) # select cols
mydata_hourly$DateTime <- as.POSIXct(mydata_hourly$DateTime) 

# Remove bad data points
mydata_hourly[which(mydata_hourly$qc_tot >=2), "ta"] <- NA # set to NA
mydata_hourly <- mutate(mydata_hourly, id = c(1:length(DateTime))) #add id col
mydata_hourly <- select(mydata_hourly, id, DateTime, ta, lux, qc_tot) # tidy df

# Add missing rows in time interval
missingData <- data.frame(id = 1:576, DateTime = seq(ymd_hm("2019-12-14 00:00"), ymd_hm("2020-01-06 23:00"), by = "hour"), ta = NA, lux = NA, qc_tot = NA) # complete time intervall, hardcoded 576 id entires as hours

## Insert hobo data into missingData; hobo interval 18:573
missingData[c(18:573), ] <- mydata_hourly
mydata_hourly <- missingData

rm(missingData)

# Regression ----

# load & prepare datasets
wbi_hourly <- read.csv("/home/leon/Documents/Uni/Umwi/M.sc./Data_csm/Data/hourly/WBI_hourly.csv", header = T, sep = "")
dwd_1443_hourly <- read.csv("/home/leon/Documents/Uni/Umwi/M.sc./Data_csm/Data/hourly/DWD_1443_hourly.csv", header = T, sep = "")
dwd_13667_hourly <- read.csv("/home/leon/Documents/Uni/Umwi/M.sc./Data_csm/Data/hourly/DWD_13667_hourly.csv", header = T, sep = "")

mydata_hourly <- mutate(mydata_hourly, src = "hobo") # add source col to easily identify different datasets

# format to fit mydata_hourly
wbi_hourly <- mutate(wbi_hourly, DateTime = mydata_hourly$DateTime) # copy DateTime from mydata_hourly
wbi_hourly <- select(wbi_hourly, DateTime, ta = AVG_TA200)
wbi_hourly <- mutate(wbi_hourly, id = c(577:1152), src = "wbi")

dwd_1443_hourly <- mutate(dwd_1443_hourly, DateTime = mydata_hourly$DateTime) # copy DateTime from mydata_hourly
dwd_1443_hourly <- select(dwd_1443_hourly, DateTime, ta = TT_TU)
dwd_1443_hourly <- mutate(dwd_1443_hourly, id = c(1153:1728), src = "dwd_1443")

dwd_13667_hourly <- mutate(dwd_13667_hourly, DateTime = mydata_hourly$DateTime) # copy DateTime from mydata_hourly
dwd_13667_hourly <- select(dwd_13667_hourly, DateTime, ta = LUFTTEMPERATUR)
dwd_13667_hourly <- mutate(dwd_13667_hourly, id = c(1729:2304), src = "dwd_13667")

# models
master_data <- select(mydata_hourly, DateTime, ta_hobo = ta)
master_data <- cbind(master_data, ta_wbi = wbi_hourly$ta)
master_data <- cbind(master_data, ta_dwd1443 = dwd_1443_hourly$ta)
master_data <- cbind(master_data, ta_dwd13667 = dwd_13667_hourly$ta)

lm1 <- lm(data = master_data, ta_hobo ~ ta_wbi, na.action = na.exclude)
summary(lm1) # R-sq .96 

lm2 <- lm(data = master_data, ta_hobo ~ ta_dwd1443)
summary(lm2) # R-sq .92

lm3 <- lm(data = master_data, ta_hobo ~ ta_dwd13667)
summary(lm3) # R-sq .94

# Plot
par(mfrow = c(2,2))

plot(x = master_data$ta_hobo, y = master_data$ta_wbi)
abline(a = 0, b = 1, col = "red")

plot(x = master_data$ta_hobo, y = master_data$ta_dwd1443)
abline(a = 0, b = 1, col = "red")

plot(x = master_data$ta_hobo, y = master_data$ta_dwd13667)
abline(a = 0, b = 1, col = "red")

par(mfrow = c(1,1))

# Fill NA in hobodata ----
length(which(is.na(master_data$ta_hobo)) == T) - 20 # No. of NA values MINUS the manually added ones aka synthetic NAs (20) in hobo dataset for use in Ex3

# predict values based on lm1 with a+b*ta_wbi
preds_wbi <- as.numeric(coef(lm1)[1]) + as.numeric(coef(lm1)[2]) * master_data$ta_wbi

# find NA values
ta_NA <- which(is.na(master_data$ta_hobo))

# fill NA values
mydata_hourly[ta_NA, "ta"] <- preds_wbi[ta_NA]

master_data[ta_NA, "ta_hobo"] <- preds_wbi[ta_NA]

# Compare ta_hobo corrected & ta_wbi
ggplot(master_data) +
  geom_line(aes(x = DateTime, y = ta_hobo), col = "red") +
  geom_line(aes(x = DateTime, y = ta_wbi))

# Format to desired df layout ----

#create df cols date & hour with layout
mydata_out <- select(mydata_hourly, th = ta) %>% 
  mutate(date = as.Date(mydata_hourly$DateTime, format = "%Y-%m-%d")) %>% 
  mutate(hour = format(mydata_hourly$DateTime, "%H"))

mydata_out[, "origin"] <- "H" # set all values of origin to H
mydata_out[ta_NA, "origin"] <- "R" # set predictes values to R

mydata_out$th <- round(mydata_out$th, 3) # round temp to 3 digits

mydata_out <- select(mydata_out, date, hour, th, origin) # sort to layout

# export
export(x = mydata_out, file = "10350007_Th.tsv", format = "tsv")
