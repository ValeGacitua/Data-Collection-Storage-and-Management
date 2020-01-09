setwd("~/Documents/Uni/Umwi/M.sc./Data_csm/Data/hourly")

require(dplyr)
require(lubridate)
require(data.table)

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
mydata[1, 7] <- 0 # set the first value to 0

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

lux_df$qc3 <- 0L # set qc3 values to 0

# Minimum threshold
  
  lux_df[, excThr1 := fifelse((shift(lux_df$lux, 1) > lmin), T, F, F)] # check for values exceeding threshold
  
  # lead and lag indices; add -1 because of missing first value due to shift()
  lag_val <- which(lux_df$excThr1 == T) - 2 # T_i-1
  val <- which(lux_df$excThr1 == T) - 1 # T_i
  lead_val <- which(lux_df$excThr1 == T) # T_i+1
  
  # set qc3 values
  lux_df[lag_val, 3] <- 1L
  lux_df[val, 3] <- 1L
  lux_df[lead_val, 3] <- 1L

# Maximum threshold

  lux_df[, excThr2 := fifelse((shift(lux_df$lux, 1) > lmax), T, F, F)] # check for values exceeding threshold
  
  # lead and lag indices; add -1 because of missing first value due to shift()
  lag_val3 <- which(lux_df$excThr2 == T) - 4 # T_i-3
  lag_val2 <- which(lux_df$excThr2 == T) - 3 # T_i-2
  lag_val1 <- which(lux_df$excThr2 == T) - 2 # T_i-1
  val <- which(lux_df$excThr2 == T) - 1 # T_i
  lead_val1 <- which(lux_df$excThr2 == T) # T_i+1
  lead_val2 <- which(lux_df$excThr2 == T) + 1 # T_i+2
  lead_val3 <- which(lux_df$excThr2 == T) + 2 # T_i+3
  
  # set qc3 values
  
  lux_df[lag_val3, 3] <- 1L
  lux_df[lag_val2, 3] <- 1L
  lux_df[lag_val1, 3] <- 1L
  lux_df[val, 3] <- 1L
  lux_df[lead_val1, 3] <- 1L
  lux_df[lead_val2, 3] <- 1L
  lux_df[lead_val3, 3] <- 1L

# Correct false flags
  mydata <- mutate(mydata, hrs = as.integer(format(mydata$DateTime, "%H"))) # create col with hour as integer
  day <- mydata[which(between(mydata$hrs, lower = 6, upper = 18)), 1] # select all id during day
  
  mydata[mydata$id[!(mydata$id %in% day)], 8] <- 0
  
  mydata <- select(mydata, -hrs, -DateTime) # remove hrs col

# Add qc3 to mydata
  mydata$qc3 <- lux_df$qc3
  
# remove intermediates
  rm(lux_df)
  rm(lead_val)
  rm(lead_val1)
  rm(lead_val2)
  rm(lead_val3)
  rm(val)
  rm(lag_val)
  rm(lag_val1)
  rm(lag_val2)
  rm(lag_val3)
  rm(lmax)
  rm(lmin)
  rm(day)

# Flagging ----

mydata <- mutate(mydata, qc_tot = apply(mydata[, c(6:8)], 1, sum))

# Summarise the qc cols
mydata_hourly <- select(mydata, ta, lux, qc1, qc2, qc3, qc_tot) %>% 
  mutate(DateTime = as.POSIXct(paste(mydata$date, mydata$hm))) %>%
  group_by(DateTime = cut(DateTime, breaks = "60 min")) %>% 
  summarise_all(list(~mean(.), ~sum(.))) # compute both mean and sum and select needed col later; ~FUN(.) creates nicer col names but only providing FUN also works
  
mydata_hourly <-  select(mydata_hourly, DateTime, ta = ta_mean, lux = lux_mean, qc_tot = qc_tot_sum) # select cols
mydata_hourly$DateTime <- as.POSIXct(mydata_hourly$DateTime) 

# Add missing rows in time interval
missingData_start <- data.frame(DateTime = seq(ymd_hm("2019-12-14 00:00"), ymd_hm("2019-12-14 16:00"), by = "hour"), ta = 0, lux = 0, qc_tot = 0)
missingData_end <- data.frame(DateTime = seq(ymd_hm("2020-01-06 21:00"), ymd_hm("2020-01-06 23:00"), by = "hour"),  ta = 0, lux = 0, qc_tot = 0)

mydata_hourly <- full_join(missingData_start, full_join(mydata_hourly, missingData_end)) # join dfs  !!! 2019-12-14 16:00:00 is doubled and 2020-01-06 20:00:00 is missing !!!
mydata_hourly <- mutate(mydata_hourly, id = c(1:length(DateTime))) #add id col

rm(missingData_start)
rm(missingData_end)

# Remove bad data points

bad_ta <- which(mydata_hourly$qc_tot > 3)
mydata_hourly[which(mydata_hourly$qc3_sum >=2), 2] <- NA # set to NA

rm(bad_ta)

# Regression ----

# load datasets
wbi_hourly <- read.csv("/home/leon/Documents/Uni/Umwi/M.sc./Data_csm/Data/hourly/WBI_hourly.csv", header = T, sep = "")
dwd_1443_hourly <- read.csv("/home/leon/Documents/Uni/Umwi/M.sc./Data_csm/Data/hourly/DWD_1443_hourly.csv", header = T, sep = "")

# format to fit mydata_hourly
wbi_hourly <- mutate(wbi_hourly, DateTime = mydata_hourly$DateTime) # copy DateTime from mydata_hourly
wbi_hourly <- select(wbi_hourly, DateTime, ta = AVG_TA200)
wbi_hourly <- mutate(wbi_hourly, lux = NA, qc_tot = NA, id = c(577:1152))



