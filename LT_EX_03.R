setwd("~/Documents/Uni/Umwi/M.sc./Data_csm/Data/indices/")

require(dplyr)
require(zoo)
require(rdwd) # powerful package for DWD data
require(lubridate)

# load data
mydata <- read.csv(file = "~/Documents/Uni/Umwi/M.sc./Data_csm/Data/hourly/10350007_Th.tsv", sep = "", header = T)

# mean temps
t_avg <- mean(mydata$th)

day <- 6:18 # create day dummy vector

t_day <- select(mydata, hour, th) %>%
  filter(hour %in% day)

t_day <- mean(t_day$th)

t_nht <- select(mydata, hour, th) %>%
  filter(!(hour %in% day))

t_nht <- mean(t_nht$th)
  
d_nd <- t_day - t_nht # differnce day-night

# mean daily amplitude

t_max <- aggregate(mydata[,"th"], by = list(mydata[,"date"]), FUN = max)
t_min <- aggregate(mydata[,"th"], by = list(mydata[,"date"]), FUN = min)

t_max_avg <- mean(t_max$x)
t_min_avg <- mean(t_min$x)

t_amp <- t_max_avg - t_min_avg

# flashiness
flash <- function(temp) {
  n <- length(temp) # read out length of input data
  t_fl <<- c() # create return vector; <<- defines global variable
  
  for(i in 1:n) t_fl[i] <- sum(abs(temp[i]-temp[i-1])/(n-1)) # calculate RB-index
  
  return(t_fl)
}

t_flsh <- mean(flash(temp = mydata$th)) # average of the hourly flashiness

# most rapid temp change
t_range <- function(x) {
  r <- range(x) # calculate range
  out <- r[2]- r[1] # calulate difference
  return(out)
}

delta_t3 <- rollapply(mydata$th, width = 3, FUN = t_range, align = "right", fill = NA) #temp changes in 3 hour intervals
delta_t12 <- rollapply(mydata$th, width = 12, FUN = t_range, align = "right", fill = NA) #temp changes in 12 hour intervals

max(delta_t3, na.rm = T) # max 3 hr change rate
max(delta_t12, na.rm = T) # max 12 hr change rate

# fraction of NA-values

## Hardcoded no. of synthetic NAs (20) !!
Na_frac <- (length(which(mydata$origin == "R")) - 20) / length(mydata$date) # fraction of NAs in dataset

# light intensity indices
mydata_mins <- read.csv(file = "/home/leon/Documents/Uni/Umwi/M.sc./Data_csm/Data/header/10350007.tsv", skip = 5, sep = "")

l_md <- mean(mydata_mins[which(mydata_mins$hm == "12:00"),"lux"]) # mean at noon
l_95 <- as.numeric(quantile(x = mydata_mins$lux, probs = .95)) # 95 % percentile

# Long term-average
dwd_hist <- readDWD(file = "/home/leon/Documents/Uni/Umwi/M.sc./Data_csm/Data/indices/stundenwerte_TU_01443_19510101_20181231_hist.zip", progbar = T)

m_int <- c(paste(12, 1:31), paste(1, 1:6)) # vector with measurement interval
dwd_hist <- mutate(dwd_hist, md = paste(month(dwd_hist$MESS_DATUM), day(dwd_hist$MESS_DATUM))) # create col with month and day
dwd_hist <- dwd_hist[which(dwd_hist$md %in% m_int), c("MESS_DATUM", "TT_TU", "md")] # filter for m_int

t_avgRef <- mean(dwd_hist$TT_TU, na.rm = T) # t_avgRef = 2.896, t_avgHOBO = 5.133
