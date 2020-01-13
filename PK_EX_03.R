#-----------------------------------------------------
# Exercise 3
# Transform Time Series Data into Indices
#-----------------------------------------------------

# import data from Exercise 2
library(readr)
setwd("C:\\Users\\Patrick Kacic\\Documents\\Environmental Sciences\\1. Semester\\Data Storage, Collection, Management")
X10347386_Th <- read_delim("10347386_Th.tsv", 
                           "\t", escape_double = FALSE, col_types = cols(hour = col_double()), 
                           trim_ws = TRUE)


#-----------------------------------------------------
# 1 Temperature Indices
#-----------------------------------------------------

#------------------------------------------
# 1.1 Mean Temperature
#------------------------------------------

library(dplyr)
library(lubridate)

t_avg <- mean(X10347386_Th$th)
t_avg <- round(t_avg, digits = 3)
t_avg # 6.786

day <- 06:18

t_day <- select(X10347386_Th, hour, th) %>% 
          filter(hour %in% day)

t_day <- mean(t_day$th)
t_day <- round(t_day, digits = 3)
t_day # 7.047

t_nht <- select(X10347386_Th, hour, th) %>% 
            filter(!(hour %in% day))
t_nht <- mean(t_nht$th)
t_nht <- round(t_nht, digits = 3)
t_nht # 6.478

# calculate day-night amplitude
d_nd <- t_day - t_nht
d_nd # 0.569


#------------------------------------------
# 1.2 Mean Daily Amplitude
#------------------------------------------

t_max <- aggregate(X10347386_Th$th,by = list(day = X10347386_Th$date), FUN = max)
t_min <- aggregate(X10347386_Th$th, by = list(day = X10347386_Th$date), FUN = min)

t_max_avg <- mean(t_max$x)
t_max_avg <- round(t_max_avg, digits = 3)
t_max_avg # 8.836

t_min_avg <- mean(t_min$x)
t_min_avg <- round(t_min_avg, digits = 3)
t_min_avg # 4.824

t_amp <- t_max_avg - t_min_avg
t_amp # 4.012


#------------------------------------------
# 1.3 Flashiness
#------------------------------------------

library(rwrfhydro)
t_fl <- RBFlash(X10347386_Th$th)
t_fl <- round(t_fl, digits = 3)
t_fl # 0.051 = average absolute temperature change per hour



#------------------------------------------
# 1.4 Most Rapid Temperature Change
#------------------------------------------

library(zoo)

# calulate function for difference
t_range <- function(x) {
            r <- range(x) 
              out <- r[2]- r[1] 
                return(out)
                  }

d_t3 <- X10347386_Th %>% 
          mutate(t03 = rollapply(th, 3, FUN = t_range, align = "right", fill = NA ))

d_t12 <- X10347386_Th %>% 
          mutate(t12 = rollapply(th, 12, FUN = t_range, align = "right", fill = NA ))

d_t3_max <- max(d_t3$t03, na.rm = TRUE)
d_t3_max <- round(d_t3_max, digits = 3)
d_t3_max # 5.471

d_t12_max <- max(d_t12$t12, na.rm = TRUE)
d_t12_max <- round(d_t12_max, digits = 3)
d_t12_max # 6.221



#------------------------------------------
# 1.5 Fraction of NA-Values
#------------------------------------------

# NA values = R values; (f_NA = 47 / 576)
f_NA <- sum(X10347386_Th$origin == "R") / length(X10347386_Th$origin)
f_NA <- round(f_NA, digits = 3)
f_NA




#-----------------------------------------------------
# 2 Light Intensity Indices (with 10-min Series)
#-----------------------------------------------------

Z10347386 <- read_delim("10347386.tsv", "\t", 
                        escape_double = FALSE, col_types = cols(hm = col_time(format = "%H:%M")), 
                        trim_ws = TRUE, skip = 5)

# calulate average light intensity at midday (12:00)
l_md <- filter(Z10347386, (hour(hm) == 12 & minute(hm) == 00))
l_md <- mean(l_md$lux)
l_md <- round(l_md, digits = 3)
l_md # 1246.825

# calulate 95th percentile of light intensity
l_95 <- quantile(Z10347386$lux, .95)
l_95 <- round(l_95, digits = 3)
l_95 # 874.575



#-----------------------------------------------------
# 4 Long-Term Average
#-----------------------------------------------------

DWD_1443_hist <- read_delim("DWD 1443 hist.txt", ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format = "%Y%m%d%H")), 
                            trim_ws = TRUE)

interval <- c(paste(12, 14:31), paste(1, 1:6))
interval

lta <- mutate(DWD_1443_hist, md = paste(month(DWD_1443_hist$MESS_DATUM), 
          day(DWD_1443_hist$MESS_DATUM)))
lta <- lta[which(lta$md %in% interval), 
                     c("MESS_DATUM", "TT_TU", "md")]

lta <- mean(lta$TT_TU)
lta <- round(lta, digits = 3)
lta

t_avg


