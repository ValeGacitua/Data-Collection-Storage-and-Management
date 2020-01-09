#-----------------------------------------------------
# Exercise 2
# Data Quality Control and Hourly Series
#-----------------------------------------------------


# import edited raw data from Exercise 1
setwd("C:\\Users\\Patrick Kacic\\Documents\\Environmental Sciences\\1. Semester\\Data Storage, Collection, Management")
Y10347386 <- read_tsv("10347386.tsv", skip = 5)
Y10347386

library(lubridate)
library(dplyr)
a1 <- ymd_hms(df$date)
head(a1)
e1 <- data.frame(Time = format(a1, '%H:%M'))

df3 <- Y10347386 %>% mutate(e1$Time) %>% 
                     select(-hm)
df3

df4 <- df3[,c(1,2,5,3,4)] #change order
df4

names(df4) <- c("id", "date", "hm", "ta", "lux")
df4 # = .tsv data with 'hm' without seconds


#-----------------------------------------------------
# 1 Quality Control
#-----------------------------------------------------

#------------------------------------------
# 1.1 Measurement range (Plausible Values)
#------------------------------------------

# Check each temperature data point to be in the measurement range. See the HOBO manual for specification.
# Plausible Values: > -20 and < 70 ° Celsius


plot(df4$ta, main = "Temperature distribution of HOBO 10347386", xlab = "measurement points", 
     ylab = "air temperature in ° Celsius")

# minimum and maximum
min(X10347386$ta) # -1.456
max(X10347386$ta) # 17.475

# Definition of non-plausible values as 1 and plausible values as 0
df6 <- df4 %>% mutate(qc1 = if_else(df4$ta < -20, 1, if_else(df4$ta > 70, 1, 0)))
max(df6$qc1)


#------------------------------------------
# 1.2 Plausible rate of change
#------------------------------------------

# Check each temperature data point to have not more than 1K temperature change 
# compared to the previous data point. Check out the lead() and lag() functions 
# in the dplyr-package and try to use them in a mutate-command.

# calculate difference of ta value to previous ta value
df6$diff_ta <- lag(df6$ta,1) - df6$ta

# mutate to qc2 by flagging ta values > 1 and < -1 as 1 and other values as 0
df8 <-  df6 %>% mutate(qc2 = if_else(df6$diff_ta > 1, 1, if_else(df6$diff_ta < -1, 1, 0)))

# change NA value from 1st observation to 0
df8$qc2[is.na(df8$qc2)] <- 0

# rearrange table and delete coloumn "diff_ta
df9 <- df8[,c(1,2,3,4,5,6,8,7)] %>% select(-diff_ta)



#------------------------------------------
# 1.3 Minimum variability (Persistence)
#------------------------------------------

# If temperature has not changed during the last 60 minutes
# (i.e. data point Ti plus 5 data points before from Ti−1 to Ti−5) flag the corresponding data point Ti 
# as bad data point.

# calculate difference of ta value to previous ta value
df9$diff_ta <- (lag(df9$ta,1) - df9$ta)

# change NA value from 1st observation to 0
df9$diff_ta[is.na(df9$diff_ta)] <- 0

# define that if the sum of a diff_ta value and the 5 values before is 0, then flag it with 1, otherwise 0
df10 <- df9 %>% mutate(qc3 = if_else
                       (sum(df9$diff_ta + 
                        lag(df9$diff_ta,1) + 
                           lag(df9$diff_ta,2) + 
                              lag(df9$diff_ta,3) +
                                 lag(df9$diff_ta,4) +
                                  lag(df9$diff_ta,5)) == 0, 1, 0, 0))

sum(df10$qc3)

# rearrange table
df11 <- df10[,c(1,2,3,4,5,6,7,9,8)]



#------------------------------------------
# 1.4 Light intensity
#------------------------------------------

# The last quality check uses measured light intensity to flag bad data points.
# Define two thresholds L1 [lux] and L2 [lux] with L2 > L1:

# 1. Flag a temperature value Ti (and also Ti−1 and Ti+1) if measured light intensity 
# is higher than L1 (i.e. 3 values in total are flagged)

# 2. Flag a temperature value Ti (and also Ti−3 - Ti+3) if measured light intensity 
# is higher than L2 (i.e. 7 values in total are flagged)


min(X10347386$lux) # 0
max(X10347386$lux) # 14466.8
mean(X10347386$lux) # 218.9

plot(X10347386$lux)

# Calculate 95th and 99th percentile
quantile(df11$lux, c(.95, .99))
#   95%      99% 
# 874.575 4383.090 

# L2 > L1
# L2 == 4383.090 lux
# L1 == 874.575 lux


#-----------------------------------------
# L1
# flag Ti, Ti-1 and Ti+1 when lux > 874.575
#-----------------------------------------

library(zoo)
df12 <- df11 %>% mutate(qc4 = if_else(df11$lux > 874.575, 1, 0, 0)) %>% 
                  mutate(qc4_2 = rollapply(qc4, 3, FUN = sum, align = "center", fill = NA))

# replace values of "2" in qc4_2 to "1"
df12$qc4_2[df12$qc4_2 > 1] <- 1

# change NA value from 1st observation to 0
df12$qc4_2[is.na(df12$qc4_2)] <- 0

qc4 <- df12[df12$qc4 == '1',]
qc4

qc4_2 <- df12[df12$qc4_2 == '1',]
qc4_2
# often between 9:00 and 13:30 due to exposition to east --> only daytimes, no night outliers


#-----------------------------------------
# L2
# flag Ti, Ti-3 to Ti+3 when lux > 4383.090
#-----------------------------------------

df13 <- df12 %>% mutate(qc5 = if_else(df12$lux > 4383.090, 1, 0, 0)) %>% 
  mutate(qc5_2 = rollapply(qc5, 7, FUN = sum, align = "center", fill = NA))

# replace values of "2" in qc5_2 to "1"
df13$qc5_2[df13$qc5_2 > 1] <- 1

# change NA value from 1st observation to 0
df13$qc5_2[is.na(df13$qc5_2)] <- 0

qc5 <- df13[df13$qc5 == '1',]
qc5

qc5_2 <- df13[df13$qc5_2 == '1',]
qc5_2
# often between 10:00 and 12:30 due to exposition to east --> only daytimes, no night outliers


# rearrange table and unnecessary coloumns
df14 <- df13[,c(1,2,3,4,5,6,7,8,11,13,9,10,12)] %>% select(-diff_ta) %>% select(-qc4) %>% select(-qc5)

names(df14) <- c("id", "date", "hm", "ta", "lux", "qc1", "qc2", "qc3", "qc4", "qc5")

df14

# investigation of highest lux values and measured temperature
df15 <- df14 %>% arrange(-df14$lux)

# subset coloumns 1 to 100 and rows 1 to 10
df15 <- df15[c(1:100), c(1:10)]



#-----------------------------------------------------
# 2 Flagging System to Identify Bad Data
#-----------------------------------------------------

# subset coloumns qc1 to qc5
# df16 <- df14[c(1:3456), c(6:10)]
# df14$qc_total <- cbind(df16, total = rowSums(df16))

# df17 <- df14 %>% select(-qc_total.qc1) %>% select(-qc_total.qc2) %>% 
                  # select(-qc_total.qc3) %>% select(-qc_total.qc4) %>% select(-qc_total.qc5)





