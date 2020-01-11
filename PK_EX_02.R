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
df16 <- df14[c(1:3456), c(6:10)]

# sum up all qc rows
df16 <- df14 %>% mutate(rowSums(df16))

names(df16) <- c("id", "date", "hm", "ta", "lux", "qc1", "qc2", "qc3", "qc4", "qc5", "qc_total")


# Flag statistics per quality check
qc1_count <- length(which(df16$qc1 == 1))
qc1_count # 0
qc1_perc <- qc1_count / 3456
qc1_perc

qc2_count <- length(which(df16$qc2 == 1))
qc2_count # 32
qc2_perc <- qc2_count / 3456
qc2_perc

qc3_count <- length(which(df16$qc3 == 1))
qc3_count # 0
qc3_perc <- qc3_count / 3456
qc3_perc

qc4_count <- length(which(df16$qc4 == 1))
qc4_count # 216
qc4_perc <- qc4_count / 3456
qc4_perc

qc5_count <- length(which(df16$qc5 == 1))
qc5_count # 95
qc5_perc <- qc5_count / 3456
qc5_perc

qc_table <- matrix(c(1,0,2,32,3,0,4,216,5,95), ncol = 2, byrow = TRUE)
colnames(qc_table) <- c("Quality Check", "Number of flags")
qc_table <- as.table(qc_table)
qc_table


# Aggregation to hour if qc_total <= 1 of one hour; if qc_total >= 2 of one hour then no aggregation

# 1: identify qc_total <= 1 and >= 2 for hour and mark as 1 or 0
df17 <- df16 %>% mutate(hour = format(a1, '%H')) %>% 
                  group_by(date, hour) %>% 
                    summarise(ta = mean(ta), lux = mean(lux), qc_total_sum = sum(qc_total)) %>%
                        ungroup()

df18 <- df17 %>% mutate(qc_result = if_else(df17$qc_total_sum <= 1, 1, 0, 0))

library(naniar)
df19 <- df18 %>% replace_with_na(replace = list(qc_result = 0)) %>% 
                   mutate(origin = qc_result)

# replace values of "1" in origin to "H" and from "NA" to "R"
df19$origin[df19$origin == 1] <- "H"
df19$origin[is.na(df19$origin)] <- "R"

# replace ta_hobo values with NA that are NA in qc_result
df19$ta <- ifelse(is.na(df19$qc_result), NA, df19$ta)
sum(is.na(df19$ta)) # 47





#-----------------------------------------------------
# 3 Filling Gaps with Regression Model
#-----------------------------------------------------

#------------------------------------------
# 3.1 Reference Stations
#------------------------------------------

# Import WBI data --> distance to df19: 1.86 km
WBI <- read_delim("WBI.csv", ";", escape_double = FALSE, 
                  trim_ws = TRUE,)

# Import DWD 1443 -> distance to df19: 2.99 km
DWD_1443 <- read_delim("DWD 1443.txt", ";", 
                       escape_double = FALSE, trim_ws = TRUE)

# Import DWD 13667 --> distance to df19: 1.35 km
DWD_13667 <- read_delim("DWD 13667.txt", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

# Create tables like df19
wbi <- WBI %>% mutate(df19$date) %>% 
                select(-Tag) %>% 
                  mutate(df19$hour) %>% 
                    select(-Stunde)
wbi <- wbi[,c(2,3,1)]
names(wbi) <- c("date", "hour", "ta")

dwd_1443 <- DWD_1443 %>% mutate(df19$date) %>% 
                          mutate(df19$hour) %>% 
                            select(-MESS_DATUM, -STATIONS_ID, -QN_9, -RF_TU, -eor)
dwd_1443 <- dwd_1443[,c(2,3,1)]
names(dwd_1443) <- c("date", "hour", "ta")

dwd_13667 <- DWD_13667 %>% mutate(df19$date) %>% 
                            mutate(df19$hour) %>% 
                              select(-STATIONS_ID, -MESS_DATUM, -QUALITAETS_NIVEAU, 
                                        -STRUKTUR_VERSION, -REL_FEUCHTE, -eor)
dwd_13667 <- dwd_13667[,c(2,3,1)]
names(dwd_13667) <- c("date", "hour", "ta")

rm(DWD_13667, DWD_1443, WBI)

# create dataframe for linear model
model_data <- select(df19, date, hour, ta_hobo = ta) %>% 
                mutate(ta_wbi = wbi$ta) %>% 
                  mutate(ta_dwd_1443 = dwd_1443$ta) %>% 
                    mutate(ta_dwd_13667 = dwd_13667$ta)


#------------------------------------------
# 3.2 Regression Model
#------------------------------------------

library(stats)
par(mfrow = c(2,2))
model1 <- lm(data = model_data, ta_hobo ~ ta_wbi, na.action = na.exclude)
summary(model1) # R^2 = 0.9484 
plot(x = model_data$ta_wbi, y = model_data$ta_hobo, 
     main = "Linear Regression Model for HOBO Temperature ~ WBI Temperature", 
        xlab = "WBI Temperature", 
            ylab = "HOBO Temperature")
abline(a = 0, b = 1, col = "red")

model2 <- lm(data = model_data, ta_hobo ~ ta_dwd_1443, na.action = na.exclude)
summary(model2) # R^2 = 0.8637
plot(x = model_data$ta_dwd_1443, y = model_data$ta_hobo,
    main = "Linear Regression Model for HOBO Temperature ~ DWD_1443 Temperature", 
        xlab = "DWD_1443 Temperature", 
            ylab = "HOBO Temperature")
abline(a = 0, b = 1, col = "red")

#-----------------
# model3 = best model --> highest R^2
#-----------------
model3 <- lm(data = model_data, ta_hobo ~ ta_dwd_13667, na.action = na.exclude)
summary(model3) # R^2 = 0.9605
plot(x = model_data$ta_dwd_13667, y = model_data$ta_hobo,
    main = "Linear Regression Model for HOBO Temperature ~ DWD_13667 Temperature", 
        xlab = "DWD_13667 Temperature", 
            ylab = "HOBO Temperature")
abline(a = 0, b = 1, col = "red")


# ?residuals
# residual = difference between prediction of model and the actual value of ta_hobo
# calculate residuals of model3 by subtracting ta_hobo of the fitted values of model3
summary(model_data$ta_hobo - model3$fitted.values)

# 0% indicates that the model explains none of the variability of the 
# response data around its mean.
# 100% indicates that the model explains all the variability of the 
# response data around its mean.

# plot model_data
ggplot(data = model_data) + 
  geom_line(aes(x = date, y = ta_hobo), col = "red") +
    geom_line(aes(x = date, y = ta_wbi), col = "blue") +
      geom_line(aes(x = date, y = ta_dwd_1443), col = "green") +
        geom_line(aes(x = date, y = ta_dwd_13667), col = "black") +
          theme_light()

# extract intercept and slope value from model3
coef(model3)
model3_intercept <- coef(model3)[1]
model3_intercept
model3_slope <- coef(model3)[2]
model3_slope

# prediction formula data --> = NA values from ta_hobo
# predict_formula <- as.numeric(model3_intercept) + as.numeric(model3_intercept) * model_data$ta_dwd_13667
# predict_formula
# ERROR --> values way to high: up to 21 ° Celsius

# predict NA values in ta_hobo with model3 data
model_data$pred <- predict(model3, newdata = model_data)

# replace NAs with predictions from model3
model_data$interp <- ifelse(is.na(model_data$ta_hobo), model_data$pred, model_data$ta_hobo)

# plot existing and interpolated data
ggplot() +
  geom_point(data = model_data, aes(date, ta_hobo), size = 4) +
      geom_point(data = model_data, aes(date, interp), col = "red")

#plot model, hobo and dwd_13667 data
ggplot() +
  geom_line(data = model_data, aes(date, pred), col = "black") +
    geom_line(data = model_data, aes(date, ta_hobo), col = "red") +
      geom_line(data = model_data, aes(date, ta_dwd_13667), col = "blue")





#-----------------------------------------------------
# 4 Upload Hourly Series
#-----------------------------------------------------

df20 <- select(model_data, date, hour) %>% 
          mutate(origin = df19$origin) %>% 
            mutate(th = round(model_data$interp, digits = 3))
df20 <- df20[,c(1,2,4,3)]

write_tsv(df2, "C:/Users/Patrick Kacic/Documents/Environmental Sciences/1. Semester/Data Storage, Collection, Management/10347386_Th.tsv")

