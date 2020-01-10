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


###READ TABLE###


df2 <- read_tsv("https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2020/_01header/10347351.tsv", skip=5)



###EXERCISE 2###


###QUALITY CONTROL 1 -temperature must be between -20C and 70C###

df2$qc1 <- 0
df2$qc1[df2$ta <= -20 | df2$ta >= 70] <- 1


###QUALITY CONTROL 2 - compare rate of change in temperature###

###Comparing data using Lead and Lag Functions###
df2$qc2lead <- lead(df2$ta,1) - df2$ta
df2$qc2lag <- df2$ta - lag(df2$ta,1)

###Flag for QC2 showing high temperature changes - used only Lag Function###
df2$qc2 <- 0
df2$qc2[df2$qc2lag >= 1.000 | df2$qc2lag <= -1.000] <- 1  


###QUALITY CONTROL 3 - Flag comparing the last 5 data points for temperature change###

# Minimum variability
check_temp_var <- function(in_df) {
  # checks if the temperature has changed compared to the hour before
  # if there is no difference, the tupel gets the value 1, otherwise, it gets the value 0
  in_df <- in_df[order(in_df$id),]
  in_df["qc3"] = 0
  n <- length(in_df$ta)
  for (index in 6:n) {
    if ((in_df$ta[index]==in_df$ta[index-1]) &
        (in_df$ta[index-1]==in_df$ta[index-2]) &
        (in_df$ta[index-2]==in_df$ta[index-3]) &
        (in_df$ta[index-3]==in_df$ta[index-4]) &
        (in_df$ta[index-4]==in_df$ta[index-5])){
      in_df$qc3[index] = 1
    }
  }
  return(in_df)
}

df2 <- check_temp_var(df2)


###QUALITY CONTROL 4 - LIGH INTENSITY CHECKS QUANTILES L1 95% AND L2 99%###

quantile(df2$lux, probs = c(.95, .99))


check_lux <- function(in_df) {
  # checks if the lux value is above certain thresholds to exclude temperature data influenced by radiation
  # if the lux value is higher than the threshold, the tupel gets the value 1, otherwise, it gets the value 0
  in_df["qc4"] = 0
  day <- in_df[(hour(in_df$hm) >= 6) & (hour(in_df$hm) < 18),]
  night <- in_df[(hour(in_df$hm) < 6) | (hour(in_df$hm) >= 18),]
  # only use day time values (sun only shines during the day)
  n <- length(day$lux)
  l_one <- quantile(day$lux, .95)
  l_two <- quantile(day$lux, .99)
  # because of the low lux values (all under 1200),
  # I did not consider any threshold good, that's why I took 95%- and 99%-quantiles
  for (index in 2:n-1) {
    if (day$lux[index] > l_one){
      day$qc4[(index-1):(index+1)] = 1
      # for L1, 1 tupel before and after are also flagged
    }
  }
  for (index in 4:n-3) {
    if (day$lux[index] > l_two){
      day$qc4[(index-3):(index+3)] = 1
      # for L2 3 tuples before and after are also flagged
    }
  }
  out_df <- rbind(day, night)
  out_df <- out_df[order(out_df$id),]
  return(out_df)
}

df2 <- check_lux(df2)




###PART 2 - HOUR COMPARISONS###

###1 - Create QC Total###

df2$QC_Total = rowSums(df2[,c(6,9,10,15)])
df2

tbl_df(df2)

df2$Hour <- as.POSIXlt(df2$hm)$hour
df2$DayHour <- paste(df2$date, df2$Hour)

df3 <- aggregate(cbind(df2$ta, df2$lux, df2$QC_Total) ~ df2$DayHour, data = df2, FUN = mean)




