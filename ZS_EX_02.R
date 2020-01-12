# load packages
library("tidyverse")
library("lubridate")
library("rio")
library("skimr")
library("zoo")


################################
# Exercise 2.1 - Quality Checks
################################

# TODO: er wollte, dass wir lead() und lag() und mutate() benutzen bei "Plausible rate of change"

# read in new data
df_2 <- read_tsv("https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2020/_01header/10347359.tsv", skip=5)

# Plausible values
check_temp_range <- function(in_df) {
  # checks every temp value to be within -20 and +70
  # if not in the range, the tupel gets the value 1, otherwise, it gets the value 0
  in_df <- in_df[order(in_df$id),]
  in_df["qc_temp_range"] = 0
  index = 1
  for (temp in in_df$ta) {
    if ((temp < (-20)) | (temp > 70)){
      in_df$qc_temp_range[index] = 1
    }
    index = index+1
  }
  return(in_df)
}

df_2 <- check_temp_range(df_2)


# Plausible rate of change
check_temp_change <- function(in_df) {
  # checks the temperature difference of one value to the previous value
  # if the difference is higher than 1, the tupel gets the value 1, otherwise, it gets the value 0
  in_df <- in_df[order(in_df$id),]
  in_df["qc_temp_change"] = 0
  n <- length(in_df$ta)
  for (index in 2:n) {
    if (abs(in_df$ta[index]-in_df$ta[index-1]) > 1 ){
      in_df$qc_temp_change[index] = 1
    }
  }
  return(in_df)
}

df_2 <- check_temp_change(df_2)


# Minimum variability
check_temp_var <- function(in_df) {
  # checks if the temperature has changed compared to the hour before
  # if there is no difference, the tupel gets the value 1, otherwise, it gets the value 0
  in_df <- in_df[order(in_df$id),]
  in_df["qc_temp_var"] = 0
  n <- length(in_df$ta)
  for (index in 6:n) {
    if ((in_df$ta[index]==in_df$ta[index-1]) &
        (in_df$ta[index-1]==in_df$ta[index-2]) &
        (in_df$ta[index-2]==in_df$ta[index-3]) &
        (in_df$ta[index-3]==in_df$ta[index-4]) &
        (in_df$ta[index-4]==in_df$ta[index-5])){
      in_df$qc_temp_var[index] = 1
    }
  }
  return(in_df)
}

df_2 <- check_temp_var(df_2)


# Light intensity (during day time!)
check_lux <- function(in_df) {
  # checks if the lux value is above certain thresholds to exclude temperature data influenced by radiation
  # if the lux value is higher than the threshold, the tupel gets the value 1, otherwise, it gets the value 0
  in_df["qc_lux"] = 0
  day <- in_df[(hour(in_df$hm) >= 6) & (hour(in_df$hm) < 18),]
  night <- in_df[(hour(in_df$hm) < 6) | (hour(in_df$hm) >= 18),]
  day <- day[order(day$id),]
  # only use day time values (sun only shines during the day)
  n <- length(day$lux)
  l_one <- quantile(day$lux, .95)
  l_two <- quantile(day$lux, .99)
  # because of the low lux values (all under 1200),
  # I did not consider any threshold good, that's why I took 95%- and 99%-quantiles
  for (index in 2:(n-1)) {
    if (day$lux[index] > l_one){
      day$qc_lux[(index-1):(index+1)] = 1
      # for L1, 1 tupel before and after are also flagged
    }
  }
  for (index in 4:(n-3)) {
    if (day$lux[index] > l_two){
      day$qc_lux[(index-3):(index+3)] = 1
      # for L2 3 tuples before and after are also flagged
    }
  }
  out_df <- rbind(day, night)
  out_df <- out_df[order(out_df$id),]
  return(out_df)
}

df_2 <- check_lux(df_2)


#################################
# Exercise 2.1 - Flagging System
#################################

# number of flagged data points
# measurement range:
sum(df_2$qc_temp_range) # no malfunctions from the sensor
# plausible rate of change:
sum(df_2$qc_temp_change) # when I opened the window maybe
# minimum variablility:
sum(df_2$qc_temp_var) # maybe because it was under yoghurt lid and air circulation was bad?
# light intensity:
sum(df_2$qc_lux) # actually it should be okay, but included for the sake of the exercise

# set flagged data to NA
flag_to_NA <- function(in_df) {
  in_df <- mutate(in_df, qc_total=qc_temp_range+qc_temp_change+qc_temp_var+qc_lux)
  # counts together the flags  in qc_total
  in_df$ta[in_df$qc_total > 0] <- NA
  # when at least one flag, assign NA-value to ta
  return(in_df)
}

df_2 <- flag_to_NA(df_2)


# aggregate 60min into 1h if at least 5 data points, otherwise set to NA
aggregate_hour <- function(in_df) {
  # aggregates hourly temperature into th
  # assigns an "H" to data points filled with HOBO-data
  # and "R" to those later filled with a regression
  out_df <- in_df %>%
    mutate(hour=hour(hm), th=0) %>%
    group_by(date, hour) %>%
    summarize(th=mean(ta[sum(!is.na(ta))>=5], na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(origin="H")
  out_df$th[is.na(out_df$th)] <- NA
  out_df$origin[is.na(out_df$th)] <- "R"
  return(out_df)
}

df_3 <- aggregate_hour(df_2)


##############################
# Exercise 2.3 - Filling Gaps
##############################

# load data files
# all temperature data hourly from 2m above ground
setwd("H:/Daten/Studium/2_Master/1_Semester/57170_Data_Collection_Storage_Management/Data/Wetterdienst/aktuell")
station_wbi <- read.csv2("WBI_station.csv") #ready
station_airport <- read.delim("DWD_01443.txt", sep=";", col.names=c("useless1", "date_hm", "useless2", "th",
                                                                    "useless3", "useless4")) #too long
station_city <- read.delim("DWD_13667.txt", sep=";", col.names=c("useless1", "date_hm", "useless2", "useless3",
                                                                 "th", "useless4", "useless5")) #too long
# delete unnecessary columns and rows
station_airport <- station_airport %>%
  select(-c("useless1", "useless2", "useless3", "useless4")) %>%
  filter((date_hm >= 2019121400) & (date_hm<2020010700)) %>%
  arrange(date_hm)

station_city <- station_city %>%
  select(-c("useless1", "useless2", "useless3", "useless4", "useless5")) %>%
  filter((date_hm >= 2019121400) & (date_hm<2020010700)) %>%
  arrange(date_hm)

# distances from the station to my location
library(geosphere)
distm(c(48.0031,7.8204) ,c(47.98,7.83))
distm(c(48.0031,7.8204) ,c(48.0232,7.8343))
distm(c(48.0031,7.8204) ,c(48.001,7.845))
# HOBO location:        48.0031°  Nord,   7.8204° Ost
# WBI station location: 47.98°    Nord,   7.83°   Ost -> 2760.067 m
# DWD airport station:  48.0232°  Nord,   7.8343° Ost -> 2697.675 m
# DWD city station:     48.001°   Nord,   7.845	° Ost -> 2730.475 m
# -> DWD city station nearest, but all distances around 2,7km

# order the data frame again, just to be sure
df_3 <- df_3 %>%
  arrange(date, hour)


# fit three regressions
lm_wbi <- lm(df_3$th ~ station_wbi$AVG_TA200)
lm_airport <- lm(df_3$th ~ station_airport$th)
lm_city <- lm(df_3$th ~ station_city$th)

# view regression summaries -> the wbi station has the best R-square
summary(lm_wbi)     # Multiple R-squared:  0.9862,	Adjusted R-squared:  0.9862
summary(lm_airport) # Multiple R-squared:  0.9539,	Adjusted R-squared:  0.9538
summary(lm_city)    # Multiple R-squared:  0.9744,	Adjusted R-squared:  0.9744

# look at diagnistic plots -> the wbi station hast the best residual plot, the rest is equally "good"
par(mfrow=c(2,2))
plot(lm_wbi)
# in the beginning underestimated, in the end overestimated
# residual outliers 469/111/52, line quite even

plot(lm_airport)
# in the beginning and end overestimated, in the middle underestimated
# residual outliers 146/96/53, line curvy

plot(lm_city)
# in the beginning underestimated, in the end overestimated
# residual outliers 146/111/90, line slightly curvy

# scatterplot showing the relationship between your HOBO data and the reference data and the regression model.
par(mfrow=c(1,1))
plot(df_3$th ~ station_wbi$AVG_TA200, main="HOBO Data and WBI Station Data\nfrom 2019/12/14 to 2020/01/06",
     las=1, ylab="HOBO Temperature [°C]", xlab="WBI Station Temperature [°C]", col=scales::alpha("black", 0.5), pch=16, cex=1.5)
points(station_wbi$AVG_TA200[469], df_3$th[469], pch=16, cex=1.5, col=scales::alpha("darkred", 0.7))
points(station_wbi$AVG_TA200[52], df_3$th[52], pch=16, cex=1.5, col=scales::alpha("darkred", 0.7))
points(station_wbi$AVG_TA200[111], df_3$th[111], pch=16, cex=1.5, col=scales::alpha("darkred", 0.7))
# TODO: maybe do an outlier test and mark these?
abline(0.46636, 0.98015, col="red4", lwd=2)

# redo for shorter variable names for predict()
values_hobo <- df_3$th
values_station <- station_wbi$AVG_TA200
lm_wbi <- lm(values_hobo~values_station) 

# predict missing values
missing_index <- which(is.na(df_3$th)) # get indexes of NA-values
missing_values <- station_wbi$AVG_TA200[missing_index] # get values from station at missing_indexes
preds <- predict(lm_wbi, data.frame(values_station = missing_values)) # predict new values
df_preds <- cbind(missing_index, preds) # make a dataframe out of index and predicted value
df_3$th[df_preds[,1]] <- df_preds[,2] # insert predicted values


####################################
# Exercise 2.4 - Prepare for Upload
####################################

df_final <- df_3
df_final$th <- round(df_final$th, 3)

# save as .tsv
setwd("H:/Daten/Studium/2_Master/1_Semester/57170_Data_Collection_Storage_Management/Data")
write.table(df_final, file='10347359_Th.tsv', sep='\t', quote=F, row.names = FALSE)

