# load packages
library("tidyverse")
library("lubridate")
library("rio")
library("skimr")
library("zoo")


#####################################
# Exercise 3.1 - Temperature Indices
#####################################

df_temp <- read_tsv("https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2020/_02hourly/10347359_Th.tsv")
day <- df_temp[(df_temp$hour >= 6) & (df_temp$hour < 18),]
night <- df_temp[(df_temp$hour < 6) | (df_temp$hour >= 18),]

# Average temperature
round(mean(df_temp$th),3)

# Mean day temperature
round(mean(day$th),3)

# Mean night temperature
round(mean(night$th),3)

# Difference TNHT - TDAY
round(abs(mean(night$th)-mean(day$th)),3)

# Daily amplitude
df_ampli <- df_temp %>%
  group_by(date) %>%
  summarize(t_min=min(th), t_max=max(th)) %>%
  ungroup() %>%
  mutate(t_range=abs(t_min-t_max))
round(mean(df_ampli$t_range),3)

# Flashiness index
n <- length(df_temp$th)
x = 0
for (i in 2:n) {x = x + abs(df_temp$th[i] - df_temp$th[i-1])} 
round((x/(n-1)), 3)

# Most rapid temperature change in 3 hours
change <- function (vec) {return(abs(max(vec)-min(vec)))}
df_change <- df_temp %>%
  mutate(t_3 = rollapply(th, 3, FUN=change, align="right", fill=NA))
round(max(df_change$t_3, na.rm = TRUE),3)

# Most rapid temperature change in 12 hours
df_change <- df_change %>%
  mutate(t_12 = rollapply(th, 12, FUN=change, align="right", fill=NA))
round(max(df_change$t_12, na.rm = TRUE),3)

# Fraction of missing values
df_HR <- count(df_temp, origin)
round(df_HR[2,2]/df_HR[1,2],3)


###############################
# Exercise 3.2 - Light Indices
###############################

df_lux <- read_tsv("https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2020/_01header/10347359.tsv", skip=5)

# Average light intensity at midday (12:00)
round(mean(df_lux$lux[(hour(df_lux$hm)==12 & minute(df_lux$hm)==0)]),3)

# 95th percentile of light intensity
round(quantile(df_lux$lux, .95),3)


###################################
# Exercise 3.4 - Long-Term Average
###################################

# load data
setwd("H:/Daten/Studium/2_Master/1_Semester/57170_Data_Collection_Storage_Management/Data/Wetterdienst/historisch")
df_old <- read.delim("DWD_01443_historic.txt", sep=";", col.names=c("useless1", "date_hm", "useless2", "th",
                                                                    "useless3", "useless4")) #too long
setwd("H:/Daten/Studium/2_Master/1_Semester/57170_Data_Collection_Storage_Management/Data/Wetterdienst/aktuell")
df_new <- read.delim("DWD_01443.txt", sep=";", col.names=c("useless1", "date_hm", "useless2", "th",
                                                           "useless3", "useless4")) #too long

# delete unnecessary columns
df_old <- df_old %>%
  select(-c("useless1", "useless2", "useless3", "useless4"))
df_new <- df_new %>%
  select(-c("useless1", "useless2", "useless3", "useless4"))

#get yearly averages
year_average <- function(in_df, start_hm, end_hm) {
  year_avg <- mean(in_df$th[(in_df$date_hm >= start_hm) & (in_df$date_hm < end_hm)])
  return(year_avg)
}

avg_19_20 <- year_average(df_new, 2019121400, 2020010700)
avg_18_19 <- year_average(df_new, 2018121400, 2019010700)
avg_17_18 <- year_average(df_old, 2017121400, 2018010700)
avg_16_17 <- year_average(df_old, 2016121400, 2017010700)
avg_15_16 <- year_average(df_old, 2015121400, 2016010700)
avg_14_15 <- year_average(df_old, 2014121400, 2015010700)
avg_13_14 <- year_average(df_old, 2013121400, 2014010700)
avg_12_13 <- year_average(df_old, 2012121400, 2013010700)
avg_11_12 <- year_average(df_old, 2011121400, 2012010700)
avg_10_11 <- year_average(df_old, 2010121400, 2011010700)
avg_09_10 <- year_average(df_old, 2009121400, 2010010700)
avg_08_09 <- year_average(df_old, 2008121400, 2009010700)
avg_07_08 <- year_average(df_old, 2007121400, 2008010700)
avg_06_07 <- year_average(df_old, 2006121400, 2007010700)
avg_05_06 <- year_average(df_old, 2005121400, 2006010700)
avg_04_05 <- year_average(df_old, 2004121400, 2005010700)
avg_03_04 <- year_average(df_old, 2003121400, 2004010700)
avg_02_03 <- year_average(df_old, 2002121400, 2003010700)
avg_01_02 <- year_average(df_old, 2001121400, 2002010700)
avg_00_01 <- year_average(df_old, 2000121400, 2001010700)


# comparison hobo average with longterm average (but you would have to deal with regression here to y=mx+b)
hobo_avg <- round(mean(df_temp$th),3)

longterm_avg <- mean(c(avg_19_20, avg_18_19, avg_17_18, avg_16_17, avg_15_16, avg_14_15, avg_13_14, avg_12_13, avg_11_12,
                       avg_10_11, avg_09_10, avg_08_09, avg_07_08, avg_06_07, avg_05_06, avg_04_05, avg_03_04, avg_02_03,
                       avg_01_02, avg_00_01))

longterm_var <- var(c(avg_19_20, avg_18_19, avg_17_18, avg_16_17, avg_15_16, avg_14_15, avg_13_14, avg_12_13, avg_11_12,
                      avg_10_11, avg_09_10, avg_08_09, avg_07_08, avg_06_07, avg_05_06, avg_04_05, avg_03_04, avg_02_03,
                      avg_01_02, avg_00_01))

