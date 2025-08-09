
################################################################################

#Preprocess weather data from XEMA station X8_ZonaUniversitaria (2021-2024)

################################################################################

#Load libraries

library(lubridate)
library(magrittr) 
library(dplyr) 
library(activity)
library(tidyr)
library (readr)
library(TTR)

#Set up working directory

setwd("/home/yourdirectory")

#Load data

xema_x8_21 <- read.csv("/home/yourpath/df_meteocat_aspb_X8_2021.csv",sep= ";",dec = ",")
xema_x8_22 <- read.csv("/home/yourpath/df_meteocat_aspb_X8_2022.csv",sep= ";",dec = ",")
xema_x8_23 <- read.csv("/home/yourpath/df_meteocat_aspb_X8_2023.csv",sep= ";",dec = ",")
xema_x8_24 <- read.csv("/home/yourpath/df_meteocat_aspb_X8_2024.csv",sep= ";",dec = ",")

#Join df

xema <- rbind(xema_x8_21, xema_x8_22, xema_x8_23, xema_x8_24)

#Delete unuseful columns

xema <- xema[,-c(2,3,4,5,6,10,11,13)] 

#Ensure "timestamp" is a POSIXct object

xema$timestamp <- as.POSIXct(xema$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") 

summary(xema)#Are there any NA values in xema? Yes, there are 14
xema <- xema %>% drop_na() #Delete rows with NAs
sum(is.na(xema)) #Verify there are not NAs anymore

#Rename variables in xema

xema = dplyr::rename(xema, datetime_utc = timestamp, temp_xema = temperature, RH_xema = relative_humidity, 
                         precip_xema = precipitation, solar_rad_xema = global_solar_radiation)

#Aggregate variable HOD (Hour of the Day) 

xema <- xema %>%
  mutate(HOD_utc = as.numeric(format(datetime_utc, "%H")) + 
           as.numeric(format(datetime_utc, "%M")) / 60 + 
           as.numeric(format(datetime_utc, "%S")) / 3600) %>%
  mutate (HOD_utc = floor(HOD_utc))%>%
  mutate (datetime_utc = floor_date(datetime_utc, unit = "hour")) #round time

#Round datetime_utc to hours

xema$utc_1h <- floor_date(xema$datetime_utc, unit = "hour") 
#E.g.: Between 13:00 and 13:59, utc_1h = 13:00

#Group variables by hour since weather data are each 30min

xema <- xema %>%
  group_by(utc_1h) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) 

#Aggregate new variables (date, year, latitude and longitude)

xema$date <- as.Date(xema$utc_1h)

Sys.setlocale("LC_TIME", "C") #change system language to English
xema$month <- format(xema$date, "%b")

xema$year <-year(xema$date)

xema$latitude <-41.37919 #Location of the weather station
xema$longitude <-2.10540

#Introduce "season" variable according to specific dates
#Equinox and solstice dates for each year obtained from:
#http://www.xemaskin.uma.es/cimes/pages/astronomy/equisol.php

xema<- xema %>%
  mutate(season = case_when(
    
    (date >= as.Date("2021-01-01") & date <= as.Date("2021-03-19")) |
      (date >= as.Date("2021-12-21") & date <= as.Date("2022-03-19")) |
      (date >= as.Date("2022-12-21") & date <= as.Date("2023-03-19")) |
      (date >= as.Date("2023-12-22") & date <= as.Date("2024-03-19")) |
      (date >= as.Date("2024-12-21") & date <= as.Date("2024-12-31")) ~ "winter",
    
    (date >= as.Date("2021-03-20") & date <= as.Date("2021-06-20")) |
      (date >= as.Date("2022-03-20") & date <= as.Date("2022-06-20")) |
      (date >= as.Date("2023-03-20") & date <= as.Date("2023-06-20")) |
      (date >= as.Date("2024-03-20") & date <= as.Date("2024-06-19"))  ~ "spring",
    
    (date >= as.Date("2021-06-21") & date <= as.Date("2021-09-21")) |
      (date >= as.Date("2022-06-21") & date <= as.Date("2022-09-22")) |
      (date >= as.Date("2023-06-21") & date <= as.Date("2023-09-22")) |
      (date >= as.Date("2024-06-20") & date <= as.Date("2024-09-21")) ~ "summer",
    
    (date >= as.Date("2021-09-22") & date <= as.Date("2021-12-20")) |
      (date >= as.Date("2022-09-23") & date <= as.Date("2022-12-20")) |
      (date >= as.Date("2023-09-23") & date <= as.Date("2023-12-21")) |
      (date >= as.Date("2024-09-22") & date <= as.Date("2024-12-20")) ~ "autumn", 
    
    TRUE ~ NA_character_
  ))

# Transform datetime_utc (in UTC) to local time (Europe/Madrid)

xema$datetime_local <- with_tz(xema$utc_1h, tz = "Europe/Madrid")

# Define is_st function to detect summer time 

is_st <- function(datetime) {
  return(ifelse(dst(datetime), 2, 1))  # is_st = 2 (summer), is_st = 1 (winter)
}

# Calculate time zone (tz) with is_st function

xema$tz <- sapply(xema$datetime_local, is_st)

#Obtain approximate hours for sunrise, sunset and day length with package "Activity"

snt<-get_suntimes(xema$datetime_local,
                  xema$latitude,  
                  xema$longitude,
                  offset= xema$tz) #offset = tz

xema$sunrise_local_decimal<-snt$sunrise #Clock time in decimals
xema$sunset_local_decimal<-snt$sunset #Clock time in decimals
xema$daylength<-snt$daylength

#Local time in decimals (time_local_decimal)

xema <- xema %>%
  mutate(time_local_decimal = as.numeric(format(datetime_local, "%H")) + 
           as.numeric(format(datetime_local, "%M")) / 60 + 
           as.numeric(format(datetime_local, "%S")) / 3600)

#Time distance (in decimals) from sunrise and sunset (dist_sunrise, dist_sunset)

xema$dist_sunrise<-abs(xema$time_local_decimal-xema$sunrise_local_decimal)
xema$dist_sunset<-abs(xema$time_local_decimal-xema$sunset_local_decimal)

#Day of year (DOY)

xema$DOY <-yday(xema$date)

#Cumulative precipitation in the last 24 hours (cum_precip_24h)

xema <- xema[order(xema$utc_1h), ] # Ensure chronological order for cumulative variables

xema$cum_precip_24h <- c(NA, runSum(xema$precip_xema, n = 24)[-length(xema$precip_xema)])

#Cumulative precipitation in the last 12 hours (cum_precip_12h)

xema$cum_precip_12h <- c(NA, runSum(xema$precip_xema, n = 12)[-length(xema$precip_xema)])

#Cumulative precipitation in the last 6 hours (cum_precip_6h)

xema$cum_precip_6h <- c(NA, runSum(xema$precip_xema, n = 6)[-length(xema$precip_xema)])

#Mean temperature in the last 24 hours (av_temp_24h)

xema$av_temp_24h <- runMean(c(NA, head(xema$temp_xema, -1)), n = 24)

#Mean temperature in the last 12 hours (av_temp_12h)

xema$av_temp_12h <- runMean(c(NA, head(xema$temp_xema, -1)), n = 12)

#Mean temperature in the last 6 hours (av_temp_6h)

xema$av_temp_6h <- runMean(c(NA, head(xema$temp_xema, -1)), n = 6)

#Mean RH in the last 24 hours (av_RH_24h)

xema$av_RH_24h <- runMean(c(NA, head(xema$RH_xema, -1)), n = 24)

#Mean RH in the last 12 hours (av_RH_12h)

xema$av_RH_12h <- runMean(c(NA, head(xema$RH_xema, -1)), n = 12)

#Mean RH in the last 6 hours (av_RH_6h)

xema$av_RH_6h <- runMean(c(NA, head(xema$RH_xema, -1)), n = 6)

#Activity Degree Hours (ADH)

#We have transformed the concept of Growing Degree Hours (GDH) 
#And we have applied it to mosquito activity, not growth 
#A mosquito is only active within a range of temperatures (T_min and T_max)
#Below T_min and above T_max the activity is not possible
#We have established a T_min = 10ºC and a T_max = 35ºC according to the literature and our data

T_min <- 10 
T_max <- 35 

#If the recorded temperature at X hour (temp) is above T_min and below T_max, ADH = Tª - T_min
#If not, ADH = 0

xema$ADH <- ifelse(xema$temp_xema > T_min & xema$temp_xema < T_max, 
                       xema$temp_xema - T_min, 0)

#Cumulative ADH in the last 24 hours (cum_ADH_24h)

xema$cum_ADH_24h <- c(NA, runSum(xema$ADH, n = 24)[-length(xema$ADH)])

#Cumulative ADH in the last 12 hours (cum_ADH_12h)

xema$cum_ADH_12h <- c(NA, runSum(xema$ADH, n = 12)[-length(xema$ADH)])

#Cumulative ADH in the last 6 hours (cum_ADH_6h)

xema$cum_ADH_6h <- c(NA, runSum(xema$ADH, n = 6)[-length(xema$ADH)])

#Save df in .csv format in the current wd

write.csv(xema, 
          file = "~/yourpath/df_xema.csv", 
          row.names = FALSE)


