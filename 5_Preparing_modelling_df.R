################################################################################

#Preparing modelling dataframes

################################################################################

#Load libraries

library(lubridate)
library(knitr)
library(ggsci)
library(magrittr) 
library(dplyr) 
library(zoo)
library(readr)

#Set up working directory

setwd("/yourdirectory")

#Load data

df_aspb <- read.csv("~/yourpath/df_aspb_2021_2024.csv")
df_xema <- read_csv("~/yourpath/df_xema.csv")
df_sensor <- read_csv("~/yourpath/df_TestPulse_2021_2024.csv")
sensorStatus <- read.csv("~/yourpath/sensor_status.csv")

################################################################################

#Filter data from May to October according to S1_Supplementary information

df_aspb <- df_aspb[df_aspb$month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct"), ]
df_xema <- df_xema [df_xema$month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct"), ]
df_sensor <- df_sensor[df_sensor$month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct"), ]

################################################################################

#Determine sensor status

#Expand the original df (by days) to hours

HOD <- tibble(HOD = 0:23)
sensorStatus <- sensorStatus %>%
  mutate(date = dmy(date))%>%
  crossing(HOD) %>%  
  mutate(utc_1h = as.POSIXct(paste(date, HOD), format="%Y-%m-%d %H", tz="UTC"))

Sys.setlocale("LC_TIME", "C") #change system language to English
sensorStatus$month <- format(sensorStatus$date, "%b")

#Filter df by date and sensor status
sensorStatus <- sensorStatus [sensorStatus$month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct") 
                              & sensorStatus$year %in% c("2021", "2022", "2023", "2024"), ]

#Delete unuseful columns

sensorStatus<- sensorStatus[, -c(1,3,4,6,8)]

#Join df by trap and utc_1h

#Ensure same format for join variables

#For unknown reasons, when loading df_aspb, some datetime objects (from 00:00 to 01:00) are not being processed correctly

df_aspb$datetime_utc <- as.POSIXct(df_aspb$datetime_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
df_aspb$utc_1h <- floor_date(df_aspb$datetime_utc, unit = "hour")

sensorStatus$utc_1h <- as.POSIXct(sensorStatus$utc_1h, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
df_sensor$utc_1h <- as.POSIXct(df_sensor$utc_1h, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#First join (sensor status + testpulse)
#Objective: define more accurately periods of sensor activity/inactivity

df_sensor1 <- full_join(sensorStatus,df_sensor, by = c("utc_1h","trap"))

#There are some hours where there is no testpulse (the sensor was OFF) but the sensor status is ON
#This is because sensor status was calculated for the whole day
#Change sensor status to OFF in these cases

df_sensor1$status <-ifelse(df_sensor1$status=="ON" & is.na(df_sensor1$HOD_utc),"OFF",df_sensor1$status)

#There are other cases, where the sensor status is "unknown" and there is no testpulse
#That means that the sensor was also OFF

df_sensor1$status <-ifelse(df_sensor1$status=="UNKNOWN" & is.na(df_sensor1$HOD_utc),"OFF",df_sensor1$status)

#In those cases when the status is UNKNOWN but there is testpulse, the status should be "ON"

df_sensor1$status <-ifelse(df_sensor1$status=="UNKNOWN" & !is.na(df_sensor1$HOD_utc),"ON",df_sensor1$status)

#In those cases the status is OFF but there is testpulse, the status should be "ON"

df_sensor1$status <-ifelse(df_sensor1$status=="OFF" & !is.na(df_sensor1$HOD_utc),"ON",df_sensor1$status)

#Subset df_sensor1 to keep useful columns for next joins

df_sensor1 <- df_sensor1[, c(1, 2, 3)]

################################################################################

#Process smart-trap data

#Split "class" in df_asb in two columns (genus and sex)

df_aspb <- df_aspb %>%
  separate(class, into = c("genus", "sex"), sep = " ")

#Percentage of male/female

df_aspb %>%
  count(sex) %>%
  mutate(percent = n / sum(n) * 100)

#   sex     n  percent
# female 16941 65.21538
#   male  9036 34.78462

df_aspb %>%
  count(genus, sex) %>%
  group_by(genus) %>%
  mutate(percent = n / sum(n) * 100)

#genus sex        n percent
#Aedes female 11538    62.8
#Aedes male    6845    37.2
#Culex female  5403    71.1
#Culex male    2191    28.9

#The percentage of males is not negligible
#Try to remove males from the df and to model again

df_aspb_female <- df_aspb[df_aspb$sex=="female", ]

#Change df_aspb from long format to wide format (count number of mosquitoes of the same genus/trap/hour)

df_aspb1 <- df_aspb %>%
  group_by(trap,utc_1h, genus, temp_sensor, RH_sensor) %>%
  summarise(counts = n(), .groups = "drop") %>%
  pivot_wider(names_from = genus, values_from = counts, values_fill = 0)

df_aspb1_female <- df_aspb_female %>%
  group_by(trap,utc_1h, genus, temp_sensor, RH_sensor) %>%
  summarise(counts = n(), .groups = "drop") %>%
  pivot_wider(names_from = genus, values_from = counts, values_fill = 0)

#Second join: join data from sensor status (accurate) + mosquito classification
df_aspb2 <- full_join(df_sensor1,df_aspb1, by = c("utc_1h","trap"))

df_aspb2_female <- full_join(df_sensor1,df_aspb1_female, by = c("utc_1h","trap"))

#In df_aspb2, some cases the sensor (status) is ON, but Aedes and Culex are NA
#If the smart-trap was ON at that moment, Aedes and Culex counts should be 0 and not NA
#These are true 0 (there is a sampling effort but no mosquitoes are recorded)
#NA appear because in df_aspb there is only presence data 
#When joining with df_sensor, some datetimes with no captures are filled with NAs
#When the smart-traps are OFF, there are no sampling effort and the counts are NAs.
#Change NA values with 0 when sensor status is ON

#dummie test
kk<- df_aspb2[df_aspb2$status=="ON" & is.na(df_aspb2$Aedes) & is.na(df_aspb2$Culex), ]
kk <- kk %>%
  mutate(
    Aedes = ifelse(status == "ON" & is.na(Aedes) & is.na(Culex), 0, Aedes),
    Culex = ifelse(status == "ON" & Aedes==0 & is.na(Culex), 0, Culex))
#checked

df_aspb2 <- df_aspb2 %>%
  mutate(
    Aedes = ifelse(status == "ON" & is.na(Aedes) & is.na(Culex), 0, Aedes),
    Culex = ifelse(status == "ON" & Aedes==0 & is.na(Culex), 0, Culex))

df_aspb2_female <- df_aspb2_female %>%
  mutate(
    Aedes = ifelse(status == "ON" & is.na(Aedes) & is.na(Culex), 0, Aedes),
    Culex = ifelse(status == "ON" & Aedes==0 & is.na(Culex), 0, Culex))
#Now all 0 are true absences

#Group by utc_1h and aggregate the total Aedes and Culex counts for all traps
#Count the number of operative traps to calculate sampling effort

df_aspb3 <- df_aspb2 %>%
  group_by(utc_1h) %>%
  summarise(
    Aedes = sum(Aedes[status == "ON"], na.rm = TRUE), 
    Culex = sum(Culex[status == "ON"], na.rm = TRUE),  
    N_operative_traps = n_distinct(trap[status == "ON"]) 
  ) %>%
  ungroup()

df_aspb3_female <- df_aspb2_female %>%
  group_by(utc_1h) %>%
  summarise(
    Aedes = sum(Aedes[status == "ON"], na.rm = TRUE), 
    Culex = sum(Culex[status == "ON"], na.rm = TRUE),  
    N_operative_traps = n_distinct(trap[status == "ON"]) 
  ) %>%
  ungroup()

#If N_operative_traps = 0, Aedes & Culex should be NA

df_aspb3$Aedes <-ifelse(df_aspb3$Aedes==0 & df_aspb3$N_operative_traps==0,NA,df_aspb3$Aedes)
df_aspb3$Culex <-ifelse(df_aspb3$Culex==0 & df_aspb3$N_operative_traps==0,NA,df_aspb3$Culex)

df_aspb3_female$Aedes <-ifelse(df_aspb3_female$Aedes==0 & df_aspb3_female$N_operative_traps==0,NA,df_aspb3_female$Aedes)
df_aspb3_female$Culex <-ifelse(df_aspb3_female$Culex==0 & df_aspb3_female$N_operative_traps==0,NA,df_aspb3_female$Culex)

#Introduce sampling effort = number of mosquitoes / number of operative traps at that moment

df_aspb4 <- df_aspb3 %>%
  mutate( 
    Aedes_SE = Aedes / N_operative_traps,
    Culex_SE = Culex / N_operative_traps) %>%
  mutate(across(c(Aedes_SE, Culex_SE), ~ replace(., is.nan(.), 0)))

df_aspb4_female <- df_aspb3_female %>%
  mutate( 
    Aedes_SE = Aedes / N_operative_traps,
    Culex_SE = Culex / N_operative_traps) %>%
  mutate(across(c(Aedes_SE, Culex_SE), ~ replace(., is.nan(.), 0)))

#Now there are two relevant df for modelling
#df_aspb2 <- Mosquito counts/trap
#df_aspb4 <- which has aggregated info (Total number of mosquitoes in BCN per sampling effort)

#df_aspb2 will be used for modelling with meteo data collected by the smart-trap sensors (count data)
#df_aspb4 will be used fot modelling meteo data from XEMA stations (continuos data)

#join df_aspb2 with df_sensor (testpulse data) to add meteo info when Aedes & Culex = 0
#df_aspb2 observations > df_sensor (testpulse) because it includes NA observations when the sensor was off
#df_sensor only contains data when the sensor was ON

df_model_perTrap <- full_join(df_aspb2, df_sensor, by = c("utc_1h","trap"))

#dummie test
kk <- df_model_perTrap[is.na(df_model_perTrap$HOD_utc) & !is.na(df_model_perTrap$Aedes), ]

#There are 157 observations where there are Aedes and Culex counts but no testpulse
#Since there was no testpulse data, the sensor was assumed to be OFF
#However, if there are captures is because the sensor was ON
#How is this possible? I do not know. Maybe some testpulse data was not sent to the server (connexion failed)
#Change sensor status to ON in these cases

df_model_perTrap$status <-ifelse(df_model_perTrap$status=="OFF" & !is.na(df_model_perTrap$Aedes),"ON",df_model_perTrap$status)

#Generally, always get meteo data from tagged mosquito records (real-time) and not from testpulse (each 30min)
#Attribute only meteo data from testpulse when Aedes & Culex = 0 (no meteo data from tagged event)

df_model_perTrap$temp_sensor.x <-ifelse(is.na(df_model_perTrap$temp_sensor.x) & !is.na(df_model_perTrap$temp_sensor.y),
                                        df_model_perTrap$temp_sensor.y,df_model_perTrap$temp_sensor.x)

df_model_perTrap$RH_sensor.x <-ifelse(is.na(df_model_perTrap$RH_sensor.x) & !is.na(df_model_perTrap$RH_sensor.y),
                                        df_model_perTrap$RH_sensor.y,df_model_perTrap$RH_sensor.x)

#Delete unuseful columns an rename variables

df_model_perTrap <- df_model_perTrap[, -c(9,10)]
df_model_perTrap <- rename(df_model_perTrap, temp_sensor = temp_sensor.x, RH_sensor = RH_sensor.x)

#Add extra variables

# Transform datetime_utc (in UTC) to local time (Europe/Madrid)

df_model_perTrap$datetime_local <- with_tz(df_model_perTrap$utc_1h, tz = "Europe/Madrid")

# Define is_st function to detect summer time 

is_st <- function(datetime) {
  return(ifelse(dst(datetime), 2, 1))  # is_st = 2 (summer), is_st = 1 (winter)
}

# Calculate time zone (tz) with is_st function

df_model_perTrap$tz <- sapply(df_model_perTrap$datetime_local, is_st)

#Obtain approximate hours for sunrise, sunset and day length with package "Activity"

snt<-get_suntimes(df_model_perTrap$datetime_local,
                  df_model_perTrap$latitude,  
                  df_model_perTrap$longitude,
                  offset= df_model_perTrap$tz) #offset = tz

df_model_perTrap$sunrise_local_decimal<-snt$sunrise #Clock time in decimals
df_model_perTrap$sunset_local_decimal<-snt$sunset #Clock time in decimals
df_model_perTrap$daylength<-snt$daylength

#Local time in decimals (time_local_decimal)

df_model_perTrap <- df_model_perTrap %>%
  mutate(time_local_decimal = as.numeric(format(datetime_local, "%H")) + 
           as.numeric(format(datetime_local, "%M")) / 60 + 
           as.numeric(format(datetime_local, "%S")) / 3600)

#Time distance (in decimals) from sunrise and sunset (dist_sunrise, dist_sunset)

df_model_perTrap$dist_sunrise<-abs(df_model_perTrap$time_local_decimal-df_model_perTrap$sunrise_local_decimal)
df_model_perTrap$dist_sunset<-abs(df_model_perTrap$time_local_decimal-df_model_perTrap$sunset_local_decimal)

#Day of year (DOY)

df_model_perTrap$DOY <-yday(df_model_perTrap$date)

#Calculate cumulative average values for temperature and ADH (Activity Degree Hours) 24h, 12h and 6h before
#When the sensor is OFF, we need to deal with NA values
#When there is any NA in the previous observations, the cum value is also NA

#Mean temperature in the last 24 hours (av_temp_24h)

df_model_perTrap <- df_model_perTrap %>%
    arrange(trap, utc_1h) %>% #order by trap and datetime
    group_by(trap) %>% #group by trap
    mutate(
    temp_sensor_lag = lag(temp_sensor),#create a lag variable
    av_temp_24h = rollapply(
      data = temp_sensor_lag,
      width = 24,
      FUN = function(x) {
        if(sum(!is.na(x)) == 24) { #if any of the 24 previous rows contains NA, it returns NA
          mean(x)
        } else {
          NA
        }
      },
      align = "right",
      fill = NA
    )
  ) %>%
  ungroup()

#Mean temperature in the last 12 hours (av_temp_12h)

df_model_perTrap <- df_model_perTrap %>%
  arrange(trap, utc_1h) %>% #order by trap and datetime
  group_by(trap) %>% #group by trap
  mutate(
    temp_sensor_lag = lag(temp_sensor),#create a lag variable
    av_temp_12h = rollapply(
      data = temp_sensor_lag,
      width = 12,
      FUN = function(x) {
        if(sum(!is.na(x)) == 12) { #if any of the 12 previous rows contains NA, it returns NA
          mean(x)
        } else {
          NA
        }
      },
      align = "right",
      fill = NA
    )
  ) %>%
  ungroup()

#Mean temperature in the last 6 hours (av_temp_6h)

df_model_perTrap <- df_model_perTrap %>%
  arrange(trap, utc_1h) %>% #order by trap and datetime
  group_by(trap) %>% #group by trap
  mutate(
    temp_sensor_lag = lag(temp_sensor),#create a lag variable
    av_temp_6h = rollapply(
      data = temp_sensor_lag,
      width = 6,
      FUN = function(x) {
        if(sum(!is.na(x)) == 6) { #if any of the 6 previous rows contains NA, it returns NA
          mean(x)
        } else {
          NA
        }
      },
      align = "right",
      fill = NA
    )
  ) %>%
  ungroup()

#Activity Degree Hours (ADH)

#We have transformed the concept of Growing Degree Hours (GDH) 
#And we have applied it to mosquito activity, not growth 
#A mosquito is only active within a range of temperatures (T_min and T_max)
#Below T_min and above T_max the activity is not possible
#We have established a T_min = 10ºC and a T_max = 35ºC according to the literature and our data

T_min <- 10 
T_max <- 35 

#If the recorded temperature at X hour (temp_sensor) is above T_min and below T_max, ADH = temp_sensor - T_min
#If not, ADH = 0

df_model_perTrap$ADH <- ifelse(df_model_perTrap$temp_sensor > T_min & df_model_perTrap$temp_sensor < T_max, 
                           df_model_perTrap$temp_sensor - T_min, 0)

#Cumulative ADH in the last 24 hours (cum_ADH_24h)

df_model_perTrap <- df_model_perTrap %>%
  arrange(trap, utc_1h) %>% #order by trap and datetime
  group_by(trap) %>% #group by trap
  mutate(
    ADH_lag = lag(ADH),#create a lag variable
    cum_ADH_24h = rollapply(
      data = ADH_lag,
      width = 24,
      FUN = function(x) {
        if(sum(!is.na(x)) == 24) { #if any of the 24 previous rows contains NA, it returns NA
          sum (x)
        } else {
          NA
        }
      },
      align = "right",
      fill = NA
    )
  ) %>%
  ungroup()

#Cumulative ADH in the last 12 hours (cum_ADH_12h)

df_model_perTrap <- df_model_perTrap %>%
  arrange(trap, utc_1h) %>% #order by trap and datetime
  group_by(trap) %>% #group by trap
  mutate(
    ADH_lag = lag(ADH),#create a lag variable
    cum_ADH_12h = rollapply(
      data = ADH_lag,
      width = 12,
      FUN = function(x) {
        if(sum(!is.na(x)) == 12) { #if any of the 12 previous rows contains NA, it returns NA
          sum (x)
        } else {
          NA
        }
      },
      align = "right",
      fill = NA
    )
  ) %>%
  ungroup()

#Cumulative ADH in the last 6 hours (cum_ADH_6h)

df_model_perTrap <- df_model_perTrap %>%
  arrange(trap, utc_1h) %>% #order by trap and datetime
  group_by(trap) %>% #group by trap
  mutate(
    ADH_lag = lag(ADH),#create a lag variable
    cum_ADH_6h = rollapply(
      data = ADH_lag,
      width = 6,
      FUN = function(x) {
        if(sum(!is.na(x)) == 6) { #if any of the 6 previous rows contains NA, it returns NA
          sum (x)
        } else {
          NA
        }
      },
      align = "right",
      fill = NA
    )
  ) %>%
  ungroup()

#Save df in a .csv file

write.csv(df_model_perTrap, 
          file = "~/yourpath/df_model_perTrap.csv", 
          row.names = FALSE)

#Join df_aspb4 with df_xema

df_model_allTraps <- full_join(df_aspb4, df_xema, by = c("utc_1h"))

df_model_allTraps_female <- full_join(df_aspb4_female, df_xema, by = c("utc_1h"))

#Save df in a .csv file

write.csv(df_model_allTraps, 
          file = "~/yourpath/df_model_allTraps.csv", 
          row.names = FALSE)

write.csv(df_model_allTraps_female, 
          file = "~/yourpath/df_model_allTraps_female.csv", 
          row.names = FALSE)