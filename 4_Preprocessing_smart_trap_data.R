
################################################################################

#Preprocessing smart-trap data from Senscape Hub 

################################################################################

#Data was downloaled from https://senscape.eu via API request
#Data was obtained from 4 smart-traps (aspb1, aspb2, aspb3 and aspb5)
#For the period comprised between 01/01/2021 and 31/12/2024

################################################################################

#Mosquito classification data

################################################################################

#Load libraries

library(lubridate)
library(dplyr) 
library(magrittr) 
library(activity)
library(ggplot2)

#Load data from different traps

aspb1 <- read.csv("~/yourpath/data_ASPB1_SantAndreu.csv")
aspb2 <- read.csv("~/yourpath/data_ASPB2_Horta.csv")
aspb3 <- read.csv("~/yourpath/data_ASPB3_Pedralbes.csv")
aspb5 <- read.csv("~/yourpath/data_ASPB5_Zoo.csv")

# Join together 

aspb <- rbind(aspb1, aspb2, aspb3, aspb5)

#Redefine datetime objects

aspb$datetime_utc <- as.POSIXct(aspb$record_time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
aspb$datetime_local <- as.POSIXct(aspb$datetime_local, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())

#Delete unuseful columns from "aspb"

aspb<- aspb[,-c(1,3,5,8,9,10,11)]

#Rename variables

aspb <- aspb  %>% 
        rename(temp_sensor = temperature,
               RH_sensor = humidity,
               class = classification,
               device_name = client_name)

#Add new date-time variables

aspb$utc_1h <- floor_date(aspb$datetime_utc, unit = "hour") #Round utc time to "hours"
#E.g.: Between 13:00 and 13:59, utc_1h = 13:00

aspb$date <- as.Date(aspb$datetime_utc)

Sys.setlocale("LC_TIME", "C") #change system language to English
aspb$month <- format(aspb$date, "%b")

aspb$year <-year(aspb$date)

aspb<- aspb %>%
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

#Create new variables

aspb <- aspb  %>% 
  
  mutate(genus = case_when(grepl("Culex", class) ~ "Culex",
                           grepl("Aedes", class) ~ "Aedes"),
         
         sex = case_when(grepl("female", class) ~ "Female",
                         grepl("male", class) ~ "Male"),
         
         trap= case_when(grepl ("1", device_name)~ "1",
                         grepl ("2", device_name)~ "2",
                         grepl ("3", device_name)~ "3",
                         grepl ("5", device_name)~ "4")) %>% #Device 5 (Zoo)is trap 4 
  
  mutate(latitude = case_when(trap == 1 ~ 41.43313, 
                              trap == 2 ~ 41.43900,
                              trap == 3 ~ 41.38878,
                              trap == 4 ~ 41.38633,
                              TRUE ~ NA_real_),
         
         longitude = case_when(trap == 1 ~ 2.19107,
                               trap == 2 ~ 2.147321,
                               trap == 3 ~ 2.117010,
                               trap == 4 ~ 2.189701,
                               TRUE ~ NA_real_))

#Check for outliers

ggplot(aspb, aes(x = utc_1h, y = temp_sensor)) +
  geom_line(alpha = 0.7) +
  labs(x = "Datetime", y = "Temperature", 
       title = "Temporal series of temperature") +
  facet_wrap(~"Temperature") +
  theme_minimal()

#There is an outlier the day 28/08/2023 in trap 3
#Delete it

aspb <- aspb %>%
mutate(temp_sensor = ifelse(trap == 3 & date == "2023-08-28", NA, temp_sensor),
       RH_sensor = ifelse(trap == 3 & date == "2023-08-28", NA, RH_sensor))

#Save the df in a .cvs file

write.csv(aspb, file = "~/Desktop/ACTIVITY/ASPB/Fitact_NewAlgorithm/df_aspb_2021_2024.csv", row.names = FALSE)

################################################################################

#Test pulse data

################################################################################

#Load test pulse data from different traps

TestPulse1 <- read.csv("~/Desktop/ACTIVITY/ASPB/Data_download/TestPulse_ASPB1.csv")
TestPulse2 <- read.csv("~/Desktop/ACTIVITY/ASPB/Data_download/TestPulse_ASPB2.csv")
TestPulse3 <- read.csv("~/Desktop/ACTIVITY/ASPB/Data_download/TestPulse_ASPB3.csv")
TestPulse4 <- read.csv("~/Desktop/ACTIVITY/ASPB/Data_download/TestPulse_ASPB5.csv")

# Join together 

TestPulse  <- rbind(TestPulse1, TestPulse2, TestPulse3, TestPulse4)

#Delete unuseful columns 

TestPulse <- TestPulse[,-c(1,3,4,8,9,10,11)]

#Redefine datetime objects

TestPulse$record_time <- ymd_hms (TestPulse$record_time, tz = "UTC")
TestPulse$utc_1h <- floor_date(TestPulse$record_time, unit = "hour") #Round utc time to "hours"
#E.g.: Between 13:00 and 13:59, utc_1h = 13:00

#Rename variables

TestPulse <- TestPulse  %>% 
  rename(datetime_utc = record_time, 
         temp_sensor = temperature,
         RH_sensor = humidity,
         device_name = client_name)

#Add new variables

TestPulse <- TestPulse %>% 
  
  mutate(trap= case_when(grepl ("1", device_name)~ "1",
                         grepl ("2", device_name)~ "2",
                         grepl ("3", device_name)~ "3",
                         grepl ("5", device_name)~ "4"), #Device 5 (Zoo) is trap 4 
         
         latitude = case_when(trap == 1 ~ 41.43313, 
                              trap == 2 ~ 41.43900,
                              trap == 3 ~ 41.38878,
                              trap == 4 ~ 41.38633,
                              TRUE ~ NA_real_),
         
         longitude = case_when(trap == 1 ~ 2.19107,
                               trap == 2 ~ 2.147321,
                               trap == 3 ~ 2.117010,
                               trap == 4 ~ 2.189701,
                               TRUE ~ NA_real_),
         
         HOD_utc = as.numeric(format(datetime_utc, "%H")) + #Hour of day (HOD)
           as.numeric(format(datetime_utc, "%M")) / 60 + 
           as.numeric(format(datetime_utc, "%S")) / 3600) %>%
  
  mutate (HOD_utc = floor(HOD_utc))

#Group by date and hour and calculate the average temperature and humidity 
#Test-pulse measures are each 30min

TestPulse_av <- TestPulse %>%
  group_by(trap,utc_1h,HOD_utc) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) 

#Add new date-time variables

TestPulse_av$datetime_local <- with_tz(TestPulse_av$utc_1h, tzone = "Europe/Madrid")

TestPulse_av$date <- as.Date(TestPulse_av$utc_1h)

Sys.setlocale("LC_TIME", "C") #change system language to English
TestPulse_av$month <- format(TestPulse_av$date, "%b")

TestPulse_av$year <-year(TestPulse_av$date)

TestPulse_av<- TestPulse_av %>%
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

#Check for outliers

ggplot(TestPulse_av, aes(x = utc_1h, y = temp_sensor)) +
  geom_line(alpha = 0.7) +
  labs(x = "Datetime", y = "Temperature", 
       title = "Temporal series of temperature") +
  facet_wrap(~"Temperature") +
  theme_minimal()

#Delete temperature data from 17/08/2023 19:00h to 08/09/2023 9:00h in trap 3

TestPulse_av <- TestPulse_av %>%
  
                mutate(temp_sensor = ifelse(trap == 3 & 
                       utc_1h >= as.POSIXct("2023-08-27 19:00", tz = "UTC") & 
                       utc_1h <= as.POSIXct("2023-09-08 09:00", tz = "UTC"), 
                       NA, temp_sensor),
                       
                RH_sensor = ifelse(trap == 3 & 
                       utc_1h >= as.POSIXct("2023-08-27 19:00", tz = "UTC") & 
                       utc_1h <= as.POSIXct("2023-09-08 09:00", tz = "UTC"), 
                       NA, RH_sensor))

#Save the df in a .cvs file

write.csv(TestPulse_av, 
          file = "~/yourpath/df_TestPulse_2021_2024.csv", 
          row.names = FALSE)
