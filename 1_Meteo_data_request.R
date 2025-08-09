################################################################################

#Download hourly weather data from XEMA X8_Zona_Universitaria for the period 2021-2024

################################################################################

library(meteospain)
library(tidyverse)
library(writexl)

api_key_meteocat <- "yourapikey"

#Establish requested dates

start_dates2021 <- seq(as.Date('2021-01-01'), as.Date('2021-12-31'), 'days')
start_dates2022 <- seq(as.Date('2022-01-01'), as.Date('2022-12-31'), 'days')
start_dates2023 <- seq(as.Date('2023-01-01'), as.Date('2023-12-31'), 'days')
start_dates2024 <- seq(as.Date('2024-01-01'), as.Date('2024-12-31'), 'days')

#2021

library(purrr)
df_meteocat_aspb_X8_2021<-map(
  .x = start_dates2021,
  .f = function(start_date) {
    res <- get_meteo_from(
      'meteocat',
      meteocat_options(
        api_key = api_key_meteocat,
        resolution = 'hourly',
        start_date = start_date,
        stations= 'X8'
      )
    )
    return(res)
  }
) |>
  list_rbind()

df_meteocat_aspb_X8_2021<- as.data.frame(df_meteocat_aspb_X8_2021)
write_csv(df_meteocat_aspb_X8_2021, "df_meteocat_aspb_X8_2021.csv")

#2022

df_meteocat_aspb_X8_2022<-map(
  .x = start_dates2022,
  .f = function(start_date) {
    res <- get_meteo_from(
      'meteocat',
      meteocat_options(
        api_key = api_key_meteocat,
        resolution = 'hourly',
        start_date = start_date,
        stations= 'X8'
      )
    )
    return(res)
  }
) |>
  list_rbind()

df_meteocat_aspb_X8_2022<- as.data.frame(df_meteocat_aspb_X8_2022)
write_csv(df_meteocat_aspb_X8_2022, "df_meteocat_aspb_X8_2022.csv")

#2023

df_meteocat_aspb_X8_2023<-map(
  .x = start_dates2023,
  .f = function(start_date) {
    res <- get_meteo_from(
      'meteocat',
      meteocat_options(
        api_key = api_key_meteocat,
        resolution = 'hourly',
        start_date = start_date,
        stations= 'X8'
      )
    )
    return(res)
  }
) |>
  list_rbind()

df_meteocat_aspb_X8_2023<- as.data.frame(df_meteocat_aspb_X8_2023)
write_csv(df_meteocat_aspb_X8_2023, "df_meteocat_aspb_X8_2023.csv")

#2024

df_meteocat_aspb_X8_2024<-map(
  .x = start_dates2024,
  .f = function(start_date) {
    res <- get_meteo_from(
      'meteocat',
      meteocat_options(
        api_key = api_key_meteocat,
        resolution = 'hourly',
        start_date = start_date,
        stations= 'X8'
      )
    )
    return(res)
  }
) |>
  list_rbind()

df_meteocat_aspb_X8_2024<- as.data.frame(df_meteocat_aspb_X8_2024)
write_csv(df_meteocat_aspb_X8_2024, "df_meteocat_aspb_X8_2024.csv")

