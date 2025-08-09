
################################################################################

#Evolution of the hourly mosquito activity in Barcelona
#From 2004 to the present

################################################################################

#Load libraries

library(dplyr)
library(lubridate)
library(magrittr) 
library(activity)
library(tidyr)
library (readr)
library(TTR)
library(dunn.test)
library(Kendall)
library(purrr)
library(cowplot)
library(ggplot2)
library(readr)

#Define wd

setwd("/yourdirectory")

################################################################################

#Unzip files

input_folder <- "/yourpath/"
output_folder <- "/yourpath/"

# List of .nc files in input folder
files <- list.files(input_folder, pattern = "\\.nc$", full.names = TRUE)

# Unzip and rename files
for (file in files) {
  # Extract base name
  base_name <- tools::file_path_sans_ext(basename(file))
  
  # Extract year and month
  year_month <- sub(".*_(\\d{4})_(\\d{2}).*", "\\1_\\2", base_name)
  
  # Create a folder with year and month name
  dir.create(file.path(output_folder, year_month), showWarnings = FALSE)
  
  # Unzip .nc file inside their corresponding folder
  unzip(file, exdir = file.path(output_folder, year_month))
  
  # Rename files
  new_file_instant <- file.path(output_folder, year_month, paste0(base_name, "_instant.nc"))
  new_file_accum <- file.path(output_folder, year_month, paste0(base_name, "_accum.nc"))
  
  files_unzipped <- list.files(file.path(output_folder, year_month), full.names = TRUE)
  if (length(files_unzipped) == 2) {
    file.rename(files_unzipped[1], new_file_instant)
    file.rename(files_unzipped[2], new_file_accum)
  }
}

#From each compressed file, two files are generated: "instant" and "accum"
#Variables of interest should be extracted from both of them

################################################################################

#Load unzip files

#Function to transform .nc files to df
#Extract variables from .nc files

transform_nc_to_df <- function(file_instant, file_accum) {
  
  # Read time
  time_i <- h5read(file_instant, "valid_time")
  time_a <- h5read(file_accum, "valid_time")
  
  # Tranform time to POSIXct object
  time_i <- as.POSIXct(time_i, origin = "1970-01-01", tz = "UTC")
  time_a <- as.POSIXct(time_a, origin = "1970-01-01", tz = "UTC")
  
  # Read instant variables
  tp   <- as.vector(h5read(file_instant, "tp"))
  ssrd <- as.vector(h5read(file_instant, "ssrd"))
  
  # Read cumulated variables
  t2m <- as.vector(h5read(file_accum, "t2m"))
  d2m <- as.vector(h5read(file_accum, "d2m"))
  
  # Create separate df
  df_i <- data.frame(time = time_i, tp = tp, ssrd = ssrd)
  df_a <- data.frame(time = time_a, t2m = t2m, d2m = d2m)
  
  # join by time 
  df_final <- full_join(df_i, df_a, by = "time") %>% arrange(time)
  
  return(df_final)
}

# Apply to all months within a year

# Base folder
base_dir <- "/yourpath"

# Obtain folders corresponding to an specific year
read_year_data <- function(year) {
  folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  folders_years <- grep(paste0("^", year, "_"), basename(folders), value = TRUE)
  
  dfs_month <- lapply(folders_years, function(month) {
    folder_path <- file.path(base_dir, month)
    f_instant <- list.files(folder_path, pattern = "_instant.nc$", full.names = TRUE)
    f_accum   <- list.files(folder_path, pattern = "_accum.nc$", full.names = TRUE)
    
    if (length(f_instant) == 1 && length(f_accum) == 1) {
      transform_nc_to_df(f_instant, f_accum)
    } else {
      NULL
    }
  })
  
  #Combine month in one single df per year
  bind_rows(dfs_month)
}

#Use the function to each year and write them in .csv files

load_all_years <- function(start_year = 2004, end_year = 2024) {
  years <- start_year:end_year
  data_list <- lapply(years, read_year_data)
  names(data_list) <- paste0("df_", years)
  return(data_list)
}

write_all_years <- function(data_list, path = "/yourpath/ERA5_df") {
  for (name in names(data_list)) {
    year <- gsub("df_", "", name)  
    file_path <- file.path(path, paste0("df_", year, ".csv"))
    write.csv(data_list[[name]], file = file_path, row.names = FALSE)
  }
}

all_data <- load_all_years()
write_all_years(all_data)

#Bind all df in one single for all years

df_2004_2024 <- do.call(rbind, all_data)
write.csv(df_2004_2024, file = "/hyourpath/df_2004_2024.csv", row.names = FALSE)

################################################################################

#Load df_2004_2024 and pre-process it

df_2004_2024 <- read.csv("~/yourpath/df_2004_2024.csv")

#List of variables in df_2004_2024:

#t2m:	Air temperature (2m above the ground)(K)
#d2m: Temperature at dew point (K)
#tp: Total precipitation (m)
#ssrd: Surface solar radiation downwards (J/m²)

#Transform variables to adjust to XEMA (Meteocat) units:

#temp_xema (ºC)
#RH_xema (%)
#precip_xema (mm)
#solar_rad_xema (W/m²)

#Also incorporate other variables that are included in the model

#dist_sunset (decimal hours)
#HOD_utc (hours)
#cum_precip_24h (mm)

#Magnus formula to calculate relative humidity

magnus_RH <- function(t, td) {
  # t = t2m
  # td = d2m
  a <- 17.625
  b <- 243.04
  alpha_t  <- (a * t) / (b + t)
  alpha_td <- (a * td) / (b + td)
  RH <- 100 * exp(alpha_td - alpha_t)
  return(RH)
}

#Transform units

df_2004_2024 <- df_2004_2024 %>%
  mutate(
    time = ifelse(
      grepl("^\\d{4}-\\d{2}-\\d{2}$", time), 
      paste0(time, " 00:00:00"), 
      time),
    time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    utc_1h = as.POSIXct (time, format= "%Y-%m-%d %H:%M:%S",  tz = "UTC"),
    temp_xema = t2m - 273.15,
    dew_point_C = d2m - 273.15,
    precip_xema = tp * 1000,
    solar_rad_xema = ssrd / 3600,
    RH_xema = magnus_RH(temp_xema, dew_point_C)
  )

#Add required variables to the model

df_2004_2024$date <- as.Date(df_2004_2024$utc_1h)

Sys.setlocale("LC_TIME", "C") #change system language to English
df_2004_2024$month <- format(df_2004_2024$date, "%b")

df_2004_2024$year <-year(df_2004_2024$date)

#Aprox coordinates of ERA5 box request (same as XEMA [X8])
df_2004_2024$latitude <-41.37919 
df_2004_2024$longitude <-2.10540

# Transform datetime (in UTC) to local time (Europe/Madrid)
# Be aware that until 16 of March 1940, local time in Spain was UTC + 0

df_2004_2024$datetime_local <- with_tz(df_2004_2024$utc_1h, tz = "Europe/Madrid")

# Define is_st function to detect summer time 

is_st <- function(datetime) {
  return(ifelse(dst(datetime), 2, 1))  # is_st = 2 (summer), is_st = 1 (winter)
}

# Calculate time zone (tz) with is_st function

df_2004_2024$tz <- sapply(df_2004_2024$datetime_local, is_st)

#Obtain approximate hours for sunrise, sunset and day length with package "Activity"

snt<-get_suntimes(df_2004_2024$datetime_local,
                  df_2004_2024$latitude,  
                  df_2004_2024$longitude,
                  offset= df_2004_2024$tz) #offset = tz

df_2004_2024$sunrise_local_decimal<-snt$sunrise #Clock time in decimals
df_2004_2024$sunset_local_decimal<-snt$sunset #Clock time in decimals
df_2004_2024$daylength<-snt$daylength

#Local time in decimals (time_local_decimal)

df_2004_2024 <- df_2004_2024 %>%
  mutate(time_local_decimal = as.numeric(format(datetime_local, "%H")) + 
           as.numeric(format(datetime_local, "%M")) / 60 + 
           as.numeric(format(datetime_local, "%S")) / 3600)

#Time distance (in decimals) from sunrise and sunset (dist_sunrise, dist_sunset)

df_2004_2024$dist_sunrise<-abs(df_2004_2024$time_local_decimal-df_2004_2024$sunrise_local_decimal)
df_2004_2024$dist_sunset<-abs(df_2004_2024$time_local_decimal-df_2004_2024$sunset_local_decimal)

#Day of year (DOY)

df_2004_2024$DOY <-yday(df_2004_2024$date)

#Hour of Day (HOD)

df_2004_2024 <- df_2004_2024 %>%
  mutate(HOD_utc = as.numeric(format(utc_1h, "%H")) + 
           as.numeric(format(utc_1h, "%M")) / 60 + 
           as.numeric(format(utc_1h, "%S")) / 3600) 

#Cumulative precipitation in the last 12 hours (cum_precip_12h)

df_2004_2024 <- df_2004_2024[order(df_2004_2024$utc_1h), ] # Ensure chronological order for cumulative variables

df_2004_2024$cum_precip_12h <- c(NA, runSum(df_2004_2024$precip_xema, n = 12)[-length(df_2004_2024$precip_xema)])

#Average RH in the last 12 hours (av_RH_12h)

df_2004_2024 <- df_2004_2024[order(df_2004_2024$utc_1h), ] # Ensure chronological order for cumulative variables

df_2004_2024$av_RH_12h <- c(NA, runMean(df_2004_2024$precip_xema, n = 12)[-length(df_2004_2024$precip_xema)])

#Write a .csv file

write.csv(df_2004_2024, file = "~/yourpath/df_2004_2024_processed.csv", row.names = FALSE)

################################################################################

#Modeling hourly mosquito density based on historical climatic data

# Load df

df_2004_2024_processed <- read_csv("~/yourpath/df_2004_2024_processed.csv")
df_2004_2024_processed_clean <- na.omit(df_2004_2024_processed) # Ensure no NAs

#At this point, RF models should be loaded in R environment
#Script can be found in 6_Modelling_mosquito_abundance.R

#Predicted
RF_pred_2004_2024 <- df_2004_2024_processed_clean
RF_pred_2004_2024$Aedes_pred <- predict(rf_Ae13, newdata = df_2004_2024_processed_clean)
RF_pred_2004_2024$Culex_pred <- predict(rf_Cx13, newdata = df_2004_2024_processed_clean)

#Observed
df_model_allTraps <- read_csv("~/Desktop/ACTIVITY/ASPB/Fitact_NewAlgorithm/df_model_allTraps_female.csv")
df_model_allTraps_clean <- na.omit(df_model_allTraps) # Ensure no NAs

#Save df in .csv format in the current wd

write.csv(RF_pred_2004_2024, 
          file = "~/yourpath/RF_pred_2004_2024.csv", 
          row.names = FALSE)

################################################################################

#Represent predicted hourly mosquito abundance in calendar heatmaps

#Load data

RF_pred_2004_2024 <- read_csv("~/yourpath/RF_pred_2004_2024.csv")

RF_pred_2004_2024$month <- factor(RF_pred_2004_2024$month, levels = month.abb) #Order months

# Max predicted values for Aedes and Culex

max(RF_pred_2004_2024$Aedes_pred) #2.63
max(RF_pred_2004_2024$Culex_pred) #1.54

#Max observed values for Aedes and Culex

max(df_model_allTraps_clean$Aedes_SE) #7
max(df_model_allTraps_clean$Culex_SE) #5

#Aedes

ggplot(RF_pred_2004_2024,aes(x = HOD_utc, y = month, fill = Aedes_pred)) +
  geom_tile(colour = "black", width = 1) + 
  facet_wrap(~year) + 
  scale_fill_gradient(low = "white", high = "darkorange2", na.value = "grey90")+
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23), 
    labels = c("00:00", "06:00", "12:00", "18:00", "23:00"), 
    expand = c(0, 0), 
    limits = c(-0.5, 23.5)
  ) + 
  xlab("Hour of the day") + 
  ylab("Month of the year") + 
  ggtitle("Hourly Aedes albopictus counts corrected by sampling effort") + 
  theme_minimal(base_size = 12) +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),       
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(2, "lines")
    )

#Culex

ggplot(RF_pred_2004_2024,aes(x = HOD_utc, y = month, fill = Culex_pred)) +
  geom_tile(colour = "black", width = 1) + 
  facet_wrap(~year) + 
  scale_fill_gradient(low = "white", high = "darkmagenta", na.value = "grey90") +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23), 
    labels = c("00:00", "06:00", "12:00", "18:00", "23:00"), 
    expand = c(0, 0), 
    limits = c(-0.5, 23.5)
  ) + 
  xlab("Hour of the day") + 
  ylab("Month of the year") + 
  ggtitle("Hourly Culex pipiens counts corrected by sampling effort") + 
  theme_minimal(base_size = 12) +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),       
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(2, "lines")
  )

#Select only years 2004 and 2024

RF_pred_selected <- RF_pred_2004_2024 %>%
  filter(year %in% c(2004, 2014, 2024))

#Aedes
ggplot(RF_pred_selected, aes(x = HOD_utc, y = month, fill = Aedes_pred)) +
  geom_tile(colour = "black", width = 1) + 
  facet_wrap(~year) + 
  scale_fill_gradient(low = "white", high = "darkorange2", na.value = "grey90") +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23), 
    labels = c("00:00", "06:00", "12:00", "18:00", "23:00"), 
    expand = c(0, 0), 
    limits = c(-0.5, 23.5)
  ) + 
  xlab("Hour of the day") + 
  ylab("Month of the year") + 
  ggtitle("Hourly Aedes albopictus counts corrected by sampling effort") + 
  theme_minimal(base_size = 12) +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),       
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(2, "lines")
  )

#Culex
ggplot(RF_pred_selected, aes(x = HOD_utc, y = month, fill = Culex_pred)) +
  geom_tile(colour = "black", width = 1) + 
  facet_wrap(~year) + 
  scale_fill_gradient(low = "white", high = "darkmagenta", na.value = "grey90") +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23), 
    labels = c("00:00", "06:00", "12:00", "18:00", "23:00"), 
    expand = c(0, 0), 
    limits = c(-0.5, 23.5)
  ) + 
  xlab("Hour of the day") + 
  ylab("Month of the year") + 
  ggtitle("Hourly Culex pipiens counts corrected by sampling effort") + 
  theme_minimal(base_size = 12) +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),       
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(2, "lines")
  )

#Comparison between observed and predicted mosquitoes for 2024

df_model_allTraps <- df_model_allTraps <- read_csv("~/yourpath/df_model_allTraps_female.csv")

df_model_allTraps_clean <- na.omit(df_model_allTraps)

df_model_allTraps_clean$month <- factor(df_model_allTraps_clean$month, levels = month.abb) #Order months

#Aedes

#Observed
obs_Ae <- ggplot(df_model_allTraps_clean,aes(x = HOD_utc, y = month, fill = Aedes_SE)) +
  geom_tile(colour = "grey90", width = 1) + 
  scale_fill_gradient(low = "white", high = "darkorange2", na.value = "grey90",
                      limits=c(0,7))+ 
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23), 
    labels = c("00:00", "06:00", "12:00", "18:00", "23:00"), 
    expand = c(0, 0), 
    limits = c(-0.5, 23.5)
  ) + 
  xlab("Hour of the day") + 
  ylab("Month of the year") + 
  ggtitle("Time-series calendar heatmap for OBSERVED Aedes albopictus in 2024") + 
  theme_minimal(base_size = 12) +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),       
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 12, face = "bold")
  )

#Predicted
pred_Ae <- ggplot(RF_pred_2004_2024[RF_pred_2004_2024$year==2024 & RF_pred_2004_2024$month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct"),],
       aes(x = HOD_utc, y = month, fill = Aedes_pred)) +
  geom_tile(colour = "grey90", width = 1) + 
  scale_fill_gradient(low = "white", high = "darkorange2", na.value = "grey90",
                      limits=c(0,7))+
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23), 
    labels = c("00:00", "06:00", "12:00", "18:00", "23:00"), 
    expand = c(0, 0), 
    limits = c(-0.5, 23.5)
  ) + 
  xlab("Hour of the day") + 
  ylab("Month of the year") + 
  ggtitle("Time-series calendar heatmap for PREDICTED Aedes albopictus in 2024") + 
  theme_minimal(base_size = 12) +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),       
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 12, face = "bold")
  )

plot_grid(obs_Ae, pred_Ae, ncol = 2)

#Culex

#Observed
obs_Cx <- ggplot(df_model_allTraps_clean,aes(x = HOD_utc, y = month, fill = Culex_SE)) +
  geom_tile(colour = "grey90", width = 1) + 
  scale_fill_gradient(low = "white", high = "darkmagenta", na.value = "grey90",
                      limits=c(0,5))+ 
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23), 
    labels = c("00:00", "06:00", "12:00", "18:00", "23:00"), 
    expand = c(0, 0), 
    limits = c(-0.5, 23.5)
  ) + 
  xlab("Hour of the day") + 
  ylab("Month of the year") + 
  ggtitle("Time-series calendar heatmap for OBSERVED Culex pipiens in 2024") + 
  theme_minimal(base_size = 12) +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),       
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 12, face = "bold")
  )

#Predicted
pred_Cx <- ggplot(RF_pred_2004_2024[RF_pred_2004_2024$year==2024 & RF_pred_2004_2024$month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct"),],
       aes(x = HOD_utc, y = month, fill = Culex_pred)) +
  geom_tile(colour = "grey90", width = 1) + 
  scale_fill_gradient(low = "white", high = "darkmagenta", na.value = "grey90",
                      limits=c(0,5))+
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23), 
    labels = c("00:00", "06:00", "12:00", "18:00", "23:00"), 
    expand = c(0, 0), 
    limits = c(-0.5, 23.5)
  ) + 
  xlab("Hour of the day") + 
  ylab("Month of the year") + 
  ggtitle("Time-series calendar heatmap for PREDICTED Culex pipiens in 2024") + 
  theme_minimal(base_size = 12) +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),       
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 12, face = "bold")
  )

plot_grid(obs_Cx,pred_Cx, ncol = 2)

################################################################################

#Quantitative analysis of differences

################################################################################

# Mann-Kendall test to analyse temporal tendencies

#Aedes

kendall_tendency_Ae <- RF_pred_2004_2024 %>%
  group_by(month, HOD_utc) %>%
  nest() %>%
  mutate(
    mk_result = map(data, ~ tryCatch(MannKendall(.x$Aedes_pred), error = function(e) NA)),
    p_value = map_dbl(mk_result, function(res) {
      if (inherits(res, "Kendall")) res$sl else NA_real_
    }),
    tau = map_dbl(mk_result, function(res) {
      if (inherits(res, "Kendall")) res$tau else NA_real_
    }),
    significance = p_value < 0.05
  ) %>%
  dplyr::select(month, HOD_utc, p_value, tau, significance)

#Plot tendencies in a calendar 

kendall_tendency_Ae <- kendall_tendency_Ae %>%
  mutate(trend_color = case_when(
    significance== "TRUE" & tau > 0 ~ "Increasing",
    significance== "TRUE" & tau < 0 ~ "Decreasing",
    TRUE ~ "None"
  ))

ggplot(kendall_tendency_Ae, aes(x = HOD_utc, y = month, fill = trend_color)) +
  geom_tile(color = "grey80", width = 1, height = 1) +
  scale_fill_manual(
    values = c(
      "Increasing" = "red",
      "Decreasing" = "blue",
      "None" = "white"
    ),
    name = "Tendency"
  ) +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23),
    labels = c("00:00", "06:00", "12:00", "18:00", "23:00"),
    expand = c(0, 0),
    limits = c(-0.5, 23.5)
  ) +
  labs(
    title = "Mann-Kendall tendencies for Aedes",
    x = "HOD",
    y = "Month"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text = element_text(size = 12, face = "bold")
  )

#Culex

kendall_tendency_Cx <- RF_pred_2004_2024 %>%
  group_by(month, HOD_utc) %>%
  nest() %>%
  mutate(
    mk_result = map(data, ~ tryCatch(MannKendall(.x$Culex_pred), error = function(e) NA)),
    p_value = map_dbl(mk_result, function(res) {
      if (inherits(res, "Kendall")) res$sl else NA_real_
    }),
    tau = map_dbl(mk_result, function(res) {
      if (inherits(res, "Kendall")) res$tau else NA_real_
    }),
    significance = p_value < 0.05
  ) %>%
  dplyr::select(month, HOD_utc, p_value, tau, significance)

#Plot tendencies in a calendar 

kendall_tendency_Cx <- kendall_tendency_Cx %>%
  mutate(trend_color = case_when(
    significance== "TRUE" & tau > 0 ~ "Increasing",
    significance== "TRUE" & tau < 0 ~ "Decreasing",
    TRUE ~ "None"
  ))

ggplot(kendall_tendency_Cx, aes(x = HOD_utc, y = month, fill = trend_color)) +
  geom_tile(color = "grey80", width = 1, height = 1) +
  scale_fill_manual(
    values = c(
      "Increasing" = "red",
      "Decreasing" = "blue",
      "None" = "white"
    ),
    name = "Tendency"
  ) +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23),
    labels = c("00:00", "06:00", "12:00", "18:00", "23:00"),
    expand = c(0, 0),
    limits = c(-0.5, 23.5)
  ) +
  labs(
    title = "Mann-Kendall tendencies for Culex",
    x = "HOD",
    y = "Month"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text = element_text(size = 12, face = "bold")
  )

