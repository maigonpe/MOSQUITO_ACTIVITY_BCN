################################################################################

#Supplementary material:exploration of environmental data

################################################################################

#Load libraries

library(lubridate)
library(knitr)
library(ggsci)
library(magrittr) 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(reshape2)
library(tidyr)
library(nortest)
library(patchwork)

#Set up working directory

setwd("/yourdirectory")

################################################################################

#Fig S: Comparison of meteo data: XEMA vs smart-traps

################################################################################

#Analyse potential differences between weather variables recorded by xema (automatic weather stations)
#and weather data recorded by the smart-trap sensors

#Load data

df_xema <- read_csv("~/yourpath/df_xema.csv")
df_sensor <- read_csv("~/yourpath/df_TestPulse_2021_2024.csv")

#First split df by trap

df_sensor1 <- df_sensor [df_sensor$trap==1,] #weather data recorded by smart-trap 1
df_sensor2 <- df_sensor [df_sensor$trap==2,] #weather data recorded by smart-trap 2
df_sensor3 <- df_sensor [df_sensor$trap==3,] #weather data recorded by smart-trap 3
df_sensor4 <- df_sensor [df_sensor$trap==4,] #weather data recorded by smart-trap 4

#Join df_xema with df_sensor (1,2,3,4) by "utc_1h"

meteo1 <- full_join(df_xema, df_sensor1, by = "utc_1h")
meteo2 <- full_join(df_xema, df_sensor2, by = "utc_1h")
meteo3 <- full_join(df_xema, df_sensor3, by = "utc_1h")
meteo4 <- full_join(df_xema, df_sensor4, by = "utc_1h")

#Delete repeated columns

meteo1<- meteo1[,-c(10,11,33,36,37,38,39,40,41,42)]
meteo2<- meteo2[,-c(10,11,33,36,37,38,39,40,41,42)]
meteo3<- meteo3[,-c(10,11,33,36,37,38,39,40,41,42)]
meteo4<- meteo4[,-c(10,11,33,36,37,38,39,40,41,42)]

#Pivot from wide to long df with a column for data origin (xema, sensor)

meteo1_long <- meteo1 %>%
  pivot_longer(
    cols = starts_with("temp_") | starts_with("RH_"),
    names_to = c(".value", "origen"),
    names_pattern = "(temp|RH)_(xema|sensor)")

meteo2_long <- meteo2 %>%
  pivot_longer(
    cols = starts_with("temp_") | starts_with("RH_"),
    names_to = c(".value", "origen"),
    names_pattern = "(temp|RH)_(xema|sensor)")

meteo3_long <- meteo3 %>%
  pivot_longer(
    cols = starts_with("temp_") | starts_with("RH_"),
    names_to = c(".value", "origen"),
    names_pattern = "(temp|RH)_(xema|sensor)")

meteo4_long <- meteo4 %>%
  pivot_longer(
    cols = starts_with("temp_") | starts_with("RH_"),
    names_to = c(".value", "origen"),
    names_pattern = "(temp|RH)_(xema|sensor)")

#Plot temporal differences between XEMA and smart-trap data

#Temperature

#ASPB1
ggplot(meteo1_long, aes(x = utc_1h, y = temp, color = factor(origen, levels = c("xema", "sensor")))) +
  geom_line(alpha = 0.5) +
  labs(x = "Datetime", y = "Temperature", 
       title = "Temporal series of temperature: xema (X8) vs sensor (ASPB1)") +
  facet_wrap(~"Temperature") +
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),   
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank())

#ASPB2
ggplot(meteo2_long, aes(x = utc_1h, y = temp, color = factor(origen, levels = c("xema", "sensor")))) +
  geom_line(alpha = 0.5) +
  labs(x = "Datetime", y = "Temperature", 
       title = "Temporal series of temperature: xema (X8) vs sensor (ASPB2)") +
  facet_wrap(~"Temperature") +
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),   
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank())

#ASPB3
ggplot(meteo3_long, aes(x = utc_1h, y = temp, color = factor(origen, levels = c("xema", "sensor")))) +
  geom_line(alpha = 0.5) +
  labs(x = "Datetime", y = "Temperature", 
       title = "Temporal series of temperature: xema (X8) vs sensor (ASPB3)") +
  facet_wrap(~"Temperature") +
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),   
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank())

#ASPB4
ggplot(meteo4_long, aes(x = utc_1h, y = temp, color = factor(origen, levels = c("xema", "sensor")))) +
  geom_line(alpha = 0.5) +
  labs(x = "Datetime", y = "Temperature", 
       title = "Temporal series of temperature: xema (X8) vs sensor (ASPB4)") +
  facet_wrap(~"Temperature") +
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),   
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank())

#Humidity

#ASPB1
ggplot(meteo1_long, aes(x = utc_1h, y = RH, color = factor(origen, levels = c("xema", "sensor")))) +
  geom_line(alpha = 0.5) +
  labs(x = "Datetime", y = "Relative humidity", 
       title = "Temporal series of humidity: xema (X8) vs sensor (ASPB1)") +
  facet_wrap(~"Humidity") +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),   
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank())

#ASPB2
ggplot(meteo2_long, aes(x = utc_1h, y = RH, color = factor(origen, levels = c("xema", "sensor")))) +
  geom_line(alpha = 0.5) +
  labs(x = "Datetime", y = "Relative humidity", 
       title = "Temporal series of humidity: xema (X8) vs sensor (ASPB2)") +
  facet_wrap(~"Relative Humidity") +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),   
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank())

#ASPB3
ggplot(meteo3_long, aes(x = utc_1h, y = RH, color = factor(origen, levels = c("xema", "sensor")))) +
  geom_line(alpha = 0.5) +
  labs(x = "Datetime", y = "Relative humidity", 
       title = "Temporal series of humidity: xema (X8) vs sensor (ASPB3)") +
  facet_wrap(~"Relative Humidity") +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),   
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank())

#ASPB4
ggplot(meteo4_long, aes(x = utc_1h, y = RH, color = factor(origen, levels = c("xema", "sensor")))) +
  geom_line(alpha = 0.5) +
  labs(x = "Datetime", y = "Relative humidity", 
       title = "Temporal series of humidity: xema (X8) vs sensor (ASPB4)") +
  facet_wrap(~"Relative Humidity") +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),   
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank())

################################################################################

#Fig S: Thermal limits of Aedes albopictus and Culex pipiens 

################################################################################

#Load data (mosquito counts aggregated for all traps + meteo data from XEMA)

df_model_allTraps <- read_csv("~/yourpath/df_model_allTraps.csv")

#Generate new variables for mosquito suitability

df_model_allTraps <- df_model_allTraps %>%
  mutate(Ae_suit = ifelse(Aedes > 0, 1, Aedes),
         Cx_suit = ifelse(Culex > 0, 1, Culex))

# Convert to factor
df_model_allTraps <- df_model_allTraps %>%
  mutate(Ae_suit = as.factor(Ae_suit),
         Cx_suit = as.factor(Cx_suit))

#Omit NAs

df_model_allTraps <- na.omit (df_model_allTraps)

#Violin plots

vp_aedes <- ggplot(df_model_allTraps[df_model_allTraps$Ae_suit == 1, ], 
                   aes(x = factor(Ae_suit), y = temp_xema, fill = "Aedes")) +
  geom_violin(color = "darkorange", trim = FALSE, alpha = 0.5) +  
  geom_boxplot(color = "darkorange",width = 0.1, outlier.shape = NA, alpha = 0.7) +  
  theme_minimal() +
  labs(title = "Range of temperatures where Aedes albopictus is present",
       x = "Presence",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("Aedes" = "darkorange")) +  
  scale_y_continuous(limits = c(5, 40), breaks = seq(5, 40, 5)) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12, face = "bold"),  
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank()  
  )

vp_culex <- ggplot(df_model_allTraps[df_model_allTraps$Cx_suit == 1, ], 
                   aes(x = factor(Cx_suit), y = temp_xema, fill = "Culex")) +
  geom_violin(color = "darkorchid", trim = FALSE, alpha = 0.5) +  
  geom_boxplot(color = "darkorchid", width = 0.1, outlier.shape = NA, alpha = 0.7) +  
  theme_minimal() +
  labs(title = "Range of temperatures where Culex pipiens is present",
       x = "Presence",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("Culex" = "darkorchid")) +  
  scale_y_continuous(limits = c(5, 40), breaks = seq(5, 40, 5)) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12, face = "bold"),  
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank()  
  )

grid.arrange(vp_aedes, vp_culex, ncol = 2)

################################################################################

#Fig S: Daily average temperatures by month 

################################################################################

#Proportion of days/month where the average Tª is < 18ºC and > 28ºC
#Temperature data obtained from XEMA station 

#Load data

df_xema <- read_csv("~/yourpath/df_xema.csv")

#New "month" variable

Sys.setlocale("LC_TIME", "C") 
df_xema <- df_xema %>%  mutate (month= month(utc_1h),day= day(utc_1h)) %>% 
  mutate (month_name = month (month, label = TRUE, abbr = TRUE))

dailyTemp <- df_xema %>%
  group_by(year, month_name, day) %>%
  summarise(temp_xema_mean = mean(temp_xema, na.rm = TRUE)) %>%
  ungroup()

dailyTemp_summary <- dailyTemp %>%
  group_by(year, month_name) %>%
  summarise(
    total_days = n(),
    NumOfDays_AvTempBelow18 = sum(temp_xema_mean < 18, na.rm = TRUE),
    NumOfDays_AvTempAbove28 = sum(temp_xema_mean > 28, na.rm = TRUE)) %>%
  mutate(
    PropDays_AvTempBelow18 = (NumOfDays_AvTempBelow18 / total_days)*100,
    PropDays_AvTempAbove28 = (NumOfDays_AvTempAbove28 / total_days)*100)

ggplot(dailyTemp_summary, aes(x = month_name, y = PropDays_AvTempBelow18, group = factor(year), color = factor(year))) +
  geom_line(size=1) +  
  geom_point(size=2, shape=15) +  
  scale_color_manual(values = c("2021" = "lightskyblue1", "2022" = "lightskyblue2", "2023" = "lightskyblue3", "2024" = "lightskyblue4")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25))+
  labs(title = "Proportion of days where the average temperature < 18°C",
       x = "Month", y = "Percentage of days",
       color = "Year") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 12, face = "bold"), 
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()) 

ggplot(dailyTemp_summary, aes(x = month_name, y = PropDays_AvTempAbove28, group = factor(year), color = factor(year))) +
  geom_line(size=1) +  
  geom_point(size=2, shape=15) +  
  scale_color_manual(values = c("2021" = "lightskyblue1", "2022" = "lightskyblue2", "2023" = "lightskyblue3", "2024" = "lightskyblue4")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25))+
  labs(title = "Proportion of days where the average temperature > 28°C",
       x = "Month", y = "Percentage of days",
       color = "Year") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 12, face = "bold"), 
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()) 

#From June to September, the percentage of days/month where the average temperature is < 18ºC is 0%
#In May and October, the percentage of days/month where the average temperature is < 18ºC is inferior to 50% of the days
#Exception of May 2021, which is below 75%
#To be more specific, look in detail the average daily temperature of May and October 

#Mean daily temperature in May
ggplot(dailyTemp[dailyTemp$month_name == "May",], aes(x = day, y = temp_xema_mean, group = factor(year), color = factor(year))) +
  geom_line(size=1) +  
  geom_point(size=0)+
  scale_y_continuous(limits = c(10, 25), breaks = seq(0, 25, by = 5)) +  
  scale_x_continuous(limits = c(1, 31), breaks = seq(1, 31, by = 1)) + 
  scale_color_manual(values = c("2021" = "lightskyblue1", "2022" = "lightskyblue2", "2023" = "lightskyblue3", "2024" = "lightskyblue4")) +
  labs(title = "Mean daily temperature in May",
       x = "Days of the month", y = "Average temperature",
       color = "Year") +
  geom_hline(yintercept = 18, color = "red", linetype = "dashed", size = 1)+ 
  annotate("text", x = 31, y = 18, label = "18ºC", color = "red", size = 5, vjust = -1) + 
  theme_minimal()+
  theme(panel.grid.minor = element_blank()) +
  theme(
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12)) 

#Mean daily temperature in October
ggplot(dailyTemp[dailyTemp$month_name == "Oct",], aes(x = day, y = temp_xema_mean, group = factor(year), color = factor(year))) +
  geom_line(size=1) +  
  geom_point(size=0)+
  scale_y_continuous(limits = c(10, 25), breaks = seq(0, 25, by = 5)) +  
  scale_x_continuous(limits = c(1, 31), breaks = seq(1, 31, by = 1)) + 
  scale_color_manual(values = c("2021" = "lightskyblue1", "2022" = "lightskyblue2", "2023" = "lightskyblue3", "2024" = "lightskyblue4")) +
  labs(title = "Mean daily temperature in October",
       x = "Days of the month", y = "Average temperature",
       color = "Year") +
  geom_hline(yintercept = 18, color = "red", linetype = "dashed", size = 1)+ 
  annotate("text", x = 31, y = 18, label = "18ºC", color = "red", size = 5, vjust = -1) + 
  theme_minimal()+
  theme(panel.grid.minor = element_blank()) 

################################################################################

#Fig S: Correlation matrix

################################################################################

#Load data

df_model_allTraps <- read_csv("~/yourpath/df_model_allTraps.csv")

corr_matrix <- cor(df_model_allTraps %>% 
                     dplyr::select(temp_xema, RH_xema, precip_xema, solar_rad_xema,
                                   dist_sunrise, dist_sunset, HOD_utc, daylength, DOY, 
                                   cum_precip_24h, cum_precip_12h, cum_precip_6h,
                                   av_temp_24h, av_temp_12h, av_temp_6h, 
                                   ADH, cum_ADH_24h, cum_ADH_12h, cum_ADH_6h)%>%
                     na.omit())

melted_corr_matrix <- melt(corr_matrix)

png("CorrMatrix.png", width = 6, height = 4, units = "in", res = 300)
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "steelblue1", high = "firebrick2", mid = "snow", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation index") +
  geom_text(aes(label = round(value, 1)), color = "black", size = 2.5) +  
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 9, hjust = 1), 
    axis.title = element_blank(),
    axis.ticks = element_blank(),  
    axis.ticks.length = unit(0, "pt"),  
    panel.grid = element_blank()  
  ) +
  coord_fixed()
dev.off()

################################################################################

#Fig S: Data distribution analysis

################################################################################

#Load data

df_model_allTraps <- read_csv("~/yourpath/df_model_allTraps.csv")

#Histogram of count frequency

df_model_allTraps_hist <- df_model_allTraps %>%
  pivot_longer(cols = c(Aedes_SE, Culex_SE), 
               names_to = "Genus", 
               values_to = "Counts")

ggplot(df_model_allTraps_hist, aes(x = Counts, fill = Genus)) + 
  geom_histogram(alpha = 0.5, position = position_dodge(width = 0.3), bins = 30) + 
  scale_fill_manual(values = c("Aedes_SE" = "darkorange", "Culex_SE" = "darkorchid")) +  
  labs(x = "Aedes and Culex counts / sampling effort", y = "Frequency") +  
  scale_y_continuous(limits = c(0, 15000), breaks = seq(0, 15000, by = 5000)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"), 
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Normality assessment

# Q-Q plots
#Aedes
qqnorm(df_model_allTraps$Aedes_SE)
qqline(df_model_allTraps$Aedes_SE, col = "red")

#Culex
qqnorm(df_model_allTraps$Culex_SE)
qqline(df_model_allTraps$Culex_SE, col = "red")

#Anderson-Darling normality test
ad.test(df_model_allTraps$Aedes_SE) #p-value < 0.05
ad.test(df_model_allTraps$Culex_SE) #p-value < 0.05

################################################################################

#Fig S6: Non-linear relationships between explanatory and response variables

################################################################################

#Load data

df_model_allTraps <- read_csv("~/yourpath/df_model_allTraps.csv")

#Represent one-by-one relationships

# List of explanatory variables considered for modeling
exp_vars <- c("temp_xema", "RH_xema", "precip_xema", "solar_rad_xema",
              "dist_sunset", "HOD_utc", "DOY", "cum_precip_24h")

#Aedes
# Function to create a list of graphics
plot_Ae <- lapply(exp_vars, function(var) {
  ggplot(df_model_allTraps, aes(x = .data[[var]], y = Aedes_SE)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    labs(x = var, y = "Aedes_SE") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12, face = "bold"), 
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank()
    )
})

panel_Ae <- wrap_plots(plot_Ae, ncol = 2)+
            plot_annotation(title = "a",
              theme = theme(plot.title = element_text(size = 16, face = "bold")))# Combine plots
print(panel_Ae)

#Culex
# Function to create a list of graphics
plot_Cx <- lapply(exp_vars, function(var) {
  ggplot(df_model_allTraps, aes(x = .data[[var]], y = Culex_SE)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    labs(x = var, y = "Culex_SE") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12, face = "bold"), 
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank()
    )
})

# Combine plots
panel_Cx <- wrap_plots(plot_Cx, ncol = 2)+
            plot_annotation(title = "b",
             theme = theme(plot.title = element_text(size = 16, face = "bold")))
print(panel_Cx)


################################################################################

#Fig S: Photoperiod and temperature during the study period

################################################################################

#Representation of the photoperiod during the year

df_xema$month <- factor(df_xema$month, levels = month.abb) #Order months

monthlyPhotop <- df_xema %>%
  group_by(month) %>%
  summarise(
    sunrise_mean = mean(sunrise_local_decimal, na.rm = TRUE),
    sunset_mean = mean(sunset_local_decimal, na.rm = TRUE),
    daylength_mean = mean(daylength, na.rm = TRUE)) %>%
  ungroup()

monthlyTemp <- df_xema %>%
  group_by(year, month) %>%
  summarise(temp_xema_mean = mean(temp_xema, na.rm = TRUE),
            temp_xema_min = min(temp_xema, na.rm = TRUE),
            temp_xema_max= max (temp_xema, na.rm= TRUE)) %>%
  ungroup()

#Temperature + photoperiod
ggplot() +
  #Temperature
  geom_ribbon(data = monthlyTemp, 
              aes(x = month, ymin = temp_xema_min, ymax = temp_xema_max, fill = factor(year),   
                  group = factor(year)), alpha = 0.2) +
  geom_line(data = monthlyTemp, 
            aes(x = month, y = temp_xema_mean, color = factor(year), group = year), 
            size = 1) +
  geom_point(data = monthlyTemp, 
             aes(x = month, y = temp_xema_mean, color = factor(year)), 
             size = 0) +
  #Photoperiod
  geom_line(data = monthlyPhotop, 
            aes(x = month, y = (daylength_mean - 8) * (40/8), group = 1), size = 1, linetype= "dashed") +
  geom_point(data = monthlyPhotop, 
             aes(x = month, y = (daylength_mean - 8) * (40/8)), size = 0, alpha=0) +
  #scales
  scale_y_continuous(
    name = "Temperature (°C)",
    limits = c(0, 40),
    breaks = seq(0, 40, by = 5),
    sec.axis = sec_axis(~ . * (8/40) + 8, name = "Daylight Hours")
  ) +
  labs(title = "Monthly Average Temperature with Photoperiod",
       x = "Month", color = "Year", fill = "Year") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

################################################################################

#Fig S: Changes in temperature and precipitation during a 20 year period (2004-2024)

################################################################################

yearPrecip <- df_2004_2024_processed %>%
  group_by(year) %>%
  summarise(
    precip_sum = sum(precip_xema, na.rm = TRUE)) %>%
  ungroup()

yearTemp <- df_2004_2024_processed%>%
  group_by(year) %>%
  summarise(temp_xema_mean = mean(temp_xema, na.rm = TRUE),
            temp_xema_min = min(temp_xema, na.rm = TRUE),
            temp_xema_max= max (temp_xema, na.rm= TRUE)) %>%
  ungroup()

# Scale precipitation to fit the plot
scale_factor <- 0.05

ggplot() +
  # Temperature (max, min)
  geom_ribbon(data = yearTemp, 
              aes(x = year, ymin = temp_xema_min, ymax = temp_xema_max), 
              fill = "firebrick1", alpha = 0.2) +
  
  # Temperature (avrg)
  geom_line(data = yearTemp, 
            aes(x = year, y = temp_xema_mean), 
            color = "firebrick1", size = 1) +
  
  # Precipitation
  geom_col(data = yearPrecip, 
           aes(x = year, y = precip_sum * scale_factor), 
           fill = "lightskyblue", width = 0.6, alpha = 0.6) +
  
  # Ejes
  scale_y_continuous(
    name = "Mean Temperature (°C)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Total Annual Precipitation (mm)")
  ) +
  
  # Estética
  labs(
    title = "Annual Trends of Temperature and Precipitation (2004–2024)",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.y.left = element_text(color = "firebrick1", size = 12),
    axis.title.y.right = element_text(color = "lightskyblue", size = 12),
    axis.text = element_text(size = 11)
  )

#
