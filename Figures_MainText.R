
################################################################################

#Main text figures

################################################################################

#Fig: BCN map 

################################################################################

library(mapSpain)
library(sf)
library(leaflet)
library(dplyr)
library()

points <- data.frame(
  name = c("Trap 1", "Trap 2", "Trap 3", "Trap 4", "XEMA"),
  lat = c(41.43314, 41.43922, 41.38878, 41.38633, 41.3792),
  lon = c(2.19108, 2.14717, 2.11700, 2.18969, 2.10539),
  colors= c("#1E90FF","#1E90FF", "#1E90FF","#1E90FF","#FF8C00")
)

bcn_province <- esp_get_munic_siane(region = "Barcelona", epsg = 4326)

barcelones <- c(
  "Barcelona", 
  "L'Hospitalet de Llobregat", 
  "Badalona", 
  "Santa Coloma de Gramenet", 
  "Sant Adrià de Besòs"
)

barcelones_shape <- bcn_province %>%
  filter(name %in% barcelones)

barcelones_perim <- st_union(barcelones_shape)

leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(
    data = barcelones_perim,
    color = "black",
    weight = 1.5,
    fill = FALSE,
    opacity = 1
  ) %>%
  addCircleMarkers(
    lng = points$lon,
    lat = points$lat,
    radius = 4,
    color = points$colors,
    fillOpacity = 0.8,
    label = points$name,
    popup = points$name,
    stroke = TRUE,
    weight = 1
  ) %>%
  addScaleBar(position = "topright", options = scaleBarOptions(imperial = FALSE)) %>%
  setView(lng = mean(points$lon), lat = mean(points$lat), zoom = 13)

  
################################################################################

#Fig: Raw data (Senscape output)

################################################################################

library(dplyr)
library(ggplot2)
library(lubridate)

df_aspb <- read.csv("~/yourpath/df_aspb_2021_2024.csv")

df_aspb$date <- as.Date(df_aspb$date) #ensure correct format
df_aspb$utc_1h <- as.POSIXct(df_aspb$utc_1h,format="%Y-%m-%d %H", tz="UTC")#ensure correct format
Sys.setlocale("LC_TIME", "C") #change system language to English

# Select an specific time period. 
df_time <- df_aspb %>%
  filter(date >= as.Date("2024-08-01") & date <= as.Date("2024-08-31"))

# Contar observaciones por fecha y género
counts <- df_time %>%
  group_by(date,genus, sex) %>%
  summarise(count = n(), .groups = "drop")

# Plot
p<-ggplot(counts, aes(x = date, y = count, color = genus, linetype = sex)) +
  geom_line() +
  geom_point(size = 1) +
  scale_linetype_manual(values = c("Female" = "solid", "Male" = "dashed")) +
  scale_color_manual(values = c("Aedes" = "darkorange", "Culex" = "darkorchid1")) +
  scale_x_date(
    breaks = as.Date(c("2024-08-01", "2024-08-15", "2024-08-31")),
    date_labels = "%d %b") +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10)) +
  labs(
    x = "Date",
    y = "Counts",
    color = "Genus",
    linetype = "Sex") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(color= "white", size = 12, face = "bold"),
    axis.text.y = element_text(color= "white",size = 12, face = "bold"),
    panel.grid = element_blank(), 
    axis.line = element_line(color = "white"),
    panel.background = element_rect(fill = "transparent", color = NA), 
    plot.background = element_rect(fill = "transparent", color = NA)  
  )

setwd("/yourdirectory")
ggsave("RawCountsTransparent.png", plot = p, bg = "transparent", dpi = 300)
